#include <iostream>
#include <string_view>
#include <vector>
#include <charconv>
#include <map>
#include <variant>
#include <optional>
#include <string>
#include <array>
#include <list>
#include <thread>
#include <future>
#include <sstream>
#include <numeric>
#include <ranges>
#include <algorithm>

#include "Json.hpp"

#include <windows.h>
#include <wininet.h>
#pragma comment(lib, "wininet.lib")

template<class Ty> Ty operator~(std::future<Ty>& f) { return f.get(); }
template<class Ty> Ty operator~(std::future<Ty>&& f) { return std::move(f).get(); }

#define await ~

std::string& to_lower(std::string& str) {
    for (auto& c : str) c = std::tolower(c);
    return str;
}


struct fetch_settings {
    std::string ua = "C++";
    std::string method = "GET";
    std::string version = "HTTP/1.1";
    std::string data = "";
    std::vector<std::string> headers = {};
    std::vector<std::string> accept{ "text/*" };
    INTERNET_PORT port = INTERNET_DEFAULT_HTTPS_PORT;
};

struct response {
    response(bool success = false) : ok(success) {}

    operator bool() { return ok; }

    bool ok = false;
    std::string statusText{};
    std::uint32_t status{};
    std::map<std::string, std::string> headers{};
    std::string body{};
};

template<class Ty, auto Deleter>
struct handle {
    Ty ptr = nullptr;
    Ty operator->() const { return ptr; }
    operator Ty() const { return ptr; }
    handle(Ty ptr) : ptr(ptr) {}
    handle(const handle&) = delete;
    handle(handle&& o) : ptr(o.ptr) { o.ptr = nullptr; }
    ~handle() { if (ptr) Deleter(ptr); }
};

struct url {
    std::string scheme{};
    std::string domain{};
    std::string path{};
    std::string form{};
    std::string resource{};

    static url parse(std::string_view str) {
        // Url is formatted like this:
        // <scheme>://<domain>/<path>?<form>
        url result{};
        // Parse scheme
        std::size_t _p1 = str.find_first_of(':');
        result.scheme = str.substr(0ull, _p1);
        str = str.substr(_p1 + 3ull);
        // Parse domain
        std::size_t _p2 = str.find_first_of('/');
        result.domain = str.substr(0ull, _p2);
        if (_p2 == std::wstring_view::npos) return result;
        str = str.substr(_p2);
        // Parse path and form
        std::size_t _p3 = str.find_first_of('?');
        result.path = str.substr(0ull, _p3);
        if (_p3 == std::wstring_view::npos) return result;
        result.form = str.substr(_p3 + 1ull);
        result.resource = result.path + "?" + result.form;
        return result;
    }
};

std::future<response> fetch(const std::string& uri, fetch_settings&& settings = {}) {
    std::promise<response> _promise;
    std::future<response> _future = _promise.get_future();
    std::thread{ [uri, settings = std::move(settings), promise = std::move(_promise)]() mutable {
        url _url = url::parse(uri);
        bool _ssl = _url.scheme == "https";

        auto _fail = [&]() {
            auto _error = GetLastError();
            return promise.set_value(response{});
        };
        using internet = handle<HINTERNET, InternetCloseHandle>;

        // See if internet is available
        if (InternetAttemptConnect(0) != ERROR_SUCCESS) return _fail();

        // Open internet
        internet _opened = InternetOpen(
            /* user agent   */ settings.ua.c_str(),
            /* access type  */ INTERNET_OPEN_TYPE_PRECONFIG,
            /* proxy name   */ nullptr,
            /* proxy bypass */ nullptr,
            /* flags        */ 0 // Async not necessary
        );

        if (!_opened) return _fail();

        internet _connected = InternetConnect(_opened,
            /* host name    */ _url.domain.c_str(),
            /* port         */ INTERNET_DEFAULT_HTTPS_PORT,
            /* username     */ nullptr,
            /* password     */ nullptr,
            /* service      */ INTERNET_SERVICE_HTTP,
            /* flags        */ 0,
            /* context      */ 0
        );

        if (!_connected) return _fail();

        // Construct a headers string from the vector of headers
        std::string _headers = "";
        for (auto& _header : settings.headers)
            _headers += _header + '\n';
        std::size_t _headerLength = _headers.size() * sizeof(std::string::value_type);

        // Create the null terminated array of strings of accepted types
        auto _acceptTypes = std::make_unique<const char*[]>(settings.accept.size() + 1);
        for (std::size_t i = 0ull; i < settings.accept.size(); ++i)
            _acceptTypes[i] = settings.accept[i].c_str();
        _acceptTypes[settings.accept.size()] = nullptr;

        internet _request = HttpOpenRequest(_connected,
            /* method       */ settings.method.c_str(),
            /* resource     */ _url.resource.c_str(),
            /* http version */ settings.version.c_str(),
            /* referrer     */ nullptr,
            /* accept types */ _acceptTypes.get(),
            /* flags        */ INTERNET_FLAG_SECURE,
            /* context      */ 0
        );

        if (!HttpSendRequest(_request,
            /* headers      */ _headers.c_str(),
            /* length       */ _headerLength,
            /* post data    */ settings.data.data(),
            /* data length  */ settings.data.size() * sizeof(std::string::value_type)
        )) return _fail();

        // Get the response
        response _response;

        // Get status code
        DWORD _statusSize = sizeof(DWORD);
        DWORD _status{};
        if (!HttpQueryInfo(_request,
            /* info level   */ HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,
            /* buffer       */ &_status,
            /* length       */ &_statusSize,
            /* header index */ nullptr
        )) return _fail();
        
        _response.status = _status;
        _response.ok = _status >= 200 && _status < 300;

        // Get status text
        DWORD _statusTextSize = 0;
        HttpQueryInfo(_request,
            /* info level   */ HTTP_QUERY_STATUS_TEXT,
            /* buffer       */ nullptr,
            /* length       */ &_statusTextSize,
            /* header index */ nullptr
        );

        auto _statusBuffer = std::make_unique<char[]>(_statusTextSize / sizeof(char));
        if (!HttpQueryInfo(_request,
            /* info level   */ HTTP_QUERY_STATUS_TEXT,
            /* buffer       */ _statusBuffer.get(),
            /* length       */ &_statusTextSize,
            /* header index */ nullptr
        )) return _fail();

        _response.statusText = { _statusBuffer.get(), _statusTextSize };

        // Get the length of the buffer of all headers
        DWORD _length = 0;
        HttpQueryInfo(_request,
            /* info level   */ HTTP_QUERY_RAW_HEADERS,
            /* buffer       */ nullptr,
            /* length       */ &_length,
            /* header index */ nullptr
        );

        auto _buffer = std::make_unique<char[]>(_length / sizeof(char));
        if (!HttpQueryInfo(_request,
            /* info level   */ HTTP_QUERY_RAW_HEADERS,
            /* buffer       */ _buffer.get(),
            /* length       */ &_length,
            /* header index */ nullptr
        )) return _fail();

        // Copy all retrieved headers into the response
        char* _iter = _buffer.get();
        while (true) {
            // Headers are all null terminated, so use std::string's
            // constructor to automatically find that and use it.
            std::string _header{ _iter };
            if (_header.size() == 0) break;
            _iter += _header.size() + 1; // next header (+1 for null terminator)
            std::size_t _split = _header.find_first_of(':');
            // If no ':', can't split to key/value
            if (_split == std::string::npos) continue;
            std::string _key = _header.substr(0, _split);
            std::string _value = _header.substr(_split + 1ull);
            // Remove preceding whitespace
            _value = _value.substr(_value.find_first_not_of(" \t\n\r\t\v"));
            _response.headers[to_lower(_key)] = _value;
        }

        // Read the result body
        char _c[100]{};
        DWORD _read{};
        while (InternetReadFile(_request, &_c, 100, &_read)) {
            if (_read == 0) break;
            _response.body.append(_c, _read);
        }

        promise.set_value(std::move(_response));
    } }.detach();
    return _future;
}

int main() {
    using namespace kaixo;

    response result = await fetch("https://api.kaixo.me/recommend?genre=electro");

    if (!result.ok) return -1;

    json _json = json::parse(result.body);

    return 0;
}

 