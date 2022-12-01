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


template<class Base>
struct polymorphic_vector {

    struct rtti {
        virtual ~rtti() = default;
        virtual Base* get() = 0;
        virtual void move(void* to) = 0;
    };

    template<std::derived_from<Base> Derived> 
    struct rtti_impl : rtti {
        template<class ...Args>
        rtti_impl(Args&& ...args)
            : value(std::forward<Args>(args)...) {}

        Derived value;

        virtual Base* get() override {
            return dynamic_cast<Base*>(&value);
        }

        virtual void move(void* to) override {
            new (to) rtti_impl<Derived>{ std::move(value) };
        }
    };

    std::vector<std::size_t> accessors;
    std::uint8_t* data = new std::uint8_t[sizeof(Base) * 4];
    std::size_t bytes = sizeof(Base) * 4;
    std::size_t used = 0;

    static_assert(sizeof(rtti) == sizeof(void*));

    polymorphic_vector() = default;
    polymorphic_vector(const polymorphic_vector&) = delete;
    polymorphic_vector(polymorphic_vector&&) = default;
    polymorphic_vector& operator=(const polymorphic_vector&) = delete;
    polymorphic_vector& operator=(polymorphic_vector&&) = default;
    ~polymorphic_vector() {
        // Call all the destructors
        for (auto& accessor : accessors)
            reinterpret_cast<rtti*>(data + accessor)->~rtti();
        delete data;
    }

    std::uint8_t* allocate(std::size_t n) {
        if (bytes - used < n) {
            auto newSize = bytes * 2 + n;
            auto newData = new std::uint8_t[newSize];
            for (auto& accessor : accessors) {
                reinterpret_cast<rtti*>(data + accessor)->move(newData + accessor);
                reinterpret_cast<rtti*>(data + accessor)->~rtti();
            }
            delete data;
            data = newData;
            bytes = newSize;
        }
        std::size_t prev = used;
        used += n;
        return data + prev;
    }

    template<std::derived_from<Base> Derived, class ...Args>
    Derived& emplace_back(Args&&...args) {
        constexpr std::size_t size = sizeof(rtti_impl<Derived>);
        // Insert enough space to construct the object instance of Derived
        auto it = allocate(size);
        // Get the pointer to the memory location where to construct
        std::uint8_t* ptr = &*it;
        // Construct the object and rtti info in place at the memory location
        auto* obj = new (ptr) rtti_impl<Derived>{ std::forward<Args>(args)... };
        // Dynamic cast to the base pointer to calculate offset in bytes from begin
        std::size_t offset = ptr - data;
        // Emplace the offset to the accessor vector
        accessors.emplace_back(offset);
        return obj->value;
    }

    template<std::derived_from<Base> Derived>
    Derived* at(std::size_t index) {
        return dynamic_cast<Derived*>(raw_access(index)->get());
    }

    rtti* raw_access(std::size_t index) {
        return reinterpret_cast<rtti*>(data + accessors[index]);
    }
};

#include <typeindex>
#include "bag.hpp"

using namespace kaixo;

void test_bag() {
    bag _bag{ "hello" };
    bag _bag2{ 1, 3, std::string{ "long ass string"}};
    std::vector _vals{ 3, 4, 7, 9 };

    _bag.assign(std::string{ "carrot" }, 10.0, 100000ull, std::vector<int>{ 1, 2, 3 });
    _bag.insert(_bag.begin(), "Hello World");
    _bag.insert(_bag.begin() + 3, _bag2.begin(), _bag2.end());
    _bag.shrink_to_fit();
    _bag.insert(_bag.begin(), std::array{ 1, 3, 4, 5, 6, 7, 8 });
    _bag.insert(_bag.begin() + 3, _vals.begin(), _vals.end());
    _bag.erase(_bag.begin() + 5, _bag.begin() + 9);

    for (auto& val : _bag) {
        std::cout << val.type().name() << "\n";

        if (auto str = val.cast<std::string>()) {
            std::cout << *str << "\n";
        }
    }

    return;
}


int main() {
    using namespace kaixo;

    test_bag();


    return 0;
}

 