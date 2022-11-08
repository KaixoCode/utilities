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

#include "Json.hpp"

#include <windows.h>
#include <winhttp.h>
#include <wincrypt.h>
#include <schannel.h>
#pragma comment(lib, "winhttp.lib")
#pragma comment(lib, "crypt32.lib")
#pragma comment(lib, "schannel.lib")

struct Buffer {
    template<std::size_t N>
    Buffer(const wchar_t(&str)[N]) {
        size = N * sizeof(wchar_t);
        data = new std::uint8_t[size];
        std::memcpy(data, str, size);
    }
    Buffer(const std::wstring& str) {
        size = str.size() * sizeof(std::wstring::value_type);
        data = new std::uint8_t[size];
        std::memcpy(data, str.data(), size);
    }
    Buffer(const Buffer&) = delete;
    Buffer(Buffer&& o) : data(o.data), size(o.size) { o.data = nullptr, o.size = 0; }
    ~Buffer() { delete[] data; }

    std::uint8_t* data = nullptr;
    std::size_t size = 0;
};

struct FetchSettings {
    std::wstring ua = L"C++";
    std::wstring method = L"GET";
    std::wstring version = L"HTTP/1.1";
    Buffer data = L"";
    std::vector<std::wstring> headers = {};
    INTERNET_PORT port = INTERNET_DEFAULT_HTTPS_PORT;
};

struct Response {
    Response(bool success = false) : success(success) {}
    bool success = false;

    operator bool() { return success; }

    std::vector<std::wstring> headers;
    std::wstring body;
};

struct Internet {
    HINTERNET handle = nullptr;
    operator HINTERNET() const { return handle; }
    Internet(HINTERNET handle) : handle(handle) {}
    Internet(const Internet&) = delete;
    Internet(Internet&& o) : handle(o.handle) { o.handle = nullptr; }
    ~Internet() { if (handle) WinHttpCloseHandle(handle); }
};

template<class Ty, auto Deleter, auto ...Args>
struct Handle {
    Ty handle = nullptr;
    Ty operator->() const { return handle; }
    operator Ty() const { return handle; }
    Handle(Ty handle) : handle(handle) {}
    Handle(const Handle&) = delete;
    Handle(Handle&& o) : handle(o.handle) { o.handle = nullptr; }
    ~Handle() { if (handle) Deleter(handle, Args...); }
};

struct Url {
    std::wstring scheme{};
    std::wstring domain{};
    std::wstring path{};
    std::wstring form{};

    static Url parse(std::wstring_view url) {
        // Url is formatted like this:
        // <scheme>://<domain>/<path>?<form>
        Url result{};
        // Parse scheme
        std::size_t _p1 = url.find_first_of(L':');
        result.scheme = url.substr(0ull, _p1);
        url = url.substr(_p1 + 3ull);
        // Parse domain
        std::size_t _p2 = url.find_first_of(L'/');
        result.domain = url.substr(0ull, _p2);
        if (_p2 == std::wstring_view::npos) return result;
        url = url.substr(_p2);
        // Parse path and form
        std::size_t _p3 = url.find_first_of(L'?');
        result.path = url.substr(0ull, _p3);
        if (_p3 == std::wstring_view::npos) return result;
        result.form = url.substr(_p3 + 1ull);
        return result;
    }
};

std::future<Response> fetch(const std::wstring& url, FetchSettings&& settings = {}) {
    std::promise<Response> _promise;
    std::future<Response> _future = _promise.get_future();
    std::thread{ [url, settings = std::move(settings), promise = std::move(_promise)]() mutable {
        Url _url = Url::parse(url);
        bool _ssl = _url.scheme == L"https";

        auto _fail = [&]() {
            auto _error = GetLastError();
            return promise.set_value(Response{});
        };
        using Internet = Handle<HINTERNET, WinHttpCloseHandle>;

        Internet _opened = WinHttpOpen(
            /* user agent   */ settings.ua.c_str(),
            /* access type  */ WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY,
            /* proxy name   */ WINHTTP_NO_PROXY_NAME,
            /* proxy bypass */ WINHTTP_NO_PROXY_BYPASS,
            /* flags        */ WINHTTP_FLAG_ASYNC
        );
        if (!_opened) return _fail();

        Internet _connected = WinHttpConnect(_opened,
            /* host name    */ _url.domain.c_str(),
            /* port         */ settings.port,
            /* reserved     */ 0
        );
        if (!_connected) return _fail();

        Internet _request = WinHttpOpenRequest(_connected,
            /* method       */ settings.method.c_str(),
            /* resource     */ _url.path.c_str(),
            /* http version */ settings.version.c_str(),
            /* referrer     */ WINHTTP_NO_REFERER,
            /* accept types */ WINHTTP_DEFAULT_ACCEPT_TYPES,
            /* flags        */ 0
        );
        if (!_request) return _fail();

        std::wstring _headers = L"";
        for (auto& _header : settings.headers)
            _headers += _header + L"\r\n";
        std::size_t _headerLength = _headers.size() * sizeof(std::wstring::value_type);

        bool _result = WinHttpSendRequest(_request,
            /* headers      */ _headers.c_str(),
            /* length       */ _headerLength,
            /* post data    */ settings.data.data,
            /* data length  */ settings.data.size,
            /* total size   */ settings.data.size + _headerLength,
            /* context      */ 0
        );
        if (!_result) return _fail();

        // This awaits the request
        _result = WinHttpReceiveResponse(_request, 0);

        // If request requires an SSL certificate
        if (_ssl) {
            using CertStore = Handle<HCERTSTORE, CertCloseStore, 0>;
            using IssuerList = Handle<SecPkgContext_IssuerListInfoEx*, GlobalFree>;
            using CertChain = Handle<PCCERT_CHAIN_CONTEXT, CertFreeCertificateChain>;
            using Cert = Handle<PCERT_CONTEXT, CertFreeCertificateContext>;

            // Open the certificate store
            CertStore _store = CertOpenSystemStore(0, TEXT("MY"));
            if (!_store) return _fail();

            // Query the request for the certificate issuer list
            IssuerList _issuerList = nullptr;
            DWORD _size = sizeof(SecPkgContext_IssuerListInfoEx*);
            if (!WinHttpQueryOption(_request,
                /* option       */ WINHTTP_OPTION_CLIENT_CERT_ISSUER_LIST,
                /* buffer       */ &_issuerList.handle,
                /* size         */ &_size
            )) return _fail();

            // Build search criteria from the issuer list
            CERT_CHAIN_FIND_BY_ISSUER_PARA SrchCriteria;
            ::ZeroMemory(&SrchCriteria, sizeof(CERT_CHAIN_FIND_BY_ISSUER_PARA));
            SrchCriteria.cbSize = sizeof(CERT_CHAIN_FIND_BY_ISSUER_PARA);
            SrchCriteria.cIssuer = _issuerList->cIssuers;
            SrchCriteria.rgIssuer = _issuerList->aIssuers;

            // Find the certificate chain in the store using the search criteria
            CertChain _chain = CertFindChainInStore(_store,
                /* encoding     */ X509_ASN_ENCODING,
                /* flags        */ CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG |
                /*              */ // Do not perform wire download when building chains.
                /*              */ CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG,
                /*              */ // Do not search pCacheEntry->_ClientCertStore 
                /*              */ // for issuer certs.
                /* type         */ CERT_CHAIN_FIND_BY_ISSUER,
                /* criteria     */ &SrchCriteria,
                /* context      */ nullptr
            );
            if (!_chain) return _fail();

            // Set the certificate for the http request
            Cert _cert = (PCERT_CONTEXT)_chain->rgpChain[0]->rgpElement[0]->pCertContext;
            if (!WinHttpSetOption(_request,
                /* option       */ WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
                /* buffer       */ _cert,
                /* size         */ sizeof(CERT_CONTEXT)
            )) return _fail();

            // Resend the request
            _result = WinHttpSendRequest(_request,
                /* headers      */ _headers.c_str(),
                /* length       */ _headerLength,
                /* post data    */ settings.data.data,
                /* data length  */ settings.data.size,
                /* total size   */ settings.data.size + _headerLength,
                /* context      */ 0
            );
            if (!_result) return _fail();

            _result = WinHttpReceiveResponse(_request, 0);
            if (!_result) return _fail();
        }

        Response _response{ true };

        // Get length of headers buffer
        DWORD _length = 0;
        WinHttpQueryHeaders(_request, 
            /* info level   */ WINHTTP_QUERY_RAW_HEADERS,
            /* header name  */ WINHTTP_HEADER_NAME_BY_INDEX, 
            /* buffer       */ WINHTTP_NO_OUTPUT_BUFFER, 
            /* length       */ &_length, 
            /* header index */ WINHTTP_NO_HEADER_INDEX
        );

        wchar_t* _buffer = new wchar_t[_length / sizeof(wchar_t)];
        if (!WinHttpQueryHeaders(_request,
            /* info level   */ WINHTTP_QUERY_RAW_HEADERS,
            /* header name  */ WINHTTP_HEADER_NAME_BY_INDEX,
            /* buffer       */ _buffer,
            /* length       */ &_length,
            /* header index */ WINHTTP_NO_HEADER_INDEX
        )) {
            auto _error = GetLastError();
            return promise.set_value(Response{});
        }

        // Copy all retrieved headers into the response
        wchar_t* _iter = _buffer;
        while (true) {
            std::wstring _header{ _iter };
            if (_header.size() == 0) break;
            _iter += _header.size() + 1;
            _response.headers.push_back(std::move(_header));
        }
        delete[] _buffer;

        // Get content size
        DWORD _size = 0;
        WinHttpQueryDataAvailable(_request, &_size);

        char* _body = new char[_size / sizeof(char)];
        auto _res = WinHttpReadData(_request,
            /* buffer       */ _body,
            /* bytes        */ _size,
            /* bytes read   */ 0
        );

        std::string aaefa{ _body, _size };

        promise.set_value(std::move(_response));
    } }.detach();
    return _future;
}

int main() {
    using namespace kaixo;

    auto result = fetch(L"https://api.kaixo.me/releases");

    auto res = result.get();

    return 0;
}

 