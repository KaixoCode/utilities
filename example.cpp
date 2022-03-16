

#include <vector>
#include <iostream>
#include <iomanip>
#include "utils.hpp"

#include <variant>
#include <cassert>

// -- header file --
#include <iostream>
struct override;

template<class = override, class...>
inline auto logger = std::clog;

template<class... Tys, class T>
void log(T t) { logger<override, Tys...> << t; }

// -- cpp file --
#include <fstream>

template<> auto logger<override> = std::ofstream{ "log.txt" };

struct dud {};
template<auto Impl, class A = dud>
struct infix {
    A a;

    template<class Ty>
    constexpr friend auto operator<(Ty&& a, const infix& op) {
        return infix<Impl, Ty>{ std::forward<Ty>(a) };
    }

    template<class Ty> requires (std::invocable<decltype(Impl), A, Ty>)
        constexpr friend auto operator>(infix&& op, Ty&& b)
        -> decltype(Impl(std::forward<A>(op.a), std::forward<Ty>(b))) {
        return Impl(std::forward<A>(op.a), std::forward<Ty>(b));
    }
};

#include <array>
#include <map>

constexpr infix < [](auto& a, auto& b) { return a.contains(b); } > contains;

constexpr infix < []<class Ty>(Ty&& val, auto& container) {
    return std::find(std::begin(container), std::end(container),
        std::forward<Ty>(val)) != std::end(container);
} > in;

constexpr infix < []<class A, class B>(A&& a, B&& b) {
    if constexpr (std::integral<A> && std::integral<B>)
        return a % b;
    else
        return std::fmod(std::forward<A>(a), std::forward<B>(b));
} > mod;


#define wrap(x) []<class ...Tys> requires requires(Tys&&...args) { x(std::forward<Tys>(args)...); } (Tys&&...args)\
    -> decltype(x(std::forward<Tys>(args)...)) { return x(std::forward<Tys>(args)...); }

template<class ...Tys>
struct overloaded : Tys... {
    using Tys::operator()...;
};

template<std::size_t N, std::size_t M>
struct dfa {
    struct state {
        std::pair<char, std::size_t> fun[M];
        bool accept = false;

        constexpr std::size_t transition(char c) const {
            auto res = std::find_if(std::begin(fun), std::end(fun), [=](auto& v) { return v.first == c; });
            return res->second;
        }
    };

    state states[N];
    
    template<std::size_t N>
    constexpr bool accepts(const char(&c)[N]) const {
        auto q = states;
        for (std::size_t i = 0; i < N - 1; i++) {
            q = &states[q->transition(c[i])];
        }
        return q->accept;
    }
};


template<class Ty, std::size_t Size, std::size_t Arity = 2, class Pred = std::less<Ty>>
struct heap {
    using value_type = Ty;
    using size_type = std::size_t;
    using reference = Ty&;
    using const_reference = const Ty&;
    using value_compare = Pred;

    constexpr heap() = default;

    template<std::same_as<Ty> ...Tys>
    constexpr heap(Tys&&...tys) 
        : m_Size(std::min(sizeof...(Tys), Size)) {
        size_type index = 0;
        ((m_Data[index++] = tys), ...);
        for (int64_t i = (m_Size - Arity) / Arity; i >= 0; i--)
            sink(i); // Sink all layers
    }

    template<class ...Tys>
    constexpr Ty& emplace(Tys&& ...vals) {
        assert(m_Size != Size); // Assert not full
        // Put value at end
        m_Data[m_Size] = Ty{ std::forward<Tys>(vals)... };
        auto me = m_Size, pa = (m_Size - 1) / 2;
        while (comp(m_Data[pa], m_Data[me])) { // While parent smaller
            std::swap(m_Data[me], m_Data[pa]); // Swap with parent
            if (pa == 0) break; // If root node, done
            me = pa, pa = (me - 1) / 2; // New parent
        }
        ++m_Size;
        return m_Data[me];
    }

    constexpr void push(Ty&& val) { emplace(std::move(val)); }
    constexpr void push(const Ty& val) { emplace(val); }

    constexpr size_type size() const { return m_Size; }
    constexpr size_type max_size() const { return Size; }
    constexpr size_type arity() const { return Arity; }

    constexpr Ty& top() { return m_Data[0]; }
    constexpr const Ty& top() const { return m_Data[0]; }

    constexpr void pop() {
        m_Data[0] = m_Data[m_Size - 1]; // Move last element to top
        --m_Size; // Decrease size
        sink(0); // Sink the root node down
    }

private:
    size_type m_Size = 0;
    Ty m_Data[Size];
    [[no_unique_address]] Pred comp;

    constexpr void sink(size_type i) {
        size_type max = i; // Find highest value
        for (size_type n = Arity * i + 1; n <= Arity * i + Arity; n++)
            if (n < m_Size and comp(m_Data[n], m_Data[max])) max = n;

        if (max != i) { // If not itself, swap and sink further down
            std::swap(m_Data[i], m_Data[max]);
            sink(max);
        }
    }

    constexpr friend auto& operator<<(auto& a, const heap& v) {
        size_type m = Size, l = 0, k = 1, p = 0;
        for (auto& i : v.m_Data) {
            for (auto j = 0; j < m * (Arity - 1); j++) a << " ";
            a << std::setw(2) << std::setfill(' ') << i, ++p;
            if (p >= v.m_Size) break;
            for (auto j = 0; j < m * (Arity - 1); j++) a << " ";
            if ((++l) % k == 0) l = 0, k *= Arity, a << '\n', m /= Arity;
        }

        return a;
    }
};

template<class ...Tys>
heap(Tys&&...)->heap<std::tuple_element_t<0, std::tuple<Tys...>>, sizeof...(Tys)>;

struct Connection {
    std::size_t vertex = std::numeric_limits<std::size_t>::max();
    double weight = std::numeric_limits<double>::max();
};

template<std::size_t S = 0>
struct Vertex {
    constexpr static auto size = S;
    Connection connections[S];
};

template<> struct Vertex<0> { 
    constexpr static auto size = 0;
    Connection* connections = nullptr;
};

#include <span>

template<class ...Vertices>
struct Graph {
    constexpr static auto size = sizeof...(Vertices);
    constexpr static auto edges = (Vertices::size + ...);

    struct {
    private:
        std::size_t offsets[size + 1]{};
        Connection connections[edges]{};
    public:
        constexpr auto operator[](std::size_t i) const {
            return std::span{ connections + offsets[i], offsets[i + 1] - offsets[i] };
        }
        friend struct Graph;
    } vertices;

    constexpr Graph(Vertices&& ...vs) {
        std::size_t i1 = 0, i2 = 0;
        vertices.offsets[0] = 0;
        ((std::copy_n(vs.connections, vs.size, vertices.connections + i2),
            vertices.offsets[++i1] = i2 + vs.size, i2 += vs.size), ...);
    }

    // Implemented using Dijkstra's Algorithm
    constexpr auto shortest_path(std::size_t a) const {
        constexpr auto infinity = std::numeric_limits<double>::max();
        std::array<double, size> dist{}; // Final distances
        dist.fill(infinity), dist[a] = 0; // itself = 0, rest = infinity

        bool done[size]{}; // Keep track of checked vertices
        std::size_t count = size, u = a, next = u;
        for (auto _ = 0ull; _ < size; ++_, done[u] = true, u = next) {
            double s = infinity;
            for (auto& [v, w] : vertices[u]) {
                if (!done[v] && dist[v] < s) s = dist[v], next = v;
                if (dist[v] > dist[u] + w) dist[v] = dist[u] + w;
            }
        }

        return dist;
    }
};

int main() {

    constexpr Graph graph{
        Vertex{ { { 1,  7. }, { 2,  9. }, { 5, 14. } } },
        Vertex{ { { 0,  7. }, { 2, 10. }, { 3, 15. } } },
        Vertex{ { { 0,  9. }, { 1, 10. }, { 3, 11. }, { 5, 2. } } },
        Vertex{ { { 1, 15. }, { 2, 11. }, { 4,  6. } } },
        Vertex{ { { 3,  6. }, { 5,  9. } } },
        Vertex{ { { 0, 14. }, { 2,  2. }, { 4,  9. } } },
    };

    constexpr auto aeon = sizeof(graph);

    constexpr auto sp = graph.shortest_path(0);

    //using my_parser = parser<
    //    p<"char"> = satisfy < [](std::string_view str, auto c) -> result<char> {
    //        if (str.size() > 0 && str[0] == c) 
    //            return { true, str.substr(1), str[0] };
    //        else return { false };
    //    } >,
    //    p<"word"> = satisfy < [](std::string_view str, std::string_view word) -> result<std::string_view> {
    //        bool matches = str.substr(0, std::size(word)) == word;
    //        if (matches) return { true, str.substr(std::size(word)), str.substr(0, std::size(word)) };
    //        else return { false };
    //    } >,
    //    p<"symbol"> = p<"char">('a') * p<"char">('b') * p<"char">('c') * p<"char">('d') * p<"char">('e')
    //>;
    //    
    //constexpr auto res = my_parser::parse<"symbol">("abcdef");
    //constexpr auto resv = res.value.value;


    constexpr auto h = heap{ 9, 39, 29, 30, 10, 69, 30, 63, 53, 2, 3, 8, 32, 11, 22 };

    std::cout << h;

    constexpr auto m = dfa<3, 2>{ { 
        {.fun{ { '0', 0 }, { '1', 1 } }, .accept = true },
        {.fun{ { '0', 2 }, { '1', 0 } } },
        {.fun{ { '0', 1 }, { '1', 2 } } },
    } };
    constexpr auto ae = m.accepts("1001");


    return 0;
}

