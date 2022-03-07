#include "smart_tuple.hpp"
#include "pa_function.hpp"
#include "lambda.hpp"
#include "struct_tuple.hpp"
#include "type_linker.hpp"
#include "constexpr_counter.hpp"

namespace named_requirements
{
    namespace details
    {
        template<typename U, typename T1, typename T2>
        concept same_as_either = std::is_same_v<T1, U> || std::is_same_v<T2, U>;
    }

    template<typename T>
    concept CopyAssignable = std::is_copy_assignable_v<T>;

    template<typename T>
    concept CopyConstructible = std::is_copy_constructible_v<T>;

    template<typename T, typename A>
    concept CopyInsertable = requires(A m, T * p, T(*v)())
    {
        { std::allocator_traits<A>::construct(m, p, v()) };
    };

    template<typename T>
    concept MoveAssignable = std::is_move_assignable_v<T>;

    template<typename T>
    concept MoveConstructible = std::is_move_constructible_v<T>;

    template<typename T, typename A>
    concept MoveInsertable = requires (A m, T * p, T(*v)())
    {
        { std::allocator_traits<A>::construct(m, p, v()) };
    };

    template<typename T>
    concept DefaultConstructible = std::is_default_constructible_v<T>;

    template<typename T>
    concept Destructible = std::is_destructible_v<T>;

    template<typename T>
    concept EqualityComparable = std::equality_comparable<T>;

    template<typename T, typename U>
    concept EqualityComparableWith = std::equality_comparable_with<T, U>;

    template<typename T, typename A>
    concept Erasable = requires(A m, T * p)
    {
        { std::allocator_traits<A>::destroy(m, p) };
    };

    template<typename T>
    concept TriviallyCopyable = std::is_trivially_copyable<T>::value;

    template<typename T>
    concept TrivialType = std::is_trivial<T>::value;

    template<typename T>
    concept StandardLayoutType = std::is_standard_layout<T>::value;

    template<typename T>
    concept PODType = std::is_pod<T>::value;

    template<typename T>
    concept Swappable = std::swappable<T>;

    template<typename T, typename U>
    concept SwappableWith = std::swappable_with<T, U>;

    template<typename T>
    concept NullablePointer = requires(T p, T q, std::nullptr_t np)
    {
        requires EqualityComparable<T>;
        requires DefaultConstructible<T>;
        requires CopyConstructible<T>;
        requires CopyAssignable<T>;
        requires Destructible<T>;

    { p = np } -> std::same_as<T&>;
    { p != q } -> std::convertible_to<bool>;
    { p == np } -> std::convertible_to<bool>;
    { np == p } -> std::convertible_to<bool>;
    { p != np } -> std::convertible_to<bool>;
    { np != p } -> std::convertible_to<bool>;
    };

    template<typename It>
    concept LegacyIterator = requires(It r)
    {
        requires CopyConstructible<It>;
        requires CopyAssignable<It>;
        requires Destructible<It>;
        requires Swappable<It>;

    typename std::iterator_traits<It>::value_type;
    typename std::iterator_traits<It>::difference_type;
    typename std::iterator_traits<It>::reference;
    typename std::iterator_traits<It>::pointer;
    typename std::iterator_traits<It>::iterator_category;

    { *r };
    { ++r } -> std::same_as<It&>;
    };

    template<typename It>
    concept LegacyInputIterator = requires(It i, It j)
    {
        requires LegacyIterator<It>;
        requires EqualityComparable<It>;

    { i != j } -> std::convertible_to<bool>;
    { *i } -> std::same_as<typename std::iterator_traits<It>::reference>;
    { ++i } -> std::same_as<It&>;
    { *i++ } -> std::convertible_to<typename std::iterator_traits<It>::value_type>;
    };

    template<typename X>
    concept LegacyOutputIterator = requires(X r, typename std::iterator_traits<X>::value_type o)
    {
        requires LegacyIterator<X>;
        requires std::is_class_v<X> || std::is_pointer_v<X>;

    { *r = o };
    { ++r } -> std::same_as<X&>;
    { r++ } -> std::convertible_to<const X&>;
    { *r++ = o };
    };

    template<typename It>
    concept LegacyForwardIterator = requires(It i)
    {
        requires !LegacyOutputIterator<It>
    || (LegacyOutputIterator<It> && std::same_as<typename std::iterator_traits<It>::value_type&, typename std::iterator_traits<It>::reference>);

    { i++ } -> std::same_as<It>;
    { *i++ } -> std::same_as<typename std::iterator_traits<It>::reference>;
    };

    template<typename It>
    concept LegacyBidirectionalIterator = requires(It a)
    {
        requires LegacyForwardIterator<It>;

    { --a } -> std::same_as<It&>;
    { a-- } -> std::convertible_to<const It&>;
    { *a-- } -> std::same_as<typename std::iterator_traits<It>::reference>;
    };

    template<typename It>
    concept LegacyContiguousIterator = requires(It a, std::ptrdiff_t n)
    {
        requires LegacyIterator<It>;

    { a + n } -> std::same_as<It>;
    { *(a + n) } -> std::same_as<typename std::iterator_traits<It>::reference>;
    };

    template<typename It>
    concept LegacyRandomAccessIterator = requires(It i, It a, It b, It & r, typename std::iterator_traits<It>::difference_type n)
    {
        requires LegacyBidirectionalIterator<It>;

    { r += n } -> std::same_as<It&>;
    { a + n } -> std::same_as<It>;
    { n + a } -> std::same_as<It>;
    { r -= n } -> std::same_as<It&>;
    { i - n } -> std::same_as<It>;
    { b - a } -> std::same_as<typename std::iterator_traits<It>::difference_type>;
    { i[n] } -> std::convertible_to<typename std::iterator_traits<It>::reference>;
    { a < b } -> std::convertible_to<bool>;
    { a > b } -> std::convertible_to<bool>;
    { a >= b } -> std::convertible_to<bool>;
    { a <= b } -> std::convertible_to<bool>;
    };

    template<typename A>
    concept Allocator = requires(A a, A a1, typename A::value_type * p, const typename A::value_type * cp,
        typename A::value_type r, typename std::allocator_traits<A>::size_type n)
    {
        { *p } -> std::same_as<typename A::value_type&>;
        { *cp } -> std::same_as<const typename A::value_type&>;

        { std::pointer_traits<typename A::value_type*>::pointer_to(r) };
        { a.allocate(n) } -> std::same_as<typename A::value_type*>;
        { a.deallocate(p, n) };

        { a == a1 } -> std::same_as<bool>;
        { a != a1 } -> std::same_as<bool>;

        { A(a) } noexcept;
        { A(std::move(a)) } noexcept;
    };

    template<typename It>
    concept ConstexprIterator = requires(It i)
    {
        std::is_constant_evaluated();
    };

    template<typename C>
    concept Container = requires(C a, C b, C && rv)
    {
        requires DefaultConstructible<C>;
        requires CopyConstructible<C>;
        requires EqualityComparable<C>;
        requires Swappable<C>;

        requires CopyInsertable<typename C::value_type, std::allocator<typename C::value_type>>;
        requires Destructible<typename C::value_type>;
        requires Erasable<typename C::value_type, std::allocator<typename C::value_type>>;

    typename C::reference;
    typename C::const_reference;
    typename C::const_iterator;
        requires LegacyForwardIterator<typename C::iterator>&& std::convertible_to<typename C::iterator, typename C::const_iterator>;
        requires std::signed_integral<typename C::difference_type>;
        requires std::unsigned_integral<typename C::size_type>;

    { C() } -> std::same_as<C>;
    { C(a) } -> std::same_as<C>;
    { C(rv) } -> std::same_as<C>;
    { a = b } -> std::same_as<C&>;
    { a = rv } -> std::same_as<C&>;
    { a.~C() } -> std::same_as<void>;
    { a.begin() } -> details::same_as_either<typename C::iterator, typename C::const_iterator>;
    { a.end() } -> details::same_as_either<typename C::iterator, typename C::const_iterator>;
    { a.cbegin() } -> std::same_as<typename C::const_iterator>;
    { a.cend() } -> std::same_as<typename C::const_iterator>;
    { a == b } -> std::convertible_to<bool>;
    { a != b } -> std::convertible_to<bool>;
    { a.swap(b) } -> std::same_as<void>;
    { std::swap(a, b) } -> std::same_as<void>;
    { a.size() } -> std::same_as<typename C::size_type>;
    { a.max_size() } -> std::same_as<typename C::size_type>;
    { a.empty() } -> std::convertible_to<bool>;
    };

    template<typename C>
    concept ReversibleContainer = requires(C a)
    {
        requires LegacyBidirectionalIterator<typename C::iterator>;
        requires LegacyBidirectionalIterator<typename C::const_iterator>;
        requires std::same_as<typename C::reverse_iterator, std::reverse_iterator<typename C::iterator>>;
        requires std::same_as<typename C::const_reverse_iterator, std::reverse_iterator<typename C::const_iterator>>;

    { a.rbegin() } -> std::same_as<typename C::reverse_iterator>;
    { a.rend() } -> std::same_as<typename C::reverse_iterator>;
    { a.crbegin() } -> std::same_as<typename C::const_reverse_iterator>;
    { a.crend() } -> std::same_as<typename C::const_reverse_iterator>;
    };

    template<typename X, typename A = typename X::allocator_type>
    concept AllocatorAwareContainer = requires(X a, X b, X & t, X && rv, A m)
    {
        requires std::same_as<typename A::value_type, typename X::value_type>;
        requires DefaultConstructible<A>;
        requires CopyInsertable<typename X::value_type, X>;
        requires MoveInsertable<typename X::value_type, X>;

    { a.get_allocator() } -> std::same_as<A>;
    { X() } -> std::same_as<X>;
    { X(m) } -> std::same_as<X>;
    { X(t, m) } -> std::same_as<X>;
    { X(rv) } -> std::same_as<X>;
    { A(static_cast<A&&>(m)) } noexcept -> std::same_as<A>;
    { X(rv, m) } -> std::same_as<X>;
    { a = t } -> std::same_as<X&>;
    { a = rv } -> std::same_as<X&>;
    { a.swap(b) } -> std::same_as<void>;
    };
}


struct A
{
private:
    A() {};
    A(A&&) {};
    A(const A&) {};
};

#include <vector>
#include <intrin.h>

class bowl
{
public:
    using value_type = int;
    using reference = value_type&;
    using const_reference = const value_type&;
    using pointer = value_type*;
    using const_pointer = const value_type*;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;

    bowl() {
        add_bucket();
    }

    bowl(const bowl& other) {
        *this = other;
    }

    bowl(bowl&& other) {
        *this = std::move(other);
    }

    bowl& operator=(const bowl& other) {
        m_Buckets = std::move(other.m_Buckets);
        m_Size = std::move(other.m_Size);
        m_Index = std::move(other.m_Index);
        return *this;
    }

    bowl& operator=(bowl&& other) {
        m_Buckets = std::move(other.m_Buckets);
        m_Size = std::move(other.m_Size);
        m_Index = std::move(other.m_Index);
        other.m_Buckets = nullptr;
        other.m_Size = 0;
        other.m_Index = 0;
        return *this;
    }

    void add_bucket() {
        bucket** _buckets = m_Buckets; // Backup of array
        m_Buckets = new bucket * [++m_Size]; // New array
        for (size_type i = 0; i < m_Size - 1; i++)
            m_Buckets[i] = _buckets[i]; // Move buckets over
        m_Buckets[m_Size - 1] = new bucket; // Add new bucket
        delete[] _buckets; // Delete old array of buckets
    }

    void clean() {
        if (m_Buckets) { // If buckets
            for (size_type i = 0; i < m_Size; i++)
                delete m_Buckets[i]; // Delete buckets
            delete[] m_Buckets; // And delete array
        }
    }

    size_type size() const {
        size_type _size = 0;
        for (size_type i = 0; i < m_Size; i++)
            _size += m_Buckets[i]->size();
        return _size;
    }

    size_type max_size() const {
        return static_cast<size_type>(-1);
    }

    bool empty() const {
        return size() == 0;
    }

    void swap(bowl& other) {
        std::swap(other.m_Buckets, m_Buckets);
        std::swap(other.m_Index, m_Index);
        std::swap(other.m_Size, m_Size);
    }

    class bucket;
    template<bool Const>
    class iterator_type {
    public:
        using type = std::conditional_t<Const, bowl::bucket* const*, bowl::bucket**>;
        using value_type = typename bowl::value_type;
        using reference = std::conditional_t<Const, bowl::reference, bowl::const_reference>;
        using difference_type = std::ptrdiff_t;
        using pointer = std::conditional_t<Const, bowl::pointer, bowl::const_pointer>;

        iterator_type() {}
        iterator_type(type a, size_type b, size_type i) : m_Buckets(a), m_Bucket(b), m_Index(i) {}
        iterator_type(const iterator_type<false>& other) : m_Buckets(other.m_Buckets), m_Bucket(other.m_Bucket), m_Index(other.m_Index) {}

        iterator_type& operator++() {
            m_Index++;
            if (m_Index >= (m_Buckets[m_Bucket])->size())
                m_Bucket++, m_Index = 0;
            return *this;
        }

        iterator_type operator++(int) {
            iterator_type _backup = *this;
            m_Index++;
            if (m_Index >= (m_Buckets[m_Bucket])->size())
                m_Bucket++, m_Index = 0;
            return _backup;
        }

        reference operator*() { return (m_Buckets[m_Bucket])->operator[](m_Index); }
        bool operator==(const iterator_type& other) const { return other.m_Bucket == m_Bucket && other.m_Index == m_Index; }
        bool operator!=(const iterator_type& other) const { return !(other == *this); }

    private:
        type m_Buckets = nullptr;
        size_type m_Index = 0;
        size_type m_Bucket = 0;

        friend class bowl;
    };

    using iterator = iterator_type<false>;
    using const_iterator = iterator_type<true>;

    class bucket {
        friend class bowl;
        friend class bowl::iterator;
        friend class bowl::const_iterator;

        value_type* m_Data[64];
        size_type m_Indices = 0xFFFFFFFFFFFFFFFF;

        ~bucket() {
            const auto _inverted = ~m_Indices;
            for (size_type i = 0; i < 64; i++)
                if (_inverted & (1ULL << i))
                    delete m_Data[i];
        }

        void insert(value_type&& value) {
            const size_type _index = first_available_index();
            m_Indices &= ~(1ULL << _index);
            m_Data[_index] = new value_type{ std::move(value) };
        }

        void erase(size_type index) {
            m_Indices |= 1ULL << index;
            delete m_Data[index];
        }

        value_type& operator[](size_type index) {
            const size_type _index = nth_index(index);
            return *m_Data[_index];
        }

        constexpr size_type size() const { return 64ull - std::popcount(m_Indices); }
        constexpr bool full() const { return std::popcount(m_Indices) == 0; }
        constexpr bool empty() const { return std::popcount(m_Indices) == 64; }

        constexpr size_type first_available_index() const { return std::countr_zero(m_Indices); }
        size_type nth_index(size_type n) const { return std::countr_zero(_pdep_u64(1ULL << n, ~m_Indices)); }
    } **m_Buckets = nullptr;

    iterator begin() { return { m_Buckets, 0ull, 0ull }; }
    iterator end() { return { m_Buckets, m_Size, 0ull }; }
    const_iterator begin() const { return { m_Buckets, 0ull, 0ull }; }
    const_iterator end() const { return { m_Buckets, m_Size, 0ull }; }
    const_iterator cbegin() const { return { m_Buckets, 0ull, 0ull }; }
    const_iterator cend() const { return { m_Buckets, m_Size, 0ull }; }

    void insert(value_type&& value) {
        m_Buckets[m_Index]->insert(std::move(value));
        while (m_Buckets[m_Index]->full())
            if (++m_Index >= m_Size)
                add_bucket();
    }

    void erase(const iterator& it) {
        (m_Buckets[it.m_Bucket])->erase(it.m_Index);
        m_Index = it.m_Bucket;
    }

    bool operator==(const bowl& other) const {
        if (size() != other.size()) return false;
        for (const_iterator _b1 = begin(), _b2 = other.begin(); _b1 != end(); ++_b1, ++_b2)
            if (*_b1 != *_b2) return false;
        return true;
    }

    bool operator !=(const bowl& other) const {
        return !(other == *this);
    }


    size_type m_Index = 0;
    size_type m_Size = 0;
};


void PositiveDivisors(int n, std::vector<int>& divs)
{
    divs.push_back(1);
    for (int i = 2; i < n / 2 + 1; i++)
        if (n % i == 0) divs.push_back(i);
    divs.push_back(n);
}

#include <array>




/* y       n
 x + + + + + + + +
   + 0 1 1 1 1 1 1 \
   + - 0 1 1 1 1 1  |
   + - - 0 1 1 1 1  |
 n + - - - 0 1 1 1  }- k=(n-1)*n/2
   + - - - - 0 1 1  |  i=max(x,y)*n+min(x,y)
   + - - - - - 0 1  |
   + - - - - - - 0 /
 */
class graph_data {
public:
    using size_type = std::size_t;
    using value_type = bool;

    void resize(std::size_t n) {
        const std::size_t _k = std::ceil(0.125 * (n - 1) * n / 2); // Bytes
        if (_k < n) {
            uint8_t* _newData = new uint8_t[_k];
            for (size_type i = 0; i < m_Size; i++)
                _newData[i] = m_Data[i];
        }
    }

    size_type size() const { return m_Size; }

private:

    std::pair<size_type, size_type> to_pos(size_type index) {
        return { index / size(), index % size() };
    }

    size_type to_index(const std::pair<size_type, size_type>& index) {
        return std::max(index.first, index.second) * size() + std::min(index.first, index.second);
    }

    size_t m_Size;
    uint8_t* m_Data;
};

#include <ranges>
namespace g1
{
    //    1 2 3 4 5
    //  1 . . . . .
    //  2 . . . . .
    //  3 . . . . .
    //  4 . . . . .
    //  5 . . . . . 
    template<class Ty>
    class graph {
    public:
        using value_type = Ty;
        using size_type = std::size_t;

        class vertex {
        public:
            template<std::constructible_from<Ty> ...Args>
            vertex(graph& graph, Args&&...args)
                : graph(graph), m_Value(new value_type{ std::forward<Args>(args)... }) {}

            vertex(vertex&& other)
                : graph(other.graph), m_Value(other.value()) {
                other.m_Value = nullptr;
            }

            vertex(const vertex& other)
                : graph(other.graph), m_Value(new value_type{ *other.m_Value }) {
            }

            ~vertex() { delete m_Value; }

            void connect(size_type other) { graph.connect(graph.index_of(*this), other); }
            void connect(vertex& other) { graph.connect(*this, other); }
            void disconnect(size_type other) { graph.disconnect(graph.index_of(*this), other); }
            void disconnect(vertex& other) { graph.disconnect(*this, other); }

            bool adjacent(size_type index) const { return graph.adjacent(graph.index_of(*this), index); }

            auto neighbourhood() const {
                using namespace std::views;
                return iota(0ull, graph.size())
                    | filter([&](size_t index) { return adjacent(index); })
                    | transform([&](size_type index) { return graph.m_Vertices[index]; });
            }

            size_type degree() const { return graph.degree(*this); };
            value_type& value() { return *m_Value; }

            graph& graph;

        private:
            value_type* m_Value = nullptr;
            friend class graph;
        };

        void disconnect(size_type a, size_type b) { m_BitMap.at(a * size() + b) = false; m_BitMap.at(a * size() + b) = false; }
        void disconnect(vertex& a, vertex& b) { connect(index_of(a), index_of(b)); }
        void connect(size_type a, size_type b) { m_BitMap.at(a * size() + b) = true; m_BitMap.at(a + b * size()) = true; }
        void connect(vertex& a, vertex& b) { connect(index_of(a), index_of(b)); }
        bool adjacent(size_type a, size_type b) const { return m_BitMap.at(a * size() + b); }
        bool adjacent(vertex& a, vertex& b) const { return adjacent(index_of(a), index_of(b)); }

        size_type degree(size_type a) const {
            size_type _degree = 0;
            for (size_type i = 0; i < size(); i++)
                if (i != a && m_BitMap.at(a * size() + i)) _degree++;
            return _degree;
        }
        size_type degree(vertex const& a) const { return degree(index_of(a)); }

        size_type index_of(const vertex& a) const { return std::distance(m_Vertices.data(), &a); }
        size_type size() const { return m_Vertices.size(); }

        std::vector<vertex> const& vertices() const { return m_Vertices; }

        template<class ...Args>
        vertex& emplace(Args&&...args) { return m_Vertices.emplace_back(*this, std::forward<Args>(args)...); }



    private:
        std::vector<vertex> m_Vertices;
        std::vector<bool> m_BitMap;
        friend class vertex;
    };
}

#include <bitset>

template<class Ty, std::size_t N>
class graph {
public:
    using value_type = Ty;
    using size_type = std::size_t;

    consteval static auto createEdges(auto es) {
        std::bitset<N* N> edges;
        for (auto& i : es)
            edges.set(i.first * N + i.second), edges.set(i.first + i.second * N);
        return edges;
    }

    consteval graph(const Ty(&vs)[N], std::initializer_list<std::pair<size_type, size_type>> es)
        : vertices(), edges(createEdges(es)) {
        for (size_type i = 0; i < N; i++)
            vertices[i] = vs[i];
    }

    constexpr graph(std::array<Ty, N> vs)
        : vertices(vs), edges()
    {}

    std::bitset<N* N> edges;
    std::array<Ty, N> vertices;
};

template<class Ty, size_t N>
graph(std::array<Ty, N>)->graph<Ty, N>;

namespace woof {
    struct IpAddress {
        template<size_t N>
        consteval IpAddress(const char(&val)[N]) : bytes() {
            size_t byte = 0;
            int curByte = 0;
            for (auto& i : val) {
                if (i == '\0')
                    break;

                if (i == '.' || i == '\'') {
                    bytes[byte] = curByte;
                    curByte = 0; ++byte;
                    continue;
                }

                curByte = curByte * 10 + (i - '0');
                if (curByte > 255) throw std::overflow_error("Byte overflow!");
            }
            bytes[byte] = curByte;
        }

        uint8_t bytes[4];
    };


    template<char... Cs>
    consteval IpAddress operator""_ipv4() { return IpAddress{ { Cs... } }; }

    template<std::size_t Size>
    struct name_object {
        static constexpr std::size_t size = Size - 1;
        char data[Size - 1]{};

        constexpr explicit(false) name_object(const char(&str)[Size]) {
            std::copy_n(str, Size - 1, data);
        }

        constexpr explicit(false) operator std::string_view() const {
            return { data, Size - 1 };
        }
    };

    template<std::size_t Size>
    name_object(const char(&)[Size])->name_object<Size>;

    template<name_object Name, class T>
    struct arg {
        using type = T;

        constexpr static std::string_view name = Name;

        constexpr arg()
            : value() {}

        constexpr arg(T&& arg)
            : value(std::move(arg)) {}

        constexpr arg(const T& arg)
            : value(arg) {}

        constexpr arg(const arg& other)
            : value(other.value) {}

        constexpr arg(arg&& other)
            : value(std::move(other.value)) {}

        T value;
    };

    template<name_object Name>
    struct arg<Name, void> {
        using type = void;

        constexpr static std::string_view name = Name;

        constexpr arg() {}

        template<class Ty>
        constexpr arg<Name, std::decay_t<Ty>> operator=(Ty&& arg) const {
            return { std::forward<Ty>(arg) };
        }

        template<class Ty>
        constexpr explicit(false) operator arg<Name, Ty>() const { return { }; }
    };

    //template<name_object Name>
    //consteval arg<Name, void> operator""_t() { return {}; }

    template<name_object Name, class Arg, class ...Args>
    struct type_finder {
        using type = std::conditional_t<Arg::name == Name, typename Arg::type,
            typename type_finder<Name, Args...>::type>;
    };

    template<name_object Name, class Arg>
    struct type_finder<Name, Arg> {
        using type = std::conditional_t<Arg::name == Name, typename Arg::type, void>;
    };

    template<class ...Args>
    struct object {
        std::tuple<Args...> data;

        constexpr object(Args&&...args)
            : data(args...) {}

        template<name_object Name>
        constexpr auto& get() const {
            return std::get<arg<Name, typename type_finder<Name, Args...>::type>>(data).value;
        }

        template<name_object Name>
        constexpr auto& operator[](arg<Name, void>) const {
            return std::get<arg<Name, typename type_finder<Name, Args...>::type>>(data).value;
        }
        template<name_object Name>
        constexpr auto& get() {
            return std::get<arg<Name, typename type_finder<Name, Args...>::type>>(data).value;
        }

        template<name_object Name>
        constexpr auto& operator[](arg<Name, void>) {
            return std::get<arg<Name, typename type_finder<Name, Args...>::type>>(data).value;
        }
    };

    struct Email {
        std::string email;
        Email(auto e) : email(e) {}
        void operator=(auto e) { email = e; }
    };
}

//namespace aeofamef
//{
//#include "meta_struct.hpp"
//
//    using namespace kaixo;
//
//    using has_carrot = meta_struct<
//        field<"carrot", int&>
//    >;
//
//    using has_soup = meta_struct<
//        field<"soup", float&>
//    >;
//
//    constexpr auto& get_carrot(has_carrot t) { return t.get<"carrot">(); }
//    constexpr auto& get_soup(has_soup t) { return t.get<"soup">(); }
//
//    using has_carrot_soup = meta_struct<
//        field<"carrot", int&>,
//        field<"soup", float&>
//    >;
//
//    constexpr auto get_carrot_plus_soup(has_carrot_soup t) { return t.get<"carrot">() + t.get<"soup">(); }
//    constexpr auto add_carrot_to_soup(has_carrot_soup t) { t.get<"soup">() += t.get<"carrot">(); }
//
//
//
//    using has_get = meta_struct<
//        virtual_function<"get", double()>
//    >;
//
//    constexpr auto call_get(has_get t) { return t.run<"get">(); }
//
//    using carrot_soup_bowl = meta_struct <
//        field<"carrot", int, required>,
//        field<"soup", float, required>,
//        field < "bowl", double, [](auto& self) { return self.get<"soup">() + 100; } > ,
//        function < "addToBowl", [](auto& self, double amount) { self.get<"bowl">() += amount; } > ,
//        function < "get", [](auto& self) -> auto& { return self.get<"bowl">(); } >
//    > ;
//
//    using carrot_has_get = meta_struct<
//        field<"carrot", int>,
//        virtual_function<"get", double()>
//    >;
//
//    constexpr auto carrot_plus_call_get(carrot_has_get t) { return t.run<"get">() + t.get<"carrot">(); }
//
//    using has_add_to_bowl = meta_struct<
//        virtual_function<"addToBowl", void(double)>
//    >;
//
//    constexpr auto add_2_to_bowl(has_add_to_bowl t) { return t.run<"addToBowl">(2); }
//
//
//    using transform = meta_struct <
//        field<"value", double>,
//        virtual_function<"transform", double(double)>
//    >;
//
//    using my_transform = meta_struct <
//        field<"mult", double>,
//        function < "transform", [](auto& self, double v) { return v * self.get<"mult">(); } >
//    > ;
//
//    constexpr auto transform_value(transform t) {
//        return t.run<"transform">((double)t.get<"value">());
//    }
//}
namespace kaixo
{
    template<class ...Args>
    struct constructor {};
    template<class Ty, class ...Constructors>
    concept link_constructors = kaixo::link_types<kaixo::type_group<Constructors...>, kaixo::type_group<Ty>>;

    template<class Ty>
    struct try_parse {
        static inline Ty parse(std::string_view& args);
    };

    template<class Ty> requires (std::floating_point<Ty> || std::integral<Ty>)
        struct try_parse<Ty> {
        static inline Ty parse(std::string_view& args) {
            args = args.substr(args.find_first_not_of(" \t"));
            std::size_t length;
            Ty val = 0;
            auto res = std::from_chars(args.data(), args.data() + args.size(), val);
            if (res.ec == std::errc()) {
                args = { res.ptr, args.size() - (res.ptr - args.data()) };
                return val;
            }
            else
                throw nullptr;
        }
    };

    template<class, class > struct decompose_constructor;
    template<class Ty, class ...Args>
    struct decompose_constructor<Ty, constructor<Args...>> {
        static inline bool try_construct(Ty** assign, std::string_view& args) {
            try {
                *assign = new Ty{ try_parse<Args>::parse(args)... };
                return true;
            }
            catch (...) { return false; }
        }
    };

    template<class, class> struct decompose_type_group;
    template<class Ty, class ...Cs>
    struct decompose_type_group<Ty, type_group<Cs...>> {
        static inline Ty try_construct(std::string_view args) {
            Ty* val = nullptr;
            (decompose_constructor<Ty, Cs>::try_construct(&val, args) || ...);
            if (val == nullptr) throw nullptr;
            Ty a = *val;
            delete val;
            return a;
        }
    };

    template<class> struct decompose_tuple;
    template<class ...Tys>
    struct decompose_tuple<std::tuple<Tys...>> {
        using type = constructor<Tys...>;
    };

    template<class Ty>
    Ty construct(const std::string& args) {
        static_assert(link_constructors<Ty, decompose_tuple<struct_info<Ty>::field_types>::type>);
        return decompose_type_group<Ty, linked_types<Ty>>::try_construct(args);
    }
}

using namespace kaixo;

int Add(int a, int b) { return a + b; }
struct Object { int Add(int a, int b) { return a + b; } };

struct Functor
{
    int someValue;
    int operator()(int a, int b) { return a + b + someValue; }
};

void pa_function_example()
{
    // Functor
    pa_function<int(int, int)> functor = Functor{ 5 };
    pa_function<int(int)> functor2 = functor(1);
    int functorResult = functor2(3);
    assert(functorResult == 9);

    // Lambda
    pa_function<int(int, int)> lambda = [](int a, int b) { return a + b; };
    pa_function<int(int)> lambda2 = lambda(1);
    int lambdaResult = lambda2(3);
    assert(lambdaResult == 4);

    // Function Pointer
    pa_function<int(int, int)> funPtr = Add;
    pa_function<int(int)> funPtr2 = funPtr(1);
    int funPtrResult = funPtr2(3);
    assert(funPtrResult == 4);

    // Member function
    Object obj;
    pa_function<int(int, int)> memberFun = { &Object::Add, obj };
    pa_function<int(int)> memberFun2 = memberFun(1);
    int memberFunResult = memberFun2(3);
    assert(memberFunResult == 4);
}

void smart_tuple_example()
{
    smart_tuple<int, float, double> thing{ 1, 0.5f, 4. };

    // By value
    int val1 = thing[0];
    float val2 = thing[1];
    double val3 = thing[2];
    assert(val1 == 1);
    assert(val2 == 0.5f);
    assert(val3 == 4.);

    // By reference
    int& val4 = thing[0];
    val4++;
    int val5 = thing[0];
    assert(val4 == val5);
}

void lambda_example()
{
    //int a = 0;
    //double b = 1;
    //
    //lambda t = [=](int c) -> int { return a + b + c; };
    //
    //std::cout << typeid(decltype(t)).name() << std::endl;
    //
    //t.get<0>() += 10;
    //t.get<1>() += 10;
    //
    //std::cout << t(1) << std::endl;
}

class Thing
{
public:
    Thing(int a, short b, long long c, double d, float e) {}
};

struct Apple {
    short q = 1;
    long long d = 2;
    int a = 3;
    double b = 4;
    float c = 5;
};

void struct_tuple_example()
{
    //Apple apple;
    //
    //constexpr size_t fields = struct_info<Apple>::fields;
    //using tuple = struct_info<Apple>::field_types;
    //std::cout << fields << std::endl;
    //std::cout << typeid(tuple).name() << std::endl;
    //
    //tuple _tuple = as_tuple(apple);
    //std::cout << typeid(decltype(_tuple)).name() << std::endl;
    //std::cout << std::get<0>(_tuple) << std::endl;
    //std::cout << std::get<1>(_tuple) << std::endl;
    //std::cout << std::get<2>(_tuple) << std::endl;
    //std::cout << std::get<3>(_tuple) << std::endl;
    //std::cout << std::get<4>(_tuple) << std::endl;
    //
    //constexpr size_t args = constructor_info<Thing>::args;
    //std::cout << args << std::endl;
    //std::cout << typeid(constructor_info<Thing>::arg_types).name() << std::endl;
}


void type_linker_example()
{
    //link_types<type_group<double, double>, type_group<int, short>>;
    //link_types<type_group<short, long>, type_group<char, float>>;
    //
    //using type1 = linked_types<int, short>;
    //using type2 = linked_types<char, float>;
    //
    //std::cout << typeid(type1).name() << std::endl;
    //std::cout << typeid(type2).name() << std::endl;
}

void constexpr_counter_example()
{
    //number<counter(0)::get()> nmr1;
    //number<counter(0)::get()> nmr2;
    //number<counter(0)::get()> nmr3;
    //number<counter(0)::get()> nmr4;
    //number<counter(0)::get()> nmr5;
    //
    //std::cout << typeid(nmr1).name() << std::endl;
    //std::cout << typeid(nmr2).name() << std::endl;
    //std::cout << typeid(nmr3).name() << std::endl;
    //std::cout << typeid(nmr4).name() << std::endl;
    //std::cout << typeid(nmr5).name() << std::endl;
    //
    //number<counter(1)::get()> nmr12;
    //number<counter(1)::get()> nmr22;
    //number<counter(1)::get()> nmr32;
    //number<counter(1)::get()> nmr42;
    //number<counter(1)::get()> nmr52;
    //
    //std::cout << typeid(nmr12).name() << std::endl;
    //std::cout << typeid(nmr22).name() << std::endl;
    //std::cout << typeid(nmr32).name() << std::endl;
    //std::cout << typeid(nmr42).name() << std::endl;
    //std::cout << typeid(nmr52).name() << std::endl;

}

//template<class O>
//struct base_converter
//{
//    template<class T> requires (!std::same_as<O, T> && (link_types<type_group<T>, type_group<O>>, true))
//    operator T();
//
//    //template<class T> requires ((link_types<type_group<T>, type_group<O>>, true))
//    //operator T();
//};
//
//template<class T>
//concept get_base = requires() {
//    new T{ base_converter<T>{} };
//};
//
//template<class T> requires (get_base<T>, true)
//using base_type = nth_type_of_t<0, linked_types<T>>;
//
//template<class>
//struct Pointer;
//
//template<class T> requires (std::same_as<base_type<T>, T>)
//struct Pointer<T> {
//    using base = base_type<T>;
//    T* ptr;
//};
//
//template<class T> requires (!std::same_as<base_type<T>, T>)
//struct Pointer<T> : public Pointer<base_type<T>> {
//    using base = Pointer<base_type<T>>;
//    T* ptr;
//};
//
//
//struct Base {
//};
//
//struct Derived : Base {
//};
//

// pe 1
size_t sum_of_multiples(size_t num, size_t max) {
    return ((std::ceil(max / (double)num) * num) / 2. * std::floor((max - 1.) / num));
}

size_t sum_of_all_multiples(size_t a, size_t b, size_t max) {
    return sum_of_multiples(a, max) + sum_of_multiples(b, max) - sum_of_multiples(a * b, max);
}

void solution1() {
    std::cout << sum_of_all_multiples(3, 5, 1000) << std::endl;
}

// pe 2
template<size_t T> constexpr size_t fib = fib<T - 1> +fib<T - 2>;
template<> constexpr size_t fib<0> = 1;
template<> constexpr size_t fib<1> = 2;

template<size_t T> constexpr bool even = !(fib<T> % 2);

template<size_t T, size_t B>
struct fib_below : std::conditional_t<(fib<T> <= B),
    fib_below<T + 1, B>, std::integral_constant<size_t, T>> {};
template<size_t T> constexpr size_t fib_below_v = fib_below<0, T>::value;

template<size_t ...Is>
constexpr size_t sum_of_even_fibs(std::index_sequence<Is...>) {
    return ((even<Is> ? fib<Is> : 0) + ...);
}

template<size_t T> constexpr size_t sum_of_even_fib_below =
sum_of_even_fibs(std::make_index_sequence<fib_below_v<T>>{});

void solution2() {
    std::cout << sum_of_even_fib_below<4000000> << std::endl;
}

// pe 3

#include <vector>

auto get_prime_factors(size_t number) {
    size_t n_primes = number / 2;
    std::vector<std::pair<size_t, size_t>> primes;
    primes.reserve(n_primes);

    auto is_prime = [&](size_t v) {
        for (auto& prime : primes) {
            if (v % prime.first == 0)
                return false;
        }
        return true;
    };

    primes.push_back({ 2, 0 });
    size_t new_prime = 1;
    while (primes.size() < n_primes) {
        while (!is_prime((new_prime += 2)));
        primes.push_back({ new_prime, 0 });
        if (new_prime > n_primes)
            break;
        new_prime = primes.back().first;
    }

    if (is_prime(number))
        primes.push_back({ number, 0 });

    while (number != 1) {
        for (auto& prime : primes) {
            while (number % prime.first == 0) {
                number /= prime.first;
                prime.second++;
            }
        }
    }

    std::vector<std::pair<size_t, size_t>> factors;
    for (auto& prime : primes) {
        if (prime.second)
            factors.push_back(prime);
    }

    return factors;
}

void solution3() {
    size_t number = 600851475143;
    auto factors = get_prime_factors(number);
    bool first = true;
    std::cout << number << " = ";
    for (auto& prime : factors) {
        if (!first)
            std::cout << " + ";
        first = false;
        std::cout << prime.first << "^" << prime.second;
    }
}



// pe 4

auto to_vector(size_t number) {
    std::vector<char> numbers;
    do {
        numbers.push_back(number % 10);
    } while ((number /= 10) > 0);
    return numbers;
}

bool is_palindromic(size_t number) {
    auto nmrs = to_vector(number);

    size_t size = nmrs.size();
    for (int i = 0; i < size / 2; i++)
        if (nmrs[i] != nmrs[size - i - 1])
            return false;

    return true;
}

void solution4() {
    size_t biggest = 0;
    for (size_t a = 100; a < 1000; a++) {
        for (size_t b = 100; b < 1000; b++) {
            size_t product = a * b;
            if (product > biggest && is_palindromic(product))
                biggest = product;
        }
    }

    std::cout << biggest;
}


// pe 699



#include <set>

size_t sigma(size_t number) {
    auto factors = get_prime_factors(number);

    std::vector<size_t> divisors;
    std::vector<size_t> nmrs;
    for (auto& i : factors)
        nmrs.push_back(0);

    bool done = false;
    while (!done) {

        size_t product = 1;
        for (int i = 0; i < factors.size(); i++) {
            product *= std::pow(factors[i].first, nmrs[i]);
        }

        divisors.push_back(product);

        for (int i = 0; i < factors.size(); i++) {
            nmrs[i]++;
            if (nmrs[i] > factors[i].second) {
                nmrs[i] = 0;
                if (i == factors.size() - 1)
                    done = true;
            }
            else
                break;
        }
    }

    size_t sum = 0;
    for (auto& d : divisors)
        sum += d;

    return sum;
}

size_t gcd(size_t a, size_t b) {
    while (a != 0 && b != 0) {
        if (a > b)
            a %= b;
        else
            b %= a;
    }
    return std::max(a, b);
}


size_t T(size_t number) {
    size_t sum = 0;
    auto log3 = std::log(3);
    for (size_t n = 2; n <= number; n++) {

        size_t s = sigma(n);
        size_t g = gcd(s, n);

        size_t a = std::log(n / g) / log3;
        double b = std::log(n / g) / log3;

        if (b == a)
            sum += n;
    }
    return sum;
}

void solution699() {

    std::cout << T(1e6) << std::endl;



}


#include <map>
#include <any>
#include <stack>

struct runtime {
    using fun_id = std::size_t;
    using var_id = std::size_t;
    using type_id = std::size_t;

    struct type_info {
        type_id type_id;
    };

    struct function_storage {

    };

    struct variable_storage {
        std::any data;
        type_info type;
    };

    std::map<fun_id, function_storage> functions;
    std::map<var_id, variable_storage> variables;

    struct instruction {
        virtual void execute(runtime&) = 0;
    };

    struct create_variable : instruction {
        var_id id;
        type_id type;

        void execute(runtime& rt) override {
            rt.variables.emplace(id, type);
        };
    };

    struct assign_variable : instruction {
        var_id var;
        var_id value;

        void execute(runtime& rt) override {

        }
    };

    std::string code = R"~~(

int entrypoint() {
    int b = 100;
    int a = 10;
    a += b;
    return a;
}

)~~";

};

template<class Type>
class my_vector {
public:
    using value_type = Type;
    using pointer = Type*;
    using const_pointer = const Type*;
    using reference = Type&;
    using const_reference = const Type&;
    using size_type = size_t;
    using difference_type = ptrdiff_t;
private:
    pointer m_Data = nullptr;
    size_type m_Size = 0;
    size_type m_AllocatedSize = 0;

public:

    class const_iterator {
    public:
        using value_type = Type;
        using pointer = Type*;
        using const_pointer = const Type*;
        using reference = Type&;
        using const_reference = const Type&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;
    protected:
        pointer m_Ptr;
    public:
        const_iterator(pointer ptr) : m_Ptr(ptr) {}
        const_iterator(const_iterator&& other) = default;
        const_iterator(const const_iterator& other) = default;
        const_iterator& operator=(const_iterator&&) = default;
        const_iterator& operator=(const const_iterator&) = default;
        const_iterator& operator++() { m_Ptr++; return *this; }
        const_iterator& operator--() { m_Ptr--; return *this; }
        const_iterator& operator+=(size_type amt) { this->m_Ptr += amt; return *this; }
        const_iterator& operator-=(size_type amt) { this->m_Ptr -= amt; return *this; }
        const_iterator operator++(int) { const_iterator _backup = *this; m_Ptr++; return _backup; }
        const_iterator operator--(int) { const_iterator _backup = *this; m_Ptr--; return _backup; }
        const_iterator operator+(size_type amt) const { const_iterator _new = *this; _new.m_Ptr += amt; return _new; }
        const_iterator operator-(size_type amt) const { const_iterator _new = *this; _new.m_Ptr -= amt; return _new; }
        difference_type operator-(const const_iterator& other) const { return this->m_Ptr - other.m_Ptr; }

        bool operator!=(const const_iterator& other) const { return other.m_Ptr != m_Ptr; }
        bool operator==(const const_iterator& other) const { return other.m_Ptr == m_Ptr; }

        const_reference operator*() const { return *this->m_Ptr; }
        const_pointer operator->() const { return this->m_Ptr; }
        friend class my_vector;
    };

    class iterator : const_iterator {
    public:
        using value_type = Type;
        using pointer = Type*;
        using const_pointer = const Type*;
        using reference = Type&;
        using const_reference = const Type&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;
        using const_iterator::const_iterator;
        iterator(iterator&& other) = default;
        iterator(const iterator& other) = default;
        iterator& operator=(iterator&&) = default;
        iterator& operator=(const iterator&) = default;
        iterator& operator++() { this->m_Ptr++; return *this; }
        iterator& operator--() { this->m_Ptr--; return *this; }
        iterator& operator+=(size_type amt) { this->m_Ptr += amt; return *this; }
        iterator& operator-=(size_type amt) { this->m_Ptr -= amt; return *this; }
        iterator operator++(int) { iterator _backup = *this; this->m_Ptr++; return _backup; }
        iterator operator--(int) { iterator _backup = *this; this->m_Ptr--; return _backup; }
        iterator operator+(size_type amt) const { iterator _new = *this; _new.m_Ptr += amt; return _new; }
        iterator operator-(size_type amt) const { iterator _new = *this; _new.m_Ptr -= amt; return _new; }
        difference_type operator-(const iterator& other) const { return this->m_Ptr - other.m_Ptr; }

        bool operator!=(const iterator& other) const { return other.m_Ptr != this->m_Ptr; }
        bool operator==(const iterator& other) const { return other.m_Ptr == this->m_Ptr; }

        reference operator*() { return *this->m_Ptr; }
        pointer operator->() { return this->m_Ptr; }
        friend class my_vector;
    };

    my_vector() { allocate(2); }
    ~my_vector() { clear(); }

    bool empty() const { return m_Size == 0; }
    size_type size() const { return m_Size; }
    size_type capacity() const { return m_AllocatedSize; }

    void clear() {
        for (size_type _index = 0; _index < m_Size; _index++)
            m_Data[_index].~value_type();

        delete[] reinterpret_cast<uint8_t*>(m_Data);
        m_Data = nullptr;
        m_Size = 0;
    }

    void erase(iterator iter) {
        std::move(iter + 1, end(), iter);
        end()->~value_type();
        m_Size--;
    }

    void reserve(size_type amount) {
        if (amount < m_Size) return; // If already enough memory, return
        allocate(amount);
    }

    void push_back(value_type&& elem) {
        check_size();
        new (&m_Data[m_Size]) value_type(std::move(elem));
        m_Size++;
    }

    void push_back(const_reference elem) {
        check_size();
        new (&m_Data[m_Size]) value_type(elem);
        m_Size++;
    }

    template<class ...Args>
    reference emplace_back(Args&& ...args) requires std::constructible_from<value_type, Args...> {
        check_size();
        new (&m_Data[m_Size]) value_type(std::forward<Args>(args)...);
        m_Size++;
        return m_Data[m_Size - 1];
    }

    template<class ...Args>
    reference emplace(Args&& ...args) requires std::constructible_from<value_type, Args...> {
        check_size();
        new (&m_Data[m_Size]) value_type(std::forward<Args>(args)...);
        m_Size++;
        return m_Data[m_Size - 1];
    }

    reference at(size_type index) { return m_Data[index]; }
    const_reference at(size_type index) const { return m_Data[index]; }
    reference operator[](size_type index) { return m_Data[index]; }
    const_reference operator[](size_type index) const { return m_Data[index]; }

    reference front() { return *m_Data[0]; }
    reference back() { return *m_Data[m_Size - 1]; }
    const_reference front() const { return *m_Data[0]; }
    const_reference back() const { return *m_Data[m_Size - 1]; }

    pointer data() { return m_Data; }
    iterator begin() { return m_Data; }
    iterator end() { return m_Data + m_Size; }
    const_pointer data() const { return m_Data; }
    const_iterator begin() const { return m_Data; }
    const_iterator end() const { return m_Data + m_Size; }
    const_iterator cbegin() const { return m_Data; }
    const_iterator cend() const { return m_Data + m_Size; }

private:
    void check_size() {
        if (m_Size + 1 > m_AllocatedSize) allocate(m_AllocatedSize * 2);
    }

    void allocate(size_type amount) {
        pointer _new = reinterpret_cast<pointer>(new uint8_t[amount * sizeof value_type]);
        for (size_type _index = 0; _index < m_Size; _index++) {
            new (&_new[_index]) value_type(std::move(m_Data[_index]));
            m_Data[_index].~value_type();
        }
        delete[] reinterpret_cast<uint8_t*>(m_Data);
        m_Data = _new;
        m_AllocatedSize = amount;
    }
};

struct Woofers {
    int val;
    Woofers() { puts("construct"); }
    Woofers(int val) : val(val) { puts("construct"); }
    Woofers(const Woofers& other) : val(other.val) { puts("copy"); }
    Woofers(Woofers&& other) : val(other.val) { puts("move"); }
    Woofers& operator=(const Woofers& other) { val = other.val; puts("copy assign"); return *this; }
    Woofers& operator=(Woofers&& other) { val = other.val; puts("move assign"); return *this; }
    ~Woofers() { puts("delete"); }
};

#include <algorithm>

#include "vec.hpp"
#include "axial_array.hpp"




template<class Type>
struct ref_or_val {
    using type = Type;
    enum state : uint8_t { has_ref, has_val, has_null };

    constexpr ref_or_val() : m_Null(0), m_State(has_null) {}
    constexpr ref_or_val(type& v) : m_Ref(v), m_State(has_ref) {}
    constexpr ref_or_val(type&& v) : m_Val(std::move(v)), m_State(has_val) {}
    constexpr ref_or_val(const type&& v) : m_Val(std::move(v)), m_State(has_val) {}
    constexpr ref_or_val(const type& v) = delete;
    constexpr ref_or_val(const ref_or_val& other) { *this = other; }
    constexpr ref_or_val(ref_or_val&& other) { *this = std::move(other); }

    constexpr ref_or_val& operator=(type& v) { return assign(v), * this; }
    constexpr ref_or_val& operator=(type&& v) { return assign(std::move(v)), * this; }
    constexpr ref_or_val& operator=(const type&& v) { return assign(std::move(v)), * this; }
    constexpr ref_or_val& operator=(const ref_or_val& v) { return assign(v), * this; }
    constexpr ref_or_val& operator=(ref_or_val&& v) { return assign(std::move(v)), * this; }
    constexpr ref_or_val& operator=(const type& v) = delete;

    constexpr type& get() { return m_State == state::has_ref ? m_Ref.get() : m_Val; }
    constexpr const type& get() const { return m_State == state::has_ref ? m_Ref.get() : m_Val; }
    constexpr operator type& () { return m_State == state::has_ref ? m_Ref.get() : m_Val; }
    constexpr operator const type& () const { return m_State == state::has_ref ? m_Ref.get() : m_Val; }

    constexpr void assign(type& v) {
        cleanup();
        new (&m_Ref) std::reference_wrapper<type>(v);
        m_State = state::has_ref;
    }

    constexpr void assign(type&& v) {
        cleanup();
        new (&m_Val) type{ std::move(v) };
        m_State = state::has_val;
    }

    constexpr void assign(const ref_or_val& v) {
        m_State = v.m_State;
        m_State == state::has_ref
            ? assign(v.m_Ref.get())
            : assign(type{ v.m_Val });
    }

    constexpr void assign(ref_or_val&& v) {
        m_State = v.m_State;
        m_State == state::has_ref
            ? assign(v.m_Ref.get())
            : assign(std::move(v.m_Val));
        v.invalidate();
    }

    constexpr explicit operator bool() const { return m_State != state::has_null; }

private:
    union {
        std::reference_wrapper<type> m_Ref;
        type m_Val;
        void* m_Null;
    };
    state m_State;

    constexpr void cleanup() {
        if constexpr (!std::is_trivially_destructible_v<type>)
            if (m_State == state::has_val) m_Val.~type();
    }

    constexpr void invalidate() {
        new (&m_Null) std::nullptr_t{};
        m_State = state::has_null;
    }
};

#include <list>

#include <functional>


#include "list_comprehension.hpp"

struct C {
    C() = delete;
    C(int v) : v(v) { puts("create"); };
    C(C&& v) : v(v.v) { puts("move"); }
    C(const C& v) : v(v.v) { puts("copy"); }
    ~C() { puts("destroy"); }
    C& operator++() { v++; return *this; }
    bool operator==(const C& o) const { return o.v == v; }
    int v;
};

struct binary {
    using size_type = size_t;

    template<std::integral Ty>
    binary(Ty data)
        : data(reinterpret_cast<uint8_t*>(&data), reinterpret_cast<uint8_t*>(&data) + sizeof Ty)
    {}

    struct bit {
        operator bool() { return (byte >> index) & 1U; }
        bit& operator=(bool v) {
            byte ^= (-v ^ byte) & (1u << index);
            return *this;
        }
        uint8_t& byte;
        uint8_t index;
    };

    struct iterator {
        using value_type = bit;

        iterator& operator++() { index++; return *this; }
        bit operator*() { return bin->at(index); }
        bool operator==(const iterator& o) const { return o.index == index; }

        size_type index;
        binary* bin;
    };

    bit operator[](size_t index) {
        if (index < size())
            resize(index);
        return at(index);
    }

    bit at(size_t index) {
        return { data[index / 8], static_cast<uint8_t>(index % 8) };
    }

    size_type size() const { return data.size() * 8; }
    void resize(size_t size) { data.resize((size + 7) / 8); }

    iterator begin() { return { 0, this }; }
    iterator end() { return { size(), this }; }

private:
    std::vector<uint8_t> data;
};

template<class Ty>
class const_array_list_iterator : std::vector<Ty*>::const_iterator {
    using m_Base = std::vector<Ty*>::const_iterator;
public:
    using iterator_concept = m_Base::iterator_concept;
    using iterator_category = m_Base::iterator_category;
    using value_type = Ty;
    using difference_type = m_Base::difference_type;
    using pointer = Ty*;
    using reference = Ty&;

    auto& operator*() { return *m_Base::operator*(); }
    auto operator->() { return *m_Base::operator->(); }
};

template<class Ty>
struct array_list_iterator : std::vector<Ty*>::iterator {
    using m_Base = std::vector<Ty*>::iterator;
public:
    using iterator_concept = m_Base::iterator_concept;
    using iterator_category = m_Base::iterator_category;
    using value_type = Ty;
    using difference_type = m_Base::difference_type;
    using pointer = Ty*;
    using reference = Ty&;

    auto& operator*() { return *m_Base::operator*(); }
    auto operator->() { return *m_Base::operator->(); }
};

template<class Ty>
struct array_list {
    using value_type = Ty;
    using pointer = Ty*;
    using const_pointer = const Ty*;
    using reference = Ty&;
    using const_reference = const Ty&;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;

    using iterator = array_list_iterator<Ty>;
    using const_iterator = const_array_list_iterator<Ty>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    constexpr array_list() {}
    constexpr array_list(std::initializer_list<Ty> l) {}
    constexpr array_list(const array_list& other) : m_Arr(other.m_Arr) {}
    constexpr array_list(array_list&& other) : m_Arr(std::move(other.m_Arr)) {}
    array_list& operator=(const array_list& other) { m_Arr = other.m_Arr; }
    array_list& operator=(array_list&& other) { m_Arr = std::move(other.m_Arr); }

    constexpr reference at(size_type pos) { return *m_Arr.at(pos); }
    constexpr const_reference at(size_type pos) const { return *m_Arr.at(pos); }
    constexpr reference operator[](size_type pos) { return *m_Arr[pos]; }
    constexpr const_reference operator[](size_type pos) const { return *m_Arr[pos]; }
    constexpr reference front() { return *m_Arr.front(); }
    constexpr const_reference front() const { return *m_Arr.front(); }
    constexpr reference back() { return *m_Arr.back(); }
    constexpr const_reference back() const { return *m_Arr.back(); }

    constexpr iterator begin() { return { m_Arr.begin() }; }
    constexpr const_iterator begin() const { return { m_Arr.begin() }; }
    constexpr iterator end() { return { m_Arr.end() }; }
    constexpr const_iterator end() const { return { m_Arr.end() }; }
    constexpr const_iterator cbegin() const { return { m_Arr.cbegin() }; }
    constexpr const_iterator cend() const { return { m_Arr.cend() }; }
    constexpr iterator rbegin() { return { m_Arr.rbegin() }; }
    constexpr const_iterator rbegin() const { return { m_Arr.rbegin() }; }
    constexpr iterator rend() { return { m_Arr.rend() }; }
    constexpr const_iterator rend() const { return { m_Arr.rend() }; }
    constexpr const_iterator crbegin() const { return { m_Arr.crbegin() }; }
    constexpr const_iterator crend() const { return { m_Arr.crend() }; }

    constexpr bool empty() const { return m_Arr.empty(); }
    constexpr size_type size() const { return m_Arr.size(); }
    constexpr size_type max_size() const { return m_Arr.max_size(); }
    constexpr void reserve(size_type n) { m_Arr.reserve(n); }
    constexpr size_type capacity() const { return m_Arr.capacity(); }
    constexpr void shrink_to_fit() { m_Arr.shrink_to_fit(); }

    constexpr void clear() { m_Arr.clear(); }
    constexpr void insert(iterator pos, const Ty& val) { m_Arr.insert(pos, new Ty{ val }); }
    constexpr void insert(const_iterator pos, const Ty& val) { m_Arr.insert(pos, new Ty{ val }); }
    constexpr void insert(iterator pos, Ty&& val) { m_Arr.insert(pos, new Ty{ std::move(val) }); }
    constexpr void insert(const_iterator pos, Ty&& val) { m_Arr.insert(pos, new Ty{ std::move(val) }); }

    template<class... Args>
    constexpr iterator emplace(const_iterator pos, Args&&... args) {
        return m_Arr.emplace(pos, new Ty{ std::forward<Args>(args)... });
    }

    constexpr iterator erase(const_iterator pos) {
        delete* pos;
        return m_Arr.erase(pos);
    }

    constexpr void push_back(const Ty& val) { m_Arr.push_back(new Ty{ val }); }
    constexpr void push_back(Ty&& val) { m_Arr.push_back(new Ty{ std::move(val) }); }

    template<class... Args>
    constexpr reference emplace_back(Args&&... args) {
        return m_Arr.emplace_back(new Ty{ std::forward<Args>(args)... });
    }

    constexpr void pop_back() { delete m_Arr.back(); m_Arr.pop_back(); }
    constexpr void swap(array_list& other) { m_Arr.swap(other.m_Arr); }

private:
    std::vector<Ty*> m_Arr;
};

#include <map>
#include <variant>

template<class Ty, class ...Tys>
concept OneOf = (std::same_as<Ty, Tys> || ...);

class Json
{
public:
    using Floating = double;
    using Integral = int64_t;
    using Unsigned = uint64_t;
    using String = std::string;
    using Boolean = bool;
    using Array = std::vector<Json>;
    using Object = std::map<String, Json, std::less<void>>;
    using Null = std::nullptr_t;

private:
    using JsonValue = std::variant<Floating, Integral, Unsigned, String, Boolean, Array, Object, Null>;
    struct Type { enum { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null }; };
    JsonValue m_Value;

    template<class Ty> struct Alias { using Type = Ty; };
    template<std::signed_integral Ty> struct Alias<Ty> { using Type = Integral; };
    template<std::unsigned_integral Ty> struct Alias<Ty> { using Type = Unsigned; };

public:
    template<class Ty = Null>
    Json(const Ty& ty = {}) : m_Value(static_cast<Alias<Ty>::Type>(ty)) {}

    template<class Ty> Ty get() const { return static_cast<Ty>(std::get<Alias<Ty>::Type>(m_Value)); }
    template<class Ty> Ty& ref() { return std::get<Ty>(m_Value); }
    template<class Ty> const Ty& ref() const { return std::get<Ty>(m_Value); }

    Json& operator[](std::string_view index)
    {
        if (m_Value.index() == Type::Null) m_Value = Object{};
        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
        auto _it = ref<Object>().find(index);
        if (_it == ref<Object>().end()) return ref<Object>()[std::string{ index }];
        else return _it->second;
    }
};

#include <bit>


































template<class Ky, class Ty>
class hash_map {
public:
    using key_type = Ky;
    using mapped_type = Ty;
    using value_type = std::pair<const Ky, Ty>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using reference = value_type&;
    using const_reference = const value_type&;
    using pointer = value_type*;
    using const_pointer = const value_type*;

private:
    class node {
    public:
        node() = default;
        node(const Ky& key) : m_Value(new value_type{ key, {} }) {}
        node(const value_type& val) : m_Value(new value_type{ val }) {}

        value_type* find(const Ky& key) {
            if (!m_Value) return nullptr;
            if (key == m_Value->first) return m_Value;
            if (m_Next) return m_Next->find(key);
            return nullptr;
        }

        void erase(const Ky& key, size_type& size) {
            if (!m_Value) return;
            if (key == m_Value->first) {
                size--;
                delete m_Value; // Delete the value
                if (m_Next) {   // Move the next node over to this one
                    m_Value = m_Next->m_Value;
                    m_Next = m_Next->m_Next;
                    m_Next->m_Value = nullptr;
                    m_Next->m_Next = nullptr;
                    delete m_Next; // Delete the next
                }
            }
            else if (!m_Next) return;
            else m_Next->erase(key, size);
        }

        value_type* find_or_insert(const Ky& key, size_type& size) {
            if (!m_Value) return size++, m_Value = new value_type{ key, {} };
            if (key == m_Value->first) return m_Value;
            if (!m_Next) return m_Next = new node{ key }, size++, m_Next->m_Value;
            return m_Next->find_or_insert(key, size);
        }

        value_type* puts(const value_type& val, size_type& size) {
            if (!m_Value) return size++, m_Value = new value_type{ val };
            if (val.first == m_Value->first) return m_Value->second = val.second, m_Value;
            if (!m_Next) return m_Next = new node{ val }, size++, m_Next->m_Value;
            return m_Next->puts(val, size);
        }

        ~node() {
            if (m_Value) delete m_Value;
            if (m_Next) delete m_Next;
        }

    private:
        value_type* m_Value = nullptr;
        node* m_Next = nullptr;

        friend class hash_map::iterator;
    };

public:
    class iterator {
    public:
        using iterator_category = std::forward_iterator_tag;
        using value_type = hash_map::value_type;
        using difference_type = hash_map::difference_type;
        using reference = hash_map::reference;
        using pointer = hash_map::pointer;

        iterator() : m_Nodes(nullptr), m_Size(0) {};
        iterator(node** n, size_type size) : m_Nodes(n), m_Size(size) {
            if (!m_Size) m_Base = *m_Nodes; // If no size, we're at the end
            else do if ((*m_Nodes)->m_Value) { // if linked list has value
                m_Base = (*m_Nodes);      // Set base node
                break;
            } while (++m_Nodes, --m_Size); // Keep incrementing till m_Size == 0
            if (!m_Base) m_Base = *m_Nodes; // If no base base set, it's empty
        }

        iterator& operator++() {
            if (m_Base->m_Next) m_Base = m_Base->m_Next; // First try linked list
            else while (m_Size--) // Otherwise find next linked list with content
                if (!m_Size) m_Base = *++m_Nodes; // If no size left, we're at the end
                else if ((*++m_Nodes)->m_Value) { // check if value
                    m_Base = (*m_Nodes); // Set new base node
                    break;
                }
            return *this;
        }

        iterator operator++(int) {
            iterator _next = *this;
            ++(*this);
            return _next;
        }

        void swap(iterator& other) {
            std::swap(other.m_Base, m_Base);
            std::swap(other.m_Nodes, m_Nodes);
            std::swap(other.m_Size, m_Size);
        }

        bool operator==(const iterator& other) const { return m_Base == other.m_Base; }
        bool operator!=(const iterator& other) const { return m_Base != other.m_Base; }
        value_type& operator*() { return *m_Base->m_Value; }
        value_type* operator->() { return m_Base->m_Value; }

    protected:
        node* m_Base = nullptr;
        node** m_Nodes;
        size_type m_Size;
    };

    class const_iterator : public iterator {
    public:
        const value_type& operator*() { return *this->m_Base->m_Value; }
    };

    hash_map() { resize(16); }
    hash_map(std::initializer_list<value_type> il) {
        resize(16); // Insert everything from list
        for (auto& i : il) puts(i);
    }

    hash_map(const hash_map& other) {
        resize(other.buckets());
        for (auto& i : other) puts(i);
    }

    hash_map(hash_map&& other) : m_Nodes(other.m_Nodes),
        m_Buckets(other.m_Buckets), m_Size(other.m_Size) {
        other.m_Buckets = 0;
        other.m_Size = 0;
        other.m_Nodes = nullptr;
    }

    hash_map& operator=(const hash_map& other) {
        cleanup();
        resize(other.buckets());
        for (auto& i : other) puts(i);
        return *this;
    }

    hash_map& operator=(hash_map&& other) {
        cleanup();
        m_Nodes = other.m_Nodes;
        m_Buckets = other.m_Buckets;
        m_Size = other.m_Size;
        other.m_Buckets = 0;
        other.m_Size = 0;
        other.m_Nodes = nullptr;
        return *this;
    }

    ~hash_map() { cleanup(); }

    bool empty() const { return m_Size != 0; }
    size_type size() const { return m_Size; }
    size_type buckets() const { return m_Buckets; }
    iterator begin() { return iterator{ m_Nodes, m_Buckets }; }
    iterator end() { return iterator{ m_Nodes + m_Buckets, 0 }; }
    const_iterator begin() const { return const_iterator{ m_Nodes, m_Buckets }; }
    const_iterator end() const { return const_iterator{ m_Nodes + m_Buckets, 0 }; }
    const_iterator cbegin() const { return const_iterator{ m_Nodes, m_Buckets }; }
    const_iterator cend() const { return const_iterator{ m_Nodes + m_Buckets, 0 }; }
    Ty& operator[](const Ky& key) { return m_Nodes[hash(key)]->find_or_insert(key, m_Size)->second; }
    Ty& puts(const Ky& key, const Ty& val) { return m_Nodes[hash(key)]->puts({ key, val }, m_Size)->second; }
    Ty& puts(const value_type& val) { return m_Nodes[hash(val.first)]->puts(val, m_Size)->second; }
    Ty& at(const Ky& key) { return m_Nodes[hash(key)]->find(key)->second; }
    const Ty& at(const Ky& key) const { return m_Nodes[hash(key)]->find(key)->second; }
    void erase(const Ky& key) { m_Nodes[hash(key)]->erase(key, m_Size); }

    void resize(size_type buckets) {
        if (buckets < m_Buckets) return;  // Only resize if specified bigger
        buckets = std::bit_ceil(buckets); // Round up to nearest power of 2
        node** _new = new node * [buckets]; // Allocated new array
        for (size_type i = 0; i < m_Buckets; i++) _new[i] = m_Nodes[i];
        for (size_type i = m_Buckets; i < buckets; i++) _new[i] = new node;
        delete[] m_Nodes; // Delete the original array
        m_Buckets = buckets, m_Nodes = _new;// Update size, assign new array
    }

private:
    size_type m_Size = 0, m_Buckets = 0;
    node** m_Nodes = nullptr;

    size_t hash(const Ky& key) const { return std::hash<Ky>{}(key)& (m_Buckets - 1); }
    void cleanup() { // Delete array of nodes
        for (size_type i = 0; i < m_Buckets; i++) delete m_Nodes[i];
        delete[] m_Nodes;
    }
};

template<class Ty>
class linked_list {
public:
    using value_type = Ty;
    using reference = Ty&;
    using const_reference = const Ty&;
    using pointer = Ty*;
    using const_pointer = const Ty*;
    using difference_type = std::ptrdiff_t;
    using size_type = std::size_t;

    struct node {
        value_type value;
        node* next = nullptr;

        ~node() { delete next; }

        reference push(const_reference val) { return next ? next->push(val) : (next = new node{ val })->value; };

        template<class ...Args>
        reference emplace(Args&& ...args) {
            return next
                ? next->emplace(std::forward<Args>(args)...)
                : (next = new node{ { std::forward<Args>(args)... } })->value;
        };
    };

    class const_iterator {
    public:
        using iterator_category = std::forward_iterator_tag;
        using value_type = typename linked_list::value_type;
        using difference_type = typename linked_list::difference_type;
        using pointer = typename linked_list::const_pointer;
        using reference = const value_type&;

        const_iterator() {}
        const_iterator(node* n) : m_Node(n) {}

        const_iterator& operator++() { m_Node = m_Node->next; return *this; }
        const_iterator operator++(int) { const_iterator _t = *this; m_Node = m_Node->next; return _t; }
        reference operator*() { return m_Node->value; }

        bool operator==(const const_iterator& other) const { return m_Node == other.m_Node; }

    private:
        node* m_Node = nullptr;

        friend class linked_list;
    };

    class iterator : public const_iterator {
    public:
        using iterator_category = std::forward_iterator_tag;
        using value_type = typename linked_list::value_type;
        using difference_type = typename linked_list::difference_type;
        using pointer = typename linked_list::const_pointer;
        using reference = value_type&;

        using const_iterator::const_iterator;

        iterator& operator++() { this->m_Node = this->m_Node->next; return *this; }
        iterator operator++(int) { iterator _t = *this; this->m_Node = this->m_Node->next; return _t; }
        reference operator*() { return this->m_Node->value; }

        bool operator==(const iterator& other) const { return this->m_Node == other.m_Node; }

        friend class linked_list;
    };

    iterator begin() { return { m_Start }; }
    iterator end() { return { nullptr }; }
    const_iterator begin() const { return { m_Start }; }
    const_iterator end() const { return { nullptr }; }
    const_iterator cbegin() const { return { m_Start }; }
    const_iterator cend() const { return { nullptr }; }

    reference front() const { return m_Start->value; }

    iterator erase_after(const_iterator pos) {
        auto _b = pos.m_Node->next;
        if (_b) {
            pos.m_Node->next = pos.m_Node->next->next;
            delete _b;
            m_Size--;
        }
        return { pos.m_Node->next };
    }

    iterator erase_after(const_iterator first, const_iterator last) {
        while (first != last) erase_after(first), ++first;
        return last;
    }

    void push_front(const Ty& val) { m_Size++, m_Start = new node{ val, m_Start }; }
    void push_front(Ty&& val) { m_Size++, m_Start = new node{ std::move(val), m_Start }; }
    void pop_front() {
        auto _b = m_Start;
        if (_b) {
            m_Start = m_Start->next;
            delete _b;
            m_Size--;
        }
    }

    template<class ...Args>
    reference emplace_front(Args&& ...args) { return (m_Size++, m_Start = new node{ { std::forward<Args>(args)... }, m_Start })->value; }

    iterator insert_after(const_iterator pos, const Ty& val) { return m_Insert(pos, val); }
    iterator insert_after(const_iterator pos, Ty&& val) { return m_Insert(pos, std::move(val)); }

    iterator insert_after(const_iterator pos, size_type count, const Ty& val) {
        while (count--) m_Insert(pos, val);
        return pos;
    }

    template<class It>
    iterator insert_after(const_iterator pos, It first, It last) {
        while (first != last) m_Insert(pos, *first), ++first;
        return pos;
    }

    iterator insert_after(const_iterator pos, std::initializer_list<Ty> il) {
        for (auto& i : il) m_Insert(pos, i);
        return pos;
    }

    template<class ...Args>
    iterator emplace_after(const_iterator pos, Args&& ...args) { return m_Insert(pos, std::forward<Args>(args)...); }

private:
    node* m_Start = nullptr;
    size_type m_Size = 0;

    template<class ...Args>
    iterator m_Insert(const_iterator& pos, Args&& ...args) {
        auto _b = pos.m_Node->next;
        pos.m_Node->next = new node{ { std::forward<Args>(args)... } };
        pos.m_Node->next->next = _b;
        pos = pos.m_Node->next;
        m_Size++;
        return pos.m_Node;
    }
};

template<class Symbol = std::uint8_t, class State = std::size_t>
struct TuringMachine {
    enum Direction { Left = -1, None = 0, Right = 1 };
    constexpr static State HALT = static_cast<State>(-1);
    using Instruction = std::tuple<State, Symbol, Direction>;

    std::map<std::pair<State, Symbol>, Instruction> instructions;
    std::list<Symbol> tape{ 0 };
    std::list<Symbol>::iterator head = tape.begin();
    State state = 0;

    Symbol& Read() { return *head; }
    Symbol& Move(Direction d) {
        return d == Left ? head == tape.begin() ? *(head = tape.insert(tape.begin(), 0)) : *--head
            : d == Right ? ++head == tape.end() ? *(head = tape.insert(tape.end(), 0)) : *head : *head;
    }

    bool Step() {
        auto& [nstate, write, move] = instructions.at({ state, Read() });
        return (Read() = write, Move(move), state = nstate) != HALT;
    }
};

void trash() {
    using enum TuringMachine<>::Direction;
    enum : std::size_t { _ = 2, HALT = TuringMachine<>::HALT };
    TuringMachine<> machine{
        {   // Instruction table for binary addition
            { { 0, 0, }, { 0, 0, Right } },
            { { 0, 1, }, { 0, 1, Right } },
            { { 0, _, }, { 1, _, Right } },

            { { 1, 0, }, { 1, 0, Right } },
            { { 1, 1, }, { 1, 1, Right } },
            { { 1, _, }, { 2, _, Left } },

            { { 2, 0, }, { 2, 1, Left } },
            { { 2, 1, }, { 3, 0, Left } },
            { { 2, _, }, { 5, _, Right } },

            { { 3, 0, }, { 3, 0, Left } },
            { { 3, 1, }, { 3, 1, Left } },
            { { 3, _, }, { 4, _, Left } },

            { { 4, 0, }, { 0, 1, Right } },
            { { 4, 1, }, { 4, 0, Left } },
            { { 4, _, }, { 0, 1, Right } },

            { { 5, 0, }, { 5, _, Right } },
            { { 5, 1, }, { 5, _, Right } },
            { { 5, _, }, { HALT, _, None } },
        },
        // Initial tape state
        { 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, _, 1, 0, 1, 1, 1, 1, 0, 1, 1, _ }
    };

    for (auto& i : machine.tape)
        std::cout << (i == 2 ? '_' : i == 1 ? '1' : '0');
    std::cout << '\n';

    while (machine.Step());

    for (auto& i : machine.tape)
        std::cout << (i == 2 ? '_' : i == 1 ? '1' : '0');


    linked_list<int> _list;
    _list.push_front(1);
    _list.insert_after(_list.begin(), 2);
    _list.insert_after(_list.begin(), 3);

    for (auto& i : _list)
        std::cout << i;

    hash_map<std::string, int> _map;
    _map["hello"] = 10;
    _map["world"] = 12;
    _map["0"] = 13;

    int val1 = _map["hello"];
    int val2 = _map["world"];
    int val3 = _map["0"];
    _map.puts("0", 1032);

    _map = { { "hello", 1 }, { "world", 2 } };

    for (auto& i : _map)
    {
        std::cout << i.first << ", " << i.second << '\n';
    }

    _map.erase("hello");

    int val4 = _map["hello"];


    array_list<int> ints;
    ints.push_back(0);
    ints.push_back(1);
    ints.push_back(2);
    ints.push_back(3);

    for (auto& i : ints) {
        std::cout << i << std::endl;
    }

    std::vector<bool> a;

    a[0] = 1;

    pa_function<int(int, int, int)> f = [](int a, int b, int c) { return a + b + c; };


    binary bin = 913951935;
    for (auto i : bin) {
        std::cout << i << std::endl;
    }


    constexpr auto size = sizeof binary::bit;
    std::array<int, 2>;
    axial_array<int, 3> _a1{ 1, 2, 3, 4, 5 };
    axial_array<int, 3> _a2{ 5, 4, 3, 2, 1 };
    _a1.swap(_a2);


    const axial_array<int, 3> _arr{
            {  1,  2,  3 },
          {  4,  5,  6,  7 },
        {  8,  9, 10, 11, 12 },
          { 13, 14, 15, 16 },
            { 17, 18, 19 },
    };

    for (auto [val, pos] : _arr.with_index()) {
        std::cout << pos.x << ", " << pos.y << " : " << val << std::endl;
    }

    for (auto& val : _arr) {
        std::cout << val << std::endl;
    }


    axial_array<int, 4> _arr2{
          {  1,  2,  3,  4 },
        {  5,  6,  7,  8,  9 },
      { 10, 11, 12, 13, 14, 15 },
    { 16, 17, 18, 19, 20, 21, 22 },
      { 23, 24, 25, 26, 27, 28 },
        { 29, 30, 31, 32, 33 },
          { 34, 35, 36, 37 },
    };

    axial_array<int, 5> _arr3{
              {  1,  2,  3,  4,  5 },
            {  6,  7,  8,  9, 10, 11 },
          { 12, 13, 14, 15, 16, 17, 18 },
        { 19, 20, 21, 22, 23, 24, 25, 26 },
      { 27, 28, 29, 30, 31, 32, 33, 34, 35 },
        { 36, 37, 38, 39, 40, 41, 42, 43 },
          { 44, 45, 46, 47, 48, 49, 50 },
            { 51, 52, 53, 54, 55, 56 },
              { 57, 58, 59, 60, 61 }
    };

    axial_array<int, 6> _arr4{
                  {  1,  2,  3,  4,  5,  6 },
                {  7,  8,  9, 10, 11, 12, 13 },
              { 14, 15, 16, 17, 18, 19, 20, 21 },
            { 22, 23, 24, 25, 26, 27, 28, 29, 30 },
          { 31, 32, 33, 34, 35, 36, 37, 38, 39, 40 },
        { 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51 },
          { 52, 53, 54, 55, 56, 57, 58, 59, 60, 61 },
            { 62, 63, 64, 65, 66, 67, 68, 69, 70 },
              { 71, 72, 73, 74, 75, 76, 77, 78 },
                { 79, 80, 81, 82, 83, 84, 85 },
                  { 86, 87, 88, 89, 90, 91 }
    };

    for (auto [val, pos] : _arr4.with_index()) {
        std::cout << pos.x << ", " << pos.y << " : " << val << std::endl;
    }

    for (auto& val : _arr4) {
        std::cout << val << std::endl;
    }

    axial_array<int, 4>::key _key{ -1, 1 };
    auto z = _key.z();


    // _arr[{ -1, 1 }] = 2;

    6 + 7 + 8 + 9 + 10 + 11 + 10 + 9 + 8 + 7 + 6;
    5 + 6 + 7 + 8 + 9 + 8 + 7 + 6 + 5;         // 61
    4 + 5 + 6 + 7 + 6 + 5 + 4;                 // 37
    3 + 4 + 5 + 4 + 3;
    2 + 3 + 2;

    constexpr int x = 5;

    -(x - 1) <=> (x - 1);

    2 * x - 1;

    constexpr float avg = 3 * (x * x - x) + 1;
    constexpr float avg1 = 3 * x * (x - 1);


    _arr[{ 0, 0 }];

    constexpr auto si = sizeof vec<double, 4>;

    vec<double, 2> v2{ 1, 2 };
    vec<double, 3> v3{ 3, 4, 5 };
    vec<double, 4> v4{ 3, 4, 5, 6 };

    vec<double, 2> v20{ 1, 2 };
    vec<double, 2> v21{ v2 };

    vec<double, 3> v30{ 1, 2, 3 };
    vec<double, 3> v31{ v2, 2 };
    vec<double, 3> v32{ 1, v2 };
    vec<double, 3> v33{ v3 };

    vec<double, 4> v40{ 1, 2, 3, 4 };
    vec<double, 4> v41{ 1, v2, 2 };
    vec<double, 4> v42{ v2, v2 };
    vec<double, 4> v43{ v3, 2 };
    vec<double, 4> v44{ 1, v3 };
    vec<double, 4> v45{ v4 };

    vec<double, 2> v22 = v3.xy;
    vec<double, 2> v23 = v4.yz;

    vec<double, 3> v34 = v4.yzw;
    vec<double, 3> v35 = v4.xyz;

    //vec<double, 4> v41{ 1, v2, 4 };
    //vec<double, 4> v42{ v2, 3, 4 };
    //vec<double, 4> v43{ v2, v2 };
    //vec<double, 4> v44{ v3, 2, 3, 4 };
    //vec<double, 4> v45{ 1, v3 };

    v4 /= 4;
    vec<double, 4> vv1{ 1, 2, 3, 4 };
    vec<double, 4> vv2{ 1, 2, 3, 4 };
    auto v = vv1 != vv2;

    std::is_trivial_v<vec<double, 4>>;
    std::is_trivially_constructible_v<vec<double, 4>>;
    std::is_trivially_copyable_v<vec<double, 4>>;
    std::is_trivially_copy_assignable_v<vec<double, 4>>;
    std::is_trivially_copy_constructible_v<vec<double, 4>>;
    std::is_trivially_default_constructible_v<vec<double, 4>>;
    std::is_trivially_destructible_v<vec<double, 4>>;
    std::is_trivially_move_assignable_v<vec<double, 4>>;
    std::is_trivially_move_constructible_v<vec<double, 4>>;

    //auto _new = v41 + vec<double, 4>{ 1, 2, 3, 4 };

    //vfa.at(3) = 3;

    //double& a = _new[0];
    //double& b = _new[1];
    //double& c = _new[2];
   // double& d = _new[3];



    constexpr_counter_example();
    type_linker_example();
    pa_function_example();
    smart_tuple_example();
    lambda_example();
    struct_tuple_example();
}


//template<int Width, int Height>
//constexpr bool has_path1(const Screen<Width, Height>& screen) { //
//    int ry = 0, y = 0, x = 0, d = 1;                           //
//    while (ry != Height) {                                     // Continue until the reset y is Height
//        while (screen[{ x + 1, y }] == 0) ++x, d = 1;          // Check ahead, if free: move and reset direction to 1
//        if (x == Width - 1) break;                             // If after move we're at the edge: win
//        if (screen[{ x, y + d }] == 0) y += d;                 // Check in current direction, if 0: move
//        else { if (d == 1) d = -1; else y = ++ry, x = 0; }     // Otherwise, if dir is 1, try other dir, 
//    }                                                          // Otherwise start at next y and set x back to 0
//    return x == Width - 1;                                     // Win if we're at the final x
//}

struct Point { int x, y; };
template<int Width, int Height>
struct Screen {
    constexpr static auto width = Width;
    constexpr static auto height = Height;
    constexpr auto& operator[](Point p) { return data[p.x + p.y * Width]; }
    constexpr auto& operator[](Point p) const { return data[p.x + p.y * Width]; }
    std::uint8_t data[Width * Height];
};

constexpr bool check(auto& s, auto& c, Point p) {
    if (p.x < 0 || p.x >= s.width || p.y < 0 || p.y >= s.height) return false;
    return s[p] == 0 && c[p] == 0 && (c[p] = 1, p.x == s.width - 1
        || check(s, c, { p.x + 1, p.y }) || check(s, c, { p.x - 1, p.y })
        || check(s, c, { p.x, p.y + 1 }) || check(s, c, { p.x, p.y - 1 }));
}

constexpr bool has_path(auto& screen) {
    std::decay_t<decltype(screen)> checked{};
    return check(screen, checked, { 0, 0 });
}


struct everything { template<class Ty> operator Ty(); };
template<std::size_t N, class Fun, class ...Args> struct partially_invocable;
template<std::size_t N, class Fun, class ...Args> requires std::invocable<Fun, Args...>
struct partially_invocable<N, Fun, Args...> : std::bool_constant<true> {};
template<std::size_t N, class Fun, class ...Args> requires (!std::invocable<Fun, Args...>)
struct partially_invocable<N, Fun, Args...> : std::bool_constant<partially_invocable<N - 1, Fun, Args..., everything>::value> {};
template<class Fun, class ...Args> struct partially_invocable<0, Fun, Args...>
: std::bool_constant<false> {
    static_assert(std::invocable<Fun, Args...>, "Incompatible arguments in function call");
};

constexpr auto my_forward(auto& val) { return std::ref(val); }
constexpr auto my_forward(auto&& val) { return std::move(val); }

template<class Lambda>
struct function : Lambda {
    template<class ...Args> requires partially_invocable<20, Lambda, Args...>::value
        constexpr auto operator()(Args&&...args) const {
        if constexpr (std::invocable<Lambda, Args...>)
            return Lambda::operator()(std::forward<Args>(args)...);
        else {
            auto _l = [...args = my_forward(args), fun = *this](auto&&...rem) {
                return fun(args..., std::forward<decltype(rem)>(rem)...);
            };
            return function<decltype(_l)>{ std::move(_l) };
        }
    }
};