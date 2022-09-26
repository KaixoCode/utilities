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

//class Json
//{
//public:
//    using Floating = double;
//    using Integral = int64_t;
//    using Unsigned = uint64_t;
//    using String = std::string;
//    using Boolean = bool;
//    using Array = std::vector<Json>;
//    using Object = std::map<String, Json, std::less<void>>;
//    using Null = std::nullptr_t;
//
//    enum class Type { Floating, Integral, Unsigned, String, Boolean, Array, Object, Null };
//private:
//    using JsonValue = std::variant<Floating, Integral, Unsigned, String, Boolean, Array, Object, Null>;
//    JsonValue m_Value;
//
//    template<class Ty> struct Alias { using Type = Ty; };
//    template<std::signed_integral Ty> struct Alias<Ty> { using Type = Integral; };
//    template<std::unsigned_integral Ty> struct Alias<Ty> { using Type = Unsigned; };
//
//public:
//    template<class Ty = Null>
//    Json(const Ty& ty = {}) : m_Value(static_cast<Alias<Ty>::Type>(ty)) {}
//
//    template<class Ty> Ty get() const { return static_cast<Ty>(std::get<Alias<Ty>::Type>(m_Value)); }
//    template<class Ty> Ty& ref() { return std::get<Ty>(m_Value); }
//    template<class Ty> const Ty& ref() const { return std::get<Ty>(m_Value); }
//
//    Json& operator[](std::string_view index)
//    {
//        if (m_Value.index() == Type::Null) m_Value = Object{};
//        else if (m_Value.index() != Type::Object) throw std::exception("Not an object.");
//        auto _it = ref<Object>().find(index);
//        if (_it == ref<Object>().end()) return ref<Object>()[std::string{ index }];
//        else return _it->second;
//    }
//};

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

    template<std::size_t N>
    constexpr heap(const Ty(&arr)[N])
        : m_Size(std::min(N, Size)) {
        std::copy_n(arr, N, m_Data);
        for (int64_t i = (m_Size - Arity) / Arity; i >= 0; i--)
            sink(i); // Sink all layers
    }

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

    constexpr bool empty() const { return m_Size == 0; }
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


#include <span>
#include <ranges>

//namespace old {
//
//    struct Edge {
//        std::size_t vertex = std::numeric_limits<std::size_t>::max();
//        double length = std::numeric_limits<double>::max();
//    };
//
//    template<std::size_t S = 0>
//    struct Vertex {
//        constexpr static auto size = S;
//        Edge edges[S];
//    };
//
//    template<> struct Vertex<0> {
//        constexpr static auto size = 0;
//        Edge* edges = nullptr;
//    };
//
//    template<std::size_t Size, std::size_t Edges>
//    struct Graph {
//        struct {
//            std::size_t offsets[Size + 1]{};
//            Edge edges[Edges]{};
//
//            constexpr auto from_edge(std::size_t edge) const {
//                for (std::size_t i = 0; i < Size + 1; ++i)
//                    if (offsets[i] > edge) return i - 1;
//                return Size;
//            }
//
//            constexpr auto operator[](std::size_t i) const {
//                return std::span{ edges + offsets[i], offsets[i + 1] - offsets[i] };
//            }
//
//            friend struct Graph;
//        } vertices;
//
//        constexpr Graph(std::size_t(&a)[Size + 1], Edge(&b)[Edges]) {
//            std::copy_n(a, Size + 1, vertices.offsets);
//            std::copy_n(b, Edges, vertices.edges);
//        }
//
//        template<class ...Vertices>
//        constexpr Graph(Vertices&& ...vs) {
//            std::size_t i1 = 0, i2 = 0;
//            vertices.offsets[0] = 0;
//            ((std::copy_n(vs.edges, vs.size, vertices.edges + i2),
//                vertices.offsets[++i1] = i2 + vs.size, i2 += vs.size), ...);
//        }
//
//        // Implemented using Dijkstra's Algorithm
//        constexpr auto shortest_path(std::size_t a) const {
//            constexpr auto infinity = std::numeric_limits<double>::max();
//            std::array<double, Size> dist{}; // distances
//            dist.fill(infinity), dist[a] = 0; // itself = 0, rest = infinity
//
//            bool checked[Size]{}; // Keep track of checked vertices
//            checked[a] = true; // No need to check itself
//            for (std::size_t _ = 0, u = 0; _ < Size; ++_) {
//                double closest = infinity; // Find closest unchecked vertex
//                for (std::size_t next = 0; next < Size; ++next)
//                    if (!checked[next] && dist[next] < closest)
//                        closest = dist[u], u = next;
//                for (auto [v, len] : vertices[u]) // relax vertex u and its neighbours
//                    dist[v] = std::min(dist[v], dist[u] + len);
//                checked[u] = true; // Now we've checked this one
//            }
//
//            return dist;
//        }
//
//        constexpr auto minimum_spanning_tree() const {
//            struct CompleteEdge {
//                std::size_t a, b;
//                double length;
//            };
//
//            std::size_t offsets[Size + 1]{};
//            CompleteEdge newEdges[Size - 1]{};
//
//            bool in_w[Size]{};
//            StackVector<std::size_t, Size> w{};
//            w.push_back(0), in_w[0] = true; // Start at v = 0
//
//            for (std::size_t i = 0; i < Size - 1; ++i) {
//                double len = std::numeric_limits<double>::max();
//                std::size_t v = 0, u = 0;
//                for (auto vx : w.span()) // Loop over vertices in w
//                    for (auto [ux, lenx] : vertices[vx]) // Loop over edges of v
//                        // pick smallest edge who's other vertex is not in w
//                        if (!in_w[ux] && lenx < len) len = lenx, v = vx, u = ux;
//
//                w.push_back(u), in_w[u] = true; // Add connected vertex to choices
//                newEdges[i] = CompleteEdge{ v, u, len }; // Add new edge
//                ++offsets[u + 1], ++offsets[v + 1]; // Add 1 to offsets
//            }
//
//            // Calculate offsets
//            std::size_t prev = 0;
//            for (auto& i : offsets) i += prev, prev = i;
//
//            // Order the edges
//            Edge finalEdges[2 * Size - 2]{};
//            std::size_t cntr[Size]{};
//            for (std::size_t i = 0; i < Size - 1; ++i) {
//                auto e = newEdges[i];
//                finalEdges[offsets[e.a] + cntr[e.a]++] = Edge{ e.b, e.length };
//                finalEdges[offsets[e.b] + cntr[e.b]++] = Edge{ e.a, e.length };
//            }
//
//            return Graph<Size, 2 * Size - 2>{ offsets, finalEdges };
//        }
//    };
//
//    template<class ...Vertices>
//    Graph(Vertices&&...)->Graph<sizeof...(Vertices), (Vertices::size + ...)>;
//}

template<class Ty, std::size_t Size> struct StackVector {
    constexpr void push_back(Ty&& v) { m_D[m_S] = std::move(v); ++m_S; }
    constexpr void push_back(const Ty& v) { m_D[m_S] = v; ++m_S; }
    constexpr void pop_back() { --m_S; }
    constexpr auto size() const { return m_S; }
    constexpr auto data() const { return m_D; }
    constexpr auto data() { return m_D; }
    constexpr auto begin() const { return m_D; }
    constexpr auto end() const { return m_D + m_S; }
    constexpr auto cbegin() const { return m_D; }
    constexpr auto cend() const { return m_D + m_S; }
    constexpr auto begin() { return m_D; }
    constexpr auto end() { return m_D + m_S; }
    Ty m_D[Size]{};
    std::size_t m_S = 0;
};
struct Edge { std::size_t a, b; double length; };
template<std::size_t Vertices, std::size_t Edges>
struct UndirectedGraph {
    Edge edges[Edges]{}; // All the edge weights
    std::size_t part_of[Edges * 2]{}; // Mapping from vertex to edge
    std::size_t offsets[Vertices + 1]{};
    // Construct with template pack of edges, creates array
    template<class ...Tys> constexpr UndirectedGraph(Tys&& ...es)
        : UndirectedGraph(StackVector<Edge, Edges>{
            { std::forward<Tys>(es)... }, sizeof...(Tys) }) {}
            // Construct with stack array
            constexpr UndirectedGraph(StackVector<Edge, Edges>&& arr) {
                // Copy edges and calculate offsets
                std::copy_n(arr.data(), arr.size(), edges);
                for (auto& [a, b, _] : arr)
                    ++offsets[a + 1], ++offsets[b + 1];
                // Convert offsets to be accumulative
                std::size_t p = 0;
                for (auto& i : offsets) p = i += p;
                // Using offsets, get edges that vertices are part of.
                std::size_t i1 = 0, now[Vertices + 1]{};
                for (auto& [a, b, _] : arr)
                    part_of[offsets[a] + now[a]++] =
                    part_of[offsets[b] + now[b]++] = i1++;
            }
            // View of all incident edges of vertex v
            constexpr auto incident_edges(std::size_t v) const {
                return std::views::transform(std::span<const std::size_t>{
                    part_of + offsets[v], offsets[v + 1] - offsets[v] },
                    [&](auto i) -> const Edge& { return edges[i]; });
            }
            // Shortest path from vertex a, implemented using Dijkstra's Algorithm
            constexpr auto shortest_path(std::size_t a) const {
                constexpr auto infinity = std::numeric_limits<double>::max();
                std::array<double, Vertices> dist{}; // distances
                dist.fill(infinity), dist[a] = 0; // itself = 0, rest = infinity
                bool checked[Vertices]{}; // Keep track of checked vertices
                checked[a] = true; // No need to check itself
                for (std::size_t _ = 0; _ < Vertices; ++_) {
                    double len = infinity; std::size_t u = 0;
                    for (std::size_t ux = 0; ux < Vertices; ++ux)
                        if (!checked[ux] && dist[ux] < len)
                            len = dist[ux], u = ux; // Find closest
                    // relax vertex u and its neighbours
                    for (auto& [vx, ux, len] : incident_edges(u)) {
                        auto& v = (vx == u ? ux : vx); // Pick other vertex as v
                        dist[v] = std::min(dist[v], dist[u] + len);
                    }
                    checked[u] = true; // Now we've checked this one
                } // Return all the distances!
                return dist;
            }
            // Minimum spanning tree using Prim's algorithm
            constexpr auto minimum_spanning_tree() const {
                constexpr double infinity = std::numeric_limits<double>::max();
                StackVector<Edge, Vertices - 1> out;
                bool in_w[Vertices]{}; // Keep track of checked vertices 
                StackVector<std::size_t, Vertices> w{}; // and available ones
                w.push_back(0), in_w[0] = true; // Start with v = 0
                for (std::size_t _ = 0; _ < Vertices - 1; ++_) {
                    double len = infinity; std::size_t v = 0, u = 0;
                    for (auto& vx : w) // Loop over incident edges of vertices in w
                        for (auto& [a, b, lenx] : incident_edges(vx)) {
                            auto& ux = (a == vx ? b : a); // Pick other vertex as u
                            // pick smallest edge whose other vertex is not in w
                            if (!in_w[ux] && lenx < len) len = lenx, v = vx, u = ux;
                        }
                    // Add new vertex to w, and add edge to output
                    w.push_back(u), in_w[u] = true; // Add connected vertex to choices
                    out.push_back({ v, u, len }); // Add new edge
                } // Finally construct the new graph!
                return UndirectedGraph<Vertices, Vertices - 1>{ std::move(out) };
            }
};


struct integer {
    int value = 0;
    constexpr integer() = default;
    constexpr integer(int v) : value(v) {};
    constexpr integer(const integer&) = default;
    constexpr integer(integer&&) = default;
    constexpr operator int& () { return value; }
    constexpr operator const int& () const { return value; }
    constexpr operator int() const { return value; }
};

constexpr bool operator|(integer a, integer b) { return b % a == 0; }
constexpr bool operator|(int a, integer b) { return b % a == 0; }
constexpr bool operator|(integer a, int b) { return b % a == 0; }
constexpr integer operator*(integer a, integer b) { return a.value * b.value; }
constexpr integer operator*(int a, integer b) { return a * b.value; }
constexpr integer operator*(integer a, int b) { return a.value * b; }
constexpr integer operator+(integer a, integer b) { return a.value + b.value; }
constexpr integer operator+(int a, integer b) { return a + b.value; }
constexpr integer operator+(integer a, int b) { return a.value + b; }


template<class Ty>
class object {
public:
    using value_type = Ty;
    using super = object<Ty>;
protected:
    class data : public value_type::data {
    public:
        using data_type = typename Ty::data;
        template<class ...Tys> requires (!std::is_aggregate_v<typename Ty::data>) &&
            requires(Tys&&...args) { new typename Ty::data(std::forward<Tys>(args)...); }
        constexpr data(Tys&&...args) : Ty::data(std::forward<Tys>(args)...) {}
        template<class ...Tys> requires (std::is_aggregate_v<typename Ty::data>) &&
            requires(Tys&&...args) { new typename Ty::data{ std::forward<Tys>(args)... }; }
        constexpr data(Tys&&...args) : Ty::data{ std::forward<Tys>(args)... } {}
    private:
        std::size_t ref = 1;
        friend class object<value_type>;
    } *me = nullptr;
public:
    template<class ...Tys> requires
        requires(Tys&&...args) { new data(std::forward<Tys>(args)...); }
    constexpr object(Tys&&...args) : me(new data(std::forward<Tys>(args)...)) {}
    constexpr object(const object& other) : me(other.me) { me->ref++; }
    constexpr object(object&& other) : me(other.me) { other.me = nullptr; }
    constexpr object& operator=(const object& other) { clean(), me = other.me, ++me->ref; return *this; }
    constexpr object& operator=(object&& other) { clean(), me = other.me, other.me = nullptr; return *this; }
    constexpr ~object() { clean(); }
    constexpr data* operator->() { return me; }
    constexpr const data* operator->() const { return me; }
    constexpr std::size_t hash_code() const { return std::bit_cast<std::size_t>(me); }
    template<class T>
    constexpr bool equals(object<T> other) const {
        if constexpr (std::same_as<Ty, T>) return other.me == me; else return false;
    }
private:
    constexpr void clean() {
        if (--me->ref == 0) delete me;
    }
};

//class MyClass : public object<MyClass> {
//    friend class object<MyClass>;
//    struct data {
//        int a = 1;
//        int b = 2;
//    };
//public:
//    constexpr MyClass() {}
//    constexpr MyClass(int a, int b) : super(a, b) {}
//
//    constexpr int sum() const {
//        return me->a + me->b;
//    }
//
//    constexpr void add(MyClass other) {
//        me->a += other->a;
//        me->b += other->b;
//    }
//};
//
//constexpr MyClass sum(MyClass a, MyClass b) {
//    return { a->a + b->a, a->b + b->b };
//}

//template<typename, std::size_t> concept everything = true;
//template<class ...Tys>
//struct Tuple {
//private:
//    constexpr static auto make_offsets() {
//        std::size_t offset = 0;
//        std::array<std::size_t, sizeof...(Tys)> res{};
//        std::size_t* data = res.data();
//        ((*(data++) = offset, offset += sizeof(Tys)), ...);
//        return res;
//    }
//    constexpr static auto bytes = (sizeof(Tys) + ...);
//    constexpr static std::array<std::size_t, sizeof...(Tys)> offsets = make_offsets();
//public:
//    template<std::size_t I>
//    using value_type = std::tuple_element_t<I, std::tuple<Tys...>>;
//
//    template<class ...Args>
//    constexpr Tuple(Args&&...args) {
//        std::size_t offset = 0;
//        ((std::construct_at(&m_Data[offset], std::forward<Args>(args)), offset += sizeof(args)), ...);
//    }
//
//    template<std::size_t I>
//    constexpr value_type<I>& get() { return *std::bit_cast<value_type<I>*>(&m_Data[offsets[I]]); }
//
//    template<std::size_t I>
//    constexpr const value_type<I>& get() const { return (const value_type<I>)(m_Data[offsets[I]]); }
//
//private:
//    uint8_t m_Data[bytes]{};
//};


struct Color {
    float r, g, b, a;
};

//class ColorMap {
//public:
//    ColorMap(std::initializer_list<
//        std::pair<std::string_view, Color>> values) {
//        for (auto& i : values) tree.insert(i.first, i.second);
//    }
//
//    struct Node {
//        ~Node() { if (nodes) delete nodes; }
//        std::array<Node, 26>* nodes = nullptr;
//        Color value{};
//
//        void insert(std::string_view str, const Color& v) {
//            if (str.size() == 0) value = v;
//            else {
//                if (!nodes) nodes = new std::array<Node, 26>{};
//                (*nodes)[str[0] - 'a'].insert(str.substr(1), v);
//            }
//        }
//
//        Color& at(std::string_view str) {
//            return str.size() == 0 ? value : (*nodes)[str[0] - 'a'].at(str.substr(1));
//        }
//    };
//
//    Color& operator[](const std::string& str) {
//        return tree.at(str);
//    }
//
//    Node tree;
//};



//template<std::size_t N>struct S{char v[N-1]{};constexpr S(const char(&x)[N])
//{std::copy_n(x,N-1,v);}constexpr operator std::string_view()const{return{v,N
//-1};}};template<S K,auto V>struct MapVal{constexpr static auto v=V;constexpr
//static auto k=K;};template<class...V>struct Map{using q=std::tuple_element_t
//<0,std::tuple<decltype(V::v)...>>;constexpr static auto y(std::string_view 
//r){auto h=14695981039346656037ULL;for(auto c:r)h=1099511628211ULL*(h^(std::
//size_t)c);return h;}constexpr static auto s=[](){constexpr auto n=[]<auto N,
//auto f>()->std::size_t{constexpr bool c=[](){bool u[N]{};auto i=0ull;return(
//(i=y(V::k)%N,u[i]?1:(u[i]=1,0))||...);}();if constexpr(c)return f.operator()
//<N+1,f>();else return N;};return n.operator()<sizeof...(V),n>();}();constexpr
//static auto d=[](){std::array<const q*,s>res{};return((res[y(V::k)%s]=&V::v
//),...),res;}();constexpr auto&operator[](auto&e)const{return*d[y(e)%s];}};


//template<size_t N>struct S{char v[N-1];constexpr S(const char(&x)[N]){std::
//copy_n(x,N-1,v);}constexpr operator std::string_view()const{return{v,N-1};}};
//template<S K,auto V>struct Val{constexpr static auto v=V;constexpr static auto
//k=K;};template<class...V>struct Map{constexpr static auto y(std::string_view r)
//{auto h=14695981039346656037ULL;for(auto c:r)h=1099511628211ULL*(h^(size_t)c);
//return h;}constexpr static auto s=[]{constexpr auto n=[]<auto N,auto f>(){
//constexpr bool c=[]{bool u[N]{};auto i=0ull;return((i=y(V::k)%N,u[i]?1:(u[i]
//=1,0))||...);}();if constexpr(c)return f.operator()<N+1,f>();else return N;};
//return n.operator()<sizeof...(V),n>();}();constexpr static auto d=[]{std::array
//<std::tuple_element_t<0,std::tuple<decltype(V::v)...>>*,s>res{};return((res[y(V
//::k)%s]=&V::v),...),res;}();constexpr auto&at(auto&e)const{return*d[y(e)%s];}};

#include <numeric>
#include <string_view>
#include <algorithm>
#include <cstddef>
#include <utility>
template<std::size_t N> struct str_t {
    char value[N - 1]{};
    consteval str_t(const char(&val)[N]) { std::copy_n(val, N - 1, value); }
    constexpr operator std::string_view() const { return { value, N - 1 }; }
};
template<str_t Key, auto Val> struct MapVal {
    constexpr static auto val = Val;
    constexpr static auto key = Key;
};
template<class... Vals> struct Map {
    using value_type = std::tuple_element_t<0, std::tuple<decltype(Vals::val)...>>;
    constexpr static std::size_t hash(std::string_view val) { // Constexpr str hash
        return std::accumulate(val.begin(), val.end(), 14695981039346656037ULL,
            [](std::size_t h, char c) { return 1099511628211ULL * (h ^ c); });
    }
    constexpr static auto size = [] { // Minimal size to prevent collisions
        constexpr auto try_n = []<std::size_t N, auto Me>() -> std::size_t {
            constexpr bool has_collision = [] { bool u[N]{}; std::size_t i = 0;
            return ((i = hash(Vals::key) % N, u[i] ? 1 : (u[i] = 1, 0)) || ...);
            }(); // If collision, try 1 more
            if constexpr (has_collision) return Me.operator() < N + 1, Me > ();
            else return N;
        };
        return try_n.operator() < sizeof...(Vals), try_n > ();
    }();
    constexpr static auto data = [] { // Construct the array with values
        std::array<std::pair<const value_type*, std::string_view>, size> res{};
        return ((res[hash(Vals::key) % size] = { &Vals::val, Vals::key }), ...), res;
    }();
    constexpr auto& at(auto& val) const {
        auto& res = data[hash(val) % size];
        if (res.second == val) return res.first; else throw "Value not in Map";
    }
};

template<std::size_t N>
struct tag {
    constexpr static auto size = N - 1;
    char value[N - 1];
    constexpr tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
    constexpr operator std::string_view() const { return { value, N - 1 }; }
    constexpr std::string_view str() const { return { value, N - 1 }; }
    template<std::size_t A>
    constexpr bool operator==(const tag<A>& other) const {
        if constexpr (N != A) return false;
        else return str() == other.str();
    }
};

struct match_result {
    bool match;
    std::string_view remainder;
};

template<tag Name>
struct non_terminal_t {
    constexpr static auto name = Name;

    template<class Grammar>
    constexpr static match_result match(std::string_view str) {
        return Grammar::template non_terminal<non_terminal_t>::template match<Grammar>(str);
    }
};

template<tag Name> constexpr auto nt = non_terminal_t<Name>{};

template<tag Name, auto Lambda = 0>
struct terminal_t {
    constexpr static auto name = Name;

    template<class Grammar>
    constexpr static match_result match(std::string_view str) {
        if constexpr (std::invocable<decltype(Lambda), std::string_view>) {
            return Lambda(str); // if custom parser
        }
        else {
            if (str.starts_with(name.str())) return { true, str.substr(name.size) };
            else return { false, str };
        }
    }
};

template<tag Name, auto Lambda = 0> constexpr auto t = terminal_t<Name, Lambda>{};

template<class ...Tys>
struct and_link {
    template<class Grammar>
    constexpr static match_result match(std::string_view str) {
        match_result res{ true, str };
        bool match = ((res = Tys::template match<Grammar>(res.remainder), res.match) && ...);
        return { match, res.remainder };
    }
};
template<class ...Tys>
struct or_link {
    template<class Grammar>
    constexpr static match_result match(std::string_view str) {
        match_result res{ true, str };
        ((res = Tys::template match<Grammar>(str), res.match) || ...);
        return res;
    }
};

template<class A, class B>
constexpr and_link<A, B> operator*(A, B) { return {}; }
template<class A, class ...B>
constexpr and_link<A, B...> operator*(A, and_link<B...>) { return {}; }
template<class A, class ...B>
constexpr and_link<B..., A> operator*(and_link<B...>, A) { return {}; }

template<class A, class B>
constexpr or_link<A, B> operator|(A, B) { return {}; }
template<class A, class ...B>
constexpr or_link<A, B...> operator|(A, or_link<B...>) { return {}; }
template<class A, class ...B>
constexpr or_link<B..., A> operator|(or_link<B...>, A) { return {}; }

template<tag Name, class Ty>
struct non_terminal_link {
    constexpr static auto link = Name;

    template<class Grammar>
    constexpr static match_result match(std::string_view str) {
        return Ty::template match<Grammar>(str);
    }
};

template<class Nt>
constexpr auto operator<<=(const Nt& b, auto a) {
    return non_terminal_link<Nt::name, decltype(a)>{};
}

template<class ...Tys>
struct grammar {
    using start = kaixo::head_t<std::tuple<Tys...>>;

    template<class Nt, class Ty, class ...Tys> struct find_nt;

    template<class Nt, class Ty, class ...Tys> requires (Nt::name == Ty::link)
        struct find_nt<Nt, Ty, Tys...> { using type = Ty; };

    template<class Nt, class Ty, class ...Tys> requires (!(Nt::name == Ty::link))
        struct find_nt<Nt, Ty, Tys...> : find_nt<Nt, Tys...> {};

    template<class Nt>
    using non_terminal = typename find_nt<Nt, Tys...>::type;

    constexpr grammar(Tys...) {}

    constexpr match_result match(std::string_view str) const {
        match_result res = start::template match<grammar>(str);
        return { res.remainder.size() == 0, res.remainder };
    }
};

#include <list>

void trash() {
    {

        constexpr static auto is_digit = [](char c) { return c >= '0' && c <= '9'; };
        constexpr static auto is_alpha = [](char c) { return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'; };

        constexpr auto MUL_OP = nt<"MULOP">;
        constexpr auto ADD_OP = nt<"ADDOP">;
        constexpr auto POW_OP = nt<"POWOP">;
        constexpr auto number = t < "number", [](std::string_view str) {
            std::string_view res = str;
            std::size_t size = 0;
            while (res.size() > 0 && is_digit(res[0])) res = res.substr(1), ++size;
            return match_result{ size > 0, res };
        } > ;
        constexpr auto variable = t < "variable", [](std::string_view str) {
            std::string_view res = str;
            std::size_t size = 0;
            while (res.size() > 0 && is_alpha(res[0])) res = res.substr(1), ++size;
            return match_result{ size > 0, res };
        } > ;
        constexpr auto PRIMARY = nt<"PRIMARY">;
        constexpr auto EXPR = nt<"EXPR">;
        constexpr auto TERM = nt<"TERM">;
        constexpr auto FACTOR = nt<"FACTOR">;
        constexpr auto S = nt<"S">;

        constexpr grammar g{
            S <<= EXPR,
            EXPR <<= TERM * ADD_OP * EXPR | TERM,
            TERM <<= FACTOR * MUL_OP * TERM | FACTOR,
            FACTOR <<= PRIMARY * POW_OP * FACTOR | PRIMARY,
            PRIMARY <<= number | variable | t<"("> *EXPR * t<")">,
            ADD_OP <<= t<"+"> | t<"-">,
            MUL_OP <<= t<"*"> | t<"/">,
            POW_OP <<= t<"^">,
        };

        constexpr auto m = g.match("(2+2)*8^2");
    }



    // [1, 2, 3, 4, 5, 6, 7]
    //
    //          1
    //      2       3
    //    4   5   6   7
    //

    //constexpr auto agrg = cs.at("yellow");

    //constexpr auto v1 = cs.at("yallow");

    //ColorMap colors{ 
    //    { "red", { 1, 0, 0, 1 } },
    //    { "green", { 0, 1, 0, 1 } },
    //    { "blue", { 0, 0, 1, 1 } },
    //    { "yellow", { 1, 1, 0, 1 } },
    //};

    //auto r1 = colors["red"];
    //auto r2 = colors["green"];
    //auto r3 = colors["blue"];


    // (1 - x)^-n = (1 + x + x^2 + ...)
    // (1 - x)^-2 * (x^2 * (1 + x + x^2 + ...))^3 = (1-x)^-2*x^6*(1-x)^-3=x^6*(1-x)^-5

    std::tuple values{ 0, 1.5, 2.1f, "carrot", 129, "soup" };

    kaixo::tfor(values,
        [](std::floating_point auto& i) {
            std::cout << "floating: " << i << '\n';
        },
        [](std::integral auto& i) {
            std::cout << "integral: " << i << '\n';
        },
            [](auto& i) {
            std::cout << "other:    " << i << '\n';
        });

    return 0;



    constexpr integer a = 2;
    constexpr integer b = 4;
    constexpr integer c = 4;
    constexpr integer x = 8;
    constexpr integer y = 4;

    static_assert(1 | a and a | 0);
    static_assert((a | b) && (b | c) ? a | c : true);
    static_assert((a | b) && (a | c) ? a | (b * x + c * y) : true);

    constexpr auto l = []() {
        int a = 0, b = 1;
        kaixo::tfor<10>([&]<auto I>{
            a += b;
        });
        return a;
    };

    constexpr auto res = l();

    constexpr UndirectedGraph<5, 7> graph{
        Edge{ 0, 1, 45. }, Edge{ 0, 2, 30. },
        Edge{ 1, 2, 15. }, Edge{ 1, 3, 10. },
        Edge{ 1, 4, 20. }, Edge{ 2, 3, 30. },
        Edge{ 3, 4, 35. },
    };

    //constexpr auto sp = graph.shortest_path(0);
    //constexpr auto mst = graph.minimum_spanning_tree();
    //constexpr auto sp1 = mst.shortest_path(0);

    //constexpr auto sp = graph.shortest_path(0);
    //constexpr auto mst = graph.minimum_spanning_tree();
    //constexpr auto sp1 = mst.shortest_path(0);

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

template<size_t V>
struct number {
    constexpr static inline size_t value = V;
};
template<class Type>
struct wrapper {
    using type = Type;
};
template<class ...Types>
struct type_group {
    constexpr static inline size_t count = sizeof...(Types);
    using types = std::tuple<Types...>;
};
template<size_t, class>
struct nth_type_of;
template<size_t N, template<class...> class Type, class ...Types>
struct nth_type_of<N, Type<Types...>> {
    using type = typename std::tuple_element<N, std::tuple<Types...>>::type;
};
template<size_t N, class Group>
using nth_type_of_t = typename nth_type_of<N, Group>::type;

template<class Test, template<class...> class Ref>
struct is_specialization : std::false_type {};

template<template<class...> class Ref, class... Args>
struct is_specialization<Ref<Args...>, Ref> : std::true_type {};

template<class T, template<class...> class S>
concept specialization_of = is_specialization<T, S>::value;

template<class T, class ...Tys>
concept one_of = (std::same_as<Tys, T> || ...);

template<class>
struct member_signature;
template<class Return, class T, class... Args>
struct member_signature<Return(T::*)(Args...) const> {
    using type = Return(Args...);
};
template<class Return, class T, class... Args>
struct member_signature<Return(T::*)(Args...)> {
    using type = Return(Args...);
};
template<class T>
using member_signature_t = typename member_signature<T>::type;

template<class, class = void>
struct lambda_signature;
template<class _Fx>
struct lambda_signature<_Fx, std::void_t<decltype(&_Fx::operator())>> {
    using type = member_signature<decltype(&_Fx::operator())>::type;
};
template<class T>
using lambda_signature_t = typename lambda_signature<T>::type;

template<class>
struct funptr_to_type;
template<class Return, class ...Args>
struct funptr_to_type<Return(*)(Args...)> {
    using type = Return(Args...);
};
template<class T>
using funptr_to_type_t = typename funptr_to_type<T>::type;

template<class, std::size_t>
struct last_n_args;
template<class Return, class ...Args, std::size_t N>
struct last_n_args<Return(Args...), N> {
    template<std::size_t... I>
    static inline Return(*last_n(std::index_sequence<I...>))(std::tuple_element_t<sizeof...(Args) - N + I, std::tuple<Args...>>...) {};
    using type = typename funptr_to_type<decltype(last_n(std::make_index_sequence<N>{})) > ::type;
};
template<class T, std::size_t N>
using last_n_args_t = typename last_n_args<T, N>::type;

template<class, std::size_t>
struct first_n_args;
template<class Return, typename ...Args, std::size_t N>
struct first_n_args<Return(Args...), N> {
    template<std::size_t... I>
    static inline Return(*first_n(std::index_sequence<I...>))(std::tuple_element_t<I, std::tuple<Args...>>...) {};
    using type = typename funptr_to_type<decltype(first_n(std::make_index_sequence<N>{})) > ::type;
};
template<class T, std::size_t N>
using first_n_args_t = typename first_n_args<T, N>::type;

template<class Func, class ...Tys>
concept are_first_n = requires(typename first_n_args<Func, sizeof...(Tys)>::type func, Tys&&...tys) {
    func(std::forward<Tys>(tys)...);
};

void print_struct(auto& data)
{
    std::byte* _bytes = reinterpret_cast<std::byte*>(std::addressof(data));

    constexpr size_t _size = sizeof(decltype(data));
    constexpr size_t _bytesperrow = alignof(decltype(data));

    for (int i = 0; i < _size; i++)
    {
        size_t index = (1 + std::floor(i / _bytesperrow)) * _bytesperrow - (1 + i % _bytesperrow);
        std::bitset<8> _byte = (char8_t)_bytes[index];
        std::cout << _byte << " ";
        if ((i + 1) % _bytesperrow == 0)
            std::cout << std::endl;
    }
}

// group(operation):
//  - Closure: all results of operation -> group
//  - Identity: XI = IX = X
//  - Inverse: AB = BA = I
//  - associativity: (ab)c = a(bc) for all a,b,c
// if:
//  ab = ba for all a,b: commutative (Abelian)

#include <utility>

struct Data {
    int value = 10;

    int Test() { return Test(*this); }
    static int Test(Data self) {
        return self.value;
    }
};


struct clvalue_test {
    template<class Ty>
    constexpr operator const Ty& ();
};

struct rvalue_test {
    template<class Ty>
    constexpr operator Ty && ();
};

struct lvalue_test {
    template<class Ty> requires (!std::is_const_v<Ty>)
        constexpr operator Ty& ();
};

constexpr auto value_tester = [](auto fun) {
    enum { VALUE, CLVALUE, RVALUE, LVALUE, OTHER };
    if constexpr (std::invocable<decltype(fun), rvalue_test>
        && std::invocable<decltype(fun), lvalue_test>) return VALUE;
    else if constexpr (std::invocable<decltype(fun), clvalue_test>) return CLVALUE;
    else if constexpr (std::invocable<decltype(fun), rvalue_test>) return RVALUE;
    else if constexpr (std::invocable<decltype(fun), lvalue_test>) return LVALUE;
    else return OTHER;
};

int test(const int&) { return 0; };

template<class Ty>
struct storage {
    Ty value;
    constexpr operator Ty() const { return value; }
};

template<class Ty> storage(Ty&)->storage<Ty&>;
template<class Ty> storage(const Ty&)->storage<const Ty&>;
template<class Ty> storage(Ty&&)->storage<Ty>;

//template<class F, class ...Tys>
//constexpr auto curry(F&& f, Tys&&... ps) {
//	if constexpr (std::invocable<F, Tys...>) {
//		return f(std::forward<Tys>(ps)...);
//	} else {
//		return [f, ...ps = storage{ std::forward<Tys>(ps) }]
//			<class Arg, class ...Args>(Arg&& arg, Args&&... args) {
//			return curry(f, ps..., storage{ std::forward<Arg>(arg) }, 
//				storage{ std::forward<Args>(args) }...);
//		};
//	}
//}

//template<class F, class ...Tys>
//constexpr auto curry(F&& f, Tys&&... ps) {
//	if constexpr (std::invocable<F, Tys...>) {
//		return f(std::forward<Tys>(ps)...);
//	} else {
//		return [f, ...ps = storage{ std::forward<Tys>(ps) }]
//			<class Arg, class ...Args>(Arg&& arg, Args&&... args) {
//			return curry(f, ps.value..., std::forward<Arg>(arg), 
//				std::forward<Args>(args)...);
//		};
//	}
//}

#include <utility>
#include <tuple>

template<class F, class ...Tys>
constexpr auto curry(F&& f, Tys&&... ps) {
    constexpr bool invocable = requires{ f(std::forward<Tys>(ps)...); };
    if constexpr (invocable) {
        return f(std::forward<Tys>(ps)...);
    }
    else {
        return[f = std::move(f), ps = std::forward_as_tuple(std::forward<Tys>(ps)...)]
            <class Arg, class ...Args>(Arg && arg, Args&&... args) {
            return[f = std::move(f), tuple = std::tuple_cat(std::move(ps), std::tuple<Arg&&, Args&&...>(
                std::forward<Arg>(arg), std::forward<Args>(args)...))]
                <std::size_t ...Is>(std::index_sequence<Is...>) {
                return curry(std::move(f), std::get<Is>(tuple)...);
            }(std::make_index_sequence<sizeof...(Tys) + 1 + sizeof...(Args)>{});
        };
    }
}

//template<class F, class ...Tys>
//constexpr auto curry(F&& f, Tys&&... ps) {
//	if constexpr (std::invocable<F, Tys...>) {
//		return f(std::forward<Tys>(ps)...);
//	} else {
//		return [f, ...ps = ps]
//			<class Arg, class ...Args>(Arg&& arg, Args&&... args) {
//			return curry(f, ps..., std::forward<Arg>(arg), 
//				std::forward<Args>(args)...);
//		};
//	}
//}

constexpr int add(int a, int b, int c, int d) {
    return a + b + c + d;
}

#include <iostream>

struct Ataa {
    int value;
    Ataa(int value = 1) : value(value) { std::cout << "Construct\n"; }
    Ataa(const Ataa& a) : value(a.value) { std::cout << "Copy\n"; }
    Ataa(Ataa&& a) : value(a.value) { std::cout << "Move\n"; }
    ~Ataa() { std::cout << "Destroy\n"; }
};

int add2(const Ataa& a, Ataa& b, Ataa& c, Ataa&& d) {
    return a.value + b.value + c.value + d.value;
}

#include <vector>
#include <array>

template<class Ty, std::size_t Size>
struct finite_group {
    constexpr finite_group(std::array<Ty, Size> elements, Ty(*op)(Ty, Ty))
        : elements(elements), op(op) {

    }

    constexpr const Ty& identity() const {
        for (auto& i : elements) {
            bool done = true;
            for (auto& j : elements) {
                if (op(i, j) != j) {
                    done = false;
                    break;
                }
            }
            if (done) return i;
        }
    }

    std::array<Ty, Size> elements{};
    Ty(*op)(Ty, Ty) = nullptr;
};

#include <numeric>
#include <ranges>

template<class Ty, std::size_t N>
struct permutation {
    std::array<Ty, N> data{};

    constexpr permutation(std::array<Ty, N>&& data) : data(std::move(data)) {}
    constexpr permutation(std::ranges::range auto&& a) {
        std::ranges::copy(std::move(a), data.begin());
    }

    constexpr permutation operator*(const permutation& other) const {
        return std::views::transform(other.data, [&](auto& v) { return data[v]; });
    }
};

template<class Ty, std::size_t N>
struct cycle_notation {
    std::array<std::pair<Ty, Ty>, N> data{};

    constexpr cycle_notation() {}
    constexpr cycle_notation(std::array<std::pair<Ty, Ty>, N>&& data) : data(data) {}
    constexpr cycle_notation(auto&& a) {
        std::ranges::copy(std::move(a), data.begin());
    }

    template<class ...Tys> requires (std::convertible_to<Ty, Tys> && ...)
        constexpr auto operator()(Tys&&... vals) const
        -> cycle_notation<Ty, N + sizeof...(Tys)> {
        std::array<std::pair<Ty, Ty>, N + sizeof...(Tys)> res{};
        std::copy(data.begin(), data.end(), res.begin());
        std::array<Ty, sizeof...(Tys)> in{ static_cast<Ty>(vals)... };
        for (std::size_t i = 0; i < sizeof...(Tys); ++i)
            res[i + N] = { in[i], in[(i + 1) % (sizeof...(Tys))] };
        return { res };
    }

    constexpr operator permutation<Ty, N>() const {
        std::array<Ty, N> res{};
        for (auto& i : data) {
            res[i.first] = i.second;
        }
        return { std::move(res) };
    }
};

template<class Ty, class ...Tys>
constexpr cycle_notation<Ty, sizeof...(Tys) + 1> cycle(Ty&& ty, Tys&&... tys) {
    return cycle_notation<Ty, 0>{}(std::forward<Ty>(ty), std::forward<Tys>(tys)...);
}

template<class Ty> struct json_type {};
template<std::size_t N> struct json_tag {
    char value[N - 1];
    constexpr json_tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
    constexpr operator std::string_view() const { return { value, N - 1 }; }
    constexpr std::string_view str() const { return { value, N - 1 }; }
};

struct json {
    constexpr json(auto...) {}
    constexpr auto operator[](auto) const { return 0; }
};

template<json_tag Name, class Ty>
struct json_member;

constexpr json_tag jm_x{ "x" };
template<class Ty> struct json_member<jm_x, Ty> { Ty x; };
constexpr json_tag jm_y{ "y" };
template<class Ty> struct json_member<jm_y, Ty> { Ty y; };


template<class Ty>
struct vec2 :
    json_member<"x", Ty>,
    json_member<"y", Ty> {};

template<json_tag Name>
struct json_assign {
    template<class Ty>
    constexpr auto operator=(Ty&& val) const {
        return json_member<Name, Ty>{ std::forward<Ty>(val) };
    }
};

//template<json_tag Name>
//constexpr auto operator""m() {
//	return json_assign<Name>{};
//}


constexpr std::size_t ipow(std::size_t x, std::size_t p) {
    if (p == 0) return 1ull;
    else if (p == 1) return x;
    else {
        const auto tmp = ipow(x, p / 2);
        if (p % 2 == 0) return tmp * tmp;
        else return x * tmp * tmp;
    }
}

constexpr std::size_t modpow(std::size_t base, std::size_t power, std::size_t mod) {
    auto view = std::views::iota(0ull, sizeof(power))
        | std::views::transform([](auto v) { return (1ull << v); })
        | std::views::filter([=](auto v) { return power & v; });
    auto op = [=](auto res, auto c) { return (res * (ipow(base, c) % mod)) % mod; };
    return std::accumulate(view.begin(), view.end(), 1ull, op);
}



template <class T, class Tuple, size_t... Is>
constexpr T construct_from_tuple(Tuple& tuple, std::index_sequence<Is...>) {
    return T{ std::get<Is>(tuple)... };
}

template <class T, class Tuple>
constexpr T construct_from_tuple(Tuple& tuple) {
    return construct_from_tuple<T>(tuple,
        std::make_index_sequence<std::tuple_size_v<std::decay_t<Tuple>>>{}
    );
}

// String literal wrapper for template parameter
template<std::size_t N>
struct tag {
    constexpr static auto size = N - 1;
    char value[N - 1];
    constexpr tag(std::string_view view) : value() { std::copy_n(view.data(), N - 1, value); }
    constexpr tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
    constexpr operator std::string_view() const { return { value, N - 1 }; }
    constexpr std::string_view str() const { return { value, N - 1 }; }
    constexpr bool operator==(const tag& other) const { return str() == other.str(); }
    template<std::size_t M> requires (M != N)
        constexpr bool operator==(const tag<M>&) const { return false; }
};



template<bool Parsed, tag Remainder, class Ty = std::string_view>
struct result_type {

    using value_type = Ty;

    constexpr static auto parsed = Parsed;
    constexpr static auto remainder = Remainder;
    Ty value{ };
};

struct dud {};
template<class ...Parsers>
struct and_parser_t {
    constexpr static auto size = sizeof...(Parsers);

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::template value_type;

    template<std::size_t I>
    using parser = std::tuple_element_t<I, std::tuple<Parsers...>>;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        return parse_i<0, Grammar, Str>();
    }

    template<std::size_t I, class Grammar, tag Str>
    consteval static auto parse_i() {
        if constexpr (I == size) return result_type<true, Str, std::tuple<>>{ {} };
        else {
            constexpr auto res = parser<I>::template parse<Grammar, Str>();
            if constexpr (res.parsed) {
                constexpr auto recu = parse_i<I + 1, Grammar, res.remainder>();
                if constexpr (!recu.parsed) return result_type<false, Str>{};
                else {
                    constexpr auto tuple = std::tuple_cat(std::tuple{ res.value }, recu.value);
                    return result_type<true, recu.remainder, decltype(tuple)>{ std::move(tuple) };
                }
            }
            else return result_type<false, Str>{};
        }
    }
};

template<class ...Parsers>
struct or_parser_t {
    constexpr static auto size = sizeof...(Parsers);

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::template value_type;

    template<std::size_t I>
    using parser = std::tuple_element_t<I, std::tuple<Parsers...>>;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        return parse_i<0, Grammar, Str>();
    }

    template<std::size_t I, class Grammar, tag Str>
    consteval static auto parse_i() {
        if constexpr (I == size) return result_type<false, Str>{ };
        else {
            constexpr auto res = parser<I>::template parse<Grammar, Str>();
            if constexpr (res.parsed) return res;
            else return parse_i<I + 1, Grammar, Str>();
        }
    }
};

template<tag Name, class Parser>
struct named_parser {
    constexpr static auto name = Name;
    using parser = Parser;

    template<class Grammar, tag Str>
    using value_type = Parser::template value_type<Grammar, Str>;

    template<class Parser, tag Str>
    consteval static auto parse() {
        return parser::template parse<Parser, Str>();
    }
};

template<tag Name, auto Lambda = 0>
struct token {
    constexpr static auto name = Name;

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::value_type;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        if constexpr (std::same_as<int, decltype(Lambda)>) {
            if constexpr (Str.str().starts_with(Name.str())) {
                constexpr auto size = Str.size - Name.size;
                if constexpr (size == 0)
                    return result_type<true, "\0", value_type<Grammar, Str>>{ Name.str() };
                else {
                    constexpr auto new_str = Str.str().substr(Name.size);
                    return result_type < true, tag<size + 1>{ new_str }, value_type<Grammar, Str >> { Name.str() };
                }
            }
            else return result_type<false, Str>{};
        }
        else return Lambda.template operator() < Grammar, Str > ();
    }
};

template<tag Name>
struct non_terminal {
    constexpr static auto name = Name;

    template<class Grammar, tag Str>
    using value_type = std::decay_t<decltype(Grammar::template get<Name>)>::template value_type<Grammar, Str>;

    template<class Parser>
    consteval auto operator<<=(Parser) const { return named_parser<Name, Parser>{}; }

    template<class Grammar, tag Str>
    consteval static auto parse() {
        return Grammar::template get<Name>.template parse<Grammar, Str>();
    }
};

template<class Ty, class Parser>
struct typed_parser {
    using type = Ty;
    using parser = Parser;

    template<class Grammar, tag Str>
    using value_type = typename decltype(parse<Grammar, Str>())::template value_type<Grammar, Str>;

    template<class Grammar, tag Str>
    consteval static auto parse() {
        constexpr auto res = Parser::template parse<Grammar, Str>();
        if constexpr (res.parsed)
            return result_type<true, res.remainder, type>{ construct(res.value) };
        else return result_type<false, Str>{};
    }

    template<class Ty>
    consteval static auto construct(const Ty& value) {
        if constexpr (kaixo::specialization<Ty, std::tuple>)
            return construct_from_tuple<type>(value);
        else return type{ value };
    }
};

template<auto ...Parsers>
struct parser {
    template<tag Name>
    static constexpr auto get_impl(auto& P, auto&... Ps) {
        if constexpr (std::decay_t<decltype(P)>::name == Name) return P;
        else return get_impl<Name>(Ps...);
    }

    template<tag Name>
    static constexpr auto get = get_impl<Name>(Parsers...);

    template<tag Name, tag Str>
    consteval static auto parse() {
        return get<Name>.template parse<parser, Str>();
    }
};

template<class A, class B> requires (!kaixo::specialization<A, and_parser_t> && !kaixo::specialization<B, and_parser_t>)
consteval auto operator*(A, B) { return and_parser_t<A, B>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, and_parser_t>)
consteval auto operator*(and_parser_t<Bs...>, A) { return and_parser_t<Bs..., A>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, and_parser_t>)
consteval auto operator*(A, and_parser_t<Bs...>) { return and_parser_t<A, Bs...>{}; }

template<class A, class B> requires (!kaixo::specialization<A, or_parser_t> && !kaixo::specialization<B, or_parser_t>)
consteval auto operator|(A, B) { return or_parser_t<A, B>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, or_parser_t>)
consteval auto operator|(or_parser_t<Bs...>, A) { return or_parser_t<Bs..., A>{}; }
template<class A, class ...Bs> requires (!kaixo::specialization<A, or_parser_t>)
consteval auto operator|(A, or_parser_t<Bs...>) { return or_parser_t<A, Bs...>{}; }

template<class Ty>
struct type_t {
    using type = Ty;
};

template<class Ty>
constexpr auto t = type_t<Ty>{};

template<class A, class B>
consteval auto operator^(type_t<A>, B) {
    return typed_parser<A, B>{};
}


#include <vector>
#include <iostream>

#include <numbers>

namespace kaixo {
    enum class AngleType { Degrees, Radians };

    namespace detail {
        using namespace std::numbers;
        template<AngleType A, AngleType B, class Out, class In>
        constexpr decltype(auto) convert(const In& v) {
            if constexpr (A == B) {
                if constexpr (std::same_as<std::decay_t<In>, std::decay_t<Out>>) return v;
                else return static_cast<Out>(v);
            }
            else if (A == AngleType::Radians) return static_cast<Out>(180. * v / pi_v<double>);
            else if (A == AngleType::Degrees) return static_cast<Out>(pi_v<double> *v / 180.);
        }
    }

    template<class Ty, AngleType Vy = AngleType::Radians>
    class AngleBase {
    public:
        constexpr AngleBase() {}
        template<std::convertible_to<Ty> T>
        constexpr AngleBase(const T& v) : m_Value(static_cast<Ty>(v)) {}
        template<std::convertible_to<Ty> T, AngleType V>
        constexpr AngleBase(AngleBase<T, V> v) : m_Value(detail::convert<V, Vy, Ty>(v.value())) {}

        constexpr decltype(auto) degrees() { return detail::convert<Vy, AngleType::Degrees, Ty>(value()); }
        constexpr decltype(auto) radians() { return detail::convert<Vy, AngleType::Radians, Ty>(value()); }
        constexpr decltype(auto) degrees() const { return detail::convert<Vy, AngleType::Degrees, Ty>(value()); }
        constexpr decltype(auto) radians() const { return detail::convert<Vy, AngleType::Radians, Ty>(value()); }

        constexpr Ty& value() { return m_Value; }
        constexpr Ty const& value() const { return m_Value; }

        constexpr explicit operator AngleBase<Ty, AngleType::Degrees>() const { return degrees(); }
        constexpr explicit operator AngleBase<Ty, AngleType::Radians>() const { return radians(); }

        constexpr explicit operator Ty& () { return value(); }
        constexpr explicit operator Ty const& () const { return value(); }

    protected:
        Ty m_Value{};
    };

    template<class Ty> using Degrees = AngleBase<Ty, AngleType::Degrees>;
    template<class Ty> using Radians = AngleBase<Ty, AngleType::Radians>;
}


template<class Ty>
concept range_type = requires (Ty a, Ty b) {
    { a == b } -> std::convertible_to<bool>;
    { ++a } -> std::convertible_to<Ty>;
    { a + b } -> std::convertible_to<Ty>;
    { a = a + b } -> std::convertible_to<Ty>;
};

template<range_type Ty>
struct range {
    using value_type = Ty;
    using reference = Ty&;
    using const_reference = const Ty&;
    using difference_type = std::ptrdiff_t;
    using size_type = std::size_t;

    class iterator {
    public:
        using value_type = Ty;
        using reference = Ty&;
        using const_reference = const Ty&;
        using difference_type = std::ptrdiff_t;
        using size_type = std::size_t;

        constexpr iterator(Ty value, const range* r) : m_Value(value), m_Range(r) {}

        constexpr iterator& operator++() { m_Value = m_Value + m_Range->difference; return *this; }
        constexpr Ty const& operator*() const { return m_Value; }
        constexpr bool operator==(const iterator& other) const { return other.m_Value == m_Value; }

    private:
        Ty m_Value;
        const range* m_Range;
    };

    using const_iterator = iterator;

    constexpr iterator begin() const { return { a, this }; }
    constexpr iterator end() const { return { b, this }; }

    Ty a{};
    Ty b{};
    Ty difference{ [] { Ty v{}; return ++v; }() };
};

template<class Ty> range(Ty, Ty)->range<Ty>;
template<class Ty> range(Ty, Ty, Ty)->range<Ty>;

#include <map>

struct State {
    void* data{ nullptr };

    template<class Ty> void assign() {
        if constexpr (sizeof(Ty) <= sizeof data)
            new (&data) Ty{};
        else data = new Ty;
    }

    template<class Ty> Ty const& get() const {
        if constexpr (sizeof(Ty) <= sizeof data)
            return *static_cast<Ty*>((void*)&data);
        else return *static_cast<Ty*>(data);
    }

    template<class Ty> Ty& get() {
        if constexpr (sizeof(Ty) <= sizeof data)
            return *static_cast<Ty*>((void*)&data);
        else return *static_cast<Ty*>(data);
    }

    template<class Ty> void clean() {
        delete static_cast<Ty*>(data);
    }
};

template<class Ty>
struct StateId {
    std::string_view id;
    constexpr const char* get() const { return id.data(); }
    constexpr bool operator==(std::string_view view) const {
        return id.data() == view.data();
    }
    constexpr friend bool operator==(std::string_view view, const StateId& id) {
        return id.id.data() == view.data();
    }
};

struct StateListener {
    virtual void update(std::string_view id, State& state) = 0;
};

class Object {
public:
    template<class Ty>
    Ty get(const StateId<Ty>& id) const {
        if (!m_States.contains(id.get()))
            return {};
        auto const& _state = m_States.at(id.get());
        return _state.get<Ty>();
    }

    template<class Ty, std::convertible_to<Ty> T>
    Ty set(const StateId<Ty>& id, T&& val) {
        if (!m_States.contains(id.get()))
            m_States[id.get()].assign<Ty>();
        auto& _state = m_States[id.get()];
        _state.get<Ty>() = std::forward<T>(val);
        for (auto& i : m_Listeners)
            i->update(id.id, _state);
        return _state.get<Ty>();
    }

    template<std::derived_from<StateListener> Ty>
    void link(Ty& l) {
        m_Listeners.push_back(dynamic_cast<StateListener*>(&l));
    }

private:
    std::map<const char*, State> m_States{};
    std::vector<StateListener*> m_Listeners;
};

constexpr StateId<bool> Hovering{ "Hovering" };
constexpr StateId<bool> Focused{ "Focused" };
constexpr StateId<int> Pressed{ "Pressed" };

struct MyObject : public Object, public StateListener {

    MyObject() {
        link(*this);
    }

    void update(std::string_view id, State& state) override {

        if (Hovering == id) {
            std::cout << state.get<bool>() << "\n";
        }

        return;
    }

};

template<class Ty> struct delegate;

template<class R, class ...A>
struct delegate<R(A...)> {

    template<class Obj>
    constexpr delegate(Obj& o, R(Obj::* fun)(A...))
        : object_ptr(static_cast<void*>(&o)),
        fun_ptr(mem_method<Obj>) {
        new (&storage) mem_storage<Obj>{ fun };
    }

    template<std::invocable<A...> Lambda>
        requires (sizeof(Lambda) <= sizeof(void*)
    && !std::convertible_to<Lambda, R(*)(A...)>)
        constexpr delegate(Lambda lambda)
        : fun_ptr(small_lambda_method<Lambda>) {
        new (&object_ptr) Lambda{ std::move(lambda) };
    }

    template<std::invocable<A...> Lambda>
        requires (sizeof(Lambda) > sizeof(void*)
    && !std::convertible_to<Lambda, R(*)(A...)>)
        constexpr delegate(Lambda lambda)
        : fun_ptr(big_lambda_method<Lambda>),
        cleanup(cleanup_method<Lambda>) {
        object_ptr = new Lambda{ std::move(lambda) };
    }

    template<std::invocable<A...> Lambda>
        requires (std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate(Lambda lambda)
        : fun_ptr(fun_ptr_method) {
        new (&storage) (R(*)(A...)){ (R(*)(A...)) lambda };
    }

    template<class Obj>
    constexpr delegate& operator=(std::pair<Obj&, R(Obj::*)(A...)> a) {
        clean();
        object_ptr = static_cast<void*>(&a.first);
        new (&storage) mem_storage<Obj>{ a.second };
        fun_ptr = mem_method<Obj>;
        return *this;
    }

    template<std::invocable<A...> Lambda>
        requires (sizeof(Lambda) <= sizeof(void*)
    && !std::convertible_to<Lambda, R(*)(A...)>)
        constexpr delegate& operator=(Lambda lambda) {
        clean();
        fun_ptr = small_lambda_method<Lambda>;
        new (&object_ptr) Lambda{ std::move(lambda) };
        return *this;
    }

    template<std::invocable<A...> Lambda>
        requires (sizeof(Lambda) > sizeof(void*)
    && !std::convertible_to<Lambda, R(*)(A...)>)
        constexpr delegate& operator=(Lambda lambda) {
        clean();
        fun_ptr = big_lambda_method<Lambda>;
        object_ptr = new Lambda{ std::move(lambda) };
        cleanup = cleanup_method<Lambda>;
        return *this;
    }

    template<std::invocable<A...> Lambda>
        requires (std::convertible_to<Lambda, R(*)(A...)>)
    constexpr delegate& operator=(Lambda lambda) {
        clean();
        fun_ptr = fun_ptr_method;
        new (&storage) (R(*)(A...)){ (R(*)(A...)) lambda };
        return *this;
    }

    template<std::convertible_to<A> ...Args>
    constexpr R operator()(Args...args) {
        return (*fun_ptr)(object_ptr, storage, args...);
    }

    constexpr ~delegate() { clean(); }

private:
    template<class Obj>
    struct mem_storage {
        R(Obj::* fun)(A...);
    };
    struct dummy {};
    constexpr static auto mem_fun_size = sizeof(mem_storage<dummy>);

    void* object_ptr = nullptr;
    R(*fun_ptr)(void*, uint8_t(&)[mem_fun_size], A...);
    uint8_t storage[mem_fun_size]{};
    void(*cleanup)(void*) = nullptr;

    constexpr void clean() {
        if (cleanup) {
            cleanup(object_ptr);
            cleanup = nullptr;
        }
    }

    template<class Obj>
    constexpr static void cleanup_method(void* obj) {
        delete static_cast<Obj*>(obj);
    }

    template<class Obj>
    constexpr static R mem_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (static_cast<Obj*>(o)->*reinterpret_cast<mem_storage<Obj>*>(&fun)->fun)(args...);
    }

    template<class Lambda>
    constexpr static R small_lambda_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (*reinterpret_cast<Lambda*>(&o))(args...);
    }

    template<class Lambda>
    constexpr static R big_lambda_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (*static_cast<Lambda*>(o))(args...);
    }

    constexpr static R fun_ptr_method(void* o, uint8_t(&fun)[mem_fun_size], A...args) {
        return (*reinterpret_cast<R(**)(A...)>(&fun))(args...);
    }
};

struct Type {
    int thing(int a) {
        std::cout << a << '\n';
        return a;
    }
};

struct Functor {
    int operator()(int a) { return a + 1; }
};
struct Functor2 {
    int m1 = 10;
    int m2 = 20;
    int m3 = 30;
    int operator()(int a) { return a + m1 + m2 + m3; }
};

#include <functional>


template<std::size_t N, class CharType = char>
struct string_literal {
    constexpr static std::size_t npos = std::basic_string_view<CharType>::npos;

    using view_type = std::basic_string_view<CharType>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using value_type = CharType;
    using reference = CharType&;
    using const_reference = const CharType&;
    using pointer = CharType*;
    using const_pointer = const CharType*;

    class const_iterator {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using value_type = const CharType;
        using reference = const CharType&;

        constexpr const_iterator(const const_iterator&) = default;
        constexpr const_iterator(const_iterator&&) = default;
        constexpr const_iterator& operator=(const const_iterator&) = default;
        constexpr const_iterator& operator=(const_iterator&&) = default;
        constexpr const_iterator() : m_Ptr(nullptr) {}
        constexpr const_iterator(const CharType* ptr) : m_Ptr(ptr) {}

        constexpr reference operator*() const { return *m_Ptr; }
        constexpr const_iterator& operator+=(difference_type d) { m_Ptr += d; return *this; }
        constexpr const_iterator& operator-=(difference_type d) { m_Ptr -= d; return *this; }
        constexpr const_iterator& operator++() { ++m_Ptr; return *this; }
        constexpr const_iterator& operator--() { --m_Ptr; return *this; }
        constexpr const_iterator operator++(int) { auto _c = *this; ++m_Ptr; return _c; }
        constexpr const_iterator operator--(int) { auto _c = *this; --m_Ptr; return _c; }

        constexpr reference operator[](difference_type d) const { return m_Ptr[d]; }

        constexpr auto operator<=>(const const_iterator& other) const = default;

        friend constexpr const_iterator operator+(difference_type a, const const_iterator& b) { return { a + b.m_Ptr }; }
        friend constexpr const_iterator operator+(const const_iterator& a, difference_type b) { return { a.m_Ptr + b }; }
        friend constexpr const_iterator operator-(difference_type a, const const_iterator& b) { return { a - b.m_Ptr }; }
        friend constexpr const_iterator operator-(const const_iterator& a, difference_type b) { return { a.m_Ptr - b }; }
        friend constexpr difference_type operator-(const const_iterator& a, const const_iterator& b) { return a.m_Ptr - b.m_Ptr; }
    protected:
        const CharType* m_Ptr;
    };

    class iterator : public const_iterator {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using value_type = CharType;
        using reference = CharType&;

        constexpr iterator(const iterator&) = default;
        constexpr iterator(iterator&&) = default;
        constexpr iterator& operator=(const iterator&) = default;
        constexpr iterator& operator=(iterator&&) = default;
        constexpr iterator() : const_iterator(nullptr) {}
        constexpr iterator(CharType* ptr) : const_iterator(ptr) {}

        constexpr reference operator*() const { return *const_cast<CharType*>(this->m_Ptr); }
        constexpr iterator& operator+=(difference_type d) { this->m_Ptr += d; return *this; }
        constexpr iterator& operator-=(difference_type d) { this->m_Ptr -= d; return *this; }
        constexpr iterator& operator++() { ++this->m_Ptr; return *this; }
        constexpr iterator& operator--() { --this->m_Ptr; return *this; }
        constexpr iterator operator++(int) { auto _c = *this; ++this->m_Ptr; return _c; }
        constexpr iterator operator--(int) { auto _c = *this; --this->m_Ptr; return _c; }

        constexpr reference operator[](difference_type d) const { return const_cast<CharType*>(this->m_Ptr)[d]; }

        constexpr auto operator<=>(const iterator& other) const = default;

        friend constexpr iterator operator+(difference_type a, const iterator& b) { return { a + b.m_Ptr }; }
        friend constexpr iterator operator+(const iterator& a, difference_type b) { return { a.m_Ptr + b }; }
        friend constexpr iterator operator-(difference_type a, const iterator& b) { return { a - b.m_Ptr }; }
        friend constexpr iterator operator-(const iterator& a, difference_type b) { return { a.m_Ptr - b }; }
        friend constexpr difference_type operator-(const iterator& a, const iterator& b) { return a.m_Ptr - b.m_Ptr; }
    };

    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    constexpr ~string_literal() = default;
    constexpr string_literal() = default;
    constexpr string_literal(const CharType(&data)[N]) {
        std::copy_n(data, N, m_Data);
    }

    constexpr string_literal(string_literal&&) = default;
    constexpr string_literal(const string_literal&) = default;
    constexpr string_literal& operator=(string_literal&&) = default;
    constexpr string_literal& operator=(const string_literal&) = default;

    template<std::size_t I> requires (I < N)
        constexpr string_literal& operator=(const CharType(&data)[I]) {
        std::copy_n(data, I, m_Data);
    }

    constexpr iterator begin() { return { m_Data }; }
    constexpr iterator end() { return { m_Data + size() }; }
    constexpr const_iterator begin() const { return { m_Data }; }
    constexpr const_iterator end() const { return { m_Data + size() }; }
    constexpr const_iterator cbegin() const { return begin(); }
    constexpr const_iterator cend() const { return end(); }
    constexpr reverse_iterator rbegin() { return end(); }
    constexpr reverse_iterator rend() { return begin(); }
    constexpr const_reverse_iterator rbegin() const { return end(); }
    constexpr const_reverse_iterator rend() const { return begin(); }
    constexpr const_reverse_iterator crbegin() const { return end(); }
    constexpr const_reverse_iterator crend() const { return begin(); }

    constexpr reference at(size_type d) { return m_Data[d]; }
    constexpr const_reference at(size_type d) const { return m_Data[d]; }
    constexpr reference operator[](size_type d) { return m_Data[d]; }
    constexpr const_reference operator[](size_type d) const { return m_Data[d]; }
    constexpr reference front() { return m_Data[0]; }
    constexpr const_reference front() const { return m_Data[0]; }
    constexpr reference back() { return m_Data[size() - 1]; }
    constexpr const_reference back() const { return m_Data[size() - 1]; }

    constexpr pointer data() { return m_Data; }
    constexpr const_pointer data() const { return m_Data; }
    constexpr const_pointer c_str() const { return m_Data; }
    constexpr size_type size() const { return N - 1; }
    constexpr size_type length() const { return size(); }
    constexpr size_type max_size() const { return size(); }
    constexpr bool empty() const { return false; }
    constexpr void swap(string_literal& other) { std::swap(data(), other.data()); }

    constexpr view_type view() const { return { data(), size() }; }
    constexpr operator view_type() const { return { data(), size() }; }

    template<std::size_t I>
    constexpr auto operator==(const string_literal<I, CharType>& other) const {
        if constexpr (I != N) return false;
        else return view() == other.view();
    };
    template<std::size_t I>
    constexpr auto operator<=>(const string_literal<I, CharType>& other) const { return view() <=> other.view(); }

    constexpr auto starts_with(view_type t) const { return view().starts_with(t); }
    constexpr auto ends_with(view_type t) const { return view().ends_with(t); }
    constexpr auto substr(size_type pos = 0, size_type count = npos) const { return view().substr(pos, count); }
    constexpr auto find(std::string_view t, size_type pos = 0) const { return view().find(t, pos); }
    constexpr auto rfind(view_type t, size_type pos = 0) const { return view().rfind(t, pos); }
    constexpr auto find_first_of(view_type t, size_type pos = 0) const { return view().find_first_of(t, pos); }
    constexpr auto find_first_not_of(view_type t, size_type pos = 0) const { return view().find_first_not_of(t, pos); }
    constexpr auto find_last_of(view_type t, size_type pos = 0) const { return view().find_last_of(t, pos); }
    constexpr auto find_last_not_of(view_type t, size_type pos = 0) const { return view().find_last_not_of(t, pos); }

    CharType m_Data[N]{};
};


#include <array>

template<string_literal Str>
struct meta {
    constexpr static auto definition = Str;

    struct member {
        std::string_view name{};
    };

    constexpr static std::size_t member_count = []() {
        std::size_t _count = 0;
        for (auto& i : definition) if (i == ';') ++_count;
        return _count;
    }();

    constexpr static std::array<member, member_count> members = []() {
        std::array<member, member_count> _members{};
        auto _view = definition.view();

        std::size_t _index = 0;
        while (true) {
            auto _end = _view.find_first_of(';');
            if (_end == decltype(definition)::npos) break;
            auto _sub = _view.substr(0, _end);
            auto _last = _sub.find_last_of(' ');
            auto _name = _sub.substr(_last + 1);
            _members[_index++].name = _name;
            _view = _view.substr(_end + 1);
        }

        return _members;
    }();
};

template<auto Ptr, string_literal Name>
struct Member {
    constexpr static auto ptr = Ptr;
    constexpr static auto name = Name;
};

template<class Type>
struct Meta {
    template<class Ty>
    struct Member {
        Type::Ty* ptr;
        std::string_view name;
    };
};


#define META(Name)  struct Name {                                            \
    using Type = Name;                                                       \
    template<auto Ptr, string_literal Name>                                  \
    struct Variable {                                                        \
        using type = kaixo::member_type_t<decltype(Ptr)>;                    \
        constexpr static auto _n_Type = 0;                                   \
        constexpr static auto ptr = Ptr;                                     \
        constexpr static auto name = Name.view();                            \
    };                                                                       \
    template<class> struct SignaturedFunction;                               \
    struct GeneralFunction {                                                 \
        virtual void _n_Link() = 0;                                          \
    };                                                                       \
    template<class R, class ...As>                                           \
    struct SignaturedFunction<R(As...)> : GeneralFunction {                  \
        virtual void _n_Link() override {};                                  \
        constexpr SignaturedFunction(R(*fun)(Type&, As...)) : fun(fun) {}    \
        R(*fun)(Type&, As...);                                               \
    };                                                                       \
    struct FunctionBase {                                                    \
        const GeneralFunction* fun;                                          \
        template<class Ret, class ...Args>                                   \
        constexpr Ret invoke(Type& v, Args...args) const {                   \
            auto _self = dynamic_cast<const Type::template                   \
                SignaturedFunction<Ret(Args...)>*>(fun);                     \
            if (!_self) throw "Wrong call!";                                 \
            else return _self->fun(v, args...);                              \
        }                                                                    \
        template<class Ret, class ...Args>                                   \
        constexpr Ret invoke(const Type& v, Args...args) const {             \
            auto _self = dynamic_cast<const Type::template                   \
                SignaturedFunction<Ret(Args...)>*>(fun);                     \
            if (!_self) throw "Wrong call!";                                 \
            else return _self->fun(v, args...);                              \
        }                                                                    \
        template<class Sig>                                                  \
        constexpr bool invocable() const {                                   \
            auto _self = dynamic_cast<const Type::template                   \
                SignaturedFunction<Sig>*>(fun);                              \
            return _self != nullptr;                                         \
        }                                                                    \
    };                                                                       \
    struct GeneralVariable {                                                 \
        virtual void _n_Link() = 0;                                          \
    };                                                                       \
    template<class Ty>                                                       \
    struct TypedVariable : GeneralVariable {                                 \
        virtual void _n_Link() override {};                                  \
        constexpr TypedVariable(Ty Type::* var) : var(var) {}                \
        Ty Type::* var;                                                      \
    };                                                                       \
    struct VariableBase {                                                    \
        const GeneralVariable* var;                                          \
        template<class Ty>                                                   \
        constexpr Ty& as(Type& v) const {                                    \
            auto _self = dynamic_cast<const Type::template                   \
                TypedVariable<Ty>*>(var);                                    \
            if (!_self) throw "Wrong call!";                                 \
            else return v.*(_self->var);                                     \
        }                                                                    \
        template<class Ty>                                                   \
        constexpr const Ty& as(const Type& v) const {                        \
            auto _self = dynamic_cast<const Type::template                   \
                TypedVariable<Ty>*>(var);                                    \
            if (!_self) throw "Wrong call!";                                 \
            else return v.*(_self->var);                                     \
        }                                                                    \
        template<class Ty>                                                   \
        constexpr bool is() const {                                          \
            auto _self = dynamic_cast<const Type::template                   \
                TypedVariable<Ty>*>(var);                                    \
            return _self != nullptr;                                         \
        }                                                                    \
    };                                                                       \
    template<auto Ptr, string_literal Name>                                  \
    struct Function {                                                        \
        constexpr static auto _n_Type = 1;                                   \
        constexpr static auto ptr = Ptr;                                     \
        constexpr static auto name = Name.view();                            \
        using signature = kaixo::signature_t<decltype(ptr)>;                 \
        using return_type = kaixo::function_return_t<signature>;             \
        using argument_types = kaixo::function_args_t<signature>;            \
        template<class ...Args>                                              \
        constexpr static decltype(auto) invoke(Type& v, Args&&...as) {       \
            return (v.*ptr)(std::forward<Args>(as)...);                      \
        }                                                                    \
        template<class ...Args>                                              \
        constexpr static decltype(auto) invoke(const Type& v, Args&&...as) { \
            return (v.*ptr)(std::forward<Args>(as)...);                      \
        }                                                                    \
    };                                                                       \
    template<auto Ptr, string_literal Name>                                  \
        requires (std::is_member_object_pointer_v<decltype(Ptr)>)            \
    struct Function<Ptr, Name> {                                             \
        constexpr static auto _n_Type = 1;                                   \
        constexpr static auto ptr = Ptr;                                     \
        constexpr static auto name = Name.view();                            \
        template<class ...Args>                                              \
        constexpr static decltype(auto) invoke(Type& v, Args&&...as) {       \
            return (v.*ptr)(std::forward<Args>(as)...);                      \
        }                                                                    \
        template<class ...Args>                                              \
        constexpr static decltype(auto) invoke(const Type& v, Args&&...as) { \
            return (v.*ptr)(std::forward<Args>(as)...);                      \
        }                                                                    \
    };                                                                       \
    template<std::size_t I> struct _n_Members { using type = void; };        \
    constexpr static std::size_t _n_Start = __COUNTER__;                     \

#define END_META                                                                          \
    template<std::size_t I> using _n_Members_t = _n_Members<I>::type;                     \
    constexpr static std::size_t _n_End = __COUNTER__;                                    \
    constexpr static std::size_t _n_Member_count = _n_End - _n_Start - 1;                 \
    constexpr static std::size_t variable_count =                                         \
    []<std::size_t ...Is>(std::index_sequence<Is...>){                                    \
        std::size_t _count = 0;                                                           \
        ((Type::template _n_Members_t<Is>::_n_Type == 0                                   \
            ? (++_count, true) : false), ...);                                            \
        return _count;                                                                    \
    }(std::make_index_sequence<Type::_n_Member_count>{});                                 \
    constexpr static std::size_t function_count =                                         \
    []<std::size_t ...Is>(std::index_sequence<Is...>){                                    \
        std::size_t _count = 0;                                                           \
        ((Type::template _n_Members_t<Is>::_n_Type == 1                                   \
            ? (++_count, true) : false), ...);                                            \
        return _count;                                                                    \
    }(std::make_index_sequence<Type::_n_Member_count>{});                                 \
    template<std::size_t I> using variable = decltype(                                    \
    [](){                                                                                 \
        constexpr auto try_n = []                                                         \
        <std::size_t N, std::size_t M>(auto& s) {                                         \
            if constexpr (std::same_as<void, Type::template _n_Members_t<N>>) return 0;   \
            else if constexpr (Type::template _n_Members_t<N>::_n_Type == 0) {            \
                if constexpr (M == 0) return Type::template _n_Members_t<N>{};            \
                else return s.operator()<N + 1, M - 1>(s);                                \
            } else return s.operator()<N + 1, M>(s);                                      \
        };                                                                                \
        return try_n.operator()<0, I>(try_n);                                             \
    }());                                                                                 \
    template<std::size_t I> using function = decltype(                                    \
    [](){                                                                                 \
        constexpr auto try_n = []                                                         \
        <std::size_t N, std::size_t M>(auto& s) {                                         \
            if constexpr (std::same_as<void, Type::template _n_Members_t<N>>) return 0;   \
            else if constexpr (Type::template _n_Members_t<N>::_n_Type == 1) {            \
                if constexpr (M == 0) return Type::template _n_Members_t<N>{};            \
                else return s.operator()<N + 1, M - 1>(s);                                \
            } else return s.operator()<N + 1, M>(s);                                      \
        };                                                                                \
        return try_n.operator()<0, I>(try_n);                                             \
    }());                                                                                 \
    template<std::size_t I, class ...As>                                                  \
    constexpr static decltype(auto) _n_Fun_type_t(Type & v, As ...as) {                   \
        return (v.*(Type::template function<I>::ptr))(as...);                             \
    };                                                                                    \
    template<std::size_t I>                                                               \
    constexpr static auto _n_Fun_types =                                                  \
        Type::template SignaturedFunction<kaixo::minimal_signature_t                      \
            <decltype(Type::template function<I>::ptr)>>{ _n_Fun_type_t<I> };             \
    constexpr static auto functions() {                                                   \
        return []<std::size_t ...Is>(std::index_sequence<Is...>) {                        \
            return std::array<FunctionBase, Type::function_count>{                        \
            dynamic_cast<const GeneralFunction*>(&Type::template _n_Fun_types<Is>)... };  \
        }(std::make_index_sequence<Type::function_count>{});                              \
    }                                                                                     \
    template<std::size_t I>                                                               \
    constexpr static auto _n_Var_types =                                                  \
        Type::template TypedVariable<typename Type::template variable<I>::type>{          \
            Type::template variable<I>::ptr };                                            \
    constexpr static auto variables() {                                                   \
        return []<std::size_t ...Is>(std::index_sequence<Is...>) {                        \
            return std::array<VariableBase, Type::variable_count>{                        \
            dynamic_cast<const GeneralVariable*>(&Type::template _n_Var_types<Is>)... };  \
        }(std::make_index_sequence<Type::variable_count>{});                              \
    }                                                                                     \
};

#define VAR(ty, name, init) ty name init; \
template<> struct _n_Members<__COUNTER__ - _n_Start - 1> { using type = Variable<&Type::name, #name>; };
#define FUN(pre, name, def) pre name def; \
template<> struct _n_Members<__COUNTER__ - _n_Start - 1> { using type = Function<&Type::name, #name>; };
#define TFUN(pre, name, def) pre name def; \
struct _twrapper_##name {                                        \
    constexpr _twrapper_##name(Type& self) : self(self) {}       \
    Type& self;                                                  \
    template<class ...Args> requires                             \
    requires(Type& self, Args&&...args) {                        \
        { self.name(std::forward<Args>(args)...) };              \
    }                                                            \
    constexpr decltype(auto) operator()(Args&&...args) const {   \
        return self.name(std::forward<Args>(args)...);           \
    }                                                            \
} _fun_##name{ *this };                                          \
template<> struct _n_Members<__COUNTER__ - _n_Start - 1> { using type = Function<&Type::_fun_##name, #name>; };

//META(Carrot)
//    VAR(int, a, { 3 })
//    VAR(double, b, { 31 })
//    FUN(constexpr double, add, (int a, int b) const { return a + b; })
//END_META

template<class Ty, std::size_t N>
struct NamedArray : std::array<Ty, N> {
    constexpr Ty& operator[](std::size_t i) {
        return std::array<Ty, N>::operator[](i);
    }

    constexpr const Ty& operator[](std::size_t i)const {
        return std::array<Ty, N>::operator[](i);
    }

    constexpr bool has(std::string_view name) const {
        for (auto& e : *this) {
            if (e.name() == name) return true;
        }
        return false;
    }

    constexpr Ty& operator[](std::string_view name) {
        for (auto& e : *this) {
            if (e.name() == name) return e;
        }
        throw std::exception("Bad access");
    }

    constexpr const Ty& operator[](std::string_view name) const {
        for (auto& e : *this) {
            if (e.name() == name) return e;
        }
        throw std::exception("Bad access");
    }
};

template<auto Ptr, string_literal Name>
struct Function {
    constexpr static auto ptr = Ptr;
    constexpr static auto name = Name.view();
    using signature = kaixo::signature_t<decltype(ptr)>;
    using return_type = kaixo::function_return_t<signature>;
    using argument_types = kaixo::function_args_t<signature>;
    template<class ...Args>
    constexpr static decltype(auto) invoke(Type& v, Args&&...as) {
        return (v.*ptr)(std::forward<Args>(as)...);
    }
    template<class ...Args>
    constexpr static decltype(auto) invoke(const Type& v, Args&&...as) {
        return (v.*ptr)(std::forward<Args>(as)...);
    }
};

template<auto Lambda, string_literal Name>
    requires (kaixo::has_fun_op<decltype(Lambda)>)
struct Function<Lambda, Name> {
    constexpr static auto ptr = Lambda;
    constexpr static auto name = Name.view();
    template<class ...Args>
    constexpr static decltype(auto) invoke(Type& v, Args&&...as) {
        return (v.*ptr)(std::forward<Args>(as)...);
    }
    template<class ...Args>
    constexpr static decltype(auto) invoke(const Type& v, Args&&...as) {
        return (v.*ptr)(std::forward<Args>(as)...);
    }
};

template<class ...Funs>
struct Functions {
    using Type = kaixo::member_class_t<std::decay_t<decltype(
        kaixo::template head_t<std::tuple<Funs...>>::ptr)>>;

    constexpr static std::size_t count = sizeof...(Funs);
    template<class> struct SignaturedFunction;
    struct GeneralFunction {
        constexpr GeneralFunction(std::string_view view)
            : name(view) {}
        std::string_view name{};
        virtual void _n_Link() = 0;
    };
    template<class R, class ...As>
    struct SignaturedFunction<R(As...)> : GeneralFunction {
        virtual void _n_Link() override {};
        constexpr SignaturedFunction(R(*fun)(Type&, As...), std::string_view name)
            : fun(fun), GeneralFunction(name) {}
        R(*fun)(Type&, As...);
    };
    struct FunctionBase {
        const GeneralFunction* fun;
        constexpr auto name() const { return fun->name; }
        template<class Ret, class ...Args>
        constexpr Ret invoke(const Type& v, Args...args) const {
            auto _self = dynamic_cast<const SignaturedFunction<Ret(Args...)>*>(fun);
            if (!_self) throw "Wrong call!"; else return _self->fun(*const_cast<Type*>(&v), args...);
        }
        template<class Sig>
        constexpr bool invocable() const {
            auto _self = dynamic_cast<const SignaturedFunction<Sig>*>(fun);
            return _self != nullptr;
        }
    };

    template<std::size_t I> using function = std::tuple_element_t<I, std::tuple<Funs...>>;

    template<std::size_t I, class ...As>
    constexpr static decltype(auto) _n_Fun_type_t(Type& v, As ...as) {
        return (v.*(function<I>::ptr))(as...);
    };

    template<std::size_t I>
    constexpr static auto _n_Fun_types = SignaturedFunction<kaixo::minimal_signature_t<
        decltype(Functions::template function<I>::ptr)>>{
        &_n_Fun_type_t<I>, Functions::template function<I>::name };

    constexpr static auto functions() {
        return[]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return NamedArray<FunctionBase, count>{
                dynamic_cast<const GeneralFunction*>(&_n_Fun_types<Is>)... };
        }(std::make_index_sequence<count>{});
    }
};

template<>
struct Functions<> {
    constexpr static std::size_t count = 0;

    constexpr static auto functions() {
        return;
    }
};

template<auto Ptr, string_literal Name>
struct Variable {
    using type = std::decay_t<kaixo::member_type_t<decltype(Ptr)>>;
    constexpr static auto ptr = Ptr;
    constexpr static auto name = Name.view();
};

template<class ...Vars>
struct Variables {
    using Type = kaixo::member_class_t<std::decay_t<decltype(
        kaixo::template head_t<std::tuple<Vars...>>::ptr)>>;

    constexpr static std::size_t count = sizeof...(Vars);

    struct GeneralVariable {
        constexpr GeneralVariable(std::string_view view)
            : name(view) {}
        std::string_view name{};
        virtual void _n_Link() = 0;
    };

    template<class Ty>
    struct TypedVariable : GeneralVariable {
        virtual void _n_Link() override {};
        constexpr TypedVariable(Ty Type::* var, std::string_view name)
            : var(var), GeneralVariable(name) {}
        Ty Type::* var;
    };

    struct VariableBase {
        const GeneralVariable* var;
        constexpr auto name() const { return var->name; }
        template<class Ty>
        constexpr Ty& as(Type& v) const {
            auto _self = dynamic_cast<const TypedVariable<Ty>*>(var);
            if (!_self) throw "Wrong call!"; else return v.*(_self->var);
        }
        template<class Ty>
        constexpr Ty Type::* as() const {
            auto _self = dynamic_cast<const TypedVariable<Ty>*>(var);
            if (!_self) throw "Wrong call!"; else return _self->var;
        }
        template<class Ty>
        constexpr const Ty& as(const Type& v) const {
            auto _self = dynamic_cast<const TypedVariable<Ty>*>(var);
            if (!_self) throw "Wrong call!"; else return v.*(_self->var);
        }
        template<class Ty>
        constexpr bool is() const {
            auto _self = dynamic_cast<const TypedVariable<Ty>*>(var);
            return _self != nullptr;
        }
    };

    template<std::size_t I> using variable = std::tuple_element_t<I, std::tuple<Vars...>>;

    template<std::size_t I>
    constexpr static auto _n_Var_types = TypedVariable<Variables::template variable<I>::type>{
        Variables::template variable<I>::ptr, Variables::template variable<I>::name };

    constexpr static auto variables() {
        return[]<std::size_t ...Is>(std::index_sequence<Is...>) {
            return NamedArray<VariableBase, count>{
                dynamic_cast<const GeneralVariable*>(&_n_Var_types<Is>)... };
        }(std::make_index_sequence<count>{});
    }
};

template<class Vars, class Funs>
struct Reflect : Vars, Funs {};

struct Potato {
    int a = 3;
    double b = 4;

    constexpr double add(int a, int b) const { return a + b; }

    using reflect = Reflect<
        Variables<
        Variable<&Potato::a, "a">,
        Variable<&Potato::b, "b">
        >,
        Functions<
        Function<&Potato::add, "add">
        >
    >;
};

template<class Ty>
struct PointBase {
    Ty x;
    Ty y;
};

template<class Ty>
struct Point : PointBase<Ty>, Reflect<
    Variables<
    Variable<&PointBase<Ty>::x, "x">,
    Variable<&PointBase<Ty>::y, "y">
    >, Functions<>
> {};


template<class Ty>
constexpr auto distance2(auto& p1, auto& p2) {
    constexpr auto v1 = std::decay_t<decltype(p1)>::variables();
    constexpr auto v2 = std::decay_t<decltype(p2)>::variables();

    if constexpr (v1.has("x") && v1.has("y") && v2.has("x") && v2.has("y")) {
        constexpr auto x1 = v1["x"];
        constexpr auto x2 = v2["x"];
        constexpr auto y1 = v1["y"];
        constexpr auto y2 = v2["y"];

        if constexpr (x1.is<Ty>() && x2.is<Ty>() && y1.is<Ty>() && y2.is<Ty>()) {
            constexpr auto x1v = x1.as<Ty>();
            constexpr auto x2v = x2.as<Ty>();
            constexpr auto y1v = y1.as<Ty>();
            constexpr auto y2v = y2.as<Ty>();

            return
                (p1.*x1v - p2.*x2v) * (p1.*x1v - p2.*x2v) +
                (p1.*y1v - p2.*y2v) * (p1.*y1v - p2.*y2v);
        }
        else return;
    }
    else return;
}

struct Data {
    int a = 10;
    int b = 20;
};

template<auto Ptr>
struct Thing {};




struct dynamic_tuple {

    constexpr dynamic_tuple() = default;

    template<class ...Args>
    constexpr dynamic_tuple(Args&&...args) {
        constexpr std::size_t _size = ((sizeof(Args)) + ...);
        data.reserve(_size);
        (push_back(args), ...);
    }

    template<class Ty>
    constexpr std::decay_t<Ty>& push_back(Ty&& ty) {
        return emplace_back<std::decay_t<Ty>>(std::forward<Ty>(ty));
    }

    template<class Ty, class...Args>
    constexpr Ty& emplace_back(Args&& ...args) {
        info.push_back(entry_info{ &typeid(Ty), (info.empty() ? 0ull : info.back().offset) + sizeof(Ty) });
        data.resize(data.size() + sizeof(Ty));
        return *std::construct_at((Ty*)(&data.at(data.size() - sizeof(Ty))), std::forward<Args>(args)...);
    }

    struct entry {
        uint8_t* value;
        const type_info* type;

        template<class Ty> constexpr bool is() const { return *type == typeid(Ty); }
        template<class Ty> constexpr Ty& as() const { return *(Ty*)(value); }
        template<class Ty> constexpr operator Ty& () const { return *(Ty*)(value); }
    };

    struct const_entry {
        const uint8_t* value;
        const type_info* type;

        template<class Ty> constexpr bool is() const { return *type == typeid(Ty); }
        template<class Ty> constexpr const Ty& as() const { return *(const Ty*)(value); }
        template<class Ty> constexpr operator const Ty& () const { return *(const Ty*)(value); }
    };

    constexpr entry operator[](std::size_t i) { return { data.data() + info[i].offset, info[i + 1].type }; }
    constexpr const_entry operator[](std::size_t i) const { return { data.data() + info[i].offset, info[i + 1].type }; }

    struct entry_info {
        const type_info* type;
        std::size_t offset;
    };

    struct iterator {
        std::vector<entry_info>::iterator info;
        uint8_t* data;

        constexpr iterator& operator++() { ++info; return *this; }
        constexpr entry operator*() { return { data + info[-1].offset, info->type }; }
        constexpr bool operator==(const iterator& o) const { return info == o.info; }
    };

    constexpr iterator begin() { return { info.begin() + 1, data.data() }; };
    constexpr iterator end() { return { info.end(), data.data() }; };

    std::vector<entry_info> info{ { 0, 0 } };
    std::vector<uint8_t> data{};
};


struct uv_entry {
    constexpr virtual ~uv_entry() = default;
};

template<class Ty>
struct uv_typed_entry : uv_entry {
    Ty value{};
    template<class ...Args>
    constexpr uv_typed_entry(Args&&...args)
        : value(std::forward<Args>(args)...) {}
};

class uv_element {
    uv_entry* value{ nullptr };
public:
    constexpr uv_element(uv_entry* value) : value(value) {}
    constexpr uv_element() = default;
    constexpr ~uv_element() { delete value; }

    constexpr uv_element(uv_element&& o) noexcept
        : value(o.value) {
        o.value = nullptr;
    }

    constexpr uv_element& operator=(uv_element&& o) noexcept {
        value = o.value, o.value = nullptr;
        return *this;
    }

    constexpr uv_element(const uv_element& o) = delete;
    constexpr uv_element& operator=(const uv_element& o) = delete;

    template<class Ty> requires (!std::same_as<uv_element, std::decay_t<Ty>>)
        constexpr uv_element& operator=(Ty&& arg) {
        assign<std::decay_t<Ty>>(std::forward<Ty>(arg));
        return *this;
    }

    template<class Ty, class ...Args>
    constexpr void assign(Args&&...args) {
        delete value;
        value = dynamic_cast<uv_entry*>(
            new uv_typed_entry<Ty>{ std::forward<Args>(args)... });
    }

    template<class Ty>
    constexpr Ty& as() {
        if (auto _self = dynamic_cast<uv_typed_entry<Ty>*>(value))
            return _self->value;
        else throw;
    }

    template<class Ty>
    constexpr const Ty& as() const {
        if (auto _self = dynamic_cast<const uv_typed_entry<Ty>*>(value))
            return _self->value;
        else throw;
    }

    template<class Ty>
    constexpr explicit operator Ty& () {
        if (auto _self = dynamic_cast<uv_typed_entry<Ty>*>(value))
            return _self->value;
        else throw;
    }

    template<class Ty>
    constexpr explicit operator const Ty& () const {
        if (auto _self = dynamic_cast<const uv_typed_entry<Ty>*>(value))
            return _self->value;
        else throw;
    }

    template<class Ty>
    constexpr bool is() const {
        return dynamic_cast<const uv_typed_entry<Ty>*>(value) != nullptr;
    }
};

class untyped_vector : std::vector<uv_element> {
    using parent = std::vector<uv_element>;
public:
    using value_type = parent::value_type;
    using reference = parent::reference;
    using const_reference = parent::const_reference;
    using pointer = parent::pointer;
    using const_pointer = parent::const_pointer;
    using size_type = parent::size_type;
    using difference_type = parent::difference_type;
    using iterator = parent::iterator;
    using const_iterator = parent::const_iterator;
    using reverse_iterator = parent::reverse_iterator;
    using const_reverse_iterator = parent::const_reverse_iterator;

    template<class ...Args>
    constexpr untyped_vector(Args&& ...args) {
        parent::reserve(sizeof...(Args));
        (this->push_back(std::forward<Args>(args)), ...);
    }

    template<class Ty>
    constexpr void push_back(Ty&& arg) {
        this->emplace_back<std::decay_t<Ty>>(std::forward<Ty>(arg));
    }

    template<class Ty, class ...Args>
    constexpr Ty& emplace_back(Args&&...args) {
        auto _ptr = new uv_typed_entry<Ty>(std::forward<Args>(args)...);
        parent::emplace_back(dynamic_cast<uv_entry*>(_ptr));
        return _ptr->value;
    }

    using parent::operator=;
    using parent::assign;
    using parent::at;
    using parent::operator[];
    using parent::front;
    using parent::back;
    using parent::data;
    using parent::begin;
    using parent::cbegin;
    using parent::rbegin;
    using parent::crbegin;
    using parent::end;
    using parent::cend;
    using parent::rend;
    using parent::crend;
    using parent::empty;
    using parent::size;
    using parent::max_size;
    using parent::reserve;
    using parent::capacity;
    using parent::shrink_to_fit;
    using parent::clear;
    using parent::erase;
    using parent::pop_back;
    using parent::resize;
    using parent::swap;
};



template<class ...Args>
struct Struct {
    constexpr static std::size_t bytes = (sizeof(Args) + ...);

    template<std::size_t I>
    constexpr static std::size_t member_size = sizeof(std::tuple_element_t<I, std::tuple<Args...>>);

    template<std::size_t I>
    using member_type = std::tuple_element_t<I, std::tuple<Args...>>;

    template<std::size_t I> struct member_t {
        constexpr static std::size_t value = member_size<I - 1> +member_t<I - 1>::value;
        using type = member_type<I>;
    };
    template<> struct member_t<0> { constexpr static std::size_t value = 0; };
    template<std::size_t I> constexpr static auto member = member_t<I>{};

    template<class Member>
    constexpr auto operator->*(const Member&) const -> Member::type const& {
        return *(const Member::type*)(&data[Member::value]);
    }

    uint8_t data[bytes]{};
};


template<class DataClass>
concept data_class = requires() {
    typename DataClass::access_tier;
    typename DataClass::dependencies;
};

template<std::size_t Level>
struct tier : std::integral_constant<std::size_t, Level> {};

template<class Me, class ...Tys>
struct dependency_selector {
    static_assert(((Tys::access_tier::value
        <= Me::access_tier::value) && ...), "Invalid Dependency");
};

template<class Me>
struct dependency_selector<Me> {};

class Cache {
public:
    using access_tier = tier<0>;
    using dependencies = dependency_selector<Cache>;
};

class TransitionsTradeBlocker {
public:
    using access_tier = tier<2>;
    using dependencies = dependency_selector<TransitionsTradeBlocker, Cache>;
};

class SettingsManager {
public:
    using access_tier = tier<1>;                                    // vvv  Won't work  vvv
    using dependencies = dependency_selector<SettingsManager, Cache, TransitionsTradeBlocker>;
};


template<class Ty, std::size_t I>
using greater_than = Ty;

#include <source_location>

namespace contracts {

    template<std::size_t I>
    struct arg_t {
        constexpr static auto value = I;
        constexpr static decltype(auto) get(auto& vals) {
            return std::get<value>(vals);
        }
    };

    template<auto Lambda>
    struct condition_t {
        constexpr static auto value = Lambda;
        constexpr static decltype(auto) get(auto& vals) {
            return value(vals);
        }
    };


    template<class A, class B>
    constexpr auto operator>(A, B) {
        return condition_t < [](auto& vals) -> decltype(auto) {
            return A::get(vals) > B::get(vals);
        } > {};
    }

    template<class A, class B>
    constexpr auto operator+(A, B) {
        return condition_t < [](auto& vals) -> decltype(auto) {
            return A::get(vals) + B::get(vals);
        } > {};
    }

    template<std::size_t I>
    constexpr auto arg = arg_t<I>{};

    template<auto Condition, class ...Args>
    constexpr void pre(Args&&...args) {
        const std::tuple _tuple{ std::forward<Args>(args)... };
        if (!Condition.get(_tuple))
            throw std::runtime_error("Condition was not satisfied.");
    }
}

using namespace contracts;

constexpr auto difference(auto a, auto b) {
    pre<(arg<0> > arg<1>)>(a, b);
    return a - b;
}


void aoinenoa() {

    auto resae = difference(11, 10);

    using my_struct = Struct<int, double, float>;

    my_struct inst{};

    auto vale = inst->*my_struct::member<1>;

    constexpr auto as = my_struct::member<1>;



    constexpr Point<double> p1{ 1, 2 };
    constexpr Point<double> p2{ 2, 4 };

    dynamic_tuple tpl{ 1, 10. };
    tpl.emplace_back<float>(20.f);

    for (auto i : tpl) {
        if (i.is<double>())     std::cout << "double: " << i.as<double>() << '\n';
        else if (i.is<int>())   std::cout << "int:    " << i.as<int>() << '\n';
        else if (i.is<float>()) std::cout << "float:  " << i.as<float>() << '\n';
    }

    if (tpl[0].is<int>())    std::cout << "int:    " << tpl[0].as<int>() << '\n';
    if (tpl[1].is<double>()) std::cout << "double: " << tpl[1].as<double>() << '\n';
    if (tpl[2].is<float>())  std::cout << "float:  " << tpl[2].as<float>() << '\n';

    //constexpr auto var = Carrot::variables()[0];
    //if constexpr (var.is<int>()) {
    //    int& value = var.as<int>(cal);
    //}

    //auto& fun = Carrot::functions()[0];
    //if (fun.invocable<double(int, int)>()) {
    //    double res = fun.invoke<double>(cal, 1, 1);
    //}

    Type thing;
    delegate<int(int)> d{ thing, &Type::thing };
    int res = d(1.f);

    constexpr auto aeff = sizeof(delegate<int(int)>);
    constexpr auto girn = sizeof(std::function<int(int)>);

    d = [](int a) { return a + 1; };
    res = d(1);

    int a1 = 10;
    int a2 = 20;
    d = [a1, a2](int a) { return a + a1 + a2; };
    res = d(1);

    int b1 = 10;
    int b2 = 20;
    int b3 = 30;
    d = [b1, b2, b3](int a) { return a + b1 + b2 + b3; };
    res = d(1);

    d = Functor{};
    res = d(1);

    d = Functor2{};
    res = d(1);

    MyObject obj;
    obj.set(Hovering, true);
    bool v = obj.get(Hovering);
    obj.set(Hovering, false);

    return 0;
}

struct HeapChecker {

    static inline std::vector<void*> allocated{};

    constexpr static void add(void* ptr) {
        allocated.push_back(ptr);
    }

    constexpr static std::size_t check(void* ptr) {
        return std::erase(allocated, ptr);
    }
};

struct RefCounted {
};



struct Object {
    constexpr Object()
        : refs(HeapChecker::check(this)) {}

    constexpr virtual ~Object() = default;

    constexpr void remember() { if (refs) ++refs; }
    constexpr void forget() { if (refs && --refs == 0) delete this; }

    static void* operator new(std::size_t size) {
        void* _ptr = ::operator new(size);
        HeapChecker::add(_ptr);
        return _ptr;
    }

private:
    std::size_t refs;
};

template<std::derived_from<Object> Ty>
struct Ptr {

    constexpr Ptr() = default;

    template<std::derived_from<Ty> T>
    constexpr Ptr(Ptr<T>&& val)
        : value(val.value) {
        val.value = nullptr;
    }

    template<std::derived_from<Ty> T>
    constexpr Ptr(const Ptr<T>& val)
        : value(val.value) {
        if (value) value->remember();
    }

    template<std::derived_from<Ty> T>
    constexpr Ptr(T*&& val) : value(val) {}

    template<std::derived_from<Ty> T>
    constexpr Ptr(T*& val) : value(val) { val->remember(); }

    template<std::derived_from<Ty> T>
    constexpr Ptr& operator=(Ptr<T>&& val) {
        if (value) value->forget();
        value = val.value;
        val.value = nullptr;
        return *this;
    }

    template<std::derived_from<Ty> T>
    constexpr Ptr& operator=(const Ptr<T>& val) {
        if (value) value->forget();
        value = val.value;
        if (value) value->remember();
        return *this;
    }

    template<std::derived_from<Ty> T>
    constexpr Ptr& operator=(T*&& val) {
        if (value) value->forget();
        value = val;
        return *this;
    }

    template<std::derived_from<Ty> T>
    constexpr Ptr& operator=(T*& val) {
        if (value) value->forget();
        value = val;
        if (value) value->remember();
        return *this;
    }

    constexpr ~Ptr() { if (value) value->forget(); }

    constexpr Ty* operator->() { return value; }
    constexpr const Ty* operator->() const { return value; }
    constexpr Ty& operator*() { return *value; }
    constexpr const Ty& operator*() const { return *value; }

private:
    Ty* value = nullptr;

    template<std::derived_from<Object>>
    friend struct Ptr;
};




//namespace kaixo {
//    template<class> struct delegate;
//    template<class Ret, class ...Args>
//    struct delegate<Ret(Args...)> {
//        using return_type = Ret;
//        using argument_types = pack<Args...>;
//        using function_type = Ret(Args...);
//    private:
//        using stored_function_type = Ret(uint8_t*, Args...);
//        template<class Ty> // Check if type is small (8 bytes)
//        constexpr static bool small = 
//            sizeof(std::decay_t<Ty>) <= sizeof(uint8_t) * 8;
//        template<class Ty> // Check if type convertible to fun ptr
//        constexpr static bool to_ptr = // (usually lambda)
//            std::convertible_to<std::decay_t<Ty>, function_type*>;
//
//        // Small functor
//        struct small_functor {
//            constexpr small_functor(Functor&& functor)
//                : functor(std::forward<Functor>(functor)){}
//
//            std::decay_t<Functor> functor{};
//
//            template<class Functor>
//            static return_type call(uint8_t* data, Args...args) {
//                small_functor* _data = std::bit_cast<small_functor*>(data);
//                return (_data->functor)(args...);
//            }
//        };
//
//    public:
//
//        template<std::invocable<Args...> Functor>
//            requires (to_ptr<Functor>)
//        consteval delegate(const Functor& functor) {
//            function_type* _ptr = functor;
//        }
//
//        template<std::invocable<Args...> Functor> 
//            requires (small<Functor> && !to_ptr<Functor>) 
//        constexpr delegate(Functor&& functor) 
//            : ptr(&small_functor<Functor>::call) {
//            small_functor<Functor> const* _data = static_cast<small_functor<Functor>*>(static_cast<void*>(data));
//
//            //small_functor<Functor> const* _data = std::bit_cast<small_functor<Functor> const*>(&data[0]);
//            std::construct_at(_data, std::forward<Functor>(functor));
//        }
//
//        constexpr return_type operator()(Args...args) {
//            return (*ptr)(data, args);
//        }
//
//        uint8_t data[8]{};
//        stored_function_type* ptr = nullptr;
//    };
//}


#include "pack_utils.hpp"
#include "utils.hpp"
#include "string_literal.hpp"

using namespace kaixo;
namespace kaixo {
    template<class Ty> 
    concept has_dependencies = requires () { typename Ty::dependencies; };
    template<class> 
    struct dependencies_t { using type = pack<>; };
    template<has_dependencies Ty> 
    struct dependencies_t<Ty> { using type = typename Ty::dependencies; };
    template<class Ty>
    using dependencies = typename dependencies_t<Ty>::type;

    template<class Ty, class Var>
    concept has_name = Ty::name == Var::name;

    template<string_literal Name, class Ty>
    struct named_value {
        constexpr static auto name = Name;
        using value_type = Ty;
        Ty value;
    };

    template<string_literal Name>
    struct var_t {
        using dependencies = pack<var_t>;
        using definitions = pack<var_t>;
        constexpr static string_literal name = Name;

        template<class Ty> constexpr auto operator=(Ty&& value) const {
            return named_value<name, Ty>{ std::forward<Ty>(value) };
        }
    };

    template<string_literal Name> constexpr auto var = var_t<Name>{};
    template<class Ty> concept is_var = requires() { Ty::name; };

    template<class ...Args>
    struct named_tuple : std::tuple<Args...> {
        using std::tuple<Args...>::tuple;

        template<class Var> constexpr decltype(auto) get() const {
            // Filter the arguments by name and get the first one, then get the
            // index of that type in the pack, so we can use the std::get function
            constexpr auto filter = []<has_name<Var>>{};
            using named_value_type = pack<Args...>::template filter<filter>::head;
            constexpr std::size_t index = pack<Args...>::template index<named_value_type>;
            return std::get<index>(*this).value;
        }
    };
    template<class ...Args> named_tuple(Args...)->named_tuple<Args...>;

    template<class Lambda, class Dependencies>
    struct expression : Lambda, Dependencies {
        using dependencies = Dependencies::unique;

        template<class ...Args>
        constexpr decltype(auto) operator()(Args&&... vals) const {
            return Lambda::operator()(named_tuple{ std::forward<Args>(vals)... });
        }
    };
    template<class Ty>
    concept is_expression = specialization<Ty, expression>;

    template<class Ty, class ...Args>
    constexpr decltype(auto) use(Ty& v, const named_tuple<Args...>& vals) {
        if constexpr (is_expression<Ty>) return v(vals);
        else if constexpr (is_var<Ty>) return vals.get<Ty>();
        else return v;
    }

    template<class A, class B>
    concept valid_op = is_var<std::decay_t<A>> || is_expression<std::decay_t<A>>
                    || is_var<std::decay_t<B>> || is_expression<std::decay_t<B>>;

#define def_op(op)                                                  \
    template<class Av, class Bv> requires valid_op<Av, Bv>          \
    constexpr auto operator op(Av&& a, Bv&& b) {                    \
        using A = std::decay_t<Av>;                                 \
        using B = std::decay_t<Bv>;                                 \
        if constexpr (is_var<A> && !is_var<B>) {                    \
            return expression{ [                                    \
                b = std::forward<Bv>(b)                             \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return vals.get<A>() op use(b, vals);               \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        } else if constexpr (!is_var<A> && is_var<B>) {             \
            return expression{ [                                    \
                a = std::forward<Av>(a)                             \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return use(a, vals) op vals.get<B>();               \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        } else if constexpr (is_var<A> && is_var<B>) {              \
            return expression{ [                                    \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return vals.get<A>() op vals.get<B>();              \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        } else {                                                    \
            return expression{ [                                    \
                a = std::forward<Av>(a), b = std::forward<Bv>(b)    \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return use(a, vals) op use(b, vals);                \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        }                                                           \
    }

    def_op(+) def_op(-) def_op(*) def_op(/) def_op(%) def_op(&&) def_op(||)
    def_op(|) def_op(&) def_op(^) def_op(==) def_op(!=) def_op(<<) def_op(>>)
    def_op(<=) def_op(>=) def_op(>) def_op(<) def_op(+=)
#undef def_op
}

#include <cassert>


template<std::size_t, bool = false> struct int_type;

template<> struct int_type<1, true> { using type = bool; };
template<> struct int_type<8, true> { using type = uint8_t; };
template<> struct int_type<16, true> { using type = uint16_t; };
template<> struct int_type<32, true> { using type = uint32_t; };
template<> struct int_type<64, true> { using type = uint64_t; };
template<> struct int_type<1, false> { using type = bool; };
template<> struct int_type<8, false> { using type = int8_t; };
template<> struct int_type<16, false> { using type = int16_t; };
template<> struct int_type<32, false> { using type = int32_t; };
template<> struct int_type<64, false> { using type = int64_t; };

consteval std::size_t up_2_pow(std::size_t v) {
    if (v < 8) return 8;
    else if (v < 16) return 16;
    else if (v < 32) return 32;
    else return 64;
}

template<std::size_t S, bool U>
struct int_mask {
    using value_type = typename int_type<up_2_pow(S), U>::type;
    value_type value : S{};

    constexpr int_mask() = default;
    constexpr int_mask(value_type v) : value(v) {}
    constexpr int_mask& operator=(value_type v) { value = v; return *this; }

    constexpr operator value_type () const { return value; }
};

template<std::size_t S> 
struct int_type<S, false> {
    using type = int_mask<S, false>;
};

template<std::size_t S> 
struct int_type<S, true> {
    using type = int_mask<S, true>;
};

template<std::size_t S, bool U = false> using int_t = typename int_type<S, U>::type;

template<class Ty>
struct TypeInfo {

    constexpr static std::size_t size = sizeof(Ty);
    constexpr static std::string_view name = []() {
        std::string_view _name = __FUNCSIG__;
        return _name.substr(29, _name.size() - 68);
    }();
};

#include <memory>


namespace kaixo {
    template<class> class delegate;
    template<class Ret, class ...Args>
    class delegate<Ret(Args...)> {
        template<class> struct functor;
        template<class Functor> 
            requires (sizeof(std::decay_t<Functor>) <= 16)
        struct functor<Functor> {
            std::decay_t<Functor> fun;
            constexpr static Ret invoke(void* data, Args...args) {
                return static_cast<functor*>(data)->fun(std::forward<Args>(args)...);
            }
            constexpr static void clean(void* data) {}
        };

        template<class Functor>
            requires (sizeof(std::decay_t<Functor>) > 16)
        struct functor<Functor> {
            std::unique_ptr<std::decay_t<Functor>> fun;
            constexpr functor(Functor functor)
                : fun(std::make_unique<std::decay_t<Functor>>(
                    std::forward<Functor>(functor))) {}
            constexpr static Ret invoke(void* data, Args...args) {
                return (*static_cast<functor*>(data)->fun)(std::forward<Args>(args)...);
            }
            constexpr static void clean(void* data) {
                (static_cast<functor*>(data)->fun).release(); 
            }
        };

        template<class Obj, class Fun>
        struct mem_fun {
            Obj* obj; Fun fun;
            constexpr static Ret invoke(void* data, Args...args) {
                mem_fun* _fun = static_cast<mem_fun*>(data);
                return (_fun->obj->*_fun->fun)(std::forward<Args>(args)...);
            }
        };

        struct fun_ptr {
            Ret(*fun)(Args...);
            constexpr static Ret invoke(void* data, Args...args) {
                return static_cast<fun_ptr*>(data)->fun(std::forward<Args>(args)...);
            }
        };
    public:
        template<std::invocable<Args...> Arg>
        constexpr delegate(Arg&& arg) 
            : fun(&functor<Arg>::invoke), clean(&functor<Arg>::clean) {
            new (data) functor<Arg>{ std::forward<Arg>(arg) };
        }

        template<class Obj, class Fun>
        constexpr delegate(Obj& obj, Fun fun)
            : fun(&mem_fun<Obj, Fun>::invoke) { 
            new (data) mem_fun<Obj, Fun>(&obj, fun); 
        }

        constexpr delegate(Ret(*fun)(Args...)) 
            : fun(&fun_ptr::invoke) { new (data) fun_ptr{ fun }; }

        constexpr Ret operator()(Args...args) const {
            return (*fun)(const_cast<void*>(static_cast<const void*>(data)), 
                std::forward<Args>(args)...);
        }

        constexpr ~delegate() { if (clean) clean(static_cast<void*>(data)); }

        uint8_t data[16];
        Ret(*fun)(void*, Args...) = nullptr;
        void(*clean)(void*) = nullptr;
    };


    template<class Lambda>
    delegate(Lambda)->delegate<minimal_signature_t<Lambda>>;
    template<class Obj, class Fun>
    delegate(Obj&, Fun)->delegate<minimal_signature_t<Fun>>;
}

#include <functional>
#include <iostream>

struct A {
    int value;
    A(int value = 0) : value(value) { std::cout << "Construct\n"; }
    A(const A& a) : value(a.value) { std::cout << "Copy\n"; }
    A(A&& a) : value(a.value) { std::cout << "Move\n"; }
    ~A() { std::cout << "Destroy\n"; }
};


int add(A& a, A& b) {
    return a.value + b.value;
}


struct Functor {
    int a = 10;
    constexpr int operator()(int b) {
        return a + b;
    }

    constexpr int add(int b) const { return a + b; }
};


struct RTTI_in_place {
    template<class Ty>
    constexpr static void destroy_impl(void* const ptr) {
        std::destroy_at(static_cast<Ty*>(ptr));
    }

    template<class Ty>
    constexpr static void move_impl(void* const dst, void* const src) {
        std::construct_at(static_cast<Ty*>(dst), std::move(*static_cast<Ty*>(src)));
    }

    template<class Ty>
    constexpr static void copy_impl(void* const dst, const void* const src) {
        std::construct_at(static_cast<Ty*>(dst), *static_cast<const Ty*>(src));
    }

    template<class Ty>
    constexpr static RTTI_in_place create() {
        return RTTI_in_place{ destroy_impl<Ty>, move_impl<Ty>, copy_impl<Ty> };
    }

    void(*destroy)(void* const);
    void(*move)(void* const, void* const);
    void(*copy)(void* const, const void* const);
};

struct RTTI_dynamic {
    template<class Ty>
    constexpr static void destroy_impl(void* const ptr) {
        ::delete static_cast<Ty*>(ptr);
    }

    template<class Ty>
    constexpr static void* move_impl(void* const src) {
        return ::new Ty{ std::move(*static_cast<Ty*>(src)) };
    }

    template<class Ty>
    constexpr static void* copy_impl(const void* const src) {
        return ::new Ty{ std::move(*static_cast<const Ty*>(src)) };
    }

    template<class Ty>
    constexpr static RTTI_dynamic create() {
        return RTTI_dynamic{ destroy_impl<Ty>, move_impl<Ty>, copy_impl<Ty> };
    }

    void(*destroy)(void* const) = nullptr;
    void*(*move)(void* const) = nullptr;
    void*(*copy)(const void* const) = nullptr;
};

struct Any {

    template<class Ty>
    constexpr Any(Ty&& me) {
        using type = std::decay_t<Ty>;
        m_Data.rtti = RTTI_dynamic::create<type>();
        m_Data.ptr = ::new type{ std::forward<Ty>(me) };
    }

    struct Data {
        constexpr Data() {}
        constexpr Data(const Data& data) : ptr(data.copy()), rtti(data.rtti) {}
        constexpr Data(Data&& data) noexcept : ptr(data.ptr), rtti(data.rtti) { data.ptr = nullptr, data.rtti = {}; }

        void* ptr = nullptr;
        RTTI_dynamic rtti{};

        constexpr void* copy() const { return rtti.copy(ptr); }
        constexpr void* move() const { return rtti.move(ptr); }
    };

    Data m_Data;
};

namespace kaixo {

    struct multi_fun_base { virtual ~multi_fun_base() {}; };

    template<class> struct callable_multi_fun_base;
    template<class Ret, class ...Args>
    struct callable_multi_fun_base<Ret(Args...)> 
        : virtual multi_fun_base { // Virtual inheritance for single base
        virtual Ret call(void*, Args...) = 0;
    };

    template<class, class> struct callable_multi_fun;
    template<class Fun, class Ret, class ...Args>
    struct callable_multi_fun<Fun, Ret(Args...)> : callable_multi_fun_base<Ret(Args...)> {
        virtual Ret call(void* fun, Args...args) override {
            return (*static_cast<Fun*>(fun))(args...);
        }
    };

    template<class Fun, class... Sigs> // Inherit base for all signatures
    struct multi_fun_start : callable_multi_fun<Fun, Sigs>... {};

    struct multi_fun_storage_base {
        virtual ~multi_fun_storage_base() {};
        virtual void* get() = 0;
    };

    template<class Fun> struct multi_fun_storage : multi_fun_storage_base  {
        Fun fun; // Simple function wrapper
        virtual void* get() override { return &fun; };
    };

    template<class ...Sigs> struct multi_fun {
        multi_fun(auto fun) // vvv Create instance for all signatures given fun
            : ptr(new multi_fun_start<std::decay_t<decltype(fun)>, Sigs...>{}),
            data(new multi_fun_storage<std::decay_t<decltype(fun)>>{ fun }) {}
        ~multi_fun() { delete ptr; delete data; }

        template<class Sig> auto call(auto...args) {
            auto _c = dynamic_cast<callable_multi_fun_base<Sig>*>(ptr);
            return (*_c).call(data->get(), args...);
        }

        multi_fun_base* ptr;
        multi_fun_storage_base* data;
    };
}



template<size_t N, class C>
struct incrementer {
    friend constexpr auto magic_incr(incrementer<N, C>);
};

template<size_t N, class C, size_t V>
struct incrementer_def {
    friend constexpr auto magic_incr(incrementer<N, C>) { return V; };
};

template<size_t N, class C, class>
concept checker_c = requires() {
    magic_incr(incrementer<N, C>{});
};

// This checker first checks if the magic_incr friend function has been defined for N
// in any case, because everything in the boolean expression needs to be valid code
// the incrementer_def is evaluated, creating the definition for the magic_incr friend
// function anyway. So the next time this checker is evaluated for N, it will be valid.
// we need a unique class T to reevaluate N each time.
template<size_t N, class C, class T>
struct checker : std::bool_constant<checker_c<N, C, T> && (sizeof(incrementer_def<N, C, N + 1>), true)> {};

template<size_t, class, auto>
struct incr;

template<size_t V, class C, auto L> requires (!checker<V, C, decltype(L)>::value)
struct incr<V, C, L> {
    constexpr static size_t get() { return V; }
};

template<size_t V, class C, auto L> requires (checker<V, C, decltype(L)>::value)
struct incr<V, C, L> : incr<V + 1, C, L> {
    using incr<V + 1, C, L>::get;
};

// 
struct dud {};
template<std::size_t, class, class...>
constexpr auto vtable = dud{}; // Not found: convertible to bool

template<auto> struct unq {};

template<auto V>
constexpr unq<V> unq_v{};

template<auto Mf, class Base, class ...Args>
constexpr auto find_impl(Base* me, Args ...args) {
    using ret = kaixo::function_return_t<kaixo::minimal_signature_t<decltype(Mf)>>;
    // Function id
    constexpr auto _id1 = incr<0, dud, unq_v<Mf>>::get();
    constexpr auto _id2 = incr<0, Base, unq_v<Mf>>::get();
    auto test = [&]<std::size_t I>(auto & f) -> ret {
        // Make sure it recurses till we get to a non-lambda
        if constexpr (!std::same_as<decltype(::vtable<I, decltype(Mf), Args...>), const dud>) {
            // Make sure we have the correct function
            if constexpr (vtable<I, decltype(Mf), Args...>.second == _id2) {
                // Find derived type from first argument
                using type = kaixo::member_class_t<decltype(vtable<I, decltype(Mf), Args...>.first)>;
                // Try dynamic cast
                if (auto _ptr = dynamic_cast<type*>(me)) {
                    return (_ptr->*(vtable<I, decltype(Mf), Args...>.first))(args...);
                }
            }
            // Recurse if not correct derived
            return f.operator()<I+1>(f);
        }
    };

    return test.operator()<0>(test);
}

#define toverride(b, d, f) \
template<class ...Args> constexpr auto \
vtable<(incr<0, b, []{}>::get()), decltype(&b::f<Args...>), Args...>\
    = std::pair{ &d::f<Args...>, (incr<0, b, unq_v<&b::f<Args...>>>::get()) };

struct Base {
    constexpr virtual ~Base() {}

    template<class Arg>
    constexpr int my_fun1(Arg arg) { return find_impl<&Base::my_fun1<Arg>>(this, arg); }

    template<class Arg>
    constexpr int my_fun2(Arg arg) { return find_impl<&Base::my_fun2<Arg>>(this, arg); }
};

struct Derived1 : Base {

    template<class Arg>
    constexpr int my_fun1(Arg arg) {
        std::cout << "called my_fun1 on Derived1 with: " << typeid(Arg).name() << "\n"; 
        return 1;
    }

    template<class Arg>
    constexpr int my_fun2(Arg arg) {
        std::cout << "called my_fun2 on Derived1 with: " << typeid(Arg).name() << "\n"; 
        return 2;
    }
};

//toverride(Base, Derived1, my_fun1);
//toverride(Base, Derived1, my_fun2);

struct Derived2 : Derived1 {
    template<class Arg>
    constexpr int my_fun1(Arg arg) {
        std::cout << "called my_fun1 on Derived2 with: " << typeid(Arg).name() << "\n";        
        return 3;
    }

    template<class Arg>
    constexpr int my_fun2(Arg arg) {
        std::cout << "called my_fun2 on Derived2 with: " << typeid(Arg).name() << "\n";
        return 4;
    }
};

//toverride(Base, Derived2, my_fun1);
//toverride(Base, Derived2, my_fun2);



#include <string>

namespace ta {
    template<size_t N, class C> struct incrementer {
        friend constexpr auto magic_incr(incrementer<N, C>);
    };

    template<size_t N, class C, size_t V> struct incrementer_def {
        friend constexpr auto magic_incr(incrementer<N, C>) { return V; };
    };

    template<size_t N, class C, class> concept checker_c = requires() {
        magic_incr(incrementer<N, C>{});
    };

    template<size_t N, class C, class T>
    struct checker : std::bool_constant<checker_c<N, C, T> && (sizeof(incrementer_def<N, C, N + 1>), true)> {};

    template<size_t, auto, auto> struct incr;

    template<size_t V, auto C, auto L> requires (!checker<V, decltype(C), decltype(L)>::value)
        struct incr<V, C, L> {
        constexpr static size_t get() { return V; }
    };

    template<size_t V, auto C, auto L> requires (checker<V, decltype(C), decltype(L)>::value)
        struct incr<V, C, L> : incr<V + 1, C, L> {
        using incr<V + 1, C, L>::get;
    };
}
struct dud {};
struct type_array {};
template<type_array Arr, std::size_t I> constexpr auto type_array_v = dud{};
template<type_array Arr, std::size_t I> using type_array_t = decltype(type_array_v<Arr, I>);

template<type_array Arr>
struct type_array_i {
    template<std::size_t I>
    using element = typename std::decay_t<type_array_t<Arr, I>>::type;

    constexpr static std::size_t size = []() {
        constexpr auto finder = []<std::size_t N>(auto& finder) {
            if constexpr (std::same_as<type_array_t<Arr, N>, const dud>) return N;
            else return finder.template operator()<(N+1)>(finder);
        };
        return finder.template operator()<0>(finder);
    }();
};

#define push(a, t) template<> constexpr auto type_array_v<a, (ta::incr<0, a, []{}>::get())> = std::type_identity<t>{};

constexpr type_array my_arr;

push(my_arr, int);
push(my_arr, float);
push(my_arr, std::string);

static_assert(type_array_i<my_arr>::size == 3ull);
static_assert(std::same_as<type_array_i<my_arr>::element<0>, int>);
static_assert(std::same_as<type_array_i<my_arr>::element<1>, float>);
static_assert(std::same_as<type_array_i<my_arr>::element<2>, std::string>);



template<class T>
struct is_callable_impl {
private:
    typedef char(&yes)[1];
    typedef char(&no)[2];

    struct Fallback { void operator()(); };
    struct Derived : T, Fallback { };

    template<typename U, U> struct Check;

    template<typename>
    static yes test(...);

    template<typename C>
    static no test(Check<void (Fallback::*)(), &C::operator()>*);

public:
    constexpr static bool value = sizeof(test<Derived>(0)) == sizeof(yes);
};

template<class T> requires (std::is_fundamental_v<T>)
struct is_callable_impl<T> {
    constexpr static bool value = false;
};

template<class T>
concept is_callable = is_callable_impl<T>::value;

template<class Ty>
constexpr static auto check_invocable(Ty&& v) {
    if constexpr (std::invocable<Ty>) return check_invocable(std::forward<Ty>(v)());
    else return std::forward<Ty>(v);
}

template<class, class> struct fun_arity {};
template<class Fun>
struct fun_arity<Fun, kaixo::pack<>> : std::integral_constant<std::size_t, 0> {};
template<class Fun, class ...Tys>
struct fun_arity<Fun, kaixo::pack<Tys...>>
    : std::conditional_t<std::invocable<Fun, Tys...>, // if invocable
    std::integral_constant<std::size_t, sizeof...(Tys)>, // Get integral constant
    fun_arity<Fun, typename kaixo::template pack<Tys...>::init>>{}; // Otherwise, remove head and recurse

template<class Fun1, class Fun2, class Args, std::size_t ...Is, std::size_t ...Ns>
constexpr auto invoke_double_impl(Fun1& fun1, Fun2& fun2, Args&& args, std::index_sequence<Is...>, std::index_sequence<Ns...>) {
    return ((fun1 << (fun2 << ... << std::get<Is>(args))) << ... << std::get<Ns + sizeof...(Is)>(args));
}

template<class Fun1, class Fun2, class ...Tys> requires requires (Fun1& fun1, Fun2& fun2, std::tuple<Tys&&...> args) {
    { invoke_double_impl(fun1, fun2, args, std::make_index_sequence<fun_arity<Fun2, kaixo::pack<Tys...>>::value>{},
        std::make_index_sequence<sizeof...(Tys) - fun_arity<Fun2, kaixo::pack<Tys...>>::value>{}) };
}
constexpr auto invoke_double(Fun1& fun1, Fun2& fun2, Tys&&...args) {
    constexpr auto arity = fun_arity<Fun2, kaixo::pack<Tys...>>::value;
    return invoke_double_impl(fun1, fun2, std::forward_as_tuple(std::forward<Tys>(args)...),
        std::make_index_sequence<arity>{}, std::make_index_sequence<sizeof...(Tys) - arity>{});
}

template<is_callable Fun, class Arg>
constexpr auto operator<<(Fun f, Arg&& v) {
    if constexpr (std::invocable<Fun, Arg&&>) return check_invocable(f(v));
    else if constexpr (is_callable<std::decay_t<Arg>>) return[v, f = f]<class ...Tys>
        (Tys&&...args) {
        return invoke_double(v, f, std::forward<Tys>(args)...);
    };
    else return[v = std::forward<Arg>(v), f = f]<class ...Args>(Args&& ...args)
        requires (std::invocable<Fun, Arg&&, Args&&...>) {
        return check_invocable(f(v, std::forward<Args>(args)...));
    };
}

namespace kaixo {

    // general type traits, concept, and helpers:

    // The entire structure makes use of definitions and dependencies
    // to deduce necessary arguments to lazily evaluated expressions.
    template<class Ty> concept has_dependencies = requires () { typename Ty::dependencies; };
    template<class Ty> concept has_definitions = requires () { typename Ty::definitions; };
    template<class Ty> concept is_named = requires() { Ty::name; };

    // General way to retrieve dependencies and definitions from any 
    // type, even ones that don't have any.
    template<class> struct dependencies_t { using type = pack<>; };
    template<has_dependencies Ty> struct dependencies_t<Ty> { using type = typename Ty::dependencies; };
    template<class Ty> using dependencies = typename dependencies_t<Ty>::type;
    template<class> struct definitions_t { using type = pack<>; };
    template<has_definitions Ty> struct definitions_t<Ty> { using type = typename Ty::definitions; };
    template<class Ty> using definitions = typename definitions_t<Ty>::type;

    // For the purpose of list comprehension, we only 
    // need a begin and end method for iterating.
    template<class Ty> concept is_container = requires(const Ty ty) {
        { ty.begin() }; { ty.end() };
    };

    // fundamental classes:

    // Named value is used inside the named tuple, 
    // links a constexpr name to a value of type Ty
    template<string_literal Name, class Ty>
    struct named_value {
        constexpr static auto name = Name;
        using value_type = Ty;
        Ty value;
    };

    // Type wrapper for a constexpr name 
    template<string_literal Name>
    struct var_t {
        using dependencies = pack<var_t>; // to make life easier later on, a var 
        using definitions = pack<var_t>; // depends on itself, and defines itself
        constexpr static auto name = Name;

        // Assignment operator to create a named_value
        template<class Ty> constexpr auto operator=(Ty&& value) const {
            return named_value<name, Ty>{ std::forward<Ty>(value) };
        }
    };
    template<string_literal Name> constexpr auto var = var_t<Name>{};
    template<class Ty> concept is_var = is_named<Ty> && has_definitions<Ty> && has_dependencies<Ty>;

    template<class A, class B> concept name_equals = is_named<A> && is_named<B> && A::name == B::name;

    // std::tuple of named_values, contains a get method
    // to retrieve a value given a var
    template<class ...Args>
    struct named_tuple : std::tuple<Args...> {
        using std::tuple<Args...>::tuple;

        template<is_var Var> constexpr decltype(auto) get() const {
            // Filter the arguments by name and get the first one, then get the
            // index of that type in the pack, so we can use the std::get function
            constexpr auto filter = []<name_equals<Var>>{};
            using named_value_type = pack<Args...>::template filter<filter>::head;
            constexpr std::size_t index = pack<Args...>::template index<named_value_type>;
            return std::get<index>(*this).value;
        }
    };
    template<class ...Args> named_tuple(Args...)->named_tuple<Args...>;

    // Lazily evaluated expression
    template<class Lambda, class Dependencies>
    struct expression : Lambda, Dependencies {
        using dependencies = Dependencies::unique;

        template<class ...Args>
        constexpr decltype(auto) operator()(Args&&... vals) const {
            // The named tuple should contain all the dependencies of this expression.
            return Lambda::operator()(named_tuple{ std::forward<Args>(vals)... });
        }
    };

    template<class Ty> concept is_expression = specialization<Ty, expression>;

    // Constructing the container:

    // In order to make our lives easier, we always use a wrapped container
    template<is_container C>
    struct wrapped_container {
        // Retrieve iterator and value types using begin
        using iterator = decltype(std::declval<C>().begin());
        using value_type = decltype(*std::declval<C>().begin());

        using dependencies = dependencies<C>;
        using definitions = definitions<C>;

        std::reference_wrapper<C> value;

        constexpr iterator begin() const { return value.get().begin(); }
        constexpr iterator end() const { return value.get().end(); }
    };

    template<is_container C>
    constexpr wrapped_container<C> operator-(C& v) { return { v }; }

    // A container that is linked to a name (var)
    template<string_literal Name, is_container C>
    struct named_container : wrapped_container<C> {
        using definitions = definitions<C>::template append<var_t<Name>>::unique;

        constexpr static auto name = Name;
    };

    template<is_var Var, is_container C>
    constexpr auto operator<(Var, wrapped_container<C>&& c) {
        return named_container<Var::name, C>{ std::move(c) };
    }

    template<class Ty>
    concept is_named_container = is_named<Ty> && is_container<Ty>;

    // Concatenation of list comprehension parts

    template<class Expr, class ...Parts>
    struct lc_construct {
        using definitions = concat<definitions<std::decay_t<Parts>>...>;
        using dependencies = remove<definitions, concat<dependencies<Expr>, dependencies<std::decay_t<Parts>>...>>;

        Expr expression;
        std::tuple<Parts...> parts;
    };

    template<class Ty>
    concept is_lc_construct = specialization<Ty, lc_construct>;

    template<class Ty>
    concept valid_construct_part = has_definitions<Ty> || has_dependencies<Ty>;

    // Start construction with an expression and a part.
    template<is_expression A, valid_construct_part B>
    constexpr auto operator|(A&& expr, B&& part) {
        return lc_construct{ std::move(expr), std::forward_as_tuple(std::move(part)) };
    }

    // Add a part to a construct object.
    template<is_lc_construct A, valid_construct_part B>
    constexpr auto operator,(A&& a, B&& b) {
        return lc_construct{ std::move(a.expression),
            std::tuple_cat(std::move(a.parts), std::forward_as_tuple(std::move(b))) };
    }

    // Construct final list comprehension object:

    constexpr static auto container_filter = []<kaixo::is_named_container>{};
    constexpr static auto constraint_filter = []<kaixo::is_expression>{};

    template<class Expr, class ...Parts>
    struct list_comprehension {

        constexpr list_comprehension(lc_construct<Expr, Parts&&...>&& parts)
            : expression(std::move(parts.expression)),
            containers(iterate<pack<Parts...>::template
                indices_filter<container_filter>>([&]<std::size_t ...Is>() {
            return std::tuple{ std::get<Is>(parts.parts)... };
        })),
            constraints(iterate<pack<Parts...>::template
                indices_filter<constraint_filter>>([&]<std::size_t ...Is>() {
            return std::tuple{ std::get<Is>(parts.parts)... };
        })) {}

        Expr expression;

        pack<Parts...>::template filter<container_filter>::template as<std::tuple> containers;
        pack<Parts...>::template filter<constraint_filter>::template as<std::tuple> constraints;
    };


    struct lc_t {
        template<class Expr, class ...Parts>
        constexpr auto operator[](lc_construct<Expr, Parts...>&& lc) const {
            return list_comprehension<Expr, std::decay_t<Parts>...>{ std::move(lc) };
        }
    };

    constexpr lc_t lc;

    // Operator overloads:

    // function to make writing a general operator overload a little easier
    // extracts the 3 different cases: expression, var, value.
    template<class Ty, class ...Args>
    constexpr decltype(auto) use(Ty& v, const named_tuple<Args...>& vals) {
        if constexpr (is_expression<Ty>) return v(vals); // Run expression
        else if constexpr (is_var<Ty>) return vals.get<Ty>(); // Get value of var
        else return v; // Forward the value
    }

    // to prevent operator overloads for any 2 types, either of the
    // arguments must be a variable or an expression.
    template<class A, class B>
    concept valid_op = (is_var<A> || is_expression<A> || is_var<B> || is_expression<B>)
        && !is_named_container<A> && !is_named_container<B>;

    // Operator macro to define all operators more efficiently
#define def_op(op)                                                  \
    template<class Av, class Bv>                                    \
        requires valid_op<std::decay_t<Av>, std::decay_t<Bv>>       \
    constexpr auto operator op(Av&& a, Bv&& b) {                    \
        using A = std::decay_t<Av>;                                 \
        using B = std::decay_t<Bv>;                                 \
        /* Lots of ifs to prevent capturing of vars, as we only */  \
        /* need their type, not their value. Saves some memory. */  \
        if constexpr (is_var<A> && is_var<B>) {                     \
            return expression{ [                                    \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return vals.get<A>() op vals.get<B>();              \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        } else if constexpr (is_var<A> && !is_var<B>) {             \
            return expression{ [                                    \
                b = std::forward<Bv>(b)                             \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return vals.get<A>() op use(b, vals);               \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        } else if constexpr (!is_var<A> && is_var<B>) {             \
            return expression{ [                                    \
                a = std::forward<Av>(a)                             \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return use(a, vals) op vals.get<B>();               \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        } else {                                                    \
            return expression{ [                                    \
                a = std::forward<Av>(a), b = std::forward<Bv>(b)    \
            ] <class...Args>(const named_tuple<Args...>&vals) {     \
                return use(a, vals) op use(b, vals);                \
            }, concat<dependencies<A>, dependencies<B>>{} };        \
        }                                                           \
    }

    def_op(+) def_op(-) def_op(*) def_op(/ ) def_op(%) def_op(&&) def_op(|| );
    def_op(| ) def_op(&) def_op(^) def_op(== ) def_op(!= ) def_op(<< ) def_op(>> );
    def_op(<= ) def_op(>= ) def_op(> ) def_op(< ) def_op(+= );
#undef def_op
}



template<class Ty>
struct get_t {
    template<class Access>
    constexpr get_t(Access access)
        : data(std::bit_cast<void*>(access)),
        access(&access_impl<Access>) {}

    constexpr get_t(Ty& val)
        : data(&val), access(&access_impl2) {}

    constexpr get_t()
        : data(nullptr), access(access_impl2) {}

    void* data;
    Ty(*access)(void*);

    template<class Access>
    constexpr static Ty access_impl(void* data) {
        return std::bit_cast<Access>(data)();
    }

    constexpr static Ty access_impl2(void* data) {
        return *static_cast<Ty*>(data);
    }
};

template<class Ty>
struct set_t {
    template<class Access>
    constexpr set_t(Access access)
        : data(std::bit_cast<void*>(access)),
        access(&access_impl<Access>) {}

    constexpr set_t(Ty& val)
        : data(&val), access(&access_impl2) {}

    constexpr set_t()
        : data(nullptr), access(access_impl2) {}

    void* data;
    void(*access)(void*, Ty);

    template<class Access>
    constexpr static void access_impl(void* data, Ty val) {
        (*reinterpret_cast<Access*>(data))(val);
    }

    constexpr static void access_impl2(void* data, Ty val) {
        *static_cast<Ty*>(data) = val;
    }
};

template<class Ty, class Me = void>
struct alias {
    constexpr alias& operator=(Ty val) { set.access(set.data, val); return *this; }
    constexpr operator Ty() const { return get.access(get.data); }

    get_t<Ty> get;
    set_t<Ty> set;

    friend Me;
};

template<class Ty, class Me = void>
struct readonly {
    constexpr operator Ty() const { return get.access(get.data); }

    get_t<Ty> get;
    set_t<Ty> set;

private:
    constexpr readonly& operator=(Ty val) { set.access(set.data, val); return *this; }
    friend Me;
};

template<class Ty, class Me = void>
struct writeonly {
    constexpr writeonly& operator=(Ty val) { set.access(set.data, val); return *this; }

    get_t<Ty> get;
    set_t<Ty> set;

private:
    constexpr operator Ty() const { return get.access(get.data); }
    friend Me;
};


// INTERFACE THING

template<class T> struct add_any_arg { using type = T(std::any&); };
template<class R, class ...As>
struct add_any_arg<R(As...)> { using type = R(std::any&, As...); };
template<class Ty> using add_any_arg_t = typename add_any_arg<Ty>::type;
template<class T> struct get_ret_type { using type = T; };
template<class R, class ...As>
struct get_ret_type<R(As...)> { using type = R; };
template<class Ty> using get_ret_type_t = typename get_ret_type<Ty>::type;

template<class Ty>
concept has_signature = requires() {
    typename Ty::signature;
};

template<class Ty>
struct type_impl {
    using type = typename Ty::type&;
};

template<class Ty> requires has_signature<Ty>
struct type_impl<Ty> {
    using type = typename Ty::signature;
};

template<class Fun>
using type_impl_t = typename type_impl<Fun>::type;

template<class Ty>
struct member {
    member(const member&) = delete;
    member(member&&) = default;
    member& operator=(const member&) = delete;
    member& operator=(member&&) = default;

    member(Ty& val) : _value(&val) {}

    operator Ty& () { return *_value; }
    operator Ty const& () const { return *_value; }
    template<std::convertible_to<Ty> Arg>
    member& operator=(Arg&& v) { return (*_value = std::forward<Arg>(v), *this); }

private:
    Ty* _value;
};

template<class ...Funs>
class interface {
    using vtable = std::tuple<add_any_arg_t<type_impl_t<Funs>>*...>;
public:
    interface(const interface&) = delete;
    interface(interface&&) = default;
    interface& operator=(const interface&) = delete;
    interface& operator=(interface&&) = default;

    template<class Ty>
    interface(Ty& value) : _storage(std::ref(value)), _vtable{
        [](std::any& val, auto ...args) -> get_ret_type_t<type_impl_t<Funs>> {
            if constexpr (has_signature<Funs>) {
                constexpr auto v = &Funs::template call<Ty>;
                return v(std::any_cast<std::reference_wrapper<Ty>&>(val).get(), args...);
            }
 else {
  constexpr auto v = &Funs::template get<Ty>;
  return v(std::any_cast<std::reference_wrapper<Ty>&>(val).get(), args...);
}
}... } {}

template<class Ty>
interface(Ty&& value) : _storage(std::forward<Ty>(value)), _vtable{
    [](std::any& val, auto ...args) -> get_ret_type_t<type_impl_t<Funs>> {
        if constexpr (has_signature<Funs>) {
            constexpr auto v = &Funs::template call<Ty>;
            return v(std::any_cast<Ty&>(val), args...);
        }
else {
 constexpr auto v = &Funs::template get<Ty>;
 return v(std::any_cast<Ty&>(val), args...);
}
}... } {}

template<class Fun, class ...Args> decltype(auto) call(Args&& ...args) const {
    using type = add_any_arg_t<type_impl_t<Fun>>*;
    return std::get<type>(const_cast<vtable&>(_vtable))(
        const_cast<std::any&>(_storage), std::forward<Args>(args)...);
}

template<class Fun> auto get() -> decltype(call<Fun>()) {
    return call<Fun>();
}

private:
    std::any _storage;
    vtable _vtable;
};

struct DrawContext { /* ... */ };
struct Point {
    double x;
    double y;
};

struct draw_fun {
    using signature = void(DrawContext&);
    void call(this auto& self, DrawContext& c) { self.draw(c); }
};

struct update_fun {
    using signature = void();
    void call(this auto& self) { self.update(); }
};

struct hitbox_fun {
    using signature = bool(const Point&);
    bool call(this const auto& self, const Point& p) { return self.hitbox(p); }
};

struct value_mem {
    using type = int;
    int& get(this auto& self) { return self.value; }
};

struct GuiObject : interface<
    draw_fun,
    update_fun,
    hitbox_fun,
    value_mem
> {
    using interface::interface;

    void draw(DrawContext& v) { call<draw_fun>(v); }
    void update() { call<update_fun>(); }
    bool hitbox(const Point& p) { return call<hitbox_fun>(p); }
    member<int> value = get<value_mem>();
};

struct CustomGuiObject {
    int value = 1;
    void draw(DrawContext& v) { std::cout << value << ": drawing...\n"; }
    void update() { std::cout << value << ": updating...\n"; }
    bool hitbox(const Point& p) const {
        std::cout << value << ": hitbox test: [" << p.x << ", " << p.y << "]\n";
        return false;
    }
};

void setValue(GuiObject obj, int v) {
    obj.value = v;
}

template<auto V>
struct constant_v {
    constexpr static auto value = V;
};

template<char ...S>
consteval std::size_t to_index() {
    constexpr std::size_t size = sizeof...(S);
    using values = kaixo::to_pack<static_cast<int>(S - '0')...>;
    return[&]<std::size_t ...Is>(std::index_sequence<Is...>) {
        std::size_t m = 1;
        std::size_t res = 0;
        ((res += m * (values::template element<size - Is - 1>), m *= 10), ...);
        return res;
    }(std::make_index_sequence<size>{});
}

template<char ...S>
    requires ((S >= '0' && S <= '9') && ...)
consteval constant_v<to_index<S...>()> operator""_() {
    return {};
}

template<class ...Tys>
struct tuple : std::tuple<Tys...> {
    using std::tuple<Tys...>::tuple;
    using types = kaixo::pack<Tys...>;

    template<class Self, std::size_t I>
    constexpr auto operator[](this Self&& self, constant_v<I>)
        noexcept(noexcept(std::get<I>(std::forward<Self>(self))))
        -> decltype(std::get<I>(std::forward<Self>(self))) {
        return            std::get<I>(std::forward<Self>(self));
    }
};










#include <variant>

template<class ...Args, class ...Functors>
constexpr void visit(std::variant<Args...>& v, Functors&& ...functors) {
    const kaixo::overloaded _overloaded{ std::forward<Functors>(functors)... };
    using types = kaixo::pack<Args...>;
    kaixo::generate_template_switch<sizeof...(Args)>(
        [&]<std::size_t I> {
        using type = typename types::template element<I>;
        _overloaded(std::get<type>(v));
    }
    )(v.index());
}



void myfun(int, double, float, char, long) {}

template<kaixo::type_concepts::aggregate Ty> // require aggregate type
    requires (kaixo::info<Ty>::members::size > 1 // With at least 2 members
&& kaixo::info<Ty>::members::are_arithmetic) // And all members are arithmetic
constexpr auto sum_struct(const Ty& val) {
    using type_info = kaixo::info<Ty>; // Sum all members
    return kaixo::sequence<type_info::members::size>([&]<std::size_t ...Is>{
        return ((val.*type_info::template member<Is>) + ...);
    });
};


int func(double&, int, long&&);

#include <vector>

struct serialized_data {
    std::size_t read_index = 0;
    std::vector<std::byte> bytes;

    constexpr void seek(std::size_t v) { read_index = v; }
    constexpr void seek_start() { seek(0); }

    template<kaixo::type_concepts::trivial Ty>
    constexpr void write(const Ty& val) {
        constexpr std::size_t size = sizeof(Ty);
        const auto start = reinterpret_cast<const std::byte*>(&val);
        bytes.insert(bytes.end(), start, start + size);
    }

    template<class Ty> requires requires(Ty ty) {
        { std::begin(ty) };
        { std::end(ty) };
        { std::size(ty) };
    }
    constexpr void write(const Ty& val) {
        using type = std::decay_t<decltype(*std::begin(val))>;
        bytes.reserve(bytes.size() + std::size(val) * sizeof(type));
        write<std::size_t>(std::size(val));
        for (auto& v : val) write(v);
    }

    template<kaixo::type_concepts::aggregate Ty>
    constexpr void write(const Ty& val) {
        using info = kaixo::info<Ty>;
        constexpr std::size_t size = info::members::size;
        kaixo::sequence<size>([&]<std::size_t ...Is>{
            (write(val.*info::template member_ptr<Is>), ...);
        });
    }

    template<kaixo::type_concepts::trivial Ty>
    constexpr Ty read() {
        constexpr std::size_t size = sizeof(Ty);
        const auto start = bytes.data() + read_index;
        read_index += size;
        return *reinterpret_cast<const Ty*>(start);
    }

    template<class Ty> requires requires(Ty ty) {
        { std::begin(ty) };
        { std::end(ty) };
        { std::size(ty) };
    }
    constexpr Ty read() {
        using type = std::decay_t<decltype(*std::begin(std::declval<Ty>()))>;
        std::size_t size = read<std::size_t>();
        Ty val{};
        for (std::size_t i = 0; i < size; ++i)
            val.insert(std::end(val), read<type>());
        return val;
    }

    template<kaixo::type_concepts::aggregate Ty>
    constexpr Ty read() {
        using members = kaixo::info<Ty>::members;
        return members::for_each([&]<class ...Tys>{
            return Ty{ read<Tys>()... };
        });
    }

    template<class Ty>
    constexpr auto& operator<<(const Ty& val) {
        write(val);
        return *this;
    }

    template<class Ty>
    constexpr auto& operator>>(Ty& val) {
        val = read<Ty>();
        return *this;
    }
};

#include <map>
#include <typeindex>
#include <any>

struct runtime {

    struct member_info {
        std::string_view name;
        std::size_t offset;
        const std::type_info* type;
    };

    struct function_info {
        std::string_view name;
        void* value;
        const std::type_info* result;
        std::vector<const std::type_info*> arguments;
    };

    struct class_info {
        std::map<std::string_view, member_info> members;
        std::map<std::string_view, function_info> functions;
    };

    std::map<std::type_index, class_info> registered_classes;

    template<class Ty>
    inline auto add(std::string_view name, Ty val) {

        using type_info = kaixo::info<Ty>;

        if constexpr (type_info::is_member_object_pointer) {
            using object = type_info::object::type;
            using type = type_info::value_type::type;
            auto offset = ((std::size_t) & reinterpret_cast<char const volatile&>((((object*)0)->*val)));
            std::type_index _type = typeid(object);
            registered_classes[_type].members.emplace(name, member_info{
                .name = name,
                .offset = offset,
                .type = &typeid(type)
                });
        }
        else if constexpr (type_info::is_member_function_pointer) {
            using object = type_info::object::type;
            using signature = type_info::signature;
            std::type_index _type = typeid(object);
            registered_classes[_type].functions.emplace(name, function_info{
                .name = name,
                .value = std::bit_cast<void*>(val),
                .result = &typeid(typename signature::result::type),
                .arguments = kaixo::sequence<signature::arguments::size>([&]<std::size_t ...Is> {
                    std::vector<const std::type_info*> _arguments{};
                    _arguments.reserve(signature::arguments::size);
                    ((_arguments.push_back(&typeid(typename signature::arguments::template element<Is>))), ...);
                    return _arguments;
                })
                });
        }

        return val;
    }
} runtime;

template<auto Ty>
constexpr std::string_view get_type_or_fun_name(std::string_view name) {
    using info = kaixo::info_v<Ty>;
    if constexpr (info::is_member_function_pointer)
        return info::function_name;
    else if constexpr (info::is_member_object_pointer)
        return name.substr(name.find_last_of(":") + 1);
    else return "";
}

#define KAIXO_GLUE(a, b) KAIXO_GLUE_I(a, b)
#define KAIXO_GLUE_I(a, b) a##b
//#define register(x) auto KAIXO_GLUE(_registered_type_, __COUNTER__) = runtime.add(get_type_or_fun_name<&x>(#x), &x);

template<std::size_t I>
constexpr auto _register_type = kaixo::dud{};

constexpr std::size_t _type_register_start = __COUNTER__;

template<auto V, kaixo::string_literal Name>
struct type_storage {
    constexpr static auto value = V;
    constexpr static std::string_view name = Name.view();
};

struct type_info_storage_t {
    std::map<std::type_index, std::map<std::string_view, std::size_t>> storage;
} type_info_storage;

#define register_impl(x, c) template<>                         \
constexpr auto _register_type<c - _type_register_start - 1> =  \
type_storage<&x, #x>{};                                        \
std::size_t KAIXO_GLUE(_kaixo_init, c) = [](){                 \
    constexpr auto name = get_type_or_fun_name<&x>(#x);        \
    using object = kaixo::info<decltype(&x)>::object::type;    \
    type_info_storage.storage[typeid(object)][name] =          \
        c - _type_register_start - 1;                          \
    return c - _type_register_start - 1;                       \
}();
//#define register(x) register_impl(x, __COUNTER__)


template<class ...Args>
struct dynamic_call_result {
    std::tuple<Args...> args;
    void* object = nullptr;
    void* value = nullptr;
    const std::type_info* type = nullptr;

    template<class Ty>
    operator Ty () {
        if (typeid(Ty(void*, Args...)) == *type) {
            Ty(*fun)(void*, Args...) = static_cast<Ty(*)(void*, Args...)>(value);
            return std::apply(fun, std::tuple_cat(std::tuple{ object }, args));
        }
        else throw std::bad_cast();
    }
};

struct dynamic_access_result {
    void* object = nullptr;
    void* value = nullptr;
    const std::type_info* type = nullptr;

    template<class Ty>
    operator Ty& () {
        if (typeid(Ty) == *type)
            return *static_cast<Ty*>(value);
        else throw std::bad_cast();
    }

    template<class ...Args>
    dynamic_call_result<Args...> operator()(Args&& ...args) {
        if (typeid(void(void*, Args...)) == *type) {
            void(*fun)(void*, Args...) = static_cast<void(*)(void*, Args...)>(value);
            std::apply(fun, std::tuple{ object, args... });
            return {};
        }
        else return dynamic_call_result<Args...>{
            .args = std::tuple{ std::forward<Args>(args)... },
                .object = object,
                .value = value,
                .type = type
        };
    }
};

template<auto V>
dynamic_access_result handle_access(void* ptr) {
    using info = kaixo::info_v<V.value>;
    if constexpr (info::is_member_object_pointer) {
        using object = info::object::type;
        object* value = static_cast<object*>(ptr);
        return dynamic_access_result{
            .object = ptr,
            .value = &(value->*(V.value)),
            .type = &typeid(value->*(V.value))
        };
    }
    else if constexpr (info::is_member_function_pointer) {
        using object = info::object::type;
        using fptr = info::arguments::
            template prepend<void*>:: // Add void* as first argument
            template to_function<typename info::result::type>;
        constexpr auto _fun = []<class ...Args>(void* self, Args...args)
            -> typename info::result::type {
            return ((*static_cast<object*>(self)).*(V.value))(args...);
        };
        fptr* _ptr = _fun;
        return dynamic_access_result{
            .object = ptr,
            .value = _ptr,
            .type = &typeid(fptr)
        };
    }
    return {};
}

template<class Ty = int>
auto access(void* ptr, const std::type_info& type, std::string_view name) {
    std::size_t index = type_info_storage.storage[type][name];

    return kaixo::generate_template_switch<32ull>([&]<std::size_t I> {
        using _type = std::decay_t<decltype(_register_type<I>)>;
        if constexpr (!std::is_same_v<_type, kaixo::dud>) {
            return handle_access<_register_type<I>>(ptr);
        }
        else return dynamic_access_result{};
    })(index);
}

struct dynamic {
    void* data;
    const std::type_info* type;
    void(*deleter)(void*);

    template<class Ty>
    dynamic(Ty* data)
        : deleter(&kaixo::RTTI_Ftable<Ty>::deleter),
        data(static_cast<void*>(data)), type(&typeid(Ty)) {}

    dynamic(const dynamic&) = delete;
    dynamic(dynamic&& other) : data(other.data), type(other.type), deleter(other.deleter) {
        other.data = nullptr, other.type = nullptr, other.deleter = nullptr;
    };

    template<class Ty>
    dynamic& operator=(Ty* ptr) {
        if (data) deleter(data);
        deleter = kaixo::RTTI_Ftable<Ty>::deleter;
        data = static_cast<void*>(ptr);
        type = &typeid(Ty);
        return *this;
    }

    dynamic& operator=(dynamic&& other) {
        data = other.data, type = other.type, deleter = other.deleter;
        other.data = nullptr, other.type = nullptr, other.deleter = nullptr;
        return *this;
    }

    template<class Ty>
    auto operator[](Ty v) {
        return access<>(data, *type, v);
    }

    ~dynamic() { if (deleter) deleter(data); }
};



struct MyClass {
    int value = 10;

    void add(int val) { value += val; }
};

register(MyClass, value);
register(MyClass, add);


struct N {
    std::string name;
    std::vector<int> values;
    std::size_t length;
};

register(N, name);
register(N, values);
register(N, length);




struct Struct {
    int f = 3;
    double a = 1;
    char c = 6;
    double e = 2;
    int b = 4;
    long d = 5;
};

class Base {

};

class Derived : public Base {

};

template<class ...Args>
struct forwarding_tuple : std::tuple<Args&&...> {
    using types = kaixo::pack<Args&&...>;
    using std::tuple<Args&&...>::tuple;

    template<std::size_t I>
    constexpr typename types::template element<I> get() {
        using result = typename types::template element<I>;
        if constexpr (kaixo::info<result>::is_lvalue_reference)
            return std::get<I>(*this);
        else if constexpr (kaixo::info<result>::is_rvalue_reference)
            return std::move(std::get<I>(*this));
    }
};

template<class ...Args>
forwarding_tuple(Args&&...)->forwarding_tuple<Args...>;

//template<std::size_t I, class ...Args>
//constexpr decltype(auto) get(Args&&...args) {
//    forwarding_tuple _args{ std::forward<Args>(args)... };
//
//    return _args.get<I>();
//}

template<class ...Args>
struct template_pack : std::tuple<std::add_lvalue_reference_t<Args>...>, kaixo::pack<Args...> {
    using _parent = std::tuple<std::add_lvalue_reference_t<Args>...>;
    using _parent::_parent;

    template<std::size_t I>
    constexpr decltype(auto) get() const {
        using result = template_pack::element_info<I>;
        if constexpr (result::is_lvalue_reference)
            return std::get<I>(*this);
        else return std::move(std::get<I>(*this));
    }
};

template<class Ty>
struct forwarder {
    constexpr forwarder(Ty val) {}
};

template<class Ty>
forwarder(Ty)->forwarder<Ty>;

template<std::size_t I, class ...Args>
constexpr decltype(auto) g(Args&&...args) {
    const template_pack<Args...> _args{ args... };

    return _args.get<I>();
}


namespace event_example {
    struct ref_counted {
        virtual ~ref_counted() = default;
        void remember() { ++_refs; }
        void forget() { if (--_refs == 0) delete this; }
        std::size_t _refs = 1;
    };

    template<class Ty> struct pointer {
        constexpr pointer(Ty* value) : value(value) {}
        constexpr ~pointer() { clean(); }
        constexpr pointer(const pointer& p) : value(p.value) { value->remember(); }
        constexpr pointer(pointer&& p) noexcept : value(p.value) { p.value = nullptr; }
        constexpr pointer& operator=(const pointer& p) { clean(), value = p.value, value->remember(); }
        constexpr pointer& operator=(pointer&& p) { clean(), value = p.value, p.value = nullptr; }
        constexpr Ty* operator->() { return value; }
        constexpr Ty& operator*() { return *value; }
        template<class T> requires std::derived_from<Ty, T>
        constexpr operator pointer<T>() { value->remember(); return { dynamic_cast<T*>(value) }; }
        void clean() { if (value) value->forget(); }
        Ty* value = nullptr;
    };

    using state_id = const void*;
    template<class Ty> struct state {
        constexpr bool operator==(state_id ptr) const { return ptr == this; };
    };

    struct event_listener;
    struct event {
        virtual ~event() = default;
        virtual bool forward(const event_listener&) const = 0;
    };

    template<class Ty> struct event_type {
        virtual void handle(const Ty& arg) = 0;
    };

    struct state_storage {
        std::any value;
        template<class Ty> Ty& get() { return std::any_cast<Ty&>(value); }
    };

    struct state_listener : virtual ref_counted {
        virtual void changed(state_id, state_storage&) = 0;
    };

    struct event_listener : virtual ref_counted {
        template<class Event>
        void handle_event(const Event& e) {
            ((event_type<Event>*)this)->handle(e);
        }

        template<class Ty> const Ty& get(const state<Ty>& s) const {
            auto _it = data.find(&s);
            if (_it == data.end() || !_it->second.value.has_value()) return Ty{};
            else return std::any_cast<const Ty&>(_it->second.value);
        }

        template<class Ty, class Val> void set(const state<Ty>& s, Val&& value) {
            data[&s].value.emplace<Ty>(std::forward<Val>(value));
            for (auto& _l : state_listeners) _l->changed(&s, data[&s]);
        }

        template<class Ty> void link(pointer<Ty> obj) {
            state_listeners.push_back(obj);
        }

        template<class Ty> void unlink(pointer<Ty> obj) {
            state_listeners.erase(std::remove_if(state_listeners.begin(), state_listeners.end(),
                [&](pointer<state_listener>& p) { return p.value == obj.value; }), state_listeners.end());
        }

        std::map<state_id, state_storage> data{};
        std::vector<pointer<state_listener>> state_listeners{};
        std::vector<pointer<event_listener>> event_listeners{};
    };

    struct object : event_listener {};





    constexpr state<bool> Hovering;

    struct MyEvent : event {
        bool forward(const event_listener& obj) const override { return obj.get(Hovering); };
    };

    struct MyEvent2 : event {
        bool forward(const event_listener& obj) const override { return true; };
    };
}

using namespace event_example;

struct Thing : object {
    bool hovering = false;

    Thing() {

    }

    void changed(state_id id, state_storage& value) {
        if (Hovering == id) {
            hovering = value.get<bool>();
        }
    }
};