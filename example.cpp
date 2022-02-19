#include <iostream>

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
        std::bitset<N * N> edges;
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

    std::bitset<N * N> edges;
    std::array<Ty, N> vertices;
};

template<class Ty, size_t N>
graph(std::array<Ty, N>)->graph<Ty, N>;

namespace woof{
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
name_object(const char (&)[Size])->name_object<Size>;

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

#include "meta_struct.hpp"

using namespace kaixo;

using has_carrot = meta_struct<
    field<"carrot", int&>
>;

using has_soup = meta_struct<
    field<"soup", float&>
>;

constexpr auto& get_carrot(has_carrot t) { return t.get<"carrot">(); }
constexpr auto& get_soup(has_soup t) { return t.get<"soup">(); }

using has_carrot_soup = meta_struct<
    field<"carrot", int&>, 
    field<"soup", float&>
>;

constexpr auto get_carrot_plus_soup(has_carrot_soup t) { return t.get<"carrot">() + t.get<"soup">(); }
constexpr auto add_carrot_to_soup(has_carrot_soup t) { t.get<"soup">() += t.get<"carrot">(); }

using carrot_soup_bowl = meta_struct <
    field<"carrot", int, required>,
    field<"soup", float, required>,
    field<"bowl", double, [](auto& self) { return self.get<"soup">() + 100; }>,
    function<"addToBowl", [](auto& self, double amount){ self.get<"bowl">() += amount; }>,
    function<"get", [](auto& self) -> auto& { return self.get<"bowl">(); }>
>;

using has_get = meta_struct<
    virtual_function<"get", double()>
>;

constexpr auto call_get(has_get t) { return t.run<"get">(); }

using carrot_has_get = meta_struct<
    field<"carrot", int>,
    virtual_function<"get", double()>
>;

constexpr auto carrot_plus_call_get(carrot_has_get t) { return t.run<"get">() + t.get<"carrot">(); }

int main()
{
    carrot_soup_bowl _a{ arg<"carrot"> = 1, arg<"soup"> = 1 };

    auto res = carrot_plus_call_get(carrot_soup_bowl{ arg<"carrot"> = 1, arg<"soup"> = 1 });

    auto res1 = get_carrot_plus_soup(_a);
    
    auto& res2 = get_carrot(_a);
    auto& res3 = get_soup(_a);
    
    add_carrot_to_soup(_a);
    
    auto val1 = _a.get<"soup">();
    auto val2 = _a.get<"carrot">();
    auto val3 = _a.get<"bowl">();
    
    _a.run<"addToBowl">(30);
    _a.run<"get">();
    auto val4 = _a.get<"bowl">();

    return 0;
}

