#include "smart_tuple.hpp"
#include "pa_function.hpp"
#include "lambda.hpp"
#include "struct_tuple.hpp"
#include "type_linker.hpp"
#include "constexpr_counter.hpp"

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
template<size_t T> constexpr size_t fib = fib<T - 1> + fib<T - 2>;
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
    do { numbers.push_back(number % 10);
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
            } else
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

    constexpr ref_or_val& operator=(type& v) { return assign(v), *this; }
    constexpr ref_or_val& operator=(type&& v) { return assign(std::move(v)), *this; }
    constexpr ref_or_val& operator=(const type&& v) { return assign(std::move(v)), *this; }
    constexpr ref_or_val& operator=(const ref_or_val& v) { return assign(v), *this; }
    constexpr ref_or_val& operator=(ref_or_val&& v) { return assign(std::move(v)), *this; }
    constexpr ref_or_val& operator=(const type& v) = delete;

    constexpr type& get() { return m_State == state::has_ref ? m_Ref.get() : m_Val; }
    constexpr const type& get() const { return m_State == state::has_ref ? m_Ref.get() : m_Val; }
    constexpr operator type&() { return m_State == state::has_ref ? m_Ref.get() : m_Val; }
    constexpr operator const type&() const { return m_State == state::has_ref ? m_Ref.get() : m_Val; }

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

int main()
{

    var<int> a, b, c;

    std::map<std::string, int> mymap{ { "apple", 1 }, { "carrot", 3 } };
    var<std::string> key;
    var<int> value;
    auto r1 = lc[value | (key, value) <- mymap];

    std::cout << "";
}


void nothing() {
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

    for (auto[val, pos] : _arr.with_index()) {
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

    6 + 7 + 8 + 9 +10 +11 +10 + 9 + 8 + 7 + 6;
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