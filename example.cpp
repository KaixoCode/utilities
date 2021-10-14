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

#include "dynamic_array.hpp"


int main()
{
    size_t size = 200;
    const dynamic_array<int> arr{ size };

    // Fill array with 0s
    

    // Loop over array
    for (auto& i : arr) {

    }

    // Access index
    arr[0] = 10;

    // Get size
    std::cout << arr.size();

    constexpr_counter_example();
    type_linker_example();
    pa_function_example();
    smart_tuple_example();
    lambda_example();
    struct_tuple_example();
}