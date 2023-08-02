#include <iostream>
#include <set>
#include <deque>
#include <vector>
#include <map>
#include <ranges>
#include <complex>
#include <list>
#include <fstream>

#include "kaixo/type_utils.hpp"

struct MyType {
    MyType() { 
        std::cout << "Construct\n"; 
    }

    MyType(MyType&& v) noexcept
        : a(v.a), b(v.b), c(v.c) { 
        std::cout << "Move\n"; 
    }

    MyType(const MyType& v)
        : a(v.a), b(v.b), c(v.c) {
        std::cout << "Copy\n";
    }
    
    MyType& operator=(MyType&& v) noexcept {
        a = v.a, b = v.b, c = v.c;
        std::cout << "Move Assign\n"; 
        return *this;
    }

    MyType& operator=(const MyType& v) {
        a = v.a, b = v.b, c = v.c;
        std::cout << "Copy Assign\n";
        return *this;
    }

    ~MyType() {
        std::cout << "Destruct\n";
    }

    int a = 1;
    float b = 2;
    std::string c = "Woof";

    template<std::size_t I>
    constexpr auto& get() & {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }

    template<std::size_t I>
    constexpr auto&& get() && {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }
    template<std::size_t I>
    constexpr auto& get() const & {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }

    template<std::size_t I>
    constexpr auto&& get() const && {
        if constexpr (I == 0) return a;
        else if constexpr (I == 1) return b;
        else if constexpr (I == 2) return c;
    }
};

namespace std {
    template<> struct tuple_size<MyType> : std::integral_constant<std::size_t, 3> {};
    template<> struct tuple_element<0, MyType> : std::type_identity<int> {};
    template<> struct tuple_element<1, MyType> : std::type_identity<float> {};
    template<> struct tuple_element<2, MyType> : std::type_identity<std::string> {};
}

struct sA {
    int a;
    std::string b;
};

struct sB {
    std::string a;
    float b;
    int c;
};

struct sC {
    char a;
    short b;
    short c;
    int d;
};

#include "kaixo/expression.hpp"
#include "kaixo/list_comprehension.hpp"
#include "kaixo/overloads.hpp"
#include "kaixo/range.hpp"
#include "kaixo/zipped_range.hpp"
#include "kaixo/break.hpp"

using namespace kaixo;
using namespace kaixo::concepts;
using namespace kaixo::type_traits;
using namespace kaixo::default_variables;
#include "kaixo/serializer.hpp"


constexpr std::uint64_t fnv1a_32(char const* s, std::size_t count) {
    std::uint64_t hash = 2166136261ull;

    for (std::size_t i = 0; i < count; ++i) {
        hash ^= s[i];
        hash *= 16777619ull;
    }

    return hash;
}

template<std::size_t N>
constexpr std::uint64_t constexpr_string_hash(const char(&s)[N]) {
    return fnv1a_32(s, N);
}

template<class Ty>
constexpr std::uint64_t unique_id = constexpr_string_hash(type_name<Ty>.m_Data);


namespace kaixoaa {
    template<class, class, class> 
    struct function_storage;

    template<class Ty, class R, class ...Args>
    struct function_storage<Ty, R, info<Args...>> {
        Ty object;

        constexpr R do_call(Args... args)  {
            return object(std::forward<Args>(args)...);
        }
    };

    template<class Ty> 
    class function {
    public:
        using result_type = info<Ty>::result::type;
        using arguments = info<Ty>::arguments;

        template<class Arg>
            requires arguments::template can_invoke<Arg>::value
        constexpr function(Arg&& arg) { _initialize(std::forward<Arg>(arg)); }

        constexpr ~function() { if (_clean) _clean(_object); }

        template<class Self, class ...Args>
            requires invocable<Ty, Args&&...>
        constexpr result_type operator()(this Self&& self, Args&&...args) noexcept(nothrow_invocable<Ty, Args&&...>){
            return std::forward<Self>(self)._fun(
                std::forward<Self>(self)._object, std::forward<Args>(args)...);
        }

    private:
        using _fun_signature = to_function_t<result_type, typename arguments::template prepend<void*>>;
        void* _object;
        void(*_clean)(void*);
        _fun_signature* _fun;

        template<class Ty>
        constexpr void _initialize(Ty&& val) {
            using type = decay_t<Ty>;
            if constexpr (empty<type>) {
                _clean = nullptr;
                _object = nullptr;
                _fun = [](void*, auto...args) constexpr {
                    return type{}(args...);
                };
            } else if constexpr (concepts::function<remove_pointer_t<type>> && pointer<type>) {
                _clean = nullptr;
                _object = val;
                _fun = [](void* ptr, auto...args) constexpr {
                    return (reinterpret_cast<type>(ptr))(args...);
                };
            } else {
                using _fun_type = function_storage<type, result_type, arguments>;
                _clean = [](void* obj) {
                    delete reinterpret_cast<_fun_type*>(obj);
                };
                _object = new _fun_type{ std::forward<Ty>(val) };
                _fun = [](void* obj, auto...args) constexpr {
                    return reinterpret_cast<_fun_type*>(obj)->do_call(args...);
                };
            }
        }
    };

    template<class Ty>
    function(Ty&&)->function<typename info<decay_t<Ty>>::fun_decay::signature::type>;
}

namespace kaixoaef {

    template<class> struct function_impl_base;
    template<class R, class ...Args> 
    struct function_impl_base<R(Args...)> {
        constexpr virtual R do_call(Args...) = 0;
    };

    template<class>
    class function_impl;
    
    template<class R, class ...Args>
    class function_impl<R(Args...)> {
    public:
        template<class Arg>
            requires invocable<Arg&&, Args...>
        constexpr function_impl(Arg&& arg) { _initialize(std::forward<Arg>(arg)); }

        constexpr R operator()(Args...args) {
            return _impl()->do_call(std::forward<Args>(args)...);
        }

        constexpr static std::size_t _smallSize = 8;

        using _ptr = function_impl_base<R(Args...)>;

        void* _storage[_smallSize];

        template<class Arg>
        constexpr void _initialize(Arg&& arg) {
            using type = decay_t<Arg>;
            // Function pointer
            if constexpr (concepts::function<remove_pointer_t<type>> && pointer<type>) {
                _storage[_smallSize - 1] = &_storage[0];
                _storage[0] = &_storage[1];
                _storage[1] = arg;
                //_storage[2] = +[](void** self, Args ...args) -> R {
                //    return reinterpret_cast<type>(self[1])(std::forward<Args>(args)...);
                //};
            }

        }

        constexpr _ptr* _impl() { return reinterpret_cast<_ptr*>(_storage[_smallSize - 1]); }
    };

    template<class Ty>
    class function : public function_impl<typename info<Ty>::fun_decay::signature::type> {
    public:

    };


    template<class Ty>
    function(Ty&&) -> function<typename info<decay_t<Ty>>::fun_decay::signature::type>;
}

#include "Json.hpp"

template<string_literal Name>
struct _json_property {
    using _is_property = int;
    constexpr static std::string_view name = Name.view();
};

template<class Ty>
concept _is_property = requires() { typename Ty::_is_property; };

#define json_property(Name, C) no_unique_address]] _json_property<Name> KAIXO_UNIQUE_NAME;[[

template<class Ty>
constexpr auto to_json(Ty& value) {
    using types = binding_types_t<Ty>;
    json _result = json::object();
    indexed_for<1, types::size>([&]<std::size_t I>{
        using type1 = types::template element<I - 1>::type;
        using type2 = types::template element<I>::type;
        if constexpr (_is_property<type1>) {
            _result[type1::name] = tuples::get<I>(value);
        }
    });
    return _result;
}

struct JsonObject {
    [[json_property("test")]]
    int test = 1;

    [[json_property("value")]]
    double value = 4.2;

    [[json_property("name")]]
    std::string name = "Hello World";
};


int main() {
    JsonObject _obj;

    auto _json = to_json(_obj);





    //kaixo::function fun1{ [](int a, double b) -> int { return a + b; } };
    //kaixo::function fun2{ myfun };
    //kaixo::function fun3{ [val](int a, double b) -> int { return a + b + val; } };

    //constexpr auto sroin = sizeof kaixo::function<int(int, double)>;

    //auto res1 = fun1(1, 1);
    //auto res2 = fun2(1, 1);
    //auto res3 = fun3(1, 1);

    return 0;
    {
        //auto rsion = aefae | split<int>;

        //auto roisn = rsion.get<0>();

        //std::tuple<int, int, int> t1{ 1, 2, 3 };
        //std::tuple<int, int, int> t2{ 4, 5, 6 };
        //
        ////   | 0 1 2 3 4 5 6 7 8  <- First index
        ////---+------------------
        //// 0 | 1 2 3 1 2 3 1 2 3
        //// 1 | 4 4 4 5 5 5 6 6 6
        //// ^
        //// Second index
        //
        //all_t<std::tuple<int, int, int>>::types::size;
        //
        //auto sorin = cartesian(std::tuple{}, t1, t2);
        //
        //auto oisnr = empty_view{} | join;
        //
        //auto res = cartesian(t1, t2);
        //
        //auto el = std::get<3>(res); // Should get (1, 5)
        //
        //auto& v1 = std::get<0>(el);
        //auto& v2 = std::get<1>(el);
        // ^^^ Yes these are references to the original element!


        //static_assert(pack::cartesian_t<info<int, double>, info<float, char>>::size == 4);

        //decay_t<decltype(feaef)>::cartesian_element_view<6>::types::size;

        //auto roisn = concat(std::move(t1), t2) | get<5>;

        return 0;

    }
    /*
    using namespace kaixo::pack;

    static_assert(same_as<
        info<                                                      // Bunch of function signatures
            void       (&)(double, int        ),                   //
            int        (*)(int,    double     ),                   //
            std::size_t   (float,  std::string),                   //
            std::string(*)(double, int        ),                   //
            void       (*)(double, char       )>                   //
        ::filter<                                                  //
               with<arguments_t>(index_filter_v<is_integral> != 0) // First argument may not be integral
            && with<return_type_t>(!is_void)                       // Return type may not be void
            && is_pointer>                                         // Must be a pointer
        ::element<0>::type,                                        // Grab first element
                                                                   //
        std::string(*)(double, int)                                //
    >);


    

    constexpr auto myfilter = []<std::size_t Index, class Type>() -> bool {
        if constexpr (Index == 0) return std::is_integral_v<Type>;
        else if constexpr (Index == 1) return std::is_floating_point_v<Type>;
        else return false;
    };

    static_assert(same_as<
        conditional_transform_t<myfilter, std::map, info<int, int>>,
        info<int, int>>);
    
    static_assert(same_as<
        conditional_transform_t<myfilter, std::map, info<int, float>>,
        std::map<int, float>>);
    
    static_assert(same_as<
        conditional_transform_t<myfilter, std::map, info<int, float, double>>,
        info<int, float, double>>);

    info_t<std::vector, std::set>::instantiate<int>::size;

    instantiate_t<std::vector, int>;

    zip_t<info<int, double>, info<float, int>>::size;

    cartesian_t<info<int, double>, info<float, short>>::size;

    info<int>::reinstantiate<std::vector<double>>;
    
    info<int>::append<sA>::size;

    concat_t<info<int, double>, info<float, int>>;

    join_t<info<info<int, double>, info<float, int>>>;

    transform_t<std::vector, int>;

    info<int>::transform<std::vector>;

    tparams_t<int>;

    constexpr auto efaefaefa = type_name<int>;

    std::cout << efaefaefa.view() << "\n";

    info<int(void)>::arguments;

    take_last_t<3, int, double, float, int>;

    drop_last_while_t<is_integral, int, double, float, char, short, int>;
    drop_while_t<is_integral, int, double, float, char, short, int>;
    drop_last_t<2, int, double, float, char, short, int>;
    drop_t<1, int, double, float, char, short, int>;

    erase_t<3, int, double, float, short>;

    swap_t<0, short, int, double, float>;

    static_assert(same_as<
        info<sA, sB, sC>                                // Pack of structs
            ::filter<with<binding_types_t>(
                     (count_filter_v<is_integral> > 0)  // Must have at least 1 integral member
                  && (count_unique_v<> == 3)            // Must have at least 3 unique members
                  && (contains_v<std::string>))>        // Must have an std::string as member
            ::element<0>::type,                         // Grab first match
        sB>);                   

    constexpr auto sionr = matches_filter<sizeof_v<> == 4, std::uint32_t>;

    pack::count_unique_v<int, double, float, int, char, double, float, short>;
    pack::first_indices_v<int, double, float, int, char, double, float, short>;
    pack::unique_t<int, double, float, int, char, double, float, short>;

    static_assert(same_as<
        info<std::vector<int>,            // Pack of containers
             std::set<double>, 
             std::deque<char>,
             std::list<std::string>>
            ::template transform<grab::value_type> // Grab value type of containers
            ::filter<(sizeof_v<> > 4)>    // Filter based on size
            ::transform<std::vector>,     // Instantiate as vector

        info<std::vector<double>,         // Only value types > 4 bytes remain
             std::vector<std::string>>    // And containers are now vector
    >);

    pack::reverse_t<MyType>;

    index_not_filter_v<[]<integral>{}, int, short, double, float>;

    pack::insert_t<3, info<int, double>, char, float, short>;

    take_while_t<is_integral, int, short, double, float>; 

    using sorter = pack::sort_t<pack::sorters::size>;
    using sorted = sorter::type<double, int, char, short>;


    pack::replace_t<dud, info<int>, double, int, float, short, int>;

    pack::replace_t<dud, int, double>::type<double, int, float, short, double, short, int>;

    pack::replace_filter_t<dud, is_integral, int, double, short, float>;

    pack::filter_t<is_integral, int, float, double, short, float, long>;
    pack::first_index_v<info<int>, double, float, int>;

    pack::first_indices_v<info<int, float>, int, double, float, short, int>;

    info<int, float, double, short, int, float>::first_indices<int, float>;

    pack::drop_while_t<is_integral, int, short, double, float>;
    pack::take_while_t<is_integral, int, short, double, float>;

    pack::indices_except_v<info<int, short>, int, short, double, float>;

    pack::remove_indices_t<as_array<0, 1, 2, 3>, int, short, double, float>;
    pack::remove_t<info<int, short>, int, short, double, float>;

    pack::keep_indices_t<as_array<1, 2>, int, short, double, float>;
    
    pack::keep_t<info<int, short>, int, short, double, float>;

    static_assert(same_as<
        split_t<info<int>, info<double, int, float, int, short, int>>,
        info<info<double>, info<float>, info<short>, info<>>>);
    
    static_assert(same_as<
        split_before_t<info<int>, info<double, int, float, int, short, int>>,
        info<info<double>, info<int, float>, info<int, short>, info<int>>>);

    static_assert(same_as<
        split_after_t<info<int>, info<double, int, float, int, short, int>>,
        info<info<double, int>, info<float, int>, info<short, int>, info<>>>);
    
    static_assert(same_as<
        split_filter_t<is_same<int>, info<double, int, float, int, short, int>>,
        info<info<double>, info<float>, info<short>, info<>>>);
    
    static_assert(same_as<
        split_before_filter_t<is_same<int>, info<double, int, float, int, short, int>>,
        info<info<double>, info<int, float>, info<int, short>, info<int>>>);

    static_assert(same_as<
        split_after_filter_t<is_same<int>, info<double, int, float, int, short, int>>,
        info<info<double, int>, info<float, int>, info<short, int>, info<>>>);

    static_assert(!matches_filter<[]<floating_point>{}, int>);
    static_assert(matches_filter<[]<integral>{}, int>);
    static_assert(!matches_filter<is_floating_point, int>);
    static_assert(matches_filter<is_integral, int>);
    static_assert(!matches_filter<[]<class Ty>{ return floating_point<Ty>;}, int>);
    static_assert(matches_filter<[]<class Ty>{ return integral<Ty>;}, int>);
    static_assert(!matches_filter<std::false_type{}, int>);
    static_assert(matches_filter<std::true_type{}, int>);
    static_assert(!matches_filter<false, int>);
    static_assert(matches_filter<true, int>);
    static_assert(!matches_filter<0, int>);
    static_assert(matches_filter<1, int>);

    constexpr auto rsoin = count_unique_v<
        int, double, float, int, char, int, short, short, short, 
        short, long, float, float, double, float, double, int, short, char, long long>;

    constexpr auto oirnr = (1 + 0 + 0 + 4) / 2.5;

    constexpr auto sorin = count_all_v<int, double>.evaluate<float, int, double, char, int>();

    constexpr auto roisn = matches_filter<is_same<int>, int>;

    constexpr auto oinsr = pack::detail::indices_impl<info<int>, int, double, char, int, float, short>::value[1];

    evaluate_type_filter<pack::indices_v<int, double>[0] == 1, info<float, int, double>>;

    pack::indices_except_filter_v<is_integral, double, int, float, short, int>;

    pack::index_v<info<int, double>, float, short, double, char>;

    count_all_v<info<int, double>, float, int, double, char, int>;

    matches_filter<pack::count_filter_v<is_integral> == 2, info<int, double, char, float>>;

    pack::element_t<3, int, double, float, char, long, short>;

    pack::detail::index_impl<int, double, float, char, int, long, short>::value;

    matches_filter<pack::index_v<int> == 2, info<double, float, int>>;

    matches_filter<count_all_v<int, double>, info<int, double, char, int>>;

    index_filter_v<is_integral, double, float, int>;

    pack::indices_filter_v<is_integral, double, int, float, char, short>;

    //using namespace kaixo::pack;

    //kaixo::info<kaixo::info<char, int>, kaixo::info<float, double>>::filter<contains_all_v<char, int>>::size;
    //
    //contains_v<int>.value<std::tuple<int, double>>;
    //
    //contains_all<kaixo::info<int, double>, kaixo::info<int, double, float>>::value;
    //
    //contains_all<kaixo::info<int>>::value;
    //
    //kaixo::info<kaixo::info<char, int>, kaixo::info<float, double>>::filter<count_v<int> == 1>::size;
    //
    //
    //kaixo::info<int, double, char, long>::filter<(contains_v<int> > 2)>::size;
    //
    //kaixo::sizeof_v<> == 2;
    //
    //kaixo::sizeof_v<>;
    //element_t<0>;

    return 0;*/
}