#include <iostream>
#include <set>
#include <deque>
#include <vector>
#include <map>
#include <ranges>
#include <list>

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
using namespace kaixo::operators;
using namespace kaixo::concepts;
using namespace kaixo::type_traits;
using namespace kaixo::default_variables;
#include "serializer.hpp"


struct MyTrivialStruct {
    int a;
    double b;
};

struct MyNonTrivialStruct {
    std::string str;
    std::vector<std::string> values;
};

class MyClass {
    std::string a;
    int b;

public:
    constexpr MyClass(std::string a, int b)
        :a(a), b(b) {}

    std::string& getA() { return a; }
    int& getB() { return b; }
};

template<>
struct serialize<MyClass> {
    static serialized_object write(MyClass& value) {
        serialized_object _result;
        _result.write(value.getA());
        _result.write(value.getB());
        return _result;
    }

    static MyClass read(serialized_object& data) {
        auto mem1 = data.read<std::string>();
        auto mem2 = data.read<int>();
        return MyClass(std::move(mem1), mem2);
    }
};

int main() {

    serialized_object data;
    {
        int val1 = 420;                        // Trivial type

        std::string val2 = "Hello World";      // Non-trivial type
        
        std::array<int, 4> val3{ 1, 2, 3, 4 }; // Contiguous trivial container

        std::vector<std::string> val4{         // Non-trivial container
            "String 1", "More Strings", "Plenty of Strings" };

        MyTrivialStruct val5{ 10, 2.0323 };    // Trivial struct

        MyNonTrivialStruct val6{               // Non-trivial struct
            .str = "Test",
            .values = { "Woof", "Carrot", "Thing", "Aaa" } };

        MyClass val7{ "Private Member", 69 };  // Class w/ serialize overloads

        data.write(val1);
        data.write(val2);
        data.write(val3);
        data.write(val4);
        data.write(val5);
        data.write(val6);
        data.write(val7);
    }

    {
        auto v1 = data.read<int>();
        auto v2 = data.read<std::string>();
        auto v3 = data.read<std::array<int, 4>>();
        auto v4 = data.read<std::vector<std::string>>();
        auto v5 = data.read<MyTrivialStruct>();
        auto v6 = data.read<MyNonTrivialStruct>();
        auto v7 = data.read<MyClass>();

        return 0;
    }

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