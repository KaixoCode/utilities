#include <iostream>


#include "meta_struct.hpp"

using namespace kaixo;

using transformer = meta_struct<
    field<"a", double>,
    field<"b", double>,
    virtual_function<"transform", double(double, double)>
>;

auto transform(transformer t) { return t.run<"transform">(t.get<"a">(), t.get<"b">()); }


constexpr static auto x = arg<"x">;
constexpr static auto y = arg<"y">;
constexpr static auto width = arg<"width">;
constexpr static auto height = arg<"height">;
constexpr static auto area = arg<"area">;
constexpr static auto start = arg<"start">;
constexpr static auto end = arg<"end">;
constexpr static auto distance = arg<"distance">;
constexpr static auto size = arg<"size">;
constexpr static auto draw = arg<"draw">;

constexpr static auto distance_fun = [](auto& self, auto& other) {
    return std::sqrt(
        (self[x] - other[x]) * (self[x] - other[x]) +
        (self[y] - other[y]) * (self[y] - other[y]));
};

template<class Type>
using point = meta_struct<
    field<"x", Type>,
    field<"y", Type>,
    function<"distance", distance_fun>
>;

constexpr static auto area_fun = [](auto& self) {
    return (self[width] - self[x]) * (self[height] - self[y]);
};

template<class Type>
constexpr static auto start_fun = [](auto& self) {
    return point<Type>{ x = self[x], y = self[y] };
};

template<class Type>
constexpr static auto end_fun = [](auto& self) {
    return point<Type>{ x = self[width], y = self[height] };
};

template<class Type>
using rect = meta_struct<
    field<"x", Type>, field<"y", Type>,
    field<"width", Type>, field<"height", Type>,
    function<"area", area_fun>,
    function<"start", start_fun<Type&>>,
    function<"end", end_fun<Type&>>
>;

using component = meta_struct<
    field<"size", rect<double>>,
    virtual_function<"draw", void()>
>;

using drawable = meta_struct<
    virtual_function<"draw", void()>
>;

#include <vector>

int main() {
    rect<double> _r{ x = 0, y = 0, width = 50, height = 50 };

    std::vector<drawable> drawables;

    component _c{ 
        size = _r, 
        draw = [](auto& self) {
            std::cout << self << '\n';
        } 
    };

    drawable _d = _c;

    drawables.push_back(_c);

    _c[draw]();

    double _area1 = _r[area]();

    point<double&> _start = _r[start]();
    point<double&> _end = _r[end]();

    double _distance = _start[distance](_end);

    meta_struct a1{ arg<"a"> = 1, arg<"b"> = 2, };
    meta_struct a2{ arg<"c"> = 3, arg<"d"> = "hello", };
    meta_struct a3 = construct(a1, a2, arg<"e"> = 5);
    meta_struct a4 = construct(arg<"f"> = 9, arg<"g"> = 7);
    meta_struct a5 = construct(a3, a4, arg<"d"> = "world");

    a5.get<"a">() = 10;

    using mult_transformer = meta_struct<
        function<"transform", [](auto& self, double a, double b){ return a * b; }>
    >;    

    using add_transformer = meta_struct<
        function<"transform", [](auto& self, double a, double b){ return a + b; }>
    >;

    meta_struct arg1{ arg<"a"> = 8, arg<"b"> = 5 };
    meta_struct arg2{ arg<"a"> = 15, arg<"b"> = 34 };

    auto res1 = transform({ mult_transformer(), a5 });
    auto res2 = transform({ add_transformer(), a4 });



    //meta_struct e = adae;




    //meta_struct<
    //    field<"value", int, []{ return 10; }>,
    //    field<"carrot", int>,
    //    field<"potato", int>
    //    //function<"woof", [](auto& self, double b){ return b; }>
    //> d{ c };

    //meta_struct<
    //    virtual_function<"woof", double(double)>
    //> e{ d };


    //my_transform mult_10 = { arg<"mult"> = 10 };

    //mult_10.get<"mult">() = 20;

    //auto res6 = transform_value({ arg<"value"> = 5, mult_10 });



    //auto res = carrot_plus_call_get(carrot_soup_bowl{ arg<"carrot"> = 1, arg<"soup"> = 1 });

    //carrot_soup_bowl _a{ arg<"carrot"> = 1, arg<"soup"> = 1 };
 
    //add_2_to_bowl(_a);
 
    //auto res1 = get_carrot_plus_soup(_a);
 
    //auto& res2 = get_carrot(_a);
    //auto& res3 = get_soup(_a);
    //
    //add_carrot_to_soup(_a);
    //
    //auto val1 = _a.get<"soup">();
    //auto val2 = _a.get<"carrot">();
    //auto val3 = _a.get<"bowl">();
    //
    //_a.run<"addToBowl">(30);
    //_a.run<"get">();
    //auto val4 = _a.get<"bowl">();

    return 0;
}

