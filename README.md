# list comprehension
 List comprehension in C++. Works by creating `expr<Result>` objects, which are basically lambdas that return the result of some expression, so the result can be evaluated later for some values of `var<Type>` used in the expression. a var holds either a value or a reference.
```cpp
using namespace kaixo;
int main() {
    
    // Simple list comprehension with ranges and a constraint.
    var<int> a, b, c;
    auto r1 = lc[(a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c];

    // Parallel iteration
    var<std::tuple<int, int>> d;
    auto r2 = lc[d | d <- (range(0, 10), range(0, 10))];
    
    // Iteration on some container
    std::vector<int> data{ 1, 2, 3, 4, 5 };
    auto r3 = lc[a + b | a <- data, b <- range(0, 10)];

    // Determine resulting container
    std::vector<std::string> strings{ "hello", "carrot", "pizza" };
    var<std::string> e;
    auto r4 = lc[map(e, a) | (e, a) <- (strings, range(0, 100))]; 
    
    // Call std functions
    std::vector<int> ints1{ 5, 2, 7, 3, 1, 9 };
    std::vector<int> ints2{ 4, 1, 8, 9, 3, 2 };
    auto r5 = lc[max(a, b) | (a, b) <- (ints1, ints2)];
    
    // Automatically uses std::string when working with characters (to<char>() because tolower() returns int)
    std::string mystr = "HelloWorld";
    var<char> g;
    std::string r6 = lc[tolower(g).to<char>() | g <- mystr];
    
    // Nested list comprehension for lists of lists
    var<int> x;
    var<std::vector<int>> xs;
    std::vector<std::vector<int>> xxs{ { 1,3,5,2,3,1,2,4,5 }, { 1,2,3,4,5,6,7,8,9 }, { 1,2,4,2,1,6,3,1,3,2,3,6 } };
    auto r8 = lc[lcv[x | x <- xs, x % 2 == 0] | xs <- xxs];
    
    // Using a used variable as a container
    auto r9 = lc[x | xs <- xxs, x <- xs];
    
    // Make a utility function
    auto indices = [x = var<int>{}, i = var<int>{}](auto& data, var<int> a) mutable {
        // Parallel iteration of value and index, constraint on value == argument, store index.
        return lc[i | (x, i) <- (data, range(0, data.size())), a == x];
    };

    std::vector<int> datas{ 0, 1, 1, 0, 0, 1, 0 };
    auto r10 = indices(datas, 1);
    
    // Parallel iteration with names for each instead of tuple
    auto r11 = lc[(a + b + c) | (a, b, c) <- (range(0, 10), range(0, 10), range(0, 10))];
}
```
# axial_array
 Axial array for storing hexagonal grids of data. coords (0, 0) are the center, here's a [link](https://www.redblobgames.com/grids/hexagons/#coordinates-axial) to a really cool website that explains axial coordinates.
```cpp
    // Axial array with sidelength 3
    const axial_array<int, 3> _arr{
            {  1,  2,  3 },
          {  4,  5,  6,  7 },
        {  8,  9, 10, 11, 12 },
          { 13, 14, 15, 16 },
            { 17, 18, 19 },
    };

    // Iterate with indices
    for (auto[val, pos] : _arr.with_index()) {
        std::cout << pos.x << ", " << pos.y << " : " << val << std::endl;
    }

    // Iterate normally
    for (auto& val : _arr) {
        std::cout << val << std::endl;
    }
    
    // Index access
    _arr[{ 0, 0 }]; // 0, 0 is the center.
    
    // Some cool looking constructors, you can also directly construct
    // with 1d array, since behind the scenes it is just a single array.
    // values are stored in the order that they are numbered down below.
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
```
# pa_function
 Partial application function class in C++ with consistent typing. Works with functors, (capturing) lambdas, function pointer, and member functions. 
```cpp
using namespace kaixo;
int Add(int a, int b) { return a + b; }
struct Object { int Add(int a, int b) { return a + b; } };

struct Functor
{
    int someValue;
    int operator()(int a, int b) { return a + b + someValue; }
};

int main()
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
```

# smart_tuple
 Not really that smart, just added an index operator. It checks if the implicit casting is to the correct type at runtime, and throws a ```std::bad_cast``` if it tries to cast to the wrong type. Also added a ```smart_tuple::get<size_t>()``` method and you can explicitly get a type from a result like this: ```tuple[1].get<int>()```
```cpp
using namespace kaixo;
int main()
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
```

# lambda
 Lambda class that can deduce the types of the captured variables. The deduction guide only works with captures that are not a reference, and are default constructible.
```cpp
using namespace kaixo;
void lambda_example()
{
    int a = 0;
    double b = 1;

    lambda t = [=](int c) -> int { return a + b + c; };

    std::cout << typeid(decltype(t)).name() << std::endl;

    t.get<0>() += 10;
	t.get<1>() += 10;

    std::cout << t(1) << std::endl;
}
```
