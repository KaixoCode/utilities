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