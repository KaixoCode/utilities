#include <concepts>

#include <bit>
#include <iostream>

#include "utils.hpp"



using namespace kaixo;
using namespace kaixo::fold;
using namespace kaixo::type_concepts;
using namespace kaixo::type_traits;

#include "linq.hpp"

struct Person {
    std::size_t age;
    std::string_view name;
};

linq_class(Person) {
    linq_member_object(age);
    linq_member_object(name);
};


int main() {


    std::array<Person, 10> people{
        Person{ 12, "Henry" }, Person{ 19, "Jimmy" }, 
        Person{ 21, "Tess" },  Person{ 16, "Sam" },
        Person{ 26, "Ben" },   Person{ 22, "Nate" },
        Person{ 25, "Billy" }, Person{ 29, "Carl" },
        Person{ 15, "Kaerl" }, Person{ 18, "James" },
    };

    auto names = 
        from person in people 
        where person.age >= 18 
            and person.name.starts_with("B") 
        select (person.name);

    for (auto [name] : names) {
        std::cout << name << '\n';
    }

    return 0;
}