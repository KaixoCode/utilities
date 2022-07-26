#include <concepts>
#include <bit>
#include <iostream>
#include <vector>
#include <string>

#include "type_utils.hpp"

struct Struct {
    int a;
    double b;
    float c;
};

int main() {

    using namespace kaixo;

    info<int, double>::insert<2, int>::size;

    return 0;
}