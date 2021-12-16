#include "Parser.hpp"

#include <iostream>


int main() {



    MyLang::MyLexer lexer;

    std::string code = R"~~(

if (x = not 1) then 
{
    x := 0
    if (y < x and x = 1 or y > x) then
        y := x*(1+y-apple/carrot)
    else
        y := z
}
else 
    x := 0 + x; 


)~~";

    auto res = lexer.Tokenize(code);

    MyLang::TokenVector v = res;
    auto res2 = MyLang::MyParser::TranslationUnit::Parse(v);


    res2->Print();


    return 0;
}