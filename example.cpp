#include "Parser.hpp"

#include <iostream>



int main() {
        


    MyLang::MyLexer lexer;

    std::string code = R"~~(
x := 10
if (x > 01) then
    y := x + 10111
else
    z := x * 11101;



)~~";


    auto res = lexer.Tokenize(code);

    MyLang::TokenVector v = res;
    auto res2 = MyLang::MyParser::TranslationUnit::Parse(v);
    MyLang::Context ctx;
    res2->Eval(ctx);



    res2->Print();


    return 0;
}