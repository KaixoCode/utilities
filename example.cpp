#include "Parser.hpp"

#include <iostream>


int main() {



    MyLang::MyLexer lexer;

    std::string text = "if (x = 1) then x := 1 else x := 4 + x; ";
    auto res = lexer.Tokenize(text);

    MyLang::TokenVector v = res;
    auto res2 = MyLang::MyParser::TranslationUnit::Parse(v);

    std::cout << "ee";

    return 0;
}