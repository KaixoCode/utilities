#include "Parser.hpp"

#include <map>
#include <iostream>

template<class Symbol = std::uint8_t, class State = std::size_t>
struct TuringMachine
{
    enum Direction { Left = -1, None = 0, Right = 1 };
    constexpr static State HALT = static_cast<State>(-1);
    using Instruction = std::tuple<State, Symbol, Direction>;

    std::map<std::pair<State, Symbol>, Instruction> instructions;
    std::list<Symbol> tape{ 0 };
    std::list<Symbol>::iterator head = tape.begin();
    State state = 0;

    Symbol& Read() { return *head; }
    Symbol& Move(Direction d)
    {
        return d == Left ? head == tape.begin() ? *(head = tape.insert(tape.begin(), 0)) : *--head
            : d == Right ? ++head == tape.end() ? *(head = tape.insert(tape.end(), 0)) : *head : *head;
    }

    bool Step()
    {
        auto& [nstate, write, move] = instructions.at({ state, Read() });
        return (Read() = write, Move(move), state = nstate) != HALT;
    }
};

int main() {
    using enum TuringMachine<>::Direction;
    enum : std::size_t { _ = 2, HALT = TuringMachine<>::HALT };
    TuringMachine<> machine { 
        {   // Instruction table for binary addition
            { { 0, 0, }, { 0, 0, Right } },
            { { 0, 1, }, { 0, 1, Right } },
            { { 0, _, }, { 1, _, Right } },

            { { 1, 0, }, { 1, 0, Right } },
            { { 1, 1, }, { 1, 1, Right } },
            { { 1, _, }, { 2, _, Left } },

            { { 2, 0, }, { 2, 1, Left } },
            { { 2, 1, }, { 3, 0, Left } },
            { { 2, _, }, { 5, _, Right } },

            { { 3, 0, }, { 3, 0, Left } },
            { { 3, 1, }, { 3, 1, Left } },
            { { 3, _, }, { 4, _, Left } },

            { { 4, 0, }, { 0, 1, Right } },
            { { 4, 1, }, { 4, 0, Left } },
            { { 4, _, }, { 0, 1, Right } },

            { { 5, 0, }, { 5, _, Right } },
            { { 5, 1, }, { 5, _, Right } },
            { { 5, _, }, { HALT, _, None } },
        },
        // Initial tape state
        { 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, _, 1, 0, 1, 1, 1, 1, 0, 1, 1, _ }
    };
    
    for (auto& i : machine.tape)
        std::cout << (i == 2 ? '_' : i == 1 ? '1' : '0');
    std::cout << '\n';

    while (machine.Step());

    for (auto& i : machine.tape)
        std::cout << (i == 2 ? '_' : i == 1 ? '1' : '0');


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