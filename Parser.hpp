#include <vector>
#include <string>
#include <cctype>
#include <memory>
#include <initializer_list>
#include <stack>
#include <cstddef>
#include <list>
#include <optional>
#include <variant>
#include <ranges>

namespace kaixo
{
    using TokenType = std::size_t;

    struct Token
    {
        TokenType type;
        std::string val;
    };

    class Lexer
    {
    public:
        enum State { Error, Matching, Accepting };

        struct TokenLexer
        {
            State state = State::Matching;
            TokenType type = static_cast<TokenType>(-1);
            bool capture = true; // Add to final token list when matched.

            bool TryNext(char c)
            {
                // If we're in an Error state, always return false
                if (state == Error) return false;
                state = Next(c); // Otherwise try next character
                return state != Error; // Success if not in error state
            }

            virtual State Next(char c) = 0;
            virtual void Reset() { state = Matching; };
        };

        struct WordLexer : public TokenLexer
        {
            WordLexer(const std::string& word) : word(word) {};

            size_t index = 0; // Current index in word
            std::string word; // Word to match
            State Next(char c) override
            {
                // If character matches and not past word end
                if (index < word.size() && word[index] == c)
                    if (index == word.size() - 1) return Accepting; // Final character = Accepting
                    else return index++, Matching; // Otherwise just a match
                return Error; // else error
            }

            void Reset() override { index = 0; TokenLexer::Reset(); }
        };

        struct WordsLexer : public TokenLexer
        {
            WordsLexer(std::initializer_list<const char*> words)
                : words(words.begin(), words.end()) {}

            std::vector<WordLexer> words; // list of words to match

            State Next(char c) override
            {
                State state = Error;  // Initially Error
                for (auto& i : words) // Go over all words
                    if (i.TryNext(c) && i.state > state)
                        state = i.state;
                return state; // Return state, Error if no matches.
            }

            void Reset()
            {   // Recurse down to all words.
                for (auto& i : words)
                    i.Reset();
                TokenLexer::Reset();
            }
        };

        struct WhitespaceLexer : public TokenLexer
        {
            WhitespaceLexer() { this->capture = false; }

            State Next(char c) override { return std::isspace(c) ? Accepting : Error; }
        };

        struct Settings
        {
            bool skipws = true; // Skip whitespace
        } settings;

        Lexer(const Settings& s = {})
            : settings(s)
        {
            if (settings.skipws)
                lexers.emplace_back(std::make_unique<WhitespaceLexer>());
        }

        template<class Ty>
        void Add() { lexers.emplace_back(std::make_unique<Ty>()); }

        std::vector<Token> Tokenize(const std::string& in)
        {
            std::vector<Token> tokens;
            size_t pindex = 0;  // Previous index
            size_t index = 0;   // Current index
            size_t lmindex = 0; // Index of last accepting match

            int lastMatch = -1; // Index of last matching type

            while (index < in.size()) // Loop until we're at the end of the input
            {
                size_t amount = 0; // Keep track of match count.
                for (size_t i = lexers.size() - 1; i < lexers.size(); i--)
                    if (lexers[i]->TryNext(in[index])) // Try next character
                    {
                        amount++; // Match, so add to count

                        // If state is accepting, remember match
                        if (lexers[i]->state == Accepting)
                            lastMatch = i, lmindex = index;
                    }

                // If no more matches, use last match if exists
                if (amount == 0 && lastMatch != -1)
                {
                    // If type should be captured, add to tokens
                    if (lexers[lastMatch]->capture)
                        tokens.push_back({ lexers[lastMatch]->type,
                            in.substr(pindex, lmindex + 1 - pindex) });

                    // Reset the state in all lexers
                    for (auto& i : lexers)
                        i->Reset();

                    pindex = lmindex + 1; // Reset the index back to.
                    index = lmindex + 1;  // last matched index.
                    lastMatch = -1;  // Reset last match
                }
                else
                    index++;
            }

            return tokens;
        }

    private:
        // All the lexers, ordered from low to high precedence
        std::vector<std::unique_ptr<TokenLexer>> lexers;
    };

    // Simple vector wrapper with dropped counter
    struct TokenVector
    {
        TokenVector(std::vector<Token>& tokens)
            : tokens(tokens)
        {}

        Token& PopFront() { return *(tokens.begin() + dropped++); }
        Token& Front() { return *(tokens.begin() + dropped); }
        TokenVector Current() { return *this; }
        void SetState(TokenVector& b) { dropped = b.dropped; }
        bool Empty() { return dropped == tokens.size(); }

        std::vector<Token>& tokens;
        size_t dropped = 0;
    };

    // Wrapper for variant of unique ptrs
    template<class ...Ty>
    struct PtrVariant
    {
        template<class Ty>
        void operator=(Ty&& val) { value = std::move(val); }

        template<class Ty> Ty& get() { return *std::get<std::unique_ptr<Ty>>(value); }
        template<size_t N> auto& get() { return *std::get<N>(value); }

        size_t index() { return value.index(); }
        template<size_t N> bool is() { return index() == N; }

        std::variant<std::unique_ptr<Ty>...> value;
    };

    class Parser
    {
    public:
        // Parse single token
        template<class Type = void>
        static bool TryParse(TokenVector& tokens, Token token, Type* res = nullptr)
        {
            if (tokens.Front().type == token.type && (token.val == "" || tokens.Front().val == token.val))
            {
                if constexpr (!std::same_as<Type, void>)
                    if (res) (*res) = tokens.Front().val;
                tokens.PopFront();
                return true;
            }
            return false;
        }

        // Parse a type
        template<class Type, class Ty>
        static bool TryParse(TokenVector& tokens, Ty* assign = nullptr)
        {
            if (std::optional<Type> res = Type::Parse(tokens))
            {
                if (assign) *assign = std::make_unique<Type>(std::move(res.value()));
                return true;
            }
            return false;
        }

        // Parse a vector of types
        template<class Type>
        static bool TryParse(TokenVector& tokens, std::vector<Type>* assign)
        {
            while (std::optional<Type> res = Type::Parse(tokens))
                assign->push_back(std::move(res.value()));

            return assign->size();
        }
    };
}

namespace MyLang
{
    using namespace kaixo;

    enum MyTypes
    {
        KEYWORD, OPERATOR, IDENTIFIER, CHAR, CONSTANT
    };

    class MyLexer : public Lexer
    {
    public:

        // Matches any singe character
        struct CharLexer : public TokenLexer
        {
            CharLexer() { this->type = CHAR; }

            bool active = true;

            State Next(char c) override { return active ? active = false, Accepting : Error; }
            void Reset() override { active = true; TokenLexer::Reset(); }
        };

        // Matches any keyword
        struct KeywordLexer : public WordsLexer
        {
            KeywordLexer() 
                : WordsLexer({ "if", "else", "then" }) 
            {
                type = KEYWORD; 
            }
        };

        // Matches any operator
        struct OperatorLexer : public WordsLexer
        {
            OperatorLexer() 
                : WordsLexer({ "+", "-", "*", "/", "<", ">", "=", ":=", "not", "and", "or" }) 
            {
                type = OPERATOR; 
            }
        };

        // Matches any string of alpha numerical characters
        struct IdentifierLexer : public TokenLexer
        {
            IdentifierLexer() { type = IDENTIFIER; }

            State Next(char c) override { return std::isalnum(c) ? Accepting : Error; }
        };

        // Matches any single 1 or 0
        struct ConstantLexer : public TokenLexer
        {
            ConstantLexer() { type = CONSTANT; }

            bool first = true;

            State Next(char c) override
            {   // Only match on first and if 0 or 1, otherwise Error state
                return first && (c == '0' || c == '1') ? first = false, Accepting : Error;
            }

            void Reset() override { first = true; TokenLexer::Reset(); }
        };

        MyLexer()
            : Lexer({.skipws = true /* Skip whitespace */ })
        {   // Add all lexers from high to low precedence
            Add<KeywordLexer>();
            Add<ConstantLexer>();
            Add<OperatorLexer>();
            Add<IdentifierLexer>();
            Add<CharLexer>();
        }
    };

    class MyParser : public Parser
    {
    public:
        struct TranslationUnit;
        struct Statement;
        struct SelectionStatement;
        struct CompoundStatement;
        struct AssignStatement;
        struct PrimaryExpression;
        struct Expression;

        struct CompoundStatement
        {
            std::vector<Statement> statements;

            static std::optional<CompoundStatement> Parse(TokenVector& tokens)
            {
                TokenVector current = tokens.Current();
                CompoundStatement result;

                if (TryParse(current, { CHAR, "{" })
                 && TryParse<Statement>(current, &result.statements)
                 && TryParse(current, { CHAR, "}" }))
                {
                    tokens.SetState(current);
                    return result;
                }

                return {};
            }
        };

        struct SelectionStatement
        {
            std::unique_ptr<Expression> expr;
            std::unique_ptr<Statement> thenstmnt;
            std::unique_ptr<Statement> elsestmnt;

            static std::optional<SelectionStatement> Parse(TokenVector& tokens)
            {
                TokenVector current = tokens.Current();
                SelectionStatement result;

                // IF "(" Expression ")" THEN Statement ELSE Statement
                if (TryParse(current, { KEYWORD, "if" })
                 && TryParse(current, { CHAR, "(" })
                 && TryParse<Expression>(current, &result.expr)
                 && TryParse(current, { CHAR, ")" })
                 && TryParse(current, { KEYWORD, "then" })
                 && TryParse<Statement>(current, &result.thenstmnt)
                 && TryParse(current, { KEYWORD, "else" })
                 && TryParse<Statement>(current, &result.elsestmnt))
                {
                    tokens.SetState(current);
                    return result;
                }

                return {};
            }
        };

        struct AssignStatement
        {
            std::string identifier;
            std::unique_ptr<Expression> expr;

            static std::optional<AssignStatement> Parse(TokenVector& tokens)
            {
                TokenVector current = tokens.Current();
                AssignStatement result;

                // IDENTIFIER ":=" Expression
                if (TryParse(current, { IDENTIFIER }, &result.identifier)
                 && TryParse(current, { OPERATOR, ":=" })
                 && TryParse<Expression>(current, &result.expr))
                {
                    tokens.SetState(current);
                    return result;
                }

                return {};
            }
        };

        struct Statement
        {
            PtrVariant<SelectionStatement, CompoundStatement, AssignStatement> value;

            static std::optional<Statement> Parse(TokenVector& tokens)
            {
                TokenVector current = tokens.Current();
                Statement result;

                // SelectionStatement | CompoundStatement | AssignStatement
                if (TryParse<SelectionStatement>(current, &result.value)
                 || TryParse<CompoundStatement>(current, &result.value)
                 || TryParse<AssignStatement>(current, &result.value))
                {
                    tokens.SetState(current);
                    return result;
                }

                return {};
            }
        };

        struct Expression
        {
            struct Constant
            {
                Constant(const std::string& val)
                    : value(std::stoi(val.c_str()))
                {}

                bool value;
            };

            enum class Operator
            {
                // Operators ordered from low to high precedence
                Or = 0, And, Equality, Relational, Additive, Multiplicative, Unary, Primary
            };

            // Simple test to see if str is part of operator group
            constexpr static bool IsOp(const std::string& str, Operator op)
            {
                switch (op) {
                case Operator::Or: return str == "or";
                case Operator::And: return str == "and";
                case Operator::Unary: return str == "not";
                case Operator::Equality: return str == "=";
                case Operator::Relational: return str == ">" || str == "<";
                case Operator::Additive: return str == "+" || str == "-";
                case Operator::Multiplicative: return str == "*" || str == "/";
                }
            }

            // Returns true if op is unary operator
            constexpr static bool IsUnary(Operator op) { return op == Operator::Unary; }

            // ExprPart is either Constant, Identifier, or Operator
            struct ExprPart
            {
                enum Type { Constant = 0, Identifier = 1, Operator = 2 };
                ExprPart(Expression::Constant v) : value(v) {}
                ExprPart(const std::string& v) : value(v) {}
                ExprPart(Expression::Operator v) : value(v) {}

                template<Type N> auto& get() { return *std::get<N>(value); }

                Type index() { return static_cast<Type>(value.index()); }
                template<Type N> bool is() { return index() == N; }

                std::variant<Expression::Constant, std::string, Expression::Operator> value;
            };

            // An expression is built up out of ExprParts
            std::vector<ExprPart> parts;

            // Parse a primary expression (identifier, constant, nested expression)
            static std::vector<ExprPart> ParsePrimary(TokenVector& tokens)
            {
                if (tokens.Empty()) return {};
                TokenVector current = tokens.Current();
                Token& now = current.PopFront();

                if (now.type == CONSTANT) // Parsed a constant
                {
                    tokens.SetState(current);
                    return { Constant{ now.val } };
                }
                else if (now.type == IDENTIFIER) // Parsed an identifier
                {
                    tokens.SetState(current);
                    return { now.val };
                }
                else if (now.type == CHAR && now.val == "(") // Nested expression
                {
                    // Parse the expression itself, starting at lowest precedence operator (Or)
                    auto res = ParseExpression(current, Operator::Or);

                    // Check that it ends with ")", otherwise abort and return empty
                    if (current.Empty()) return {};
                    Token& now = current.PopFront();
                    if (now.type == CHAR && now.val == ")")
                    {
                        tokens.SetState(current);
                        return res;
                    }
                }

                return {};
            }

            // Recursive expression parser, given the operator
            static std::vector<ExprPart> ParseExpression(TokenVector& tokens, Operator op)
            {
                // If operator is primary, parse primary
                if (op == Operator::Primary) return ParsePrimary(tokens);

                // If unary operator, check before recurse
                bool _unaryAdd = false;
                if (IsUnary(op))
                {
                    if (tokens.Empty()) return {};
                    TokenVector current = tokens.Current();
                    Token& now = current.PopFront();
                    if (now.type == OPERATOR && IsOp(now.val, op))
                    {
                        // If is operator, we need to add the operator later, and save state
                        _unaryAdd = true;
                        tokens.SetState(current);
                    }
                }

                // Start with recursion to next operator
                Operator next = static_cast<Operator>(static_cast<size_t>(op) + 1);
                auto res = ParseExpression(tokens, next); // Get parts

                // If we need to add the operator, do it now after recurse (postfix)
                if (_unaryAdd) res.emplace_back(op);

                // Only if not unary operator
                if (!IsUnary(op))
                {
                    // If all tokens consumed, return result
                    if (tokens.Empty()) return res;

                    // Backup, and try current operator
                    TokenVector current = tokens.Current();
                    Token& now = current.PopFront();
                    if (now.type == OPERATOR && IsOp(now.val, op))
                    {
                        // If operator present, recurse with same operator
                        auto nextres = ParseExpression(current, op);

                        // Insert result from recursion before adding operator (postfix)
                        res.insert(res.end(), nextres.begin(), nextres.end()); // First this
                        res.emplace_back(op); // Then operator
                        tokens.SetState(current); // Set the state
                    }
                }

                return res; // Finally return result
            }

            static std::optional<Expression> Parse(TokenVector& tokens)
            {
                TokenVector current = tokens.Current();

                Expression expr;
                expr.parts = ParseExpression(current, Operator::Or);
                if (expr.parts.empty()) return {};

                tokens.SetState(current);
                return expr;
            }
        };

        struct TranslationUnit
        {
            std::vector<Statement> statements;

            static std::optional<TranslationUnit> Parse(TokenVector& tokens)
            {
                TokenVector current = tokens.Current();
                TranslationUnit result;

                // Statement* ";"
                if (TryParse<Statement>(current, &result.statements)
                 && TryParse(current, { CHAR, ";" }))
                {
                    tokens.SetState(current);
                    return result;
                }

                return {};
            }
        };
    };
}