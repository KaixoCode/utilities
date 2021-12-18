#include "Parser.hpp"

#include <iostream>

#include <numbers>


double erfinv(double y)
{
    constexpr double a[4] {  0.886226899, -1.645349621,  0.914624893, -0.140543331 };
    constexpr double b[4] { -2.118377725,  1.442710462, -0.329097515,  0.012229801 };
    constexpr double c[4] { -1.970840454, -1.624906493,  3.429567803,  1.641345311 };
    constexpr double d[2] {  3.543889200,  1.637067800 };

    const double y0 = 0.7;

    double x, z;

    if (y < -1 || y > 1)
        return 0;

    if (std::abs(y) == 1.0)
    {
        x = -y * std::log(0.0);
    }
    else if (y < -y0)
    {
        z = std::sqrt(-std::log((1.0 + y) / 2.0));
        x = -(((c[3] * z + c[2]) * z + c[1]) * z + c[0]) / ((d[1] * z + d[0]) * z + 1.0);
    }
    else 
    {
        if (y < y0)
        {
            z = y * y;
            x = y * (((a[3] * z + a[2]) * z + a[1]) * z + a[0]) / ((((b[3] * z + b[3]) * z + b[1]) * z + b[0]) * z + 1.0);
        }
        else
        {
            z = std::sqrt(-std::log((1.0 - y) / 2.0));
            x = (((c[3] * z + c[2]) * z + c[1]) * z + c[0]) / ((d[1] * z + d[0]) * z + 1.0);
        };

        x = x - (std::erf(x) - y) / (2.0 / std::sqrt(std::numbers::pi_v<double>) * std::exp(-x * x));
        x = x - (std::erf(x) - y) / (2.0 / std::sqrt(std::numbers::pi_v<double>) * std::exp(-x * x));
    }

  return x;
}

double ncdf(double x)
{
    return 0.5 * (1 + std::erf(x / std::numbers::sqrt2_v<double>));
}

double incdf(double x)
{
    return erfinv(x * 2 - 1) * std::numbers::sqrt2_v<double>;
}

double gamma(double Z)
{
    const double RECIP_E = 0.36787944117144232159552377016147;  // RECIP_E = (E^-1) = (1.0 / E)
    const double TWOPI = 6.283185307179586476925286766559;  // TWOPI = 2.0 * PI

    double D = 1.0 / (10.0 * Z);
    D = 1.0 / ((12 * Z) - D);
    D = (D + Z) * RECIP_E;
    D = pow(D, Z);
    D *= sqrt(TWOPI / Z);

    return D;
}

static double igf(double S, double Z)
{
    if (Z < 0.0)
    {
        return 0.0;
    }
    double Sc = (1.0 / S);
    Sc *= pow(Z, S);
    Sc *= exp(-Z);

    double Sum = 1.0;
    double Nom = 1.0;
    double Denom = 1.0;

    for (int I = 0; I < 200; I++)
    {
        Nom *= Z;
        S++;
        Denom *= S;
        Sum += (Nom / Denom);
    }

    return Sum * Sc;
}

double chisqr(double Cv, double Dof)
{
    if (Cv < 0 || Dof < 1)
    {
        return 0.0;
    }
    double K = ((double)Dof) * 0.5;
    double X = Cv * 0.5;
    if (Dof == 2)
    {
        return exp(-1.0 * X);
    }

    double PValue = igf(K, X);
    if (isnan(PValue) || isinf(PValue) || PValue <= 1e-8)
    {
        return 1e-14;
    }

    PValue /= gamma(K);
    //PValue /= tgamma(K); 

    return (1.0 - PValue);
}

double ichisqr(double a, double Dof)
{
    double Cv = 5;
    double error = 1;
    int iters = 0;
    while (std::abs(error) > 0.00000000001 && iters < 100000)
    {
        double res = chisqr(Cv, Dof);
        error = res - a;
        Cv += error;
        iters++;
    }

    return Cv;
}

struct Range
{
    double start;
    double end;
};

struct Sample
{
    enum Parameter { μ, σ, σ², p };

    Sample() {}

    Sample(std::initializer_list<double> observations)
    {
        n = observations.size();
        double sum = 0;
        for (auto& i : observations) sum += i;
        mean = sum / n;
        sum = 0;
        for (auto& i : observations) sum += (i - mean) * (i - mean);
        var = sum / n;
    }

    template<Parameter>
    Range CI(double percent);

    template<>
    Range CI<μ>(double percent)
    {
        double a = percent / 100.;
        double alpha = 1 - 0.5 * (1 - a);
        double c = incdf(alpha);
        double error = c * std::sqrt(var) / std::sqrt(n);
        return { mean - error, mean + error };
    }

    template<>
    Range CI<σ²>(double percent)
    {
        double a = percent / 100.;
        double degrees = n - 1;
        double alpha1 = 1 - 0.5 * (1 - a);
        double alpha2 = 0.5 * (1 - a);
        double error1 = degrees * var / ichisqr(alpha1, degrees);
        double error2 = degrees * var / ichisqr(alpha2, degrees);
        return { error2, error1 };
    }

    template<>
    Range CI<σ>(double percent)
    {
        auto [a, b] = CI<σ²>(percent);
        return { std::sqrt(a), std::sqrt(b) };
    }

    template<>
    Range CI<p>(double percent)
    {
        double a = percent / 100.;
        double alpha = 1 - 0.5 * (1 - a);
        double c = incdf(alpha);
        double pd = proportion / n;
        double error = c * std::sqrt(pd * (1 - pd) / n);
        return { pd - error, pd + error };
    }

    template<Parameter>
    bool Test(double, double, bool = true);

    template<>
    bool Test<μ>(double observed, double percent, bool rr)
    {
        enum { Right, Left};
        double alpha = percent / 100.;
        double testvar = var / n;
        double h0 = mean;
        double h1 = observed;
        bool side = observed < mean; // true = left, false = right;

        // Using Rejection region
        if (rr)
        {
            if (side == Right)
            {
                alpha = 1 - alpha;
                double rr = incdf(alpha);
                double c = rr * std::sqrt(testvar) + h0;
                return h1 > c;
            }
            else
            {
                double rr = incdf(alpha);
                double c = rr * std::sqrt(testvar) + h0;
                return h1 < c;
            }

            // Using pvalue
            if (side == Right)
            {
                double c = (h1 - h0) / std::sqrt(testvar);
                double pvalue = 1 - ncdf(c);
                return pvalue < alpha;
            }
            else
            {
                double c = (h1 - h0) / std::sqrt(testvar);
                double pvalue = ncdf(c);
                return pvalue < alpha;
            }
        }
    }

    union
    {
        struct
        {
            double mean;
            double var;
        }; // Normal

        double proportion; // Binomial
    };

    double n;
};

template<char ...cs>
struct Base
{
    template<char ...> struct period;
    template<char c, char ...cs> struct period<c, cs...> { enum { value = c == '.' ? sizeof...(cs) + 1 : period<cs...>::value }; };
    template<char c> struct period<c> { enum { value = c == '.' ? 1 : 0 }; };

    constexpr static std::uint8_t as_numbers[sizeof...(cs)]{ static_cast<std::uint8_t>(cs - '0')... };
    constexpr static std::size_t numbers = sizeof...(cs) - period<cs...>::value;
    constexpr static bool has_period = sizeof...(cs) != numbers;
    constexpr static std::size_t decimals = has_period ? sizeof...(cs) - numbers - 1 : 0;
    constexpr Base()
    {}

    constexpr auto operator()(std::size_t base)
    {
        // Float
        if constexpr (has_period)
        {
            double result = 0;
            double p = 1;

            for (std::size_t position = 0; position < numbers; position++)
                result += as_numbers[numbers - position - 1] * p, p *= base;

            p = 1;
            for (std::size_t position = 0; position < decimals; position++)
                p /= base, result += as_numbers[numbers + position + 1] * p;

            return result;
        }
        else // Integral
        {
            std::size_t result = 0;
            std::size_t p = 1;
            
            for (std::size_t position = 0; position < numbers; position++)
                result += as_numbers[numbers - position - 1] * p, p *= base;
            
            return result;
        }
    }
};

template<char ...cs> constexpr auto operator ""base() { return Base<cs...>{}; }
template<char ...cs> constexpr auto operator ""base2() { return Base<cs...>{}(2); }
template<char ...cs> constexpr auto operator ""base3() { return Base<cs...>{}(3); }
template<char ...cs> constexpr auto operator ""base4() { return Base<cs...>{}(4); }
template<char ...cs> constexpr auto operator ""base5() { return Base<cs...>{}(5); }
template<char ...cs> constexpr auto operator ""base6() { return Base<cs...>{}(6); }
template<char ...cs> constexpr auto operator ""base7() { return Base<cs...>{}(7); }
template<char ...cs> constexpr auto operator ""base8() { return Base<cs...>{}(8); }
template<char ...cs> constexpr auto operator ""base9() { return Base<cs...>{}(9); }
template<char ...cs> constexpr auto operator ""base10() { return Base<cs...>{}(10); }

int main() {
        
    using enum Sample::Parameter;

    Sample sample;
    sample.mean = 115;
    sample.var = 10;
    sample.n = 20;

    constexpr auto a1 = 1.31483base; decltype(a1)::decimals; decltype(a1)::numbers; decltype(a1)::has_period; decltype(a1)::as_numbers;
    constexpr auto a2 = 13.1483base; decltype(a2)::decimals; decltype(a2)::numbers; decltype(a2)::has_period; decltype(a2)::as_numbers;
    constexpr auto a3 = 131.483base; decltype(a3)::decimals; decltype(a3)::numbers; decltype(a3)::has_period; decltype(a3)::as_numbers;
    constexpr auto a4 = 1314.83base; decltype(a4)::decimals; decltype(a4)::numbers; decltype(a4)::has_period; decltype(a4)::as_numbers;
    constexpr auto a5 = 13148.3base; decltype(a5)::decimals; decltype(a5)::numbers; decltype(a5)::has_period; decltype(a5)::as_numbers;
    constexpr auto a6 = 131483.base; decltype(a6)::decimals; decltype(a6)::numbers; decltype(a6)::has_period; decltype(a6)::as_numbers;
    constexpr auto a7 = 131483base;  decltype(a7)::decimals; decltype(a7)::numbers; decltype(a7)::has_period; decltype(a7)::as_numbers;
    

    constexpr auto b4 = 1231.23base4;
    constexpr auto b6 = 51.4532base6;
    constexpr auto b120 = 9138.431base(120);

    auto resa = sample.CI<σ²>(95);




    constexpr auto binary = 0b10;
    constexpr auto octal = 010;
    constexpr auto decimal = 10;
    constexpr auto hex = 0x10;




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