#include <array>

#include <map>
#include <list>
#include <vector>

//template<int Width, int Height>
//constexpr bool has_path1(const Screen<Width, Height>& screen) { //
//    int ry = 0, y = 0, x = 0, d = 1;                           //
//    while (ry != Height) {                                     // Continue until the reset y is Height
//        while (screen[{ x + 1, y }] == 0) ++x, d = 1;          // Check ahead, if free: move and reset direction to 1
//        if (x == Width - 1) break;                             // If after move we're at the edge: win
//        if (screen[{ x, y + d }] == 0) y += d;                 // Check in current direction, if 0: move
//        else { if (d == 1) d = -1; else y = ++ry, x = 0; }     // Otherwise, if dir is 1, try other dir, 
//    }                                                          // Otherwise start at next y and set x back to 0
//    return x == Width - 1;                                     // Win if we're at the final x
//}

struct Point { int x, y; };
template<int Width, int Height>
struct Screen {
    constexpr static auto width = Width;
    constexpr static auto height = Height;
    constexpr auto& operator[](Point p) { return data[p.x + p.y * Width]; }
    constexpr auto& operator[](Point p) const { return data[p.x + p.y * Width]; }
    std::uint8_t data[Width * Height];
};

constexpr bool check(auto& s, auto& c, Point p) {
    if (p.x < 0 || p.x >= s.width || p.y < 0 || p.y >= s.height) return false;
    return s[p] == 0 && c[p] == 0 && (c[p] = 1, p.x == s.width - 1
        || check(s, c, { p.x + 1, p.y }) || check(s, c, { p.x - 1, p.y })
        || check(s, c, { p.x, p.y + 1 }) || check(s, c, { p.x, p.y - 1 }));
}

constexpr bool has_path(auto& screen) {
    std::decay_t<decltype(screen)> checked{};
    return check(screen, checked, { 0, 0 });
}









int main() {





    return 0;
}

