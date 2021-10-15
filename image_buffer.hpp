#pragma once
#include <utility>
#include <algorithm>
#include <cassert>
#include <string>

#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>
#include <BMP.h>

struct color {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
};

class image_buffer {
    std::size_t m_Width = 0;
    std::size_t m_Height = 0;
    color* m_Buffer = nullptr;
public:
    struct pos {
        std::size_t x;
        std::size_t y;

        bool operator==(const pos& other) const { return other.x == x && other.y == y; };
        bool operator!=(const pos& other) const { return !(other == *this); };
    };

    struct pixel {
        color& color;
        image_buffer::pos position;

        void operator=(const ::color& c) { color = c; }
        operator ::color& () { return color; }
        operator ::color const& () const { return color; }
        ::color* operator->() { return &color; }
        const ::color const* operator->() const { return &color; }
        ::color& operator*() { return color; }
        ::color const& operator*() const { return color; }
    };

    struct const_pixel {
        const color& color;
        image_buffer::pos position;

        operator ::color const& () const { return color; }
        const ::color const* operator->() const { return &color; }
        ::color const& operator*() const { return color; }
    };

    class iterator {
        image_buffer& m_Buffer;
        image_buffer::pos m_Pos;
        iterator(image_buffer& b, image_buffer::pos p) : m_Buffer(b), m_Pos(p) {};
    public:

        iterator& operator++() {
            if (++m_Pos.x == m_Buffer.width()) m_Pos.y++, m_Pos.x = 0;
            return *this;
        }

        iterator& operator--() {
            if (m_Pos.x == 0) m_Pos.y--, m_Pos.x = m_Buffer.width() - 1;
            else m_Pos.x--;
            return *this;
        }

        image_buffer::pixel operator*() { return image_buffer::pixel{ m_Buffer[m_Pos], m_Pos }; }
        image_buffer::const_pixel operator*() const { return image_buffer::const_pixel{ m_Buffer[m_Pos], m_Pos }; }

        bool operator!=(const iterator& other) const { return other.m_Pos != m_Pos; }
        bool operator==(const iterator& other) const { return other.m_Pos == m_Pos; }

        friend class image_buffer;
    };

    class const_iterator {
        image_buffer const& m_Buffer;
        image_buffer::pos m_Pos;
        const_iterator(const image_buffer& b, image_buffer::pos p) : m_Buffer(b), m_Pos(p) {};
    public:

        const_iterator& operator++() {
            if (++m_Pos.x == m_Buffer.width()) m_Pos.y++, m_Pos.x = 0;
            return *this;
        }

        const_iterator& operator--() {
            if (m_Pos.x == 0) m_Pos.y--, m_Pos.x = m_Buffer.width() - 1;
            else m_Pos.x--;
            return *this;
        }

        image_buffer::const_pixel operator*() const { return image_buffer::const_pixel{ m_Buffer[m_Pos], m_Pos }; }

        bool operator !=(const const_iterator& other) const { return other.m_Pos != m_Pos; }

        friend class image_buffer;
    };

    image_buffer() {};

    image_buffer(std::size_t w, std::size_t h)
        : m_Width(w), m_Height(h),
        m_Buffer(new color[width() * height()]) {}

    image_buffer(image_buffer&& other)
        : m_Width(other.width()), m_Height(other.height()),
        m_Buffer(other.data()) {
        other.invalidate();
    }

    image_buffer(const image_buffer& other)
        : m_Width(other.width()), m_Height(other.height()),
        m_Buffer(new color[width() * height()]) {
        std::copy_n(other.data(), other.size(), data());
    }

    ~image_buffer() { delete[] m_Buffer; }

    image_buffer& operator=(image_buffer&& other) {
        delete[] m_Buffer;
        m_Width = other.width();
        m_Height = other.height();
        m_Buffer = other.data();
        other.invalidate();
        return *this;
    }

    image_buffer& operator=(const image_buffer& other) {
        delete[] m_Buffer;
        m_Width = other.width();
        m_Height = other.height();
        m_Buffer = new color[width() * height()];
        std::copy_n(other.data(), other.size(), data());
    }

    color& operator[](std::size_t index) {
        assert(index < size());
        return m_Buffer[index];
    }

    color& operator[](const image_buffer::pos& p) {
        assert(p.x < width() && p.y < height());
        return m_Buffer[p.x + p.y * width()];
    }

    color const& operator[](std::size_t index) const {
        assert(index < size());
        return m_Buffer[index];
    }

    color const& operator[](const image_buffer::pos& p) const {
        assert(p.x < width() && p.y < height());
        return m_Buffer[p.x + p.y * width()];
    }

    color* data() { return m_Buffer; }
    const color* data() const { return m_Buffer; }
    image_buffer::iterator begin() { return { *this, { 0, 0 } }; }
    image_buffer::iterator end() { return { *this, { 0, height() } }; }
    image_buffer::const_iterator begin() const { return { *this, { 0, 0 } }; }
    image_buffer::const_iterator end() const { return { *this, { 0, height() } }; }
    std::size_t size() const { return m_Width * m_Height; }
    std::size_t width() const { return m_Width; }
    std::size_t height() const { return m_Height; }

    void fill(const color& c) { std::fill(begin(), end(), c); }

private:
    void invalidate() {
        m_Buffer = nullptr;
        m_Width = 0;
        m_Height = 0;
    }
    friend class iterator;
};

struct image {
    enum class file_format { png, bmp } format;
    std::string filename;
    image_buffer buffer;
    size_t channels;

    image() {};
    image(const std::string& filename) : filename(filename) { load_image(filename); }

    void load_image(const std::string& filename) {
        format = filename.find(".png") > filename.find(".bpm") ? file_format::bmp : file_format::png;
        switch (format) {
        case file_format::png: load_png(filename); break;
        case file_format::bmp: load_bmp(filename); break;
        }
    }

    void load_png(const std::string& filename) {
        format = file_format::png;
        int w, h, c;
        auto* image_data = stbi_load(filename.c_str(), &w, &h, &c, 4);
        channels = static_cast<size_t>(c);

        buffer = { static_cast<size_t>(w), static_cast<size_t>(h) };
        for (auto [color, pos] : buffer) {
            color.r = image_data[channels * pos.x + pos.y * buffer.width() + 0];
            color.g = image_data[channels * pos.x + pos.y * buffer.width() + 1];
            color.b = image_data[channels * pos.x + pos.y * buffer.width() + 2];
            if (channels == 4)
                color.a = image_data[channels * pos.x + pos.y * buffer.width() + 3];
        }
    }

    void load_bmp(const std::string& filename) {
        format = file_format::bmp;

        BMP bmp{ filename.c_str() };
        buffer = { static_cast<size_t>(bmp.bmp_info_header.width), 
            static_cast<size_t>(bmp.bmp_info_header.height) };
        for (auto [color, pos] : buffer) {
            color.r = bmp.data[channels * pos.x + pos.y * buffer.width() + 0];
            color.g = bmp.data[channels * pos.x + pos.y * buffer.width() + 1];
            color.b = bmp.data[channels * pos.x + pos.y * buffer.width() + 2];
        }
    }
};