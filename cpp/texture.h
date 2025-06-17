#ifndef TEXTURE_H
#define TEXTURE_H

#include "vec3.h"
#include "color.h"
#include "image.h"
#include "perlin_noise.h"

using std::make_shared;
using std::shared_ptr;

class texture {
  public:
    virtual ~texture() = default;

    virtual color value(double u, double v, const point3& p) const = 0;
};

class solid_color : public texture {
  private:
    color albedo;

  public:
    solid_color(const color& albedo) : albedo(albedo) {}

    solid_color(double red, double green, double blue) : albedo(red, green, blue) {}

    color value(double u, double v, const point3& p) const override {
        return albedo;
    }
};

class checker_texture : public texture {
  private:
    double inv_scale;
    shared_ptr<texture> even;
    shared_ptr<texture> odd;

  public:
    checker_texture(double scale, shared_ptr<texture> even, shared_ptr<texture> odd)
        : inv_scale(1.0 / scale), even(even), odd(odd) {}

    checker_texture(double scale, const color& even, const color& odd)
        : checker_texture(scale, make_shared<solid_color>(even), make_shared<solid_color>(odd)) {}

    color value(double u, double v, const point3& p) const override {
        int x_int = int(std::floor(p.x() * inv_scale));
        int y_int = int(std::floor(p.y() * inv_scale));
        int z_int = int(std::floor(p.z() * inv_scale));
        
        return (x_int + y_int + z_int) % 2 == 0 ? even->value(u, v, p) : odd->value(u, v, p);
    }
};

class image_texture : public texture {
  private:
    image img;

  public:
    image_texture(const char* filepath) : img(filepath) {}
    
    color value(double u, double v, const point3& p) const override {
        int i = int(u * img.width());
        int j = int((1 - v) * img.height());
        return img.pixel_color(u * img.width(), (1 - v) * img.height());
    }
};

class noise_texture : public texture {
  private:
    perlin_noise noise;
    double frequency;

  public:
    noise_texture(double frequency) : frequency(frequency) {}
    
    color value(double u, double v, const point3& p) const override {
        return color(1, 1, 1) * (0.5 + 0.5 * std::sin(frequency * p.z() + 10 * std::fabs(noise.turbulence(p, 7))));
    }
};

#endif