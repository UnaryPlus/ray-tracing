#ifndef CAMERA_H
#define CAMERA_H

#include "hittable.h"
#include "color.h"
#include "material.h"

class camera {
  public:
    double aspect_ratio = 1.0;
    int image_width = 100;
    int samples_per_pixel = 10;
    int max_depth = 10;
    color background = color(0, 0, 0);

    double vfov = pi/2;
    point3 center = point3(0, 0, 0);
    point3 lookat = point3(0, 0, -1);
    vec3 up = vec3(0, 1, 0);

    double defocus_angle = 0;
    double focus_dist = 10;

    void render(const hittable& world) {
        initialize();

        std::cout << "P3\n" << image_width << ' ' << image_height << "\n255\n";
        for(int j = 0; j < image_height; j++) {
            std::clog << "Scanlines remaining: " << (image_height - j) << '\n';
            for(int i = 0; i < image_width; i++) {
                color pixel_color(0, 0, 0);
                for(int sample = 0; sample < samples_per_pixel; sample++) {
                    ray r = get_ray(i, j);
                    pixel_color += ray_color(r, max_depth, world);
                }
                write_color(std::cout, pixel_color * pixel_samples_scale);
            }
        }
    }

  private:
    int image_height;
    double pixel_samples_scale;
    point3 pixel00_loc;
    vec3 pixel_delta_u;
    vec3 pixel_delta_v;
    vec3 defocus_disk_u;
    vec3 defocus_disk_v;

    void initialize() {
        image_height = int(std::ceil(image_width / aspect_ratio));
        pixel_samples_scale = 1.0 / samples_per_pixel;

        double h = std::tan(vfov/2);
        double viewport_height = 2.0 * h * focus_dist;
        double viewport_width = viewport_height * image_width / image_height;

        vec3 W = unit_vector(center - lookat); // back
        vec3 U = unit_vector(cross(up, W));    // right
        vec3 V = cross(W, U);                  // up

        auto viewport_u = viewport_width * U;
        auto viewport_v = -viewport_height * V;
        pixel_delta_u = viewport_u / image_width;
        pixel_delta_v = viewport_v / image_height;

        auto viewport_upper_left = center - focus_dist*W - viewport_u/2 - viewport_v/2;
        pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v);

        double defocus_radius = focus_dist * std::tan(defocus_angle/2);
        defocus_disk_u = defocus_radius * U;
        defocus_disk_v = defocus_radius * V;
    }

    ray get_ray(int i, int j) const {
        vec3 offset = sample_square();
        point3 pixel_sample = pixel00_loc + ((i + offset.x()) * pixel_delta_u) + ((j + offset.y()) * pixel_delta_v);
        point3 ray_origin = (defocus_angle <= 0) ? center : defocus_disk_sample();        
        return ray(ray_origin, pixel_sample - ray_origin, random_double());
    }

    // Return a random vector in the unit square [-0.5, +0.5] x [-0.5, +0.5].
    vec3 sample_square() const {
        return vec3(random_double() - 0.5, random_double() - 0.5, 0);
    }

    point3 defocus_disk_sample() const {
        vec3 p = random_in_unit_disk();
        return center + (p[0] * defocus_disk_u) + (p[1] * defocus_disk_v);
    }

    color ray_color(const ray& r, int depth, const hittable& world) const {
        if(depth <= 0)
            return color(0, 0, 0);

        hit_record rec;
        if(world.hit(r, interval(0.001, infinity), rec)) {
            color color_from_emission = rec.mat->emitted(r, rec);
            color attenuation;
            ray scattered;
            if(rec.mat->scatter(r, rec, attenuation, scattered)) {
                return color_from_emission + attenuation * ray_color(scattered, depth - 1, world);
            }
            else {
                return color_from_emission;
            }
        }
        else {
            return background;
        }
    }
};

#endif