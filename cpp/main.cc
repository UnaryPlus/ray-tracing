#include "vec3.h"
#include "color.h"
#include "ray.h"
#include "hittable.h"
#include "sphere.h"
#include "hittable_list.h"
#include "utils.h"
#include "camera.h"
#include "material.h"
#include "bvh_node.h"
#include <iostream>

int main() {
    hittable_list spheres;
    auto ground_material = make_shared<lambertian>(color(0.5, 0.5, 0.5));
    spheres.add(make_shared<sphere>(point3(0, -1000, 0), 1000, ground_material));

    // little spheres
    for(int a = -11; a < 11; a++) {
        for(int b = -11; b < 11; b++) {
            point3 center(a + 0.9*random_double(), 0.2, b + 0.9*random_double());
            if((center - point3(4, 0.2, 0)).length() <= 0.9) 
                continue;
            
            shared_ptr<material> sphere_material;
            double choose_mat = random_double();
            if(choose_mat < 0.8) {
                // diffuse
                auto albedo = color::random() * color::random();
                sphere_material = make_shared<lambertian>(albedo);
                auto center2 = center + vec3(0, random_double(0,.5), 0);
                spheres.add(make_shared<sphere>(center, center2, 0.2, sphere_material));
            } else if(choose_mat < 0.95) {
                // metal
                auto albedo = color::random(0.5, 1);
                auto fuzz = random_double(0, 0.5);
                sphere_material = make_shared<metal>(albedo, fuzz);
                spheres.add(make_shared<sphere>(center, 0.2, sphere_material));
            } else {
                // glass
                sphere_material = make_shared<dielectric>(1.5);
                spheres.add(make_shared<sphere>(center, 0.2, sphere_material));
            }
        }
    }

    // big spheres
    auto material1 = make_shared<dielectric>(1.5);
    auto material2 = make_shared<lambertian>(color(0.4, 0.2, 0.1));
    auto material3 = make_shared<metal>(color(0.7, 0.6, 0.5), 0.0);
    spheres.add(make_shared<sphere>(point3(0, 1, 0), 1.0, material1));
    spheres.add(make_shared<sphere>(point3(-4, 1, 0), 1.0, material2));
    spheres.add(make_shared<sphere>(point3(4, 1, 0), 1.0, material3));

    auto world = bvh_node(spheres);

    camera cam;
    cam.aspect_ratio = 16.0 / 9.0;
    cam.image_width = 400;
    cam.samples_per_pixel = 100;
    cam.max_depth = 50;

    cam.vfov = degrees(20);
    cam.center = point3(13, 2, 3);
    cam.lookat = point3(0, 0, 0);
    cam.up = vec3(0, 1, 0);

    cam.defocus_angle = degrees(0.6);
    cam.focus_dist = 10.0;

    cam.render(world);
}