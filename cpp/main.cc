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
#include "texture.h"
#include <iostream>

void bouncing_spheres() {
    hittable_list spheres;
    auto checker = make_shared<checker_texture>(0.32, color(0.2, 0.3, 0.1), color(0.9, 0.9, 0.9));
    spheres.add(make_shared<sphere>(point3(0, -1000, 0), 1000, make_shared<lambertian>(checker)));

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

    auto world = bvh_node(spheres);
    cam.render(world);
}

void checkered_spheres() {
    hittable_list world;
    auto checker_tex = make_shared<checker_texture>(0.32, color(0.2, 0.3, 0.1), color(0.9, 0.9, 0.9));
    auto checker_mat = make_shared<lambertian>(checker_tex);
    world.add(make_shared<sphere>(point3(0, -10, 0), 10, checker_mat)); 
    world.add(make_shared<sphere>(point3(0, +10, 0), 10, checker_mat));

    camera cam;
    cam.aspect_ratio = 16.0 / 9.0;
    cam.image_width = 400;
    cam.samples_per_pixel = 100;
    cam.max_depth = 50;

    cam.vfov = degrees(20);
    cam.center = point3(13, 2, 3);
    cam.lookat = point3(0, 0, 0);
    cam.up = vec3(0, 1, 0);

    cam.defocus_angle = 0;

    cam.render(world);
}

void earth() {
    auto earth_tex = make_shared<image_texture>("images/earthmap.jpg");
    auto earth_mat = make_shared<lambertian>(earth_tex);
    auto globe = sphere(point3(0, 0, 0), 2, earth_mat);

    camera cam;
    cam.aspect_ratio = 16.0 / 9.0;
    cam.image_width = 400;
    cam.samples_per_pixel = 100;
    cam.max_depth = 50;

    cam.vfov = degrees(20);
    cam.center = point3(0,0,12);
    cam.lookat = point3(0,0,0);
    cam.up = vec3(0,1,0);

    cam.defocus_angle = 0;

    cam.render(globe);
}

int main() {
    switch(3) {
        case 1: bouncing_spheres(); break;
        case 2: checkered_spheres(); break;
        case 3: earth(); break;
    }
}