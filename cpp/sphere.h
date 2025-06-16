#ifndef SPHERE_H
#define SPHERE_H

#include "hittable.h"
#include "interval.h"
#include "vec3.h"

using std::shared_ptr;

class sphere : public hittable {
  private:
    point3 center1;
    point3 center2;
    double radius;
    shared_ptr<material> mat;

  public:
    // Moving sphere
    sphere(const point3& center1, const point3& center2, double radius, shared_ptr<material> mat)
        : center1(center1), center2(center2), radius(std::fmax(0,radius)), mat(mat) {}

    // Stationary sphere
    sphere(const point3& center, double radius, shared_ptr<material> mat) 
        : sphere(center, center, radius, mat) {}

    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        point3 center = center1 + r.time() * (center2 - center1);
        vec3 oc = center - r.origin();
        double a = r.direction().length_squared();
        double h = dot(r.direction(), oc);
        double c = oc.length_squared() - radius*radius;

        double discriminant = h*h - a*c;
        if (discriminant < 0)
            return false;

        double sqrtd = std::sqrt(discriminant);

        // Find the nearest root that lies in the acceptable range.
        double root = (h - sqrtd) / a;
        if (!ray_t.surrounds(root)) {
            root = (h + sqrtd) / a;
            if (!ray_t.surrounds(root))
                return false;
        }

        rec.t = root;
        rec.p = r.at(rec.t);
        vec3 outward_normal = (rec.p - center) / radius;
        rec.set_face_normal(r, outward_normal);
        rec.mat = mat;

        return true;
    }
};

#endif