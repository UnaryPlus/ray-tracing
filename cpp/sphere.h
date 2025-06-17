#ifndef SPHERE_H
#define SPHERE_H

#include "hittable.h"
#include "interval.h"
#include "vec3.h"

using std::shared_ptr;

class sphere : public hittable {
  private:
    point3 center0;
    point3 center1;
    double radius;
    shared_ptr<material> mat;
    box bbox;

  public:
    // Moving sphere
    sphere(const point3& center0, const point3& center1, double radius, shared_ptr<material> mat)
        : center0(center0), center1(center1), radius(radius), mat(mat) {
        auto rvec = vec3(radius, radius, radius);
        box box0(center0 - rvec, center0 + rvec);
        box box1(center1 - rvec, center1 + rvec);
        bbox = box(box0, box1);
    }

    // Stationary sphere
    sphere(const point3& center, double radius, shared_ptr<material> mat) 
        : center0(center), center1(center), radius(radius), mat(mat) {
        auto rvec = vec3(radius, radius, radius);
        bbox = box(center - rvec, center + rvec);
    }

    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        point3 center = center0 + r.time() * (center1 - center0);
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
        get_sphere_uv(outward_normal, rec.u, rec.v);
        rec.mat = mat;

        return true;
    }

    box bounding_box() const override { return bbox; }
  
  private:
    static void get_sphere_uv(const point3& p, double& u, double& v) {
        u = std::atan2(-p.z(), p.x()) / (2 * pi) + 0.5;
        v = std::acos(-p.y()) / pi;
    }
};

#endif