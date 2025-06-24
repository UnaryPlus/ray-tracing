#ifndef QUAD_H
#define QUAD_H

#include "hittable.h"
#include "hittable_list.h"
#include "box.h"

class quad : public hittable {
  private:
    point3 Q;
    vec3 u, v;
    shared_ptr<material> mat;
    box bbox;
    vec3 normal, w;
    double D;

  public:
    quad(const point3& Q, const vec3& u, const vec3& v, shared_ptr<material> mat)
        : Q(Q), u(u), v(v), mat(mat) {
        box bbox_diagonal1 = box(Q, Q + u + v);
        box bbox_diagonal2 = box(Q + u, Q + v);
        bbox = box(bbox_diagonal1, bbox_diagonal2);

        auto n = cross(u, v);
        normal = unit_vector(n);
        w = n / dot(n, n);
        D = dot(normal, Q);
    }

    box bounding_box() const override { return bbox; }

    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        double denom = dot(normal, r.direction());

        // No hit if the ray is parallel to the plane.
        if (std::fabs(denom) < 1e-8)
            return false;

        // Return false if the hit point parameter t is outside the ray interval.
        double t = (D - dot(normal, r.origin())) / denom;
        if (!ray_t.surrounds(t))
            return false;

        // Determine if the hit point lies within the planar shape using its plane coordinates.
        point3 intersection = r.at(t);
        vec3 rel = intersection - Q;
        double alpha = dot(w, cross(rel, v));
        double beta = dot(w, cross(u, rel));

        if(!(interval::unit.contains(alpha) && interval::unit.contains(beta)))
            return false;
        
        // Ray hits the 2D shape.
        rec.p = intersection;
        rec.t = t;
        rec.u = alpha;
        rec.v = beta;
        rec.mat = mat;
        rec.set_face_normal(r, normal);

        return true;
    }
};

shared_ptr<hittable_list> quad_box(const box& b, shared_ptr<material> mat) {
    auto min_corner = point3(b.x.min, b.y.min, b.z.min);
    auto max_corner = point3(b.x.max, b.y.max, b.z.max);

    auto dx = vec3(b.x.size(), 0, 0);
    auto dy = vec3(0, b.y.size(), 0);
    auto dz = vec3(0, 0, b.z.size());
    
    auto sides = make_shared<hittable_list>();
    sides->add(make_shared<quad>(min_corner, dz, dy, mat));
    sides->add(make_shared<quad>(min_corner, dx, dz, mat));
    sides->add(make_shared<quad>(min_corner, dy, dx, mat));
    sides->add(make_shared<quad>(max_corner, -dy, -dz, mat));
    sides->add(make_shared<quad>(max_corner, -dz, -dx, mat));
    sides->add(make_shared<quad>(max_corner, -dx, -dy, mat));

    return sides;
}

#endif