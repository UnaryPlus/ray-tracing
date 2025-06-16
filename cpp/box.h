#ifndef BOX_H
#define BOX_H

#include "interval.h"
#include "vec3.h"
#include "ray.h"

class box {
  public:
    interval x, y, z;
    
    box() {} // Empty

    box(const interval& x, const interval& y, const interval& z)
        : x(x), y(y), z(z) {}

    box(const point3& a, const point3& b)
        : x((a[0] <= b[0]) ? interval(a[0], b[0]) : interval(b[0], a[0]))
        , y((a[1] <= b[1]) ? interval(a[1], b[1]) : interval(b[1], a[1]))
        , z((a[2] <= b[2]) ? interval(a[2], b[2]) : interval(b[2], a[2]))
        {}

    box(const box& box0, const box& box1)
        : x(box0.x, box1.x)
        , y(box0.y, box1.y)
        , z(box0.z, box1.z)
        {}

    const interval& axis_interval(int n) const {
        return (n == 1) ? y : (n == 2) ? z : x;
    }

    bool hit(const ray& r, interval ray_t) const {
        const point3& ray_orig = r.origin();
        const vec3& ray_dir = r.direction();

        for(int axis = 0; axis < 3; axis++) {
            const interval& ax = axis_interval(axis);

            double t0 = (ax.min - ray_orig[axis]) / ray_dir[axis];
            double t1 = (ax.max - ray_orig[axis]) / ray_dir[axis];
            if(t0 > t1) std::swap(t0, t1);

            if(t0 > ray_t.min) ray_t.min = t0;
            if(t1 < ray_t.max) ray_t.max = t1;

            if(ray_t.max <= ray_t.min) return false;
        }

        return true;
    }
};

#endif