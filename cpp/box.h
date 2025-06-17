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
        : x(x), y(y), z(z) {
        pad_to_minimum();
    }

    box(const point3& a, const point3& b)
        : x(std::fmin(a[0], b[0]), std::fmax(a[0], b[0]))
        , y(std::fmin(a[1], b[1]), std::fmax(a[1], b[1]))
        , z(std::fmin(a[2], b[2]), std::fmax(a[2], b[2])) {
        pad_to_minimum();
    }

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

    int longest_axis() const {
        if(x.size() > y.size()) 
            return x.size() > z.size() ? 0 : 2;
        else
            return y.size() > z.size() ? 1 : 2;
    } 

    static const box empty, universe;

  private:
    void pad_to_minimum() {
        double delta = 0.0001;
        if(x.size() < delta) x = x.with_size(delta);
        if(y.size() < delta) y = y.with_size(delta);
        if(z.size() < delta) z = z.with_size(delta);
    }
};

const box box::empty = box(interval::empty, interval::empty, interval::empty);
const box box::universe = box(interval::universe, interval::universe, interval::universe);

box operator+(const box& bbox, const vec3& offset) {
    return box(bbox.x + offset.x(), bbox.y + offset.y(), bbox.z + offset.z());
}

box operator+(const vec3& offset, const box& bbox) {
    return bbox + offset;
}

#endif