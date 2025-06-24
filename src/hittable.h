#ifndef HITTABLE_H
#define HITTABLE_H

#include "ray.h"
#include "interval.h"
#include "box.h"

using std::shared_ptr;

class material;

class hit_record {
  public:
    point3 p;
    vec3 normal;
    shared_ptr<material> mat;
    double t;
    double u;
    double v;
    bool front_face;

    void set_face_normal(const ray& r, const vec3& outward_normal) {
        // Sets the hit record normal vector.
        // NOTE: the parameter `outward_normal` is assumed to have unit length.

        front_face = dot(r.direction(), outward_normal) < 0;
        normal = front_face ? outward_normal : -outward_normal;
    }
};

class hittable {
  public:
    virtual ~hittable() = default;

    virtual bool hit(const ray& r, interval ray_t, hit_record& rec) const = 0;

    virtual box bounding_box() const = 0;
};

class translate : public hittable {
  private:
    shared_ptr<hittable> object;
    vec3 offset;
    box bbox;

  public:
    translate(shared_ptr<hittable> object, const vec3& offset) 
        : object(object), offset(offset) {
        bbox = object->bounding_box() + offset;
    }

    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        ray offset_r = ray(r.origin() - offset, r.direction(), r.time());
        if(object->hit(offset_r, ray_t, rec)) {
            rec.p += offset;
            return true;
        }
        else {
            return false;
        }
    }

    box bounding_box() const override { return bbox; }
};

class rotate_y : public hittable {
  private:
    shared_ptr<hittable> object;
    double cos_theta;
    double sin_theta;
    box bbox;

  public:
    rotate_y(shared_ptr<hittable> object, double theta) 
        : object(object), cos_theta(std::cos(theta)), sin_theta(std::sin(theta)) {

        bbox = object->bounding_box();
        point3 min(+infinity, bbox.y.min, +infinity);
        point3 max(-infinity, bbox.y.max, -infinity);

        for(int i = 0; i < 2; i++) {
            for(int k = 0; k < 2; k++) {
                point3 corner(i ? bbox.x.max : bbox.x.min, 0, k ? bbox.z.max : bbox.z.min);
                point3 test = rotate_pos(corner);

                for(int j = 0; j <= 2; j += 2) {
                    min[j] = std::min(min[j], test[j]);
                    max[j] = std::max(max[j], test[j]);
                }
            }
        }

        bbox = box(min, max);
    }

    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        ray rotated_r = ray(rotate_neg(r.origin()), rotate_neg(r.direction()), r.time());
        if(object->hit(rotated_r, ray_t, rec)) {
            rec.p = rotate_pos(rec.p);
            rec.normal = rotate_pos(rec.normal);
            return true;
        }
        else {
            return false;
        }
    }

    box bounding_box() const override { return bbox; }

  private:
    vec3 rotate_neg(const vec3& v) const {
        return vec3(cos_theta * v.x() - sin_theta * v.z(), v.y(), sin_theta * v.x() + cos_theta * v.z());
    }

    vec3 rotate_pos(const vec3& v) const {
        return vec3(cos_theta * v.x() + sin_theta * v.z(), v.y(), -sin_theta * v.x() + cos_theta * v.z());
    }
};

#endif