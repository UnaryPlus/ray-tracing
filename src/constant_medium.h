#ifndef CONSTANT_MEDIUM_H
#define CONSTANT_MEDIUM_H

#include "hittable.h"
#include "color.h"
#include "material.h"

using std::make_shared;

class constant_medium : public hittable {
  private:
    shared_ptr<hittable> boundary;
    double neg_inv_density;
    shared_ptr<material> iso_mat;

  public:
    constant_medium(shared_ptr<hittable> boundary, double density, const color& albedo)
        : boundary(boundary), neg_inv_density(-1 / density), iso_mat(make_shared<isotropic>(albedo)) {}

    constant_medium(shared_ptr<hittable> boundary, double density, shared_ptr<texture> tex)
        : boundary(boundary), neg_inv_density(-1 / density), iso_mat(make_shared<isotropic>(tex)) {}
    
    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        hit_record rec1, rec2;

        if(!boundary->hit(r, interval::universe, rec1))
            return false;
        
        if(!boundary->hit(r, interval(rec1.t + 0.0001, infinity), rec2))
            return false;
        
        double t0 = std::max(rec1.t, ray_t.min);
        double t1 = std::min(rec2.t, ray_t.max);

        if(t0 >= t1)
            return false;
        
        double ray_scale = r.direction().length();
        double distance_inside_boundary = (t1 - t0) * ray_scale;
        double hit_distance = neg_inv_density * std::log(random_double());

        if(hit_distance > distance_inside_boundary)
            return false;
        
        rec.t = t0 + hit_distance / ray_scale;
        rec.p = r.at(rec.t);
        rec.mat = iso_mat;
        rec.u = rec1.u;
        rec.v = rec1.v;
        // normal and front_face are not used by isotropic material
        return true;
    }

    box bounding_box() const override { return boundary->bounding_box(); }
};

#endif