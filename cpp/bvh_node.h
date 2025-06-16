#ifndef BVH_NODE_H
#define BVH_NODE_H

#include "hittable.h"
#include "hittable_list.h"
#include "box.h"
#include <algorithm>

using std::shared_ptr;

class bvh_node : public hittable {
  private:
    shared_ptr<hittable> left;
    shared_ptr<hittable> right;
    box bbox;

  public:
    bvh_node(hittable_list list) : bvh_node(list.objects, 0, list.objects.size()) {}

    bvh_node(std::vector<shared_ptr<hittable>>& objects, size_t start, size_t end) {
        bbox = box::empty;
        for(size_t i = start; i < end; i++) {
            bbox = box(bbox, objects[i]->bounding_box());
        }

        int axis = bbox.longest_axis();

        auto box_compare = [axis](const shared_ptr<hittable> a, const shared_ptr<hittable> b) {
            auto a_axis_interval = a->bounding_box().axis_interval(axis);
            auto b_axis_interval = b->bounding_box().axis_interval(axis);
            return a_axis_interval.min < b_axis_interval.min;
        };

        size_t object_span = end - start;
        if(object_span == 1) {
            left = right = objects[start];
        } else if(object_span == 2) {
            left = objects[start];
            right = objects[start + 1];
        } else {
            std::sort(std::begin(objects) + start, std::begin(objects) + end, box_compare);
            auto mid = start + object_span/2;
            left = make_shared<bvh_node>(objects, start, mid);
            right = make_shared<bvh_node>(objects, mid, end);
        }
    }

    bool hit(const ray& r, interval ray_t, hit_record& rec) const override {
        if(!bbox.hit(r, ray_t))
            return false;
        
        bool hit_left = left->hit(r, ray_t, rec);
        bool hit_right = right->hit(r, hit_left ? interval(ray_t.min, rec.t) : ray_t, rec);

        return hit_left || hit_right;
    }

    box bounding_box() const override { return bbox; }
};

#endif