#ifndef INTERVAL_H
#define INTERVAL_H

#include "utils.h"

class interval {
  public:
    double min, max;

    interval(double min, double max) : min(min), max(max) {}

    double size() const {
        return max - min;
    }

    bool contains(double x) const {
        return min <= x && x <= max;
    }

    bool surrounds(double x) const {
        return min < x && x < max;
    }

    double clamp(double x) const {
        return (x < min) ? min : (x > max) ? max : x;
    }

    static const interval empty, universe;
};

const interval interval::empty = interval(1, 0);
const interval interval::universe = interval(-infinity, +infinity);

#endif