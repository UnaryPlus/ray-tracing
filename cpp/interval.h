#ifndef INTERVAL_H
#define INTERVAL_H

#include "utils.h"

class interval {
  public:
    double min, max;

    interval() : min(+infinity), max(-infinity) {}

    interval(double min, double max) : min(min), max(max) {}

    interval(const interval& a, const interval& b)
        : min(a.min <= b.min ? a.min : b.min)
        , max(a.max >= b.max ? a.max : b.max)
        {}

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

    interval with_size(double delta) const {
        double avg = (min + max) / 2;
        return interval(avg - delta/2, avg + delta/2);
    }

    static const interval empty, unit, universe;
};

const interval interval::empty = interval(+infinity, -infinity);
const interval interval::unit = interval(0, 1);
const interval interval::universe = interval(-infinity, +infinity);

interval operator+(const interval& ival, double displacement) {
    return interval(ival.min + displacement, ival.max + displacement);
}

interval operator+(double displacement, const interval& ival) {
    return ival + displacement;
}

#endif