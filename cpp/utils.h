#ifndef UTILS_H
#define UTILS_H

#include <limits>
#include <random>

const double infinity = std::numeric_limits<double>::infinity();
const double pi = 3.1415926535897932385;

inline double degrees(double x) {
    return x * pi / 180.0;
}

inline double random_double() {
    static std::uniform_real_distribution<double> distribution(0.0, 1.0);
    static std::mt19937 generator;
    return distribution(generator);
}

inline double random_double(double min, double max) {
    return min + (max - min)*random_double();
}

inline int random_int(int min, int max) {
    return int(random_double(min, max+1));
}

#endif