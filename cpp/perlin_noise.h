#ifndef PERLIN_NOISE_H
#define PERLIN_NOISE_H

#include "utils.h"
#include "vec3.h"

class perlin_noise {
  private:
    static const int point_count = 256;
    vec3 randvec[point_count];
    int perm_x[point_count];
    int perm_y[point_count];
    int perm_z[point_count];

  public:
    perlin_noise() {
        for(int i = 0; i < point_count; i++) {
            randvec[i] = random_unit_vector();
            perm_x[i] = perm_y[i] = perm_z[i] = i;
        }
        random_shuffle(perm_x, point_count);
        random_shuffle(perm_y, point_count);
        random_shuffle(perm_z, point_count);
    }

    double noise(const point3& p) const {
        double u = p.x() - std::floor(p.x());
        double v = p.y() - std::floor(p.y());
        double w = p.z() - std::floor(p.z());

        auto i = int(std::floor(p.x()));
        auto j = int(std::floor(p.y()));
        auto k = int(std::floor(p.z()));
        vec3 c[2][2][2];

        for(int di = 0; di < 2; di++) {
            for(int dj = 0; dj < 2; dj++) {
                for(int dk = 0; dk < 2; dk++) {
                    c[di][dj][dk] = randvec[
                        perm_x[(i+di) & 255] ^
                        perm_y[(j+dj) & 255] ^
                        perm_z[(k+dk) & 255]
                    ];
                }
            }
        }

        return perlin_interp(c, u, v, w);
    }

    double turbulence(const point3& p, int depth) const {
        double accum = 0.0;
        double weight = 1.0;
        auto temp_p = p;
        
        for(int i = 0; i < depth; i++) {
            accum += weight * noise(temp_p);
            weight *= 0.5;
            temp_p *= 2;
        }

        return accum;
    }
  
  private:
    static double perlin_interp(const vec3 c[2][2][2], double u, double v, double w) {
        double uu = u*u*(3 - 2*u);
        double vv = v*v*(3 - 2*v);
        double ww = w*w*(3 - 2*w);
        double accum = 0.0;

        for (int i=0; i < 2; i++) {
            for (int j=0; j < 2; j++) {
                for (int k=0; k < 2; k++) {
                    accum += (i*u + (1-i)*(1-u))
                           * (j*v + (1-j)*(1-v))
                           * (k*w + (1-k)*(1-w))
                           * dot(c[i][j][k], vec3(u-i, v-j, w-k));
                }
            }
        }
        return accum;
    }
};

#endif