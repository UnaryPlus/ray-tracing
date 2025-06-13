// g++ create_ppm.cc -o create_ppm && ./create_ppm > image.ppm

#include <cstdio>

int main() {
    int image_width = 256;
    int image_height = 256;

    printf("P3\n%d %d\n255\n", image_width, image_height);
    for(int j = 0; j < image_height; j++) {
        for(int i = 0; i < image_width; i++) {
            double r = double(i) / (image_width - 1); 
            double g = double(j) / (image_height - 1);
            double b = 0.0;

            int ir = int(r * 255.999);
            int ig = int(g * 255.999);
            int ib = int(b * 255.999);

            printf("%d %d %d\n", ir, ig, ib);
        }
    }
}