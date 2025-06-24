#ifndef IMAGE_H
#define IMAGE_H

#define STB_IMAGE_IMPLEMENTATION
#define STBI_FAILURE_USERMSG
#include "external/stb_image.h"

#include "color.h"
#include <iostream>

class image {
  private:
    float *fdata = nullptr;
    int image_width = 0;
    int image_height = 0;

  public:
    image() {}

    image(const char *filepath) {
        fdata = stbi_loadf(filepath, &image_width, &image_height, nullptr, 3);
        if(fdata == nullptr) {
            std::cerr << "ERROR: Could not load image file '" << filepath << "'.\n";
        }
    }

    ~image() {
        STBI_FREE(fdata);
    }

    int width() const { return image_width; }
    int height() const { return image_height; }

    color pixel_color(int x, int y) const {
        if(fdata == nullptr)
            return color(1, 0, 1);
        
        int i = (y * image_width + x) * 3;
        return color(fdata[i], fdata[i+1], fdata[i+2]);
    }
};

#endif