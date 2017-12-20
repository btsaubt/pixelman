#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void makePic(int width, int height, int red, int green, int blue){
    const int dimx = width, dimy = height;
    int i, j;
    FILE *fp = fopen("first.ppm", "wb"); /* b - binary mode */
    (void) fprintf(fp, "P6\n%d %d\n255\n", dimx, dimy);
    for (j = 0; j < dimy; ++j)
    {
        for (i = 0; i < dimx; ++i)
        {
            static unsigned char color[3];
            color[0] = red;  /* red */
            color[1] = green;  /* green */
            color[2] = blue;  /* blue */
            (void) fwrite(color, 1, 3, fp);
        }
    }
    (void) fclose(fp);
}
