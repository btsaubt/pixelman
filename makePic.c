#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void makePic(int width, int red[][width], int green[][width], int blue[][width]){
    const int dimx = width, dimy = sizeof(red[0]);
    int i, j;
    FILE *fp = fopen("first.ppm", "wb"); /* b - binary mode */
    (void) fprintf(fp, "P6\n%d %d\n255\n", dimx, dimy);
    for (j = 0; j < dimy; ++j)
    {
        for (i = 0; i < dimx; ++i)
        {
            static unsigned char color[3];
            color[0] = red[i][j];  /* red */
            color[1] = green[i][j];  /* green */
            color[2] = blue[i][j];  /* blue */
            (void) fwrite(color, 1, 3, fp);
        }
    }
    (void) fclose(fp);
}
