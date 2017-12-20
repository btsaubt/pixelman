#include <stdio.h>
#define new_min(x,y) ((x) <= (y)) ? (x) : (y)

void inputPic(int effect)
{
  int pix_x=300,pix_y=300;    // image dimns in pixels
  static int image[300][300][4]; // first [] number here is total pixels of each color in
                              // my image, 3 is for //RGB values
  FILE *streamIn;
  // //opening 24bit image
  streamIn = fopen("edwards.bmp", "r"); // a bigger star in black and a smaller
  //                                         // star in blue (refer figure attached)

  int byte;
  int i,j;
  for(i=0;i<57;i++) {
    byte = fgetc(streamIn);  // strip out BMP header-> for //24bit bmp image
  }

  // // initiating with new "i" different from above
  int k;
  for(k=0;k<pix_y;k++) {
    for(j=0;j<pix_x;j++) {
      image[k][j][3] = fgetc(streamIn); 
      image[k][j][2] = fgetc(streamIn);  // use BMP 24bit with no alpha channel
      image[k][j][1] = fgetc(streamIn);  // BMP uses BGR but we want RGB, grab //byte-by-byte
      image[k][j][0] = fgetc(streamIn);
    }
  }
  if(effect==1){
  int temp;
  for(k=0;k<pix_y;k++) {
    for(j=0;j<pix_x;j++) {
      temp = (image[k][j][2] + image[k][j][1] + image[k][j][0])/3;
      image[k][j][2] = temp; 
      image[k][j][1] = temp;
      image[k][j][0] = temp;
    }
  }
  }
  if(effect==2){
  float tempR;
  float tempG;
  float tempB;
  for(k=0;k<pix_y;k++) {
    for(j=0;j<pix_x;j++) {
      tempR = image[k][j][2]*.189 + image[k][j][1]*.769 + image[k][j][0]*.393;
      tempG = image[k][j][2]*.168 + image[k][j][1]*.686 + image[k][j][0]*.349;
      tempB = image[k][j][2]*.131 + image[k][j][1]*.534 + image[k][j][0]*.272;
      image[k][j][2] = new_min((int) tempB, 255); 
      image[k][j][1] = new_min((int) tempG, 255);
      image[k][j][0] = new_min((int) tempR, 255);
    }
  }
  }
  FILE *fp = fopen("outimage.ppm", "wb"); /* b - binary mode */
  (void) fprintf(fp, "P6\n%d %d\n255\n", pix_x, pix_y);
  for(k=0;k<pix_y;k++) {
    for(j=0;j<pix_x;j++) {
          static unsigned char color[3];
          color[0] = image[k][j][0]; /* red */
          color[1] = image[k][j][1];  /* green */
          color[2] = image[k][j][2];  /* blue */
          (void) fwrite(color, 1, 3, fp);
      }
  }
  (void) fclose(fp);
}
