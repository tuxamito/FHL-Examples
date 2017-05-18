#include <stdio.h>
#include <stdlib.h>

#define PRANGE 10
#define VRANGE 1

int main(int argc,char* argv[]) {
  int ELEM;
  
  // Get Command Line Arguments
  if(argc!=2){
    printf("Incorrect syntax: second argument is the number of particles");
    exit(2);
  }

  ELEM = atoi(argv[1]);
  
  srand(0);

  FILE* fp = fopen("pos.csv", "w");
  FILE* fv = fopen("vel.csv", "w");

  for(int i=0; i<ELEM-1; ++i) {
    fprintf(fp, "%f,",  (float)(drand48()*2*PRANGE-PRANGE));
    fprintf(fp, "%f,",  (float)(drand48()*2*PRANGE-PRANGE));
    fprintf(fp, "%f\n", (float)(drand48()*2*PRANGE-PRANGE));
  }
  if(ELEM>0) {
    fprintf(fp, "%f,", (float)(drand48()*2*PRANGE-PRANGE));
    fprintf(fp, "%f,", (float)(drand48()*2*PRANGE-PRANGE));
    fprintf(fp, "%f",  (float)(drand48()*2*PRANGE-PRANGE));
  }

  
  for(int i=0; i<ELEM-1; ++i) {
    fprintf(fv, "%f,",  (float)(drand48()*2*VRANGE-VRANGE));
    fprintf(fv, "%f,",  (float)(drand48()*2*VRANGE-VRANGE));
    fprintf(fv, "%f\n", (float)(drand48()*2*VRANGE-VRANGE));
  }
  if(ELEM>0) {
    fprintf(fv, "%f,", (float)(drand48()*2*VRANGE-VRANGE));
    fprintf(fv, "%f,", (float)(drand48()*2*VRANGE-VRANGE));
    fprintf(fv, "%f",  (float)(drand48()*2*VRANGE-VRANGE));
  }

  fclose(fp);
  fclose(fv);
  
  return 0;
}
