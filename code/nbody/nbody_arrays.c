#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SOFTENING 1e-9

void bodyForce(float *posx, float *posy, float *posz, float *frcx, float *frcy, float *frcz, int n)
{
  for (int i = 0; i < n; i++)
    {
      frcx[i] = 0.0f;
      frcy[i] = 0.0f;
      frcz[i] = 0.0f;
 
      for (int j = 0; j < n; j++)
	{
	  float dx = posx[j] - posx[i];
	  float dy = posy[j] - posy[i];
	  float dz = posz[j] - posz[i];
	  float distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;
	  float invDist = 1.0f / sqrtf(distSqr);
	  float invDist3 = invDist * invDist * invDist;
 
	  frcx[i] += dx * invDist3;
	  frcy[i] += dy * invDist3;
	  frcz[i] += dz * invDist3;
        }
    }
}

void velocities(float *frcx, float *frcy, float *frcz, float *velx, float *vely, float *velz, float dt, int n)
{
  for (int i = 0; i < n; i++)
    {
      velx[i] += dt*frcx[i];
      vely[i] += dt*frcy[i];
      velz[i] += dt*frcz[i];
    }
}
 
void integrate(float *posx, float *posy, float *posz, float *velx, float *vely, float *velz, float dt, int n)
{
  for (int i = 0 ; i < n; i++)
    {
      posx[i] += velx[i]*dt;
      posy[i] += vely[i]*dt;
      posz[i] += velz[i]*dt;
    }
}


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


void generateTArray(float *tx, float *ty, float *tz, char* string, int n) {
  char* p = string;
  int i = 0;
  float v1, v2, v3;
  
  while(p != 0) {
    if(p[0] == '\n')
      p++;
    sscanf(p, "%f,%f,%f", &v1, &v2, &v3);

    tx[i] = v1;
    ty[i] = v2;
    tz[i] = v3;

    i++;
    p = strstr(p, "\n");
  }
}

void printPos(float *posx, float *posy, float *posz, int n) {
  for (int i = 0; i < n; i++)
    {
      printf("%f %f %f\n", posx[i], posy[i], posz[i]);
    }
}

/////////////////////////////////////////////////////////////////////////////

char* readFile(const char* file) {
  FILE* f = fopen(file, "rb");
  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  fseek(f, 0, SEEK_SET);
  
  char* string = malloc(fsize + 1);
  size_t r = fread(string, fsize, 1, f);

  if(r != 1) {
    free(string);
    return 0;
  }
  
  fclose(f);
  
  string[fsize] = 0;

  return string;
}

int countLines(char* file) {
  long i = 0;
  int  c = 1;
  
  while (file[i] != 0) {
    if(file[i] == '\n') {
      c++;
    }
    i++;
  }

  return c;
}

int main(const int argc, const char** argv) { 
  int nBodies;
  const float dt   = 0.1; // time step
  const int nIters = 2;  // simulation iterations


  char* posstr = readFile("pos.csv");
  char* velstr = readFile("vel.csv");

  nBodies = countLines(posstr);

  float *posx = (float*)malloc(nBodies*sizeof(float));
  float *posy = (float*)malloc(nBodies*sizeof(float));
  float *posz = (float*)malloc(nBodies*sizeof(float));

  float *velx = (float*)malloc(nBodies*sizeof(float));
  float *vely = (float*)malloc(nBodies*sizeof(float));
  float *velz = (float*)malloc(nBodies*sizeof(float));

  float *frcx = (float*)malloc(nBodies*sizeof(float));
  float *frcy = (float*)malloc(nBodies*sizeof(float));
  float *frcz = (float*)malloc(nBodies*sizeof(float));
  

  generateTArray(posx, posy, posz, posstr, nBodies);
  generateTArray(velx, vely, velz, velstr, nBodies);

  free(posstr);
  free(velstr);
  
  for (int iter = 0; iter < nIters; iter++)
    {
      bodyForce(posx, posy, posz, frcx, frcy, frcz, nBodies); // compute interbody forces
      velocities(frcx, frcy, frcz, velx, vely, velz, dt, nBodies); // update velocities
      integrate(posx, posy, posz, velx, vely, velz, dt, nBodies);  // integrate for dt
    }
 

  printPos(posx, posy, posz, nBodies);


  free(velx);
  free(vely);
  free(velz);

  free(posx);
  free(posy);
  free(posz);

  free(frcx);
  free(frcy);
  free(frcz);
  
 
  return 0;
}
