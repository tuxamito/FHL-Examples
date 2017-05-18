#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SOFTENING 1e-9

typedef struct
{
  float x, y, z;
} Triple;
 
void bodyForce(Triple *pos, Triple *frc, int n)
{
  for (int i = 0; i < n; i++)
    {
      frc[i].x = 0.0f;
      frc[i].y = 0.0f;
      frc[i].z = 0.0f;
 
      for (int j = 0; j < n; j++)
	{
	  float dx = pos[j].x - pos[i].x;
	  float dy = pos[j].y - pos[i].y;
	  float dz = pos[j].z - pos[i].z;
	  float distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;
	  float invDist = 1.0f / sqrtf(distSqr);
	  float invDist3 = invDist * invDist * invDist;
 
	  frc[i].x += dx * invDist3;
	  frc[i].y += dy * invDist3;
	  frc[i].z += dz * invDist3;
        }
    }
}
 
void velocities(Triple *frc, Triple *vel, float dt, int n)
{
  for (int i = 0; i < n; i++)
    {
      vel[i].x += dt*frc[i].x;
      vel[i].y += dt*frc[i].y;
      vel[i].z += dt*frc[i].z;
    }
}
 
void integrate(Triple *pos, Triple *vel, float dt, int n)
{
  for (int i = 0 ; i < n; i++)
    {
      pos[i].x += vel[i].x*dt;
      pos[i].y += vel[i].y*dt;
      pos[i].z += vel[i].z*dt;
    }
}


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


void generateTArray(Triple* t, char* string, int n) {
  char* p = string;
  int i = 0;
  float v1, v2, v3;
  
  while(p != 0) {
    if(p[0] == '\n')
      p++;
    sscanf(p, "%f,%f,%f", &v1, &v2, &v3);

    t[i].x = v1;
    t[i].y = v2;
    t[i].z = v3;

    i++;
    p = strstr(p, "\n");
  }
}

void printPos(Triple *pos, int n) {
  for (int i = 0; i < n; i++)
    {
      printf("%f %f %f\n", pos[i].x, pos[i].y, pos[i].z);
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
 
  Triple *pStruct = (Triple*)malloc(nBodies*sizeof(Triple));
  Triple *vStruct = (Triple*)malloc(nBodies*sizeof(Triple));
  Triple *fStruct = (Triple*)malloc(nBodies*sizeof(Triple));

  generateTArray(pStruct, posstr, nBodies);
  generateTArray(vStruct, velstr, nBodies);

  free(posstr);
  free(velstr);
  
  for (int iter = 0; iter < nIters; iter++)
    {
      bodyForce(pStruct, fStruct, nBodies); // compute interbody forces
      velocities(fStruct, vStruct, dt, nBodies); // update velocities
      integrate(pStruct, vStruct, dt, nBodies);  // integrate for dt
    }
 

  printPos(pStruct, nBodies);
  
  free(pStruct);
  free(vStruct);
  free(fStruct);
 
  return 0;
}
