#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */
 /*@@
   @file      CartGrid3D.c
   @date      Thu Oct  7 13:20:06 1999
   @author    Tom Goodale
   @desc 
   Set up coordinates for a 3D Cartesian grid.
   C Conversion of Fortran routine written by Gab.
   @enddesc 
 @@*/

/*#define CCTK_DEBUG*/

#include <stdio.h>
#include <math.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/CartGrid3D.c,v 1.29 2001/08/18 21:40:40 allen Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_CartGrid3D_c)

void DecodeSymParameters3D(int sym[6]);
void CartGrid3D(CCTK_ARGUMENTS);

#define max(a,b) ((a) > (b) ? (a) : (b))
#define SQR(a) ((a)*(a))

void CartGrid3D(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
  
  int iconv, i, j, k;
  double dconv;
  CCTK_REAL x_origin,y_origin,z_origin;
  CCTK_REAL this_dx,this_dy,this_dz;
  CCTK_REAL xmin1,ymin1,zmin1,xmax1,ymax1,zmax1;
  CCTK_REAL lowerx,upperx,lowery,uppery,lowerz,upperz;
  int lowerxi,upperxi,loweryi,upperyi,lowerzi,upperzi;
  char infoline[120];

  int domainsym[6];
  int cntstag[3];


  /* Some compilers warn about variables which might be uninitialized
     because they are assigned in some if/else statements only */
  x_origin = y_origin = z_origin = 0.0;
  lowerxi = loweryi = lowerzi = 0;
  upperxi = cctk_gsh[0]-1;
  upperyi = cctk_gsh[1]-1;
  upperzi = cctk_gsh[2]-1;
  this_dx = this_dy = this_dz = 0.0;

  /* Avoid origin?  Default is yes */
  cntstag[0] = no_origin && no_originx && avoid_origin && avoid_originx;
  cntstag[1] = no_origin && no_originy && avoid_origin && avoid_originy;
  cntstag[2] = no_origin && no_originz && avoid_origin && avoid_originz;

  /* Determine symmetries of domain */
  DecodeSymParameters3D(domainsym);
  
  /* Calculate physical indices, using symmetries and periodicity */
  if (domainsym[0]) 
  {
    lowerxi = cctk_nghostzones[0];
  }  
  else
  {
    lowerxi = 0;
  }
  if (domainsym[2]) 
  {
    loweryi = cctk_nghostzones[1];
  }  
  else
  {
    loweryi = 0;
  }
  if (domainsym[4]) 
  {
    lowerzi = cctk_nghostzones[2];
  }  
  else
  {
    lowerzi = 0;
  }

  if (periodic)
  {
    if (periodic_x)
    {
      lowerxi = cctk_nghostzones[0]-1;
      upperxi = cctk_gsh[0]-1-cctk_nghostzones[0];
    }
    if (periodic_y)
    {
      loweryi = cctk_nghostzones[1]-1;
      upperyi = cctk_gsh[1]-1-cctk_nghostzones[1];
    }
    if (periodic_z)
    {
      lowerzi = cctk_nghostzones[2]-1;
      upperzi = cctk_gsh[2]-1-cctk_nghostzones[2];
    }
  }

  dconv = pow(2, cctk_convlevel);
  iconv = (int)dconv;

  /****************************************************************
   *
   *  BYRANGE
   *
   *  User gives: minimum and maximum values of coordinates and
   *              the number of gridpoints on the coarse grid
   *
   ***************************************************************/
  /**************************************************************
   *
   *   BOX (-0.5 to 0.5)
   *
   *   User gives: number of gridpoints on the coarse grid
   *
   **************************************************************/

  if (CCTK_Equals(type,"byrange") || CCTK_Equals(type,"box")) 
  { 
    
    if (CCTK_Equals(type,"box"))
    {
      
      /*  Coordinates are all -0.5 to 0.5 */
      xmax1 = 0.5;
      ymax1 = 0.5;
      zmax1 = 0.5;
      
      xmin1 = -0.5;
      ymin1 = -0.5;
      zmin1 = -0.5;
      
    }
    else
    {

      if (xyzmax != -424242)
      {
          xmax1 = xyzmax;
          ymax1 = xyzmax;
          zmax1 = xyzmax;
      }
      else
      {
          xmax1 = xmax;
          ymax1 = ymax;
          zmax1 = zmax;
      }
  
      if (xyzmin != -424242)
      {
          xmin1 = xyzmin;
          ymin1 = xyzmin;
          zmin1 = xyzmin;
      }
      else
      {
          xmin1 = xmin;
          ymin1 = ymin;
          zmin1 = zmin;
      }

    }


    
    /* Grid spacing on coarsest grid */
    /* TODO: Put the coordinates into arrays, and loop over the
       dimensions.  That gets ride of all the triplicated code. */
    if (domainsym[0]) 
    {
      if (cntstag[0]) 
      {
        *coarse_dx = xmax1 / (cctk_gsh[0] - cctk_nghostzones[0] - 0.5);
        x_origin = - (cctk_nghostzones[0] - 0.5) * *coarse_dx * iconv;
      } 
      else 
      {
        *coarse_dx = xmax1 / (cctk_gsh[0] - cctk_nghostzones[0] - 1);
        x_origin = -  cctk_nghostzones[0]        * *coarse_dx * iconv;
      }
    } 
    else 
    {
      *coarse_dx = (xmax1 - xmin1) / max(cctk_gsh[0] - 1, 1);
      x_origin = xmin1;
    }

    if (domainsym[2]) 
    {
      if (cntstag[1]) 
      {
        *coarse_dy = ymax1 / (cctk_gsh[1] - cctk_nghostzones[1] - 0.5);
        y_origin = - (cctk_nghostzones[1] - 0.5) * *coarse_dy * iconv;
      } 
      else 
      {
        *coarse_dy = ymax1 / (cctk_gsh[1] - cctk_nghostzones[1] - 1);
        y_origin = -  cctk_nghostzones[1]      * *coarse_dy * iconv;
      }
    } 
    else 
    {
      *coarse_dy = (ymax1 - ymin1) / max(cctk_gsh[1] - 1, 1);
      y_origin = ymin1;
    }
    
    if (domainsym[4]) 
    {
      if (cntstag[2]) 
      {
        *coarse_dz = zmax1 / (cctk_gsh[2] - cctk_nghostzones[2] - 0.5);
        z_origin = - (cctk_nghostzones[2] - 0.5) * *coarse_dz * iconv;
      } 
      else 
      {
        *coarse_dz = zmax1 / (cctk_gsh[2] - cctk_nghostzones[2] - 1);
        z_origin = -  cctk_nghostzones[2]      * *coarse_dz * iconv;
      }
    } 
    else 
    {
      *coarse_dz = (zmax1 - zmin1) / max(cctk_gsh[2] - 1, 1);
      z_origin = zmin1;
    }
    
    /* dx,dy,dz on the grid we are on */
    this_dx = *coarse_dx * iconv;
    this_dy = *coarse_dy * iconv;
    this_dz = *coarse_dz * iconv;

  }

  /**************************************************************
   *  BYSPACING
   *
   *  User gives: grid spacing on the coarsest GH and
   *              the number of gridpoints on the coarsest GH
   *
   **************************************************************/

  else if (CCTK_Equals(type,"byspacing")) 
  {

    /* Dx, Dy, Dx on the coarsest grid */

    if (dxyz > 0) 
    {
      *coarse_dx = dxyz;
      *coarse_dy = dxyz;
      *coarse_dz = dxyz;
    }
    else
    {
      *coarse_dx = dx;
      *coarse_dy = dy;
      *coarse_dz = dz;
    }

    /* dx, dy, dz on the grid we are on */
    this_dx = *coarse_dx * iconv;
    this_dy = *coarse_dy * iconv;
    this_dz = *coarse_dz * iconv;
    
    
    
    /* Set minimum values of coordinates */
    if (domainsym[0]) 
    {
      x_origin = (- cctk_nghostzones[0] + cntstag[0] * 0.5) * this_dx;
    } 
    else 
    {
      x_origin = - 0.5 * (cctk_gsh[0]-1 - cntstag[0] * (cctk_gsh[0])%2)
        * this_dx;
    }
    
    if (domainsym[2]) 
    {
      y_origin = (- cctk_nghostzones[1] + cntstag[1] * 0.5) * this_dy;
    } 
    else 
    {
      y_origin = - 0.5 * (cctk_gsh[1]-1 - cntstag[1] * (cctk_gsh[1])%2)
        * this_dy;
    }
    
    if (domainsym[4]) 
    {
      z_origin = (- cctk_nghostzones[2] + cntstag[2] * 0.5) * this_dz;
    } 
    else 
    {
      z_origin = - 0.5 * (cctk_gsh[2]-1 - cntstag[2] * (cctk_gsh[2])%2)
       * this_dz;
    }
  }

  /* Set spatial coordinates */

  for(i=0; i < cctk_lsh[0]; i++)
  {
    for(j=0; j < cctk_lsh[1]; j++)
    {
      for(k=0; k < cctk_lsh[2]; k++)
      {
        x[CCTK_GFINDEX3D(cctkGH,i,j,k)] = this_dx*(i+cctk_lbnd[0]) + x_origin;
        y[CCTK_GFINDEX3D(cctkGH,i,j,k)] = this_dy*(j+cctk_lbnd[1]) + y_origin;
        z[CCTK_GFINDEX3D(cctkGH,i,j,k)] = this_dz*(k+cctk_lbnd[2]) + z_origin;
        r[CCTK_GFINDEX3D(cctkGH,i,j,k)] = sqrt(SQR(x[CCTK_GFINDEX3D(cctkGH,i,j,k)])+
                                               SQR(y[CCTK_GFINDEX3D(cctkGH,i,j,k)])+
                                               SQR(z[CCTK_GFINDEX3D(cctkGH,i,j,k)]));
      }
    }
  }
  
  cctk_delta_space[0] = this_dx;
  cctk_delta_space[1] = this_dy;
  cctk_delta_space[2] = this_dz;
  
  cctk_origin_space[0] = x_origin;
  cctk_origin_space[1] = y_origin;
  cctk_origin_space[2] = z_origin;
  
  lowerx = x_origin;
  upperx = x_origin+this_dx*(cctk_gsh[0]-1); 
  CCTK_CoordRegisterRange(cctkGH,lowerx,upperx,-1,"x","cart3d");
  CCTK_CoordRegisterRangePhysIndex(cctkGH,lowerxi,upperxi,-1,"x","cart3d");

  lowery = y_origin;
  uppery = y_origin+this_dy*(cctk_gsh[1]-1); 
  CCTK_CoordRegisterRange(cctkGH,lowery,uppery,-1,"y","cart3d");
  CCTK_CoordRegisterRangePhysIndex(cctkGH,loweryi,upperyi,-1,"y","cart3d");

  lowerz = z_origin;
  upperz = z_origin+this_dz*(cctk_gsh[2]-1); 
  CCTK_CoordRegisterRange(cctkGH,lowerz,upperz,-1,"z","cart3d");
  CCTK_CoordRegisterRangePhysIndex(cctkGH,lowerzi,upperzi,-1,"z","cart3d");
  
  CCTK_INFO("Grid Spacings:");
  sprintf(infoline," %s%12.7e  %s%12.7e  %s%12.7e  ",  
                   "dx=>",cctk_delta_space[0],
                   "dy=>",cctk_delta_space[1],
                   "dz=>",cctk_delta_space[2]);
  CCTK_INFO(infoline);
  
  CCTK_INFO("Computational Coordinates:");
  sprintf(infoline," %s[%6.3f,%6.3f]  %s[%6.3f,%6.3f]  %s[%6.3f,%6.3f]  ", 
                   "x=>",lowerx,upperx,
                   "y=>",lowery,uppery,
                   "z=>",lowerz,upperz);
  CCTK_INFO(infoline);

  CCTK_INFO("Indices of Physical Coordinates:");
  sprintf(infoline," %s[%d,%d]  %s[%d,%d]  %s[%d,%d]  ", 
                   "x=>",lowerxi,upperxi,
 	           "y=>",loweryi,upperyi,
	           "z=>",lowerzi,upperzi);
  CCTK_INFO(infoline);


#ifdef CCTK_DEBUG
  printf("\n");
  printf("CartGrid3D\n");
  printf(" ----------\n");
  printf("Dx,Dy,Dz on coarse grid %6.3f %6.3f, %6.3f\n", *coarse_dx,*coarse_dy,*coarse_dz);
  printf("Dx,Dy,Dz on this grid %6.3f %6.3f, %6.3f\n", cctk_delta_space[0],cctk_delta_space[1],cctk_delta_space[2]);
  printf(" Convergence level = %d\n",cctk_convlevel);
  printf("  Minimum global coords %6.3f %6.3f %6.3f\n", x_origin,y_origin,z_origin);
  printf("  Maximum global coords %6.3f %6.3f %6.3f\n", x_origin+this_dx*(cctk_gsh[0]-1),
                                                        y_origin+this_dy*(cctk_gsh[1]-1),
                                                        z_origin+this_dz*(cctk_gsh[2]-1)); 
  printf("  Minimum local coords %6.3f %6.3f %6.3f\n", x[0],y[0],z[0]);
  printf("  Maximum local coords %6.3f %6.3f %6.3f\n", x[CCTK_GFINDEX3D(cctkGH, cctk_lsh[0]-1,cctk_lsh[1]-1,cctk_lsh[2]-1)],
                                                       y[CCTK_GFINDEX3D(cctkGH, cctk_lsh[0]-1,cctk_lsh[1]-1,cctk_lsh[2]-1)],
                                                       z[CCTK_GFINDEX3D(cctkGH, cctk_lsh[0]-1,cctk_lsh[1]-1,cctk_lsh[2]-1)]);

#endif

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;  

}
