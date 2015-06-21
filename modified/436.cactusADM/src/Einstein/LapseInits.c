#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
 /*@@
   @file      LapseInits.c
   @date      
   @author    Gabrielle Allen
   @desc 
   Initialise the lapse function 
   @enddesc 
 @@*/

#include <math.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/LapseInits.c,v 1.12 2002/01/04 10:18:17 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_LapseInits_c)

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
void LapseOne(CCTK_ARGUMENTS);
void LapseGaussian(CCTK_ARGUMENTS);
void LapsePsiMinusTwo(CCTK_ARGUMENTS);
void LapseIsotropic(CCTK_ARGUMENTS);

void LapseOne(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS

  int index,i,j,k,nx,ny,nz;

  nx = cctk_lsh[0];
  ny = cctk_lsh[1];
  nz = cctk_lsh[2];
  
  for(k=0; k < nz; k++)
  {
    for(j=0; j < ny; j++)
    {
      for(i=0; i < nx; i++)
      {

        index = CCTK_GFINDEX3D(cctkGH, i,j,k);
        alp[index] = 1.0;

      }
    }
  }

  USE_CCTK_CARGUMENTS   return;

}


void LapseGaussian(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int index,i,j,k,nx,ny,nz;

  nx = cctk_lsh[0];
  ny = cctk_lsh[1];
  nz = cctk_lsh[2];
  
  for(k=0; k < nz; k++)
  {
    for(j=0; j < ny; j++)
    {
      for(i=0; i < nx; i++)
      {
        index = CCTK_GFINDEX3D(cctkGH, i,j,k);
        alp[index] = 1.0 + gaussian_amplitude*
	  exp(-r[index]*r[index]/gaussian_sigma2);
      }
    }
  }

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;

}



void LapsePsiMinusTwo(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int index,i,j,k,nx,ny,nz;

  CCTK_REAL scut;

  nx = cctk_lsh[0];
  ny = cctk_lsh[1];
  nz = cctk_lsh[2];

  /*
  Here we use the following expression for the
  initial lapse:

  alp = ((1 - 2s + s*psi)/(psi-s)))^2

  where psi is the conformal factor, s is a cutoff
  value between 0 and 1 (to stop the lapse from becoming
  too small).

  Notice that if s=0, then alp=1/psi^2, while if
  s=1 then alp=1. 
  */

  scut = sqrt(psiminustwo_cut);

  for(k=0; k < nz; k++)
  {
    for(j=0; j < ny; j++)
    {
      for(i=0; i < nx; i++)
      {
        index = CCTK_GFINDEX3D(cctkGH,i,j,k);
        alp[index] = pow((1.0 - 2.0*scut + scut*psi[index])
                   / (psi[index]-scut),2);
      }
    }
  }

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;

}



void LapseIsotropic(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int index,i,j,k,nx,ny,nz;

  nx = cctk_lsh[0];
  ny = cctk_lsh[1];
  nz = cctk_lsh[2];

  /*
  The isotropic lapse has the form:

  alp = 2/psi - 1

  with psi the conformal factor.

  This is exact for one black hole in
  isotropic coordinates.
  */

  for(k=0; k < nz; k++)
  {
    for(j=0; j < ny; j++)
    {
      for(i=0; i < nx; i++)
      {
        index = CCTK_GFINDEX3D(cctkGH,i,j,k);
        alp[index] = 2.0/psi[index] - 1.0;
      }
    }
  }

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;

}
