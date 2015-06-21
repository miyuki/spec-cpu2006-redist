#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
/*@@
   @file      ConfPhys.c
   @date      September 3rd 1999
   @author    Gabrielle Allen
   @desc
              Conversions between physical and conformal metrics.
              Be very careful using these functions, note that
              conformal_state is not changed, this is up to the
              calling routine
   @enddesc
 @@*/
                    

#include "cctk.h"
#include "cctk_FortranString.h"

#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/ConfPhys.c,v 1.7 2002/01/04 10:00:01 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_ConfPhys_c)

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
void CCTK_FCALL phystoconf_ (int *nx,
                                         int *ny,
                                         int *nz,
                                         CCTK_REAL *psi,
                                         CCTK_REAL *gxx,
                                         CCTK_REAL *gxy,
                                         CCTK_REAL *gxz,
                                         CCTK_REAL *gyy,
                                         CCTK_REAL *gyz,
                                         CCTK_REAL *gzz);
void CCTK_FCALL conftophys_ (int *nx,
                                         int *ny,
                                         int *nz,
                                         CCTK_REAL *psi,
                                         CCTK_REAL *gxx,
                                         CCTK_REAL *gxy,
                                         CCTK_REAL *gxz,
                                         CCTK_REAL *gyy,
                                         CCTK_REAL *gyz,
                                         CCTK_REAL *gzz);


void ConfToPhys (int nx,
                 int ny,
                 int nz,
                 CCTK_REAL *psi,
                 CCTK_REAL *gxx,
                 CCTK_REAL *gxy,
                 CCTK_REAL *gxz,
                 CCTK_REAL *gyy,
                 CCTK_REAL *gyz,
                 CCTK_REAL *gzz)
{
  CCTK_REAL psi4;
  int index;


  CCTK_WARN (4, "Converting metric: conformal -> physical");

  index = nx * ny * nz;
  while (--index >= 0)
  {
    /* this should be faster than psi4 = pow (psi[index], 4) */
    psi4 = psi[index] * psi[index];
    psi4 = psi4 * psi4;

    gxx[index] *= psi4;
    gxy[index] *= psi4;
    gxz[index] *= psi4;
    gyy[index] *= psi4;
    gyz[index] *= psi4;
    gzz[index] *= psi4;
  }
}


void CCTK_FCALL conftophys_ (int *nx,
                                         int *ny,
                                         int *nz,
                                         CCTK_REAL *psi,
                                         CCTK_REAL *gxx,
                                         CCTK_REAL *gxy,
                                         CCTK_REAL *gxz,
                                         CCTK_REAL *gyy,
                                         CCTK_REAL *gyz,
                                         CCTK_REAL *gzz)
{
  ConfToPhys (*nx, *ny, *nz, psi, gxx, gxy, gxz, gyy, gyz, gzz);
}


void PhysToConf (int nx,
                 int ny,
                 int nz,
                 CCTK_REAL *psi, 
                 CCTK_REAL *gxx,
                 CCTK_REAL *gxy,
                 CCTK_REAL *gxz,
                 CCTK_REAL *gyy,
                 CCTK_REAL *gyz,
                 CCTK_REAL *gzz)
{
  int index;
  CCTK_REAL psi4;


  CCTK_WARN (4, "Converting metric: physical -> conformal");

  index = nx * ny * nz;
  while (--index >= 0)
  {
    /* this should be faster than psi4 = pow (psi[index], 4) */
    psi4 = psi[index] * psi[index];
    psi4 = psi4 * psi4;

    /* get the reciprocal for turning divisions into multiplications */
    psi4 = 1.0 / psi4;

    gxx[index] *= psi4;
    gxy[index] *= psi4;
    gxz[index] *= psi4;
    gyy[index] *= psi4;
    gyz[index] *= psi4;
    gzz[index] *= psi4;
  }
}

void CCTK_FCALL phystoconf_ (int *nx,
                                         int *ny,
                                         int *nz,
                                         CCTK_REAL *psi,
                                         CCTK_REAL *gxx,
                                         CCTK_REAL *gxy,
                                         CCTK_REAL *gxz,
                                         CCTK_REAL *gyy,
                                         CCTK_REAL *gyz,
                                         CCTK_REAL *gzz)
{
  PhysToConf (*nx, *ny, *nz, psi, gxx, gxy, gxz, gyy, gyz, gzz);
}
