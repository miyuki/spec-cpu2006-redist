/*@@
  @header   DYDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DYDG_guts.h to compute first 
  derivatives of the physical metric with respect to y
  @enddesc
@@*/

#ifndef DYDG_DECLARE
#define DYDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DYDCG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DYDG_PSI
#define DYDG_PSI psi(i,j,k)

#undef  DYDG_DYDPSI_O_PSI
#define DYDG_DYDPSI_O_PSI psiy(i,j,k)

#undef  DYDG_GXX    
#define DYDG_GXX    gxx(i,j,k)
#undef  DYDG_GXY   
#define DYDG_GXY    gxy(i,j,k)
#undef  DYDG_GXZ   
#define DYDG_GXZ    gxz(i,j,k)
#undef  DYDG_GYY    
#define DYDG_GYY    gyy(i,j,k)
#undef  DYDG_GYZ   
#define DYDG_GYZ    gyz(i,j,k)
#undef  DYDG_GZZ   
#define DYDG_GZZ    gzz(i,j,k)

/* Output variables */ 
#undef  DYDG_DYDGXX
#define DYDG_DYDGXX  dydgxx
#undef  DYDG_DYDGXY
#define DYDG_DYDGXY  dydgxy
#undef  DYDG_DYDGXZ
#define DYDG_DYDGXZ  dydgxz
#undef  DYDG_DYDGYY
#define DYDG_DYDGYY  dydgyy
#undef  DYDG_DYDGYZ
#define DYDG_DYDGYZ  dydgyz
#undef  DYDG_DYDGZZ
#define DYDG_DYDGZZ  dydgzz

/* Internal variables */
#undef  DYDG_PSI4
#define DYDG_PSI4  dydg_psi4

#undef  DYDG_FAC
#define DYDG_FAC   dydg_fac

/* Declare internal variables */
      CCTK_REAL DYDG_PSI4;
      CCTK_REAL DYDG_FAC;

/* Declare output variables */
      CCTK_REAL DYDG_DYDGXX;
      CCTK_REAL DYDG_DYDGXY;
      CCTK_REAL DYDG_DYDGXZ;
      CCTK_REAL DYDG_DYDGYY;
      CCTK_REAL DYDG_DYDGYZ;
      CCTK_REAL DYDG_DYDGZZ;

#endif

#ifdef CCODE

/* Output variables */ 
#undef  DYDG_DYDGXX
#define DYDG_DYDGXX  delg211
#undef  DYDG_DYDGXY
#define DYDG_DYDGXY  delg212
#undef  DYDG_DYDGXZ
#define DYDG_DYDGXZ  delg213
#undef  DYDG_DYDGYY
#define DYDG_DYDGYY  delg222
#undef  DYDG_DYDGYZ
#define DYDG_DYDGYZ  delg223
#undef  DYDG_DYDGZZ
#define DYDG_DYDGZZ  delg233

/* Input variables */

#undef  DYDG_PSI
#define DYDG_PSI psi[ijk]

#undef  DYDG_DYDPSI_O_PSI
#define DYDG_DYDPSI_O_PSI psiy[ijk]

#undef  DYDG_GXX    
#define DYDG_GXX    gxx[ijk]
#undef  DYDG_GXY   
#define DYDG_GXY    gxy[ijk]
#undef  DYDG_GXZ   
#define DYDG_GXZ    gxz[ijk]
#undef  DYDG_GYY    
#define DYDG_GYY    gyy[ijk]
#undef  DYDG_GYZ   
#define DYDG_GYZ    gyz[ijk]
#undef  DYDG_GZZ   
#define DYDG_GZZ    gzz[ijk]

/* Internal variables */
#undef  DYDG_PSI4
#define DYDG_PSI4  dydg_psi4

#undef  DYDG_FAC
#define DYDG_FAC   dydg_fac

/* Declare internal variables */
double DYDG_PSI4;
double DYDG_FAC;

/* Declare output variables */
double DYDG_DYDGXX;
double DYDG_DYDGXY;
double DYDG_DYDGXZ;
double DYDG_DYDGYY;
double DYDG_DYDGYZ;
double DYDG_DYDGZZ;

#endif

#endif
