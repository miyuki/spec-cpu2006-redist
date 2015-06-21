/*@@
  @header   DZDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DZDG_guts.h to compute first 
  derivatives of the physical metric with respect to y
  @enddesc
@@*/

#ifndef DZDG_DECLARE
#define DZDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DZDCG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DZDG_PSI
#define DZDG_PSI psi(i,j,k)

#undef  DZDG_DZDPSI_O_PSI
#define DZDG_DZDPSI_O_PSI psiz(i,j,k)

#undef  DZDG_GXX    
#define DZDG_GXX    gxx(i,j,k)
#undef  DZDG_GXY   
#define DZDG_GXY    gxy(i,j,k)
#undef  DZDG_GXZ   
#define DZDG_GXZ    gxz(i,j,k)
#undef  DZDG_GYY    
#define DZDG_GYY    gyy(i,j,k)
#undef  DZDG_GYZ   
#define DZDG_GYZ    gyz(i,j,k)
#undef  DZDG_GZZ   
#define DZDG_GZZ    gzz(i,j,k)

/* Output variables */ 
#undef  DZDG_DZDGXX
#define DZDG_DZDGXX  dzdgxx
#undef  DZDG_DZDGXY
#define DZDG_DZDGXY  dzdgxy
#undef  DZDG_DZDGXZ
#define DZDG_DZDGXZ  dzdgxz
#undef  DZDG_DZDGYY
#define DZDG_DZDGYY  dzdgyy
#undef  DZDG_DZDGYZ
#define DZDG_DZDGYZ  dzdgyz
#undef  DZDG_DZDGZZ
#define DZDG_DZDGZZ  dzdgzz

/* Internal variables */
#undef  DZDG_PSI4
#define DZDG_PSI4  dzdg_psi4

#undef  DZDG_FAC
#define DZDG_FAC   dzdg_fac

/* Declare internal variables */
      CCTK_REAL DZDG_PSI4;
      CCTK_REAL DZDG_FAC;

/* Declare output variables */
      CCTK_REAL DZDG_DZDGXX;
      CCTK_REAL DZDG_DZDGXY;
      CCTK_REAL DZDG_DZDGXZ;
      CCTK_REAL DZDG_DZDGYY;
      CCTK_REAL DZDG_DZDGYZ;
      CCTK_REAL DZDG_DZDGZZ;

#endif


#ifdef CCODE

/* Output variables */ 
#undef  DZDG_DZDGXX
#define DZDG_DZDGXX  delg311
#undef  DZDG_DZDGXY
#define DZDG_DZDGXY  delg312
#undef  DZDG_DZDGXZ
#define DZDG_DZDGXZ  delg313
#undef  DZDG_DZDGYY
#define DZDG_DZDGYY  delg322
#undef  DZDG_DZDGYZ
#define DZDG_DZDGYZ  delg323
#undef  DZDG_DZDGZZ
#define DZDG_DZDGZZ  delg333

/* Input variables */

#undef  DZDG_PSI
#define DZDG_PSI psi[ijk]

#undef  DZDG_DZDPSI_O_PSI
#define DZDG_DZDPSI_O_PSI psiz[ijk]

#undef  DZDG_GXX    
#define DZDG_GXX    gxx[ijk]
#undef  DZDG_GXY   
#define DZDG_GXY    gxy[ijk]
#undef  DZDG_GXZ   
#define DZDG_GXZ    gxz[ijk]
#undef  DZDG_GYY    
#define DZDG_GYY    gyy[ijk]
#undef  DZDG_GYZ   
#define DZDG_GYZ    gyz[ijk]
#undef  DZDG_GZZ   
#define DZDG_GZZ    gzz[ijk]

/* Internal variables */
#undef  DZDG_PSI4
#define DZDG_PSI4  dzdg_psi4

#undef  DZDG_FAC
#define DZDG_FAC   dzdg_fac

/* Declare internal variables */
double DZDG_PSI4;
double DZDG_FAC;

/* Declare output variables */
double DZDG_DZDGXX;
double DZDG_DZDGXY;
double DZDG_DZDGXZ;
double DZDG_DZDGYY;
double DZDG_DZDGYZ;
double DZDG_DZDGZZ;

#endif

#endif
