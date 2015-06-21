/*@@
  @header   CDZCDK_declare.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to compute first conformal
  derivatives of the extrinsic curvature with respect to z
  @enddesc
@@*/

#ifndef CDZCDK_DECLARE
#define CDZCDK_DECLARE

#include "CactusEinstein/Einstein/src/macro/DZDK_declare.h"
#include "CactusEinstein/Einstein/src/macro/CHR2_declare.h"

#ifdef FCODE

/* Input variables */
#undef  CDZCDK_KXX 
#define CDZCDK_KXX kxx(i,j,k)
#undef  CDZCDK_KXY 
#define CDZCDK_KXY kxy(i,j,k)
#undef  CDZCDK_KXZ 
#define CDZCDK_KXZ kxz(i,j,k)
#undef  CDZCDK_KYY 
#define CDZCDK_KYY kyy(i,j,k)
#undef  CDZCDK_KYZ 
#define CDZCDK_KYZ kyz(i,j,k)
#undef  CDZCDK_KZZ 
#define CDZCDK_KZZ kzz(i,j,k)

/* Output variables */ 
#undef  CDZCDK_CDZCDKXX
#define CDZCDK_CDZCDKXX  cdzcdk_cdzcdkxx
#undef  CDZCDK_CDZCDKXY
#define CDZCDK_CDZCDKXY  cdzcdk_cdzcdkxy
#undef  CDZCDK_CDZCDKXZ
#define CDZCDK_CDZCDKXZ  cdzcdk_cdzcdkxz
#undef  CDZCDK_CDZCDKYY
#define CDZCDK_CDZCDKYY  cdzcdk_cdzcdkyy
#undef  CDZCDK_CDZCDKYZ
#define CDZCDK_CDZCDKYZ  cdzcdk_cdzcdkyz
#undef  CDZCDK_CDZCDKZZ
#define CDZCDK_CDZCDKZZ  cdzcdk_cdzcdkzz

/* Declare output variables */
      CCTK_REAL CDZCDK_CDZCDKXX
      CCTK_REAL CDZCDK_CDZCDKXY
      CCTK_REAL CDZCDK_CDZCDKXZ
      CCTK_REAL CDZCDK_CDZCDKYY
      CCTK_REAL CDZCDK_CDZCDKYZ
      CCTK_REAL CDZCDK_CDZCDKZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  CDZCDK_KXX 
#define CDZCDK_KXX kxx[ijk]
#undef  CDZCDK_KXY 
#define CDZCDK_KXY kxy[ijk]
#undef  CDZCDK_KXZ 
#define CDZCDK_KXZ kxz[ijk]
#undef  CDZCDK_KYY 
#define CDZCDK_KYY kyy[ijk]
#undef  CDZCDK_KYZ 
#define CDZCDK_KYZ kyz[ijk]
#undef  CDZCDK_KZZ 
#define CDZCDK_KZZ kzz[ijk]

/* Output variables */ 
#undef  CDZCDK_CDZCDKXX
#define CDZCDK_CDZCDKXX  cdzcdk_cdzcdkxx
#undef  CDZCDK_CDZCDKXY
#define CDZCDK_CDZCDKXY  cdzcdk_cdzcdkxy
#undef  CDZCDK_CDZCDKXZ
#define CDZCDK_CDZCDKXZ  cdzcdk_cdzcdkxz
#undef  CDZCDK_CDZCDKYY
#define CDZCDK_CDZCDKYY  cdzcdk_cdzcdkyy
#undef  CDZCDK_CDZCDKYZ
#define CDZCDK_CDZCDKYZ  cdzcdk_cdzcdkyz
#undef  CDZCDK_CDZCDKZZ
#define CDZCDK_CDZCDKZZ  cdzcdk_cdzcdkzz

/* Declare output variables */
double CDZCDK_CDZCDKXX;
double CDZCDK_CDZCDKXY;
double CDZCDK_CDZCDKXZ;
double CDZCDK_CDZCDKYY;
double CDZCDK_CDZCDKYZ;
double CDZCDK_CDZCDKZZ;

#endif

#endif
