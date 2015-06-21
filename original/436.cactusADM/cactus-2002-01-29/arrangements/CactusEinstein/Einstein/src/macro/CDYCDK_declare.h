/*@@
  @header   CDYCDK_declare.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to compute first conformal
  derivatives of the extrinsic curvature with respect to y
  @enddesc
@@*/

#ifndef CDYCDK_DECLARE
#define CDYCDK_DECLARE

#include "CactusEinstein/Einstein/src/macro/DYDK_declare.h"
#include "CactusEinstein/Einstein/src/macro/CHR2_declare.h"

#ifdef FCODE

/* Input variables */
#undef  CDYCDK_KXX 
#define CDYCDK_KXX kxx(i,j,k)
#undef  CDYCDK_KXY 
#define CDYCDK_KXY kxy(i,j,k)
#undef  CDYCDK_KXZ 
#define CDYCDK_KXZ kxz(i,j,k)
#undef  CDYCDK_KYY 
#define CDYCDK_KYY kyy(i,j,k)
#undef  CDYCDK_KYZ 
#define CDYCDK_KYZ kyz(i,j,k)
#undef  CDYCDK_KZZ 
#define CDYCDK_KZZ kzz(i,j,k)

/* Output variables */ 
#undef  CDYCDK_CDYCDKXX
#define CDYCDK_CDYCDKXX  cdycdk_cdycdkxx
#undef  CDYCDK_CDYCDKXY
#define CDYCDK_CDYCDKXY  cdycdk_cdycdkxy
#undef  CDYCDK_CDYCDKXZ
#define CDYCDK_CDYCDKXZ  cdycdk_cdycdkxz
#undef  CDYCDK_CDYCDKYY
#define CDYCDK_CDYCDKYY  cdycdk_cdycdkyy
#undef  CDYCDK_CDYCDKYZ
#define CDYCDK_CDYCDKYZ  cdycdk_cdycdkyz
#undef  CDYCDK_CDYCDKZZ
#define CDYCDK_CDYCDKZZ  cdycdk_cdycdkzz

/* Declare output variables */
      CCTK_REAL CDYCDK_CDYCDKXX
      CCTK_REAL CDYCDK_CDYCDKXY
      CCTK_REAL CDYCDK_CDYCDKXZ
      CCTK_REAL CDYCDK_CDYCDKYY
      CCTK_REAL CDYCDK_CDYCDKYZ
      CCTK_REAL CDYCDK_CDYCDKZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  CDYCDK_KXX 
#define CDYCDK_KXX kxx[ijk]
#undef  CDYCDK_KXY 
#define CDYCDK_KXY kxy[ijk]
#undef  CDYCDK_KXZ 
#define CDYCDK_KXZ kxz[ijk]
#undef  CDYCDK_KYY 
#define CDYCDK_KYY kyy[ijk]
#undef  CDYCDK_KYZ 
#define CDYCDK_KYZ kyz[ijk]
#undef  CDYCDK_KZZ 
#define CDYCDK_KZZ kzz[ijk]

/* Output variables */ 
#undef  CDYCDK_CDYCDKXX
#define CDYCDK_CDYCDKXX  cdycdk_cdycdkxx
#undef  CDYCDK_CDYCDKXY
#define CDYCDK_CDYCDKXY  cdycdk_cdycdkxy
#undef  CDYCDK_CDYCDKXZ
#define CDYCDK_CDYCDKXZ  cdycdk_cdycdkxz
#undef  CDYCDK_CDYCDKYY
#define CDYCDK_CDYCDKYY  cdycdk_cdycdkyy
#undef  CDYCDK_CDYCDKYZ
#define CDYCDK_CDYCDKYZ  cdycdk_cdycdkyz
#undef  CDYCDK_CDYCDKZZ
#define CDYCDK_CDYCDKZZ  cdycdk_cdycdkzz

/* Declare output variables */
double CDYCDK_CDYCDKXX;
double CDYCDK_CDYCDKXY;
double CDYCDK_CDYCDKXZ;
double CDYCDK_CDYCDKYY;
double CDYCDK_CDYCDKYZ;
double CDYCDK_CDYCDKZZ;

#endif

#endif
