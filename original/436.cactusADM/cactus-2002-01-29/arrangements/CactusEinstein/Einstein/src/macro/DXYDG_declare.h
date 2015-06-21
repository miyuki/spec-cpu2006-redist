/*@@
  @header   DXYDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declaration for macro to calculate the (first and) second derivatives 
  of the physical metric with respect to x,y

  The macro uses @seefile DXDG_guts.h , @seefile DYDG_guts.h and 
  @seefile DXDG_declare.h , @seefile DYDG_declare.h
  @enddesc
@@*/

#ifndef DXYDG_DECLARE
#define DXYDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DYDG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DXYDG_GXX_IPJP 
#define DXYDG_GXX_IPJP gxx(i+1,j+1,k)
#undef  DXYDG_GXX_IPJM 
#define DXYDG_GXX_IPJM gxx(i+1,j-1,k)
#undef  DXYDG_GXX_IMJP 
#define DXYDG_GXX_IMJP gxx(i-1,j+1,k)
#undef  DXYDG_GXX_IMJM 
#define DXYDG_GXX_IMJM gxx(i-1,j-1,k)

#undef  DXYDG_GXY_IPJP 
#define DXYDG_GXY_IPJP gxy(i+1,j+1,k)
#undef  DXYDG_GXY_IPJM 
#define DXYDG_GXY_IPJM gxy(i+1,j-1,k)
#undef  DXYDG_GXY_IMJP 
#define DXYDG_GXY_IMJP gxy(i-1,j+1,k)
#undef  DXYDG_GXY_IMJM 
#define DXYDG_GXY_IMJM gxy(i-1,j-1,k)

#undef  DXYDG_GXZ_IPJP 
#define DXYDG_GXZ_IPJP gxz(i+1,j+1,k)
#undef  DXYDG_GXZ_IPJM 
#define DXYDG_GXZ_IPJM gxz(i+1,j-1,k)
#undef  DXYDG_GXZ_IMJP 
#define DXYDG_GXZ_IMJP gxz(i-1,j+1,k)
#undef  DXYDG_GXZ_IMJM 
#define DXYDG_GXZ_IMJM gxz(i-1,j-1,k)

#undef  DXYDG_GYY_IPJP 
#define DXYDG_GYY_IPJP gyy(i+1,j+1,k)
#undef  DXYDG_GYY_IPJM 
#define DXYDG_GYY_IPJM gyy(i+1,j-1,k)
#undef  DXYDG_GYY_IMJP 
#define DXYDG_GYY_IMJP gyy(i-1,j+1,k)
#undef  DXYDG_GYY_IMJM 
#define DXYDG_GYY_IMJM gyy(i-1,j-1,k)

#undef  DXYDG_GYZ_IPJP 
#define DXYDG_GYZ_IPJP gyz(i+1,j+1,k)
#undef  DXYDG_GYZ_IPJM 
#define DXYDG_GYZ_IPJM gyz(i+1,j-1,k)
#undef  DXYDG_GYZ_IMJP 
#define DXYDG_GYZ_IMJP gyz(i-1,j+1,k)
#undef  DXYDG_GYZ_IMJM 
#define DXYDG_GYZ_IMJM gyz(i-1,j-1,k)

#undef  DXYDG_GZZ_IPJP 
#define DXYDG_GZZ_IPJP gzz(i+1,j+1,k)
#undef  DXYDG_GZZ_IPJM 
#define DXYDG_GZZ_IPJM gzz(i+1,j-1,k)
#undef  DXYDG_GZZ_IMJP 
#define DXYDG_GZZ_IMJP gzz(i-1,j+1,k)
#undef  DXYDG_GZZ_IMJM 
#define DXYDG_GZZ_IMJM gzz(i-1,j-1,k)

/* Output variables */
#undef  DXYDG_DXYDGXX
#define DXYDG_DXYDGXX deldelg1211
#undef  DXYDG_DXYDGXY
#define DXYDG_DXYDGXY deldelg1212
#undef  DXYDG_DXYDGXZ
#define DXYDG_DXYDGXZ deldelg1213
#undef  DXYDG_DXYDGYY
#define DXYDG_DXYDGYY deldelg1222
#undef  DXYDG_DXYDGYZ
#define DXYDG_DXYDGYZ deldelg1223
#undef  DXYDG_DXYDGZZ
#define DXYDG_DXYDGZZ deldelg1233

/* Internal variables */
#undef  DXYDG_FAC 
#define DXYDG_FAC dxydg_fac
#undef  DXYDG_OO4DXDY
#define DXYDG_OO4DXDY dxydg_oo4dxdy
#undef  DXYDG_DXYDPSI_O_PSI
#define DXYDG_DXYDPSI_O_PSI psixy(i,j,k)

/* Declare internal variables */
      CCTK_REAL DXYDG_FAC
      CCTK_REAL DXYDG_OO4DXDY 

/* Declare output variables */
      CCTK_REAL DXYDG_DXYDGXX
      CCTK_REAL DXYDG_DXYDGXY
      CCTK_REAL DXYDG_DXYDGXZ
      CCTK_REAL DXYDG_DXYDGYY
      CCTK_REAL DXYDG_DXYDGYZ
      CCTK_REAL DXYDG_DXYDGZZ

#endif

#ifdef CCODE

/* Input variables */
#undef  DXYDG_GXX_IPJP 
#define DXYDG_GXX_IPJP gxx[ di + dj + ijk]
#undef  DXYDG_GXX_IPJM 
#define DXYDG_GXX_IPJM gxx[ di - dj + ijk]
#undef  DXYDG_GXX_IMJP 
#define DXYDG_GXX_IMJP gxx[-di + dj + ijk]
#undef  DXYDG_GXX_IMJM 
#define DXYDG_GXX_IMJM gxx[-di - dj + ijk]

#undef  DXYDG_GXY_IPJP 
#define DXYDG_GXY_IPJP gxy[ di + dj + ijk]
#undef  DXYDG_GXY_IPJM 
#define DXYDG_GXY_IPJM gxy[ di - dj + ijk]
#undef  DXYDG_GXY_IMJP 
#define DXYDG_GXY_IMJP gxy[-di + dj + ijk]
#undef  DXYDG_GXY_IMJM 
#define DXYDG_GXY_IMJM gxy[-di - dj + ijk]

#undef  DXYDG_GXZ_IPJP 
#define DXYDG_GXZ_IPJP gxz[ di + dj + ijk]
#undef  DXYDG_GXZ_IPJM 
#define DXYDG_GXZ_IPJM gxz[ di - dj + ijk]
#undef  DXYDG_GXZ_IMJP 
#define DXYDG_GXZ_IMJP gxz[-di + dj + ijk]
#undef  DXYDG_GXZ_IMJM 
#define DXYDG_GXZ_IMJM gxz[-di - dj + ijk]

#undef  DXYDG_GYY_IPJP 
#define DXYDG_GYY_IPJP gyy[ di + dj + ijk]
#undef  DXYDG_GYY_IPJM 
#define DXYDG_GYY_IPJM gyy[ di - dj + ijk]
#undef  DXYDG_GYY_IMJP 
#define DXYDG_GYY_IMJP gyy[-di + dj + ijk]
#undef  DXYDG_GYY_IMJM 
#define DXYDG_GYY_IMJM gyy[-di - dj + ijk]

#undef  DXYDG_GYZ_IPJP 
#define DXYDG_GYZ_IPJP gyz[ di + dj + ijk]
#undef  DXYDG_GYZ_IPJM 
#define DXYDG_GYZ_IPJM gyz[ di - dj + ijk]
#undef  DXYDG_GYZ_IMJP 
#define DXYDG_GYZ_IMJP gyz[-di + dj + ijk]
#undef  DXYDG_GYZ_IMJM 
#define DXYDG_GYZ_IMJM gyz[-di - dj + ijk]

#undef  DXYDG_GZZ_IPJP 
#define DXYDG_GZZ_IPJP gzz[ di + dj + ijk]
#undef  DXYDG_GZZ_IPJM 
#define DXYDG_GZZ_IPJM gzz[ di - dj + ijk]
#undef  DXYDG_GZZ_IMJP 
#define DXYDG_GZZ_IMJP gzz[-di + dj + ijk]
#undef  DXYDG_GZZ_IMJM 
#define DXYDG_GZZ_IMJM gzz[-di - dj + ijk]

/* Output variables */
#undef  DXYDG_DXYDGXX
#define DXYDG_DXYDGXX deldelg1211
#undef  DXYDG_DXYDGXY
#define DXYDG_DXYDGXY deldelg1212
#undef  DXYDG_DXYDGXZ
#define DXYDG_DXYDGXZ deldelg1213
#undef  DXYDG_DXYDGYY
#define DXYDG_DXYDGYY deldelg1222
#undef  DXYDG_DXYDGYZ
#define DXYDG_DXYDGYZ deldelg1223
#undef  DXYDG_DXYDGZZ
#define DXYDG_DXYDGZZ deldelg1233

/* Internal variables */
#undef  DXYDG_FAC 
#define DXYDG_FAC dxydg_fac
#undef  DXYDG_OO4DXDY
#define DXYDG_OO4DXDY dxydg_oo4dxdy
#undef  DXYDG_DXYDPSI_O_PSI
#define DXYDG_DXYDPSI_O_PSI psixy[ijk]

/* Declare internal variables */
double DXYDG_FAC;
double DXYDG_OO4DXDY = 1/(4*cctkGH->cctk_delta_space[0]*cctkGH->cctk_delta_space[1]);

/* Declare output variables */
double DXYDG_DXYDGXX;
double DXYDG_DXYDGXY;
double DXYDG_DXYDGXZ;
double DXYDG_DXYDGYY;
double DXYDG_DXYDGYZ;
double DXYDG_DXYDGZZ;

#endif

#endif
