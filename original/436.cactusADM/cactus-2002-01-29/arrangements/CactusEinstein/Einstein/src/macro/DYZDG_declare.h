/*@@
  @header   DYZDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declaration for macro to calculate the (first and) second derivatives 
  of the physical metric with respect to x,z

  The macro uses @seefile DYDG_guts.h , @seefile DZDG_guts.h and 
  @seefile DYDG_declare.h , @seefile DZDG_declare.h
  @enddesc
@@*/

#ifndef DYZDG_DECLARE
#define DYZDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DYDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DZDG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DYZDG_GXX_JPKP 
#define DYZDG_GXX_JPKP gxx(i,j+1,k+1)
#undef  DYZDG_GXX_JPKM 
#define DYZDG_GXX_JPKM gxx(i,j+1,k-1)
#undef  DYZDG_GXX_JMKP 
#define DYZDG_GXX_JMKP gxx(i,j-1,k+1)
#undef  DYZDG_GXX_JMKM 
#define DYZDG_GXX_JMKM gxx(i,j-1,k-1)

#undef  DYZDG_GXY_JPKP 
#define DYZDG_GXY_JPKP gxy(i,j+1,k+1)
#undef  DYZDG_GXY_JPKM 
#define DYZDG_GXY_JPKM gxy(i,j+1,k-1)
#undef  DYZDG_GXY_JMKP 
#define DYZDG_GXY_JMKP gxy(i,j-1,k+1)
#undef  DYZDG_GXY_JMKM 
#define DYZDG_GXY_JMKM gxy(i,j-1,k-1)

#undef  DYZDG_GXZ_JPKP 
#define DYZDG_GXZ_JPKP gxz(i,j+1,k+1)
#undef  DYZDG_GXZ_JPKM 
#define DYZDG_GXZ_JPKM gxz(i,j+1,k-1)
#undef  DYZDG_GXZ_JMKP 
#define DYZDG_GXZ_JMKP gxz(i,j-1,k+1)
#undef  DYZDG_GXZ_JMKM 
#define DYZDG_GXZ_JMKM gxz(i,j-1,k-1)

#undef  DYZDG_GYY_JPKP 
#define DYZDG_GYY_JPKP gyy(i,j+1,k+1)
#undef  DYZDG_GYY_JPKM 
#define DYZDG_GYY_JPKM gyy(i,j+1,k-1)
#undef  DYZDG_GYY_JMKP 
#define DYZDG_GYY_JMKP gyy(i,j-1,k+1)
#undef  DYZDG_GYY_JMKM 
#define DYZDG_GYY_JMKM gyy(i,j-1,k-1)

#undef  DYZDG_GYZ_JPKP 
#define DYZDG_GYZ_JPKP gyz(i,j+1,k+1)
#undef  DYZDG_GYZ_JPKM 
#define DYZDG_GYZ_JPKM gyz(i,j+1,k-1)
#undef  DYZDG_GYZ_JMKP 
#define DYZDG_GYZ_JMKP gyz(i,j-1,k+1)
#undef  DYZDG_GYZ_JMKM 
#define DYZDG_GYZ_JMKM gyz(i,j-1,k-1)

#undef  DYZDG_GZZ_JPKP 
#define DYZDG_GZZ_JPKP gzz(i,j+1,k+1)
#undef  DYZDG_GZZ_JPKM 
#define DYZDG_GZZ_JPKM gzz(i,j+1,k-1)
#undef  DYZDG_GZZ_JMKP 
#define DYZDG_GZZ_JMKP gzz(i,j-1,k+1)
#undef  DYZDG_GZZ_JMKM 
#define DYZDG_GZZ_JMKM gzz(i,j-1,k-1)

/* Output variables */
#undef  DYZDG_DYZDGXX
#define DYZDG_DYZDGXX deldelg2311
#undef  DYZDG_DYZDGXY
#define DYZDG_DYZDGXY deldelg2312
#undef  DYZDG_DYZDGXZ
#define DYZDG_DYZDGXZ deldelg2313
#undef  DYZDG_DYZDGYY
#define DYZDG_DYZDGYY deldelg2322
#undef  DYZDG_DYZDGYZ
#define DYZDG_DYZDGYZ deldelg2323
#undef  DYZDG_DYZDGZZ
#define DYZDG_DYZDGZZ deldelg2333

/* Internal variables */
#undef  DYZDG_FAC 
#define DYZDG_FAC dyzdg_fac
#undef  DYZDG_OO4DYDZ
#define DYZDG_OO4DYDZ dyzdg_oo4dydz
#undef  DYZDG_DYZDPSI_O_PSI
#define DYZDG_DYZDPSI_O_PSI psiyz(i,j,k)

/* Declare internal variables */
      CCTK_REAL DYZDG_FAC
      CCTK_REAL DYZDG_OO4DYDZ

/* Declare output variables */
      CCTK_REAL DYZDG_DYZDGXX
      CCTK_REAL DYZDG_DYZDGXY
      CCTK_REAL DYZDG_DYZDGXZ
      CCTK_REAL DYZDG_DYZDGYY
      CCTK_REAL DYZDG_DYZDGYZ
      CCTK_REAL DYZDG_DYZDGZZ

#endif

#ifdef CCODE

/* Input variables */
#undef  DYZDG_GXX_JPKP 
#define DYZDG_GXX_JPKP gxx[ dj + dk + ijk]
#undef  DYZDG_GXX_JPKM 
#define DYZDG_GXX_JPKM gxx[ dj - dk + ijk]
#undef  DYZDG_GXX_JMKP 
#define DYZDG_GXX_JMKP gxx[-dj + dk + ijk]
#undef  DYZDG_GXX_JMKM 
#define DYZDG_GXX_JMKM gxx[-dj - dk + ijk]

#undef  DYZDG_GXY_JPKP 
#define DYZDG_GXY_JPKP gxy[ dj + dk + ijk]
#undef  DYZDG_GXY_JPKM 
#define DYZDG_GXY_JPKM gxy[ dj - dk + ijk]
#undef  DYZDG_GXY_JMKP 
#define DYZDG_GXY_JMKP gxy[-dj + dk + ijk]
#undef  DYZDG_GXY_JMKM 
#define DYZDG_GXY_JMKM gxy[-dj - dk + ijk]

#undef  DYZDG_GXZ_JPKP 
#define DYZDG_GXZ_JPKP gxz[ dj + dk + ijk]
#undef  DYZDG_GXZ_JPKM 
#define DYZDG_GXZ_JPKM gxz[ dj - dk + ijk]
#undef  DYZDG_GXZ_JMKP 
#define DYZDG_GXZ_JMKP gxz[-dj + dk + ijk]
#undef  DYZDG_GXZ_JMKM 
#define DYZDG_GXZ_JMKM gxz[-dj - dk + ijk]

#undef  DYZDG_GYY_JPKP 
#define DYZDG_GYY_JPKP gyy[ dj + dk + ijk]
#undef  DYZDG_GYY_JPKM 
#define DYZDG_GYY_JPKM gyy[ dj - dk + ijk]
#undef  DYZDG_GYY_JMKP 
#define DYZDG_GYY_JMKP gyy[-dj + dk + ijk]
#undef  DYZDG_GYY_JMKM 
#define DYZDG_GYY_JMKM gyy[-dj - dk + ijk]

#undef  DYZDG_GYZ_JPKP 
#define DYZDG_GYZ_JPKP gyz[ dj + dk + ijk]
#undef  DYZDG_GYZ_JPKM 
#define DYZDG_GYZ_JPKM gyz[ dj - dk + ijk]
#undef  DYZDG_GYZ_JMKP 
#define DYZDG_GYZ_JMKP gyz[-dj + dk + ijk]
#undef  DYZDG_GYZ_JMKM 
#define DYZDG_GYZ_JMKM gyz[-dj - dk + ijk]

#undef  DYZDG_GZZ_JPKP 
#define DYZDG_GZZ_JPKP gzz[ dj + dk + ijk]
#undef  DYZDG_GZZ_JPKM 
#define DYZDG_GZZ_JPKM gzz[ dj - dk + ijk]
#undef  DYZDG_GZZ_JMKP 
#define DYZDG_GZZ_JMKP gzz[-dj + dk + ijk]
#undef  DYZDG_GZZ_JMKM 
#define DYZDG_GZZ_JMKM gzz[-dj - dk + ijk]

/* Output variables */
#undef  DYZDG_DYZDGXX
#define DYZDG_DYZDGXX deldelg2311
#undef  DYZDG_DYZDGXY
#define DYZDG_DYZDGXY deldelg2312
#undef  DYZDG_DYZDGXZ
#define DYZDG_DYZDGXZ deldelg2313
#undef  DYZDG_DYZDGYY
#define DYZDG_DYZDGYY deldelg2322
#undef  DYZDG_DYZDGYZ
#define DYZDG_DYZDGYZ deldelg2323
#undef  DYZDG_DYZDGZZ
#define DYZDG_DYZDGZZ deldelg2333

/* Internal variables */
#undef  DYZDG_FAC 
#define DYZDG_FAC dyzdg_fac
#undef  DYZDG_OO4DYDZ
#define DYZDG_OO4DYDZ dyzdg_oo4dydz
#undef  DYZDG_DYZDPSI_O_PSI
#define DYZDG_DYZDPSI_O_PSI psiyz[ijk]

/* Declare internal variables */
double DYZDG_FAC;
double DYZDG_OO4DYDZ = 1/(4*cctkGH->cctk_delta_space[1]*cctkGH->cctk_delta_space[2]);

/* Declare output variables */
double DYZDG_DYZDGXX;
double DYZDG_DYZDGXY;
double DYZDG_DYZDGXZ;
double DYZDG_DYZDGYY;
double DYZDG_DYZDGYZ;
double DYZDG_DYZDGZZ;

#endif

#endif
