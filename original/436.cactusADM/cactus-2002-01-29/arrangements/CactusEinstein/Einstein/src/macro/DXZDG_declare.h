/*@@
  @header   DXZDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declaration for macro to calculate the (first and) second derivatives 
  of the physical metric with respect to x,z

  The macro uses @seefile DXDG_guts.h , @seefile DZDG_guts.h and 
  @seefile DXDG_declare.h , @seefile DZDG_declare.h
  @enddesc
@@*/

#ifndef DXZDG_DECLARE
#define DXZDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/DZDG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DXZDG_GXX_IPKP 
#define DXZDG_GXX_IPKP gxx(i+1,j,k+1)
#undef  DXZDG_GXX_IPKM 
#define DXZDG_GXX_IPKM gxx(i+1,j,k-1)
#undef  DXZDG_GXX_IMKP 
#define DXZDG_GXX_IMKP gxx(i-1,j,k+1)
#undef  DXZDG_GXX_IMKM 
#define DXZDG_GXX_IMKM gxx(i-1,j,k-1)

#undef  DXZDG_GXY_IPKP 
#define DXZDG_GXY_IPKP gxy(i+1,j,k+1)
#undef  DXZDG_GXY_IPKM 
#define DXZDG_GXY_IPKM gxy(i+1,j,k-1)
#undef  DXZDG_GXY_IMKP 
#define DXZDG_GXY_IMKP gxy(i-1,j,k+1)
#undef  DXZDG_GXY_IMKM 
#define DXZDG_GXY_IMKM gxy(i-1,j,k-1)

#undef  DXZDG_GXZ_IPKP 
#define DXZDG_GXZ_IPKP gxz(i+1,j,k+1)
#undef  DXZDG_GXZ_IPKM 
#define DXZDG_GXZ_IPKM gxz(i+1,j,k-1)
#undef  DXZDG_GXZ_IMKP 
#define DXZDG_GXZ_IMKP gxz(i-1,j,k+1)
#undef  DXZDG_GXZ_IMKM 
#define DXZDG_GXZ_IMKM gxz(i-1,j,k-1)

#undef  DXZDG_GYY_IPKP 
#define DXZDG_GYY_IPKP gyy(i+1,j,k+1)
#undef  DXZDG_GYY_IPKM 
#define DXZDG_GYY_IPKM gyy(i+1,j,k-1)
#undef  DXZDG_GYY_IMKP 
#define DXZDG_GYY_IMKP gyy(i-1,j,k+1)
#undef  DXZDG_GYY_IMKM 
#define DXZDG_GYY_IMKM gyy(i-1,j,k-1)

#undef  DXZDG_GYZ_IPKP 
#define DXZDG_GYZ_IPKP gyz(i+1,j,k+1)
#undef  DXZDG_GYZ_IPKM 
#define DXZDG_GYZ_IPKM gyz(i+1,j,k-1)
#undef  DXZDG_GYZ_IMKP 
#define DXZDG_GYZ_IMKP gyz(i-1,j,k+1)
#undef  DXZDG_GYZ_IMKM 
#define DXZDG_GYZ_IMKM gyz(i-1,j,k-1)

#undef  DXZDG_GZZ_IPKP 
#define DXZDG_GZZ_IPKP gzz(i+1,j,k+1)
#undef  DXZDG_GZZ_IPKM 
#define DXZDG_GZZ_IPKM gzz(i+1,j,k-1)
#undef  DXZDG_GZZ_IMKP 
#define DXZDG_GZZ_IMKP gzz(i-1,j,k+1)
#undef  DXZDG_GZZ_IMKM 
#define DXZDG_GZZ_IMKM gzz(i-1,j,k-1)

/* Output variables */
#undef  DXZDG_DXZDGXX
#define DXZDG_DXZDGXX deldelg1311
#undef  DXZDG_DXZDGXY
#define DXZDG_DXZDGXY deldelg1312
#undef  DXZDG_DXZDGXZ
#define DXZDG_DXZDGXZ deldelg1313
#undef  DXZDG_DXZDGYY
#define DXZDG_DXZDGYY deldelg1322
#undef  DXZDG_DXZDGYZ
#define DXZDG_DXZDGYZ deldelg1323
#undef  DXZDG_DXZDGZZ
#define DXZDG_DXZDGZZ deldelg1333

/* Internal variables */
#undef  DXZDG_FAC 
#define DXZDG_FAC dxzdg_fac
#undef  DXZDG_OO4DXDZ
#define DXZDG_OO4DXDZ dxzdg_oo4dxdz
#undef  DXZDG_DXZDPSI_O_PSI
#define DXZDG_DXZDPSI_O_PSI psixz(i,j,k)

/* Declare internal variables */
      CCTK_REAL DXZDG_FAC
      CCTK_REAL DXZDG_OO4DXDZ 

/* Declare output variables */
      CCTK_REAL DXZDG_DXZDGXX
      CCTK_REAL DXZDG_DXZDGXY
      CCTK_REAL DXZDG_DXZDGXZ
      CCTK_REAL DXZDG_DXZDGYY
      CCTK_REAL DXZDG_DXZDGYZ
      CCTK_REAL DXZDG_DXZDGZZ

#endif

#ifdef CCODE

/* Input variables */
#undef  DXZDG_GXX_IPKP 
#define DXZDG_GXX_IPKP gxx[ di + dk + ijk]
#undef  DXZDG_GXX_IPKM 
#define DXZDG_GXX_IPKM gxx[ di - dk + ijk]
#undef  DXZDG_GXX_IMKP 
#define DXZDG_GXX_IMKP gxx[-di + dk + ijk]
#undef  DXZDG_GXX_IMKM 
#define DXZDG_GXX_IMKM gxx[-di - dk + ijk]

#undef  DXZDG_GXY_IPKP 
#define DXZDG_GXY_IPKP gxy[ di + dk + ijk]
#undef  DXZDG_GXY_IPKM 
#define DXZDG_GXY_IPKM gxy[ di - dk + ijk]
#undef  DXZDG_GXY_IMKP 
#define DXZDG_GXY_IMKP gxy[-di + dk + ijk]
#undef  DXZDG_GXY_IMKM 
#define DXZDG_GXY_IMKM gxy[-di - dk + ijk]

#undef  DXZDG_GXZ_IPKP 
#define DXZDG_GXZ_IPKP gxz[ di + dk + ijk]
#undef  DXZDG_GXZ_IPKM 
#define DXZDG_GXZ_IPKM gxz[ di - dk + ijk]
#undef  DXZDG_GXZ_IMKP 
#define DXZDG_GXZ_IMKP gxz[-di + dk + ijk]
#undef  DXZDG_GXZ_IMKM 
#define DXZDG_GXZ_IMKM gxz[-di - dk + ijk]

#undef  DXZDG_GYY_IPKP 
#define DXZDG_GYY_IPKP gyy[ di + dk + ijk]
#undef  DXZDG_GYY_IPKM 
#define DXZDG_GYY_IPKM gyy[ di - dk + ijk]
#undef  DXZDG_GYY_IMKP 
#define DXZDG_GYY_IMKP gyy[-di + dk + ijk]
#undef  DXZDG_GYY_IMKM 
#define DXZDG_GYY_IMKM gyy[-di - dk + ijk]

#undef  DXZDG_GYZ_IPKP 
#define DXZDG_GYZ_IPKP gyz[ di + dk + ijk]
#undef  DXZDG_GYZ_IPKM 
#define DXZDG_GYZ_IPKM gyz[ di - dk + ijk]
#undef  DXZDG_GYZ_IMKP 
#define DXZDG_GYZ_IMKP gyz[-di + dk + ijk]
#undef  DXZDG_GYZ_IMKM 
#define DXZDG_GYZ_IMKM gyz[-di - dk + ijk]

#undef  DXZDG_GZZ_IPKP 
#define DXZDG_GZZ_IPKP gzz[ di + dk + ijk]
#undef  DXZDG_GZZ_IPKM 
#define DXZDG_GZZ_IPKM gzz[ di - dk + ijk]
#undef  DXZDG_GZZ_IMKP 
#define DXZDG_GZZ_IMKP gzz[-di + dk + ijk]
#undef  DXZDG_GZZ_IMKM 
#define DXZDG_GZZ_IMKM gzz[-di - dk + ijk]

/* Output variables */
#undef  DXZDG_DXZDGXX
#define DXZDG_DXZDGXX deldelg1311
#undef  DXZDG_DXZDGXY
#define DXZDG_DXZDGXY deldelg1312
#undef  DXZDG_DXZDGXZ
#define DXZDG_DXZDGXZ deldelg1313
#undef  DXZDG_DXZDGYY
#define DXZDG_DXZDGYY deldelg1322
#undef  DXZDG_DXZDGYZ
#define DXZDG_DXZDGYZ deldelg1323
#undef  DXZDG_DXZDGZZ
#define DXZDG_DXZDGZZ deldelg1333

/* Internal variables */
#undef  DXZDG_FAC 
#define DXZDG_FAC dxzdg_fac
#undef  DXZDG_OO4DXDZ
#define DXZDG_OO4DXDZ dxzdg_oo4dxdz
#undef  DXZDG_DXZDPSI_O_PSI
#define DXZDG_DXZDPSI_O_PSI psixz[ijk]

/* Declare internal variables */
double DXZDG_FAC;
double DXZDG_OO4DXDZ = 1/(4*cctkGH->cctk_delta_space[0]*cctkGH->cctk_delta_space[2]);

/* Declare output variables */
double DXZDG_DXZDGXX;
double DXZDG_DXZDGXY;
double DXZDG_DXZDGXZ;
double DXZDG_DXZDGYY;
double DXZDG_DXZDGYZ;
double DXZDG_DXZDGZZ;

#endif

#endif
