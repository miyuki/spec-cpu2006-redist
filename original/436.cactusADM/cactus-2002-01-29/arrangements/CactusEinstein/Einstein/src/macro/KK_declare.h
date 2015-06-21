/*@@
  @header   KK_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate term K_{ik}K^{k}_{l}

  Requires: Upper physical metric
            Lower extrinsic curvature

  Provides: Components of K_{ik}K^{k}_{l}

  @enddesc
@@*/

#ifndef KK_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"

#ifdef FCODE

/* Input variables */
#undef  KK_HXX
#define KK_HXX kxx(i,j,k)
#undef  KK_HXY
#define KK_HXY kxy(i,j,k)
#undef  KK_HXZ
#define KK_HXZ kxz(i,j,k)
#undef  KK_HYY
#define KK_HYY kyy(i,j,k)
#undef  KK_HYZ
#define KK_HYZ kyz(i,j,k)
#undef  KK_HZZ
#define KK_HZZ kzz(i,j,k)

/* Output variables */
#undef  KK_KKXX
#define KK_KKXX KK11
#undef  KK_KKXY
#define KK_KKXY KK12
#undef  KK_KKXZ
#define KK_KKXZ KK13
#undef  KK_KKYY
#define KK_KKYY KK22
#undef  KK_KKYZ
#define KK_KKYZ KK23
#undef  KK_KKZZ
#define KK_KKZZ KK33

/* Declare output variables */
      CCTK_REAL KK_KKXX
      CCTK_REAL KK_KKXY
      CCTK_REAL KK_KKXZ
      CCTK_REAL KK_KKYY
      CCTK_REAL KK_KKYZ
      CCTK_REAL KK_KKZZ

#endif

#ifdef CCODE

/* Input variables */
#undef  KK_HXX
#define KK_HXX kxx[ijk]
#undef  KK_HXY
#define KK_HXY kxy[ijk]
#undef  KK_HXZ
#define KK_HXZ kxz[ijk]
#undef  KK_HYY
#define KK_HYY kyy[ijk]
#undef  KK_HYZ
#define KK_HYZ kyz[ijk]
#undef  KK_HZZ
#define KK_HZZ kzz[ijk]

/* Output variables */
#undef  KK_KKXX
#define KK_KKXX KK11
#undef  KK_KKXY
#define KK_KKXY KK12
#undef  KK_KKXZ
#define KK_KKXZ KK13
#undef  KK_KKYY
#define KK_KKYY KK22
#undef  KK_KKYZ
#define KK_KKYZ KK23
#undef  KK_KKZZ
#define KK_KKZZ KK33

/* Declare output variables */
double KK_KKXX;
double KK_KKXY;
double KK_KKXZ;
double KK_KKYY;
double KK_KKYZ;
double KK_KKZZ;

#endif

#define KK_DECLARE

#endif
