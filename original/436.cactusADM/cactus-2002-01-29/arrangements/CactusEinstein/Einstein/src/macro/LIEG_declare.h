/*@@
  @header   LIEG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the Lie derivative of the lower 
  physical metric

  @enddesc
@@*/

#ifndef LIEG_DECLARE
#define LIEG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DB_declare.h"
#include "CactusEinstein/Einstein/src/macro/DG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  LIEG_BX
#define LIEG_BX betax(i,j,k)
#undef  LIEG_BY
#define LIEG_BY betay(i,j,k)
#undef  LIEG_BZ
#define LIEG_BZ betaz(i,j,k)
#undef  LIEG_GXX
#define LIEG_GXX gxx(i,j,k)
#undef  LIEG_GXY
#define LIEG_GXY gxy(i,j,k)
#undef  LIEG_GXZ
#define LIEG_GXZ gxz(i,j,k)
#undef  LIEG_GYY
#define LIEG_GYY gyy(i,j,k)
#undef  LIEG_GYZ
#define LIEG_GYZ gyz(i,j,k)
#undef  LIEG_GZZ
#define LIEG_GZZ gzz(i,j,k)

/* Output variables */
#undef  LIEG_LGXX
#define LIEG_LGXX lieg_lgxx
#undef  LIEG_LGXY
#define LIEG_LGXY lieg_lgxy
#undef  LIEG_LGXZ
#define LIEG_LGXZ lieg_lgxz
#undef  LIEG_LGYY
#define LIEG_LGYY lieg_lgyy
#undef  LIEG_LGYZ
#define LIEG_LGYZ lieg_lgyz
#undef  LIEG_LGZZ
#define LIEG_LGZZ lieg_lgzz

/* Declare output variables */
      CCTK_REAL LIEG_LGXX
      CCTK_REAL LIEG_LGXY
      CCTK_REAL LIEG_LGXZ
      CCTK_REAL LIEG_LGYY
      CCTK_REAL LIEG_LGYZ
      CCTK_REAL LIEG_LGZZ

#endif

#ifdef CCODE

/* Input variables */
#undef  LIEG_BX
#define LIEG_BX betax[ijk]
#undef  LIEG_BY
#define LIEG_BY betay[ijk]
#undef  LIEG_BZ
#define LIEG_BZ betaz[ijk]
#undef  LIEG_GXX
#define LIEG_GXX gxx[ijk]
#undef  LIEG_GXY
#define LIEG_GXY gxy[ijk]
#undef  LIEG_GXZ
#define LIEG_GXZ gxz[ijk]
#undef  LIEG_GYY
#define LIEG_GYY gyy[ijk]
#undef  LIEG_GYZ
#define LIEG_GYZ gyz[ijk]
#undef  LIEG_GZZ
#define LIEG_GZZ gzz[ijk]

/* Output variables */
#undef  LIEG_LGXX
#define LIEG_LGXX lieg_lgxx
#undef  LIEG_LGXY
#define LIEG_LGXY lieg_lgxy
#undef  LIEG_LGXZ
#define LIEG_LGXZ lieg_lgxz
#undef  LIEG_LGYY
#define LIEG_LGYY lieg_lgyy
#undef  LIEG_LGYZ
#define LIEG_LGYZ lieg_lgyz
#undef  LIEG_LGZZ
#define LIEG_LGZZ lieg_lgzz

/* Declare output variables */
      double LIEG_LGXX;
      double LIEG_LGXY;
      double LIEG_LGXZ;
      double LIEG_LGYY;
      double LIEG_LGYZ;
      double LIEG_LGZZ;

#endif

#endif
