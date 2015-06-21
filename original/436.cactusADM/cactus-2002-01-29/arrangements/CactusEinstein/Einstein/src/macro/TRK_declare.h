/*@@
  @header   TRK_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate the trace of the Extrinsic
  curvature

  @enddesc
@@*/

#ifndef TRK_DECLARE
#define TRK_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"

#ifdef FCODE

/* Input variables */
#undef  TRK_HXX
#define TRK_HXX kxx(i,j,k)
#undef  TRK_HXY
#define TRK_HXY kxy(i,j,k)
#undef  TRK_HXZ
#define TRK_HXZ kxz(i,j,k)
#undef  TRK_HYY
#define TRK_HYY kyy(i,j,k)
#undef  TRK_HYZ
#define TRK_HYZ kyz(i,j,k)
#undef  TRK_HZZ
#define TRK_HZZ kzz(i,j,k)

/* Output variables */
#undef  TRK_TRK
#define TRK_TRK trk_trK

/* Declare output variables */
       CCTK_REAL TRK_TRK

#endif

#ifdef CCODE

/* Input variables */
#undef  TRK_HXX
#define TRK_HXX kxx[ijk]
#undef  TRK_HXY
#define TRK_HXY kxy[ijk]
#undef  TRK_HXZ
#define TRK_HXZ kxz[ijk]
#undef  TRK_HYY
#define TRK_HYY kyy[ijk]
#undef  TRK_HYZ
#define TRK_HYZ kyz[ijk]
#undef  TRK_HZZ
#define TRK_HZZ kzz[ijk]

/* Output variables */
#undef  TRK_TRK
#define TRK_TRK trk_trK

/* Declare output variables */
CCTK_REAL TRK_TRK;

#endif

#endif
