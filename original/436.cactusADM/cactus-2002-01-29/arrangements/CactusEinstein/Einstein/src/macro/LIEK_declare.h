/*@@
  @header   LIEK_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the Lie derivative of the lower 
  physical metric

  @enddesc
@@*/

#ifndef LIEK_DECLARE
#define LIEK_DECLARE

#include "CactusEinstein/Einstein/src/macro/DB_declare.h"
#include "CactusEinstein/Einstein/src/macro/DK_declare.h"

#ifdef FCODE

/* Input variables */
#undef  LIEK_BX
#define LIEK_BX betax(i,j,k)
#undef  LIEK_BY
#define LIEK_BY betay(i,j,k)
#undef  LIEK_BZ
#define LIEK_BZ betaz(i,j,k)
#undef  LIEK_KXX
#define LIEK_KXX kxx(i,j,k)
#undef  LIEK_KXY
#define LIEK_KXY kxy(i,j,k)
#undef  LIEK_KXZ
#define LIEK_KXZ kxz(i,j,k)
#undef  LIEK_KYY
#define LIEK_KYY kyy(i,j,k)
#undef  LIEK_KYZ
#define LIEK_KYZ kyz(i,j,k)
#undef  LIEK_KZZ
#define LIEK_KZZ kzz(i,j,k)

/* Output variables */
#undef  LIEK_LKXX
#define LIEK_LKXX liek_lkxx
#undef  LIEK_LKXY
#define LIEK_LKXY liek_lkxy
#undef  LIEK_LKXZ
#define LIEK_LKXZ liek_lkxz
#undef  LIEK_LKYY
#define LIEK_LKYY liek_lkyy
#undef  LIEK_LKYZ
#define LIEK_LKYZ liek_lkyz
#undef  LIEK_LKZZ
#define LIEK_LKZZ liek_lkzz

/* Declare output variables */
      CCTK_REAL LIEK_LKXX
      CCTK_REAL LIEK_LKXY
      CCTK_REAL LIEK_LKXZ
      CCTK_REAL LIEK_LKYY
      CCTK_REAL LIEK_LKYZ
      CCTK_REAL LIEK_LKZZ

#endif

#ifdef CCODE

/* Input variables */
#undef  LIEK_BX
#define LIEK_BX betax[ijk]
#undef  LIEK_BY
#define LIEK_BY betay[ijk]
#undef  LIEK_BZ
#define LIEK_BZ betaz[ijk]
#undef  LIEK_KXX
#define LIEK_KXX kxx[ijk]
#undef  LIEK_KXY
#define LIEK_KXY kxy[ijk]
#undef  LIEK_KXZ
#define LIEK_KXZ kxz[ijk]
#undef  LIEK_KYY
#define LIEK_KYY kyy[ijk]
#undef  LIEK_KYZ
#define LIEK_KYZ kyz[ijk]
#undef  LIEK_KZZ
#define LIEK_KZZ kzz[ijk]

/* Output variables */
#undef  LIEK_LKXX
#define LIEK_LKXX liek_lkxx
#undef  LIEK_LKXY
#define LIEK_LKXY liek_lkxy
#undef  LIEK_LKXZ
#define LIEK_LKXZ liek_lkxz
#undef  LIEK_LKYY
#define LIEK_LKYY liek_lkyy
#undef  LIEK_LKYZ
#define LIEK_LKYZ liek_lkyz
#undef  LIEK_LKZZ
#define LIEK_LKZZ liek_lkzz

/* Declare output variables */
      double LIEK_LKXX;
      double LIEK_LKXY;
      double LIEK_LKXZ;
      double LIEK_LKYY;
      double LIEK_LKYZ;
      double LIEK_LKZZ;

#endif

#endif
