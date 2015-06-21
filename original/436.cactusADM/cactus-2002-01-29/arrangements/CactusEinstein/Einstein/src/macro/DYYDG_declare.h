/*@@
  @header   DYYDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the (first and) second derivatives 
  of the physical metric with respect to y

  The macro uses @seefile DYDG_guts.h and @seefile DYDG_declare.h
  @enddesc
@@*/

#ifndef DYYDG_DECLARE
#define DYYDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DYDG_declare.h"

#ifdef FCODE

/* Output variables */
#undef  DYYDG_DYYDGXX
#define DYYDG_DYYDGXX dyydgxx
#undef  DYYDG_DYYDGXY
#define DYYDG_DYYDGXY dyydgxy
#undef  DYYDG_DYYDGXZ
#define DYYDG_DYYDGXZ dyydgxz
#undef  DYYDG_DYYDGYY
#define DYYDG_DYYDGYY dyydgyy
#undef  DYYDG_DYYDGYZ
#define DYYDG_DYYDGYZ dyydgyz
#undef  DYYDG_DYYDGZZ
#define DYYDG_DYYDGZZ dyydgzz

/* Internal variables */
#undef  DYYDG_FAC
#define DYYDG_FAC dyydg_fac
#undef  DYYDG_OODY2
#define DYYDG_OODY2 dyydg_oody2
#undef  DYYDG_DYYDPSI_O_PSI
#define DYYDG_DYYDPSI_O_PSI psiyy(i,j,k)

/* Declare internal variables */
      CCTK_REAL DYYDG_FAC
      CCTK_REAL DYYDG_OODY2

/* Declare output variables */
      CCTK_REAL DYYDG_DYYDGXX
      CCTK_REAL DYYDG_DYYDGXY
      CCTK_REAL DYYDG_DYYDGXZ
      CCTK_REAL DYYDG_DYYDGYY
      CCTK_REAL DYYDG_DYYDGYZ
      CCTK_REAL DYYDG_DYYDGZZ

#endif

#ifdef CCODE

/* Output variables */
#undef  DYYDG_DYYDGXX
#define DYYDG_DYYDGXX deldelg2211
#undef  DYYDG_DYYDGXY
#define DYYDG_DYYDGXY deldelg2212
#undef  DYYDG_DYYDGXZ
#define DYYDG_DYYDGXZ deldelg2213
#undef  DYYDG_DYYDGYY
#define DYYDG_DYYDGYY deldelg2222
#undef  DYYDG_DYYDGYZ
#define DYYDG_DYYDGYZ deldelg2223
#undef  DYYDG_DYYDGZZ
#define DYYDG_DYYDGZZ deldelg2233

/* Internal variables */
#undef  DYYDG_FAC
#define DYYDG_FAC dyydg_fac
#undef  DYYDG_OODY2
#define DYYDG_OODY2 dyydg_oody2
#undef  DYYDG_DYYDPSI_O_PSI
#define DYYDG_DYYDPSI_O_PSI psiyy[ijk]

/* Declare internal variables */
double DYYDG_FAC;
double DYYDG_OODY2 = 1/(cctkGH->cctk_delta_space[1]*cctkGH->cctk_delta_space[1]);

/* Declare output variables */
double DYYDG_DYYDGXX;
double DYYDG_DYYDGXY;
double DYYDG_DYYDGXZ;
double DYYDG_DYYDGYY;
double DYYDG_DYYDGYZ;
double DYYDG_DYYDGZZ;

#endif

#endif
