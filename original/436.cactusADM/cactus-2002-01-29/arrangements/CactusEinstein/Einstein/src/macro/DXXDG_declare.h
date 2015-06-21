/*@@
  @header   DXXDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the (first and) second derivatives 
  of the physical metric with respect to x

  The macro uses @seefile DXDG_guts.h and @seefile DXDG_declare.h
  @enddesc
@@*/

#ifndef DXXDG_DECLARE
#define DXXDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDG_declare.h"

#ifdef FCODE

/* Output variables */
#undef  DXXDG_DXXDGXX
#define DXXDG_DXXDGXX dxxdgxx
#undef  DXXDG_DXXDGXY
#define DXXDG_DXXDGXY dxxdgxy
#undef  DXXDG_DXXDGXZ
#define DXXDG_DXXDGXZ dxxdgxz
#undef  DXXDG_DXXDGYY
#define DXXDG_DXXDGYY dxxdgyy
#undef  DXXDG_DXXDGYZ
#define DXXDG_DXXDGYZ dxxdgyz
#undef  DXXDG_DXXDGZZ
#define DXXDG_DXXDGZZ dxxdgzz

/* Internal variables */
#undef  DXXDG_FAC
#define DXXDG_FAC dxxdg_fac
#undef  DXXDG_OODX2
#define DXXDG_OODX2 dxxdg_oodx2
#undef  DXXDG_DXXDPSI_O_PSI
#define DXXDG_DXXDPSI_O_PSI psixx(i,j,k)

/* Declare internal variables */
      CCTK_REAL DXXDG_FAC
      CCTK_REAL DXXDG_OODX2

/* Declare output variables */
      CCTK_REAL DXXDG_DXXDGXX
      CCTK_REAL DXXDG_DXXDGXY
      CCTK_REAL DXXDG_DXXDGXZ
      CCTK_REAL DXXDG_DXXDGYY
      CCTK_REAL DXXDG_DXXDGYZ
      CCTK_REAL DXXDG_DXXDGZZ

#endif


#ifdef CCODE

/* Output variables */
#undef  DXXDG_DXXDGXX
#define DXXDG_DXXDGXX deldelg1111
#undef  DXXDG_DXXDGXY
#define DXXDG_DXXDGXY deldelg1112
#undef  DXXDG_DXXDGXZ
#define DXXDG_DXXDGXZ deldelg1113
#undef  DXXDG_DXXDGYY
#define DXXDG_DXXDGYY deldelg1122
#undef  DXXDG_DXXDGYZ
#define DXXDG_DXXDGYZ deldelg1123
#undef  DXXDG_DXXDGZZ
#define DXXDG_DXXDGZZ deldelg1133

/* Internal variables */
#undef  DXXDG_FAC
#define DXXDG_FAC dxxdg_fac
#undef  DXXDG_OODX2
#define DXXDG_OODX2 dxxdg_oodx2
#undef  DXXDG_DXXDPSI_O_PSI
#define DXXDG_DXXDPSI_O_PSI psixx[ijk]

/* Declare internal variables */
double DXXDG_FAC;
double DXXDG_OODX2 = 1/(cctkGH->cctk_delta_space[0]*cctkGH->cctk_delta_space[0]);
/*double DXXDG_DXXDPSI_O_PSI;*/

/* Declare output variables */
double DXXDG_DXXDGXX;
double DXXDG_DXXDGXY;
double DXXDG_DXXDGXZ;
double DXXDG_DXXDGYY;
double DXXDG_DXXDGYZ;
double DXXDG_DXXDGZZ;

#endif

#endif
