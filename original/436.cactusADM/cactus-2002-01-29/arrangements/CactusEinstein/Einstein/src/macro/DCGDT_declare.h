/*@@
  @header   DCGDT_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DCGDT_guts.h to calculate the source term 
  in the evolution equation for the conformal 3-metric. That is

  d g~_ij/dt =( - 2 alpha K_ij + L_beta g_ij )/Psi^4

  where g~ is the conformal metric
  @enddesc
@@*/

#ifndef DCGDT_DECLARE
#define DCGDT_DECLARE

#include "CactusEinstein/Einstein/src/macro/LIEG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DCGDT_A
#define DCGDT_A alp(i,j,k)
#undef  DCGDT_KXX 
#define DCGDT_KXX kxx(i,j,k)
#undef  DCGDT_KXY 
#define DCGDT_KXY kxy(i,j,k)
#undef  DCGDT_KXZ 
#define DCGDT_KXZ kxz(i,j,k)
#undef  DCGDT_KYY 
#define DCGDT_KYY kyy(i,j,k)
#undef  DCGDT_KYZ 
#define DCGDT_KYZ kyz(i,j,k)
#undef  DCGDT_KZZ 
#define DCGDT_KZZ kzz(i,j,k)
#undef  DCGDT_PSI 
#define DCGDT_PSI psi(i,j,k)

/* Internal variables */
#undef  DCGDT_IPSI4
#define DCGDT_IPSI4 cdgdt_ipsi4

/* Output variables */ 
#undef  DCGDT_DCGXXDT
#define DCGDT_DCGXXDT cdgdt_cdgxxdt
#undef  DCGDT_DCGXYDT
#define DCGDT_DCGXYDT cdgdt_cdgxydt
#undef  DCGDT_DCGXZDT
#define DCGDT_DCGXZDT cdgdt_cdgxzdt
#undef  DCGDT_DCGYYDT
#define DCGDT_DCGYYDT cdgdt_cdgyydt
#undef  DCGDT_DCGYZDT
#define DCGDT_DCGYZDT cdgdt_cdgyzdt
#undef  DCGDT_DCGZZDT
#define DCGDT_DCGZZDT cdgdt_cdgzzdt

/* Declare internal variables */
      CCTK_REAL DCGDT_IPSI4

/* Declare output variables */
      CCTK_REAL DCGDT_DCGXXDT
      CCTK_REAL DCGDT_DCGXYDT
      CCTK_REAL DCGDT_DCGXZDT
      CCTK_REAL DCGDT_DCGYYDT
      CCTK_REAL DCGDT_DCGYZDT
      CCTK_REAL DCGDT_DCGZZDT

#endif


#ifdef CCODE

/* Input variables */
#undef  DCGDT_A
#define DCGDT_A alp[ijk]
#undef  DCGDT_KXX 
#define DCGDT_KXX kxx[ijk]
#undef  DCGDT_KXY 
#define DCGDT_KXY kxy[ijk]
#undef  DCGDT_KXZ 
#define DCGDT_KXZ kxz[ijk]
#undef  DCGDT_KYY 
#define DCGDT_KYY kyy[ijk]
#undef  DCGDT_KYZ 
#define DCGDT_KYZ kyz[ijk]
#undef  DCGDT_KZZ 
#define DCGDT_KZZ kzz[ijk]
#undef  DCGDT_PSI 
#define DCGDT_PSI psi[ijk]

/* Internal variables */
#undef  DCGDT_IPSI4
#define DCGDT_IPSI4 cdgdt_ipsi4

/* Output variables */ 
#undef  DCGDT_DCGXXDT
#define DCGDT_DCGXXDT cdgdt_cdgxxdt
#undef  DCGDT_DCGXYDT
#define DCGDT_DCGXYDT cdgdt_cdgxydt
#undef  DCGDT_DCGXZDT
#define DCGDT_DCGXZDT cdgdt_cdgxzdt
#undef  DCGDT_DCGYYDT
#define DCGDT_DCGYYDT cdgdt_cdgyydt
#undef  DCGDT_DCGYZDT
#define DCGDT_DCGYZDT cdgdt_cdgyzdt
#undef  DCGDT_DCGZZDT
#define DCGDT_DCGZZDT cdgdt_cdgzzdt

/* Declare internal variables */
      double DCGDT_IPSI4;

/* Declare output variables */
      double DCGDT_DCGXXDT;
      double DCGDT_DCGXYDT;
      double DCGDT_DCGXZDT;
      double DCGDT_DCGYYDT;
      double DCGDT_DCGYZDT;
      double DCGDT_DCGZZDT;

#endif

#endif
