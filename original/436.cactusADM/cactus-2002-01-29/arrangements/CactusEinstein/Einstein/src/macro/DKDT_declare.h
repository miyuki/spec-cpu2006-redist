/*@@
  @header   DKDT_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DKDT_guts.h to calculate the source term 
  in the evolution equation for the extrinsic curvature. That is

  d K_ij/dt = alpha*(R_ij-2 K_ik K^k_j + K_ij trK] - nabla_i nabla_j alpha

       + Lie_beta K_ij
 
  @enddesc
@@*/

#ifndef DKDT_DECLARE
#define DKDT_DECLARE

#include "CactusEinstein/Einstein/src/macro/RICCI_declare.h"
#include "CactusEinstein/Einstein/src/macro/KK_declare.h"
#include "CactusEinstein/Einstein/src/macro/TRK_declare.h"
#include "CactusEinstein/Einstein/src/macro/CDCDA_declare.h"
#include "CactusEinstein/Einstein/src/macro/LIEK_declare.h"


#ifdef FCODE

/* Input variables */
#undef  DKDT_A
#define DKDT_A alp(i,j,k)
#undef  DKDT_KXX 
#define DKDT_KXX kxx(i,j,k)
#undef  DKDT_KXY 
#define DKDT_KXY kxy(i,j,k)
#undef  DKDT_KXZ 
#define DKDT_KXZ kxz(i,j,k)
#undef  DKDT_KYY 
#define DKDT_KYY kyy(i,j,k)
#undef  DKDT_KYZ 
#define DKDT_KYZ kyz(i,j,k)
#undef  DKDT_KZZ 
#define DKDT_KZZ kzz(i,j,k)

/* Output variables */ 
#undef  DKDT_DKXXDT
#define DKDT_DKXXDT dkdt_dkxxdt
#undef  DKDT_DKXYDT
#define DKDT_DKXYDT dkdt_dkxydt
#undef  DKDT_DKXZDT
#define DKDT_DKXZDT dkdt_dkxzdt
#undef  DKDT_DKYYDT
#define DKDT_DKYYDT dkdt_dkyydt
#undef  DKDT_DKYZDT
#define DKDT_DKYZDT dkdt_dkyzdt
#undef  DKDT_DKZZDT
#define DKDT_DKZZDT dkdt_dkzzdt

/* Declare output variables */
      CCTK_REAL DKDT_DKXXDT
      CCTK_REAL DKDT_DKXYDT
      CCTK_REAL DKDT_DKXZDT
      CCTK_REAL DKDT_DKYYDT
      CCTK_REAL DKDT_DKYZDT
      CCTK_REAL DKDT_DKZZDT

#endif


#ifdef CCODE

/* Input variables */
#undef  DKDT_A
#define DKDT_A alp[ijk]
#undef  DKDT_KXX 
#define DKDT_KXX kxx[ijk]
#undef  DKDT_KXY 
#define DKDT_KXY kxy[ijk]
#undef  DKDT_KXZ 
#define DKDT_KXZ kxz[ijk]
#undef  DKDT_KYY 
#define DKDT_KYY kyy[ijk]
#undef  DKDT_KYZ 
#define DKDT_KYZ kyz[ijk]
#undef  DKDT_KZZ 
#define DKDT_KZZ kzz[ijk]

/* Output variables */ 
#undef  DKDT_DKXXDT
#define DKDT_DKXXDT dkdt_dkxxdt
#undef  DKDT_DKXYDT
#define DKDT_DKXYDT dkdt_dkxydt
#undef  DKDT_DKXZDT
#define DKDT_DKXZDT dkdt_dkxzdt
#undef  DKDT_DKYYDT
#define DKDT_DKYYDT dkdt_dkyydt
#undef  DKDT_DKYZDT
#define DKDT_DKYZDT dkdt_dkyzdt
#undef  DKDT_DKZZDT
#define DKDT_DKZZDT dkdt_dkzzdt

/* Declare output variables */
      double DKDT_DKXXDT;
      double DKDT_DKXYDT;
      double DKDT_DKXZDT;
      double DKDT_DKYYDT;
      double DKDT_DKYZDT;
      double DKDT_DKZZDT;

#endif

#endif
