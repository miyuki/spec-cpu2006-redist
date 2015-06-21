/*@@
  @header   DKDT_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the source term in the evolution equation for the
  extrinsic curvature. That is

  d K_ij/dt = alpha*(R_ij-2 K_ik K^k_j + K_ij trK] - nabla_i nabla_j alpha

       + Lie_beta K_ij
 
  @enddesc
@@*/

#ifndef DKDT_GUTS
#define DKDT_GUTS

#include "CactusEinstein/Einstein/src/macro/RICCI_guts.h"
#include "CactusEinstein/Einstein/src/macro/KK_guts.h"
#include "CactusEinstein/Einstein/src/macro/TRK_guts.h"
#include "CactusEinstein/Einstein/src/macro/CDCDA_guts.h"

#ifdef FCODE 

      DKDT_DKXXDT = DKDT_A*(RICCI_RXX-2*KK_KKXX+DKDT_KXX*TRK_TRK)-CDCDA_CDXXDA
      DKDT_DKXYDT = DKDT_A*(RICCI_RXY-2*KK_KKXY+DKDT_KXY*TRK_TRK)-CDCDA_CDXYDA
      DKDT_DKXZDT = DKDT_A*(RICCI_RXZ-2*KK_KKXZ+DKDT_KXZ*TRK_TRK)-CDCDA_CDXZDA
      DKDT_DKYYDT = DKDT_A*(RICCI_RYY-2*KK_KKYY+DKDT_KYY*TRK_TRK)-CDCDA_CDYYDA
      DKDT_DKYZDT = DKDT_A*(RICCI_RYZ-2*KK_KKYZ+DKDT_KYZ*TRK_TRK)-CDCDA_CDYZDA
      DKDT_DKZZDT = DKDT_A*(RICCI_RZZ-2*KK_KKZZ+DKDT_KZZ*TRK_TRK)-CDCDA_CDZZDA

      IF (shift_state .ne. SHIFT_INACTIVE) THEN

#include "CactusEinstein/Einstein/src/macro/LIEK_guts.h"

        DKDT_DKXXDT = DKDT_DKXXDT + LIEK_LKXX
        DKDT_DKXYDT = DKDT_DKXYDT + LIEK_LKXY
        DKDT_DKXZDT = DKDT_DKXZDT + LIEK_LKXZ
        DKDT_DKYYDT = DKDT_DKYYDT + LIEK_LKYY
        DKDT_DKYZDT = DKDT_DKYZDT + LIEK_LKYZ
        DKDT_DKZZDT = DKDT_DKZZDT + LIEK_LKZZ

      END IF


#endif

#ifdef CCODE

      DKDT_DKXXDT = DKDT_A*(RICCI_RXX-2*KK_KKXX+DKDT_KXX*TRK_TRK)-CDCDA_CDXXDA;
      DKDT_DKXYDT = DKDT_A*(RICCI_RXY-2*KK_KKXY+DKDT_KXY*TRK_TRK)-CDCDA_CDXYDA;
      DKDT_DKXZDT = DKDT_A*(RICCI_RXZ-2*KK_KKXZ+DKDT_KXZ*TRK_TRK)-CDCDA_CDXZDA;
      DKDT_DKYYDT = DKDT_A*(RICCI_RYY-2*KK_KKYY+DKDT_KYY*TRK_TRK)-CDCDA_CDYYDA;
      DKDT_DKYZDT = DKDT_A*(RICCI_RYZ-2*KK_KKYZ+DKDT_KYZ*TRK_TRK)-CDCDA_CDYZDA;
      DKDT_DKZZDT = DKDT_A*(RICCI_RZZ-2*KK_KKZZ+DKDT_KZZ*TRK_TRK)-CDCDA_CDZZDA;

      if (*shift_state != SHIFT_INACTIVE) 
      {

#include "CactusEinstein/Einstein/src/macro/LIEK_guts.h"

        DKDT_DKXXDT = DKDT_DKXXDT + LIEK_LKXX;
        DKDT_DKXYDT = DKDT_DKXYDT + LIEK_LKXY;
        DKDT_DKXZDT = DKDT_DKXZDT + LIEK_LKXZ;
        DKDT_DKYYDT = DKDT_DKYYDT + LIEK_LKYY;
        DKDT_DKYZDT = DKDT_DKYZDT + LIEK_LKYZ;
        DKDT_DKZZDT = DKDT_DKZZDT + LIEK_LKZZ;

      }

#endif

#endif

