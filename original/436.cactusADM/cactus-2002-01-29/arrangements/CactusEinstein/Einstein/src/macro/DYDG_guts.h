/*@@
  @header   DYDG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  physical metric with respect to y

  The macro is defined in terms of standard variables in
  @seefile DYDG_declare.h
  @enddesc
@@*/

#ifndef DYDG_GUTS
#define DYDG_GUTS

#include "CactusEinstein/Einstein/src/macro/DYDCG_guts.h"

#ifdef FCODE 

      IF (conformal_state /= CONFORMAL_METRIC) THEN
        DYDG_PSI4 = 1
        DYDG_FAC  = 0
      ELSE
        DYDG_PSI4 = DYDG_PSI**4
        DYDG_FAC  = 4*DYDG_PSI4*DYDG_DYDPSI_O_PSI
      ENDIF

      DYDG_DYDGXX = DYDCG_DYDCGXX*DYDG_PSI4 + DYDG_FAC*DYDG_GXX
      DYDG_DYDGXY = DYDCG_DYDCGXY*DYDG_PSI4 + DYDG_FAC*DYDG_GXY
      DYDG_DYDGXZ = DYDCG_DYDCGXZ*DYDG_PSI4 + DYDG_FAC*DYDG_GXZ
      DYDG_DYDGYY = DYDCG_DYDCGYY*DYDG_PSI4 + DYDG_FAC*DYDG_GYY
      DYDG_DYDGYZ = DYDCG_DYDCGYZ*DYDG_PSI4 + DYDG_FAC*DYDG_GYZ
      DYDG_DYDGZZ = DYDCG_DYDCGZZ*DYDG_PSI4 + DYDG_FAC*DYDG_GZZ

#endif

#ifdef CCODE

      DYDG_PSI4 = ((*conformal_state != CONFORMAL_METRIC)?1:DYDG_PSI*DYDG_PSI*DYDG_PSI*DYDG_PSI);

      DYDG_FAC  = ((*conformal_state != CONFORMAL_METRIC)?0:4*DYDG_PSI4*DYDG_DYDPSI_O_PSI);

      DYDG_DYDGXX = DYDCG_DYDCGXX*DYDG_PSI4 + DYDG_FAC*DYDG_GXX;
      DYDG_DYDGXY = DYDCG_DYDCGXY*DYDG_PSI4 + DYDG_FAC*DYDG_GXY;
      DYDG_DYDGXZ = DYDCG_DYDCGXZ*DYDG_PSI4 + DYDG_FAC*DYDG_GXZ;
      DYDG_DYDGYY = DYDCG_DYDCGYY*DYDG_PSI4 + DYDG_FAC*DYDG_GYY;
      DYDG_DYDGYZ = DYDCG_DYDCGYZ*DYDG_PSI4 + DYDG_FAC*DYDG_GYZ;
      DYDG_DYDGZZ = DYDCG_DYDCGZZ*DYDG_PSI4 + DYDG_FAC*DYDG_GZZ;

#endif

#endif
