/*@@
  @header   DXDG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  physical metric with respect to x

  The macro is defined in terms of standard variables in
  @seefile DXDG_declare.h
  @enddesc
@@*/

#ifndef DXDG_GUTS
#define DXDG_GUTS

#include "CactusEinstein/Einstein/src/macro/DXDCG_guts.h"

#ifdef FCODE 

      IF (conformal_state /= CONFORMAL_METRIC) THEN
        DXDG_PSI4 = 1
        DXDG_FAC  = 0
      ELSE
        DXDG_PSI4 = DXDG_PSI**4
        DXDG_FAC  = 4*DXDG_PSI4*DXDG_DXDPSI_O_PSI
      ENDIF

      DXDG_DXDGXX = DXDCG_DXDCGXX*DXDG_PSI4 + DXDG_FAC*DXDG_GXX
      DXDG_DXDGXY = DXDCG_DXDCGXY*DXDG_PSI4 + DXDG_FAC*DXDG_GXY
      DXDG_DXDGXZ = DXDCG_DXDCGXZ*DXDG_PSI4 + DXDG_FAC*DXDG_GXZ
      DXDG_DXDGYY = DXDCG_DXDCGYY*DXDG_PSI4 + DXDG_FAC*DXDG_GYY
      DXDG_DXDGYZ = DXDCG_DXDCGYZ*DXDG_PSI4 + DXDG_FAC*DXDG_GYZ
      DXDG_DXDGZZ = DXDCG_DXDCGZZ*DXDG_PSI4 + DXDG_FAC*DXDG_GZZ
   
#endif

#ifdef CCODE

      DXDG_PSI4 = ((*conformal_state != CONFORMAL_METRIC)?1:DXDG_PSI*DXDG_PSI*DXDG_PSI*DXDG_PSI);

      DXDG_FAC  = ((*conformal_state != CONFORMAL_METRIC)?0:4*DXDG_PSI4*DXDG_DXDPSI_O_PSI);

      DXDG_DXDGXX = DXDCG_DXDCGXX*DXDG_PSI4 + DXDG_FAC*DXDG_GXX;
      DXDG_DXDGXY = DXDCG_DXDCGXY*DXDG_PSI4 + DXDG_FAC*DXDG_GXY;
      DXDG_DXDGXZ = DXDCG_DXDCGXZ*DXDG_PSI4 + DXDG_FAC*DXDG_GXZ;
      DXDG_DXDGYY = DXDCG_DXDCGYY*DXDG_PSI4 + DXDG_FAC*DXDG_GYY;
      DXDG_DXDGYZ = DXDCG_DXDCGYZ*DXDG_PSI4 + DXDG_FAC*DXDG_GYZ;
      DXDG_DXDGZZ = DXDCG_DXDCGZZ*DXDG_PSI4 + DXDG_FAC*DXDG_GZZ;

#endif

#endif
