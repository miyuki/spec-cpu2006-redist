/*@@
  @header   DXYDG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the (first and) second derivatives of the 
  physical metric with respect to x,y

  The macro is defined in terms of standard variables in
  @seefile DXYDG_declare.h

  The macro uses @seefile DXDG_guts.h , @seefile DYDG_guts.h and 
  @seefile DXDG_declare.h , @seefile DYDG_declare.h
  @enddesc
@@*/

#ifndef DXYDG_GUTS
#define DXYDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDG_guts.h"
#include "CactusEinstein/Einstein/src/macro/DYDG_guts.h"

#ifdef FCODE 

      DXYDG_OO4DXDY = 1D0/(4D0*dx*dy)

      /* Factor involving 2nd derivative of conformal factor */ 
      IF (conformal_state /= CONFORMAL_METRIC) THEN
        DXYDG_FAC  = 0
      ELSE
        DXYDG_FAC  = DXDG_PSI4*(4*DXYDG_DXYDPSI_O_PSI + 12*DXDG_DXDPSI_O_PSI*DYDG_DYDPSI_O_PSI)
      ENDIF
      
      /* Now calculate the second deriatives */
      DXYDG_DXYDGXX = DYDCG_DYDCGXX*DXDG_FAC+DXDCG_DXDCGXX*DYDG_FAC+DXYDG_FAC*DXDG_GXX
     &+DXDG_PSI4*DXYDG_OO4DXDY*
     &(DXYDG_GXX_IPJP-DXYDG_GXX_IPJM-DXYDG_GXX_IMJP+DXYDG_GXX_IMJM);

      DXYDG_DXYDGXY = DYDCG_DYDCGXY*DXDG_FAC+DXDCG_DXDCGXY*DYDG_FAC+DXYDG_FAC*DXDG_GXY
     &+DXDG_PSI4*DXYDG_OO4DXDY*
     &(DXYDG_GXY_IPJP-DXYDG_GXY_IPJM-DXYDG_GXY_IMJP+DXYDG_GXY_IMJM);

      DXYDG_DXYDGXZ = DYDCG_DYDCGXZ*DXDG_FAC+DXDCG_DXDCGXZ*DYDG_FAC+DXYDG_FAC*DXDG_GXZ
     &+DXDG_PSI4*DXYDG_OO4DXDY*
     &(DXYDG_GXZ_IPJP-DXYDG_GXZ_IPJM-DXYDG_GXZ_IMJP+DXYDG_GXZ_IMJM);

      DXYDG_DXYDGYY = DYDCG_DYDCGYY*DXDG_FAC+DXDCG_DXDCGYY*DYDG_FAC+DXYDG_FAC*DXDG_GYY
     &+DXDG_PSI4*DXYDG_OO4DXDY*
     &(DXYDG_GYY_IPJP-DXYDG_GYY_IPJM-DXYDG_GYY_IMJP+DXYDG_GYY_IMJM);

      DXYDG_DXYDGYZ = DYDCG_DYDCGYZ*DXDG_FAC+DXDCG_DXDCGYZ*DYDG_FAC+DXYDG_FAC*DXDG_GYZ
     &+DXDG_PSI4*DXYDG_OO4DXDY*
     &(DXYDG_GYZ_IPJP-DXYDG_GYZ_IPJM-DXYDG_GYZ_IMJP+DXYDG_GYZ_IMJM);

      DXYDG_DXYDGZZ = DYDCG_DYDCGZZ*DXDG_FAC+DXDCG_DXDCGZZ*DYDG_FAC+DXYDG_FAC*DXDG_GZZ
     &+DXDG_PSI4*DXYDG_OO4DXDY*
     &(DXYDG_GZZ_IPJP-DXYDG_GZZ_IPJM-DXYDG_GZZ_IMJP+DXYDG_GZZ_IMJM);

#endif

#ifdef CCODE

/* Factor involving 2nd derivative of conformal factor */ 
      DXYDG_FAC   = ((*conformal_state != CONFORMAL_METRIC)?0:
              DXDG_PSI4*(4*DXYDG_DXYDPSI_O_PSI + 12*DXDG_DXDPSI_O_PSI*DYDG_DYDPSI_O_PSI));

/* Now calculate the second deriatives */
      DXYDG_DXYDGXX = DYDCG_DYDCGXX*DXDG_FAC+DXDCG_DXDCGXX*DYDG_FAC+DXYDG_FAC*DXDG_GXX
               +DXDG_PSI4*DXYDG_OO4DXDY*
               (DXYDG_GXX_IPJP-DXYDG_GXX_IPJM-DXYDG_GXX_IMJP+DXYDG_GXX_IMJM);

      DXYDG_DXYDGXY = DYDCG_DYDCGXY*DXDG_FAC+DXDCG_DXDCGXY*DYDG_FAC+DXYDG_FAC*DXDG_GXY
               +DXDG_PSI4*DXYDG_OO4DXDY*
               (DXYDG_GXY_IPJP-DXYDG_GXY_IPJM-DXYDG_GXY_IMJP+DXYDG_GXY_IMJM);

      DXYDG_DXYDGXZ = DYDCG_DYDCGXZ*DXDG_FAC+DXDCG_DXDCGXZ*DYDG_FAC+DXYDG_FAC*DXDG_GXZ
               +DXDG_PSI4*DXYDG_OO4DXDY*
               (DXYDG_GXZ_IPJP-DXYDG_GXZ_IPJM-DXYDG_GXZ_IMJP+DXYDG_GXZ_IMJM);

      DXYDG_DXYDGYY = DYDCG_DYDCGYY*DXDG_FAC+DXDCG_DXDCGYY*DYDG_FAC+DXYDG_FAC*DXDG_GYY
               +DXDG_PSI4*DXYDG_OO4DXDY*
               (DXYDG_GYY_IPJP-DXYDG_GYY_IPJM-DXYDG_GYY_IMJP+DXYDG_GYY_IMJM);

      DXYDG_DXYDGYZ = DYDCG_DYDCGYZ*DXDG_FAC+DXDCG_DXDCGYZ*DYDG_FAC+DXYDG_FAC*DXDG_GYZ
               +DXDG_PSI4*DXYDG_OO4DXDY*
               (DXYDG_GYZ_IPJP-DXYDG_GYZ_IPJM-DXYDG_GYZ_IMJP+DXYDG_GYZ_IMJM);

      DXYDG_DXYDGZZ = DYDCG_DYDCGZZ*DXDG_FAC+DXDCG_DXDCGZZ*DYDG_FAC+DXYDG_FAC*DXDG_GZZ
               +DXDG_PSI4*DXYDG_OO4DXDY*
               (DXYDG_GZZ_IPJP-DXYDG_GZZ_IPJM-DXYDG_GZZ_IMJP+DXYDG_GZZ_IMJM);

#endif

#endif