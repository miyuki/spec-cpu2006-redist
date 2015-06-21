/*@@
  @header   DXXDG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the (first and) second derivatives of the 
  physical metric with respect to x

  The macro is defined in terms of standard variables in
  @seefile DXXDG_declare.h

  The macro uses @seefile DXDG_guts.h and @seefile DXDG_declare.h
  @enddesc
@@*/

#ifndef DXXDG_GUTS
#define DXXDG_GUTS

#include "CactusEinstein/Einstein/src/macro/DXDG_guts.h"

#ifdef FCODE 

      DXXDG_OODX2 = 1D0/(cctk_delta_space(1)*cctk_delta_space(1))

      /* Factor involving 2nd derivative of conformal factor */ 
      IF (conformal_state /= CONFORMAL_METRIC) THEN
        DXXDG_FAC  = 0
      ELSE
        DXXDG_FAC  = DXDG_PSI4*(4*DXXDG_DXXDPSI_O_PSI + 12*DXDG_DXDPSI_O_PSI*DXDG_DXDPSI_O_PSI)
      ENDIF
      
      /* Now calculate the second derivatives */
      DXXDG_DXXDGXX = 2*DXDCG_DXDCGXX*DXDG_FAC+DXXDG_FAC*DXDG_GXX+DXDG_PSI4
     &  *DXXDG_OODX2*(DXDCG_GXX_IP-2*DXDG_GXX+DXDCG_GXX_IM)

      DXXDG_DXXDGXY = 2*DXDCG_DXDCGXY*DXDG_FAC+DXXDG_FAC*DXDG_GXY+DXDG_PSI4
     & *DXXDG_OODX2*(DXDCG_GXY_IP-2*DXDG_GXY+DXDCG_GXY_IM)

      DXXDG_DXXDGXZ = 2*DXDCG_DXDCGXZ*DXDG_FAC+DXXDG_FAC*DXDG_GXZ+DXDG_PSI4
     &  *DXXDG_OODX2*(DXDCG_GXZ_IP-2*DXDG_GXZ+DXDCG_GXZ_IM)

      DXXDG_DXXDGYY = 2*DXDCG_DXDCGYY*DXDG_FAC+DXXDG_FAC*DXDG_GYY+DXDG_PSI4
     &  *DXXDG_OODX2*(DXDCG_GYY_IP-2*DXDG_GYY+DXDCG_GYY_IM)

      DXXDG_DXXDGYZ = 2*DXDCG_DXDCGYZ*DXDG_FAC+DXXDG_FAC*DXDG_GYZ+DXDG_PSI4
     &  *DXXDG_OODX2*(DXDCG_GYZ_IP-2*DXDG_GYZ+DXDCG_GYZ_IM)

      DXXDG_DXXDGZZ = 2*DXDCG_DXDCGZZ*DXDG_FAC+DXXDG_FAC*DXDG_GZZ+DXDG_PSI4
     &  *DXXDG_OODX2*(DXDCG_GZZ_IP-2*DXDG_GZZ+DXDCG_GZZ_IM)

#endif

#ifdef CCODE

/* Factor involving 2nd derivative of conformal factor */ 
DXXDG_FAC   = ((*conformal_state != CONFORMAL_METRIC)?0:
              DXDG_PSI4*(4*DXXDG_DXXDPSI_O_PSI + 12*DXDG_DXDPSI_O_PSI*DXDG_DXDPSI_O_PSI));

/* Now calculate the second derivatives */
      DXXDG_DXXDGXX = 2*DXDCG_DXDCGXX*DXDG_FAC+DXXDG_FAC*DXDG_GXX+DXDG_PSI4
        *DXXDG_OODX2*(DXDCG_GXX_IP-2*DXDG_GXX+DXDCG_GXX_IM);

      DXXDG_DXXDGXY = 2*DXDCG_DXDCGXY*DXDG_FAC+DXXDG_FAC*DXDG_GXY+DXDG_PSI4
        *DXXDG_OODX2*(DXDCG_GXY_IP-2*DXDG_GXY+DXDCG_GXY_IM);

      DXXDG_DXXDGXZ = 2*DXDCG_DXDCGXZ*DXDG_FAC+DXXDG_FAC*DXDG_GXZ+DXDG_PSI4
        *DXXDG_OODX2*(DXDCG_GXZ_IP-2*DXDG_GXZ+DXDCG_GXZ_IM);

      DXXDG_DXXDGYY = 2*DXDCG_DXDCGYY*DXDG_FAC+DXXDG_FAC*DXDG_GYY+DXDG_PSI4
        *DXXDG_OODX2*(DXDCG_GYY_IP-2*DXDG_GYY+DXDCG_GYY_IM);

      DXXDG_DXXDGYZ = 2*DXDCG_DXDCGYZ*DXDG_FAC+DXXDG_FAC*DXDG_GYZ+DXDG_PSI4
        *DXXDG_OODX2*(DXDCG_GYZ_IP-2*DXDG_GYZ+DXDCG_GYZ_IM);

      DXXDG_DXXDGZZ = 2*DXDCG_DXDCGZZ*DXDG_FAC+DXXDG_FAC*DXDG_GZZ+DXDG_PSI4
        *DXXDG_OODX2*(DXDCG_GZZ_IP-2*DXDG_GZZ+DXDCG_GZZ_IM);
 

#endif

#endif
