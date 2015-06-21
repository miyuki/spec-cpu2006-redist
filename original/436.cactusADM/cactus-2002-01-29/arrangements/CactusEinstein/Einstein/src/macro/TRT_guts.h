/*@@
  @header   TRT_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the trace of the (4-)stress energy tensor

  @enddesc
@@*/

#ifndef TRT_GUTS
#define TRT_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"

#ifdef FCODE

      TRT_IALP2 = 1D0/TRT_ALP**2

      TRT_TRT = -TRT_TTT*TRT_IALP2+TRT_TXX*UPPERMET_UXX+
     &    TRT_TYY*UPPERMET_UYY+TRT_TZZ*UPPERMET_UZZ+2D0*(
     &    TRT_TXY*UPPERMET_UXY+TRT_TXZ*UPPERMET_UXZ+TRT_TYZ*
     &    UPPERMET_UYZ)


      if (shift_state /= SHIFT_INACTIVE) then
       
        TRT_TRT = TRT_TRT - TRT_IALP2*(TRT_TXX*TRT_BX*TRT_BX
     &   + TRT_TYY*TRT_BY*TRT_BY + TRT_TZZ*TRT_BZ*TRT_BZ
     &   + 2D0*(TRT_TXY*TRT_BX*TRT_BY + TRT_TXZ*TRT_BX*TRT_BZ
     &   + TRT_TYZ*TRT_BY*TRT_BZ -TRT_TTX*TRT_BX -TRT_TTY*TRT_BY
     &   -TRT_TTZ*TRT_BZ))

      endif
 
#endif

#ifdef CCODE

      TRT_IALP2 = 1D0/TRT_ALP**2;

      TRT_TRT = -TRT_TTT*TRT_IALP2+TRT_TXX*UPPERMET_UXX+
         TRT_TYY*UPPERMET_UYY+TRT_TZZ*UPPERMET_UZZ+2D0*(
         TRT_TXY*UPPERMET_UXY+TRT_TXZ*UPPERMET_UXZ+TRT_TYZ*
         UPPERMET_UYZ)


      if (shift) 
      {
        TRT_TRT = TRT_TRT - TRT_IALP2*(TRT_TXX*TRT_BX*TRT_BX
        + TRT_TYY*TRT_BY*TRT_BY + TRT_TZZ*TRT_BZ*TRT_BZ
        + 2D0*(TRT_TXY*TRT_BX*TRT_BY + TRT_TXZ*TRT_BX*TRT_BZ
        + TRT_TYZ*TRT_BY*TRT_BZ -TRT_TTX*TRT_BX -TRT_TTY*TRT_BY
        -TRT_TTZ*TRT_BZ))

      }

#endif

#endif
  
