/*@@
  @header   TRK_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the trace of the extrinsic curvature

  The macro is defined in terms of standard variables in
  TRK_declare.h

  Requires: Upper physical metric
            Lower extrainsic curvature

  Provides: Trace of the extrinsic curvature

  @enddesc
@@*/

#ifndef TRK_GUTS
#define TRK_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"

#ifdef FCODE

      TRK_TRK = UPPERMET_UXX*TRK_HXX + UPPERMET_UYY*TRK_HYY
     &      + UPPERMET_UZZ*TRK_HZZ + 2D0*(UPPERMET_UXY*TRK_HXY
     &      + UPPERMET_UXZ*TRK_HXZ + UPPERMET_UYZ*TRK_HYZ)
 
#endif

#ifdef CCODE

      TRK_TRK = UPPERMET_UXX*TRK_HXX + UPPERMET_UYY*TRK_HYY
            + UPPERMET_UZZ*TRK_HZZ + 2*(UPPERMET_UXY*TRK_HXY
            + UPPERMET_UXZ*TRK_HXZ + UPPERMET_UYZ*TRK_HYZ);

#endif

#endif
  
