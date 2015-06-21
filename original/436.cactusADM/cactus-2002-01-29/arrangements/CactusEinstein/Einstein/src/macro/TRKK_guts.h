/*@@
  @header   TRKK_guts.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the trace of the K_lj K^j_i

  @enddesc
@@*/

#ifndef TRKK_GUTS
#define TRKK_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/KK_guts.h"

#ifdef FCODE

      TRKK_TRKK = UPPERMET_UXX*KK_KKXX + UPPERMET_UYY*KK_KKYY
     &      + UPPERMET_UZZ*KK_KKZZ + 2D0*(UPPERMET_UXY*KK_KKXY
     &      + UPPERMET_UXZ*KK_KKXZ + UPPERMET_UYZ*KK_KKYZ)
 
#endif

#ifdef CCODE
 
      TRKK_TRKK = UPPERMET_UXX*KK_KKXX + UPPERMET_UYY*KK_KKYY
           + UPPERMET_UZZ*KK_KKZZ + 2.0*(UPPERMET_UXY*KK_KKXY
           + UPPERMET_UXZ*KK_KKXZ + UPPERMET_UYZ*KK_KKYZ);
 
#endif

#endif
  
