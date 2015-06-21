/*@@
  @header   STRESSENERGY_guts.h
  @date     Nov 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the hydro quantities 
  scheme
  @enddesc
@@*/

#ifndef STRESSENERGY_GUTS
#define STRESSENERGY_GUTS

c    These geometrical quantities are supplied for CalcTmunu

#include "CactusEinstein/Einstein/src/macro/DETG_guts.h"
#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"

      uxx = UPPERMET_UXX; uyy = UPPERMET_UYY; uzz = UPPERMET_UZZ
      uxy = UPPERMET_UXY; uxz = UPPERMET_UXZ; uyz = UPPERMET_UYZ

      detg = DETG_DETCG

c    Zero the stress-energy tensor

      STRESSENERGY_TTT = 0.0D0; STRESSENERGY_TTX = 0.0D0
      STRESSENERGY_TTY = 0.0D0; STRESSENERGY_TTZ = 0.0D0
      STRESSENERGY_TXX = 0.0D0; STRESSENERGY_TXY = 0.0D0 
      STRESSENERGY_TXZ = 0.0D0; STRESSENERGY_TYY = 0.0D0
      STRESSENERGY_TYZ = 0.0D0; STRESSENERGY_TZZ = 0.0D0

      
c    Calculate the stress-energy tensor
#include "CalcTmunu.inc"


c    Calculate the trace of the stress-energy tensor
#include "CactusEinstein/Einstein/src/macro/TRT_guts.h"

#endif
