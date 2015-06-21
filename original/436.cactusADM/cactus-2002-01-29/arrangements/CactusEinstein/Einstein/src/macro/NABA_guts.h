/*@@
  @header   NABA_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the nabla operator acting on the lapse

  That is alpha_i^i

  @enddesc
@@*/

#ifndef NABA_GUTS
#define NABA_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/CDCDA_guts.h"

#ifdef FCODE 

      NABA_NABA = UPPERMET_UXX*CDCDA_CDXXDA+UPPERMET_UYY*CDCDA_CDYYDA
     & +UPPERMET_UZZ*CDCDA_CDZZDA+2*(UPPERMET_UXY*CDCDA_CDXYDA+UPPERMET_UXZ*CDCDA_CDXZDA
     & +UPPERMET_UYZ*CDCDA_CDYZDA)

#endif

#ifdef CCODE

      NABA_NABA = UPPERMET_UXX*CDCDA_CDXXDA+UPPERMET_UYY*CDCDA_CDYYDA
      +UPPERMET_UZZ*CDCDA_CDZZDA+2*(UPPERMET_UXY*CDCDA_CDXYDA+UPPERMET_UXZ*CDCDA_CDXZDA
      +UPPERMET_UYZ*CDCDA_CDYZDA);

#endif

#endif

