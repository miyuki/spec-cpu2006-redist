/*@@
  @header   TRRICCI_guts.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the trace of the 3-Ricci

  @enddesc
@@*/

#ifndef TRRICCI_GUTS
#define TRRICCI_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/RICCI_guts.h"

#ifdef FCODE

      TRRICCI_TRRICCI = UPPERMET_UXX*RICCI_RXX + UPPERMET_UYY*RICCI_RYY
     &      + UPPERMET_UZZ*RICCI_RZZ + 2D0*(UPPERMET_UXY*RICCI_RXY
     &      + UPPERMET_UXZ*RICCI_RXZ + UPPERMET_UYZ*RICCI_RYZ)
 
#endif

#ifdef CCODE

      TRRICCI_TRRICCI = UPPERMET_UXX*RICCI_RXX + UPPERMET_UYY*RICCI_RYY
           + UPPERMET_UZZ*RICCI_RZZ + 2D0*(UPPERMET_UXY*RICCI_RXY
           + UPPERMET_UXZ*RICCI_RXZ + UPPERMET_UYZ*RICCI_RYZ);

#endif

#endif
  
