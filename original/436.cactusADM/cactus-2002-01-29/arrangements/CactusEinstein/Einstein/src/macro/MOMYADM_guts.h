/*@@
  @header   MOMYADM_guts.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the spacetime part of the 
  y-Momentum Constraint. That is:

       Del_j K_y^j - Del_y trK 

  @enddesc
@@*/

#ifndef MOMYADM_GUTS
#define MOMYADM_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/CDK_guts.h"

#ifdef FCODE 

      MOMYADM_MOMYADM = (CDXCDK_CDXCDKXY - CDYCDK_CDYCDKXX)*UPPERMET_UXX 
     &  + (CDXCDK_CDXCDKYY - CDYCDK_CDYCDKXY)*UPPERMET_UXY + 
     &  (CDXCDK_CDXCDKYZ - 2*CDYCDK_CDYCDKXZ + CDZCDK_CDZCDKXY)*UPPERMET_UXZ + 
     &  (-CDYCDK_CDYCDKYZ + CDZCDK_CDZCDKYY)*UPPERMET_UYZ + 
     &  (-CDYCDK_CDYCDKZZ + CDZCDK_CDZCDKYZ)*UPPERMET_UZZ

#endif

#ifdef CCODE

      MOMYADM_MOMYADM = (CDXCDK_CDXCDKXY - CDYCDK_CDYCDKXX)*UPPERMET_UXX 
       + (CDXCDK_CDXCDKYY - CDYCDK_CDYCDKXY)*UPPERMET_UXY + 
       (CDXCDK_CDXCDKYZ - 2*CDYCDK_CDYCDKXZ + CDZCDK_CDZCDKXY)*UPPERMET_UXZ + 
       (-CDYCDK_CDYCDKYZ + CDZCDK_CDZCDKYY)*UPPERMET_UYZ + 
       (-CDYCDK_CDYCDKZZ + CDZCDK_CDZCDKYZ)*UPPERMET_UZZ;

#endif

#endif

