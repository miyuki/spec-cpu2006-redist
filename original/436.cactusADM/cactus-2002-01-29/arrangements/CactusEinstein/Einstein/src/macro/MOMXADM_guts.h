/*@@
  @header   MOMXADM_guts.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the spacetime part of the 
  x-Momentum Constraint. That is:

       Del_j K_x^j - Del_x trK 

  @enddesc
@@*/

#ifndef MOMXADM_GUTS
#define MOMXADM_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/CDK_guts.h"

#ifdef FCODE 

      MOMXADM_MOMXADM = (-CDXCDK_CDXCDKXY + CDYCDK_CDYCDKXX)*UPPERMET_UXY 
     & + (-CDXCDK_CDXCDKXZ + CDZCDK_CDZCDKXX)*UPPERMET_UXZ + 
     & (-CDXCDK_CDXCDKYY + CDYCDK_CDYCDKXY)*UPPERMET_UYY + 
     & (-2*CDXCDK_CDXCDKYZ + CDYCDK_CDYCDKXZ + CDZCDK_CDZCDKXY)*
     & UPPERMET_UYZ + (-CDXCDK_CDXCDKZZ + CDZCDK_CDZCDKXZ)*UPPERMET_UZZ

#endif

#ifdef CCODE

      MOMXADM_MOMXADM = (-CDXCDK_CDXCDKXY + CDYCDK_CDYCDKXX)*UPPERMET_UXY 
      + (-CDXCDK_CDXCDKXZ + CDZCDK_CDZCDKXX)*UPPERMET_UXZ + 
      (-CDXCDK_CDXCDKYY + CDYCDK_CDYCDKXY)*UPPERMET_UYY + 
      (-2*CDXCDK_CDXCDKYZ + CDYCDK_CDYCDKXZ + CDZCDK_CDZCDKXY)*
      UPPERMET_UYZ + (-CDXCDK_CDXCDKZZ + CDZCDK_CDZCDKXZ)*UPPERMET_UZZ;

#endif

#endif

