/*@@
  @header   MOMZADM_guts.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the spacetime part of the 
  z-Momentum Constraint. That is:

       Del_j K_z^j - Del_z trK 

  @enddesc
@@*/

#ifndef MOMZADM_GUTS
#define MOMZADM_GUTS

#include "CactusEinstein/Einstein/src/macro/UPPERMET_guts.h"
#include "CactusEinstein/Einstein/src/macro/CDK_guts.h"

#ifdef FCODE 

      MOMZADM_MOMZADM = (CDXCDK_CDXCDKXZ - CDZCDK_CDZCDKXX)*UPPERMET_UXX + 
     &  (CDXCDK_CDXCDKYZ + CDYCDK_CDYCDKXZ - 2*CDZCDK_CDZCDKXY)*UPPERMET_UXY + 
     &  (CDXCDK_CDXCDKZZ - CDZCDK_CDZCDKXZ)*UPPERMET_UXZ + (CDYCDK_CDYCDKYZ - 
     &  CDZCDK_CDZCDKYY)*UPPERMET_UYY + (CDYCDK_CDYCDKZZ - CDZCDK_CDZCDKYZ)*UPPERMET_UYZ

#endif

#ifdef CCODE

      MOMZADM_MOMZADM = (CDXCDK_CDXCDKXZ - CDZCDK_CDZCDKXX)*UPPERMET_UXX + 
       (CDXCDK_CDXCDKYZ + CDYCDK_CDYCDKXZ - 2*CDZCDK_CDZCDKXY)*UPPERMET_UXY + 
       (CDXCDK_CDXCDKZZ - CDZCDK_CDZCDKXZ)*UPPERMET_UXZ + (CDYCDK_CDYCDKYZ - 
       CDZCDK_CDZCDKYY)*UPPERMET_UYY + (CDYCDK_CDYCDKZZ - CDZCDK_CDZCDKYZ)*UPPERMET_UYZ;

#endif

#endif

