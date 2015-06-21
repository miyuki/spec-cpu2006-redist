/*@@
  @header   UPPERMET_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the components of the upper 
  physical metric.
  @enddesc
@@*/

#ifndef UPPERMET_DECLARE
#define UPPERMET_DECLARE

#include "CactusEinstein/Einstein/src/macro/DETG_declare.h"

#ifdef FCODE

/* Output variables */
#undef  UPPERMET_UXX
#define UPPERMET_UXX uppermet_uxx
#undef  UPPERMET_UXY
#define UPPERMET_UXY uppermet_uxy
#undef  UPPERMET_UXZ
#define UPPERMET_UXZ uppermet_uxz
#undef  UPPERMET_UYY
#define UPPERMET_UYY uppermet_uyy
#undef  UPPERMET_UYZ
#define UPPERMET_UYZ uppermet_uyz
#undef  UPPERMET_UZZ
#define UPPERMET_UZZ uppermet_uzz

/* Temporary variables */

#undef  UPPERMET_PSI4DET
#define UPPERMET_PSI4DET uppermet_fdet

/* Declare internal variables */
      CCTK_REAL UPPERMET_PSI4DET

/* Declare output variables */
      CCTK_REAL UPPERMET_UXX, UPPERMET_UXY, UPPERMET_UXZ
      CCTK_REAL UPPERMET_UYY, UPPERMET_UYZ, UPPERMET_UZZ

#endif




#ifdef CCODE

/* Output variables */
#undef  UPPERMET_UXX
#define UPPERMET_UXX uppermet_uxx
#undef  UPPERMET_UXY
#define UPPERMET_UXY uppermet_uxy
#undef  UPPERMET_UXZ
#define UPPERMET_UXZ uppermet_uxz
#undef  UPPERMET_UYY
#define UPPERMET_UYY uppermet_uyy
#undef  UPPERMET_UYZ
#define UPPERMET_UYZ uppermet_uyz
#undef  UPPERMET_UZZ
#define UPPERMET_UZZ uppermet_uzz

/* Internal variables */
#undef  UPPERMET_PSI4DET 
#define UPPERMET_PSI4DET uppermet_psi4detg

/* Declare internal variables */
CCTK_REAL UPPERMET_PSI4DET;

/* Declare output variables */
CCTK_REAL UPPERMET_UXX;
CCTK_REAL UPPERMET_UXY;
CCTK_REAL UPPERMET_UXZ;
CCTK_REAL UPPERMET_UYY;
CCTK_REAL UPPERMET_UYZ;
CCTK_REAL UPPERMET_UZZ;

#endif

/* Symmetries */
#undef  UPPERMET_UYX
#define UPPERMET_UYX UPPERMET_UXY
#undef  UPPERMET_UZX
#define UPPERMET_UZX UPPERMET_UXZ
#undef  UPPERMET_UZY
#define UPPERMET_UZY UPPERMET_UYZ

#endif



