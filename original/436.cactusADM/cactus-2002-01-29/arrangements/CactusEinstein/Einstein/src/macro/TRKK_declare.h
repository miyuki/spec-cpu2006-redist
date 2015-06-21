/*@@
  @header   TRKK_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate the trace of 
  K_lj K^j_i

  @enddesc
@@*/

#ifndef TRKK_DECLARE
#define TRKK_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"
#include "CactusEinstein/Einstein/src/macro/KK_declare.h"

#ifdef FCODE

/* Output variables */
#undef  TRKK_TRKK
#define TRKK_TRKK trkk_trkk

/* Declare output variables */
       CCTK_REAL TRKK_TRKK

#endif

#ifdef CCODE

/* Output variables */
#undef  TRKK_TRKK
#define TRKK_TRKK trkk_trkk

/* Declare output variables */
double TRKK_TRKK;

#endif

#endif
