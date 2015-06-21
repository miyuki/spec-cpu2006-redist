/*@@
  @header   TRRICCI_guts.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate the trace of 3-Ricci tensor

  @enddesc
@@*/

#ifndef TRRICCI_DECLARE
#define TRRICCI_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"
#include "CactusEinstein/Einstein/src/macro/RICCI_declare.h"

#ifdef FCODE

/* Input variables */

/* Output variables */
#undef  TRRICCI_TRRICCI
#define TRRICCI_TRRICCI trricci_trricci

/* Declare output variables */
       CCTK_REAL TRRICCI_TRRICCI

#endif

#ifdef CCODE

/* Input variables */

/* Output variables */
#undef  TRRICCI_TRRICCI
#define TRRICCI_TRRICCI trricci_trricci

/* Declare output variables */
double TRRICCI_TRRICCI;

#endif

#endif
