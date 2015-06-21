/*@@
  @header   NABA_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the nabla operator acting on the lapse

  That is alpha_i^i
  @enddesc
@@*/

#ifndef NABA_DECLARE
#define NABA_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"
#include "CactusEinstein/Einstein/src/macro/CDCDA_declare.h"

/* Output variables */ 
#undef  NABA_NABA
#define NABA_NABA naba_naba

#ifdef FCODE

/* Declare output variables */
      CCTK_REAL NABA_NABA

#endif

#ifdef CCODE

/* Declare output variables */
      double  NABA_NABA;

#endif

#endif
