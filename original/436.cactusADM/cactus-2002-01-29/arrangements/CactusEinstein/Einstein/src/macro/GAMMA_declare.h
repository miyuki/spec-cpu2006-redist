/*@@
  @header   GAMMA_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate Gauge Source Functions (Gammas)

  That is GAMMA_a = g^bc CHR2_abc
  @enddesc
@@*/

#ifndef GAMMA_DECLARE
#define GAMMA_DECLARE

#include "CactusEinstein/Einstein/src/macro/CHR2_declare.h"
#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"

#ifdef FCODE

/* Output variables */
#undef  GAMMA_GAMMAX 
#define GAMMA_GAMMAX gamma_gammax
#undef  GAMMA_GAMMAY
#define GAMMA_GAMMAY gamma_gammay
#undef  GAMMA_GAMMAZ
#define GAMMA_GAMMAZ gamma_gammaz

/* Declare output variables */
      CCTK_REAL GAMMA_GAMMAX
      CCTK_REAL GAMMA_GAMMAY
      CCTK_REAL GAMMA_GAMMAZ

#endif


#ifdef CCODE

/* Output variables */
#undef  GAMMA_GAMMAX 
#define GAMMA_GAMMAX gamma_gammax
#undef  GAMMA_GAMMAY
#define GAMMA_GAMMAY gamma_gammay
#undef  GAMMA_GAMMAZ
#define GAMMA_GAMMAZ gamma_gammaz

/* Declare output variables */
double GAMMA_GAMMAX;
double GAMMA_GAMMAY;
double GAMMA_GAMMAZ;

#endif

#endif



