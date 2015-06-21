/*@@
  @header   RICCI_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the components of the Ricci tensor. 

  Requires: Second derivatives of physical metric
            Christoffel symbols of first kind
            Christoffel symbols of second kind
            Upper physical metric

  Provides: Components of the physical Ricci tensor

  @enddesc
@@*/

#ifndef RICCI_DECLARE 

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"
#include "CactusEinstein/Einstein/src/macro/DDG_declare.h"
#include "CactusEinstein/Einstein/src/macro/CHR1_declare.h"
#include "CactusEinstein/Einstein/src/macro/CHR2_declare.h"

#ifdef FCODE

/* Output variables */ 
#undef  RICCI_RXX
#define RICCI_RXX ricci_R11
#undef  RICCI_RXY
#define RICCI_RXY ricci_R12
#undef  RICCI_RXZ
#define RICCI_RXZ ricci_R13
#undef  RICCI_RYY
#define RICCI_RYY ricci_R22
#undef  RICCI_RYZ
#define RICCI_RYZ ricci_R23
#undef  RICCI_RZZ
#define RICCI_RZZ ricci_R33

/* Declare output variables */
      CCTK_REAL RICCI_RXX
      CCTK_REAL RICCI_RXY
      CCTK_REAL RICCI_RXZ
      CCTK_REAL RICCI_RYY
      CCTK_REAL RICCI_RYZ
      CCTK_REAL RICCI_RZZ

#endif

#ifdef CCODE

/* Output variables */ 
#undef  RICCI_RXX
#define RICCI_RXX ricci_R11
#undef  RICCI_RXY
#define RICCI_RXY ricci_R12
#undef  RICCI_RXZ
#define RICCI_RXZ ricci_R13
#undef  RICCI_RYY
#define RICCI_RYY ricci_R22
#undef  RICCI_RYZ
#define RICCI_RYZ ricci_R23
#undef  RICCI_RZZ
#define RICCI_RZZ ricci_R33

/* Declare output variables */
double RICCI_RXX;
double RICCI_RXY;
double RICCI_RXZ;
double RICCI_RYY;
double RICCI_RYZ;
double RICCI_RZZ;

#endif

#define RICCI_DECLARE

#endif
