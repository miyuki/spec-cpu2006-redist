/*@@
  @header   HAMADM_declare.h
  @date     Aug 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate vacuum part of Hamiltonian
  constraint
 
  @enddesc
@@*/

#ifndef HAMADM_DECLARE
#define HAMADM_DECLARE

#include "CactusEinstein/Einstein/src/macro/TRRICCI_declare.h"
#include "CactusEinstein/Einstein/src/macro/TRKK_declare.h"
#include "CactusEinstein/Einstein/src/macro/TRK_declare.h"

#ifdef FCODE

/* Output variables */ 
#undef  HAMADM_HAMADM
#define HAMADM_HAMADM hamadm_ham_adm

/* Declare output variables */
      CCTK_REAL HAMADM_HAMADM

#endif


#ifdef CCODE

/* Output variables */
#undef  HAMADM_HAMADM
#define HAMADM_HAMADM hamadm_ham_adm

/* Declare output variables */
      double HAMADM_HAMADM;

#endif

#endif
