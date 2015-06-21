/*@@
  @header  WAVEG_declare.h
  @date     Nov 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate wave operator acting on the metric 

  That is g^lm g_ij,lm

  @enddesc
@@*/

#ifndef WAVEG_DECLARE
#define WAVEG_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"
#include "CactusEinstein/Einstein/src/macro/DDG_declare.h"

/* Output variables */ 
#undef  WAVEG_DDGXX
#define WAVEG_DDGXX waveg_ddgxx
#undef  WAVEG_DDGXY
#define WAVEG_DDGXY waveg_ddgxy
#undef  WAVEG_DDGXZ
#define WAVEG_DDGXZ waveg_ddgxz
#undef  WAVEG_DDGYY
#define WAVEG_DDGYY waveg_ddgyy
#undef  WAVEG_DDGYZ
#define WAVEG_DDGYZ waveg_ddgyz
#undef  WAVEG_DDGZZ
#define WAVEG_DDGZZ waveg_ddgzz

#ifdef FCODE

      CCTK_REAL WAVEG_DDGXX
      CCTK_REAL WAVEG_DDGXY
      CCTK_REAL WAVEG_DDGXZ
      CCTK_REAL WAVEG_DDGYY
      CCTK_REAL WAVEG_DDGYZ
      CCTK_REAL WAVEG_DDGZZ

#endif

#ifdef CCODE

      double WAVEG_DDGXX
      double WAVEG_DDGXY
      double WAVEG_DDGXZ
      double WAVEG_DDGYY
      double WAVEG_DDGYZ
      double WAVEG_DDGZZ

#endif

#endif

