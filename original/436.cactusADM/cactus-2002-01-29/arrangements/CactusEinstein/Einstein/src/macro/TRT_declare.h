/*@@
  @header   TRT_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Declarations for macro to calculate the trace of the 
  (4-)stress-energy tensor

  @enddesc
@@*/

#ifndef TRT_DECLARE
#define TRT_DECLARE

#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"

#ifdef FCODE

/* Input variables */
#undef  TRT_TTT
#define TRT_TTT Ttt
#undef  TRT_TTX
#define TRT_TTX Ttx
#undef  TRT_TTY
#define TRT_TTY Tty
#undef  TRT_TTZ
#define TRT_TTZ Ttz
#undef  TRT_TXX
#define TRT_TXX Txx
#undef  TRT_TXY
#define TRT_TXY Txy
#undef  TRT_TXZ
#define TRT_TXZ Txz
#undef  TRT_TYY
#define TRT_TYY Tyy
#undef  TRT_TYZ
#define TRT_TYZ Tyz
#undef  TRT_TZZ
#define TRT_TZZ Tzz
#undef  TRT_ALP
#define TRT_ALP alp(i,j,k)
#undef  TRT_BX
#define TRT_BX  betax(i,j,k)
#undef  TRT_BY
#define TRT_BY  betay(i,j,k)
#undef  TRT_BZ
#define TRT_BZ  betaz(i,j,k)

/* Temporary variables */
#undef  TRT_IALP2 
#define TRT_IALP2 trt_ialp2

/* Output variables */
#undef  TRT_TRT
#define TRT_TRT trt_trt

/* Declare temporary variables */
       CCTK_REAL TRT_IALP2

/* Declare output variables */
       CCTK_REAL TRT_TRT

#endif

#ifdef CCODE

/* Input variables */
#undef  TRT_TTT
#define TRT_TTT Ttt
#undef  TRT_TTX
#define TRT_TTX Ttx
#undef  TRT_TTY
#define TRT_TTY Tty
#undef  TRT_TTZ
#define TRT_TTZ Ttz
#undef  TRT_TXX
#define TRT_TXX Txx
#undef  TRT_TXY
#define TRT_TXY Txy
#undef  TRT_TXZ
#define TRT_TXZ Txz
#undef  TRT_TYY
#define TRT_TYY Tyy
#undef  TRT_TYZ
#define TRT_TYZ Tyz
#undef  TRT_TZZ
#define TRT_TZZ Tzz
#undef  TRT_ALP
#define TRT_ALP alp[ijk]
#undef  TRT_BX
#define TRT_BX  betax[ijk]
#undef  TRT_BY
#define TRT_BY  betay[ijk]
#undef  TRT_BZ
#define TRT_BZ  betaz[ijk]

/* Temporary variables */
#undef  TRT_IALP2 
#define TRT_IALP2 trt_ialp2

/* Output variables */
#undef  TRT_TRT
#define TRT_TRT trt_trt

/* Declare temporary variables */
       double TRT_IALP2;

/* Declare output variables */
       double TRT_TR;

#endif

#endif
