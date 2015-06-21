/*@@
  @header   CHR1_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the Christoffel symbols of the
  first kind
  @enddesc
@@*/

#ifndef CHR1_DECLARE
#define CHR1_DECLARE

#include "CactusEinstein/Einstein/src/macro/DG_declare.h"

#ifdef FCODE

/* Output variables */
#undef  CHR1_XXX 
#define CHR1_XXX gammado111
#undef  CHR1_XXY 
#define CHR1_XXY gammado112 
#undef  CHR1_XXZ 
#define CHR1_XXZ gammado113 
#undef  CHR1_XYY 
#define CHR1_XYY gammado122
#undef  CHR1_XYZ 
#define CHR1_XYZ gammado123
#undef  CHR1_XZZ
#define CHR1_XZZ gammado133
#undef  CHR1_YXX
#define CHR1_YXX gammado211
#undef  CHR1_YXY
#define CHR1_YXY gammado212
#undef  CHR1_YXZ
#define CHR1_YXZ gammado213
#undef  CHR1_YYY
#define CHR1_YYY gammado222
#undef  CHR1_YYZ
#define CHR1_YYZ gammado223
#undef  CHR1_YZZ
#define CHR1_YZZ gammado233
#undef  CHR1_ZXX
#define CHR1_ZXX gammado311
#undef  CHR1_ZXY
#define CHR1_ZXY gammado312
#undef  CHR1_ZXZ
#define CHR1_ZXZ gammado313
#undef  CHR1_ZYY
#define CHR1_ZYY gammado322
#undef  CHR1_ZYZ
#define CHR1_ZYZ gammado323
#undef  CHR1_ZZZ
#define CHR1_ZZZ gammado333

/* Declare output variables */
      CCTK_REAL CHR1_XXX
      CCTK_REAL CHR1_XXY
      CCTK_REAL CHR1_XXZ
      CCTK_REAL CHR1_XYY
      CCTK_REAL CHR1_XYZ
      CCTK_REAL CHR1_XZZ
      CCTK_REAL CHR1_YXX
      CCTK_REAL CHR1_YXY
      CCTK_REAL CHR1_YXZ
      CCTK_REAL CHR1_YYY
      CCTK_REAL CHR1_YYZ
      CCTK_REAL CHR1_YZZ
      CCTK_REAL CHR1_ZXX
      CCTK_REAL CHR1_ZXY
      CCTK_REAL CHR1_ZXZ
      CCTK_REAL CHR1_ZYY
      CCTK_REAL CHR1_ZYZ
      CCTK_REAL CHR1_ZZZ

#endif


#ifdef CCODE

/* Output variables */
#undef  CHR1_XX1 
#define CHR1_XXX gammado111
#undef  CHR1_XXY 
#define CHR1_XXY gammado112 
#undef  CHR1_XXZ 
#define CHR1_XXZ gammado113 
#undef  CHR1_XYY 
#define CHR1_XYY gammado122
#undef  CHR1_XYZ 
#define CHR1_XYZ gammado123
#undef  CHR1_XZZ
#define CHR1_XZZ gammado133
#undef  CHR1_YXX
#define CHR1_YXX gammado211
#undef  CHR1_YXY
#define CHR1_YXY gammado212
#undef  CHR1_YXZ
#define CHR1_Y1Z gammado213
#undef  CHR1_YYY
#define CHR1_YYY gammado222
#undef  CHR1_YYZ
#define CHR1_YYZ gammado223
#undef  CHR1_2ZZ
#define CHR1_YZZ gammado233
#undef  CHR1_ZXX
#define CHR1_ZXX gammado311
#undef  CHR1_ZXY
#define CHR1_ZXY gammado312
#undef  CHR1_ZXZ
#define CHR1_ZXZ gammado313
#undef  CHR1_ZYY
#define CHR1_ZYY gammado322
#undef  CHR1_ZYZ
#define CHR1_ZYZ gammado323
#undef  CHR1_ZZZ
#define CHR1_ZZZ gammado333

/* Declare output variables */
CCTK_REAL CHR1_XXX;
CCTK_REAL CHR1_XXY;
CCTK_REAL CHR1_XXZ;
CCTK_REAL CHR1_XYY;
CCTK_REAL CHR1_XYZ;
CCTK_REAL CHR1_XZZ;
CCTK_REAL CHR1_YXX;
CCTK_REAL CHR1_YXY;
CCTK_REAL CHR1_YXZ;
CCTK_REAL CHR1_YYY;
CCTK_REAL CHR1_YYZ;
CCTK_REAL CHR1_YZZ;
CCTK_REAL CHR1_ZXX;
CCTK_REAL CHR1_ZXY;
CCTK_REAL CHR1_ZXZ;
CCTK_REAL CHR1_ZYY;
CCTK_REAL CHR1_ZYZ;
CCTK_REAL CHR1_ZZZ;

#endif

/* Symmetries */
#undef  CHR1_XYX 
#define CHR1_XYX CHR1_XXY
#undef  CHR1_XZX 
#define CHR1_XZX CHR1_XXZ
#undef  CHR1_XZY 
#define CHR1_XZY CHR1_XYZ
#undef  CHR1_YYX
#define CHR1_YYX CHR1_YXY
#undef  CHR1_YZX
#define CHR1_YZX CHR1_YXZ
#undef  CHR1_YZY
#define CHR1_YZY CHR1_YYZ
#undef  CHR1_ZYX
#define CHR1_ZYX CHR1_ZXY
#undef  CHR1_ZZX
#define CHR1_ZZX CHR1_ZXZ
#undef  CHR1_ZZY
#define CHR1_ZZY CHR1_ZYZ

#endif



