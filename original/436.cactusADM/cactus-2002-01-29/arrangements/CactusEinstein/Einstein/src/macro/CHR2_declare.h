/*@@
  @header   CHR2_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the Christoffel symbols of the
  second kind

  That is CHR2_cab =  g^cd CHR1_dab

                   =  1/2 g^cd (g_ad,b + gbd,a - gab,d)

  Requires: Christoffel symbols of first kind
            Uppermetric

  Provides: Christoffel sybols of second kind

  @enddesc
@@*/

#ifndef CHR2_DECLARE
#define CHR2_DECLARE

#include "CactusEinstein/Einstein/src/macro/CHR1_declare.h"
#include "CactusEinstein/Einstein/src/macro/UPPERMET_declare.h"

#ifdef FCODE

/* Output variables */
#undef  CHR2_XXX 
#define CHR2_XXX gamma111
#undef  CHR2_XXY 
#define CHR2_XXY gamma112 
#undef  CHR2_XXZ 
#define CHR2_XXZ gamma113 
#undef  CHR2_XYY 
#define CHR2_XYY gamma122
#undef  CHR2_XYZ 
#define CHR2_XYZ gamma123
#undef  CHR2_XZZ
#define CHR2_XZZ gamma133
#undef  CHR2_YXX
#define CHR2_YXX gamma211
#undef  CHR2_YXY
#define CHR2_YXY gamma212
#undef  CHR2_YXZ
#define CHR2_YXZ gamma213
#undef  CHR2_YYY
#define CHR2_YYY gamma222
#undef  CHR2_YYZ
#define CHR2_YYZ gamma223
#undef  CHR2_YZZ
#define CHR2_YZZ gamma233
#undef  CHR2_ZXX
#define CHR2_ZXX gamma311
#undef  CHR2_ZXY
#define CHR2_ZXY gamma312
#undef  CHR2_ZXZ
#define CHR2_ZXZ gamma313
#undef  CHR2_ZYY
#define CHR2_ZYY gamma322
#undef  CHR2_ZYZ
#define CHR2_ZYZ gamma323
#undef  CHR2_ZZZ
#define CHR2_ZZZ gamma333

/* Declare output variables */
      CCTK_REAL CHR2_XXX
      CCTK_REAL CHR2_XXY
      CCTK_REAL CHR2_XXZ
      CCTK_REAL CHR2_XYY
      CCTK_REAL CHR2_XYZ
      CCTK_REAL CHR2_XZZ
      CCTK_REAL CHR2_YXX
      CCTK_REAL CHR2_YXY
      CCTK_REAL CHR2_YXZ
      CCTK_REAL CHR2_YYY
      CCTK_REAL CHR2_YYZ
      CCTK_REAL CHR2_YZZ
      CCTK_REAL CHR2_ZXX
      CCTK_REAL CHR2_ZXY
      CCTK_REAL CHR2_ZXZ
      CCTK_REAL CHR2_ZYY
      CCTK_REAL CHR2_ZYZ
      CCTK_REAL CHR2_ZZZ

#endif


#ifdef CCODE

/* Output variables */
#undef  CHR2_XXX 
#define CHR2_XXX gamma111
#undef  CHR2_XXY 
#define CHR2_XXY gamma112 
#undef  CHR2_XXZ 
#define CHR2_XXZ gamma113 
#undef  CHR2_XYY 
#define CHR2_XYY gamma122
#undef  CHR2_XYZ 
#define CHR2_XYZ gamma123
#undef  CHR2_XZZ
#define CHR2_XZZ gamma133
#undef  CHR2_YXX
#define CHR2_YXX gamma211
#undef  CHR2_YXY
#define CHR2_YXY gamma212
#undef  CHR2_YXZ
#define CHR2_YXZ gamma213
#undef  CHR2_YYY
#define CHR2_YYY gamma222
#undef  CHR2_YYZ
#define CHR2_YYZ gamma223
#undef  CHR2_YZZ
#define CHR2_YZZ gamma233
#undef  CHR2_ZXX
#define CHR2_ZXX gamma311
#undef  CHR2_ZXY
#define CHR2_ZXY gamma312
#undef  CHR2_ZXZ
#define CHR2_ZXZ gamma313
#undef  CHR2_ZYY
#define CHR2_ZYY gamma322
#undef  CHR2_ZYZ
#define CHR2_ZYZ gamma323
#undef  CHR2_ZZZ
#define CHR2_ZZZ gamma333

/* Declare output variables */
CCTK_REAL CHR2_XXX;
CCTK_REAL CHR2_XXY;
CCTK_REAL CHR2_XXZ;
CCTK_REAL CHR2_XYY;
CCTK_REAL CHR2_XYZ;
CCTK_REAL CHR2_XZZ;
CCTK_REAL CHR2_YXX;
CCTK_REAL CHR2_YXY;
CCTK_REAL CHR2_YXZ;
CCTK_REAL CHR2_YYY;
CCTK_REAL CHR2_YYZ;
CCTK_REAL CHR2_YZZ;
CCTK_REAL CHR2_ZXX;
CCTK_REAL CHR2_ZXY;
CCTK_REAL CHR2_ZXZ;
CCTK_REAL CHR2_ZYY;
CCTK_REAL CHR2_ZYZ;
CCTK_REAL CHR2_ZZZ;

#endif

/* Symmetries */
#undef  CHR2_XYX 
#define CHR2_XYX CHR2_XXY
#undef  CHR2_XZX 
#define CHR2_XZX CHR2_XXZ
#undef  CHR2_XZY 
#define CHR2_XZY CHR2_XYZ
#undef  CHR2_YYX
#define CHR2_YYX CHR2_YXY
#undef  CHR2_YZX
#define CHR2_YZX CHR2_YXZ
#undef  CHR2_YZY
#define CHR2_YZY CHR2_YYZ
#undef  CHR2_ZYX
#define CHR2_ZYX CHR2_ZXY
#undef  CHR2_ZZX
#define CHR2_ZZX CHR2_ZXZ
#undef  CHR2_ZZY
#define CHR2_ZZY CHR2_ZYZ

#endif



