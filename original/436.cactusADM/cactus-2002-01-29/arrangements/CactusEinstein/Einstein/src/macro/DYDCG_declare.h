/*@@
  @header   DYDCG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DYDCG_guts.h to compute first 
  derivatives of the conformal metric with respect to y
  @enddesc
@@*/

#ifndef DYDCG_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DYDCG_GXX_JP 
#define DYDCG_GXX_JP gxx(i,j+1,k)
#undef  DYDCG_GXX_JM
#define DYDCG_GXX_JM gxx(i,j-1,k)
#undef  DYDCG_GXY_JP 
#define DYDCG_GXY_JP gxy(i,j+1,k)
#undef  DYDCG_GXY_JM 
#define DYDCG_GXY_JM gxy(i,j-1,k)
#undef  DYDCG_GXZ_JP 
#define DYDCG_GXZ_JP gxz(i,j+1,k)
#undef  DYDCG_GXZ_JM
#define DYDCG_GXZ_JM gxz(i,j-1,k)
#undef  DYDCG_GYY_JP 
#define DYDCG_GYY_JP gyy(i,j+1,k)
#undef  DYDCG_GYY_JM 
#define DYDCG_GYY_JM gyy(i,j-1,k)
#undef  DYDCG_GYZ_JP 
#define DYDCG_GYZ_JP gyz(i,j+1,k)
#undef  DYDCG_GYZ_JM 
#define DYDCG_GYZ_JM gyz(i,j-1,k)
#undef  DYDCG_GZZ_JP 
#define DYDCG_GZZ_JP gzz(i,j+1,k)
#undef  DYDCG_GZZ_JM
#define DYDCG_GZZ_JM gzz(i,j-1,k)

/* Output variables */ 
#undef  DYDCG_DYDCGXX
#define DYDCG_DYDCGXX  delgb211
#undef  DYDCG_DYDCGXY
#define DYDCG_DYDCGXY  delgb212
#undef  DYDCG_DYDCGXZ
#define DYDCG_DYDCGXZ  delgb213
#undef  DYDCG_DYDCGYY
#define DYDCG_DYDCGYY  delgb222
#undef  DYDCG_DYDCGYZ
#define DYDCG_DYDCGYZ  delgb223
#undef  DYDCG_DYDCGZZ
#define DYDCG_DYDCGZZ  delgb233

/* Internal variables */
#undef  DYDCG_DY
#define DYDCG_DY    dy
#undef  DYDCG_OO2DY   
#define DYDCG_OO2DY cdydg_oo2dy

/* Declare internal variables */
      CCTK_REAL DYDCG_OO2DY

/* Declare output variables */
      CCTK_REAL DYDCG_DYDCGXX
      CCTK_REAL DYDCG_DYDCGXY
      CCTK_REAL DYDCG_DYDCGXZ
      CCTK_REAL DYDCG_DYDCGYY
      CCTK_REAL DYDCG_DYDCGYZ
      CCTK_REAL DYDCG_DYDCGZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DYDCG_GXX_JP 
#define DYDCG_GXX_JP gxx[ dj+ijk]
#undef  DYDCG_GXX_JM
#define DYDCG_GXX_JM gxx[-dj+ijk]
#undef  DYDCG_GXY_JP 
#define DYDCG_GXY_JP gxy[ dj+ijk]
#undef  DYDCG_GXY_JM 
#define DYDCG_GXY_JM gxy[-dj+ijk]
#undef  DYDCG_GXZ_JP 
#define DYDCG_GXZ_JP gxz[ dj+ijk]
#undef  DYDCG_GXZ_JM
#define DYDCG_GXZ_JM gxz[-dj+ijk]
#undef  DYDCG_GYY_JP 
#define DYDCG_GYY_JP gyy[ dj+ijk]
#undef  DYDCG_GYY_JM 
#define DYDCG_GYY_JM gyy[-dj+ijk]
#undef  DYDCG_GYZ_JP 
#define DYDCG_GYZ_JP gyz[ dj+ijk]
#undef  DYDCG_GYZ_JM 
#define DYDCG_GYZ_JM gyz[-dj+ijk]
#undef  DYDCG_GZZ_JP 
#define DYDCG_GZZ_JP gzz[ dj+ijk]
#undef  DYDCG_GZZ_JM
#define DYDCG_GZZ_JM gzz[-dj+ijk]

/* Output variables */ 
#undef  DYDCG_DYDCGXX
#define DYDCG_DYDCGXX  delgb211
#undef  DYDCG_DYDCGXY
#define DYDCG_DYDCGXY  delgb212
#undef  DYDCG_DYDCGXZ
#define DYDCG_DYDCGXZ  delgb213
#undef  DYDCG_DYDCGYY
#define DYDCG_DYDCGYY  delgb222
#undef  DYDCG_DYDCGYZ
#define DYDCG_DYDCGYZ  delgb223
#undef  DYDCG_DYDCGZZ
#define DYDCG_DYDCGZZ  delgb233

/* Internal variables */
#undef  DYDCG_OO2DY   
#define DYDCG_OO2DY cdydg_oo2dy

/* Declare internal variables */
double DYDCG_OO2DY;

/* Declare output variables */
double DYDCG_DYDCGXX;
double DYDCG_DYDCGXY;
double DYDCG_DYDCGXZ;
double DYDCG_DYDCGYY;
double DYDCG_DYDCGYZ;
double DYDCG_DYDCGZZ;

#endif

#define DYDCG_DECLARE

#endif

