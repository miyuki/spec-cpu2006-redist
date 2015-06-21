/*@@
  @header   DZDCG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DZDCG_guts.h to compute first 
  derivatives of the conformal metric with respect to z
  @enddesc
@@*/

#ifndef DZDCG_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DZDCG_GXX_KP 
#define DZDCG_GXX_KP gxx(i,j,k+1)
#undef  DZDCG_GXX_KM
#define DZDCG_GXX_KM gxx(i,j,k-1)
#undef  DZDCG_GXY_KP 
#define DZDCG_GXY_KP gxy(i,j,k+1)
#undef  DZDCG_GXY_KM 
#define DZDCG_GXY_KM gxy(i,j,k-1)
#undef  DZDCG_GXZ_KP 
#define DZDCG_GXZ_KP gxz(i,j,k+1)
#undef  DZDCG_GXZ_KM
#define DZDCG_GXZ_KM gxz(i,j,k-1)
#undef  DZDCG_GYY_KP 
#define DZDCG_GYY_KP gyy(i,j,k+1)
#undef  DZDCG_GYY_KM 
#define DZDCG_GYY_KM gyy(i,j,k-1)
#undef  DZDCG_GYZ_KP 
#define DZDCG_GYZ_KP gyz(i,j,k+1)
#undef  DZDCG_GYZ_KM 
#define DZDCG_GYZ_KM gyz(i,j,k-1)
#undef  DZDCG_GZZ_KP 
#define DZDCG_GZZ_KP gzz(i,j,k+1)
#undef  DZDCG_GZZ_KM
#define DZDCG_GZZ_KM gzz(i,j,k-1)

/* Output variables */ 
#undef  DZDCG_DZDCGXX
#define DZDCG_DZDCGXX  delgb311
#undef  DZDCG_DZDCGXY
#define DZDCG_DZDCGXY  delgb312
#undef  DZDCG_DZDCGXZ
#define DZDCG_DZDCGXZ  delgb313
#undef  DZDCG_DZDCGYY
#define DZDCG_DZDCGYY  delgb322
#undef  DZDCG_DZDCGYZ
#define DZDCG_DZDCGYZ  delgb323
#undef  DZDCG_DZDCGZZ
#define DZDCG_DZDCGZZ  delgb333

/* Internal variables */
#undef  DZDCG_DZ
#define DZDCG_DZ    dz
#undef  DZDCG_OO2DZ   
#define DZDCG_OO2DZ cdzdg_oo2dz

/* Declare internal variables */
      CCTK_REAL DZDCG_OO2DZ

/* Declare output variables */
      CCTK_REAL DZDCG_DZDCGXX
      CCTK_REAL DZDCG_DZDCGXY
      CCTK_REAL DZDCG_DZDCGXZ
      CCTK_REAL DZDCG_DZDCGYY
      CCTK_REAL DZDCG_DZDCGYZ
      CCTK_REAL DZDCG_DZDCGZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DZDCG_GXX_KP 
#define DZDCG_GXX_KP gxx[ dk+ijk]
#undef  DZDCG_GXX_KM
#define DZDCG_GXX_KM gxx[-dk+ijk]
#undef  DZDCG_GXY_KP 
#define DZDCG_GXY_KP gxy[ dk+ijk]
#undef  DZDCG_GXY_KM 
#define DZDCG_GXY_KM gxy[-dk+ijk]
#undef  DZDCG_GXZ_KP 
#define DZDCG_GXZ_KP gxz[ dk+ijk]
#undef  DZDCG_GXZ_KM
#define DZDCG_GXZ_KM gxz[-dk+ijk]
#undef  DZDCG_GYY_KP 
#define DZDCG_GYY_KP gyy[ dk+ijk]
#undef  DZDCG_GYY_KM 
#define DZDCG_GYY_KM gyy[-dk+ijk]
#undef  DZDCG_GYZ_KP 
#define DZDCG_GYZ_KP gyz[ dk+ijk]
#undef  DZDCG_GYZ_KM 
#define DZDCG_GYZ_KM gyz[-dk+ijk]
#undef  DZDCG_GZZ_KP 
#define DZDCG_GZZ_KP gzz[ dk+ijk]
#undef  DZDCG_GZZ_KM
#define DZDCG_GZZ_KM gzz[-dk+ijk]

/* Output variables */ 
#undef  DZDCG_DZDCGXX
#define DZDCG_DZDCGXX  delgb311
#undef  DZDCG_DZDCGXY
#define DZDCG_DZDCGXY  delgb312
#undef  DZDCG_DZDCGXZ
#define DZDCG_DZDCGXZ  delgb313
#undef  DZDCG_DZDCGYY
#define DZDCG_DZDCGYY  delgb322
#undef  DZDCG_DZDCGYZ
#define DZDCG_DZDCGYZ  delgb323
#undef  DZDCG_DZDCGZZ
#define DZDCG_DZDCGZZ  delgb333

/* Internal variables */
#undef  DZDCG_OO2DZ   
#define DZDCG_OO2DZ cdzdg_oo2dz

/* Declare internal variables */
double DZDCG_OO2DZ;

/* Declare output variables */
double DZDCG_DZDCGXX;
double DZDCG_DZDCGXY;
double DZDCG_DZDCGXZ;
double DZDCG_DZDCGYY;
double DZDCG_DZDCGYZ;
double DZDCG_DZDCGZZ;

#endif

#define DZDCG_DECLARE

#endif
