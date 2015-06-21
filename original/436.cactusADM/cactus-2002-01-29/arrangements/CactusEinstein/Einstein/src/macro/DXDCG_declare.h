/*@@
  @header   DXDCG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DXDCG_guts.h to compute first 
  derivatives of the conformal metric with respect to x

  Requires: Conformal lower metric at i+1,i-1 ; dx

  Provides: Derivative of conformal lower metric wrt x
  @enddesc
@@*/

#ifndef DXDCG_DECLARE
#define DXDCG_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DXDCG_GXX_IP 
#define DXDCG_GXX_IP gxx(i+1,j,k)
#undef  DXDCG_GXX_IM
#define DXDCG_GXX_IM gxx(i-1,j,k)
#undef  DXDCG_GXY_IP 
#define DXDCG_GXY_IP gxy(i+1,j,k)
#undef  DXDCG_GXY_IM 
#define DXDCG_GXY_IM gxy(i-1,j,k)
#undef  DXDCG_GXZ_IP 
#define DXDCG_GXZ_IP gxz(i+1,j,k)
#undef  DXDCG_GXZ_IM
#define DXDCG_GXZ_IM gxz(i-1,j,k)
#undef  DXDCG_GYY_IP 
#define DXDCG_GYY_IP gyy(i+1,j,k)
#undef  DXDCG_GYY_IM 
#define DXDCG_GYY_IM gyy(i-1,j,k)
#undef  DXDCG_GYZ_IP 
#define DXDCG_GYZ_IP gyz(i+1,j,k)
#undef  DXDCG_GYZ_IM 
#define DXDCG_GYZ_IM gyz(i-1,j,k)
#undef  DXDCG_GZZ_IP 
#define DXDCG_GZZ_IP gzz(i+1,j,k)
#undef  DXDCG_GZZ_IM
#define DXDCG_GZZ_IM gzz(i-1,j,k)

/* Output variables */ 
#undef  DXDCG_DXDCGXX
#define DXDCG_DXDCGXX  delgb111
#undef  DXDCG_DXDCGXY
#define DXDCG_DXDCGXY  delgb112
#undef  DXDCG_DXDCGXZ
#define DXDCG_DXDCGXZ  delgb113
#undef  DXDCG_DXDCGYY
#define DXDCG_DXDCGYY  delgb122
#undef  DXDCG_DXDCGYZ
#define DXDCG_DXDCGYZ  delgb123
#undef  DXDCG_DXDCGZZ
#define DXDCG_DXDCGZZ  delgb133

/* Internal variables */
#undef  DXDCG_DX   
#define DXDCG_DX dx
#undef  DXDCG_OO2DX   
#define DXDCG_OO2DX cdxdg_oo2dx

/* Declare internal variables */
      CCTK_REAL DXDCG_OO2DX

/* Declare output variables */
      CCTK_REAL DXDCG_DXDCGXX
      CCTK_REAL DXDCG_DXDCGXY
      CCTK_REAL DXDCG_DXDCGXZ
      CCTK_REAL DXDCG_DXDCGYY
      CCTK_REAL DXDCG_DXDCGYZ
      CCTK_REAL DXDCG_DXDCGZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DXDCG_GXX_IP 
#define DXDCG_GXX_IP gxx[ di+ijk]
#undef  DXDCG_GXX_IM
#define DXDCG_GXX_IM gxx[-di+ijk]
#undef  DXDCG_GXY_IP 
#define DXDCG_GXY_IP gxy[ di+ijk]
#undef  DXDCG_GXY_IM 
#define DXDCG_GXY_IM gxy[-di+ijk]
#undef  DXDCG_GXZ_IP 
#define DXDCG_GXZ_IP gxz[ di+ijk]
#undef  DXDCG_GXZ_IM
#define DXDCG_GXZ_IM gxz[-di+ijk]
#undef  DXDCG_GYY_IP 
#define DXDCG_GYY_IP gyy[ di+ijk]
#undef  DXDCG_GYY_IM 
#define DXDCG_GYY_IM gyy[-di+ijk]
#undef  DXDCG_GYZ_IP 
#define DXDCG_GYZ_IP gyz[ di+ijk]
#undef  DXDCG_GYZ_IM 
#define DXDCG_GYZ_IM gyz[-di+ijk]
#undef  DXDCG_GZZ_IP 
#define DXDCG_GZZ_IP gzz[ di+ijk]
#undef  DXDCG_GZZ_IM
#define DXDCG_GZZ_IM gzz[-di+ijk]

/* Output variables */ 
#undef  DXDCG_DXDCGXX
#define DXDCG_DXDCGXX  delgb111
#undef  DXDCG_DXDCGXY
#define DXDCG_DXDCGXY  delgb112
#undef  DXDCG_DXDCGXZ
#define DXDCG_DXDCGXZ  delgb113
#undef  DXDCG_DXDCGYY
#define DXDCG_DXDCGYY  delgb122
#undef  DXDCG_DXDCGYZ
#define DXDCG_DXDCGYZ  delgb123
#undef  DXDCG_DXDCGZZ
#define DXDCG_DXDCGZZ  delgb133

/* Internal variables */
#undef  DXDCG_OO2DX   
#define DXDCG_OO2DX cdxdg_oo2dx

/* Declare internal variables */
double DXDCG_OO2DX;

/* Declare output variables */
double DXDCG_DXDCGXX;
double DXDCG_DXDCGXY;
double DXDCG_DXDCGXZ;
double DXDCG_DXDCGYY;
double DXDCG_DXDCGYZ;
double DXDCG_DXDCGZZ;

#endif

#endif
