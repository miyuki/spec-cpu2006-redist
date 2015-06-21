/*@@
  @header   DXDK_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DXDK_guts.h to compute first 
  derivatives of the extrinsic curvature with respect to x
  @enddesc
@@*/

#ifndef DXDK_DECLARE
#define DXDK_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DXDK_KXX_IP 
#define DXDK_KXX_IP kxx(i+1,j,k)
#undef  DXDK_KXX_IM
#define DXDK_KXX_IM kxx(i-1,j,k)
#undef  DXDK_KXY_IP 
#define DXDK_KXY_IP kxy(i+1,j,k)
#undef  DXDK_KXY_IM 
#define DXDK_KXY_IM kxy(i-1,j,k)
#undef  DXDK_KXZ_IP 
#define DXDK_KXZ_IP kxz(i+1,j,k)
#undef  DXDK_KXZ_IM
#define DXDK_KXZ_IM kxz(i-1,j,k)
#undef  DXDK_KYY_IP 
#define DXDK_KYY_IP kyy(i+1,j,k)
#undef  DXDK_KYY_IM 
#define DXDK_KYY_IM kyy(i-1,j,k)
#undef  DXDK_KYZ_IP 
#define DXDK_KYZ_IP kyz(i+1,j,k)
#undef  DXDK_KYZ_IM 
#define DXDK_KYZ_IM kyz(i-1,j,k)
#undef  DXDK_KZZ_IP 
#define DXDK_KZZ_IP kzz(i+1,j,k)
#undef  DXDK_KZZ_IM
#define DXDK_KZZ_IM kzz(i-1,j,k)

/* Output variables */ 
#undef  DXDK_DXDKXX
#define DXDK_DXDKXX  dxdk_dxdkxx
#undef  DXDK_DXDKXY
#define DXDK_DXDKXY  dxdk_dxdkxy
#undef  DXDK_DXDKXZ
#define DXDK_DXDKXZ  dxdk_dxdkxz
#undef  DXDK_DXDKYY
#define DXDK_DXDKYY  dxdk_dxdkyy
#undef  DXDK_DXDKYZ
#define DXDK_DXDKYZ  dxdk_dxdkyz
#undef  DXDK_DXDKZZ
#define DXDK_DXDKZZ  dxdk_dxdkzz

/* Internal variables */
#undef  DXDK_DX
#define DXDK_DX    dx
#undef  DXDK_OO2DX   
#define DXDK_OO2DX dxdk_oo2dx

/* Declare internal variables */
      CCTK_REAL DXDK_OO2DX

/* Declare output variables */
      CCTK_REAL DXDK_DXDKXX
      CCTK_REAL DXDK_DXDKXY
      CCTK_REAL DXDK_DXDKXZ
      CCTK_REAL DXDK_DXDKYY
      CCTK_REAL DXDK_DXDKYZ
      CCTK_REAL DXDK_DXDKZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DXDK_KXX_IP 
#define DXDK_KXX_IP kxx[ di+ijk]
#undef  DXDK_KXX_IM
#define DXDK_KXX_IM kxx[-di+ijk]
#undef  DXDK_KXY_IP 
#define DXDK_KXY_IP kxy[ di+ijk]
#undef  DXDK_KXY_IM 
#define DXDK_KXY_IM kxy[-di+ijk]
#undef  DXDK_KXZ_IP 
#define DXDK_KXZ_IP kxz[ di+ijk]
#undef  DXDK_KXZ_IM
#define DXDK_KXZ_IM kxz[-di+ijk]
#undef  DXDK_KYY_IP 
#define DXDK_KYY_IP kyy[ di+ijk]
#undef  DXDK_KYY_IM 
#define DXDK_KYY_IM kyy[-di+ijk]
#undef  DXDK_KYZ_IP 
#define DXDK_KYZ_IP kyz[ di+ijk]
#undef  DXDK_KYZ_IM 
#define DXDK_KYZ_IM kyz[-di+ijk]
#undef  DXDK_KZZ_IP 
#define DXDK_KZZ_IP kzz[ di+ijk]
#undef  DXDK_KZZ_IM
#define DXDK_KZZ_IM kzz[-di+ijk]

/* Output variables */ 
#undef  DXDK_DXDKXX
#define DXDK_DXDKXX  dxdk_dxdkxx
#undef  DXDK_DXDKXY
#define DXDK_DXDKXY  dxdk_dxdkxy
#undef  DXDK_DXDKXZ
#define DXDK_DXDKXZ  dxdk_dxdkxz
#undef  DXDK_DXDKYY
#define DXDK_DXDKYY  dxdk_dxdkyy
#undef  DXDK_DXDKYZ
#define DXDK_DXDKYZ  dxdk_dxdkyz
#undef  DXDK_DXDKZZ
#define DXDK_DXDKZZ  dxdk_dxdkzz

/* Internal variables */
#undef  DXDK_DX
#define DXDK_DX    dx0
#undef  DXDK_OO2DX   
#define DXDK_OO2DX dxdk_oo2dx

/* Declare internal variables */
double DXDK_DX;
double DXDK_OO2DX;

/* Declare output variables */
double DXDK_DXDKXX;
double DXDK_DXDKXY;
double DXDK_DXDKXZ;
double DXDK_DXDKYY;
double DXDK_DXDKYZ;
double DXDK_DXDKZZ;

#endif

#endif
