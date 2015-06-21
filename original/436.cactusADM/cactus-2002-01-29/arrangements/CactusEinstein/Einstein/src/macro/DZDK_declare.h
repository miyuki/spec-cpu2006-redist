/*@@
  @header   DZDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DZDG_guts.h to compute first 
  derivatives of the conformal metric with respect to z
  @enddesc
@@*/

#ifndef DZDK_DECLARE
#define DZDK_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DZDK_KXX_KP 
#define DZDK_KXX_KP kxx(i,j,k+1)
#undef  DZDK_KXX_KM
#define DZDK_KXX_KM kxx(i,j,k-1)
#undef  DZDK_KXY_KP 
#define DZDK_KXY_KP kxy(i,j,k+1)
#undef  DZDK_KXY_KM 
#define DZDK_KXY_KM kxy(i,j,k-1)
#undef  DZDK_KXZ_KP 
#define DZDK_KXZ_KP kxz(i,j,k+1)
#undef  DZDK_KXZ_KM
#define DZDK_KXZ_KM kxz(i,j,k-1)
#undef  DZDK_KYY_KP 
#define DZDK_KYY_KP kyy(i,j,k+1)
#undef  DZDK_KYY_KM 
#define DZDK_KYY_KM kyy(i,j,k-1)
#undef  DZDK_KYZ_KP 
#define DZDK_KYZ_KP kyz(i,j,k+1)
#undef  DZDK_KYZ_KM 
#define DZDK_KYZ_KM kyz(i,j,k-1)
#undef  DZDK_KZZ_KP 
#define DZDK_KZZ_KP kzz(i,j,k+1)
#undef  DZDK_KZZ_KM
#define DZDK_KZZ_KM kzz(i,j,k-1)

/* Output variables */ 
#undef  DZDK_DZDKXX
#define DZDK_DZDKXX dzdk_dzdkxx 
#undef  DZDK_DZDKXY
#define DZDK_DZDKXY dzdk_dzdkxy
#undef  DZDK_DZDKXZ
#define DZDK_DZDKXZ dzdk_dzdkxz
#undef  DZDK_DZDKYY
#define DZDK_DZDKYY dzdk_dzdkyy
#undef  DZDK_DZDKYZ
#define DZDK_DZDKYZ dzdk_dzdkyz
#undef  DZDK_DZDKZZ
#define DZDK_DZDKZZ dzdk_dzdkzz

/* Internal variables */
#undef  DZDK_DZ
#define DZDK_DZ    dz
#undef  DZDK_OO2DZ   
#define DZDK_OO2DZ dzdk_oo2dz

/* Declare internal variables */
      CCTK_REAL DZDK_OO2DZ

/* Declare output variables */
      CCTK_REAL DZDK_DZDKXX
      CCTK_REAL DZDK_DZDKXY
      CCTK_REAL DZDK_DZDKXZ
      CCTK_REAL DZDK_DZDKYY
      CCTK_REAL DZDK_DZDKYZ
      CCTK_REAL DZDK_DZDKZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DZDK_KXX_KP 
#define DZDK_KXX_KP kxx[ dk+ijk]
#undef  DZDK_KXX_KM
#define DZDK_KXX_KM kxx[-dk+ijk]
#undef  DZDK_KXY_KP 
#define DZDK_KXY_KP kxy[ dk+ijk]
#undef  DZDK_KXY_KM 
#define DZDK_KXY_KM kxy[-dk+ijk]
#undef  DZDK_KXZ_KP 
#define DZDK_KXZ_KP kxz[ dk+ijk]
#undef  DZDK_KXZ_KM
#define DZDK_KXZ_KM kxz[-dk+ijk]
#undef  DZDK_KYY_KP 
#define DZDK_KYY_KP kyy[ dk+ijk]
#undef  DZDK_KYY_KM 
#define DZDK_KYY_KM kyy[-dk+ijk]
#undef  DZDK_KYZ_KP 
#define DZDK_KYZ_KP kyz[ dk+ijk]
#undef  DZDK_KYZ_KM 
#define DZDK_KYZ_KM kyz[-dk+ijk]
#undef  DZDK_KZZ_KP 
#define DZDK_KZZ_KP kzz[ dk+ijk]
#undef  DZDK_KZZ_KM
#define DZDK_KZZ_KM kzz[-dk+ijk]

/* Output variables */ 
#undef  DZDK_DZDKXX
#define DZDK_DZDKXX dzdk_dzdkxx 
#undef  DZDK_DZDKXY
#define DZDK_DZDKXY dzdk_dzdkxy
#undef  DZDK_DZDKXZ
#define DZDK_DZDKXZ dzdk_dzdkxz
#undef  DZDK_DZDKYY
#define DZDK_DZDKYY dzdk_dzdkyy
#undef  DZDK_DZDKYZ
#define DZDK_DZDKYZ dzdk_dzdkyz
#undef  DZDK_DZDKZZ
#define DZDK_DZDKZZ dzdk_dzdkzz

/* Internal variables */
#undef  DZDK_OO2DZ   
#define DZDK_OO2DZ dzdk_oo2dz

/* Declare internal variables */
double DZDK_OO2DZ;

/* Declare output variables */
double DZDK_DZDKXX;
double DZDK_DZDKXY;
double DZDK_DZDKXZ;
double DZDK_DZDKYY;
double DZDK_DZDKYZ;
double DZDK_DZDKZZ;

#endif

#endif
