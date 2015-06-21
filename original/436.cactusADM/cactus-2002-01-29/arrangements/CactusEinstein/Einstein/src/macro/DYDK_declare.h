/*@@
  @header   DYDK_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DYDG_guts.h to compute first 
  derivatives of the extrinsic curvature with respect to y
  @enddesc
@@*/

#ifndef DYDK_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DYDK_KXX_JP 
#define DYDK_KXX_JP kxx(i,j+1,k)
#undef  DYDK_KXX_JM
#define DYDK_KXX_JM kxx(i,j-1,k)
#undef  DYDK_KXY_JP 
#define DYDK_KXY_JP kxy(i,j+1,k)
#undef  DYDK_KXY_JM 
#define DYDK_KXY_JM kxy(i,j-1,k)
#undef  DYDK_KXZ_JP 
#define DYDK_KXZ_JP kxz(i,j+1,k)
#undef  DYDK_KXZ_JM
#define DYDK_KXZ_JM kxz(i,j-1,k)
#undef  DYDK_KYY_JP 
#define DYDK_KYY_JP kyy(i,j+1,k)
#undef  DYDK_KYY_JM 
#define DYDK_KYY_JM kyy(i,j-1,k)
#undef  DYDK_KYZ_JP 
#define DYDK_KYZ_JP kyz(i,j+1,k)
#undef  DYDK_KYZ_JM 
#define DYDK_KYZ_JM kyz(i,j-1,k)
#undef  DYDK_KZZ_JP 
#define DYDK_KZZ_JP kzz(i,j+1,k)
#undef  DYDK_KZZ_JM
#define DYDK_KZZ_JM kzz(i,j-1,k)

/* Output variables */ 
#undef  DYDK_DYDKXX
#define DYDK_DYDKXX dydk_dydkxx
#undef  DYDK_DYDKXY
#define DYDK_DYDKXY dydk_dydkxy
#undef  DYDK_DYDKXZ
#define DYDK_DYDKXZ dydk_dydkxz
#undef  DYDK_DYDKYY
#define DYDK_DYDKYY dydk_dydkyy
#undef  DYDK_DYDKYZ
#define DYDK_DYDKYZ dydk_dydkyz
#undef  DYDK_DYDKZZ
#define DYDK_DYDKZZ dydk_dydkzz 

/* Internal variables */
#undef  DYDK_DY
#define DYDK_DY    dy
#undef  DYDK_OO2DY   
#define DYDK_OO2DY dydk_oo2dy

/* Declare internal variables */
      CCTK_REAL DYDK_OO2DY

/* Declare output variables */
      CCTK_REAL DYDK_DYDKXX
      CCTK_REAL DYDK_DYDKXY
      CCTK_REAL DYDK_DYDKXZ
      CCTK_REAL DYDK_DYDKYY
      CCTK_REAL DYDK_DYDKYZ
      CCTK_REAL DYDK_DYDKZZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DYDK_KXX_JP 
#define DYDK_KXX_JP kxx[ dj+ijk]
#undef  DYDK_KXX_JM
#define DYDK_KXX_JM kxx[-dj+ijk]
#undef  DYDK_KXY_JP 
#define DYDK_KXY_JP kxy[ dj+ijk]
#undef  DYDK_KXY_JM 
#define DYDK_KXY_JM kxy[-dj+ijk]
#undef  DYDK_KXZ_JP 
#define DYDK_KXZ_JP kxz[ dj+ijk]
#undef  DYDK_KXZ_JM
#define DYDK_KXZ_JM kxz[-dj+ijk]
#undef  DYDK_KYY_JP 
#define DYDK_KYY_JP kyy[ dj+ijk]
#undef  DYDK_KYY_JM 
#define DYDK_KYY_JM kyy[-dj+ijk]
#undef  DYDK_KYZ_JP 
#define DYDK_KYZ_JP kyz[ dj+ijk]
#undef  DYDK_KYZ_JM 
#define DYDK_KYZ_JM kyz[-dj+ijk]
#undef  DYDK_KZZ_JP 
#define DYDK_KZZ_JP kzz[ dj+ijk]
#undef  DYDK_KZZ_JM
#define DYDK_KZZ_JM kzz[-dj+ijk]

/* Output variables */ 
#undef  DYDK_DYDKXX
#define DYDK_DYDKXX dydk_dydkxx
#undef  DYDK_DYDKXY
#define DYDK_DYDKXY dydk_dydkxy
#undef  DYDK_DYDKXZ
#define DYDK_DYDKXZ dydk_dydkxz
#undef  DYDK_DYDKYY
#define DYDK_DYDKYY dydk_dydkyy
#undef  DYDK_DYDKYZ
#define DYDK_DYDKYZ dydk_dydkyz
#undef  DYDK_DYDKZZ
#define DYDK_DYDKZZ dydk_dydkzz 

/* Internal variables */
#undef  DYDK_OO2DY   
#define DYDK_OO2DY dydk_oo2dy

/* Declare internal variables */
double DYDK_OO2DY;

/* Declare output variables */
double DYDK_DYDKXX;
double DYDK_DYDKXY;
double DYDK_DYDKXZ;
double DYDK_DYDKYY;
double DYDK_DYDKYZ;
double DYDK_DYDKZZ;

#endif

#define DYDK_DECLARE

#endif

