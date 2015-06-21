/*@@
  @header   DA_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DA_guts.h to compute first 
  spatial derivatives of the lapse
  @enddesc
@@*/

#ifndef DA_DECLARE
#define DA_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DA_A_IP 
#define DA_A_IP alp(i+1,j,k)
#undef  DA_A_IM
#define DA_A_IM alp(i-1,j,k)
#undef  DA_A_JP 
#define DA_A_JP alp(i,j+1,k)
#undef  DA_A_JM 
#define DA_A_JM alp(i,j-1,k)
#undef  DA_A_KP 
#define DA_A_KP alp(i,j,k+1)
#undef  DA_A_KM
#define DA_A_KM alp(i,j,k-1)

/* Output variables */ 
#undef  DA_DXDA
#define DA_DXDA  da_dxda
#undef  DA_DYDA
#define DA_DYDA  da_dyda
#undef  DA_DZDA
#define DA_DZDA  da_dzda

/* Internal variables */
#undef  DA_OO2DX   
#define DA_OO2DX da_oo2dx
#undef  DA_OO2DY   
#define DA_OO2DY da_oo2dy
#undef  DA_OO2DZ   
#define DA_OO2DZ da_oo2dz
#undef  DA_DX   
#define DA_DX dx
#undef  DA_DY   
#define DA_DY dy
#undef  DA_DZ   
#define DA_DZ dz

/* Declare internal variables */
      CCTK_REAL DA_OO2DX
      CCTK_REAL DA_OO2DY
      CCTK_REAL DA_OO2DZ

/* Declare output variables */
      CCTK_REAL DA_DXDA
      CCTK_REAL DA_DYDA
      CCTK_REAL DA_DZDA

#endif


#ifdef CCODE

/* Input variables */
#undef  DA_A_IP 
#define DA_A_IP alp[ di + ijk]
#undef  DA_A_IM
#define DA_A_IM alp[-di + ijk]
#undef  DA_A_JP 
#define DA_A_JP alp[ dj + ijk]
#undef  DA_A_JM 
#define DA_A_JM alp[-dj + ijk]
#undef  DA_A_KP 
#define DA_A_KP alp[ dk + ijk]
#undef  DA_A_KM
#define DA_A_KM alp[-dk + ijk]

/* Output variables */ 
#undef  DA_DXDA
#define DA_DXDA da_dxda
#undef  DA_DYDA
#define DA_DYDA da_dyda
#undef  DA_DZDA
#define DA_DZDA da_dzda

/* Internal variables */
#undef  DA_OO2DX   
#define DA_OO2DX da_oo2dx
#undef  DA_OO2DY   
#define DA_OO2DY da_oo2dy
#undef  DA_OO2DZ   
#define DA_OO2DZ da_oo2dz
#undef  DA_DX   
#define DA_DX cctkGH->cctk_delta_space[0]
#undef  DA_DY   
#define DA_DY cctkGH->cctk_delta_space[1]
#undef  DA_DZ   
#define DA_DZ cctkGH->cctk_delta_space[2]

/* Declare internal variables */
      double DA_OO2DX;
      double DA_OO2DY;
      double DA_OO2DZ;

/* Declare output variables */
      double DA_DXDA;
      double DA_DYDA;
      double DA_DZDA;

#endif

#endif
