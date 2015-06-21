/*@@
  @header   DDA_declare.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DDA_guts.h to compute second
  spatial derivatives of the lapse
  @enddesc
@@*/

#ifndef DDA_DECLARE
#define DDA_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DDA_A
#define DDA_A alp(i,j,k)
#undef  DDA_A_IP 
#define DDA_A_IP alp(i+1,j,k)
#undef  DDA_A_IM
#define DDA_A_IM alp(i-1,j,k)
#undef  DDA_A_JP 
#define DDA_A_JP alp(i,j+1,k)
#undef  DDA_A_JM 
#define DDA_A_JM alp(i,j-1,k)
#undef  DDA_A_KP 
#define DDA_A_KP alp(i,j,k+1)
#undef  DDA_A_KM
#define DDA_A_KM alp(i,j,k-1)
#undef  DDA_A_IPJP 
#define DDA_A_IPJP alp(i+1,j+1,k)
#undef  DDA_A_IPJM 
#define DDA_A_IPJM alp(i+1,j-1,k)
#undef  DDA_A_IMJP 
#define DDA_A_IMJP alp(i-1,j+1,k)
#undef  DDA_A_IMJM 
#define DDA_A_IMJM alp(i-1,j-1,k)
#undef  DDA_A_IPKP 
#define DDA_A_IPKP alp(i+1,j,k+1)
#undef  DDA_A_IPKM 
#define DDA_A_IPKM alp(i+1,j,k-1)
#undef  DDA_A_IMKP 
#define DDA_A_IMKP alp(i-1,j,k+1)
#undef  DDA_A_IMKM 
#define DDA_A_IMKM alp(i-1,j,k-1)
#undef  DDA_A_JPKP 
#define DDA_A_JPKP alp(i,j+1,k+1)
#undef  DDA_A_JPKM 
#define DDA_A_JPKM alp(i,j+1,k-1)
#undef  DDA_A_JMKP 
#define DDA_A_JMKP alp(i,j-1,k+1)
#undef  DDA_A_JMKM 
#define DDA_A_JMKM alp(i,j-1,k-1)

/* Output variables */ 
#undef  DDA_DXXDA
#define DDA_DXXDA dda_dxxda
#undef  DDA_DXYDA
#define DDA_DXYDA dda_dxyda
#undef  DDA_DXZDA
#define DDA_DXZDA dda_dxzda
#undef  DDA_DYYDA
#define DDA_DYYDA dda_dyyda
#undef  DDA_DYZDA
#define DDA_DYZDA dda_dyzda
#undef  DDA_DZZDA
#define DDA_DZZDA dda_dzzda

/* Internal variables */
#undef  DDA_OODX2   
#define DDA_OODX2 dda_oodx2
#undef  DDA_OODY2   
#define DDA_OODY2 dda_oody2
#undef  DDA_OODZ2   
#define DDA_OODZ2 dda_oodz2
#undef  DDA_OO4DXDY
#define DDA_OO4DXDY dda_oo4dxdy
#undef  DDA_OO4DXDZ
#define DDA_OO4DXDZ dda_oo4dxdz
#undef  DDA_OO4DYDZ
#define DDA_OO4DYDZ dda_oo4dydz
#undef  DDA_DX
#define DDA_DX dx
#undef  DDA_DY
#define DDA_DY dy
#undef  DDA_DZ
#define DDA_DZ dz

/* Declare internal variables */
      CCTK_REAL DDA_OODX2
      CCTK_REAL DDA_OODY2
      CCTK_REAL DDA_OODZ2
      CCTK_REAL DDA_OO4DXDY
      CCTK_REAL DDA_OO4DXDZ
      CCTK_REAL DDA_OO4DYDZ

/* Declare output variables */
      CCTK_REAL DDA_DXXDA
      CCTK_REAL DDA_DXYDA
      CCTK_REAL DDA_DXZDA
      CCTK_REAL DDA_DYYDA
      CCTK_REAL DDA_DYZDA
      CCTK_REAL DDA_DZZDA

#endif


#ifdef CCODE

/* Input variables */
#undef  DDA_A
#define DDA_A alp[ijk]
#undef  DDA_A_IP 
#define DDA_A_IP alp[ di + ijk]
#undef  DDA_A_IM
#define DDA_A_IM alp[-di + ijk]
#undef  DDA_A_JP 
#define DDA_A_JP alp[ dj + ijk]
#undef  DDA_A_JM 
#define DDA_A_JM alp[-dj + ijk]
#undef  DDA_A_KP 
#define DDA_A_KP alp[ dk + ijk]
#undef  DDA_A_KM
#define DDA_A_KM alp[-dk + ijk]
#undef  DDA_A_IPJP 
#define DDA_A_IPJP alp[ di + dj + ijk]
#undef  DDA_A_IPJM 
#define DDA_A_IPJM alp[ di - dj + ijk]
#undef  DDA_A_IMJP 
#define DDA_A_IMJP alp[-di + dj + ijk]
#undef  DDA_A_IMJM 
#define DDA_A_IMJM alp[-di - dj + ijk]
#undef  DDA_A_IPKP 
#define DDA_A_IPKP alp[ di + dk + ijk]
#undef  DDA_A_IPKM 
#define DDA_A_IPKM alp[ di - dk + ijk]
#undef  DDA_A_IMKP 
#define DDA_A_IMKP alp[-di + dk + ijk]
#undef  DDA_A_IMKM 
#define DDA_A_IMKM alp[-di - dk + ijk]
#undef  DDA_A_JPKP 
#define DDA_A_JPKP alp[ dj + dk + ijk]
#undef  DDA_A_JPKM 
#define DDA_A_JPKM alp[ dj - dk + ijk]
#undef  DDA_A_JMKP 
#define DDA_A_JMKP alp[-dj + dk + ijk]
#undef  DDA_A_JMKM 
#define DDA_A_JMKM alp[-dj - dk + ijk]

/* Output variables */ 
#undef  DDA_DXXDA
#define DDA_DXXDA dda_dxxda
#undef  DDA_DXYDA
#define DDA_DXYDA dda_dxyda
#undef  DDA_DXZDA
#define DDA_DXZDA dda_dxzda
#undef  DDA_DYYDA
#define DDA_DYYDA dda_dyyda
#undef  DDA_DYZDA
#define DDA_DYZDA dda_dyzda
#undef  DDA_DZZDA
#define DDA_DZZDA dda_dzzda

/* Internal variables */
#undef  DDA_OODX2   
#define DDA_OODX2 dda_oodx2
#undef  DDA_OODY2   
#define DDA_OODY2 dda_oody2
#undef  DDA_OODZ2   
#define DDA_OODZ2 dda_oodz2
#undef  DDA_OO4DXDY
#define DDA_OO4DXDY dda_oo4dxdy
#undef  DDA_OO4DXDZ
#define DDA_OO4DXDZ dda_oo4dxdz
#undef  DDA_OO4DYDZ
#define DDA_OO4DYDZ dda_oo4dydz
#undef  DDA_DX   
#define DDA_DX cctkGH->cctk_delta_space[0]
#undef  DDA_DY   
#define DDA_DY cctkGH->cctk_delta_space[1]
#undef  DDA_DZ   
#define DDA_DZ cctkGH->cctk_delta_space[2]

/* Declare internal variables */
      CCTK_REAL DDA_OODX2;
      CCTK_REAL DDA_OODY2;
      CCTK_REAL DDA_OODZ2;
      CCTK_REAL DDA_OO4DXDY;
      CCTK_REAL DDA_OO4DXDZ;
      CCTK_REAL DDA_OO4DYDZ;

/* Declare output variables */
      CCTK_REAL DDA_DXXDA;
      CCTK_REAL DDA_DXYDA;
      CCTK_REAL DDA_DXZDA;
      CCTK_REAL DDA_DYYDA;
      CCTK_REAL DDA_DYZDA;
      CCTK_REAL DDA_DZZDA;

#endif

#endif
