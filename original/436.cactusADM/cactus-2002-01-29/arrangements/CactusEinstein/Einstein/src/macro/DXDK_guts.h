/*@@
  @header   DXDK_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  extrinsic curvature with respect to x
  @enddesc
@@*/

#ifndef DXDK_GUTS
#define DXDK_GUTS

#ifdef FCODE 

      DXDK_OO2DX = 1D0/(2D0*DXDK_DX)
    
      DXDK_DXDKXX = DXDK_OO2DX*(DXDK_KXX_IP - DXDK_KXX_IM)
      DXDK_DXDKXY = DXDK_OO2DX*(DXDK_KXY_IP - DXDK_KXY_IM)
      DXDK_DXDKXZ = DXDK_OO2DX*(DXDK_KXZ_IP - DXDK_KXZ_IM)
      DXDK_DXDKYY = DXDK_OO2DX*(DXDK_KYY_IP - DXDK_KYY_IM)
      DXDK_DXDKYZ = DXDK_OO2DX*(DXDK_KYZ_IP - DXDK_KYZ_IM)
      DXDK_DXDKZZ = DXDK_OO2DX*(DXDK_KZZ_IP - DXDK_KZZ_IM)

#endif

#ifdef CCODE

      DXDK_OO2DX = 1/(2*cctkGH->cctk_delta_space[0]);
    
      DXDK_DXDKXX = DXDK_OO2DX*(DXDK_KXX_IP - DXDK_KXX_IM);
      DXDK_DXDKXY = DXDK_OO2DX*(DXDK_KXY_IP - DXDK_KXY_IM);
      DXDK_DXDKXZ = DXDK_OO2DX*(DXDK_KXZ_IP - DXDK_KXZ_IM);
      DXDK_DXDKYY = DXDK_OO2DX*(DXDK_KYY_IP - DXDK_KYY_IM);
      DXDK_DXDKYZ = DXDK_OO2DX*(DXDK_KYZ_IP - DXDK_KYZ_IM);
      DXDK_DXDKZZ = DXDK_OO2DX*(DXDK_KZZ_IP - DXDK_KZZ_IM);

#endif

#endif
