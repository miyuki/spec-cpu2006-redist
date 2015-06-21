/*@@
  @header   DYDG_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  extrinsic curvature with respect to y
  @enddesc
@@*/

#ifndef DYDK_GUTS
#define DYDK_GUTS

#ifdef FCODE 

      DYDK_OO2DY = 1D0/(2D0*DYDK_DY)
    
      DYDK_DYDKXX = DYDK_OO2DY*(DYDK_KXX_JP - DYDK_KXX_JM)
      DYDK_DYDKXY = DYDK_OO2DY*(DYDK_KXY_JP - DYDK_KXY_JM)
      DYDK_DYDKXZ = DYDK_OO2DY*(DYDK_KXZ_JP - DYDK_KXZ_JM)
      DYDK_DYDKYY = DYDK_OO2DY*(DYDK_KYY_JP - DYDK_KYY_JM)
      DYDK_DYDKYZ = DYDK_OO2DY*(DYDK_KYZ_JP - DYDK_KYZ_JM)
      DYDK_DYDKZZ = DYDK_OO2DY*(DYDK_KZZ_JP - DYDK_KZZ_JM)

#endif


#ifdef CCODE

      DYDK_OO2DY = 1/(2*cctkGH->cctk_delta_space[1]);
    
      DYDK_DYDKXX = DYDK_OO2DY*(DYDK_KXX_JP - DYDK_KXX_JM);
      DYDK_DYDKXY = DYDK_OO2DY*(DYDK_KXY_JP - DYDK_KXY_JM);
      DYDK_DYDKXZ = DYDK_OO2DY*(DYDK_KXZ_JP - DYDK_KXZ_JM);
      DYDK_DYDKYY = DYDK_OO2DY*(DYDK_KYY_JP - DYDK_KYY_JM);
      DYDK_DYDKYZ = DYDK_OO2DY*(DYDK_KYZ_JP - DYDK_KYZ_JM);
      DYDK_DYDKZZ = DYDK_OO2DY*(DYDK_KZZ_JP - DYDK_KZZ_JM);

#endif

#endif
