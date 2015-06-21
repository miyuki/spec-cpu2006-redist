/*@@
  @header   DZDG_guts.h
  @date     Jul 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  extrinsic with respect to z
  @enddesc
@@*/

#ifndef DZDK_GUTS
#define DZDK_GUTS

#ifdef FCODE 

      DZDK_OO2DZ = 1D0/(2D0*DZDK_DZ)
    
      DZDK_DZDKXX = DZDK_OO2DZ*(DZDK_KXX_KP - DZDK_KXX_KM)
      DZDK_DZDKXY = DZDK_OO2DZ*(DZDK_KXY_KP - DZDK_KXY_KM)
      DZDK_DZDKXZ = DZDK_OO2DZ*(DZDK_KXZ_KP - DZDK_KXZ_KM)
      DZDK_DZDKYY = DZDK_OO2DZ*(DZDK_KYY_KP - DZDK_KYY_KM)
      DZDK_DZDKYZ = DZDK_OO2DZ*(DZDK_KYZ_KP - DZDK_KYZ_KM)
      DZDK_DZDKZZ = DZDK_OO2DZ*(DZDK_KZZ_KP - DZDK_KZZ_KM)

#endif

#ifdef CCODE

      DZDK_OO2DZ = 1/(2*cctkGH->cctk_delta_space[2]);
    
      DZDK_DZDKXX = DZDK_OO2DZ*(DZDK_KXX_KP - DZDK_KXX_KM);
      DZDK_DZDKXY = DZDK_OO2DZ*(DZDK_KXY_KP - DZDK_KXY_KM);
      DZDK_DZDKXZ = DZDK_OO2DZ*(DZDK_KXZ_KP - DZDK_KXZ_KM);
      DZDK_DZDKYY = DZDK_OO2DZ*(DZDK_KYY_KP - DZDK_KYY_KM);
      DZDK_DZDKYZ = DZDK_OO2DZ*(DZDK_KYZ_KP - DZDK_KYZ_KM);
      DZDK_DZDKZZ = DZDK_OO2DZ*(DZDK_KZZ_KP - DZDK_KZZ_KM);

#endif

#endif
