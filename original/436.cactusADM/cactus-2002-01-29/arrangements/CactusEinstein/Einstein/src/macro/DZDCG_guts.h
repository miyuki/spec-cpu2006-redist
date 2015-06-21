/*@@
  @header   DZDCG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  conformal metric with respect to z

  The macro is defined in terms of standard variables in

  Requires: lower conformal metric at k+1,k-1 ; dz0

  Provides: derivative of lower conformal metric wrt z

  @seefile DZDCG_declare.h
  @enddesc
@@*/

#ifndef DZDCG_GUTS
#define DZDCG_GUTS

#ifdef FCODE 

      DZDCG_OO2DZ = 1D0/(2D0*DZDCG_DZ)
    
      DZDCG_DZDCGXX = DZDCG_OO2DZ*(DZDCG_GXX_KP - DZDCG_GXX_KM)
      DZDCG_DZDCGXY = DZDCG_OO2DZ*(DZDCG_GXY_KP - DZDCG_GXY_KM)
      DZDCG_DZDCGXZ = DZDCG_OO2DZ*(DZDCG_GXZ_KP - DZDCG_GXZ_KM)
      DZDCG_DZDCGYY = DZDCG_OO2DZ*(DZDCG_GYY_KP - DZDCG_GYY_KM)
      DZDCG_DZDCGYZ = DZDCG_OO2DZ*(DZDCG_GYZ_KP - DZDCG_GYZ_KM)
      DZDCG_DZDCGZZ = DZDCG_OO2DZ*(DZDCG_GZZ_KP - DZDCG_GZZ_KM)

#endif

#ifdef CCODE

      DZDCG_OO2DZ = 1/(2*cctkGH->cctk_delta_space[2]);
    
      DZDCG_DZDCGXX = DZDCG_OO2DZ*(DZDCG_GXX_KP - DZDCG_GXX_KM);
      DZDCG_DZDCGXY = DZDCG_OO2DZ*(DZDCG_GXY_KP - DZDCG_GXY_KM);
      DZDCG_DZDCGXZ = DZDCG_OO2DZ*(DZDCG_GXZ_KP - DZDCG_GXZ_KM);
      DZDCG_DZDCGYY = DZDCG_OO2DZ*(DZDCG_GYY_KP - DZDCG_GYY_KM);
      DZDCG_DZDCGYZ = DZDCG_OO2DZ*(DZDCG_GYZ_KP - DZDCG_GYZ_KM);
      DZDCG_DZDCGZZ = DZDCG_OO2DZ*(DZDCG_GZZ_KP - DZDCG_GZZ_KM);

#endif

#endif
