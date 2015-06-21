/*@@
  @header   DYDCG_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Macro to calculate the first derivatives of the 
  conformal metric with respect to y

  The macro is defined in terms of standard variables in

  Requires: lower conformal metric at j+1,j-1 ; dy0

  Provides: derivative of lower conformal metric wrt y

  @seefile DYDCG_declare.h
  @enddesc
@@*/

#ifndef DYDCG_GUTS
#define DYDCG_GUTS

#ifdef FCODE 

      DYDCG_OO2DY = 1D0/(2D0*DYDCG_DY)
    
      DYDCG_DYDCGXX = DYDCG_OO2DY*(DYDCG_GXX_JP - DYDCG_GXX_JM)
      DYDCG_DYDCGXY = DYDCG_OO2DY*(DYDCG_GXY_JP - DYDCG_GXY_JM)
      DYDCG_DYDCGXZ = DYDCG_OO2DY*(DYDCG_GXZ_JP - DYDCG_GXZ_JM)
      DYDCG_DYDCGYY = DYDCG_OO2DY*(DYDCG_GYY_JP - DYDCG_GYY_JM)
      DYDCG_DYDCGYZ = DYDCG_OO2DY*(DYDCG_GYZ_JP - DYDCG_GYZ_JM)
      DYDCG_DYDCGZZ = DYDCG_OO2DY*(DYDCG_GZZ_JP - DYDCG_GZZ_JM)

#endif


#ifdef CCODE

      DYDCG_OO2DY = 1/(2*cctkGH->cctk_delta_space[1]);
    
      DYDCG_DYDCGXX = DYDCG_OO2DY*(DYDCG_GXX_JP - DYDCG_GXX_JM);
      DYDCG_DYDCGXY = DYDCG_OO2DY*(DYDCG_GXY_JP - DYDCG_GXY_JM);
      DYDCG_DYDCGXZ = DYDCG_OO2DY*(DYDCG_GXZ_JP - DYDCG_GXZ_JM);
      DYDCG_DYDCGYY = DYDCG_OO2DY*(DYDCG_GYY_JP - DYDCG_GYY_JM);
      DYDCG_DYDCGYZ = DYDCG_OO2DY*(DYDCG_GYZ_JP - DYDCG_GYZ_JM);
      DYDCG_DYDCGZZ = DYDCG_OO2DY*(DYDCG_GZZ_JP - DYDCG_GZZ_JM);

#endif

#endif
