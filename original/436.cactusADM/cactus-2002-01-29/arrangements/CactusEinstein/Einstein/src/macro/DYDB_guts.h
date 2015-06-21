/*@@
  @header   DYDB_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the first derivatives of the 
  shift with respect to y

  The macro is defined in terms of standard variables in
  @seefile DYDB_declare.h
  @enddesc
@@*/

#ifndef DYDB_GUTS
#define DYDB_GUTS

#ifdef FCODE 

      DYDB_OO2DY = 1D0/(2D0*DYDB_DY)
    
      DYDB_DYDBX = DYDB_OO2DY*(DYDB_BX_JP - DYDB_BX_JM)
      DYDB_DYDBY = DYDB_OO2DY*(DYDB_BY_JP - DYDB_BY_JM)
      DYDB_DYDBZ = DYDB_OO2DY*(DYDB_BZ_JP - DYDB_BZ_JM)

#endif

#ifdef CCODE

      DYDB_OO2DY = 1/(2*cctkGH->cctk_delta_space[1]);
    
      DYDB_DYDBX = DYDB_OO2DY*(DYDB_BX_JP - DYDB_BX_JM);
      DYDB_DYDBY = DYDB_OO2DY*(DYDB_BY_JP - DYDB_BY_JM);
      DYDB_DYDBZ = DYDB_OO2DY*(DYDB_BZ_JP - DYDB_BZ_JM);

#endif

#endif

