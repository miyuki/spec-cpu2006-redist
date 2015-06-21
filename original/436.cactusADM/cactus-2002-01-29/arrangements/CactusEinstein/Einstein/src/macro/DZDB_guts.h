/*@@
  @header   DZDB_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the first derivatives of the 
  shift with respect to z

  The macro is defined in terms of standard variables in
  @seefile DZDB_declare.h
  @enddesc
@@*/

#ifndef DZDB_GUTS
#define DZDB_GUTS

#ifdef FCODE 

      DZDB_OO2DZ = 1D0/(2D0*DZDB_DZ)
    
      DZDB_DZDBX = DZDB_OO2DZ*(DZDB_BX_KP - DZDB_BX_KM)
      DZDB_DZDBY = DZDB_OO2DZ*(DZDB_BY_KP - DZDB_BY_KM)
      DZDB_DZDBZ = DZDB_OO2DZ*(DZDB_BZ_KP - DZDB_BZ_KM)

#endif

#ifdef CCODE

      DZDB_OO2DZ = 1/(2*cctkGH->cctk_delta_space[2]);
    
      DZDB_DZDBX = DZDB_OO2DZ*(DZDB_BX_KP - DZDB_BX_KM);
      DZDB_DZDBY = DZDB_OO2DZ*(DZDB_BY_KP - DZDB_BY_KM);
      DZDB_DZDBZ = DZDB_OO2DZ*(DZDB_BZ_KP - DZDB_BZ_KM);

#endif

#endif

