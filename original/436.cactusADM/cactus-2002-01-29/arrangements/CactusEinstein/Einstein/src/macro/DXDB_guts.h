/*@@
  @header   DXDB_guts.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc

  Macro to calculate the first derivatives of the 
  shift with respect to x

  The macro is defined in terms of standard variables in
  @seefile DXDB_declare.h
  @enddesc
@@*/

#ifndef DXDB_GUTS
#define DXDB_GUTS

#ifdef FCODE 

      DXDB_OO2DX = 1D0/(2D0*DXDB_DX)
    
      DXDB_DXDBX = DXDB_OO2DX*(DXDB_BX_IP - DXDB_BX_IM)
      DXDB_DXDBY = DXDB_OO2DX*(DXDB_BY_IP - DXDB_BY_IM)
      DXDB_DXDBZ = DXDB_OO2DX*(DXDB_BZ_IP - DXDB_BZ_IM)

#endif

#ifdef CCODE

      DXDB_OO2DX = 1/(2*cctkGH->cctk_delta_space[0]);
    
      DXDB_DXDBX = DXDB_OO2DX*(DXDB_BX_IP - DXDB_BX_IM);
      DXDB_DXDBY = DXDB_OO2DX*(DXDB_BY_IP - DXDB_BY_IM);
      DXDB_DXDBZ = DXDB_OO2DX*(DXDB_BZ_IP - DXDB_BZ_IM);

#endif

#endif

