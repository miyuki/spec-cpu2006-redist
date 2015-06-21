/*@@
  @header   DXDB_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DXDB_guts.h to compute first 
  derivatives of the shift with respect to x
  @enddesc
@@*/

#ifndef DXDB_DECLARE
#define DXDB_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DXDB_BX_IP 
#define DXDB_BX_IP betax(i+1,j,k)
#undef  DXDB_BX_IM
#define DXDB_BX_IM betax(i-1,j,k)
#undef  DXDB_BY_IP 
#define DXDB_BY_IP betay(i+1,j,k)
#undef  DXDB_BY_IM 
#define DXDB_BY_IM betay(i-1,j,k)
#undef  DXDB_BZ_IP 
#define DXDB_BZ_IP betaz(i+1,j,k)
#undef  DXDB_BZ_IM
#define DXDB_BZ_IM betaz(i-1,j,k)

/* Output variables */ 
#undef  DXDB_DXDBX
#define DXDB_DXDBX  dxdb_dxdbx
#undef  DXDB_DXDBY
#define DXDB_DXDBY  dxdb_dxdby
#undef  DXDB_DXDBZ
#define DXDB_DXDBZ  dxdb_dxdbz

/* Internal variables */
#undef  DXDB_DX
#define DXDB_DX dx
#undef  DXDB_OO2DX   
#define DXDB_OO2DX dxdb_oo2dx

/* Declare internal variables */
      CCTK_REAL DXDB_OO2DX

/* Declare output variables */
      CCTK_REAL DXDB_DXDBX
      CCTK_REAL DXDB_DXDBY
      CCTK_REAL DXDB_DXDBZ

#endif


#ifdef CCODE

/* Input variables */
#undef  DXDB_BX_IP 
#define DXDB_BX_IP betax[ di+ijk]
#undef  DXDB_BX_IM
#define DXDB_BX_IM betax[-di+ijk]
#undef  DXDB_BY_IP 
#define DXDB_BY_IP betay[ di+ijk]
#undef  DXDB_BY_IM 
#define DXDB_BY_IM betay[-di+ijk]
#undef  DXDB_BZ_IP 
#define DXDB_BZ_IP betaz[ di+ijk]
#undef  DXDB_BZ_IM
#define DXDB_BZ_IM betaz[-di+ijk]

/* Output variables */ 
#undef  DXDB_DXDBX
#define DXDB_DXDBX  delbeta11
#undef  DXDB_DXDBY
#define DXDB_DXDBY  delbeta12
#undef  DXDB_DXDBZ
#define DXDB_DXDBZ  delbeta13

/* Internal variables */
#undef  DXDB_OO2DX   
#define DXDB_OO2DX dxdb_oo2dx

/* Declare internal variables */
double DXDB_OO2DX;

/* Declare output variables */
double DXDB_DXDBX;
double DXDB_DXDBY;
double DXDB_DXDBZ;

#endif

#endif
