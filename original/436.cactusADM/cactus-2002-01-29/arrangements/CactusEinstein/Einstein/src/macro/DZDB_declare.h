/*@@
  @header   DZDB_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DZDB_guts.h to compute first 
  derivatives of the shift with respect to z
  @enddesc
@@*/

#ifndef DZDB_DECLARE
#define DZDB_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DZDB_BX_KP 
#define DZDB_BX_KP betax(i,j,k+1)
#undef  DZDB_BX_KM
#define DZDB_BX_KM betax(i,j,k-1)
#undef  DZDB_BY_KP 
#define DZDB_BY_KP betay(i,j,k+1)
#undef  DZDB_BY_KM 
#define DZDB_BY_KM betay(i,j,k-1)
#undef  DZDB_BZ_KP 
#define DZDB_BZ_KP betaz(i,j,k+1)
#undef  DZDB_BZ_KM
#define DZDB_BZ_KM betaz(i,j,k-1)

/* Output variables */ 
#undef  DZDB_DZDBX
#define DZDB_DZDBX  dzdb_dzdbx
#undef  DZDB_DZDBY
#define DZDB_DZDBY  dzdb_dzdby
#undef  DZDB_DZDBZ
#define DZDB_DZDBZ  dzdb_dzdbz

/* Internal variables */
#undef  DZDB_DZ
#define DZDB_DZ dz
#undef  DZDB_OO2DZ   
#define DZDB_OO2DZ dzdb_oo2dz

/* Declare internal variables */
      CCTK_REAL DZDB_OO2DZ;

/* Declare output variables */
      CCTK_REAL DZDB_DZDBX
      CCTK_REAL DZDB_DZDBY
      CCTK_REAL DZDB_DZDBZ

#endif

#ifdef CCODE

/* Input variables */
#undef  DZDB_BX_KP 
#define DZDB_BX_KP betax[ dj+ijk]
#undef  DZDB_BX_KM
#define DZDB_BX_KM betax[-dj+ijk]
#undef  DZDB_BY_KP 
#define DZDB_BY_KP betay[ dj+ijk]
#undef  DZDB_BY_KM 
#define DZDB_BY_KM betay[-dj+ijk]
#undef  DZDB_BZ_KP 
#define DZDB_BZ_KP betaz[ dj+ijk]
#undef  DZDB_BZ_KM
#define DZDB_BZ_KM betaz[-dj+ijk]

/* Output variables */ 
#undef  DZDB_DZDBX
#define DZDB_DZDBX  delbeta31
#undef  DZDB_DZDBY
#define DZDB_DZDBY  delbeta32
#undef  DZDB_DZDBZ
#define DZDB_DZDBZ  delbeta33

/* Internal variables */
#undef  DZDB_OO2DZ   
#define DZDB_OO2DZ dzdb_oo2dz

/* Declare internal variables */
double DZDB_OO2DZ;

/* Declare output variables */
double DZDB_DZDBX;
double DZDB_DZDBY;
double DZDB_DZDBZ;

#endif

#endif
