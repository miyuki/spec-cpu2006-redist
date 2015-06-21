/*@@
  @header   DXDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro @seefile DXDG_guts.h to compute first 
  derivatives of the physical metric with respect to x
  @enddesc
@@*/

#ifndef DXDG_DECLARE
#define DXDG_DECLARE

#include "CactusEinstein/Einstein/src/macro/DXDCG_declare.h"

#ifdef FCODE

/* Input variables */
#undef  DXDG_PSI
#define DXDG_PSI psi(i,j,k)

#undef  DXDG_DXDPSI_O_PSI
#define DXDG_DXDPSI_O_PSI psix(i,j,k)

#undef  DXDG_GXX    
#define DXDG_GXX    gxx(i,j,k)
#undef  DXDG_GXY   
#define DXDG_GXY    gxy(i,j,k)
#undef  DXDG_GXZ   
#define DXDG_GXZ    gxz(i,j,k)
#undef  DXDG_GYY    
#define DXDG_GYY    gyy(i,j,k)
#undef  DXDG_GYZ   
#define DXDG_GYZ    gyz(i,j,k)
#undef  DXDG_GZZ   
#define DXDG_GZZ    gzz(i,j,k)

/* Output variables */ 
#undef  DXDG_DXDGXX
#define DXDG_DXDGXX  dxdgxx
#undef  DXDG_DXDGXY
#define DXDG_DXDGXY  dxdgxy
#undef  DXDG_DXDGXZ
#define DXDG_DXDGXZ  dxdgxz
#undef  DXDG_DXDGYY
#define DXDG_DXDGYY  dxdgyy
#undef  DXDG_DXDGYZ
#define DXDG_DXDGYZ  dxdgyz
#undef  DXDG_DXDGZZ
#define DXDG_DXDGZZ  dxdgzz

/* Internal variables */
#undef  DXDG_PSI4
#define DXDG_PSI4  dxdg_psi4

#undef  DXDG_FAC
#define DXDG_FAC   dxdg_fac

/* Declare internal variables */
      CCTK_REAL DXDG_PSI4;
      CCTK_REAL DXDG_FAC;

/* Declare output variables */
      CCTK_REAL DXDG_DXDGXX;
      CCTK_REAL DXDG_DXDGXY;
      CCTK_REAL DXDG_DXDGXZ;
      CCTK_REAL DXDG_DXDGYY;
      CCTK_REAL DXDG_DXDGYZ;
      CCTK_REAL DXDG_DXDGZZ;

#endif

#ifdef CCODE

/* Input variables */
#undef  DXDG_PSI
#define DXDG_PSI psi[ijk]

#undef  DXDG_DXDPSI_O_PSI
#define DXDG_DXDPSI_O_PSI psix[ijk]

#undef  DXDG_GXX    
#define DXDG_GXX    gxx[ijk]
#undef  DXDG_GXY   
#define DXDG_GXY    gxy[ijk]
#undef  DXDG_GXZ   
#define DXDG_GXZ    gxz[ijk]
#undef  DXDG_GYY    
#define DXDG_GYY    gyy[ijk]
#undef  DXDG_GYZ   
#define DXDG_GYZ    gyz[ijk]
#undef  DXDG_GZZ   
#define DXDG_GZZ    gzz[ijk]

/* Output variables */ 
#undef  DXDG_DXDGXX
#define DXDG_DXDGXX  delg111
#undef  DXDG_DXDGXY
#define DXDG_DXDGXY  delg112
#undef  DXDG_DXDGXZ
#define DXDG_DXDGXZ  delg113
#undef  DXDG_DXDGYY
#define DXDG_DXDGYY  delg122
#undef  DXDG_DXDGYZ
#define DXDG_DXDGYZ  delg123
#undef  DXDG_DXDGZZ
#define DXDG_DXDGZZ  delg133

/* Internal variables */
#undef  DXDG_PSI4
#define DXDG_PSI4  dxdg_psi4

#undef  DXDG_FAC
#define DXDG_FAC   dxdg_fac

/* Declare internal variables */
double DXDG_PSI4;
double DXDG_FAC;

/* Declare output variables */
double DXDG_DXDGXX;
double DXDG_DXDGXY;
double DXDG_DXDGXZ;
double DXDG_DXDGYY;
double DXDG_DXDGYZ;
double DXDG_DXDGZZ;

#endif

#endif
