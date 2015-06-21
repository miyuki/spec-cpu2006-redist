/*@@
  @header   DETG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Determinants of physical and conformal 3-metric
  @enddesc
@@*/

#ifndef DETG_DECLARE
#define DETG_DECLARE

#ifdef FCODE

/* Input variables */
#undef  DETG_PSI
#define DETG_PSI psi(i,j,k)

#undef  DETG_GXX
#define DETG_GXX gxx(i,j,k)
#undef  DETG_GXY
#define DETG_GXY gxy(i,j,k)
#undef  DETG_GXZ
#define DETG_GXZ gxz(i,j,k)
#undef  DETG_GYY
#define DETG_GYY gyy(i,j,k)
#undef  DETG_GYZ
#define DETG_GYZ gyz(i,j,k)
#undef  DETG_GZZ
#define DETG_GZZ gzz(i,j,k)

/* Output variables */
#undef  DETG_DETG
#define DETG_DETG detg_detg
#undef  DETG_DETCG
#define DETG_DETCG detg_detcg


/* Temporary variables */
#undef  DETG_PSI4
#define DETG_PSI4   detg_psi4

#undef  DETG_TEMPXX
#define DETG_TEMPXX detg_tempxx
#undef  DETG_TEMPXY
#define DETG_TEMPXY detg_tempxy
#undef  DETG_TEMPXZ
#define DETG_TEMPXZ detg_tempxz
#undef  DETG_TEMPYY
#define DETG_TEMPYY detg_tempyy
#undef  DETG_TEMPYZ
#define DETG_TEMPYZ detg_tempyz
#undef  DETG_TEMPZZ
#define DETG_TEMPZZ detg_tempzz

/* Declare internal variables */
      CCTK_REAL DETG_PSI4
      CCTK_REAL DETG_TEMPXX,DETG_TEMPXY,DETG_TEMPXZ
      CCTK_REAL DETG_TEMPYY,DETG_TEMPYZ,DETG_TEMPZZ

/* Declare output variables */
      CCTK_REAL DETG_DETG, DETG_DETCG

#endif




#ifdef CCODE

/* Input variables */
#undef  DETG_PSI
#define DETG_PSI psi[ijk]

#undef  DETG_GXX
#define DETG_GXX gxx[ijk]
#undef  DETG_GXY
#define DETG_GXY gxy[ijk]
#undef  DETG_GXZ
#define DETG_GXZ gxz[ijk]
#undef  DETG_GYY
#define DETG_GYY gyy[ijk]
#undef  DETG_GYZ
#define DETG_GYZ gyz[ijk]
#undef  DETG_GZZ
#define DETG_GZZ gzz[ijk]

/* Output variables */
#undef  DETG_DETG
#define DETG_DETG  detg_detg
#undef  DETG_DETCG
#define DETG_DETCG detg_detcg

/* Internal variables */

#undef  DETG_PSI4 
#define DETG_PSI4 detg_psi4

#undef  DETG_TEMPXX
#define DETG_TEMPXX detg_tempxx
#undef  DETG_TEMPXY
#define DETG_TEMPXY detg_tempxy
#undef  DETG_TEMPXZ
#define DETG_TEMPXZ detg_tempxz
#undef  DETG_TEMPYY
#define DETG_TEMPYY detg_tempyy
#undef  DETG_TEMPYZ
#define DETG_TEMPYZ detg_tempyz
#undef  DETG_TEMPZZ
#define DETG_TEMPZZ detg_tempzz

/* Declare internal variables */
double DETG_PSI4;
double DETG_TEMPXX;
double DETG_TEMPXY;
double DETG_TEMPXZ;
double DETG_TEMPYY;
double DETG_TEMPYZ;
double DETG_TEMPZZ;

/* Declare output variables */
double DETG_DETG;
double DETG_DETCG;

#endif

#endif


