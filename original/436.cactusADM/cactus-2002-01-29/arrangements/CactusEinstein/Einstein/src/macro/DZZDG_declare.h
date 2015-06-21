/*@@
  @header   DZZDG_declare.h
  @date     Jun 98
  @author   Gabrielle Allen
  @desc
  Declarations for macro to calculate the (first and) second derivatives 
  of the physical metric with respect to z

  The macro uses @seefile DZDG_guts.h and @seefile DZDG_declare.h
  @enddesc
@@*/

#ifndef DZZDG_DECLARE
#define DZZDG_DECLARE

#ifdef FCODE

/* Output variables */
#undef  DZZDG_DZZDGXX
#define DZZDG_DZZDGXX dzzdgxx
#undef  DZZDG_DZZDGXY
#define DZZDG_DZZDGXY dzzdgxy
#undef  DZZDG_DZZDGXZ
#define DZZDG_DZZDGXZ dzzdgxz
#undef  DZZDG_DZZDGYY
#define DZZDG_DZZDGYY dzzdgyy
#undef  DZZDG_DZZDGYZ
#define DZZDG_DZZDGYZ dzzdgyz
#undef  DZZDG_DZZDGZZ
#define DZZDG_DZZDGZZ dzzdgzz

/* Internal variables */
#undef  DZZDG_FAC
#define DZZDG_FAC dzzdg_fac
#undef  DZZDG_OODZ2
#define DZZDG_OODZ2 dzzdg_oodz2
#undef  DZZDG_DZZDPSI_O_PSI
#define DZZDG_DZZDPSI_O_PSI psizz(i,j,k)

/* Declare internal variables */
      CCTK_REAL DZZDG_FAC
      CCTK_REAL DZZDG_OODZ2

/* Declare output variables */
      CCTK_REAL DZZDG_DZZDGXX
      CCTK_REAL DZZDG_DZZDGXY
      CCTK_REAL DZZDG_DZZDGXZ
      CCTK_REAL DZZDG_DZZDGYY
      CCTK_REAL DZZDG_DZZDGYZ
      CCTK_REAL DZZDG_DZZDGZZ

#endif

#ifdef CCODE

/* Output variables */
#undef  DZZDG_DZZDGXX
#define DZZDG_DZZDGXX deldelg3311
#undef  DZZDG_DZZDGXY
#define DZZDG_DZZDGXY deldelg3312
#undef  DZZDG_DZZDGXZ
#define DZZDG_DZZDGXZ deldelg3313
#undef  DZZDG_DZZDGYY
#define DZZDG_DZZDGYY deldelg3322
#undef  DZZDG_DZZDGYZ
#define DZZDG_DZZDGYZ deldelg3323
#undef  DZZDG_DZZDGZZ
#define DZZDG_DZZDGZZ deldelg3333

/* Internal variables */
#undef  DZZDG_FAC
#define DZZDG_FAC dzzdg_fac
#undef  DZZDG_OODZ2
#define DZZDG_OODZ2 dzzdg_oodz2
#undef  DZZDG_DZZDPSI_O_PSI
#define DZZDG_DZZDPSI_O_PSI psizz[ijk]

/* Declare internal variables */
double DZZDG_FAC;
double DZZDG_OODZ2 = 1/(cctkGH->cctk_delta_space[2]*cctkGH->cctk_delta_space[2]);

/* Declare output variables */
double DZZDG_DZZDGXX;
double DZZDG_DZZDGXY;
double DZZDG_DZZDGXZ;
double DZZDG_DZZDGYY;
double DZZDG_DZZDGYZ;
double DZZDG_DZZDGZZ;

#endif

#endif
