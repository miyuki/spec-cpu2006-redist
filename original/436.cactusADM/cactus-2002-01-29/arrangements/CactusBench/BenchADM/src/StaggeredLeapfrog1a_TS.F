/*@@
  @file      StaggeredLeapfrog1a_TS.F
  @desc
     First step of staggered leapfrog stepper for the ADM Evolution.
     for time symmetric initial data
  @enddesc
@@*/

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

/*@@
  @routine    Bench_StaggeredLeapfrog1
  @desc
  Time symmetric initial data
  @enddesc
  @calls
  @calledby
  @history
  @endhistory 
@@*/

      subroutine Bench_StaggeredLeapfrog1a_TS(CCTK_ARGUMENTS)

      implicit none

      DECLARE_CCTK_ARGUMENTS
      DECLARE_CCTK_PARAMETERS

      ADM_gxx = gxx
      ADM_gxy = gxy
      ADM_gxz = gxz
      ADM_gyy = gyy
      ADM_gyz = gyz
      ADM_gzz = gzz
      
      ADM_kxx_stag_p = kxx
      ADM_kxy_stag_p = kxy
      ADM_kxz_stag_p = kxz
      ADM_kyy_stag_p = kyy
      ADM_kyz_stag_p = kyz
      ADM_kzz_stag_p = kzz

      ADM_kxx_stag = kxx
      ADM_kxy_stag = kxy
      ADM_kxz_stag = kxz
      ADM_kyy_stag = kyy
      ADM_kyz_stag = kyz
      ADM_kzz_stag = kzz

c     Need this because we are not applying BCs
c     A bit dangerous since it is driver dependent
      ADM_kxx_stag_p_p = 0.0d0
      ADM_kxy_stag_p_p = 0.0d0
      ADM_kxz_stag_p_p = 0.0d0
      ADM_kyy_stag_p_p = 0.0d0
      ADM_kyz_stag_p_p = 0.0d0
      ADM_kzz_stag_p_p = 0.0d0

      end subroutine Bench_StaggeredLeapfrog1a_TS
