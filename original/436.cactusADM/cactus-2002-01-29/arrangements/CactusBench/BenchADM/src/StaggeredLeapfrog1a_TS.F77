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

      integer i,j,k
      integer nx,ny,nz

      DECLARE_CCTK_PARAMETERS

      nx = cctk_lsh(1)
      ny = cctk_lsh(2)
      nz = cctk_lsh(3)

      do k=1,nz
         do j=1,ny
            do i=1,nx

      ADM_gxx(i,j,k) = gxx(i,j,k)
      ADM_gxy(i,j,k) = gxy(i,j,k)
      ADM_gxz(i,j,k) = gxz(i,j,k)
      ADM_gyy(i,j,k) = gyy(i,j,k)
      ADM_gyz(i,j,k) = gyz(i,j,k)
      ADM_gzz(i,j,k) = gzz(i,j,k)
      
      ADM_kxx_stag_p(i,j,k) = kxx(i,j,k)
      ADM_kxy_stag_p(i,j,k) = kxy(i,j,k)
      ADM_kxz_stag_p(i,j,k) = kxz(i,j,k)
      ADM_kyy_stag_p(i,j,k) = kyy(i,j,k)
      ADM_kyz_stag_p(i,j,k) = kyz(i,j,k)
      ADM_kzz_stag_p(i,j,k) = kzz(i,j,k)

      ADM_kxx_stag(i,j,k) = kxx(i,j,k)
      ADM_kxy_stag(i,j,k) = kxy(i,j,k)
      ADM_kxz_stag(i,j,k) = kxz(i,j,k)
      ADM_kyy_stag(i,j,k) = kyy(i,j,k)
      ADM_kyz_stag(i,j,k) = kyz(i,j,k)
      ADM_kzz_stag(i,j,k) = kzz(i,j,k)

c     Need this because we are not applying BCs
c     A bit dangerous since it is driver dependent
      ADM_kxx_stag_p_p(i,j,k) = 0.0d0
      ADM_kxy_stag_p_p(i,j,k) = 0.0d0
      ADM_kxz_stag_p_p(i,j,k) = 0.0d0
      ADM_kyy_stag_p_p(i,j,k) = 0.0d0
      ADM_kyz_stag_p_p(i,j,k) = 0.0d0
      ADM_kzz_stag_p_p(i,j,k) = 0.0d0

	end do
	end do
	end do

	return
      end 
