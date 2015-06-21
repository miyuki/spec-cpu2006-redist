/*@@
  @file      ADM_Preloop.F
  @date      Aug 1998
  @author    Gabrielle Allen
  @desc 
     Report on parameters used for ADM_Evolution
  @enddesc 
@@*/

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

/*@@
  @routine    Bench_Preloop
  @date       Aug 1998
  @author     Gabrielle Allen
  @desc 
     Report on parameters used for ADM_Evolution
     Basically just a copy of ADM_Lapse.F with output
  @enddesc 
  @calls     
  @history 
   @hauthor Gerd Lanfermann
   @hdate July 99
   @hdesc
     For the slicing, the routine have been changed to use the slicing handles.
     Read ./Einstein/Slicing.c for the details. There is a grid scalar
     called "active_slicing_handle" which defines what slicing to use in the
     next iteration(it's a number). Every slicing checks against this number 
     by calling GetSlicingHandle, if it matches, it does the slicing.

     This basically substituted the prev. CCTK_Equals(slicing,"  ") comparison.

  @endhistory 
@@*/

      subroutine Bench_Preloop(CCTK_ARGUMENTS)

      implicit none

      DECLARE_CCTK_ARGUMENTS
      DECLARE_CCTK_PARAMETERS

c     Perhaps this and others should go into cctk.h
      INTEGER Einstein_GetSlicingHandle

      CHARACTER*200 :: infoline
      CHARACTER*200 :: fortslicing
      CCTK_INT      :: nslicing

c     Get a fortran string for the slicing
      call CCTK_FortranString(nslicing,slicing,fortslicing)
      
c     Set up the limits of loops for the evolution methods
c     ----------------------------------------------------
c      (stencil width = 1)
c      i1 = 2
c      i2 = cctk_lsh(1)-1
c      j1 = 2
c      j2 = cctk_lsh(2)-1
c      k1 = 2
c      k2 = cctk_lsh(3)-1

c     GEODESIC SLICING
c     ----------------

      if (active_slicing_handle .eq. Einstein_GetSlicingHandle("geodesic")) then

         call CCTK_INFO("Geodesic slicing (lapse set to unity)")
         call CCTK_INFO("  lapse will not be evolved in ADM routines")

      else
         call CCTK_WARN(1,"WARNING slicing registered but not coded!")
      end if

      return
      end

