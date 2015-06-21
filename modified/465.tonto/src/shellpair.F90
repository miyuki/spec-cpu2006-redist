!-------------------------------------------------------------------------------
!
! SHELLPAIR : pair of gaussian SHELLs, incorporating stored intermediate data
! valuable for integral evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA  02111-1307, USA.
!
! $Id: shellpair.foo,v 1.2 2003/02/19 07:48:58 reaper Exp $
!-------------------------------------------------------------------------------

module SHELLPAIR_MODULE

#  include "shellpair.use"

   implicit none

#  include "macros"
#  include "shellpair.int"


contains

   subroutine create(self)
    SHELLPAIR :: self
   ! Creates a shell2 object
     PTR :: self
     STACK("SHELLPAIR:create")
     START_TIMER("SHELLPAIR:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(SHELLPAIR_SIZE)
     call nullify_ptr_part_(self)
     STOP_TIMER("SHELLPAIR:create")
      UNSTACK
   end subroutine
 
   subroutine create_1(self,shell_a,shell_b)
    SHELLPAIR :: self
   ! Create a copy of a shell1 objects
     PTR :: self
     SHELL, IN :: shell_a,shell_b
     STACK("SHELLPAIR:create_1")
     START_TIMER("SHELLPAIR:create_1")
     call create_(self)
     call copy_(self,shell_a,shell_b)
     STOP_TIMER("SHELLPAIR:create_1")
      UNSTACK
   end subroutine
 
   subroutine destroy(self)
    SHELLPAIR :: self
   ! Destroys a shell2 object
     PTR :: self
     STACK("SHELLPAIR:destroy")
     START_TIMER("SHELLPAIR:destroy")
     if (NOT associated(self)) then; STOP_TIMER("SHELLPAIR:destroy") UNSTACK return; end if
     call destroy_ptr_part_(self)
     DELETE_MEMORY(SHELLPAIR_SIZE)
     deallocate(self)
     STOP_TIMER("SHELLPAIR:destroy")
      UNSTACK
   end subroutine
 
   subroutine nullify_ptr_part(self)
    SHELLPAIR :: self
   ! Nullify the pointer parts of self
     STACK("SHELLPAIR:nullify_ptr_part")
     START_TIMER("SHELLPAIR:nullify_ptr_part")
     call nullify_ptr_part_(self%a)
     call nullify_ptr_part_(self%b)
     nullify(self%exponent_sum)
     nullify(self%exponent_inv)
     nullify(self%a_exponent_inv)
     nullify(self%b_exponent_inv)
     nullify(self%cc_prefactor)
     nullify(self%normalising_factors)
     nullify(self%hrr_comp_to_use)
     nullify(self%hrr_components)
     nullify(self%hrr_index_smaller)
     nullify(self%hrr_index_larger)
     nullify(self%form_3dints_x_indices)
     nullify(self%form_3dints_y_indices)
     nullify(self%form_3dints_z_indices)
     nullify(self%form_3dints_yz_rms_indices)
     STOP_TIMER("SHELLPAIR:nullify_ptr_part")
      CHECK
   end subroutine
 
   subroutine destroy_ptr_part(self)
    SHELLPAIR :: self
   ! Destroy the pointer parts of self
     STACK("SHELLPAIR:destroy_ptr_part")
     START_TIMER("SHELLPAIR:destroy_ptr_part")
     call destroy_ptr_part_(self%a)
     call destroy_ptr_part_(self%b)
     call destroy_(self%exponent_sum)
     call destroy_(self%exponent_inv)
     call destroy_(self%a_exponent_inv)
     call destroy_(self%b_exponent_inv)
     call destroy_(self%cc_prefactor)
     call destroy_(self%normalising_factors)
     call destroy_(self%hrr_comp_to_use)
     call destroy_(self%hrr_components)
     call destroy_(self%hrr_index_smaller)
     call destroy_(self%hrr_index_larger)
     call destroy_(self%form_3dints_x_indices)
     call destroy_(self%form_3dints_y_indices)
     call destroy_(self%form_3dints_z_indices)
     call destroy_(self%form_3dints_yz_rms_indices)
     STOP_TIMER("SHELLPAIR:destroy_ptr_part")
      UNSTACK
   end subroutine
 
!   created result(res)
!   ! Returns true if self has been created
!     self :: PTR
!     res :: BIN
!     res = associated(self)
!   end
 
!   destroyed result(res)
!   ! Returns true if self has *not* been created
!     self :: PTR
!     res :: BIN
!     res = NOT associated(self)
!   end
 
   subroutine create_copy(self,ab)
    SHELLPAIR :: self
   ! Create a copy of the "ab" shellpair
     PTR :: self
     SHELLPAIR, IN :: ab
     STACK("SHELLPAIR:create_copy")
     START_TIMER("SHELLPAIR:create_copy")
     call create_(self)
     call copy_(self,ab)
     STOP_TIMER("SHELLPAIR:create_copy")
      UNSTACK
   end subroutine
 
   subroutine copy(self,ab)
    SHELLPAIR :: self
   ! Copy the using shellpair "ab" and also set precalculated data.
   ! NOTE : ensure the ptr parts have been destroyed beforehand.
     SHELLPAIR, IN :: ab
     STACK("SHELLPAIR:copy")
     START_TIMER("SHELLPAIR:copy")
     call copy_(self%a,ab%a)
     call copy_(self%b,ab%b)
     call make_precalculated_data_(self)
     STOP_TIMER("SHELLPAIR:copy")
      UNSTACK
   end subroutine
 
   subroutine copy_1(self,shell_a,shell_b)
    SHELLPAIR :: self
   ! Copy the shell2 using shell1 objects
     SHELL, IN :: shell_a,shell_b
     STACK("SHELLPAIR:copy_1")
     START_TIMER("SHELLPAIR:copy_1")
     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     call make_precalculated_data_(self)
     STOP_TIMER("SHELLPAIR:copy_1")
      UNSTACK
   end subroutine
 
   subroutine copy_a(self,shell_a)
    SHELLPAIR :: self
   ! Copy the "a" shell of the shell2 objects from "shell_a"
     SHELL, IN :: shell_a
     STACK("SHELLPAIR:copy_a")
     START_TIMER("SHELLPAIR:copy_a")
     call copy_(self%a,shell_a)
     STOP_TIMER("SHELLPAIR:copy_a")
      UNSTACK
   end subroutine
 
   subroutine copy_b(self,shell_b)
    SHELLPAIR :: self
   ! Copy the "b" shell of the shell2 objects from "shell_b"
     SHELL, IN :: shell_b
     STACK("SHELLPAIR:copy_b")
     START_TIMER("SHELLPAIR:copy_b")
     call copy_(self%b,shell_b)
     STOP_TIMER("SHELLPAIR:copy_b")
      UNSTACK
   end subroutine
 
   subroutine unnormalise(self)
    SHELLPAIR :: self
   ! Unnormalise each shell
     STACK("SHELLPAIR:unnormalise")
     START_TIMER("SHELLPAIR:unnormalise")
     call unnormalise_(self%a)
     call unnormalise_(self%b)
     STOP_TIMER("SHELLPAIR:unnormalise")
      CHECK
   end subroutine
 
   subroutine make_precalculated_data(self)
    SHELLPAIR :: self
   ! Precalculate some data for the 1/r_{12} electron repulsion integrals
     REAL :: b_cc,a,b,ab_sum,ab_inv
     INT :: ag,bg,i,j,ub
     REALVEC(:), PTR :: anorm,bnorm
     STACK("SHELLPAIR:make_precalculated_data")
     START_TIMER("SHELLPAIR:make_precalculated_data")
     self%n_gaussian_pairs = self%a%n_cc*self%b%n_cc
     self%l_max = max(self%a%l,self%b%l)
     self%l_min = min(self%a%l,self%b%l)
     self%l_sum = self%a%l + self%b%l
     call create_(self%exponent_sum,self%n_gaussian_pairs)
     call create_(self%exponent_inv,self%n_gaussian_pairs)
     call create_(self%a_exponent_inv,self%n_gaussian_pairs)
     call create_(self%b_exponent_inv,self%n_gaussian_pairs)
     call create_(self%cc_prefactor,self%n_gaussian_pairs)
     call create_(self%normalising_factors,n_comp_(self%a%l)*n_comp_(self%b%l))
     call create_(anorm,n_comp_(self%a%l))
     call create_(bnorm,n_comp_(self%b%l))
     call normalising_factors_(anorm,self%a%l)
     call normalising_factors_(bnorm,self%b%l)
     i = 0
     do bg = 1,self%b%n_cc
       b      = self%b%ex(bg)
       b_cc   = self%b%cc(bg)
       do ag = 1,self%a%n_cc
         i = i + 1
         a = self%a%ex(ag)
         ab_sum = a + b
         ab_inv = ONE/ab_sum
         self%exponent_sum(i)        = ab_sum
         self%exponent_inv(i)        = ab_inv
         self%cc_prefactor(i)        = b_cc*self%a%cc(ag) *ab_inv*sqrt(ab_inv)
         self%a_exponent_inv(i)      = a*ab_inv
         self%b_exponent_inv(i)      = b*ab_inv
       end do
     end do
     i = 0
     do bg=1,n_comp_(self%b%l)
       do ag=1,n_comp_(self%a%l)
         i = i + 1
         self%normalising_factors(i) = anorm(ag)*bnorm(bg)
       end do
     end do
     call destroy_(bnorm)
     call destroy_(anorm)
 
     ub = n_comp_sum_(self%l_sum) - n_comp_sum_((self%l_max-1))
     call create_(self%hrr_index_larger,0,self%l_sum,0,self%l_sum,0,self%l_sum)
     call create_(self%hrr_index_smaller,0,self%l_sum,0,self%l_sum,0,self%l_sum)
     call create_(self%form_3dints_x_indices,ub)
     call create_(self%form_3dints_y_indices,ub)
     call create_(self%form_3dints_z_indices,ub)
     call create_(self%hrr_components,3,n_comp_sum_(self%l_sum))
     call create_(self%hrr_comp_to_use,n_comp_sum_(self%l_sum))
     i=0;
     call make_gaussian_xyz_powers_(i,self%hrr_components,self%l_sum,self%hrr_index_smaller,self%hrr_comp_to_use)
     call make_gaussian_xyz_indices_(self%l_max,self%form_3dints_x_indices,self%form_3dints_y_indices, &
                      self%form_3dints_z_indices,self%hrr_index_larger,self%l_sum)
     call create_(self%form_3dints_yz_rms_indices,ub)
     do i=1,ub
       j = self%form_3dints_z_indices(i)
       self%form_3dints_yz_rms_indices(i) = -self%l_sum - 2 + j*(2*self%l_sum+5-j)/2 + &
                                   self%form_3dints_y_indices(i)
     end do
     STOP_TIMER("SHELLPAIR:make_precalculated_data")
      UNSTACK
   end subroutine

end
