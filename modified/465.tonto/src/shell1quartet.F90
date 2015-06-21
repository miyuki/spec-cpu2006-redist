!*******************************************************************************
!
! SHELL1QUARTET : Group of 4 SHELL1s, used for two-electron integrals.  This
! module is designed for speed, but still try to keep it readable.
!
! In getting optimal speed, this module makes some assumptions...
! 1.  The bra is on the outer loop, then ket is on the inner loop.
!
!
! Copyright (C) Daniel Grimwood, 2002
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
! $Id: shell1quartet.foo,v 1.2.2.4.4.1 2004/07/02 03:57:36 reaper Exp $
!*******************************************************************************

module SHELL1QUARTET_MODULE

#  include "shell1quartet.use"

  implicit none

#  include "macros"
#  include "shell1quartet.int"


  INT :: n_cc_cutoff = 4
  ! The minimum number of contractions of the ab shellpair for which to
  ! precalculate the cd shellpair data.

  INT :: ERI_rms_min_l = 4
  ! The minimum value of l angular momentum for a shellpair to perform the
  ! reduced multiplication scheme.

contains

!*******************************************************************************
!  Create/Destroy routines.
!*******************************************************************************

   subroutine create(self)
    SHELL1QUARTET :: self
   ! Create a shell4 object, but no its component shells.
     PTR :: self
     STACK("SHELL1QUARTET:create")
     START_TIMER("SHELL1QUARTET:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(SHELL1QUARTET_SIZE)
     self%ab_nullify = TRUE
     self%cd_nullify = TRUE
     call nullify_ptr_part_(self)
     STOP_TIMER("SHELL1QUARTET:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SHELL1QUARTET :: self
   ! Destroy a shell4 object.
     PTR :: self
     STACK("SHELL1QUARTET:destroy")
     START_TIMER("SHELL1QUARTET:destroy")
     if (associated(self)) then
       call destroy_ptr_part_(self)
       DELETE_MEMORY(SHELL1QUARTET_SIZE)
       deallocate(self)
     end if
     STOP_TIMER("SHELL1QUARTET:destroy")
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

   subroutine nullify_ptr_part(self)
    SHELL1QUARTET :: self
   ! Nullify the pointer parts of self
     STACK("SHELL1QUARTET:nullify_ptr_part")
     START_TIMER("SHELL1QUARTET:nullify_ptr_part")
     call nullify_ptr_part_(self%a)
     call nullify_ptr_part_(self%b)
     call nullify_ptr_part_(self%c)
     call nullify_ptr_part_(self%d)
     STOP_TIMER("SHELL1QUARTET:nullify_ptr_part")
      UNSTACK
   end subroutine

   subroutine destroy_ptr_part(self)
    SHELL1QUARTET :: self
   ! Destroy the pointer parts of self
     STACK("SHELL1QUARTET:destroy_ptr_part")
     START_TIMER("SHELL1QUARTET:destroy_ptr_part")
     call destroy_ab_(self)
     call destroy_cd_(self)
     STOP_TIMER("SHELL1QUARTET:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine destroy_ab(self)
    SHELL1QUARTET :: self
   ! Destroy the shella and shellb pointer parts of self
     STACK("SHELL1QUARTET:destroy_ab")
     START_TIMER("SHELL1QUARTET:destroy_ab")
     if (self%ab_nullify) then
       nullify(self%a)
       nullify(self%b)
       nullify(self%ab_exponent_sum)
       nullify(self%ab_normalising_factors)
       nullify(self%ab_hrr_index_larger)
       nullify(self%ab_hrr_index_smaller)
       nullify(self%ab_form_3dints_x_indices)
       nullify(self%ab_form_3dints_y_indices)
       nullify(self%ab_form_3dints_z_indices)
       nullify(self%ab_form_3dints_yz_rms_indices)
       nullify(self%ab_hrr_components)
       nullify(self%ab_hrr_comp_to_use)
     else
       call destroy_ptr_part_(self%a)
       call destroy_ptr_part_(self%b)
       call destroy_(self%ab_exponent_sum)
       if (self%ab_l_max > 1) then
         call destroy_(self%ab_normalising_factors)
         call destroy_(self%ab_hrr_index_larger)
         call destroy_(self%ab_hrr_index_smaller)
         call destroy_(self%ab_hrr_components)
         call destroy_(self%ab_hrr_comp_to_use)
       end if
       call destroy_(self%ab_form_3dints_x_indices)
       call destroy_(self%ab_form_3dints_y_indices)
       call destroy_(self%ab_form_3dints_z_indices)
       call destroy_(self%ab_form_3dints_yz_rms_indices)
     end if
     call destroy_(self%ab_cc_prefactor)
     call destroy_(self%ab_pair_center)
     call destroy_(self%ab_center_diff)
     STOP_TIMER("SHELL1QUARTET:destroy_ab")
      UNSTACK
   end subroutine

   subroutine destroy_cd(self)
    SHELL1QUARTET :: self
   ! Destroy the shellc and shelld pointer parts of self
     STACK("SHELL1QUARTET:destroy_cd")
     START_TIMER("SHELL1QUARTET:destroy_cd")
     if (self%cd_nullify) then
       nullify(self%c)
       nullify(self%d)
       nullify(self%cd_exponent_sum)
       nullify(self%cd_normalising_factors)
       nullify(self%cd_hrr_index_larger)
       nullify(self%cd_hrr_index_smaller)
       nullify(self%cd_form_3dints_x_indices)
       nullify(self%cd_form_3dints_y_indices)
       nullify(self%cd_form_3dints_z_indices)
       nullify(self%cd_form_3dints_yz_rms_indices)
       nullify(self%cd_hrr_components)
       nullify(self%cd_hrr_comp_to_use)
     else
       call destroy_ptr_part_(self%c)
       call destroy_ptr_part_(self%d)
       call destroy_(self%cd_exponent_sum)
       if (self%cd_l_max > 1) then
         call destroy_(self%cd_normalising_factors)
         call destroy_(self%cd_hrr_index_larger)
         call destroy_(self%cd_hrr_index_smaller)
         call destroy_(self%cd_hrr_components)
         call destroy_(self%cd_hrr_comp_to_use)
       end if
       if (self%cd_l_sum > 1 OR self%ab_l_sum > 1) then
         call destroy_(self%cd_form_3dints_x_indices)
         call destroy_(self%cd_form_3dints_y_indices)
         call destroy_(self%cd_form_3dints_z_indices)
         call destroy_(self%cd_form_3dints_yz_rms_indices)
       end if
     end if
     call destroy_(self%cd_cc_prefactor)
     call destroy_(self%cd_pair_center)
     call destroy_(self%cd_center_diff)
     STOP_TIMER("SHELL1QUARTET:destroy_cd")
      UNSTACK
   end subroutine

!*******************************************************************************
!  Setting parts of self from other shells and shell1s.
!*******************************************************************************

   subroutine set(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    SHELL1QUARTET :: self
   ! Set the shell4 using shell objects and positions
     SHELL, IN :: shell_a,shell_b,shell_c,shell_d
     REALVEC(:), IN :: pos_a,pos_b,pos_c,pos_d
     STACK("SHELL1QUARTET:set")
     START_TIMER("SHELL1QUARTET:set")
     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     call copy_(self%c,shell_c)
     call copy_(self%d,shell_d)
     self%pos_a = pos_a
     self%pos_b = pos_b
     self%pos_c = pos_c
     self%pos_d = pos_d
     call precalculate_ab_(self)
     call precalculate_cd_(self)
     STOP_TIMER("SHELL1QUARTET:set")
      UNSTACK
   end subroutine

   subroutine set_ab(self,shell_a,shell_b,pos_a,pos_b)
    SHELL1QUARTET :: self
   ! Copy the a and b parts of the shell4 using from shell1 objects
     SHELL, IN :: shell_a,shell_b
     REALVEC(:), IN :: pos_a,pos_b
     STACK("SHELL1QUARTET:set_ab")
     START_TIMER("SHELL1QUARTET:set_ab")
     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     self%pos_a = pos_a
     self%pos_b = pos_b
     call precalculate_ab_(self)
     STOP_TIMER("SHELL1QUARTET:set_ab")
      UNSTACK
   end subroutine

   subroutine set_cd(self,shell_c,shell_d,pos_c,pos_d)
    SHELL1QUARTET :: self
   ! Copy the c and d parts of the shell4 using from shell1 objects
     SHELL, IN :: shell_c,shell_d
     REALVEC(:), IN :: pos_c,pos_d
     STACK("SHELL1QUARTET:set_cd")
     START_TIMER("SHELL1QUARTET:set_cd")
     call copy_(self%c,shell_c)
     call copy_(self%d,shell_d)
     self%pos_c = pos_c
     self%pos_d = pos_d
     call precalculate_cd_(self)
     STOP_TIMER("SHELL1QUARTET:set_cd")
      UNSTACK
   end subroutine

!*******************************************************************************
!  Miscellaneous Routines.
!*******************************************************************************

   subroutine unnormalise(self)
    SHELL1QUARTET :: self
   ! Unnormalise each shell in this shell quartet
     STACK("SHELL1QUARTET:unnormalise")
     START_TIMER("SHELL1QUARTET:unnormalise")
     call unnormalise_(self%a)
     call unnormalise_(self%b)
     call unnormalise_(self%c)
     call unnormalise_(self%d)
     STOP_TIMER("SHELL1QUARTET:unnormalise")
      CHECK
   end subroutine

!*******************************************************************************
!  Precalculated stuff.
!*******************************************************************************

  subroutine precalculate_ab(self)
    SHELL1QUARTET :: self
    REALVEC(:), PTR :: anorm,bnorm
    REALVEC(3) :: AB,At,P
    REAL :: b_cc,a,b,ab_sum,ab_inv,r2ab,prefac
    INT :: ag,bg,i,j,ub
    STACK("SHELL1QUARTET:precalculate_ab")
    START_TIMER("SHELL1QUARTET:precalculate_ab")
    self%ab_nullify = FALSE
    self%ab_n_gaussian_pairs = self%a%n_cc*self%b%n_cc
    self%ab_l_max = max(self%a%l,self%b%l)
    self%ab_l_min = min(self%a%l,self%b%l)
    self%ab_l_sum = self%a%l + self%b%l
    call create_(self%ab_exponent_sum,self%ab_n_gaussian_pairs)
    call create_(self%ab_cc_prefactor,self%ab_n_gaussian_pairs)
    call create_(self%ab_pair_center,3,self%ab_n_gaussian_pairs)
    call create_(self%ab_center_diff,3,self%ab_n_gaussian_pairs)
    AB = self%pos_a-self%pos_b
    ! Want position of shell1 with higher angular momentum.
    if (self%a%l > self%b%l) then; At = self%pos_a
    else;                  At = self%pos_b
    end if
    r2ab = dot_product(AB,AB)
    i = 0
    self%ab_kappa_max = ZERO
    do bg = 1,self%b%n_cc
      b      = self%b%ex(bg)
      b_cc   = self%b%cc(bg)
      do ag = 1,self%a%n_cc
        i = i + 1
        a = self%a%ex(ag)
        ab_sum = a + b
        ab_inv = ONE/ab_sum
        P = (b*self%pos_b + a*self%pos_a) * ab_inv
        self%ab_exponent_sum(i)  = ab_sum
        prefac               = b_cc*self%a%cc(ag) *ab_inv*sqrt(ab_inv)* &
                                                       exp(-a*b*r2ab*ab_inv)
        self%ab_kappa_max = max(self%ab_kappa_max,prefac/(sqrt(ab_inv)*ab_inv))
        self%ab_cc_prefactor(i)  = prefac
        self%ab_pair_center(:,i) = P
        self%ab_center_diff(:,i) = P - At
      end do
    end do

    if (self%ab_l_max > 1) then
      call create_(self%ab_normalising_factors,n_comp_(self%a%l)*n_comp_(self%b%l))
      call create_(anorm,n_comp_(self%a%l))
      call create_(bnorm,n_comp_(self%b%l))
      call normalising_factors_(anorm,self%a%l)
      call normalising_factors_(bnorm,self%b%l)
      i = 0
      do bg=1,n_comp_(self%b%l)
        do ag=1,n_comp_(self%a%l)
          i = i + 1
          self%ab_normalising_factors(i) = anorm(ag)*bnorm(bg)
        end do
      end do
      call destroy_(bnorm)
      call destroy_(anorm)

      call create_(self%ab_hrr_index_larger,0,self%ab_l_sum,0,self%ab_l_sum,0,self%ab_l_sum)
      call create_(self%ab_hrr_index_smaller,0,self%ab_l_sum,0,self%ab_l_sum,0,self%ab_l_sum)
      call create_(self%ab_hrr_components,3,n_comp_sum_(self%ab_l_sum))
      call create_(self%ab_hrr_comp_to_use,n_comp_sum_(self%ab_l_sum))
      i=0;
      call make_gaussian_xyz_powers_(i,self%ab_hrr_components,self%ab_l_sum,self%ab_hrr_index_smaller,self%ab_hrr_comp_to_use)
      call make_gaussian_xyz_power_index_(self%ab_l_max,self%ab_hrr_index_larger,self%ab_l_sum)
      call make_gaussian_xyz_indices_(self%ab_l_max,self%ab_form_3dints_x_indices, &
                self%ab_form_3dints_y_indices,self%ab_form_3dints_z_indices,self%ab_l_sum)
    end if
    ub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
    call create_(self%ab_form_3dints_x_indices,ub)
    call create_(self%ab_form_3dints_y_indices,ub)
    call create_(self%ab_form_3dints_z_indices,ub)
    call make_gaussian_xyz_indices_(self%ab_l_max,self%ab_form_3dints_x_indices, &
              self%ab_form_3dints_y_indices,self%ab_form_3dints_z_indices,self%ab_l_sum)
    call create_(self%ab_form_3dints_yz_rms_indices,ub)
    do i=1,ub
      j = self%ab_form_3dints_z_indices(i)
      self%ab_form_3dints_yz_rms_indices(i) = -self%ab_l_sum - 2 + j*(2*self%ab_l_sum+5-j)/2 + &
                                  self%ab_form_3dints_y_indices(i)
    end do
    STOP_TIMER("SHELL1QUARTET:precalculate_ab")
     UNSTACK
  end subroutine

  subroutine precalculate_cd(self)
    SHELL1QUARTET :: self
    REALVEC(:), PTR :: cnorm,dnorm
    REALVEC(3) :: CD,Ct,Q
    REAL :: d_cc,c,d,cd_sum,cd_inv,r2cd,prefac
    INT :: cg,dg,i,j,ub
    STACK("SHELL1QUARTET:precalculate_cd")
    START_TIMER("SHELL1QUARTET:precalculate_cd")
    self%cd_nullify = FALSE
    self%cd_n_gaussian_pairs = self%c%n_cc*self%d%n_cc
    self%cd_l_max = max(self%c%l,self%d%l)
    self%cd_l_min = min(self%c%l,self%d%l)
    self%cd_l_sum = self%c%l + self%d%l
    call create_(self%cd_exponent_sum,self%cd_n_gaussian_pairs)
    call create_(self%cd_cc_prefactor,self%cd_n_gaussian_pairs)
    call create_(self%cd_pair_center,3,self%cd_n_gaussian_pairs)
    call create_(self%cd_center_diff,3,self%cd_n_gaussian_pairs)
    CD = self%pos_c-self%pos_d
    ! Want position of shell1 with higher angular momentum.
    if (self%c%l > self%d%l) then; Ct = self%pos_c
    else;                  Ct = self%pos_d
    end if
    r2cd = dot_product(CD,CD)
    i = 0
    self%cd_kappa_max = ZERO
    do dg = 1,self%d%n_cc
      d      = self%d%ex(dg)
      d_cc   = self%d%cc(dg)
      do cg = 1,self%c%n_cc
        i = i + 1
        c = self%c%ex(cg)
        cd_sum = c + d
        cd_inv = ONE/cd_sum
        Q = (d*self%pos_d + c*self%pos_c) * cd_inv
        self%cd_exponent_sum(i)  = cd_sum
        prefac               = d_cc*self%c%cc(cg) *cd_inv*sqrt(cd_inv)* &
                                                  exp(-c*d*r2cd*cd_inv)
        self%cd_cc_prefactor(i)  = prefac
        self%cd_kappa_max = max(self%cd_kappa_max,prefac/(sqrt(cd_inv)*cd_inv))
        self%cd_pair_center(:,i) = Q
        self%cd_center_diff(:,i) = Q - Ct
      end do
    end do

    if (self%cd_l_max > 1) then
      call create_(self%cd_normalising_factors,n_comp_(self%c%l)*n_comp_(self%d%l))
      call create_(cnorm,n_comp_(self%c%l))
      call create_(dnorm,n_comp_(self%d%l))
      call normalising_factors_(cnorm,self%c%l)
      call normalising_factors_(dnorm,self%d%l)
      i = 0
      do dg=1,n_comp_(self%d%l)
        do cg=1,n_comp_(self%c%l)
          i = i + 1
          self%cd_normalising_factors(i) = cnorm(cg)*dnorm(dg)
        end do
      end do
      call destroy_(dnorm)
      call destroy_(cnorm)

      call create_(self%cd_hrr_index_larger,0,self%cd_l_sum,0,self%cd_l_sum,0,self%cd_l_sum)
      call create_(self%cd_hrr_index_smaller,0,self%cd_l_sum,0,self%cd_l_sum,0,self%cd_l_sum)
      call create_(self%cd_hrr_components,3,n_comp_sum_(self%cd_l_sum))
      call create_(self%cd_hrr_comp_to_use,n_comp_sum_(self%cd_l_sum))
      i=0;
      call make_gaussian_xyz_powers_(i,self%cd_hrr_components,self%cd_l_sum,self%cd_hrr_index_smaller,self%cd_hrr_comp_to_use)
      call make_gaussian_xyz_power_index_(self%cd_l_max,self%cd_hrr_index_larger,self%cd_l_sum)
    end if
    if (self%cd_l_sum > 1 OR self%ab_l_sum > 1) then
      ub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))
      call create_(self%cd_form_3dints_x_indices,ub)
      call create_(self%cd_form_3dints_y_indices,ub)
      call create_(self%cd_form_3dints_z_indices,ub)
      call make_gaussian_xyz_indices_(self%cd_l_max,self%cd_form_3dints_x_indices, &
                self%cd_form_3dints_y_indices,self%cd_form_3dints_z_indices,self%cd_l_sum)
      call create_(self%cd_form_3dints_yz_rms_indices,ub)
      do i=1,ub
        j = self%cd_form_3dints_z_indices(i)
        self%cd_form_3dints_yz_rms_indices(i) = -self%cd_l_sum - 2 + j*(2*self%cd_l_sum+5-j)/2 + &
                                    self%cd_form_3dints_y_indices(i)
      end do
    end if
    STOP_TIMER("SHELL1QUARTET:precalculate_cd")
     UNSTACK
  end subroutine

  subroutine set_ab_1(self,shellpr,pos_a,pos_b)
    SHELL1QUARTET :: self
  ! Copy the a and b parts of the shell4 using from shell1 objects
    SHELLPAIR, IN, target :: shellpr
    REALVEC(:), IN :: pos_a,pos_b
    REALVEC(3) :: AB,At,P,b_pos_b
    REAL :: r2ab,a,b,ab_inv,prefac
    INT :: i,bg,ag

    STACK("SHELL1QUARTET:set_ab_1")
    START_TIMER("SHELL1QUARTET:set_ab_1")
    self%ab_nullify = TRUE
    self%a => shellpr%a
    self%b => shellpr%b
    self%ab_exponent_sum        => shellpr%exponent_sum

    self%ab_l_max = shellpr%l_max
    if (self%ab_l_max > 1) then
      self%ab_normalising_factors   => shellpr%normalising_factors
      self%ab_hrr_index_larger      => shellpr%hrr_index_larger
      self%ab_hrr_index_smaller     => shellpr%hrr_index_smaller
      self%ab_hrr_components        => shellpr%hrr_components
      self%ab_hrr_comp_to_use       => shellpr%hrr_comp_to_use
    end if
    self%ab_form_3dints_x_indices => shellpr%form_3dints_x_indices
    self%ab_form_3dints_y_indices => shellpr%form_3dints_y_indices
    self%ab_form_3dints_z_indices => shellpr%form_3dints_z_indices
    self%ab_form_3dints_yz_rms_indices => shellpr%form_3dints_yz_rms_indices

    self%pos_a = pos_a
    self%pos_b = pos_b
    self%ab_n_gaussian_pairs = shellpr%n_gaussian_pairs
    self%ab_l_min = shellpr%l_min
    self%ab_l_sum = shellpr%l_sum
    call create_(self%ab_cc_prefactor,self%ab_n_gaussian_pairs)
    call create_(self%ab_pair_center,3,self%ab_n_gaussian_pairs)
    call create_(self%ab_center_diff,3,self%ab_n_gaussian_pairs)
     AB = pos_a-pos_b
     ! Want position of shell1 with higher angular momentum.
     if (self%a%l > self%b%l) then; At = pos_a
     else;                  At = pos_b
     end if
     r2ab = dot_product(AB,AB)
     self%r2ab = r2ab
     i = 0
     self%ab_kappa_max = ZERO
     do bg = 1,self%b%n_cc
       b       = self%b%ex(bg)
       b_pos_b = b*pos_b
       do ag = 1,self%a%n_cc
         i = i + 1
         a = self%a%ex(ag)
         ab_inv = ONE/self%ab_exponent_sum(i)
         P = (b_pos_b + a*pos_a) * ab_inv
         prefac = shellpr%cc_prefactor(i)*exp(-a*b*r2ab*ab_inv)
         self%ab_cc_prefactor(i)  = prefac
         self%ab_kappa_max = max(self%ab_kappa_max,prefac/(sqrt(ab_inv)*ab_inv))
         self%ab_pair_center(:,i) = P
         self%ab_center_diff(:,i) = P - At
       end do
     end do
    STOP_TIMER("SHELL1QUARTET:set_ab_1")
     UNSTACK
  end subroutine

  subroutine set_cd_1(self,shellpr,pos_c,pos_d)
    SHELL1QUARTET :: self
  ! Copy the c and d parts of the shell4 using from shell1 objects
    SHELLPAIR, IN, target :: shellpr
    REALVEC(:), IN :: pos_c,pos_d
    REALVEC(3) :: CD,Ct,Q,d_pos_d
    REAL :: r2cd,c,d,d_r2cd,cd_inv,prefac
    INT :: i,dg,cg

    STACK("SHELL1QUARTET:set_cd_1")
    START_TIMER("SHELL1QUARTET:set_cd_1")
    self%cd_nullify = TRUE
    self%cd_l_max = shellpr%l_max
    self%pos_c = pos_c
    self%pos_d = pos_d
    self%cd_n_gaussian_pairs = shellpr%n_gaussian_pairs
    self%cd_l_min = shellpr%l_min
    self%cd_l_sum = shellpr%l_sum
    CD = pos_c-pos_d
    r2cd = dot_product(CD,CD)
    self%r2cd = r2cd


    self%c => shellpr%a
    self%d => shellpr%b
    self%cd_exponent_sum            => shellpr%exponent_sum
    if (self%cd_l_max > 1) then
      self%cd_normalising_factors   => shellpr%normalising_factors
      self%cd_hrr_index_larger      => shellpr%hrr_index_larger
      self%cd_hrr_index_smaller     => shellpr%hrr_index_smaller
      self%cd_hrr_components        => shellpr%hrr_components
      self%cd_hrr_comp_to_use       => shellpr%hrr_comp_to_use
    end if
    if (self%cd_l_sum > 1 OR self%ab_l_sum > 1) then
      self%cd_form_3dints_x_indices => shellpr%form_3dints_x_indices
      self%cd_form_3dints_y_indices => shellpr%form_3dints_y_indices
      self%cd_form_3dints_z_indices => shellpr%form_3dints_z_indices
      self%cd_form_3dints_yz_rms_indices => shellpr%form_3dints_yz_rms_indices
    end if

    call create_(self%cd_cc_prefactor,self%cd_n_gaussian_pairs)
    call create_(self%cd_pair_center,3,self%cd_n_gaussian_pairs)
    call create_(self%cd_center_diff,3,self%cd_n_gaussian_pairs)
    ! Want position of shell1 with higher angular momentum.
    if (self%c%l > self%d%l) then; Ct = pos_c
    else;                  Ct = pos_d
    end if
    i = 0
    self%cd_kappa_max = ZERO
    do dg = 1,self%d%n_cc
      d       = self%d%ex(dg)
      d_pos_d = d*pos_d
      d_r2cd  = d*r2cd
      do cg = 1,self%c%n_cc
        i       = i + 1
        c       = self%c%ex(cg)
        cd_inv  = ONE/self%cd_exponent_sum(i)
        Q       = (d_pos_d + c*pos_c) * cd_inv
        prefac  = shellpr%cc_prefactor(i)*exp(-c*d_r2cd*cd_inv)
        self%cd_cc_prefactor(i)  = prefac
        self%cd_pair_center(:,i) = Q
        self%cd_center_diff(:,i) = Q - Ct
      end do
    end do
    STOP_TIMER("SHELL1QUARTET:set_cd_1")
     UNSTACK
  end subroutine

!*******************************************************************************
!  ERI cutoffs
!*******************************************************************************

  PURE function cd_kappa_max(self) result(res)
    SHELL1QUARTET :: self
  ! Return the largest kappa_cd used in the Lindh integrals.
    IN :: self
    REAL :: res
    REALVEC(3) :: CD
    REAL :: d,d_cc,c,cd_inv,prefac,r2_cd
    INT :: dg,cg
    CD = self%pos_d - self%pos_c
    r2_cd = dot_product(CD,CD)
    res = ZERO
    do dg = 1,self%d%n_cc
      d      = self%d%ex(dg)
      d_cc   = self%d%cc(dg)
      do cg = 1,self%c%n_cc
        c = self%c%ex(cg)
        cd_inv = ONE/(c+d)
        prefac = d_cc*self%c%cc(cg) * cd_inv * sqrt(cd_inv) * exp(-c*d*r2_cd*cd_inv)
        res = max(res,prefac/(sqrt(cd_inv)*cd_inv))
      end do
    end do
    STOP_TIMER("SHELL1QUARTET:cd_kappa_max")
  end function

   PURE function skip_ERI(self) result(res)
    SHELL1QUARTET :: self
   ! Whether the ERI block will be less than a cutoff value.
     IN :: self
     BIN :: res
     res = (self%ab_kappa_max*self%cd_kappa_max < SHELL4_ERI_CUTOFF)
     STOP_TIMER("SHELL1QUARTET:skip_ERI")
   end function

   PURE function skip_ERI_1(self,cutoff) result(res)
    SHELL1QUARTET :: self
   ! Whether the ERI block will be less than a cutoff value.
     IN :: self
     REAL, IN :: cutoff
     BIN :: res
     res = (self%ab_kappa_max*self%cd_kappa_max < cutoff)
     STOP_TIMER("SHELL1QUARTET:skip_ERI_1")
   end function

!*******************************************************************************
!                   Roland Lindh-style integrals
!
! Electron repulsion integrals from Lindh, Ryu and Liu,
! J. Chem. Phys 95(8) 1991, 5889-5897.
!
! See also:
! Obara and Saika, J. Chem. Phys. 84(7), 1986, 3963-3974.
! Head-Gordon and Pople, J. Chem. Phys. 89(9), 1988, 5777-5786.
!
!*******************************************************************************

  subroutine get_ERI(self,abcd)
    SHELL1QUARTET :: self
  ! The main routine to produce the electron repulsion integral block (ab|cd).
  ! Use this routine only!
  ! The rest of the routines are now specialised, eg make_pppp will ONLY accept
  ! a pppp shell4.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    INT :: opt,maxl

    STACK("SHELL1QUARTET:get_ERI")
    START_TIMER("SHELL1QUARTET:get_ERI")
    maxl = max(self%ab_l_max,self%cd_l_max)
    select case (maxl)
      case (0)                                              ! max is s function
        call make_ssss_(self,abcd)
      case (1)                                              ! max is p function
        if (self%ab_l_sum==0) opt = 1
        if (self%ab_l_sum==1) opt = 4
        if (self%ab_l_sum==2) opt = 7
        if (self%cd_l_sum==1) opt = opt + 1
        if (self%cd_l_sum==2) opt = opt + 2
        select case (opt)
          case (9); call make_pppp_(self,abcd)
          case (8); call make_ppps_(self,abcd)
          case (7); call make_ppss_(self,abcd)
          case (6); call make_pspp_(self,abcd)
          case (5); call make_psps_(self,abcd)
          case (4); call make_psss_(self,abcd)
          case (3); call make_sspp_(self,abcd)
          case (2); call make_ssps_(self,abcd)
          case (1); call make_ssss_(self,abcd)
        end select
      case (2)                                              ! max is d function
        if (self%ab_l_min==0 AND self%cd_l_min==0) then
          if (self%ab_l_max==2 AND self%cd_l_max==2) then
            call make_dsds_(self,abcd)
          else if (self%ab_l_max==2 AND self%cd_l_max==1) then
            call make_dsps_(self,abcd)
          else if (self%ab_l_max==1 AND self%cd_l_max==2) then
            call make_psds_(self,abcd)
          else
            call make_abcd_(self,abcd)
          end if
        else
          call make_abcd_(self,abcd)
        end if
      case default                                          ! general
        if (self%ab_l_sum == 0) then;      call make_sscd_(self,abcd)
        else if (self%cd_l_sum == 0) then; call make_abss_(self,abcd)
        else;                    call make_abcd_(self,abcd)
        end if
    end select
    STOP_TIMER("SHELL1QUARTET:get_ERI")
     CHECK
  end subroutine

  subroutine make_abcd(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ab|cd) integrals, summed over the primitives
  ! (uses the transfer equation to make (ab|cd) from (es|fs))
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT3(:,:,:), PTR :: escd
    REALMAT(:,:), PTR :: esfs
    INT :: eub,fub
    STACK("SHELL1QUARTET:make_abcd")
    START_TIMER("SHELL1QUARTET:make_abcd")
    nullify(esfs)
    nullify(escd)
    eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
    fub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))
    call create_(esfs,eub,fub)
    if (self%cd_l_sum==1) then
      call make_esps_(self,esfs)
    else if (self%ab_l_sum==1) then
      call make_psfs_(self,esfs)
    else
      call make_esfs_(self,esfs)
    end if
    call create_(escd,eub, self%c%n_comp, self%d%n_comp)
    call transfer_cd_(self,esfs,escd)
    call transfer_ab_(self,escd,abcd)
    call destroy_(escd)
    call destroy_(esfs)
    call to_normalise_(self,abcd)
    STOP_TIMER("SHELL1QUARTET:make_abcd")
     CHECK
  end subroutine

  subroutine make_ascd(self,escd)
    SHELL1QUARTET :: self
  ! Makes the (as|cd) or (sb|cd) integrals, summed over the primitives.
    IN :: self
    REALMAT3(:,:,:), OUT :: escd
    REALMAT(:,:), PTR :: esfs
    INT :: eub,fub
    STACK("SHELL1QUARTET:make_ascd")
    START_TIMER("SHELL1QUARTET:make_ascd")
    nullify(esfs)
    eub = n_comp_(self%ab_l_sum)
    fub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))
    call create_(esfs,eub,fub)
    if (self%cd_l_sum==1) then
      call make_esps_(self,esfs)
    else
      call make_esfs_(self,esfs)
    end if
    call transfer_cd_(self,esfs,escd)
    call destroy_(esfs)
    call to_normalise_ecd_(self,escd)
    STOP_TIMER("SHELL1QUARTET:make_ascd")
     CHECK
  end subroutine

  subroutine make_abcs(self,abfs)
    SHELL1QUARTET :: self
  ! Makes the (ab|cs) or (ab|sd) integrals, summed over the primitives.
    IN :: self
    REALMAT3(:,:,:), OUT :: abfs
    REALMAT(:,:), PTR :: esfs
    INT :: eub,fub
    STACK("SHELL1QUARTET:make_abcs")
    START_TIMER("SHELL1QUARTET:make_abcs")
    nullify(esfs)
    eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
    fub = n_comp_(self%cd_l_sum)
    call create_(esfs,eub,fub)
    if (self%cd_l_sum==1) then
      call make_esps_(self,esfs)
    else
      call make_esfs_(self,esfs)
    end if
    call transfer_ab_(self,esfs,abfs)
    call destroy_(esfs)
    call to_normalise_abf_(self,abfs)
    STOP_TIMER("SHELL1QUARTET:make_abcs")
     CHECK
  end subroutine

  subroutine make_ascs(self,ac)
    SHELL1QUARTET :: self
  ! Makes the (as|cs) or (as|sd) or (sb|cs) or (sb|sd) integrals, summed over
  ! the primitives.
    IN :: self
    REALMAT(:,:), OUT :: ac
    INT :: j
    STACK("SHELL1QUARTET:make_ascs")
    START_TIMER("SHELL1QUARTET:make_ascs")
    if (self%cd_l_sum==1) then
      call make_esps_(self,ac)
    else if (self%ab_l_sum==1) then
      call make_psfs_(self,ac)
    else
      call make_esfs_(self,ac)
    end if
    if (self%ab_l_sum > 1) then
      if (self%cd_l_sum > 1) then
        do j=1,size(ac,2)
          ac(:,j) = ac(:,j) * self%ab_normalising_factors(:) * self%cd_normalising_factors(j)
        end do
      else
        do j=1,size(ac,2)
          ac(:,j) = ac(:,j) * self%ab_normalising_factors(:)
        end do
      end if
    else if (self%cd_l_sum > 1) then
      do j=1,size(ac,2)
        ac(:,j) = ac(:,j) * self%cd_normalising_factors(j)
      end do
    end if
    STOP_TIMER("SHELL1QUARTET:make_ascs")
     CHECK
  end subroutine

  subroutine make_esfs(self,esfs)
    SHELL1QUARTET :: self
  ! Makes the initial (es|fs) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: esfs
    REALMAT3(:,:,:), PTR :: Ixa,Iya,Iza
    RYS, PTR :: rys1
    REALMAT(:,:), PTR :: Ix,Iy,Iz
    REALVEC(:), PTR :: Ix1,Iy1,Iz1,Ixf,Iyf,Izf
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: ce,cf,bb,ce1,wt
    REAL :: Ix12,Iy12,Iz12,Ix21,Iy21,Iz21,Ix22,Iy22,Iz22
    REAL :: Ix1f1,Iy1f1,Iz1f1,Ix1f,Iy1f,Iz1f,Ix1fp1,Iy1fp1,Iz1fp1
    REAL :: Ix2f1,Iy2f1,Iz2f1,Ix2f,Iy2f,Iz2f,Ix2fp1,Iy2fp1,Iz2fp1
    REAL :: Ixe11,Iye11,Ize11,Ixe1,Iye1,Ize1,Ixep11,Iyep11,Izep11
    REAL :: Ixef,Iyef,Izef,Ixef1,Iyef1,Izef1
    REAL :: t2,t2_rz,t2_re,half_zinv,half_einv,f1_bb,f1_cf
    INT :: ag,bg,cg,dg,nroots,eub,fub,dim1,dim2
    INT :: e,f,e1,fp1,ep1,n,i,j,k,n_sum

    STACK("SHELL1QUARTET:make_esfs")
    START_TIMER("SHELL1QUARTET:make_esfs")
    eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
    fub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))
    dim1 = self%ab_l_sum + 1
    dim2 = self%cd_l_sum + 1
    nroots = (dim1+dim2)/2

    ! number of elements to sum over
    n_sum = nroots * self%ab_n_gaussian_pairs * self%cd_n_gaussian_pairs
    call create_(Ixa,n_sum,dim1,dim2)
    call create_(Iya,n_sum,dim1,dim2)
    call create_(Iza,n_sum,dim1,dim2)

    call create_(rys1,nroots)

    i = 0
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            half_einv = HALF * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            xx = (normab * self%cd_cc_prefactor(j) * sqrt(rho))
            rys1%w(:) = rys1%w(:) * (normab * self%cd_cc_prefactor(j) * sqrt(rho))

            ! Now make the 2 dimensional integrals.
            do n=1,nroots
              i = i + 1
              Ix => Ixa(i,:,:)
              Iy => Iya(i,:,:)
              Iz => Iza(i,:,:)
              t2    = rys1%r(n)
              wt    = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              Ix12 = QCx - t2_re * QPx
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix(1,1) = ONE;  Iy(1,1) = ONE;  Iz(1,1) = ONE*wt
              Ix(2,1) = Ix21; Iy(2,1) = Iy21; Iz(2,1) = Iz21*wt
              Ix(1,2) = Ix12; Iy(1,2) = Iy12; Iz(1,2) = Iz12*wt
              Ix(2,2) = Ix22; Iy(2,2) = Iy22; Iz(2,2) = Iz22*wt
              if (self%cd_l_sum>1) then
                cf    = (ONE - t2_re) * half_einv
                Ix1f  = Ix12; Iy1f  = Iy12; Iz1f  = Iz12
                Ix1f1 = ONE;  Iy1f1 = ONE;  Iz1f1 = ONE
                Ix2f  = Ix22; Iy2f  = Iy22; Iz2f  = Iz22
                Ix2f1 = Ix21; Iy2f1 = Iy21; Iz2f1 = Iz21
                f1_cf = ZERO
                do fp1 = 3,self%cd_l_sum+1
                  f1_cf = f1_cf + cf
                  Ix1fp1 = Ix12 * Ix1f + f1_cf * Ix1f1
                  Iy1fp1 = Iy12 * Iy1f + f1_cf * Iy1f1
                  Iz1fp1 = Iz12 * Iz1f + f1_cf * Iz1f1
                  Ix2fp1 = Ix12 * Ix2f + f1_cf * Ix2f1 + bb * Ix1f
                  Iy2fp1 = Iy12 * Iy2f + f1_cf * Iy2f1 + bb * Iy1f
                  Iz2fp1 = Iz12 * Iz2f + f1_cf * Iz2f1 + bb * Iz1f
                  Ix(1,fp1) = Ix1fp1; Iy(1,fp1) = Iy1fp1; Iz(1,fp1) = Iz1fp1*wt
                  Ix(2,fp1) = Ix2fp1; Iy(2,fp1) = Iy2fp1; Iz(2,fp1) = Iz2fp1*wt
                  Ix1f1 = Ix1f;   Iy1f1 = Iy1f;   Iz1f1 = Iz1f
                  Ix1f  = Ix1fp1; Iy1f  = Iy1fp1; Iz1f  = Iz1fp1
                  Ix2f1 = Ix2f;   Iy2f1 = Iy2f;   Iz2f1 = Iz2f
                  Ix2f  = Ix2fp1; Iy2f  = Iy2fp1; Iz2f  = Iz2fp1
                end do
              end if
              if (self%ab_l_sum>1) then
                Ixe1  = Ix21; Iye1  = Iy21; Ize1  = Iz21
                Ixe11 = ONE;   Iye11 = ONE;   Ize11 = ONE
                ce    = (ONE - t2_rz) * half_zinv
                ce1 = ZERO
                do ep1 = 3, self%ab_l_sum+1
                  ce1 = ce1 + ce
                  Ixep11 = Ix21 * Ixe1 + ce1 * Ixe11
                  Iyep11 = Iy21 * Iye1 + ce1 * Iye11
                  Izep11 = Iz21 * Ize1 + ce1 * Ize11
                  Ix(ep1,1) = Ixep11; Iy(ep1,1) = Iyep11; Iz(ep1,1) = Izep11*wt
                  Ixe11 = Ixe1;   Iye11 = Iye1;   Ize11 = Ize1
                  Ixe1  = Ixep11; Iye1  = Iyep11; Ize1  = Izep11
                end do
              end if
              if (self%ab_l_sum>1 AND self%cd_l_sum>1) then
                Ix1 => Ix(:,1)
                Iy1 => Iy(:,1)
                Iz1 => Iz(:,1)
                ce1 = ZERO
                do e = 2, self%ab_l_sum
                  e1  = e - 1
                  ep1 = e + 1
                  ce1 = ce1 + ce
                  Ixef1 = Ix1(e); Iyef1 = Iy1(e); Izef1 = Iz1(e)
                  f1_bb = ZERO
                  do f=2, self%cd_l_sum+1
                    Ixf => Ix(:,f);  Iyf => Iy(:,f);  Izf => Iz(:,f)
                    f1_bb = f1_bb + bb
                    Ixef  = Ixf(e); Iyef  = Iyf(e); Izef  = Izf(e)
                    Ixf(ep1) = Ix21*Ixef + ce1*Ixf(e1) + f1_bb*Ixef1
                    Iyf(ep1) = Iy21*Iyef + ce1*Iyf(e1) + f1_bb*Iyef1
                    Izf(ep1) = Iz21*Izef + ce1*Izf(e1) + f1_bb*Izef1
                    Ixef1 = Ixef;   Iyef1 = Iyef;   Izef1 = Izef
                  end do
                end do
              end if
            end do
          end do
        end do
      end do
    end do

    call destroy_(rys1)

    call form_esfs_(self,Ixa,Iya,Iza,esfs,eub,fub,n_sum)

    call destroy_(Iza)
    call destroy_(Iya)
    call destroy_(Ixa)
    STOP_TIMER("SHELL1QUARTET:make_esfs")
     CHECK
  end subroutine

  subroutine make_esps(self,esps)
    SHELL1QUARTET :: self
  ! Makes the initial (es|ps) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: esps
    REALMAT3(:,:,:), PTR :: Ixa,Iya,Iza
    RYS, PTR :: rys1
    REALMAT(:,:), PTR :: Ix,Iy,Iz
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: ce,bb,ce1,wt
    REAL :: Ix21,Iy21,Iz21
    REAL :: Ixe11,Iye11,Ize11,Ixe1,Iye1,Ize1,Ixep11,Iyep11,Izep11
    REAL :: t2,t2_rz,t2_re,half_zinv
    REAL :: Ixe12,Iye12,Ize12,Ixe2,Iye2,Ize2,Ixep12,Iyep12,Izep12
    INT :: ag,bg,cg,dg,nroots,eub,fub,dim1,dim2
    INT :: ep1,n,i,j,k,n_sum

    STACK("SHELL1QUARTET:make_esps")
    START_TIMER("SHELL1QUARTET:make_esps")
    eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
    fub = 3
    dim1 = self%ab_l_sum + 1
    dim2 = 2
    nroots = (dim1+2)/2

    ! number of elements to sum over
    n_sum = nroots * self%ab_n_gaussian_pairs * self%cd_n_gaussian_pairs
    call create_(Ixa,n_sum,dim1,dim2)
    call create_(Iya,n_sum,dim1,dim2)
    call create_(Iza,n_sum,dim1,dim2)

    call create_(rys1,nroots)

    i = 0
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            ! Now make the 2 dimensional integrals.
            do n=1,nroots
              i = i + 1
              Ix => Ixa(i,:,:)
              Iy => Iya(i,:,:)
              Iz => Iza(i,:,:)
              wt=rys1%w(n)
              t2    = rys1%r(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_re * half_zinv
              ce    = (ONE - t2_rz) * half_zinv
              Ixe12 = QCx - t2_re * QPx
              Iye12 = QCy - t2_re * QPy
              Ize12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ixe2 = Ixe12 * Ix21 + bb
              Iye2 = Iye12 * Iy21 + bb
              Ize2 = Ize12 * Iz21 + bb
              Ix(1,1) = ONE;   Iy(1,1) = ONE;   Iz(1,1) = wt
              Ix(1,2) = Ixe12; Iy(1,2) = Iye12; Iz(1,2) = Ize12*wt
              Ix(2,1) = Ix21;  Iy(2,1) = Iy21;  Iz(2,1) = Iz21*wt
              Ix(2,2) = Ixe2;  Iy(2,2) = Iye2;  Iz(2,2) = Ize2*wt
              if (self%ab_l_sum>1) then
                Ixep11 = Ix21*Ix21 + ce;                  Ix(3,1) = Ixep11
                Iyep11 = Iy21*Iy21 + ce;                  Iy(3,1) = Iyep11
                Izep11 = Iz21*Iz21 + ce;                  Iz(3,1) = Izep11*wt
                Ixep12 = Ix21*Ixe2 + ce*Ixe12 + bb*Ix21;  Ix(3,2) = Ixep12
                Iyep12 = Iy21*Iye2 + ce*Iye12 + bb*Iy21;  Iy(3,2) = Iyep12
                Izep12 = Iz21*Ize2 + ce*Ize12 + bb*Iz21;  Iz(3,2) = Izep12*wt
                if (self%ab_l_sum>2) then
                  Ixe11 = Ix21;   Iye11 = Iy21;   Ize11 = Iz21
                  Ixe12 = Ixe2;   Iye12 = Iye2;   Ize12 = Ize2
                  Ixe1 = Ixep11;  Iye1 = Iyep11;  Ize1 = Izep11
                  Ixe2 = Ixep12;  Iye2 = Iyep12;  Ize2 = Izep12
                  ce1 = ce
                  do ep1 = 4, self%ab_l_sum+1
                    ce1 = ce1 + ce
                    Ixep11 = Ix21*Ixe1 + ce1*Ixe11;            Ix(ep1,1) = Ixep11
                    Iyep11 = Iy21*Iye1 + ce1*Iye11;            Iy(ep1,1) = Iyep11
                    Izep11 = Iz21*Ize1 + ce1*Ize11;            Iz(ep1,1) = Izep11*wt
                    Ixep12 = Ix21*Ixe2 + ce1*Ixe12 + bb*Ixe1;  Ix(ep1,2) = Ixep12
                    Iyep12 = Iy21*Iye2 + ce1*Iye12 + bb*Iye1;  Iy(ep1,2) = Iyep12
                    Izep12 = Iz21*Ize2 + ce1*Ize12 + bb*Ize1;  Iz(ep1,2) = Izep12*wt
                    Ixe11 = Ixe1;   Iye11 = Iye1;   Ize11 = Ize1
                    Ixe12 = Ixe2;   Iye12 = Iye2;   Ize12 = Ize2
                    Ixe1 = Ixep11;  Iye1 = Iyep11;  Ize1 = Izep11
                    Ixe2 = Ixep12;  Iye2 = Iyep12;  Ize2 = Izep12
                  end do
                end if
              end if
            end do
          end do
        end do
      end do
    end do

    call destroy_(rys1)

    if (self%ab_l_min < ERI_rms_min_l) then
      call form_esps_no_rm_(self,Ixa,Iya,Iza,esps,eub)
    else
      call form_esps_rm_(self,Ixa,Iya,Iza,esps,eub,n_sum)
    end if

    call destroy_(Iza)
    call destroy_(Iya)
    call destroy_(Ixa)
    STOP_TIMER("SHELL1QUARTET:make_esps")
     CHECK
  end subroutine

  subroutine make_psfs(self,psfs)
    SHELL1QUARTET :: self
  ! Makes the initial (ps|fs) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: psfs
    REALMAT3(:,:,:), PTR :: Ixa,Iya,Iza
    RYS, PTR :: rys1
    REALMAT(:,:), PTR :: Ix,Iy,Iz
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: cf,bb,wt
    REAL :: Ix12,Iy12,Iz12,Ix21,Iy21,Iz21,Ix22,Iy22,Iz22
    REAL :: Ix1f1,Iy1f1,Iz1f1,Ix1f,Iy1f,Iz1f,Ix1fp1,Iy1fp1,Iz1fp1
    REAL :: Ix2f1,Iy2f1,Iz2f1,Ix2f,Iy2f,Iz2f,Ix2fp1,Iy2fp1,Iz2fp1
    REAL :: t2,t2_rz,t2_re,half_einv,cf1
    INT :: ag,bg,cg,dg,nroots,eub,fub,dim1,dim2
    INT :: fp1,n,i,j,k,n_sum

    STACK("SHELL1QUARTET:make_psfs")
    START_TIMER("SHELL1QUARTET:make_psfs")
    eub = 3
    fub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))
    dim1 = 2
    dim2 = self%cd_l_sum + 1
    nroots = (dim1+dim2)/2

    ! number of elements to sum over
    n_sum = nroots * self%ab_n_gaussian_pairs * self%cd_n_gaussian_pairs
    call create_(Ixa,n_sum,dim1,dim2)
    call create_(Iya,n_sum,dim1,dim2)
    call create_(Iza,n_sum,dim1,dim2)

    call create_(rys1,nroots)

    i = 0
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            half_einv = HALF * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            ! Now make the 2 dimensional integrals.
            do n=1,nroots
              i = i + 1
              Ix => Ixa(i,:,:)
              Iy => Iya(i,:,:)
              Iz => Iza(i,:,:)
              t2    = rys1%r(n)
              wt = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              cf    = (ONE - t2_re) * half_einv
              Ix12 = QCx - t2_re * QPx
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix(1,1) = ONE;  Iy(1,1) = ONE;  Iz(1,1) = wt
              Ix(2,1) = Ix21; Iy(2,1) = Iy21; Iz(2,1) = Iz21 * wt
              Ix(1,2) = Ix12; Iy(1,2) = Iy12; Iz(1,2) = Iz12 * wt
              Ix(2,2) = Ix22; Iy(2,2) = Iy22; Iz(2,2) = Iz22 * wt
              if (self%cd_l_sum>1) then
                Ix1f  = Ix12; Iy1f  = Iy12; Iz1f  = Iz12
                Ix1f1 = ONE;  Iy1f1 = ONE;  Iz1f1 = ONE
                Ix2f  = Ix22; Iy2f  = Iy22; Iz2f  = Iz22
                Ix2f1 = Ix21; Iy2f1 = Iy21; Iz2f1 = Iz21
                cf1 = ZERO
                do fp1 = 3,self%cd_l_sum+1
                  cf1 = cf1 + cf
                  Ix1fp1 = Ix12*Ix1f + cf1*Ix1f1;            Ix(1,fp1) = Ix1fp1
                  Iy1fp1 = Iy12*Iy1f + cf1*Iy1f1;            Iy(1,fp1) = Iy1fp1
                  Iz1fp1 = Iz12*Iz1f + cf1*Iz1f1;            Iz(1,fp1) = Iz1fp1*wt
                  Ix2fp1 = Ix12*Ix2f + cf1*Ix2f1 + bb*Ix1f;  Ix(2,fp1) = Ix2fp1
                  Iy2fp1 = Iy12*Iy2f + cf1*Iy2f1 + bb*Iy1f;  Iy(2,fp1) = Iy2fp1
                  Iz2fp1 = Iz12*Iz2f + cf1*Iz2f1 + bb*Iz1f;  Iz(2,fp1) = Iz2fp1*wt
                  Ix1f1 = Ix1f;   Iy1f1 = Iy1f;   Iz1f1 = Iz1f
                  Ix1f  = Ix1fp1; Iy1f  = Iy1fp1; Iz1f  = Iz1fp1
                  Ix2f1 = Ix2f;   Iy2f1 = Iy2f;   Iz2f1 = Iz2f
                  Ix2f  = Ix2fp1; Iy2f  = Iy2fp1; Iz2f  = Iz2fp1
                end do
              end if
            end do
          end do
        end do
      end do
    end do

    call destroy_(rys1)

    if (self%cd_l_min < ERI_rms_min_l) then
      call form_psfs_no_rm_(self,Ixa,Iya,Iza,psfs,fub)
    else
      call form_psfs_rm_(self,Ixa,Iya,Iza,psfs,fub,n_sum)
    end if

    call destroy_(Iza)
    call destroy_(Iya)
    call destroy_(Ixa)
    STOP_TIMER("SHELL1QUARTET:make_psfs")
     CHECK
  end subroutine

  subroutine form_esfs(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    SHELL1QUARTET :: self
  ! Forms (es|fs) from the two dimensional integrals, summed over primitives.
  ! This is the main routine, all the others are specialised and may break if
  ! given the wrong shell4.
    IN :: self
    REALMAT3(:,:,:), IN :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,fub,n_sum
    STACK("SHELL1QUARTET:form_esfs")
    START_TIMER("SHELL1QUARTET:form_esfs")
    if (self%ab_l_sum==1) then !sp
      if (self%cd_l_min < ERI_rms_min_l) then !s or p or d
        call form_psfs_no_rm_(self,Ix,Iy,Iz,esfs,fub)
      else
        call form_psfs_rm_(self,Ix,Iy,Iz,esfs,fub,n_sum)
      end if
    else if (self%cd_l_sum==1) then !sp
      if (self%ab_l_min < ERI_rms_min_l) then !s or p or d
        call form_esps_no_rm_(self,Ix,Iy,Iz,esfs,eub)
      else
        call form_esps_rm_(self,Ix,Iy,Iz,esfs,eub,n_sum)
      end if
    else if (self%ab_l_min < ERI_rms_min_l AND self%cd_l_min < ERI_rms_min_l) then !s or p or d
      call form_esfs_no_rm_(self,Ix,Iy,Iz,esfs,eub,fub)
    else
      call form_esfs_rm_(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    end if
    STOP_TIMER("SHELL1QUARTET:form_esfs")
     CHECK
  end subroutine

  subroutine form_esfs_rm(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    SHELL1QUARTET :: self
  ! Forms (es|fs) from the two dimensional integrals, summed over primitives.
  ! This version uses the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), IN :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,fub,n_sum
    INTVEC(:), PTR :: e_x,f_x,ii_e_ivec,ii_f_ivec
    REALMAT3(:,:,:), PTR :: Ief
    INT :: e,f,zf,yf,xf,ze,ye,iie,iif
    INT :: dime,dimf,dime1,dimf1

    STACK("SHELL1QUARTET:form_esfs_rm")
    START_TIMER("SHELL1QUARTET:form_esfs_rm")
    dime  = self%ab_l_sum+1
    dime1 = self%ab_l_sum+2
    dimf  = self%cd_l_sum+1
    dimf1 = self%cd_l_sum+2

    e_x => self%ab_form_3dints_x_indices
    f_x => self%cd_form_3dints_x_indices
    ii_e_ivec => self%ab_form_3dints_yz_rms_indices
    ii_f_ivec => self%cd_form_3dints_yz_rms_indices

    call create_(Ief,n_sum,dime*dime1/2,dimf*dimf1/2)

    ! Apply reduced multiplication scheme to Iy and Iz, store in triangle.
    iif = 0
    do zf=1,dimf
      do yf=1,dimf1-zf
        iif = iif + 1
        iie = 0
        do ze=1,dime
          do ye=1,dime1-ze
            iie = iie + 1
            Ief(:,iie,iif) = Iy(:,ye,yf) * Iz(:,ze,zf)
          end do
        end do
      end do
    end do
    ! Now add in the Ix 2d integrals and sum over contractions and roots
    esfs = sum(Ix(:,e_x,f_x) * Ief(:,ii_e_ivec,ii_f_ivec),1)
    call destroy_(Ief)
    STOP_TIMER("SHELL1QUARTET:form_esfs_rm")
     CHECK
  end subroutine

  subroutine form_esfs_no_rm(self,Ix,Iy,Iz,esfs,eub,fub)
    SHELL1QUARTET :: self
  ! Forms (es|fs) from the two dimensional integrals, summed over primitives.
  ! This version does not use the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), IN :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,fub
    INTVEC(:), PTR :: e_x,e_y,e_z,f_x,f_y,f_z

    STACK("SHELL1QUARTET:form_esfs_no_rm")
    START_TIMER("SHELL1QUARTET:form_esfs_no_rm")
    e_x => self%ab_form_3dints_x_indices
    e_y => self%ab_form_3dints_y_indices
    e_z => self%ab_form_3dints_z_indices
    f_x => self%cd_form_3dints_x_indices
    f_y => self%cd_form_3dints_y_indices
    f_z => self%cd_form_3dints_z_indices

    esfs = sum(Ix(:,e_x,f_x) * Iy(:,e_y,f_y) * Iz(:,e_z,f_z),1)
    STOP_TIMER("SHELL1QUARTET:form_esfs_no_rm")
     CHECK
  end subroutine

  subroutine form_esps_no_rm(self,Ix,Iy,Iz,esfs,eub)
    SHELL1QUARTET :: self
  ! Forms (es|ps) from the two dimensional integrals, summed over primitives.
  ! This version does not use the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), target :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub
    INTVEC(:), PTR :: e_x,e_y,e_z
    REALVEC(:), PTR :: Ix1,Iy1,Iz1

    STACK("SHELL1QUARTET:form_esps_no_rm")
    START_TIMER("SHELL1QUARTET:form_esps_no_rm")
    e_x => self%ab_form_3dints_x_indices
    e_y => self%ab_form_3dints_y_indices
    e_z => self%ab_form_3dints_z_indices

    esfs(:,1) = sum(Ix(:,e_x,2) * Iy(:,e_y,1) * Iz(:,e_z,1),1)
    esfs(:,2) = sum(Ix(:,e_x,1) * Iy(:,e_y,2) * Iz(:,e_z,1),1)
    esfs(:,3) = sum(Ix(:,e_x,1) * Iy(:,e_y,1) * Iz(:,e_z,2),1)
    STOP_TIMER("SHELL1QUARTET:form_esps_no_rm")
     CHECK
  end subroutine

  subroutine form_esps_rm(self,Ix,Iy,Iz,esfs,eub,n_sum)
    SHELL1QUARTET :: self
  ! Forms (es|ps) from the two dimensional integrals, summed over primitives.
  ! This version does uses the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), target :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,n_sum
    INTVEC(:), PTR :: e_x,ii_e_ivec
    REALVEC(:), PTR :: Ix1,Iy1,Iz1
    REALMAT3(:,:,:), PTR :: Ief
    INT :: e,ze,ye,xe,iie,dime,dime1

    STACK("SHELL1QUARTET:form_esps_rm")
    START_TIMER("SHELL1QUARTET:form_esps_rm")
    dime  = self%ab_l_sum+1
    dime1 = self%ab_l_sum+2

    e_x => self%ab_form_3dints_x_indices
    ii_e_ivec => self%ab_form_3dints_yz_rms_indices

    call create_(Ief,n_sum,dime*dime1/2,3)
    ! Apply reduced multiplication scheme to Iy and Iz, store in triangle.
    iie = 0
    do ze=1,dime
      do ye=1,dime1-ze
        iie = iie + 1
        Iy1 => Iy(:,ye,1)
        Iz1 => Iz(:,ze,1)
        Ief(:,iie,1) = Iy1 * Iz1
        Ief(:,iie,2) = Iy(:,ye,2) * Iz1
        Ief(:,iie,3) = Iy1 * Iz(:,ze,2)
      end do
    end do
    ! Now add in the Ix 2d integrals and sum over contractions and roots
    do e=1,eub
      iie = ii_e_ivec(e)
      xe = e_x(e)
      Ix1 => Ix(:,xe,1)
      esfs(e,1) = sum(Ix(:,xe,2) * Ief(:,iie,1))
      esfs(e,2) = sum(Ix1 * Ief(:,iie,2))
      esfs(e,3) = sum(Ix1 * Ief(:,iie,3))
    end do
    call destroy_(Ief)
    STOP_TIMER("SHELL1QUARTET:form_esps_rm")
     CHECK
  end subroutine

  subroutine form_psfs_rm(self,Ix,Iy,Iz,esfs,fub,n_sum)
    SHELL1QUARTET :: self
  ! Forms (ps|fs) from the two dimensional integrals, summed over primitives.
  ! This version does uses the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), target :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: fub,n_sum
    INTVEC(:), PTR :: f_x,ii_f_ivec
    REALMAT3(:,:,:), PTR :: Ief
    REALVEC(:), PTR :: Ix1,Iy1,Iz1
    INT :: f,zf,yf,xf,iif,dimf,dimf1

    STACK("SHELL1QUARTET:form_psfs_rm")
    START_TIMER("SHELL1QUARTET:form_psfs_rm")
    dimf  = self%cd_l_sum+1
    dimf1 = self%cd_l_sum+2

    f_x => self%cd_form_3dints_x_indices
    ii_f_ivec => self%cd_form_3dints_yz_rms_indices

    call create_(Ief,n_sum,3,dimf*dimf1/2)
    ! Apply reduced multiplication scheme to Iy and Iz, store in triangle.
    iif = 0
    do zf=1,dimf
      do yf=1,dimf1-zf
        iif = iif + 1
        Iy1 => Iy(:,1,yf)
        Iz1 => Iz(:,1,zf)
        Ief(:,1,iif) = Iy1 * Iz1
        Ief(:,2,iif) = Iy(:,2,yf) * Iz1
        Ief(:,3,iif) = Iy1 * Iz(:,2,zf)
      end do
    end do
    ! Now add in the Ix 2d integrals and sum over contractions and roots
    do f=1,fub
      xf = f_x(f)
      iif = ii_f_ivec(f)
      Ix1 => Ix(:,1,xf)
      esfs(1,f) = sum(Ix(:,2,xf) * Ief(:,1,iif))
      esfs(2,f) = sum(Ix1 * Ief(:,2,iif))
      esfs(3,f) = sum(Ix1 * Ief(:,3,iif))
    end do
    call destroy_(Ief)
    STOP_TIMER("SHELL1QUARTET:form_psfs_rm")
     CHECK
  end subroutine

  subroutine form_psfs_no_rm(self,Ix,Iy,Iz,esfs,fub)
    SHELL1QUARTET :: self
  ! Forms (ps|fs) from the two dimensional integrals, summed over primitives.
  ! This version does not use the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), target :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: fub
    INTVEC(:), PTR :: f_x,f_y,f_z
    REALVEC(:), PTR :: Ix1,Iy1,Iz1
    INT :: f,zf,yf,xf

    STACK("SHELL1QUARTET:form_psfs_no_rm")
    START_TIMER("SHELL1QUARTET:form_psfs_no_rm")
    f_x => self%cd_form_3dints_x_indices
    f_y => self%cd_form_3dints_y_indices
    f_z => self%cd_form_3dints_z_indices

    esfs(1,:) = sum(Ix(:,2,f_x) * Iy(:,1,f_y) * Iz(:,1,f_z),1)
    esfs(2,:) = sum(Ix(:,1,f_x) * Iy(:,2,f_y) * Iz(:,1,f_z),1)
    esfs(3,:) = sum(Ix(:,1,f_x) * Iy(:,1,f_y) * Iz(:,2,f_z),1)
    STOP_TIMER("SHELL1QUARTET:form_psfs_no_rm")
     CHECK
  end subroutine

  subroutine make_dsds(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ds|ds) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(3,3) :: dsds
    STACK("SHELL1QUARTET:make_dsds")
    START_TIMER("SHELL1QUARTET:make_dsds")
    call make_dsds_(self,dsds)
    if (self%a%l == 2) then
      if (self%c%l == 2) then
        abcd(:,1,:,1) = dsds ! dsds
      else
        abcd(:,1,1,:) = dsds ! dssd
      end if
    else
      if (self%c%l == 2) then
        abcd(1,:,:,1) = dsds ! sdds
      else
        abcd(1,:,1,:) = dsds ! sdsd
      end if
    end if
    STOP_TIMER("SHELL1QUARTET:make_dsds")
     CHECK
  end subroutine

  subroutine make_dsds_1(self,dsds)
    SHELL1QUARTET :: self
  ! Makes the initial (es|fs) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: dsds
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: ce,cf,bb,bb2,wt
    REAL :: Ix12,Iy12,Iz12,Ix21,Iy21,Iz21,Ix22,Iy22,Iz22
    REAL :: Ix13,Iy13,Iz13,Ix31,Iy31,Iz31,Ix33,Iy33,Iz33
    REAL :: Ix23,Iy23,Iz23,Ix32,Iy32,Iz32
    REAL :: dsds11,dsds12,dsds13,dsds14,dsds15,dsds16
    REAL :: dsds21,dsds22,dsds23,dsds24,dsds25,dsds26
    REAL :: dsds31,dsds32,dsds33,dsds34,dsds35,dsds36
    REAL :: dsds41,dsds42,dsds43,dsds44,dsds45,dsds46
    REAL :: dsds51,dsds52,dsds53,dsds54,dsds55,dsds56
    REAL :: dsds61,dsds62,dsds63,dsds64,dsds65,dsds66
    REAL :: norm1,norm2,norm3,norm4,norm5,norm6
    REAL :: norm1_norm2,norm1_norm3,norm1_norm4
    REAL :: norm1_norm5,norm1_norm6,norm2_norm3
    REAL :: norm2_norm4,norm2_norm5,norm2_norm6
    REAL :: norm3_norm4,norm3_norm5,norm3_norm6
    REAL :: norm4_norm5,norm4_norm6,norm5_norm6
    REAL :: t2,t2_rz,t2_re,half_zinv,half_einv
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_dsds_1")
    START_TIMER("SHELL1QUARTET:make_dsds_1")
    call create_(rys1,3)

    dsds11 = ZERO; dsds12 = ZERO; dsds13 = ZERO
    dsds14 = ZERO; dsds15 = ZERO; dsds16 = ZERO
    dsds21 = ZERO; dsds22 = ZERO; dsds23 = ZERO
    dsds24 = ZERO; dsds25 = ZERO; dsds26 = ZERO
    dsds31 = ZERO; dsds32 = ZERO; dsds33 = ZERO
    dsds34 = ZERO; dsds35 = ZERO; dsds36 = ZERO
    dsds41 = ZERO; dsds42 = ZERO; dsds43 = ZERO
    dsds44 = ZERO; dsds45 = ZERO; dsds46 = ZERO
    dsds51 = ZERO; dsds52 = ZERO; dsds53 = ZERO
    dsds54 = ZERO; dsds55 = ZERO; dsds56 = ZERO
    dsds61 = ZERO; dsds62 = ZERO; dsds63 = ZERO
    dsds64 = ZERO; dsds65 = ZERO; dsds66 = ZERO

    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            half_einv = HALF * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * (normab * self%cd_cc_prefactor(j) * sqrt(rho))

            ! Now make the 2 dimensional integrals.
            do n=1,3
              t2    = rys1%r(n)
              wt    = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              bb2   = bb+bb
              ce    = (ONE - t2_rz) * half_zinv
              cf    = (ONE - t2_re) * half_einv
              Ix12 = QCx - t2_re * QPx
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix13 = Ix12 * Ix12 + cf
              Iy13 = Iy12 * Iy12 + cf
              Iz13 = Iz12 * Iz12 + cf
              Ix31 = Ix21 * Ix21 + ce
              Iy31 = Iy21 * Iy21 + ce
              Iz31 = Iz21 * Iz21 + ce
              Ix23 = Ix12 * (Ix22 + bb) + cf * Ix21
              Iy23 = Iy12 * (Iy22 + bb) + cf * Iy21
              Iz23 = Iz12 * (Iz22 + bb) + cf * Iz21
              Ix32 = Ix21 * (Ix22 + bb) + ce * Ix12
              Iy32 = Iy21 * (Iy22 + bb) + ce * Iy12
              Iz32 = Iz21 * (Iz22 + bb) + ce * Iz12
              Ix33 = Ix21 * Ix23 + ce * Ix13 + bb2 * Ix22
              Iy33 = Iy21 * Iy23 + ce * Iy13 + bb2 * Iy22
              Iz33 = Iz21 * Iz23 + ce * Iz13 + bb2 * Iz22

              Iz12 = Iz12 * wt
              Iz21 = Iz21 * wt
              Iz22 = Iz22 * wt
              Iz13 = Iz13 * wt
              Iz31 = Iz31 * wt
              Iz23 = Iz23 * wt
              Iz32 = Iz32 * wt
              Iz33 = Iz33 * wt

              dsds33 = dsds33 +               Iz33
              dsds36 = dsds36 +        Iy12 * Iz32
              dsds32 = dsds32 +        Iy13 * Iz31
              dsds63 = dsds63 +        Iy21 * Iz23
              dsds66 = dsds66 +        Iy22 * Iz22
              dsds62 = dsds62 +        Iy23 * Iz21
              dsds23 = dsds23 +        Iy31 * Iz13
              dsds26 = dsds26 +        Iy32 * Iz12
              dsds22 = dsds22 +        Iy33 * wt
              dsds35 = dsds35 + Ix12 *        Iz32
              dsds34 = dsds34 + Ix12 * Iy12 * Iz31
              dsds65 = dsds65 + Ix12 * Iy21 * Iz22
              dsds64 = dsds64 + Ix12 * Iy22 * Iz21
              dsds25 = dsds25 + Ix12 * Iy31 * Iz12
              dsds24 = dsds24 + Ix12 * Iy32 * wt
              dsds31 = dsds31 + Ix13 *        Iz31
              dsds61 = dsds61 + Ix13 * Iy21 * Iz21
              dsds21 = dsds21 + Ix13 * Iy31 * wt
              dsds53 = dsds53 + Ix21 *        Iz23
              dsds56 = dsds56 + Ix21 * Iy12 * Iz22
              dsds52 = dsds52 + Ix21 * Iy13 * Iz21
              dsds43 = dsds43 + Ix21 * Iy21 * Iz13
              dsds46 = dsds46 + Ix21 * Iy22 * Iz12
              dsds42 = dsds42 + Ix21 * Iy23 * wt
              dsds55 = dsds55 + Ix22 *        Iz22
              dsds54 = dsds54 + Ix22 * Iy12 * Iz21
              dsds45 = dsds45 + Ix22 * Iy21 * Iz12
              dsds44 = dsds44 + Ix22 * Iy22 * wt
              dsds51 = dsds51 + Ix23 *        Iz21
              dsds41 = dsds41 + Ix23 * Iy21 * wt
              dsds13 = dsds13 + Ix31 *        Iz13
              dsds16 = dsds16 + Ix31 * Iy12 * Iz12
              dsds12 = dsds12 + Ix31 * Iy13 * wt
              dsds15 = dsds15 + Ix32 *        Iz12
              dsds14 = dsds14 + Ix32 * Iy12 * wt
              dsds11 = dsds11 + Ix33 *        wt
            end do
          end do
        end do
      end do
    end do

    call destroy_(rys1)
    norm1 = self%cd_normalising_factors(1)
    norm2 = self%cd_normalising_factors(2)
    norm3 = self%cd_normalising_factors(3)
    norm4 = self%cd_normalising_factors(4)
    norm5 = self%cd_normalising_factors(5)
    norm6 = self%cd_normalising_factors(6)
    norm1_norm2 = norm1 * norm2
    norm1_norm3 = norm1 * norm3
    norm1_norm4 = norm1 * norm4
    norm1_norm5 = norm1 * norm5
    norm1_norm6 = norm1 * norm6
    norm2_norm3 = norm2 * norm3
    norm2_norm4 = norm2 * norm4
    norm2_norm5 = norm2 * norm5
    norm2_norm6 = norm2 * norm6
    norm3_norm4 = norm3 * norm4
    norm3_norm5 = norm3 * norm5
    norm3_norm6 = norm3 * norm6
    norm4_norm5 = norm4 * norm5
    norm4_norm6 = norm4 * norm6
    norm5_norm6 = norm5 * norm6
    dsds(1,1) = dsds11 * norm1 * norm1
    dsds(1,2) = dsds12 * norm1_norm2
    dsds(1,3) = dsds13 * norm1_norm3
    dsds(1,4) = dsds14 * norm1_norm4
    dsds(1,5) = dsds15 * norm1_norm5
    dsds(1,6) = dsds16 * norm1_norm6
    dsds(2,1) = dsds21 * norm1_norm2
    dsds(2,2) = dsds22 * norm2 * norm2
    dsds(2,3) = dsds23 * norm2_norm3
    dsds(2,4) = dsds24 * norm2_norm4
    dsds(2,5) = dsds25 * norm2_norm5
    dsds(2,6) = dsds26 * norm2_norm6
    dsds(3,1) = dsds31 * norm1_norm3
    dsds(3,2) = dsds32 * norm2_norm3
    dsds(3,3) = dsds33 * norm3 *  norm3
    dsds(3,4) = dsds34 * norm3_norm4
    dsds(3,5) = dsds35 * norm3_norm5
    dsds(3,6) = dsds36 * norm3_norm6
    dsds(4,1) = dsds41 * norm1_norm4
    dsds(4,2) = dsds42 * norm2_norm4
    dsds(4,3) = dsds43 * norm3_norm4
    dsds(4,4) = dsds44 * norm4 * norm4
    dsds(4,5) = dsds45 * norm4_norm5
    dsds(4,6) = dsds46 * norm4_norm6
    dsds(5,1) = dsds51 * norm1_norm5
    dsds(5,2) = dsds52 * norm2_norm5
    dsds(5,3) = dsds53 * norm3_norm5
    dsds(5,4) = dsds54 * norm4_norm5
    dsds(5,5) = dsds55 * norm5 * norm5
    dsds(5,6) = dsds56 * norm5_norm6
    dsds(6,1) = dsds61 * norm1_norm6
    dsds(6,2) = dsds62 * norm2_norm6
    dsds(6,3) = dsds63 * norm3_norm6
    dsds(6,4) = dsds64 * norm4_norm6
    dsds(6,5) = dsds65 * norm5_norm6
    dsds(6,6) = dsds66 * norm6 * norm6
    STOP_TIMER("SHELL1QUARTET:make_dsds_1")
     CHECK
  end subroutine

  subroutine make_psds(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ds|ds) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(3,3) :: psds
    STACK("SHELL1QUARTET:make_psds")
    START_TIMER("SHELL1QUARTET:make_psds")
    call make_psds_(self,psds)
    if (self%a%l == 1) then
      if (self%c%l == 2) then
        abcd(:,1,:,1) = psds ! psds
      else
        abcd(:,1,1,:) = psds ! pssd
      end if
    else
      if (self%c%l == 2) then
        abcd(1,:,:,1) = psds ! spds
      else
        abcd(1,:,1,:) = psds ! spsd
      end if
    end if
    STOP_TIMER("SHELL1QUARTET:make_psds")
     CHECK
  end subroutine

  subroutine make_psds_1(self,psds)
    SHELL1QUARTET :: self
  ! Makes the initial (ps|fs) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: psds
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: cf,bb,wt
    REAL :: Ix12,Iy12,Iz12,Ix21,Iy21,Iz21,Ix22,Iy22,Iz22
    REAL :: Ix13,Iy13,Iz13,Ix23,Iy23,Iz23
    REAL :: psds11,psds12,psds13,psds14,psds15,psds16
    REAL :: psds21,psds22,psds23,psds24,psds25,psds26
    REAL :: psds31,psds32,psds33,psds34,psds35,psds36
    REAL :: t2,t2_rz,t2_re,half_einv
    REAL :: norm1,norm2,norm3,norm4,norm5,norm6
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_psds_1")
    START_TIMER("SHELL1QUARTET:make_psds_1")
    call create_(rys1,2)
    psds11 = ZERO; psds12 = ZERO; psds13 = ZERO
    psds14 = ZERO; psds15 = ZERO; psds16 = ZERO
    psds21 = ZERO; psds22 = ZERO; psds23 = ZERO
    psds24 = ZERO; psds25 = ZERO; psds26 = ZERO
    psds31 = ZERO; psds32 = ZERO; psds33 = ZERO
    psds34 = ZERO; psds35 = ZERO; psds36 = ZERO

    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            half_einv = HALF * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            ! Now make the 2 dimensional integrals.
            do n=1,2
              t2    = rys1%r(n)
              wt = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              cf    = (ONE - t2_re) * half_einv
              Ix12 = QCx - t2_re * QPx
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix13 = Ix12*Ix12 + cf
              Iy13 = Iy12*Iy12 + cf
              Iz13 = (Iz12*Iz12 + cf)*wt
              Iz12 = Iz12 * wt
              Iz21 = Iz21 * wt
              Ix23 = Ix12*(Ix22 + bb) + cf*Ix21
              Iy23 = Iy12*(Iy22 + bb) + cf*Iy21
              Iz23 = Iz12*(Iz22 + bb) + cf*Iz21
              Iz22 = Iz22 * wt

              psds11 = psds11 + Ix23 *        wt
              psds12 = psds12 + Ix21 * Iy13 * wt
              psds13 = psds13 + Ix21 *        Iz13
              psds14 = psds14 + Ix22 * Iy12 * wt
              psds15 = psds15 + Ix22 *        Iz12
              psds16 = psds16 + Ix21 * Iy12 * Iz12
              psds21 = psds21 + Ix13 * Iy21 * wt
              psds22 = psds22 +        Iy23 * wt
              psds23 = psds23 +        Iy21 * Iz13
              psds24 = psds24 + Ix12 * Iy22 * wt
              psds25 = psds25 + Ix12 * Iy21 * Iz12
              psds26 = psds26 +        Iy22 * Iz12
              psds31 = psds31 + Ix13 *        Iz21
              psds32 = psds32 +        Iy13 * Iz21
              psds33 = psds33 +               Iz23
              psds34 = psds34 + Ix12 * Iy12 * Iz21
              psds35 = psds35 + Ix12 *        Iz22
              psds36 = psds36 +        Iy12 * Iz22
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)
    norm1 = self%cd_normalising_factors(1)
    norm2 = self%cd_normalising_factors(2)
    norm3 = self%cd_normalising_factors(3)
    norm4 = self%cd_normalising_factors(4)
    norm5 = self%cd_normalising_factors(5)
    norm6 = self%cd_normalising_factors(6)
    psds(1,1) = psds11 * norm1
    psds(1,2) = psds12 * norm2
    psds(1,3) = psds13 * norm3
    psds(1,4) = psds14 * norm4
    psds(1,5) = psds15 * norm5
    psds(1,6) = psds16 * norm6
    psds(2,1) = psds21 * norm1
    psds(2,2) = psds22 * norm2
    psds(2,3) = psds23 * norm3
    psds(2,4) = psds24 * norm4
    psds(2,5) = psds25 * norm5
    psds(2,6) = psds26 * norm6
    psds(3,1) = psds31 * norm1
    psds(3,2) = psds32 * norm2
    psds(3,3) = psds33 * norm3
    psds(3,4) = psds34 * norm4
    psds(3,5) = psds35 * norm5
    psds(3,6) = psds36 * norm6
    STOP_TIMER("SHELL1QUARTET:make_psds_1")
     CHECK
  end subroutine

  subroutine make_dsps(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ds|ps) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(3,3) :: dsps
    STACK("SHELL1QUARTET:make_dsps")
    START_TIMER("SHELL1QUARTET:make_dsps")
    call make_dsps_(self,dsps)
    if (self%a%l == 2) then
      if (self%c%l == 1) then
        abcd(:,1,:,1) = dsps ! dsps
      else
        abcd(:,1,1,:) = dsps ! dssp
      end if
    else
      if (self%c%l == 1) then
        abcd(1,:,:,1) = dsps ! sdps
      else
        abcd(1,:,1,:) = dsps ! sdsp
      end if
    end if
    STOP_TIMER("SHELL1QUARTET:make_dsps")
     CHECK
  end subroutine

  subroutine make_dsps_1(self,dsps)
    SHELL1QUARTET :: self
  ! Makes the initial (es|ps) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: dsps
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: ce,bb,wt,t2,t2_rz,t2_re,half_zinv
    REAL :: Ix12,Iy12,Iz12,Ix21,Iy21,Iz21,Ix22,Iy22,Iz22
    REAL :: Ix31,Iy31,Iz31,Ix32,Iy32,Iz32
    REAL :: dsps11,dsps21,dsps31,dsps41,dsps51,dsps61
    REAL :: dsps12,dsps22,dsps32,dsps42,dsps52,dsps62
    REAL :: dsps13,dsps23,dsps33,dsps43,dsps53,dsps63
    REAL :: norm1,norm2,norm3,norm4,norm5,norm6
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_dsps_1")
    START_TIMER("SHELL1QUARTET:make_dsps_1")
    call create_(rys1,2)

    dsps11 = ZERO; dsps21 = ZERO; dsps31 = ZERO
    dsps41 = ZERO; dsps51 = ZERO; dsps61 = ZERO
    dsps12 = ZERO; dsps22 = ZERO; dsps32 = ZERO
    dsps42 = ZERO; dsps52 = ZERO; dsps62 = ZERO
    dsps13 = ZERO; dsps23 = ZERO; dsps33 = ZERO
    dsps43 = ZERO; dsps53 = ZERO; dsps63 = ZERO

    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            ! Now make the 2 dimensional integrals.
            do n=1,2
              wt=rys1%w(n)
              t2    = rys1%r(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_re * half_zinv
              ce    = (ONE - t2_rz) * half_zinv
              Ix12 = QCx - t2_re * QPx
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix31 = Ix21 * Ix21 + ce
              Iy31 = Iy21 * Iy21 + ce
              Iz31 = (Iz21 * Iz21 + ce)*wt
              Iz21 = Iz21 * wt
              Iz12 = Iz12 * wt
              Ix32 = Ix21 * (Ix22 + bb) + ce * Ix12
              Iy32 = Iy21 * (Iy22 + bb) + ce * Iy12
              Iz32 = Iz21 * (Iz22 + bb) + ce * Iz12 ! this includes wt
              Iz22 = Iz22 * wt

              dsps11 = dsps11 + Ix32 *        wt
              dsps21 = dsps21 + Ix12 * Iy31 * wt
              dsps31 = dsps31 + Ix12 *        Iz31
              dsps41 = dsps41 + Ix22 * Iy21 * wt
              dsps51 = dsps51 + Ix22 *        Iz21
              dsps61 = dsps61 + Ix12 * Iy21 * Iz21
              dsps12 = dsps12 + Ix31 * Iy12 * wt
              dsps22 = dsps22 +        Iy32 * wt
              dsps32 = dsps32 +        Iy12 * Iz31
              dsps42 = dsps42 + Ix21 * Iy22 * wt
              dsps52 = dsps52 + Ix21 * Iy12 * Iz21
              dsps62 = dsps62 +        Iy22 * Iz21
              dsps13 = dsps13 + Ix31 *        Iz12
              dsps23 = dsps23 +        Iy31 * Iz12
              dsps33 = dsps33 +               Iz32
              dsps43 = dsps43 + Ix21 * Iy21 * Iz12
              dsps53 = dsps53 + Ix21 *        Iz22
              dsps63 = dsps63 +        Iy21 * Iz22
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)

    norm1 = self%ab_normalising_factors(1)
    norm2 = self%ab_normalising_factors(2)
    norm3 = self%ab_normalising_factors(3)
    norm4 = self%ab_normalising_factors(4)
    norm5 = self%ab_normalising_factors(5)
    norm6 = self%ab_normalising_factors(6)
    dsps(1,1) = dsps11 * norm1
    dsps(2,1) = dsps21 * norm2
    dsps(3,1) = dsps31 * norm3
    dsps(4,1) = dsps41 * norm4
    dsps(5,1) = dsps51 * norm5
    dsps(6,1) = dsps61 * norm6
    dsps(1,2) = dsps12 * norm1
    dsps(2,2) = dsps22 * norm2
    dsps(3,2) = dsps32 * norm3
    dsps(4,2) = dsps42 * norm4
    dsps(5,2) = dsps52 * norm5
    dsps(6,2) = dsps62 * norm6
    dsps(1,3) = dsps13 * norm1
    dsps(2,3) = dsps23 * norm2
    dsps(3,3) = dsps33 * norm3
    dsps(4,3) = dsps43 * norm4
    dsps(5,3) = dsps53 * norm5
    dsps(6,3) = dsps63 * norm6
    STOP_TIMER("SHELL1QUARTET:make_dsps_1")
     CHECK
  end subroutine

  subroutine make_dsss(self,dsss)
    SHELL1QUARTET :: self
  ! Makes the initial (es|ps) integrals, summed over the primitives
    IN :: self
    REALVEC(:), OUT :: dsss
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,rho_zinv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,Px,Py,Pz
    REAL :: ce,wt,t2,t2_rz,half_zinv
    REAL :: Ix2,Iy2,Iz2
    REAL :: dsss1,dsss2,dsss3,dsss4,dsss5,dsss6
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_dsss")
    START_TIMER("SHELL1QUARTET:make_dsss")
    call create_(rys1,2)

    dsss1 = ZERO; dsss2 = ZERO; dsss3 = ZERO
    dsss4 = ZERO; dsss5 = ZERO; dsss6 = ZERO

    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)
            do n=1,2
              wt=rys1%w(n)
              t2    = rys1%r(n)
              t2_rz = t2 * rho_zinv
              ce    = (ONE - t2_rz) * half_zinv
              Ix2   = PAx + t2_rz * QPx
              Iy2   = PAy + t2_rz * QPy
              Iz2   = PAz + t2_rz * QPz
              dsss1 = dsss1 + (Ix2 * Ix2 + ce)*wt
              dsss2 = dsss2 + (Iy2 * Iy2 + ce)*wt
              dsss3 = dsss3 + (Iz2 * Iz2 + ce)*wt
              Iz2   = Iz2 * wt
              dsss4 = dsss4 + Ix2 * Iy2 * wt
              dsss5 = dsss5 + Ix2 * Iz2
              dsss6 = dsss6 + Iy2 * Iz2
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)
    dsss(1) = dsss1 * self%ab_normalising_factors(1)
    dsss(2) = dsss2 * self%ab_normalising_factors(2)
    dsss(3) = dsss3 * self%ab_normalising_factors(3)
    dsss(4) = dsss4 * self%ab_normalising_factors(4)
    dsss(5) = dsss5 * self%ab_normalising_factors(5)
    dsss(6) = dsss6 * self%ab_normalising_factors(6)
    STOP_TIMER("SHELL1QUARTET:make_dsss")
     CHECK
  end subroutine

  subroutine make_ssds(self,ssds)
    SHELL1QUARTET :: self
  ! Makes the initial (ps|fs) integrals, summed over the primitives
    IN :: self
    REALVEC(:), OUT :: ssds
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_einv
    REAL :: QPx,QPy,QPz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: cf,wt
    REAL :: Ix2,Iy2,Iz2
    REAL :: ssds1,ssds2,ssds3,ssds4,ssds5,ssds6
    REAL :: t2,t2_re,half_einv
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_ssds")
    START_TIMER("SHELL1QUARTET:make_ssds")
    call create_(rys1,2)
    ssds1 = ZERO; ssds2 = ZERO; ssds3 = ZERO
    ssds4 = ZERO; ssds5 = ZERO; ssds6 = ZERO

    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_einv = rho * einv
            half_einv = HALF * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            ! Now make the 2 dimensional integrals.
            do n=1,2
              t2    = rys1%r(n)
              wt = rys1%w(n)
              t2_re = t2 * rho_einv
              cf    = (ONE - t2_re) * half_einv
              Ix2   = QCx - t2_re * QPx
              Iy2   = QCy - t2_re * QPy
              Iz2   = QCz - t2_re * QPz
              ssds1 = ssds1 + (Ix2*Ix2 + cf)*wt
              ssds2 = ssds2 + (Iy2*Iy2 + cf)*wt
              ssds3 = ssds3 + (Iz2*Iz2 + cf)*wt
              Iz2   = Iz2 * wt
              ssds4 = ssds4 + Ix2 * Iy2 * wt
              ssds5 = ssds5 + Ix2 * Iz2
              ssds6 = ssds6 + Iy2 * Iz2
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)
    ssds(1) = ssds1 * self%cd_normalising_factors(1)
    ssds(2) = ssds2 * self%cd_normalising_factors(2)
    ssds(3) = ssds3 * self%cd_normalising_factors(3)
    ssds(4) = ssds4 * self%cd_normalising_factors(4)
    ssds(5) = ssds5 * self%cd_normalising_factors(5)
    ssds(6) = ssds6 * self%cd_normalising_factors(6)
    STOP_TIMER("SHELL1QUARTET:make_ssds")
     CHECK
  end subroutine

  subroutine make_pppp(self,abcd)
    SHELL1QUARTET :: self
  ! Make the (pp|pp) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), target :: abcd
    REALMAT(:,:), PTR :: Iab
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz
    REAL :: BAx,BAy,BAz,DCx,DCy,DCz
    REAL :: ce,cf,bb,bb_2,t2,t2_rz,t2_re,w,half_zinv,half_einv
    REAL :: Ix12,Ix13,Ix21,Ix22,Ix23,Ix31,Ix32,Ix33
    REAL :: Iy12,Iy13,Iy21,Iy22,Iy23,Iy31,Iy32,Iy33
    REAL :: Iz12,Iz21,Iz22,Iz23
    REAL :: Iz13_w,Iz31_w,Iz21_w,Iz22_w,Iz12_w,Iz32_w,Iz23_w,Iz33_w
    REAL :: px__px_,py__px_,pz__px_
    REAL :: px__py_,py__py_,pz__py_
    REAL :: px__pz_,py__pz_,pz__pz_
    REAL :: px__dxx,py__dxx,pz__dxx
    REAL :: px__dyy,py__dyy,pz__dyy
    REAL :: px__dzz,py__dzz,pz__dzz
    REAL :: px__dxy,py__dxy,pz__dxy
    REAL :: px__dxz,py__dxz,pz__dxz
    REAL :: px__dyz,py__dyz,pz__dyz
    REAL :: dxx_px_,dyy_px_,dzz_px_,dxy_px_,dxz_px_,dyz_px_
    REAL :: dxx_py_,dyy_py_,dzz_py_,dxy_py_,dxz_py_,dyz_py_
    REAL :: dxx_pz_,dyy_pz_,dzz_pz_,dxy_pz_,dxz_pz_,dyz_pz_
    REAL :: dxx_dxx,dyy_dxx,dzz_dxx,dxy_dxx,dxz_dxx,dyz_dxx
    REAL :: dxx_dyy,dyy_dyy,dzz_dyy,dxy_dyy,dxz_dyy,dyz_dyy
    REAL :: dxx_dzz,dyy_dzz,dzz_dzz,dxy_dzz,dxz_dzz,dyz_dzz
    REAL :: dxx_dxy,dyy_dxy,dzz_dxy,dxy_dxy,dxz_dxy,dyz_dxy
    REAL :: dxx_dxz,dyy_dxz,dzz_dxz,dxy_dxz,dxz_dxz,dyz_dxz
    REAL :: dxx_dyz,dyy_dyz,dzz_dyz,dxy_dyz,dxz_dyz,dyz_dyz
    REAL :: Iy12_Iz21,Iy12_Iz12,Iy13_Iz21,Iy21_Iz21,Iy21_Iz22
    REAL :: Iy21_Iz12,Iy31_Iz12,Iy21_Iz13,Iy12_Iz31,Iy12_Iz22
    REAL :: Iy22_Iz12,Iy22_Iz21
    REAL :: Ix22_bb,Iy22_bb,Iz22_bb
    REAL :: Iy12_w, Iy21_w, Iy22_w, Iy13_w, Iy31_w, Iy32_w, Iy23_w
    REAL :: px_,py_,pz_,dxx_,dyy_,dzz_,dxy_,dxz_,dyz_
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_pppp")
    START_TIMER("SHELL1QUARTET:make_pppp")
    px__px_ = ZERO; py__px_ = ZERO; pz__px_ = ZERO
    dxx_px_ = ZERO; dyy_px_ = ZERO; dzz_px_ = ZERO
    dxy_px_ = ZERO; dxz_px_ = ZERO; dyz_px_ = ZERO
    px__py_ = ZERO; py__py_ = ZERO; pz__py_ = ZERO
    dxx_py_ = ZERO; dyy_py_ = ZERO; dzz_py_ = ZERO
    dxy_py_ = ZERO; dxz_py_ = ZERO; dyz_py_ = ZERO
    px__pz_ = ZERO; py__pz_ = ZERO; pz__pz_ = ZERO
    dxx_pz_ = ZERO; dyy_pz_ = ZERO; dzz_pz_ = ZERO
    dxy_pz_ = ZERO; dxz_pz_ = ZERO; dyz_pz_ = ZERO
    px__dxx = ZERO; py__dxx = ZERO; pz__dxx = ZERO
    dxx_dxx = ZERO; dyy_dxx = ZERO; dzz_dxx = ZERO
    dxy_dxx = ZERO; dxz_dxx = ZERO; dyz_dxx = ZERO
    px__dyy = ZERO; py__dyy = ZERO; pz__dyy = ZERO
    dxx_dyy = ZERO; dyy_dyy = ZERO; dzz_dyy = ZERO
    dxy_dyy = ZERO; dxz_dyy = ZERO; dyz_dyy = ZERO
    px__dzz = ZERO; py__dzz = ZERO; pz__dzz = ZERO
    dxx_dzz = ZERO; dyy_dzz = ZERO; dzz_dzz = ZERO
    dxy_dzz = ZERO; dxz_dzz = ZERO; dyz_dzz = ZERO
    px__dxy = ZERO; py__dxy = ZERO; pz__dxy = ZERO
    dxx_dxy = ZERO; dyy_dxy = ZERO; dzz_dxy = ZERO
    dxy_dxy = ZERO; dxz_dxy = ZERO; dyz_dxy = ZERO
    px__dxz = ZERO; py__dxz = ZERO; pz__dxz = ZERO
    dxx_dxz = ZERO; dyy_dxz = ZERO; dzz_dxz = ZERO
    dxy_dxz = ZERO; dxz_dxz = ZERO; dyz_dxz = ZERO
    px__dyz = ZERO; py__dyz = ZERO; pz__dyz = ZERO
    dxx_dyz = ZERO; dyy_dyz = ZERO; dzz_dyz = ZERO
    dxy_dyy = ZERO; dxz_dyy = ZERO; dyz_dyy = ZERO
    dxy_dyz = ZERO; dxz_dyz = ZERO; dyz_dyz = ZERO

    call create_(rys1,3)
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w = rys1%w * normab * self%cd_cc_prefactor(j) * sqrt(rho)
            half_einv = HALF * einv
            do n=1,3
              t2    = rys1%r(n)
              w     = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              bb_2  = bb+bb
              cf    = (ONE - t2_re) * half_einv
              ce    = (ONE - t2_rz) * half_zinv
              Ix12 = QCx - t2_re * QPx           ! form 2 dimensional integrals.
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix13 = Ix12 * Ix12 + cf
              Iy13 = Iy12 * Iy12 + cf
              Iz13_w = (Iz12 * Iz12 + cf) * w
              Ix31 = Ix21 * Ix21 + ce
              Iy31 = Iy21 * Iy21 + ce
              Iz31_w = (Iz21 * Iz21 + ce) * w
              Ix22_bb = Ix22 + bb
              Iy22_bb = Iy22 + bb
              Iz22_bb = Iz22 + bb
              Ix23 = Ix12 * Ix22_bb + cf * Ix21
              Iy23 = Iy12 * Iy22_bb + cf * Iy21
              Iz23 = Iz12 * Iz22_bb + cf * Iz21
              Iz21_w = Iz21 * w    ! Merge the weights and reduce multiplications.
              Iz22_w = Iz22 * w
              Iz12_w = Iz12 * w
              Iz23_w = Iz23 * w
              Iy12_w = Iy12 * w
              Iy21_w = Iy21 * w
              Iy22_w = Iy22 * w
              Iy13_w = Iy13 * w
              Iy31_w = Iy31 * w
              Iy23_w = Iy23 * w
              Ix32 = Ix21 * Ix22_bb + ce * Ix12
              Iy32 = Iy21 * Iy22_bb + ce * Iy12
              Iz32_w = Iz21_w * Iz22_bb + ce * Iz12_w
              Ix33 = Ix21 * Ix23 + ce * Ix13 + bb_2 * Ix22
              Iy33 = Iy21 * Iy23 + ce * Iy13 + bb_2 * Iy22
              Iz33_w = Iz21_w * Iz23 + ce * Iz13_w + bb_2 * Iz22_w
              Iy32_w = Iy32 * w

              Iy12_Iz21 = Iy12 * Iz21_w
              Iy12_Iz12 = Iy12 * Iz12_w
              Iy12_Iz22 = Iy12 * Iz22_w
              Iy12_Iz31 = Iy12 * Iz31_w
              Iy13_Iz21 = Iy13 * Iz21_w
              Iy21_Iz21 = Iy21 * Iz21_w
              Iy21_Iz22 = Iy21 * Iz22_w
              Iy21_Iz12 = Iy21 * Iz12_w
              Iy21_Iz13 = Iy21 * Iz13_w
              Iy31_Iz12 = Iy31 * Iz12_w
              Iy22_Iz12 = Iy22 * Iz12_w
              Iy22_Iz21 = Iy22 * Iz21_w

              px__px_ = px__px_ + Ix22 * w     !  combine 2d ints.
              py__px_ = py__px_ + Ix12 * Iy21_w
              pz__px_ = pz__px_ + Ix12 * Iz21_w
              dxx_px_ = dxx_px_ + Ix32 * w
              dyy_px_ = dyy_px_ + Ix12 * Iy31_w
              dzz_px_ = dzz_px_ + Ix12 * Iz31_w
              dxy_px_ = dxy_px_ + Ix22 * Iy21_w
              dxz_px_ = dxz_px_ + Ix22 * Iz21_w
              dyz_px_ = dyz_px_ + Ix12 * Iy21_Iz21
              px__py_ = px__py_ + Ix21 * Iy12_w
              py__py_ = py__py_ + Iy22_w
              pz__py_ = pz__py_ + Iy12_Iz21
              dxx_py_ = dxx_py_ + Ix31 * Iy12_w
              dyy_py_ = dyy_py_ + Iy32_w
              dzz_py_ = dzz_py_ + Iy12_Iz31
              dxy_py_ = dxy_py_ + Ix21 * Iy22_w
              dxz_py_ = dxz_py_ + Ix21 * Iy12_Iz21
              dyz_py_ = dyz_py_ + Iy22_Iz21
              px__pz_ = px__pz_ + Ix21 * Iz12_w
              py__pz_ = py__pz_ + Iy21_Iz12
              pz__pz_ = pz__pz_ + Iz22_w
              dxx_pz_ = dxx_pz_ + Ix31 * Iz12_w
              dyy_pz_ = dyy_pz_ + Iy31_Iz12
              dzz_pz_ = dzz_pz_ + Iz32_w
              dxy_pz_ = dxy_pz_ + Ix21 * Iy21_Iz12
              dxz_pz_ = dxz_pz_ + Ix21 * Iz22_w
              dyz_pz_ = dyz_pz_ + Iy21_Iz22
              px__dxx = px__dxx + Ix23 * w
              py__dxx = py__dxx + Ix13 * Iy21_w
              pz__dxx = pz__dxx + Ix13 * Iz21_w
              dxx_dxx = dxx_dxx + Ix33 * w
              dyy_dxx = dyy_dxx + Ix13 * Iy31_w
              dzz_dxx = dzz_dxx + Ix13 * Iz31_w
              dxy_dxx = dxy_dxx + Ix23 * Iy21_w
              dxz_dxx = dxz_dxx + Ix23 * Iz21_w
              dyz_dxx = dyz_dxx + Ix13 * Iy21_Iz21
              px__dyy = px__dyy + Ix21 * Iy13_w
              py__dyy = py__dyy + Iy23_w
              pz__dyy = pz__dyy + Iy13_Iz21
              dxx_dyy = dxx_dyy + Ix31 * Iy13_w
              dyy_dyy = dyy_dyy + Iy33 * w
              dzz_dyy = dzz_dyy + Iy13 * Iz31_w
              dxy_dyy = dxy_dyy + Ix21 * Iy23_w
              dxz_dyy = dxz_dyy + Ix21 * Iy13_Iz21
              dyz_dyy = dyz_dyy + Iy23 * Iz21_w
              px__dzz = px__dzz + Ix21 * Iz13_w
              py__dzz = py__dzz + Iy21_Iz13
              pz__dzz = pz__dzz + Iz23_w
              dxx_dzz = dxx_dzz + Ix31 * Iz13_w
              dyy_dzz = dyy_dzz + Iy31 * Iz13_w
              dzz_dzz = dzz_dzz + Iz33_w
              dxy_dzz = dxy_dzz + Ix21 * Iy21_Iz13
              dxz_dzz = dxz_dzz + Ix21 * Iz23_w
              dyz_dzz = dyz_dzz + Iy21 * Iz23_w
              px__dxy = px__dxy + Ix22 * Iy12_w
              py__dxy = py__dxy + Ix12 * Iy22_w
              pz__dxy = pz__dxy + Ix12 * Iy12_Iz21
              dxx_dxy = dxx_dxy + Ix32 * Iy12_w
              dyy_dxy = dyy_dxy + Ix12 * Iy32_w
              dzz_dxy = dzz_dxy + Ix12 * Iy12_Iz31
              dxy_dxy = dxy_dxy + Ix22 * Iy22_w
              dxz_dxy = dxz_dxy + Ix22 * Iy12_Iz21
              dyz_dxy = dyz_dxy + Ix12 * Iy22_Iz21
              px__dxz = px__dxz + Ix22 * Iz12_w
              py__dxz = py__dxz + Ix12 * Iy21_Iz12
              pz__dxz = pz__dxz + Ix12 * Iz22_w
              dxx_dxz = dxx_dxz + Ix32 * Iz12_w
              dyy_dxz = dyy_dxz + Ix12 * Iy31_Iz12
              dzz_dxz = dzz_dxz + Ix12 * Iz32_w
              dxy_dxz = dxy_dxz + Ix22 * Iy21_Iz12
              dxz_dxz = dxz_dxz + Ix22 * Iz22_w
              dyz_dxz = dyz_dxz + Ix12 * Iy21_Iz22
              px__dyz = px__dyz + Ix21 * Iy12_Iz12
              py__dyz = py__dyz + Iy22_Iz12
              pz__dyz = pz__dyz + Iy12_Iz22
              dxx_dyz = dxx_dyz + Ix31 * Iy12_Iz12
              dyy_dyz = dyy_dyz + Iy32 * Iz12_w
              dzz_dyz = dzz_dyz + Iy12 * Iz32_w
              dxy_dyz = dxy_dyz + Ix21 * Iy22_Iz12
              dxz_dyz = dxz_dyz + Ix21 * Iy12_Iz22
              dyz_dyz = dyz_dyz + Iy22 * Iz22_w
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)

    BAx = self%pos_b(1) - self%pos_a(1)
    BAy = self%pos_b(2) - self%pos_a(2)
    BAz = self%pos_b(3) - self%pos_a(3)
    DCx = self%pos_d(1) - self%pos_c(1)
    DCy = self%pos_d(2) - self%pos_c(2)
    DCz = self%pos_d(3) - self%pos_c(3)

    px_  = px__dxx + DCx * px__px_
    py_  = py__dxx + DCx * py__px_
    pz_  = pz__dxx + DCx * pz__px_
    dxx_ = dxx_dxx + DCx * dxx_px_
    dyy_ = dyy_dxx + DCx * dyy_px_
    dzz_ = dzz_dxx + DCx * dzz_px_
    dxy_ = dxy_dxx + DCx * dxy_px_
    dxz_ = dxz_dxx + DCx * dxz_px_
    dyz_ = dyz_dxx + DCx * dyz_px_
    Iab => abcd(:,:,1,1)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dxy + DCy * px__px_
    py_  = py__dxy + DCy * py__px_
    pz_  = pz__dxy + DCy * pz__px_
    dxx_ = dxx_dxy + DCy * dxx_px_
    dyy_ = dyy_dxy + DCy * dyy_px_
    dzz_ = dzz_dxy + DCy * dzz_px_
    dxy_ = dxy_dxy + DCy * dxy_px_
    dxz_ = dxz_dxy + DCy * dxz_px_
    dyz_ = dyz_dxy + DCy * dyz_px_
    Iab => abcd(:,:,2,1)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dxz + DCz * px__px_
    py_  = py__dxz + DCz * py__px_
    pz_  = pz__dxz + DCz * pz__px_
    dxx_ = dxx_dxz + DCz * dxx_px_
    dyy_ = dyy_dxz + DCz * dyy_px_
    dzz_ = dzz_dxz + DCz * dzz_px_
    dxy_ = dxy_dxz + DCz * dxy_px_
    dxz_ = dxz_dxz + DCz * dxz_px_
    dyz_ = dyz_dxz + DCz * dyz_px_
    Iab => abcd(:,:,3,1)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dxy + DCx * px__py_
    py_  = py__dxy + DCx * py__py_
    pz_  = pz__dxy + DCx * pz__py_
    dxx_ = dxx_dxy + DCx * dxx_py_
    dyy_ = dyy_dxy + DCx * dyy_py_
    dzz_ = dzz_dxy + DCx * dzz_py_
    dxy_ = dxy_dxy + DCx * dxy_py_
    dxz_ = dxz_dxy + DCx * dxz_py_
    dyz_ = dyz_dxy + DCx * dyz_py_
    Iab => abcd(:,:,1,2)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dyy + DCy * px__py_
    py_  = py__dyy + DCy * py__py_
    pz_  = pz__dyy + DCy * pz__py_
    dxx_ = dxx_dyy + DCy * dxx_py_
    dyy_ = dyy_dyy + DCy * dyy_py_
    dzz_ = dzz_dyy + DCy * dzz_py_
    dxy_ = dxy_dyy + DCy * dxy_py_
    dxz_ = dxz_dyy + DCy * dxz_py_
    dyz_ = dyz_dyy + DCy * dyz_py_
    Iab => abcd(:,:,2,2)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dyz + DCz * px__py_
    py_  = py__dyz + DCz * py__py_
    pz_  = pz__dyz + DCz * pz__py_
    dxx_ = dxx_dyz + DCz * dxx_py_
    dyy_ = dyy_dyz + DCz * dyy_py_
    dzz_ = dzz_dyz + DCz * dzz_py_
    dxy_ = dxy_dyz + DCz * dxy_py_
    dxz_ = dxz_dyz + DCz * dxz_py_
    dyz_ = dyz_dyz + DCz * dyz_py_
    Iab => abcd(:,:,3,2)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dxz + DCx * px__pz_
    py_  = py__dxz + DCx * py__pz_
    pz_  = pz__dxz + DCx * pz__pz_
    dxx_ = dxx_dxz + DCx * dxx_pz_
    dyy_ = dyy_dxz + DCx * dyy_pz_
    dzz_ = dzz_dxz + DCx * dzz_pz_
    dxy_ = dxy_dxz + DCx * dxy_pz_
    dxz_ = dxz_dxz + DCx * dxz_pz_
    dyz_ = dyz_dxz + DCx * dyz_pz_
    Iab => abcd(:,:,1,3)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dyz + DCy * px__pz_
    py_  = py__dyz + DCy * py__pz_
    pz_  = pz__dyz + DCy * pz__pz_
    dxx_ = dxx_dyz + DCy * dxx_pz_
    dyy_ = dyy_dyz + DCy * dyy_pz_
    dzz_ = dzz_dyz + DCy * dzz_pz_
    dxy_ = dxy_dyz + DCy * dxy_pz_
    dxz_ = dxz_dyz + DCy * dxz_pz_
    dyz_ = dyz_dyz + DCy * dyz_pz_
    Iab => abcd(:,:,2,3)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_

    px_  = px__dzz + DCz * px__pz_
    py_  = py__dzz + DCz * py__pz_
    pz_  = pz__dzz + DCz * pz__pz_
    dxx_ = dxx_dzz + DCz * dxx_pz_
    dyy_ = dyy_dzz + DCz * dyy_pz_
    dzz_ = dzz_dzz + DCz * dzz_pz_
    dxy_ = dxy_dzz + DCz * dxy_pz_
    dxz_ = dxz_dzz + DCz * dxz_pz_
    dyz_ = dyz_dzz + DCz * dyz_pz_
    Iab => abcd(:,:,3,3)
    Iab(1,1) = dxx_ + BAx * px_
    Iab(2,1) = dxy_ + BAy * px_
    Iab(3,1) = dxz_ + BAz * px_
    Iab(1,2) = dxy_ + BAx * py_
    Iab(2,2) = dyy_ + BAy * py_
    Iab(3,2) = dyz_ + BAz * py_
    Iab(1,3) = dxz_ + BAx * pz_
    Iab(2,3) = dyz_ + BAy * pz_
    Iab(3,3) = dzz_ + BAz * pz_
    STOP_TIMER("SHELL1QUARTET:make_pppp")
     CHECK
  end subroutine

  subroutine make_ppps(self,abcd)
    SHELL1QUARTET :: self
  ! Make the (pp|ps) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT3(:,:,:), PTR :: Iabf
    STACK("SHELL1QUARTET:make_ppps")
    START_TIMER("SHELL1QUARTET:make_ppps")
    call create_(Iabf,3,3,3)
    call make_ppps_(self,Iabf)
    if (self%c%l==0) then
      abcd(:,:,1,:) = Iabf
    else ! .d.l==0
      abcd(:,:,:,1) = Iabf
    end if
    call destroy_(Iabf)
    STOP_TIMER("SHELL1QUARTET:make_ppps")
     CHECK
  end subroutine

  subroutine make_ppps_1(self,Iabf)
    SHELL1QUARTET :: self
  ! Make the (pp|ps) integrals, summed over primitives.
    IN :: self
    REALMAT3(:,:,:), OUT :: Iabf
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz,BAx,BAy,BAz
    REAL :: ce,bb,t2,t2_rz,t2_re,w,half_zinv,half_einv
    REAL :: Ix12,Ix21,Ix22,Ix31,Ix32
    REAL :: Iy12,Iy21,Iy22,Iy31,Iy32
    REAL :: Iz12,Iz21,Iz22,Iz31,Iz32
    REAL :: px__px_,py__px_,pz__px_
    REAL :: px__py_,py__py_,pz__py_
    REAL :: px__pz_,py__pz_,pz__pz_
    REAL :: dxx_px_,dyy_px_,dzz_px_,dxy_px_,dxz_px_,dyz_px_
    REAL :: dxx_py_,dyy_py_,dzz_py_,dxy_py_,dxz_py_,dyz_py_
    REAL :: dxx_pz_,dyy_pz_,dzz_pz_,dxy_pz_,dxz_pz_,dyz_pz_
    REAL :: Iy12_Iz21,Iy21_Iz12,Ix12_Iz21
    REAL :: Iy12_w,Iy21_w,Iy22_w
    INT :: ag,bg,cg,dg,nroots,n,j,k

    STACK("SHELL1QUARTET:make_ppps_1")
    START_TIMER("SHELL1QUARTET:make_ppps_1")
    px__px_ = ZERO; py__px_ = ZERO; pz__px_ = ZERO
    dxx_px_ = ZERO; dyy_px_ = ZERO; dzz_px_ = ZERO
    dxy_px_ = ZERO; dxz_px_ = ZERO; dyz_px_ = ZERO
    px__py_ = ZERO; py__py_ = ZERO; pz__py_ = ZERO
    dxx_py_ = ZERO; dyy_py_ = ZERO; dzz_py_ = ZERO
    dxy_py_ = ZERO; dxz_py_ = ZERO; dyz_py_ = ZERO
    px__pz_ = ZERO; py__pz_ = ZERO; pz__pz_ = ZERO
    dxx_pz_ = ZERO; dyy_pz_ = ZERO; dzz_pz_ = ZERO
    dxy_pz_ = ZERO; dxz_pz_ = ZERO; dyz_pz_ = ZERO

    nroots = (self%a%l+self%b%l+self%c%l+self%d%l+2)/2
    call create_(rys1,nroots)

    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            half_einv = HALF * einv
            do n=1,nroots
              t2    = rys1%r(n)
              w     = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              ce    = (ONE - t2_rz) * half_zinv
              Ix12 = QCx - t2_re * QPx       ! form 2 dimensional integrals.
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix31 = Ix21 * Ix21 + ce
              Iy31 = Iy21 * Iy21 + ce
              Iz31 = Iz21 * Iz21 + ce
              Ix32 = Ix21 * (Ix22 + bb) + ce * Ix12
              Iy32 = Iy21 * (Iy22 + bb) + ce * Iy12
              Iz32 = Iz21 * (Iz22 + bb) + ce * Iz12

              Iz21 = Iz21 * w    ! Merge the weights and reduce multiplications.
              Iz31 = Iz31 * w
              Iz12 = Iz12 * w
              Iz22 = Iz22 * w
              Iy12_Iz21 = Iy12 * Iz21
              Iy21_Iz12 = Iy21 * Iz12
              Ix12_Iz21 = Ix12 * Iz21
              Iy12_w    = Iy12 * w
              Iy21_w    = Iy21 * w
              Iy22_w    = Iy22 * w

              px__px_ = px__px_ + Ix22 * w         ! combine 2d ints.
              py__px_ = py__px_ + Ix12 * Iy21_w
              pz__px_ = pz__px_ + Ix12_Iz21
              dxx_px_ = dxx_px_ + Ix32 * w
              dyy_px_ = dyy_px_ + Ix12 * Iy31 * w
              dzz_px_ = dzz_px_ + Ix12 * Iz31
              dxy_px_ = dxy_px_ + Ix22 * Iy21_w
              dxz_px_ = dxz_px_ + Ix22 * Iz21
              dyz_px_ = dyz_px_ + Ix12_Iz21 * Iy21
              px__py_ = px__py_ + Ix21 * Iy12_w
              py__py_ = py__py_ + Iy22_w
              pz__py_ = pz__py_ + Iy12_Iz21
              dxx_py_ = dxx_py_ + Ix31 * Iy12_w
              dyy_py_ = dyy_py_ + Iy32 * w
              dzz_py_ = dzz_py_ + Iy12 * Iz31
              dxy_py_ = dxy_py_ + Ix21 * Iy22_w
              dxz_py_ = dxz_py_ + Ix21 * Iy12_Iz21
              dyz_py_ = dyz_py_ + Iy22 * Iz21
              px__pz_ = px__pz_ + Ix21 * Iz12
              py__pz_ = py__pz_ + Iy21_Iz12
              pz__pz_ = pz__pz_ + Iz22
              dxx_pz_ = dxx_pz_ + Ix31 * Iz12
              dyy_pz_ = dyy_pz_ + Iy31 * Iz12
              dzz_pz_ = dzz_pz_ + Iz32 * w
              dxy_pz_ = dxy_pz_ + Ix21 * Iy21_Iz12
              dxz_pz_ = dxz_pz_ + Ix21 * Iz22
              dyz_pz_ = dyz_pz_ + Iy21 * Iz22
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)

    BAx = self%pos_b(1) - self%pos_a(1)
    BAy = self%pos_b(2) - self%pos_a(2)
    BAz = self%pos_b(3) - self%pos_a(3)

    Iabf(1,1,1) = dxx_px_ + BAx * px__px_
    Iabf(1,1,2) = dxx_py_ + BAx * px__py_
    Iabf(1,1,3) = dxx_pz_ + BAx * px__pz_
    Iabf(2,1,1) = dxy_px_ + BAy * px__px_
    Iabf(2,1,2) = dxy_py_ + BAy * px__py_
    Iabf(2,1,3) = dxy_pz_ + BAy * px__pz_
    Iabf(3,1,1) = dxz_px_ + BAz * px__px_
    Iabf(3,1,2) = dxz_py_ + BAz * px__py_
    Iabf(3,1,3) = dxz_pz_ + BAz * px__pz_
    Iabf(1,2,1) = dxy_px_ + BAx * py__px_
    Iabf(1,2,2) = dxy_py_ + BAx * py__py_
    Iabf(1,2,3) = dxy_pz_ + BAx * py__pz_
    Iabf(2,2,1) = dyy_px_ + BAy * py__px_
    Iabf(2,2,2) = dyy_py_ + BAy * py__py_
    Iabf(2,2,3) = dyy_pz_ + BAy * py__pz_
    Iabf(3,2,1) = dyz_px_ + BAz * py__px_
    Iabf(3,2,2) = dyz_py_ + BAz * py__py_
    Iabf(3,2,3) = dyz_pz_ + BAz * py__pz_
    Iabf(1,3,1) = dxz_px_ + BAx * pz__px_
    Iabf(1,3,2) = dxz_py_ + BAx * pz__py_
    Iabf(1,3,3) = dxz_pz_ + BAx * pz__pz_
    Iabf(2,3,1) = dyz_px_ + BAy * pz__px_
    Iabf(2,3,2) = dyz_py_ + BAy * pz__py_
    Iabf(2,3,3) = dyz_pz_ + BAy * pz__pz_
    Iabf(3,3,1) = dzz_px_ + BAz * pz__px_
    Iabf(3,3,2) = dzz_py_ + BAz * pz__py_
    Iabf(3,3,3) = dzz_pz_ + BAz * pz__pz_
    STOP_TIMER("SHELL1QUARTET:make_ppps_1")
     CHECK
  end subroutine

  subroutine make_pspp(self,abcd)
    SHELL1QUARTET :: self
  ! Make the (ps|pp) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT3(:,:,:), PTR :: Iabf
    STACK("SHELL1QUARTET:make_pspp")
    START_TIMER("SHELL1QUARTET:make_pspp")
    call create_(Iabf,3,3,3)
    call make_pspp_(self,Iabf)
    if (self%a%l==0) then
      abcd(1,:,:,:) = Iabf
    else ! .b.l==0
      abcd(:,1,:,:) = Iabf
    end if
    call destroy_(Iabf)
    STOP_TIMER("SHELL1QUARTET:make_pspp")
     CHECK
  end subroutine

  subroutine make_pspp_1(self,Iabf)
    SHELL1QUARTET :: self
  ! Make the (ps|pp) integrals, summed over primitives.
    IN :: self
    REALMAT3(:,:,:), OUT :: Iabf
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,QCx,QCy,QCz,Px,Py,Pz,DCx,DCy,DCz
    REAL :: cf,bb,t2,t2_rz,t2_re,w,half_zinv,half_einv
    REAL :: Ix12,Ix13,Ix21,Ix22,Ix23
    REAL :: Iy12,Iy13,Iy21,Iy22,Iy23
    REAL :: Iz12,Iz13,Iz21,Iz22,Iz23
    REAL :: px__px_,py__px_,pz__px_
    REAL :: px__py_,py__py_,pz__py_
    REAL :: px__pz_,py__pz_,pz__pz_
    REAL :: px__dxx,py__dxx,pz__dxx
    REAL :: px__dyy,py__dyy,pz__dyy
    REAL :: px__dzz,py__dzz,pz__dzz
    REAL :: px__dxy,py__dxy,pz__dxy
    REAL :: px__dxz,py__dxz,pz__dxz
    REAL :: px__dyz,py__dyz,pz__dyz
    REAL :: Iy12_Iz21,Iy21_Iz12,Ix21_Iz12
    REAL :: Iy12_w,Iy21_w,Iy22_w
    INT :: ag,bg,cg,dg,nroots,n,j,k

    STACK("SHELL1QUARTET:make_pspp_1")
    START_TIMER("SHELL1QUARTET:make_pspp_1")
    px__px_ = ZERO; py__px_ = ZERO; pz__px_ = ZERO
    px__py_ = ZERO; py__py_ = ZERO; pz__py_ = ZERO
    px__pz_ = ZERO; py__pz_ = ZERO; pz__pz_ = ZERO
    px__dxx = ZERO; py__dxx = ZERO; pz__dxx = ZERO
    px__dyy = ZERO; py__dyy = ZERO; pz__dyy = ZERO
    px__dzz = ZERO; py__dzz = ZERO; pz__dzz = ZERO
    px__dxy = ZERO; py__dxy = ZERO; pz__dxy = ZERO
    px__dxz = ZERO; py__dxz = ZERO; pz__dxz = ZERO
    px__dyz = ZERO; py__dyz = ZERO; pz__dyz = ZERO

    nroots = (self%ab_l_sum+self%cd_l_sum+2)/2
    call create_(rys1,nroots)
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * normab * self%cd_cc_prefactor(j) * sqrt(rho)
            half_zinv = HALF * zinv
            half_einv = HALF * einv
            do n=1,nroots
              t2    = rys1%r(n)
              w     = rys1%w(n)
              t2_rz = t2 * rho_zinv
              t2_re = t2 * rho_einv
              bb    = t2_rz * half_einv
              cf    = (ONE - t2_re) * half_einv
              Ix12 = QCx - t2_re * QPx           ! form 2 dimensional integrals.
              Iy12 = QCy - t2_re * QPy
              Iz12 = QCz - t2_re * QPz
              Ix21 = PAx + t2_rz * QPx
              Iy21 = PAy + t2_rz * QPy
              Iz21 = PAz + t2_rz * QPz
              Ix22 = Ix12 * Ix21 + bb
              Iy22 = Iy12 * Iy21 + bb
              Iz22 = Iz12 * Iz21 + bb
              Ix13 = Ix12 * Ix12 + cf
              Iy13 = Iy12 * Iy12 + cf
              Iz13 = Iz12 * Iz12 + cf
              Ix23 = Ix12 * (Ix22 + bb) + cf * Ix21
              Iy23 = Iy12 * (Iy22 + bb) + cf * Iy21
              Iz23 = Iz12 * (Iz22 + bb) + cf * Iz21

              Iz21 = Iz21 * w    ! Merge the weights and reduce multiplications.
              Iz12 = Iz12 * w
              Iz22 = Iz22 * w
              Iz13 = Iz13 * w
              Iy12_Iz21 = Iy12 * Iz21
              Iy21_Iz12 = Iy21 * Iz12
              Ix21_Iz12 = Ix21 * Iz12
              Iy12_w    = Iy12 * w
              Iy21_w    = Iy21 * w
              Iy22_w    = Iy22 * w

              px__px_ = px__px_ + Ix22 * w     ! combine 2d ints.
              py__px_ = py__px_ + Ix12 * Iy21_w
              pz__px_ = pz__px_ + Ix12 * Iz21
              px__py_ = px__py_ + Ix21 * Iy12_w
              py__py_ = py__py_ + Iy22_w
              pz__py_ = pz__py_ + Iy12_Iz21
              px__pz_ = px__pz_ + Ix21_Iz12
              py__pz_ = py__pz_ + Iy21_Iz12
              pz__pz_ = pz__pz_ + Iz22
              px__dxx = px__dxx + Ix23 * w
              py__dxx = py__dxx + Ix13 * Iy21_w
              pz__dxx = pz__dxx + Ix13 * Iz21
              px__dyy = px__dyy + Ix21 * Iy13 * w
              py__dyy = py__dyy + Iy23 * w
              pz__dyy = pz__dyy + Iy13 * Iz21
              px__dzz = px__dzz + Ix21 * Iz13
              py__dzz = py__dzz + Iy21 * Iz13
              pz__dzz = pz__dzz + Iz23 * w
              px__dxy = px__dxy + Ix22 * Iy12_w
              py__dxy = py__dxy + Ix12 * Iy22_w
              pz__dxy = pz__dxy + Ix12 * Iy12_Iz21
              px__dxz = px__dxz + Ix22 * Iz12
              py__dxz = py__dxz + Ix12 * Iy21_Iz12
              pz__dxz = pz__dxz + Ix12 * Iz22
              px__dyz = px__dyz + Ix21_Iz12 * Iy12
              py__dyz = py__dyz + Iy22 * Iz12
              pz__dyz = pz__dyz + Iy12 * Iz22
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)

    DCx = self%pos_d(1) - self%pos_c(1)
    DCy = self%pos_d(2) - self%pos_c(2)
    DCz = self%pos_d(3) - self%pos_c(3)

    Iabf(1,1,1) = px__dxx + DCx * px__px_
    Iabf(2,1,1) = py__dxx + DCx * py__px_
    Iabf(3,1,1) = pz__dxx + DCx * pz__px_
    Iabf(1,2,1) = px__dxy + DCy * px__px_
    Iabf(2,2,1) = py__dxy + DCy * py__px_
    Iabf(3,2,1) = pz__dxy + DCy * pz__px_
    Iabf(1,3,1) = px__dxz + DCz * px__px_
    Iabf(2,3,1) = py__dxz + DCz * py__px_
    Iabf(3,3,1) = pz__dxz + DCz * pz__px_
    Iabf(1,1,2) = px__dxy + DCx * px__py_
    Iabf(2,1,2) = py__dxy + DCx * py__py_
    Iabf(3,1,2) = pz__dxy + DCx * pz__py_
    Iabf(1,2,2) = px__dyy + DCy * px__py_
    Iabf(2,2,2) = py__dyy + DCy * py__py_
    Iabf(3,2,2) = pz__dyy + DCy * pz__py_
    Iabf(1,3,2) = px__dyz + DCz * px__py_
    Iabf(2,3,2) = py__dyz + DCz * py__py_
    Iabf(3,3,2) = pz__dyz + DCz * pz__py_
    Iabf(1,1,3) = px__dxz + DCx * px__pz_
    Iabf(2,1,3) = py__dxz + DCx * py__pz_
    Iabf(3,1,3) = pz__dxz + DCx * pz__pz_
    Iabf(1,2,3) = px__dyz + DCy * px__pz_
    Iabf(2,2,3) = py__dyz + DCy * py__pz_
    Iabf(3,2,3) = pz__dyz + DCy * pz__pz_
    Iabf(1,3,3) = px__dzz + DCz * px__pz_
    Iabf(2,3,3) = py__dzz + DCz * py__pz_
    Iabf(3,3,3) = pz__dzz + DCz * pz__pz_
    STOP_TIMER("SHELL1QUARTET:make_pspp_1")
     CHECK
  end subroutine

  subroutine make_psps(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ps|ps) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(3,3) :: psps
    STACK("SHELL1QUARTET:make_psps")
    START_TIMER("SHELL1QUARTET:make_psps")
    call make_psps_(self,psps)
    if (self%a%l == 1) then
      if (self%c%l ==1) then
        abcd(:,1,:,1) = psps ! psps
      else
        abcd(:,1,1,:) = psps ! pssp
      end if
    else
      if (self%c%l ==1) then
        abcd(1,:,:,1) = psps ! spps
      else
        abcd(1,:,1,:) = psps ! spsp
      end if
    end if
    STOP_TIMER("SHELL1QUARTET:make_psps")
     CHECK
  end subroutine

  subroutine make_psps_1(self,psps)
    SHELL1QUARTET :: self
  ! Makes the (ps|ps) integrals, summed over primitives.
  ! Does ps|ps, ps|sp, sp|ps, sp|sp.
    IN :: self
    REALMAT(:,:), OUT :: psps
    RYS, PTR :: rys1
    REAL :: zeta,eta,xx,normab,zinv,rho,einv,norm,half_zinv
    REAL :: QPx,QPy,QPz,QCx,QCy,QCz,PAx,PAy,PAz,Px,Py,Pz,Qx,Qy,Qz
    REAL :: root,tmp2,tmp3,tmp4,weight,rho_zinv,rho_einv
    REAL :: Ix12,Iy12,Iz12,Ix21,Iy21,Iz21
    REAL :: psps11,psps12,psps13,psps21,psps22,psps23,psps31,psps32,psps33
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_psps_1")
    START_TIMER("SHELL1QUARTET:make_psps_1")
    call create_(rys1,2)

    psps11=ZERO; psps12=ZERO; psps13=ZERO
    psps21=ZERO; psps22=ZERO; psps23=ZERO
    psps31=ZERO; psps32=ZERO; psps33=ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            !half_rho_einv_zinv = rho_einv * half_zinv
            norm = normab * self%cd_cc_prefactor(j) * sqrt(rho)
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            do n=1,2
              root   = rys1%r(n)
              weight = rys1%w(n) * norm
              tmp2 = root * rho_zinv
              tmp3 = root * rho_einv
              !tmp4 = root * half_rho_einv_zinv*weight
              tmp4 = tmp3 * half_zinv*weight
              Ix21 = (PAx + tmp2 * QPx)*weight
              Iy21 = (PAy + tmp2 * QPy)*weight
              Iz21 = (PAz + tmp2 * QPz)*weight
              ! Note that weights are included in the above variables, which
              ! filter through to the others.
              Ix12 = QCx - tmp3 * QPx
              Iy12 = QCy - tmp3 * QPy
              Iz12 = QCz - tmp3 * QPz
              psps11 = psps11 + Ix12 * Ix21 + tmp4 ! = Ix22
              psps12 = psps12 + Ix21 * Iy12
              psps13 = psps13 + Ix21 * Iz12
              psps21 = psps21 + Ix12 * Iy21
              psps22 = psps22 + Iy12 * Iy21 + tmp4 ! = Iy22
              psps23 = psps23 + Iy21 * Iz12
              psps31 = psps31 + Ix12 * Iz21
              psps32 = psps32 + Iy12 * Iz21
              psps33 = psps33 + Iz12 * Iz21 + tmp4 ! = Iz22
            end do
          end do
        end do
      end do
    end do
    psps(1,1) = psps11
    psps(2,1) = psps21
    psps(3,1) = psps31
    psps(1,2) = psps12
    psps(2,2) = psps22
    psps(3,2) = psps32
    psps(1,3) = psps13
    psps(2,3) = psps23
    psps(3,3) = psps33
    call destroy_(rys1)
    STOP_TIMER("SHELL1QUARTET:make_psps_1")
     CHECK
  end subroutine

  subroutine make_abss(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ab|ss) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(:,:), PTR :: abss
    STACK("SHELL1QUARTET:make_abss")
    START_TIMER("SHELL1QUARTET:make_abss")
    call create_(abss,size(abcd,1),size(abcd,2))
    call make_abss_(self,abss)
    abcd(:,:,1,1) = abss(:,:)
    call destroy_(abss)
    STOP_TIMER("SHELL1QUARTET:make_abss")
     CHECK
  end subroutine

  subroutine make_abss_1(self,ab)
    SHELL1QUARTET :: self
  ! Makes the (ab|ss) integrals, summed over primitives.
    IN :: self
    REALMAT(:,:), OUT :: ab
    REALVEC(:), PTR :: esss
    SHELL2, PTR :: sh
    INT :: eub,i,j,k,imax
    STACK("SHELL1QUARTET:make_abss_1")
    START_TIMER("SHELL1QUARTET:make_abss_1")
    eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
    call create_(esss,eub)
    call make_esss_(self,esss)
    call create_(sh,self%a,self%b,self%pos_a,self%pos_b)
    call transfer_(sh,esss,ab)
    if (self%ab_l_sum > 1) then
      k = 0
      imax = self%a%n_comp
      do j=1,self%b%n_comp
        do i=1,imax
          k = k + 1
          ab(i,j) = ab(i,j) * self%ab_normalising_factors(k)
        end do
      end do
    end if
    call destroy_(sh)
    call destroy_(esss)
    STOP_TIMER("SHELL1QUARTET:make_abss_1")
     CHECK
  end subroutine

  subroutine make_asss(self,asss)
    SHELL1QUARTET :: self
  ! Makes the (as|ss) or (sb|ss) integrals, summed over primitives.
    REALVEC(:), OUT :: asss
    STACK("SHELL1QUARTET:make_asss")
    START_TIMER("SHELL1QUARTET:make_asss")
    call make_esss_(self,asss)
    if (self%ab_l_sum > 1) asss = asss * self%ab_normalising_factors
    STOP_TIMER("SHELL1QUARTET:make_asss")
     CHECK
  end subroutine

  subroutine make_esss(self,esss)
    SHELL1QUARTET :: self
  ! Makes the (es|ss) integrals, summed over primitives.
    IN :: self
    REALVEC(:), OUT :: esss
    REALVEC(:), PTR :: Izz
    REALMAT(:,:), PTR :: Ix,Iy,Iz,Iyz
    INTVEC(:), PTR :: e_x,e_y,e_z,ii_ivec
    RYS, PTR :: rys1
    REAL :: zeta,zinv,eta,rho,xx,normab,rho_zinv,half_zinv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,Px,Py,Pz
    REAL :: rzt,ce1,rzthze,Ix2,Iy2,Iz2,Ix3,Iy3,Iz3,wt
    REAL :: Ixe,Ixe1,Iye,Iye1,Ize,Ize1,Ixep1,Iyep1,Izep1
    INT :: ag,bg,cg,dg,nroots,eub,dim
    INT :: ep1,n,dim1,y,z,ii,e,j,k
    BIN :: apply_rms

    STACK("SHELL1QUARTET:make_esss")
    START_TIMER("SHELL1QUARTET:make_esss")
    dim = self%ab_l_sum+1
    dim1 = self%ab_l_sum+2
    eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))

    nroots = (dim1) / 2
    call create_(rys1,nroots)
    call create_(Ix,nroots,dim)
    call create_(Iy,nroots,dim)
    call create_(Iz,nroots,dim)
    call create_(Iyz,nroots,dim*dim1/2)

    apply_rms = self%ab_l_min < ERI_rms_min_l
    e_x => self%ab_form_3dints_x_indices
    e_y => self%ab_form_3dints_y_indices
    e_z => self%ab_form_3dints_z_indices
    if (apply_rms) ii_ivec => self%ab_form_3dints_yz_rms_indices

    esss = ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w = rys1%w * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            ! Form the 2 dimensional integrals
            do n=1,nroots
              wt = rys1%w(n)
              rzt      = rys1%r(n) * rho_zinv
              rzthze   = (ONE - rzt) * half_zinv
              Ix2 = PAx + rzt * QPx
              Iy2 = PAy + rzt * QPy
              Iz2 = PAz + rzt * QPz
              Ix3 = Ix2 * Ix2 + rzthze
              Iy3 = Iy2 * Iy2 + rzthze
              Iz3 = Iz2 * Iz2 + rzthze
              Ix(n,1) = ONE; Iy(n,1) = ONE; Iz(n,1) = wt
              Ix(n,2) = Ix2; Iy(n,2) = Iy2; Iz(n,2) = Iz2 * wt
              Ix(n,3) = Ix3; Iy(n,3) = Iy3; Iz(n,3) = Iz3 * wt

              if (self%ab_l_sum > 2) then
                Ixe = Ix3;    Iye = Iy3;    Ize = Iz3
                Ixe1 = Ix2;   Iye1 = Iy2;   Ize1 = Iz2
                ce1 = rzthze
                do ep1 = 4, self%ab_l_sum+1
                  ce1 = ce1 + rzthze
                  Ixep1 = Ix2 * Ixe + ce1 * Ixe1
                  Iyep1 = Iy2 * Iye + ce1 * Iye1
                  Izep1 = Iz2 * Ize + ce1 * Ize1
                  Ix(n,ep1) = Ixep1;   Iy(n,ep1) = Iyep1;   Iz(n,ep1) = Izep1 * wt
                  Ixe1 = Ixe;   Iye1 = Iye;   Ize1 = Ize
                  Ixe = Ixep1;  Iye = Iyep1;  Ize = Izep1
                end do
              end if
            end do

            if (apply_rms) then
              ! Apply the reduced multiplication scheme.
              ii = 0
              do z=1,dim
                Izz => Iz(:,z)
                do y=1,dim1-z
                  ii = ii + 1
                  Iyz(:,ii) = Izz * Iy(:,y)
                end do
              end do
              esss = esss + sum(Ix(:,e_x) * Iyz(:,ii_ivec),1)
              !do e=1,eub
              !  esss(e) = esss(e) + sum(Ix(:,e_x(e)) * Iyz(:,ii_ivec(e)))
              !end
            else
              esss = esss + sum(Ix(:,e_x) * Iy(:,e_y) * Iz(:,e_z),1)
              !do e=1,eub
              !  esss(e) = esss(e) + sum(Ix(:,e_x(e)) * Iy(:,e_y(e)) * Iz(:,e_z(e)))
              !end
            end if
          end do
        end do
      end do
    end do
    call destroy_(Iyz)
    call destroy_(Iz)
    call destroy_(Iy)
    call destroy_(Ix)
    call destroy_(rys1)
    STOP_TIMER("SHELL1QUARTET:make_esss")
     CHECK
  end subroutine

  subroutine make_sscd(self,abcd)
    SHELL1QUARTET :: self
  ! Makes the (ss|cd) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(:,:), PTR :: sscd
    STACK("SHELL1QUARTET:make_sscd")
    START_TIMER("SHELL1QUARTET:make_sscd")
    call create_(sscd,size(abcd,3),size(abcd,4))
    call make_sscd_(self,sscd)
    abcd(1,1,:,:) = sscd(:,:)
    call destroy_(sscd)
    STOP_TIMER("SHELL1QUARTET:make_sscd")
     CHECK
  end subroutine

  subroutine make_sscd_1(self,sscd)
    SHELL1QUARTET :: self
  ! Makes the (ss|cd) integrals, summed over primitives.
    IN :: self
    REALMAT(:,:), OUT :: sscd
    REALVEC(:), PTR :: ssfs
    SHELL2, PTR :: sh
    INT :: fub,i,j,k,imax
    STACK("SHELL1QUARTET:make_sscd_1")
    START_TIMER("SHELL1QUARTET:make_sscd_1")
    fub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))
    call create_(ssfs,fub)
    call make_ssfs_(self,ssfs)
    call create_(sh,self%c,self%d,self%pos_c,self%pos_d)
    call transfer_(sh,ssfs,sscd)
    if (self%cd_l_sum > 1) then
      k = 0
      imax = self%c%n_comp
      do j=1,self%d%n_comp
        do i=1,imax
          k = k + 1
          sscd(i,j) = sscd(i,j) * self%cd_normalising_factors(k)
        end do
      end do
    end if
    call destroy_(sh)
    call destroy_(ssfs)
    STOP_TIMER("SHELL1QUARTET:make_sscd_1")
     CHECK
  end subroutine

  subroutine make_sscs(self,sscs)
    SHELL1QUARTET :: self
  ! Makes the (ss|cs) or (ss|sd) integrals, summed over primitives.
    IN :: self
    REALVEC(:), OUT :: sscs
    STACK("SHELL1QUARTET:make_sscs")
    START_TIMER("SHELL1QUARTET:make_sscs")
    call make_ssfs_(self,sscs)
    if (self%cd_l_sum > 1) sscs = sscs * self%cd_normalising_factors
    STOP_TIMER("SHELL1QUARTET:make_sscs")
     CHECK
  end subroutine

  subroutine make_ssfs(self,ssfs)
    SHELL1QUARTET :: self
  ! Makes the (ss|cd) integrals, summed over primitives.
    IN :: self
    REALVEC(:), OUT :: ssfs
    REALVEC(:), PTR :: Izz
    REALMAT(:,:), PTR :: Ix,Iy,Iz,Iyz
    RYS, PTR :: rys1
    INTVEC(:), PTR :: f_x,f_y,f_z,ii_ivec
    REAL :: zeta,eta,einv,rho,xx
    REAL :: normab,rho_einv,half_einv
    REAL :: QPx,QPy,QPz,QCx,QCy,QCz,Qx,Qy,Qz,Px,Py,Pz
    REAL :: Ix2,Iy2,Iz2,Ix3,Iy3,Iz3
    REAL :: Ixf,Ixf1,Iyf,Iyf1,Izf,Izf1,Ixfp1,Iyfp1,Izfp1
    REAL :: ret,cf1,rethen,wt
    INT :: ag,bg,cg,dg,nroots,fub,f,y,z
    INT :: fp1,n,ii,dim0,dim1,j,k
    BIN :: apply_rms

    STACK("SHELL1QUARTET:make_ssfs")
    START_TIMER("SHELL1QUARTET:make_ssfs")
    dim0 = self%cd_l_sum+1
    dim1 = self%cd_l_sum+2
    fub = n_comp_sum_(self%cd_l_sum) - n_comp_sum_((self%cd_l_max-1))

    nroots = dim1/2
    call create_(rys1,nroots)
    call create_(Ix,nroots,dim0)
    call create_(Iy,nroots,dim0)
    call create_(Iz,nroots,dim0)
    call create_(Iyz,nroots,dim0*dim1/2)

    apply_rms = self%cd_l_min < ERI_rms_min_l
    f_x => self%cd_form_3dints_x_indices
    f_y => self%cd_form_3dints_y_indices
    f_z => self%cd_form_3dints_z_indices
    if (apply_rms) ii_ivec => self%cd_form_3dints_yz_rms_indices

    ssfs=ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w = rys1%w * normab * self%cd_cc_prefactor(j) * sqrt(rho)
            half_einv = HALF * einv

            ! Form the 2 dimensional integrals.
            do n=1,nroots
              ret     = rys1%r(n) * rho_einv
              rethen  = (ONE - ret) * half_einv
              Ix2 = QCx - ret * QPx
              Iy2 = QCy - ret * QPy
              Iz2 = QCz - ret * QPz
              Ix3 = Ix2 * Ix2 + rethen
              Iy3 = Iy2 * Iy2 + rethen
              Iz3 = Iz2 * Iz2 + rethen
              wt = rys1%w(n)

              Ix(n,1) = ONE; Iy(n,1) = ONE; Iz(n,1) = wt
              Ix(n,2) = Ix2; Iy(n,2) = Iy2; Iz(n,2) = Iz2 * wt
              Ix(n,3) = Ix3; Iy(n,3) = Iy3; Iz(n,3) = Iz3 * wt

              if (self%cd_l_sum > 2) then
                Ixf = Ix3;    Iyf = Iy3;    Izf = Iz3
                Ixf1 = Ix2;   Iyf1 = Iy2;   Izf1 = Iz2
                cf1 = rethen
                do fp1 = 4, self%cd_l_sum+1
                  cf1 = cf1+rethen
                  !cf1 = (fp1-2) * rethen
                  Ixfp1 = Ix2 * Ixf + cf1 * Ixf1
                  Iyfp1 = Iy2 * Iyf + cf1 * Iyf1
                  Izfp1 = Iz2 * Izf + cf1 * Izf1
                  Ix(n,fp1) = Ixfp1;   Iy(n,fp1) = Iyfp1;   Iz(n,fp1) = Izfp1 * wt
                  Ixf1 = Ixf;   Iyf1 = Iyf;   Izf1 = Izf
                  Ixf = Ixfp1;  Iyf = Iyfp1;  Izf = Izfp1
                end do
              end if
            end do

            if (apply_rms) then
              ! Apply the reduced multiplication scheme.
              ii = 0
              do z=1,dim0
                Izz => Iz(:,z)
                do y=1,dim1-z
                  ii = ii + 1
                  Iyz(:,ii) = Izz * Iy(:,y)
                end do
              end do
              ssfs = ssfs + sum(Ix(:,f_x) * Iyz(:,ii_ivec),1)
              !do f=1,fub
              !  ssfs(f) = ssfs(f) + sum(Ix(:,f_x(f)) * Iyz(:,ii_ivec(f)))
              !end
            else
              ssfs = ssfs + sum(Ix(:,f_x) * Iy(:,f_y) * Iz(:,f_z),1)
              !do f=1,fub
              !  ssfs(f) = ssfs(f) + sum(Ix(:,f_x(f)) * Iy(:,f_y(f)) * Iz(:,f_z(f)))
              !end
            end if
          end do
        end do
      end do
    end do
    call destroy_(Iyz)
    call destroy_(Iz)
    call destroy_(Iy)
    call destroy_(Ix)
    call destroy_(rys1)
    STOP_TIMER("SHELL1QUARTET:make_ssfs")
     CHECK
  end subroutine

  subroutine make_ppss(self,abcd)
    SHELL1QUARTET :: self
  ! Creates the initial (pp|ss) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(3,3) :: abss
    STACK("SHELL1QUARTET:make_ppss")
    START_TIMER("SHELL1QUARTET:make_ppss")
    call make_ppss_(self,abss)
    abcd(:,:,1,1) = abss(:,:)
    STOP_TIMER("SHELL1QUARTET:make_ppss")
     CHECK
  end subroutine

  subroutine make_ppss_1(self,abss)
    SHELL1QUARTET :: self
  ! Creates the initial (pp|ss) integrals, summed over primitives.
    IN :: self
    REALMAT(:,:), OUT :: abss
    RYS, PTR :: rys1
    REAL :: zeta,zinv,eta,rho,xx
    REAL :: normab,norm,rho_zinv,half_zinv
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz
    REAL :: Qx,Qy,Qz,Px,Py,Pz
    REAL :: rzt,rzthze
    REAL :: Ix2,Iy2,Iz2,Ix3,Iy3,Iz3,Ix2_w,Iy2_w,Iz2_w,w
    REAL :: px_s,py_s,pz_s,dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s
    REAL :: BAx,BAy,BAz
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_ppss_1")
    START_TIMER("SHELL1QUARTET:make_ppss_1")
    call create_(rys1,2)              ! nroots = 2
    px_s  = ZERO; py_s  = ZERO; pz_s  = ZERO
    dxx_s = ZERO; dyy_s = ZERO; dzz_s = ZERO
    dxy_s = ZERO; dxz_s = ZERO; dyz_s = ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            norm = normab * self%cd_cc_prefactor(j) * sqrt(rho)
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w = rys1%w * norm
            do n=1,2
              rzt      = rys1%r(n) * rho_zinv
              w        = rys1%w(n)
              rzthze   = (ONE - rzt) * half_zinv * w
              Ix2 = PAx + rzt * QPx
              Iy2 = PAy + rzt * QPy
              Iz2 = PAz + rzt * QPz
              Iz2_w = Iz2 * w
              Iy2_w = Iy2 * w
              Ix2_w = Ix2 * w
              Ix3 = Ix2 * Ix2_w + rzthze
              Iy3 = Iy2 * Iy2_w + rzthze
              Iz3 = Iz2 * Iz2_w + rzthze

              px_s  = px_s  + Ix2_w
              py_s  = py_s  + Iy2_w
              pz_s  = pz_s  + Iz2_w
              dxx_s = dxx_s + Ix3
              dyy_s = dyy_s + Iy3
              dzz_s = dzz_s + Iz3
              dxy_s = dxy_s + Ix2 * Iy2_w
              dxz_s = dxz_s + Ix2 * Iz2_w
              dyz_s = dyz_s + Iy2 * Iz2_w
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)

    ! Transfer equation.
    BAx = self%pos_b(1) - self%pos_a(1)
    BAy = self%pos_b(2) - self%pos_a(2)
    BAz = self%pos_b(3) - self%pos_a(3)
    abss(1,1) = dxx_s + BAx * px_s
    abss(1,2) = dxy_s + BAx * py_s
    abss(1,3) = dxz_s + BAx * pz_s
    abss(2,1) = dxy_s + BAy * px_s
    abss(2,2) = dyy_s + BAy * py_s
    abss(2,3) = dyz_s + BAy * pz_s
    abss(3,1) = dxz_s + BAz * px_s
    abss(3,2) = dyz_s + BAz * py_s
    abss(3,3) = dzz_s + BAz * pz_s
    STOP_TIMER("SHELL1QUARTET:make_ppss_1")
     CHECK
  end subroutine

  subroutine make_sspp(self,abcd)
    SHELL1QUARTET :: self
  ! Creates the (ss|pp) integrals, summed over primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT(3,3) :: sscd
    STACK("SHELL1QUARTET:make_sspp")
    START_TIMER("SHELL1QUARTET:make_sspp")
    call make_sspp_(self,sscd)
    abcd(1,1,:,:) = sscd(:,:)
    STOP_TIMER("SHELL1QUARTET:make_sspp")
     CHECK
  end subroutine

  subroutine make_sspp_1(self,sscd)
    SHELL1QUARTET :: self
  ! Creates the (ss|pp) integrals, summed over primitives.
    IN :: self
    REALMAT(:,:), OUT :: sscd
    RYS, PTR :: rys1
    REAL :: zeta,eta,einv,rho,xx
    REAL :: normab,rho_einv,half_einv
    REAL :: QPx,QPy,QPz,QCx,QCy,QCz,DCx,DCy,DCz
    REAL :: Qx,Qy,Qz,Px,Py,Pz
    REAL :: ret,rethen
    REAL :: Ix2,Iy2,Iz2,Ix3,Iy3,Iz3,Ix2_w,Iy2_w,Iz2_w,w
    REAL :: px_s,py_s,pz_s,dxx_s,dyy_s,dzz_s,dxy_s,dxz_s,dyz_s
    INT :: ag,bg,cg,dg,n,j,k

    STACK("SHELL1QUARTET:make_sspp_1")
    START_TIMER("SHELL1QUARTET:make_sspp_1")
    px_s  = ZERO; py_s  = ZERO; pz_s  = ZERO
    dxx_s = ZERO; dyy_s = ZERO; dzz_s = ZERO
    dxy_s = ZERO; dxz_s = ZERO; dyz_s = ZERO

    call create_(rys1,2)
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            half_einv = HALF * einv
            rho_einv = rho * einv
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rys1%w = rys1%w * normab * self%cd_cc_prefactor(j) * sqrt(rho)

            do n=1,2
              ret     = rys1%r(n) * rho_einv
              w       = rys1%w(n)
              rethen  = (ONE - ret) * half_einv * w
              Ix2 = QCx - ret * QPx
              Iy2 = QCy - ret * QPy
              Iz2 = QCz - ret * QPz
              Iz2_w = Iz2 * w
              Iy2_w = Iy2 * w
              Ix2_w = Ix2 * w
              Ix3 = Ix2 * Ix2_w + rethen
              Iy3 = Iy2 * Iy2_w + rethen
              Iz3 = Iz2 * Iz2_w + rethen

              px_s  = px_s  + Ix2_w
              py_s  = py_s  + Iy2_w
              pz_s  = pz_s  + Iz2_w
              dxx_s = dxx_s + Ix3
              dyy_s = dyy_s + Iy3
              dzz_s = dzz_s + Iz3
              dxy_s = dxy_s + Ix2 * Iy2_w
              dxz_s = dxz_s + Ix2 * Iz2_w
              dyz_s = dyz_s + Iy2 * Iz2_w
            end do
          end do
        end do
      end do
    end do
    call destroy_(rys1)

    ! Transfer equation.
    DCx = self%pos_d(1) - self%pos_c(1)
    DCy = self%pos_d(2) - self%pos_c(2)
    DCz = self%pos_d(3) - self%pos_c(3)
    sscd(1,1) = dxx_s + DCx * px_s
    sscd(1,2) = dxy_s + DCx * py_s
    sscd(1,3) = dxz_s + DCx * pz_s
    sscd(2,1) = dxy_s + DCy * px_s
    sscd(2,2) = dyy_s + DCy * py_s
    sscd(2,3) = dyz_s + DCy * pz_s
    sscd(3,1) = dxz_s + DCz * px_s
    sscd(3,2) = dyz_s + DCz * py_s
    sscd(3,3) = dzz_s + DCz * pz_s
    STOP_TIMER("SHELL1QUARTET:make_sspp_1")
     CHECK
  end subroutine

  subroutine make_psss(self,abcd)
    SHELL1QUARTET :: self
  ! Creates the (ps|ss) or (sp|ss) integrals, summed over  primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALVEC(3) :: psss
    STACK("SHELL1QUARTET:make_psss")
    START_TIMER("SHELL1QUARTET:make_psss")
    call make_psss_(self,psss)
    abcd(1,1,1,1) = psss(1)
    if (self%a%l == 1) then           ! psss
      abcd(2,1,1,1) = psss(2)
      abcd(3,1,1,1) = psss(3)
    else                          ! spss
      abcd(1,2,1,1) = psss(2)
      abcd(1,3,1,1) = psss(3)
    end if
    STOP_TIMER("SHELL1QUARTET:make_psss")
     CHECK
  end subroutine

  subroutine make_psss_1(self,psss)
    SHELL1QUARTET :: self
  ! Creates the initial (ps|ss) or (sp|ss) integrals, summed over the
  ! primitives.
    IN :: self
    REALVEC(:), OUT :: psss
    RYS, PTR :: rys1
    REAL :: zeta,zinv,eta,rho,xx,normab,norm_w,rzt
    REAL :: QPx,QPy,QPz,PAx,PAy,PAz,Qx,Qy,Qz,Px,Py,Pz
    REAL :: psss1,psss2,psss3
    INT :: ag,bg,cg,dg,j,k

    STACK("SHELL1QUARTET:make_psss_1")
    START_TIMER("SHELL1QUARTET:make_psss_1")
    call create_(rys1,1)
    psss1 = ZERO
    psss2 = ZERO
    psss3 = ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        zinv = ONE/zeta
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        PAx = self%ab_center_diff(1,k)
        PAy = self%ab_center_diff(2,k)
        PAz = self%ab_center_diff(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            rho  = zeta * eta / (zeta + eta)
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            rzt = rys1%r(1) * rho * zinv
            norm_w = rys1%w(1) * normab * self%cd_cc_prefactor(j) * sqrt(rho)
            psss1 = psss1 + norm_w * (PAx + rzt * QPx)
            psss2 = psss2 + norm_w * (PAy + rzt * QPy)
            psss3 = psss3 + norm_w * (PAz + rzt * QPz)
          end do
        end do
      end do
    end do
    call destroy_(rys1)
    psss(1) = psss1
    psss(2) = psss2
    psss(3) = psss3
    STOP_TIMER("SHELL1QUARTET:make_psss_1")
     CHECK
  end subroutine

  subroutine make_ssps(self,abcd)
    SHELL1QUARTET :: self
  ! Creates the (ss|ps) or (ss|sp) integrals, summed over the primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALVEC(3) :: ssps
    STACK("SHELL1QUARTET:make_ssps")
    START_TIMER("SHELL1QUARTET:make_ssps")
    call make_ssps_(self,ssps)
    abcd(1,1,1,1) = ssps(1)
    if (self%c%l == 1) then           ! ssps
      abcd(1,1,2,1) = ssps(2)
      abcd(1,1,3,1) = ssps(3)
    else                          ! sssp
      abcd(1,1,1,2) = ssps(2)
      abcd(1,1,1,3) = ssps(3)
    end if
    STOP_TIMER("SHELL1QUARTET:make_ssps")
     CHECK
  end subroutine

  subroutine make_ssps_1(self,ssps)
    SHELL1QUARTET :: self
  ! Creates the (ss|ps) or (ss|sp) integrals, summed over the primitives.
    IN :: self
    REALVEC(:), OUT :: ssps
    RYS, PTR :: rys1
    REAL :: zeta,eta,einv,rho,xx,ret,normab,norm_w
    REAL :: QPx,QPy,QPz,QCx,QCy,QCz,Qx,Qy,Qz,Px,Py,Pz
    REAL :: ssps1,ssps2,ssps3
    INT :: ag,bg,cg,dg,k,j

    STACK("SHELL1QUARTET:make_ssps_1")
    START_TIMER("SHELL1QUARTET:make_ssps_1")
    call create_(rys1,1)
    ssps1=ZERO
    ssps2=ZERO
    ssps3=ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            einv = ONE/eta
            Qx = self%cd_pair_center(1,j)
            Qy = self%cd_pair_center(2,j)
            Qz = self%cd_pair_center(3,j)
            QCx = self%cd_center_diff(1,j)
            QCy = self%cd_center_diff(2,j)
            QCz = self%cd_center_diff(3,j)
            rho  = zeta * eta / (zeta + eta)
            QPx  = Qx - Px;   QPy  = Qy - Py;   QPz  = Qz - Pz
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            ret = rys1%r(1) * rho * einv
            norm_w = rys1%w(1) * normab * self%cd_cc_prefactor(j) * sqrt(rho)
            ssps1 = ssps1 + norm_w * (QCx - ret * QPx)
            ssps2 = ssps2 + norm_w * (QCy - ret * QPy)
            ssps3 = ssps3 + norm_w * (QCz - ret * QPz)
          end do
        end do
      end do
    end do
    call destroy_(rys1)
    ssps(1) = ssps1
    ssps(2) = ssps2
    ssps(3) = ssps3
    STOP_TIMER("SHELL1QUARTET:make_ssps_1")
     CHECK
  end subroutine

  subroutine make_ssss(self,abcd)
    SHELL1QUARTET :: self
  ! Creates the (ss|ss) integrals, summed over the primitives.
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REAL :: ssss
    STACK("SHELL1QUARTET:make_ssss")
    START_TIMER("SHELL1QUARTET:make_ssss")
    call make_ssss_(self,ssss)
    abcd(1,1,1,1) = ssss
    STOP_TIMER("SHELL1QUARTET:make_ssss")
     CHECK
  end subroutine

  subroutine make_ssss_1(self,ssss)
    SHELL1QUARTET :: self
  ! Creates the (ss|ss) integrals, summed over the primitives.
    IN :: self
    REAL, OUT :: ssss
    RYS, PTR :: rys1
    REAL :: zeta,eta,rho,xx,normab,QPx,QPy,QPz,Px,Py,Pz
    INT :: ag,bg,cg,dg,j,k

    STACK("SHELL1QUARTET:make_ssss_1")
    START_TIMER("SHELL1QUARTET:make_ssss_1")
    call create_(rys1,1)
    ssss = ZERO
    k = 0
    do bg = 1, self%b%n_cc
      do ag = 1, self%a%n_cc
        k = k + 1
        zeta = self%ab_exponent_sum(k)
        normab = self%ab_cc_prefactor(k) * TWOPI5ON2
        Px = self%ab_pair_center(1,k)
        Py = self%ab_pair_center(2,k)
        Pz = self%ab_pair_center(3,k)
        j = 0
        do dg = 1, self%d%n_cc
          do cg = 1, self%c%n_cc
            j = j + 1
            eta  = self%cd_exponent_sum(j)
            QPx = self%cd_pair_center(1,j) - Px
            QPy = self%cd_pair_center(2,j) - Py
            QPz = self%cd_pair_center(3,j) - Pz
            rho  = zeta * eta / (zeta + eta)
            xx   = rho * (QPx*QPx + QPy*QPy + QPz*QPz)
            call get_weights_(rys1,xx)
            ssss = ssss + normab * self%cd_cc_prefactor(j) * sqrt(rho) * rys1%w(1)
          end do
        end do
      end do
    end do
    call destroy_(rys1)
    STOP_TIMER("SHELL1QUARTET:make_ssss_1")
     CHECK
  end subroutine

  subroutine transfer_cd(self,esfs,escd)
    SHELL1QUARTET :: self
  ! Applies the transfer equation to (es|fs) to give (es|cd)
    IN :: self
    REALMAT(:,:), IN :: esfs
    REALMAT3(:,:,:), OUT :: escd
    STACK("SHELL1QUARTET:transfer_cd")
    START_TIMER("SHELL1QUARTET:transfer_cd")
    if (self%c%l > self%d%l) then
      call transfer_l_c_highest_(self,esfs,escd)
    else
      call transfer_l_d_highest_(self,esfs,escd)
    end if
    STOP_TIMER("SHELL1QUARTET:transfer_cd")
     CHECK
  end subroutine

  subroutine transfer_ab(self,escd,abcd)
    SHELL1QUARTET :: self
  ! Applies the transfer equation to (es|cd) to give (ab|cd)
    IN :: self
    REALMAT3(:,:,:), IN :: escd
    REALMAT4(:,:,:,:), OUT :: abcd
    STACK("SHELL1QUARTET:transfer_ab")
    START_TIMER("SHELL1QUARTET:transfer_ab")
    if (self%a%l > self%b%l) then
      call transfer_l_a_highest_(self,escd,abcd)
    else
      call transfer_l_b_highest_(self,escd,abcd)
    end if
    STOP_TIMER("SHELL1QUARTET:transfer_ab")
     CHECK
  end subroutine

  subroutine transfer_ab_1(self,escd,abcd)
    SHELL1QUARTET :: self
  ! Applies the transfer equation to (es|cd) to give (ab|cd)
    IN :: self
    REALMAT(:,:), IN :: escd
    REALMAT3(:,:,:), OUT :: abcd
    STACK("SHELL1QUARTET:transfer_ab_1")
    START_TIMER("SHELL1QUARTET:transfer_ab_1")
    if (self%a%l > self%b%l) then
      call transfer_l_a_highest_(self,escd,abcd)
    else
      call transfer_l_b_highest_(self,escd,abcd)
    end if
    STOP_TIMER("SHELL1QUARTET:transfer_ab_1")
     CHECK
  end subroutine

   subroutine transfer_l_c_highest(self,esfs,escd)
    SHELL1QUARTET :: self
   ! Applies the transfer equation to (es|fs) to give (es|cd)
     IN :: self
     REALMAT(:,:), IN :: esfs
     REALMAT3(:,:,:), OUT :: escd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components_c,components_d
     INTMAT3(:,:,:), PTR :: index_c,index_d
     INTVEC(:), PTR :: component_to_use
     REALVEC(3) :: CD
     INT :: c,d,c1,c2,c3,d1,ld,cub,dub
     INT :: cx,cy,cz,dx,dy,dz,j,clb,dlb,e,eub
     REAL :: CDi,CDx,CDy,CDz,esfs_ec

     STACK("SHELL1QUARTET:transfer_l_c_highest")
     START_TIMER("SHELL1QUARTET:transfer_l_c_highest")
     select case (self%d%l)
       case (0)
         escd(:,:,1)=esfs

       case (1)
         if (self%c%l==1) then
           eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
           CD   = self%pos_c - self%pos_d
           CDx=CD(1); CDy=CD(2); CDz=CD(3)
           do e = 1, eub
             esfs_ec = esfs(e,1)
             escd(e,1,1)=esfs(e,4)+CDx*esfs_ec
             escd(e,1,2)=esfs(e,7)+CDy*esfs_ec
             escd(e,1,3)=esfs(e,8)+CDz*esfs_ec
             esfs_ec = esfs(e,2)
             escd(e,2,1)=esfs(e,7)+CDx*esfs_ec
             escd(e,2,2)=esfs(e,5)+CDy*esfs_ec
             escd(e,2,3)=esfs(e,9)+CDz*esfs_ec
             esfs_ec = esfs(e,3)
             escd(e,3,1)=esfs(e,8)+CDx*esfs_ec
             escd(e,3,2)=esfs(e,9)+CDy*esfs_ec
             escd(e,3,3)=esfs(e,6)+CDz*esfs_ec
           end do
         else
           clb = n_comp_sum_((self%c%l-1))
           eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
           CD   = self%pos_c - self%pos_d
           cub  = self%c%n_comp
           components_c => self%cd_hrr_components(:,clb+1:)
           index_c => self%cd_hrr_index_larger
           CDx = CD(1); CDy = CD(2); CDz = CD(3)
           do c = 1, cub
             cx = components_c(1,c)
             cy = components_c(2,c)
             cz = components_c(3,c)
             c1 = index_c(cx+1,cy,cz)
             c2 = index_c(cx,cy+1,cz)
             c3 = index_c(cx,cy,cz+1)
             do e = 1, eub
               esfs_ec = esfs(e,c)
               escd(e,c,1) = esfs(e,c1) + CDx * esfs_ec
               escd(e,c,2) = esfs(e,c2) + CDy * esfs_ec
               escd(e,c,3) = esfs(e,c3) + CDz * esfs_ec
             end do
           end do
         end if

       case default
         clb  = n_comp_sum_((self%c%l-1))
         eub  = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
         CD   = self%pos_c - self%pos_d
         cub  = n_comp_sum_((self%cd_l_sum-1)) - clb

         index_c => self%cd_hrr_index_larger
         index_d => self%cd_hrr_index_smaller
         components_c => self%cd_hrr_components(:,clb+1:)

         nullify(int_new)
         call create_(int_new,eub,cub,3)

         CDx = CD(1); CDy = CD(2); CDz = CD(3)
         do c = 1, cub
           cx = components_c(1,c)
           cy = components_c(2,c)
           cz = components_c(3,c)
           c1 = index_c(cx+1,cy,cz)
           c2 = index_c(cx,cy+1,cz)
           c3 = index_c(cx,cy,cz+1)
           do e = 1, eub
             esfs_ec = esfs(e,c)
             int_new(e,c,1) = esfs(e,c1) + CDx * esfs_ec
             int_new(e,c,2) = esfs(e,c2) + CDy * esfs_ec
             int_new(e,c,3) = esfs(e,c3) + CDz * esfs_ec
           end do
         end do

         do ld=2, self%d%l - 1
           dlb              = n_comp_sum_((ld-1))
           dub              = n_comp_(ld)
           cub              = n_comp_sum_((self%cd_l_sum-ld)) - clb
           component_to_use => self%cd_hrr_comp_to_use(dlb+1:dlb+dub)
           components_d     => self%cd_hrr_components(:,dlb+1:dlb+dub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,eub,cub,dub)
           do d=1,dub
             dx = components_d(1,d)
             dy = components_d(2,d)
             dz = components_d(3,d)
             j=component_to_use(d)
             call subtract_from_component_(self,dx,dy,dz,j)
             d1 = index_d(dx,dy,dz)
             CDi=CD(j)
             do c=1,cub
               cx = components_c(1,c)
               cy = components_c(2,c)
               cz = components_c(3,c)
               call add_to_component_(self,cx,cy,cz,j)
               c1 = index_c(cx,cy,cz)
               int_new(:,c,d)=int_old(:,c1,d1)+CDi*int_old(:,c,d1)
             end do
           end do
           call destroy_(int_old)
         end do

         dlb              = n_comp_sum_((self%d%l-1))
         dub              = self%d%n_comp
         cub              = self%c%n_comp
         component_to_use => self%cd_hrr_comp_to_use(dlb+1:dlb+dub)
         components_d     => self%cd_hrr_components(:,dlb+1:dlb+dub)
         int_old          => int_new
         do d=1,dub
           dx = components_d(1,d)
           dy = components_d(2,d)
           dz = components_d(3,d)
           j=component_to_use(d)
           call subtract_from_component_(self,dx,dy,dz,j)
           d1 = index_d(dx,dy,dz)
           CDi=CD(j)
           do c=1,cub
             cx = components_c(1,c)
             cy = components_c(2,c)
             cz = components_c(3,c)
             call add_to_component_(self,cx,cy,cz,j)
             c1 = index_c(cx,cy,cz)
             escd(:,c,d)=int_old(:,c1,d1)+CDi*int_old(:,c,d1)
           end do
         end do
         call destroy_(int_old)
     end select
     STOP_TIMER("SHELL1QUARTET:transfer_l_c_highest")
      CHECK
   end subroutine

   subroutine transfer_l_d_highest(self,esfs,escd)
    SHELL1QUARTET :: self
   ! Applies the transfer equation to (es|fs) to give (es|cd)
     IN :: self
     REALMAT(:,:), IN :: esfs
     REALMAT3(:,:,:), OUT :: escd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components_c,components_d
     INTMAT3(:,:,:), PTR :: index_c,index_d
     INTVEC(:), PTR :: component_to_use
     REALVEC(3) :: DC
     INT :: c,d,c1,d1,d2,d3,lc,cub,dub
     INT :: cx,cy,cz,dx,dy,dz,j,clb,dlb,e,eub
     REAL :: DCi,DCx,DCy,DCz,esfs_ed

     STACK("SHELL1QUARTET:transfer_l_d_highest")
     START_TIMER("SHELL1QUARTET:transfer_l_d_highest")
     select case (self%c%l)
       case (0)
         escd(:,1,:)=esfs

       case (1)
         if (self%d%l==1) then
           eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
           DC   = self%pos_d - self%pos_c
           DCx=DC(1); DCy=DC(2); DCz=DC(3)
           do e = 1, eub
             esfs_ed = esfs(e,1)
             escd(e,1,1)=esfs(e,4)+DCx*esfs_ed
             escd(e,2,1)=esfs(e,7)+DCy*esfs_ed
             escd(e,3,1)=esfs(e,8)+DCz*esfs_ed
             esfs_ed = esfs(e,2)
             escd(e,1,2)=esfs(e,7)+DCx*esfs_ed
             escd(e,2,2)=esfs(e,5)+DCy*esfs_ed
             escd(e,3,2)=esfs(e,9)+DCz*esfs_ed
             esfs_ed = esfs(e,3)
             escd(e,1,3)=esfs(e,8)+DCx*esfs_ed
             escd(e,2,3)=esfs(e,9)+DCy*esfs_ed
             escd(e,3,3)=esfs(e,6)+DCz*esfs_ed
           end do
         else
           dlb = n_comp_sum_((self%d%l-1))
           eub = n_comp_sum_(self%ab_l_sum) - n_comp_sum_((self%ab_l_max-1))
           DC   = self%pos_d - self%pos_c
           dub  = self%d%n_comp
           components_d => self%cd_hrr_components(:,dlb+1:)
           index_d => self%cd_hrr_index_larger
           DCx=DC(1); DCy=DC(2); DCz=DC(3)
           do d=1,dub
             dx = components_d(1,d)
             dy = components_d(2,d)
             dz = components_d(3,d)
             d1 = index_d(dx+1,dy,dz)
             d2 = index_d(dx,dy+1,dz)
             d3 = index_d(dx,dy,dz+1)
             do e = 1, eub
               esfs_ed = esfs(e,d)
               escd(e,1,d)=esfs(e,d1)+DCx*esfs_ed
               escd(e,2,d)=esfs(e,d2)+DCy*esfs_ed
               escd(e,3,d)=esfs(e,d3)+DCz*esfs_ed
             end do
           end do
         end if

       case default
         dlb = n_comp_sum_((self%d%l-1))
         eub = n_comp_sum_((self%ab_l_sum)) - n_comp_sum_((self%ab_l_max-1))
         DC   = self%pos_d - self%pos_c
         dub  = n_comp_sum_((self%cd_l_sum-1)) - dlb

         index_d => self%cd_hrr_index_larger
         index_c => self%cd_hrr_index_smaller
         components_d => self%cd_hrr_components(:,dlb+1:)

         nullify(int_new)
         call create_(int_new,eub,dub,3)
         DCx=DC(1); DCy=DC(2); DCz=DC(3)
         do d=1,dub
           dx = components_d(1,d)
           dy = components_d(2,d)
           dz = components_d(3,d)
           d1 = index_d(dx+1,dy,dz)
           d2 = index_d(dx,dy+1,dz)
           d3 = index_d(dx,dy,dz+1)
           do e = 1, eub
             esfs_ed = esfs(e,d)
             int_new(e,d,1)=esfs(e,d1)+DCx*esfs_ed
             int_new(e,d,2)=esfs(e,d2)+DCy*esfs_ed
             int_new(e,d,3)=esfs(e,d3)+DCz*esfs_ed
           end do
         end do
         do lc=2, self%c%l - 1
           clb              = n_comp_sum_((lc-1))
           cub              = n_comp_(lc)
           dub              = n_comp_sum_((self%cd_l_sum-lc)) - dlb
           component_to_use => self%cd_hrr_comp_to_use(clb+1:clb+cub)
           components_c     => self%cd_hrr_components(:,clb+1:clb+cub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,eub,dub,cub)
           do c=1,cub
             cx = components_c(1,c)
             cy = components_c(2,c)
             cz = components_c(3,c)
             j=component_to_use(c)
             call subtract_from_component_(self,cx,cy,cz,j)
             c1 = index_c(cx,cy,cz)
             DCi=DC(j)
             do d=1,dub
               dx = components_d(1,d)
               dy = components_d(2,d)
               dz = components_d(3,d)
               call add_to_component_(self,dx,dy,dz,j)
               d1 = index_d(dx,dy,dz)
               int_new(:,d,c)=int_old(:,d1,c1)+DCi*int_old(:,d,c1)
             end do
           end do
           call destroy_(int_old)
         end do
         clb              = n_comp_sum_((self%c%l-1))
         cub              = self%c%n_comp
         dub              = self%d%n_comp
         component_to_use => self%cd_hrr_comp_to_use(clb+1:clb+cub)
         components_c     => self%cd_hrr_components(:,clb+1:clb+cub)
         int_old          => int_new
         do c=1,cub
           cx = components_c(1,c)
           cy = components_c(2,c)
           cz = components_c(3,c)
           j=component_to_use(c)
           call subtract_from_component_(self,cx,cy,cz,j)
           c1 = index_c(cx,cy,cz)
           DCi=DC(j)
           do d=1,dub
             dx = components_d(1,d)
             dy = components_d(2,d)
             dz = components_d(3,d)
             call add_to_component_(self,dx,dy,dz,j)
             d1 = index_d(dx,dy,dz)
             escd(:,c,d)=int_old(:,d1,c1)+DCi*int_old(:,d,c1)
           end do
         end do
         call destroy_(int_old)
     end select
     STOP_TIMER("SHELL1QUARTET:transfer_l_d_highest")
      CHECK
   end subroutine

   subroutine transfer_l_a_highest(self,escd,abcd)
    SHELL1QUARTET :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT3(:,:,:), IN :: escd
     REALMAT4(:,:,:,:), OUT :: abcd
     REALMAT4(:,:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     INTVEC(:), PTR :: component_to_use
     REALVEC(3) :: AB
     INT :: a,b,c,d,a1,a2,a3,b1,lb,aub,bub,cub,dub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb
     REAL :: ABi,ABx,ABy,ABz,escd_acd

     STACK("SHELL1QUARTET:transfer_l_a_highest")
     START_TIMER("SHELL1QUARTET:transfer_l_a_highest")
     select case (self%b%l)
       case (0)
         abcd(:,1,:,:)=escd

       case (1)
         if (self%a%l==1) then
           AB   = self%pos_b - self%pos_a
           ABx = AB(1);    ABy = AB(2);    ABz = AB(3)
           cub  = self%c%n_comp
           dub  = self%d%n_comp
           do d = 1, dub
             do c = 1, cub
               escd_acd = escd(1,c,d)
               abcd(1,1,c,d) = escd(4,c,d) + ABx * escd_acd
               abcd(1,2,c,d) = escd(7,c,d) + ABy * escd_acd
               abcd(1,3,c,d) = escd(8,c,d) + ABz * escd_acd
               escd_acd = escd(2,c,d)
               abcd(2,1,c,d) = escd(7,c,d) + ABx * escd_acd
               abcd(2,2,c,d) = escd(5,c,d) + ABy * escd_acd
               abcd(2,3,c,d) = escd(9,c,d) + ABz * escd_acd
               escd_acd = escd(3,c,d)
               abcd(3,1,c,d) = escd(8,c,d) + ABx * escd_acd
               abcd(3,2,c,d) = escd(9,c,d) + ABy * escd_acd
               abcd(3,3,c,d) = escd(6,c,d) + ABz * escd_acd
             end do
           end do
         else
           alb = n_comp_sum_((self%a%l-1))
           AB   = self%pos_a - self%pos_b
           aub  = self%a%n_comp
           cub  = self%c%n_comp
           dub  = self%d%n_comp
           components_a => self%ab_hrr_components(:,alb+1:)
           index_a => self%ab_hrr_index_larger
           ABx=AB(1); ABy=AB(2); ABz=AB(3)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             a1 = index_a(ax+1,ay,az)
             a2 = index_a(ax,ay+1,az)
             a3 = index_a(ax,ay,az+1)
             do d=1,dub
               do c=1,cub
                 escd_acd = escd(a,c,d)
                 abcd(a,1,c,d)=escd(a1,c,d) + ABx * escd_acd
                 abcd(a,2,c,d)=escd(a2,c,d) + ABy * escd_acd
                 abcd(a,3,c,d)=escd(a3,c,d) + ABz * escd_acd
               end do
             end do
           end do
         end if

       case default
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%pos_a - self%pos_b
         aub  = n_comp_sum_((self%ab_l_sum-1)) - alb
         bub  = self%b%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         index_a => self%ab_hrr_index_larger
         index_b => self%ab_hrr_index_smaller
         components_a => self%ab_hrr_components(:,alb+1:)

         nullify(int_new)
         call create_(int_new,cub,dub,bub,aub)
         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           a1 = index_a(ax+1,ay,az)
           a2 = index_a(ax,ay+1,az)
           a3 = index_a(ax,ay,az+1)
           do d=1,dub
             do c=1,cub
               escd_acd = escd(a,c,d)
               int_new(c,d,1,a)=escd(a1,c,d) + ABx * escd_acd
               int_new(c,d,2,a)=escd(a2,c,d) + ABy * escd_acd
               int_new(c,d,3,a)=escd(a3,c,d) + ABz * escd_acd
             end do
           end do
         end do

         do lb=2, self%b%l - 1
           blb              = n_comp_sum_((lb-1))
           bub              = n_comp_(lb)
           aub              = n_comp_sum_((self%ab_l_sum-lb)) - alb
           component_to_use => self%ab_hrr_comp_to_use(blb+1:blb+bub)
           components_b     => self%ab_hrr_components(:,blb+1:blb+bub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,cub,dub,bub,aub)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             j = component_to_use(b)
             call subtract_from_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             ABi=AB(j)
             do a=1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               call add_to_component_(self,ax,ay,az,j)
               a1 = index_a(ax,ay,az)
               int_new(:,:,b,a)=int_old(:,:,b1,a1) + ABi * int_old(:,:,b1,a)
             end do
           end do
           call destroy_(int_old)
         end do

         blb              = n_comp_sum_((self%b%l-1))
         bub              = self%b%n_comp
         aub              = self%a%n_comp
         component_to_use => self%ab_hrr_comp_to_use(blb+1:blb+bub)
         components_b     => self%ab_hrr_components(:,blb+1:blb+bub)
         int_old          => int_new
         do b=1,bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           j = component_to_use(b)
           call subtract_from_component_(self,bx,by,bz,j)
           b1 = index_b(bx,by,bz)
           ABi=AB(j)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             call add_to_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             abcd(a,b,:,:)=int_old(:,:,b1,a1) + ABi * int_old(:,:,b1,a)
           end do
         end do
         call destroy_(int_old)
     end select
     STOP_TIMER("SHELL1QUARTET:transfer_l_a_highest")
      CHECK
   end subroutine

   subroutine transfer_l_b_highest(self,escd,abcd)
    SHELL1QUARTET :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT3(:,:,:), IN :: escd
     REALMAT4(:,:,:,:), OUT :: abcd
     REALMAT4(:,:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     INTVEC(:), PTR :: component_to_use
     REALVEC(3) :: BA
     INT :: a,b,c,d,a1,b1,b2,b3,la,aub,bub,cub,dub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb
     REAL :: BAi,BAx,BAy,BAz,escd_bcd

     STACK("SHELL1QUARTET:transfer_l_b_highest")
     START_TIMER("SHELL1QUARTET:transfer_l_b_highest")
     select case (self%a%l)
       case (0)
         abcd(1,:,:,:)=escd

       case (1)
         if (self%b%l==1) then
           BA   = self%pos_b - self%pos_a
           BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
           cub  = self%c%n_comp
           dub  = self%d%n_comp
           do d = 1, dub
             do c = 1, cub
               escd_bcd = escd(1,c,d)
               abcd(1,1,c,d) = escd(4,c,d) + BAx * escd_bcd
               abcd(2,1,c,d) = escd(7,c,d) + BAy * escd_bcd
               abcd(3,1,c,d) = escd(8,c,d) + BAz * escd_bcd
               escd_bcd = escd(2,c,d)
               abcd(1,2,c,d) = escd(7,c,d) + BAx * escd_bcd
               abcd(2,2,c,d) = escd(5,c,d) + BAy * escd_bcd
               abcd(3,2,c,d) = escd(9,c,d) + BAz * escd_bcd
               escd_bcd = escd(3,c,d)
               abcd(1,3,c,d) = escd(8,c,d) + BAx * escd_bcd
               abcd(2,3,c,d) = escd(9,c,d) + BAy * escd_bcd
               abcd(3,3,c,d) = escd(6,c,d) + BAz * escd_bcd
             end do
           end do
         else
           blb = n_comp_sum_((self%b%l-1))
           BA   = self%pos_b - self%pos_a
           bub  = self%b%n_comp
           cub  = self%c%n_comp
           dub  = self%d%n_comp
           components_b => self%ab_hrr_components(:,blb+1:)
           index_b => self%ab_hrr_index_larger
           BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
           do b = 1, bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             b1 = index_b(bx+1,by,bz)
             b2 = index_b(bx,by+1,bz)
             b3 = index_b(bx,by,bz+1)
             do d = 1, dub
               do c = 1, cub
                 escd_bcd = escd(b,c,d)
                 abcd(1,b,c,d) = escd(b1,c,d) + BAx * escd_bcd
                 abcd(2,b,c,d) = escd(b2,c,d) + BAy * escd_bcd
                 abcd(3,b,c,d) = escd(b3,c,d) + BAz * escd_bcd
               end do
             end do
           end do
         end if

       case default
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%pos_b - self%pos_a
         bub  = n_comp_sum_((self%ab_l_sum-1)) - blb
         aub  = self%a%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         index_b => self%ab_hrr_index_larger
         index_a => self%ab_hrr_index_smaller
         components_b => self%ab_hrr_components(:,blb+1:)

         nullify(int_new)
         call create_(int_new,cub,dub,bub,aub)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           b1 = index_b(bx+1,by,bz)
           b2 = index_b(bx,by+1,bz)
           b3 = index_b(bx,by,bz+1)
           do c = 1, cub
             do d = 1, dub
               escd_bcd = escd(b,c,d)
               int_new(c,d,b,1) = escd(b1,c,d) + BAx * escd_bcd
               int_new(c,d,b,2) = escd(b2,c,d) + BAy * escd_bcd
               int_new(c,d,b,3) = escd(b3,c,d) + BAz * escd_bcd
             end do
           end do
         end do

         do la=2, self%a%l - 1
           alb              = n_comp_sum_((la-1))
           aub              = n_comp_(la)
           bub              = n_comp_sum_((self%ab_l_sum-la)) - blb
           component_to_use => self%ab_hrr_comp_to_use(alb+1:alb+aub)
           components_a     => self%ab_hrr_components(:,alb+1:alb+aub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,cub,dub,bub,aub)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             j=component_to_use(a)
             call subtract_from_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             BAi=BA(j)
             do b=1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               call add_to_component_(self,bx,by,bz,j)
               b1 = index_b(bx,by,bz)
               int_new(:,:,b,a)=int_old(:,:,b1,a1) + BAi * int_old(:,:,b,a1)
             end do
           end do
           call destroy_(int_old)
         end do

         alb              = n_comp_sum_((self%a%l-1))
         aub              = self%a%n_comp
         bub              = self%b%n_comp
         component_to_use => self%ab_hrr_comp_to_use(alb+1:alb+aub)
         components_a     => self%ab_hrr_components(:,alb+1:alb+aub)
         int_old          => int_new
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           j = component_to_use(a)
           call subtract_from_component_(self,ax,ay,az,j)
           a1 = index_a(ax,ay,az)
           BAi=BA(j)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             call add_to_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             abcd(a,b,:,:)=int_old(:,:,b1,a1) + BAi * int_old(:,:,b,a1)
           end do
         end do
         call destroy_(int_old)
     end select
     STOP_TIMER("SHELL1QUARTET:transfer_l_b_highest")
      CHECK
   end subroutine

   subroutine transfer_l_a_highest_1(self,escd,abcd)
    SHELL1QUARTET :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT(:,:), IN :: escd
     REALMAT3(:,:,:), OUT :: abcd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     INTVEC(:), PTR :: component_to_use
     REALVEC(3) :: AB
     INT :: a,b,f,a1,a2,a3,b1,lb,aub,bub,fub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb
     REAL :: ABi,ABx,ABy,ABz,escd_acd

     STACK("SHELL1QUARTET:transfer_l_a_highest_1")
     START_TIMER("SHELL1QUARTET:transfer_l_a_highest_1")
     select case (self%b%l)
       case (0)
         abcd(:,1,:)=escd

       case (1)
         if (self%a%l==1) then
           AB   = self%pos_b - self%pos_a
           ABx = AB(1);    ABy = AB(2);    ABz = AB(3)
           fub  = size(escd,2)
           do f = 1, fub
             escd_acd = escd(1,f)
             abcd(1,1,f) = escd(4,f) + ABx * escd_acd
             abcd(1,2,f) = escd(7,f) + ABy * escd_acd
             abcd(1,3,f) = escd(8,f) + ABz * escd_acd
             escd_acd = escd(2,f)
             abcd(2,1,f) = escd(7,f) + ABx * escd_acd
             abcd(2,2,f) = escd(5,f) + ABy * escd_acd
             abcd(2,3,f) = escd(9,f) + ABz * escd_acd
             escd_acd = escd(3,f)
             abcd(3,1,f) = escd(8,f) + ABx * escd_acd
             abcd(3,2,f) = escd(9,f) + ABy * escd_acd
             abcd(3,3,f) = escd(6,f) + ABz * escd_acd
           end do
         else
           alb = n_comp_sum_((self%a%l-1))
           AB   = self%pos_a - self%pos_b
           aub  = self%a%n_comp
           fub  = size(escd,2)
           components_a => self%ab_hrr_components(:,alb+1:)
           index_a => self%ab_hrr_index_larger
           ABx=AB(1); ABy=AB(2); ABz=AB(3)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             a1 = index_a(ax+1,ay,az)
             a2 = index_a(ax,ay+1,az)
             a3 = index_a(ax,ay,az+1)
             do f=1,fub
               escd_acd = escd(a,f)
               abcd(a,1,f)=escd(a1,f) + ABx * escd_acd
               abcd(a,2,f)=escd(a2,f) + ABy * escd_acd
               abcd(a,3,f)=escd(a3,f) + ABz * escd_acd
             end do
           end do
         end if

       case default
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%pos_a - self%pos_b
         aub  = n_comp_sum_((self%ab_l_sum-1)) - alb
         bub  = self%b%n_comp
         fub  = size(escd,2)

         index_a => self%ab_hrr_index_larger
         index_b => self%ab_hrr_index_smaller
         components_a => self%ab_hrr_components(:,alb+1:)

         nullify(int_new)
         call create_(int_new,fub,bub,aub)
         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           a1 = index_a(ax+1,ay,az)
           a2 = index_a(ax,ay+1,az)
           a3 = index_a(ax,ay,az+1)
           do f=1,fub
             escd_acd = escd(a,f)
             int_new(f,1,a)=escd(a1,f) + ABx * escd_acd
             int_new(f,2,a)=escd(a2,f) + ABy * escd_acd
             int_new(f,3,a)=escd(a3,f) + ABz * escd_acd
           end do
         end do

         do lb=2, self%b%l - 1
           blb              = n_comp_sum_((lb-1))
           bub              = n_comp_(lb)
           aub              = n_comp_sum_((self%ab_l_sum-lb)) - alb
           component_to_use => self%ab_hrr_comp_to_use(blb+1:blb+bub)
           components_b     => self%ab_hrr_components(:,blb+1:blb+bub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,fub,bub,aub)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             j = component_to_use(b)
             call subtract_from_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             ABi=AB(j)
             do a=1,aub
               ax = components_a(1,a)
               ay = components_a(2,a)
               az = components_a(3,a)
               call add_to_component_(self,ax,ay,az,j)
               a1 = index_a(ax,ay,az)
               int_new(:,b,a)=int_old(:,b1,a1) + ABi * int_old(:,b1,a)
             end do
           end do
           call destroy_(int_old)
         end do

         blb              = n_comp_sum_((self%b%l-1))
         bub              = self%b%n_comp
         aub              = self%a%n_comp
         component_to_use => self%ab_hrr_comp_to_use(blb+1:blb+bub)
         components_b     => self%ab_hrr_components(:,blb+1:blb+bub)
         int_old          => int_new
         do b=1,bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           j = component_to_use(b)
           call subtract_from_component_(self,bx,by,bz,j)
           b1 = index_b(bx,by,bz)
           ABi=AB(j)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             call add_to_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             do f=1,fub
               abcd(a,b,f)=int_old(f,b1,a1) + ABi * int_old(f,b1,a)
             end do
           end do
         end do
         call destroy_(int_old)
     end select
     STOP_TIMER("SHELL1QUARTET:transfer_l_a_highest_1")
      CHECK
   end subroutine

   subroutine transfer_l_b_highest_1(self,escd,abcd)
    SHELL1QUARTET :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT(:,:), IN :: escd
     REALMAT3(:,:,:), OUT :: abcd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     INTVEC(:), PTR :: component_to_use
     REALVEC(3) :: BA
     INT :: a,b,f,a1,b1,b2,b3,la,aub,bub,fub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb
     REAL :: BAi,BAx,BAy,BAz,escd_bcd

     STACK("SHELL1QUARTET:transfer_l_b_highest_1")
     START_TIMER("SHELL1QUARTET:transfer_l_b_highest_1")
     select case (self%a%l)
       case (0)
         abcd(1,:,:)=escd

       case (1)
         if (self%b%l==1) then
           BA   = self%pos_b - self%pos_a
           BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
           fub  = size(escd,2)
           do f = 1, fub
             escd_bcd = escd(1,f)
             abcd(1,1,f) = escd(4,f) + BAx * escd_bcd
             abcd(2,1,f) = escd(7,f) + BAy * escd_bcd
             abcd(3,1,f) = escd(8,f) + BAz * escd_bcd
             escd_bcd = escd(2,f)
             abcd(1,2,f) = escd(7,f) + BAx * escd_bcd
             abcd(2,2,f) = escd(5,f) + BAy * escd_bcd
             abcd(3,2,f) = escd(9,f) + BAz * escd_bcd
             escd_bcd = escd(3,f)
             abcd(1,3,f) = escd(8,f) + BAx * escd_bcd
             abcd(2,3,f) = escd(9,f) + BAy * escd_bcd
             abcd(3,3,f) = escd(6,f) + BAz * escd_bcd
           end do
         else
           blb = n_comp_sum_((self%b%l-1))
           BA   = self%pos_b - self%pos_a
           bub  = self%b%n_comp
           fub  = size(escd,2)
           components_b => self%ab_hrr_components(:,blb+1:)
           index_b => self%ab_hrr_index_larger
           BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
           do b = 1, bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             b1 = index_b(bx+1,by,bz)
             b2 = index_b(bx,by+1,bz)
             b3 = index_b(bx,by,bz+1)
             do f = 1, fub
               escd_bcd = escd(b,f)
               abcd(1,b,f) = escd(b1,f) + BAx * escd_bcd
               abcd(2,b,f) = escd(b2,f) + BAy * escd_bcd
               abcd(3,b,f) = escd(b3,f) + BAz * escd_bcd
             end do
           end do
         end if

       case default
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%pos_b - self%pos_a
         bub  = n_comp_sum_((self%ab_l_sum-1)) - blb
         aub  = self%a%n_comp
         fub  = size(escd,2)

         index_b => self%ab_hrr_index_larger
         index_a => self%ab_hrr_index_smaller
         components_b => self%ab_hrr_components(:,blb+1:)

         nullify(int_new)
         call create_(int_new,fub,bub,aub)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components_b(1,b)
           by = components_b(2,b)
           bz = components_b(3,b)
           b1 = index_b(bx+1,by,bz)
           b2 = index_b(bx,by+1,bz)
           b3 = index_b(bx,by,bz+1)
           do f = 1, fub
             escd_bcd = escd(b,f)
             int_new(f,b,1) = escd(b1,f) + BAx * escd_bcd
             int_new(f,b,2) = escd(b2,f) + BAy * escd_bcd
             int_new(f,b,3) = escd(b3,f) + BAz * escd_bcd
           end do
         end do

         do la=2, self%a%l - 1
           alb              = n_comp_sum_((la-1))
           aub              = n_comp_(la)
           bub              = n_comp_sum_((self%ab_l_sum-la)) - blb
           component_to_use => self%ab_hrr_comp_to_use(alb+1:alb+aub)
           components_a     => self%ab_hrr_components(:,alb+1:alb+aub)
           int_old          => int_new
           nullify(int_new)
           call create_(int_new,fub,bub,aub)
           do a=1,aub
             ax = components_a(1,a)
             ay = components_a(2,a)
             az = components_a(3,a)
             j=component_to_use(a)
             call subtract_from_component_(self,ax,ay,az,j)
             a1 = index_a(ax,ay,az)
             BAi=BA(j)
             do b=1,bub
               bx = components_b(1,b)
               by = components_b(2,b)
               bz = components_b(3,b)
               call add_to_component_(self,bx,by,bz,j)
               b1 = index_b(bx,by,bz)
               int_new(:,b,a)=int_old(:,b1,a1) + BAi * int_old(:,b,a1)
             end do
           end do
           call destroy_(int_old)
         end do

         alb              = n_comp_sum_((self%a%l-1))
         aub              = self%a%n_comp
         bub              = self%b%n_comp
         component_to_use => self%ab_hrr_comp_to_use(alb+1:alb+aub)
         components_a     => self%ab_hrr_components(:,alb+1:alb+aub)
         int_old          => int_new
         do a=1,aub
           ax = components_a(1,a)
           ay = components_a(2,a)
           az = components_a(3,a)
           j = component_to_use(a)
           call subtract_from_component_(self,ax,ay,az,j)
           a1 = index_a(ax,ay,az)
           BAi=BA(j)
           do b=1,bub
             bx = components_b(1,b)
             by = components_b(2,b)
             bz = components_b(3,b)
             call add_to_component_(self,bx,by,bz,j)
             b1 = index_b(bx,by,bz)
             do f=1,fub
               abcd(a,b,f)=int_old(f,b1,a1) + BAi * int_old(f,b,a1)
             end do
           end do
         end do
         call destroy_(int_old)
     end select
     STOP_TIMER("SHELL1QUARTET:transfer_l_b_highest_1")
      CHECK
   end subroutine

   ALWAYS_PURE subroutine add_to_component(self,x,y,z,j)
    SHELL1QUARTET :: self
   ! Adds one to the component specified by j.  Used by transfer equation.
   ! j=1 => x=x+1, j=2 => y=y+1, j=3 => z=z+1.
     IN :: self
     INT, INOUT :: x,y,z
     INT, IN :: j
     select case(j)
       case (1); x = x + 1
       case (2); y = y + 1
       case (3); z = z + 1
     end select
     STOP_TIMER("SHELL1QUARTET:add_to_component")
   end subroutine

   ALWAYS_PURE subroutine subtract_from_component(self,x,y,z,j)
    SHELL1QUARTET :: self
   ! Subtracts one from the component specified by j.  Used by transfer
   ! equation.  j=1 => x=x-1, j=2 => y=y-1, j=3 => z=z-1.
     IN :: self
     INT, INOUT :: x,y,z
     INT, IN :: j
     select case(j)
       case (1); x = x - 1
       case (2); y = y - 1
       case (3); z = z - 1
     end select
     STOP_TIMER("SHELL1QUARTET:subtract_from_component")
   end subroutine

 !******************************************************************************
 !                  Normalisation routines.
 !******************************************************************************

   PURE subroutine to_normalise(self,abcd)
    SHELL1QUARTET :: self
   ! Multiply the matrix by the orbital normalisation coefficients
   ! for the orbitals a, b, c and d.
     IN :: self
     REALMAT4(:,:,:,:), INOUT :: abcd
     INT :: aub,bub,cub,dub,a,b,c,d,opt,i,j
     REAL :: norm_cd

     opt = 0
     if (self%ab_l_max < 2) opt = opt + 1
     if (self%cd_l_max < 2) opt = opt + 2
     select case (opt)
       case (0)
         aub=self%a%n_comp
         bub=self%b%n_comp
         cub=self%c%n_comp
         dub=self%d%n_comp
         j = 0
         do d=1,dub
           do c=1,cub
             j = j + 1
             norm_cd=self%cd_normalising_factors(j)
             i = 0
             do b=1,bub
               do a=1,aub
                 i = i+1
                 abcd(a,b,c,d)=abcd(a,b,c,d)*norm_cd*self%ab_normalising_factors(i)
               end do
             end do
           end do
         end do

       case (1)
         cub = self%c%n_comp
         dub = self%d%n_comp
         j = 0
         do d=1,dub
           do c=1,cub
             j = j + 1
             abcd(:,:,c,d)=abcd(:,:,c,d)*self%cd_normalising_factors(j)
           end do
         end do
       case (2)
         aub = self%a%n_comp
         bub = self%b%n_comp
         cub = self%c%n_comp
         dub = self%d%n_comp
         do d=1,dub
           do c=1,cub
             i = 0
             do b=1,bub
               do a=1,aub
                 i = i + 1
                 abcd(a,b,c,d)=abcd(a,b,c,d)*self%ab_normalising_factors(i)
               end do
             end do
           end do
         end do
       case (3)
         ! do nothing
     end select
     STOP_TIMER("SHELL1QUARTET:to_normalise")
   end subroutine

   PURE subroutine to_normalise_1(self,X)
    SHELL1QUARTET :: self
   ! Multiply the matrix by the orbital normalisation coefficients for the
   ! orbitals a, b, c and d.
     IN :: self
     REALMAT5(:,:,:,:,:), INOUT :: X
     INT :: i,dim
     dim = size(X,5)
     do i = 1,dim
        call to_normalise_(self,X(:,:,:,:,i))
     end do
     STOP_TIMER("SHELL1QUARTET:to_normalise_1")
   end subroutine

   PURE subroutine to_normalise_ecd(self,ecd)
    SHELL1QUARTET :: self
   ! Multiply the matrix by the orbital normalisation coefficients for the
   ! orbitals where either a.l or b.l = 0.  This routine works on a REALMAT3.
     IN :: self
     REALMAT3(:,:,:), INOUT :: ecd
     INT :: cub,dub,c,d,opt,j

     opt = 0
     if (self%ab_l_max < 2) opt = opt + 1
     if (self%cd_l_max < 2) opt = opt + 2
     select case (opt)
       case (0)
         cub=self%c%n_comp
         dub=self%d%n_comp
         j = 0
         do d=1,dub
           do c=1,cub
             j = j + 1
             ecd(:,c,d)=ecd(:,c,d) * &
                    self%ab_normalising_factors(:)*self%cd_normalising_factors(j)
           end do
         end do
       case (1)
         cub = self%c%n_comp
         dub = self%d%n_comp
         j = 0
         do d=1,dub
           do c=1,cub
             j = j + 1
             ecd(:,c,d)=ecd(:,c,d)* self%cd_normalising_factors(j)
           end do
         end do
       case (2)
         cub = self%c%n_comp
         dub = self%d%n_comp
         do d=1,dub
           do c=1,cub
             ecd(:,c,d)=ecd(:,c,d) * self%ab_normalising_factors(:)
           end do
         end do
       case (3)
         ! do nothing
     end select
     STOP_TIMER("SHELL1QUARTET:to_normalise_ecd")
   end subroutine

   PURE subroutine to_normalise_abf(self,abf)
    SHELL1QUARTET :: self
   ! Multiply the matrix by the orbital normalisation coefficients for the
   ! orbitals where either c.l or d.l = 0.  This routine works on a REALMAT3.
     IN :: self
     REALMAT3(:,:,:), INOUT :: abf
     INT :: aub,bub,fub,a,b,f,opt,i
     REAL :: normf

     opt = 0
     if (self%cd_l_max < 2) opt = opt + 1
     if (self%ab_l_max < 2) opt = opt + 2
     select case (opt)
       case (0)
         aub = self%a%n_comp
         bub = self%b%n_comp
         fub = max(self%c%n_comp,self%d%n_comp)
         do f=1,fub
           normf=self%cd_normalising_factors(f)
           i = 0
           do b=1,bub
             do a=1,aub
               i = i + 1
               abf(a,b,f)=abf(a,b,f) * self%ab_normalising_factors(i)*normf
             end do
           end do
         end do
       case (1)
         aub = self%a%n_comp
         bub = self%b%n_comp
         fub = max(self%c%n_comp,self%d%n_comp)
         do f=1,fub
           i = 0
           do b=1,bub
             do a=1,aub
               i = i + 1
               abf(a,b,f)=abf(a,b,f) * self%ab_normalising_factors(i)
             end do
           end do
         end do
       case (2)
         fub = max(self%c%n_comp,self%d%n_comp)
         do f=1,fub
           abf(:,:,f)=abf(:,:,f) * self%cd_normalising_factors(f)
         end do
       case (3)
         ! do nothing
     end select
     STOP_TIMER("SHELL1QUARTET:to_normalise_abf")
   end subroutine

!*******************************************************************************
!       make the J and K contributions from the shell4 and density matrix.
!*******************************************************************************

   subroutine make_r_JK(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the restricted part of the J and K matrices from the density elements
   ! in P, and the two electron integrals calculated from self.
     IN :: self
     REALMAT(:,:), INOUT :: J,K
      REALMAT(:,:), IN :: P
     REAL, IN :: factor
     INT, IN :: fa,la,fb,lb,fc,lc,fd,ld
     INT :: opt
     STACK("SHELL1QUARTET:make_r_JK")
     START_TIMER("SHELL1QUARTET:make_r_JK")
     opt = 0
     if (self%d%l==0) opt = opt + 1
     if (self%c%l==0) opt = opt + 2
     if (self%b%l==0) opt = opt + 4
     if (self%a%l==0) opt = opt + 8
     select case (opt)
       case (0);  call make_r_JK_abcd_(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld) ! abcd
       case (1);  call make_r_JK_abcs_(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd) ! abcs
       case (2);  call make_r_JK_absd_(self,J,K,P,factor,fa,la,fb,lb,fc,fd,ld) ! absd
       case (3);  call make_r_JK_abss_(self,J,K,P,factor,fa,la,fb,lb,fc,fd) ! abss
       case (4);  call make_r_JK_ascd_(self,J,K,P,factor,fa,la,fb,fc,lc,fd,ld) ! ascd
       case (5);  call make_r_JK_ascs_(self,J,K,P,factor,fa,la,fb,fc,lc,fd) ! ascs
       case (6);  call make_r_JK_assd_(self,J,K,P,factor,fa,la,fb,fc,fd,ld) ! assd
       case (7);  call make_r_JK_asss_(self,J,K,P,factor,fa,la,fb,fc,fd) ! asss
       case (8);  call make_r_JK_sbcd_(self,J,K,P,factor,fa,fb,lb,fc,lc,fd,ld) ! sbcd
       case (9);  call make_r_JK_sbcs_(self,J,K,P,factor,fa,fb,lb,fc,lc,fd) ! sbcs
       case (10); call make_r_JK_sbsd_(self,J,K,P,factor,fa,fb,lb,fc,fd,ld) ! sbsd
       case (11); call make_r_JK_sbss_(self,J,K,P,factor,fa,fb,lb,fc,fd) ! sbss
       case (12); call make_r_JK_sscd_(self,J,K,P,factor,fa,fb,fc,lc,fd,ld) ! sscd
       case (13); call make_r_JK_sscs_(self,J,K,P,factor,fa,fb,fc,lc,fd) ! sscs
       case (14); call make_r_JK_sssd_(self,J,K,P,factor,fa,fb,fc,fd,ld) ! sssd
       case (15); call make_r_JK_ssss_(self,J,K,P,factor,fa,fb,fc,fd) ! ssss
     end select
     STOP_TIMER("SHELL1QUARTET:make_r_JK")
      CHECK
   end subroutine

   subroutine make_r_JK_ssss(self,J,K,P,factor,fa,fb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an ssss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,fc,fd
     REAL, IN :: factor
     REAL :: I0,I_abcd
     STACK("SHELL1QUARTET:make_r_JK_ssss")
     START_TIMER("SHELL1QUARTET:make_r_JK_ssss")
     call make_ssss_(self,I0)
     I_abcd = factor * I0
     J(fa,fb) = J(fa,fb) + I_abcd*P(fd,fc)
     J(fc,fd) = J(fc,fd) + I_abcd*P(fb,fa)
     K(fa,fc) = K(fa,fc) + I_abcd*P(fd,fb)
     K(fa,fd) = K(fa,fd) + I_abcd*P(fc,fb)
     K(fb,fc) = K(fb,fc) + I_abcd*P(fd,fa)
     K(fb,fd) = K(fb,fd) + I_abcd*P(fc,fa)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_ssss")
      CHECK
   end subroutine

   subroutine make_r_JK_abcd(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For any shell4!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), target :: J,K
     INT, IN :: fa,la,fb,lb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT4(:,:,:,:), PTR :: I4
     REALVEC(:), PTR :: Kc,Kd
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     INT :: a,b,c,d
     STACK("SHELL1QUARTET:make_r_JK_abcd")
     START_TIMER("SHELL1QUARTET:make_r_JK_abcd")
     call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
     if (self%a%l==1 AND self%b%l==1 AND self%c%l==1 AND self%d%l==1) then
       call make_pppp_(self,I4)
     else
       call make_abcd_(self,I4)
     end if
     if (factor > 0.9) then
       do d = fd,ld
         Kd => K(:,d)
         do c = fc,lc
           P_dc = P(d,c)
           Kc => K(:,c)
           Jcd = ZERO
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             Kbc = ZERO
             Kbd = ZERO
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Kc(a)  = Kc(a)  + I_abcd*P_db
               Kd(a)  = Kd(a)  + I_abcd*P_cb
               Jcd    = Jcd    + I_abcd*P(b,a)
               Kbc    = Kbc    + I_abcd*P(d,a)
               Kbd    = Kbd    + I_abcd*P(c,a)
             end do
             Kc(b) = Kc(b) + Kbc
             Kd(b) = Kd(b) + Kbd
           end do
           J(c,d) = J(c,d) + Jcd
         end do
       end do
     else if (self%a%l<2) then
       do d = fd,ld
         Kd => K(:,d)
         do c = fc,lc
           P_dc = P(d,c)
           Kc => K(:,c)
           Jcd = ZERO
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             Kbc = ZERO
             Kbd = ZERO
             do a = fa,la
               I_abcd = factor*I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Kc(a)  = Kc(a)  + I_abcd*P_db
               Kd(a)  = Kd(a)  + I_abcd*P_cb
               Jcd    = Jcd    + I_abcd*P(b,a)
               Kbc    = Kbc    + I_abcd*P(d,a)
               Kbd    = Kbd    + I_abcd*P(c,a)
             end do
             Kc(b) = Kc(b) + Kbc
             Kd(b) = Kd(b) + Kbd
           end do
           J(c,d) = J(c,d) + Jcd
         end do
       end do
     else
       do d = fd,ld
         Kd => K(:,d)
         do c = fc,lc
           P_dc = factor*P(d,c)
           Kc => K(:,c)
           Jcd = ZERO
           do b = fb,lb
             P_db = factor*P(d,b)
             P_cb = factor*P(c,b)
             Kbc = ZERO
             Kbd = ZERO
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Kc(a)  = Kc(a)  + I_abcd*P_db
               Kd(a)  = Kd(a)  + I_abcd*P_cb
               Jcd    = Jcd    + I_abcd*P(b,a)
               Kbc    = Kbc    + I_abcd*P(d,a)
               Kbd    = Kbd    + I_abcd*P(c,a)
             end do
             Kc(b) = Kc(b) + factor*Kbc
             Kd(b) = Kd(b) + factor*Kbd
           end do
           J(c,d) = J(c,d) + factor*Jcd
         end do
       end do
     end if
     call destroy_(I4)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_abcd")
      CHECK
   end subroutine

   subroutine make_r_JK_sscd(self,J,K,P,factor,fa,fb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an sscd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_db,P_da,P_ba,Kbd,Kad,Jab,I_abcd
     INT :: c,d
     STACK("SHELL1QUARTET:make_r_JK_sscd")
     START_TIMER("SHELL1QUARTET:make_r_JK_sscd")
     call create_(I2,fc,lc,fd,ld)
     if (self%c%l==1 AND self%d%l==1) then
       call make_sspp_(self,I2)
     else
       call make_sscd_(self,I2)
     end if
     if (factor > 0.9) then
       Jab = ZERO
       P_ba = P(fb,fa)
       do d = fd,ld
         Kad = ZERO
         Kbd = ZERO
         P_da = P(d,fa)
         P_db = P(d,fb)
         do c = fc,lc
           I_abcd = I2(c,d)
           Jab     = Jab     + I_abcd*P(d,c)
           J(c,d)  = J(c,d)  + I_abcd*P_ba
           K(fa,c) = K(fa,c) + I_abcd*P_db
           K(fb,c) = K(fb,c) + I_abcd*P_da
           Kad     = Kad     + I_abcd*P(c,fb)
           Kbd     = Kbd     + I_abcd*P(c,fa)
         end do
         K(fa,d) = K(fa,d) + Kad
         K(fb,d) = K(fb,d) + Kbd
       end do
       J(fa,fb) = J(fa,fb) + Jab
     else if (self%c%l<2) then
       Jab = ZERO
       P_ba = P(fb,fa)
       do d = fd,ld
         Kad = ZERO
         Kbd = ZERO
         P_da = P(d,fa)
         P_db = P(d,fb)
         do c = fc,lc
           I_abcd = factor*I2(c,d)
           Jab     = Jab     + I_abcd*P(d,c)
           J(c,d)  = J(c,d)  + I_abcd*P_ba
           K(fa,c) = K(fa,c) + I_abcd*P_db
           K(fb,c) = K(fb,c) + I_abcd*P_da
           Kad     = Kad     + I_abcd*P(c,fb)
           Kbd     = Kbd     + I_abcd*P(c,fa)
         end do
         K(fa,d) = K(fa,d) + Kad
         K(fb,d) = K(fb,d) + Kbd
       end do
       J(fa,fb) = J(fa,fb) + Jab
     else
       Jab = ZERO
       P_ba = factor*P(fb,fa)
       do d = fd,ld
         Kad = ZERO
         Kbd = ZERO
         P_da = factor*P(d,fa)
         P_db = factor*P(d,fb)
         do c = fc,lc
           I_abcd = I2(c,d)
           Jab     = Jab     + I_abcd*P(d,c)
           J(c,d)  = J(c,d)  + I_abcd*P_ba
           K(fa,c) = K(fa,c) + I_abcd*P_db
           K(fb,c) = K(fb,c) + I_abcd*P_da
           Kad     = Kad     + I_abcd*P(c,fb)
           Kbd     = Kbd     + I_abcd*P(c,fa)
         end do
         K(fa,d) = K(fa,d) + factor*Kad
         K(fb,d) = K(fb,d) + factor*Kbd
       end do
       J(fa,fb) = J(fa,fb) + factor*Jab
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sscd")
      CHECK
   end subroutine

   subroutine make_r_JK_abcs(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an abcs shell4!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,lb,fc,lc,fd
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     INT :: a,b,c
     STACK("SHELL1QUARTET:make_r_JK_abcs")
     START_TIMER("SHELL1QUARTET:make_r_JK_abcs")
     call create_(I3,fa,la,fb,lb,fc,lc)
     if (self%a%l==1 AND self%b%l==1 AND self%c%l==1) then
       call make_ppps_(self,I3)
     else
       call make_abcs_(self,I3)
     end if
     if (factor > 0.9) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           P_db = P(fd,b)
           P_cb = P(c,b)
           Kbc = ZERO
           Kbd = ZERO
           do a = fa,la
             I_abcd  = I3(a,b,c)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
             K(a,c)  = K(a,c)  + I_abcd*P_db
             K(a,fd) = K(a,fd) + I_abcd*P_cb
             Kbc     = Kbc     + I_abcd*P(fd,a)
             Kbd     = Kbd     + I_abcd*P(c,a)
           end do
           K(b,c)  = K(b,c)  + Kbc
           K(b,fd) = K(b,fd) + Kbd
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else if (self%a%l<2) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           P_db = P(fd,b)
           P_cb = P(c,b)
           Kbc = ZERO
           Kbd = ZERO
           do a = fa,la
             I_abcd  = factor*I3(a,b,c)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
             K(a,c)  = K(a,c)  + I_abcd*P_db
             K(a,fd) = K(a,fd) + I_abcd*P_cb
             Kbc     = Kbc     + I_abcd*P(fd,a)
             Kbd     = Kbd     + I_abcd*P(c,a)
           end do
           K(b,c)  = K(b,c)  + Kbc
           K(b,fd) = K(b,fd) + Kbd
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else
       do c = fc,lc
         P_dc = factor*P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           P_db = factor*P(fd,b)
           P_cb = factor*P(c,b)
           Kbc = ZERO
           Kbd = ZERO
           do a = fa,la
             I_abcd  = I3(a,b,c)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
             K(a,c)  = K(a,c)  + I_abcd*P_db
             K(a,fd) = K(a,fd) + I_abcd*P_cb
             Kbc     = Kbc     + I_abcd*P(fd,a)
             Kbd     = Kbd     + I_abcd*P(c,a)
           end do
           K(b,c)  = K(b,c)  + factor*Kbc
           K(b,fd) = K(b,fd) + factor*Kbd
         end do
         J(c,fd) = J(c,fd) + factor*Jcd
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_abcs")
      CHECK
   end subroutine

   subroutine make_r_JK_absd(self,J,K,P,factor,fa,la,fb,lb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an absd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,lb,fc,fd,ld
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     INT :: a,b,d
     STACK("SHELL1QUARTET:make_r_JK_absd")
     START_TIMER("SHELL1QUARTET:make_r_JK_absd")
     call create_(I3,fa,la,fb,lb,fd,ld)
     if (self%a%l==1 AND self%b%l==1 AND self%d%l==1) then
       call make_ppps_(self,I3)
     else
       call make_abcs_(self,I3)
     end if
     if (factor > 0.9) then
       do d = fd,ld
         P_dc = P(d,fc)
         Jcd = ZERO
         do b = fb,lb
           P_db = P(d,b)
           P_cb = P(fc,b)
           Kbc = ZERO
           Kbd = ZERO
           do a = fa,la
             I_abcd  = I3(a,b,d)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
             K(a,fc) = K(a,fc) + I_abcd*P_db
             K(a,d)  = K(a,d)  + I_abcd*P_cb
             Kbc     = Kbc     + I_abcd*P(d,a)
             Kbd     = Kbd     + I_abcd*P(fc,a)
           end do
           K(b,fc) = K(b,fc) + Kbc
           K(b,d)  = K(b,d)  + Kbd
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else if (self%a%l<2) then
       do d = fd,ld
         P_dc = P(d,fc)
         Jcd = ZERO
         do b = fb,lb
           P_db = P(d,b)
           P_cb = P(fc,b)
           Kbc = ZERO
           Kbd = ZERO
           do a = fa,la
             I_abcd  = factor*I3(a,b,d)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
             K(a,fc) = K(a,fc) + I_abcd*P_db
             K(a,d)  = K(a,d)  + I_abcd*P_cb
             Kbc     = Kbc     + I_abcd*P(d,a)
             Kbd     = Kbd     + I_abcd*P(fc,a)
           end do
           K(b,fc) = K(b,fc) + Kbc
           K(b,d)  = K(b,d)  + Kbd
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else
       do d = fd,ld
         P_dc = factor*P(d,fc)
         Jcd = ZERO
         do b = fb,lb
           P_db = factor*P(d,b)
           P_cb = factor*P(fc,b)
           Kbc = ZERO
           Kbd = ZERO
           do a = fa,la
             I_abcd  = I3(a,b,d)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
             K(a,fc) = K(a,fc) + I_abcd*P_db
             K(a,d)  = K(a,d)  + I_abcd*P_cb
             Kbc     = Kbc     + I_abcd*P(d,a)
             Kbd     = Kbd     + I_abcd*P(fc,a)
           end do
           K(b,fc) = K(b,fc) + factor*Kbc
           K(b,d)  = K(b,d)  + factor*Kbd
         end do
         J(fc,d) = J(fc,d) + factor*Jcd
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_absd")
      CHECK
   end subroutine

   subroutine make_r_JK_sbcd(self,J,K,P,factor,fa,fb,lb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a spcd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,lb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: Jab,P_ba,Kad,Kbd,P_da,P_db,I_abcd
     INT :: b,c,d
     STACK("SHELL1QUARTET:make_r_JK_sbcd")
     START_TIMER("SHELL1QUARTET:make_r_JK_sbcd")
     call create_(I3,fb,lb,fc,lc,fd,ld)
     if (self%b%l==1 AND self%c%l==1 AND self%d%l==1) then
       call make_pspp_(self,I3)
     else
       call make_ascd_(self,I3)
     end if
     if (factor > 0.9) then
       do b=fb,lb
         Jab = ZERO
         P_ba = P(b,fa)
         do d = fd,ld
           Kad = ZERO
           Kbd = ZERO
           P_da = P(d,fa)
           P_db = P(d,b)
           do c = fc,lc
             I_abcd  = I3(b,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
             K(fa,c) = K(fa,c) + I_abcd*P_db
             K(b,c)  = K(b,c)  + I_abcd*P_da
             Kad     = Kad     + I_abcd*P(c,b)
             Kbd     = Kbd     + I_abcd*P(c,fa)
           end do
           K(fa,d) = K(fa,d) + Kad
           K(b,d)  = K(b,d)  + Kbd
         end do
         J(fa,b) = J(fa,b) + Jab
       end do
     else if (self%c%l<2) then
       do b=fb,lb
         Jab = ZERO
         P_ba = P(b,fa)
         do d = fd,ld
           Kad = ZERO
           Kbd = ZERO
           P_da = P(d,fa)
           P_db = P(d,b)
           do c = fc,lc
             I_abcd  = factor*I3(b,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
             K(fa,c) = K(fa,c) + I_abcd*P_db
             K(b,c)  = K(b,c)  + I_abcd*P_da
             Kad     = Kad     + I_abcd*P(c,b)
             Kbd     = Kbd     + I_abcd*P(c,fa)
           end do
           K(fa,d) = K(fa,d) + Kad
           K(b,d)  = K(b,d)  + Kbd
         end do
         J(fa,b) = J(fa,b) + Jab
       end do
     else
       do b=fb,lb
         Jab = ZERO
         P_ba = factor*P(b,fa)
         do d = fd,ld
           Kad = ZERO
           Kbd = ZERO
           P_da = factor*P(d,fa)
           P_db = factor*P(d,b)
           do c = fc,lc
             I_abcd  = I3(b,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
             K(fa,c) = K(fa,c) + I_abcd*P_db
             K(b,c)  = K(b,c)  + I_abcd*P_da
             Kad     = Kad     + I_abcd*P(c,b)
             Kbd     = Kbd     + I_abcd*P(c,fa)
           end do
           K(fa,d) = K(fa,d) + factor*Kad
           K(b,d)  = K(b,d)  + factor*Kbd
         end do
         J(fa,b) = J(fa,b) + factor*Jab
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sbcd")
      CHECK
   end subroutine

   subroutine make_r_JK_ascd(self,J,K,P,factor,fa,la,fb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a pscd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: Jab,P_ba,Kad,Kbd,P_da,P_db,I_abcd
     INT :: a,c,d
     STACK("SHELL1QUARTET:make_r_JK_ascd")
     START_TIMER("SHELL1QUARTET:make_r_JK_ascd")
     call create_(I3,fa,la,fc,lc,fd,ld)
     if (self%a%l==1 AND self%c%l==1 AND self%d%l==1) then
       call make_pspp_(self,I3)
     else
       call make_ascd_(self,I3)
     end if
     if (factor > 0.9) then
       do a=fa,la
         Jab = ZERO
         P_ba = P(fb,a)
         do d = fd,ld
           Kad = ZERO
           Kbd = ZERO
           P_da = P(d,a)
           P_db = P(d,fb)
           do c = fc,lc
             I_abcd  = I3(a,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
             K(a,c)  = K(a,c)  + I_abcd*P_db
             K(fb,c) = K(fb,c) + I_abcd*P_da
             Kad     = Kad     + I_abcd*P(c,fb)
             Kbd     = Kbd     + I_abcd*P(c,a)
           end do
           K(a,d)  = K(a,d)  + Kad
           K(fb,d) = K(fb,d) + Kbd
         end do
         J(a,fb) = J(a,fb) + Jab
       end do
     else if (self%c%l<2) then
       do a=fa,la
         Jab = ZERO
         P_ba = P(fb,a)
         do d = fd,ld
           Kad = ZERO
           Kbd = ZERO
           P_da = P(d,a)
           P_db = P(d,fb)
           do c = fc,lc
             I_abcd  = factor*I3(a,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
             K(a,c)  = K(a,c)  + I_abcd*P_db
             K(fb,c) = K(fb,c) + I_abcd*P_da
             Kad     = Kad     + I_abcd*P(c,fb)
             Kbd     = Kbd     + I_abcd*P(c,a)
           end do
           K(a,d)  = K(a,d)  + Kad
           K(fb,d) = K(fb,d) + Kbd
         end do
         J(a,fb) = J(a,fb) + Jab
       end do
     else
       do a=fa,la
         Jab = ZERO
         P_ba = factor*P(fb,a)
         do d = fd,ld
           Kad = ZERO
           Kbd = ZERO
           P_da = factor*P(d,a)
           P_db = factor*P(d,fb)
           do c = fc,lc
             I_abcd  = I3(a,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
             K(a,c)  = K(a,c)  + I_abcd*P_db
             K(fb,c) = K(fb,c) + I_abcd*P_da
             Kad     = Kad     + I_abcd*P(c,fb)
             Kbd     = Kbd     + I_abcd*P(c,a)
           end do
           K(a,d)  = K(a,d)  + factor*Kad
           K(fb,d) = K(fb,d) + factor*Kbd
         end do
         J(a,fb) = J(a,fb) + factor*Jab
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_ascd")
      CHECK
   end subroutine

   subroutine make_r_JK_abss(self,J,K,P,factor,fa,la,fb,lb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an abss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,lb,fc,fd
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     INT :: a,b
     STACK("SHELL1QUARTET:make_r_JK_abss")
     START_TIMER("SHELL1QUARTET:make_r_JK_abss")
     call create_(I2,fa,la,fb,lb)
     if (self%a%l==1 AND self%b%l==1) then
       call make_ppss_(self,I2)
     else
       call make_abss_(self,I2)
     end if
     if (factor > 0.9) then
       Jcd = ZERO
       P_dc = P(fd,fc)
       do b = fb,lb
         Kbc = ZERO
         Kbd = ZERO
         P_db = P(fd,b)
         P_cb = P(fc,b)
         do a = fa,la
           I_abcd = I2(a,b)
           J(a,b) = J(a,b) + I_abcd*P_dc
           Jcd    = Jcd    + I_abcd*P(b,a)
           K(a,fc) = K(a,fc) + I_abcd*P_db
           K(a,fd) = K(a,fd) + I_abcd*P_cb
           Kbc    = Kbc    + I_abcd*P(fd,a)
           Kbd    = Kbd    + I_abcd*P(fc,a)
         end do
         K(b,fc) = K(b,fc) + Kbc
         K(b,fd) = K(b,fd) + Kbd
       end do
       J(fc,fd) = J(fc,fd) + Jcd
     else if (self%a%l<2) then
       Jcd = ZERO
       P_dc = P(fd,fc)
       do b = fb,lb
         Kbc = ZERO
         Kbd = ZERO
         P_db = P(fd,b)
         P_cb = P(fc,b)
         do a = fa,la
           I_abcd = factor*I2(a,b)
           J(a,b) = J(a,b) + I_abcd*P_dc
           Jcd    = Jcd    + I_abcd*P(b,a)
           K(a,fc) = K(a,fc) + I_abcd*P_db
           K(a,fd) = K(a,fd) + I_abcd*P_cb
           Kbc    = Kbc    + I_abcd*P(fd,a)
           Kbd    = Kbd    + I_abcd*P(fc,a)
         end do
         K(b,fc) = K(b,fc) + Kbc
         K(b,fd) = K(b,fd) + Kbd
       end do
       J(fc,fd) = J(fc,fd) + Jcd
     else
       Jcd = ZERO
       P_dc = factor*P(fd,fc)
       do b = fb,lb
         Kbc = ZERO
         Kbd = ZERO
         P_db = factor*P(fd,b)
         P_cb = factor*P(fc,b)
         do a = fa,la
           I_abcd = I2(a,b)
           J(a,b) = J(a,b) + I_abcd*P_dc
           Jcd    = Jcd    + I_abcd*P(b,a)
           K(a,fc) = K(a,fc) + I_abcd*P_db
           K(a,fd) = K(a,fd) + I_abcd*P_cb
           Kbc    = Kbc    + I_abcd*P(fd,a)
           Kbd    = Kbd    + I_abcd*P(fc,a)
         end do
         K(b,fc) = K(b,fc) + factor*Kbc
         K(b,fd) = K(b,fd) + factor*Kbd
       end do
       J(fc,fd) = J(fc,fd) + factor*Jcd
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_abss")
      CHECK
   end subroutine

   subroutine make_r_JK_ascs(self,J,K,P,factor,fa,la,fb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a ascs shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,fc,lc,fd
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     INT :: a,c
     STACK("SHELL1QUARTET:make_r_JK_ascs")
     START_TIMER("SHELL1QUARTET:make_r_JK_ascs")
     call create_(I2,fa,la,fc,lc)
     if (self%a%l==1 AND self%c%l==1) then
       call make_psps_(self,I2)
     else if (self%a%l==2 AND self%c%l==2) then
       call make_dsds_(self,I2)
     else if (self%a%l==2 AND self%c%l==1) then
       call make_dsps_(self,I2)
     else if (self%a%l==1 AND self%c%l==2) then
       call make_psds_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       P_db = P(fd,fb)
       Kbd = ZERO
       do c = fc,lc
         P_dc = P(fd,c)
         P_cb = P(c,fb)
         Jcd = ZERO
         Kbc = ZERO
         do a = fa,la
           I_abcd  = I2(a,c)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           Jcd     = Jcd     + I_abcd*P(fb,a)
           K(a,c)  = K(a,c)  + I_abcd*P_db
           K(a,fd) = K(a,fd) + I_abcd*P_cb
           Kbc     = Kbc     + I_abcd*P(fd,a)
           Kbd     = Kbd     + I_abcd*P(c,a)
         end do
         K(fb,c) = K(fb,c) + Kbc
         J(c,fd) = J(c,fd) + Jcd
       end do
       K(fb,fd) = K(fb,fd) + Kbd
     else if (self%a%l<2) then
       P_db = P(fd,fb)
       Kbd = ZERO
       do c = fc,lc
         P_dc = P(fd,c)
         P_cb = P(c,fb)
         Jcd = ZERO
         Kbc = ZERO
         do a = fa,la
           I_abcd  = factor*I2(a,c)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           Jcd     = Jcd     + I_abcd*P(fb,a)
           K(a,c)  = K(a,c)  + I_abcd*P_db
           K(a,fd) = K(a,fd) + I_abcd*P_cb
           Kbc     = Kbc     + I_abcd*P(fd,a)
           Kbd     = Kbd     + I_abcd*P(c,a)
         end do
         K(fb,c) = K(fb,c) + Kbc
         J(c,fd) = J(c,fd) + Jcd
       end do
       K(fb,fd) = K(fb,fd) + Kbd
     else
       P_db = factor*P(fd,fb)
       Kbd = ZERO
       do c = fc,lc
         P_dc = factor*P(fd,c)
         P_cb = factor*P(c,fb)
         Jcd = ZERO
         Kbc = ZERO
         do a = fa,la
           I_abcd  = I2(a,c)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           Jcd     = Jcd     + I_abcd*P(fb,a)
           K(a,c)  = K(a,c)  + I_abcd*P_db
           K(a,fd) = K(a,fd) + I_abcd*P_cb
           Kbc     = Kbc     + I_abcd*P(fd,a)
           Kbd     = Kbd     + I_abcd*P(c,a)
         end do
         K(fb,c) = K(fb,c) + factor*Kbc
         J(c,fd) = J(c,fd) + factor*Jcd
       end do
       K(fb,fd) = K(fb,fd) + factor*Kbd
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_ascs")
      CHECK
   end subroutine

   subroutine make_r_JK_assd(self,J,K,P,factor,fa,la,fb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a assd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,fc,fd,ld
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
     INT :: a,d
     STACK("SHELL1QUARTET:make_r_JK_assd")
     START_TIMER("SHELL1QUARTET:make_r_JK_assd")
     call create_(I2,fa,la,fd,ld)
     if (self%a%l==1 AND self%d%l==1) then
       call make_psps_(self,I2)
     else if (self%a%l==2 AND self%d%l==2) then
       call make_dsds_(self,I2)
     else if (self%a%l==2 AND self%d%l==1) then
       call make_dsps_(self,I2)
     else if (self%a%l==1 AND self%d%l==2) then
       call make_psds_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       P_cb = P(fc,fb)
       Kbc = ZERO
       do d = fd,ld
         P_dc = P(d,fc)
         P_db = P(d,fb)
         Jcd = ZERO
         Kbd = ZERO
         do a = fa,la
           I_abcd  = I2(a,d)
           Jcd     = Jcd     + I_abcd*P(fb,a)
           Kbc     = Kbc     + I_abcd*P(d,a)
           Kbd     = Kbd     + I_abcd*P(fc,a)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           K(a,fc) = K(a,fc) + I_abcd*P_db
           K(a,d)  = K(a,d)  + I_abcd*P_cb
         end do
         K(fb,d) = K(fb,d) + Kbd
         J(fc,d) = J(fc,d) + Jcd
       end do
       K(fb,fc) = K(fb,fc) + Kbc
     else if (self%a%l<2) then
       P_cb = P(fc,fb)
       Kbc = ZERO
       do d = fd,ld
         P_dc = P(d,fc)
         P_db = P(d,fb)
         Jcd = ZERO
         Kbd = ZERO
         do a = fa,la
           I_abcd  = factor*I2(a,d)
           Jcd     = Jcd     + I_abcd*P(fb,a)
           Kbc     = Kbc     + I_abcd*P(d,a)
           Kbd     = Kbd     + I_abcd*P(fc,a)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           K(a,fc) = K(a,fc) + I_abcd*P_db
           K(a,d)  = K(a,d)  + I_abcd*P_cb
         end do
         K(fb,d) = K(fb,d) + Kbd
         J(fc,d) = J(fc,d) + Jcd
       end do
       K(fb,fc) = K(fb,fc) + Kbc
     else
       P_cb = factor*P(fc,fb)
       Kbc = ZERO
       do d = fd,ld
         P_dc = factor*P(d,fc)
         P_db = factor*P(d,fb)
         Jcd = ZERO
         Kbd = ZERO
         do a = fa,la
           I_abcd  = I2(a,d)
           Jcd     = Jcd     + I_abcd*P(fb,a)
           Kbc     = Kbc     + I_abcd*P(d,a)
           Kbd     = Kbd     + I_abcd*P(fc,a)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           K(a,fc) = K(a,fc) + I_abcd*P_db
           K(a,d)  = K(a,d)  + I_abcd*P_cb
         end do
         K(fb,d) = K(fb,d) + factor*Kbd
         J(fc,d) = J(fc,d) + factor*Jcd
       end do
       K(fb,fc) = K(fb,fc) + factor*Kbc
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_assd")
      CHECK
   end subroutine

   subroutine make_r_JK_sbcs(self,J,K,P,factor,fa,fb,lb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a sbcs shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,lb,fc,lc,fd
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,P_da,P_ca,Kac,Kad,I_abcd
     INT :: b,c
     STACK("SHELL1QUARTET:make_r_JK_sbcs")
     START_TIMER("SHELL1QUARTET:make_r_JK_sbcs")
     call create_(I2,fb,lb,fc,lc)
     if (self%b%l==1 AND self%c%l==1) then
       call make_psps_(self,I2)
     else if (self%b%l==2 AND self%c%l==2) then
       call make_dsds_(self,I2)
     else if (self%b%l==2 AND self%c%l==1) then
       call make_dsps_(self,I2)
     else if (self%b%l==1 AND self%c%l==2) then
       call make_psds_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       Kad = ZERO
       P_da = P(fd,fa)
       do c = fc,lc
         P_dc = P(fd,c)
         P_ca = P(c,fa)
         Jcd = ZERO
         Kac = ZERO
         do b = fb,lb
           I_abcd  = I2(b,c)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           Kac     = Kac     + I_abcd*P(fd,b)
           Kad     = Kad     + I_abcd*P(c,b)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
           K(b,c)  = K(b,c)  + I_abcd*P_da
           K(b,fd) = K(b,fd) + I_abcd*P_ca
         end do
         K(fa,c) = K(fa,c) + Kac
         J(c,fd) = J(c,fd) + Jcd
       end do
       K(fa,fd) = K(fa,fd) + Kad
     else if (self%b%l<2) then
       Kad = ZERO
       P_da = P(fd,fa)
       do c = fc,lc
         P_dc = P(fd,c)
         P_ca = P(c,fa)
         Jcd = ZERO
         Kac = ZERO
         do b = fb,lb
           I_abcd  = factor*I2(b,c)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           Kac     = Kac     + I_abcd*P(fd,b)
           Kad     = Kad     + I_abcd*P(c,b)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
           K(b,c)  = K(b,c)  + I_abcd*P_da
           K(b,fd) = K(b,fd) + I_abcd*P_ca
         end do
         K(fa,c) = K(fa,c) + Kac
         J(c,fd) = J(c,fd) + Jcd
       end do
       K(fa,fd) = K(fa,fd) + Kad
     else
       Kad = ZERO
       P_da = factor*P(fd,fa)
       do c = fc,lc
         P_dc = factor*P(fd,c)
         P_ca = factor*P(c,fa)
         Jcd = ZERO
         Kac = ZERO
         do b = fb,lb
           I_abcd  = I2(b,c)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           Kac     = Kac     + I_abcd*P(fd,b)
           Kad     = Kad     + I_abcd*P(c,b)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
           K(b,c)  = K(b,c)  + I_abcd*P_da
           K(b,fd) = K(b,fd) + I_abcd*P_ca
         end do
         K(fa,c) = K(fa,c) + factor*Kac
         J(c,fd) = J(c,fd) + factor*Jcd
       end do
       K(fa,fd) = K(fa,fd) + factor*Kad
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sbcs")
      CHECK
   end subroutine

   subroutine make_r_JK_sbsd(self,J,K,P,factor,fa,fb,lb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a sbsd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,lb,fc,fd,ld
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_ca,Jcd,P_dc,P_da,Kac,Kad,I_abcd
     INT :: b,d
     STACK("SHELL1QUARTET:make_r_JK_sbsd")
     START_TIMER("SHELL1QUARTET:make_r_JK_sbsd")
     call create_(I2,fb,lb,fd,ld)
     if (self%b%l==1 AND self%d%l==1) then
       call make_psps_(self,I2)
     else if (self%b%l==2 AND self%d%l==2) then
       call make_dsds_(self,I2)
     else if (self%b%l==2 AND self%d%l==1) then
       call make_dsps_(self,I2)
     else if (self%b%l==1 AND self%d%l==2) then
       call make_psds_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       P_ca = P(fc,fa)
       Kac = ZERO
       do d = fd,ld
         Kad = ZERO
         Jcd = ZERO
         P_dc = P(d,fc)
         P_da = P(d,fa)
         do b = fb,lb
           I_abcd  = I2(b,d)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           Kac     = Kac     + I_abcd*P(d,b)
           Kad     = Kad     + I_abcd*P(fc,b)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
           K(b,fc) = K(b,fc) + I_abcd*P_da
           K(b,d)  = K(b,d)  + I_abcd*P_ca
         end do
         K(fa,d) = K(fa,d) + Kad
         J(fc,d) = J(fc,d) + Jcd
       end do
       K(fa,fc) = K(fa,fc) + Kac
     else if (self%b%l<2) then
       P_ca = P(fc,fa)
       Kac = ZERO
       do d = fd,ld
         Kad = ZERO
         Jcd = ZERO
         P_dc = P(d,fc)
         P_da = P(d,fa)
         do b = fb,lb
           I_abcd  = factor*I2(b,d)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           Kac     = Kac     + I_abcd*P(d,b)
           Kad     = Kad     + I_abcd*P(fc,b)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
           K(b,fc) = K(b,fc) + I_abcd*P_da
           K(b,d)  = K(b,d)  + I_abcd*P_ca
         end do
         K(fa,d) = K(fa,d) + Kad
         J(fc,d) = J(fc,d) + Jcd
       end do
       K(fa,fc) = K(fa,fc) + Kac
     else
       P_ca = factor*P(fc,fa)
       Kac = ZERO
       do d = fd,ld
         Kad = ZERO
         Jcd = ZERO
         P_dc = factor*P(d,fc)
         P_da = factor*P(d,fa)
         do b = fb,lb
           I_abcd  = I2(b,d)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           Kac     = Kac     + I_abcd*P(d,b)
           Kad     = Kad     + I_abcd*P(fc,b)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
           K(b,fc) = K(b,fc) + I_abcd*P_da
           K(b,d)  = K(b,d)  + I_abcd*P_ca
         end do
         K(fa,d) = K(fa,d) + factor*Kad
         J(fc,d) = J(fc,d) + factor*Jcd
       end do
       K(fa,fc) = K(fa,fc) + factor*Kac
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sbsd")
      CHECK
   end subroutine

   subroutine make_r_JK_asss(self,J,K,P,factor,fa,la,fb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a asss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,la,fb,fc,fd
     REAL, IN :: factor
      REALVEC(fa:la) :: I
     REAL :: P_dc,Jcd,P_db,P_cb,Kbc,Kbd,I_abcd
      INT :: a
     STACK("SHELL1QUARTET:make_r_JK_asss")
     START_TIMER("SHELL1QUARTET:make_r_JK_asss")
     if (self%a%l==1) then
       call make_psss_(self,I)
       P_cb = P(fc,fb)
       P_dc = P(fd,fc)
       P_db = P(fd,fb)
       Kbc = ZERO
       Jcd = ZERO
       Kbd = ZERO
       do a = fa,la
         I_abcd  = factor*I(a)
         Jcd     = Jcd     + I_abcd*P(fb,a)
         Kbc     = Kbc     + I_abcd*P(fd,a)
         Kbd     = Kbd     + I_abcd*P(fc,a)
         J(a,fb) = J(a,fb) + I_abcd*P_dc
         K(a,fc) = K(a,fc) + I_abcd*P_db
         K(a,fd) = K(a,fd) + I_abcd*P_cb
       end do
       K(fb,fd) = K(fb,fd) + Kbd
       J(fc,fd) = J(fc,fd) + Jcd
       K(fb,fc) = K(fb,fc) + Kbc
     else
       if (self%a%l==2) then
         call make_dsss_(self,I)
       else
         call make_asss_(self,I)
       end if
       P_cb = factor*P(fc,fb)
       P_dc = factor*P(fd,fc)
       P_db = factor*P(fd,fb)
       Kbc = ZERO
       Jcd = ZERO
       Kbd = ZERO
       do a = fa,la
         I_abcd  = I(a)
         Jcd     = Jcd     + I_abcd*P(fb,a)
         Kbc     = Kbc     + I_abcd*P(fd,a)
         Kbd     = Kbd     + I_abcd*P(fc,a)
         J(a,fb) = J(a,fb) + I_abcd*P_dc
         K(a,fc) = K(a,fc) + I_abcd*P_db
         K(a,fd) = K(a,fd) + I_abcd*P_cb
       end do
       K(fb,fd) = K(fb,fd) + factor*Kbd
       J(fc,fd) = J(fc,fd) + factor*Jcd
       K(fb,fc) = K(fb,fc) + factor*Kbc
     end if
     STOP_TIMER("SHELL1QUARTET:make_r_JK_asss")
      CHECK
   end subroutine

   subroutine make_r_JK_sbss(self,J,K,P,factor,fa,fb,lb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For a sbss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,lb,fc,fd
     REAL, IN :: factor
     REALVEC(fb:lb) :: I
     REAL :: P_ca,Jcd,P_dc,P_da,Kac,Kad,I_abcd
      INT :: b
     STACK("SHELL1QUARTET:make_r_JK_sbss")
     START_TIMER("SHELL1QUARTET:make_r_JK_sbss")
     if (self%b%l==1) then
       call make_psss_(self,I)
       P_ca = P(fc,fa)
       P_dc = P(fd,fc)
       P_da = P(fd,fa)
       Kac = ZERO
       Kad = ZERO
       Jcd = ZERO
       do b = fb,lb
         I_abcd  = factor*I(b)
         Jcd     = Jcd     + I_abcd*P(b,fa)
         Kac     = Kac     + I_abcd*P(fd,b)
         Kad     = Kad     + I_abcd*P(fc,b)
         J(fa,b) = J(fa,b) + I_abcd*P_dc
         K(b,fc) = K(b,fc) + I_abcd*P_da
         K(b,fd) = K(b,fd) + I_abcd*P_ca
       end do
       K(fa,fd) = K(fa,fd) + Kad
       J(fc,fd) = J(fc,fd) + Jcd
       K(fa,fc) = K(fa,fc) + Kac
     else
       if (self%b%l==2) then
         call make_dsss_(self,I)
       else
         call make_asss_(self,I)
       end if
       P_ca = factor*P(fc,fa)
       P_dc = factor*P(fd,fc)
       P_da = factor*P(fd,fa)
       Kac = ZERO
       Kad = ZERO
       Jcd = ZERO
       do b = fb,lb
         I_abcd  = I(b)
         Jcd     = Jcd     + I_abcd*P(b,fa)
         Kac     = Kac     + I_abcd*P(fd,b)
         Kad     = Kad     + I_abcd*P(fc,b)
         J(fa,b) = J(fa,b) + I_abcd*P_dc
         K(b,fc) = K(b,fc) + I_abcd*P_da
         K(b,fd) = K(b,fd) + I_abcd*P_ca
       end do
       K(fa,fd) = K(fa,fd) + factor*Kad
       J(fc,fd) = J(fc,fd) + factor*Jcd
       K(fa,fc) = K(fa,fc) + factor*Kac
     end if
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sbss")
      CHECK
   end subroutine

   subroutine make_r_JK_sscs(self,J,K,P,factor,fa,fb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an sscs shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,fc,lc,fd
     REAL, IN :: factor
      REALVEC(:), PTR :: I
     REAL :: P_db,P_da,P_ba,Kbd,Kad,Jab,I_abcd
      INT :: c
     STACK("SHELL1QUARTET:make_r_JK_sscs")
     START_TIMER("SHELL1QUARTET:make_r_JK_sscs")
     call create_(I,fc,lc)
     if (self%c%l==1) then
       call make_ssps_(self,I)
       P_db = P(fd,fb)
       P_da = P(fd,fa)
       P_ba = P(fb,fa)
       Kbd = ZERO
       Kad = ZERO
       Jab = ZERO
       do c = fc,lc
         I_abcd = factor*I(c)
         Jab      = Jab     + I_abcd*P(fd,c)
         J(c,fd)  = J(c,fd) + I_abcd*P_ba
         K(fa,c)  = K(fa,c) + I_abcd*P_db
         Kad      = Kad     + I_abcd*P(c,fb)
         K(fb,c)  = K(fb,c) + I_abcd*P_da
         Kbd      = Kbd     + I_abcd*P(c,fa)
       end do
       J(fa,fb) = J(fa,fb) + Jab
       K(fa,fd) = K(fa,fd) + Kad
       K(fb,fd) = K(fb,fd) + Kbd
     else
       if (self%c%l==2) then
         call make_ssds_(self,I)
       else
         call make_sscs_(self,I)
       end if
       P_db = factor*P(fd,fb)
       P_da = factor*P(fd,fa)
       P_ba = factor*P(fb,fa)
       Kbd = ZERO
       Kad = ZERO
       Jab = ZERO
       do c = fc,lc
         I_abcd = I(c)
         Jab      = Jab     + I_abcd*P(fd,c)
         J(c,fd)  = J(c,fd) + I_abcd*P_ba
         K(fa,c)  = K(fa,c) + I_abcd*P_db
         Kad      = Kad     + I_abcd*P(c,fb)
         K(fb,c)  = K(fb,c) + I_abcd*P_da
         Kbd      = Kbd     + I_abcd*P(c,fa)
       end do
       J(fa,fb) = J(fa,fb) + factor*Jab
       K(fa,fd) = K(fa,fd) + factor*Kad
       K(fb,fd) = K(fb,fd) + factor*Kbd
     end if
     call destroy_(I)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sscs")
      CHECK
   end subroutine

   subroutine make_r_JK_sssd(self,J,K,P,factor,fa,fb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J and K contribution due to self and P and add it in.
   ! For an sssd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J,K
     INT, IN :: fa,fb,fc,fd,ld
     REAL, IN :: factor
     REALVEC(:), PTR :: I
     REAL :: Jab,P_cb,P_ca,P_ba,Kbc,Kac,I_abcd
     INT :: d
     STACK("SHELL1QUARTET:make_r_JK_sssd")
     START_TIMER("SHELL1QUARTET:make_r_JK_sssd")
     call create_(I,fd,ld)
     if (self%d%l==1) then
       call make_ssps_(self,I)
       P_cb = P(fc,fb)
       P_ca = P(fc,fa)
       P_ba = P(fb,fa)
       Jab = ZERO
       Kac = ZERO
       Kbc = ZERO
       do d = fd,ld
         I_abcd = factor*I(d)
         Jab     = Jab     + I_abcd*P(d,fc)
         J(fc,d) = J(fc,d) + I_abcd*P_ba
         Kac     = Kac     + I_abcd*P(d,fb)
         K(fa,d) = K(fa,d) + I_abcd*P_cb
         Kbc     = Kbc     + I_abcd*P(d,fa)
         K(fb,d) = K(fb,d) + I_abcd*P_ca
       end do
       K(fb,fc) = K(fb,fc) + Kbc
       K(fa,fc) = K(fa,fc) + Kac
       J(fa,fb) = J(fa,fb) + Jab
     else
       if (self%d%l==2) then
         call make_ssds_(self,I)
       else
         call make_sscs_(self,I)
       end if
       P_cb = factor*P(fc,fb)
       P_ca = factor*P(fc,fa)
       P_ba = factor*P(fb,fa)
       Jab = ZERO
       Kac = ZERO
       Kbc = ZERO
       do d = fd,ld
         I_abcd = I(d)
         Jab     = Jab     + I_abcd*P(d,fc)
         J(fc,d) = J(fc,d) + I_abcd*P_ba
         Kac     = Kac     + I_abcd*P(d,fb)
         K(fa,d) = K(fa,d) + I_abcd*P_cb
         Kbc     = Kbc     + I_abcd*P(d,fa)
         K(fb,d) = K(fb,d) + I_abcd*P_ca
       end do
       K(fb,fc) = K(fb,fc) + factor*Kbc
       K(fa,fc) = K(fa,fc) + factor*Kac
       J(fa,fb) = J(fa,fb) + factor*Jab
     end if
     call destroy_(I)
     STOP_TIMER("SHELL1QUARTET:make_r_JK_sssd")
      CHECK
   end subroutine

!*******************************************************************************
!       make only the J contributions from the shell4 and density matrix.
!*******************************************************************************

   subroutine make_r_J(self,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the restricted part of the J and K matrices from the density elements
   ! in P, and the two electron integrals calculated from self.
     IN :: self
     REALMAT(:,:), INOUT :: J
     REALMAT(:,:), IN :: P
     REAL, IN :: factor
     INT, IN :: fa,la,fb,lb,fc,lc,fd,ld
     INT :: opt
     STACK("SHELL1QUARTET:make_r_J")
     START_TIMER("SHELL1QUARTET:make_r_J")
     opt = 0
     if (self%d%l==0) opt = opt + 1
     if (self%c%l==0) opt = opt + 2
     if (self%b%l==0) opt = opt + 4
     if (self%a%l==0) opt = opt + 8
     select case (opt)
       case (0);  call make_r_J_abcd_(self,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld) ! abcd
       case (1);  call make_r_J_abcs_(self,J,P,factor,fa,la,fb,lb,fc,lc,fd) ! abcs
       case (2);  call make_r_J_absd_(self,J,P,factor,fa,la,fb,lb,fc,fd,ld) ! absd
       case (3);  call make_r_J_abss_(self,J,P,factor,fa,la,fb,lb,fc,fd) ! abss
       case (4);  call make_r_J_ascd_(self,J,P,factor,fa,la,fb,fc,lc,fd,ld) ! ascd
       case (5);  call make_r_J_ascs_(self,J,P,factor,fa,la,fb,fc,lc,fd) ! ascs
       case (6);  call make_r_J_assd_(self,J,P,factor,fa,la,fb,fc,fd,ld) ! assd
       case (7);  call make_r_J_asss_(self,J,P,factor,fa,la,fb,fc,fd) ! asss
       case (8);  call make_r_J_sbcd_(self,J,P,factor,fa,fb,lb,fc,lc,fd,ld) ! sbcd
       case (9);  call make_r_J_sbcs_(self,J,P,factor,fa,fb,lb,fc,lc,fd) ! sbcs
       case (10); call make_r_J_sbsd_(self,J,P,factor,fa,fb,lb,fc,fd,ld) ! sbsd
       case (11); call make_r_J_sbss_(self,J,P,factor,fa,fb,lb,fc,fd) ! sbss
       case (12); call make_r_J_sscd_(self,J,P,factor,fa,fb,fc,lc,fd,ld) ! sscd
       case (13); call make_r_J_sscs_(self,J,P,factor,fa,fb,fc,lc,fd) ! sscs
       case (14); call make_r_J_sssd_(self,J,P,factor,fa,fb,fc,fd,ld) ! sssd
       case (15); call make_r_J_ssss_(self,J,P,factor,fa,fb,fc,fd) ! ssss
     end select
     STOP_TIMER("SHELL1QUARTET:make_r_J")
      CHECK
   end subroutine

   subroutine make_r_J_ssss(self,J,P,factor,fa,fb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an ssss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,fc,fd
     REAL, IN :: factor
     REAL :: I0,I_abcd
     STACK("SHELL1QUARTET:make_r_J_ssss")
     START_TIMER("SHELL1QUARTET:make_r_J_ssss")
     call make_ssss_(self,I0)
     I_abcd = factor * I0
     J(fa,fb) = J(fa,fb) + I_abcd*P(fd,fc)
     J(fc,fd) = J(fc,fd) + I_abcd*P(fb,fa)
     STOP_TIMER("SHELL1QUARTET:make_r_J_ssss")
      CHECK
   end subroutine

   subroutine make_r_J_abcd(self,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For any shell4!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), target :: J
     INT, IN :: fa,la,fb,lb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT4(:,:,:,:), PTR :: I4
     REAL :: P_dc,Jcd,I_abcd
     INT :: a,b,c,d
     STACK("SHELL1QUARTET:make_r_J_abcd")
     START_TIMER("SHELL1QUARTET:make_r_J_abcd")
     call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
     if (self%a%l==1 AND self%b%l==1 AND self%c%l==1 AND self%d%l==1) then
       call make_pppp_(self,I4)
     else
       call make_abcd_(self,I4)
     end if
     if (factor > 0.9) then
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           Jcd = ZERO
           do b = fb,lb
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Jcd    = Jcd    + I_abcd*P(b,a)
             end do
           end do
           J(c,d) = J(c,d) + Jcd
         end do
       end do
     else if (self%a%l<2) then
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           Jcd = ZERO
           do b = fb,lb
             do a = fa,la
               I_abcd = factor*I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Jcd    = Jcd    + I_abcd*P(b,a)
             end do
           end do
           J(c,d) = J(c,d) + Jcd
         end do
       end do
     else
       do d = fd,ld
         do c = fc,lc
           P_dc = factor*P(d,c)
           Jcd = ZERO
           do b = fb,lb
             do a = fa,la
               I_abcd = I4(a,b,c,d)
               J(a,b) = J(a,b) + I_abcd*P_dc
               Jcd    = Jcd    + I_abcd*P(b,a)
             end do
           end do
           J(c,d) = J(c,d) + factor*Jcd
         end do
       end do
     end if
     call destroy_(I4)
     STOP_TIMER("SHELL1QUARTET:make_r_J_abcd")
      CHECK
   end subroutine

   subroutine make_r_J_sscd(self,J,P,factor,fa,fb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an sscd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_ba,Jab,I_abcd
     INT :: c,d
     STACK("SHELL1QUARTET:make_r_J_sscd")
     START_TIMER("SHELL1QUARTET:make_r_J_sscd")
     call create_(I2,fc,lc,fd,ld)
     if (self%c%l==1 AND self%d%l==1) then
       call make_sspp_(self,I2)
     else
       call make_sscd_(self,I2)
     end if
     if (factor > 0.9) then
       Jab = ZERO
       P_ba = P(fb,fa)
       do d = fd,ld
         do c = fc,lc
           I_abcd = I2(c,d)
           Jab     = Jab     + I_abcd*P(d,c)
           J(c,d)  = J(c,d)  + I_abcd*P_ba
         end do
       end do
       J(fa,fb) = J(fa,fb) + Jab
     else if (self%c%l<2) then
       Jab = ZERO
       P_ba = P(fb,fa)
       do d = fd,ld
         do c = fc,lc
           I_abcd = factor*I2(c,d)
           Jab     = Jab     + I_abcd*P(d,c)
           J(c,d)  = J(c,d)  + I_abcd*P_ba
         end do
       end do
       J(fa,fb) = J(fa,fb) + Jab
     else
       Jab = ZERO
       P_ba = factor*P(fb,fa)
       do d = fd,ld
         do c = fc,lc
           I_abcd = I2(c,d)
           Jab     = Jab     + I_abcd*P(d,c)
           J(c,d)  = J(c,d)  + I_abcd*P_ba
         end do
       end do
       J(fa,fb) = J(fa,fb) + factor*Jab
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_J_sscd")
      CHECK
   end subroutine

   subroutine make_r_J_abcs(self,J,P,factor,fa,la,fb,lb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an abcs shell4!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,lb,fc,lc,fd
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: P_dc,Jcd,I_abcd
     INT :: a,b,c
     STACK("SHELL1QUARTET:make_r_J_abcs")
     START_TIMER("SHELL1QUARTET:make_r_J_abcs")
     call create_(I3,fa,la,fb,lb,fc,lc)
     if (self%a%l==1 AND self%b%l==1 AND self%c%l==1) then
       call make_ppps_(self,I3)
     else
       call make_abcs_(self,I3)
     end if
     if (factor > 0.9) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           do a = fa,la
             I_abcd  = I3(a,b,c)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
           end do
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else if (self%a%l<2) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           do a = fa,la
             I_abcd  = factor*I3(a,b,c)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
           end do
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else
       do c = fc,lc
         P_dc = factor*P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           do a = fa,la
             I_abcd  = I3(a,b,c)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
           end do
         end do
         J(c,fd) = J(c,fd) + factor*Jcd
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_J_abcs")
      CHECK
   end subroutine

   subroutine make_r_J_absd(self,J,P,factor,fa,la,fb,lb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an absd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,lb,fc,fd,ld
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: P_dc,Jcd,I_abcd
     INT :: a,b,d
     STACK("SHELL1QUARTET:make_r_J_absd")
     START_TIMER("SHELL1QUARTET:make_r_J_absd")
     call create_(I3,fa,la,fb,lb,fd,ld)
     if (self%a%l==1 AND self%b%l==1 AND self%d%l==1) then
       call make_ppps_(self,I3)
     else
       call make_abcs_(self,I3)
     end if
     if (factor > 0.9) then
       do d = fd,ld
         P_dc = P(d,fc)
         Jcd = ZERO
         do b = fb,lb
           do a = fa,la
             I_abcd  = I3(a,b,d)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
           end do
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else if (self%a%l<2) then
       do d = fd,ld
         P_dc = P(d,fc)
         Jcd = ZERO
         do b = fb,lb
           do a = fa,la
             I_abcd  = factor*I3(a,b,d)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
           end do
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else
       do d = fd,ld
         P_dc = factor*P(d,fc)
         Jcd = ZERO
         do b = fb,lb
           do a = fa,la
             I_abcd  = I3(a,b,d)
             J(a,b)  = J(a,b)  + I_abcd*P_dc
             Jcd     = Jcd     + I_abcd*P(b,a)
           end do
         end do
         J(fc,d) = J(fc,d) + factor*Jcd
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_J_absd")
      CHECK
   end subroutine

   subroutine make_r_J_sbcd(self,J,P,factor,fa,fb,lb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a spcd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,lb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: Jab,P_ba,P_da,P_db,I_abcd
     INT :: b,c,d
     STACK("SHELL1QUARTET:make_r_J_sbcd")
     START_TIMER("SHELL1QUARTET:make_r_J_sbcd")
     call create_(I3,fb,lb,fc,lc,fd,ld)
     if (self%b%l==1 AND self%c%l==1 AND self%d%l==1) then
       call make_pspp_(self,I3)
     else
       call make_ascd_(self,I3)
     end if
     if (factor > 0.9) then
       do b=fb,lb
         Jab = ZERO
         P_ba = P(b,fa)
         do d = fd,ld
           P_da = P(d,fa)
           P_db = P(d,b)
           do c = fc,lc
             I_abcd  = I3(b,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
           end do
         end do
         J(fa,b) = J(fa,b) + Jab
       end do
     else if (self%c%l<2) then
       do b=fb,lb
         Jab = ZERO
         P_ba = P(b,fa)
         do d = fd,ld
           P_da = P(d,fa)
           P_db = P(d,b)
           do c = fc,lc
             I_abcd  = factor*I3(b,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
           end do
         end do
         J(fa,b) = J(fa,b) + Jab
       end do
     else
       do b=fb,lb
         Jab = ZERO
         P_ba = factor*P(b,fa)
         do d = fd,ld
           P_da = factor*P(d,fa)
           P_db = factor*P(d,b)
           do c = fc,lc
             I_abcd  = I3(b,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
           end do
         end do
         J(fa,b) = J(fa,b) + factor*Jab
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_J_sbcd")
      CHECK
   end subroutine

   subroutine make_r_J_ascd(self,J,P,factor,fa,la,fb,fc,lc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a pscd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT3(:,:,:), PTR :: I3
     REAL :: Jab,P_ba,I_abcd
     INT :: a,c,d
     STACK("SHELL1QUARTET:make_r_J_ascd")
     START_TIMER("SHELL1QUARTET:make_r_J_ascd")
     call create_(I3,fa,la,fc,lc,fd,ld)
     if (self%a%l==1 AND self%c%l==1 AND self%d%l==1) then
       call make_pspp_(self,I3)
     else
       call make_ascd_(self,I3)
     end if
     if (factor > 0.9) then
       do a=fa,la
         Jab = ZERO
         P_ba = P(fb,a)
         do d = fd,ld
           do c = fc,lc
             I_abcd  = I3(a,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
           end do
         end do
         J(a,fb) = J(a,fb) + Jab
       end do
     else if (self%c%l<2) then
       do a=fa,la
         Jab = ZERO
         P_ba = P(fb,a)
         do d = fd,ld
           do c = fc,lc
             I_abcd  = factor*I3(a,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
           end do
         end do
         J(a,fb) = J(a,fb) + Jab
       end do
     else
       do a=fa,la
         Jab = ZERO
         P_ba = factor*P(fb,a)
         do d = fd,ld
           do c = fc,lc
             I_abcd  = I3(a,c,d)
             Jab     = Jab     + I_abcd*P(d,c)
             J(c,d)  = J(c,d)  + I_abcd*P_ba
           end do
         end do
         J(a,fb) = J(a,fb) + factor*Jab
       end do
     end if
     call destroy_(I3)
     STOP_TIMER("SHELL1QUARTET:make_r_J_ascd")
      CHECK
   end subroutine

   subroutine make_r_J_abss(self,J,P,factor,fa,la,fb,lb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an abss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,lb,fc,fd
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,I_abcd
     INT :: a,b
     STACK("SHELL1QUARTET:make_r_J_abss")
     START_TIMER("SHELL1QUARTET:make_r_J_abss")
     call create_(I2,fa,la,fb,lb)
     if (self%a%l==1 AND self%b%l==1) then
       call make_ppss_(self,I2)
     else
       call make_abss_(self,I2)
     end if
     if (factor > 0.9) then
       Jcd = ZERO
       P_dc = P(fd,fc)
       do b = fb,lb
         do a = fa,la
           I_abcd = I2(a,b)
           J(a,b) = J(a,b) + I_abcd*P_dc
           Jcd    = Jcd    + I_abcd*P(b,a)
         end do
       end do
       J(fc,fd) = J(fc,fd) + Jcd
     else if (self%a%l<2) then
       Jcd = ZERO
       P_dc = P(fd,fc)
       do b = fb,lb
         do a = fa,la
           I_abcd = factor*I2(a,b)
           J(a,b) = J(a,b) + I_abcd*P_dc
           Jcd    = Jcd    + I_abcd*P(b,a)
         end do
       end do
       J(fc,fd) = J(fc,fd) + Jcd
     else
       Jcd = ZERO
       P_dc = factor*P(fd,fc)
       do b = fb,lb
         do a = fa,la
           I_abcd = I2(a,b)
           J(a,b) = J(a,b) + I_abcd*P_dc
           Jcd    = Jcd    + I_abcd*P(b,a)
         end do
       end do
       J(fc,fd) = J(fc,fd) + factor*Jcd
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_J_abss")
      CHECK
   end subroutine

   subroutine make_r_J_ascs(self,J,P,factor,fa,la,fb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a ascs shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,fc,lc,fd
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,I_abcd
     INT :: a,c
     STACK("SHELL1QUARTET:make_r_J_ascs")
     START_TIMER("SHELL1QUARTET:make_r_J_ascs")
     call create_(I2,fa,la,fc,lc)
     if (self%a%l==1 AND self%c%l==1) then
       call make_psps_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do a = fa,la
           I_abcd  = I2(a,c)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           Jcd     = Jcd     + I_abcd*P(fb,a)
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else if (self%a%l<2) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do a = fa,la
           I_abcd  = factor*I2(a,c)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           Jcd     = Jcd     + I_abcd*P(fb,a)
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else
       do c = fc,lc
         P_dc = factor*P(fd,c)
         Jcd = ZERO
         do a = fa,la
           I_abcd  = I2(a,c)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
           Jcd     = Jcd     + I_abcd*P(fb,a)
         end do
         J(c,fd) = J(c,fd) + factor*Jcd
       end do
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_J_ascs")
      CHECK
   end subroutine

   subroutine make_r_J_assd(self,J,P,factor,fa,la,fb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a assd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,fc,fd,ld
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,I_abcd
     INT :: a,d
     STACK("SHELL1QUARTET:make_r_J_assd")
     START_TIMER("SHELL1QUARTET:make_r_J_assd")
     call create_(I2,fa,la,fd,ld)
     if (self%a%l==1 AND self%d%l==1) then
       call make_psps_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       do d = fd,ld
         P_dc = P(d,fc)
         Jcd = ZERO
         do a = fa,la
           I_abcd  = I2(a,d)
           Jcd     = Jcd     + I_abcd*P(fb,a)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else if (self%a%l<2) then
       do d = fd,ld
         P_dc = P(d,fc)
         Jcd = ZERO
         do a = fa,la
           I_abcd  = factor*I2(a,d)
           Jcd     = Jcd     + I_abcd*P(fb,a)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else
       do d = fd,ld
         P_dc = factor*P(d,fc)
         Jcd = ZERO
         do a = fa,la
           I_abcd  = I2(a,d)
           Jcd     = Jcd     + I_abcd*P(fb,a)
           J(a,fb) = J(a,fb) + I_abcd*P_dc
         end do
         J(fc,d) = J(fc,d) + factor*Jcd
       end do
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_J_assd")
      CHECK
   end subroutine

   subroutine make_r_J_sbcs(self,J,P,factor,fa,fb,lb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a sbcs shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,lb,fc,lc,fd
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_dc,Jcd,I_abcd
     INT :: b,c
     STACK("SHELL1QUARTET:make_r_J_sbcs")
     START_TIMER("SHELL1QUARTET:make_r_J_sbcs")
     call create_(I2,fb,lb,fc,lc)
     if (self%b%l==1 AND self%c%l==1) then
       call make_psps_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           I_abcd  = I2(b,c)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else if (self%b%l<2) then
       do c = fc,lc
         P_dc = P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           I_abcd  = factor*I2(b,c)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
         end do
         J(c,fd) = J(c,fd) + Jcd
       end do
     else
       do c = fc,lc
         P_dc = factor*P(fd,c)
         Jcd = ZERO
         do b = fb,lb
           I_abcd  = I2(b,c)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
         end do
         J(c,fd) = J(c,fd) + factor*Jcd
       end do
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_J_sbcs")
      CHECK
   end subroutine

   subroutine make_r_J_sbsd(self,J,P,factor,fa,fb,lb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a sbsd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,lb,fc,fd,ld
     REAL, IN :: factor
     REALMAT(:,:), PTR :: I2
     REAL :: P_ca,Jcd,P_dc,P_da,I_abcd
     INT :: b,d
     STACK("SHELL1QUARTET:make_r_J_sbsd")
     START_TIMER("SHELL1QUARTET:make_r_J_sbsd")
     call create_(I2,fb,lb,fd,ld)
     if (self%b%l==1 AND self%d%l==1) then
       call make_psps_(self,I2)
     else
       call make_ascs_(self,I2)
     end if
     if (factor > 0.9) then
       P_ca = P(fc,fa)
       do d = fd,ld
         Jcd = ZERO
         P_dc = P(d,fc)
         P_da = P(d,fa)
         do b = fb,lb
           I_abcd  = I2(b,d)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else if (self%b%l<2) then
       P_ca = P(fc,fa)
       do d = fd,ld
         Jcd = ZERO
         P_dc = P(d,fc)
         P_da = P(d,fa)
         do b = fb,lb
           I_abcd  = factor*I2(b,d)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
         end do
         J(fc,d) = J(fc,d) + Jcd
       end do
     else
       P_ca = factor*P(fc,fa)
       do d = fd,ld
         Jcd = ZERO
         P_dc = factor*P(d,fc)
         P_da = factor*P(d,fa)
         do b = fb,lb
           I_abcd  = I2(b,d)
           Jcd     = Jcd     + I_abcd*P(b,fa)
           J(fa,b) = J(fa,b) + I_abcd*P_dc
         end do
         J(fc,d) = J(fc,d) + factor*Jcd
       end do
     end if
     call destroy_(I2)
     STOP_TIMER("SHELL1QUARTET:make_r_J_sbsd")
      CHECK
   end subroutine

   subroutine make_r_J_asss(self,J,P,factor,fa,la,fb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a asss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,la,fb,fc,fd
     REAL, IN :: factor
     REALVEC(fa:la) :: I
     REAL :: P_dc,Jcd,I_abcd
     INT :: a
     STACK("SHELL1QUARTET:make_r_J_asss")
     START_TIMER("SHELL1QUARTET:make_r_J_asss")
     if (self%a%l==1) then
       call make_psss_(self,I)
       P_dc = P(fd,fc)
       Jcd = ZERO
       do a = fa,la
         I_abcd  = factor*I(a)
         Jcd     = Jcd     + I_abcd*P(fb,a)
         J(a,fb) = J(a,fb) + I_abcd*P_dc
       end do
       J(fc,fd) = J(fc,fd) + Jcd
     else
       if (self%a%l==2) then
         call make_dsss_(self,I)
       else
         call make_asss_(self,I)
       end if
       P_dc = factor*P(fd,fc)
       Jcd = ZERO
       do a = fa,la
         I_abcd  = I(a)
         Jcd     = Jcd     + I_abcd*P(fb,a)
         J(a,fb) = J(a,fb) + I_abcd*P_dc
       end do
       J(fc,fd) = J(fc,fd) + factor*Jcd
     end if
     STOP_TIMER("SHELL1QUARTET:make_r_J_asss")
      CHECK
   end subroutine

   subroutine make_r_J_sbss(self,J,P,factor,fa,fb,lb,fc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For a sbss shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,lb,fc,fd
     REAL, IN :: factor
     REALVEC(fb:lb) :: I
     REAL :: Jcd,P_dc,I_abcd
      INT :: b
     STACK("SHELL1QUARTET:make_r_J_sbss")
     START_TIMER("SHELL1QUARTET:make_r_J_sbss")
     if (self%b%l==1) then
       call make_psss_(self,I)
       P_dc = P(fd,fc)
       Jcd = ZERO
       do b = fb,lb
         I_abcd  = factor*I(b)
         Jcd     = Jcd     + I_abcd*P(b,fa)
         J(fa,b) = J(fa,b) + I_abcd*P_dc
       end do
       J(fc,fd) = J(fc,fd) + Jcd
     else
       if (self%b%l==2) then
         call make_dsss_(self,I)
       else
         call make_asss_(self,I)
       end if
       P_dc = factor*P(fd,fc)
       Jcd = ZERO
       do b = fb,lb
         I_abcd  = I(b)
         Jcd     = Jcd     + I_abcd*P(b,fa)
         J(fa,b) = J(fa,b) + I_abcd*P_dc
       end do
       J(fc,fd) = J(fc,fd) + factor*Jcd
     end if
     STOP_TIMER("SHELL1QUARTET:make_r_J_sbss")
      CHECK
   end subroutine

   subroutine make_r_J_sscs(self,J,P,factor,fa,fb,fc,lc,fd)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an sscs shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,fc,lc,fd
     REAL, IN :: factor
     REALVEC(:), PTR :: I
     REAL :: P_ba,Jab,I_abcd
     INT :: c
     STACK("SHELL1QUARTET:make_r_J_sscs")
     START_TIMER("SHELL1QUARTET:make_r_J_sscs")
     call create_(I,fc,lc)
     if (self%c%l==1) then
       call make_ssps_(self,I)
       P_ba = P(fb,fa)
       Jab = ZERO
       do c = fc,lc
         I_abcd = factor*I(c)
         Jab      = Jab     + I_abcd*P(fd,c)
         J(c,fd)  = J(c,fd) + I_abcd*P_ba
       end do
       J(fa,fb) = J(fa,fb) + Jab
     else
       if (self%c%l==2) then
         call make_ssds_(self,I)
       else
         call make_sscs_(self,I)
       end if
       P_ba = factor*P(fb,fa)
       Jab = ZERO
       do c = fc,lc
         I_abcd = I(c)
         Jab      = Jab     + I_abcd*P(fd,c)
         J(c,fd)  = J(c,fd) + I_abcd*P_ba
       end do
       J(fa,fb) = J(fa,fb) + factor*Jab
     end if
     call destroy_(I)
     STOP_TIMER("SHELL1QUARTET:make_r_J_sscs")
      CHECK
   end subroutine

   subroutine make_r_J_sssd(self,J,P,factor,fa,fb,fc,fd,ld)
    SHELL1QUARTET :: self
   ! Make the J contribution due to self and P and add it in.
   ! For an sssd shell4 only!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), INOUT :: J
     INT, IN :: fa,fb,fc,fd,ld
     REAL, IN :: factor
     REALVEC(:), PTR :: I
     REAL :: Jab,P_ba,I_abcd
     INT :: d
     STACK("SHELL1QUARTET:make_r_J_sssd")
     START_TIMER("SHELL1QUARTET:make_r_J_sssd")
     call create_(I,fd,ld)
     if (self%d%l==1) then
       call make_ssps_(self,I)
       P_ba = P(fb,fa)
       Jab = ZERO
       do d = fd,ld
         I_abcd = factor*I(d)
         Jab     = Jab     + I_abcd*P(d,fc)
         J(fc,d) = J(fc,d) + I_abcd*P_ba
       end do
       J(fa,fb) = J(fa,fb) + Jab
     else
       if (self%d%l==2) then
         call make_ssds_(self,I)
       else
         call make_sscs_(self,I)
       end if
       P_ba = factor*P(fb,fa)
       Jab = ZERO
       do d = fd,ld
         I_abcd = I(d)
         Jab     = Jab     + I_abcd*P(d,fc)
         J(fc,d) = J(fc,d) + I_abcd*P_ba
       end do
       J(fa,fb) = J(fa,fb) + factor*Jab
     end if
     call destroy_(I)
     STOP_TIMER("SHELL1QUARTET:make_r_J_sssd")
      CHECK
   end subroutine

!*******************************************************************************
!  Output Routines.
!*******************************************************************************

   subroutine put(self)
    SHELL1QUARTET :: self
   ! Put the shell4 information to file "out"
     INT :: n_cc,i
     STACK("SHELL1QUARTET:put")
     START_TIMER("SHELL1QUARTET:put")
     call flush_(stdout)
     call show_(stdout,"A shell l quantum number =",self%a%l)
     call show_(stdout,"B shell l quantum number =",self%b%l)
     call show_(stdout,"C shell l quantum number =",self%c%l)
     call show_(stdout,"D shell l quantum number =",self%d%l)
     call show_(stdout,"A position               =",self%pos_a)
     call show_(stdout,"B position               =",self%pos_b)
     call show_(stdout,"C position               =",self%pos_c)
     call show_(stdout,"D position               =",self%pos_d)
     call flush_(stdout)
     call dash_(stdout,int_fields=1,real_fields=8)
     call put_(stdout,"N", int_width=TRUE)
     call put_(stdout,"ex_a")
     call put_(stdout,"cc_a")
     call put_(stdout,"ex_b")
     call put_(stdout,"cc_b")
     call put_(stdout,"ex_c")
     call put_(stdout,"cc_c")
     call put_(stdout,"ex_d")
     call put_(stdout,"cc_d")
     call flush_(stdout)
     call dash_(stdout,int_fields=1,real_fields=8)
     n_cc = max(self%a%n_cc,self%b%n_cc,self%c%n_cc,self%d%n_cc)
     do i = 1,n_cc
        call put_(stdout,i)
        if (i<=self%a%n_cc) then
        call put_(stdout, self%a%ex(i))
        call put_(stdout, self%a%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        if (i<=self%b%n_cc) then
        call put_(stdout, self%b%ex(i))
        call put_(stdout, self%b%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        if (i<=self%c%n_cc) then
        call put_(stdout, self%c%ex(i))
        call put_(stdout, self%c%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        if (i<=self%d%n_cc) then
        call put_(stdout, self%d%ex(i))
        call put_(stdout, self%d%cc(i))
        else
        call tab_(stdout,real_fields=2)
        end if
        call flush_(stdout)
     end do
     call dash_(stdout,int_fields=1,real_fields=8)
     STOP_TIMER("SHELL1QUARTET:put")
      CHECK
   end subroutine

end
