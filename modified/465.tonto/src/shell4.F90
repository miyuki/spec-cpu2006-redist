!
! Copyright (C) Daniel Grimwood, March 1998
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
! $Id: shell4.foo,v 1.49.2.6 2003/11/13 05:33:21 reaper Exp $
!*******************************************************************************

module SHELL4_MODULE

#  include "shell4.use"

  implicit none

#  include "macros"
#  include "shell4.int"


contains

!*******************************************************************************
!  Create/Destroy routines.
!*******************************************************************************

   subroutine create(self)
    SHELL4 :: self
   ! Create a shell4 object, but no its component shells.
     PTR :: self
     STACK("SHELL4:create")
     START_TIMER("SHELL4:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(SHELL4_SIZE)
     call nullify_ptr_part_(self)
     STOP_TIMER("SHELL4:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,shell_a,shell_b,shell_c,shell_d)
    SHELL4 :: self
   ! Create a shell4 object from copies of shell1s.
     PTR :: self
     SHELL1, IN :: shell_a,shell_b,shell_c,shell_d
     STACK("SHELL4:create_1")
     START_TIMER("SHELL4:create_1")
     call create_(self)
     call copy_(self,shell_a,shell_b,shell_c,shell_d)
     STOP_TIMER("SHELL4:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    SHELL4 :: self
   ! Create a shell4 object from copies of shells and their positions.
     PTR :: self
     SHELL, IN :: shell_a,shell_b,shell_c,shell_d
     REALVEC(3), IN :: pos_a,pos_b,pos_c,pos_d
     STACK("SHELL4:create_2")
     START_TIMER("SHELL4:create_2")
     call create_(self)
     call copy_(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
     STOP_TIMER("SHELL4:create_2")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SHELL4 :: self
   ! Destroy a shell4 object.
     PTR :: self
     STACK("SHELL4:destroy")
     START_TIMER("SHELL4:destroy")
     if (associated(self)) then
       call destroy_ptr_part_(self)
       DELETE_MEMORY(SHELL4_SIZE)
       deallocate(self)
     end if
     STOP_TIMER("SHELL4:destroy")
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
    SHELL4 :: self
   ! Nullify the pointer parts of self
     STACK("SHELL4:nullify_ptr_part")
     START_TIMER("SHELL4:nullify_ptr_part")
     call nullify_ptr_part_(self%a)
     call nullify_ptr_part_(self%b)
     call nullify_ptr_part_(self%c)
     call nullify_ptr_part_(self%d)
     STOP_TIMER("SHELL4:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SHELL4 :: self
   ! Destroy the pointer parts of self
     STACK("SHELL4:destroy_ptr_part")
     START_TIMER("SHELL4:destroy_ptr_part")
     call destroy_ab_(self)
     call destroy_cd_(self)
     STOP_TIMER("SHELL4:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine destroy_ab(self)
    SHELL4 :: self
   ! Destroy the shella and shellb pointer parts of self
     STACK("SHELL4:destroy_ab")
     START_TIMER("SHELL4:destroy_ab")
     call destroy_ptr_part_(self%a)
     call destroy_ptr_part_(self%b)
     STOP_TIMER("SHELL4:destroy_ab")
      UNSTACK
   end subroutine

   subroutine destroy_cd(self)
    SHELL4 :: self
   ! Destroy the shellc and shelld pointer parts of self
     STACK("SHELL4:destroy_cd")
     START_TIMER("SHELL4:destroy_cd")
     call destroy_ptr_part_(self%c)
     call destroy_ptr_part_(self%d)
     STOP_TIMER("SHELL4:destroy_cd")
      UNSTACK
   end subroutine

   subroutine create_copy(self,shell)
    SHELL4 :: self
   ! Create a copy of "shell"
     PTR :: self
     SHELL4, IN :: shell
     STACK("SHELL4:create_copy")
     START_TIMER("SHELL4:create_copy")
     call create_(self)
     call copy_(self,shell)
     STOP_TIMER("SHELL4:create_copy")
      UNSTACK
   end subroutine

!*******************************************************************************
!  Setting parts of self from other shells and shell1s.
!*******************************************************************************

   subroutine copy(self,shell)
    SHELL4 :: self
   ! Make a copy of "shell"
     SHELL4, IN :: shell
     STACK("SHELL4:copy")
     START_TIMER("SHELL4:copy")
     call copy_(self%a,shell%a)
     call copy_(self%b,shell%b)
     call copy_(self%c,shell%c)
     call copy_(self%d,shell%d)
     STOP_TIMER("SHELL4:copy")
      UNSTACK
   end subroutine

   subroutine copy_1(self,shell_a,shell_b,shell_c,shell_d)
    SHELL4 :: self
   ! Copy the shell4 using from shell1 objects
     SHELL1, IN :: shell_a,shell_b,shell_c,shell_d
     STACK("SHELL4:copy_1")
     START_TIMER("SHELL4:copy_1")
     call copy_(self%a,shell_a)
     call copy_(self%b,shell_b)
     call copy_(self%c,shell_c)
     call copy_(self%d,shell_d)
     STOP_TIMER("SHELL4:copy_1")
      UNSTACK
   end subroutine

   subroutine copy_2(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    SHELL4 :: self
   ! Set the shell4 using shell objects and positions
     SHELL, IN :: shell_a,shell_b,shell_c,shell_d
     REALVEC(:), IN :: pos_a,pos_b,pos_c,pos_d
     STACK("SHELL4:copy_2")
     START_TIMER("SHELL4:copy_2")
     call copy_(self%a,shell_a,pos_a)
     call copy_(self%b,shell_b,pos_b)
     call copy_(self%c,shell_c,pos_c)
     call copy_(self%d,shell_d,pos_d)
     STOP_TIMER("SHELL4:copy_2")
      UNSTACK
   end subroutine

   subroutine set(self,shell_a,shell_b,shell_c,shell_d)
    SHELL4 :: self
   ! Set the shell4 using from shell1 objects
     SHELL1, IN :: shell_a,shell_b,shell_c,shell_d
     STACK("SHELL4:set")
     START_TIMER("SHELL4:set")
     call set_(self%a,shell_a)
     call set_(self%b,shell_b)
     call set_(self%c,shell_c)
     call set_(self%d,shell_d)
     STOP_TIMER("SHELL4:set")
      CHECK
   end subroutine

   subroutine copy_ab(self,shell_a,shell_b,pos_a,pos_b)
    SHELL4 :: self
   ! Copy the a and b parts of the shell4 using from shell1 objects
     SHELL, IN :: shell_a,shell_b
     REALVEC(:), IN :: pos_a,pos_b
     STACK("SHELL4:copy_ab")
     START_TIMER("SHELL4:copy_ab")
     call copy_(self%a,shell_a,pos_a)
     call copy_(self%b,shell_b,pos_b)
     STOP_TIMER("SHELL4:copy_ab")
      UNSTACK
   end subroutine

   subroutine copy_cd(self,shell_c,shell_d,pos_c,pos_d)
    SHELL4 :: self
   ! Copy the c and d parts of the shell4 using from shell1 objects
     SHELL, IN :: shell_c,shell_d
     REALVEC(:), IN :: pos_c,pos_d
     STACK("SHELL4:copy_cd")
     START_TIMER("SHELL4:copy_cd")
     call copy_(self%c,shell_c,pos_c)
     call copy_(self%d,shell_d,pos_d)
     STOP_TIMER("SHELL4:copy_cd")
      UNSTACK
   end subroutine

   subroutine set_1(self,shell_a,shell_b,shell_c,shell_d,pos_a,pos_b,pos_c,pos_d)
    SHELL4 :: self
   ! Set the shell4 using shell objects and positions
     SHELL, IN :: shell_a,shell_b,shell_c,shell_d
     REALVEC(:), IN :: pos_a,pos_b,pos_c,pos_d
     STACK("SHELL4:set_1")
     START_TIMER("SHELL4:set_1")
     call set_(self%a,shell_a,pos_a)
     call set_(self%b,shell_b,pos_b)
     call set_(self%c,shell_c,pos_c)
     call set_(self%d,shell_d,pos_d)
     STOP_TIMER("SHELL4:set_1")
      CHECK
   end subroutine

!*******************************************************************************
!  Miscellaneous Routines.
!*******************************************************************************

   subroutine unnormalise(self)
    SHELL4 :: self
   ! Unnormalise each shell in this shell quartet
     STACK("SHELL4:unnormalise")
     START_TIMER("SHELL4:unnormalise")
     call unnormalise_(self%a)
     call unnormalise_(self%b)
     call unnormalise_(self%c)
     call unnormalise_(self%d)
     STOP_TIMER("SHELL4:unnormalise")
      CHECK
   end subroutine

!*******************************************************************************
!  ERI cutoffs
!*******************************************************************************

  PURE function ab_kappa_max(self) result(res)
    SHELL4 :: self
  ! Return the largest kappa_ab used in the Lindh integrals.
    IN :: self
    REAL :: res
    REALVEC(3) :: AB
    REAL :: b,b_cc,a,ab_inv,prefac,r2_ab
    INT :: bg,ag
    AB = self%b%pos - self%a%pos
    r2_ab = dot_product(AB,AB)
    res = ZERO
    do bg = 1,self%b%n_cc
      b      = self%b%ex(bg)
      b_cc   = self%b%cc(bg)
      do ag = 1,self%a%n_cc
        a = self%a%ex(ag)
        ab_inv = ONE/(a+b)
        prefac = b_cc*self%a%cc(ag) * ab_inv * sqrt(ab_inv) * exp(-a*b*r2_ab*ab_inv)
        res = max(res,prefac/(sqrt(ab_inv)*ab_inv))
      end do
    end do
    STOP_TIMER("SHELL4:ab_kappa_max")
  end function

  PURE function cd_kappa_max(self) result(res)
    SHELL4 :: self
  ! Return the largest kappa_cd used in the Lindh integrals.
    IN :: self
    REAL :: res
    REALVEC(3) :: CD
    REAL :: d,d_cc,c,cd_inv,prefac,r2_cd
    INT :: dg,cg
    CD = self%d%pos - self%c%pos
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
    STOP_TIMER("SHELL4:cd_kappa_max")
  end function

   PURE function skip_ERI(self) result(res)
    SHELL4 :: self
   ! Whether the ERI block will be less than a cutoff value.
     IN :: self
     BIN :: res
     res = (ab_kappa_max_(self)*cd_kappa_max_(self) < SHELL4_ERI_CUTOFF)
     STOP_TIMER("SHELL4:skip_ERI")
   end function

   PURE function skip_ERI_1(self,cutoff) result(res)
    SHELL4 :: self
   ! Whether the ERI block will be less than a cutoff value.
     IN :: self
     REAL, IN :: cutoff
     BIN :: res
     res = (ab_kappa_max_(self)*cd_kappa_max_(self) < cutoff)
     STOP_TIMER("SHELL4:skip_ERI_1")
   end function

!*******************************************************************************
!                            CADPAC-style integrals
!*******************************************************************************

  subroutine make_ERI_ints(self,I)
    SHELL4 :: self
  ! Make the ERI integral matrix, using Gauss-Hermite quadrature, like in
  ! CADPAC.
  ! This is not expected to be as efficient as Daniel's code, below!
  ! But probably much easier to understand.
    REALMAT4(:,:,:,:) :: I
    REALMAT4(:,:,:,:), PTR :: II
    GAUSSIAN4 :: G
    INT :: a,b,c,d
    STACK("SHELL4:make_ERI_ints")
    START_TIMER("SHELL4:make_ERI_ints")
    I = ZERO
    call create_(II,self%a%n_comp,self%b%n_comp,self%c%n_comp,self%d%n_comp)
    G%a%l   = self%a%l;   G%b%l   = self%b%l;   G%c%l   = self%c%l;   G%d%l   = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_ERI_ints_(G,II)
            I = I + II*self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
          end do
        end do
      end do
    end do
    call destroy_(II)
    call to_normalise_(self,I)
    STOP_TIMER("SHELL4:make_ERI_ints")
     CHECK
  end subroutine

  subroutine make_spin_orbit_ints(self,S,O)
    SHELL4 :: self
  ! Make the spin same orbit integrals, "Sx", ... , and the
  ! spin other orbit integrals "Ox", ... , using
  ! Gauss-Hermite quadrature. Probably not the best implementation.
    REALMAT5(:,:,:,:,:) :: S,O
    STACK("SHELL4:make_spin_orbit_ints")
    START_TIMER("SHELL4:make_spin_orbit_ints")
    call make_spin_orbit_ints_(self,S(:,:,:,:,1),S(:,:,:,:,2),S(:,:,:,:,3), &
                          O(:,:,:,:,1),O(:,:,:,:,2),O(:,:,:,:,3))
    STOP_TIMER("SHELL4:make_spin_orbit_ints")
     CHECK
  end subroutine

  subroutine make_spin_orbit_ints_1(self,Sx,Sy,Sz,Ox,Oy,Oz)
    SHELL4 :: self
  ! Make the spin same orbit integrals, "Sx", ... , and the
  ! spin other orbit integrals "Ox", ... , using
  ! Gauss-Hermite quadrature. Probably not the best implementation.
    REALMAT4(:,:,:,:) :: Sx,Sy,Sz,Ox,Oy,Oz
    REALMAT4(:,:,:,:), PTR :: SSx,SSy,SSz,OOx,OOy,OOz
    GAUSSIAN4 :: G
    INT :: a,b,c,d,na,nb,nc,nd
    REAL :: cc
    STACK("SHELL4:make_spin_orbit_ints_1")
    START_TIMER("SHELL4:make_spin_orbit_ints_1")
    Sx = ZERO; Sy = ZERO; Sz = ZERO
    Ox = ZERO; Oy = ZERO; Oz = ZERO
    na = self%a%n_comp; nb = self%b%n_comp; nc =self%c%n_comp; nd =self%d%n_comp
    call create_(SSx,na,nb,nc,nd); call create_(SSy,na,nb,nc,nd); call create_(SSz,na,nb,nc,nd)
    call create_(OOx,na,nb,nc,nd); call create_(OOy,na,nb,nc,nd); call create_(OOz,na,nb,nc,nd)
    G%a%l   = self%a%l;   G%b%l   = self%b%l;   G%c%l   = self%c%l;   G%d%l   = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_spin_orbit_ints_(G,SSx,SSy,SSz,OOx,OOy,OOz)
            cc = self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
            Sx = Sx + SSx*cc; Sy = Sy + SSy*cc; Sz = Sz + SSz*cc
            Ox = Ox + OOx*cc; Oy = Oy + OOy*cc; Oz = Oz + OOz*cc
          end do
        end do
      end do
    end do
    call destroy_(OOz); call destroy_(OOy); call destroy_(OOx)
    call destroy_(SSz); call destroy_(SSy); call destroy_(SSx)
    call to_normalise_(self,Sx); call to_normalise_(self,Sy); call to_normalise_(self,Sz)
    call to_normalise_(self,Ox); call to_normalise_(self,Oy); call to_normalise_(self,Oz)
    STOP_TIMER("SHELL4:make_spin_orbit_ints_1")
     CHECK
  end subroutine

  subroutine make_spin_spin_dipole_ints(self,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz)
    SHELL4 :: self
  ! Make the spin spin magnetic dipole integrals, "Dij"
  ! using Gauss-Hermite quadrature. For sure, not the
  ! best implementation, but where else will you get em', eh?
    REALMAT4(:,:,:,:) :: Dxx,Dyy,Dzz,Dxy,Dxz,Dyz
    REALMAT4(:,:,:,:), PTR :: Mxx,Myy,Mzz,Mxy,Mxz,Myz
    GAUSSIAN4 :: G
    INT :: a,b,c,d,na,nb,nc,nd
    REAL :: cc
    STACK("SHELL4:make_spin_spin_dipole_ints")
    START_TIMER("SHELL4:make_spin_spin_dipole_ints")
    Dxx = ZERO; Dyy = ZERO; Dzz = ZERO
    Dxy = ZERO; Dxz = ZERO; Dyz = ZERO
    na = self%a%n_comp; nb = self%b%n_comp; nc =self%c%n_comp; nd =self%d%n_comp
    call create_(Mxx,na,nb,nc,nd); call create_(Myy,na,nb,nc,nd); call create_(Mzz,na,nb,nc,nd)
    call create_(Mxy,na,nb,nc,nd); call create_(Mxz,na,nb,nc,nd); call create_(Myz,na,nb,nc,nd)
    G%a%l   = self%a%l;   G%b%l   = self%b%l;   G%c%l   = self%c%l;   G%d%l   = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_spin_spin_dipole_ints_(G,Mxx,Myy,Mzz,Mxy,Mxz,Myz)
            cc = self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
            Dxx = Dxx + Mxx*cc; Dyy = Dyy + Myy*cc; Dzz = Dzz + Mzz*cc
            Dxy = Dxy + Mxy*cc; Dxz = Dxz + Mxz*cc; Dyz = Dyz + Myz*cc
          end do
        end do
      end do
    end do
    call destroy_(Myz); call destroy_(Mxz); call destroy_(Mxy)
    call destroy_(Mzz); call destroy_(Myy); call destroy_(Mxx)
    call to_normalise_(self,Dxx); call to_normalise_(self,Dyy); call to_normalise_(self,Dzz)
    call to_normalise_(self,Dxy); call to_normalise_(self,Dxz); call to_normalise_(self,Dyz)
    STOP_TIMER("SHELL4:make_spin_spin_dipole_ints")
     CHECK
  end subroutine

  subroutine make_ERI_derivatives(self,AA,BB,CC,DD)
    SHELL4 :: self
  !
    REALMAT5(:,:,:,:,:), optional :: AA,BB,CC,DD
    REALMAT5(:,:,:,:,:), PTR :: AX,BX,CX,DX
    GAUSSIAN4 :: G
    INT :: a,b,c,d, na,nb,nc,nd
    REAL :: fac
    STACK("SHELL4:make_ERI_derivatives")
    START_TIMER("SHELL4:make_ERI_derivatives")
    G%a%l = self%a%l; G%b%l = self%b%l; G%c%l = self%c%l; G%d%l = self%d%l
    G%a%pos = self%a%pos; G%b%pos = self%b%pos; G%c%pos = self%c%pos; G%d%pos = self%d%pos
    na = self%a%n_comp; nb = self%b%n_comp; nc =self%c%n_comp; nd =self%d%n_comp
    if (present(AA)) then
      AA = ZERO; call create_(AX,na,nb,nc,nd,3)
    end if
    if (present(BB)) then
      BB = ZERO; call create_(BX,na,nb,nc,nd,3)
    end if
    if (present(CC)) then
      CC = ZERO; call create_(CX,na,nb,nc,nd,3)
    end if
    if (present(DD)) then
      DD = ZERO; call create_(DX,na,nb,nc,nd,3)
    end if
    do a = 1,self%a%n_cc
      do b = 1,self%b%n_cc
        do c = 1,self%c%n_cc
          do d = 1,self%d%n_cc
            G%a%ex = self%a%ex(a)
            G%b%ex = self%b%ex(b)
            G%c%ex = self%c%ex(c)
            G%d%ex = self%d%ex(d)
            call make_ERI_derivatives_(G,AA=AX,BB=BX,CC=CX,DD=DX)
            fac = self%a%cc(a)*self%b%cc(b)*self%c%cc(c)*self%d%cc(d)
            if (present(AA)) AA = AA + AX*fac
            if (present(BB)) BB = BB + BX*fac
            if (present(CC)) CC = CC + CX*fac
            if (present(DD)) DD = DD + DX*fac
          end do
        end do
      end do
    end do
    if (present(DD)) then
      call destroy_(DX); call to_normalise_(self,DD)
    end if
    if (present(CC)) then
      call destroy_(CX); call to_normalise_(self,CC)
    end if
    if (present(BB)) then
      call destroy_(BX); call to_normalise_(self,BB)
    end if
    if (present(AA)) then
      call destroy_(AX); call to_normalise_(self,AA)
    end if
    STOP_TIMER("SHELL4:make_ERI_derivatives")
     CHECK
  end subroutine

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
    SHELL4 :: self
  ! Makes the (ab|cd) integrals, summed over the primitives
  ! (uses the transfer equation to make (ab|cd) from (es|fs))
    IN :: self
    REALMAT4(:,:,:,:), OUT :: abcd
    REALMAT3(:,:,:), PTR :: escd
    REALMAT(:,:), PTR :: esfs
    INT :: eub,fub,ab_l_max,cd_l_max
    STACK("SHELL4:get_ERI")
    START_TIMER("SHELL4:get_ERI")
    nullify(esfs)
    nullify(escd)
    ab_l_max = max(self%a%l,self%b%l)-1
    cd_l_max = max(self%c%l,self%d%l)-1
    eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
    fub = n_comp_sum_((self%c%l+self%d%l)) - n_comp_sum_(cd_l_max)
    call create_(esfs,eub,fub)
    call make_esfs_(self,esfs)
    call create_(escd,eub, self%c%n_comp, self%d%n_comp)
    call transfer_cd_(self,esfs,escd)
    call transfer_ab_(self,escd,abcd)
    call destroy_(escd)
    call destroy_(esfs)
    call to_normalise_(self,abcd)
    STOP_TIMER("SHELL4:get_ERI")
     CHECK
  end subroutine

  subroutine make_esfs(self,esfs)
    SHELL4 :: self
  ! Makes the initial (es|fs) integrals, summed over the primitives
    IN :: self
    REALMAT(:,:), OUT :: esfs
    REALMAT3(:,:,:), PTR :: Ixa,Iya,Iza
    RYS, PTR :: rys1
    REALMAT(:,:), PTR :: Ix,Iy,Iz
    REALVEC(3) :: At,Ct,P,Q,PA,QC,QP,AB,CD
    REAL :: zeta,eta,xx,zinv,rho,einv,rho_zinv,rho_einv
    REAL :: ce,cf,bb,ce1,kappa_ab,kappa_cd,normab,norm,r2ab,r2cd
    REAL :: a_cc,b_cc,c_cc,d_cc,a_ex,b_ex,c_ex,d_ex
    REAL :: t2,t2_rz,t2_re,half_zinv,half_einv,f1_bb,f1_cf
    INT :: ag,bg,cg,dg,nroots,eub,fub,dim1,dim2
    INT :: e,f,e1,f1,fp1,ep1,n,i,n_sum,ab_l_sum,cd_l_sum,ab_l_max,cd_l_max

    STACK("SHELL4:make_esfs")
    START_TIMER("SHELL4:make_esfs")
    ab_l_sum = self%a%l + self%b%l
    cd_l_sum = self%c%l + self%d%l
    ab_l_max = max(self%a%l,self%b%l)-1
    cd_l_max = max(self%c%l,self%d%l)-1
    eub = n_comp_sum_(ab_l_sum) - n_comp_sum_(ab_l_max)
    fub = n_comp_sum_(cd_l_sum) - n_comp_sum_(cd_l_max)
    dim1 = ab_l_sum + 1
    dim2 = cd_l_sum + 1
    nroots = (dim1+dim2)/2

    ! number of elements to sum over
    n_sum = nroots*self%a%n_cc*self%b%n_cc*self%c%n_cc*self%d%n_cc
    call create_(Ixa,n_sum,dim1,dim2)
    call create_(Iya,n_sum,dim1,dim2)
    call create_(Iza,n_sum,dim1,dim2)

    AB = self%a%pos-self%b%pos
    CD = self%c%pos-self%d%pos
    r2ab = dot_product(AB,AB)
    r2cd = dot_product(CD,CD)
    ! Want position of shell1 with higher angular momentum.
    if (self%a%l > self%b%l) then; At = self%a%pos
    else;                  At = self%b%pos
    end if
    if (self%c%l > self%d%l) then; Ct = self%c%pos
    else;                  Ct = self%d%pos
    end if

    call create_(rys1,nroots)

    i = 0
    do ag = 1, self%a%n_cc
      a_cc = self%a%cc(ag)
      a_ex = self%a%ex(ag)
      do bg = 1, self%b%n_cc
        b_cc = self%b%cc(bg)
        b_ex = self%b%ex(bg)
        zeta = a_ex + b_ex
        zinv = ONE/zeta
        half_zinv = HALF * zinv
        kappa_ab = exp(-a_ex*b_ex*r2ab*zinv)
        normab = TWOPI5ON2 * b_cc*a_cc * zinv * sqrt(zinv) * kappa_ab
        P = (b_ex*self%b%pos + a_ex*self%a%pos) * zinv
        PA = P - At
        do cg = 1, self%c%n_cc
          c_cc = self%c%cc(cg)
          c_ex = self%c%ex(cg)
          do dg = 1, self%d%n_cc
            d_cc = self%d%cc(dg)
            d_ex = self%d%ex(dg)
            eta = c_ex + d_ex
            einv = ONE/eta
            half_einv = HALF * einv
            kappa_cd = exp(-c_ex*d_ex*r2cd*einv)
            norm = normab * d_cc*c_cc * einv * sqrt(einv) * kappa_cd
            Q = (d_ex*self%d%pos + c_ex*self%c%pos) * einv
            QC = Q - Ct
            rho  = zeta * eta / (zeta + eta)
            rho_zinv = rho * zinv
            rho_einv = rho * einv
            half_einv = HALF * einv
            QP = Q - P
            xx   = rho * dot_product(QP,QP)
            call get_weights_(rys1,xx)
            rys1%w(:) = rys1%w(:) * (norm * sqrt(rho))

            ! Now make the 2 dimensional integrals.
            do n=1,nroots
              i = i + 1
              Ix => Ixa(i,:,:)
              Iy => Iya(i,:,:)
              Iz => Iza(i,:,:)
              t2      = rys1%r(n)
              t2_rz   = t2 * rho_zinv
              t2_re   = t2 * rho_einv
              cf      = (ONE - t2_re) * half_einv
              ce      = (ONE - t2_rz) * half_zinv
              bb      = t2_rz * half_einv
              Ix(1,1) = ONE
              Iy(1,1) = ONE
              Iz(1,1) = ONE
              if (ab_l_sum>0) then
                Ix(2,1) = PA(1) + t2_rz * QP(1)
                Iy(2,1) = PA(2) + t2_rz * QP(2)
                Iz(2,1) = PA(3) + t2_rz * QP(3)
                do e = 2, ab_l_sum
                  e1 = e - 1
                  ep1 = e + 1
                  ce1 = (e-1) * ce
                  Ix(ep1,1) = Ix(2,1) * Ix(e,1) + ce1 * Ix(e1,1)
                  Iy(ep1,1) = Iy(2,1) * Iy(e,1) + ce1 * Iy(e1,1)
                  Iz(ep1,1) = Iz(2,1) * Iz(e,1) + ce1 * Iz(e1,1)
                end do
              end if
              if (cd_l_sum>0) then
                Ix(1,2) = QC(1) - t2_re * QP(1)
                Iy(1,2) = QC(2) - t2_re * QP(2)
                Iz(1,2) = QC(3) - t2_re * QP(3)
                do f = 2,cd_l_sum
                  f1 = f - 1
                  fp1   = f + 1
                  f1_cf = (f-1)*cf
                  Ix(1,fp1) = Ix(1,2) * Ix(1,f) + f1_cf * Ix(1,f1)
                  Iy(1,fp1) = Iy(1,2) * Iy(1,f) + f1_cf * Iy(1,f1)
                  Iz(1,fp1) = Iz(1,2) * Iz(1,f) + f1_cf * Iz(1,f1)
                end do
                if (ab_l_sum>0) then
                  Ix(2,2) = Ix(1,2) * Ix(2,1) + bb
                  Iy(2,2) = Iy(1,2) * Iy(2,1) + bb
                  Iz(2,2) = Iz(1,2) * Iz(2,1) + bb
                  do f = 2,cd_l_sum
                    f1 = f - 1
                    fp1   = f + 1
                    f1_cf = (f-1)*cf
                    Ix(2,fp1) = Ix(1,2) * Ix(2,f) + f1_cf * Ix(2,f1) + bb * Ix(1,f)
                    Iy(2,fp1) = Iy(1,2) * Iy(2,f) + f1_cf * Iy(2,f1) + bb * Iy(1,f)
                    Iz(2,fp1) = Iz(1,2) * Iz(2,f) + f1_cf * Iz(2,f1) + bb * Iz(1,f)
                  end do
                end if
              end if
              do e = 2, ab_l_sum
                e1  = e - 1
                ep1 = e + 1
                ce1 = (e-1) * ce
                do f=2, cd_l_sum+1
                  f1 = f - 1
                  f1_bb = (f-1)*bb
                  Ix(ep1,f) = Ix(2,1)*Ix(e,f) + ce1*Ix(e1,f) + f1_bb*Ix(e,f1)
                  Iy(ep1,f) = Iy(2,1)*Iy(e,f) + ce1*Iy(e1,f) + f1_bb*Iy(e,f1)
                  Iz(ep1,f) = Iz(2,1)*Iz(e,f) + ce1*Iz(e1,f) + f1_bb*Iz(e,f1)
                end do
              end do
              Iz = Iz * rys1%w(n)
            end do
          end do
        end do
      end do
    end do

    call destroy_(rys1)

    call form_3d_ints_(self,Ixa,Iya,Iza,esfs,eub,fub,n_sum)

    call destroy_(Iza)
    call destroy_(Iya)
    call destroy_(Ixa)
    STOP_TIMER("SHELL4:make_esfs")
     CHECK
  end subroutine

  subroutine form_3d_ints(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    SHELL4 :: self
  ! Forms (es|fs) from the two dimensional integrals, summed over primitives.
  ! This is the main routine, all the others are specialised and may break if
  ! given the wrong shell4.
    IN :: self
    REALMAT3(:,:,:), IN :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,fub,n_sum
    STACK("SHELL4:form_3d_ints")
    START_TIMER("SHELL4:form_3d_ints")
    if (min(self%a%l,self%b%l) < 3 AND min(self%c%l,self%d%l) < 3) then !s or p or d
      call form_3d_ints_no_rm_(self,Ix,Iy,Iz,esfs,eub,fub)
    else
      call form_3d_ints_rm_(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    end if
    STOP_TIMER("SHELL4:form_3d_ints")
     CHECK
  end subroutine

  subroutine form_3d_ints_rm(self,Ix,Iy,Iz,esfs,eub,fub,n_sum)
    SHELL4 :: self
  ! Forms the three dimensional integrals from the two dimensional integrals,
  ! summed over primitives.
  ! This version uses the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), IN :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,fub,n_sum
    INTVEC(:), PTR :: e_x,e_y,e_z,f_x,f_y,f_z,ii_e_ivec,ii_f_ivec
    REALMAT3(:,:,:), PTR :: Ief
    INT :: e,f,zf,yf,ze,ye,iie,iif,m
    INT :: dime,dimf,dime1,dimf1,dime2,dimf2

    STACK("SHELL4:form_3d_ints_rm")
    START_TIMER("SHELL4:form_3d_ints_rm")
    dime  = self%a%l+self%b%l+1
    dime1 = dime+1
    dime2 = 2*dime1+1
    dimf  = self%c%l+self%d%l+1
    dimf1 = dimf+1
    dimf2 = 2*dimf1+1

    call create_(e_x,eub);  call create_(e_y,eub);  call create_(e_z,eub)
    call create_(f_x,fub);  call create_(f_y,fub);  call create_(f_z,fub)
    m = max(self%a%l,self%b%l); call make_gaussian_xyz_indices_(m,e_x,e_y,e_z,self%a%l+self%b%l)
    m = max(self%c%l,self%d%l); call make_gaussian_xyz_indices_(m,f_x,f_y,f_z,self%c%l+self%d%l)

    call create_(ii_e_ivec,eub)
    do e=1,eub
      ze = e_z(e)
      ii_e_ivec(e) = -dime1 + ze*(dime2-ze)/2 + e_y(e)
    end do
    call create_(ii_f_ivec,fub)
    do f=1,fub
      zf = f_z(f)
      ii_f_ivec(f) = -dimf1 + zf*(dimf2-zf)/2 + f_y(f)
    end do

    ! Apply reduced multiplication scheme to Iy and Iz, store in triangle.
    call create_(Ief,n_sum,dime*dime1/2,dimf*dimf1/2)
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
    esfs = sum(Ix(:,e_x,f_x) * Ief(:,ii_e_ivec,ii_f_ivec),dim=1)
    call destroy_(Ief)

    call destroy_(ii_f_ivec)
    call destroy_(ii_e_ivec)
    call destroy_(f_z);  call destroy_(f_y);  call destroy_(f_x)
    call destroy_(e_z);  call destroy_(e_y);  call destroy_(e_x)
    STOP_TIMER("SHELL4:form_3d_ints_rm")
     CHECK
  end subroutine

  subroutine form_3d_ints_no_rm(self,Ix,Iy,Iz,esfs,eub,fub)
    SHELL4 :: self
  ! Forms the three dimensional integrals from the two dimensional integrals,
  ! summed over primitives.
  ! This version does not use the reduced multiplication scheme.
    IN :: self
    REALMAT3(:,:,:), IN :: Ix,Iy,Iz
    REALMAT(:,:), OUT :: esfs
    INT, IN :: eub,fub
    INT :: e,f,m
    INTVEC(:), PTR :: e_x,e_y,e_z,f_x,f_y,f_z

    STACK("SHELL4:form_3d_ints_no_rm")
    START_TIMER("SHELL4:form_3d_ints_no_rm")
    call create_(e_x,eub);  call create_(e_y,eub);  call create_(e_z,eub)
    call create_(f_x,fub);  call create_(f_y,fub);  call create_(f_z,fub)
    m = max(self%a%l,self%b%l); call make_gaussian_xyz_indices_(m,e_x,e_y,e_z,self%a%l+self%b%l)
    m = max(self%c%l,self%d%l); call make_gaussian_xyz_indices_(m,f_x,f_y,f_z,self%c%l+self%d%l)

    !esfs = sum(Ix(:,e_x,f_x) * Iy(:,e_y,f_y) * Iz(:,e_z,f_z),dim=1)
    do e=1,eub
      do f=1,fub
        esfs(e,f) = sum(Ix(:,e_x(e),f_x(f)) * Iy(:,e_y(e),f_y(f)) * Iz(:,e_z(e),f_z(f)))
      end do
    end do

    call destroy_(f_z);  call destroy_(f_y);  call destroy_(f_x)
    call destroy_(e_z);  call destroy_(e_y);  call destroy_(e_x)
    STOP_TIMER("SHELL4:form_3d_ints_no_rm")
     CHECK
  end subroutine

  subroutine transfer_cd(self,esfs,escd)
    SHELL4 :: self
  ! Applies the transfer equation to (es|fs) to give (es|cd)
    IN :: self
    REALMAT(:,:), IN :: esfs
    REALMAT3(:,:,:), OUT :: escd
    STACK("SHELL4:transfer_cd")
    START_TIMER("SHELL4:transfer_cd")
    if (self%c%l > self%d%l) then
      call transfer_l_c_highest_(self,esfs,escd)
    else
      call transfer_l_d_highest_(self,esfs,escd)
    end if
    STOP_TIMER("SHELL4:transfer_cd")
     CHECK
  end subroutine

  subroutine transfer_ab(self,escd,abcd)
    SHELL4 :: self
  ! Applies the transfer equation to (es|cd) to give (ab|cd)
    IN :: self
    REALMAT3(:,:,:), IN :: escd
    REALMAT4(:,:,:,:), OUT :: abcd
    STACK("SHELL4:transfer_ab")
    START_TIMER("SHELL4:transfer_ab")
    if (self%a%l > self%b%l) then
      call transfer_l_a_highest_(self,escd,abcd)
    else
      call transfer_l_b_highest_(self,escd,abcd)
    end if
    STOP_TIMER("SHELL4:transfer_ab")
     CHECK
  end subroutine

  subroutine transfer_ab_1(self,escd,abcd)
    SHELL4 :: self
  ! Applies the transfer equation to (es|cd) to give (ab|cd)
    IN :: self
    REALMAT(:,:), IN :: escd
    REALMAT3(:,:,:), OUT :: abcd
    STACK("SHELL4:transfer_ab_1")
    START_TIMER("SHELL4:transfer_ab_1")
    if (self%a%l > self%b%l) then
      call transfer_l_a_highest_(self,escd,abcd)
    else
      call transfer_l_b_highest_(self,escd,abcd)
    end if
    STOP_TIMER("SHELL4:transfer_ab_1")
     CHECK
  end subroutine

   subroutine transfer_l_c_highest(self,esfs,escd)
    SHELL4 :: self
   ! Applies the transfer equation to (es|fs) to give (es|cd)
     IN :: self
     REALMAT(:,:), IN :: esfs
     REALMAT3(:,:,:), OUT :: escd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components,components_c,components_d
     INTMAT3(:,:,:), PTR :: index_c,index_d
     INTVEC(:), PTR :: comp_to_use,component_to_use
     REALVEC(3) :: CD
     INT :: c,d,c1,c2,c3,d1,ld,cub,dub,ab_l_max
     INT :: cx,cy,cz,dx,dy,dz,j,clb,dlb,tmp,e,eub
     REAL :: CDi,CDx,CDy,CDz,esfs_ec

     STACK("SHELL4:transfer_l_c_highest")
     START_TIMER("SHELL4:transfer_l_c_highest")
     select case (self%d%l)
       case (0)
         escd(:,:,1)=esfs

       case (1)
         ab_l_max = max(self%a%l,self%b%l)-1
         clb = n_comp_sum_((self%c%l-1))
         eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         CD   = self%c%pos - self%d%pos
         cub  = self%c%n_comp

         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)) - clb)
         call create_(index_c,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call make_gaussian_xyz_powers_(self%c%l,components,(self%c%l+self%d%l),index_c)

         CDx = CD(1); CDy = CD(2); CDz = CD(3)
         do c = 1, cub
           cx = components(1,c)
           cy = components(2,c)
           cz = components(3,c)
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
         call destroy_(components)
         call destroy_(index_c)

       case default
         ab_l_max = max(self%a%l,self%b%l)-1
         clb  = n_comp_sum_((self%c%l-1))
         eub  = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         CD   = self%c%pos - self%d%pos
         cub  = n_comp_sum_(((self%c%l+self%d%l)-1)) - clb

         call create_(index_c,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(index_d,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)))
         call create_(comp_to_use, n_comp_sum_((self%c%l+self%d%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%c%l+self%d%l),index_d,comp_to_use)
         call make_gaussian_xyz_power_index_(self%c%l,index_c,(self%c%l+self%d%l))
         components_c => components(:,clb+1:)

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
           cub              = n_comp_sum_(((self%c%l+self%d%l)-ld)) - clb
           component_to_use => comp_to_use(dlb+1:dlb+dub)
           components_d     => components(:,dlb+1:dlb+dub)
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
         component_to_use => comp_to_use(dlb+1:dlb+dub)
         components_d     => components(:,dlb+1:dlb+dub)
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
         call destroy_(index_c)
         call destroy_(index_d)
         call destroy_(components)
         call destroy_(comp_to_use)
     end select
     STOP_TIMER("SHELL4:transfer_l_c_highest")
      CHECK
   end subroutine

   subroutine transfer_l_d_highest(self,esfs,escd)
    SHELL4 :: self
   ! Applies the transfer equation to (es|fs) to give (es|cd)
     IN :: self
     REALMAT(:,:), IN :: esfs
     REALMAT3(:,:,:), OUT :: escd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components,components_c,components_d
     INTMAT3(:,:,:), PTR :: index_c,index_d
     INTVEC(:), PTR :: comp_to_use,component_to_use
     REALVEC(3) :: DC
     INT :: c,d,c1,d1,d2,d3,lc,cub,dub,ab_l_max
     INT :: cx,cy,cz,dx,dy,dz,j,clb,dlb,tmp,eub
     REAL :: DCi,DCx,DCy,DCz

     STACK("SHELL4:transfer_l_d_highest")
     START_TIMER("SHELL4:transfer_l_d_highest")
     select case (self%c%l)
       case (0)
         escd(:,1,:)=esfs

       case (1)
         ab_l_max = max(self%a%l,self%b%l)-1
         dlb = n_comp_sum_((self%d%l-1))
         eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         DC   = self%d%pos - self%c%pos
         dub  = self%d%n_comp

         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)) - dlb)
         call create_(index_d,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call make_gaussian_xyz_powers_(self%d%l,components,(self%c%l+self%d%l),index_d)

         DCx=DC(1); DCy=DC(2); DCz=DC(3)
         do d=1,dub
           dx = components(1,d)
           dy = components(2,d)
           dz = components(3,d)
           d1 = index_d(dx+1,dy,dz)
           d2 = index_d(dx,dy+1,dz)
           d3 = index_d(dx,dy,dz+1)
             escd(:,1,d)=esfs(:,d1)+DCx*esfs(:,d)
             escd(:,2,d)=esfs(:,d2)+DCy*esfs(:,d)
             escd(:,3,d)=esfs(:,d3)+DCz*esfs(:,d)
         end do
         call destroy_(components)
         call destroy_(index_d)

       case default
         ab_l_max = max(self%a%l,self%b%l)-1
         dlb = n_comp_sum_((self%d%l-1))
         eub = n_comp_sum_((self%a%l+self%b%l)) - n_comp_sum_(ab_l_max)
         DC   = self%d%pos - self%c%pos
         dub  = n_comp_sum_((self%c%l+self%d%l-1)) - dlb

         call create_(index_c,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(index_d,0,(self%c%l+self%d%l),0,(self%c%l+self%d%l),0,(self%c%l+self%d%l))
         call create_(components,3, n_comp_sum_((self%c%l+self%d%l)))
         call create_(comp_to_use, n_comp_sum_((self%c%l+self%d%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%c%l+self%d%l),index_c,comp_to_use)
         call make_gaussian_xyz_power_index_(self%d%l,index_d,(self%c%l+self%d%l))
         components_d => components(:,dlb+1:)

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
             int_new(:,d,1)=esfs(:,d1)+DCx*esfs(:,d)
             int_new(:,d,2)=esfs(:,d2)+DCy*esfs(:,d)
             int_new(:,d,3)=esfs(:,d3)+DCz*esfs(:,d)
         end do
         do lc=2, self%c%l - 1
           clb              = n_comp_sum_((lc-1))
           cub              = n_comp_(lc)
           dub              = n_comp_sum_(((self%c%l+self%d%l)-lc)) - dlb
           component_to_use => comp_to_use(clb+1:clb+cub)
           components_c     => components(:,clb+1:clb+cub)
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
         component_to_use => comp_to_use(clb+1:clb+cub)
         components_c     => components(:,clb+1:clb+cub)
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
         call destroy_(index_c)
         call destroy_(index_d)
         call destroy_(components)
         call destroy_(comp_to_use)
     end select
     STOP_TIMER("SHELL4:transfer_l_d_highest")
      CHECK
   end subroutine

   subroutine transfer_l_a_highest(self,escd,abcd)
    SHELL4 :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT3(:,:,:), IN :: escd
     REALMAT4(:,:,:,:), OUT :: abcd
     REALMAT4(:,:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components,components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     REALVEC(3) :: AB
     INT :: a,b,c,d,a1,a2,a3,b1,lb,aub,bub,cub,dub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     REAL :: ABi,ABx,ABy,ABz,escd_acd
     INTVEC(:), PTR :: comp_to_use,component_to_use

     STACK("SHELL4:transfer_l_a_highest")
     START_TIMER("SHELL4:transfer_l_a_highest")
     select case (self%b%l)
       case (0)
         abcd(:,1,:,:)=escd

       case (1)
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = self%a%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - alb)
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%a%l,components,(self%a%l+self%b%l),index_a)

         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components(1,a)
           ay = components(2,a)
           az = components(3,a)
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
         call destroy_(index_a)
         call destroy_(components)

       case default
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - alb
         bub  = self%b%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_b,comp_to_use)
         tmp=self%a%l; call make_gaussian_xyz_power_index_(tmp,index_a,(self%a%l+self%b%l))
         components_a => components(:,alb+1:)

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
           aub              = n_comp_sum_(((self%a%l+self%b%l)-lb)) - alb
           component_to_use => comp_to_use(blb+1:blb+bub)
           components_b     => components(:,blb+1:blb+bub)
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
         component_to_use => comp_to_use(blb+1:blb+bub)
         components_b     => components(:,blb+1:blb+bub)
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
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_a)
         call destroy_(index_b)
     end select
     STOP_TIMER("SHELL4:transfer_l_a_highest")
      CHECK
   end subroutine

   subroutine transfer_l_b_highest(self,escd,abcd)
    SHELL4 :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT3(:,:,:), IN :: escd
     REALMAT4(:,:,:,:), OUT :: abcd
     REALMAT4(:,:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components,components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     REALVEC(3) :: BA
     INT :: a,b,c,d,a1,b1,b2,b3,la,aub,bub,cub,dub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     REAL :: BAi,BAx,BAy,BAz,escd_bcd
     INTVEC(:), PTR :: comp_to_use,component_to_use

     STACK("SHELL4:transfer_l_b_highest")
     START_TIMER("SHELL4:transfer_l_b_highest")
     select case (self%a%l)
       case (0)
         abcd(1,:,:,:)=escd

       case (1)
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = self%b%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - blb)
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%b%l,components,(self%a%l+self%b%l),index_b)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components(1,b)
           by = components(2,b)
           bz = components(3,b)
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

         call destroy_(components)
         call destroy_(index_b)

       case default
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - blb
         aub  = self%a%n_comp
         cub  = self%c%n_comp
         dub  = self%d%n_comp

         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_a,comp_to_use)
         tmp=self%b%l; call make_gaussian_xyz_power_index_(tmp,index_b,(self%a%l+self%b%l))
         components_b => components(:,blb+1:)

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
           bub              = n_comp_sum_(((self%a%l+self%b%l)-la)) - blb
           component_to_use => comp_to_use(alb+1:alb+aub)
           components_a     => components(:,alb+1:alb+aub)
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
         component_to_use => comp_to_use(alb+1:alb+aub)
         components_a     => components(:,alb+1:alb+aub)
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
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_b)
         call destroy_(index_a)
     end select
     STOP_TIMER("SHELL4:transfer_l_b_highest")
      CHECK
   end subroutine

   subroutine transfer_l_a_highest_1(self,escd,abcd)
    SHELL4 :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT(:,:), IN :: escd
     REALMAT3(:,:,:), OUT :: abcd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components,components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     REALVEC(3) :: AB
     INT :: a,b,f,a1,a2,a3,b1,lb,aub,bub,fub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     REAL :: ABi,ABx,ABy,ABz,escd_acd
     INTVEC(:), PTR :: comp_to_use,component_to_use

     STACK("SHELL4:transfer_l_a_highest_1")
     START_TIMER("SHELL4:transfer_l_a_highest_1")
     select case (self%b%l)
       case (0)
         abcd(:,1,:)=escd

       case (1)
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = self%a%n_comp
         fub  = size(escd,2)

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - alb)
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%a%l,components,(self%a%l+self%b%l),index_a)

         ABx=AB(1); ABy=AB(2); ABz=AB(3)
         do a=1,aub
           ax = components(1,a)
           ay = components(2,a)
           az = components(3,a)
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
         call destroy_(index_a)
         call destroy_(components)

       case default
         alb = n_comp_sum_((self%a%l-1))
         AB   = self%a%pos - self%b%pos
         aub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - alb
         bub  = self%b%n_comp
         fub  = size(escd,2)

         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_b,comp_to_use)
         tmp=self%a%l; call make_gaussian_xyz_power_index_(tmp,index_a,(self%a%l+self%b%l))
         components_a => components(:,alb+1:)

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
           aub              = n_comp_sum_(((self%a%l+self%b%l)-lb)) - alb
           component_to_use => comp_to_use(blb+1:blb+bub)
           components_b     => components(:,blb+1:blb+bub)
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
         component_to_use => comp_to_use(blb+1:blb+bub)
         components_b     => components(:,blb+1:blb+bub)
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
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_a)
         call destroy_(index_b)
     end select
     STOP_TIMER("SHELL4:transfer_l_a_highest_1")
      CHECK
   end subroutine

   subroutine transfer_l_b_highest_1(self,escd,abcd)
    SHELL4 :: self
   ! Applies the transfer equation to (es|cd) to give (ab|cd)
     IN :: self
     REALMAT(:,:), IN :: escd
     REALMAT3(:,:,:), OUT :: abcd
     REALMAT3(:,:,:), PTR :: int_new,int_old
     INTMAT(:,:), PTR :: components,components_a,components_b
     INTMAT3(:,:,:), PTR :: index_a,index_b
     REALVEC(3) :: BA
     INT :: a,b,f,a1,b1,b2,b3,la,aub,bub,fub
     INT :: ax,ay,az,bx,by,bz,j,alb,blb,tmp
     REAL :: BAi,BAx,BAy,BAz,escd_bcd
     INTVEC(:), PTR :: comp_to_use,component_to_use

     STACK("SHELL4:transfer_l_b_highest_1")
     START_TIMER("SHELL4:transfer_l_b_highest_1")
     select case (self%a%l)
       case (0)
         abcd(1,:,:)=escd

       case (1)
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = self%b%n_comp
         fub  = size(escd,2)

         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)) - blb)
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call make_gaussian_xyz_powers_(self%b%l,components,(self%a%l+self%b%l),index_b)

         BAx = BA(1);    BAy = BA(2);    BAz = BA(3)
         do b = 1, bub
           bx = components(1,b)
           by = components(2,b)
           bz = components(3,b)
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

         call destroy_(components)
         call destroy_(index_b)

       case default
         blb = n_comp_sum_((self%b%l-1))
         BA   = self%b%pos - self%a%pos
         bub  = n_comp_sum_(((self%a%l+self%b%l)-1)) - blb
         aub  = self%a%n_comp
         fub  = size(escd,2)

         call create_(index_a,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(index_b,0,(self%a%l+self%b%l),0,(self%a%l+self%b%l),0,(self%a%l+self%b%l))
         call create_(components,3, n_comp_sum_((self%a%l+self%b%l)))
         call create_(comp_to_use, n_comp_sum_((self%a%l+self%b%l)) )
         tmp=0; call make_gaussian_xyz_powers_(tmp,components,(self%a%l+self%b%l),index_a,comp_to_use)
         tmp=self%b%l; call make_gaussian_xyz_power_index_(tmp,index_b,(self%a%l+self%b%l))
         components_b => components(:,blb+1:)

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
           bub              = n_comp_sum_(((self%a%l+self%b%l)-la)) - blb
           component_to_use => comp_to_use(alb+1:alb+aub)
           components_a     => components(:,alb+1:alb+aub)
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
         component_to_use => comp_to_use(alb+1:alb+aub)
         components_a     => components(:,alb+1:alb+aub)
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
         call destroy_(comp_to_use)
         call destroy_(components)
         call destroy_(index_b)
         call destroy_(index_a)
     end select
     STOP_TIMER("SHELL4:transfer_l_b_highest_1")
      CHECK
   end subroutine

   ALWAYS_PURE subroutine add_to_component(self,x,y,z,j)
    SHELL4 :: self
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
     STOP_TIMER("SHELL4:add_to_component")
   end subroutine

   ALWAYS_PURE subroutine subtract_from_component(self,x,y,z,j)
    SHELL4 :: self
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
     STOP_TIMER("SHELL4:subtract_from_component")
   end subroutine

!*******************************************************************************
!                  Normalisation routines.
!*******************************************************************************

   subroutine to_normalise(self,abcd)
    SHELL4 :: self
   ! Multiply the matrix by the orbital normalisation coefficients
   ! for the orbitals a, b, c and d.
     IN :: self
     REALMAT4(:,:,:,:), INOUT :: abcd
     INT :: aub,bub,cub,dub,a,b,c,d
     REAL :: norm_d,norm_cd,norm_bcd
     REALVEC(:), PTR :: anorm,bnorm,cnorm,dnorm

     STACK("SHELL4:to_normalise")
     START_TIMER("SHELL4:to_normalise")
     call create_(anorm,n_comp_(self%a%l));    call normalising_factors_(anorm,self%a%l)
     call create_(bnorm,n_comp_(self%b%l));    call normalising_factors_(bnorm,self%b%l)
     call create_(cnorm,n_comp_(self%c%l));    call normalising_factors_(cnorm,self%c%l)
     call create_(dnorm,n_comp_(self%d%l));    call normalising_factors_(dnorm,self%d%l)
     aub=self%a%n_comp
     bub=self%b%n_comp
     cub=self%c%n_comp
     dub=self%d%n_comp
     do d=1,dub
       norm_d = dnorm(d)
       do c=1,cub
         norm_cd = norm_d*cnorm(c)
         do b=1,bub
           norm_bcd = norm_cd*bnorm(b)
           do a=1,aub
             abcd(a,b,c,d)=abcd(a,b,c,d)*norm_bcd*anorm(a)
           end do
         end do
       end do
     end do
     call destroy_(dnorm)
     call destroy_(cnorm)
     call destroy_(bnorm)
     call destroy_(anorm)
     STOP_TIMER("SHELL4:to_normalise")
      CHECK
   end subroutine

   subroutine to_normalise_1(self,X)
    SHELL4 :: self
   ! Multiply the matrix by the orbital normalisation coefficients for the
   ! orbitals a, b, c and d.
     IN :: self
     REALMAT5(:,:,:,:,:), INOUT :: X
     INT :: i,dim
     STACK("SHELL4:to_normalise_1")
     START_TIMER("SHELL4:to_normalise_1")
     dim = size(X,5)
     do i = 1,dim
        call to_normalise_(self,X(:,:,:,:,i))
     end do
     STOP_TIMER("SHELL4:to_normalise_1")
      CHECK
   end subroutine

!*******************************************************************************
!       make the J and K contributions from the shell4 and density matrix.
!*******************************************************************************

   subroutine make_r_JK(self,J,K,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    SHELL4 :: self
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
     STACK("SHELL4:make_r_JK")
     START_TIMER("SHELL4:make_r_JK")
     call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
     call get_ERI_(self,I4)
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
     STOP_TIMER("SHELL4:make_r_JK")
      CHECK
   end subroutine

!*******************************************************************************
!       make only the J contributions from the shell4 and density matrix.
!*******************************************************************************

   subroutine make_r_J(self,J,P,factor,fa,la,fb,lb,fc,lc,fd,ld)
    SHELL4 :: self
   ! Make the J contribution due to self and P and add it in.
   ! For any shell4!
     IN :: self
     REALMAT(:,:), IN :: P
     REALMAT(:,:), target :: J
     INT, IN :: fa,la,fb,lb,fc,lc,fd,ld
     REAL, IN :: factor
     REALMAT4(:,:,:,:), PTR :: I4
     REAL :: P_dc,Jcd,P_db,P_cb,I_abcd
     INT :: a,b,c,d
     STACK("SHELL4:make_r_J")
     START_TIMER("SHELL4:make_r_J")
     call create_(I4,fa,la,fb,lb,fc,lc,fd,ld)
     call get_ERI_(self,I4)
     if (factor > 0.9) then
       do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           Jcd = ZERO
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             do a = fa,la
               I_abcd = I4(a,b,c,d)
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
             P_db = factor*P(d,b)
             P_cb = factor*P(c,b)
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
     STOP_TIMER("SHELL4:make_r_J")
      CHECK
   end subroutine

!*******************************************************************************
!  Output Routines.
!*******************************************************************************

   subroutine put(self)
    SHELL4 :: self
   ! Put the shell4 information to file "out"
     INT :: n_cc,i
     STACK("SHELL4:put")
     START_TIMER("SHELL4:put")
     call flush_(stdout)
     call show_(stdout,"A shell l quantum number =",self%a%l)
     call show_(stdout,"B shell l quantum number =",self%b%l)
     call show_(stdout,"C shell l quantum number =",self%c%l)
     call show_(stdout,"D shell l quantum number =",self%d%l)
     call show_(stdout,"A position               =",self%a%pos)
     call show_(stdout,"B position               =",self%b%pos)
     call show_(stdout,"C position               =",self%c%pos)
     call show_(stdout,"D position               =",self%d%pos)
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
     STOP_TIMER("SHELL4:put")
      CHECK
   end subroutine

end
