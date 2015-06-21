!-------------------------------------------------------------------------------
!
! UNITCELL: Data structure for a crystal unit cell
!
! $Id: unitcell.foo,v 1.6.2.6 2003/11/13 05:33:02 reaper Exp $
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1999
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
!-------------------------------------------------------------------------------

module UNITCELL_MODULE

#  include "unitcell.use"

   implicit none

#  include "macros"
#  include "unitcell.int"


contains

!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self)
    UNITCELL :: self
   ! Create the object
      PTR :: self
      STACK("UNITCELL:create")
      START_TIMER("UNITCELL:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(UNITCELL_SIZE)
      call set_defaults_(self)
     STOP_TIMER("UNITCELL:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    UNITCELL :: self
   ! Destroy the object
      PTR :: self
      STACK("UNITCELL:destroy")
      START_TIMER("UNITCELL:destroy")
      if (NOT associated(self)) then; STOP_TIMER("UNITCELL:destroy") UNSTACK return; end if
      DELETE_MEMORY(UNITCELL_SIZE)
      deallocate(self)
     STOP_TIMER("UNITCELL:destroy")
      UNSTACK
   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   subroutine set_defaults(self)
    UNITCELL :: self
   ! Set up a default crystal object
      STACK("UNITCELL:set_defaults")
      START_TIMER("UNITCELL:set_defaults")
      self%angle        = UNITCELL_ANGLES
      call convert_from_(self%angle,"degree")
      self%length       = UNITCELL_LENGTHS
      call make_info_(self)
     STOP_TIMER("UNITCELL:set_defaults")
      CHECK
   end subroutine

   subroutine create_copy(self,unitcell)
    UNITCELL :: self
   ! Create a copy of "unitcell"
     PTR :: self
     UNITCELL, IN :: unitcell
     STACK("UNITCELL:create_copy")
     START_TIMER("UNITCELL:create_copy")
     call create_(self)
     call copy_(self,unitcell)
     STOP_TIMER("UNITCELL:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,unitcell)
    UNITCELL :: self
   ! Set self to be "unitcell"
     UNITCELL, IN :: unitcell
     STACK("UNITCELL:copy")
     START_TIMER("UNITCELL:copy")
     self = unitcell
     STOP_TIMER("UNITCELL:copy")
      CHECK
   end subroutine

!  ***************
!  Basic cell info
!  ***************

   function alpha(self) result(res)
    UNITCELL :: self
   ! Return the alpha angle, in radians.
     REAL :: res
     STACK("UNITCELL:alpha")
     START_TIMER("UNITCELL:alpha")
     ENSURE(self%info_made,"UNITCELL:alpha ... cell info not made")
     res = self%angle(1)
     STOP_TIMER("UNITCELL:alpha")
      CHECK
   end function

   function beta(self) result(res)
    UNITCELL :: self
   ! Return the beta angle, in radians.
     REAL :: res
     STACK("UNITCELL:beta")
     START_TIMER("UNITCELL:beta")
     ENSURE(self%info_made,"UNITCELL:beta ... cell info not made")
     res = self%angle(2)
     STOP_TIMER("UNITCELL:beta")
      CHECK
   end function

   function gamma(self) result(res)
    UNITCELL :: self
   ! Return the gamma angle, in radians.
     REAL :: res
     STACK("UNITCELL:gamma")
     START_TIMER("UNITCELL:gamma")
     ENSURE(self%info_made,"UNITCELL:gamma ... cell info not made")
     res = self%angle(3)
     STOP_TIMER("UNITCELL:gamma")
      CHECK
   end function

   function alpha_star(self) result(res)
    UNITCELL :: self
   ! Return the alpha reciprocal lattice angle, in radians.
     REAL :: res
     REAL :: tmp,alpha,beta,gamma
     STACK("UNITCELL:alpha_star")
     START_TIMER("UNITCELL:alpha_star")
     ENSURE(self%info_made,"UNITCELL:alpha_star ... cell info not made")
     alpha = self%angle(1); beta  = self%angle(2); gamma = self%angle(3)
     tmp = (cos(beta)*cos(gamma)-cos(alpha))/(sin(beta)*sin(gamma))
     res = arccos_(tmp)
     STOP_TIMER("UNITCELL:alpha_star")
      CHECK
   end function

   function beta_star(self) result(res)
    UNITCELL :: self
   ! Return the beta reciprocal lattice angle, in radians.
     REAL :: res
     REAL :: tmp,alpha,beta,gamma
     STACK("UNITCELL:beta_star")
     START_TIMER("UNITCELL:beta_star")
     ENSURE(self%info_made,"UNITCELL:beta_star ... cell info not made")
     alpha = self%angle(1); beta  = self%angle(2); gamma = self%angle(3)
     tmp = (cos(gamma)*cos(alpha)-cos(beta))/(sin(gamma)*sin(alpha))
     res = arccos_(tmp)
     STOP_TIMER("UNITCELL:beta_star")
      CHECK
   end function

   function gamma_star(self) result(res)
    UNITCELL :: self
   ! Return the gamma reciprocal lattice angle, in radians.
     REAL :: res
     REAL :: tmp,alpha,beta,gamma
     STACK("UNITCELL:gamma_star")
     START_TIMER("UNITCELL:gamma_star")
     ENSURE(self%info_made,"UNITCELL:gamma_star ... cell info not made")
     alpha = self%angle(1); beta  = self%angle(2); gamma = self%angle(3)
     tmp = (cos(alpha)*cos(beta)-cos(gamma))/(sin(alpha)*sin(beta))
     res = arccos_(tmp)
     STOP_TIMER("UNITCELL:gamma_star")
      CHECK
   end function

   function a(self) result(res)
    UNITCELL :: self
   ! Return the a cell length, in bohr.
     REAL :: res
     STACK("UNITCELL:a")
     START_TIMER("UNITCELL:a")
     ENSURE(self%info_made,"UNITCELL:a ... cell info not made")
     res = self%length(1) 
     STOP_TIMER("UNITCELL:a")
      CHECK
   end function

   function b(self) result(res)
    UNITCELL :: self
   ! Return the b cell length, in bohr.
     REAL :: res
     STACK("UNITCELL:b")
     START_TIMER("UNITCELL:b")
     ENSURE(self%info_made,"UNITCELL:b ... cell info not made")
     res = self%length(2) 
     STOP_TIMER("UNITCELL:b")
      CHECK
   end function

   function c(self) result(res)
    UNITCELL :: self
   ! Return the c cell length, in bohr.
     REAL :: res
     STACK("UNITCELL:c")
     START_TIMER("UNITCELL:c")
     ENSURE(self%info_made,"UNITCELL:c ... cell info not made")
     res = self%length(3) 
     STOP_TIMER("UNITCELL:c")
      CHECK
   end function

   function a_star(self) result(res)
    UNITCELL :: self
   ! Return the a reciprocal lattice length, in bohr.
     REAL :: res
     STACK("UNITCELL:a_star")
     START_TIMER("UNITCELL:a_star")
     ENSURE(self%info_made,"UNITCELL:a_star ... cell info not made")
     res = self%length(2) * self%length(3) * sin(self%angle(1)) / self%volume
     STOP_TIMER("UNITCELL:a_star")
      CHECK
   end function

   function b_star(self) result(res)
    UNITCELL :: self
   ! Return the b reciprocal lattice length, in bohr.
     REAL :: res
     STACK("UNITCELL:b_star")
     START_TIMER("UNITCELL:b_star")
     ENSURE(self%info_made,"UNITCELL:b_star ... cell info not made")
     res = self%length(3) * self%length(1) * sin(self%angle(2)) / self%volume
     STOP_TIMER("UNITCELL:b_star")
      CHECK
   end function

   function c_star(self) result(res)
    UNITCELL :: self
   ! Return the c reciprocal lattice length, in bohr.
     REAL :: res
     STACK("UNITCELL:c_star")
     START_TIMER("UNITCELL:c_star")
     ENSURE(self%info_made,"UNITCELL:c_star ... cell info not made")
     res = self%length(1) * self%length(2) * sin(self%angle(3)) / self%volume
     STOP_TIMER("UNITCELL:c_star")
      CHECK
   end function

!  **************************
!  Make cell axis/volume info
!  **************************

   subroutine make_info(self)
    UNITCELL :: self
   ! Calculate the various unit cell axis matrices.
      STACK("UNITCELL:make_info")
      START_TIMER("UNITCELL:make_info")
      self%info_made = FALSE
      call make_volume_(self)
      call make_direct_matrix_(self)
      call make_reciprocal_matrix_(self)
      call make_direct_U_matrix_(self)
      call make_reciprocal_U_matrix_(self)
      self%info_made = TRUE
     STOP_TIMER("UNITCELL:make_info")
      CHECK
   end subroutine

   subroutine make_volume(self)
    UNITCELL :: self
   ! Calculate the cell volume
      REAL :: a,b,c,ca,cb,cg,sb
      STACK("UNITCELL:make_volume")
      START_TIMER("UNITCELL:make_volume")
      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      sb = sin(self%angle(2))
      self%volume = a*b*c*sqrt(ONE-ca**2-cb**2-cg**2+TWO*ca*cb*cg)
     STOP_TIMER("UNITCELL:make_volume")
      CHECK
   end subroutine

   subroutine make_direct_matrix(self)
    UNITCELL :: self
   ! Calculate the direct cell matrices (i.e. cell axes) in units of BOHRS.
      REAL :: v,a,b,c,ca,cb,cg,sb
      STACK("UNITCELL:make_direct_matrix")
      START_TIMER("UNITCELL:make_direct_matrix")
      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      sb = sin(self%angle(2))
      v = self%volume
      ! Direct cell matrix
      self%direct_matrix(1,1) = a
      self%direct_matrix(1,2) = b*cg
      self%direct_matrix(1,3) = c*cb
      self%direct_matrix(2,1) = ZERO
      self%direct_matrix(2,2) = v/(a*c*sb)
      self%direct_matrix(2,3) = ZERO
      self%direct_matrix(3,1) = ZERO
      self%direct_matrix(3,2) = b*(ca-cg*cb)/sb
      self%direct_matrix(3,3) = c*sb
     STOP_TIMER("UNITCELL:make_direct_matrix")
      CHECK
   end subroutine

   subroutine make_reciprocal_matrix(self)
    UNITCELL :: self
   ! Calculate the reciprocal cell matrices (i.e. reciprocal cell axes) in units
   ! of 1/BOHRS. Also calculate the inverse direct cell matrix.
      REAL :: v,a,b,c,ca,cb,cg,sb
      STACK("UNITCELL:make_reciprocal_matrix")
      START_TIMER("UNITCELL:make_reciprocal_matrix")
      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      sb = sin(self%angle(2))
      v = self%volume
      ! Reciprocal cell matrix
      self%reciprocal_matrix(1,1) = ONE/a
      self%reciprocal_matrix(1,2) = ZERO
      self%reciprocal_matrix(1,3) = ZERO
      self%reciprocal_matrix(2,1) = b*c*(ca*cb-cg)/sb/v
      self%reciprocal_matrix(2,2) = a*c*sb/v
      self%reciprocal_matrix(2,3) = a*b*(cb*cg-ca)/sb/v
      self%reciprocal_matrix(3,1) = -cb/a/sb
      self%reciprocal_matrix(3,2) = ZERO
      self%reciprocal_matrix(3,3) = ONE/c/sb
      self%inverse_matrix = transpose(self%reciprocal_matrix)
     STOP_TIMER("UNITCELL:make_reciprocal_matrix")
      CHECK
   end subroutine

   subroutine make_direct_U_matrix(self)
    UNITCELL :: self
   ! Return the transformation matrix which changes the thermal tensor
   ! from the crystal axis system into the cartesian axis system.
   ! See comments for reciprocal_U_tensor_matrix below.
      REAL :: len
      INT :: i
      STACK("UNITCELL:make_direct_U_matrix")
      START_TIMER("UNITCELL:make_direct_U_matrix")
      do i = 1,3
         len = norm_(self%reciprocal_matrix(:,i))
         self%direct_U_matrix(i,:) = len*self%direct_matrix(:,i)
      end do
     STOP_TIMER("UNITCELL:make_direct_U_matrix")
      CHECK
   end subroutine

   subroutine make_reciprocal_U_matrix(self)
    UNITCELL :: self
   ! Return the transformation matrix which changes the thermal tensor
   ! from the cartesian axis system into the crystal axis system.
   ! The thermal tensor in the crystal axis system U_{ij} is defined
   ! by the temperature factor expansion:
   !         TF = exp ( -2\pi^2 U_{ij} h_i h_j a^*_i a^*_j )
   ! where h are the Miller indices and a^* are the reciprocal lattice
   ! constants (in bohr^{-2}). This is as used by systems like Xtal.
   ! The thermal tensor in the cartesian axis system U_{ij} is defined
   ! by the temperature factor expansion:
   !         TF = exp ( -0.5 U_{ij} k_i k_j )
   ! where k = 2\pi B h, and B is the reciprocal cell matrix.
      REAL :: len
      INT :: i
      STACK("UNITCELL:make_reciprocal_U_matrix")
      START_TIMER("UNITCELL:make_reciprocal_U_matrix")
      do i = 1,3
         len = ONE/norm_(self%reciprocal_matrix(:,i))
         self%reciprocal_U_matrix(:,i) = self%reciprocal_matrix(:,i)*len
      end do
     STOP_TIMER("UNITCELL:make_reciprocal_U_matrix")
      CHECK
   end subroutine

!  **************************
!  Geometry altering routines
!  **************************

   subroutine change_from_fractional(self,g)
    UNITCELL :: self
   ! Change the columns of geometry array "g" *from* crystal fractional
   ! coordinates into standard cartesian coordiantes
      REALMAT(:,:) :: g
      INT :: n,n_atom
   STACK("UNITCELL:change_from_fractional")
   START_TIMER("UNITCELL:change_from_fractional")
   ENSURE(size(g,1)==3,"UNITCELL:change_from_fractional ... incorrect dimension for g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call rotate_by_(g(:,n),self%direct_matrix)
      end do
     STOP_TIMER("UNITCELL:change_from_fractional")
      CHECK
   end subroutine

   subroutine change_into_fractional(self,g)
    UNITCELL :: self
   ! Change the columns of geometry array "g" from standard cartesian
   ! coordinates *into* crystal fractional coordinates
      REALMAT(:,:) :: g
      INT :: n,n_atom
   STACK("UNITCELL:change_into_fractional")
   START_TIMER("UNITCELL:change_into_fractional")
   ENSURE(size(g,1)==3,"UNITCELL:change_into_fractional ... incorrect dimension for g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call rotate_by_(g(:,n),self%inverse_matrix)
      end do
     STOP_TIMER("UNITCELL:change_into_fractional")
      CHECK
   end subroutine

   subroutine change_from_fractional_1(self,p)
    UNITCELL :: self
   ! Change the position "p" *from* crystal fractional coordinates into standard
   ! cartesian coordiantes
      REALVEC(3) :: p
      STACK("UNITCELL:change_from_fractional_1")
      START_TIMER("UNITCELL:change_from_fractional_1")
      call rotate_by_(p,self%direct_matrix)
     STOP_TIMER("UNITCELL:change_from_fractional_1")
      CHECK
   end subroutine

   subroutine change_into_fractional_1(self,p)
    UNITCELL :: self
   ! Change the position "p" from standard cartesian coordinates *into* crystal
   ! fractional coordinates
      REALVEC(3) :: p
      STACK("UNITCELL:change_into_fractional_1")
      START_TIMER("UNITCELL:change_into_fractional_1")
      call rotate_by_(p,self%inverse_matrix)
     STOP_TIMER("UNITCELL:change_into_fractional_1")
      CHECK
   end subroutine

!  ************
!  Read methods
!  ************

   recursive subroutine read_keywords(self)
    UNITCELL :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("UNITCELL:read_keywords")
     START_TIMER("UNITCELL:read_keywords")
     ENSURE(next_item_(stdin)=="{","UNITCELL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("UNITCELL:read_keywords")
      CHECK
   end subroutine

   subroutine process_keyword(self,keyword)
    UNITCELL :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*) :: keyword
     STR(STR_SIZE) :: word
     STACK("UNITCELL:process_keyword")
     START_TIMER("UNITCELL:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                ")  ! exit read_loop
        case ("a=               "); call read_a_(self)
        case ("alpha=           "); call read_alpha_(self)
        case ("angles=          "); call read_angles_(self)
        case ("b=               "); call read_b_(self)
        case ("beta=            "); call read_beta_(self)
        case ("c=               "); call read_c_(self)
        case ("gamma=           "); call read_gamma_(self)
        case ("dimensions=      "); call read_lengths_(self)
        case ("junk=            "); call read_junk_(self)
        case ("lengths=         "); call read_lengths_(self)
        case ("put              "); call put_(self)
        case ("units=           "); call read_units_(self)
        case default;                       allocate(tonto%known_keywords(13))
        tonto%known_keywords(1) = "}                "
        tonto%known_keywords(2) = "a=               "
        tonto%known_keywords(3) = "alpha=           "
        tonto%known_keywords(4) = "angles=          "
        tonto%known_keywords(5) = "b=               "
        tonto%known_keywords(6) = "beta=            "
        tonto%known_keywords(7) = "c=               "
        tonto%known_keywords(8) = "gamma=           "
        tonto%known_keywords(9) = "dimensions=      "
        tonto%known_keywords(10) = "junk=            "
        tonto%known_keywords(11) = "lengths=         "
        tonto%known_keywords(12) = "put              "
        tonto%known_keywords(13) = "units=           "
        call unknown_(tonto,word,"UNITCELL:process_keyword")
        deallocate(tonto%known_keywords)
      end select
     call update_(self)
     STOP_TIMER("UNITCELL:process_keyword")
      CHECK
   end subroutine

   subroutine read_units(self)
    UNITCELL :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("UNITCELL:read_units")
      START_TIMER("UNITCELL:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("UNITCELL:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    UNITCELL :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("UNITCELL:read_junk")
      START_TIMER("UNITCELL:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("UNITCELL:read_junk")
      CHECK
   end subroutine

   subroutine update(self)
    UNITCELL :: self
   ! Update the cell information
     STACK("UNITCELL:update")
     START_TIMER("UNITCELL:update")
     call make_info_(self)
     STOP_TIMER("UNITCELL:update")
      CHECK
   end subroutine

   subroutine read_lengths(self)
    UNITCELL :: self
   ! Read the unit cell axis lengths
      STACK("UNITCELL:read_lengths")
      START_TIMER("UNITCELL:read_lengths")
      call read_(stdin,self%length)
     STOP_TIMER("UNITCELL:read_lengths")
      CHECK
   end subroutine

   subroutine read_a(self)
    UNITCELL :: self
   ! Read the a length
      STACK("UNITCELL:read_a")
      START_TIMER("UNITCELL:read_a")
      call read_(stdin,self%length(1))
     STOP_TIMER("UNITCELL:read_a")
      CHECK
   end subroutine

   subroutine read_b(self)
    UNITCELL :: self
   ! Read the b length
      STACK("UNITCELL:read_b")
      START_TIMER("UNITCELL:read_b")
      call read_(stdin,self%length(2))
     STOP_TIMER("UNITCELL:read_b")
      CHECK
   end subroutine

   subroutine read_c(self)
    UNITCELL :: self
   ! Read the c length
      STACK("UNITCELL:read_c")
      START_TIMER("UNITCELL:read_c")
      call read_(stdin,self%length(3))
     STOP_TIMER("UNITCELL:read_c")
      CHECK
   end subroutine

   subroutine read_angles(self)
    UNITCELL :: self
   ! Read the unit cell angles
      STACK("UNITCELL:read_angles")
      START_TIMER("UNITCELL:read_angles")
      call read_(stdin,self%angle)
     STOP_TIMER("UNITCELL:read_angles")
      CHECK
   end subroutine

   subroutine read_alpha(self)
    UNITCELL :: self
   ! Read the alpha angle
      STACK("UNITCELL:read_alpha")
      START_TIMER("UNITCELL:read_alpha")
      call read_(stdin,self%angle(1))
     STOP_TIMER("UNITCELL:read_alpha")
      CHECK
   end subroutine

   subroutine read_beta(self)
    UNITCELL :: self
   ! Read the beta angle
      STACK("UNITCELL:read_beta")
      START_TIMER("UNITCELL:read_beta")
      call read_(stdin,self%angle(2))
     STOP_TIMER("UNITCELL:read_beta")
      CHECK
   end subroutine

   subroutine read_gamma(self)
    UNITCELL :: self
   ! Read the gamma angle
      STACK("UNITCELL:read_gamma")
      START_TIMER("UNITCELL:read_gamma")
      call read_(stdin,self%angle(3))
     STOP_TIMER("UNITCELL:read_gamma")
      CHECK
   end subroutine

   subroutine read_CIF(self,cif)
    UNITCELL :: self
   ! Read cell information from a CIF file "cif"
      CIF :: cif
      REAL :: err
      BIN :: found
      STACK("UNITCELL:read_CIF")
      START_TIMER("UNITCELL:read_CIF")
      call set_defaults_(self)
      self%angle(1) = 90 ! Reset defaults (already set in read_CIF)
      self%angle(2) = 90 ! in degrees !
      self%angle(3) = 90
      call read_item_(cif,"_cell_angle_alpha",self%angle(1),err,found)
      call read_item_(cif,"_cell_angle_beta" ,self%angle(2),err,found)
      call read_item_(cif,"_cell_angle_gamma",self%angle(3),err,found)
      call read_item_(cif,"_cell_length_a",self%length(1),err)
      call read_item_(cif,"_cell_length_b",self%length(2),err)
      call read_item_(cif,"_cell_length_c",self%length(3),err)
      call convert_from_(self%length,"angstrom")
      call convert_from_(self%angle,"degree")    ! Now convert to rads
      call update_(self)
     STOP_TIMER("UNITCELL:read_CIF")
      CHECK
   end subroutine

!  ***********
!  Put methods
!  ***********

   subroutine put(self)
    UNITCELL :: self
   ! Put unitcell information 
     STACK("UNITCELL:put")
     START_TIMER("UNITCELL:put")
     call flush_(stdout)
     call text_(stdout,"Unitcell information:")
     call flush_(stdout)
     call show_(stdout,"alpha angle (rad)        = ",self%angle(1))
     call show_(stdout,"beta  angle (rad)        = ",self%angle(2))
     call show_(stdout,"gamma angle (rad)        = ",self%angle(3))
     call show_(stdout,"a cell parameter (bohr)  = ",self%length(1))
     call show_(stdout,"b cell parameter (bohr)  = ",self%length(2))
     call show_(stdout,"c cell parameter (bohr)  = ",self%length(3))
     call show_(stdout,"Cell volume(bohr^3)      = ",self%volume)
     call flush_(stdout)
     call show_(stdout,"alpha* angle (rad)       = ",alpha_star_(self))
     call show_(stdout,"beta*  angle (rad)       = ",beta_star_(self))
     call show_(stdout,"gamma* angle (rad)       = ",gamma_star_(self))
     call show_(stdout,"a* cell parameter (bohr) = ",a_star_(self))
     call show_(stdout,"b* cell parameter (bohr) = ",b_star_(self))
     call show_(stdout,"c* cell parameter (bohr) = ",c_star_(self))
     call flush_(stdout)
     call text_(stdout,"Direct cell matrix/bohr:")
     call put_(stdout,self%direct_matrix)
     call flush_(stdout)
     call text_(stdout,"Inverse direct cell matrix/bohr:")
     call put_(stdout,self%inverse_matrix)
     call flush_(stdout)
     call text_(stdout,"Reciprocal cell matrix/(bohr^{-1}):")
     call put_(stdout,self%reciprocal_matrix)
     call flush_(stdout)
     call text_(stdout,"Direct U cell matrix/bohr:")
     call put_(stdout,self%direct_U_matrix)
     call flush_(stdout)
     call text_(stdout,"Reciprocal U cell matrix/(bohr^{-1}):")
     call put_(stdout,self%reciprocal_U_matrix)
     call flush_(stdout)
     STOP_TIMER("UNITCELL:put")
      CHECK
   end subroutine

   subroutine put_CX(self,label)
    UNITCELL :: self
   ! Output some information for the Crystal Explorer program.
       STR(STR_SIZE) :: label
       STACK("UNITCELL:put_CX")
       START_TIMER("UNITCELL:put_CX")
       call flush_(stdout)
       call text_(stdout,"begin crystalcell " // trim(label))
       call show_(stdout,"   a =",to_units_(self%length(1),"angstrom"))
       call show_(stdout,"   b =",to_units_(self%length(2),"angstrom"))
       call show_(stdout,"   c =",to_units_(self%length(3),"angstrom"))
       call show_(stdout,"   alpha =",to_units_(self%angle(1),"degree"))
       call show_(stdout,"   beta  =",to_units_(self%angle(2),"degree"))
       call show_(stdout,"   gamma =",to_units_(self%angle(3),"degree"))
       call text_(stdout,"end crystalcell")
     STOP_TIMER("UNITCELL:put_CX")
      CHECK
   end subroutine

end
