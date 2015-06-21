!-------------------------------------------------------------------------------
!
! DFTGRID: Numerical integration grid for DFT calculations
!
! This routine gets a 3-D grid which is a combination of a
! radial grid and a spherical grid.
!
! In getting a complete molecular grid array, firstly a grid at the
! origin is made. This is the purpose of the routine "make_atom_grid".
! This grid is then effectively copied to each atom of the molecule.
! More exactly, for each atom in the molecule, this grid at the origin
! is copied, re-scaled depending on the Bragg-Slater radii,
! and then displaced so that its origin is now centred on the atom.
!
! Each re-scaled, displaced grid is then "partitioned".  The partitioning
! modifies the weights of the displaced grid so that effectively it doesn't
! overlap with the displaced grids on all the other atoms.
!
! The displace and partition of the original "atom" grid is done by
! the routine "rescale_displace_partition".
!
! Copyright (C) S. K. Wolff, 1999
!     hacked by dylan, 10 minutes later.
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
! $Id: dftgrid.foo,v 1.18.2.11 2004/04/21 07:28:57 reaper Exp $
!
!-------------------------------------------------------------------------------

module DFTGRID_MODULE

#  include "dftgrid.use"

   implicit none

#  include "macros"
#  include "dftgrid.int"


contains


!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self,root_name,name,genre,format)
    DFTGRID :: self
   ! Create an object
      PTR :: self
      STR(STR_SIZE), optional :: root_name,name,genre,format
      STACK("DFTGRID:create")
      START_TIMER("DFTGRID:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(DFTGRID_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
      call set_(self%archive,root_name,name,genre,format)
     STOP_TIMER("DFTGRID:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    DFTGRID :: self
   ! Destroy object
      PTR :: self
      STACK("DFTGRID:destroy")
      START_TIMER("DFTGRID:destroy")
      if (NOT associated(self)) then; STOP_TIMER("DFTGRID:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(DFTGRID_SIZE)
      deallocate(self)
     STOP_TIMER("DFTGRID:destroy")
      UNSTACK
   end subroutine

   subroutine destroy_ptr_part(self)
    DFTGRID :: self
     STACK("DFTGRID:destroy_ptr_part")
     START_TIMER("DFTGRID:destroy_ptr_part")
     if (associated(self%single_atom_points)) call destroy_(self%single_atom_points)
     if (associated(self%single_atom_weights)) call destroy_(self%single_atom_weights)
     STOP_TIMER("DFTGRID:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    DFTGRID :: self
     STACK("DFTGRID:nullify_ptr_part")
     START_TIMER("DFTGRID:nullify_ptr_part")
     nullify(self%single_atom_points)
     nullify(self%single_atom_weights)
     STOP_TIMER("DFTGRID:nullify_ptr_part")
      CHECK
   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Return TRUE if object is destroyed
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   subroutine create_copy(self,d)
    DFTGRID :: self
   ! Create a copy a dftgrid "d"
      PTR :: self
       DFTGRID :: d
      STACK("DFTGRID:create_copy")
      START_TIMER("DFTGRID:create_copy")
      call create_(self)
      call nullify_ptr_part_(self)
      call copy_(self,d)
     STOP_TIMER("DFTGRID:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,d)
    DFTGRID :: self
   ! Copy a dftgrid "d"
      DFTGRID :: d
      STACK("DFTGRID:copy")
      START_TIMER("DFTGRID:copy")
      if (associated(d%single_atom_points)) call create_copy_(self%single_atom_points,d%single_atom_points)
      if (associated(d%single_atom_weights)) call create_copy_(self%single_atom_weights,d%single_atom_weights)
      self = d
      call copy_(self%archive,d%archive)
     STOP_TIMER("DFTGRID:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    DFTGRID :: self
   ! Set up a default dftgrid object
      STACK("DFTGRID:set_defaults")
      START_TIMER("DFTGRID:set_defaults")
      self%spherical_grid_kind     = DFTGRID_SPHERICAL_GRID_KIND
      self%spherical_grid_order    = DFTGRID_SPHERICAL_GRID_ORDER
      self%radial_grid_kind        = DFTGRID_RADIAL_GRID_KIND
      self%radial_grid_order       = DFTGRID_RADIAL_GRID_ORDER
      self%becke_m_partition_power = DFTGRID_BECKE_M_PARTITION_POWER
      self%gauss_chebyshev_m       = DFTGRID_GAUSS_CHEBYSHEV_M
      self%gauss_chebyshev_alpha   = DFTGRID_GAUSS_CHEBYSHEV_ALPHA
      self%euler_maclaurin_m       = DFTGRID_EULER_MACLAURIN_M
      self%euler_maclaurin_alpha   = DFTGRID_EULER_MACLAURIN_ALPHA
      call nullify_ptr_part_(self%archive)
      call set_root_name_(self%archive,DFTGRID_ARCHIVE_ROOT_NAME)
      call set_name_(self%archive,DFTGRID_ARCHIVE_NAME)
      call set_genre_(self%archive,genre_(self))
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:set_defaults")
      CHECK
   end subroutine

   subroutine set_high_accuracy(self)
    DFTGRID :: self
   ! Set up a default dftgrid object
      STACK("DFTGRID:set_high_accuracy")
      START_TIMER("DFTGRID:set_high_accuracy")
      self%spherical_grid_kind = 'lebedev'
      self%spherical_grid_order = 53
      self%radial_grid_kind = 'gauss-chebyshev'
      self%radial_grid_order = 250
      self%becke_m_partition_power = TWO
      self%gauss_chebyshev_m       = ONE
      self%gauss_chebyshev_alpha   = THREE
      self%euler_maclaurin_m       = TWO
      self%euler_maclaurin_alpha   = TWO
      call set_root_name_(self%archive,"dftgrid")
      call set_name_(self%archive,"grid")
      call set_genre_(self%archive,genre_(self))
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:set_high_accuracy")
      CHECK
   end subroutine

   function genre(self) result(a_kind)
    DFTGRID :: self
   ! Return the archive "kind" using the current dftgrid settings
      STR(STR_SIZE) :: a_kind
      STACK("DFTGRID:genre")
      START_TIMER("DFTGRID:genre")
      a_kind =                 trim(self%spherical_grid_kind) // "_"
      a_kind = trim(a_kind) // trim(to_str_(self%spherical_grid_order)) // "_"
      a_kind = trim(a_kind) // trim(self%radial_grid_kind) // "_"
      a_kind = trim(a_kind) // trim(to_str_(self%radial_grid_order))
     STOP_TIMER("DFTGRID:genre")
      CHECK
   end function

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    DFTGRID :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("DFTGRID:read_keywords")
     START_TIMER("DFTGRID:read_keywords")
     ENSURE(next_item_(stdin)=="{","DFTGRID:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("DFTGRID:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    DFTGRID :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("DFTGRID:process_keyword")
      START_TIMER("DFTGRID:process_keyword")
      word = keyword
      call to_lower_case_(word)
      self%finalized = FALSE
      select case (word)
         case ("}                       ")  ! exit case
         case ("becke_m_partition_power="); call read_becke_m_partition_power_(self)
         case ("euler_maclaurin_alpha=  "); call read_euler_maclaurin_alpha_(self)
         case ("euler_maclaurin_m=      "); call read_euler_maclaurin_m_(self)
         case ("gauss_chebyshev_alpha=  "); call read_gauss_chebyshev_alpha_(self)
         case ("gauss_chebyshev_m=      "); call read_gauss_chebyshev_m_(self)
         case ("put                     "); call put_(self)
         case ("radial_grid_kind=       "); call read_radial_grid_kind_(self)
         case ("radial_grid_order=      "); call read_radial_grid_order_(self)
         case ("set_defaults            "); call set_defaults_(self)
         case ("set_high_accuracy       "); call set_high_accuracy_(self)
         case ("spherical_grid_kind=    "); call read_spherical_grid_kind_(self)
         case ("spherical_grid_order=   "); call read_spherical_grid_order_(self)
         case default;                    allocate(tonto%known_keywords(13))
         tonto%known_keywords(1) = "}                       "
         tonto%known_keywords(2) = "becke_m_partition_power="
         tonto%known_keywords(3) = "euler_maclaurin_alpha=  "
         tonto%known_keywords(4) = "euler_maclaurin_m=      "
         tonto%known_keywords(5) = "gauss_chebyshev_alpha=  "
         tonto%known_keywords(6) = "gauss_chebyshev_m=      "
         tonto%known_keywords(7) = "put                     "
         tonto%known_keywords(8) = "radial_grid_kind=       "
         tonto%known_keywords(9) = "radial_grid_order=      "
         tonto%known_keywords(10) = "set_defaults            "
         tonto%known_keywords(11) = "set_high_accuracy       "
         tonto%known_keywords(12) = "spherical_grid_kind=    "
         tonto%known_keywords(13) = "spherical_grid_order=   "
         call unknown_(tonto,word,"DFTGRID:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("DFTGRID:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    DFTGRID :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("DFTGRID:read_units")
      START_TIMER("DFTGRID:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("DFTGRID:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    DFTGRID :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("DFTGRID:read_junk")
      START_TIMER("DFTGRID:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("DFTGRID:read_junk")
      CHECK
   end subroutine

   subroutine read_spherical_grid_kind(self)
    DFTGRID :: self
   ! Read in the spherical grid kind
      STACK("DFTGRID:read_spherical_grid_kind")
      START_TIMER("DFTGRID:read_spherical_grid_kind")
      call read_(stdin,self%spherical_grid_kind)
      select case (self%spherical_grid_kind)
         case("lebedev")
         case default;  allocate(tonto%known_keywords(1))
         tonto%known_keywords(1) = "lebedev"
         call unknown_(tonto,self%spherical_grid_kind,"DFTGRID:read_spherical_grid_kind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("DFTGRID:read_spherical_grid_kind")
      CHECK
   end subroutine

   subroutine read_spherical_grid_order(self)
    DFTGRID :: self
   ! Read in the spherical grid order
      STR(STR_SIZE) :: word
      STACK("DFTGRID:read_spherical_grid_order")
      START_TIMER("DFTGRID:read_spherical_grid_order")
      call read_(stdin,self%spherical_grid_order)
      word = to_str_(self%spherical_grid_order)
      select case (self%spherical_grid_order)
         case (9)
         case (11)
         case (13)
         case (15)
         case (17)
         case (19)
         case (23)
         case (25)
         case (27)
         case (29)
         case (35)
         case (41)
         case (47)
         case (53)
         case (59)
         case default;  allocate(tonto%known_keywords(0))
         call unknown_(tonto,word,"DFTGRID:read_spherical_grid_order")
         deallocate(tonto%known_keywords)
      end select
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_spherical_grid_order")
      CHECK
   end subroutine

   subroutine read_radial_grid_kind(self)
    DFTGRID :: self
   ! Read in the radial grid kind
      STACK("DFTGRID:read_radial_grid_kind")
      START_TIMER("DFTGRID:read_radial_grid_kind")
      call read_(stdin,self%radial_grid_kind)
      select case (self%radial_grid_kind)
         case("gauss-chebychev")
         case("euler-maclaurin")
         case default;  allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "gauss-chebychev"
         tonto%known_keywords(2) = "euler-maclaurin"
         call unknown_(tonto,self%radial_grid_kind,"DFTGRID:read_radial_grid_kind")
         deallocate(tonto%known_keywords)
      end select
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_radial_grid_kind")
      CHECK
   end subroutine

   subroutine read_radial_grid_order(self)
    DFTGRID :: self
   ! Read in the radial grid order
      STACK("DFTGRID:read_radial_grid_order")
      START_TIMER("DFTGRID:read_radial_grid_order")
      call read_(stdin,self%radial_grid_order)
      DIE_IF(self%radial_grid_order<=0,"DFTGRID:read_radial_grid_order ... must be positive")
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_radial_grid_order")
      CHECK
   end subroutine

   subroutine read_becke_m_partition_power(self)
    DFTGRID :: self
   ! Read the Becke "m" partition power. Refer to paper.
      STACK("DFTGRID:read_becke_m_partition_power")
      START_TIMER("DFTGRID:read_becke_m_partition_power")
      call read_(stdin,self%becke_m_partition_power)
      DIE_IF(self%becke_m_partition_power<=0,"DFTGRID:read_becke_m_partition_power ... must be positive")
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_becke_m_partition_power")
      CHECK
   end subroutine

   subroutine read_gauss_chebyshev_m(self)
    DFTGRID :: self
   ! Read the Gauss-Chebyshev "m" partition power. Refer to paper.
      STACK("DFTGRID:read_gauss_chebyshev_m")
      START_TIMER("DFTGRID:read_gauss_chebyshev_m")
      call read_(stdin,self%gauss_chebyshev_m)
      DIE_IF(self%gauss_chebyshev_m<=0,"DFTGRID:read_gauss_chebyshev_m ... must be positive")
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_gauss_chebyshev_m")
      CHECK
   end subroutine

   subroutine read_gauss_chebyshev_alpha(self)
    DFTGRID :: self
   ! Read the Gauss-Chebyshev "alpha" partition power. Refer to paper.
      STACK("DFTGRID:read_gauss_chebyshev_alpha")
      START_TIMER("DFTGRID:read_gauss_chebyshev_alpha")
      call read_(stdin,self%gauss_chebyshev_alpha)
      DIE_IF(self%gauss_chebyshev_alpha<=0,"DFTGRID:read_gauss_chebyshev_alpha ... must be positive")
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_gauss_chebyshev_alpha")
      CHECK
   end subroutine

   subroutine read_euler_maclaurin_m(self)
    DFTGRID :: self
   ! Read the Euler Maclaurin "m" value. Refer to paper.
      STACK("DFTGRID:read_euler_maclaurin_m")
      START_TIMER("DFTGRID:read_euler_maclaurin_m")
      call read_(stdin,self%euler_maclaurin_m)
      DIE_IF(self%euler_maclaurin_m<=0,"DFTGRID:read_euler_maclaurin_m ... must be positive")
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_euler_maclaurin_m")
      CHECK
   end subroutine

   subroutine read_euler_maclaurin_alpha(self)
    DFTGRID :: self
   ! Read the Euler Maclaurin "alpha" value. Refer to paper.
      STACK("DFTGRID:read_euler_maclaurin_alpha")
      START_TIMER("DFTGRID:read_euler_maclaurin_alpha")
      call read_(stdin,self%euler_maclaurin_alpha)
      DIE_IF(self%euler_maclaurin_alpha<=0,"DFTGRID:read_euler_maclaurin_alpha ... must be positive")
      call set_grid_data_(self)
     STOP_TIMER("DFTGRID:read_euler_maclaurin_alpha")
      CHECK
   end subroutine

!  *****************************************************************
!  Routines which evaulate the number of grid points for each scheme
!  *****************************************************************

   subroutine set_grid_data(self)
    DFTGRID :: self
   ! Set all the grid data preliminary to grid generation
      STACK("DFTGRID:set_grid_data")
      START_TIMER("DFTGRID:set_grid_data")
      call set_radial_grid_data_(self)
      call set_spherical_grid_data_(self)
      self%n_pts = self%n_spherical_pts*self%n_radial_pts
      call set_genre_(self%archive,trim(genre_(self)))
      self%finalized = TRUE
      if (associated(self%single_atom_points)) call destroy_(self%single_atom_points)
      if (associated(self%single_atom_weights)) call destroy_(self%single_atom_weights)
     STOP_TIMER("DFTGRID:set_grid_data")
      CHECK
   end subroutine

   subroutine set_radial_grid_data(self)
    DFTGRID :: self
   ! Set the radial grid data
      STACK("DFTGRID:set_radial_grid_data")
      START_TIMER("DFTGRID:set_radial_grid_data")
      select case (self%radial_grid_kind)
         case ("gauss-chebyshev");   self%n_radial_pts = self%radial_grid_order
         case ("gauss_chebyshev");   self%n_radial_pts = self%radial_grid_order
         case ("euler-maclaurin");   self%n_radial_pts = self%radial_grid_order
         case ("euler_maclaurin");   self%n_radial_pts = self%radial_grid_order
         case default;               allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "gauss-chebyshev"
         tonto%known_keywords(2) = "gauss_chebyshev"
         tonto%known_keywords(3) = "euler-maclaurin"
         tonto%known_keywords(4) = "euler_maclaurin"
         call unknown_(tonto,self%radial_grid_kind,"DFTGRID:set_radial_grid_data")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("DFTGRID:set_radial_grid_data")
      CHECK
   end subroutine

   subroutine set_spherical_grid_data(self)
    DFTGRID :: self
   ! Set any spherical grid data parameters. At the moment
   ! only spherical Lebedev grids are available
      STACK("DFTGRID:set_spherical_grid_data")
      START_TIMER("DFTGRID:set_spherical_grid_data")
      select case (self%spherical_grid_kind)
         case ("lebedev");  call set_lebedev_data_(self)
         case default;      allocate(tonto%known_keywords(1))
         tonto%known_keywords(1) = "lebedev"
         call unknown_(tonto,self%spherical_grid_kind,"DFTGRID:set_spherical_grid_data")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("DFTGRID:set_spherical_grid_data")
      CHECK
   end subroutine

   subroutine set_lebedev_data(self,mm1,mm2,mm3,nn1,nn2,nn3)
    DFTGRID :: self
   ! Sets the Lebedev grid parameters
   ! NOTE: This should match "get_lebedev_wxyz"
      INT, OUT, optional :: mm1, mm2, mm3, nn1, nn2, nn3
      INT :: m1, m2, m3, n1, n2, n3
      ! NOTE: If ai = ZERO, then .mi  = 0, for i = 1, 2, 3.
      STACK("DFTGRID:set_lebedev_data")
      START_TIMER("DFTGRID:set_lebedev_data")
      m1 = 6; m2 = 12; m3 = 8
      select case (self%spherical_grid_order)
         case (9);  m2 = 0;         n1 =  0; n2 =  1; n3 =  0
         case (11);                 n1 =  1; n2 =  0; n3 =  0
         case (13); m2 = 0; m3 = 0; n1 =  2; n2 =  1; n3 =  0
         case (15); m2 = 0;         n1 =  2; n2 =  1; n3 =  0
         case (17); m1 = 0;         n1 =  3; n2 =  1; n3 =  0
         case (19);                 n1 =  3; n2 =  0; n3 =  1
         case (23);                 n1 =  4; n2 =  1; n3 =  1
         case (25); m2 = 0;         n1 =  5; n2 =  2; n3 =  1
         case (27);                 n1 =  5; n2 =  1; n3 =  2
         case (29); m2 = 0;         n1 =  6; n2 =  2; n3 =  2
         case (35);                 n1 =  7; n2 =  2; n3 =  4
         case (41); m2 = 0;         n1 =  9; n2 =  3; n3 =  6
         case (47);                 n1 = 10; n2 =  3; n3 =  9
         case (53); m2 = 0;         n1 = 12; n2 =  4; n3 = 12
         case (59);                 n1 = 13; n2 =  4; n3 = 16
         case default; DIE("DFTGRID:set_lebedev_data ... Lebedev grid order number doesn't exist")
      end select
      self%n_spherical_pts = m1 + m2 + m3 + 24*n1 + 24*n2 + 48*n3
      if (present(mm1)) mm1 = m1
      if (present(mm2)) mm2 = m2
      if (present(mm3)) mm3 = m3
      if (present(nn1)) nn1 = n1
      if (present(nn2)) nn2 = n2
      if (present(nn3)) nn3 = n3
     STOP_TIMER("DFTGRID:set_lebedev_data")
      CHECK
   end subroutine

!  ************************
!  Grid integration methods
!  ************************

   function integrate(self,f,atom) result(res)
    DFTGRID :: self
   ! Integrate the vector function "f" over grids defined for the atoms
   ! in the ATOMVEC "atom".
      interface
         function f(r) result(res)
             REALVEC(3) :: r
            REAL :: res
         end function
      end interface
      ATOMVEC(:) :: atom
      REAL :: res
      INT :: n_pt, a, n
      REALMAT(:,:), PTR :: pt
      REALVEC(:), PTR :: wt
      STACK("DFTGRID:integrate")
      START_TIMER("DFTGRID:integrate")
      ENSURE(self%finalized,"DFTGRID:integrate ... call the set_grid_data routine")
      res = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call get_atom_grid_(self)
      do a = 1, size(atom)
         call rescale_displace_partition_(self,pt,wt,a,atom)
         do n = 1,n_pt
            res = res + wt(n)*f(pt(n,:))
         end do
      end do
      call destroy_(wt);  call destroy_(pt)
     STOP_TIMER("DFTGRID:integrate")
      CHECK
   end function

   subroutine make_matrix_elements_of(self,V,g,ans)
    DFTGRID :: self
   ! Integrate the matrix elements of a multiplicative operator "V"
   ! between all components of two gaussian functions given by the
   ! gaussian-pair "g". The result of the integration is matrix "ans".
   ! Operator "V" is represented by a function which returns a "v_grid"
   ! on a set of "pts".
      interface
         subroutine V(v_grid,pts)
            REALVEC(:) :: v_grid
            REALMAT(:,:) :: pts
         end subroutine
      end interface
      GAUSSIAN2 :: g
      REALMAT(:,:) :: ans
      INT :: n_pt,n,a,b
      REALMAT(:,:), PTR :: pt,pos,a_grid,b_grid
      REALVEC(:), PTR :: wt,scale,v_grid
      STACK("DFTGRID:make_matrix_elements_of")
      START_TIMER("DFTGRID:make_matrix_elements_of")
      ENSURE(self%finalized,"DFTGRID:make_matrix_elements_of ... call the set_grid_data routine")
      ENSURE(size(ans,1)==n_comp_(g%a),"DFTGRID:make_matrix_elements_of ... wrong size, ans")
      ENSURE(size(ans,2)==n_comp_(g%b),"DFTGRID:make_matrix_elements_of ... wrong size, ans")
      ans = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,n_comp_(g%a))
      call create_(b_grid,n_pt,n_comp_(g%b))
      call get_atom_grid_(self)
      if (equals_(g%a%pos,g%b%pos)) then
         call create_(pos,1,3); pos(1,:) = g%a%pos
         call create_(scale,1); scale(:) = ONE
      else
         call create_(pos,2,3); pos(1,:) = g%a%pos
                          pos(2,:) = g%b%pos
         call create_(scale,2); scale(:) = ONE
      end if
      do n = 1,size(pos,1)
         call rescale_displace_partition_(self,pt,wt,n,pos,scale)
         call V(v_grid,pt)
         v_grid = wt*v_grid
         call make_grid_(g%a,a_grid,pt)
         call make_grid_(g%b,b_grid,pt)
         do a = 1,n_comp_(g%a)
         do b = 1,n_comp_(g%b)
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
         end do
         end do
      end do
      call destroy_(scale)
      call destroy_(pos)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)
     STOP_TIMER("DFTGRID:make_matrix_elements_of")
      CHECK
   end subroutine

   subroutine make_cmplx_matrix_elements_of(self,V,g,ans)
    DFTGRID :: self
   ! Integrate the matrix elements of a complex multiplicative operator "V"
   ! between all components of two real gaussian functions given by the
   ! gaussian-pair "g". The result of the integration is matrix "ans".
   ! Operator "V" is represented by a function which returns a "v_grid"
   ! on a set of "pts".
      interface
         subroutine V(v_grid,pts)
            CPXVEC(:) :: v_grid
            REALMAT(:,:) :: pts
         end subroutine
      end interface
      GAUSSIAN2 :: g
      CPXMAT(:,:) :: ans
      INT :: n_pt,n,a,b
      REALMAT(:,:), PTR :: pt,pos,a_grid,b_grid
      REALVEC(:), PTR :: wt,scale
      CPXVEC(:), PTR :: v_grid
      STACK("DFTGRID:make_cmplx_matrix_elements_of")
      START_TIMER("DFTGRID:make_cmplx_matrix_elements_of")
      ENSURE(self%finalized,"DFTGRID:make_cmplx_matrix_elements_of ... call the set_grid_data routine")
      ENSURE(size(ans,1)==n_comp_(g%a),"DFTGRID:make_cmplx_matrix_elements_of ... wrong size, ans")
      ENSURE(size(ans,2)==n_comp_(g%b),"DFTGRID:make_cmplx_matrix_elements_of ... wrong size, ans")
      ans = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,n_comp_(g%a))
      call create_(b_grid,n_pt,n_comp_(g%b))
      call get_atom_grid_(self)
      if (equals_(g%a%pos,g%b%pos)) then
         call create_(pos,1,3); pos(1,:) = g%a%pos
         call create_(scale,1); scale(:) = ONE
      else
         call create_(pos,2,3); pos(1,:) = g%a%pos
                          pos(2,:) = g%b%pos
         call create_(scale,2); scale(:) = ONE
      end if
      do n = 1,size(pos,1)
         call rescale_displace_partition_(self,pt,wt,n,pos,scale)
         call V(v_grid,pt)
         v_grid = wt*v_grid
         call make_grid_(g%a,a_grid,pt)
         call make_grid_(g%b,b_grid,pt)
         do a = 1,n_comp_(g%a)
         do b = 1,n_comp_(g%b)
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
         end do
         end do
      end do
      call destroy_(scale)
      call destroy_(pos)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)
     STOP_TIMER("DFTGRID:make_cmplx_matrix_elements_of")
      CHECK
   end subroutine

   function integrate_molecular_property(self,X,mol) result(res)
    DFTGRID :: self
   ! Integrate a scalar molecular property, which is represented by a
   ! subroutine "X" which returns "values" of the property in a vector,
   ! given "mol" as the molecule, and "pts" as a set of points.
   ! The result of the integration is "res".
      interface
         subroutine X(mol,values,pts)
   use TYPES_MODULE
            MOL :: mol
            REALVEC(:) :: values
            REALMAT(:,:) :: pts
         end subroutine
      end interface
      MOL :: mol
      REAL :: res
      INT :: n_pt, a, n
      REALMAT(:,:), PTR :: pt
      REALVEC(:), PTR :: wt,values
      STACK("DFTGRID:integrate_molecular_property")
      START_TIMER("DFTGRID:integrate_molecular_property")
      ENSURE(self%finalized,"DFTGRID:integrate_molecular_property ... call the set_grid_data routine")
      res = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(values,n_pt)
      call get_atom_grid_(self)
      do a = 1, mol%n_atom
         call rescale_displace_partition_(self,pt,wt,a, mol%atom)
         call X(mol,values,pt)
         do n = 1,n_pt
            res = res+ wt(n)*values(n)
         end do
      end do
      call destroy_(values)
      call destroy_(wt);  call destroy_(pt)
     STOP_TIMER("DFTGRID:integrate_molecular_property")
      CHECK
   end function

   subroutine make_matrix_elements_of_1(self,V,mol,shell,ans)
    DFTGRID :: self
   ! Integrate the matrix elements of a multiplicative operator "V" for
   ! molecule "mol" between all components of two A.O. shells given by
   ! the shell-pair "shell". The result of the integration is matrix "ans".
   ! Operator "V" is represented by a subroutine which returns "v_grid"
   ! on a set of "pts" for a given molecule "mol".
   !
   ! This routine calculates: (a|V|b)
   !
      interface
         subroutine V(mol,v_grid,pts)
   use TYPES_MODULE
            MOL :: mol
            REALVEC(:) :: v_grid
            REALMAT(:,:) :: pts
         end subroutine
      end interface
      MOL :: mol
      SHELL2 :: shell
      REALMAT(:,:) :: ans
      ATOMVEC(:), PTR :: atom1
      REALMAT(:,:), PTR :: pt,a_grid,b_grid
      REALVEC(:), PTR :: wt,v_grid
      INT :: n_pt, n,a,b,atom_a,atom_b
      STACK("DFTGRID:make_matrix_elements_of_1")
      START_TIMER("DFTGRID:make_matrix_elements_of_1")
      ENSURE(self%finalized,"DFTGRID:make_matrix_elements_of_1 ... call the set_grid_data routine")
      ans = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,shell%a%n_comp)
      call create_(b_grid,n_pt,shell%b%n_comp)
      call get_atom_grid_(self)
      atom_a = atom_index_from_pos_(mol%atom,shell%a%pos)
      atom_b = atom_index_from_pos_(mol%atom,shell%b%pos)
      if (atom_a==atom_b) then
         call create_(atom1,1)
         atom1(1) = mol%atom(atom_a)
      else
         call create_(atom1,2)
         atom1(1) = mol%atom(atom_a)
         atom1(2) = mol%atom(atom_b)
      end if
      do n = 1,size(atom1)
         call rescale_displace_partition_(self,pt,wt,n, atom1)
         call V(mol,v_grid,pt)
         v_grid = wt*v_grid
         call make_grid_(shell%a,a_grid,pt)
         call make_grid_(shell%b,b_grid,pt)
         do a = 1,shell%a%n_comp
         do b = 1,shell%b%n_comp
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
         end do
         end do
      end do
      call nullify_ptr_part_(atom1); call destroy_(atom1)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)
      call flush_(stdout)
     STOP_TIMER("DFTGRID:make_matrix_elements_of_1")
      CHECK
   end subroutine

   subroutine make_NL_matrix_elements_of(self,V,mol,shell,ans)
    DFTGRID :: self
   ! This is the non-local version of "make_matrix_elements_of".
   ! It is specifically designed to calculate the matrix elements of
   ! non-local functionals --- i.e. that contain rho and nabla_rho.
   ! It calculates:
   !
   !         INT[dF/d(rho)*ab] + INT[dF/d(nabla_rho)*nabla(ab)],
   !
   ! where a, b are basis functions, and INT = "integral of".
   !
   ! Some of the variables used in this routine are:
   !
   ! ans       = matrix result of calculation
   ! v_grid    = dF/d(rho) on the grid of points
   ! nl_v_grid = dF/d(nabla_rho) = non-local v_grid on the grid of points
   ! a_grid    = basis function values on the grid of points
   ! nabla_a_grid = nabla of basis function values on the grid of points
   !
      interface
         subroutine V(mol,v_grid,nl_v_grid,pts)
   use TYPES_MODULE
            MOL :: mol
            REALVEC(:) :: v_grid
            REALMAT(:,:) :: nl_v_grid
            REALMAT(:,:) :: pts
         end subroutine
      end interface
      MOL :: mol
      SHELL2 :: shell
      REALMAT(:,:) :: ans
      ATOMVEC(:), PTR :: atom1
      INT :: n_pt, n,a,b,atom_a,atom_b
      REALMAT(:,:), PTR :: pt
      REALVEC(:), PTR :: wt,v_grid
      REALMAT(:,:), PTR :: nl_v_grid
      REALMAT(:,:), PTR :: a_grid,b_grid
      REALMAT3(:,:,:), PTR :: nabla_a_grid, nabla_b_grid
      STACK("DFTGRID:make_NL_matrix_elements_of")
      START_TIMER("DFTGRID:make_NL_matrix_elements_of")
      ENSURE(self%finalized,"DFTGRID:make_NL_matrix_elements_of ... call the set_grid_data routine")
      ans = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(nl_v_grid,n_pt,3)
      call create_(a_grid,n_pt,shell%a%n_comp)
      call create_(b_grid,n_pt,shell%b%n_comp)
      call create_(nabla_a_grid,n_pt,shell%a%n_comp,3)
      call create_(nabla_b_grid,n_pt,shell%b%n_comp,3)
      call get_atom_grid_(self)
      atom_a = atom_index_from_pos_(mol%atom,shell%a%pos)
      atom_b = atom_index_from_pos_(mol%atom,shell%b%pos)
      if (atom_a==atom_b) then
         call create_(atom1,1)
         atom1(1) = mol%atom(atom_a)
      else
         call create_(atom1,2)
         atom1(1) = mol%atom(atom_a)
         atom1(2) = mol%atom(atom_b)
      end if
      do n = 1,size(atom1)
         call rescale_displace_partition_(self,pt,wt,n, atom1)
         call V(mol,v_grid,nl_v_grid,pt)
         v_grid = wt*v_grid
         nl_v_grid(:,1) = wt*nl_v_grid(:,1)
         nl_v_grid(:,2) = wt*nl_v_grid(:,2)
         nl_v_grid(:,3) = wt*nl_v_grid(:,3)
         call make_grid_(shell%a,a_grid,pt)
         call make_grid_(shell%b,b_grid,pt)
         call make_nabla_grid_(shell%a,nabla_a_grid,pt)
         call make_nabla_grid_(shell%b,nabla_b_grid,pt)
         do a = 1,shell%a%n_comp
         do b = 1,shell%b%n_comp
            wt = a_grid(:,a)*b_grid(:,b)
            ans(a,b) = ans(a,b) + sum(wt*v_grid)
            wt = nabla_a_grid(:,a,1)*b_grid(:,b) + a_grid(:,a)*nabla_b_grid(:,b,1)
            ans(a,b) = ans(a,b) + sum(wt*nl_v_grid(:,1))
            wt = nabla_a_grid(:,a,2)*b_grid(:,b) + a_grid(:,a)*nabla_b_grid(:,b,2)
            ans(a,b) = ans(a,b) + sum(wt*nl_v_grid(:,2))
            wt = nabla_a_grid(:,a,3)*b_grid(:,b) + a_grid(:,a)*nabla_b_grid(:,b,3)
            ans(a,b) = ans(a,b) + sum(wt*nl_v_grid(:,3))
         end do
         end do
      end do
      call nullify_ptr_part_(atom1); call destroy_(atom1)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(nabla_b_grid); call destroy_(nabla_a_grid)
      call destroy_(v_grid); call destroy_(nl_v_grid)
      call destroy_(wt);  call destroy_(pt)
     STOP_TIMER("DFTGRID:make_NL_matrix_elements_of")
      CHECK
   end subroutine

   subroutine make_SO_matrix_elements_of(self,V,mol,shell,ans)
    DFTGRID :: self
   ! Integrate the spin orbit matrix elements of a multiplicative operator "V"
   ! for molecule "mol" between all gradient (nabla) components and shell
   ! components of two A.O. shells, given by the shell-pair "shell". The result
   ! of the integration is matrix "ans". Operator "V" is represented by a
   ! subroutine which returns "v_grid" on a set of "pts" for a given molecule
   ! "mol".
      interface
         subroutine V(mol,v_grid,pts)
   use TYPES_MODULE
            MOL :: mol
            REALVEC(:) :: v_grid
            REALMAT(:,:) :: pts
         end subroutine
      end interface
      MOL :: mol
      SHELL2 :: shell
      REALMAT4(:,:,:,:) :: ans
      ATOMVEC(:), PTR :: atom1
      INT :: n_pt, n,k,l,a,b,atom_a,atom_b
      REALMAT(:,:), PTR :: pt
      REALVEC(:), PTR :: wt, v_grid
      REALMAT3(:,:,:), PTR :: a_grid,b_grid
      STACK("DFTGRID:make_SO_matrix_elements_of")
      START_TIMER("DFTGRID:make_SO_matrix_elements_of")
      ENSURE(self%finalized,"DFTGRID:make_SO_matrix_elements_of ... call the set_grid_data routine")
      ans = ZERO
      n_pt = self%n_pts
      call create_(pt,n_pt,3);  call create_(wt,n_pt)
      call create_(v_grid,n_pt)
      call create_(a_grid,n_pt,shell%a%n_comp,3)
      call create_(b_grid,n_pt,shell%b%n_comp,3)
      call get_atom_grid_(self)
      atom_a = atom_index_from_pos_(mol%atom,shell%a%pos)
      atom_b = atom_index_from_pos_(mol%atom,shell%b%pos)
      if (atom_a==atom_b) then
         call create_(atom1,1)
         atom1(1) = mol%atom(atom_a)
      else
         call create_(atom1,2)
         atom1(1) = mol%atom(atom_a)
         atom1(2) = mol%atom(atom_b)
      end if
      do n = 1,size(atom1)
         call rescale_displace_partition_(self,pt,wt,n, atom1)
         call V(mol,v_grid,pt)
         v_grid = wt*v_grid
         call make_nabla_grid_(shell%a,a_grid,pt)
         call make_nabla_grid_(shell%b,b_grid,pt)
         do k = 1,3
         do l = 1,3
         do a = 1,shell%a%n_comp
         do b = 1,shell%b%n_comp
            wt = a_grid(:,a,k)*b_grid(:,b,l)
            ans(a,b,k,l) = ans(a,b,k,l) + sum(wt*v_grid)
         end do
         end do
         end do
         end do
      end do
      call nullify_ptr_part_(atom1); call destroy_(atom1)
      call destroy_(b_grid); call destroy_(a_grid)
      call destroy_(v_grid)
      call destroy_(wt);  call destroy_(pt)
     STOP_TIMER("DFTGRID:make_SO_matrix_elements_of")
      CHECK
   end subroutine

!  ************************
!  Grid generation routines
!  ************************

   subroutine get_grid(self,pt,wt,atom)
    DFTGRID :: self
   ! Get from an archive an entire dft integration grid, points and weights
   ! ("pt","wt") for a list of "atom" positions.
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      ATOMVEC(:) :: atom
      STACK("DFTGRID:get_grid")
      START_TIMER("DFTGRID:get_grid")
      call set_name_(self%archive,"DFT_grid")
      if (NOT exists_(self%archive)) then
        call make_grid_(self,pt,wt,atom)
        call write_(self%archive,pt,wt)
      else
        call read_(self%archive,pt,wt)
      end if
     STOP_TIMER("DFTGRID:get_grid")
      CHECK
   end subroutine

   subroutine make_grid(self,pt,wt,atom)
    DFTGRID :: self
   ! Make an entire dft integration grid, points and weights ("pt","wt")
   ! for a given a list of "atom" positions.
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      ATOMVEC(:) :: atom
      INT :: n_pt, a, n, n_atom
      STACK("DFTGRID:make_grid")
      START_TIMER("DFTGRID:make_grid")
      ENSURE(self%finalized,"DFTGRID:make_grid ... call the set_grid_data routine")
      ENSURE(size(pt,1)==self%n_pts*size(atom),"DFTGRID:make_grid ... wrong dimension, pt")
      ENSURE(size(pt,2)==3,"DFTGRID:make_grid ... wrong size, pt")
      ENSURE(size(wt)==self%n_pts*size(atom),"DFTGRID:make_grid ... wrong dimension, wt")
      n_atom = size(atom)
      n_pt = self%n_pts
      call get_atom_grid_(self)
      n = 0
      do a = 1,n_atom
         call rescale_displace_partition_(self,pt(n+1:n+n_pt,:),wt(n+1:n+n_pt),a,atom)
         n = n + n_pt
      end do
     STOP_TIMER("DFTGRID:make_grid")
      CHECK
   end subroutine

   subroutine rescale_displace_partition(self,pt,wt,a,atom)
    DFTGRID :: self
   ! Rescales the original single atom grid to adjust it to the atom size, then
   ! displaces the grid from the origin to the position of "atom" number "a",
   ! and partition it to work with all the other atoms, returning the grid
   ! ("pt","wt") = ("pt0" + disp, partition*"wt0") The weights are altered
   ! according to the Becke atomic partition function for the displaced grid
   ! centred on atom "a".
   ! NOTE : all the atoms in "atom" must be distinct!!!!!!
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      INT, IN :: a
      ATOMVEC(:), IN :: atom
      INT :: n, m, n_atom, i, j, ii
      REALVEC(3) :: disp,rij,rni,rnj
      REAL :: dij,dni,dnj,uij,vij, bsri, bsrj, chi, wij, aij, h, s, alpha
      REALVEC(3) :: posi,posj
      REALMAT(:,:), PTR :: partition
      STR(STR_SIZE) :: symbol
      STACK("DFTGRID:rescale_displace_partition")
      START_TIMER("DFTGRID:rescale_displace_partition")
      ENSURE(self%finalized,"DFTGRID:rescale_displace_partition ... call the set_grid_data routine")
      ENSURE(size(pt,1)==self%n_pts,"DFTGRID:rescale_displace_partition ... pt incorrectly dimensioned")
      ENSURE(size(pt,2)==3,"DFTGRID:rescale_displace_partition ... pt incorrectly dimensioned")
      ENSURE(size(wt)==self%n_pts,"DFTGRID:rescale_displace_partition ... wt incorrectly dimensioned")
      ENSURE(a>=1 AND a<= size(atom),"DFTGRID:rescale_displace_partition ... `a' argument out of range")
      ENSURE(associated(self%single_atom_points),"DFTGRID:rescale_displace_partition ... atom point grid not created")
      ENSURE(associated(self%single_atom_weights),"DFTGRID:rescale_displace_partition ... atom weight grid not created")
      ENSURE(size(self%single_atom_points,1)==self%n_pts,"DFTGRID:rescale_displace_partition ... points incorrectly dimensioned")
      ENSURE(size(self%single_atom_points,2)==3,"DFTGRID:rescale_displace_partition ... points incorrectly dimensioned")
      ENSURE(size(self%single_atom_weights)==self%n_pts,"DFTGRID:rescale_displace_partition ... weights incorrectly dimensioned")
      pt = self%single_atom_points
      wt = self%single_atom_weights
      symbol = chemical_symbol_(atom(a))     ! <-- Rescale the grid according to Becke's method
      if (symbol == "H") then
         alpha =      bragg_slater_radius_(atom(a))
      else
         alpha = HALF*bragg_slater_radius_(atom(a))
      end if
      wt = alpha*alpha*alpha*wt
      pt = alpha*pt
      disp = atom(a)%pos                   ! <-- Displace the grid
      do n = 1,self%n_pts
          pt(n,:) = pt(n,:) + disp
      end do
      m = self%becke_m_partition_power         ! <-- Partition the grid, below
      n_atom = size(atom)                  ! Number of atoms
      call create_(partition,self%n_pts,n_atom) ! Create partition array
      partition = ONE
      do i = 1,n_atom
          bsri = bragg_slater_radius_(atom(i))
          posi = atom(i)%pos
          do j = 1,n_atom
              if (i==j) cycle
              bsrj = bragg_slater_radius_(atom(j))
              chi = bsri/bsrj
              wij = (chi - ONE)/(chi + ONE)
              aij = wij/(wij*wij - ONE)
              posj = atom(j)%pos
              rij = posi-posj
              dij = sqrt(dot_product(rij,rij))
              do n = 1,self%n_pts
                  rni = pt(n,:) - posi
                  rnj = pt(n,:) - posj
                  dni = sqrt(dot_product(rni,rni))
                  dnj = sqrt(dot_product(rnj,rnj))
                  uij = (dni - dnj)/dij
                  vij = uij + aij*(ONE - uij*uij)
                  h = vij
                  do ii = 1, m
                      h = 1.5d0*h - HALF*h*h*h
                  end do
                  ! Optimise by doubling inside the do loop, and then halve it outside?
                  s = HALF*(ONE - h)
                  ! ===========================================
                  partition(n,i) = partition(n,i)*s
              end do
          end do
      end do
      ! Normalize the partitioning function and modify the grid weights
      do n = 1,self%n_pts
          wt(n) = wt(n)*partition(n,a)/sum(partition(n,:))
      end do
      call destroy_(partition)
     STOP_TIMER("DFTGRID:rescale_displace_partition")
      CHECK
   end subroutine

   subroutine rescale_displace_partition_1(self,pt,wt,a,pos,scale)
    DFTGRID :: self
   ! Rescales the original single atom grid according to the "scale" factors for
   ! that position "pos", then displaces the grid from the origin to the
   ! position "pos(a,:)", and partition it to work with all the other positions,
   ! returning the grid ("pt","wt") = ("pt0" + disp, partition*"wt0") The
   ! weights are altered according to a Becke-like atomic partition function
   ! where the scales are re-interpreted as mean extents.
   ! NOTE : all the positions in "atom" must be distinct!!!!!!
   ! References: A. D. Becke, J. Chem. Phys. 88 (1988) 2547.
   !             O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      INT, IN :: a
      REALMAT(:,:), IN :: pos
      REALVEC(:) :: scale
      INT :: n, m, n_centers, i, j, ii
      REALVEC(3) :: disp,rij,rni,rnj
      REAL :: dij,dni,dnj,uij,vij, bsri, bsrj, chi, wij, aij, h, s, alpha
      REALVEC(3) :: posi,posj
      REALMAT(:,:), PTR :: partition
      STACK("DFTGRID:rescale_displace_partition_1")
      START_TIMER("DFTGRID:rescale_displace_partition_1")
      ENSURE(self%finalized,"DFTGRID:rescale_displace_partition_1 ... call the set_grid_data routine")
      ENSURE(size(pt,1)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... pt incorrectly dimensioned")
      ENSURE(size(pt,2)==3,"DFTGRID:rescale_displace_partition_1 ... pt incorrectly dimensioned")
      ENSURE(size(wt)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... wt incorrectly dimensioned")
      ENSURE(a>=1 AND a<= size(pos,1),"DFTGRID:rescale_displace_partition_1 ... `a' argument out of range")
      ENSURE(associated(self%single_atom_points),"DFTGRID:rescale_displace_partition_1 ... atom point grid not created")
      ENSURE(associated(self%single_atom_weights),"DFTGRID:rescale_displace_partition_1 ... atom weight grid not created")
      ENSURE(size(self%single_atom_points,1)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... points incorrectly dimensioned")
      ENSURE(size(self%single_atom_points,2)==3,"DFTGRID:rescale_displace_partition_1 ... points incorrectly dimensioned")
      ENSURE(size(self%single_atom_weights)==self%n_pts,"DFTGRID:rescale_displace_partition_1 ... weights incorrectly dimensioned")
      pt = self%single_atom_points
      wt = self%single_atom_weights
      ! Rescale the grid according to Becke's method
      alpha = scale(a)
      wt = alpha*alpha*alpha*wt
      pt = alpha*pt
      disp = pos(a,:)                      ! <-- Displace the grid
      do n = 1,self%n_pts
          pt(n,:) = pt(n,:) + disp
      end do
      m = self%becke_m_partition_power         ! <-- Partition the grid, below
      n_centers = size(pos,1)                 ! Number of integration (atom) centres
      call create_(partition,self%n_pts,n_centers) ! Create partition array
      partition = ONE
      do i = 1,n_centers
          bsri = scale(i)
          posi = pos(i,:)
          do j = 1,n_centers
              if (i==j) cycle
              bsrj = scale(j)
              chi = bsri/bsrj
              wij = (chi - ONE)/(chi + ONE)
              aij = wij/(wij*wij - ONE)
              posj = pos(j,:)
              rij = posi-posj
              dij = sqrt(dot_product(rij,rij))
              do n = 1,self%n_pts
                  rni = pt(n,:) - posi
                  rnj = pt(n,:) - posj
                  dni = sqrt(dot_product(rni,rni))
                  dnj = sqrt(dot_product(rnj,rnj))
                  uij = (dni - dnj)/dij
                  vij = uij + aij*(ONE - uij*uij)
                  h = vij
                  do ii = 1, m
                      h = 1.5d0*h - HALF*h*h*h
                  end do
                  s = HALF*(ONE - h)
                  ! ===========================================
                  partition(n,i) = partition(n,i)*s
              end do
          end do
      end do
      ! Normalize the partitioning function and modify the grid weights
      do n = 1,self%n_pts
          wt(n) = wt(n)*partition(n,a)/sum(partition(n,:))
      end do
      call destroy_(partition)
     STOP_TIMER("DFTGRID:rescale_displace_partition_1")
      CHECK
   end subroutine

   subroutine get_atom_grid(self)
    DFTGRID :: self
   ! Ensure the atom grid has been made.
      STACK("DFTGRID:get_atom_grid")
      START_TIMER("DFTGRID:get_atom_grid")
      if (NOT associated(self%single_atom_points)) then
        ENSURE(NOT associated(self%single_atom_weights),"DFTGRID:get_atom_grid ... inconsistently allocated atom weights")
        call create_(self%single_atom_points,self%n_pts,3)
        call create_(self%single_atom_weights,self%n_pts)
       call make_atom_grid_(self,self%single_atom_points,self%single_atom_weights)
      end if
     STOP_TIMER("DFTGRID:get_atom_grid")
      CHECK
   end subroutine

   subroutine make_atom_grid(self,pt,wt)
    DFTGRID :: self
   ! Make the dft grid points "pt", and weights "wt" at the origin
   ! suitable for integrating a single atom. It is made as the
   ! direct product of a spherical grid and radial grid.
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      INT :: i, j, k
      REAL :: pi4, pi4r2w
      REALMAT(:,:), PTR :: spherical_pt
      REALVEC(:), PTR :: spherical_wt,radial_pt,radial_wt
      STACK("DFTGRID:make_atom_grid")
      START_TIMER("DFTGRID:make_atom_grid")
      ENSURE(self%finalized,"DFTGRID:make_atom_grid ... call the set_grid_data routine")
      ENSURE(size(pt,1)==self%n_pts,"DFTGRID:make_atom_grid ... pt incorrectly dimensioned")
      ENSURE(size(pt,2)==3,"DFTGRID:make_atom_grid ... pt incorrectly dimensioned")
      ENSURE(size(wt)==self%n_pts,"DFTGRID:make_atom_grid ... wt incorrectly dimensioned")
      ! Allocate the spherical and radial grids
      call create_(spherical_pt,self%n_spherical_pts,3)
      call create_(spherical_wt,self%n_spherical_pts)
      call create_(radial_pt,self%n_radial_pts)
      call create_(radial_wt,self%n_radial_pts)
      ! Make the grids
      call make_spherical_grid_(self,spherical_pt,spherical_wt)
      call make_radial_grid_(self,radial_pt,radial_wt)
      ! Do the direct product
      pi4 = FOUR*PI          ! Include the factor 4*pi*r^2 in the weights.
      k = 0
      do i = 1,self%n_radial_pts
         pi4r2w = pi4*radial_pt(i)*radial_pt(i)*radial_wt(i)
         do j = 1,self%n_spherical_pts
            k = k + 1
            wt(k)   = pi4r2w*spherical_wt(j)
            pt(k,:) = radial_pt(i)*spherical_pt(j,:)
         end do
      end do
      call destroy_(radial_wt)
      call destroy_(radial_pt)
      call destroy_(spherical_wt)
      call destroy_(spherical_pt)
     STOP_TIMER("DFTGRID:make_atom_grid")
      CHECK
   end subroutine

   subroutine make_radial_grid(self,pt,wt)
    DFTGRID :: self
   ! Make a one-dimensional radial array of coordinates "pt"
   ! and weights "wt"
     REALVEC(:) :: pt,wt
     STACK("DFTGRID:make_radial_grid")
     START_TIMER("DFTGRID:make_radial_grid")
     select case (self%radial_grid_kind)
       case ("gauss-chebyshev");   call make_gauss_chebyshev_grid_(self,pt,wt)
       case ("gauss_chebyshev");   call make_gauss_chebyshev_grid_(self,pt,wt)
       case ("euler-maclaurin");   call make_euler_maclaurin_grid_(self,pt,wt)
       case ("euler_maclaurin");   call make_euler_maclaurin_grid_(self,pt,wt)
       case default;               allocate(tonto%known_keywords(4))
       tonto%known_keywords(1) = "gauss-chebyshev"
       tonto%known_keywords(2) = "gauss_chebyshev"
       tonto%known_keywords(3) = "euler-maclaurin"
       tonto%known_keywords(4) = "euler_maclaurin"
       call unknown_(tonto,self%radial_grid_kind,"DFTGRID:make_radial_grid")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("DFTGRID:make_radial_grid")
      CHECK
   end subroutine

   subroutine make_gauss_chebyshev_grid(self,pt,wt)
    DFTGRID :: self
   ! Make a one-dimensional radial Gauss-Chebyshev array of
   ! coordinates "pt" and weights "wt".
      REALVEC(:) :: pt,wt
      INT :: i, num
      REAL :: alpha, m, rm, pm, tm, pf, a, x
      STACK("DFTGRID:make_gauss_chebyshev_grid")
      START_TIMER("DFTGRID:make_gauss_chebyshev_grid")
      alpha = self%gauss_chebyshev_alpha
      m     = self%gauss_chebyshev_m
      num   = self%radial_grid_order
      rm = ONE/m
      pm = ONE + m
      tm = ONE - m
      pf = PI/num
      do i = 1, num
         a = cos(pf*(i-HALF))
         !a = cos(PI*(2*i-1)/(TWO*num))
         x = (ONE + a)/(ONE - a)**rm
         pt(i) = alpha*x
         wt(i) = alpha*x*pf*(pm + a*tm)/(m*sqrt(ONE - a*a))
      end do
     STOP_TIMER("DFTGRID:make_gauss_chebyshev_grid")
      CHECK
   end subroutine

   subroutine make_euler_maclaurin_grid(self,pt,wt)
    DFTGRID :: self
   ! Make a one-dimensional radial Euler-Maclaurin array of
   ! coordinates "pt" and weights "wt"
      REALVEC(:) :: pt,wt
      INT :: i, num
      REAL :: alpha, m, pm, tm, pf, n1, a, x
      STACK("DFTGRID:make_euler_maclaurin_grid")
      START_TIMER("DFTGRID:make_euler_maclaurin_grid")
      alpha = self%euler_maclaurin_alpha
      m     = self%euler_maclaurin_m
      num   = self%radial_grid_order
      pm = ONE + m
      tm = ONE - m
      n1 = ONE + num
      pf = m/n1
      do i = 1, num
         a = i/n1
         x = (a**m)/(ONE - a)**m
         pt(i) = alpha*x
         wt(i) = alpha*pf*(a**(-tm))/(ONE - a)**pm
      end do
     STOP_TIMER("DFTGRID:make_euler_maclaurin_grid")
      CHECK
   end subroutine

   subroutine make_spherical_grid(self,pt,wt)
    DFTGRID :: self
   ! Make a three-dimensional spherical array of points "pt"
   ! and a one ! dimensional array of weights "wt".
   ! At the moment only spherical Lebedev grids are available
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      STACK("DFTGRID:make_spherical_grid")
      START_TIMER("DFTGRID:make_spherical_grid")
      select case (self%spherical_grid_kind)
         case ("lebedev");  call make_lebedev_grid_(self,pt,wt)
         case default;      allocate(tonto%known_keywords(1))
         tonto%known_keywords(1) = "lebedev"
         call unknown_(tonto,self%spherical_grid_kind,"DFTGRID:make_spherical_grid")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("DFTGRID:make_spherical_grid")
      CHECK
   end subroutine

   subroutine make_lebedev_grid(self,pt,wt)
    DFTGRID :: self
   ! Make a three-dimensional spherical Lebedev array of points "pt"
   ! and a one dimensional array of weights "wt".
      REALMAT(:,:) :: pt
      REALVEC(:) :: wt
      INT :: m1, m2, m3, n1, n2, n3
      INT :: k, n
      REAL :: a1, a2, a3! coefficients of quadrature expansion
      REALVEC(:), PTR :: b, c, d! coefficients of quadrature expansion
      REALVEC(:), PTR :: l, p, r, s! used to generate coordinates of grid
      REAL :: m, q, w! used to generate coordinates of grid
      REAL :: sr2, sr3
      STACK("DFTGRID:make_lebedev_grid")
      START_TIMER("DFTGRID:make_lebedev_grid")
      pt = ZERO
      wt = ZERO
      call set_lebedev_data_(self,m1,m2,m3,n1,n2,n3)
      call create_(b,n1); call create_(c,n2); call create_(d,n3)
      call create_(l,n1)
      call create_(p,n2)
      call create_(r,n3); call create_(s,n3)
      select case (self%spherical_grid_order)
         case (9)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
            !
            ENSURE(n1==0 AND n2==1 AND n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 1.0d0/105.0d0
            a2   = ZERO
            a3   = 9.0d0/280.0d0
            c(1) = 1.0d0/35.0d0
            p(1) = 0.888073833977d0
         case (11)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
            !
            ENSURE(n1==1 AND n2==0 AND n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 4.0d0/315.0d0
            a2   = 64.0d0/2835.0d0
            a3   = 27.0d0/1280.0d0
            b(1) = 11.0d0*11.0d0*11.0d0*11.0d0/725760.0d0
            l(1) = 0.301511344578d0
            !
         case (13)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
            !
            ENSURE(n1==2 AND n2==1 AND n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.0138665921047d0
            a2   = ZERO
            a3   = ZERO
            b(1) = 0.0130509318626d0
            b(2) = 0.0132064232231d0
            c(1) = 0.0119426635549d0
            l(1) = 0.286640146767d0
            l(2) = 0.659905001656d0
            p(1) = 0.841991943785d0
         case (15)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
            !
            ENSURE(n1==2 AND n2==1 AND n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.0115440115441d0
            a2   = ZERO
            a3   = 0.0119439090859d0
            b(1) = 0.0111105557106d0
            b(2) = 0.0118765012945d0
            c(1) = 0.0118123037469d0
            l(1) = 0.369602846454d0
            l(2) = 0.694354006603d0
            p(1) = 0.927330657151d0
         case (17)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 15 (1975) 48.
            !
            ENSURE(n1==3 AND n2==1 AND n3==0,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = ZERO
            a2   = 0.00200918797730d0
            a3   = 0.00988550016044d0
            b(1) = 0.00844068048232d0
            b(2) = 0.00987390742389d0
            b(3) = 0.00935732169000d0
            l(1) = 0.162263300152d0
            l(2) = 0.383386152638d0
            l(3) = 0.686647945709d0
            c(1) = 0.00969499636166d0
            p(1) = 0.878158910604d0
         case (19)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 16 (1976) 293.
            !
            ENSURE(n1==3 AND n2==0 AND n3==1,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 5.99631368862d-4
            a2   = 7.37299971862d-3
            a3   = 7.21051536014d-3
            b(1) = 7.57439415905d-3
            b(2) = 6.75382948631d-3
            b(3) = 7.11635549312d-3
            d(1) = 6.99108735330d-3
            l(1) = 0.157467667204d0
            l(2) = 0.417496122797d0
            l(3) = 0.676441040011d0
            r(1) = 0.882270011260d0
            s(1) = 0.140355381171d0
         case (23)
            !
            ! REF: V. I. Lebedev, Zh. Vyshisl. Mat. Mat. Fiz., 16 (1976) 293.
            !
            ENSURE(n1==4 AND n2==1 AND n3==1,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 1.78234044724d-3
            a2   = 5.71690594998d-3
            a3   = 5.57338317884d-3
            b(1) = 5.51877146727d-3
            b(2) = 5.15823771181d-3
            b(3) = 5.60870408259d-3
            b(4) = 4.10677702817d-3
            c(1) = 5.05184606462d-3
            d(1) = 5.53024891623d-3
            l(1) = 0.444693317871d0
            l(2) = 0.289246562758d0
            l(3) = 0.671297344270d0
            l(4) = 0.129933544765d0
            p(1) = 0.938319218138d0
            r(1) = 0.836036015482d0
            s(1) = 0.159041710538d0
         case (25)
            !
            ! REF: V. I. Lebedev, Sibirsk. Mat. Zh. 18 (1977) 132.
            !
            ENSURE(n1==5 AND n2==2 AND n3==1,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = -5.52263991974d-2
            a2   = ZERO
            a3   = 4.45027460745d-3
            b(1) = 3.97640801805d-3
            b(2) = 4.40140065038d-3
            b(3) = 4.49684106792d-3
            b(4) = 5.04915345048d-3
            b(5) = 1.72454435055d-2
            c(1) = 5.19806986406d-3
            c(2) = 4.23108309536d-3
            d(1) = 4.69572097257d-3
            l(1) = 0.698190665845d0
            l(2) = 0.658740524346d0
            l(3) = 0.449204468740d0
            l(4) = 0.252041949021d0
            l(5) = 4.03854405009d-2
            p(1) = 0.935022745881d0
            p(2) = 0.812913653173d0
            r(1) = 0.486466535887d0
            s(1) = 0.843636521069d0
         case (27)
            !
            ! REF: V. I. Lebedev, Sibirsk. Mat. Zh. 18 (1977) 132.
            !
            ENSURE(n1==5 AND n2==1 AND n3==2,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = -1.31376912733d-3
            a2   = -2.52272870489d-3
            a3   = 4.18685388170d-3
            b(1) = 5.31516797782d-3
            b(2) = 4.25613135143d-3
            b(3) = 4.11248239441d-3
            b(4) = 3.59558489976d-3
            b(5) = 4.04714237709d-3
            c(1) = 4.22958270065d-3
            d(1) = 4.07146759383d-3
            d(2) = 4.08091422578d-3
            l(1) = 0.703937339159d0
            l(2) = 0.662033866370d0
            l(3) = 0.464744872642d0
            l(4) = 0.327742065497d0
            l(5) = 0.101252624857d0
            p(1) = 0.850650808352d0
            r(1) = 0.819343388819d0
            r(2) = 0.939227929750d0
            s(1) = 0.524493924092d0
            s(2) = 0.323348454269d0
         case (29)
            !
            ! REF: V. I. Lebedev, Sibirsk. Mat. Zh. 18 (1977) 132.
            !
            ENSURE(n1==6 AND n2==2 AND n3==2,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 8.54591172878d-4
            a2   = ZERO
            a3   = 3.59911928502d-3
            b(1) = 3.65004580768d-3
            b(2) = 3.60482260142d-3
            b(3) = 3.57672966173d-3
            b(4) = 3.44978842429d-3
            b(5) = 3.10895312238d-3
            b(6) = 2.35210141366d-3
            c(1) = 3.60082093222d-3
            c(2) = 2.98234496317d-3
            d(1) = 3.57154055427d-3
            d(2) = 3.39231220501d-3
            l(1) = 0.701176641609d0
            l(2) = 0.656632941022d0
            l(3) = 0.472905413258d0
            l(4) = 0.351564034558d0
            l(5) = 0.221964523631d0
            l(6) = 0.0961830852303d0
            p(1) = 0.820326419828d0
            p(2) = 0.964408914879d0
            r(1) = 0.251003475177d0
            r(2) = 0.902442529533d0
            s(1) = 0.800072749407d0
            s(2) = 0.412772408317d0
         case (35)
            !
            ! REF: O. Treutler and R. Ahlrichs, J. Chem. Phys. 102 (1995) 346.
            !
            ENSURE(n1==7 AND n2==2 AND n3==4,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.52659765761428065d-3
            a2   = 0.25482199909403521d-2
            a3   = 0.25123173709441058d-2
            b(1) = 0.25304038224001323d-2
            b(2) = 0.25132671684706878d-2
            b(3) = 0.25017251210647733d-2
            b(4) = 0.24453733047996786d-2
            b(5) = 0.23026944325620758d-2
            b(6) = 0.20142782609526094d-2
            b(7) = 0.14624950815475142d-2
            c(1) = 0.19109513147305082d-2
            c(2) = 0.24174423575419847d-2
            d(1) = 0.22366077071364263d-2
            d(2) = 0.24169300107381179d-2
            d(3) = 0.25122368647336706d-2
            d(4) = 0.24966440519292456d-2
            l(1) = 0.69093463105113458d0
            l(2) = 0.64566647095194987d0
            l(3) = 0.49143426555639500d0
            l(4) = 0.39272598223217649d0
            l(5) = 0.28612891787658218d0
            l(6) = 0.17748365242374568d0
            l(7) = 0.07568095866244468d0
            p(1) = 0.97764280892098723d0
            p(2) = 0.88181328936054412d0
            r(1) = 0.09921769971362576d0
            r(2) = 0.20548237125466495d0
            r(3) = 0.10680182513533723d0
            r(4) = 0.31042840327515130d0
            s(1) = 0.33443631695748371d0
            s(2) = 0.45023303874296735d0
            s(3) = 0.59051570309804130d0
            s(4) = 0.55501523681448068d0
         case (41)
            !
            ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 45 (1992) 587.
            !
            ENSURE(n1==9 AND n2==3 AND n3==6,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1   = 0.3095121295d-3
            a2   = ZERO
            a3   = 0.1852379698d-2
            b(1) = 0.9764331164d-3
            b(2) = 0.1384737234d-2
            b(3) = 0.1617210647d-2
            b(4) = 0.1749564657d-2
            b(5) = 0.1818471778d-2
            b(6) = 0.1846715956d-2
            b(7) = 0.1852028828d-2
            b(8) = 0.1858812585d-2
            b(9) = 0.1871790639d-2
            c(1) = 0.1300321685d-2
            c(2) = 0.1705153996d-2
            c(3) = 0.1857161196d-2
            d(1) = 0.1555213603d-2
            d(2) = 0.1802239128d-2
            d(3) = 0.1849830560d-2
            d(4) = 0.1713904507d-2
            d(5) = 0.1802658934d-2
            d(6) = 0.1842866472d-2
            l(1) = 0.6095034115d-1
            l(2) = 0.1459036449d0
            l(3) = 0.2384736701d0
            l(4) = 0.3317920736d0
            l(5) = 0.4215761784d0
            l(6) = 0.5044419707d0
            l(7) = 0.6372546939d0
            l(8) = 0.6807744066d0
            l(9) = 0.7040954938d0
            p(1) = 0.9850133350d0
            p(2) = 0.9180452877d0
            p(3) = 0.7911019296d0
            s(1) = 0.8213021581d-1
            s(2) = 0.8999205842d-1
            s(3) = 0.1816640840d0
            s(4) = 0.1720795225d0
            s(5) = 0.2634716655d0
            s(6) = 0.3518280927d0
            r(1) = 0.2778673190d0
            r(2) = 0.5033564271d0
            r(3) = 0.5984126497d0
            r(4) = 0.3791035407d0
            r(5) = 0.4742392842d0
            r(6) = 0.5610263808d0
         case (47)
            !
            ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 45 (1992) 587.
            !
            ENSURE(n1==10 AND n2==3 AND n3==9,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1    = 0.2192942090d-3
            a2    = 0.1436433617d-2
            a3    = 0.1421940344d-2
            b(1)  = 0.6798123510d-3
            b(2)  = 0.9913184235d-3
            b(3)  = 0.1180207833d-2
            b(4)  = 0.1296599602d-2
            b(5)  = 0.1365871427d-2
            b(6)  = 0.1402988604d-2
            b(7)  = 0.1418645563d-2
            b(8)  = 0.1421376741d-2
            b(9)  = 0.1423996475d-2
            b(10) = 0.1431554042d-2
            c(1)  = 0.9254401499d-3
            c(2)  = 0.1250239995d-2
            c(3)  = 0.1394365843d-2
            d(1)  = 0.1127089094d-2
            d(2)  = 0.1345753761d-2
            d(3)  = 0.1424957283d-2
            d(4)  = 0.1261523341d-2
            d(5)  = 0.1392547106d-2
            d(6)  = 0.1418761677d-2
            d(7)  = 0.1338366684d-2
            d(8)  = 0.1393700862d-2
            d(9)  = 0.1415914757d-2
            l(1)  = 0.5087204410d-1
            l(2)  = 0.1228198790d0
            l(3)  = 0.2026890814d0
            l(4)  = 0.2847745156d0
            l(5)  = 0.3656719078d0
            l(6)  = 0.4428264886d0
            l(7)  = 0.5140619627d0
            l(8)  = 0.6306401219d0
            l(9)  = 0.6716883332d0
            l(10) = 0.6979792685d0
            p(1)  = 0.9894775374d0
            p(2)  = 0.9407768787d0
            p(3)  = 0.8457493051d0
            s(1)  = 0.6944024393d-1
            s(2)  = 0.2269004109d0
            s(3)  = 0.8025574608d-1
            s(4)  = 0.1467999527d0
            s(5)  = 0.1571507769d0
            s(6)  = 0.2365702993d0
            s(7)  = 0.7714815844d-1
            s(8)  = 0.3062936666d0
            s(9)  = 0.3822477379d0
            r(1)  = 0.2355187894d0
            r(2)  = 0.4102182474d0
            r(3)  = 0.6214302417d0
            r(4)  = 0.3245284345d0
            r(5)  = 0.5224482189d0
            r(6)  = 0.6017546634d0
            r(7)  = 0.4346575516d0
            r(8)  = 0.4908826589d0
            r(9)  = 0.5648768149d0
         case (53)
            !
            ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 45 (1992) 587.
            !
            ENSURE(n1==12 AND n2==4 AND n3==12,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            a1    = 0.1438294190d-3
            a2    = 0.0d0
            a3    = 0.1125772288d-2
            b(1)  = 0.4948029342d-3
            b(2)  = 0.7357990108d-3
            b(3)  = 0.8889132771d-3
            b(4)  = 0.9888347838d-3
            b(5)  = 0.1053299681d-2
            b(6)  = 0.1092778807d-2
            b(7)  = 0.1114389394d-2
            b(8)  = 0.1123724788d-2
            b(9)  = 0.1125239325d-2
            b(10) = 0.1126153271d-2
            b(11) = 0.1130286931d-2
            b(12) = 0.1134986534d-2
            c(1)  = 0.6823367927d-3
            c(2)  = 0.9454158160d-3
            c(3)  = 0.1074429975d-2
            c(4)  = 0.1129300086d-2
            d(1)  = 0.8436884500d-3
            d(2)  = 0.1075255720d-2
            d(3)  = 0.1108577236d-2
            d(4)  = 0.9566475323d-3
            d(5)  = 0.1080663250d-2
            d(6)  = 0.1126797131d-2
            d(7)  = 0.1022568715d-2
            d(8)  = 0.1108960267d-2
            d(9)  = 0.1122790653d-2
            d(10) = 0.1032401847d-2
            d(11) = 0.1107249382d-2
            d(12) = 0.1121780048d-2
            l(1)  = 0.4292963545d-1
            l(2)  = 0.1051426854d0
            l(3)  = 0.1750024867d0
            l(4)  = 0.2477653379d0
            l(5)  = 0.3206567123d0
            l(6)  = 0.3916520749d0
            l(7)  = 0.4590825874d0
            l(8)  = 0.5214563888d0
            l(9)  = 0.6253170244d0
            l(10) = 0.6637926744d0
            l(11) = 0.6910410398d0
            l(12) = 0.7052907007d0
            p(1)  = 0.9923235654d0
            p(2)  = 0.9557815124d0
            p(3)  = 0.8827859807d0
            p(4)  = 0.7737784472d0
            s(1)  = 0.5974048614d-1
            s(2)  = 0.1375760408d0
            s(3)  = 0.3391016526d0
            s(4)  = 0.1271675191d0
            s(5)  = 0.2693120740d0
            s(6)  = 0.1419786452d0
            s(7)  = 0.6709284600d-1
            s(8)  = 0.7057738183d-1
            s(9)  = 0.2783888477d0
            s(10) = 0.1979578938d0
            s(11) = 0.2087307061d0
            s(12) = 0.4055122137d0
            r(1)  = 0.2029128752d0
            r(2)  = 0.4602621942d0
            r(3)  = 0.5030673999d0
            r(4)  = 0.2817606422d0
            r(5)  = 0.4331561291d0
            r(6)  = 0.6256167328d0
            r(7)  = 0.3798395291d0
            r(8)  = 0.5517505421d0
            r(9)  = 0.6029619156d0
            r(10) = 0.3589606329d0
            r(11) = 0.5348666438d0
            r(12) = 0.5674997546d0
         case (59)
            !
            ! REF: V. I. Lebedev, Russian Acad. Sci. Dokl. Math. 50 (1995) 283.
            !
            ENSURE(n1==13 AND n2==4 AND n3==16,"DFTGRID:make_lebedev_grid ... Mismatch of n1, n2, n3.")
            !
            ! **** NOTE THIS TABLE IS NOT YET COMPLETE! ****
            !
            a1    = 0.110518923327d-3
            a2    = 0.920523273809d-3
            a3    = 0.913315978645d-3
            b(1)  = 0.369042189802d-3
            b(2)  = 0.560399092868d-3
            b(3)  = 0.686529762928d-3
            b(4)  = 0.772033855115d-3
            b(5)  = 0.830154595889d-3
            b(6)  = 0.868669255018d-3
            b(7)  = 0.892707628585d-3
            b(8)  = 0.906082023857d-3
            b(9)  = 0.911977725494d-3
            b(10) = 0.912872013860d-3
            b(11) = 0.913071493569d-3
            b(12) = 0.915287378455d-3
            b(13) = 0.918743627432d-3
            c(1)  = 0.517697731297d-3
            c(2)  = 0.733114368210d-3
            c(3)  = 0.846323283638d-3
            c(4)  = 0.903112269425d-3
            d(1)  = 0.648577845316d-3
            d(2)  = 0.743503091098d-3
            d(3)  = 0.799852789184d-3
            d(4)  = 0.810173149747d-3
            d(5)  = 0.848338957459d-3
            d(6)  = 0.855629925731d-3
            d(7)  = 0.880320867974d-3
            d(8)  = 0.881104818243d-3
            d(9)  = 0.885028234127d-3
            d(10) = 0.902134229904d-3
            d(11) = 0.901009167711d-3
            d(12) = 0.902269293843d-3
            d(13) = 0.915801617469d-3
            d(14) = 0.913157800319d-3
            d(15) = 0.910781357948d-3
            d(16) = 0.910576025897d-3
            l(1)  = 0.371263644966d-1
            l(2)  = 0.914006041226d-1
            l(3)  = 0.153107785247d0
            l(4)  = 0.218092889166d0
            l(5)  = 0.283987453220d0
            l(6)  = 0.349117600096d0
            l(7)  = 0.412143146144d0
            l(8)  = 0.471899362715d0
            l(9)  = 0.527314545284d0
            l(10) = 0.620947533244d0
            l(11) = 0.656972271186d0
            l(12) = 0.684178830907d0
            l(13) = 0.701260433012d0
            p(1)  = 0.107238221548d0
            p(2)  = 0.258206895950d0
            p(3)  = 0.417275295531d0
            p(4)  = 0.570036691179d0
            s(1)  = 0.982798601826d0
            s(2)  = 0.962424923033d0
            s(3)  = 0.940200799413d0
            s(4)  = 0.932082204014d0
            s(5)  = 0.904367419939d0
            s(6)  = 0.891240756007d0
            s(7)  = 0.867643562846d0
            s(8)  = 0.858197998604d0
            s(9)  = 0.839675362405d0
            s(10) = 0.816528856402d0
            s(11) = 0.801546937078d0
            s(12) = 0.777356306907d0
            s(13) = 0.766162121390d0
            s(14) = 0.755358414353d0
            s(15) = 0.734430575756d0
            s(16) = 0.704383718402d0
            r(1)  = 0.177177402262d0
            r(2)  = 0.247571646343d0
            r(3)  = 0.335461628907d0
            r(4)  = 0.317361524661d0
            r(5)  = 0.409026842709d0
            r(6)  = 0.385429115067d0
            r(7)  = 0.493222118485d0
            r(8)  = 0.478532067592d0
            r(9)  = 0.450742259316d0
            r(10) = 0.563212302076d0
            r(11) = 0.543430356969d0
            r(12) = 0.512351848642d0
            r(13) = 0.639427963475d0
            r(14) = 0.626980550902d0
            r(15) = 0.603116169310d0
            r(16) = 0.569370249847d0
         case default
            DIE("DFTGRID:make_lebedev_grid ... No grid with order "// trim(to_str_(self%spherical_grid_order)))
      end select
      n = 0 ! should be the total number of spherical points.  Use this for cross-checking.
      if (m1 /= 0) then                                    ! Points for a1, number = 6
        ! point(:) = (/ ONE, ZERO, ZERO /)
        ! .pm_perm_point(point, set, n)
        call pm_perm_point_11_(self,ONE, pt(n+1:n+6,:))
        wt(n+1:n+6) = a1
        n = n + 6
      end if
      if (m2 /= 0) then                                    ! Points for a2, number = 12
        ! point(:) = (/ sr2, sr2, ZERO /)
        ! .pm_perm_point(point, set, n)
        sr2 = ONE/sqrt(TWO)
        call pm_perm_point_12_(self,sr2, pt(n+1:n+12,:))
        wt(n+1:n+12) = a2
        n = n + 12
      end if
      if (m3 /= 0) then                                    ! Points for a3, number = 8
        ! point(:) = (/ sr3, sr3, sr3 /)
        ! .pm_perm_point(point, set, n)
        sr3 = ONE/sqrt(THREE)
        call pm_perm_point_13_(self,sr3, pt(n+1:n+8,:))
        wt(n+1:n+8) = a3
        n = n + 8
      end if
      do k = 1, n1                                         ! Points for b(k), number = 24
        m = sqrt(ONE - TWO*l(k)*l(k))
        ! point(:) = (/ l(k), l(k), m /)
        ! .pm_perm_point(point, set, n)
        call pm_perm_point_22_(self,m, l(k), pt(n+1:n+24,:))
        wt(n+1:n+24) = b(k)
        n = n + 24
      end do
      do k = 1, n2                                         ! Points for c(k), number = 24
        q = sqrt(ONE - p(k)*p(k))
        ! point(:) = (/ p(k), q, ZERO /)
        ! .pm_perm_point(point, set, n)
        call pm_perm_point_21_(self,p(k), q, pt(n+1:n+24,:))
        wt(n+1:n+24) = c(k)
        n = n + 24
      end do
      do k = 1, n3                                         ! Points for d, number = 48
        w = sqrt(ONE - r(k)*r(k) - s(k)*s(k))
        ! point(:) = (/ r(k), s(k), w /)
        ! .pm_perm_point(point, set, n)
        call pm_perm_point_31_(self,r(k), s(k), w, pt(n+1:n+48,:))
        wt(n+1:n+48) = d(k)
        n = n + 48
      end do
      call destroy_(b); call destroy_(c); call destroy_(d)
      call destroy_(l)
      call destroy_(p)
      call destroy_(r); call destroy_(s)
      ENSURE(n==self%n_spherical_pts,"DFTGRID:make_lebedev_grid ... Incorrect number of Lebedev grid points")
     STOP_TIMER("DFTGRID:make_lebedev_grid")
      CHECK
   end subroutine

   PURE subroutine pm_perm_point_11(self,a, set)
    DFTGRID :: self
   ! Permutates (a, 0, 0) according to the octahedral group
   ! "set" must be at least: dimension (1:3, 1:6)
      IN :: self
      REAL, IN :: a
      REALMAT(:,:), OUT :: set
      set(1,:) = (/    a, ZERO, ZERO /)
      set(2,:) = (/   -a, ZERO, ZERO /)
      set(3,:) = (/ ZERO,    a, ZERO /)
      set(4,:) = (/ ZERO,   -a, ZERO /)
      set(5,:) = (/ ZERO, ZERO,    a /)
      set(6,:) = (/ ZERO, ZERO,   -a /)
     STOP_TIMER("DFTGRID:pm_perm_point_11")
   end subroutine

   PURE subroutine pm_perm_point_12(self,a, set)
    DFTGRID :: self
   ! Permutates (a, a, 0) according to the octahedral group
   ! "set" must be at least: dimension (1:3, 1:12)
      IN :: self
      REAL, IN :: a
      REALMAT(:,:), OUT :: set
      set(1,:)  =  (/    a,    a, ZERO /)
      set(2,:)  =  (/   -a,    a, ZERO /)
      set(3,:)  =  (/    a,   -a, ZERO /)
      set(4,:)  =  (/   -a,   -a, ZERO /)
      set(5,:)  =  (/    a, ZERO,    a /)
      set(6,:)  =  (/   -a, ZERO,    a /)
      set(7,:)  =  (/    a, ZERO,   -a /)
      set(8,:)  =  (/   -a, ZERO,   -a /)
      set(9,:)  =  (/ ZERO,    a,    a /)
      set(10,:) =  (/ ZERO,   -a,    a /)
      set(11,:) =  (/ ZERO,    a,   -a /)
      set(12,:) =  (/ ZERO,   -a,   -a /)
     STOP_TIMER("DFTGRID:pm_perm_point_12")
   end subroutine

   PURE subroutine pm_perm_point_13(self,a, set)
    DFTGRID :: self
   ! Permutates (a, a, a) according to the octahedral group
   ! "set" must be at least: dimension (1:3, 1:8)
      IN :: self
      REAL, IN :: a
      REALMAT(:,:), OUT :: set
      set(1,:)  =  (/  a,  a,  a /)
      set(2,:)  =  (/  a, -a,  a /)
      set(3,:)  =  (/  a,  a, -a /)
      set(4,:)  =  (/  a, -a, -a /)
      set(5,:)  =  (/ -a,  a,  a /)
      set(6,:)  =  (/ -a, -a,  a /)
      set(7,:)  =  (/ -a,  a, -a /)
      set(8,:)  =  (/ -a, -a, -a /)
     STOP_TIMER("DFTGRID:pm_perm_point_13")
   end subroutine

   PURE subroutine pm_perm_point_21(self,a, b, set)
    DFTGRID :: self
   ! Permutates (a, b, 0), a.ne.b, according to the octahedral group
   ! "set" must be at least: dimension (1:3, 1:24)
      IN :: self
      REAL, IN :: a, b
      REALMAT(:,:), OUT :: set
      set(1,:)  =  (/    a,    b, ZERO /)
      set(2,:)  =  (/   -a,    b, ZERO /)
      set(3,:)  =  (/    a,   -b, ZERO /)
      set(4,:)  =  (/   -a,   -b, ZERO /)
      set(5,:)  =  (/    b,    a, ZERO /)
      set(6,:)  =  (/   -b,    a, ZERO /)
      set(7,:)  =  (/    b,   -a, ZERO /)
      set(8,:)  =  (/   -b,   -a, ZERO /)
      set(9,:)  =  (/    a, ZERO,    b /)
      set(10,:) =  (/   -a, ZERO,    b /)
      set(11,:) =  (/    a, ZERO,   -b /)
      set(12,:) =  (/   -a, ZERO,   -b /)
      set(13,:) =  (/    b, ZERO,    a /)
      set(14,:) =  (/   -b, ZERO,    a /)
      set(15,:) =  (/    b, ZERO,   -a /)
      set(16,:) =  (/   -b, ZERO,   -a /)
      set(17,:) =  (/ ZERO,    a,    b /)
      set(18,:) =  (/ ZERO,   -a,    b /)
      set(19,:) =  (/ ZERO,    a,   -b /)
      set(20,:) =  (/ ZERO,   -a,   -b /)
      set(21,:) =  (/ ZERO,    b,    a /)
      set(22,:) =  (/ ZERO,   -b,    a /)
      set(23,:) =  (/ ZERO,    b,   -a /)
      set(24,:) =  (/ ZERO,   -b,   -a /)
     STOP_TIMER("DFTGRID:pm_perm_point_21")
   end subroutine

   PURE subroutine pm_perm_point_22(self,a, b, set)
    DFTGRID :: self
   ! Permutates (a, b, b),  a.ne.b, according to the octahedral group
   ! "set" must be at least: dimension (1:3, 1:24)
      IN :: self
      REAL, IN :: a, b
      REALMAT(:,:), OUT :: set
      set(1,:)  =  (/  a, b, b /)
      set(2,:)  =  (/  a,-b, b /)
      set(3,:)  =  (/  a, b,-b /)
      set(4,:)  =  (/  a,-b,-b /)
      set(5,:)  =  (/ -a, b, b /)
      set(6,:)  =  (/ -a,-b, b /)
      set(7,:)  =  (/ -a, b,-b /)
      set(8,:)  =  (/ -a,-b,-b /)
      set(9,:)  =  (/  b, a, b /)
      set(10,:) =  (/ -b, a, b /)
      set(11,:) =  (/  b, a,-b /)
      set(12,:) =  (/ -b, a,-b /)
      set(13,:) =  (/  b,-a, b /)
      set(14,:) =  (/ -b,-a, b /)
      set(15,:) =  (/  b,-a,-b /)
      set(16,:) =  (/ -b,-a,-b /)
      set(17,:) =  (/  b, b, a /)
      set(18,:) =  (/ -b, b, a /)
      set(19,:) =  (/  b,-b, a /)
      set(20,:) =  (/ -b,-b, a /)
      set(21,:) =  (/  b, b,-a /)
      set(22,:) =  (/ -b, b,-a /)
      set(23,:) =  (/  b,-b,-a /)
      set(24,:) =  (/ -b,-b,-a /)
     STOP_TIMER("DFTGRID:pm_perm_point_22")
   end subroutine

   PURE subroutine pm_perm_point_31(self,a, b, c, set)
    DFTGRID :: self
   ! Permutates (a, b, c), a.ne.b & a.ne.c & b.ne.c, according to the octahedral
   ! group "set" must be at least: dimension (1:3, 1:48)
      IN :: self
      REAL, IN :: a, b, c
      REALMAT(:,:), OUT :: set
      set(1,:)  =  (/  a, b, c /)
      set(2,:)  =  (/ -a, b, c /)
      set(3,:)  =  (/  a,-b, c /)
      set(4,:)  =  (/ -a,-b, c /)
      set(5,:)  =  (/  b, a, c /)
      set(6,:)  =  (/ -b, a, c /)
      set(7,:)  =  (/  b,-a, c /)
      set(8,:)  =  (/ -b,-a, c /)
      set(9,:)  =  (/  a, c, b /)
      set(10,:) =  (/ -a, c, b /)
      set(11,:) =  (/  a, c,-b /)
      set(12,:) =  (/ -a, c,-b /)
      set(13,:) =  (/  b, c, a /)
      set(14,:) =  (/ -b, c, a /)
      set(15,:) =  (/  b, c,-a /)
      set(16,:) =  (/ -b, c,-a /)
      set(17,:) =  (/  c, a, b /)
      set(18,:) =  (/  c,-a, b /)
      set(19,:) =  (/  c, a,-b /)
      set(20,:) =  (/  c,-a,-b /)
      set(21,:) =  (/  c, b, a /)
      set(22,:) =  (/  c,-b, a /)
      set(23,:) =  (/  c, b,-a /)
      set(24,:) =  (/  c,-b,-a /)
      set(25,:) =  (/  a, b,-c /)
      set(26,:) =  (/ -a, b,-c /)
      set(27,:) =  (/  a,-b,-c /)
      set(28,:) =  (/ -a,-b,-c /)
      set(29,:) =  (/  b, a,-c /)
      set(30,:) =  (/ -b, a,-c /)
      set(31,:) =  (/  b,-a,-c /)
      set(32,:) =  (/ -b,-a,-c /)
      set(33,:) =  (/  a,-c, b /)
      set(34,:) =  (/ -a,-c, b /)
      set(35,:) =  (/  a,-c,-b /)
      set(36,:) =  (/ -a,-c,-b /)
      set(37,:) =  (/  b,-c, a /)
      set(38,:) =  (/ -b,-c, a /)
      set(39,:) =  (/  b,-c,-a /)
      set(40,:) =  (/ -b,-c,-a /)
      set(41,:) =  (/ -c, a, b /)
      set(42,:) =  (/ -c,-a, b /)
      set(43,:) =  (/ -c, a,-b /)
      set(44,:) =  (/ -c,-a,-b /)
      set(45,:) =  (/ -c, b, a /)
      set(46,:) =  (/ -c,-b, a /)
      set(47,:) =  (/ -c, b,-a /)
      set(48,:) =  (/ -c,-b,-a /)
     STOP_TIMER("DFTGRID:pm_perm_point_31")
   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self,output)
    DFTGRID :: self
   ! Put out the DFTGRID to file "output"
      TEXTFILE, target, optional :: output
      TEXTFILE, PTR :: out
      STACK("DFTGRID:put")
      START_TIMER("DFTGRID:put")
      if (present(output)) then
        out => output
      else
        out => stdout
      end if
      call flush_(out)
      call text_(out,"DFTGRID output:")
      call text_(out," ")
      call show_(out,"Spherical grid kind       =", self%spherical_grid_kind)
      call show_(out,"Spherical grid order      =", self%spherical_grid_order)
      call show_(out,"No. of spherical points   =", self%n_spherical_pts)
      call text_(out," ")
      call show_(out,"Radial grid kind          =", self%radial_grid_kind)
      call show_(out,"Radial grid order         =", self%radial_grid_order)
      call show_(out,"No. of radial points      =", self%n_radial_pts)
      call text_(out," ")
      call show_(out,"No. of grid points        =", self%n_pts)
      call text_(out," ")
      call show_(out,"Becke m partition power     =", self%becke_m_partition_power)
      call show_(out,"Gauss-Chebyshev alpha value =", self%gauss_chebyshev_alpha)
      call show_(out,"Gauss-Chebyshev m     value =", self%gauss_chebyshev_m)
      call show_(out,"Euler-Maclaurin alpha value =", self%euler_maclaurin_alpha)
      call show_(out,"Euler-Maclaurin m     value =", self%euler_maclaurin_m)
     STOP_TIMER("DFTGRID:put")
      CHECK
   end subroutine

!*******************************************************************************
!                  DFT functionals and derivatives
!*******************************************************************************

   subroutine d_u_lda_exchange_functional(self,p_a,p_b,dfdp_a,dfdp_b,alpha)
    DFTGRID :: self
   ! Return the derivatives of the local density exchange functional.
     REALVEC(:), IN :: p_a,p_b
     REALVEC(:) :: dfdp_a,dfdp_b
     REAL, IN :: alpha
     REAL :: const
     STACK("DFTGRID:d_u_lda_exchange_functional")
     START_TIMER("DFTGRID:d_u_lda_exchange_functional")
     const = - THREE * alpha * (THREE/(FOUR*PI))**THIRD
     dfdp_a = const* p_a**THIRD
     dfdp_b = const* p_b**THIRD
     STOP_TIMER("DFTGRID:d_u_lda_exchange_functional")
      CHECK
   end subroutine

   subroutine u_lda_exchange_functional(self,p_a,p_b,f,alpha)
    DFTGRID :: self
   ! Return the values of the local density exchange functional.
     REALVEC(:), IN :: p_a,p_b
     REALVEC(:) :: f
     REAL, IN :: alpha
     REAL :: const
     STACK("DFTGRID:u_lda_exchange_functional")
     START_TIMER("DFTGRID:u_lda_exchange_functional")
     const = - NINE/FOUR * alpha * (THREE/(FOUR*PI))**THIRD
     f = const * (p_a**(FOUR/THREE) + p_b**(FOUR/THREE))
     STOP_TIMER("DFTGRID:u_lda_exchange_functional")
      CHECK
   end subroutine

   subroutine u_becke88_exchange_functional(self,p_a,p_b,np_a,np_b,f)
    DFTGRID :: self
   ! Return the values of the Becke 88 exchange functional.
     REALVEC(:), IN :: p_a,p_b
     REALVEC(:) :: f
     REALMAT(:,:), IN :: np_a,np_b
     REAL :: beta,fac,xa,xb,paft,pbft
     INT :: i
     STACK("DFTGRID:u_becke88_exchange_functional")
     START_TIMER("DFTGRID:u_becke88_exchange_functional")
     beta = 0.0042  ! beta parameter
     fac = -THREE/TWO * (THREE/(FOUR*PI))**THIRD
     do i=1,size(p_a)
       paft = (p_a(i)) ** (FOUR/THREE)
       pbft = (p_b(i)) ** (FOUR/THREE)
       xa = max(sqrt(dot_product(np_a(i,:),np_a(i,:))) / paft,TOL(20))
       xb = max(sqrt(dot_product(np_b(i,:),np_b(i,:))) / pbft,TOL(20))
       f(i) = paft * (fac - beta*xa*xa/(ONE+SIX*beta*xa*arcsinh_(xa))) &
            + pbft * (fac - beta*xb*xb/(ONE+SIX*beta*xb*arcsinh_(xb)))
     end do
     STOP_TIMER("DFTGRID:u_becke88_exchange_functional")
      CHECK
   end subroutine

  subroutine d_u_b88_exchange_functional(self,p_a,p_b,np_a,np_b,local_a,local_b,non_local_a,non_local_b)
    DFTGRID :: self
   ! Return the derivatives of the Becke 88 exchange functional.
   ! These equations are essentially the same as in the appendix of JCP 98(7)
   ! 5612-5626.
     REALVEC(:), IN :: p_a,p_b
     REALMAT(:,:), IN :: np_a,np_b
     REALVEC(:) :: local_a,local_b
     REALMAT(:,:) :: non_local_a,non_local_b
     REALVEC(3) :: npa,npb
     REAL :: c,d,e,pa,pb,pa_third,pb_third,xa,xb,ka,kb,xa2,xb2
     REAL :: ka2,kb2,za,zb,beta,nla,nlb,paft,pbft
     INT :: i,n_pt
     STACK("DFTGRID:d_u_b88_exchange_functional")
     START_TIMER("DFTGRID:d_u_b88_exchange_functional")
     beta = 0.0042  ! beta parameter
     n_pt = size(p_a)
     c = beta*HALF
     d = - (SIX/PI)**THIRD
     e = FOUR*THIRD*beta
     do i=1,n_pt
       pa = p_a(i)
       pb = p_b(i)
       npa = np_a(i,:)
       npb = np_b(i,:)
       pa_third = pa**(THIRD)
       pb_third = pb**(THIRD)
       paft = pa*pa_third
       pbft = pb*pb_third
       xa = max(sqrt(dot_product(npa,npa)) / paft,TOL(20))
       xb = max(sqrt(dot_product(npb,npb)) / pbft,TOL(20))
       xa2 = xa*xa
       xb2 = xb*xb
       ka = ONE + SIX * beta * xa * arcsinh_(xa)
       kb = ONE + SIX * beta * xb * arcsinh_(xb)
       ka2 = ka*ka
       kb2 = kb*kb
       za = SIX*beta*xa2/sqrt(ONE+xa2)
       zb = SIX*beta*xb2/sqrt(ONE+xb2)
       local_a(i) = d * pa_third + e * pa_third * xa2 * (ONE-za) / ka2
       local_b(i) = d * pb_third + e * pb_third * xb2 * (ONE-zb) / kb2
       if (xa < TOL(19)) then
         non_local_a(i,:) = ZERO
       else
         nla = beta * (za-ONE-ka)/(ka2*paft)
         non_local_a(i,:) = nla*npa(:)
       end if
       if (xb < TOL(19)) then
         non_local_b(i,:) = ZERO
       else
         nlb = beta * (zb-ONE-kb)/(kb2*pbft)
         non_local_b(i,:) = nlb*npb(:)
       end if
     end do
     STOP_TIMER("DFTGRID:d_u_b88_exchange_functional")
      CHECK
   end subroutine

   subroutine u_gill96_exchange_functional(self,p_a,p_b,np_a,np_b,f)
    DFTGRID :: self
   ! Return the values of the Gill 96 exchange functional.
     REALVEC(:), IN :: p_a,p_b
     REALVEC(:) :: f
     REALMAT(:,:), IN :: np_a,np_b
     REAL :: alpha,fac,xa,xb,paft,pbft
     INT :: i
     STACK("DFTGRID:u_gill96_exchange_functional")
     START_TIMER("DFTGRID:u_gill96_exchange_functional")
     alpha = -THREE/TWO * (THREE/(FOUR*PI))**THIRD
     fac = ONE/137
     do i=1,size(p_a)
       paft = (p_a(i)) ** (FOUR/THREE)
       pbft = (p_b(i)) ** (FOUR/THREE)
       xa = max(sqrt(dot_product(np_a(i,:),np_a(i,:))) / paft,TOL(20))
       xb = max(sqrt(dot_product(np_b(i,:),np_b(i,:))) / pbft,TOL(20))
       f(i) = paft * (alpha - fac*xa*sqrt(xa)) + pbft * (alpha - fac*xb*sqrt(xb))
     end do
     STOP_TIMER("DFTGRID:u_gill96_exchange_functional")
      CHECK
   end subroutine

  subroutine d_u_gill96_exchange_functional(self,p_a,p_b,np_a,np_b,local_a,local_b,non_local_a,non_local_b)
    DFTGRID :: self
  ! Return the derivatives of the Gill 96 exchange functional.
     REALVEC(:), IN :: p_a,p_b
     REALMAT(:,:), IN :: np_a,np_b
     REALVEC(:) :: local_a,local_b
     REALMAT(:,:) :: non_local_a,non_local_b
     REALVEC(3) :: npa,npb
     REAL :: alpha,ft,fac,fac1,pa,pb,pa_third,pb_third,xa,xb,xa2,xb2,nla,nlb
     INT :: i,n_pt
     STACK("DFTGRID:d_u_gill96_exchange_functional")
     START_TIMER("DFTGRID:d_u_gill96_exchange_functional")
     n_pt = size(p_a)
     alpha = -THREE/TWO * (THREE/(FOUR*PI))**THIRD
     ft = FOUR/THREE
     fac = ONE/274
     fac1 = -THREE/548
     do i=1,n_pt
       pa = p_a(i)
       pb = p_b(i)
       npa = np_a(i,:)
       npb = np_b(i,:)
       pa_third = pa**(THIRD)
       pb_third = pb**(THIRD)
       xa = max(sqrt(dot_product(npa,npa))/(pa*pa_third),TOL(20))
       xb = max(sqrt(dot_product(npb,npb))/(pb*pb_third),TOL(20))
       xa2 = sqrt(xa)
       xb2 = sqrt(xb)
       local_a(i) = ft*pa_third*(alpha+fac*xa*xa2)
       local_b(i) = ft*pb_third*(alpha+fac*xb*xb2)
       if (xa < TOL(19)) then
         non_local_a(i,:) = ZERO
       else
         nla = TWO*fac1*xa2/(xa*pa*pa_third)
         non_local_a(i,:) = nla*npa(:)
       end if
       if (xb < TOL(19)) then
         non_local_b(i,:) = ZERO
       else
         nlb = TWO*fac1*xb2/(xb*pb*pb_third)
         non_local_b(i,:) = nlb*npb(:)
       end if
     end do
    STOP_TIMER("DFTGRID:d_u_gill96_exchange_functional")
     CHECK
  end subroutine

   subroutine u_lyp_correlation_functional(self,p_a,p_b,np_a,np_b,f)
    DFTGRID :: self
   ! Return the values of the Lee-Yang-Parr functional.
     REALVEC(:), IN :: p_a,p_b
     REALVEC(:) :: f
     REALMAT(:,:), IN :: np_a,np_b
     REAL :: c_f,a,b,c,d,fac,pa,pb,p,npanpa,npbnpb,npanpb,p_third
     REAL :: gamma,a_b_omega,delta,pa_pb_n,fi
     INT :: i,start,step
     STACK("DFTGRID:u_lyp_correlation_functional")
     START_TIMER("DFTGRID:u_lyp_correlation_functional")
     c_f = (THREE/TEN)*(3*PI*PI)**(THIRD+THIRD)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     fac = TWO**(11*THIRD)*c_f
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       f = ZERO
     end if
     do i=start,size(p_a),step
       pa = p_a(i)
       pb = p_b(i)
       p = pa + pb
       npanpa = dot_product(np_a(i,:),np_a(i,:))
       npbnpb = dot_product(np_b(i,:),np_b(i,:))
       npanpb = dot_product(np_a(i,:),np_b(i,:))
       p_third = p**THIRD
       gamma = ONE + d/p_third
       a_b_omega = a*b*exp(-c/p_third)/(gamma*p_third**11)
       delta = (c+d/gamma)/p_third
       pa_pb_n=pa*pb/NINE
       fi = -a * FOUR*pa*pb/(p*gamma)
       fi = fi - a_b_omega*fac*pa*pb*(pa**(EIGHT*THIRD)+pb**(EIGHT*THIRD))
       fi = fi + a_b_omega*(pa_pb_n*(FOUR*delta-ONE-(11*pa+pb*delta)/p) + pb*pb)*npanpa
       fi = fi + a_b_omega*(pa_pb_n*(FOUR*delta-ONE-(11*pb+pa*delta)/p) + pa*pa)*npbnpb
       fi = fi + a_b_omega*(12/NINE*p*p - pa_pb_n*(47-SEVEN*delta))*npanpb
       f(i) = fi
     end do
     call sum_vectors_(tonto_parallel,f)
     STOP_TIMER("DFTGRID:u_lyp_correlation_functional")
      CHECK
   end subroutine

   subroutine d_u_lyp_correlation_functional(self,p_a,p_b,np_a,np_b,local_a,local_b,non_local_a,non_local_b)
    DFTGRID :: self
   ! Return the derivatives of the LYP correlation functional.
   ! These equations are essentially the same as in the appendix of JCP 98(7)
   ! 5612-5626.
     REALVEC(:), IN :: p_a,p_b
     REALMAT(:,:), IN :: np_a,np_b
     REALVEC(:) :: local_a,local_b
     REALMAT(:,:) :: non_local_a,non_local_b
     REALVEC(3) :: npa,npb
     REAL :: a,b,c,d,e,c_f,ab9
     REAL :: pa,pb,p,p2,pa2,pb2,papb,p_third,npanpa,npbnpb,npanpb
     REAL :: gamma_inv,mu,abw9,abw27,delta,pa83,pb83,tmp1,tmp2
     REAL :: dfdnpanpb,dfdnpanpa,dfdnpbnpb
     INT :: i

     STACK("DFTGRID:d_u_lyp_correlation_functional")
     START_TIMER("DFTGRID:d_u_lyp_correlation_functional")
     c_f = (THREE/TEN)*(3*PI*PI)**(THIRD+THIRD)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     e = TWO**(11*THIRD) * NINE * c_f
     ab9 = a*b/NINE

     do i=1,size(p_a)
       pa = p_a(i)
       pb = p_b(i)
       npa = np_a(i,:)
       npb = np_b(i,:)
       npanpa = dot_product(npa,npa)
       npbnpb = dot_product(npb,npb)
       npanpb = dot_product(npa,npb)
       p = pa + pb
       p_third = p**(THIRD)
       gamma_inv = ONE/(ONE + d / p_third)
       mu=d*gamma_inv/p_third
       p2=p*p
       abw9 = ab9*exp(-c/p_third) * p_third/(p2*p2) * gamma_inv
       abw27 = abw9*THIRD
       delta = c/p_third + mu
       pa2=pa*pa
       pb2=pb*pb
       pa83=pa2*pa/pa**THIRD
       pb83=pb2*pb/pb**THIRD
       tmp1 = pa*pb/p*(7*mu*mu+delta*(7*delta-131)+517)
       tmp2 = (delta-11)/p*(pb83+pa83)
       papb=pa*pb

       local_a(i) = -4*a*pb*gamma_inv/p2*(pb+THIRD*mu*pa) &
         +abw27*npanpb*(pa*(12*delta-60)+pb*(33*delta-201) + tmp1) &
         +abw27*npanpa*pb/p2*(96*pa2-160*papb-102*pb2 &
          +(-48*pa2-4*papb+18*pb2)*delta+pa*(4*p-pb)*(delta*delta+mu*mu)) &
         +abw27*npbnpb/p2*(-45*pa2*pa+17*pa2*pb+180*pa*pb2-36*pb2*pb &
          +(9*pa2*pa-19*pa2*pb-42*pa*pb2+12*pb2*pb)*delta &
          +papb*(4*p-pa)*(delta*delta+mu*mu)) &
         -abw27*pb*e*(pa*tmp2+11*pa83+3*pb83)
       local_b(i) = -4*a*pa*gamma_inv/p2*(pa+THIRD*mu*pb) &
         +abw27*npanpb*(pb*(12*delta-60)+pa*(33*delta-201) + tmp1) &
         +abw27*npbnpb*pa/p2*(96*pb2-160*papb-102*pa2 &
          +(-48*pb2-4*papb+18*pa2)*delta+pb*(4*p-pa)*(delta*delta+mu*mu)) &
         +abw27*npanpa/p2*(-45*pb2*pb+17*pb2*pa+180*pb*pa2-36*pa2*pa &
          +(9*pb2*pb-19*pb2*pa-42*pb*pa2+12*pa2*pa)*delta &
          +papb*(4*p-pb)*(delta*delta+mu*mu)) &
         -abw27*pa*e*(pb*tmp2+11*pb83+3*pa83)

       dfdnpanpb = abw9*(12*p2-papb*(47-7*delta))
       dfdnpanpa = abw9*(NINE*pb2+papb*(FOUR*delta-ONE-(11*pa+pb*delta)/p))
       dfdnpbnpb = abw9*(NINE*pa2+papb*(FOUR*delta-ONE-(11*pb+pa*delta)/p))
       non_local_a(i,:) = TWO*dfdnpanpa*npa+dfdnpanpb*npb
       non_local_b(i,:) = TWO*dfdnpbnpb*npb+dfdnpanpb*npa
     end do
     STOP_TIMER("DFTGRID:d_u_lyp_correlation_functional")
      CHECK
   end subroutine

!*******************************************************************************
!                  DFT functionals and derivatives
!*******************************************************************************

   subroutine d_r_lda_exchange_functional(self,p,local,alpha)
    DFTGRID :: self
   ! Return the derivatives of the local density exchange functional.
     IN :: self
     REALVEC(:), IN :: p
     REALVEC(:), OUT :: local
     REAL, IN :: alpha
     REAL :: const
     STACK("DFTGRID:d_r_lda_exchange_functional")
     START_TIMER("DFTGRID:d_r_lda_exchange_functional")
     const = - SIX * alpha * (THREE/(EIGHT*PI))**THIRD
     local = const * p**THIRD
     STOP_TIMER("DFTGRID:d_r_lda_exchange_functional")
      CHECK
   end subroutine

   subroutine r_lda_exchange_functional(self,p,f,alpha)
    DFTGRID :: self
   ! Return the values of the local density exchange functional.
     IN :: self
     REALVEC(:), IN :: p
     REALVEC(:), OUT :: f
     REAL, IN :: alpha
     REAL :: const
     STACK("DFTGRID:r_lda_exchange_functional")
     START_TIMER("DFTGRID:r_lda_exchange_functional")
     const = - NINE/16 * alpha * (THREE/PI)**THIRD
     f = const * p**(FOUR/THREE)
     STOP_TIMER("DFTGRID:r_lda_exchange_functional")
      CHECK
   end subroutine

   subroutine r_becke88_exchange_functional(self,p,np,f)
    DFTGRID :: self
   ! Return the values of the Becke 88 exchange functional.
     IN :: self
     REALVEC(:), IN :: p
     REALVEC(:), OUT :: f
     REALMAT(:,:), IN :: np
     REAL :: beta,fac,x,pft
     INT :: i
     STACK("DFTGRID:r_becke88_exchange_functional")
     START_TIMER("DFTGRID:r_becke88_exchange_functional")
     beta = 0.0042  ! beta parameter
     fac = -THREE/TWO * (THREE/(FOUR*PI))**THIRD
     do i=1,size(p)
       pft = (HALF*p(i)) ** (FOUR/THREE)
       x = HALF * max(sqrt(dot_product(np(i,:),np(i,:))) / pft,TOL(20))
       f(i) = TWO * pft * (fac - beta*x*x/(ONE+SIX*beta*x*arcsinh_(x)))
     end do
     STOP_TIMER("DFTGRID:r_becke88_exchange_functional")
      CHECK
   end subroutine

  subroutine d_r_b88_exchange_functional(self,p,np,local,non_local)
    DFTGRID :: self
   ! Return the derivatives of the Becke 88 exchange functional.
   ! These equations are essentially the same as in the appendix of JCP 98(7)
   ! 5612-5626.
     REALVEC(:), IN :: p
     REALMAT(:,:), IN :: np
     REALVEC(:) :: local
     REALMAT(:,:) :: non_local
     REALVEC(3) :: np_i
     REAL :: c,d,e,p_i,p_i_third,x,k,x2,k2,z,beta,nl,p_i_ft
     INT :: i
     STACK("DFTGRID:d_r_b88_exchange_functional")
     START_TIMER("DFTGRID:d_r_b88_exchange_functional")
     beta = 0.0042  ! beta parameter
     c = beta*HALF
     d = - (SIX/PI)**THIRD
     e = FOUR*THIRD*beta
     do i=1,size(p)
       p_i = HALF*p(i)
       np_i = HALF*np(i,:)
       p_i_third = p_i**(THIRD)
       p_i_ft = p_i*p_i_third
       x = max(sqrt(dot_product(np_i,np_i)) / p_i_ft,TOL(20))
       x2 = x*x
       k = ONE + SIX * beta * x * arcsinh_(x)
       k2 = k*k
       z = SIX*beta*x2/sqrt(ONE+x2)
       local(i) = d * p_i_third + e * p_i_third * x2 * (ONE-z) / k2
       if (x < TOL(19)) then
         non_local(i,:) = ZERO
       else
         nl = beta * (z-ONE-k)/(k2*p_i_ft)
         non_local(i,:) = nl*np_i(:)
       end if
     end do
     STOP_TIMER("DFTGRID:d_r_b88_exchange_functional")
      CHECK
   end subroutine

  subroutine d_r_gill96_exchange_functional(self,p,np,local,non_local)
    DFTGRID :: self
  ! Return the derivatives of the Gill 96 exchange functional.
     IN :: self
     REALVEC(:), IN :: p
     REALMAT(:,:), IN :: np
     REALVEC(:), OUT :: local
     REALMAT(:,:), OUT :: non_local
     REALVEC(3) :: np_i
     REAL :: alpha,ft,fac,fac1,p_i,p_third,x,x2,nl
     INT :: i
     STACK("DFTGRID:d_r_gill96_exchange_functional")
     START_TIMER("DFTGRID:d_r_gill96_exchange_functional")
     alpha = -THREE/TWO * (THREE/(FOUR*PI))**THIRD
     ft = FOUR/THREE
     fac = ONE/274
     fac1 = -THREE/548
     do i=1,size(p)
       p_i = HALF*p(i)
       np_i = HALF*np(i,:)
       p_third = p_i**(THIRD)
       x = max(sqrt(dot_product(np_i,np_i))/(p_i*p_third),TOL(20))
       x2 = sqrt(x)
       local(i) = ft*p_third*(alpha+fac*x*x2)
       if (x < TOL(19)) then
         non_local(i,:) = ZERO
       else
         nl = TWO*fac1*x2/(x*p_i*p_third)
         non_local(i,:) = nl*np_i(:)
       end if
     end do
    STOP_TIMER("DFTGRID:d_r_gill96_exchange_functional")
     CHECK
  end subroutine

   subroutine r_gill96_exchange_functional(self,p,np,f)
    DFTGRID :: self
   ! Return the values of the Gill 96 exchange functional.
     IN :: self
     REALVEC(:), IN :: p
     REALVEC(:), OUT :: f
     REALMAT(:,:), IN :: np
     REALVEC(3) :: np_i
     REAL :: alpha,fac,x,pft
     INT :: i
     STACK("DFTGRID:r_gill96_exchange_functional")
     START_TIMER("DFTGRID:r_gill96_exchange_functional")
     alpha = -THREE/TWO * (THREE/(FOUR*PI))**THIRD
     fac = ONE/137
     do i=1,size(p)
       pft = (HALF*p(i)) ** (FOUR/THREE)
       np_i = HALF*np(i,:)
       x = max(sqrt(dot_product(np_i,np_i)) / pft,TOL(20))
       f(i) = TWO*pft * (alpha - fac*x*sqrt(x))
     end do
     STOP_TIMER("DFTGRID:r_gill96_exchange_functional")
      CHECK
   end subroutine

   subroutine r_lyp_correlation_functional(self,p,np,f)
    DFTGRID :: self
   ! Return the values of the Lee-Yang-Parr functional.
     IN :: self
     REALVEC(:), IN :: p
     REALVEC(:), OUT :: f
     REALMAT(:,:), IN :: np
     REAL :: c_f,a,b,c,d,fac,p_i,npnp,p_third
     REAL :: gamma,a_b_omega,delta,p2,fi
     INT :: i,start,step
     STACK("DFTGRID:r_lyp_correlation_functional")
     START_TIMER("DFTGRID:r_lyp_correlation_functional")
     c_f = (THREE/TEN)*(3*PI*PI)**(THIRD+THIRD)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     fac = TWO**(11*THIRD)*c_f
     start = 1 + this_proc_(tonto_parallel)
     step = n_proc_(tonto_parallel)
     if (tonto_parallel%do_parallel) then
       f = ZERO
     end if
     do i=start,size(p),step
       p_i = HALF*p(i)
       p2 = p_i*p_i
       npnp = QUARTER*dot_product(np(i,:),np(i,:))
       p_third = (TWO*p_i)**THIRD
       gamma = ONE + d/p_third
       a_b_omega = a*b*exp(-c/p_third)/(gamma*p_third**11)
       delta = (c+d/gamma)/p_third
       fi = -a * TWO*p_i/gamma &
            +a_b_omega*p2*npnp*(6+14*delta)/NINE &
            -a_b_omega*fac*TWO*p_i**(14*THIRD)
       f(i) = fi
     end do
     call sum_vectors_(tonto_parallel,f)
     STOP_TIMER("DFTGRID:r_lyp_correlation_functional")
      CHECK
   end subroutine

   subroutine d_r_lyp_correlation_functional(self,p,np,local,non_local)
    DFTGRID :: self
   ! Return the derivatives of the LYP correlation functional.
   ! These equations are essentially the same as in the appendix of JCP 98(7)
   ! 5612-5626.
     REALVEC(:), IN :: p
     REALMAT(:,:), IN :: np
     REALVEC(:) :: local
     REALMAT(:,:) :: non_local
     REALVEC(3) :: npa
     REAL :: a,b,c,d,e,c_f,ab9
     REAL :: pa,pa_third,p_third,npanpa
     REAL :: gamma_inv,mu,abw9_pa,delta
     INT :: i

     STACK("DFTGRID:d_r_lyp_correlation_functional")
     START_TIMER("DFTGRID:d_r_lyp_correlation_functional")
     c_f = (THREE/TEN)*(3*PI*PI)**(THIRD+THIRD)
     a = 0.04918
     b = 0.132
     c = 0.2533
     d = 0.349
     e = (TWO)**(11*THIRD) * c_f
     ab9 = a*b/NINE

     do i=1,size(p)
       pa = HALF*p(i)
       npa = HALF*np(i,:)
       npanpa = dot_product(npa,npa)
       pa_third = pa**(THIRD)
       p_third = TWO**(THIRD)*pa_third
       gamma_inv = ONE/(ONE + d / p_third)
       mu=d*gamma_inv/p_third
       abw9_pa = (HALF)**4 * ab9*exp(-c/p_third) * p_third/(pa*pa*pa) * gamma_inv
       delta = c/p_third + mu

       local(i) = -a*gamma_inv*(ONE+THIRD*mu) &
         +abw9_pa*npanpa*(SEVEN/3*(mu*mu+delta*delta)-13*delta-5) &
         -abw9_pa*e*(3*delta+9)*pa*pa*pa/pa_third
       non_local(i,:) = abw9_pa*pa*npa*(6+14*delta)
     end do
     STOP_TIMER("DFTGRID:d_r_lyp_correlation_functional")
      CHECK
   end subroutine

end
