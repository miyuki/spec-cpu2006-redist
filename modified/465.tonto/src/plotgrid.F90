!---------------------------------------------------------------------
!
! PLOTGRID: for cartesian grid generation, e.g. for plots or surfaces.
!
! Copyright (C) Dylan Jayatilaka, 1997
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
! $Id: plotgrid.foo,v 1.15.2.5 2004/04/21 09:12:56 reaper Exp $
!
!---------------------------------------------------------------------

module PLOTGRID_MODULE

#  include "plotgrid.use"

   implicit none

#  include "macros"
#  include "plotgrid.int"


contains

   subroutine create(self,atom)
    PLOTGRID :: self
   ! Create object
     PTR :: self
     ATOMVEC(:), PTR, optional :: atom
     STACK("PLOTGRID:create")
     START_TIMER("PLOTGRID:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(PLOTGRID_SIZE)
     call set_defaults_(self,atom)
     STOP_TIMER("PLOTGRID:create")
      UNSTACK
   end subroutine

   subroutine create_copy(self,grid)
    PLOTGRID :: self
   ! Create a grid object which is a duplicate of grid.
     PTR :: self
     PLOTGRID, IN :: grid
     STACK("PLOTGRID:create_copy")
     START_TIMER("PLOTGRID:create_copy")
     call create_(self)
     call copy_(self,grid)
     STOP_TIMER("PLOTGRID:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,grid)
    PLOTGRID :: self
   ! Copy the contents of grid to self.
     PLOTGRID, IN :: grid
     STACK("PLOTGRID:copy")
     START_TIMER("PLOTGRID:copy")
     self = grid
     STOP_TIMER("PLOTGRID:copy")
      CHECK
   end subroutine

   subroutine destroy(self)
    PLOTGRID :: self
   ! Destroy a PLOTGRID object
      PTR :: self
      STACK("PLOTGRID:destroy")
      START_TIMER("PLOTGRID:destroy")
      if (NOT associated(self)) then; STOP_TIMER("PLOTGRID:destroy") UNSTACK return; end if
      deallocate(self)
      DELETE_MEMORY(PLOTGRID_SIZE)
     STOP_TIMER("PLOTGRID:destroy")
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

   subroutine set_defaults(self,atom)
    PLOTGRID :: self
   ! Set up a default grid. If "atom" is present it is used to define an xyz
   ! bounding box for the molecule. 
      ATOMVEC(:), PTR, optional :: atom
      STACK("PLOTGRID:set_defaults")
      START_TIMER("PLOTGRID:set_defaults")
      self%plot_kind      = " "
      self%orbital        = 0
      self%centre         = ZERO
      self%offset         = ZERO
      self%centre_atom    = 0
      self%x_atom_1       = 0
      self%x_atom_2       = 0
      self%y_atom_1       = 0
      self%y_atom_2       = 0
      self%x_axis(:)      = PLOTGRID_X_AXIS
      self%y_axis(:)      = PLOTGRID_Y_AXIS
      self%z_axis(:)      = PLOTGRID_Z_AXIS
      self%n_x            = PLOTGRID_NX
      self%n_y            = PLOTGRID_NY
      self%n_z            = PLOTGRID_NZ
      self%width(:)       = PLOTGRID_WIDTH
      self%width(3)       = ZERO    ! assume a planar plot
      self%del            = self%width(1)/(PLOTGRID_NX - 1)
      self%x_width_set    = FALSE   ! These are set false only if the widths
      self%y_width_set    = FALSE   ! are explicitly read in
      self%z_width_set    = FALSE
      self%box_centre     = ZERO
      self%bounding_box   = PLOTGRID_WIDTH
      self%box_scale_factor = ONE
      self%box_axes(:,1)  = PLOTGRID_X_AXIS
      self%box_axes(:,2)  = PLOTGRID_Y_AXIS
      self%box_axes(:,3)  = PLOTGRID_Z_AXIS
      self%x_axis_defined = FALSE
      self%y_axis_defined = FALSE
      self%z_axis_defined = FALSE
      self%desired_separation = ZERO
      nullify(self%atom)
      if (present(atom)) then
      if (associated(atom)) then
         self%atom => atom
         call set_bounding_box_and_axes_(self,atom)
      end if
      end if
     STOP_TIMER("PLOTGRID:set_defaults")
      CHECK
   end subroutine

   subroutine reset_defaults(self)
    PLOTGRID :: self
   ! Reset a default grid. The only difference to set_defaults is that the
   ! bounding box and box_axes are not set again.
      STACK("PLOTGRID:reset_defaults")
      START_TIMER("PLOTGRID:reset_defaults")
      self%plot_kind      = " "
      self%orbital        = 0
      self%centre         = ZERO
      self%offset         = ZERO
      self%centre_atom    = 0
      self%x_atom_1       = 0
      self%x_atom_2       = 0
      self%y_atom_1       = 0
      self%y_atom_2       = 0
      self%n_x            = PLOTGRID_NX
      self%n_y            = PLOTGRID_NY
      self%n_z            = PLOTGRID_NZ
      self%width(:)       = PLOTGRID_WIDTH
      self%width(3)       = ZERO    ! assume a planar plot
      self%del            = self%width(1)/(PLOTGRID_NX - 1)
      self%x_width_set    = FALSE   ! These are set TRUE only if the widths
      self%y_width_set    = FALSE   ! are explicitly read in
      self%z_width_set    = FALSE
      self%x_axis_defined = FALSE
      self%y_axis_defined = FALSE
      self%z_axis_defined = FALSE
     STOP_TIMER("PLOTGRID:reset_defaults")
      CHECK
   end subroutine

   subroutine set_bounding_box(self,atom)
    PLOTGRID :: self
   ! Set up a default bounding box based on the "atom" list size.
   ! NOTE: This does not mark the widths as having been inputted.
      ATOMVEC(:), PTR :: atom
   STACK("PLOTGRID:set_bounding_box")
   START_TIMER("PLOTGRID:set_bounding_box")
   ENSURE(associated(atom),"PLOTGRID:set_bounding_box ... no atom list")
      self%box_centre   = centre_of_atoms_(atom)
      self%bounding_box = bounding_box_(atom)
      self%bounding_box = self%box_scale_factor*self%bounding_box
     STOP_TIMER("PLOTGRID:set_bounding_box")
      CHECK
   end subroutine

   subroutine set_bounding_box_and_axes(self,atom)
    PLOTGRID :: self
   ! Set up a default bounding box based on the "atom" list size.  NOTE: This
   ! does not mark the widths as having been inputted.  The bounding box and
   ! axes are used only with a use_bounding_box_and_axes= option.
      ATOMVEC(:), PTR :: atom
      REALVEC(3) :: sm
      REALMAT(3,3) :: sa
      INTVEC(3) :: order
   STACK("PLOTGRID:set_bounding_box_and_axes")
   START_TIMER("PLOTGRID:set_bounding_box_and_axes")
   ENSURE(associated(atom),"PLOTGRID:set_bounding_box_and_axes ... no atom list")
      call make_shape_moments_(atom,sm,sa)
      call quick_sort_(sm,order)
      sa = sa(:,order)
      if (determinant_(sa)<ZERO) sa(:,3) = -sa(:,3)
      self%x_axis = sa(:,1)
      self%y_axis = sa(:,2)
      self%z_axis = sa(:,3)
      self%box_axes     = sa
      self%box_centre   = centre_of_atoms_(atom)
      self%bounding_box = bounding_box_(atom,sa)
      self%bounding_box = self%box_scale_factor*self%bounding_box
     STOP_TIMER("PLOTGRID:set_bounding_box_and_axes")
      CHECK
   end subroutine

   subroutine set_points_widths_origin(self)
    PLOTGRID :: self
   ! Set the number of points along the x axis to be odd. Evaluate the grid
   ! point separation .del from the current x_width, .width(1); from this
   ! separation, evaluate the number of points along the y and z axes; adjust
   ! the y and z widths to be an exact multiple of .del; finally, set the origin
   ! once the widths are known.
      STACK("PLOTGRID:set_points_widths_origin")
      START_TIMER("PLOTGRID:set_points_widths_origin")
      self%n_x = 2*(self%n_x/2) + 1                ! Make # of axis points odd ...
      if(self%n_x>1) self%del = self%width(1)/(self%n_x-1)
      if(self%n_x==1) self%width = ZERO
      self%n_y  = nint(self%width(2)/self%del) + 1
      self%n_y = 2*(self%n_y/2) + 1
      self%width(2) = self%del*(self%n_y-1)            ! adjust y width to the nearest grid point
      self%n_z = nint(self%width(3)/self%del) + 1
      self%n_z = 2*(self%n_z/2) + 1
      self%width(3) = self%del*(self%n_z-1)            ! adjust z width to the nearest grid point
      self%n_pt = self%n_x*self%n_y*self%n_z
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:set_points_widths_origin")
      CHECK
   end subroutine

   subroutine set_desired_separation(self,del)
    PLOTGRID :: self
   ! Set the (approximate) desired separation "del" between grid points along an
   ! axis direction.  The *actual* separation used may not be the inputted
   ! separation, but may be slightly smaller: the current .x_width is used to
   ! work out the number of points along the x_axis which is then used to work
   ! out the actual .del.  NOTE: After this routine the y and z widths are
   ! changed to be the nearest multiple of "del" (actually, ".del") that exceeds
   ! their current value.
      REAL :: del
      STACK("PLOTGRID:set_desired_separation")
      START_TIMER("PLOTGRID:set_desired_separation")
      ENSURE(self%n_x>1,"PLOTGRID:set_desired_separation ... # of x_points must be greater than 1")
      ENSURE(del>0,"PLOTGRID:set_desired_separation ... del must be positive")
      WARN_IF(NOT self%x_width_set,"PLOTGRID:set_desired_separation ... default x_width used to calculate # of x_points")
      self%desired_separation = del
      self%n_x = ceiling(self%width(1)/del)
      call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:set_desired_separation")
      CHECK
   end subroutine

   subroutine set_centre_from_atom_list(self,atom)
    PLOTGRID :: self
   ! Set the .centre of the grid data from an "atom" list. The .origin of the
   ! plot isd also set using the current plot widths and plot centre.
      ATOMVEC(:), PTR :: atom
   STACK("PLOTGRID:set_centre_from_atom_list")
   START_TIMER("PLOTGRID:set_centre_from_atom_list")
   ENSURE(associated(atom),"PLOTGRID:set_centre_from_atom_list ... atom list not present")
   ENSURE(self%centre_atom/=0,"PLOTGRID:set_centre_from_atom_list ... no centre_atom")
      self%centre = atom(self%centre_atom)%pos
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:set_centre_from_atom_list")
      CHECK
   end subroutine

   subroutine offset_centre(self)
    PLOTGRID :: self
   ! Offset the .centre by .offset along the current axes.
      STACK("PLOTGRID:offset_centre")
      START_TIMER("PLOTGRID:offset_centre")
      self%centre = self%centre + self%offset(1)*self%x_axis
      self%centre = self%centre + self%offset(2)*self%y_axis
      self%centre = self%centre + self%offset(3)*self%z_axis
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:offset_centre")
      CHECK
   end subroutine

   subroutine set_origin(self)
    PLOTGRID :: self
   ! Set the origin of the plot (the bottom, front, left-hand corner). It is
   ! equal to the .centre of the plot minus half the (current) .widths along the
   ! (current) x, y, and z axes.
      STACK("PLOTGRID:set_origin")
      START_TIMER("PLOTGRID:set_origin")
      self%origin = self%centre
      self%origin = self%origin -self%width(1)*HALF*self%x_axis
      self%origin = self%origin -self%width(2)*HALF*self%y_axis
      self%origin = self%origin -self%width(3)*HALF*self%z_axis
     STOP_TIMER("PLOTGRID:set_origin")
      CHECK
   end subroutine

   subroutine set_x_axis_from_atom_list(self,atom)
    PLOTGRID :: self
   ! Set the x-axis of the grid data from an "atom" list. If the .x_width has
   ! not been explicitly set, it is set to twice the length between the atom
   ! separation (because probably the user wants to see at least those atoms in
   ! his plot!).
      ATOMVEC(:), PTR :: atom
   STACK("PLOTGRID:set_x_axis_from_atom_list")
   START_TIMER("PLOTGRID:set_x_axis_from_atom_list")
   ENSURE(associated(atom),"PLOTGRID:set_x_axis_from_atom_list ... atom list not present")
   ENSURE(self%x_atom_1/=0,"PLOTGRID:set_x_axis_from_atom_list ... no x_atom_1")
   ENSURE(self%x_atom_2/=0,"PLOTGRID:set_x_axis_from_atom_list ... no x_atom_2")
      self%x_axis = atom(self%x_atom_2)%pos - atom(self%x_atom_1)%pos
      if (NOT self%x_width_set) then
         self%width(1) = TWO*norm_(self%x_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_x_axis_(self)
     STOP_TIMER("PLOTGRID:set_x_axis_from_atom_list")
      CHECK
   end subroutine

   subroutine set_y_axis_from_atom_list(self,atom)
    PLOTGRID :: self
   ! Set the y-axis of the grid data from an "atom" list. If the .y_width has
   ! not been explicitly set, it is set to twice the length between the atom
   ! separation (because probably the user wants to see at least those atoms in
   ! his plot!).
      ATOMVEC(:), PTR :: atom
   STACK("PLOTGRID:set_y_axis_from_atom_list")
   START_TIMER("PLOTGRID:set_y_axis_from_atom_list")
   ENSURE(associated(atom),"PLOTGRID:set_y_axis_from_atom_list ... atom list not present")
   ENSURE(self%y_atom_1/=0,"PLOTGRID:set_y_axis_from_atom_list ... no y_atom_1")
   ENSURE(self%y_atom_2/=0,"PLOTGRID:set_y_axis_from_atom_list ... no y_atom_2")
      self%y_axis = atom(self%y_atom_2)%pos - atom(self%y_atom_1)%pos
      if (NOT self%y_width_set) then
         self%width(2) = TWO*norm_(self%y_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_y_axis_(self)
     STOP_TIMER("PLOTGRID:set_y_axis_from_atom_list")
      CHECK
   end subroutine

   subroutine set_z_axis_from_atom_list(self,atom)
    PLOTGRID :: self
   ! Set the z-axis of the grid data from an "atom" list. If the .z_width has
   ! not been explicitly set, it is set to twice the length between the atom
   ! separation (because probably the user wants to see at least those atoms in
   ! his plot!).
      ATOMVEC(:), PTR :: atom
   STACK("PLOTGRID:set_z_axis_from_atom_list")
   START_TIMER("PLOTGRID:set_z_axis_from_atom_list")
   ENSURE(associated(atom),"PLOTGRID:set_z_axis_from_atom_list ... atom list not present")
   ENSURE(self%z_atom_1/=0,"PLOTGRID:set_z_axis_from_atom_list ... no z_atom_1")
   ENSURE(self%z_atom_2/=0,"PLOTGRID:set_z_axis_from_atom_list ... no z_atom_2")
      self%z_axis = atom(self%z_atom_2)%pos - atom(self%z_atom_1)%pos
      if (NOT self%z_width_set) then
         self%width(3) = TWO*norm_(self%z_axis)
         call set_points_widths_origin_(self)
      end if
      call set_xyz_axes_from_z_axis_(self)
     STOP_TIMER("PLOTGRID:set_z_axis_from_atom_list")
      CHECK
   end subroutine

   subroutine set_xyz_axes_from_x_axis(self)
    PLOTGRID :: self
   ! Set the x,y,z axes of the grid given a new x_axis vector. Also set the
   ! origin since that is dependent on the axes.
   STACK("PLOTGRID:set_xyz_axes_from_x_axis")
   START_TIMER("PLOTGRID:set_xyz_axes_from_x_axis")
   ENSURE(NOT self%x_axis_defined,"PLOTGRID:set_xyz_axes_from_x_axis ... x_axis already explicitly defined")
   ENSURE(NOT self%y_axis_defined,"PLOTGRID:set_xyz_axes_from_x_axis ... define x_axis before y_axis")
   WARN_IF(self%z_axis_defined,"PLOTGRID:set_xyz_axes_from_x_axis ... orthonormalising inputted x_axis and y_axis to z_axis")
      call normalise_(self%x_axis)
      if (self%z_axis_defined) then; call orthonormalise_x_y_to_z_axis_(self)
      else;                      call orthonormalise_y_z_to_x_axis_(self)
      end if
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:set_xyz_axes_from_x_axis")
      CHECK
   end subroutine

   subroutine set_xyz_axes_from_y_axis(self)
    PLOTGRID :: self
   ! Set the x,y,z axes of the grid given a new y_axis vector. Also set the
   ! origin since that is dependent on the axes.
   STACK("PLOTGRID:set_xyz_axes_from_y_axis")
   START_TIMER("PLOTGRID:set_xyz_axes_from_y_axis")
   ENSURE(NOT self%y_axis_defined,"PLOTGRID:set_xyz_axes_from_y_axis ... y_axis already explicitly defined")
   ENSURE(NOT self%z_axis_defined,"PLOTGRID:set_xyz_axes_from_y_axis ... can't set y_axis: z_axis and x_axis are already defined")
   WARN("PLOTGRID:set_xyz_axes_from_y_axis ... y_axis is *always* orthonormalised to x_axis")
      call normalise_(self%y_axis)
      DIE_IF(same_as_(self%y_axis,self%x_axis),"PLOTGRID:set_xyz_axes_from_y_axis ... y_axis is the same as x_axis")
      call orthonormalise_y_z_to_x_axis_(self)
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:set_xyz_axes_from_y_axis")
      CHECK
   end subroutine

   subroutine set_xyz_axes_from_z_axis(self)
    PLOTGRID :: self
   ! Set the x,y,z axes of the grid given a new z_axis vector. Also set the
   ! origin since that is dependent on the axes.
   STACK("PLOTGRID:set_xyz_axes_from_z_axis")
   START_TIMER("PLOTGRID:set_xyz_axes_from_z_axis")
   ENSURE(NOT self%z_axis_defined,"PLOTGRID:set_xyz_axes_from_z_axis ... z_axis already explicitly defined")
   WARN_IF(x_or_y_axes_defined_(self),"PLOTGRID:set_xyz_axes_from_z_axis ... inputted x_axis, y_axis to be orthonormalised to inputted z_axis")
      call normalise_(self%z_axis)
      call orthonormalise_x_y_to_z_axis_(self)
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:set_xyz_axes_from_z_axis")
      CHECK
   end subroutine

   function x_or_y_axes_defined(self) result(res)
    PLOTGRID :: self
   ! Return TRUE if either the x or y axes have been explicitly inputted.
      BIN :: res
      STACK("PLOTGRID:x_or_y_axes_defined")
      START_TIMER("PLOTGRID:x_or_y_axes_defined")
      res = self%x_axis_defined OR self%y_axis_defined
     STOP_TIMER("PLOTGRID:x_or_y_axes_defined")
      CHECK
   end function

   subroutine orthonormalise_x_y_to_z_axis(self)
    PLOTGRID :: self
   ! Orthogonalise the x and y axes to the z axis.
      REAL :: dot
      STACK("PLOTGRID:orthonormalise_x_y_to_z_axis")
      START_TIMER("PLOTGRID:orthonormalise_x_y_to_z_axis")
      if (same_as_(self%x_axis,self%z_axis)) &        
         self%x_axis = self%y_axis                   ! Make sure x and z are different
      dot = dot_product(self%z_axis,self%x_axis)     ! Orthogonalise x_axis to z_axis
      self%x_axis = self%x_axis - dot*self%z_axis
      call normalise_(self%x_axis)
      DIE_IF(is_zero_(self%x_axis),"PLOTGRID:orthonormalise_x_y_to_z_axis ... x_axis is same as z_axis!")
      call to_cross_product_(self%y_axis,self%z_axis,self%x_axis)
     STOP_TIMER("PLOTGRID:orthonormalise_x_y_to_z_axis")
      CHECK
   end subroutine

   subroutine orthonormalise_y_z_to_x_axis(self)
    PLOTGRID :: self
   ! Orthogonalise the x and y axes to the z axis.
      REAL :: dot
      STACK("PLOTGRID:orthonormalise_y_z_to_x_axis")
      START_TIMER("PLOTGRID:orthonormalise_y_z_to_x_axis")
      if (same_as_(self%y_axis,self%x_axis)) &        
         self%y_axis = self%z_axis                   ! Make sure y and x are different
      dot = dot_product(self%x_axis,self%y_axis)     ! Orthogonalise y_axis to x_axis
      self%y_axis = self%y_axis - dot*self%x_axis
      call normalise_(self%y_axis)
      DIE_IF(is_zero_(self%y_axis),"PLOTGRID:orthonormalise_y_z_to_x_axis ... y_axis is same as x_axis!")
      call to_cross_product_(self%z_axis,self%x_axis,self%y_axis)
     STOP_TIMER("PLOTGRID:orthonormalise_y_z_to_x_axis")
      CHECK
   end subroutine

   subroutine update_for_marching_cubes(self)
    PLOTGRID :: self
   ! Update the grid data to be consistent with the (non-recursive) marching
   ! cubes algorithm. Essentially, a gridpoint is added along all dimensions.
      STACK("PLOTGRID:update_for_marching_cubes")
      START_TIMER("PLOTGRID:update_for_marching_cubes")
      WARN("PLOTGRID:update_for_marching_cubes ... adjusting grid by adding 2 extra points on all sides")
      self%n_x = self%n_x + 2
      self%n_y = self%n_y + 2
      self%n_z = self%n_z + 2
      ENSURE(self%n_x>=4,"PLOTGRID:update_for_marching_cubes ... not enough x points for marching cube isosurface algorithm")
      ENSURE(self%n_y>=4,"PLOTGRID:update_for_marching_cubes ... not enough y points for marching cube isosurface algorithm")
      ENSURE(self%n_z>=4,"PLOTGRID:update_for_marching_cubes ... not enough z points for marching cube isosurface algorithm")
      ! center remains the same, extra points added around edges
      self%width  = self%width  + 2*self%del
      self%origin = self%origin - self%del*self%x_axis
      self%origin = self%origin - self%del*self%y_axis
      self%origin = self%origin - self%del*self%z_axis
      self%n_pt = self%n_x*self%n_y*self%n_z
     STOP_TIMER("PLOTGRID:update_for_marching_cubes")
      CHECK
   end subroutine

!  *************
!  Input methods
!  *************


   subroutine read_keywords(self)
    PLOTGRID :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("PLOTGRID:read_keywords")
     START_TIMER("PLOTGRID:read_keywords")
     ENSURE(next_item_(stdin)=="{","PLOTGRID:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("PLOTGRID:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    PLOTGRID :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("PLOTGRID:process_keyword")
      START_TIMER("PLOTGRID:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                         ")  ! exit case
         case ("box_scale_factor=         "); call read_box_scale_factor_(self)
         case ("centre=                   "); call read_centre_(self)
         case ("centre_atom=              "); call read_centre_atom_(self)
         case ("centre_atoms=             "); call read_centre_atoms_(self)
         case ("crystal_plane=            "); call read_z_axis_(self)
         case ("desired_separation=       "); call read_desired_separation_(self)
         case ("kind=                     "); call read_kind_(self)
         case ("max_x_points=             "); call read_max_x_points_(self)
         case ("min_x_points=             "); call read_min_x_points_(self)
         case ("n_points=                 "); call read_x_points_(self)
         case ("normal=                   "); call read_z_axis_(self)
         case ("offset=                   "); call read_offset_(self)
         case ("orbital=                  "); call read_orbital_(self)
         case ("put                       "); call put_(self)
         case ("units=                    "); call read_units_(self)
         case ("use_bounding_box          "); call use_bounding_box_(self)
         case ("use_bounding_box_and_axes "); call use_bounding_box_and_axes_(self)
         case ("use_bounding_cube         "); call use_bounding_cube_(self)
         case ("use_bounding_cube_and_axes"); call use_bounding_cube_and_axes_(self)
         case ("x_axis=                   "); call read_x_axis_(self)
         case ("x_axis_atoms=             "); call read_x_axis_atoms_(self)
         case ("x_points=                 "); call read_x_points_(self)
         case ("x_width=                  "); call read_x_width_(self)
         case ("y_axis=                   "); call read_y_axis_(self)
         case ("y_axis_atoms=             "); call read_y_axis_atoms_(self)
         case ("y_width=                  "); call read_y_width_(self)
         case ("z_axis=                   "); call read_z_axis_(self)
         case ("z_width=                  "); call read_z_width_(self)
         case default;                    allocate(tonto%known_keywords(29))
         tonto%known_keywords(1) = "}                         "
         tonto%known_keywords(2) = "box_scale_factor=         "
         tonto%known_keywords(3) = "centre=                   "
         tonto%known_keywords(4) = "centre_atom=              "
         tonto%known_keywords(5) = "centre_atoms=             "
         tonto%known_keywords(6) = "crystal_plane=            "
         tonto%known_keywords(7) = "desired_separation=       "
         tonto%known_keywords(8) = "kind=                     "
         tonto%known_keywords(9) = "max_x_points=             "
         tonto%known_keywords(10) = "min_x_points=             "
         tonto%known_keywords(11) = "n_points=                 "
         tonto%known_keywords(12) = "normal=                   "
         tonto%known_keywords(13) = "offset=                   "
         tonto%known_keywords(14) = "orbital=                  "
         tonto%known_keywords(15) = "put                       "
         tonto%known_keywords(16) = "units=                    "
         tonto%known_keywords(17) = "use_bounding_box          "
         tonto%known_keywords(18) = "use_bounding_box_and_axes "
         tonto%known_keywords(19) = "use_bounding_cube         "
         tonto%known_keywords(20) = "use_bounding_cube_and_axes"
         tonto%known_keywords(21) = "x_axis=                   "
         tonto%known_keywords(22) = "x_axis_atoms=             "
         tonto%known_keywords(23) = "x_points=                 "
         tonto%known_keywords(24) = "x_width=                  "
         tonto%known_keywords(25) = "y_axis=                   "
         tonto%known_keywords(26) = "y_axis_atoms=             "
         tonto%known_keywords(27) = "y_width=                  "
         tonto%known_keywords(28) = "z_axis=                   "
         tonto%known_keywords(29) = "z_width=                  "
         call unknown_(tonto,word,"PLOTGRID:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("PLOTGRID:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    PLOTGRID :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("PLOTGRID:read_units")
      START_TIMER("PLOTGRID:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("PLOTGRID:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    PLOTGRID :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("PLOTGRID:read_junk")
      START_TIMER("PLOTGRID:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("PLOTGRID:read_junk")
      CHECK
   end subroutine

   subroutine read_kind(self)
    PLOTGRID :: self
   ! Read in the plot kind
      STACK("PLOTGRID:read_kind")
      START_TIMER("PLOTGRID:read_kind")
      call read_(stdin,self%plot_kind)
      select case(self%plot_kind)
         case("crystal_error_map  ")
         case("electron_density   ")
         case("laplacian_density  ")
         case("orbital_density    ")
         case("orbital            ")
         case("difference_density ")
         case("true_fermi_mobility")
         case("fermi_mobility     ")
         case("qq_plot            ")
         case("spin_density       ")
         case("current_density    ")
         case("j_density          ")
         case("jp_density         ")
         case("div_jp_density     ")
         case("jd_density         ")
         case("elf                ")
         case("tsirelson_elf      ")
         case("electric_potential ")
         case("solenoidal_jp      ")
         case("hirshfeld_density  ")
         case("stockholder_density")
         case default;              allocate(tonto%known_keywords(21))
         tonto%known_keywords(1) = "crystal_error_map  "
         tonto%known_keywords(2) = "electron_density   "
         tonto%known_keywords(3) = "laplacian_density  "
         tonto%known_keywords(4) = "orbital_density    "
         tonto%known_keywords(5) = "orbital            "
         tonto%known_keywords(6) = "difference_density "
         tonto%known_keywords(7) = "true_fermi_mobility"
         tonto%known_keywords(8) = "fermi_mobility     "
         tonto%known_keywords(9) = "qq_plot            "
         tonto%known_keywords(10) = "spin_density       "
         tonto%known_keywords(11) = "current_density    "
         tonto%known_keywords(12) = "j_density          "
         tonto%known_keywords(13) = "jp_density         "
         tonto%known_keywords(14) = "div_jp_density     "
         tonto%known_keywords(15) = "jd_density         "
         tonto%known_keywords(16) = "elf                "
         tonto%known_keywords(17) = "tsirelson_elf      "
         tonto%known_keywords(18) = "electric_potential "
         tonto%known_keywords(19) = "solenoidal_jp      "
         tonto%known_keywords(20) = "hirshfeld_density  "
         tonto%known_keywords(21) = "stockholder_density"
         call unknown_(tonto,self%plot_kind,"PLOTGRID:read_kind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("PLOTGRID:read_kind")
      CHECK
   end subroutine

   subroutine read_desired_separation(self)
    PLOTGRID :: self
   ! Read the (approximate) desired separation between grid points along an axis
   ! direction.  The number of x points for the plot is adjusted so that the
   ! *actual* separation is less than the inputted separation. NOTE: the current
   ! .x_width is used to calculate the number of points.
      REAL :: del
      STACK("PLOTGRID:read_desired_separation")
      START_TIMER("PLOTGRID:read_desired_separation")
      ENSURE(self%n_x>1,"PLOTGRID:read_desired_separation ... # of x_points must be greater than 1")
      WARN_IF(NOT self%x_width_set,"PLOTGRID:read_desired_separation ... default x_width used to calculate # of x_points")
      call read_(stdin,del)
      call set_desired_separation_(self,del)
     STOP_TIMER("PLOTGRID:read_desired_separation")
      CHECK
   end subroutine

   subroutine read_max_x_points(self)
    PLOTGRID :: self
   ! Read the *maximum number* of x points to be used for the plot. 
   ! The number of points is adjusted to be no more than this.
      INT :: max_n_x
      STACK("PLOTGRID:read_max_x_points")
      START_TIMER("PLOTGRID:read_max_x_points")
      call read_(stdin,max_n_x)
      if (self%n_x<=max_n_x) then; STOP_TIMER("PLOTGRID:read_max_x_points") CHECK return; end if
      self%n_x = max_n_x
      WARN_IF(is_even_(self%n_x),"PLOTGRID:read_max_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:read_max_x_points")
      CHECK
   end subroutine

   subroutine read_min_x_points(self)
    PLOTGRID :: self
   ! Read the *minimum number* of x points to be used for the plot. 
      INT :: min_n_x
      STACK("PLOTGRID:read_min_x_points")
      START_TIMER("PLOTGRID:read_min_x_points")
      call read_(stdin,min_n_x)
      if (min_n_x<=self%n_x) then; STOP_TIMER("PLOTGRID:read_min_x_points") CHECK return; end if
      self%n_x = min_n_x
      WARN_IF(is_even_(self%n_x),"PLOTGRID:read_min_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:read_min_x_points")
      CHECK
   end subroutine

   subroutine read_x_points(self)
    PLOTGRID :: self
   ! Read the number of x points for the plot. This is the precision
   ! of the plot.
      STACK("PLOTGRID:read_x_points")
      START_TIMER("PLOTGRID:read_x_points")
      call read_int_(stdin,self%n_x)
      ENSURE( self%n_x>0,"PLOTGRID:read_x_points ... number of x_points must be positive")
      WARN_IF(is_even_(self%n_x),"PLOTGRID:read_x_points ... resetting to nearest odd number")
      call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:read_x_points")
      CHECK
   end subroutine

   subroutine read_offset(self)
    PLOTGRID :: self
   ! Read a 3-vector, used to offset the plot along.
      STACK("PLOTGRID:read_offset")
      START_TIMER("PLOTGRID:read_offset")
      call read_(stdin,self%offset)
      call offset_centre_(self)
     STOP_TIMER("PLOTGRID:read_offset")
      CHECK
   end subroutine

   subroutine read_orbital(self)
    PLOTGRID :: self
   ! Read the orbital to plot out
      STACK("PLOTGRID:read_orbital")
      START_TIMER("PLOTGRID:read_orbital")
      call read_int_(stdin,self%orbital)
     STOP_TIMER("PLOTGRID:read_orbital")
      CHECK
   end subroutine

   subroutine read_centre_atom(self)
    PLOTGRID :: self
   ! Read the centre atom of the plot
      STACK("PLOTGRID:read_centre_atom")
      START_TIMER("PLOTGRID:read_centre_atom")
      call read_(stdin,self%centre_atom)
      call set_centre_from_atom_list_(self,self%atom)
     STOP_TIMER("PLOTGRID:read_centre_atom")
      CHECK
   end subroutine

   subroutine read_centre_atoms(self)
    PLOTGRID :: self
   ! Make the centre of the plot to be the centre of the list of atom indices.
      INTVEC(:), PTR :: atom_list
      INT :: i
      STACK("PLOTGRID:read_centre_atoms")
      START_TIMER("PLOTGRID:read_centre_atoms")
      ENSURE(associated(self%atom),"PLOTGRID:read_centre_atoms ... atom list not present")
      call read_ptr_(stdin,atom_list)
      ENSURE(size(atom_list)>0,"PLOTGRID:read_centre_atoms ... no atoms specified in input")
      self%centre = ZERO
      do i=1,size(atom_list)
        self%centre = self%centre + self%atom(atom_list(i))%pos
      end do
      self%centre = self%centre / size(atom_list)
      call destroy_(atom_list)
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:read_centre_atoms")
      CHECK
   end subroutine

   subroutine read_centre(self)
    PLOTGRID :: self
   ! Read the centre position of the plot. Also adjust the origin of the plot.
      STACK("PLOTGRID:read_centre")
      START_TIMER("PLOTGRID:read_centre")
      call read_(stdin,self%centre)
      call set_origin_(self)
     STOP_TIMER("PLOTGRID:read_centre")
      CHECK
   end subroutine

   subroutine read_x_axis(self)
    PLOTGRID :: self
   ! Read the x axis vector of the plot. Normally, the y and z axes are defined
   ! to be orthogonal to the inputted axes, based on the defualt axis settings.
   ! However, if the z axis has been explicitly defined before hand, then the x
   ! and y axes are orthogonalised to it.
      STACK("PLOTGRID:read_x_axis")
      START_TIMER("PLOTGRID:read_x_axis")
      call read_(stdin,self%x_axis)
      call set_xyz_axes_from_x_axis_(self)
      self%x_axis_defined = TRUE
     STOP_TIMER("PLOTGRID:read_x_axis")
      CHECK
   end subroutine

   subroutine read_x_axis_atoms(self)
    PLOTGRID :: self
   ! Read the atoms which define the x axis vector of the plot. If the z axis
   ! has been defined, then the x_axis is orthogonalised to it. Otherwise the y
   ! and z axes are orthogonalised to this vector.
      STACK("PLOTGRID:read_x_axis_atoms")
      START_TIMER("PLOTGRID:read_x_axis_atoms")
      call read_int_(stdin,self%x_atom_1)
      call read_int_(stdin,self%x_atom_2)
      DIE_IF(self%x_atom_1==self%x_atom_2,"PLOTGRID:read_x_axis_atoms ... cannot specify same x axis atoms")
      call set_x_axis_from_atom_list_(self,self%atom)
      self%x_axis_defined = TRUE
     STOP_TIMER("PLOTGRID:read_x_axis_atoms")
      CHECK
   end subroutine

   subroutine read_y_axis(self)
    PLOTGRID :: self
   ! Read the y axis vector of the plot. It is an error to use this command if
   ! the .z_axis has already been inputted, because the .y_axis is made
   ! orthogonal to the x_axis and z_axis. Even if the z_axis has not been
   ! defined, the y_axis is made orthogonal to the x_axis.
      STACK("PLOTGRID:read_y_axis")
      START_TIMER("PLOTGRID:read_y_axis")
      call read_(stdin,self%y_axis)
      call set_xyz_axes_from_y_axis_(self)
      self%y_axis_defined = TRUE
     STOP_TIMER("PLOTGRID:read_y_axis")
      CHECK
   end subroutine

   subroutine read_y_axis_atoms(self)
    PLOTGRID :: self
   ! Read the atoms which define the y axis vector of the plot. It is an error
   ! to use this command if the .z_axis has already been inputted, because the
   ! .y_axis is made orthogonal to the x_axis and z_axis. Even if the z_axis has
   ! not been
      STACK("PLOTGRID:read_y_axis_atoms")
      START_TIMER("PLOTGRID:read_y_axis_atoms")
      call read_int_(stdin,self%y_atom_1)
      call read_int_(stdin,self%y_atom_2)
      DIE_IF(self%y_atom_1==self%y_atom_2,"PLOTGRID:read_y_axis_atoms ... cannot specify same y axis atoms")
      call set_y_axis_from_atom_list_(self,self%atom)
      self%y_axis_defined = TRUE
     STOP_TIMER("PLOTGRID:read_y_axis_atoms")
      CHECK
   end subroutine

   subroutine read_z_axis(self)
    PLOTGRID :: self
   ! Read the z axis of the plot. NOTE: If it is inputted, the current x and y
   ! axes are made orthogonal to it.
      STACK("PLOTGRID:read_z_axis")
      START_TIMER("PLOTGRID:read_z_axis")
      call read_(stdin,self%z_axis)
      call set_xyz_axes_from_z_axis_(self)
      self%z_axis_defined = TRUE
     STOP_TIMER("PLOTGRID:read_z_axis")
      CHECK
   end subroutine

   subroutine read_z_axis_atoms(self)
    PLOTGRID :: self
   ! Read the atoms which define the z axis vector of the plot. 
      STACK("PLOTGRID:read_z_axis_atoms")
      START_TIMER("PLOTGRID:read_z_axis_atoms")
      call read_int_(stdin,self%z_atom_1)
      call read_int_(stdin,self%z_atom_2)
      DIE_IF(self%z_atom_1==self%z_atom_2,"PLOTGRID:read_z_axis_atoms ... cannot specify same z axis atoms")
      call set_z_axis_from_atom_list_(self,self%atom)
      self%z_axis_defined = TRUE
     STOP_TIMER("PLOTGRID:read_z_axis_atoms")
      CHECK
   end subroutine

   subroutine read_x_width(self)
    PLOTGRID :: self
   ! Read the x width of the plot
      STACK("PLOTGRID:read_x_width")
      START_TIMER("PLOTGRID:read_x_width")
      call read_(stdin,self%width(1))
      call set_points_widths_origin_(self)
      self%x_width_set = TRUE
     STOP_TIMER("PLOTGRID:read_x_width")
      CHECK
   end subroutine

   subroutine read_y_width(self)
    PLOTGRID :: self
   ! Read the y width of the plot
      STACK("PLOTGRID:read_y_width")
      START_TIMER("PLOTGRID:read_y_width")
      call read_(stdin,self%width(2))
      call set_points_widths_origin_(self)
      self%y_width_set = TRUE
     STOP_TIMER("PLOTGRID:read_y_width")
      CHECK
   end subroutine

   subroutine read_z_width(self)
    PLOTGRID :: self
   ! Read the z width of the plot. This is normally 0.
      STACK("PLOTGRID:read_z_width")
      START_TIMER("PLOTGRID:read_z_width")
      call read_(stdin,self%width(3))
      call set_points_widths_origin_(self)
      self%z_width_set = TRUE
     STOP_TIMER("PLOTGRID:read_z_width")
      CHECK
   end subroutine

   subroutine read_box_scale_factor(self)
    PLOTGRID :: self
   ! Read the bounding box scale factor and apply it immediately.
      STACK("PLOTGRID:read_box_scale_factor")
      START_TIMER("PLOTGRID:read_box_scale_factor")
      call read_(stdin,self%box_scale_factor)
      ENSURE(self%box_scale_factor>ZERO,"PLOTGRID:read_box_scale_factor ... scale factor not positive")
     STOP_TIMER("PLOTGRID:read_box_scale_factor")
      CHECK
   end subroutine

   subroutine set_box_scale_factor(self,factor)
    PLOTGRID :: self
   ! Set the bounding box scale factor
      REAL :: factor
      STACK("PLOTGRID:set_box_scale_factor")
      START_TIMER("PLOTGRID:set_box_scale_factor")
      self%box_scale_factor = factor
      ENSURE(self%box_scale_factor>ZERO,"PLOTGRID:set_box_scale_factor ... scale factor not positive")
     STOP_TIMER("PLOTGRID:set_box_scale_factor")
      CHECK
   end subroutine

   subroutine use_bounding_box(self)
    PLOTGRID :: self
   ! Use the default bounding box centre and bounding box widths.
       STACK("PLOTGRID:use_bounding_box")
       START_TIMER("PLOTGRID:use_bounding_box")
       call set_bounding_box_(self,self%atom)
       self%centre = self%box_centre
       self%width  = self%bounding_box
       call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:use_bounding_box")
      CHECK
   end subroutine

   subroutine use_bounding_cube(self)
    PLOTGRID :: self
   ! Use the default bounding box centre and bounding box widths.
       STACK("PLOTGRID:use_bounding_cube")
       START_TIMER("PLOTGRID:use_bounding_cube")
       call set_bounding_box_(self,self%atom)
       self%centre = self%box_centre
       self%width  = maxval(self%bounding_box)
       call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:use_bounding_cube")
      CHECK
   end subroutine

   subroutine use_bounding_box_and_axes(self)
    PLOTGRID :: self
   ! Use the default bounding box centre, bounding box axes, and bounding box
   ! widths.
       STACK("PLOTGRID:use_bounding_box_and_axes")
       START_TIMER("PLOTGRID:use_bounding_box_and_axes")
       call set_bounding_box_and_axes_(self,self%atom)
       self%centre = self%box_centre
       self%width  = self%bounding_box
       self%x_axis = self%box_axes(:,1)
       self%y_axis = self%box_axes(:,2)
       self%z_axis = self%box_axes(:,3)
       call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:use_bounding_box_and_axes")
      CHECK
   end subroutine

   subroutine use_bounding_cube_and_axes(self)
    PLOTGRID :: self
   ! Use the default bounding box centre, bounding box axes, and bounding box
   ! widths.
       STACK("PLOTGRID:use_bounding_cube_and_axes")
       START_TIMER("PLOTGRID:use_bounding_cube_and_axes")
       call set_bounding_box_and_axes_(self,self%atom)
       self%centre = self%box_centre
       self%width  = maxval(self%bounding_box)
       self%x_axis = self%box_axes(:,1)
       self%y_axis = self%box_axes(:,2)
       self%z_axis = self%box_axes(:,3)
       call set_points_widths_origin_(self)
     STOP_TIMER("PLOTGRID:use_bounding_cube_and_axes")
      CHECK
   end subroutine

!   widths_were_set result (res)
!   ! Return TRUE if any of the widths were inputted.
!      res :: BIN
!      res =  .x_width_set &
!          OR .y_width_set &
!          OR .z_width_set
!   end

!   read_crystal_plane(in,unitcell)
!   ! To read in a vector which describes a plane in the crystal.
!   ! The vector is the normal of the plane.
!     in :: INPUT
!     unitcell :: UNITCELL, IN
!     in.read( .z_axis )
!     .z_axis.rotate(unitcell.cell_matrix)
!     .z_axis.normalise
!     .x_axis.normalise
!     .y_axis.to_cross_product( .z_axis, .x_axis)
!     .y_axis.normalise
!     .x_axis.to_cross_product( .y_axis, .z_axis)
!     .x_axis.normalise
!   end

   subroutine make_points(self,x_pt,y_pt,z_pt)
    PLOTGRID :: self
   ! Make a list of the grid points
      IN :: self
      REALVEC(:), OUT :: x_pt,y_pt,z_pt
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      REAL :: ox,oy,oz
      INT :: ix,iy,iz,i_pt,t2,t3
   STACK("PLOTGRID:make_points")
   START_TIMER("PLOTGRID:make_points")
   ENSURE( size(x_pt) == self%n_pt,"PLOTGRID:make_points ... array of points not correct size")
   ENSURE( size(y_pt) == self%n_pt,"PLOTGRID:make_points ... array of points not correct size")
   ENSURE( size(z_pt) == self%n_pt,"PLOTGRID:make_points ... array of points not correct size")
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      t2 = self%n_x*self%n_y
      do i_pt = 1,self%n_pt
         iz = (i_pt-1)/t2
         t3 = i_pt - iz*t2
         iy = (t3-1)/self%n_x
         ix = t3 - iy*self%n_x
         x_pt(i_pt) = ox + ix*x1 + iy*y1 + iz*z1
         y_pt(i_pt) = oy + ix*x2 + iy*y2 + iz*z2
         z_pt(i_pt) = oz + ix*x3 + iy*y3 + iz*z3
      end do
     STOP_TIMER("PLOTGRID:make_points")
      CHECK
   end subroutine

   subroutine make_points_1(self,pt,first_pt,last_pt)
    PLOTGRID :: self
   ! Return a list of the grid points in "pt". Will return a subset of the grid points
   ! from "first_pt" to "last_pt", if these options are provided.
      IN :: self
      INT, IN, optional :: first_pt,last_pt
      REALMAT(:,:), OUT :: pt
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      REAL :: ox,oy,oz
      INT :: ix,iy,iz,i_pt,first,last,t2,t3
      STACK("PLOTGRID:make_points_1")
      START_TIMER("PLOTGRID:make_points_1")
      ENSURE(self%n_x/=0,"PLOTGRID:make_points_1 ... zero number of x points")
      ENSURE(self%n_y/=0,"PLOTGRID:make_points_1 ... zero number of y points")
      first = 1;          last = size(pt,1)
      if (present(first_pt))     first = first_pt
      if (present(last_pt))      last  = last_pt
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      t2 = self%n_x*self%n_y
      do i_pt = first,last
         iz = (i_pt-1)/t2
         t3 = i_pt - iz*t2
         iy = (t3-1)/self%n_x
         ix = t3 - iy*self%n_x
         pt(i_pt,1) = ox + ix*x1 + iy*y1 + iz*z1
         pt(i_pt,2) = oy + ix*x2 + iy*y2 + iz*z2
         pt(i_pt,3) = oz + ix*x3 + iy*y3 + iz*z3
      end do
     STOP_TIMER("PLOTGRID:make_points_1")
      CHECK
   end subroutine

   subroutine make_points_2(self,pt,fx,lx,fy,ly,fz,lz)
    PLOTGRID :: self
   ! Return a list of the grid points in "pt" in grid xyz order, starting from
   ! the first and last x, y, and z points: "fx", "lx", "fy", "ly", "fz", "lz"
   ! respectively
      IN :: self
      REALMAT(:,:), OUT :: pt
      INT, IN :: fx,lx,fy,ly,fz,lz
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      REAL :: ox,oy,oz
      INT :: x,y,z,ix,iy,iz,i_pt
   STACK("PLOTGRID:make_points_2")
   START_TIMER("PLOTGRID:make_points_2")
   ENSURE(size(pt,1)==(lx-fx+1)*(ly-fy+1)*(lz-fz+1),"PLOTGRID:make_points_2 ... wrong 1st dimension, pt")
   ENSURE(size(pt,2)==3,"PLOTGRID:make_points_2 ... wrong 2nd dimension, pt")
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      i_pt = 0
      do z = fz,lz
      do y = fy,ly
      do x = fx,lx
         ix = x - 1
         iy = y - 1
         iz = z - 1
         i_pt = i_pt + 1
         pt(i_pt,1) = ox + ix*x1 + iy*y1 + iz*z1
         pt(i_pt,2) = oy + ix*x2 + iy*y2 + iz*z2
         pt(i_pt,3) = oz + ix*x3 + iy*y3 + iz*z3
      end do
      end do
      end do
     STOP_TIMER("PLOTGRID:make_points_2")
      CHECK
   end subroutine

   function point(self,x,y,z) result(res)
    PLOTGRID :: self
   ! Return one particular point "res" of the grid, given by grid coordinates
   ! "x", "y", and "z". (1,1,1) is the bottom (front) left hand corner.
      IN :: self
      INT, IN :: x,y,z
      REALVEC(3) :: res
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      INT :: ix,iy,iz
      STACK("PLOTGRID:point")
      START_TIMER("PLOTGRID:point")
      ox = self%origin(1);      oy = self%origin(2);      oz = self%origin(3)
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      ix = x - 1
      iy = y - 1
      iz = z - 1
         res(1) = ox + ix*x1 + iy*y1 + iz*z1
         res(2) = oy + ix*x2 + iy*y2 + iz*z2
         res(3) = oz + ix*x3 + iy*y3 + iz*z3
     STOP_TIMER("PLOTGRID:point")
      CHECK
   end function

   subroutine make_cube_of_points(self,p,x,y,z)
    PLOTGRID :: self
   ! Return a cube of 8 grid points "p", where the bottom (front) left hand
   ! corner point has grid coordinates "x", "y", and "z". 
      IN :: self
      REALMAT4(3,0:1,0:1,0:1) :: p
      INT :: x,y,z
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      INT :: ix,iy,iz
      STACK("PLOTGRID:make_cube_of_points")
      START_TIMER("PLOTGRID:make_cube_of_points")
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      ix = x - 1
      iy = y - 1
      iz = z - 1
      ox = self%origin(1) + ix*x1 + iy*y1 + iz*z1 ! cube origin
      oy = self%origin(2) + ix*x2 + iy*y2 + iz*z2
      oz = self%origin(3) + ix*x3 + iy*y3 + iz*z3
      do iz = 0,1
      do iy = 0,1
      do ix = 0,1
         p(1,ix,iy,iz) = ox + ix*x1 + iy*y1 + iz*z1
         p(2,ix,iy,iz) = oy + ix*x2 + iy*y2 + iz*z2
         p(3,ix,iy,iz) = oz + ix*x3 + iy*y3 + iz*z3
      end do
      end do
      end do
     STOP_TIMER("PLOTGRID:make_cube_of_points")
      CHECK
   end subroutine

   subroutine make_cube_of_points_1(self,p,f,l,del,x,y,z)
    PLOTGRID :: self
   ! Return a cube of points "p" where the cube edge points start from index "f"
   ! and end at index "l", and each point separated by distance "del" along a
   ! grid axis direction ("del" *not* the same as the grid .del).  The (0,0,0)
   ! point of the cube corresponds to the plotgrid coordinates "x", "y", "z",
   ! which are numbered from 0 (normally the first point is numbered from 1).
   ! The axes of the cube are given by the plot grid axes. 
      IN :: self
      INT, IN :: f,l,x,y,z
      REALMAT4(3,f:l,f:l,f:l) :: p
      REAL :: del
      REAL :: x1,x2,x3,y1,y2,y3,z1,z2,z3,ox,oy,oz
      INT :: n,ix,iy,iz
      STACK("PLOTGRID:make_cube_of_points_1")
      START_TIMER("PLOTGRID:make_cube_of_points_1")
      n = l - f + 1
      x1 = self%del*self%x_axis(1); x2 = self%del*self%x_axis(2); x3 = self%del*self%x_axis(3)
      y1 = self%del*self%y_axis(1); y2 = self%del*self%y_axis(2); y3 = self%del*self%y_axis(3)
      z1 = self%del*self%z_axis(1); z2 = self%del*self%z_axis(2); z3 = self%del*self%z_axis(3)
      ix = x; iy = y; iz = z 
      ox = self%origin(1) + ix*x1 + iy*y1 + iz*z1 ! cube origin
      oy = self%origin(2) + ix*x2 + iy*y2 + iz*z2
      oz = self%origin(3) + ix*x3 + iy*y3 + iz*z3
      x1 = del*self%x_axis(1); x2 = del*self%x_axis(2); x3 = del*self%x_axis(3) ! 
      y1 = del*self%y_axis(1); y2 = del*self%y_axis(2); y3 = del*self%y_axis(3)
      z1 = del*self%z_axis(1); z2 = del*self%z_axis(2); z3 = del*self%z_axis(3)
      do iz = f,l
      do iy = f,l
      do ix = f,l
         p(1,ix,iy,iz) = ox + ix*x1 + iy*y1 + iz*z1
         p(2,ix,iy,iz) = oy + ix*x2 + iy*y2 + iz*z2
         p(3,ix,iy,iz) = oz + ix*x3 + iy*y3 + iz*z3
      end do
      end do
      end do
     STOP_TIMER("PLOTGRID:make_cube_of_points_1")
      CHECK
   end subroutine

   subroutine put(self,output)
    PLOTGRID :: self
   ! Put the grid data to file "output"
      TEXTFILE, target, optional :: output
      TEXTFILE, PTR :: out
      STACK("PLOTGRID:put")
      START_TIMER("PLOTGRID:put")
      if (present(output)) then
        out => output
      else
        out => stdout
      end if
      call flush_(out)
      call text_(out,"PLOTGRID output")
      call flush_(out)
      call show_(out,"Plot grid kind          =", self%plot_kind)
      if (self%orbital /= 0) &
      call show_(out,"Plot orbital no.        =", self%orbital)
      call show_(out,"Number of X grid points =",self%n_x)
      call show_(out,"Number of Y grid points =",self%n_y)
      call show_(out,"Number of Z grid points =",self%n_z)
      call show_(out,"Total number of points  =",self%n_pt)
      call flush_(out)
      call show_(out,"X-axis width            =",self%width(1))
      call show_(out,"Y-axis width            =",self%width(2))
      call show_(out,"Z-axis width            =",self%width(3))
      call show_(out,"Centre point            =",self%centre(1), self%centre(2), self%centre(3))
      call show_(out,"Left hand corner        =",self%origin(1), self%origin(2), self%origin(3))
      call show_(out,"X-axis vector           =",self%x_axis(1), self%x_axis(2), self%x_axis(3))
      call show_(out,"Y-axis vector           =",self%y_axis(1), self%y_axis(2), self%y_axis(3))
      call show_(out,"Z-axis vector           =",self%z_axis(1), self%z_axis(2), self%z_axis(3))
      call show_(out,"Offset vector           =",self%offset(1), self%offset(2), self%offset(3))
      call show_(out,"Box center              =",self%box_centre(1),self%box_centre(2),self%box_centre(3))
      call show_(out,"Bounding box            =",self%bounding_box(1),self%bounding_box(2),self%bounding_box(3))
      call flush_(out)
      call show_(out,"center_atom             =",self%centre_atom)
      call show_(out,"x_atom_1                =",self%x_atom_1)
      call show_(out,"x_atom_2                =",self%x_atom_2)
      call show_(out,"y_atom_1                =",self%y_atom_1)
      call show_(out,"y_atom_2                =",self%y_atom_2)
      call show_(out,"del                     =",self%del)
     STOP_TIMER("PLOTGRID:put")
      CHECK
   end subroutine

end
