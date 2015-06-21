!---------------------------------------------------------------------
!
! ISOSURFACE: 
!
! For generating triangulated iso-surfaces for display with the OPENGL 
! graphics language. An isosurface is just a list of point, and a list
! of integer triples describing each triangular face of the object.
!
! You can use a homegrown "tesselate" method, or you can use the 
! marching cubes "cubify" algorithm to get the isosurface.
!
! Copyright (C) Dylan Jayatilaka, 2002
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
! $Id: isosurface.foo,v 1.2.2.23 2004/04/21 09:12:54 reaper Exp $
!
!---------------------------------------------------------------------

module ISOSURFACE_MODULE

#  include "isosurface.use"

   implicit none         

#  include "macros"
#  include "isosurface.int"


   !iter :: INT = 0

contains

   subroutine create(self,atom)
    ISOSURFACE :: self
   ! Create a grid object
     PTR :: self
     ATOMVEC(:), PTR, optional :: atom
     STACK("ISOSURFACE:create")
     START_TIMER("ISOSURFACE:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(ISOSURFACE_SIZE)
     call nullify_ptr_part_(self)
     call set_defaults_(self,atom)
     STOP_TIMER("ISOSURFACE:create")
      UNSTACK
   end subroutine

   subroutine create_copy(self,s)
    ISOSURFACE :: self
   ! Create self as a duplicate of "s".
     PTR :: self
     ISOSURFACE, IN :: s
     STACK("ISOSURFACE:create_copy")
     START_TIMER("ISOSURFACE:create_copy")
     call create_(self)
     call copy_(self,s)
     STOP_TIMER("ISOSURFACE:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,s)
    ISOSURFACE :: self
   ! Copy self.
     ISOSURFACE, IN :: s
     STACK("ISOSURFACE:copy")
     START_TIMER("ISOSURFACE:copy")
     self = s
     if (associated(s%point)) call create_copy_(self%point,s%point)
     if (associated(s%face)) call create_copy_(self%face,s%face)
     if (associated(s%point_gradient)) call create_copy_(self%point_gradient,s%point_gradient)
     if (associated(s%point_mean_curvature)) call create_copy_(self%point_mean_curvature,s%point_mean_curvature)
     if (associated(s%point_gaussian_curvature)) call create_copy_(self%point_gaussian_curvature,s%point_gaussian_curvature)
     if (associated(s%hash)) call create_copy_(self%hash,s%hash)
     if (associated(s%surface_property_values)) call create_copy_(self%surface_property_values,s%surface_property_values)
     if (associated(s%colour)) call create_copy_(self%colour,s%colour)
     STOP_TIMER("ISOSURFACE:copy")
      CHECK
   end subroutine

   subroutine destroy(self)
    ISOSURFACE :: self
   ! Destroy object
      PTR :: self
      STACK("ISOSURFACE:destroy")
      START_TIMER("ISOSURFACE:destroy")
      if (NOT associated(self)) then; STOP_TIMER("ISOSURFACE:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      deallocate(self)
      DELETE_MEMORY(ISOSURFACE_SIZE)
     STOP_TIMER("ISOSURFACE:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    ISOSURFACE :: self
   ! Nullify the pointer parts 
      STACK("ISOSURFACE:nullify_ptr_part")
      START_TIMER("ISOSURFACE:nullify_ptr_part")
      nullify(self%point)
      nullify(self%face)
      nullify(self%point_gradient)
      nullify(self%point_mean_curvature)
      nullify(self%point_gaussian_curvature)
      nullify(self%hash)
      nullify(self%surface_property_values)
      nullify(self%colour)
      nullify(self%atom)
      ! These are for tesselate method (not fully operational)
    ! nullify(.shift)
    ! nullify(.adjoining_face)
    ! nullify(.adjoining_edge)
    ! nullify(.ok)
    ! nullify(.ok_neighbours)
     STOP_TIMER("ISOSURFACE:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    ISOSURFACE :: self
   ! Destroy the pointer parts 
      STACK("ISOSURFACE:destroy_ptr_part")
      START_TIMER("ISOSURFACE:destroy_ptr_part")
      call destroy_(self%point)
      call destroy_(self%face)
      call destroy_(self%point_gradient)
      call destroy_(self%point_mean_curvature)
      call destroy_(self%point_gaussian_curvature)
      call destroy_(self%hash)
      call destroy_(self%surface_property_values)
      call destroy_(self%colour)
      nullify(self%atom) ! never destroy this
      ! These are for tesselate method (not fully operational)
    ! .shift.destroy
    ! .adjoining_face.destroy
    ! .adjoining_edge.destroy
    ! .ok.destroy
    ! .ok_neighbours.destroy
     STOP_TIMER("ISOSURFACE:destroy_ptr_part")
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
    ISOSURFACE :: self
   ! Set up a defaults for marching cubes
      ATOMVEC(:), PTR, optional :: atom
      STACK("ISOSURFACE:set_defaults")
      START_TIMER("ISOSURFACE:set_defaults")
      self%iso_kind = " "
      self%triangulation_method = "unknown"
      self%iso_value = ISOSURFACE_ISO_VALUE
      self%n_pt   = 0
      self%n_face = 0
      self%volume = ZERO
      self%volume_min = ZERO
      self%volume_max = ZERO
      self%n_skip = 0
      self%level = 0
      self%final_level = 0
      self%del = ONE
      self%surface_property = " "
      self%chop_surface_property_range = FALSE
      self%surface_property_cutoff_range = (/-huge(ONE),huge(ONE)/)
      self%x = 0
      self%y = 0
      self%z = 0
      self%big_interior = FALSE
      self%use_interpolator = FALSE
    ! .accuracy = TOL(3)
    ! .smallness = 0.8   
    ! .flatness = 0.5
    ! .n_skip = 0
      self%surface_point = ISOSURFACE_SURFACE_POINT ! [TOL(6),TOL(6),TOL(6)]
      self%surface_property_lower_bound = ISOSURFACE_PROPERTY_BOUND
      self%surface_property_upper_bound = ISOSURFACE_PROPERTY_BOUND
      if (associated(self%colour)) call destroy_(self%colour)
      call create_(self%colour)
      call set_reverse_defaults_(self%colour)
      if (present(atom)) then
      if (associated(atom)) then
         self%atom => atom
      end if
      end if
      call set_defaults_(self%grid,atom)
     STOP_TIMER("ISOSURFACE:set_defaults")
      UNSTACK
   end subroutine

!   set_default_tesselation ::: leaky
!   ! Set up a defaults.
!      .iso_value = 0.5d0
!      .n_pt   = 4
!      .n_face = 4
!      .accuracy = TOL(2)
!      .smallness = 0.8
!      .flatness = 0.5
!      .n_skip = 0
!      ! Set up a default tesselate surface, a tetrahedron.
!      .point.destroy
!      .point.create(3,4)
!      .point(:,1) = [ 1, 1, 1 ]
!      .point(:,2) = [-1,-1, 1 ]
!      .point(:,3) = [ 1,-1,-1 ]
!      .point(:,4) = [-1, 1,-1 ]
!      .shift.destroy
!      .shift.create(4)
!      .shift = sqrt(THREE)
!      .face.destroy
!      .face.create(3,4)
!      .face(:,1)  = [ 1, 2, 3 ]
!      .face(:,2)  = [ 1, 3, 4 ]
!      .face(:,3)  = [ 1, 4, 2 ]
!      .face(:,4)  = [ 2, 4, 3 ]
!      .face.destroy
!      .face.create(3,4)
!   end

   function default_big_interior(self) result(res)
    ISOSURFACE :: self
   ! Return TRUE if the interior of the isosurface is bigger than the exterior,
   ! i.e. gradients for the isosurface plot are to be reversed by default, so that
   ! they point outside the isosurface. NOTE: gradients should be reversed if the
   ! function has bigger values *inside* the isosurface.
     BIN :: res
     STACK("ISOSURFACE:default_big_interior")
     START_TIMER("ISOSURFACE:default_big_interior")
     res = TRUE
     select case (self%iso_kind)
        case("electron_density   "); res = TRUE
        case("laplacian_density  "); res = TRUE
        case("orbital_density    "); res = TRUE
        case("orbital            "); res = TRUE
        case("delta_density      "); res = TRUE ! only for +ve densities ...
        case("true_fermi_mobility"); res = TRUE
        case("fermi_mobility     "); res = TRUE
        case("spin_density       "); res = TRUE ! only for +ve densities
        case("hirshfeld_density  "); res = TRUE 
        case("stockholder_density"); res = TRUE 
        case("elf                "); res = FALSE
        case("tsirelson_elf      "); res = FALSE
        case("electric_potential "); res = TRUE
        case default;      ! don't know what it should be ... 
     end select
     STOP_TIMER("ISOSURFACE:default_big_interior")
      CHECK
   end function

   function property_bounds_set(self) result(res)
    ISOSURFACE :: self
   ! Return TRUE if the property bounds have been set
      BIN :: res
      STACK("ISOSURFACE:property_bounds_set")
      START_TIMER("ISOSURFACE:property_bounds_set")
      res = NOT same_as_(self%surface_property_lower_bound,self%surface_property_upper_bound)
     STOP_TIMER("ISOSURFACE:property_bounds_set")
      CHECK
   end function

   function surface_point_set(self) result(res)
    ISOSURFACE :: self
   ! Return TRUE if the surface point has been set
      BIN :: res
      STACK("ISOSURFACE:surface_point_set")
      START_TIMER("ISOSURFACE:surface_point_set")
      res = any(self%surface_point/=ISOSURFACE_SURFACE_POINT)
     STOP_TIMER("ISOSURFACE:surface_point_set")
      CHECK
   end function

!  *************
!  Input methods
!  *************

   subroutine read_keywords(self)
    ISOSURFACE :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("ISOSURFACE:read_keywords")
     START_TIMER("ISOSURFACE:read_keywords")
     ENSURE(next_item_(stdin)=="{","ISOSURFACE:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("ISOSURFACE:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    ISOSURFACE :: self
   ! Process a comand "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case, 
   ! the sequence is processed as if it were a separate file.
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("ISOSURFACE:process_keyword")
      START_TIMER("ISOSURFACE:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                            ")  ! exit case
         case ("colourfunction=              "); call read_colourfunction_(self)
         case ("put_colourfunction           "); call put_(self%colour)
         case ("big_interior=                "); call read_big_interior_(self)
         case ("iso_value=                   "); call read_iso_value_(self)
         case ("kind=                        "); call read_kind_(self)
         case ("plotgrid=                    "); call read_plotgrid_(self)
         case ("surface_property_lower_bound=        "); call read_surface_property_lb_(self)
         case ("surface_property_upper_bound=        "); call read_surface_property_ub_(self)
         case ("surface_property_cutoff_range=       "); call read_(stdin,self%surface_property_cutoff_range)
                                                         self%chop_surface_property_range = TRUE
         case ("chop_surface_property_range=   "); call read_(stdin,self%chop_surface_property_range)
         case ("put                          "); call put_(self)
         case ("put_connected_area           "); call put_connected_area_(self)
         case ("put_cx                       "); call put_CX_(self,"CX")
         case ("put_faces                    "); call put_faces_(self)
         case ("put_face_colours             "); call put_face_colours_(self)
         case ("put_face_normals             "); call put_face_normals_(self)
         case ("put_normals_as_vertex_rgbs   "); call put_normals_as_vertex_RGBs_(self)
         case ("put_points                   "); call put_points_(self)
         case ("put_vertex_gradients         "); call put_vertex_gradients_(self)
         case ("put_grid                     "); call put_grid_(self)
         case ("surface_point=               "); call read_surface_point_(self)
         case ("surface_property=            "); call read_surface_property_(self)
         case ("test                         "); call test_(self)
         case ("triangulation_method=        "); call read_triangulation_method_(self)
         case ("units=                       "); call read_units_(self)
         case ("use_interpolator=            "); call read_use_interpolator_(self)
         case default;           allocate(tonto%known_keywords(27))
         tonto%known_keywords(1) = "}                            "
         tonto%known_keywords(2) = "colourfunction=              "
         tonto%known_keywords(3) = "put_colourfunction           "
         tonto%known_keywords(4) = "big_interior=                "
         tonto%known_keywords(5) = "iso_value=                   "
         tonto%known_keywords(6) = "kind=                        "
         tonto%known_keywords(7) = "plotgrid=                    "
         tonto%known_keywords(8) = "surface_property_lower_bound=        "
         tonto%known_keywords(9) = "surface_property_upper_bound=        "
         tonto%known_keywords(10) = "surface_property_cutoff_range=       "
         tonto%known_keywords(11) = "chop_surface_property_range=   "
         tonto%known_keywords(12) = "put                          "
         tonto%known_keywords(13) = "put_connected_area           "
         tonto%known_keywords(14) = "put_cx                       "
         tonto%known_keywords(15) = "put_faces                    "
         tonto%known_keywords(16) = "put_face_colours             "
         tonto%known_keywords(17) = "put_face_normals             "
         tonto%known_keywords(18) = "put_normals_as_vertex_rgbs   "
         tonto%known_keywords(19) = "put_points                   "
         tonto%known_keywords(20) = "put_vertex_gradients         "
         tonto%known_keywords(21) = "put_grid                     "
         tonto%known_keywords(22) = "surface_point=               "
         tonto%known_keywords(23) = "surface_property=            "
         tonto%known_keywords(24) = "test                         "
         tonto%known_keywords(25) = "triangulation_method=        "
         tonto%known_keywords(26) = "units=                       "
         tonto%known_keywords(27) = "use_interpolator=            "
         call unknown_(tonto,word,"ISOSURFACE:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ISOSURFACE:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_junk(self)
    ISOSURFACE :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("ISOSURFACE:read_junk")
      START_TIMER("ISOSURFACE:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("ISOSURFACE:read_junk")
      CHECK
   end subroutine

   subroutine read_big_interior(self)
    ISOSURFACE :: self
   ! Read a switch which tells if the interior of the isosurface is bigger than
   ! the exterior i.e. whether to reverse the surface gradients or not.
      STACK("ISOSURFACE:read_big_interior")
      START_TIMER("ISOSURFACE:read_big_interior")
      call read_(stdin,self%big_interior)
     STOP_TIMER("ISOSURFACE:read_big_interior")
      CHECK
   end subroutine

   subroutine read_colourfunction(self)
    ISOSURFACE :: self
   ! Read the details of the colourfunction to be used
      STACK("ISOSURFACE:read_colourfunction")
      START_TIMER("ISOSURFACE:read_colourfunction")
      call read_data_and_colour_(self%colour)
     STOP_TIMER("ISOSURFACE:read_colourfunction")
      CHECK
   end subroutine

   subroutine read_iso_value(self)
    ISOSURFACE :: self
   ! Read the defining iso_value for the isosurface
      STACK("ISOSURFACE:read_iso_value")
      START_TIMER("ISOSURFACE:read_iso_value")
      call read_(stdin,self%iso_value)
     STOP_TIMER("ISOSURFACE:read_iso_value")
      CHECK
   end subroutine

   subroutine read_kind(self)
    ISOSURFACE :: self
   ! Read the kind of isosurface to plot.
      STACK("ISOSURFACE:read_kind")
      START_TIMER("ISOSURFACE:read_kind")
      call read_(stdin,self%iso_kind)
      select case (self%iso_kind)
        case("elf                ")
        case("electric_potential ")
        case("electron_density   ")
        case("fermi_mobility     ")
        case("hirshfeld_density  ")
        case("laplacian_density  ")
        case("orbital            ")
        case("orbital_density    ")
        case("spin_density       ")
        case("stockholder_density")
        case("true_fermi_mobility")
        case("tsirelson_elf      ")
        case default; allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "elf                "
        tonto%known_keywords(2) = "electric_potential "
        tonto%known_keywords(3) = "electron_density   "
        tonto%known_keywords(4) = "fermi_mobility     "
        tonto%known_keywords(5) = "hirshfeld_density  "
        tonto%known_keywords(6) = "laplacian_density  "
        tonto%known_keywords(7) = "orbital            "
        tonto%known_keywords(8) = "orbital_density    "
        tonto%known_keywords(9) = "spin_density       "
        tonto%known_keywords(10) = "stockholder_density"
        tonto%known_keywords(11) = "true_fermi_mobility"
        tonto%known_keywords(12) = "tsirelson_elf      "
        call unknown_(tonto,self%iso_kind,"ISOSURFACE:read_kind")
        deallocate(tonto%known_keywords)
     end select
     self%big_interior = default_big_interior_(self)
     STOP_TIMER("ISOSURFACE:read_kind")
      CHECK
   end subroutine

   subroutine read_triangulation_method(self)
    ISOSURFACE :: self
   ! Read the triangulation method a number whose smallness correlates with how
   ! small each face of the generated isosurface is.
      STACK("ISOSURFACE:read_triangulation_method")
      START_TIMER("ISOSURFACE:read_triangulation_method")
      call read_(stdin,self%triangulation_method)
      select case (self%triangulation_method)
         case("marching_cube          ")
         case("recursive_marching_cube")
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:read_triangulation_method")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ISOSURFACE:read_triangulation_method")
      CHECK
   end subroutine

   subroutine read_use_interpolator(self)
    ISOSURFACE :: self
   ! Read whether to use an interpolator to evaluate the function values on the
   ! surface. This may or may not be used by the object calling the isosurface
   ! module.
      STACK("ISOSURFACE:read_use_interpolator")
      START_TIMER("ISOSURFACE:read_use_interpolator")
      call read_(stdin,self%use_interpolator)
     STOP_TIMER("ISOSURFACE:read_use_interpolator")
      CHECK
   end subroutine

   subroutine read_plotgrid(self)
    ISOSURFACE :: self
   ! Read in the plot grid data
      STACK("ISOSURFACE:read_plotgrid")
      START_TIMER("ISOSURFACE:read_plotgrid")
      call reset_defaults_(self%grid) ! don't reset bounding box or axes
      call read_keywords_(self%grid)
 ! ENSURE(.grid.width.are_all_equal,"grid is not a cubical volume")
     STOP_TIMER("ISOSURFACE:read_plotgrid")
      CHECK
   end subroutine

   subroutine read_surface_point(self)
    ISOSURFACE :: self
   ! Read a surface point which lies on, or near the surface.
      STACK("ISOSURFACE:read_surface_point")
      START_TIMER("ISOSURFACE:read_surface_point")
      call read_(stdin,self%surface_point)
     STOP_TIMER("ISOSURFACE:read_surface_point")
      CHECK
   end subroutine

   subroutine read_surface_property(self)
    ISOSURFACE :: self
   ! Read surface property
      STR(STR_SIZE) :: property
      STACK("ISOSURFACE:read_surface_property")
      START_TIMER("ISOSURFACE:read_surface_property")
      call read_(stdin,self%surface_property)
      property = self%surface_property
      call to_lower_case_(property)
!      select case (property)
!         case ("mean_curvature")
!         case ("gaussian_curvature")
!         case ("rms_curvature")
!         case ("curvedness")
!         case ("shape_index")
!         case default; UNKNOWN(property)
!      end
     STOP_TIMER("ISOSURFACE:read_surface_property")
      CHECK
   end subroutine

   subroutine read_surface_property_lb(self)
    ISOSURFACE :: self
   ! Read the value of a property lower bound.
      STACK("ISOSURFACE:read_surface_property_lb")
      START_TIMER("ISOSURFACE:read_surface_property_lb")
      call read_(stdin,self%surface_property_lower_bound)
     STOP_TIMER("ISOSURFACE:read_surface_property_lb")
      CHECK
   end subroutine

   subroutine read_surface_property_ub(self)
    ISOSURFACE :: self
   ! Read the value of a property upper bound.
      STACK("ISOSURFACE:read_surface_property_ub")
      START_TIMER("ISOSURFACE:read_surface_property_ub")
      call read_(stdin,self%surface_property_upper_bound)
     STOP_TIMER("ISOSURFACE:read_surface_property_ub")
      CHECK
   end subroutine

   subroutine read_units(self)
    ISOSURFACE :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("ISOSURFACE:read_units")
      START_TIMER("ISOSURFACE:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("ISOSURFACE:read_units")
      CHECK
   end subroutine

!   read_accuracy
!   ! Read a number which tells to what accuracy each generated isosurface
!   ! is determined
!      unit :: STR
!      stdin.read(.accuracy)
!   end
!
!   read_flatness
!   ! Read a number whose smallness correlates with the flatness of the 
!   ! generated isosurface
!      unit :: STR
!      stdin.read(.flatness)
!   end
!
!   read_smallness
!   ! Read a number whose smallness correlates with how small each face of the 
!   ! generated isosurface is.
!      unit :: STR
!      stdin.read(.smallness)
!   end

!  *********************
!  Marching cube methods
!  *********************

   subroutine cubify(self,func)
    ISOSURFACE :: self
   ! Generate the isosurface using the marching cube algorithm.
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      STACK("ISOSURFACE:cubify")
      START_TIMER("ISOSURFACE:cubify")
      call prepare_grid_(self)
      call put_(self)
      select case (self%triangulation_method)
         case("marching_cube          "); call nonrecursively_cubify_(self,func)
         case("recursive_marching_cube"); call recursively_cubify_(self,func)
         case default;        allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:cubify")
         deallocate(tonto%known_keywords)
      end select
      call put_(self)
     STOP_TIMER("ISOSURFACE:cubify")
      UNSTACK
   end subroutine

   subroutine prepare_grid(self)
    ISOSURFACE :: self
   ! Prepare the grid for the different kinds of isosurface alogorithms.
   ! Particularly, for the recursive method, set the .final_level and the
   ! .scan_level. Then output the grid information before doing anything, so
   ! the user can check everything is OK.
      INT :: n_div,n_pt
      REAL :: scale
      STACK("ISOSURFACE:prepare_grid")
      START_TIMER("ISOSURFACE:prepare_grid")
      select case (self%triangulation_method)
         case("marching_cube          ")
            call update_for_marching_cubes_(self%grid)
            self%del = self%grid%del
         case("recursive_marching_cube")
            ENSURE(are_all_equal_(self%grid%width),"ISOSURFACE:prepare_grid ... grid widths must be all equal")
            n_div = no_of_divisions_(self,ONE)  ! Should be an adjustable parameter?
            n_div = max(n_div,4)           ! At least 4 divisions ...
            n_pt = 2**n_div
            if (n_pt>(self%grid%n_x-1)) then
               WARN("ISOSURFACE:prepare_grid ... No. of points not sufficient, adjusting to be larger")
               self%grid%n_x = n_pt + 1
            end if
            self%final_level = ceiling(log(real(self%grid%n_x-2))/log(TWO))
          ! .scan_level  = ceiling(log(real(n_pt-2))/log(TWO))
            self%scan_level  = 4
            ENSURE(self%final_level<=32,"ISOSURFACE:prepare_grid ... too many grid points: # of divisions = "//trim(to_str_(self%final_level)))
            ENSURE(self%final_level>  3,"ISOSURFACE:prepare_grid ... too few grid points: # of divisions = "//trim(to_str_(self%final_level)))
            WARN("ISOSURFACE:prepare_grid ... Adjusting grid points higher to the nearest power of 2")
            self%grid%n_x = 2**self%final_level + 1     
            call set_points_widths_origin_(self%grid)
            if (self%grid%desired_separation>ZERO) then
               scale = self%grid%desired_separation/self%grid%del
               self%grid%width = self%grid%width*scale
               call set_points_widths_origin_(self%grid)
            end if
            self%del = self%grid%width(1)
         case default;        allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "marching_cube          "
         tonto%known_keywords(2) = "recursive_marching_cube"
         call unknown_(tonto,self%triangulation_method,"ISOSURFACE:prepare_grid")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ISOSURFACE:prepare_grid")
      CHECK
   end subroutine

   subroutine nonrecursively_cubify(self,func)
    ISOSURFACE :: self
   ! Generate the isosurface using the standard marching cube algorithm
   ! *without* any recursion.
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      REAL :: vol
      INT :: x,y,z
      MARCHINGCUBE :: cube
      REALMAT3(:,:,:), PTR :: f
      REALMAT4(:,:,:,:), PTR :: p
      INTVECMAT3(:,:,:), PTR :: info
      STACK("ISOSURFACE:nonrecursively_cubify")
      START_TIMER("ISOSURFACE:nonrecursively_cubify")
      call set_isosurface_info_arrays_(self)               ! Set isosurface info defaults
      call set_default_cube_(self,cube)
      self%volume_min = ZERO
      self%volume_max = ZERO
      vol = self%grid%del**3
      ! Make the 4-slab of data
      call create_(p,3,self%grid%n_x,self%grid%n_y,4)         ! slab of points
      call create_(f,self%grid%n_x,self%grid%n_y,4)           ! function values
      call create_(info,self%grid%n_x,self%grid%n_y,2,(/0,11/)) ! unique vertex index information
      call zero_(info)
      ! Find all the marching cubes. The grid points on the end
      ! are not looped over, they are only there to evaluate normals
      do z = 2,self%grid%n_z-2                      ! Loop bottom to top
         call update_4_slab_(self,p,f,z,func)             ! Get a new z-slab of function values
         call set_to_(info(:,:,1:1),info(:,:,2:2))    ! Set previously saved edge info
         call zero_(info(:,:,2:2))                     ! zero current edge info
         do y = 2,self%grid%n_y-2                   ! Loop over the slab
         do x = 2,self%grid%n_x-2                   ! ... left, right; then front, back
            call set_vertex_info_(cube,p(:,x:x+1,y:y+1,2:3),f(x:x+1,y:y+1,2:3))
            call set_case_info_(cube)
            if (is_inside_surface_(cube))      self%volume_min = self%volume_min + vol
            if (NOT is_outside_surface_(cube)) self%volume_max = self%volume_max + vol
            if (NOT is_on_surface_(cube)) cycle ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<
            call set_triangulation_info_(cube)
            ! Use previous edge info, if available. 
            if (x>2) call set_left_info_(cube,left=info(x-1,y,2)%element)
            if (y>2) call set_front_info_(cube,front=info(x,y-1,2)%element)
            if (z>2) call set_below_info_(cube,below=info(x,y,1)%element)
            call set_hessian_info_(cube,f(x-1:x+2,y-1:y+2,1:4))
            call interpolate_faces_(cube)
            call append_new_face_info_(self,cube)
            call set_to_(info(x,y,2)%element,cube%edge_vertex_index) ! current slice edge info
            call reset_(cube)
            call set_n_pt_(cube,self%n_pt)
         end do
         end do
      end do
      self%volume = HALF*(self%volume_min+self%volume_max)
      call destroy_(info)
      call destroy_(f)
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call rotate_gradients_(self)
     STOP_TIMER("ISOSURFACE:nonrecursively_cubify")
      UNSTACK
   end subroutine

   subroutine set_isosurface_info_arrays(self)
    ISOSURFACE :: self
   ! Destroy the isosurface information and set the informations arrays to nice
   ! big sizes to begin with.
      STACK("ISOSURFACE:set_isosurface_info_arrays")
      START_TIMER("ISOSURFACE:set_isosurface_info_arrays")
      self%n_pt   = 0
      self%n_face = 0
      call destroy_(self%point)
      call create_(self%point,3,ISOSURFACE_N_POINT)
      call destroy_(self%point_gradient)
      call create_(self%point_gradient,3,ISOSURFACE_N_POINT)
      call destroy_(self%point_mean_curvature)
      call create_(self%point_mean_curvature,ISOSURFACE_N_POINT)
      call destroy_(self%point_gaussian_curvature)
      call create_(self%point_gaussian_curvature,ISOSURFACE_N_POINT)
      call destroy_(self%face)
      call create_(self%face,3,ISOSURFACE_N_FACE)
     STOP_TIMER("ISOSURFACE:set_isosurface_info_arrays")
      UNSTACK
   end subroutine

   subroutine set_default_cube(self,cube)
    ISOSURFACE :: self
   ! Initialise marching "cube" to the settings required for the isosurface.
   ! NOTE: .iso_value, .del, .big_interior must all be set.
      MARCHINGCUBE, IN :: cube
      STACK("ISOSURFACE:set_default_cube")
      START_TIMER("ISOSURFACE:set_default_cube")
      call set_defaults_(cube)
      call set_iso_value_(cube,self%iso_value)
      call set_side_length_(cube,self%del)
   !  cube.set_accuracy(.accuracy)
      call set_big_interior_(cube,self%big_interior)
     STOP_TIMER("ISOSURFACE:set_default_cube")
      CHECK
   end subroutine

   subroutine update_4_slab(self,p,f,slice,func)
    ISOSURFACE :: self
   ! Make or update a "slice" of an array "f", a 4-slab of "func" function
   ! values which are evaluated at the slab points "p".  The slab involves .grid
   ! points whose z values are in the range slice-1:slice+2.  The "p" points and
   ! the function values f(:,:,2:3) correspond to the bottom and top of the
   ! slice.  The other slabs f(:,:,1) and f(:,:,4) are used for finite
   ! differences to get vertex gradients. 
      REALMAT4(:,:,:,:), INOUT :: p
      REALMAT3(:,:,:), INOUT :: f
      INT :: slice
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      INT :: fz,lz,n_pt, i,x,y,z
      REALVEC(:), PTR :: values
      REALMAT(:,:), PTR :: pt
      STACK("ISOSURFACE:update_4_slab")
      START_TIMER("ISOSURFACE:update_4_slab")
      ENSURE(slice>=2 AND slice<=self%grid%n_z-2,"ISOSURFACE:update_4_slab ... slice out f range")
      ENSURE(size(p,1)==3,"ISOSURFACE:update_4_slab ... p must have dim1=3")
      ENSURE(size(p,2)==self%grid%n_x,"ISOSURFACE:update_4_slab ... wrong size, p")
      ENSURE(size(p,3)==self%grid%n_y,"ISOSURFACE:update_4_slab ... wrong size, p")
      ENSURE(size(p,4)==4,"ISOSURFACE:update_4_slab ... p must have 4 slabs")
      ENSURE(size(f,1)==self%grid%n_x,"ISOSURFACE:update_4_slab ... wrong size, f")
      ENSURE(size(f,2)==self%grid%n_y,"ISOSURFACE:update_4_slab ... wrong size, f")
      ENSURE(size(f,3)==4,"ISOSURFACE:update_4_slab ... f must have 4 slabs")
      if (slice == 2) then 
         ! These are the first 4 slices
         fz = 1; lz = 4
      else                 
         ! In between: only calculate one slice
         p(:,:,:,1:3) = p(:,:,:,2:4)
         f(:,:,1:3) = f(:,:,2:4)
         fz = 4; lz = 4
      end if
      ! Now calculate the slab between the right limits
      n_pt = self%grid%n_x*self%grid%n_y*(lz-fz+1)
      call create_(values,n_pt)
      call create_(pt,n_pt,3)
      call make_points_(self%grid,pt,1,self%grid%n_x,1,self%grid%n_y,slice-2+fz,slice-2+lz)
      call func(values,pt)
      call set_to_(f(:,:,fz:lz),values)
      i = 0
      do z = fz,lz
      do y = 1,self%grid%n_y
      do x = 1,self%grid%n_x
         i = i + 1
         p(:,x,y,z) = pt(i,:)
       ! write(*,*) "i =",i,"pt =",pt(i,:),"val=",values(i)
      end do
      end do
      end do
      call destroy_(pt)
      call destroy_(values)
     STOP_TIMER("ISOSURFACE:update_4_slab")
      CHECK
   end subroutine

   subroutine append_new_face_info(self,cube)
    ISOSURFACE :: self
   ! Generate the isosurface using the marching cube algorithm.
      MARCHINGCUBE :: cube
      INT :: n_pt,n_face,n_col
      STACK("ISOSURFACE:append_new_face_info")
      START_TIMER("ISOSURFACE:append_new_face_info")
      n_pt   = no_of_active_edges_(cube)
      n_face = cube%n_triangle
      ! Expand info arrays if required
      n_col  = size(self%point,2)
      if ((self%n_pt+n_pt)>n_col) then
         call expand_columns_(self%point,2*n_col)
         call expand_columns_(self%point_gradient,2*n_col)
         call expand_(self%point_mean_curvature,2*n_col)
         call expand_(self%point_gaussian_curvature,2*n_col)
      end if
      n_col  = size(self%face,2)
      if ((self%n_face+n_face)>n_col) then
         call expand_columns_(self%face,2*n_col)
      end if
      ! Store isosurface info.
      call get_edge_vertex_positions_(cube,self%point(:,self%n_pt+1:self%n_pt+n_pt))
      call get_edge_vertex_gradients_(cube,self%point_gradient(:,self%n_pt+1:self%n_pt+n_pt))
      call get_edge_mean_curvatures_(cube,self%point_mean_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_edge_gaussian_curvatures_(cube,self%point_gaussian_curvature(self%n_pt+1:self%n_pt+n_pt))
      call get_triangle_vertex_indices_(cube,self%face(:,self%n_face+1:self%n_face+n_face))
      self%n_pt   = self%n_pt + n_pt
      self%n_face = self%n_face + n_face
     STOP_TIMER("ISOSURFACE:append_new_face_info")
      UNSTACK
   end subroutine

   subroutine rotate_gradients(self)
    ISOSURFACE :: self
   ! Rotate the gradients by the box axes. This needs to be done because the box
   ! axes may not be the same as the natural x-y-z axes.
      REALMAT(3,3) :: axes
      REALMAT(:,:), PTR :: old
      STACK("ISOSURFACE:rotate_gradients")
      START_TIMER("ISOSURFACE:rotate_gradients")
      axes(:,1) = self%grid%x_axis
      axes(:,2) = self%grid%y_axis
      axes(:,3) = self%grid%z_axis
      call create_(old,3,self%n_pt)
      old = self%point_gradient
      self%point_gradient = matmul(axes,old)
      call destroy_(old)
     STOP_TIMER("ISOSURFACE:rotate_gradients")
      CHECK
   end subroutine

   subroutine recursively_cubify(self,func)
    ISOSURFACE :: self
   ! Generate the isosurface using the recursive marching cube algorithm.
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      MARCHINGCUBE :: parent_cube
      REALMAT4(3,0:1,0:1,0:1) :: p2
      REALMAT3(0:1,0:1,0:1) :: f2
   STACK("ISOSURFACE:recursively_cubify")
   START_TIMER("ISOSURFACE:recursively_cubify")
   ENSURE(are_all_equal_(self%grid%width),"ISOSURFACE:recursively_cubify ... grid widths must be all equal")
      call set_isosurface_info_arrays_(self) 
      call set_default_cube_(self,parent_cube)
      self%volume_min = ZERO
      self%volume_max = ZERO
      self%n_skip = 0
      self%level = 0
      call create_(self%hash,256,3,12)
      call set_reverse_search_(self%hash,TRUE)
      self%x = 0; self%y = 0; self%z = 0
      call make_cube_of_points_(self%grid,p2,0,1,self%del,0,0,0) 
      call make_2_cube_of_values_(self,f2,func,p2)
      call set_vertex_info_(parent_cube,p2,f2)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call divide_(self,func,parent_cube) ! <<< do the work here
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call shrink_columns_(self%point,self%n_pt)
      call shrink_columns_(self%point_gradient,self%n_pt)
      call shrink_(self%point_mean_curvature,self%n_pt)
      call shrink_(self%point_gaussian_curvature,self%n_pt)
      call shrink_columns_(self%face,self%n_face)
      call rotate_gradients_(self)
      call destroy_(self%hash)
      self%volume = HALF*(self%volume_min+self%volume_max)
     STOP_TIMER("ISOSURFACE:recursively_cubify")
      UNSTACK
   end subroutine

   function no_of_divisions(self,side_length) result(res)
    ISOSURFACE :: self
   ! Return the number of binary divisions before the .grid side length
   ! becomes less than a given "side_length".
      REAL, IN :: side_length
      INT :: res
      REAL :: width
      INT :: i
   STACK("ISOSURFACE:no_of_divisions")
   START_TIMER("ISOSURFACE:no_of_divisions")
   ENSURE(are_all_equal_(self%grid%width),"ISOSURFACE:no_of_divisions ... grid is not a cubical volume")
      width = self%grid%width(1) 
      i = 0
      do 
         i = i + 1
         width = width/TWO
         if (width>side_length) cycle
         res = i
         exit
      end do
     STOP_TIMER("ISOSURFACE:no_of_divisions")
      CHECK
   end function

   recursive subroutine divide(self,func,parent_cube)
    ISOSURFACE :: self
   ! Generate the isosurface for "func" using a recursive marching cube
   ! algorithm. "parent_cube" is the enclosing parent marching cube from which
   ! the current cube was generated by a bisection method. We keep the
   ! "parent_cube" to save on function evaluations on the corners.
      interface
         subroutine func(values,pt)! The function whose isosurface we make
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      MARCHINGCUBE, IN :: parent_cube! Parent cube; importantly, has corner values
      MARCHINGCUBE :: cube! One of the 8 child cube; becomes next parent
      MARCHINGCUBEVEC(8) :: kube! The group of final 8 child "cubes"
      REALMAT4(3,0:2,0:2,0:2) :: p3! The points of all child "cubes"
      REALMAT4(3,0:4,0:4,0:4) :: p5! The interior and surrounds of the final 8-"kube"
      REALMAT3(0:2,0:2,0:2) :: f3! The interior values used to make all child "cubes"
      REALMAT3(0:4,0:4,0:4) :: f5! The interior and surrounds of the final 8-"kube"
      BINVEC(8) :: skip! Tells if we can skip any of the final 8-"kubes"
      BINMAT3(5,5,5) :: eval! Tells which of the "p5" points need to be 
                                             ! evaluated using "func".
      INTVEC(8) :: left = (/0,1,0,3,0,5,0,7/)! The lexical index of the left-cube
      INTVEC(8) :: front = (/0,0,1,2,0,0,5,6/)! The lexical index of the front-cube
      INTVEC(8) :: below = (/0,0,0,0,1,2,3,4/)! The lexical index of the bottom cube
      INTVEC(8) :: ix,iy,iz
      INT :: bit,x,y,z,k, l,f,b
      REAL :: vol
    ! del_iso :: REAL
      BIN :: do_divide
      STACK("ISOSURFACE:divide")
      START_TIMER("ISOSURFACE:divide")
      self%level = self%level + 1
      bit = self%final_level - self%level
      self%del = self%del/TWO
      vol = self%del**3
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (self%level<self%final_level) then ! Divide into 8 cubes and do again
         call make_cube_of_points_(self%grid,p3,0,2,self%del,self%x,self%y,self%z) 
         call make_3_cube_of_values_(self,f3,func,p3,parent_cube)
         call set_default_cube_(self,cube)
         do z = 0,1
         do y = 0,1
         do x = 0,1
            call set_vertex_info_(cube,p3(:,x:x+1,y:y+1,z:z+1), &
                                   f3(x:x+1,y:y+1,z:z+1))
            call set_case_info_(cube)
            if (is_inside_surface_(cube)) then
               self%volume_min = self%volume_min + vol
               self%volume_max = self%volume_max + vol
            end if
          ! del_iso = (0.50d0/.final_level)*(bit+1)
          ! del_iso = max(0.10d0,del_iso) ! should be adjustable ?
          ! do_divide = &
          !    .level < .scan_level OR &
          !   (.level== .scan_level    AND cube.is_nearly_on_surface(0.10d0)) OR &
          !   (.level==(.scan_level+1) AND cube.is_nearly_on_surface(0.05d0)) OR &
          !   (.level >(.scan_level+1) AND cube.is_on_surface)
          ! do_divide = (.level<.scan_level) OR cube.is_nearly_on_surface_old(0.10d0)
            do_divide = (self%level<self%scan_level) OR is_nearly_on_surface_(cube,1.50d0)
            if (do_divide) then 
               self%x = ibclr(self%x,bit); if (x==1) self%x = ibset(self%x,bit) ! Set cube coords
               self%y = ibclr(self%y,bit); if (y==1) self%y = ibset(self%y,bit)
               self%z = ibclr(self%z,bit); if (z==1) self%z = ibset(self%z,bit)
               call divide_(self,func,cube)                               ! <<<<<< recursive here
               self%x = ibclr(self%x,bit)                               ! Clear the cube coords
               self%y = ibclr(self%y,bit)
               self%z = ibclr(self%z,bit)
            else
               l = 2**bit - 1
               self%n_skip = self%n_skip + l*l*l + 3*l*l
            end if
         end do
         end do
         end do
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else                     ! Divide by 8 the last time, and triangulate
         call make_cube_of_points_(self%grid,p5,-1,3,self%del,self%x,self%y,self%z) 
         f5 = ZERO
         call make_3_cube_of_values_(self,f5(1:3,1:3,1:3),func,p5(:,1:3,1:3,1:3),parent_cube)
         call set_default_cube_(self,cube)
         eval = FALSE          ! First evaluate only the needed function points
         skip = TRUE
         ix = 0; iy = 0; iz = 0
         k = 0
         do z = 0,1
         do y = 0,1
         do x = 0,1
            k = k + 1
            call set_vertex_info_(cube,p5(:,x+1:x+2,y+1:y+2,z+1:z+2), &
                                   f5(x+1:x+2,y+1:y+2,z+1:z+2))
            call set_case_info_(cube)
            if (is_inside_surface_(cube))      self%volume_min = self%volume_min + vol
            if (NOT is_outside_surface_(cube)) self%volume_max = self%volume_max + vol
            if (NOT is_on_surface_(cube)) cycle 
            self%x = self%x + x                                      ! Set cube coords
            self%y = self%y + y
            self%z = self%z + z
            skip(k) = FALSE                                  
            cube%skip_bit_string = 0
            call set_triangulation_info_(cube)                      
          ! if (x>0 OR .x>0) cube.set_left_skip_bit_string 
            if (has_left_face_on_surface_(cube)) then
               if ( x>0 OR &
                  (self%x>0 AND has_key_(self%hash,(/self%x-1,self%y,self%z/),index=ix(k)))) &
                  call set_left_skip_bit_string_(cube)              ! Previous left cube exists
            end if
          ! if (y>0 OR .y>0) cube.set_front_skip_bit_string
            if (has_front_face_on_surface_(cube)) then
               if ( y>0 OR &
                  (self%y>0 AND has_key_(self%hash,(/self%x,self%y-1,self%z/),index=iy(k)))) &
                  call set_front_skip_bit_string_(cube)             ! Previous front cube exists
            end if
          ! if (z>0 OR .z>0) cube.set_below_skip_bit_string
            if (has_lower_face_on_surface_(cube)) then
               if ( z>0 OR &
                  (self%z>0 AND has_key_(self%hash,(/self%x,self%y,self%z-1/),index=iz(k)))) &
                  call set_below_skip_bit_string_(cube)             ! Previous lower cube exists
            end if
            call set_cube_bit_string_(cube)
            call set_hessian_eval_array_(cube,eval,x,y,z)          ! "eval" has the needed points
            kube(k) = cube                                   ! store for later ...
            self%x = self%x - x                                      ! Clear the cube coords
            self%y = self%y - y
            self%z = self%z - z
         end do
         end do
         end do
         eval(2:4,2:4,2:4) = FALSE                           ! <<< Evaluate only the
         call make_5_cube_of_values_(self,f5,func,p5,eval)             ! <<< required points .....
         k = 0                
         do z = 0,1            ! Now to the actual marching cube triangle interpolation
         do y = 0,1
         do x = 0,1
            k = k + 1
            if (skip(k)) cycle
            cube = kube(k)
            call set_hessian_info_(cube,f5(x:x+3,y:y+3,z:z+3))
            self%x = self%x + x                                      ! Set cube coords
            self%y = self%y + y
            self%z = self%z + z
            ! Use previously triangulated cubes if possible
            l = left(k); f = front(k); b = below(k)
            if (has_left_face_on_surface_(cube)) then
               if      ( x>0)    then; call set_left_info_(cube,kube(l)%edge_vertex_index)
               else if (ix(k)>0) then; call set_left_info_(cube,value_for_item_(self%hash,ix(k)))
          !    else if (.x>0)    then; cube.set_left_info(.hash.value_for_key([.x-1,.y,.z]))
               end if
            end if
            if (has_front_face_on_surface_(cube)) then
               if      ( y>0)    then; call set_front_info_(cube,kube(f)%edge_vertex_index)
               else if (iy(k)>0) then; call set_front_info_(cube,value_for_item_(self%hash,iy(k)))
          !    else if (.y>0)    then; cube.set_front_info(.hash.value_for_key([.x,.y-1,.z]))
               end if
            end if
            if (has_lower_face_on_surface_(cube)) then
               if      ( z>0)    then; call set_below_info_(cube,kube(b)%edge_vertex_index)
               else if (iz(k)>0) then; call set_below_info_(cube,value_for_item_(self%hash,iz(k)))
          !    else if (.z>0)    then; cube.set_below_info(.hash.value_for_key([.x,.y,.z-1]))
               end if
            end if
            call set_n_pt_(cube,self%n_pt)                             ! All known edge vertices resolved
            call interpolate_faces_(cube)                           ! Interpolate the rest ...
            call append_new_face_info_(self,cube)
            call append_pair_(self%hash,(/self%x,self%y,self%z/),cube%edge_vertex_index)
            kube(k) = cube
            self%x = self%x - x                                      ! Clear the cube coords
            self%y = self%y - y
            self%z = self%z - z
         end do
         end do
         end do
      end if
      self%del = TWO*self%del
      self%level = self%level - 1
     STOP_TIMER("ISOSURFACE:divide")
      UNSTACK
   end subroutine

   subroutine make_2_cube_of_values(self,f,func,p)
    ISOSURFACE :: self
   ! Evaluate a cube of values "f", with two points per side (starting at point
   ! 0). The values in "f" are those of the "func" function evaluated at
   ! positions "p" which are supposed to be the cube corners.
      REALMAT3(0:1,0:1,0:1) :: f
      REALMAT4(3,0:1,0:1,0:1) :: p
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      REALVEC(8) :: values
      STACK("ISOSURFACE:make_2_cube_of_values")
      START_TIMER("ISOSURFACE:make_2_cube_of_values")
      call func(values,transpose(reshape(p,(/3,8/)))) ! Evaluate the function at pt
      f = reshape(values,(/2,2,2/))                   ! Put the values into a cube
     STOP_TIMER("ISOSURFACE:make_2_cube_of_values")
      CHECK
   end subroutine

   subroutine make_3_cube_of_values(self,f,func,p,parent_cube)
    ISOSURFACE :: self
   ! Evaluate a cube of values "f", with three points per side (starting at
   ! point 0). The values in "f" are those of the "func" function evaluated at
   ! cube point positions "p". The cube points are assumed to be ordered along
   ! firstly the x-grid axis, then y-axis, then z-axis. The corner positions are
   ! not evaluated but taken from the "parent_cube" corner vertex values.
      REALMAT3(0:2,0:2,0:2), OUT :: f
      REALMAT4(3,0:2,0:2,0:2), IN :: p
      MARCHINGCUBE, IN :: parent_cube
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      REALVEC(27) :: values
      REALVEC(19) :: vals
      REALMAT(3,27) :: q
      REALMAT(19,3) :: pt
      INTVEC(19) :: edge = (/2,4,5,6,8,10,11,12,13,14,15,16,17,18,20,22,23,24,26/)
      STACK("ISOSURFACE:make_3_cube_of_values")
      START_TIMER("ISOSURFACE:make_3_cube_of_values")
      q = reshape(p,(/3,27/))    
      pt = transpose(q(:,edge))                 ! Skip edge points
      call func(vals,pt)                        ! Evaluate the function at pt
      values(edge) = vals                       ! Store values in full 3^3 array
      f = reshape(values,(/3,3,3/))               ! Put the values into a cube
      f(0,0,0) = parent_cube%value_at_vertex(0) ! Get the parent cube values
      f(2,0,0) = parent_cube%value_at_vertex(1)
      f(2,2,0) = parent_cube%value_at_vertex(2)
      f(0,2,0) = parent_cube%value_at_vertex(3)
      f(0,0,2) = parent_cube%value_at_vertex(4)
      f(2,0,2) = parent_cube%value_at_vertex(5)
      f(2,2,2) = parent_cube%value_at_vertex(6)
      f(0,2,2) = parent_cube%value_at_vertex(7)
     STOP_TIMER("ISOSURFACE:make_3_cube_of_values")
      CHECK
   end subroutine

   subroutine make_5_cube_of_values(self,f,func,p,eval)
    ISOSURFACE :: self
   ! Evaluate a 5 cube of values "f", with five points per side (starting at
   ! point 0). The values in "f" are those of the "func" function evaluated at
   ! cube point positions "p", except that only the points corresponding to the
   ! mask "eval" being TRUE are evaluated.
      REALMAT3(0:4,0:4,0:4) :: f
      REALMAT4(3,0:4,0:4,0:4) :: p
      BINMAT3(5,5,5) :: eval
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      REALVEC(125) :: values
      REALMAT(125,3) :: pt
      INT :: n_pt
      STACK("ISOSURFACE:make_5_cube_of_values")
      START_TIMER("ISOSURFACE:make_5_cube_of_values")
      n_pt = count(eval)
      pt(1:n_pt,:) = &
         reshape( &
            pack( &
               transpose(reshape(p,(/3,125/))), &
               spread(reshape(eval,(/125/)),dim=2,ncopies=3)), &
            (/n_pt,3/))
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call func(values(1:n_pt),pt(1:n_pt,:))
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      f = unpack(values,eval,f)
     STOP_TIMER("ISOSURFACE:make_5_cube_of_values")
      CHECK
   end subroutine

!  *****************
!  Tesselate methods
!  *****************

!   tesselate(func) ::: leaky
!   ! Obtain the tesselated isosurface for the function "func".
!   ! This is the main routine of the module.
!      interface
!         func(p) result (res)
!            p :: REALVEC(3)
!            res :: REAL
!         end
!      end
!   ENSURE(.point.created,"no points array")
!   ENSURE(.shift.created,"no points array")
!   ENSURE(.face.created,"no face array")
!      .initialise(func)
!      iter = 0
!      do 
!         iter = iter + 1
!         stdout.show("-------->iter =",iter)
!         if (.smooth) exit
!         .add_vertices(func)
!      end
!   end
!
!   initialise(func) 
!   ! Find the initial isovalue points for the isosurface
!      interface
!         func(p) result (res)
!            p :: REALVEC(3)
!            res :: REAL
!         end
!      end
!      n :: INT
!      p :: REALVEC(3)
!      do n = 1,.n_pt
!         p = .point(:,n)
!         func.find_isopoint(p,p,.iso_value,.accuracy) 
!         .point(:,n) = p
!      end
!      .n_skip = 0
!      .make_adjoining_data
!   end
!
!   make_adjoining_data ::: leaky
!   ! Make the initial (?) list of adjoining faces and adjoining edges
!      n,m,e,d :: INT
!      w,v :: INTVEC(3)
!      matched :: INTVEC*
!   ENSURE(.face.created,"no face array")
!      .adjoining_face.destroy
!      .adjoining_face.create(3,.n_face)
!      .adjoining_face = 0
!      .adjoining_edge.destroy
!      .adjoining_edge.create(3,.n_face)
!      .adjoining_edge = 0
!      matched.create(.n_face)
!      matched = 0
!      do n = .n_skip+1,.n_face                     ! Loop over faces
!      w = .face(:,n)
!       ! if (iter==6 AND n<=48) then
!       !    stdout.show("--->w =",w)
!       ! end
!      do m = .n_skip+1,.n_face
!         if (matched(m)==3 OR m==n) cycle
!         v = .face(:,m)
!       ! if (iter==6 AND n<=48) then
!       !    stdout.show("--->v =",v)
!       ! end
!         e = 0
!         if     ((any(v==w(1)) AND any(v==w(2)))) then; e = 1
!         else if((any(v==w(2)) AND any(v==w(3)))) then; e = 2
!         else if((any(v==w(3)) AND any(v==w(1)))) then; e = 3
!         end
!         if (e/=0) then
!            matched(m) = matched(m) + 1
!            .adjoining_face(e,n) = m
!            if     (all(v(3)/=w)) then; d = 1
!            else if(all(v(1)/=w)) then; d = 2
!            else if(all(v(2)/=w)) then; d = 3
!            end
!            .adjoining_edge(e,n) = d
!       ! if (iter==6) then
!       !    stdout.text("---")
!       !    stdout.show("n =",n)
!       !    stdout.show("m =",m)
!       !    stdout.show("e =",e)
!       !    stdout.show("d =",d)
!       ! end
!         end
!      end
!      end
!      matched.destroy
!   end
!
!   add_vertices(func) ::: leaky
!   ! Add vertices to the edges of every face which needs it.
!      interface
!         func(p) result (res)
!            p :: REALVEC(3)
!            res :: REAL
!         end
!      end
!      n_ok,n_new,n_pt,n_face, f,n,e,e1,e2,e3,m,d,d1,d2,c1,c2,t,ip :: INT
!      ic,in :: INTVEC(3)
!      p,p0,normal :: REALVEC(3)
!      face,adjoining_face,adjoining_edge,child,child_face :: INTMAT*
!      new_ok_face :: INTVEC*
!      edge :: INTMAT(3,3) = reshape([1,2,3,2,3,1,3,1,2],[3,3])
!      x1,x2 :: REAL
!      no :: INT = 0
!   ENSURE(.point.created,"no points array")
!   ENSURE(.shift.created,"no points array")
!   ENSURE(.face.created,"no face array")
!      n_ok = count(.ok)
!   stdout.show("n_ok =",n_ok)
!      n_new = .n_face - n_ok
!      n_pt  = .n_pt + 3*n_new
!      n_face = n_ok + 7*n_new
!      .point.expand_columns(n_pt)         ! May not be this many
!      .shift.expand(n_pt)
!      ! The objective is to create new versions of these from the old
!      face.create(3,n_face)        
!      adjoining_face.create(3,n_face); adjoining_face = 0
!      adjoining_edge.create(3,n_face); adjoining_edge = 0
!      ! Copy all the ok faces with ok neighbours to start
!      new_ok_face.create(.n_face)
!      f = 0
!      do n = 1,.n_face                     ! Loop over good old faces
!         !!!!!!!!!!!!!!!!!!!!!
!         if (NOT .ok(n)) cycle
!         !!!!!!!!!!!!!!!!!!!!!
!         face(:,f+1) = .face(:,n)
!         f = f + 1
!         new_ok_face(n) = f
!      end
!      ! Make 3 new points on the all the faces which are not OK.
!      ! Define the child array which stores indices of new points
!      ! as a function of the old face and old edge indices.
!      ! Define the child_face array which stores the indices of
!      ! the new corner faces in terms of the old face index.
!      child.create(3,.n_face)
!      child = 0
!      child_face.create(3,.n_face)
!      child = 0
!      ip = .n_pt     
!      do n = 1,.n_face                     ! Loop over bad old faces
!         !!!!!!!!!!!
!         if (.ok(n)) cycle        
!         !!!!!!!!!!!
!         in = .face(:,n)                   ! Parent face indices
!         do e = 1,3                        ! Loop over three old edges
!            m = .adjoining_face(e,n)       ! faces which are not OK have m
!            d = .adjoining_edge(e,n)       ! edge for the adjoining face
!            e1 = edge(1,e)
!            e2 = edge(2,e)
!            e3 = edge(3,e)
!            if (.ok(m) OR child(d,m)==0) then ! Add new child for edge
!               ip = ip + 1
!               ic(e) = ip                  ! the index of the child edge pt.
!               p = HALF*(.point(:,.face(e1,n)) + .point(:,.face(e2,n)))
!               normal = .normal_for_face(n) + .normal_for_face(m)
!               normal.normalise
!               p0 = p                      ! This is the new parent
!               x1 = ZERO
!               x2 = min(.shift(e1),.shift(e2))
!                                           ! Find the isosurface point 
!               func.find_isopoint(p,normal,.iso_value,.accuracy,x1,x2) 
!               .point(:,ip) = p            ! and add it to the expanded list
!               .shift(ip) = (p-p0).norm    ! for deciding convergence
!               child(e,n) = ip
!            else                           ! Use existing child
!               ic(e) = child(d,m)
!               d1 = edge(1,d)              ! Fix adjoining faces/edges
!               d2 = edge(2,d)
!                                           ! Connect new to old
!               c2 = child_face(d2,m)
!               c1 = child_face(d1,m)
!               adjoining_face(1,f+e1) = c2
!               adjoining_edge(1,f+e1) = 3
!               adjoining_face(3,f+e2) = c1
!               adjoining_edge(3,f+e2) = 1
!                                           ! Connect old to new
!               adjoining_face(3,c2) = f+e1
!               adjoining_edge(3,c2) = 1
!               adjoining_face(1,c1) = f+e2
!               adjoining_edge(1,c1) = 3
!            end
!         end
!                                           ! Assign 4 new faces 
!         do e = 1,3                        ! Loop over three old edges
!            e1 = edge(1,e)
!            e3 = edge(3,e)
!            face(:,f+e) = [in(e1),ic(e1),ic(e3)]
!            adjoining_face(2 ,f+e) = f + 4 ! Connect to central face
!            adjoining_edge(2 ,f+e) = e3
!            adjoining_face(e3,f+4) = f + e ! Connect central face
!            adjoining_edge(e3,f+4) = 2
!            child_face(e,n) = f+e
!         end
!         face(:,f+4) = [ic(1),ic(2),ic(3)] ! The central face
!         t = 0
!         do e = 1,3                        ! See if we need to tie-off
!            m = .adjoining_face(e,n)
!            d = .adjoining_edge(e,n)       ! edge for the adjoining face
!            if (NOT .ok(m)) cycle          ! Tie-off if get past this
!            t = t + 1
!            e1 = edge(1,e)
!            e2 = edge(2,e)
!            face(:,f+4+t) = [in(e1),in(e2),ic(e1)]
!            adjoining_face(1,f+4+t) = new_ok_face(m)
!            adjoining_edge(1,f+4+t) = d
!            adjoining_face(2,f+4+t) = f + e2
!            adjoining_edge(2,f+4+t) = 3
!            adjoining_face(3,f+4+t) = f + e1
!            adjoining_edge(3,f+4+t) = 1
!         end
! ! if (any(ic<1) OR any(ic>n_pt) OR (any(in<1) OR any(in>n_pt)) then
! !   stdout.show("ic =",ic)
! !   stdout.show("in =",in)
! !   stdout.show("ip =",ip)
! !   stdout.show("n  =",n)
! !   stdout.show("m  =",m)
! !   stdout.show("t  =",t)
! !   stdout.show("f  =",f+4+t)
! !   do e = f+1,f+4+t
! !   stdout.show("face(:,"//e.to_str.trim//")  =",face(:,e))
! !   end
! !   stdout.show("n_pt =",n_pt)
! !   stdout.show("adjoining_face(n) =",.adjoining_face(:,n))
! !   stdout.show("adjoining_edge(n) =",.adjoining_edge(:,n))
! ! end
!         f = f + 4 + t
!      end
!      child_face.destroy
!      child.destroy
!      new_ok_face.destroy
!      ! Shrink storage
!      .point.shrink_columns(ip)
!      .shift.shrink(ip)
!      adjoining_edge.shrink_columns(f)
!      adjoining_face.shrink_columns(f)
!      face.shrink_columns(f)
!      ! Destroy old faces and replace with new
!      .adjoining_edge.destroy; .adjoining_edge => adjoining_edge
!      .adjoining_face.destroy; .adjoining_face => adjoining_face
!      .face.destroy;           .face => face
!      .n_pt = .point.dim2
!      .n_face = face.dim2
!      no = no + 1
!    ! file.create("data"//no.to_str.trim)
!    ! file.open(for="write")
!    ! .put(output=file)
!    ! file.close
!    ! file.destroy
!   end
!
!   sort_faces 
!   ! This routine sorts through the list of .faces from position .n_skip and 
!   ! places those which acceptably smooth at the start of the faces array. 
!   ! The number of skipped faces .n_skip is incremented.
!      n,s :: INT
!   ENSURE(.n_face>=.n_skip,"inconsistent sizes")
!      s = 0
!      do n = .n_skip+1,.n_face        
!         if (.has_smooth_face(n)) then
!            s = s + 1
!            .face.swap_columns(.n_skip+s,n)
!         end
!      end
!      .n_skip = .n_skip + s
!   end
!
!   normal_for_face(n) result (res) 
!   ! Returns the normal for face "n"
!      n :: INT
!      res :: REALVEC(3)
!      a,b,c :: REALVEC(3)
!   ENSURE(.point.created,"no points array")
!   ENSURE(.face.created,"no face array")
!   ENSURE(n<=.face.dim2,"n too large")
!      a = .point(:,.face(1,n))
!      b = .point(:,.face(2,n))
!      c = .point(:,.face(3,n))
!      res = (b-a).cross(c-b)
!      res.normalise
!   end

!   smooth result (res) ::: leaky
!   ! Returns TRUE if the isosurface is evenly covered by enough triangles.
!      res :: BIN
!      n :: INT
!      f :: INTVEC(4)
!   ENSURE(.n_face>0,"n_face must be non-zero")
!      .make_adjoining_data
!      .ok.destroy
!      .ok.create(.n_face)
!      do n = 1,.n_face
!         f(1)   = n
!         f(2:4) = .adjoining_face(:,n)
!         if (.has_smooth_faces(f)) then; .ok(n) = TRUE
!         else;                           .ok(n) = FALSE
!         end 
!      end
!      res = all(.ok)
!   end
!
!   has_smooth_faces(n) result (res) 
!   ! Returns TRUE if the isosurface has all smooth faces "n" 
!      n :: INTVEC
!      res :: BIN
!      i :: INT
!      res = TRUE
!      do i = 1,n.dim
!         res = res AND .has_smooth_face(n(i))
!         if (NOT res) exit
!      end
!   end
!
!   has_smooth_face(n) result (res) 
!   ! Returns TRUE if the isosurface has a smooth face "n" 
!      n :: INT
!      res :: BIN
!      if (n==0) then ! assume a face with zero index is smooth
!         res = TRUE
!      else
!         res = .has_small_face(n) AND .has_flat_face(n)
!      end
!   end
!
!   has_small_face(n) result (res) 
!   ! Returns TRUE if the isosurface has a small face "n" 
!      n :: INT
!      res :: BIN
!      a,b,c :: REALVEC(3)
!   ENSURE(.point.created,"no points array")
!   ENSURE(.face.created,"no face array")
!   ENSURE(n<=.face.dim2,"n too large")
! ! ENSURE(all(.face(:,n)<.point.dim2),"face indices too large")
!      if (any(.face(:,n)>.point.dim2)) then
!         stdout.show("n_pt  =",.point.dim2)
!         stdout.show("n_pt  =",.n_pt)
!         stdout.show("faces =",.face(:,n))
!      end
!      if (n==0) then
!         res = TRUE
!      else
!      a = .point(:,.face(1,n)) 
!      b = .point(:,.face(2,n)) 
!      c = .point(:,.face(3,n)) 
!      res =  (b-a).norm < .smallness &
!         AND (c-b).norm < .smallness &
!         AND (a-c).norm < .smallness
!      end
!   end
!
!   has_flat_face(n) result (res) 
!   ! Returns TRUE if the isosurface has a flat face "n".
!      n :: INT
!      res :: BIN
!      a,b,c :: REALVEC(3)
!   ENSURE(.shift.created,"no shift array")
!   ENSURE(.face.created,"no face array")
!   ENSURE(n<=.face.dim2,"n too large")
!      if (n==0) then
!         res = TRUE
!      else
!      res = min(.shift(.face(1,n)), &
!                .shift(.face(2,n)), &
!                .shift(.face(3,n))) < .flatness
!      end
!   end
!
!   isointerval_for_face(n) result (res) 
!   ! Returns an estimate of the isosurface interval for face "n" as the
!   ! minimum of the deviations of the points from their parents.
!      n :: INT
!      res :: REAL
!   ENSURE(.shift.created,"no shift array")
!   ENSURE(.face.created,"no face array")
!   ENSURE(n<=.face.dim2,"n too large")
!      res = min(.shift(.face(1,n)), &
!                .shift(.face(2,n)), &
!                .shift(.face(3,n))) 
!   end

   function average_face_normal(self,n) result(res)
    ISOSURFACE :: self
   ! Returns the average normal for face "n"
      INT :: n
      REALVEC(3) :: res
      REALVEC(3) :: a,b,c
      STACK("ISOSURFACE:average_face_normal")
      START_TIMER("ISOSURFACE:average_face_normal")
      ENSURE(associated(self%point_gradient),"ISOSURFACE:average_face_normal ... no point_gradient array")
      ENSURE(associated(self%face),"ISOSURFACE:average_face_normal ... no face array")
      ENSURE(n<=size(self%face,2),"ISOSURFACE:average_face_normal ... n too large")
      a = self%point_gradient(:,self%face(1,n)); call normalise_(a)
      b = self%point_gradient(:,self%face(2,n)); call normalise_(b)
      c = self%point_gradient(:,self%face(3,n)); call normalise_(c)
      res = (a+b+c)/THREE
      call normalise_(res)
     STOP_TIMER("ISOSURFACE:average_face_normal")
      CHECK
   end function

   function average_face_gradient(self,n) result(res)
    ISOSURFACE :: self
   ! Returns the average gradient for face "n"
      INT :: n
      REALVEC(3) :: res
      REALVEC(3) :: a,b,c
      STACK("ISOSURFACE:average_face_gradient")
      START_TIMER("ISOSURFACE:average_face_gradient")
      ENSURE(associated(self%point_gradient),"ISOSURFACE:average_face_gradient ... no point_gradient array")
      ENSURE(associated(self%face),"ISOSURFACE:average_face_gradient ... no face array")
      ENSURE(n<=size(self%face,2),"ISOSURFACE:average_face_gradient ... n too large")
      a = self%point_gradient(:,self%face(1,n))
      b = self%point_gradient(:,self%face(2,n))
      c = self%point_gradient(:,self%face(3,n))
      res = (a+b+c)/THREE
     STOP_TIMER("ISOSURFACE:average_face_gradient")
      CHECK
   end function

!*******************************************************************************
!
! Plotting Methods
!
!*******************************************************************************


   subroutine plot_function(self,func)
    ISOSURFACE :: self
   ! Generate the isosurface using the marching cube algorithm.
      interface
         subroutine func(values,pt)
            REALVEC(:), OUT :: values
            REALMAT(:,:), IN :: pt
         end subroutine
      end interface
      STACK("ISOSURFACE:plot_function")
      START_TIMER("ISOSURFACE:plot_function")
      call create_(self%surface_property_values,size(self%point,2))
      call func(self%surface_property_values,transpose(self%point))
     STOP_TIMER("ISOSURFACE:plot_function")
      UNSTACK
   end subroutine

!  *****
!  Areas
!  *****

   subroutine put_connected_area(self)
    ISOSURFACE :: self
   ! Put the connected area for a given ".surface_property" out.
      REAL :: area
      STACK("ISOSURFACE:put_connected_area")
      START_TIMER("ISOSURFACE:put_connected_area")
      ENSURE(property_bounds_set_(self),"ISOSURFACE:put_connected_area ... unacceptable property bounds")
      ENSURE(self%surface_property_lower_bound<self%surface_property_upper_bound,"ISOSURFACE:put_connected_area ... unacceptable property bounds")
      ENSURE(surface_point_set_(self),"ISOSURFACE:put_connected_area ... surface_point not set")
      if      (self%surface_property_lower_bound/=ISOSURFACE_PROPERTY_BOUND AND self%surface_property_upper_bound/=ISOSURFACE_PROPERTY_BOUND) then
         area = connected_property_area_(self,self%surface_property,self%surface_property_lower_bound,self%surface_property_upper_bound)
      else if (self%surface_property_lower_bound/=ISOSURFACE_PROPERTY_BOUND) then
         area = connected_property_area_(self,self%surface_property,lower=self%surface_property_lower_bound)
      else if (self%surface_property_upper_bound/=ISOSURFACE_PROPERTY_BOUND) then
         area = connected_property_area_(self,self%surface_property,upper=self%surface_property_lower_bound)
      end if
      call flush_(stdout)
      call text_(stdout,"ISOSURFACE Property area")
      call flush_(stdout)
      call show_(stdout,"Surface property               =",self%surface_property)
      call show_(stdout,"Surface point                  =",self%surface_point)
      call show_(stdout,"Index of nearest surface point =",index_of_nearest_point_(self))
      if (self%surface_property_lower_bound/=ISOSURFACE_PROPERTY_BOUND) then
      call show_(stdout,"Property lower bound           =",self%surface_property_lower_bound)
      end if
      if (self%surface_property_upper_bound/=ISOSURFACE_PROPERTY_BOUND) then
      call show_(stdout,"Property upper bound           =",self%surface_property_upper_bound)
      end if
      call show_(stdout,"Connected area                 =",area)
     STOP_TIMER("ISOSURFACE:put_connected_area")
      CHECK
   end subroutine

   function index_of_nearest_point(self) result(res)
    ISOSURFACE :: self
   ! Returns the index of the nearest isosurface point to .surface_point.
      INT :: res
   STACK("ISOSURFACE:index_of_nearest_point")
   START_TIMER("ISOSURFACE:index_of_nearest_point")
   ENSURE(self%n_pt>0,"ISOSURFACE:index_of_nearest_point ... there are no isosurface points")
      res = index_of_minimum_column_norm_(self%point,offset=self%surface_point)
     STOP_TIMER("ISOSURFACE:index_of_nearest_point")
      CHECK
   end function

   function connected_property_area(self,property,lower,upper) result(res)
    ISOSURFACE :: self
   ! Returns the *connected* area of isosurface triangles where the "property"
   ! values of all vertices of a triangles connected to a "point" which lies on
   ! or near the surface is between "lower" and "upper", if present.
      STR(STR_SIZE) :: property
      REAL, optional :: lower,upper
      REAL :: res
      STR(STR_SIZE) :: prop
      REALVEC(:), PTR :: W
      STACK("ISOSURFACE:connected_property_area")
      START_TIMER("ISOSURFACE:connected_property_area")
      ENSURE(self%n_pt>0,"ISOSURFACE:connected_property_area ... there are no isosurface points")
      ENSURE(present(lower) OR present(upper),"ISOSURFACE:connected_property_area ... no bounds specified")
      prop = property
      call to_lower_case_(prop)
      select case (prop)
         case ("mean_curvature")
            res = connected_property_area_(self,self%point_mean_curvature,lower,upper)
         case ("gaussian_curvature")
            res = connected_property_area_(self,self%point_gaussian_curvature,lower,upper)
         case ("rms_curvature")
            call create_(W,self%n_pt)
            call get_vertex_RMS_curvature_(self,W)
            res = connected_property_area_(self,W,lower,upper)
            call destroy_(W)
         case ("curvedness")
            call create_(W,self%n_pt)
            call get_vertex_curvedness_(self,W)
            res = connected_property_area_(self,W,lower,upper)
            call destroy_(W)
         case ("shape_index")
            call create_(W,self%n_pt)
            call get_vertex_shape_index_(self,W)
            res = connected_property_area_(self,W,lower,upper)
            call destroy_(W)
      end select
     STOP_TIMER("ISOSURFACE:connected_property_area")
      CHECK
   end function

   function connected_property_area_1(self,property,lower,upper) result(res)
    ISOSURFACE :: self
   ! Returns the *connected* area of isosurface triangles where the "property"
   ! values of all vertices of a triangles connected to a "point" which lies on
   ! or near the surface is between "lower" and "upper", if present.
      REALVEC(:) :: property
      REAL, optional :: lower,upper
      REAL :: res
      INT :: ind
      INTVEC(2) :: loc
      STACK("ISOSURFACE:connected_property_area_1")
      START_TIMER("ISOSURFACE:connected_property_area_1")
      ENSURE(self%n_pt>0,"ISOSURFACE:connected_property_area_1 ... there are no isosurface points")
      ENSURE(size(property)==self%n_pt,"ISOSURFACE:connected_property_area_1 ... wrong size, property array")
      ENSURE(present(lower) OR present(upper),"ISOSURFACE:connected_property_area_1 ... no bounds specified")
      ind = index_of_nearest_point_(self)
      loc = minloc(self%face - ind)
      res = connected_property_area_(self,property,loc(2),lower,upper)
     STOP_TIMER("ISOSURFACE:connected_property_area_1")
      CHECK
   end function

   function connected_property_area_2(self,property,ind,lower,upper) result(res)
    ISOSURFACE :: self
   ! Returns the *connected* area of isosurface triangles where the "property"
   ! values of all vertices of triangles connected to a face with index "ind" on
   ! the surface is between "lower" and "upper", if present.
      REALVEC(:) :: property
      INT :: ind
      REAL, optional :: lower,upper
      REAL :: res
      INT :: n_vertex,i,v,p
      BINVEC(3) :: add_vertex
      INTVEC(:), PTR :: face,vertex
      STACK("ISOSURFACE:connected_property_area_2")
      START_TIMER("ISOSURFACE:connected_property_area_2")
      ENSURE(self%n_pt>0,"ISOSURFACE:connected_property_area_2 ... there are no isosurface points")
      ENSURE(size(property)==self%n_pt,"ISOSURFACE:connected_property_area_2 ... wrong size, property array")
      ENSURE(present(lower) OR present(upper),"ISOSURFACE:connected_property_area_2 ... no bounds specified")
      nullify(face)
      nullify(vertex)
      call append_(face,ind)   
      call append_(vertex,self%face(:,ind)) 
      do          ! Loop over new vertices, get all triangle faces
         n_vertex = size(vertex)
         do i = 1,self%n_face
            if (any(i==face)) cycle     ! Ignore all faces already in "face" list
            if (all(vertex/=self%face(1,i)) AND all(vertex/=self%face(1,i)) AND all(vertex/=self%face(1,i))) cycle
            add_vertex = FALSE          ! At least one vertex is connected to face "i"
            do v = 1,3 ! Are all the vertices of this face between property limits?
               p = self%face(v,i)
               if (present(lower)) then
               if (property(p)<lower) cycle
               end if
               if (present(upper)) then
               if (property(p)>upper) cycle
               end if
               add_vertex(v) = TRUE
            end do
            if (all(add_vertex)) then   ! All vertices pass limits
               call append_(face,i)           ! Add the face and any new vertices
               call append_only_if_unique_(vertex,self%face(1,i)) 
               call append_only_if_unique_(vertex,self%face(2,i)) 
               call append_only_if_unique_(vertex,self%face(3,i)) 
            end if
         end do
         if (size(vertex)==n_vertex) exit ! Exit if no more new vertices added
      end do
      res = total_area_(self,face)
      call destroy_(vertex)
      call destroy_(face)
     STOP_TIMER("ISOSURFACE:connected_property_area_2")
      CHECK
   end function

   function total_area(self,faces) result(res)
    ISOSURFACE :: self
   ! Returns the *total* area of the list of "faces".
      INTVEC(:) :: faces
      REAL :: res
      INT :: i
      STACK("ISOSURFACE:total_area")
      START_TIMER("ISOSURFACE:total_area")
      ENSURE(size(faces)>0,"ISOSURFACE:total_area ... faces array, zero size")
      ENSURE(maxval(faces)<=self%n_face,"ISOSURFACE:total_area ... faces array, value too large")
      ENSURE(minval(faces)>0,"ISOSURFACE:total_area ... faces array, nonpositive value")
      ENSURE(self%n_face>0,"ISOSURFACE:total_area ... there are no isosurface points")
      res = ZERO
      do i = 1,size(faces)
         res = res + face_area_(self,faces(i))
      end do
     STOP_TIMER("ISOSURFACE:total_area")
      CHECK
   end function

   function face_area(self,face) result(res)
    ISOSURFACE :: self
   ! Returns the face area for the face with index "face".
      INT :: face
      REAL :: res
      REALVEC(3) :: a,b,c
      STACK("ISOSURFACE:face_area")
      START_TIMER("ISOSURFACE:face_area")
      ENSURE(face>0,"ISOSURFACE:face_area ... face, nonpositive value")
      ENSURE(face<=self%n_face,"ISOSURFACE:face_area ... face, value too large")
      ENSURE(self%n_face>0,"ISOSURFACE:face_area ... there are no isosurface points")
      a = self%point(:,self%face(2,face)) - self%point(:,self%face(1,face))
      b = self%point(:,self%face(3,face)) - self%point(:,self%face(1,face))
      c = cross_(a,b)
      res = norm_(c)
     STOP_TIMER("ISOSURFACE:face_area")
      CHECK
   end function

!  ******************
!  Surface properties
!  ******************

   subroutine get_vertex_RMS_curvature(self,RMS)
    ISOSURFACE :: self
   ! Get the "RMS" curvature values for each canonical point
      REALVEC(:) :: RMS
      REALVEC(:), PTR :: k1,k2
      STACK("ISOSURFACE:get_vertex_RMS_curvature")
      START_TIMER("ISOSURFACE:get_vertex_RMS_curvature")
      ENSURE(self%n_pt>0,"ISOSURFACE:get_vertex_RMS_curvature ... no isosurface points")
      ENSURE(size(RMS)==self%n_pt,"ISOSURFACE:get_vertex_RMS_curvature ... wrong size, RMS")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call get_principal_curvatures_(self,k1,k2)
      RMS = sqrt((k1*k1+k2*k2)/TWO)
      call destroy_(k2)
      call destroy_(k1)
     STOP_TIMER("ISOSURFACE:get_vertex_RMS_curvature")
      CHECK
   end subroutine

   subroutine get_vertex_curvedness(self,C)
    ISOSURFACE :: self
   ! Get the list of Koenderinks curvedness values "C" for each canonical point
      REALVEC(:) :: C
      REALVEC(:), PTR :: k1,k2
      REAL :: fac
      STACK("ISOSURFACE:get_vertex_curvedness")
      START_TIMER("ISOSURFACE:get_vertex_curvedness")
      ENSURE(self%n_pt>0,"ISOSURFACE:get_vertex_curvedness ... no isosurface points")
      ENSURE(size(C)==self%n_pt,"ISOSURFACE:get_vertex_curvedness ... wrong size, C")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call get_principal_curvatures_(self,k1,k2)
      fac = TWO/PI
      C = fac*log(sqrt((k1*k1+k2*k2)/TWO))
      call destroy_(k2)
      call destroy_(k1)
     STOP_TIMER("ISOSURFACE:get_vertex_curvedness")
      CHECK
   end subroutine

   subroutine get_vertex_shape_index(self,SI)
    ISOSURFACE :: self
   ! Get the list of Koenderinks shape index values "SI" for each canonical point.
      REALVEC(:) :: SI
      REALVEC(:), PTR :: k1,k2
      REAL :: fac
      INT :: i
      STACK("ISOSURFACE:get_vertex_shape_index")
      START_TIMER("ISOSURFACE:get_vertex_shape_index")
      ENSURE(self%n_pt>0,"ISOSURFACE:get_vertex_shape_index ... no isosurface points")
      ENSURE(size(SI)==self%n_pt,"ISOSURFACE:get_vertex_shape_index ... wrong size, SI")
      call create_(k1,self%n_pt)
      call create_(k2,self%n_pt)
      call get_principal_curvatures_(self,k1,k2)
      fac = -TWO/PI
      do i = 1,self%n_pt
         if (k1(i)/=k2(i)) then
            SI(i) = fac*atan( (k1(i)+k2(i)) / (max(k1(i),k2(i))-min(k1(i),k2(i))) )
         else
            SI(i) = -sign(ONE,k1(i))
         end if
      end do
      call destroy_(k2)
      call destroy_(k1)
     STOP_TIMER("ISOSURFACE:get_vertex_shape_index")
      CHECK
   end subroutine

   subroutine get_principal_curvatures(self,k1,k2)
    ISOSURFACE :: self
   ! Get the principal curvatures "k1" and "k2" for each canonical point.
      REALVEC(:), PTR :: k1,k2
      REALVEC(:), PTR :: m,g
      STACK("ISOSURFACE:get_principal_curvatures")
      START_TIMER("ISOSURFACE:get_principal_curvatures")
      ENSURE(self%n_pt>0,"ISOSURFACE:get_principal_curvatures ... no isosurface points")
      ENSURE(size(k1)==self%n_pt,"ISOSURFACE:get_principal_curvatures ... wrong size, k1")
      ENSURE(size(k2)==self%n_pt,"ISOSURFACE:get_principal_curvatures ... wrong size, k1")
      m => self%point_mean_curvature
      g => self%point_gaussian_curvature
      k1 = sqrt(m*m-g)
      k2 = -k1
      k1 = m + k1
      k2 = m + k2
     STOP_TIMER("ISOSURFACE:get_principal_curvatures")
      CHECK
   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    ISOSURFACE :: self
   ! Put the isosurface data
      STACK("ISOSURFACE:put")
      START_TIMER("ISOSURFACE:put")
      call flush_(stdout)
      call text_(stdout,"ISOSURFACE data:")
      call flush_(stdout)
      call show_(stdout,"Kind of surface          =",self%iso_kind) 
      call show_(stdout,"Triangulation method     =",self%triangulation_method)
      call show_(stdout,"Iso value                =",self%iso_value) 
      call show_(stdout,"No. of points            =",self%n_pt) 
      call show_(stdout,"No. of faces             =",self%n_face) 
      call show_(stdout,"Use interpolator?        =",self%use_interpolator)
      call show_(stdout,"Interior volume          =",self%volume) 
      call show_(stdout,"Volume lower bound       =",self%volume_min) 
      call show_(stdout,"Volume upper bound       =",self%volume_max) 
      call show_(stdout,"Big interior?            =",self%big_interior) 
      call show_(stdout,"Reverse surface normals? =",self%big_interior) 
      call show_(stdout,"# of divisions           =",self%final_level) 
      call show_(stdout,"# of scan divisions      =",self%scan_level) 
      call show_(stdout,"# of func. evals skipped =",self%n_skip) 
      call show_(stdout,"% skipped                =",(1.0d2*self%n_skip)/self%grid%n_pt)
      call put_grid_(self)
     STOP_TIMER("ISOSURFACE:put")
      CHECK
   end subroutine

   subroutine put_points(self)
    ISOSURFACE :: self
   ! Put the canonically indexed list of vertices for the object
      STACK("ISOSURFACE:put_points")
      START_TIMER("ISOSURFACE:put_points")
      call flush_(stdout)
      call show_(stdout,"begin vertices ",self%n_pt)
      call put_(stdout,self%point,order="column")
      call text_(stdout,"end vertices")
     STOP_TIMER("ISOSURFACE:put_points")
      CHECK
   end subroutine

   subroutine put_faces(self)
    ISOSURFACE :: self
   ! Put the list of canonical indices for each triangular face
      INT :: f
      INTMAT(:,:), PTR :: face
      STACK("ISOSURFACE:put_faces")
      START_TIMER("ISOSURFACE:put_faces")
      call create_(face,3,self%n_face)
      do f = 1,self%n_face
         face(:,f) = self%face(:,f) - 1
      end do
      call flush_(stdout)
      call show_(stdout,"begin indices ",self%n_face)
      call put_(stdout,face,order="column")
      call text_(stdout,"end indices")
      call destroy_(face)
     STOP_TIMER("ISOSURFACE:put_faces")
      CHECK
   end subroutine

   subroutine put_vertex_gradients(self)
    ISOSURFACE :: self
   ! Put the list of gradients for each canonical point
      STACK("ISOSURFACE:put_vertex_gradients")
      START_TIMER("ISOSURFACE:put_vertex_gradients")
      call flush_(stdout)
      call show_(stdout,"begin vertex_normals ",self%n_pt)
      call put_(stdout,self%point_gradient,order="column")
      call text_(stdout,"end vertex_normals")
     STOP_TIMER("ISOSURFACE:put_vertex_gradients")
      CHECK
   end subroutine

   subroutine put_vertex_normals(self)
    ISOSURFACE :: self
   ! Put the list of normals for each canonical point
      REALMAT(:,:), PTR :: normal
      REALVEC(3) :: n
      INT :: i
      STACK("ISOSURFACE:put_vertex_normals")
      START_TIMER("ISOSURFACE:put_vertex_normals")
      ENSURE(self%n_pt>0,"ISOSURFACE:put_vertex_normals ... no isosurface points")
      call create_(normal,3,self%n_pt)
      do i = 1,self%n_pt
         n = self%point_gradient(:,i)
         call normalise_(n)
         normal(:,i) = n
      end do
      call flush_(stdout)
      call show_(stdout,"begin vertex_normals ",self%n_pt)
      call put_(stdout,normal,order="column")
      call text_(stdout,"end vertex_normals")
      call destroy_(normal)
     STOP_TIMER("ISOSURFACE:put_vertex_normals")
      CHECK
   end subroutine

   subroutine put_vertex_curvatures(self)
    ISOSURFACE :: self
   ! Put out lists of vertex surface curvature properties.
      BIN :: use_labels
      STACK("ISOSURFACE:put_vertex_curvatures")
      START_TIMER("ISOSURFACE:put_vertex_curvatures")
      use_labels = stdout%use_labels
      call set_use_labels_(stdout,FALSE)
      call flush_(stdout)
      call show_(stdout,"begin vertex_properties 5 ",self%n_pt)
      call put_vertex_mean_curvatures_(self)
      call put_vertex_gaussian_curvatures_(self)
      call put_vertex_RMS_curvature_(self)
      call put_vertex_curvedness_(self)
      call put_vertex_shape_index_(self)
      call text_(stdout,"end vertex_properties")
      call set_use_labels_(stdout,use_labels)
     STOP_TIMER("ISOSURFACE:put_vertex_curvatures")
      CHECK
   end subroutine

   subroutine put_vertex_mean_curvatures(self)
    ISOSURFACE :: self
   ! Put the list of mean curvatures for each canonical point
      STACK("ISOSURFACE:put_vertex_mean_curvatures")
      START_TIMER("ISOSURFACE:put_vertex_mean_curvatures")
      call flush_(stdout)
      call text_(stdout,"begin mean_curvature Mean_Curvature")
      call put_(stdout,self%point_mean_curvature,"column")
      call text_(stdout,"end mean_curvature")
     STOP_TIMER("ISOSURFACE:put_vertex_mean_curvatures")
      CHECK
   end subroutine

   subroutine put_vertex_gaussian_curvatures(self)
    ISOSURFACE :: self
   ! Put the list of gaussian curvatures for each canonical point
      STACK("ISOSURFACE:put_vertex_gaussian_curvatures")
      START_TIMER("ISOSURFACE:put_vertex_gaussian_curvatures")
      call flush_(stdout)
      call text_(stdout,"begin gaussian_curvature Gaussian_Curvature")
      call put_(stdout,self%point_gaussian_curvature,"column")
      call text_(stdout,"end gaussian_curvature")
     STOP_TIMER("ISOSURFACE:put_vertex_gaussian_curvatures")
      CHECK
   end subroutine

   subroutine put_vertex_RMS_curvature(self)
    ISOSURFACE :: self
   ! Put the list of RMS curvature values for each canonical point
      REALVEC(:), PTR :: RMS
      STACK("ISOSURFACE:put_vertex_RMS_curvature")
      START_TIMER("ISOSURFACE:put_vertex_RMS_curvature")
      ENSURE(self%n_pt>0,"ISOSURFACE:put_vertex_RMS_curvature ... no isosurface points")
      call create_(RMS,self%n_pt)
      call get_vertex_RMS_curvature_(self,RMS)
      call flush_(stdout)
      call text_(stdout,"begin RMS_curvature RMS_Curvature")
      call put_(stdout,RMS,"column")
      call text_(stdout,"end RMS_curvature")
      call destroy_(RMS)
     STOP_TIMER("ISOSURFACE:put_vertex_RMS_curvature")
      CHECK
   end subroutine

   subroutine put_vertex_curvedness(self)
    ISOSURFACE :: self
   ! Get the list of Koenderink curvedness values for each canonical point
      REALVEC(:), PTR :: C
      STACK("ISOSURFACE:put_vertex_curvedness")
      START_TIMER("ISOSURFACE:put_vertex_curvedness")
      ENSURE(self%n_pt>0,"ISOSURFACE:put_vertex_curvedness ... no isosurface points")
      call create_(C,self%n_pt)
      call get_vertex_curvedness_(self,C)
      call flush_(stdout)
      call text_(stdout,"begin curvedness Curvedness")
      call put_(stdout,C,"column")
      call text_(stdout,"end curvedness")
      call destroy_(C)
     STOP_TIMER("ISOSURFACE:put_vertex_curvedness")
      CHECK
   end subroutine

   subroutine put_vertex_shape_index(self)
    ISOSURFACE :: self
   ! Put the list of Koenderink shape index values for each canonical point.
      REALVEC(:), PTR :: SI
      STACK("ISOSURFACE:put_vertex_shape_index")
      START_TIMER("ISOSURFACE:put_vertex_shape_index")
      ENSURE(self%n_pt>0,"ISOSURFACE:put_vertex_shape_index ... no isosurface points")
      call create_(SI,self%n_pt)
      call get_vertex_shape_index_(self,SI)
      call flush_(stdout)
      call text_(stdout,"begin shape_index Shape_Index")
      call put_(stdout,SI,"column")
      call text_(stdout,"end shape_index")
      call destroy_(SI)
     STOP_TIMER("ISOSURFACE:put_vertex_shape_index")
      CHECK
   end subroutine

   subroutine put_grid(self)
    ISOSURFACE :: self
   ! Put the list of vertices for the object
      STACK("ISOSURFACE:put_grid")
      START_TIMER("ISOSURFACE:put_grid")
      call put_(self%grid)
     STOP_TIMER("ISOSURFACE:put_grid")
      CHECK
   end subroutine

   subroutine put_face_colours(self)
    ISOSURFACE :: self
   ! Put the colours out, at the moment just normals
      REALMAT(:,:), PTR :: n
      REALVEC(3) :: v
      INT :: i
      STACK("ISOSURFACE:put_face_colours")
      START_TIMER("ISOSURFACE:put_face_colours")
      call create_(n,3,self%n_face)
      do i = 1,self%n_face
         v = average_face_gradient_(self,i)
         if (is_zero_(v)) v = (/ONE,ZERO,ZERO/)
         v = abs(v)
         call normalise_(v)
         n(:,i) = v
      end do
      call flush_(stdout)
      call show_(stdout,"begin face_colors ",self%n_face)
      call put_(stdout,n,order="column")
      call text_(stdout,"end face_colors")
      call destroy_(n)
     STOP_TIMER("ISOSURFACE:put_face_colours")
      CHECK
   end subroutine

   subroutine put_normals_as_vertex_RGBs(self)
    ISOSURFACE :: self
   ! Put the colours out for each vertex, at the moment just normals
      REALMAT(:,:), PTR :: n
      REALVEC(3) :: v
      INT :: i
      STACK("ISOSURFACE:put_normals_as_vertex_RGBs")
      START_TIMER("ISOSURFACE:put_normals_as_vertex_RGBs")
      call create_(n,3,self%n_pt)
      do i = 1,self%n_pt
         v = self%point_gradient(:,i)
         if (is_zero_(v)) v = (/ONE,ZERO,ZERO/)
         v = abs(v)
         call normalise_(v)
         n(:,i) = v
      end do
      call flush_(stdout)
      call show_(stdout,"begin vertex_colors ",self%n_pt)
      call put_(stdout,n,order="column")
      call text_(stdout,"end vertex_colors")
      call destroy_(n)
     STOP_TIMER("ISOSURFACE:put_normals_as_vertex_RGBs")
      CHECK
   end subroutine

   subroutine put_face_normals(self)
    ISOSURFACE :: self
   ! Put the list of normals for each face
      REALMAT(:,:), PTR :: n
      REALVEC(3) :: v
      INT :: i
      STACK("ISOSURFACE:put_face_normals")
      START_TIMER("ISOSURFACE:put_face_normals")
      call create_(n,3,self%n_face)
      do i = 1,self%n_face
         v = average_face_gradient_(self,i)
         if (is_zero_(v)) v = (/ONE,ZERO,ZERO/)
         call normalise_(v)
         n(:,i) = v
      end do
      call flush_(stdout)
      call show_(stdout,"begin face_normals ",self%n_face)
      call put_(stdout,n,order="column")
      call text_(stdout,"end face_normals")
      call destroy_(n)
     STOP_TIMER("ISOSURFACE:put_face_normals")
      CHECK
   end subroutine

   subroutine put_nearest_external_atom_RGBs(self,out)
    ISOSURFACE :: self
   ! Put the nearest external atom isosurface distances, as RGB colours. "out"
   ! are the indices of the atoms outside the surface.
      INTVEC(:) :: out
      REALVEC(:), PTR :: dist
      REALMAT(:,:), PTR :: RGB
      REALVEC(2) :: val
   STACK("ISOSURFACE:put_nearest_external_atom_RGBs")
   START_TIMER("ISOSURFACE:put_nearest_external_atom_RGBs")
   ENSURE(associated(self%atom),"ISOSURFACE:put_nearest_external_atom_RGBs ... no atom data")
   ENSURE(associated(self%colour),"ISOSURFACE:put_nearest_external_atom_RGBs ... no colour function")
      call create_(RGB,3,self%n_pt)
      dist => nearest_atom_distances_(self,self%atom(out))
      call rescale_data_(self%colour,range_(dist))
      val = range_(dist)
    ! stdout.show("dist range low  =",val(1))
    ! stdout.show("dist range high =",val(2))
    ! .colour.put
      call get_RGB_for_(self%colour,dist,RGB)
      call destroy_(dist)
      call flush_(stdout)
      call show_(stdout,"begin vertex_colors ",self%n_pt)
      call put_(stdout,RGB,order="column")
      call text_(stdout,"end vertex_colors")
      call destroy_(RGB)
     STOP_TIMER("ISOSURFACE:put_nearest_external_atom_RGBs")
      CHECK
   end subroutine

   subroutine put_nearest_internal_atom_RGBs(self,in)
    ISOSURFACE :: self
   ! Put the nearest internal atom isosurface distances, as RGB colours. "in"
   ! are the indices of the atoms inside the surface.
      INTVEC(:) :: in
      REALVEC(:), PTR :: dist
      REALMAT(:,:), PTR :: RGB
      STACK("ISOSURFACE:put_nearest_internal_atom_RGBs")
      START_TIMER("ISOSURFACE:put_nearest_internal_atom_RGBs")
      ENSURE(associated(self%atom),"ISOSURFACE:put_nearest_internal_atom_RGBs ... no atom data")
      ENSURE(associated(self%colour),"ISOSURFACE:put_nearest_internal_atom_RGBs ... no colour function")
      call create_(RGB,3,self%n_pt)
      dist => nearest_atom_distances_(self,self%atom(in))
      call rescale_data_(self%colour,range_(dist))
      call get_RGB_for_(self%colour,dist,RGB)
      call destroy_(dist)
      call flush_(stdout)
      call show_(stdout,"begin vertex_colors ",self%n_pt)
      call put_(stdout,RGB,order="column")
      call text_(stdout,"end vertex_colors")
      call destroy_(RGB)
     STOP_TIMER("ISOSURFACE:put_nearest_internal_atom_RGBs")
      CHECK
   end subroutine

   subroutine put_binned_d_i_d_e_RGBs(self,in,out)
    ISOSURFACE :: self
   ! Put a binned representation of the surface, d_e vs. d_i, as RGBs. "in" and
   ! "out" are (respectively) the indices of the atoms inside and outside the
   ! surface.
      INTVEC(:) :: in,out
      REALVEC(:), PTR :: d_e,d_i
      INTMAT(:,:), PTR :: bin_count
      INTVEC(3) :: RGB255
      INT :: n_e,n_i,e,i
   STACK("ISOSURFACE:put_binned_d_i_d_e_RGBs")
   START_TIMER("ISOSURFACE:put_binned_d_i_d_e_RGBs")
   ENSURE(associated(self%atom),"ISOSURFACE:put_binned_d_i_d_e_RGBs ... no atom data")
   ENSURE(associated(self%colour),"ISOSURFACE:put_binned_d_i_d_e_RGBs ... no colour function")
      d_i => nearest_atom_distances_(self,self%atom(in))
      d_e => nearest_atom_distances_(self,self%atom(out))
      call bin_XY_data_(bin_count,d_i,d_e,0.2d0)
      call rescale_data_(self%colour,range_(bin_count))
      call destroy_(d_e)
      call destroy_(d_i)
      call flush_(stdout)
      n_i = size(bin_count,1)
      n_e = size(bin_count,2)
      call show_(stdout,"begin binned_d_i_d_e_colors ",trim(to_str_(n_i))//" "//trim(to_str_(n_e)))
      do i = 1,n_i
      do e = 1,n_e 
         call put_(stdout,i)
         call put_(stdout,e)
         RGB255 = RGB255_for_(self%colour,real(bin_count(i,e),kind=REAL_KIND))
         call put_(stdout,RGB255(1))
         call put_(stdout,RGB255(2))
         call put_(stdout,RGB255(3))
         call flush_(stdout)
      end do
      end do
      call text_(stdout,"end binned_d_i_d_e_colors")
      call destroy_(bin_count)
     STOP_TIMER("ISOSURFACE:put_binned_d_i_d_e_RGBs")
      CHECK
   end subroutine

   subroutine put_d_i_d_e_RGBs(self,in,out)
    ISOSURFACE :: self
   ! Put a binned representation of the surface, d_e vs. d_i, as RGBs.  "in" and
   ! "out" are (respectively) the indices of the atoms inside and outside the
   ! surface.  This differs from the put_binned_d_i_d_e_RGBs routine above in
   ! that the actual data values are outputted with the count next to them
   ! (converted to a RGB colour).
      INTVEC(:) :: in,out
      REALVEC(:), PTR :: d_e,d_i
      INTVEC(:), PTR :: bin_count
      INTVEC(3) :: RGB255
      REALVEC(2) :: range
      INT :: k
   STACK("ISOSURFACE:put_d_i_d_e_RGBs")
   START_TIMER("ISOSURFACE:put_d_i_d_e_RGBs")
   ENSURE(associated(self%atom),"ISOSURFACE:put_d_i_d_e_RGBs ... no atom data")
   ENSURE(associated(self%colour),"ISOSURFACE:put_d_i_d_e_RGBs ... no colour function")
      d_i => nearest_atom_distances_(self,self%atom(in))
      d_e => nearest_atom_distances_(self,self%atom(out))
      call create_(bin_count,self%n_pt)
      call bin_XY_data_(bin_count,d_i,d_e,0.2d0)
      range = range_(bin_count)
      call rescale_data_(self%colour,range)
      call flush_(stdout)
      call show_(stdout,"begin d_i_d_e_colors ",trim(to_str_(self%n_pt)))
      do k = 1,self%n_pt
         call put_(stdout,d_i(k))
         call put_(stdout,d_e(k))
         RGB255 = RGB255_for_(self%colour,real(bin_count(k),kind=REAL_KIND))
         call put_(stdout,RGB255(1))
         call put_(stdout,RGB255(2))
         call put_(stdout,RGB255(3))
         call flush_(stdout)
      end do
      call text_(stdout,"end d_i_d_e_colors")
      call destroy_(bin_count)
      call destroy_(d_e)
      call destroy_(d_i)
     STOP_TIMER("ISOSURFACE:put_d_i_d_e_RGBs")
      CHECK
   end subroutine

   function nearest_atom_distances(self,atom) result(res)
    ISOSURFACE :: self
   ! Put out the list of distances from each point on the isosurface to the
   ! nearest atom.
      ATOMVEC(:) :: atom
      REALVEC(:), PTR :: res
      REALMAT(:,:), PTR :: dist
      INT :: n_atom,i,a
      STACK("ISOSURFACE:nearest_atom_distances")
      START_TIMER("ISOSURFACE:nearest_atom_distances")
      ENSURE(associated(self%point),"ISOSURFACE:nearest_atom_distances ... no points")
      n_atom = n_atom_(atom)
      call create_(res,self%n_pt)
      call create_(dist,3,n_atom)
      do i = 1,self%n_pt
         do a = 1,n_atom
            dist(:,a) = atom(a)%pos - self%point(:,i) 
         end do
         res(i) = minval(column_norms_(dist)) 
      end do
      call destroy_(dist)
     STOP_TIMER("ISOSURFACE:nearest_atom_distances")
      UNSTACK
   end function

   subroutine put_vrml(self,out)
    ISOSURFACE :: self
   ! Put the isosurface data into VRML format to the file "out".
     TEXTFILE :: out
     REALVEC(3) :: n
     REALMAT(:,:), PTR :: RGB
     INT :: i
     STACK("ISOSURFACE:put_vrml")
     START_TIMER("ISOSURFACE:put_vrml")
     ENSURE(self%n_pt>0,"ISOSURFACE:put_vrml ... no isosurface points")
     call text_(stdout,"Generating VRML isosurface")
     call text_(out,"Shape {")
     call text_(out,"  appearance Appearance {")
     call text_(out,"    material Material {")
     call text_(out,"      diffuseColor 0.5 0.5 0.5")
     call text_(out,"      ambientIntensity 0.5")
     call text_(out,"      emissiveColor 0.1 0.1 0.1")
     call text_(out,"    }")
     call text_(out,"  }")
 
     call text_(out,"  geometry IndexedFaceSet {")
 
     ! Output the list of vertices.
     call text_(out,"    coord Coordinate {")
     call text_(out,"      point "//achar(91))
     do i=1,self%n_pt
       call put_(out,"      ")
       call put_(out,self%point(1,i))
       call put_(out,self%point(2,i))
       call put_(out,self%point(3,i))
       if (i==self%n_pt) then
         call put_(out," "//achar(93),flush=1)
       else
         call put_(out,",",flush=1)
       end if
     end do
     call text_(out,"    }")
 
     ! Output the list of vertices for each face.  Each face is ended with index
     ! -1, since in VRML we are not restricted to triangles.
     call text_(out,"    coordIndex "//achar(91))
     do i = 1,self%n_face
       call put_(out,"      ")
       call put_(out,self%face(1,i)-1)
       call put_(out,self%face(2,i)-1)
       call put_(out,self%face(3,i)-1)
       call put_(out,-1,flush=1)
     end do
     call text_(out,"    "//achar(93))
     call text_(out,"    solid FALSE")
     call text_(out,"    creaseAngle 2")
 
     ! Output the list of normals corresponding to the vertices.
     call text_(out,"    normal Normal {")
     call text_(out,"      vector "//achar(91))
     do i = 1,self%n_pt
       n = self%point_gradient(:,i)
       call normalise_(n)
       call put_(out,n(1))
       call put_(out,n(2))
       call put_(out,n(3))
       if (i==self%n_pt) then
         call put_(out," "//achar(93),flush=1)
       else
         call put_(out,",",flush=1)
       end if
     end do
     call text_(out,"    }")
 
     ! Output the colour of each vertex, if applicable.
     if (associated(self%surface_property_values)) then
       call create_(RGB,3,self%n_pt)
       call text_(stdout,"Scaling isosurface property values for colouring...")
       if (self%chop_surface_property_range) then
         call show_(stdout,"Min value used is ",self%surface_property_cutoff_range(1))
         call show_(stdout,"Max value used is ",self%surface_property_cutoff_range(2))
         call rescale_data_(self%colour,self%surface_property_cutoff_range)
       else
         call show_(stdout,"Min value used is ",minval(self%surface_property_values))
         call show_(stdout,"Max value used is ",maxval(self%surface_property_values))
         call rescale_data_(self%colour,range_(self%surface_property_values))
       end if
       call get_RGB_for_(self%colour,self%surface_property_values,RGB)
       ! We should not rescale colours for many properties!!!
       call text_(out,"    colorPerVertex TRUE")
       call text_(out,"    color Color {")
       call text_(out,"      color "//achar(91))
       do i = 1,self%n_pt
         call put_(out,RGB(1,i))
         call put_(out,RGB(2,i))
         call put_(out,RGB(3,i))
         if (i==self%n_pt) then
           call put_(out," "//achar(93),flush=1)
         else
           call put_(out,",",flush=1)
         end if
       end do
       call text_(out,"    }")
       call destroy_(RGB)
     end if
 
     call text_(out,"  }")
     call text_(out,"}")
     call text_(stdout,"done VRML isosurface")
     STOP_TIMER("ISOSURFACE:put_vrml")
      CHECK
   end subroutine

   subroutine put_CX(self,label)
    ISOSURFACE :: self
   ! Put the isosurface data in a form that the Crystal Explorer program can
   ! read it.
      STR(*) :: label
      STACK("ISOSURFACE:put_CX")
      START_TIMER("ISOSURFACE:put_CX")
      call flush_(stdout)
      call text_(stdout,"begin surface " // trim(label))
      call put_points_(self)
      call put_faces_(self)
      call put_vertex_normals_(self)
      call put_vertex_curvatures_(self)
      call text_(stdout,"end surface")
     STOP_TIMER("ISOSURFACE:put_CX")
      CHECK
   end subroutine

   subroutine put_CX_1(self,label,in,out)
    ISOSURFACE :: self
   ! Put the isosurface data in a form that the Crystal Explorer program can
   ! read it.  "in" and "out" are the indices of the atoms inside and outside
   ! the surface.
      STR(*) :: label
      INTVEC(:) :: in,out
   STACK("ISOSURFACE:put_CX_1")
   START_TIMER("ISOSURFACE:put_CX_1")
   ENSURE(associated(self%atom),"ISOSURFACE:put_CX_1 ... no atom list")
   ENSURE(maxval(in) <=size(self%atom),"ISOSURFACE:put_CX_1 ... in atoms out of range")
   ENSURE(maxval(out)<=size(self%atom),"ISOSURFACE:put_CX_1 ... out atoms out of range")
   ENSURE(minval(in) >0,"ISOSURFACE:put_CX_1 ... in atoms out of range")
   ENSURE(minval(out)>0,"ISOSURFACE:put_CX_1 ... out atoms out of range")
      call flush_(stdout)
      call text_(stdout,"begin surface " // trim(label))
      call put_points_(self)
      call put_faces_(self)
      call put_vertex_normals_(self)
      call put_vertex_curvatures_(self)
      call put_nearest_internal_atom_RGBs_(self,in)
      call put_nearest_external_atom_RGBs_(self,out)
      call put_binned_d_i_d_e_RGBs_(self,in,out)
      call put_d_i_d_e_RGBs_(self,in,out)
      call text_(stdout,"end surface")
     STOP_TIMER("ISOSURFACE:put_CX_1")
      CHECK
   end subroutine

!  **************
!  Test functions
!  **************

   subroutine test(self)
    ISOSURFACE :: self
   ! test the tesselate routine 
STACK("ISOSURFACE:test")
START_TIMER("ISOSURFACE:test")
#ifndef NOGENERIC
      call cubify_(self,test_func)
#else
      call cubify_(self,ISOSURFACE_test_func)
#endif
     STOP_TIMER("ISOSURFACE:test")
      CHECK
   end subroutine

   subroutine test_func(res,pt)
   ! this is a test isosurface
      REALMAT(:,:), IN :: pt
!      res :: REALVEC(pt.dim1) ! you can't mix assumed size with assumed shape
!                                in interface statements
      REALVEC(:), OUT :: res
      REALVEC(3) :: r
      INT :: n,i
      STACK("ISOSURFACE:test_func")
      START_TIMER("ISOSURFACE:test_func")
      n = size(pt,1)
      do i = 1,n
         r = pt(i,:)
         res(i) = r(1)**2 + r(2)**2 + r(3)**2
         res(i) = ONE/(max(res(i),TOL(4)))
      end do
     STOP_TIMER("ISOSURFACE:test_func")
      CHECK
   end subroutine

end
