!-------------------------------------------------------------------------------
!
! CLUSTER:
!
! An object to store information pertaining to a cluster of atoms or molecules
! formed from an underlying crystal structure.
!
! Note: an associated crystal and atom list should be supplied. It is intended
! that these will come from the molecule which generates the cluster (perhaps a
! cluster should contain a molecule?). These entities are not destroyed when the
! cluster is destroyed.
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
! $Id: cluster.foo,v 1.2.2.27 2003/11/13 05:34:39 reaper Exp $
!-------------------------------------------------------------------------------

module CLUSTER_MODULE

#  include "cluster.use"

   implicit none

#  include "macros"
#  include "cluster.int"


contains

   subroutine create(self)
    CLUSTER :: self
   ! Create the object
     PTR :: self
     STACK("CLUSTER:create")
     START_TIMER("CLUSTER:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(CLUSTER_SIZE)
     call nullify_ptr_part_(self)
     STOP_TIMER("CLUSTER:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,crystal,asymmetric_cell_atom)
    CLUSTER :: self
   ! Create the object IMPORTANT NOTE: the number of "asymmetric_cell_atoms"
   ! must match the second dimension of "crystal.asymmetric_unit_geometry" if
   ! both are present.  Furthermore, it is assumed that the kinds of
   ! "asymmetric_cell_atoms" match those positions in the asymmetric unit.
   ! Usually both of these entities will be obtained from a single read from a
   ! CIF file. If not, you must use the "create_from_molecule" method, below.
     PTR :: self
     CRYSTAL, PTR :: crystal
     ATOMVEC(:), PTR :: asymmetric_cell_atom
     STACK("CLUSTER:create_1")
     START_TIMER("CLUSTER:create_1")
     call create_(self)
     call set_defaults_(self,crystal,asymmetric_cell_atom)
     STOP_TIMER("CLUSTER:create_1")
      UNSTACK
   end subroutine

   subroutine create_from_molecule(self,crystal,cell_atom)
    CLUSTER :: self
   ! Create the object from a "crystal" and a "cell_atom" list. NOTE: The
   ! fragment geometry in the crystal must be consistent with the atom list
   ! positions, even though they are in different axis frames.
     PTR :: self
     CRYSTAL, PTR :: crystal
     ATOMVEC(:), PTR :: cell_atom
     ATOMVEC(:), PTR :: asymmetric_atom_list
     STACK("CLUSTER:create_from_molecule")
     START_TIMER("CLUSTER:create_from_molecule")
     ENSURE(associated(crystal),"CLUSTER:create_from_molecule ... no crystal information")
     ENSURE(crystal%n_fragment_atoms==size(cell_atom),"CLUSTER:create_from_molecule ... number of atoms inconsistent")
     call create_(self) ! <<<<
     call create_copy_(asymmetric_atom_list,cell_atom(crystal%unique_fragment_atom))
     call set_defaults_(self,crystal,asymmetric_atom_list) ! <<<<
     call nullify_basis_part_(asymmetric_atom_list)
     call nullify_coppensbasis_part_(asymmetric_atom_list)
     call destroy_(asymmetric_atom_list)
     STOP_TIMER("CLUSTER:create_from_molecule")
      UNSTACK
   end subroutine

   subroutine create_copy(self,object)
    CLUSTER :: self
   ! Create a copy of object
     CLUSTER :: object
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("CLUSTER:create_copy")
     START_TIMER("CLUSTER:create_copy")
     call create_(self)
     call copy_(self,object)
     STOP_TIMER("CLUSTER:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,cluster)
    CLUSTER :: self
   ! Copy the contents of "cluster" to self. NOTE: ensure you destroy all the
   ! parts you need to before calling this.
      CLUSTER, IN :: cluster
      STACK("CLUSTER:copy")
      START_TIMER("CLUSTER:copy")
      self = cluster
      if (associated(cluster%geometry)) &
         call create_copy_(self%geometry,cluster%geometry)
      if (associated(cluster%crystal)) &
         call create_copy_(self%crystal,cluster%crystal)
      if (associated(cluster%asymmetric_cell_atom)) &
         call create_copy_(self%asymmetric_cell_atom,cluster%asymmetric_cell_atom)
      if (associated(cluster%fragment_geometry)) &
         call create_copy_(self%fragment_geometry,cluster%fragment_geometry)
      if (associated(cluster%symop)) &
         call create_copy_(self%symop,cluster%symop)
      if (associated(cluster%symop_for_atom)) &
         call create_copy_(self%symop_for_atom,cluster%symop_for_atom)
      if (associated(cluster%parent_for_atom)) &
         call create_copy_(self%parent_for_atom,cluster%parent_for_atom)
      if (associated(cluster%atom_for_cell_atom)) &
         call create_copy_(self%atom_for_cell_atom,cluster%atom_for_cell_atom)
      if (associated(cluster%minimum_distance_to_atom)) &
         call create_copy_(self%minimum_distance_to_atom,cluster%minimum_distance_to_atom)
      if (associated(cluster%closest_fragment_atom_to_atom)) &
         call create_copy_(self%closest_fragment_atom_to_atom,cluster%closest_fragment_atom_to_atom)
      if (associated(cluster%is_fragment_atom)) &
         call create_copy_(self%is_fragment_atom,cluster%is_fragment_atom)
      if (associated(cluster%partition_factor)) &
         call create_copy_(self%partition_factor,cluster%partition_factor)
     STOP_TIMER("CLUSTER:copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CLUSTER :: self
   ! Destroy the object
      PTR :: self
      STACK("CLUSTER:destroy")
      START_TIMER("CLUSTER:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CLUSTER:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      deallocate(self)
      DELETE_MEMORY(CLUSTER_SIZE)
     STOP_TIMER("CLUSTER:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    CLUSTER :: self
   ! Nullify the pointer parts
      STACK("CLUSTER:nullify_ptr_part")
      START_TIMER("CLUSTER:nullify_ptr_part")
      nullify(self%crystal)
      nullify(self%asymmetric_cell_atom)
      nullify(self%symop)
      nullify(self%fragment_geometry)
      nullify(self%geometry)
      nullify(self%symop_for_atom)
      nullify(self%parent_for_atom)
      nullify(self%atom_for_cell_atom)
      nullify(self%minimum_distance_to_atom)
      nullify(self%closest_fragment_atom_to_atom)
      nullify(self%is_fragment_atom)
      nullify(self%partition_factor)
     STOP_TIMER("CLUSTER:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    CLUSTER :: self
   ! Destroy the pointer parts
      STACK("CLUSTER:destroy_ptr_part")
      START_TIMER("CLUSTER:destroy_ptr_part")
      call destroy_(self%crystal)   
      if (associated(self%asymmetric_cell_atom)) then
         call nullify_basis_part_(self%asymmetric_cell_atom)
         call nullify_coppensbasis_part_(self%asymmetric_cell_atom)
         call destroy_(self%asymmetric_cell_atom)
      end if
      call destroy_cell_geom_ptr_part_(self)
      call destroy_cluster_info_ptr_part_(self)
      call destroy_(self%partition_factor)
     STOP_TIMER("CLUSTER:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine destroy_cluster_info_ptr_part(self)
    CLUSTER :: self
   ! Destroy the non-symop informational ptr parts. These incclude the actual
   ! .geometry of the cluster as wellas information relating to the .symop's
   ! used to generate the cluster.
      STACK("CLUSTER:destroy_cluster_info_ptr_part")
      START_TIMER("CLUSTER:destroy_cluster_info_ptr_part")
      call destroy_(self%symop)
      call destroy_(self%geometry)
      call destroy_(self%symop_for_atom)
      call destroy_(self%parent_for_atom)
      call destroy_(self%atom_for_cell_atom)
      call destroy_(self%minimum_distance_to_atom)
      call destroy_(self%closest_fragment_atom_to_atom)
      call destroy_(self%is_fragment_atom)
     STOP_TIMER("CLUSTER:destroy_cluster_info_ptr_part")
      UNSTACK
   end subroutine

   subroutine destroy_cell_geom_ptr_part(self)
    CLUSTER :: self
   ! Destroy fragment geometry pointer parts. These are the geometrical
   ! coordinates used to generate the cluster.
      STACK("CLUSTER:destroy_cell_geom_ptr_part")
      START_TIMER("CLUSTER:destroy_cell_geom_ptr_part")
      call destroy_(self%fragment_geometry)
     STOP_TIMER("CLUSTER:destroy_cell_geom_ptr_part")
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

   subroutine set_defaults(self,crystal,asymmetric_cell_atom)
    CLUSTER :: self
   ! Set up defaults. IMPORTANT NOTE: the number of "asymmetric_cell_atoms" must
   ! match the second dimension of "crystal.asymmetric_unit_geometry" if both
   ! are present.  Furthermore, it is assumed that the kinds of
   ! "asymmetric_cell_atoms" match those positions in the asymmetric unit.
   ! Usually both of these entities will be obtained from a single read from a
   ! CIF file.
     CRYSTAL, PTR :: crystal
     ATOMVEC(:), PTR :: asymmetric_cell_atom
     STACK("CLUSTER:set_defaults")
     START_TIMER("CLUSTER:set_defaults")
     ENSURE(associated(asymmetric_cell_atom),"CLUSTER:set_defaults ... no asymmetric_cell_atom data")
     ENSURE(associated(crystal),"CLUSTER:set_defaults ... no crystal created!")
     ENSURE(associated(crystal%fragment_geometry),"CLUSTER:set_defaults ... no crystal fragment_geometry created")
     ENSURE(associated(crystal%asymmetric_unit_geometry),"CLUSTER:set_defaults ... no crystal asymmetric_unit_geometry created")
     ENSURE(size(crystal%asymmetric_unit_geometry,2)==size(asymmetric_cell_atom),"CLUSTER:set_defaults ... inconsistent # of atoms in asymmetric_cell_atom")
     nullify(self%asymmetric_cell_atom)
     nullify(self%crystal)
     call destroy_ptr_part_(self)
     self%add_criteria = CLUSTER_ADD_CRITERIA
     self%start_with_fragment = FALSE
     self%radius       = CLUSTER_RADIUS
     self%defragment   = CLUSTER_DEFRAGMENT
     self%info_made    = FALSE
     call create_copy_(self%asymmetric_cell_atom,asymmetric_cell_atom)
     call create_copy_(self%crystal,crystal)
     call set_crystal_defaults_(self,crystal)
     STOP_TIMER("CLUSTER:set_defaults")
      UNSTACK
   end subroutine

   subroutine set_crystal_defaults(self,crystal)
    CLUSTER :: self
   ! Set up the "crystal" defaults. The .fragment_geometry come from "crystal".
      CRYSTAL, PTR :: crystal
      STACK("CLUSTER:set_crystal_defaults")
      START_TIMER("CLUSTER:set_crystal_defaults")
      ENSURE(associated(crystal),"CLUSTER:set_crystal_defaults ... no crystal created!")
      ENSURE(associated(crystal%fragment_geometry),"CLUSTER:set_crystal_defaults ... no crystal fragment_geometry created")
      ENSURE(associated(crystal%asymmetric_unit_geometry),"CLUSTER:set_crystal_defaults ... no crystal asymmetric_unit_geometry created")
      call destroy_cluster_info_ptr_part_(self)
      self%n_symop = 0
      self%n_atoms = 0
      call destroy_(self%fragment_geometry)
      call create_copy_(self%fragment_geometry,crystal%fragment_geometry)
      self%n_fragment_atoms = size(crystal%fragment_geometry,2)
      self%fragment_width   = max_abs_column_difference_(crystal%fragment_geometry)
      self%fragment_offset  = mean_column_vector_(crystal%fragment_geometry)
     STOP_TIMER("CLUSTER:set_crystal_defaults")
      UNSTACK
   end subroutine

   subroutine set_add_criteria(self,criteria)
    CLUSTER :: self
   ! Set the add criteria, whether to add atoms by whole clusters within a
   ! certain radius of the starting fragment, or by individual atoms within a
   ! certain distance of the starting fragment.
      STR(STR_SIZE) :: criteria
      STACK("CLUSTER:set_add_criteria")
      START_TIMER("CLUSTER:set_add_criteria")
      self%add_criteria = criteria
      call to_lower_case_(self%add_criteria)
      select case (self%add_criteria)
         case("within_radius         ")
         case("unit_cell             ")
         case("fragment              "); self%start_with_fragment = TRUE
         case("unit_cell_and_fragment"); self%start_with_fragment = TRUE
         case default;   allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:set_add_criteria")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CLUSTER:set_add_criteria")
      CHECK
   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    CLUSTER :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("CLUSTER:read_keywords")
     START_TIMER("CLUSTER:read_keywords")
     ENSURE(next_item_(stdin)=="{","CLUSTER:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("CLUSTER:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    CLUSTER :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("CLUSTER:process_keyword")
      START_TIMER("CLUSTER:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                     ")  ! exit case
         case ("add_criteria=         "); call read_add_criteria_(self)
         case ("defragment=           "); call read_defragment_(self)
         case ("fragment_geometry=    "); call read_fragment_geometry_(self)
         case ("crystal_fragment=     "); call read_crystal_fragment_(self)
         case ("make_info             "); call make_info_(self)
         case ("partition_factors=    "); call read_partition_factors_(self)
         case ("put                   "); call put_(self)
         case ("put_tonto_input       "); call put_tonto_input_(self)
         case ("radius=               "); call read_radius_(self)
         case ("units=                "); call read_units_(self)
         case default;     allocate(tonto%known_keywords(11))
         tonto%known_keywords(1) = "}                     "
         tonto%known_keywords(2) = "add_criteria=         "
         tonto%known_keywords(3) = "defragment=           "
         tonto%known_keywords(4) = "fragment_geometry=    "
         tonto%known_keywords(5) = "crystal_fragment=     "
         tonto%known_keywords(6) = "make_info             "
         tonto%known_keywords(7) = "partition_factors=    "
         tonto%known_keywords(8) = "put                   "
         tonto%known_keywords(9) = "put_tonto_input       "
         tonto%known_keywords(10) = "radius=               "
         tonto%known_keywords(11) = "units=                "
         call unknown_(tonto,word,"CLUSTER:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CLUSTER:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    CLUSTER :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("CLUSTER:read_units")
      START_TIMER("CLUSTER:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("CLUSTER:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    CLUSTER :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("CLUSTER:read_junk")
      START_TIMER("CLUSTER:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("CLUSTER:read_junk")
      CHECK
   end subroutine

   subroutine read_add_criteria(self)
    CLUSTER :: self
   ! Read the add criteria, whether to add atoms by whole clusters within a
   ! certain radius of the starting fragment, or by individual atoms within a
   ! certain distance of the starting fragment.
      STACK("CLUSTER:read_add_criteria")
      START_TIMER("CLUSTER:read_add_criteria")
      call read_(stdin,self%add_criteria)
      call to_lower_case_(self%add_criteria)
      select case (self%add_criteria)
         case("within_radius         ")
         case("unit_cell             ")
         case("fragment              "); self%start_with_fragment = TRUE
         case("unit_cell_and_fragment"); self%start_with_fragment = TRUE
         case default;   allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:read_add_criteria")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CLUSTER:read_add_criteria")
      CHECK
   end subroutine

   subroutine read_defragment(self)
    CLUSTER :: self
   ! Read whether to defragment the cluster at the boundaries.
      STACK("CLUSTER:read_defragment")
      START_TIMER("CLUSTER:read_defragment")
      call read_(stdin,self%defragment)
    ! if (NOT .defragment AND .add_criteria=="unit_cell") then
    !    WARN("defragment= must be set TRUE when add_criteria= unit_cell")
    ! end
     STOP_TIMER("CLUSTER:read_defragment")
      CHECK
   end subroutine

   subroutine read_radius(self)
    CLUSTER :: self
   ! Read the radius of the cluster
      STACK("CLUSTER:read_radius")
      START_TIMER("CLUSTER:read_radius")
      call read_(stdin,self%radius)
     STOP_TIMER("CLUSTER:read_radius")
      CHECK
   end subroutine

   subroutine read_partition_factors(self)
    CLUSTER :: self
   ! Read the partition factors to be used. Note that the length of this array
   ! must correspond to the number of atoms in the generated cluster in order to
   ! be used. This cannot be checked at this point in the code.
      STACK("CLUSTER:read_partition_factors")
      START_TIMER("CLUSTER:read_partition_factors")
      call read_ptr_(stdin,self%partition_factor)
     STOP_TIMER("CLUSTER:read_partition_factors")
      UNSTACK
   end subroutine

   subroutine read_fragment_geometry(self)
    CLUSTER :: self
   ! Read in the fragment geometry, in cartesian atomic units, and convert to
   ! fractional coordinates.
   ! NOTE: to define the Hirshfeld surface, all fragment atom positions must
   ! correspond to the positions of actual atoms in the crystal lattice.
      REALVEC(:), PTR :: tmp
      STACK("CLUSTER:read_fragment_geometry")
      START_TIMER("CLUSTER:read_fragment_geometry")
      ENSURE(associated(self%crystal),"CLUSTER:read_fragment_geometry ... no crystal defined")
      call read_ptr_(stdin,tmp)
      ENSURE(mod(size(tmp),3)==0,"CLUSTER:read_fragment_geometry ... # of elements not divisible by 3")
      self%n_fragment_atoms = size(tmp)/3
      call destroy_(self%fragment_geometry)
      call create_(self%fragment_geometry,3,self%n_fragment_atoms)
      self%fragment_geometry = reshape(tmp,(/3,self%n_fragment_atoms/))
      call change_into_fractional_(self%crystal%unitcell,self%fragment_geometry)
      call destroy_(tmp)
     STOP_TIMER("CLUSTER:read_fragment_geometry")
      UNSTACK
   end subroutine

   subroutine read_crystal_fragment(self)
    CLUSTER :: self
   ! Read in the crystal fragment geometry, in fractional coordinate units.
   ! NOTE: to define the Hirshfeld surface, all fragment atom positions must
   ! correspond to the positions of actual atoms in the crystal lattice.
      REALVEC(:), PTR :: tmp
      STACK("CLUSTER:read_crystal_fragment")
      START_TIMER("CLUSTER:read_crystal_fragment")
      call read_ptr_(stdin,tmp)
      ENSURE(mod(size(tmp),3)==0,"CLUSTER:read_crystal_fragment ... # of elements not divisible by 3")
      self%n_fragment_atoms = size(tmp)/3
      call destroy_(self%fragment_geometry)
      call create_(self%fragment_geometry,3,self%n_fragment_atoms)
      self%fragment_geometry = reshape(tmp,(/3,self%n_fragment_atoms/))
      call destroy_(tmp)
     STOP_TIMER("CLUSTER:read_crystal_fragment")
      UNSTACK
   end subroutine

!  ***************************
!  Cluster generation routines
!  ***************************

   subroutine make_info(self)
    CLUSTER :: self
   ! Make all the cluster information from an arbitrary .fragment_geometry.
   ! NOTE: .set_defaults must be called before this.
      STACK("CLUSTER:make_info")
      START_TIMER("CLUSTER:make_info")
      ENSURE(associated(self%crystal),"CLUSTER:make_info ... no crystal")
      ENSURE(associated(self%crystal%cluster_symop),"CLUSTER:make_info ... no crystal cluster symops")
      ENSURE(associated(self%fragment_geometry),"CLUSTER:make_info ... no fragment geometry")
      if (NOT self%info_made) then
        call make_symop_list_and_geometry_(self)
        call find_fragment_atoms_(self)
        self%info_made = TRUE
      end if
     STOP_TIMER("CLUSTER:make_info")
      UNSTACK
   end subroutine

   subroutine make_symop_list_and_geometry(self)
    CLUSTER :: self
   ! Make the list of symops which transform a .crystal.unit_cell_geometry
   ! within a certain .radius of .fragment_geometry. Also make the geometry of
   ! the cluster and other informational arrays relating to the cluster. NOTE:
   ! the .fragment_geometry is shifted to the origin by an offset before any
   ! cluster generating operations are done, but it is put back afterwards.
     REALMAT(:,:), PTR :: big_cluster
     INTMAT(:,:), PTR :: symop_list
     BINVEC(:), PTR :: atom_added
     INTVEC(4) :: symop
     REALVEC(3) :: pos
     INT :: n_trial_atoms,n_cell_atoms,n_asym_atoms,i,n,c,a
     STACK("CLUSTER:make_symop_list_and_geometry")
     START_TIMER("CLUSTER:make_symop_list_and_geometry")
     ENSURE(associated(self%crystal),"CLUSTER:make_symop_list_and_geometry ... no crystal")
     ENSURE(associated(self%crystal%atom_for_unit_cell_atom),"CLUSTER:make_symop_list_and_geometry ... no crystal atom_for_unit_cell_atom info")
     ENSURE(associated(self%crystal%unit_cell_geometry),"CLUSTER:make_symop_list_and_geometry ... need the unit cell in the crystal")
     ENSURE(associated(self%fragment_geometry),"CLUSTER:make_symop_list_and_geometry ... no fragment geometry")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:make_symop_list_and_geometry ... no asymmetric_cell_atom info")
     ! The fragment is shifted by "offset" to the origin
     if (any(self%fragment_offset/=0)) then
     self%fragment_geometry = self%fragment_geometry &
                        - spread(self%fragment_offset,2,self%n_fragment_atoms)
     end if
     ! The algorithm creates "big_cluster", a cube of unit cells around the
     ! fragment.  We could use a sphere instead of a cube, but we were lazy.
     call make_big_cluster_(self,big_cluster,symop_list) 
     n_trial_atoms = size(big_cluster,2)
     ! For defragmenting, we must store which atoms in big_cluster added
     call create_(atom_added,n_trial_atoms) 
     atom_added = FALSE
     ! Initialise the arrays we really want ...
     call initialise_info_arrays_(self,n_trial_atoms)
     ! We may want to force the fragment to be part of the outputted cluster.
     n_cell_atoms = size(self%crystal%unit_cell_geometry,2)
     n_asym_atoms = size(self%asymmetric_cell_atom)
     if (self%start_with_fragment) then
       do i = 1, self%n_fragment_atoms
         pos = self%fragment_geometry(:,i)
         n = column_index_(big_cluster,pos)
         ENSURE(n>0,"CLUSTER:make_symop_list_and_geometry ... position of fragment atom not found in big_cluster")
         ENSURE(n<=n_trial_atoms,"CLUSTER:make_symop_list_and_geometry ... incorrect position of fragment atom")
         symop = symop_list(:,n)
         c = mod((n-1),n_cell_atoms) + 1         ! unit cell atom,
         a = self%crystal%atom_for_unit_cell_atom(c) ! asymmetric cell atom
         ENSURE(a>0,"CLUSTER:make_symop_list_and_geometry ... no unique atom for unit cell atom "//trim(to_str_(c)))
!        ENSURE(a<=n_asym_atoms,"asymmetric atom too large, cell atom "//c.to_str.trim)
         call add_new_atom_(self,pos,symop,a)
         atom_added(n) = TRUE
       end do
     end if
     ! Finally: extract the appropriate cluster from the big_cluster
     if (self%add_criteria/="fragment") then ! already done this above
       do i = 1,n_trial_atoms
         if (atom_added(i)) cycle
         pos = big_cluster(:,i)
         symop = symop_list(:,i)
         c = mod((i-1),n_cell_atoms) + 1         ! unit cell atom,
         a = self%crystal%atom_for_unit_cell_atom(c) ! asymmetric cell atom
         ENSURE(a>0,"CLUSTER:make_symop_list_and_geometry ... no unique atom for unit cell atom "//trim(to_str_(c)))
!        ENSURE(a<=n_asym_atoms,"asymmetric atom too large, cell atom "//c.to_str.trim)
         if (is_new_atom_(self,pos)) then
           call add_new_atom_(self,pos,symop,a)
           atom_added(i) = TRUE
         end if
       end do
     end if
     ! Add in bonded atoms if appropriate.
     if (self%defragment) call do_defragment_(self,big_cluster,symop_list,atom_added)
     ! Shift fragment and cluster back by "offset" to the origin
     if (any(self%fragment_offset/=0)) then
     self%fragment_geometry = self%fragment_geometry &
                           + spread(self%fragment_offset,2,self%n_fragment_atoms)
     self%geometry = self%geometry + spread(self%fragment_offset,2,self%n_atoms)
     end if
     ! Clean up a bit
     call shrink_info_arrays_(self)
     call destroy_(atom_added)
     call destroy_(big_cluster)
     call destroy_(symop_list)
     STOP_TIMER("CLUSTER:make_symop_list_and_geometry")
      UNSTACK
   end subroutine

   subroutine make_big_cluster(self,big_cluster,symop_list)
    CLUSTER :: self
   ! Make "big_cluster": the set of atom positions in unit cells around the
   ! central unit cell. Return also "symop_list", the list of seitz symmetry
   ! operations used to generate every atom in the "big_cluster".
     REALMAT(:,:), PTR :: big_cluster
     INTMAT(:,:), PTR :: symop_list
     REALVEC(3) :: max_dist,hkl
     REAL :: cell_a,cell_b,cell_c
     INT :: h_max,k_max,l_max,n_cells,n_cell_atoms,max_n_atoms
     INT :: first,last,i,h,h1,k,k1,l,l1,cell_atom,n_big_cluster
     STACK("CLUSTER:make_big_cluster")
     START_TIMER("CLUSTER:make_big_cluster")
     ENSURE(associated(self%crystal),"CLUSTER:make_big_cluster ... no crystal")
     ENSURE(associated(self%crystal%unit_cell_geometry),"CLUSTER:make_big_cluster ... need the unit cell in the crystal")
     ENSURE(associated(self%crystal%symop_for_unit_cell_atom),"CLUSTER:make_big_cluster ... no crystal symop_for_unit_cell_atom info")
     ! The maximum cell distances to search for cluster atoms
     max_dist = maximum_cell_axis_distance_(self)
     cell_a = self%crystal%unitcell%length(1)
     cell_b = self%crystal%unitcell%length(2)
     cell_c = self%crystal%unitcell%length(3)
     if (self%add_criteria=="unit_cell" AND NOT self%start_with_fragment AND NOT self%defragment) then
        h_max = 0
        k_max = 0
        l_max = 0
     else
        h_max = ceiling(max_dist(1)/cell_a) + 1
        k_max = ceiling(max_dist(2)/cell_b) + 1
        l_max = ceiling(max_dist(3)/cell_c) + 1
     end if
     n_cells = (2*h_max+1) * (2*k_max+1) * (2*l_max+1)
     n_cell_atoms = size(self%crystal%unit_cell_geometry,2)
     max_n_atoms = n_cell_atoms*n_cells
     ! The algorithm creates "big_cluster", a cube of unit cells around the
     ! origin.  We could use a sphere instead of a cube, but we were lazy.
     call create_(symop_list,4,max_n_atoms)
     call create_(big_cluster,3,max_n_atoms)
     big_cluster = ZERO
     ! Now loop over all lattice vectors consistent with max_dist to make
     ! "big_cluster".  Note the strange loop order is [0,0,0], [0,0,-1],
     ! [0,0,1], [0,0,-2], [0,0,2], [0,-1,0] etc.
     first = 1 ! Index of current atom.
     last  = n_cell_atoms
     do h1 = 0,2*h_max
       if (mod(h1,2)==0) then; h = h1 / 2
       else;                   h = - (h1 + 1) / 2
       end if
     ! k_max = ceiling((max_dist-abs(h)*cell_a) / cell_b)+1
       do k1 = 0,2*k_max
         if (mod(k1,2)==0) then; k = k1 / 2
         else;                   k = - (k1 + 1) / 2
         end if
       ! l_max = ceiling((max_dist-abs(h)*cell_a-abs(k)*cell_b) / cell_c)+1
         do l1 = 0,2*l_max
           if (mod(l1,2)==0) then; l = l1 / 2
           else;                   l = - (l1 + 1) / 2
           end if
           ! hkl is the displacement to shift the atoms.
           hkl = real((/h,k,l/),kind=REAL_KIND)
           ! The unit cell atom corresponding to this atom.
           cell_atom = 1
           do i = first,last
             big_cluster(:,i) = self%crystal%unit_cell_geometry(:,cell_atom) + hkl
             symop_list(:,i) = (/self%crystal%symop_for_unit_cell_atom(cell_atom),h,k,l/)
             cell_atom = cell_atom + 1
           end do
           first = first + n_cell_atoms
           last  = last  + n_cell_atoms 
         end do
       end do
     end do
     n_big_cluster = first - 1
     call shrink_columns_(big_cluster,n_big_cluster)
     call shrink_columns_(symop_list,n_big_cluster)
 ! stdout.text("symop_list:")
 ! stdout.put(transpose(symop_list))
     STOP_TIMER("CLUSTER:make_big_cluster")
      UNSTACK
   end subroutine

   function maximum_cell_axis_distance(self) result(max_dist)
    CLUSTER :: self
   ! Return the maximum distance that can be travelled along any single crystal
   ! axis direction in order that the minimum distance from the transformed
   ! fragment and the original fragment is less than the cluster radius.
     REALVEC(3) :: max_dist
     REALVEC(3) :: fragment_width
   ! fragment_length :: REAL
     STACK("CLUSTER:maximum_cell_axis_distance")
     START_TIMER("CLUSTER:maximum_cell_axis_distance")
     ENSURE(associated(self%crystal),"CLUSTER:maximum_cell_axis_distance ... no crystal")
     fragment_width = cartesian_fragment_width_(self%crystal)
   ! fragment_length = sqrt(dot_product(fragment_width,fragment_width))
     select case (self%add_criteria)   
       case("within_radius         "); max_dist = self%radius + fragment_width
       case("unit_cell             "); max_dist = ZERO
       case("fragment              "); max_dist = fragment_width
       case("unit_cell_and_fragment"); max_dist = fragment_width
       case default;                   allocate(tonto%known_keywords(4))       
       tonto%known_keywords(1) = "within_radius         "
       tonto%known_keywords(2) = "unit_cell             "
       tonto%known_keywords(3) = "fragment              "
       tonto%known_keywords(4) = "unit_cell_and_fragment"
       call unknown_(tonto,self%add_criteria,"CLUSTER:maximum_cell_axis_distance")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("CLUSTER:maximum_cell_axis_distance")
      CHECK
   end function

   subroutine initialise_info_arrays(self,max_n_atoms)
    CLUSTER :: self
   ! Initialise all informational arrays ready for a cluster .geometry
   ! generation from a general .fragment_geometry. 
     INT :: max_n_atoms
     STACK("CLUSTER:initialise_info_arrays")
     START_TIMER("CLUSTER:initialise_info_arrays")
     call destroy_cluster_info_ptr_part_(self)
     ! Initial maximum size for symop info arrays
     self%n_atoms = 0
     self%n_symop = 0
     call create_(self%symop,4,max_n_atoms)
   ! .atom_for_cell_atom.create(.crystal.n_asymmetric_unit_atoms,max_n_max)
   ! .atom_for_cell_atom = 0
     ! Initial maximum size for geometry info arrays
     call create_(self%geometry,3,max_n_atoms);      self%geometry = ZERO
     call create_(self%symop_for_atom,max_n_atoms);  self%symop_for_atom = 0
     call create_(self%parent_for_atom,max_n_atoms); self%parent_for_atom = 0
   ! .minimum_distance_to_atom.create(max_n_atoms)
   ! .closest_fragment_atom_to_atom.create(max_n_atoms)
     STOP_TIMER("CLUSTER:initialise_info_arrays")
      UNSTACK
   end subroutine

   subroutine do_defragment(self,big_cluster,symop_list,atom_added)
    CLUSTER :: self
   ! Defragment the generated cluster. This requires a surrounding "big_cluster"
   ! of atom positions, a "symop_list", the symmetry operation used to generate
   ! each atom in "big_cluster", and "atom_added", the list of atoms added to
   ! the cluster .geometry from "big_cluster", so far.
     REALMAT(:,:) :: big_cluster
     INTMAT(:,:) :: symop_list
     BINVEC(:) :: atom_added
     REALMAT(:,:), PTR :: big_cluster_xyz
     INTVEC(:), PTR :: out_atom
     BIN :: nearby
     ATOMVEC(2) :: atom_pair
     REALVEC(3) :: in_pos,out_pos
     INT :: i,in_atom,in_parent,j,out,c,a
     INT :: n_cell_atoms,n_asym_atoms,n_big_atoms,n_out_atoms
     STACK("CLUSTER:do_defragment")
     START_TIMER("CLUSTER:do_defragment")
     ENSURE(associated(self%crystal),"CLUSTER:do_defragment ... no crystal")
     ENSURE(associated(self%crystal%atom_for_unit_cell_atom),"CLUSTER:do_defragment ... no crystal atom_for_unit_cell_atom info")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:do_defragment ... no asymmetric_cell_atom info")
     ENSURE(size(symop_list,2)==size(big_cluster,2),"CLUSTER:do_defragment ... inconsistent symop_list array")
     ENSURE(size(atom_added) ==size(big_cluster,2),"CLUSTER:do_defragment ... inconsistent atom_added array")
     ENSURE(count(atom_added)==self%n_atoms,"CLUSTER:do_defragment ... wrong # of TRUE elements in atom_added array")
     ! Store the xyz positions to save time
     n_big_atoms = size(big_cluster,2)
     call create_(big_cluster_xyz,3,n_big_atoms)
     big_cluster_xyz = matmul(self%crystal%unitcell%direct_matrix,big_cluster)
     n_cell_atoms = size(self%crystal%unit_cell_geometry,2)
     n_asym_atoms = size(self%asymmetric_cell_atom)
     n_out_atoms = count(NOT atom_added)
     call create_(out_atom,n_out_atoms)
     out_atom = pack((/(i,i=1,n_big_atoms)/),mask=(NOT atom_added))
     ! Loop over atoms *inside* the cluster .....
     do in_atom = 1,n_out_atoms
       ! Worst case is to add one atom at a time, resulting in 
       ! n_big_atoms as the upper limit.
       if (in_atom > self%n_atoms) exit ! No more in_atoms were added
       in_parent = self%parent_for_atom(in_atom) ! Now we have the latest in_atom 
       in_pos = matmul(self%crystal%unitcell%direct_matrix,self%geometry(:,in_atom))
       atom_pair(1)%atomic_number = self%asymmetric_cell_atom(in_parent)%atomic_number
       atom_pair(1)%pos = in_pos
       ! Which are connected to in_atom? Loop over atoms (j) *outside* the inner
       ! cluster, that are in the big_cluster. Loop over the indices of the
       ! false parts of atom_added.
       j = 1
       do ! j = 1,n_out_atoms
         out = out_atom(j)
         out_pos = big_cluster_xyz(:,out)
         nearby = abs(out_pos(1)-in_pos(1))<SIX &
              AND abs(out_pos(2)-in_pos(2))<SIX &
              AND abs(out_pos(3)-in_pos(3))<SIX 
         if (NOT nearby) then
            j = j + 1
            if (j>n_out_atoms) exit
         else
            c = mod((out-1),n_cell_atoms) + 1       ! unit cell atom,
            a = self%crystal%atom_for_unit_cell_atom(c) ! asymmetric cell atom
            ENSURE(a>0,"CLUSTER:do_defragment ... no unique atom for unit cell atom "//trim(to_str_(c)))
            ENSURE(a<=n_asym_atoms,"CLUSTER:do_defragment ... asymmetric atom too large, cell atom "//trim(to_str_(c)))
            atom_pair(2)%atomic_number = self%asymmetric_cell_atom(a)%atomic_number
            atom_pair(2)%pos = out_pos
            if (bonded_(atom_pair,1,2)) then
              atom_added(out) = TRUE ! Add the new symop and atom
              call swap_elements_(out_atom,j,n_out_atoms)
              n_out_atoms = n_out_atoms - 1
              call add_new_atom_(self,big_cluster(:,out),symop_list(:,out),a) 
            else
               j = j + 1
            end if
            if (j>n_out_atoms) exit
         end if
       end do
     end do
     call destroy_(out_atom)
     call destroy_(big_cluster_xyz)
     STOP_TIMER("CLUSTER:do_defragment")
      CHECK
   end subroutine

   subroutine make_connection_table(self,table)
    CLUSTER :: self
   ! Make the connection "table" for the crystal unit cell geometry.
     INTVECVEC(:), PTR :: table
     REALMAT(:,:), PTR :: geometry,geometry_xyz
     ATOMVEC(2) :: atom_pair
     BINVEC(:), PTR :: atom_added
     REALVEC(3) :: pos_i,pos_j
     INT :: n_atoms,i,j,parent_i,parent_j,last_i
     BIN :: nearby
     STACK("CLUSTER:make_connection_table")
     START_TIMER("CLUSTER:make_connection_table")
     ENSURE(associated(self%crystal),"CLUSTER:make_connection_table ... no crystal")
     ENSURE(associated(self%crystal%unit_cell_geometry),"CLUSTER:make_connection_table ... no unit_cell in the crystal")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:make_connection_table ... no asymmetric_cell_atom info")
     geometry => self%crystal%unit_cell_geometry
     n_atoms = size(geometry,2)
     call create_(table,n_atoms) ! worst case is no atoms are connected
     call create_(geometry_xyz,3,n_atoms)
     geometry_xyz = matmul(self%crystal%unitcell%direct_matrix,geometry)
     call create_(atom_added,n_atoms)
     atom_added = FALSE
     do i = 1,n_atoms
        if (atom_added(i)) cycle
        pos_i = geometry_xyz(:,i)
        parent_i = self%crystal%atom_for_unit_cell_atom(i) 
        atom_pair(1)%atomic_number = self%asymmetric_cell_atom(parent_i)%atomic_number
        atom_pair(1)%pos = pos_i
        do j = 2,n_atoms
           pos_j = geometry_xyz(:,j)
           nearby = abs(pos_j(1)-pos_i(1))<SIX &
                AND abs(pos_j(2)-pos_i(2))<SIX &
                AND abs(pos_j(3)-pos_i(3))<SIX 
           if (NOT nearby) cycle
           parent_j = self%crystal%atom_for_unit_cell_atom(j) 
           atom_pair(2)%atomic_number = self%asymmetric_cell_atom(parent_j)%atomic_number
           atom_pair(2)%pos = pos_j
           if (bonded_(atom_pair,1,2)) then
              atom_added(j) = TRUE 
              call append_(table(i)%element,j)
              last_i = i
           end if
        end do
     end do
     call destroy_(geometry_xyz)
     call destroy_(atom_added)
     call shrink_(table,last_i)
 ! stdout.text("symop_list:")
 ! stdout.put(transpose(symop_list))
     STOP_TIMER("CLUSTER:make_connection_table")
      UNSTACK
   end subroutine

   subroutine shrink_info_arrays(self)
    CLUSTER :: self
   ! Shrink the informational arrays to save space.
     STACK("CLUSTER:shrink_info_arrays")
     START_TIMER("CLUSTER:shrink_info_arrays")
     call shrink_columns_(self%symop,self%n_symop) 
   ! .atom_for_cell_atom.shrink_columns(.n_symop) 
     call shrink_columns_(self%geometry,self%n_atoms) 
     call shrink_(self%symop_for_atom,self%n_atoms)
     call shrink_(self%parent_for_atom,self%n_atoms)
   ! .minimum_distance_to_atom.shrink(.n_atoms)
   ! .closest_fragment_atom_to_atom.shrink(.n_atoms)
     STOP_TIMER("CLUSTER:shrink_info_arrays")
      UNSTACK
   end subroutine

   function is_new_xyz_atom(self,pos,xyz) result(res)
    CLUSTER :: self
   ! Return TRUE only if "pos" (in crystal coordinates) is to be added to
   ! .geometry.  IMPORTANT NOTE: "xyz" is "pos" in *cartesian* coordiantes!!!
   ! This is to save some computation.
     REALVEC(3), IN :: pos,xyz
     BIN :: res
     STACK("CLUSTER:is_new_xyz_atom")
     START_TIMER("CLUSTER:is_new_xyz_atom")
     res = FALSE
     if (NOT has_column_(self%geometry(:,1:self%n_atoms),pos)) then
       select case (self%add_criteria)
         ! Add only those atoms proximate to the fragment
         case("within_radius         ")
                 res = minimum_distance2_to_xyz_(self,xyz)<=self%radius*self%radius
         ! Add only those atoms in the first unit cell
         case("unit_cell             ")
                 res = is_in_unit_cell_(self,pos)
         ! Add only those atoms in the initial fragment
         case("fragment              ")
                 res = minimum_distance2_to_xyz_(self,xyz)<= TOL(10)
         ! Add only those atoms in the first unit cell or initial fragment
         case("unit_cell_and_fragment")
                 res = minimum_distance2_to_xyz_(self,xyz)<= TOL(10) OR is_in_unit_cell_(self,pos)
         case default;    allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:is_new_xyz_atom")
         deallocate(tonto%known_keywords)
       end select
     end if
     STOP_TIMER("CLUSTER:is_new_xyz_atom")
      CHECK
   end function

   function is_new_atom(self,pos) result(res)
    CLUSTER :: self
   ! Return TRUE only if "pos" position (in crystal coordinates) is to be added
   ! to .geometry.
     REALVEC(3), IN :: pos
     BIN :: res
     STACK("CLUSTER:is_new_atom")
     START_TIMER("CLUSTER:is_new_atom")
     res = FALSE
     if (NOT has_column_(self%geometry(:,1:self%n_atoms),pos)) then
       select case (self%add_criteria)
         ! Add only those atoms proximate to the fragment
         case("within_radius         ")
                 res = minimum_distance_to_(self,pos)<=self%radius
         ! Add only those atoms in the first unit cell
         case("unit_cell             ")
                 res = is_in_unit_cell_(self,pos)
         ! Add only those atoms in the initial fragment
         case("fragment              ")
                 res = minimum_distance_to_(self,pos)<= TOL(10)
         ! Add only those atoms in the first unit cell or initial fragment
         case("unit_cell_and_fragment")
                 res = minimum_distance_to_(self,pos)<= TOL(10) OR is_in_unit_cell_(self,pos)
         case default;    allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "within_radius         "
         tonto%known_keywords(2) = "unit_cell             "
         tonto%known_keywords(3) = "fragment              "
         tonto%known_keywords(4) = "unit_cell_and_fragment"
         call unknown_(tonto,self%add_criteria,"CLUSTER:is_new_atom")
         deallocate(tonto%known_keywords)
       end select
     end if
     STOP_TIMER("CLUSTER:is_new_atom")
      CHECK
   end function

   subroutine add_new_atom(self,pos,symop,cell_atom)
    CLUSTER :: self
   ! Add a new atom with position "pos" into the cluster ".geometry" array, and
   ! update all the related information. "symop" is the symmetry operation that
   ! generated "pos". "cell_atom" is the index of the atom in the asymmetric
   ! unit cell fragment which is symmetrically equivalent to this atom with
   ! position "pos".
   ! NOTE: this routine should only be called if .any_new_atoms_in(pos) is TRUE.
     REALVEC(3) :: pos
     INTVEC(4) :: symop
     INT :: cell_atom
     INT :: q,n,n_col
     BIN :: disordered,symop_added
     REAL :: occ
     STACK("CLUSTER:add_new_atom")
     START_TIMER("CLUSTER:add_new_atom")
     if (has_column_(self%geometry(:,1:self%n_atoms),pos,eps=TOL(2))) then; STOP_TIMER("CLUSTER:add_new_atom") UNSTACK return; end if
     occ = self%asymmetric_cell_atom(cell_atom)%site_occupancy
     disordered = NOT same_as_(occ,ONE)
     WARN_IF(disordered,"CLUSTER:add_new_atom ... disordered atom "//trim(to_str_((self%n_atoms+1)))//", occ = "//trim(to_str_(occ)))
     symop_added = has_column_(self%symop,symop,col=q) ! get symop index "q" if there
     if (NOT symop_added) then    ! Add symop to .symop table if reqd.
        n_col = size(self%symop,2) 
        if ((self%n_symop+1)>n_col) then ! Expand .symop table if reqd.
           call expand_columns_(self%symop,2*n_col)
         ! .atom_for_cell_atom.expand_columns(2*n_col)
        end if
        q = self%n_symop + 1
        self%n_symop = q
        self%symop(:,q) = symop
     end if
     n_col = size(self%geometry,2)
     if ((self%n_atoms+1)>n_col) then ! Expand info arrays if reqd.
        call expand_columns_(self%geometry,2*n_col)
        call expand_(self%symop_for_atom,2*n_col)
        call expand_(self%parent_for_atom,2*n_col)
      ! .minimum_distance_to_atom.expand(2*n_col)
      ! .closest_fragment_atom_to_atom.expand(2*n_col)
     end if
     n = self%n_atoms + 1
     self%geometry(:,n) = pos
     self%symop_for_atom(n) = q
     self%parent_for_atom(n) = cell_atom
   ! .atom_for_cell_atom(cell_atom,q) = n
   ! These may take up too much time ...
   ! dist = .minimum_distance_to(pos,closest)
   ! .minimum_distance_to_atom(n) = dist
   ! .closest_fragment_atom_to_atom(n) = closest
     self%n_atoms = n
     STOP_TIMER("CLUSTER:add_new_atom")
      UNSTACK
   end subroutine

   function is_bonded_to(self,pos,p) result(res)
    CLUSTER :: self
   ! Return TRUE only if the atom with position "pos" and parent atom index "p"
   ! is bonded to one of the cluster atoms with positions in .geometry,
   ! according to a Bragg-Slater bond-distance criteria.
     REALVEC(3), IN :: pos
     INT, IN :: p
     BIN :: res
     INT :: a,pa
     ATOMVEC(2) :: atom_pair
     STACK("CLUSTER:is_bonded_to")
     START_TIMER("CLUSTER:is_bonded_to")
     ENSURE(associated(self%geometry),"CLUSTER:is_bonded_to ... no .geometry")
     ENSURE(self%n_atoms>0,"CLUSTER:is_bonded_to ... no atoms in .geometry")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:is_bonded_to ... no .cell_atom")
     ENSURE(associated(self%crystal),"CLUSTER:is_bonded_to ... no .asymmetric_cell_atom")
     res = FALSE
     do a = 1,self%n_atoms
        pa = self%parent_for_atom(a)
        atom_pair(1) = self%asymmetric_cell_atom(pa)
        atom_pair(2) = self%asymmetric_cell_atom(p)
        atom_pair(1)%pos = self%geometry(:,a)
        atom_pair(2)%pos = pos
        atom_pair%axis_system = "crystal"
        call convert_from_crystal_(atom_pair,self%crystal)
        res = bonded_(atom_pair,1,2)
        if (res) exit
     end do
     STOP_TIMER("CLUSTER:is_bonded_to")
      CHECK
   end function

   function is_in_unit_cell(self,pos) result(res)
    CLUSTER :: self
   ! Return TRUE only if the atom with position "pos" is in the first unit cell.
     REALVEC(3) :: pos
     BIN :: res
     REAL, parameter :: error = TOL(8)
     STACK("CLUSTER:is_in_unit_cell")
     START_TIMER("CLUSTER:is_in_unit_cell")
     res = all(pos(:)>=-error AND pos(:)<ONE-error)
     STOP_TIMER("CLUSTER:is_in_unit_cell")
      CHECK
   end function

   function is_near_origin(self,pos) result(res)
    CLUSTER :: self
   ! Return TRUE only if the atom with position "pos" is near the origin i.e. if
   ! corrdinates all have absolute value less than one.
     REALVEC(3) :: pos
     BIN :: res
     STACK("CLUSTER:is_near_origin")
     START_TIMER("CLUSTER:is_near_origin")
     res = &
        abs(pos(1))<=ONE AND &
        abs(pos(2))<=ONE AND &
        abs(pos(3))<=ONE 
     STOP_TIMER("CLUSTER:is_near_origin")
      CHECK
   end function

   subroutine find_fragment_atoms(self)
    CLUSTER :: self
   ! Find the fragment atoms in the .geometry of the cluster.
     REALVEC(3) :: new,frag
     INT :: i,j
     BIN :: found
     REAL :: tol
     STACK("CLUSTER:find_fragment_atoms")
     START_TIMER("CLUSTER:find_fragment_atoms")
     ENSURE(self%n_atoms>0,"CLUSTER:find_fragment_atoms ... there are no atoms in the cluster!")
     ENSURE(associated(self%geometry),"CLUSTER:find_fragment_atoms ... no geometry")
     ENSURE(associated(self%fragment_geometry),"CLUSTER:find_fragment_atoms ... no fragment geometry")
     ENSURE(associated(self%crystal),"CLUSTER:find_fragment_atoms ... no crystal")
     tol = TOL(3)/maxval(self%crystal%unitcell%length)
     call create_(self%is_fragment_atom,self%n_atoms)
     self%is_fragment_atom = FALSE
     do j = 1,self%n_fragment_atoms ! loop over fragment atoms
        frag = self%fragment_geometry(:,j)
        found = FALSE
        do i = 1,self%n_atoms
           new = self%geometry(:,i) ! cluster atom position
           if (same_as_(new,frag,tol)) then
              found = TRUE
              self%is_fragment_atom(i) = TRUE
              exit
           end if
        end do
        if (NOT found) then
         ! if (.add_criteria=="unit_cell") then
              WARN("CLUSTER:find_fragment_atoms ... fragment atom "//trim(to_str_(j))//" not found")
         ! else
         !    DIE("fragment atom "//trim(j.to_str)//" not found")
         ! end
        end if
     end do
     STOP_TIMER("CLUSTER:find_fragment_atoms")
      UNSTACK
   end subroutine

!  *******************
!  Cluster information
!  *******************

   function minimum_distance2_to_xyz(self,pos,closest_atom) result(res)
    CLUSTER :: self
   ! Return the minimum atom separation squared between the .fragment_geometry
   ! and a position "pos" given in cartesian coordinates. If present,
   ! "closest_atom" is set to the index of the atom (i.e. column) of
   ! .fragment_geometry whose position is closest to "pos".
     REALVEC(3) :: pos
     INT, optional :: closest_atom
     REAL :: res
     REALMAT(:,:), PTR :: diff
     REALVEC(:), PTR :: r2
     STACK("CLUSTER:minimum_distance2_to_xyz")
     START_TIMER("CLUSTER:minimum_distance2_to_xyz")
     ENSURE(associated(self%fragment_geometry),"CLUSTER:minimum_distance2_to_xyz ... no fragment geometry")
     ENSURE(associated(self%crystal),"CLUSTER:minimum_distance2_to_xyz ... no crystal data")
     call create_(diff,3,self%n_fragment_atoms)
     call create_(r2,self%n_fragment_atoms)
     diff = self%fragment_geometry - spread(pos,2,self%n_fragment_atoms)
     call get_column_dot_products_(diff,r2)
     res = minval(r2)
     if (present(closest_atom)) closest_atom = minval(minloc(r2))
     call destroy_(r2)
     call destroy_(diff)
     STOP_TIMER("CLUSTER:minimum_distance2_to_xyz")
      CHECK
   end function

   function minimum_distance_to(self,pos,closest_atom) result(res)
    CLUSTER :: self
   ! Return the minimum atom separation between the .fragment_geometry
   ! and a position "pos" given in crystal axis coordinates. If present,
   ! "closest_atom" is set to the index of the atom (i.e. column) of
   ! .fragment_geometry whose position is closest to "pos".
     REALVEC(3) :: pos
     INT, optional :: closest_atom
     REAL :: res
     REALMAT(:,:), PTR :: diff
     REALVEC(:), PTR :: norms
     STACK("CLUSTER:minimum_distance_to")
     START_TIMER("CLUSTER:minimum_distance_to")
     ENSURE(associated(self%fragment_geometry),"CLUSTER:minimum_distance_to ... no fragment geometry")
     ENSURE(associated(self%crystal),"CLUSTER:minimum_distance_to ... no crystal data")
     call create_(diff,3,self%n_fragment_atoms)
     call create_(norms,self%n_fragment_atoms)
     diff = self%fragment_geometry - spread(pos,2,self%n_fragment_atoms)
     call change_from_fractional_(self%crystal%unitcell,diff)  ! Put in cartesians.
     call get_column_norms_(diff,norms)
     res = minval(norms)
     if (present(closest_atom)) closest_atom = minval(minloc(norms))
     call destroy_(norms)
     call destroy_(diff)
     STOP_TIMER("CLUSTER:minimum_distance_to")
      CHECK
   end function

   function minimum_distance_to_1(self,pos) result(res)
    CLUSTER :: self
   ! Return the minimum atom separation between the .fragment_geometry
   ! and another geometry "pos" given in crystal axis coordinates.
     REALMAT(:,:) :: pos
     REAL :: res
     INT :: i
     STACK("CLUSTER:minimum_distance_to_1")
     START_TIMER("CLUSTER:minimum_distance_to_1")
     ENSURE(size(pos,2)>0,"CLUSTER:minimum_distance_to_1 ... no atom positions")
     res = minimum_distance_to_(self,pos(:,1))
     do i = 2,size(pos,2)
        res = min(minimum_distance_to_(self,pos(:,i)),res)
     end do
     STOP_TIMER("CLUSTER:minimum_distance_to_1")
      CHECK
   end function

   function cartesian_geometry(self) result(res)
    CLUSTER :: self
   ! Return the cartesian geometry for the cluster
     REALMAT(:,:), PTR :: res
     STACK("CLUSTER:cartesian_geometry")
     START_TIMER("CLUSTER:cartesian_geometry")
     ENSURE(associated(self%fragment_geometry),"CLUSTER:cartesian_geometry ... no fragment geometry")
     ENSURE(associated(self%crystal),"CLUSTER:cartesian_geometry ... no crystal data")
     call create_(res,3,self%n_atoms)
     call change_from_fractional_(self%crystal%unitcell,self%geometry)
     STOP_TIMER("CLUSTER:cartesian_geometry")
      UNSTACK
   end function

   function cluster_width(self) result(res)
    CLUSTER :: self
   ! Return the width "res" of the cluster in each of the 3 axis directions.
   ! NOTE: using crystal axis system.
      REALVEC(3) :: res
      STACK("CLUSTER:cluster_width")
      START_TIMER("CLUSTER:cluster_width")
      ENSURE(associated(self%geometry),"CLUSTER:cluster_width ... no fragment geometry")
      res = max_abs_column_difference_(self%geometry)
     STOP_TIMER("CLUSTER:cluster_width")
      CHECK
   end function

   subroutine make_fragment_atom(self,fragment_atom)
    CLUSTER :: self
   ! Make the list of fragment atoms, an ATOMVEC
     ATOMVEC(:) :: fragment_atom
     INT :: a,n,p
     STACK("CLUSTER:make_fragment_atom")
     START_TIMER("CLUSTER:make_fragment_atom")
     ENSURE(self%n_fragment_atoms>0,"CLUSTER:make_fragment_atom ... no fragment atoms")
     ENSURE(associated(self%is_fragment_atom),"CLUSTER:make_fragment_atom ... no fragment atoms")
     ENSURE(associated(self%crystal),"CLUSTER:make_fragment_atom ... no crystal info")
     n = 0
     do a = 1,self%n_atoms
         if (NOT self%is_fragment_atom(a)) cycle
         n = n + 1
         p = self%parent_for_atom(a)
         fragment_atom(n) = self%asymmetric_cell_atom(p)
         fragment_atom(n)%pos = self%geometry(:,a)
         fragment_atom(n)%axis_system = "crystal"
     end do
     call convert_from_crystal_(fragment_atom,self%crystal)
     STOP_TIMER("CLUSTER:make_fragment_atom")
      UNSTACK
   end subroutine

   function fragment_atom_indices(self) result(res)
    CLUSTER :: self
   ! Return the indices of the fragment atoms in the cluster.
     INTVEC(self%n_fragment_atoms) :: res
     INT :: a,n
     STACK("CLUSTER:fragment_atom_indices")
     START_TIMER("CLUSTER:fragment_atom_indices")
     ENSURE(self%n_fragment_atoms>0,"CLUSTER:fragment_atom_indices ... no fragment atoms")
     ENSURE(associated(self%is_fragment_atom),"CLUSTER:fragment_atom_indices ... no fragment atoms")
     n = 0
     do a = 1,self%n_atoms
         if (NOT self%is_fragment_atom(a)) cycle
         n = n + 1
         res(n) = a
     end do
     STOP_TIMER("CLUSTER:fragment_atom_indices")
      CHECK
   end function

   function nonfragment_atom_indices(self) result(res)
    CLUSTER :: self
   ! Return the indices of the nonfragment atoms in the cluster.
     INTVEC(self%n_atoms-self%n_fragment_atoms) :: res
     INT :: a,n
     STACK("CLUSTER:nonfragment_atom_indices")
     START_TIMER("CLUSTER:nonfragment_atom_indices")
     ENSURE(self%n_atoms>self%n_fragment_atoms,"CLUSTER:nonfragment_atom_indices ... no nonfragment atoms")
     ENSURE(associated(self%is_fragment_atom),"CLUSTER:nonfragment_atom_indices ... no fragment atoms")
     n = 0
     do a = 1,self%n_atoms
         if (self%is_fragment_atom(a)) cycle
         n = n + 1
         res(n) = a
     end do
     STOP_TIMER("CLUSTER:nonfragment_atom_indices")
      CHECK
   end function

!  ****************************************
!  Cluster transformations on matrices, etc
!  ****************************************

   subroutine make_partition_factors(self,matrix)
    CLUSTER :: self
   ! Make the partition factors from the cluster-fragment mapping information.
     REALMAT(:,:) :: matrix
     INT :: n_atom,a1,a2,f1,l1,f2,l2
     INTVEC(:), PTR :: first_basis_fn_for_atom,last_basis_fn_for_atom
     REAL :: factor
     STACK("CLUSTER:make_partition_factors")
     START_TIMER("CLUSTER:make_partition_factors")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:make_partition_factors ... no atom data")
     call make_atom_basis_fn_limits_(self%asymmetric_cell_atom,first_basis_fn_for_atom,last_basis_fn_for_atom)
     n_atom = n_atom_(self%asymmetric_cell_atom)
     do a1 = 1,n_atom
       f1 = first_basis_fn_for_atom(a1)
       l1 = last_basis_fn_for_atom(a1)
       do a2 = 1,n_atom
         f2 = first_basis_fn_for_atom(a2)
         l2 = last_basis_fn_for_atom(a2)
         ! Mulliken partitioning
         factor = (self%partition_factor(a1)+self%partition_factor(a2))/TWO
         matrix(f1:l1,f2:l2) = factor * matrix(f1:l1,f2:l2)
       end do
     end do
     call destroy_(last_basis_fn_for_atom)
     call destroy_(first_basis_fn_for_atom)
     STOP_TIMER("CLUSTER:make_partition_factors")
      UNSTACK
   end subroutine

   subroutine partition_density(self,matrix)
    CLUSTER :: self
   ! Applies atomic partition factors to the density matrix, useful for zeroing
   ! out certain atoms.
     REALMAT(:,:) :: matrix
     INT :: n_atom,a1,a2,f1,l1,f2,l2
     INTVEC(:), PTR :: first_basis_fn_for_atom,last_basis_fn_for_atom
     REAL :: factor
     STACK("CLUSTER:partition_density")
     START_TIMER("CLUSTER:partition_density")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:partition_density ... no atom data")
     call make_atom_basis_fn_limits_(self%asymmetric_cell_atom,first_basis_fn_for_atom,last_basis_fn_for_atom)
     n_atom = n_atom_(self%asymmetric_cell_atom)
     do a1 = 1,n_atom
       f1 = first_basis_fn_for_atom(a1)
       l1 = last_basis_fn_for_atom(a1)
       do a2 = 1,n_atom
         f2 = first_basis_fn_for_atom(a2)
         l2 = last_basis_fn_for_atom(a2)
         ! Mulliken partitioning
         factor = (self%partition_factor(a1)+self%partition_factor(a2))/TWO
         matrix(f1:l1,f2:l2) = factor * matrix(f1:l1,f2:l2)
       end do
     end do
     call destroy_(last_basis_fn_for_atom)
     call destroy_(first_basis_fn_for_atom)
     STOP_TIMER("CLUSTER:partition_density")
      UNSTACK
   end subroutine

   subroutine create_atom_list(self,atom)
    CLUSTER :: self
   ! Make a new atom list for the cluster
   ! NOTE: basis sets are pointer copied!
     ATOMVEC(:), PTR :: atom
     REALMAT3(:,:,:), PTR :: seitz
     REALMAT(3,3) :: therm
     INT :: a,p,s
     STACK("CLUSTER:create_atom_list")
     START_TIMER("CLUSTER:create_atom_list")
     ENSURE(associated(self%fragment_geometry),"CLUSTER:create_atom_list ... no crystal fragment geometry")
     ENSURE(associated(self%crystal),"CLUSTER:create_atom_list ... no crystal data")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:create_atom_list ... no atom data")
     ENSURE(self%n_atoms>0,"CLUSTER:create_atom_list ... no atoms in cluster")
     call create_(atom,self%n_atoms)
     seitz => transposed_xyz_seitz_matrices_(self%crystal) ! transposed !
     do a = 1,self%n_atoms
        p = self%parent_for_atom(a)            ! only one parent will do.
        s = self%symop_for_atom(a)             ! the symop from the parent.
        call copy_(atom(a),self%asymmetric_cell_atom(p))        ! make copy, but .basis is a ptr copy
        atom(a)%pos = self%geometry(:,a)       ! crystal axis system
        therm = self%asymmetric_cell_atom(p)%thermal_tensor
        call change_basis_(therm,self%crystal%unitcell%reciprocal_U_matrix)
        call change_basis_(therm,seitz(:,:,self%symop(1,s)))
        call change_basis_(therm,self%crystal%unitcell%direct_U_matrix)
        atom(a)%thermal_tensor = therm
        atom(a)%axis_system = "crystal"
     end do
     call destroy_(seitz)
     call resolve_axis_system_(atom,self%crystal)    ! change pos to cartesian
     STOP_TIMER("CLUSTER:create_atom_list")
      UNSTACK
   end subroutine

   subroutine make_density_matrix(self,P,D,atom)
    CLUSTER :: self
   ! Make a cluster density matrix "P" from a fragment density matrix "D", given
   ! a new cluster "atom" list (see routine create_atom_list).
     REALMAT(:,:) :: P,D
     ATOMVEC(:) :: atom
     REALMAT3(:,:,:), PTR :: ptr
     REALMAT3VEC(:), PTR :: tr
     REALMAT(:,:), PTR :: tr1,tr2,pc, W
     INT :: f1,l1,s1,n1,m1,a1,c1,f2,l2,s2,n2,m2,a2,c2
     INT :: n_shell, q,s,f3,l3,f4,l4
     INTVEC(:), PTR :: atom_for_shell, first,last, first_fn,last_fn
     STACK("CLUSTER:make_density_matrix")
     START_TIMER("CLUSTER:make_density_matrix")
     ENSURE(associated(self%crystal),"CLUSTER:make_density_matrix ... no crystal data")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:make_density_matrix ... no atom data")
     ENSURE(bases_are_all_labeled_(self%asymmetric_cell_atom),"CLUSTER:make_density_matrix ... unlabelled bases!")
     ENSURE(is_square_(D) AND size(D,1)==n_bf_(self%asymmetric_cell_atom),"CLUSTER:make_density_matrix ... D wrong shape")
     ENSURE(is_square_(P) AND size(P,1)==n_bf_(atom),"CLUSTER:make_density_matrix ... P wrong shape")
     ENSURE(associated(self%partition_factor),"CLUSTER:make_density_matrix ... no partition factors")
     n_shell = n_shell_(self%asymmetric_cell_atom)
     atom_for_shell => atom_for_shell_(self%asymmetric_cell_atom)
     call make_atom_basis_fn_limits_(self%asymmetric_cell_atom,first,last)
      call make_atom_basis_fn_limits_(atom,first_fn,last_fn)
     ptr => transposed_xyz_seitz_matrices_(self%crystal) ! transposes here
     call transpose_12_(ptr)
     call make_gaussian_xyz_matrices_(tr,ptr)
     call destroy_(ptr)
     pc => atom_pair_parent_count_(self)
     do q = 1,self%n_symop
       s = self%symop(1,q)
       ! Transform each shell pair by symop "s"
       do s1 = 1,n_shell
         f1 = first(s1); l1 = last(s1); n1 = (l1-f1+1)
         a1 = atom_for_shell(s1)
         c1 = self%atom_for_cell_atom(a1,q) ! cluster atom index
         f3 = first_fn(c1); l3 = last_fn(c1)
         m1 = inverse_triangle_number_(n1) - 1
         tr1  => tr(m1)%element(:,:,s)
         ! Transform 1st index of density matrix
         call create_(W,n1,n2)
         W = matmul(tr1,D(f1:l1,f2:l2))
         do s2 = 1, n_shell
           f2 = first(s2); l2 = last(s2); n2 = (l2-f2+2)
           a2 = atom_for_shell(s2)
           c2 = self%atom_for_cell_atom(a2,q) ! cluster atom index
           f4 = first_fn(c2); l4 = last_fn(c2)
           m2 = inverse_triangle_number_(n2) - 1
           ! Transform 2nd index of density matrix
           tr2  => tr(m2)%element(:,:,s)
           P(f3:l3,f4:l4) = P(f3:l3,f4:l4) &
                          + matmul(W,transpose(tr2))/pc(c1,c2)
         end do
         call destroy_(W)
       end do
     end do
     call destroy_(pc)
     call destroy_(last_fn); call destroy_(first_fn)
     call destroy_(last); call destroy_(first)
     call destroy_(tr)
     call destroy_(atom_for_shell)
     STOP_TIMER("CLUSTER:make_density_matrix")
      CHECK
   end subroutine

   function atom_pair_parent_count(self) result(n2)
    CLUSTER :: self
   ! Make the atom pair parent count, i.e. the number of times n2(i,j) a
   ! particular atom pair (i,j) is *generated from* a fragment atom pair (i',j')
   ! by the symmerty operations whose indices are stored in .symop. We divide
   ! by this factor to ensure that the pair effectively appears as being
   ! generated once, as an average of all the symmetry operations.  This is
   ! quite similar to the n2 factor in the Dacre-Elder-Dupuis-King symmetry
   ! method. For insight see:
   ! P.D. Dacre, CPL (1970) 7, 47
   ! M. Elder, IJQC (1973) 7, 75
   ! M. Dupuis and H.F> King, IJQC (1977) 11, 613
     REALMAT(:,:), PTR :: n2
     INT :: s1,a1,c1,s2,a2,c2
     INT :: n_shell,q
     INTVEC(:), PTR :: atom_for_shell
     STACK("CLUSTER:atom_pair_parent_count")
     START_TIMER("CLUSTER:atom_pair_parent_count")
     ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:atom_pair_parent_count ... no atom data")
     ENSURE(associated(self%atom_for_cell_atom),"CLUSTER:atom_pair_parent_count ... no atom_for_fragment_atom data")
     call create_(n2,self%n_atoms,self%n_atoms)
     n_shell = n_shell_(self%asymmetric_cell_atom)
     atom_for_shell => atom_for_shell_(self%asymmetric_cell_atom)
     do q = 1,self%n_symop
       ! Transform each shell pair by symop "s"
       do s1 = 1,n_shell
         a1 = atom_for_shell(s1)
         c1 = self%atom_for_cell_atom(a1,q)   ! cluster atom index
         do s2 = 1, n_shell
           a2 = atom_for_shell(s2)
           c2 = self%atom_for_cell_atom(a2,q) ! cluster atom index
           n2(c1,c2) = n2(c1,c2) + 1
         end do
       end do
     end do
     call destroy_(atom_for_shell)
     STOP_TIMER("CLUSTER:atom_pair_parent_count")
      UNSTACK
   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    CLUSTER :: self
   ! Put the list of vertices for the object
      STACK("CLUSTER:put")
      START_TIMER("CLUSTER:put")
      ENSURE(self%info_made,"CLUSTER:put ... call make_info first")
      call flush_(stdout)
      call text_(stdout,"Cluster information:")
      call flush_(stdout)
      call show_(stdout,"Radius                  =",self%radius)
      call show_(stdout,"Add criteria            =",self%add_criteria)
      call show_(stdout,"No. of atoms            =",self%n_atoms)
      call show_(stdout,"No. of fragment atoms   =",self%n_fragment_atoms)
      call show_(stdout,"No. of symops           =",self%n_symop)
      call show_(stdout,"Fragment width          =",self%fragment_width)
      call show_(stdout,"Cartesian width         =",cartesian_fragment_width_(self%crystal))
      call show_(stdout,"Fragment offset         =",self%fragment_offset)
      call put_cluster_table_(self,order_atoms_by="symop")
    ! .put_cluster_table(order_atoms_by="atom_distance")
     STOP_TIMER("CLUSTER:put")
      CHECK
   end subroutine

   subroutine put_cluster_table(self,order_atoms_by)
    CLUSTER :: self
   ! Put the cluster information table
      STR(*), optional :: order_atoms_by
      STR(STR_SIZE) :: order
      INT :: b,a,q
    ! i :: INT
      REAL :: dist
      INTVEC(:), PTR :: list
      STACK("CLUSTER:put_cluster_table")
      START_TIMER("CLUSTER:put_cluster_table")
      ENSURE(self%info_made,"CLUSTER:put_cluster_table ... call make_info first")
      order = "symop"
      if (present(order_atoms_by)) order = order_atoms_by
      call create_(list,self%n_atoms)
      if (order=="symop") then
         list = (/(a,a=1,self%n_atoms)/)
      else if (order=="atom_distance") then
         DIE("CLUSTER:put_cluster_table ... ordering not allowed any more, "//trim(order))
       ! .minimum_distance_to_atom.quick_sort(list)
      else
         DIE("CLUSTER:put_cluster_table ... unknown ordering, "//trim(order))
      end if
      call flush_(stdout)
      call text_(stdout,"Cluster geometry (crystal axis system):")
      call flush_(stdout)
      call text_(stdout,"Cluster atoms are ordered by "//trim(order))
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=4,width=12)
      call put_(stdout,"Atom",int_width=TRUE)
      call tab_(stdout,real_fields=3)
      call put_(stdout,"Closest",int_width=TRUE)
      call put_(stdout,"Minimum")
      call put_(stdout,"Parent",int_width=TRUE)
      call put_(stdout,"Parent",int_width=TRUE)
      call put_(stdout,"Symop",width=12)
      call flush_(stdout)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"atom",int_width=TRUE)
      call put_(stdout,"distance")
      call put_(stdout,"atom",int_width=TRUE)
      call put_(stdout,"symop",int_width=TRUE)
      call put_(stdout,"s",width=3)
      call put_(stdout,"h",width=3)
      call put_(stdout,"k",width=3)
      call put_(stdout,"l",width=3)
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=4,width=12)
      call flush_(stdout)
      do b = 1,self%n_atoms
         a = list(b)
         call put_(stdout,a)
         call put_(stdout,self%geometry(1,a))
         call put_(stdout,self%geometry(2,a))
         call put_(stdout,self%geometry(3,a))
       ! stdout.put(.closest_fragment_atom_to_atom(a))
         call put_(stdout,"n/a",int_width=TRUE)
       ! dist = .minimum_distance_to_atom(a)
         dist = -ONE
         if (dist<0) then; call put_(stdout,"n/a")
         else;             call put_(stdout,dist)
         end if
       ! do i = 1,size(.parent_for_atom(a).element)
       !    if (i>1) &
       !    stdout.tab(int_fields=4,real_fields=4)
            call put_(stdout,self%parent_for_atom(a))
            q = self%symop_for_atom(a)
            call put_(stdout,q)
            call put_(stdout,self%symop(1,q),width=3)
            call put_(stdout,self%symop(2,q),width=3)
            call put_(stdout,self%symop(3,q),width=3)
            call put_(stdout,self%symop(4,q),width=3)
            call flush_(stdout)
       ! end
      end do
      call dash_(stdout,int_fields=4,real_fields=4,width=12)
      call destroy_(list)
     STOP_TIMER("CLUSTER:put_cluster_table")
      CHECK
   end subroutine

   subroutine put_tonto_input(self)
    CLUSTER :: self
   ! Outputs the tonto input file for the cluster, given additionally the list
   ! of atoms which was used to generate the fragment_geometry in crystal.
   ! (See routine make_reduced_group_data).
      REALMAT3(:,:,:), PTR :: seitz
      REALMAT(3,3) :: therm
      INT :: a,p,s
      STACK("CLUSTER:put_tonto_input")
      START_TIMER("CLUSTER:put_tonto_input")
      ENSURE(self%info_made,"CLUSTER:put_tonto_input ... call make_info first")
      ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:put_tonto_input ... no atom data")
      ENSURE(associated(self%crystal),"CLUSTER:put_tonto_input ... no crystal data")
      ENSURE(associated(self%parent_for_atom),"CLUSTER:put_tonto_input ... no parent atoms")
      ENSURE(associated(self%symop_for_atom),"CLUSTER:put_tonto_input ... no symops for atoms")
      call text_(stdout,"   atoms= {")
      call flush_(stdout)
      call text_(stdout,"      keys= { label= ")
      call text_(stdout,'              "{ axis_system= crystal }" pos=')
!      stdout.text("              basis_label=")
      call text_(stdout,'              "{ units= angstrom^2 }" thermal_tensor= }')
      call flush_(stdout)
      call text_(stdout,"      data= {")
      seitz => transposed_xyz_seitz_matrices_(self%crystal) ! transposed !
      do a = 1,self%n_atoms
         p = self%parent_for_atom(a)
         s = self%symop_for_atom(a)
         call put_(stdout,trim(self%asymmetric_cell_atom(p)%label),int_width=TRUE)
         call put_(stdout,self%geometry(1,a))
         call put_(stdout,self%geometry(2,a))
         call put_(stdout,self%geometry(3,a))
!         stdout.put(.asymmetric_cell_atom(p).basis.label.trim)
         therm = self%asymmetric_cell_atom(p)%thermal_tensor
         call change_basis_(therm,self%crystal%unitcell%reciprocal_U_matrix)
         call change_basis_(therm,seitz(:,:,self%symop(1,s)))
         call change_basis_(therm,self%crystal%unitcell%direct_U_matrix)
         call convert_to_(therm,"angstrom^2")
         call put_(stdout,therm(1,1))
         call put_(stdout,therm(2,2))
         call put_(stdout,therm(3,3))
         call put_(stdout,therm(1,2))
         call put_(stdout,therm(1,3))
         call put_(stdout,therm(2,3))
         call flush_(stdout)
      end do
      call destroy_(seitz)
      call text_(stdout,"      }")
      call text_(stdout,"   }")
      call flush_(stdout)
     STOP_TIMER("CLUSTER:put_tonto_input")
      CHECK
   end subroutine

   subroutine put_CX(self,label)
    CLUSTER :: self
   ! Outputs some information for the Crystal Explorer program: the list of atoms in
   ! the cluster, their positions, and whether they are part of the generating
   ! fragment or not.
      STR(STR_SIZE) :: label
      INT :: n,p
      REALMAT(:,:), PTR :: geometry
      STACK("CLUSTER:put_CX")
      START_TIMER("CLUSTER:put_CX")
      ENSURE(self%info_made,"CLUSTER:put_CX ... call make_info first")
      ENSURE(associated(self%geometry),"CLUSTER:put_CX ... no cluster geometry")
      ENSURE(associated(self%crystal),"CLUSTER:put_CX ... no crystal data")
      ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:put_CX ... no atom data")
      ENSURE(associated(self%parent_for_atom),"CLUSTER:put_CX ... no parent atoms")
      ENSURE(associated(self%is_fragment_atom),"CLUSTER:put_CX ... no is_fragment_atom array")
      call create_copy_(geometry,self%geometry)
      call change_from_fractional_(self%crystal%unitcell,geometry)
      call flush_(stdout)
      call text_(stdout,"begin atoms " // trim(label))
      do n = 1,self%n_atoms
         p = self%parent_for_atom(n)
         call put_(stdout,chemical_symbol_(self%asymmetric_cell_atom(p)))
         call put_(stdout,geometry(1,n))
         call put_(stdout,geometry(2,n))
         call put_(stdout,geometry(3,n))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(1,1))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(1,2))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(1,3))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(2,2))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(2,3))
         call put_(stdout,self%asymmetric_cell_atom(p)%thermal_tensor(3,3))
         if (self%is_fragment_atom(n)) call put_text_(stdout," IN")
         call flush_(stdout)
      end do
      call text_(stdout,"end atoms")
      call destroy_(geometry)
     STOP_TIMER("CLUSTER:put_CX")
      CHECK
   end subroutine

   subroutine put_spartan(self,label)
    CLUSTER :: self
   ! Outputs some information for the Spartan program: the list of atoms in
   ! the cluster, their positions, and whether they are part of the generating
   ! fragment or not.
      STR(STR_SIZE) :: label
      STR(STR_SIZE) :: word
      INT :: n,p
      REALMAT(:,:), PTR :: geometry
   STACK("CLUSTER:put_spartan")
   START_TIMER("CLUSTER:put_spartan")
   ENSURE(self%info_made,"CLUSTER:put_spartan ... call make_info first")
   ENSURE(associated(self%geometry),"CLUSTER:put_spartan ... no cluster geometry")
   ENSURE(associated(self%crystal),"CLUSTER:put_spartan ... no crystal data")
   ENSURE(associated(self%asymmetric_cell_atom),"CLUSTER:put_spartan ... no atom data")
   ENSURE(associated(self%parent_for_atom),"CLUSTER:put_spartan ... no parent atoms")
   ENSURE(associated(self%is_fragment_atom),"CLUSTER:put_spartan ... no is_fragment_atom array")
      call create_copy_(geometry,self%geometry)
      call change_from_fractional_(self%crystal%unitcell,geometry)
      geometry = ANGSTROM_PER_BOHR*geometry
      call flush_(stdout)
      call text_(stdout,"=== SPARTAN DATA ===")
      call text_(stdout,trim(label))
      call text_(stdout,"M001")
      call text_(stdout,"0 1")
      do n = 1,self%n_atoms
         p = self%parent_for_atom(n)
         call put_(stdout,self%asymmetric_cell_atom(p)%atomic_number)
         call put_(stdout,geometry(1,n))
         call put_(stdout,geometry(2,n))
         call put_(stdout,geometry(3,n))
         call flush_(stdout)
      end do
      call text_(stdout,"ENDCART")
      call text_(stdout,"ATOMLABELS")
      do n = 1,self%n_atoms
         p = self%parent_for_atom(n)
         word = self%asymmetric_cell_atom(p)%label
         word = '"'//trim(word)//'"'
         call put_(stdout,trim(word))
         call flush_(stdout)
      end do
      call text_(stdout,"ENDATOMLABELS")
      call destroy_(geometry)
     STOP_TIMER("CLUSTER:put_spartan")
      CHECK
   end subroutine

end
