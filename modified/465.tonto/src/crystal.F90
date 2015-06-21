!-------------------------------------------------------------------------------
!
! CRYSTAL: Data structure for crystals
!
! $Id: crystal.foo,v 1.86.2.23 2004/04/21 09:12:54 reaper Exp $
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

module CRYSTAL_MODULE

#  include "crystal.use"

   use REALVEC_MODULE, only: minimise_BFGS

   implicit none

#  include "macros"
#  include "crystal.int"


   CRYSTAL, PTR :: saved_self

contains

!  **************************
!  Create and destroy methods
!  **************************

   subroutine create(self)
    CRYSTAL :: self
   ! Create an crystal object
      PTR :: self
      STACK("CRYSTAL:create")
      START_TIMER("CRYSTAL:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(CRYSTAL_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("CRYSTAL:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CRYSTAL :: self
   ! Destroy an crystal object
      PTR :: self
      STACK("CRYSTAL:destroy")
      START_TIMER("CRYSTAL:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CRYSTAL:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(CRYSTAL_SIZE)
      deallocate(self)
     STOP_TIMER("CRYSTAL:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    CRYSTAL :: self
   ! Nullify the pointer parts of the crystal object
     STACK("CRYSTAL:nullify_ptr_part")
     START_TIMER("CRYSTAL:nullify_ptr_part")
     call nullify_ptr_part_(self%spacegroup)
     nullify(self%fragment_geometry)
     nullify(self%fragment_cell_geometry)
     nullify(self%symop_for_fragment_cell_atom)
     nullify(self%atom_for_fragment_cell_atom)
     nullify(self%unique_fragment_atom)
     nullify(self%unique_atom_for_fragment_atom)
     nullify(self%unique_symop_for_fragment_atom)
     nullify(self%reduced_symop)
     nullify(self%inverted_symop)
     nullify(self%translated_symop)
     nullify(self%unique_SF_symop)
     nullify(self%repetition_factor)
     nullify(self%asymmetric_unit_geometry)
     nullify(self%unit_cell_geometry)
     nullify(self%symop_for_unit_cell_atom)
     nullify(self%atom_for_unit_cell_atom)
     nullify(self%reflections)
     STOP_TIMER("CRYSTAL:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    CRYSTAL :: self
   ! Erase all pointer information
    STACK("CRYSTAL:destroy_ptr_part")
    START_TIMER("CRYSTAL:destroy_ptr_part")
    call destroy_reflection_data_(self)
    call destroy_fragment_data_(self)
     STOP_TIMER("CRYSTAL:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine destroy_reflection_data(self)
    CRYSTAL :: self
   ! Erase all reflection data
     STACK("CRYSTAL:destroy_reflection_data")
     START_TIMER("CRYSTAL:destroy_reflection_data")
     if (reflection_data_exists_(self)) then
       self%scale_factor = ONE
       self%exp_scale_factor = ONE
       self%extinction_factor = ZERO
       call destroy_(self%reflections)
     end if
     STOP_TIMER("CRYSTAL:destroy_reflection_data")
      UNSTACK
   end subroutine

   subroutine destroy_fragment_data(self)
    CRYSTAL :: self
   ! Destroy the geometry and symmetry data for the fragment and unitcell.
     STACK("CRYSTAL:destroy_fragment_data")
     START_TIMER("CRYSTAL:destroy_fragment_data")
     call destroy_(self%fragment_geometry)
     call destroy_(self%fragment_cell_geometry)
     call destroy_(self%symop_for_fragment_cell_atom)
     call destroy_(self%atom_for_fragment_cell_atom)
     call destroy_(self%unique_fragment_atom)
     call destroy_(self%unique_atom_for_fragment_atom)
     call destroy_(self%unique_symop_for_fragment_atom)
     call destroy_(self%reduced_symop)
     call destroy_(self%inverted_symop)
     call destroy_(self%translated_symop)
     call destroy_(self%unique_SF_symop)
     call destroy_(self%repetition_factor)
     call destroy_(self%asymmetric_unit_geometry)
     call destroy_(self%unit_cell_geometry)
     call destroy_(self%symop_for_unit_cell_atom)
     call destroy_(self%atom_for_unit_cell_atom)
     STOP_TIMER("CRYSTAL:destroy_fragment_data")
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

   subroutine create_copy(self,crys)
    CRYSTAL :: self
   ! Create a copy of "crys"
     PTR :: self
     CRYSTAL, IN :: crys
     STACK("CRYSTAL:create_copy")
     START_TIMER("CRYSTAL:create_copy")
     call create_(self)
     call copy_(self,crys)
     STOP_TIMER("CRYSTAL:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,crystal)
    CRYSTAL :: self
   ! Set self to be crystal.
     CRYSTAL, IN :: crystal
     STACK("CRYSTAL:copy")
     START_TIMER("CRYSTAL:copy")
     self = crystal
     call nullify_ptr_part_(self)
     call copy_(self%spacegroup,crystal%spacegroup)
     call copy_(self%unitcell,crystal%unitcell)
     if (associated(crystal%fragment_geometry)) &
        call create_copy_(self%fragment_geometry,crystal%fragment_geometry)
     if (associated(crystal%fragment_cell_geometry)) &
        call create_copy_(self%fragment_cell_geometry,crystal%fragment_cell_geometry)
     if (associated(crystal%symop_for_fragment_cell_atom)) &
        call create_copy_(self%symop_for_fragment_cell_atom,crystal%symop_for_fragment_cell_atom)
     if (associated(crystal%atom_for_fragment_cell_atom)) &
        call create_copy_(self%atom_for_fragment_cell_atom,crystal%atom_for_fragment_cell_atom)
     if (associated(crystal%unique_fragment_atom)) &
        call create_copy_(self%unique_fragment_atom,crystal%unique_fragment_atom)
     if (associated(crystal%unique_atom_for_fragment_atom)) &
        call create_copy_(self%unique_atom_for_fragment_atom,crystal%unique_atom_for_fragment_atom)
     if (associated(crystal%unique_symop_for_fragment_atom)) &
        call create_copy_(self%unique_symop_for_fragment_atom,crystal%unique_symop_for_fragment_atom)
     if (associated(crystal%reduced_symop)) &
        call create_copy_(self%reduced_symop,crystal%reduced_symop)
     if (associated(crystal%cluster_symop)) &
        call create_copy_(self%cluster_symop,crystal%cluster_symop)
     if (associated(crystal%inverted_symop)) &
        call create_copy_(self%inverted_symop,crystal%inverted_symop)
     if (associated(crystal%translated_symop)) &
        call create_copy_(self%translated_symop,crystal%translated_symop)
     if (associated(crystal%unique_SF_symop)) &
        call create_copy_(self%unique_SF_symop,crystal%unique_SF_symop)
     if (associated(crystal%repetition_factor)) &
        call create_copy_(self%repetition_factor,crystal%repetition_factor)
     if (associated(crystal%asymmetric_unit_geometry)) &
        call create_copy_(self%asymmetric_unit_geometry,crystal%asymmetric_unit_geometry)
     if (associated(crystal%unit_cell_geometry)) &
        call create_copy_(self%unit_cell_geometry,crystal%unit_cell_geometry)
     if (associated(crystal%symop_for_unit_cell_atom)) &
        call create_copy_(self%symop_for_unit_cell_atom,crystal%symop_for_unit_cell_atom)
     if (associated(crystal%atom_for_unit_cell_atom)) &
        call create_copy_(self%atom_for_unit_cell_atom,crystal%atom_for_unit_cell_atom)
     if (associated(crystal%reflections)) &
        call create_copy_(self%reflections,crystal%reflections)
     STOP_TIMER("CRYSTAL:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    CRYSTAL :: self
   ! Set up a default crystal object
      STACK("CRYSTAL:set_defaults")
      START_TIMER("CRYSTAL:set_defaults")
      call set_defaults_(self%spacegroup)
      call set_defaults_(self%unitcell)
      call destroy_reflection_data_(self)
      self%synthesize_sigma_I     = CRYSTAL_SYNTHESISE_SIGMA_I
      self%optimise_scale         = CRYSTAL_OPTIMISE_SCALE
      self%optimise_extinction    = CRYSTAL_OPTIMISE_EXTINCTION
      self%correct_dispersion     = CRYSTAL_CORRECT_DISPERSION
      self%scale_factor           = CRYSTAL_SCALE_FACTOR
      self%exp_scale_factor       = CRYSTAL_EXP_SCALE_FACTOR
      self%extinction_factor      = CRYSTAL_EXTINCTION_FACTOR
      self%wavelength             = CRYSTAL_WAVELENGTH
      self%data_kind              = CRYSTAL_DATA_KIND
      self%thermal_smearing_model = CRYSTAL_THERMAL_SMEARING_MODEL
      self%partition_model        = CRYSTAL_PARTITION_MODEL
      self%reduced_group_info_made= FALSE
     STOP_TIMER("CRYSTAL:set_defaults")
      UNSTACK
   end subroutine

   subroutine update(self)
    CRYSTAL :: self
   ! Update the crystal information after setting values.
     STACK("CRYSTAL:update")
     START_TIMER("CRYSTAL:update")
     if (associated(self%reflections)) then
       if (have_F_exp_(self%reflections)) then
         call scale_F_exp_(self%reflections,self%exp_scale_factor)
       else
         self%optimise_scale = FALSE
         self%optimise_extinction = FALSE
       end if
       if (have_F_sigma_(self%reflections)) then
         call scale_F_sigma_(self%reflections,self%exp_scale_factor)
       else
         self%optimise_scale = FALSE
         self%optimise_extinction = FALSE
       end if
       self%exp_scale_factor = ONE
     end if
     STOP_TIMER("CRYSTAL:update")
      CHECK
   end subroutine

!  ************
!  Read methods
!  ************

   recursive subroutine read_keywords(self)
    CRYSTAL :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("CRYSTAL:read_keywords")
     START_TIMER("CRYSTAL:read_keywords")
     ENSURE(next_item_(stdin)=="{","CRYSTAL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("CRYSTAL:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    CRYSTAL :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*) :: keyword
     STR(STR_SIZE) :: word
     STACK("CRYSTAL:process_keyword")
     START_TIMER("CRYSTAL:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                          ")  ! exit read_loop
        case ("correct_dispersion=        "); call read_correct_dispersion_(self)
        case ("destroy_reflection_data    "); call destroy_reflection_data_(self)
        case ("destroy_spacegroup         "); call destroy_ptr_part_(self%spacegroup)
        case ("erase_reflection_data      "); call destroy_reflection_data_(self)
        case ("erase_spacegroup           "); call destroy_ptr_part_(self%spacegroup)
        case ("exp_scale_factor=          "); call read_exp_scale_factor_(self)
        case ("kind=                      "); call read_kind_(self)
        case ("make_f_predicted           "); call make_F_predicted_(self)
        case ("optimise_extinction=       "); call read_optimise_extinction_(self)
        case ("optimise_extinction_factor="); call read_optimise_extinction_(self)
        case ("optimize_extinction=       "); call read_optimise_extinction_(self)
        case ("optimize_extinction_factor="); call read_optimise_extinction_(self)
        case ("optimise_scale=            "); call read_optimise_scale_(self)
        case ("optimise_scale_factor=     "); call read_optimise_scale_(self)
        case ("optimize_scale=            "); call read_optimise_scale_(self)
        case ("optimize_scale_factor=     "); call read_optimise_scale_(self)
        case ("partition_model=           "); call read_partition_model_(self)
        case ("repetition_factors=        "); call read_repetition_factors_(self)
        case ("reflection_data=           "); call read_reflection_data_(self)
        case ("spacegroup=                "); call read_spacegroup_(self)
        case ("synthesize_sigma_i=        "); call read_synthesize_sigma_I_(self)
        case ("thermal_smearing_model=    "); call read_thermal_smearing_model_(self)
        case ("unitcell=                  "); call read_unitcell_(self)
        case ("wavelength=                "); call read_wavelength_(self)
        case ("put                        "); call put_(self)
        case ("put_stl                    "); call put_stl_(self)
        case ("read_cif                   "); call read_CIF_(self)
        case default;                       allocate(tonto%known_keywords(28))
        tonto%known_keywords(1) = "}                          "
        tonto%known_keywords(2) = "correct_dispersion=        "
        tonto%known_keywords(3) = "destroy_reflection_data    "
        tonto%known_keywords(4) = "destroy_spacegroup         "
        tonto%known_keywords(5) = "erase_reflection_data      "
        tonto%known_keywords(6) = "erase_spacegroup           "
        tonto%known_keywords(7) = "exp_scale_factor=          "
        tonto%known_keywords(8) = "kind=                      "
        tonto%known_keywords(9) = "make_f_predicted           "
        tonto%known_keywords(10) = "optimise_extinction=       "
        tonto%known_keywords(11) = "optimise_extinction_factor="
        tonto%known_keywords(12) = "optimize_extinction=       "
        tonto%known_keywords(13) = "optimize_extinction_factor="
        tonto%known_keywords(14) = "optimise_scale=            "
        tonto%known_keywords(15) = "optimise_scale_factor=     "
        tonto%known_keywords(16) = "optimize_scale=            "
        tonto%known_keywords(17) = "optimize_scale_factor=     "
        tonto%known_keywords(18) = "partition_model=           "
        tonto%known_keywords(19) = "repetition_factors=        "
        tonto%known_keywords(20) = "reflection_data=           "
        tonto%known_keywords(21) = "spacegroup=                "
        tonto%known_keywords(22) = "synthesize_sigma_i=        "
        tonto%known_keywords(23) = "thermal_smearing_model=    "
        tonto%known_keywords(24) = "unitcell=                  "
        tonto%known_keywords(25) = "wavelength=                "
        tonto%known_keywords(26) = "put                        "
        tonto%known_keywords(27) = "put_stl                    "
        tonto%known_keywords(28) = "read_cif                   "
        call unknown_(tonto,word,"CRYSTAL:process_keyword")
        deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CRYSTAL:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    CRYSTAL :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("CRYSTAL:read_units")
      START_TIMER("CRYSTAL:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("CRYSTAL:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    CRYSTAL :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("CRYSTAL:read_junk")
      START_TIMER("CRYSTAL:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("CRYSTAL:read_junk")
      CHECK
   end subroutine

   subroutine read_kind(self)
    CRYSTAL :: self
   ! Read the kind of crystal experiment
      STACK("CRYSTAL:read_kind")
      START_TIMER("CRYSTAL:read_kind")
      call read_(stdin,self%data_kind)
      call to_lower_case_(self%data_kind)
      select case (self%data_kind)
         case("x-ray")
         case("pnd  ")
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "x-ray"
         tonto%known_keywords(2) = "pnd  "
         call unknown_(tonto,self%data_kind,"CRYSTAL:read_kind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CRYSTAL:read_kind")
      CHECK
   end subroutine

   subroutine read_spacegroup(self)
    CRYSTAL :: self
   ! Read the spacegroup
      STACK("CRYSTAL:read_spacegroup")
      START_TIMER("CRYSTAL:read_spacegroup")
      call read_keywords_(self%spacegroup)
      call analyse_(self%spacegroup)
     STOP_TIMER("CRYSTAL:read_spacegroup")
      UNSTACK
   end subroutine

   subroutine read_unitcell(self)
    CRYSTAL :: self
   ! Read the unit cell information
      STACK("CRYSTAL:read_unitcell")
      START_TIMER("CRYSTAL:read_unitcell")
      call read_keywords_(self%unitcell)
      call make_info_(self%unitcell)
     STOP_TIMER("CRYSTAL:read_unitcell")
      CHECK
   end subroutine

   subroutine read_reflection_data(self)
    CRYSTAL :: self
   ! Read in data in the standard order from "stdin".
      STACK("CRYSTAL:read_reflection_data")
      START_TIMER("CRYSTAL:read_reflection_data")
      call read_list_keywords_(self%reflections)
     STOP_TIMER("CRYSTAL:read_reflection_data")
      UNSTACK
   end subroutine

   subroutine read_repetition_factors(self)
    CRYSTAL :: self
   ! Read in the crystal fragment repetition factors. Useful to get structure
   ! factor contributions from a small portion of the fragment.
     STACK("CRYSTAL:read_repetition_factors")
     START_TIMER("CRYSTAL:read_repetition_factors")
     call destroy_(self%repetition_factor)
     call read_ptr_(stdin,self%repetition_factor)
     STOP_TIMER("CRYSTAL:read_repetition_factors")
      UNSTACK
   end subroutine

   subroutine read_exp_scale_factor(self)
    CRYSTAL :: self
   ! Read the structure factor multiplier
      STACK("CRYSTAL:read_exp_scale_factor")
      START_TIMER("CRYSTAL:read_exp_scale_factor")
      call read_(stdin,self%exp_scale_factor)
     STOP_TIMER("CRYSTAL:read_exp_scale_factor")
      CHECK
   end subroutine

   subroutine read_wavelength(self)
    CRYSTAL :: self
   ! Read the experimental wavelength
      STACK("CRYSTAL:read_wavelength")
      START_TIMER("CRYSTAL:read_wavelength")
      call read_(stdin,self%wavelength)
     STOP_TIMER("CRYSTAL:read_wavelength")
      CHECK
   end subroutine

   subroutine read_optimise_scale(self)
    CRYSTAL :: self
   ! Read the switch whether to use and overall scale factor to minimise
   ! the chi2 statistic when calculating the structure factors.
   ! NOTE: this is not the same at the overall .scale_factor which is applied
   ! to the experimental structure factors.
      STACK("CRYSTAL:read_optimise_scale")
      START_TIMER("CRYSTAL:read_optimise_scale")
      call read_(stdin,self%optimise_scale)
     STOP_TIMER("CRYSTAL:read_optimise_scale")
      CHECK
   end subroutine

   subroutine read_synthesize_sigma_I(self)
    CRYSTAL :: self
   ! Read the switch whether to artificially create sigma(I) errors when
   ! evaluating the chi2 statistics based on intensities. Refer to routine
   ! .I_sigma
      STACK("CRYSTAL:read_synthesize_sigma_I")
      START_TIMER("CRYSTAL:read_synthesize_sigma_I")
      call read_(stdin,self%synthesize_sigma_I)
     STOP_TIMER("CRYSTAL:read_synthesize_sigma_I")
      CHECK
   end subroutine

   subroutine read_optimise_extinction(self)
    CRYSTAL :: self
   ! Read the switch whether to correct extinction or not, according to the
   ! Larson formula
      STACK("CRYSTAL:read_optimise_extinction")
      START_TIMER("CRYSTAL:read_optimise_extinction")
      call read_(stdin,self%optimise_extinction)
     STOP_TIMER("CRYSTAL:read_optimise_extinction")
      CHECK
   end subroutine

   subroutine read_correct_dispersion(self)
    CRYSTAL :: self
   ! Read the switch whether to correct dispersion or not, according to the
   ! atomic dispersion factors
      STACK("CRYSTAL:read_correct_dispersion")
      START_TIMER("CRYSTAL:read_correct_dispersion")
      call read_(stdin,self%correct_dispersion)
     STOP_TIMER("CRYSTAL:read_correct_dispersion")
      CHECK
   end subroutine

   subroutine read_thermal_smearing_model(self)
    CRYSTAL :: self
   ! Read the thermal smearing model to use to correct for thermal vibration
   ! in the calculated structure factors
      STACK("CRYSTAL:read_thermal_smearing_model")
      START_TIMER("CRYSTAL:read_thermal_smearing_model")
      call read_(stdin,self%thermal_smearing_model)
      call to_lower_case_(self%thermal_smearing_model)
      select case (self%thermal_smearing_model)
         case("       ")
         case("none   ")
         case("coppens")
         case("stewart")
         case("tanaka ")
         case default;    allocate(tonto%known_keywords(5))
         tonto%known_keywords(1) = "       "
         tonto%known_keywords(2) = "none   "
         tonto%known_keywords(3) = "coppens"
         tonto%known_keywords(4) = "stewart"
         tonto%known_keywords(5) = "tanaka "
         call unknown_(tonto,self%thermal_smearing_model,"CRYSTAL:read_thermal_smearing_model")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CRYSTAL:read_thermal_smearing_model")
      CHECK
   end subroutine

   subroutine read_partition_model(self)
    CRYSTAL :: self
   ! Read the partition model to used to correct for oversampled fragments
   ! of the unit cell when calculating the structure factors
      STACK("CRYSTAL:read_partition_model")
      START_TIMER("CRYSTAL:read_partition_model")
      call read_(stdin,self%partition_model)
      call to_lower_case_(self%partition_model)
      select case(self%partition_model)
         case("        ")
         case("none    ")
         case("mulliken")
         case("gaussian")
         case default;    allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "        "
         tonto%known_keywords(2) = "none    "
         tonto%known_keywords(3) = "mulliken"
         tonto%known_keywords(4) = "gaussian"
         call unknown_(tonto,self%partition_model,"CRYSTAL:read_partition_model")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("CRYSTAL:read_partition_model")
      CHECK
   end subroutine

   subroutine read_CIF(self)
    CRYSTAL :: self
   ! Read information from a Crystallographic Information File object, with
   ! the "name" taken from stdin.
      STR(STR_SIZE) :: name
      BIN :: found
      CIF, PTR :: cif
      STACK("CRYSTAL:read_CIF")
      START_TIMER("CRYSTAL:read_CIF")
      call read_(stdin,name)
      call create_(cif,name)
      call open_(cif)
      call find_crystal_data_block_(cif,found)
      ENSURE(found,"CRYSTAL:read_CIF ... no crsytal data block found") 
      call read_CIF_(self,cif)
      call destroy_(cif)
     STOP_TIMER("CRYSTAL:read_CIF")
      UNSTACK
   end subroutine

   subroutine read_CIF_1(self,cif)
    CRYSTAL :: self
   ! Read information from a Crystallographic Information File object, "cif"
      CIF :: cif
      STACK("CRYSTAL:read_CIF_1")
      START_TIMER("CRYSTAL:read_CIF_1")
      call set_defaults_(self)
      call read_CIF_(self%spacegroup,cif)
      call read_CIF_(self%unitcell,cif)
      call read_CIF_atoms_(self,cif)
      call update_(self)
     STOP_TIMER("CRYSTAL:read_CIF_1")
      UNSTACK
   end subroutine

   subroutine read_CIF_atoms(self,cif)
    CRYSTAL :: self
   ! Read atom information from a CIF file, "cif"
      CIF :: cif
      STR(STR_SIZE) :: ID
      BIN :: fs,fx,fy,fz
      STRVEC(STR_SIZE,:), PTR :: labels
      REALVEC(:), PTR :: x,y,z
      INT :: n
      STACK("CRYSTAL:read_CIF_atoms")
      START_TIMER("CRYSTAL:read_CIF_atoms")
      ID = "_atom_site_type_symbol"
      call find_looped_item_(cif,trim(ID),fs)
      if (NOT fs) then
      ID = "_atom_site_label"
      call find_looped_item_(cif,trim(ID),fs)
      end if
      call find_looped_item_(cif,"_atom_site_fract_x",fx)
      call find_looped_item_(cif,"_atom_site_fract_y",fy)
      call find_looped_item_(cif,"_atom_site_fract_z",fz)
      ENSURE(fs AND fx AND fy AND fz,"CRYSTAL:read_CIF_atoms ... incomplete atom information in CIF file")
      call read_looped_item_(cif,trim(ID),labels)
      call read_looped_item_(cif,"_atom_site_fract_x",x)
      call read_looped_item_(cif,"_atom_site_fract_y",y)
      call read_looped_item_(cif,"_atom_site_fract_z",z)
      ! Assign the CIF info
      n = size(labels)
      self%n_asymmetric_unit_atoms = n
      call destroy_(self%asymmetric_unit_geometry)
      call create_(self%asymmetric_unit_geometry,3,n)
      self%asymmetric_unit_geometry(1,:) = x
      self%asymmetric_unit_geometry(2,:) = y
      self%asymmetric_unit_geometry(3,:) = z
      call destroy_(z); call destroy_(y); call destroy_(x)
      call destroy_(labels)
     STOP_TIMER("CRYSTAL:read_CIF_atoms")
      UNSTACK
   end subroutine

!  ***************
!  General methods
!  ***************

   function lp_factor(self) result(res)
    CRYSTAL :: self
   ! Return the array of the Lorentz Polarization factors for all the
   ! reflections
     REALVEC(size(self%reflections)) :: res
     INT :: n
     REAL :: c,s,two_theta
     STACK("CRYSTAL:lp_factor")
     START_TIMER("CRYSTAL:lp_factor")
     ENSURE(associated(self%reflections),"CRYSTAL:lp_factor ... no reflection data")
     do n=1, n_refl_(self%reflections)
       two_theta = TWO*asin( stl_(self,n) * self%wavelength )
       c = cos(two_theta)
       s = sin(two_theta)
       if (is_zero_(s,TOL(8))) then
         res(n) = ZERO
         WARN("CRYSTAL:lp_factor ... lp_factor for (000) reflection set to zero")
       else
         res(n) = (1+c*c)/(TWO*s)
       end if
     end do
     STOP_TIMER("CRYSTAL:lp_factor")
      CHECK
   end function

   function I_pred(self) result(res)
    CRYSTAL :: self
   ! Return the array of predicted Intensities. Only the Lorentz Polarization
   ! factor and the angular velocity factor for a single crystal are used.
   ! Fundamental constants appearing in front of this are not calculated
   ! NOTE: unlike I_exp, these may include extinction and dispersion effects.
     REALVEC(size(self%reflections)) :: res
     INT :: n
     REAL :: c,s,two_theta,F2
     STACK("CRYSTAL:I_pred")
     START_TIMER("CRYSTAL:I_pred")
     ENSURE(associated(self%reflections),"CRYSTAL:I_pred ... no reflection data")
     ENSURE(have_F_exp_(self%reflections),"CRYSTAL:I_pred ... no calculated structure factors")
     do n=1, n_refl_(self%reflections)
       two_theta = TWO*asin( stl_(self,n) * self%wavelength )
       c = cos(two_theta)
       s = sin(two_theta)
       F2 = self%reflections(n)%F_pred
       F2 = F2*F2
       if (is_zero_(s,TOL(8))) then
         res(n) = F2
       else
         res(n) = (1+c*c)/(TWO*s)*F2
       end if
     end do
     STOP_TIMER("CRYSTAL:I_pred")
      CHECK
   end function

   function I_exp(self) result(res)
    CRYSTAL :: self
   ! Return the array of experimental Intensities. Only the Lorentz Polarization
   ! factor and the angular velocity factor for a single crystal are used.
   ! Fundamental constants appearing in front of this are not calculated
   ! NOTE: extinction factors, dispersion, multiple scattering corrections
   ! are not included. These are the experimental intensities with these effects
   ! removed.
     REALVEC(size(self%reflections)) :: res
     INT :: n
     REAL :: c,s,two_theta,F2
     STACK("CRYSTAL:I_exp")
     START_TIMER("CRYSTAL:I_exp")
     ENSURE(associated(self%reflections),"CRYSTAL:I_exp ... no reflection data")
     ENSURE(have_F_exp_(self%reflections),"CRYSTAL:I_exp ... no calculated structure factors")
     do n=1, n_refl_(self%reflections)
       two_theta = TWO*asin( stl_(self,n) * self%wavelength )
       c = cos(two_theta)
       s = sin(two_theta)
       F2 = abs(self%reflections(n)%F_exp)
       F2 = F2*F2
       if (is_zero_(s,TOL(8))) then
          res(n) = F2
       else
          res(n) = (1+c*c)/(TWO*s)*F2
       end if
     end do
     STOP_TIMER("CRYSTAL:I_exp")
      CHECK
   end function

   function I_sigma(self) result(res)
    CRYSTAL :: self
   ! Return the array of experimental sigma's in the Intensities.
     REALVEC(size(self%reflections)) :: res
     INT :: n
     REAL :: c,s,two_theta,F2
     STACK("CRYSTAL:I_sigma")
     START_TIMER("CRYSTAL:I_sigma")
     ENSURE(associated(self%reflections),"CRYSTAL:I_sigma ... no reflection data")
   ENSURE(have_I_pred_(self%reflections) OR have_F_exp_(self%reflections),"CRYSTAL:I_sigma ... no structure factors")
     if (NOT have_F_exp_(self%reflections) AND NOT self%synthesize_sigma_I) then
       WARN("CRYSTAL:I_sigma ... The synthesize_sigma_I flag was not set; it is now set")
       self%synthesize_sigma_I = TRUE
     end if
     if (self%synthesize_sigma_I) then
       res = self%reflections%I_pred
       res = sqrt(res/equivalence_factors_(self))
     else
       do n=1, n_refl_(self%reflections)
         two_theta = TWO*asin( stl_(self,n) * self%wavelength )
         c = cos(two_theta)
         s = sin(two_theta)
         F2 = abs(self%reflections(n)%F_exp)
         if (is_zero_(s,TOL(8))) then
           res(n) = F2
         else
           res(n) = TWO*(1+c*c)/(TWO*s)*F2
         end if
       end do
     end if
     STOP_TIMER("CRYSTAL:I_sigma")
      CHECK
   end function

   function equivalence_factors(self) result(res)
    CRYSTAL :: self
   ! Return the equivalence factors, the number of distinct reflections
   ! which are symmetry equivalent to a particular (hkl) triple, for all
   ! the reflections.
      INTVEC(size(self%reflections)) :: res
      INTVEC(3) :: hkl,new
      INT :: n,s,u,n_refl
      STACK("CRYSTAL:equivalence_factors")
      START_TIMER("CRYSTAL:equivalence_factors")
      ENSURE(associated(self%reflections),"CRYSTAL:equivalence_factors ... no reflection data")
      n_refl = n_refl_(self%reflections)
      do n = 1,n_refl
         hkl = indices_(self%reflections,n)
         u = 1
         do s = 2,self%spacegroup%n_seitz
            new = matmul(hkl,self%spacegroup%seitz(1:3,1:3,s))
            if (hkl(1)/=new(1) OR hkl(2)/=new(2) OR hkl(3)/=new(3)) u = u + 1
         end do
         res(n) = u
      end do
     STOP_TIMER("CRYSTAL:equivalence_factors")
      CHECK
   end function

   function stl(self,n) result(res)
    CRYSTAL :: self
   ! Return the value of sin(theta) / lambda for reflection n
     IN :: self
     INT, IN :: n
     REAL :: res
     INTVEC(3) :: hkl
     REAL :: kx,ky,kz
     STACK("CRYSTAL:stl")
     START_TIMER("CRYSTAL:stl")
     hkl = indices_(self%reflections,n)
     kx = dot_product(self%unitcell%reciprocal_matrix(1,:),hkl(:))
     ky = dot_product(self%unitcell%reciprocal_matrix(2,:),hkl(:))
     kz = dot_product(self%unitcell%reciprocal_matrix(3,:),hkl(:))
     res = HALF*sqrt(kx*kx+ky*ky+kz*kz)
     STOP_TIMER("CRYSTAL:stl")
      CHECK
   end function

   subroutine make_F_predicted(self)
    CRYSTAL :: self
   ! Make the predicted magnitude of the structure factors, including possibly
   ! an overall scale factor and extinction correction.
     REALVEC(:), PTR :: F_pred
     INOUT :: self
     STACK("CRYSTAL:make_F_predicted")
     START_TIMER("CRYSTAL:make_F_predicted")
     DIE_IF(NOT associated(self%reflections),"CRYSTAL:make_F_predicted ... no reflection data")
     self%n_param = 0
     if (self%optimise_extinction OR self%optimise_scale) call get_optimum_parameters_(self)
     call create_(F_pred,n_refl_(self%reflections))
     if (self%data_kind=="pnd") then
       F_pred = self%reflections%F_calc * extinction_correction_(self)
     else
       F_pred = abs(self%reflections%F_calc) * extinction_correction_(self)
     end if
     call set_F_pred_(self%reflections,F_pred)
     call destroy_(F_pred)
     STOP_TIMER("CRYSTAL:make_F_predicted")
      CHECK
   end subroutine

   subroutine get_optimum_parameters(self)
    CRYSTAL :: self
   ! Get the scale factors, extinction parameters, etc, which minimise the chi2.
   ! (To get the corrections cooresponding to these parameters see routine
   ! .extinction_correction)
     STACK("CRYSTAL:get_optimum_parameters")
     START_TIMER("CRYSTAL:get_optimum_parameters")
     if (self%optimise_extinction) then
       call optimise_extinction_factor_(self)
     else if (self%optimise_scale) then
       call optimise_scale_factor_(self)
     end if
     STOP_TIMER("CRYSTAL:get_optimum_parameters")
      CHECK
   end subroutine

   function extinction_correction(self) result(res)
    CRYSTAL :: self
   ! Return the extinction correction  factors "res" to the calculated
   ! individual structure factors.
     REALVEC(size(self%reflections)) :: res
     STACK("CRYSTAL:extinction_correction")
     START_TIMER("CRYSTAL:extinction_correction")
     res = extinction_correction_(self,self%scale_factor,self%extinction_factor)
     STOP_TIMER("CRYSTAL:extinction_correction")
      CHECK
   end function

   function extinction_correction_1(self,scale_factor,extinction_factor) result(res)
    CRYSTAL :: self
   ! Return the extinction correction  factors "res" to the calculated
   ! individual structure factors. NOTE: this routine also does scaling
   ! corrections without extinction.
     REAL :: scale_factor,extinction_factor
     REALVEC(size(self%reflections)) :: res
     CPX :: F_calc
     REALVEC(:), PTR :: angle_part
     INT :: n,n_refl
     STACK("CRYSTAL:extinction_correction_1")
     START_TIMER("CRYSTAL:extinction_correction_1")
     n_refl = n_refl_(self%reflections)
     ENSURE(associated(self%reflections),"CRYSTAL:extinction_correction_1 ... no reflection data")
     ENSURE(have_F_calc_(self%reflections),"CRYSTAL:extinction_correction_1 ... no calculated structure factors")
     if (is_zero_(extinction_factor,TOL(9))) then
        res(:) = scale_factor
     else
       call create_(angle_part,n_refl)
       angle_part = extinction_angle_part_(self)
       do n=1, n_refl
         F_calc = self%reflections(n)%F_calc
         res(n) = scale_factor / sqrt(sqrt(ONE + &
             extinction_factor*F_calc*conjg(F_calc)*angle_part(n)))
       end do
       call destroy_(angle_part)
     end if
     STOP_TIMER("CRYSTAL:extinction_correction_1")
      CHECK
   end function

   function extinction_angle_part(self) result(res)
    CRYSTAL :: self
   ! Return the angular part of the extinction correction.
     REALVEC(size(self%reflections)) :: res
      INT :: n
     REAL :: twotheta,c,s
     STACK("CRYSTAL:extinction_angle_part")
     START_TIMER("CRYSTAL:extinction_angle_part")
     do n=1, n_refl_(self%reflections)
       twotheta=TWO*asin( stl_(self,n) * self%wavelength )
       c = cos(twotheta)
       s = sin(twotheta)
       res(n) = (1+c*c)/(1+c*s)
     end do
     STOP_TIMER("CRYSTAL:extinction_angle_part")
      CHECK
   end function

   subroutine optimise_scale_factor(self)
    CRYSTAL :: self
   ! Determine the structure factor scale factor to scale the calculated
   ! structure factors .F_calc by in order to minimise the chi2. (But it does
   ! not do any scaling; see .extinction_correction routine for that)
     REAL :: top,bot,F_pred
     INT :: n,n_refl
     REFLECTION, PTR :: ref
     STACK("CRYSTAL:optimise_scale_factor")
     START_TIMER("CRYSTAL:optimise_scale_factor")
     n_refl = n_refl_(self%reflections)
     DIE_IF(NOT associated(self%reflections),"CRYSTAL:optimise_scale_factor ... no reflection data")
     DIE_IF(NOT have_F_calc_(self%reflections),"CRYSTAL:optimise_scale_factor ... no calculated structure factors")
     DIE_IF(NOT have_F_exp_(self%reflections),"CRYSTAL:optimise_scale_factor ... no experimental structure factors")
     DIE_IF(NOT have_F_sigma_(self%reflections),"CRYSTAL:optimise_scale_factor ... no structure factor errors")
     top = ZERO
     bot = ZERO
     do n=1,n_refl
       ref => self%reflections(n)
       if (self%data_kind=="pnd") then; F_pred = ref%F_calc
       else;                   F_pred = abs(ref%F_calc)
       end if
       ENSURE(ref%F_sigma/=ZERO,"CRYSTAL:optimise_scale_factor ... Structure factor has zero error!")
       top = top + F_pred * ref%F_exp  / (ref%F_sigma * ref%F_sigma)
       bot = bot + F_pred * F_pred / (ref%F_sigma * ref%F_sigma)
     end do
     self%scale_factor = top/bot
     self%n_param = 1
     STOP_TIMER("CRYSTAL:optimise_scale_factor")
      CHECK
   end subroutine

   subroutine optimise_extinction_factor(self)
    CRYSTAL :: self
   ! Optimize the .scale_factor and .extinction_factor parameter of Larson's
   ! method.
   ! NOTE: the corrections are not applied to .F_pred; use the routine
   ! .extinction_corrections to do that.
   ! Reference: Larson, A. C., in <I>Crystallographic Computing</I>
   !            Ed. Ahmed, F. R. (Copenhagen, Munksgaard 1970), pp. 291-294.
      target :: self
      REALVEC(2) :: p
      REAL :: chi2_min
      STACK("CRYSTAL:optimise_extinction_factor")
      START_TIMER("CRYSTAL:optimise_extinction_factor")
      call optimise_scale_factor_(self)
      p(1) = self%scale_factor
      p(2) = ZERO          ! This is the .extinction_factor
    ! saved_self.copy(self)
      saved_self => self
#ifndef NOGENERIC
      call minimise_BFGS(chi2,d_chi2,p,chi2_min,tol=TOL(7),gtol=TOL(7),step=TOL(4))
#else
      call REALVEC_minimise_BFGS(CRYSTAL_chi2,CRYSTAL_d_chi2,p,chi2_min,tol=TOL(7),gtol=TOL(7),step=TOL(4))
#endif
    ! saved_self.destroy_ptr_part
      self%scale_factor = p(1)
      self%extinction_factor = p(2)
      self%n_param = 2
     STOP_TIMER("CRYSTAL:optimise_extinction_factor")
      CHECK
   end subroutine

   function chi2(p) result(res)
   ! Make the chi2 between the calculated and experimental structure factors
   ! with extinction and scale parameters stored in vector p.
     REALVEC(:) :: p
     REAL :: res
     REAL :: tmp,tmp1
     REALVEC(:), PTR :: ext
     REFLECTION, PTR :: ref
     INT :: n,n_refl
     CRYSTAL, PTR :: self
   ! self.copy(saved_self)
     STACK("CRYSTAL:chi2")
     START_TIMER("CRYSTAL:chi2")
     self => saved_self
     ENSURE(associated(self%reflections),"CRYSTAL:chi2 ... no reflection data")
     ENSURE(have_F_calc_(self%reflections),"CRYSTAL:chi2 ... no calculated structure factors")
     ENSURE(have_F_exp_(self%reflections),"CRYSTAL:chi2 ... no experimental structure factors")
     ENSURE(have_F_sigma_(self%reflections),"CRYSTAL:chi2 ... no structure factor errors")
     ENSURE(size(p)==2,"CRYSTAL:chi2 ... wrong size, p")
     n_refl = n_refl_(self%reflections)
     call create_(ext,n_refl)
     ext = extinction_correction_(self,p(1),p(2))
     tmp = ZERO
     do n=1,n_refl
       ref => self%reflections(n)
       tmp1 = (abs(ref%F_calc) * ext(n) - ref%F_exp) / ref%F_sigma
       tmp = tmp + tmp1 * tmp1
     end do
       res = tmp / max(n_refl-self%n_param,1)
     call destroy_(ext)
   ! self.destroy_ptr_part
     STOP_TIMER("CRYSTAL:chi2")
      CHECK
   end function

   function d_chi2(p) result(res)
   ! Return the derivative of the chi2 with respect to the .scale_factor in
   ! p(1), and with respect to .extinction_factor in p(2). This routine is for
   ! use in the BFGS minimiser.
      REALVEC(:) :: p
      REALVEC(size(p)) :: res
      CRYSTAL, PTR :: self
   !  self.copy(saved_self)
      STACK("CRYSTAL:d_chi2")
      START_TIMER("CRYSTAL:d_chi2")
      self => saved_self
      ENSURE(size(p)==2,"CRYSTAL:d_chi2 ... wrong size, p")
      res(1) = d_chi2_d_scale_(self,p)
      res(2) = d_chi2_d_ext_(self,p)
   !  self.destroy_ptr_part
!      res(1) = saved_self.d_chi2_d_scale(p)
!      res(2) = saved_self.d_chi2_d_ext(p)
     STOP_TIMER("CRYSTAL:d_chi2")
      CHECK
   end function

   function d_chi2_d_scale(self,p) result(res)
    CRYSTAL :: self
   ! Derivative of the chi^2 with respect to the scale factor
     REALVEC(:) :: p
     REAL :: res
     REAL :: tmp,F_pred
     REALVEC(:), PTR :: ext
     REFLECTION, PTR :: ref
     INT :: n,n_refl
     STACK("CRYSTAL:d_chi2_d_scale")
     START_TIMER("CRYSTAL:d_chi2_d_scale")
     n_refl = n_refl_(self%reflections)
     ENSURE(associated(self%reflections),"CRYSTAL:d_chi2_d_scale ... no reflection data")
     ENSURE(have_F_calc_(self%reflections),"CRYSTAL:d_chi2_d_scale ... no calculated structure factors")
     ENSURE(have_F_exp_(self%reflections),"CRYSTAL:d_chi2_d_scale ... no experimental structure factors")
     ENSURE(have_F_sigma_(self%reflections),"CRYSTAL:d_chi2_d_scale ... no structure factor errors")
     ENSURE(size(p)==2,"CRYSTAL:d_chi2_d_scale ... wrong size, p")
     call create_(ext,n_refl)
     ext = extinction_correction_(self,p(1),p(2))
     tmp = ZERO
     do n=1,n_refl
       ref => self%reflections(n)
       F_pred = abs(ref%F_calc) * ext(n)
       tmp = tmp + F_pred*F_pred - ref%F_exp*F_pred / (ref%F_sigma*ref%F_sigma)
     end do
     res = TWO*tmp/(p(1)*max(n_refl-self%n_param,1))
     call destroy_(ext)
     STOP_TIMER("CRYSTAL:d_chi2_d_scale")
      CHECK
   end function

   function d_chi2_d_ext(self,p) result(res)
    CRYSTAL :: self
   ! Derivative of the chi^2 with respect to the extinction parameter.
     REALVEC(:), IN :: p
     REAL :: res
     REAL :: tmp,extn,p1
     REALVEC(:), PTR :: angle_bit,ext
     REFLECTION, PTR :: ref
     INT :: n,n_refl
     STACK("CRYSTAL:d_chi2_d_ext")
     START_TIMER("CRYSTAL:d_chi2_d_ext")
     n_refl = n_refl_(self%reflections)
     ENSURE(associated(self%reflections),"CRYSTAL:d_chi2_d_ext ... no reflection data")
     ENSURE(have_F_calc_(self%reflections),"CRYSTAL:d_chi2_d_ext ... no calculated structure factors")
     ENSURE(have_F_exp_(self%reflections),"CRYSTAL:d_chi2_d_ext ... no experimental structure factors")
     ENSURE(have_F_sigma_(self%reflections),"CRYSTAL:d_chi2_d_ext ... no structure factor errors")
     ENSURE(size(p)==2,"CRYSTAL:d_chi2_d_ext ... wrong size, p")
     call create_(angle_bit,n_refl)
     call create_(ext,n_refl)
     angle_bit = extinction_angle_part_(self)
     p1 = p(1)
     ext = extinction_correction_(self,ONE,p(2))
     tmp = ZERO
     do n = 1,n_refl
       ref => self%reflections(n)
       extn = ext(n)
       tmp = tmp + (p1 * abs(ref%F_calc) * extn - ref%F_exp) * &
           extn*extn*extn*extn*extn* ref%F_calc*ref%F_calc*ref%F_calc * &
           angle_bit(n) / (ref%F_sigma * ref%F_sigma)
     end do
     res = -p1*tmp/(TWO * max(n_refl-self%n_param,1))
     call destroy_(ext)
     call destroy_(angle_bit)
     STOP_TIMER("CRYSTAL:d_chi2_d_ext")
      CHECK
   end function

   function d_chi2_dU(self,dF) result(res)
    CRYSTAL :: self
   ! Evaluate the derivative of the chi^2 with respect to parameters U
   ! (e.g. thermal parameters) given the derivatives "dF" of where
   ! .F_calc with respect to these parameters U. NOTE: this routine
   ! assumes that the .scale_factor and .extinction_factor are fixed.
     CPXMAT(:,:) :: dF
     REALVEC(size(dF,2)) :: res
     CPXVEC(:), PTR :: Fc_conjgFcalc
     REFLECTION, PTR :: ref
     REALVEC(:), PTR :: ext,angle_bit
     REAL :: fac,Fc,Fc1,extn
     INT :: u,n_refl,n
     STACK("CRYSTAL:d_chi2_dU")
     START_TIMER("CRYSTAL:d_chi2_dU")
     n_refl = n_refl_(self%reflections)
     ENSURE(associated(self%reflections),"CRYSTAL:d_chi2_dU ... no reflection data")
     ENSURE(size(dF,1)==n_refl,"CRYSTAL:d_chi2_dU ... wrong size, dF")
     ENSURE(have_F_calc_(self%reflections),"CRYSTAL:d_chi2_dU ... no calculated structure factors")
     ENSURE(have_F_exp_(self%reflections),"CRYSTAL:d_chi2_dU ... no experimental structure factors")
     ENSURE(have_F_sigma_(self%reflections),"CRYSTAL:d_chi2_dU ... no structure factor errors")
     call create_(angle_bit,n_refl)
     angle_bit = extinction_angle_part_(self)
     call create_(ext,n_refl)
     ext = extinction_correction_(self,ONE,self%extinction_factor)
     call create_(Fc_conjgFcalc,n_refl)
     do n = 1,n_refl
       ref => self%reflections(n)
       extn = ext(n)
       Fc1 = abs(ref%F_calc)
       Fc = ( (self%scale_factor*extn*Fc1 - ref%F_exp) / &
          (ref%F_sigma * ref%F_sigma) ) * &
          (extn / Fc1 - HALF * extn * extn * extn * extn * extn * &
          self%extinction_factor * Fc1 * angle_bit(n))
       Fc_conjgFcalc(n) = Fc * conjg(ref%F_calc)
     end do
     fac = TWO*self%scale_factor/max(n_refl-self%n_param,1)
     do u = 1,size(dF,2)
        res(u) = fac * sum( Fc_conjgFcalc(:) * dF(:,u) )
     end do
     call destroy_(Fc_conjgFcalc)
     call destroy_(ext)
     call destroy_(angle_bit)
     STOP_TIMER("CRYSTAL:d_chi2_dU")
      CHECK
   end function

   subroutine make_k_pts(self,res)
    CRYSTAL :: self
   ! Convert the .reflection.hkl indices to reciprocal lattice vectors
   ! Dimension of res is [.n_refl,3]
      REALMAT(:,:) :: res
       INT :: n
      INTVEC(3) :: hkl
      REALMAT(3,3) :: rcm
      STACK("CRYSTAL:make_k_pts")
      START_TIMER("CRYSTAL:make_k_pts")
      rcm = TWO*PI*self%unitcell%reciprocal_matrix
      do n = 1, n_refl_(self%reflections)
         hkl = indices_(self%reflections,n)
         res(n,1) = dot_product(rcm(1,:),hkl(:))
         res(n,2) = dot_product(rcm(2,:),hkl(:))
         res(n,3) = dot_product(rcm(3,:),hkl(:))
      end do
     STOP_TIMER("CRYSTAL:make_k_pts")
      CHECK
   end subroutine

   PURE function n_unique_SF_k_pts(self) result(res)
    CRYSTAL :: self
   ! The number of unique k-points for an SF calculation
     IN :: self
     INT :: res
     res = self%n_unique_SF_symops * n_refl_(self%reflections)
     STOP_TIMER("CRYSTAL:n_unique_SF_k_pts")
   end function

   subroutine make_unique_SF_k_pts(self,k)
    CRYSTAL :: self
   ! Convert the hkl indices to unique reciprocal lattice vectors "k" required
   ! for structure factor calculations.
   ! (c) Dylan Jayatilaka, UWA, feb 1996
      REALMAT(:,:) :: k
      INT :: p,u,n
      REALMAT(3,3) :: b,rcm
      INTVEC(3) :: hkl
      STACK("CRYSTAL:make_unique_SF_k_pts")
      START_TIMER("CRYSTAL:make_unique_SF_k_pts")
      ENSURE(associated(self%unique_SF_symop),"CRYSTAL:make_unique_SF_k_pts ... no unique_SF_symop array!")
      ENSURE(size(k,1)>0,"CRYSTAL:make_unique_SF_k_pts ... no unique k points")
      ENSURE(size(k,1)==n_unique_SF_k_pts_(self),"CRYSTAL:make_unique_SF_k_pts ... wrong # of k points")
      p = 0
      rcm = TWO*PI*self%unitcell%reciprocal_matrix
      do u = 1,self%n_unique_SF_symops
         b = matmul(rcm,transpose(unique_SF_symop_mat_(self,u)))
         do n = 1,n_refl_(self%reflections)
            p = p + 1
            hkl = indices_(self%reflections,n)
            k(p,1) = dot_product(b(1,:),hkl(:))
            k(p,2) = dot_product(b(2,:),hkl(:))
            k(p,3) = dot_product(b(3,:),hkl(:))
         end do
      end do
     STOP_TIMER("CRYSTAL:make_unique_SF_k_pts")
      CHECK
   end subroutine

!  *************************************
!  Service methods used by other modules
!  *************************************

   subroutine make_phases_for_symop(self,u,phase,mask)
    CRYSTAL :: self
   ! Return the sum of the "phase" shifts for each (hkl) reflection from each
   ! glide vector for all symops which are equivalent to the "u"-th
   ! unique symmetry operation, .unique_SF_symop(u), as determined by the mask
   ! array.
     INT, IN :: u
     CPXVEC(:), OUT :: phase
     INTVEC(:), IN :: mask
     INTVEC(3) :: hkl
     REAL :: pi2,tx,ty,tz
     INT :: s,n,n_refl
     STACK("CRYSTAL:make_phases_for_symop")
     START_TIMER("CRYSTAL:make_phases_for_symop")
     n_refl = n_refl_(self%reflections)
     ENSURE(associated(self%unique_SF_symop),"CRYSTAL:make_phases_for_symop ... no unique_SF_symop array!")
     ENSURE(u<=self%n_unique_SF_symops,"CRYSTAL:make_phases_for_symop ... symop index out of range")
     ENSURE(size(phase)==n_refl,"CRYSTAL:make_phases_for_symop ... wrong length for phase array")
     pi2 = TWO*PI
     phase = ZERO
     do s = 1,self%spacegroup%n_seitz
       if (mask(s)/=self%unique_SF_symop(u)) cycle
       tx = pi2*self%spacegroup%seitz(1,4,s)
       ty = pi2*self%spacegroup%seitz(2,4,s)
       tz = pi2*self%spacegroup%seitz(3,4,s)
       do n=1,n_refl
         hkl = indices_(self%reflections,n)
         phase(n) = phase(n) + exp(cmplx(ZERO,hkl(1)*tx+hkl(2)*ty+hkl(3)*tz,kind=CPX_KIND))
       end do
     end do
     STOP_TIMER("CRYSTAL:make_phases_for_symop")
      CHECK
   end subroutine

   subroutine sum_unique_sf(self,sf,unique_sf)
    CRYSTAL :: self
   ! Form the structure factors "sf" from the sum of the list of
   ! unique structure factors, "unique_sf".
     CPXVEC(:) :: sf
     CPXVEC(:), IN :: unique_sf
     CPXVEC(:), PTR :: phase
     INT :: u,uf,ul,n_refl
     STACK("CRYSTAL:sum_unique_sf")
     START_TIMER("CRYSTAL:sum_unique_sf")
     n_refl = n_refl_(self%reflections)
     ENSURE( size(sf)==n_refl,"CRYSTAL:sum_unique_sf ... incorrect size for array sf")
     call create_(phase,n_refl)
     sf = ZERO
     do u = 1,self%n_unique_SF_symops
        uf = n_refl*(u-1)+1
        ul = n_refl*u
        call make_phases_for_symop_(self,u,phase,self%translated_symop)
        sf(:) = sf(:) + phase(:)*unique_sf(uf:ul)
        if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
        call make_phases_for_symop_(self,u,phase,self%inverted_symop)
        sf(:) = sf(:) + phase(:)*conjg(unique_sf(uf:ul))
     end do
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_unique_sf")
      CHECK
   end subroutine

   subroutine sum_unique_sf_ints(self,sf_ints,unique_sf_ints)
    CRYSTAL :: self
   ! Form the structure factor integrals "sf_ints" from a sum of the list
   ! of unique structure factors integrals  "unique_sf_ints".
     CPXMAT3(:,:,:) :: sf_ints
     CPXMAT3(:,:,:), IN :: unique_sf_ints
     CPXVEC(:), PTR :: phase
     INT :: u,uf,n,n_refl
     STACK("CRYSTAL:sum_unique_sf_ints")
     START_TIMER("CRYSTAL:sum_unique_sf_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(sf_ints,1)==n_refl,"CRYSTAL:sum_unique_sf_ints ... wrong size for sf_ints!")
     call create_(phase,n_refl)
     sf_ints = ZERO
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         sf_ints(n,:,:) = sf_ints(n,:,:) + phase(n)*unique_sf_ints(uf+n,:,:)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         sf_ints(n,:,:) = sf_ints(n,:,:) + phase(n)*conjg(unique_sf_ints(uf+n,:,:))
       end do
     end do
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_unique_sf_ints")
      CHECK
   end subroutine

   subroutine sum_unique_sf_deriv_U(self,sf,unique_sf)
    CRYSTAL :: self
   ! Form the structure factor derivatives "sf" (wrt the thermal paramaters,U)
   !  from a sum of the list of unique structure factor derivatives "unique_sf".
     CPXMAT(:,:) :: sf
     CPXMAT(:,:), IN :: unique_sf
     CPXVEC(:), PTR :: phase
     INT :: u,uf,n,n_refl
     STACK("CRYSTAL:sum_unique_sf_deriv_U")
     START_TIMER("CRYSTAL:sum_unique_sf_deriv_U")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(sf,1)==n_refl,"CRYSTAL:sum_unique_sf_deriv_U ... wrong size, matrix sf")
     ENSURE(size(unique_sf,2)==n_unique_SF_k_pts_(self),"CRYSTAL:sum_unique_sf_deriv_U ... wrong size, matrix sf")
     call create_(phase,n_refl)
     sf = ZERO
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         sf(n,:) = sf(n,:) + phase(n)*unique_sf(:,uf+n)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         sf(n,:) = sf(n,:) + phase(n)*conjg(unique_sf(:,uf+n))
       end do
     end do
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_unique_sf_deriv_U")
      CHECK
   end subroutine

   subroutine sum_ft_ints(self,ft_ints,unique_ft_ints)
    CRYSTAL :: self
   ! Form the Fourier transform integrals "ft_ints" from a sum of the list
   ! of unique integrals "unique_ft_ints".  Dimensions of ft_ints are
   ! [.n_refl,n_comp_a,n_comp_b].
     CPXMAT3(:,:,:) :: ft_ints
     CPXMAT3(:,:,:), IN :: unique_ft_ints
     CPXVEC(:), PTR :: phase
     INT :: u,uf,n,n_refl
     STACK("CRYSTAL:sum_ft_ints")
     START_TIMER("CRYSTAL:sum_ft_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_ints ... incorrect size for array ft_ints")
     call create_(phase,n_refl)
     ft_ints = ZERO
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + phase(n)*unique_ft_ints(uf+n,:,:)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + phase(n)*conjg(unique_ft_ints(uf+n,:,:))
       end do
     end do
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_ft_ints")
      CHECK
   end subroutine

   subroutine make_phased_matrix_for_symop(self,u,phase,mask)
    CRYSTAL :: self
   ! Return the sum of the "phase" shifts times the seitz matrices for all
   ! symops which are equivalent to the "u"-th unique symmetry operation,
   ! .unique_SF_symop(u), as determined by the "mask" array, when
   ! mask(u)==.unique_SF_symop(u).
     INT, IN :: u
     CPXMAT3(:,:,:), OUT :: phase
     INTVEC(:), IN :: mask
     INTVEC(3) :: hkl
     REALVEC(3) :: t
     REAL :: pi2
     INT :: s,n,n_refl
     REALMAT(4,4) :: seitz
     STACK("CRYSTAL:make_phased_matrix_for_symop")
     START_TIMER("CRYSTAL:make_phased_matrix_for_symop")
     pi2 = TWO*PI
     phase = ZERO
     n_refl = n_refl_(self%reflections)
     ENSURE(associated(self%unique_SF_symop),"CRYSTAL:make_phased_matrix_for_symop ... no unique_SF_symop array!")
     ENSURE(u<=self%n_unique_SF_symops,"CRYSTAL:make_phased_matrix_for_symop ... symop index out of range")
     ENSURE(size(phase,1)==n_refl,"CRYSTAL:make_phased_matrix_for_symop ... wrong size, dim=1, phase array")
     ENSURE(size(phase,2)==3,"CRYSTAL:make_phased_matrix_for_symop ... wrong size, dim=2, phase array")
     ENSURE(size(phase,3)==3,"CRYSTAL:make_phased_matrix_for_symop ... wrong size, dim=3, phase array")
     do s = 1,self%spacegroup%n_seitz
       if (mask(s)/=self%unique_SF_symop(u)) cycle
       seitz = transpose(self%spacegroup%seitz(:,:,s))
       t = (/ pi2*seitz(4,1), pi2*seitz(4,2), pi2*seitz(4,3) /)
       if (seitz(3,3)>0) then                  ! M points along +z always
         do n = 1,n_refl
           hkl = indices_(self%reflections,n)
           phase(n,:,:) = phase(n,:,:) + &
              seitz(:,:)*exp(cmplx(ZERO,dot_product(t,hkl),kind=CPX_KIND))
         end do
       else                                    ! M points in -z direction
         seitz(3,:3) = -seitz(3,:3)           ! Invert
         do n = 1,n_refl
           hkl = indices_(self%reflections,n)
           phase(n,:,:) = phase(n,:,:) + &
              seitz(:,:)*exp(cmplx(ZERO,dot_product(t,hkl),kind=CPX_KIND))
         end do
       end if
     end do
     STOP_TIMER("CRYSTAL:make_phased_matrix_for_symop")
      CHECK
   end subroutine

   subroutine sum_PND_spin_ints(self,ft_ints,unique_ft_ints)
    CRYSTAL :: self
   ! Form the Fourier transform integrals "ft_ints", required for the spin
   ! magnetic structure factors, from a sum of the list of unique structure
   ! factor intergals "unique_ft_ints".  Dimensions of ft_ints are
   ! [.n_refl,n_comp_a,n_comp_b,3].
     CPXMAT4(:,:,:,:) :: ft_ints
     CPXMAT3(:,:,:), IN :: unique_ft_ints
     CPXMAT3(:,:,:), PTR :: phase
     REALMAT(:,:), PTR :: q
     INT :: u,uf,n,n_refl
     STACK("CRYSTAL:sum_PND_spin_ints")
     START_TIMER("CRYSTAL:sum_PND_spin_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(ft_ints,1)==n_refl,"CRYSTAL:sum_PND_spin_ints ... wrong size for ft_ints!")
     call create_(q,n_refl,3)
     call create_(phase,n_refl,3,3)
     call make_k_pts_(self,q)
     ft_ints = ZERO
     do u = 1,self%n_unique_SF_symops
        uf = n_refl*(u-1)
        call make_phased_matrix_for_symop_(self,u,phase,self%translated_symop)
        do n = 1,n_refl
           ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + &
              ( q(n,3)*q(n,1)*phase(n,1,1) + q(n,3)*q(n,2)*phase(n,2,1)     &
              - q(n,1)*q(n,1)*phase(n,3,1) - q(n,2)*q(n,2)*phase(n,3,1) ) * &
              unique_ft_ints(uf+n,:,:)
           ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + &
              ( q(n,3)*q(n,1)*phase(n,1,2) + q(n,3)*q(n,2)*phase(n,2,2)     &
              - q(n,1)*q(n,1)*phase(n,3,2) - q(n,2)*q(n,2)*phase(n,3,2) ) * &
              unique_ft_ints(uf+n,:,:)
           ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + &
              ( q(n,3)*q(n,1)*phase(n,1,3) + q(n,3)*q(n,2)*phase(n,2,3)     &
              - q(n,1)*q(n,1)*phase(n,3,3) - q(n,2)*q(n,2)*phase(n,3,3) ) * &
              unique_ft_ints(uf+n,:,:)
        end do
        if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle ! for inversions...
        call make_phased_matrix_for_symop_(self,u,phase,self%inverted_symop)
        do n = 1,n_refl
           ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + &
              ( q(n,3)*q(n,1)*phase(n,1,1) + q(n,3)*q(n,2)*phase(n,2,1)     &
              - q(n,1)*q(n,1)*phase(n,3,1) - q(n,2)*q(n,2)*phase(n,3,1) ) * &
              conjg(unique_ft_ints(uf+n,:,:))
           ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + &
              ( q(n,3)*q(n,1)*phase(n,1,2) + q(n,3)*q(n,2)*phase(n,2,2)     &
              - q(n,1)*q(n,1)*phase(n,3,2) - q(n,2)*q(n,2)*phase(n,3,2) ) * &
              conjg(unique_ft_ints(uf+n,:,:))
           ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + &
              ( q(n,3)*q(n,1)*phase(n,1,3) + q(n,3)*q(n,2)*phase(n,2,3)     &
              - q(n,1)*q(n,1)*phase(n,3,3) - q(n,2)*q(n,2)*phase(n,3,3) ) * &
              conjg(unique_ft_ints(uf+n,:,:))
        end do
     end do
     call destroy_(phase)
     ! The factor of two for conversion to Bohr magnetons cancels the
     ! factor of half for the S=sigma/2 operator, and g_e x mu_B = 1.
     do n = 1,n_refl
       ft_ints(n,:,:,:) = ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)
     STOP_TIMER("CRYSTAL:sum_PND_spin_ints")
      CHECK
   end subroutine

   subroutine sum_PND_nabla_ints(self,ft_ints,unique_ft_ints)
    CRYSTAL :: self
   ! Form the fourier transform nabla_a_3 integrals "ft_ints", required for the
   ! PND magnetic structure factors, from a sum of "unique_ft_ints".
   ! Dimensions of ft_ints are [.n_refl,n_comp_a,n_comp_b,3].
     CPXMAT3(:,:,:) :: ft_ints
     CPXMAT4(:,:,:,:), IN :: unique_ft_ints
     CPXMAT3(:,:,:), PTR :: phase
     REALMAT(:,:), PTR :: q
     INT :: u,uf,n,n_refl
     STACK("CRYSTAL:sum_PND_nabla_ints")
     START_TIMER("CRYSTAL:sum_PND_nabla_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(ft_ints,1)==n_refl,"CRYSTAL:sum_PND_nabla_ints ... wrong size for ft_ints!")
     call create_(q,n_refl,3)
     call create_(phase,n_refl,3,3)
     call make_k_pts_(self,q)
     ft_ints = ZERO
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phased_matrix_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + &
          (q(n,1)*phase(n,2,1)-q(n,2)*phase(n,1,1))*unique_ft_ints(uf+n,:,:,1) + &
          (q(n,1)*phase(n,2,2)-q(n,2)*phase(n,1,2))*unique_ft_ints(uf+n,:,:,2) + &
          (q(n,1)*phase(n,2,3)-q(n,2)*phase(n,1,3))*unique_ft_ints(uf+n,:,:,3)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle ! for inversions ...
       call make_phased_matrix_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ft_ints(n,:,:) = ft_ints(n,:,:) + &
          (q(n,1)*phase(n,2,1)-q(n,2)*phase(n,1,1))*conjg(unique_ft_ints(uf+n,:,:,1)) + &
          (q(n,1)*phase(n,2,2)-q(n,2)*phase(n,1,2))*conjg(unique_ft_ints(uf+n,:,:,2)) + &
          (q(n,1)*phase(n,2,3)-q(n,2)*phase(n,1,3))*conjg(unique_ft_ints(uf+n,:,:,3))
       end do
     end do
     call destroy_(phase)
     ! The factor of 2 to convert to Bohr magnetons cancels the factor
     ! of 1/2 for the Bohr magneton.
     do n = 1,n_refl
       ft_ints(n,:,:) = ft_ints(n,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)
     STOP_TIMER("CRYSTAL:sum_PND_nabla_ints")
      CHECK
   end subroutine

   subroutine sum_ft_spin_ints(self,ft_ints,unique_ft_ints)
    CRYSTAL :: self
   ! Form the Fourier transform integrals "ft_ints", required for the spin
   ! magnetic structure factors, from a sum of the list of unique structure
   ! factor intergals "unique_ft_ints".  Dimensions of ft_ints are
   ! [.n_refl,n_comp_a,n_comp_b,3,3].
     CPXMAT5(:,:,:,:,:) :: ft_ints
     CPXMAT3(:,:,:), IN :: unique_ft_ints
     CPXVEC(:), PTR :: phase
     REALMAT(:,:), PTR :: q
     CPXMAT(:,:), PTR :: ints
     INT :: u,uf,n,n_refl
     STACK("CRYSTAL:sum_ft_spin_ints")
     START_TIMER("CRYSTAL:sum_ft_spin_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_spin_ints ... wrong size for ft_ints!")
     call create_(phase,n_refl)
     call create_(q,n_refl,3)
     call create_(ints,size(ft_ints,2),size(ft_ints,3))
     call make_k_pts_(self,q)
     ft_ints = ZERO
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ints = phase(n)*unique_ft_ints(uf+n,:,:)
         ft_ints(n,:,:,1,1) = ft_ints(n,:,:,1,1) + q(n,1)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,1) = ft_ints(n,:,:,2,1) + q(n,2)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,2) = ft_ints(n,:,:,2,2) + q(n,2)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,1) = ft_ints(n,:,:,3,1) + q(n,3)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,3,2) = ft_ints(n,:,:,3,2) + q(n,3)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,3) = ft_ints(n,:,:,3,3) + q(n,3)*q(n,3)*ints(:,:)
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle ! for inversions...
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         ints = phase(n)*conjg(unique_ft_ints(uf+n,:,:))
         ft_ints(n,:,:,1,1) = ft_ints(n,:,:,1,1) + q(n,1)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,1) = ft_ints(n,:,:,2,1) + q(n,2)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,2,2) = ft_ints(n,:,:,2,2) + q(n,2)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,1) = ft_ints(n,:,:,3,1) + q(n,3)*q(n,1)*ints(:,:)
         ft_ints(n,:,:,3,2) = ft_ints(n,:,:,3,2) + q(n,3)*q(n,2)*ints(:,:)
         ft_ints(n,:,:,3,3) = ft_ints(n,:,:,3,3) + q(n,3)*q(n,3)*ints(:,:)
       end do
     end do
     call destroy_(ints)
     ! The factor of two for conversion to Bohr magnetons cancels the
     ! factor of half for the S operator
     do n = 1,n_refl
        ft_ints(n,:,:,:,:) = -ft_ints(n,:,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_ft_spin_ints")
      CHECK
   end subroutine

   subroutine sum_ft_r_ints(self,ft_ints,unique_ft_ints,B)
    CRYSTAL :: self
   ! Form the Fourier transform dipole integrals "ft_ints", required for the PND
   ! magnetic structure factors, from a sum of the list of unique structure
   ! factor intergals "unique_ft_ints".  "B" is the external magnetic field.
   ! Note: only inversions are allowed as symmetry elements for PND simulations.
   ! Dimensions of ft are [.n_refl,n_comp_a,n_comp_b,3].
     CPXMAT4(:,:,:,:) :: ft_ints
     CPXMAT4(:,:,:,:), IN :: unique_ft_ints
     REALVEC(3) :: B
     CPXVEC(:), PTR :: phase
     REALMAT(:,:), PTR :: q
     CPXMAT3(:,:,:), PTR :: ints
     INT :: u,uf,n,n_refl
     CPX :: ci
     STACK("CRYSTAL:sum_ft_r_ints")
     START_TIMER("CRYSTAL:sum_ft_r_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE( size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_r_ints ... incorrect size for array ft_ints")
     call create_(phase,n_refl)
     call create_(q,n_refl,3)
     call create_(ints,size(ft_ints,2),size(ft_ints,3),3)
     call make_k_pts_(self,q)
     ft_ints = ZERO
     do u = 1,self%n_unique_SF_symops
       uf = n_refl*(u-1)
       call make_phases_for_symop_(self,u,phase,self%translated_symop)
       do n = 1,n_refl
         ints = phase(n)*unique_ft_ints(uf+n,:,:,:)
         ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1)) &
                                             - q(n,3)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3))
         ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2)) &
                                             - q(n,1)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1))
         ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3)) &
                                             - q(n,2)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2))
       end do
       if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle ! for inversions ...
       call make_phases_for_symop_(self,u,phase,self%inverted_symop)
       do n = 1,n_refl
         ints = -phase(n)*conjg(unique_ft_ints(uf+n,:,:,:)) ! note minus sign
         ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1)) &
                                             - q(n,3)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3))
         ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2)) &
                                             - q(n,1)*(B(1)*ints(:,:,2)-B(2)*ints(:,:,1))
         ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*(B(3)*ints(:,:,1)-B(1)*ints(:,:,3)) &
                                             - q(n,2)*(B(2)*ints(:,:,3)-B(3)*ints(:,:,2))
       end do
     end do
     call destroy_(ints)
     ! The factor of two for conversion to Bohr magnetons cancels the
     ! factor of half for the S operator
     ci = (ZERO,ONE)
     do n = 1,n_refl
       ft_ints(n,:,:,:) = -ci*ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
     end do
     call destroy_(q)
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_ft_r_ints")
      CHECK
   end subroutine

   subroutine sum_ft_nabla_ints(self,ft_ints,unique_ft_ints)
    CRYSTAL :: self
   ! Form the fourier transform nabla_a integrals "ft_ints", required for the
   ! PND magnetic striucture factors, from a sum of "unique_ft_ints".
   ! Note: only inversions are allowed as symmetry elements for PND simulations.
   ! Note: the complex conjugate nabla_b integrals are not included.
   ! Dimensions of ft_ints are [.n_refl,n_comp_a,n_comp_b,3].
      CPXMAT4(:,:,:,:) :: ft_ints
      CPXMAT4(:,:,:,:), IN :: unique_ft_ints
      CPXVEC(:), PTR :: phase
      REALMAT(:,:), PTR :: q
      CPXMAT3(:,:,:), PTR :: ints
      INT :: u,uf,n,n_refl
      STACK("CRYSTAL:sum_ft_nabla_ints")
      START_TIMER("CRYSTAL:sum_ft_nabla_ints")
      n_refl = n_refl_(self%reflections)
      ENSURE(size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_nabla_ints ... wrong size for ft_ints!")
      call create_(phase,n_refl)
      call create_(q,n_refl,3)
      call create_(ints,size(ft_ints,2),size(ft_ints,3),3)
      call make_k_pts_(self,q)
      ft_ints = ZERO
      do u = 1,self%n_unique_SF_symops
         uf = n_refl*(u-1)
         call make_phases_for_symop_(self,u,phase,self%translated_symop)
         do n = 1,n_refl
            ints = phase(n)*unique_ft_ints(uf+n,:,:,:)
            ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*ints(:,:,3)-q(n,3)*ints(:,:,2)
            ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*ints(:,:,1)-q(n,1)*ints(:,:,3)
            ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*ints(:,:,2)-q(n,2)*ints(:,:,1)
         end do
         if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle ! for inversions ...
         call make_phases_for_symop_(self,u,phase,self%inverted_symop)
         do n = 1,n_refl
            ints = -phase(n)*conjg(unique_ft_ints(uf+n,:,:,:)) ! note minus sign
            ft_ints(n,:,:,1) = ft_ints(n,:,:,1) + q(n,2)*ints(:,:,3)-q(n,3)*ints(:,:,2)
            ft_ints(n,:,:,2) = ft_ints(n,:,:,2) + q(n,3)*ints(:,:,1)-q(n,1)*ints(:,:,3)
            ft_ints(n,:,:,3) = ft_ints(n,:,:,3) + q(n,1)*ints(:,:,2)-q(n,2)*ints(:,:,1)
         end do
      end do
      call destroy_(ints)
      ! The factor of 2 to convert to Bohr magnetons cancels the factor
      ! of 1/2 for the Bohr magneton
      ! Extra minus sign introduced, but not sure why ...
      ! Reversed minus sign introduced ...
      do n = 1,n_refl
          ft_ints(n,:,:,:) = ft_ints(n,:,:,:)/(q(n,1)*q(n,1)+q(n,2)*q(n,2))
      end do
      call destroy_(q)
      call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_ft_nabla_ints")
      CHECK
   end subroutine

   subroutine sum_ft_j_ints(self,ft_ints,unique_ft_ints)
    CRYSTAL :: self
   ! Form the fourier transform j integrals "ft_ints", required for the PND
   ! magnetic striucture factors, from a sum of "unique_ft_ints".
   ! Note: only inversions are allowed as symmetry elements for PND simulations.
   ! Note: the complex conjugate nabla_b integrals are not included.
   ! Dimensions of ft_ints are [.n_refl,n_comp_a,n_comp_b,3].
     CPXMAT4(:,:,:,:) :: ft_ints
     CPXMAT4(:,:,:,:), IN :: unique_ft_ints
     CPXVEC(:), PTR :: phase
     INT :: u,uf, n,n_refl
     STACK("CRYSTAL:sum_ft_j_ints")
     START_TIMER("CRYSTAL:sum_ft_j_ints")
     n_refl = n_refl_(self%reflections)
     ENSURE(size(ft_ints,1)==n_refl,"CRYSTAL:sum_ft_j_ints ... incorrect size for array ft_ints")
     call create_(phase,n_refl)
     ft_ints = ZERO
     do u = 1,self%n_unique_SF_symops
        uf = n_refl*(u-1)
        call make_phases_for_symop_(self,u,phase,self%translated_symop)
        do n = 1,n_refl
          ft_ints(n,:,:,:) = ft_ints(n,:,:,:) + phase(n)*unique_ft_ints(uf+n,:,:,:)
        end do
        if ( all(self%inverted_symop/=self%unique_SF_symop(u)) ) cycle ! for inversions...
        call make_phases_for_symop_(self,u,phase,self%inverted_symop)
        do n = 1,n_refl
          ft_ints(n,:,:,:) = ft_ints(n,:,:,:) - phase(n)*conjg(unique_ft_ints(uf+n,:,:,:))
        end do
     end do
     call destroy_(phase)
     STOP_TIMER("CRYSTAL:sum_ft_j_ints")
      CHECK
   end subroutine


!  *****************************************
!  Reduced group, Unique operators, Z number
!  *****************************************

   subroutine make_reduced_group_data(self,atom,prune_asymmetric_unit)
    CRYSTAL :: self
   ! The reduced group are those unique seitz operators which are needed to
   ! generate the complete ".fragment_cell_geometry" from the atom fragment
   ! positions in "atom".  Some of the spacegroup symmetry operations may only
   ! lead to fragment geometries which are inversions of, or translations of,
   ! other operations in the reduced group.  This information is also worked out
   ! here, and it can be used to save work in structure factor calculations.
   ! If "prune_asymmetric_unit") is present then any non-unique atoms in the
   ! ".asymmetric_unit_geometry" are removed. NOTE: ths is probably not what you
   ! want for strcuture factor calcs, where the fragment geometry becomes the
   ! asymmetric unit, but it IS what you want when processing CIF files with
   ! possibly overcomplete asymmetric units.
      ATOMVEC(:), IN :: atom
      BIN, optional, IN :: prune_asymmetric_unit
      STACK("CRYSTAL:make_reduced_group_data")
      START_TIMER("CRYSTAL:make_reduced_group_data")
      if (self%reduced_group_info_made) call destroy_fragment_data_(self)
      self%n_fragment_atoms = size(atom)
      call create_(self%fragment_geometry,3,self%n_fragment_atoms)
      call get_geometry_(atom,self%fragment_geometry)
      call change_into_fractional_(self%unitcell,self%fragment_geometry)
      call make_reduced_symops_(self)         ! These don't seem to be used
      call make_cluster_symops_(self)
      call make_inverted_symops_(self)        ! These are used to save time in SF calcs
      call make_translated_symops_(self)
      call make_unique_SF_symops_(self)
      call make_unique_fragment_atoms_(self)  ! See if we have an asymmetric fragment
      call make_fragment_cell_geometry_(self) ! These are to get full unit cell geoms
      if (NOT associated(self%asymmetric_unit_geometry)) call make_asymmetric_geometry_(self) 
      call make_unit_cell_geometry_(self,prune_asymmetric_unit)
      call make_repetition_factors_(self)     ! Rep. factors for SF calculations
      self%Z = z_factor_(self,atom)
      self%reduced_group_info_made = TRUE
     STOP_TIMER("CRYSTAL:make_reduced_group_data")
      UNSTACK
   end subroutine

   function z_factor(self,atom) result(res)
    CRYSTAL :: self
   ! The Z crystallographic factor for ".fragment_geometry", defined as the
   ! ratio of the number of electrons in the unit cell on the number of
   ! electrons in the fragment. Hence, we require information for each "atom" in
   ! the fragment.
      ATOMVEC(:), IN :: atom
      REAL :: res
      REAL :: u,f
      INT :: n,a
      STACK("CRYSTAL:z_factor")
      START_TIMER("CRYSTAL:z_factor")
      ENSURE(associated(self%atom_for_fragment_cell_atom),"CRYSTAL:z_factor ... no atom_for_fragment_cell_atom array")
      u = ZERO
      do n = 1,self%n_fragment_cell_atoms
         a = self%atom_for_fragment_cell_atom(n)
         u = u + atom(a)%atomic_number
      end do
      f = ZERO
      do n = 1,self%n_fragment_atoms
         f = f + atom(n)%atomic_number
      end do
      res = u/f
     STOP_TIMER("CRYSTAL:z_factor")
      CHECK
   end function

   subroutine make_reduced_symops(self)
    CRYSTAL :: self
   ! Make a list of the indices of the Seitz matrices, ".reduced_symop", which
   ! will generate different geometries from that in ".fragment_geometry" when
   ! both the original and transformed geometries are converted to unit cell
   ! coordinates.  ".n_reduced_symops" is set to the number of these reduced
   ! symmetry operations.
      REALMAT(:,:), PTR :: gi,gu
      INT :: i,j,u, n
      BIN :: identical
      STACK("CRYSTAL:make_reduced_symops")
      START_TIMER("CRYSTAL:make_reduced_symops")
      call create_(gi,3,self%n_fragment_atoms)
      call create_(gu,3,self%n_fragment_atoms)
      call create_(self%reduced_symop,self%spacegroup%n_seitz)
      n = 1
      self%reduced_symop(1) = 1
      do i = 2,self%spacegroup%n_seitz
         gi = self%fragment_geometry
         call transform_geometry_(self,gi,i,to_unit_cell=TRUE)
         do j = 1,n                            ! Loop over reduced symops
            gu = self%fragment_geometry
            u = self%reduced_symop(j)
            call transform_geometry_(self,gu,u,to_unit_cell=TRUE)
            identical  = is_same_geometry_(self,gi,gu)
            if (identical) exit
         end do
         if (NOT identical) then
            n = n + 1
            self%reduced_symop(n) = i
         end if
      end do
      self%n_reduced_symops = n
      call shrink_(self%reduced_symop,n)
      call destroy_(gu)
      call destroy_(gi)
     STOP_TIMER("CRYSTAL:make_reduced_symops")
      UNSTACK
   end subroutine

   subroutine make_cluster_symops(self)
    CRYSTAL :: self
   ! Make a list of the indices of the Seitz matrices, ".cluster_symop", which
   ! will generate different geometries from that in ".fragment_geometry".
   ! This routine is the same as make_reduced_symops except that the geometries
   ! are *not* converted to unit cell coordinates.  ".n_cluster_symops" is set
   ! to the number of these symmetry operations.  These operators are useful for
   ! generating clusters of this fragment. NOTE: the unit operator, element 1,
   ! is part of the set of cluster_symops.
      REALMAT(:,:), PTR :: gi,gu
      INT :: i,j,u, n
      BIN :: identical
      STACK("CRYSTAL:make_cluster_symops")
      START_TIMER("CRYSTAL:make_cluster_symops")
      call create_(gi,3,self%n_fragment_atoms)
      call create_(gu,3,self%n_fragment_atoms)
      call create_(self%cluster_symop,self%spacegroup%n_seitz)
      n = 1
      self%cluster_symop(1) = 1
      do i = 2,self%spacegroup%n_seitz
         gi = self%fragment_geometry
         call transform_geometry_(self,gi,i)
         do j = 1,n                       ! Loop over cluster symops
            gu = self%fragment_geometry
            u = self%cluster_symop(j)
            call transform_geometry_(self,gu,u)
            identical  = is_same_geometry_(self,gi,gu)
            if (identical) exit
         end do
         if (NOT identical) then
            n = n + 1
            self%cluster_symop(n) = i
         end if
      end do
      self%n_cluster_symops = n
      call shrink_(self%cluster_symop,n)
      call destroy_(gu)
      call destroy_(gi)
     STOP_TIMER("CRYSTAL:make_cluster_symops")
      UNSTACK
   end subroutine

   subroutine make_inverted_symops(self)
    CRYSTAL :: self
   ! Determine which of the reduced symops can generate geometries from
   ! ".fragment cell_geometry" which are related by inversion.
      INT :: n,i,j
      BIN :: inverted
      STACK("CRYSTAL:make_inverted_symops")
      START_TIMER("CRYSTAL:make_inverted_symops")
      ENSURE(associated(self%reduced_symop),"CRYSTAL:make_inverted_symops ... no reduced_symop array")
      ENSURE(associated(self%spacegroup%seitz),"CRYSTAL:make_inverted_symops ... Seitz matrices not initialised")
      call create_(self%inverted_symop,self%spacegroup%n_seitz)
      self%inverted_symop = 0
      n = 0
      do i = 2,self%spacegroup%n_seitz
         do j = 1,i-1
            inverted = equals_(self%spacegroup%seitz(1:3,1:3,i), &
                      -self%spacegroup%seitz(1:3,1:3,j))
            if (inverted) then
               n = n + 1
               self%inverted_symop(i) = j
               exit
            end if
         end do
      end do
      self%n_inverted_symops = n
     STOP_TIMER("CRYSTAL:make_inverted_symops")
      UNSTACK
   end subroutine

   subroutine make_translated_symops(self)
    CRYSTAL :: self
   ! Determine which of the reduced symops can generate geometries from
   ! ".fragment_cell_geometry" which are related by translation, (including
   ! translation by the zero vector), but not inversion
      INT :: i,j
      BIN :: translated
      STACK("CRYSTAL:make_translated_symops")
      START_TIMER("CRYSTAL:make_translated_symops")
      ENSURE(associated(self%inverted_symop),"CRYSTAL:make_translated_symops ... no inverted_symop array")
      ENSURE(associated(self%spacegroup%seitz),"CRYSTAL:make_translated_symops ... Seitz matrices not initialised")
      call create_(self%translated_symop,self%spacegroup%n_seitz)
      self%translated_symop = 0
      do i = 1,self%spacegroup%n_seitz
         self%translated_symop(i) = i
         if (self%inverted_symop(i)>0) cycle
         do j = 1,i-1
            translated = equals_(self%spacegroup%seitz(1:3,1:3,i), &
                         self%spacegroup%seitz(1:3,1:3,j) )
            if (translated) then
               self%translated_symop(i) = j
               exit
            end if
         end do
      end do
     STOP_TIMER("CRYSTAL:make_translated_symops")
      UNSTACK
   end subroutine

   subroutine make_unique_SF_symops(self)
    CRYSTAL :: self
   ! Determine which are the structure-factor unique symops, i.e. those which
   ! generate fragment geometries which are different than a translation or
   ! inversion. This list should be a superset of the reduced symops.
      INT :: n,i
      STACK("CRYSTAL:make_unique_SF_symops")
      START_TIMER("CRYSTAL:make_unique_SF_symops")
      ENSURE(associated(self%inverted_symop),"CRYSTAL:make_unique_SF_symops ... no inverted_symop array")
      ENSURE(associated(self%translated_symop),"CRYSTAL:make_unique_SF_symops ... no translated_symop array")
      n = 0
      do i = 1,self%spacegroup%n_seitz
         if (self%inverted_symop(i)>0) cycle
         if (self%translated_symop(i)<i) cycle ! Only true translations count
         n = n + 1
      end do
      self%n_unique_SF_symops = n
      call create_(self%unique_SF_symop,n)
      n = 0
      do i = 1,self%spacegroup%n_seitz
         if (self%inverted_symop(i)>0) cycle
         if (self%translated_symop(i)<i) cycle
         n = n + 1
         self%unique_SF_symop(n) = i
      end do
     STOP_TIMER("CRYSTAL:make_unique_SF_symops")
      UNSTACK
   end subroutine

   function is_same_geometry(self,geom_i,geom_j) result(res)
    CRYSTAL :: self
   ! Return TRUE if the geometries "geom_i" and "geom_j" in fractional
   ! coordinates are the same, exactly.
      REALMAT(:,:) :: geom_i,geom_j
      BIN :: res
      INT :: i,j,n_atom
      BIN :: same
      BINVEC(:), PTR :: skip
      STACK("CRYSTAL:is_same_geometry")
      START_TIMER("CRYSTAL:is_same_geometry")
      ENSURE(associated(self%spacegroup%seitz),"CRYSTAL:is_same_geometry ... Seitz matrices not initialised")
      ENSURE(size(geom_i,1)==3,"CRYSTAL:is_same_geometry ... incorrect size for array geom_i")
      ENSURE(size(geom_j,1)==3,"CRYSTAL:is_same_geometry ... incorrect size for array geom_j")
      ENSURE(size(geom_i,2)==size(geom_j,2),"CRYSTAL:is_same_geometry ... incompatible sizes for geom_i, geom_j")
      n_atom = size(geom_i,2)
      call create_(skip,n_atom); skip(:) = FALSE
      do i = 1,n_atom
         do j = 1,n_atom
            same = same_as_(geom_i(:,i),geom_j(:,j), TOL(3))
            if (NOT same OR skip(j)) cycle
            skip(j) = TRUE
            exit
         end do
      end do
      res = all(skip) ! True if all atoms in i were matched (skipped) in j
      call destroy_(skip)
     STOP_TIMER("CRYSTAL:is_same_geometry")
      CHECK
   end function

   subroutine make_unique_fragment_atoms(self)
    CRYSTAL :: self
   ! Make a list of the symmetry unique atoms in the ".fragment_geometry". The
   ! atoms in ".fragment_geometry" are first put to the unit cell; then we
   ! transform each of them in turn by all the symmetry operations (placing them
   ! back into the unit cell, if necessary) and accumulating only the unique
   ! ones.
      REALMAT(:,:), PTR :: geometry
      REALVEC(3) :: pa
      INT :: u,a,s,col
      BIN :: found
      STACK("CRYSTAL:make_unique_fragment_atoms")
      START_TIMER("CRYSTAL:make_unique_fragment_atoms")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:make_unique_fragment_atoms ... no fragment_geometry")
      call create_(self%unique_atom_for_fragment_atom,self%n_fragment_atoms)
      self%unique_atom_for_fragment_atom    = 0
      self%unique_atom_for_fragment_atom(1) = 1
      call create_(self%unique_fragment_atom,1)
      self%unique_fragment_atom(1) = 1
      call create_(self%unique_symop_for_fragment_atom,self%n_fragment_atoms)
      self%unique_symop_for_fragment_atom = 1
      call create_(geometry,3,self%n_fragment_atoms)
      geometry = self%fragment_geometry
      call put_to_unit_cell_(self,geometry)
      u = 1
      do a = 2,self%n_fragment_atoms
         found = FALSE
         do s = 1,self%spacegroup%n_seitz
            pa = geometry(:,a)
            call transform_position_(self,pa,s,to_unit_cell=TRUE)
            found = has_column_(geometry(:,self%unique_fragment_atom),pa,TOL(3),col)
            if (self%unique_atom_for_fragment_atom(a)==0 AND found) then
               self%unique_atom_for_fragment_atom(a) = col
               self%unique_symop_for_fragment_atom(a) = s
               exit
            end if
         end do
         if (NOT found) then
            u = u + 1
            self%unique_atom_for_fragment_atom(a) = u
            call expand_(self%unique_fragment_atom,u)
            self%unique_fragment_atom(u) = a
         end if
      end do
      self%n_unique_fragment_atoms = u
      call destroy_(geometry)
     STOP_TIMER("CRYSTAL:make_unique_fragment_atoms")
      UNSTACK
   end subroutine

   subroutine make_fragment_cell_geometry(self)
    CRYSTAL :: self
   ! Get the all the fragment atom positions in the unit cell,
   ! ".fragment_cell_geometry", given a (possibly) partial or overcomplete set
   ! for the cell in array ".fragment_geometry".
      REALMAT(:,:), PTR :: geometry
      REALVEC(3) :: pa
      INT :: n,a,s
      BIN :: found
      STACK("CRYSTAL:make_fragment_cell_geometry")
      START_TIMER("CRYSTAL:make_fragment_cell_geometry")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:make_fragment_cell_geometry ... no fragment_geometry")
      nullify(self%fragment_cell_geometry)
      call create_(geometry,3,self%n_fragment_atoms) ! added this
      geometry = self%fragment_geometry ! added this
      call put_to_unit_cell_(self,geometry) ! added this
      n = 0
      do a = 1,self%n_fragment_atoms
         do s = 1,self%spacegroup%n_seitz
            pa = geometry(:,a) ! added this
            call transform_position_(self,pa,s,to_unit_cell=TRUE)
            if (NOT associated(self%fragment_cell_geometry)) then
              found = FALSE
            else
              found = has_column_(self%fragment_cell_geometry,pa,TOL(3))
            end if
            if (NOT found) then
               n = n + 1
               if (n==1) then
                 call create_(self%atom_for_fragment_cell_atom,1)
                 call create_(self%symop_for_fragment_cell_atom,1)
                 call create_(self%fragment_cell_geometry,3,1)
               else
                 call expand_(self%atom_for_fragment_cell_atom,n)
                 call expand_(self%symop_for_fragment_cell_atom,n)
                 call expand_(self%fragment_cell_geometry,3,n)
               end if
               self%atom_for_fragment_cell_atom(n) = a
               self%symop_for_fragment_cell_atom(n) = s
               self%fragment_cell_geometry(:,n) = pa
            end if
         end do
      end do
      self%n_fragment_cell_atoms = n
      call destroy_(geometry)
      ENSURE(associated(self%fragment_cell_geometry),"CRYSTAL:make_fragment_cell_geometry ... could not make fragment_cell-geometry")
     STOP_TIMER("CRYSTAL:make_fragment_cell_geometry")
      UNSTACK
   end subroutine

   subroutine make_unit_cell_geometry(self,prune_asymmetric_unit)
    CRYSTAL :: self
   ! Get the all the atom positions in the unit cell, ".unit_cell_geometry" from
   ! the ".asymmetric_unit_geometry", if it is created. Also checks if the
   ! ".asymmetric_unit_geometry" is really an asymmetric unit! If
   ! "prune_asymmetric_unit" is present and true, and non-asymmetric unit atoms
   ! are eliminated.
      BIN, optional :: prune_asymmetric_unit
      BIN :: prune
      REALVEC(3) :: pa
      INT :: n,a,m,s,col
      BIN :: found,non_unique
      REAL :: tol
      STACK("CRYSTAL:make_unit_cell_geometry")
      START_TIMER("CRYSTAL:make_unit_cell_geometry")
      ENSURE(associated(self%asymmetric_unit_geometry),"CRYSTAL:make_unit_cell_geometry ... no asymmetric unit geometry") 
      prune = FALSE
      if (present(prune_asymmetric_unit)) prune = prune_asymmetric_unit
      nullify(self%unit_cell_geometry)
      tol = TOL(3)/maxval(self%unitcell%length)
      n = 0
      a = 1
      do ! a = 1,.n_asymmetric_unit_atoms
         do s = 1,self%spacegroup%n_seitz
            pa = self%asymmetric_unit_geometry(:,a)
            call transform_position_(self,pa,s,to_unit_cell=TRUE)
            non_unique = has_column_(self%asymmetric_unit_geometry,pa,tol,col)
            non_unique = non_unique AND col/=a
            if (non_unique) then
               WARN("CRYSTAL:make_unit_cell_geometry ... asymmetric unit atoms "//trim(to_str_(a))//" and "//trim(to_str_(col))//" are the same!")
               WARN("CRYSTAL:make_unit_cell_geometry ... this may indicate a disordered aysmmetric unit")
               if (prune) then
                  m = max(a,col)
                  WARN("CRYSTAL:make_unit_cell_geometry ... pruning atom "//trim(to_str_(m))//" from asymmetric unit atom list")
                  self%n_asymmetric_unit_atoms = self%n_asymmetric_unit_atoms - 1
                  self%asymmetric_unit_geometry(:,m:self%n_asymmetric_unit_atoms) = self%asymmetric_unit_geometry(:,m+1:)
                  call shrink_columns_(self%asymmetric_unit_geometry,self%n_asymmetric_unit_atoms)
                  exit
               end if
            end if
            if (NOT associated(self%unit_cell_geometry)) then
              found = FALSE
            else
              found = has_column_(self%unit_cell_geometry,pa,TOL(3))
            end if
            if (NOT found) then
               n = n + 1
               if (n==1) then
                 call create_(self%atom_for_unit_cell_atom,1)
                 call create_(self%symop_for_unit_cell_atom,1)
                 call create_(self%unit_cell_geometry,3,1)
               else
                 call expand_(self%atom_for_unit_cell_atom,n)
                 call expand_(self%symop_for_unit_cell_atom,n)
                 call expand_(self%unit_cell_geometry,3,n)
               end if
               self%atom_for_unit_cell_atom(n) = a
               self%symop_for_unit_cell_atom(n) = s
               self%unit_cell_geometry(:,n) = pa
            end if
         end do
         a = a + 1
         if (a>self%n_asymmetric_unit_atoms) exit
      end do
      self%n_unit_cell_atoms = n
      ENSURE(associated(self%unit_cell_geometry),"CRYSTAL:make_unit_cell_geometry ... could not make unit_cell_geometry")
     STOP_TIMER("CRYSTAL:make_unit_cell_geometry")
      UNSTACK
   end subroutine

   subroutine make_repetition_factors(self)
    CRYSTAL :: self
   ! The number of times an atom with index "a" is mapped into itself under
   ! the reduced group is the ".repetition_factor(a)". It is used to correct
   ! structure factors for fragment geometries which are "oversampled" relative
   ! to the asymmetric cell geometry.
      REALVEC(3) :: pa,pb
      INT :: a,b,n,n_same
      BIN :: same
      STACK("CRYSTAL:make_repetition_factors")
      START_TIMER("CRYSTAL:make_repetition_factors")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:make_repetition_factors ... no fragment_geometry!")
      call create_(self%repetition_factor,self%n_fragment_atoms)
      do a = 1,self%n_fragment_atoms
         pa = self%fragment_geometry(:,a)
         call put_to_unit_cell_(self,pa)
         n_same = 0
         do b = 1,self%n_fragment_atoms
         do n = 1,self%spacegroup%n_seitz
            pb = self%fragment_geometry(:,b)
            call transform_position_(self,pb,n,to_unit_cell=TRUE)
            same = same_as_(pa,pb,TOL(3))
            if (same) n_same = n_same + 1
         end do
         end do
         self%repetition_factor(a) = n_same
      end do
     STOP_TIMER("CRYSTAL:make_repetition_factors")
      UNSTACK
   end subroutine

   subroutine transform_geometry(self,g,op,translate,ignore_glide,to_unit_cell)
    CRYSTAL :: self
   ! Transform the positions "g" in fractional coordinates with the
   ! Seitz operator with index "op". If present, "translate" will be
   ! added to the transformed position. If present and TRUE, "ignore_glide"
   ! will not add the glide vector part of the Seitz operator.
   ! If present and TRUE, "to_unit_cell" will translate the fractional
   ! coordinates into the (1,1,1) unit cell.
      REALMAT(:,:) :: g
      INT :: op
      INTVEC(3), optional :: translate
      BIN, optional :: ignore_glide,to_unit_cell
      INT :: n
      STACK("CRYSTAL:transform_geometry")
      START_TIMER("CRYSTAL:transform_geometry")
      ENSURE(size(g,1)==3,"CRYSTAL:transform_geometry ... incorrect size for array g")
      do n = 1,size(g,2)
         call transform_position_(self,g(:,n),op,translate,ignore_glide,to_unit_cell)
      end do
     STOP_TIMER("CRYSTAL:transform_geometry")
      CHECK
   end subroutine

   subroutine transform_position(self,p,op,translate,ignore_glide,to_unit_cell)
    CRYSTAL :: self
   ! Transform the position "p" in fractional coordinates with the
   ! Seitz operator with index "op". If present, "translate" will be
   ! added to the transformed position. If present and TRUE, "ignore_glide"
   ! will not add the glide vector part of the Seitz operator.
   ! If present and TRUE, "to_unit_cell" will translate the fractional
   ! coordinates into the (1,1,1) unit cell.
      REALVEC(3) :: p
      INT :: op
      INTVEC(3), optional :: translate
      BIN, optional :: ignore_glide,to_unit_cell
      BIN :: ignore,to_cell
      STACK("CRYSTAL:transform_position")
      START_TIMER("CRYSTAL:transform_position")
      ENSURE(associated(self%spacegroup%seitz),"CRYSTAL:transform_position ... Seitz matrices not initialised")
      ENSURE(op>0,"CRYSTAL:transform_position ... operator index out of bounds")
      ENSURE(op<=self%spacegroup%n_seitz,"CRYSTAL:transform_position ... operator index out of bounds")
      ignore = FALSE
      if (present(ignore_glide)) ignore = ignore_glide
      if (ignore) then
        p = matmul(self%spacegroup%seitz(1:3,1:3,op),p)
      else
        p = matmul(self%spacegroup%seitz(1:3,1:3,op),p) + self%spacegroup%seitz(1:3,4,op)
      end if
      if (present(translate)) p = p + translate
      to_cell = FALSE
      if (present(to_unit_cell)) to_cell = to_unit_cell
      if (to_cell) call put_to_unit_cell_(self,p)
     STOP_TIMER("CRYSTAL:transform_position")
      CHECK
   end subroutine

   subroutine put_to_unit_cell(self,g)
    CRYSTAL :: self
   ! Transform the geometry "g" in fractional coordinates into the
   ! (1,1,1) unit cell.  All atoms will be shifted into the first unit cell
   ! independently, so the resulting geometry may not reflect the shape of the
   ! original molecule.
      REALMAT(:,:) :: g
      INT :: n,n_atom
      STACK("CRYSTAL:put_to_unit_cell")
      START_TIMER("CRYSTAL:put_to_unit_cell")
      ENSURE( size(g,1)==3,"CRYSTAL:put_to_unit_cell ... incorrect size for array g")
      n_atom = size(g,2)
      do n = 1,n_atom
         call put_to_unit_cell_(self,g(:,n))
      end do
     STOP_TIMER("CRYSTAL:put_to_unit_cell")
      CHECK
   end subroutine

   subroutine put_to_unit_cell_1(self,p)
    CRYSTAL :: self
   ! Transform the position "p" in fractional coordinates into the
   ! (1,1,1) unit cell.
      REALVEC(3) :: p
      STACK("CRYSTAL:put_to_unit_cell_1")
      START_TIMER("CRYSTAL:put_to_unit_cell_1")
      ENSURE(size(p)==3,"CRYSTAL:put_to_unit_cell_1 ... p must be length 3")
      p(:) = mod(p(:)-floor(p(:))+TWO+0.001d0,ONE) - 0.001d0
      ! p(:)-floor(p(:))+TWO should make it positive.
     STOP_TIMER("CRYSTAL:put_to_unit_cell_1")
      CHECK
   end subroutine

   subroutine move_to_unit_cell(self,g)
    CRYSTAL :: self
   ! Transform the geometry "g" in fractional coordinates into the first
   ! lattice cell.  The shape of the molecule remains intact, so some of it
   ! may cross into other cells.  The center of the molecule will be in the
   ! first cell.
      REALMAT(:,:) :: g
      INT :: n,n_atom
      INTVEC(3) :: centre
      STACK("CRYSTAL:move_to_unit_cell")
      START_TIMER("CRYSTAL:move_to_unit_cell")
      ENSURE(size(g,1)==3,"CRYSTAL:move_to_unit_cell ... incorrect size for array g")
      n_atom = size(g,2)
      centre = unit_cell_offset_(self,g)
      do n = 1, n_atom
        g(:,n) = g(:,n) - centre
      end do
     STOP_TIMER("CRYSTAL:move_to_unit_cell")
      CHECK
   end subroutine

   function unit_cell_offset(self,g) result(res)
    CRYSTAL :: self
   ! Which hkl indices match the centre of the fragment geometry "g" when
   ! expressed in crystal coordinates.  Usually we expect that it is 0,0,0 but
   ! not always.
      REALMAT(:,:) :: g
      INTVEC(3) :: res
      STACK("CRYSTAL:unit_cell_offset")
      START_TIMER("CRYSTAL:unit_cell_offset")
      ENSURE(size(g,1)==3,"CRYSTAL:unit_cell_offset ... incorrect size for array g")
      res = sum(g,dim=2)/size(g,2)
     STOP_TIMER("CRYSTAL:unit_cell_offset")
      CHECK
   end function

   function fragment_width(self) result(res)
    CRYSTAL :: self
   ! Return the width "res" of the fragment in each of the 3 axis directions.
      REALVEC(3) :: res
      STACK("CRYSTAL:fragment_width")
      START_TIMER("CRYSTAL:fragment_width")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:fragment_width ... no fragment geometry")
      res = max_abs_column_difference_(self%fragment_geometry)
     STOP_TIMER("CRYSTAL:fragment_width")
      CHECK
   end function

   function cartesian_fragment_width(self) result(res)
    CRYSTAL :: self
   ! Return the cartesian width "res" of the fragment in each of the three axis
   ! directions.
      REALVEC(3) :: res
      STACK("CRYSTAL:cartesian_fragment_width")
      START_TIMER("CRYSTAL:cartesian_fragment_width")
      res = fragment_width_(self)
      call change_from_fractional_(self%unitcell,res)
     STOP_TIMER("CRYSTAL:cartesian_fragment_width")
      CHECK
   end function

   function reduced_symop_mat(self,r) result(res)
    CRYSTAL :: self
   ! Return the "r"-th reduced symop matrix in the unique list made by routine
   ! ".make_reduced_symops".
      REALMAT(3,3) :: res
      INT :: r
      STACK("CRYSTAL:reduced_symop_mat")
      START_TIMER("CRYSTAL:reduced_symop_mat")
      ENSURE(associated(self%reduced_symop),"CRYSTAL:reduced_symop_mat ... no reduced_symops!")
      ENSURE(r<=self%n_reduced_symops,"CRYSTAL:reduced_symop_mat ... symop index out of range")
      ENSURE(r>0,"CRYSTAL:reduced_symop_mat ... symop index out of range")
      res = self%spacegroup%seitz(1:3,1:3,self%reduced_symop(r))
     STOP_TIMER("CRYSTAL:reduced_symop_mat")
      CHECK
   end function

   function unique_SF_symop_mat(self,u) result(res)
    CRYSTAL :: self
   ! Return the "u"-th reduced symop matrix in the unique list made by routine
   ! ".make_reduced_symops".
      REALMAT(3,3) :: res
      INT :: u
      INT :: r
      STACK("CRYSTAL:unique_SF_symop_mat")
      START_TIMER("CRYSTAL:unique_SF_symop_mat")
      ENSURE(associated(self%unique_SF_symop),"CRYSTAL:unique_SF_symop_mat ... no unique_SF_symops!")
      ENSURE(u<=self%n_unique_SF_symops,"CRYSTAL:unique_SF_symop_mat ... symop index out of range")
      ENSURE(u>0,"CRYSTAL:unique_SF_symop_mat ... symop index out of range")
      r = self%unique_SF_symop(u)
      res = self%spacegroup%seitz(1:3,1:3,r)
   !  res = .spacegroup.seitz(1:3,1:3,.reduced_symop(r))
     STOP_TIMER("CRYSTAL:unique_SF_symop_mat")
      CHECK
   end function

   function transposed_xyz_seitz_matrices(self) result(res)
    CRYSTAL :: self
   ! Create and return a list of the 3x3 part of the seitz matrices
   ! in the cartesian axis system. NOTE: these are actually the
   ! *transpose* of the matrices in Hall's paper.
   !               S^T_cartesian  =  B  S^T_crystal  B^-1
      REALMAT3(:,:,:), PTR :: res
      INT :: i
      STACK("CRYSTAL:transposed_xyz_seitz_matrices")
      START_TIMER("CRYSTAL:transposed_xyz_seitz_matrices")
      ENSURE(associated(self%spacegroup%seitz),"CRYSTAL:transposed_xyz_seitz_matrices ... no Seitz matrices")
      call create_(res,3,3,self%spacegroup%n_seitz)
      do i = 1,self%spacegroup%n_seitz
         res(:,:,i) = matmul(self%unitcell%reciprocal_matrix, &
                      matmul(transpose(self%spacegroup%seitz(1:3,1:3,i)), &
                             transpose(self%unitcell%direct_matrix)))
      end do
     STOP_TIMER("CRYSTAL:transposed_xyz_seitz_matrices")
      UNSTACK
   end function

!  **************
!  Output methods
!  **************

   subroutine put(self,atom)
    CRYSTAL :: self
   ! Put out the crystal data to file "out"
      ATOMVEC(:), optional :: atom
      STACK("CRYSTAL:put")
      START_TIMER("CRYSTAL:put")
      call flush_(stdout)
      call text_(stdout,"CRYSTAL information:")
      call show_(stdout,"kind                        = ", self%data_kind)
      call put_(self%unitcell)
      call put_(self%spacegroup)
      if (associated(self%asymmetric_unit_geometry)) call put_asymmetric_unit_geometry_(self,atom)
      if (associated(self%unit_cell_geometry))       call put_unit_cell_geometry_(self,atom)
      if (associated(self%fragment_geometry))        call put_fragment_data_(self,atom)
      if (associated(self%reflections))              call put_reflection_data_(self)
     STOP_TIMER("CRYSTAL:put")
      CHECK
   end subroutine

   subroutine put_stl(self)
    CRYSTAL :: self
   ! Output sin(theta)/lambda for all reflections.
     INT :: n
     STACK("CRYSTAL:put_stl")
     START_TIMER("CRYSTAL:put_stl")
     ENSURE(associated(self%reflections),"CRYSTAL:put_stl ... No list of reflections")
     ENSURE(have_indices_(self%reflections),"CRYSTAL:put_stl ... No list of reflections")
     call text_(stdout,"sin(theta)/lambda for the reflections")
     call dash_(stdout,int_fields=3,real_fields=1)
     call put_(stdout,"h",int_width=TRUE)
     call put_(stdout,"k",int_width=TRUE)
     call put_(stdout,"l",int_width=TRUE)
     call put_(stdout,"stl")
     call flush_(stdout)
     call dash_(stdout,int_fields=3,real_fields=1)
     do n=1,size(self%reflections)
       call put_(stdout,self%reflections(n)%h)
       call put_(stdout,self%reflections(n)%k)
       call put_(stdout,self%reflections(n)%l)
       call put_(stdout,stl_(self,n))
       call flush_(stdout)
     end do
     call dash_(stdout,int_fields=3,real_fields=1)
     STOP_TIMER("CRYSTAL:put_stl")
      CHECK
   end subroutine

   subroutine put_fragment_data(self,atom)
    CRYSTAL :: self
   ! Put fragment information to file "out". Optional "atom" list may
   ! be used to enhace output.
      ATOMVEC(:), optional :: atom
      STACK("CRYSTAL:put_fragment_data")
      START_TIMER("CRYSTAL:put_fragment_data")
      call flush_(stdout)
      call text_(stdout,"Crystal fragment data:")
      call flush_(stdout)
      call show_(stdout,"No. of inputted atoms        = ",self%n_fragment_atoms,real_width=TRUE)
      call show_(stdout,"No. of fragment cell atoms   = ",self%n_fragment_cell_atoms,real_width=TRUE)
      call show_(stdout,"No. of unique fragment atoms = ",self%n_unique_fragment_atoms,real_width=TRUE)
      call show_(stdout,"Z factor                     = ",self%Z)
      call flush_(stdout)
      call show_(stdout,"Fragment partition model     = ",self%partition_model)
      call show_(stdout,"Thermal smearing model       = ",self%thermal_smearing_model)
    ! .put_reduced_symop_data
      call put_inv_trans_symop_data_(self)
      call put_fragment_geometry_(self,atom)
      call put_unique_fragment_geometry_(self,atom)
      call put_repetition_factors_(self,atom)
     STOP_TIMER("CRYSTAL:put_fragment_data")
      CHECK
   end subroutine

   subroutine put_reduced_symop_data(self)
    CRYSTAL :: self
   ! Put out the reduced symop data
      INT :: n,s
      BIN :: inverted,translated
      STACK("CRYSTAL:put_reduced_symop_data")
      START_TIMER("CRYSTAL:put_reduced_symop_data")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:put_reduced_symop_data ... fragment_geometry?")
      ENSURE(associated(self%unique_atom_for_fragment_atom),"CRYSTAL:put_reduced_symop_data ... no unique_atom_for_fragment_atom array!")
      call flush_(stdout)
      call text_(stdout,"Crystal fragment reduced group information:")
      call flush_(stdout)
      call text_(stdout,"NOTE: this table is NOT used any more in structure factor calculations")
      call flush_(stdout)
      call dash_(stdout,int_fields=5)
      call put_(stdout,"Reduced",int_width=TRUE)
      call put_(stdout,"Seitz",int_width=TRUE)
      call put_(stdout,"Inv.",int_width=TRUE)
      call put_(stdout,"Trans.",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Symop",int_width=TRUE)
      call put_(stdout,"Symop",int_width=TRUE)
      call put_(stdout,"of?",int_width=TRUE)
      call put_(stdout,"of?",int_width=TRUE)
      call put_(stdout,"Unique?",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=5)
      do n = 1,self%n_reduced_symops
         call put_(stdout,n)
         s = self%reduced_symop(n)
         call put_(stdout,s)
         inverted = self%inverted_symop(s)>0
         if (inverted) then;   call put_(stdout,self%inverted_symop(s))
         else;                 call tab_(stdout,int_fields=1)
         end if
         translated = self%translated_symop(s)<n
         call put_(stdout,self%translated_symop(s))
         if (inverted OR translated) then; call put_(stdout,"No",int_width=TRUE)
         else;                             call put_(stdout,"Yes",int_width=TRUE)
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=5)
     STOP_TIMER("CRYSTAL:put_reduced_symop_data")
      CHECK
   end subroutine

   subroutine put_inv_trans_symop_data(self)
    CRYSTAL :: self
   ! Put out the inverted translated symop data
      INT :: n
      BIN :: inverted,translated
      STACK("CRYSTAL:put_inv_trans_symop_data")
      START_TIMER("CRYSTAL:put_inv_trans_symop_data")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:put_inv_trans_symop_data ... fragment_geometry?")
      ENSURE(associated(self%unique_atom_for_fragment_atom),"CRYSTAL:put_inv_trans_symop_data ... no unique_atom_for_fragment_atom array")
      call flush_(stdout)
      call text_(stdout,"Crystal Inversion/Translation related symop information:")
      call flush_(stdout)
      call text_(stdout,"NOTE: this table IS used in structure factor calculations")
      call flush_(stdout)
      call dash_(stdout,int_fields=4)
      call put_(stdout,"Seitz",int_width=TRUE)
      call put_(stdout,"Inv.",int_width=TRUE)
      call put_(stdout,"Trans.",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Symop",int_width=TRUE)
      call put_(stdout,"of?",int_width=TRUE)
      call put_(stdout,"of?",int_width=TRUE)
      call put_(stdout,"Unique?",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=4)
      do n = 1,self%spacegroup%n_seitz
         call put_(stdout,n)
         inverted = self%inverted_symop(n)>0
         if (inverted) then;   call put_(stdout,self%inverted_symop(n))
         else;                 call tab_(stdout,int_fields=1)
         end if
         translated = self%translated_symop(n)<n
         call put_(stdout,self%translated_symop(n))
         if (inverted OR translated) then; call put_(stdout,"No",int_width=TRUE)
         else;                             call put_(stdout,"Yes",int_width=TRUE)
         end if
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=4)
     STOP_TIMER("CRYSTAL:put_inv_trans_symop_data")
      CHECK
   end subroutine

   subroutine put_fragment_geometry(self,atom)
    CRYSTAL :: self
   ! Put out the fragment_geometry information
      ATOMVEC(:), optional :: atom
      STR(STR_SIZE) :: symbol
      INT :: n
      STACK("CRYSTAL:put_fragment_geometry")
      START_TIMER("CRYSTAL:put_fragment_geometry")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:put_fragment_geometry ... fragment_geometry?")
      ENSURE(associated(self%unique_atom_for_fragment_atom),"CRYSTAL:put_fragment_geometry ... no unique_atom_for_fragment_atom array")
      call flush_(stdout)
      call text_(stdout,"Crystal fragment cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Fragment",int_width=TRUE)
      call put_(stdout,"Unique",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do n = 1,self%n_fragment_atoms
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=TRUE)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%unique_atom_for_fragment_atom(n))
         call put_(stdout,self%fragment_geometry(1,n))
         call put_(stdout,self%fragment_geometry(2,n))
         call put_(stdout,self%fragment_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)
     STOP_TIMER("CRYSTAL:put_fragment_geometry")
      CHECK
   end subroutine

   subroutine put_unique_fragment_geometry(self,atom)
    CRYSTAL :: self
   ! Put out the unique fragment_geometry information.  These are unique atoms
   ! in fragment_geometry, and so are not necessarily the same as the asymmetric
   ! unit atoms.
      ATOMVEC(:), optional :: atom
      INT :: n,u
      STR(STR_SIZE) :: symbol
      STACK("CRYSTAL:put_unique_fragment_geometry")
      START_TIMER("CRYSTAL:put_unique_fragment_geometry")
      ENSURE(associated(self%unique_fragment_atom),"CRYSTAL:put_unique_fragment_geometry ... unique_fragment_atoms?")
      ENSURE(associated(self%fragment_geometry),"CRYSTAL:put_unique_fragment_geometry ... fragment_geometry?")
      call flush_(stdout)
      call text_(stdout,"Crystal unique atom unit cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Unique",int_width=TRUE)
      call put_(stdout,"Fragment",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do u = 1,self%n_unique_fragment_atoms
         call put_(stdout,u)
         n = self%unique_fragment_atom(u)
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=TRUE)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%fragment_geometry(1,n))
         call put_(stdout,self%fragment_geometry(2,n))
         call put_(stdout,self%fragment_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)
     STOP_TIMER("CRYSTAL:put_unique_fragment_geometry")
      CHECK
   end subroutine

   subroutine put_fragment_cell_geometry(self,atom)
    CRYSTAL :: self
   ! Put out the full fragment cell geometry information
      ATOMVEC(:), optional :: atom
      INT :: n,f
      STR(STR_SIZE) :: symbol
      STACK("CRYSTAL:put_fragment_cell_geometry")
      START_TIMER("CRYSTAL:put_fragment_cell_geometry")
      call flush_(stdout)
      call text_(stdout,"Full fragment cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      call put_(stdout,"Cell ",int_width=TRUE)
      call put_(stdout,"Fragment",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"symop",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      do n = 1,self%n_fragment_cell_atoms
         call put_(stdout,n)
         if (present(atom)) then
            f = self%atom_for_fragment_cell_atom(n)
            symbol = trim(chemical_symbol_(atom(f))) // " (" // trim(to_str_(f)) // ")"
         else
            call put_(stdout,f)
         end if
         call put_(stdout,symbol,int_width=TRUE)
         call put_(stdout,self%fragment_cell_geometry(1,n))
         call put_(stdout,self%fragment_cell_geometry(2,n))
         call put_(stdout,self%fragment_cell_geometry(3,n))
         call put_(stdout,self%symop_for_fragment_cell_atom(n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=3)
     STOP_TIMER("CRYSTAL:put_fragment_cell_geometry")
      CHECK
   end subroutine

   subroutine put_repetition_factors(self,atom)
    CRYSTAL :: self
   ! Put out the repetition factors
      ATOMVEC(:), optional :: atom
      INT :: n,rf
      STR(STR_SIZE) :: symbol
      STACK("CRYSTAL:put_repetition_factors")
      START_TIMER("CRYSTAL:put_repetition_factors")
      call flush_(stdout)
      call text_(stdout,"Crystal fragment atom repetition factors:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2)
      call put_(stdout,"Fragment",int_width=TRUE)
      call put_(stdout,"Rep.",int_width=TRUE)
      call flush_(stdout)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Factor",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=2)
      do n = 1,self%n_fragment_atoms
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=TRUE)
         else
            call put_(stdout,n)
         end if
         rf = self%repetition_factor(n)
         call put_(stdout,rf)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2)
     STOP_TIMER("CRYSTAL:put_repetition_factors")
      CHECK
   end subroutine

   subroutine put_F_calc(self)
    CRYSTAL :: self
   ! Put the hkl Miller index triple followed by the calculated
   ! structure factor to add onto existing structure factors.
      STACK("CRYSTAL:put_F_calc")
      START_TIMER("CRYSTAL:put_F_calc")
      ENSURE(associated(self%reflections),"CRYSTAL:put_F_calc ... no reflection data")
      call set_keys_(self%reflections,(/"indices","F_calc "/))
      call put_(self%reflections)
      call clear_keys_(self%reflections)
     STOP_TIMER("CRYSTAL:put_F_calc")
      CHECK
   end subroutine

   subroutine put_F_stats(self)
    CRYSTAL :: self
   ! Output the structure factor goodness of fit statistics
   ! stdout.
      STACK("CRYSTAL:put_F_stats")
      START_TIMER("CRYSTAL:put_F_stats")
      call put_F_stats_(self%reflections)
      call put_correction_data_(self)
     STOP_TIMER("CRYSTAL:put_F_stats")
      CHECK
   end subroutine

   subroutine put_I_stats(self)
    CRYSTAL :: self
   ! Output the intensity goodness of fit statistics
      STACK("CRYSTAL:put_I_stats")
      START_TIMER("CRYSTAL:put_I_stats")
      call put_I_stats_(self%reflections)
      call put_correction_data_(self)
     STOP_TIMER("CRYSTAL:put_I_stats")
      CHECK
   end subroutine

   subroutine put_correction_data(self)
    CRYSTAL :: self
   ! Output the correction data
     BIN :: real_width
     STACK("CRYSTAL:put_correction_data")
     START_TIMER("CRYSTAL:put_correction_data")
     real_width = TRUE
     call flush_(stdout)
     call show_(stdout,"Using scale factor          = ", self%optimise_scale,real_width)
     call show_(stdout,"Using extinction            = ", self%optimise_extinction,real_width)
     call show_(stdout,"Thermal smearing model      = ", self%thermal_smearing_model)
     call show_(stdout,"Fragment partition model    = ", self%partition_model)
     call show_(stdout,"Correct dispersion?         = ", self%correct_dispersion,real_width=TRUE)
     if (self%optimise_extinction) then;
     call show_(stdout,"Optimize extinction         = ", TRUE,real_width)
     call show_(stdout,"Secondary extinction factor = ", self%extinction_factor)
     else
     call show_(stdout,"Optimize extinction         = ", FALSE,real_width)
     end if
     if (self%optimise_scale) then
     call show_(stdout,"Optimize scale factor       = ", TRUE,real_width)
     call show_(stdout,"Scale factor                = ", self%scale_factor)
     else
     call show_(stdout,"Optimize scale factor       = ", FALSE,real_width)
     end if
     STOP_TIMER("CRYSTAL:put_correction_data")
      CHECK
   end subroutine

   subroutine put_reflection_data(self)
    CRYSTAL :: self
   ! Output the reflection data t
     STACK("CRYSTAL:put_reflection_data")
     START_TIMER("CRYSTAL:put_reflection_data")
     ENSURE(associated(self%reflections),"CRYSTAL:put_reflection_data ... no reflections")
     if (have_F_calc_(self) OR have_F_pred_(self)) call put_correction_data_(self)
     call put_(self%reflections)
     STOP_TIMER("CRYSTAL:put_reflection_data")
      CHECK
   end subroutine

   subroutine put_reflection_phases(self)
    CRYSTAL :: self
   ! Output the reflection phases
     STACK("CRYSTAL:put_reflection_phases")
     START_TIMER("CRYSTAL:put_reflection_phases")
     ENSURE(associated(self%reflections),"CRYSTAL:put_reflection_phases ... no reflection data")
     call set_keys_(self%reflections,(/"indices","F_calc ","F_phase"/))
     call put_(self%reflections)
     call clear_keys_(self%reflections)
     STOP_TIMER("CRYSTAL:put_reflection_phases")
      CHECK
   end subroutine

   subroutine put_qq_plot(self,name)
    CRYSTAL :: self
   ! Output a qq plot to the text file.
   ! It is a plot of the experimental quantile vs expected quantile.
     STR(STR_SIZE), optional :: name
     STACK("CRYSTAL:put_qq_plot")
     START_TIMER("CRYSTAL:put_qq_plot")
     ENSURE(associated(self%reflections),"CRYSTAL:put_qq_plot ... no reflection data")
     call put_F_qq_plot_(self%reflections,name)
     STOP_TIMER("CRYSTAL:put_qq_plot")
      CHECK
   end subroutine

   subroutine put_labelled_qq_plot(self,name)
    CRYSTAL :: self
   ! Output a qq plot to the text file.
   ! It is a plot of the experimental quantile vs expected quantile.
     STR(STR_SIZE), optional :: name
   STACK("CRYSTAL:put_labelled_qq_plot")
   START_TIMER("CRYSTAL:put_labelled_qq_plot")
   ENSURE(associated(self%reflections),"CRYSTAL:put_labelled_qq_plot ... no reflection data")
     call put_labelled_F_qq_plot_(self%reflections,name)
     STOP_TIMER("CRYSTAL:put_labelled_qq_plot")
      CHECK
   end subroutine

   subroutine put_chi2_vs_angle_plot(self)
    CRYSTAL :: self
   ! Output a table with the chi^2 for the structure factor data set broken
   ! into sections.
   ! Reads from stdin the number of divisions in the plot.
     INT :: num_sections
     INT :: n,num,k,n_refl
     INTVEC(:), PTR :: section_for
     REAL :: stl,stl_min,stl_max,chi2,width,stl_mean

     STACK("CRYSTAL:put_chi2_vs_angle_plot")
     START_TIMER("CRYSTAL:put_chi2_vs_angle_plot")
     call read_(stdin,num_sections)
   ENSURE(num_sections > 0,"CRYSTAL:put_chi2_vs_angle_plot ... number of sections to plot not positive")
   ENSURE(associated(self%reflections),"CRYSTAL:put_chi2_vs_angle_plot ... no structure factors")
     n_refl = n_refl_(self%reflections)

     ! Work out the limits of the sin(theta)/lambda.
     stl_min = 1000
     stl_max = ZERO
     do n=1, n_refl
       stl = stl_(self,n) * BOHR_PER_ANGSTROM
       if (stl < stl_min) stl_min = stl
       if (stl > stl_max) stl_max = stl
     end do
     width = (TOL(3) + stl_max - stl_min)/num_sections

     call create_(section_for,n_refl)
     ! Determine which section each reflection belongs to.
     do n=1, n_refl
       stl = stl_(self,n) * BOHR_PER_ANGSTROM
       section_for(n) = ceiling((TOL(3) + stl - stl_min) / width)
     end do

     call flush_(stdout)
     call text_(stdout,"Chi^2 vs angle plot")
     call flush_(stdout)
     call text_(stdout,"sin(theta)/lambda in Angstrom^(-1)")
     call show_(stdout,"Smallest sin(theta)/lambda  = ",stl_min)
     call show_(stdout,"Largest sin(theta)/lambda   = ",stl_max)
     call flush_(stdout)
     call put_(stdout,"stl")
     call put_(stdout,"chi^2",flush=1)
     call dash_(stdout,real_fields=2)
     do n = 1, num_sections
       chi2 = ZERO
       num = 0
       stl_mean = stl_min + (n - HALF) * width
       do k = 1, n_refl
         if (n==section_for(k)) then
           chi2 = chi2 + F_z2_(self%reflections(k))
           num = num + 1
         end if
       end do
       call put_(stdout,stl_mean)
       if (num==0) then
         call flush_(stdout)
         cycle
       else
         chi2 = chi2 / max(num - self%n_param,1)
         call put_(stdout,chi2,flush=1)
       end if
     end do
     call flush_(stdout)
     call destroy_(section_for)
     STOP_TIMER("CRYSTAL:put_chi2_vs_angle_plot")
      CHECK
   end subroutine

   subroutine put_fcalc_plots(self)
    CRYSTAL :: self
   ! Output some different plots about the calculated structure factors.
     REALVEC(:), PTR :: ext
     REFLECTIONVEC(:), PTR :: ext_refs
     INT :: n,n_refl
     REAL :: y,w

     STACK("CRYSTAL:put_fcalc_plots")
     START_TIMER("CRYSTAL:put_fcalc_plots")
     n_refl = n_refl_(self%reflections)

     call text_(stdout,"The effects of angle.")
     call text_(stdout,"Scatter plot of (Fexp-Fpred)/F_sigma vs sin(theta)/lambda")
     do n=1,n_refl
       call put_(stdout,stl_(self,n))
       call put_(stdout,F_z_(self%reflections(n)))
       call flush_(stdout)
     end do
     call flush_(stdout)

     call text_(stdout,"The effects of intensity.")
     call text_(stdout,"Scatter plot of (Fexp-Fpred)/F_sigma vs Fexp")
     do n=1,n_refl
       call put_(stdout,self%reflections(n)%F_exp)
       call put_(stdout,F_z_(self%reflections(n)))
       call flush_(stdout)
     end do
     call flush_(stdout)

     call create_(ext_refs,n_refl)
     call create_(ext,n_refl)
     ext = extinction_correction_(self)
     call set_F_exp_(ext_refs,self%reflections%F_pred)
     call set_F_pred_(ext_refs,self%reflections%F_pred * ext)
     call set_F_sigma_(ext_refs,self%reflections%F_sigma)
     call destroy_(ext)

     call text_(stdout,"The effects of extinction.")
     w = ZERO
     y = ZERO
     do n=1,n_refl
       y = y + abs(F_z_(ext_refs(n)))
       w = w + abs(F_r_(ext_refs(n)))
     end do
     y = y / n_refl
     w = w / n_refl
     call show_(stdout,"Average value of abs(Fcalc_ext-Fcalc)/F_sigma is ",y)
     call show_(stdout,"Average value of abs(extinction correction) is ",w)
     call flush_(stdout)

     call text_(stdout,"The effects of intensity on extinction.")
     call text_(stdout,"Scatter plot of (Fcalc_ext-Fcalc)/F_sigma vs Fpred")
     do n=1,n_refl
       call put_(stdout,ext_refs(n)%F_pred)
       call put_(stdout,F_z_(ext_refs(n)))
       call flush_(stdout)
     end do
     call flush_(stdout)

     call text_(stdout,"The effects of scattering angle on extinction.")
     call text_(stdout,"Scatter plot of (Fcalc_ext-Fcalc)/F_sigma vs sin(theta)/lambda")
     do n=1,n_refl
       call put_(stdout,stl_(self,n))
       call put_(stdout,F_z_(ext_refs(n)))
       call flush_(stdout)
     end do
     call destroy_(ext_refs)

     call flush_(stdout)
     STOP_TIMER("CRYSTAL:put_fcalc_plots")
      CHECK
   end subroutine

   subroutine make_crystal_error_map(self,map,pts)
    CRYSTAL :: self
   ! Make the crystal error "map" for the supplied points "pts" from the crystal
   ! structure factors
     REALVEC(:) :: map
     REALMAT(:,:) :: pts
      REALMAT(:,:), PTR :: k
     REALVEC(:), PTR :: F,phase
     INT :: n_pts,n_refl,n
     REAL :: fac
   STACK("CRYSTAL:make_crystal_error_map")
   START_TIMER("CRYSTAL:make_crystal_error_map")
   ENSURE(associated(self%reflections),"CRYSTAL:make_crystal_error_map ... no structure factors")
   ENSURE(have_F_calc_(self),"CRYSTAL:make_crystal_error_map ... no calculated structure factors")
   ENSURE(have_F_exp_(self),"CRYSTAL:make_crystal_error_map ... no experimental structure factors")
   ENSURE( size(pts,2)==3,"CRYSTAL:make_crystal_error_map ... incorrect dimension for points array")
     n_pts = size(pts,1)
     n_refl = n_refl_(self%reflections)
     call create_(k,n_refl,3); call make_k_pts_(self,k)
     call create_(F,n_refl)
     F = (self%reflections%F_exp - self%reflections%F_pred) * sign(ONE,real(self%reflections%F_calc))
     call create_(phase,n_refl)
     do n = 1,n_pts
        call to_product_of_(phase,k,pts(n:n,:),transpose_b=TRUE)
        map(n) = sum(F*cos(phase))
     end do
     fac = TWO/self%unitcell%volume
     map = fac*map
     call destroy_(phase)
     call destroy_(F)
     call destroy_(k)
     STOP_TIMER("CRYSTAL:make_crystal_error_map")
      CHECK
   end subroutine

   subroutine put_PND_sf(self,name)
    CRYSTAL :: self
   ! Output the magnetic structure factors
       STR(STR_SIZE) :: name
       CPXVEC(:), PTR :: FM_s,FM_l
       ARCHIVE :: arch
        INT :: n
       STACK("CRYSTAL:put_PND_sf")
       START_TIMER("CRYSTAL:put_PND_sf")
       call create_(FM_s,n_refl_(self%reflections))
       call set_(arch,name,"PND_spin_sf")
       call read_(arch,FM_s)
       call create_(FM_l,n_refl_(self%reflections))
       call set_(arch,name,"PND_nabla_sf")
       call read_(arch,FM_l)
       call text_(stdout,"PND magnetic structure factors:")
       call flush_(stdout)
       call dash_(stdout,int_fields=3,real_fields=3)
       call put_(stdout,"h", int_width=TRUE)
       call put_(stdout,"k", int_width=TRUE)
       call put_(stdout,"l", int_width=TRUE)
       call put_(stdout,"FM_s")
       call put_(stdout,"FM_l")
       call put_(stdout,"FM")
       call flush_(stdout)
       call dash_(stdout,int_fields=3,real_fields=3)
       do n = 1, n_refl_(self%reflections)
          call put_(stdout,self%reflections(n)%h)
          call put_(stdout,self%reflections(n)%k)
          call put_(stdout,self%reflections(n)%l)
          call put_(stdout,real(FM_s(n)) )
          call put_(stdout,real(FM_l(n)) )
          call put_(stdout,real(FM_s(n)+FM_l(n)) )
          call flush_(stdout)
       end do
       call dash_(stdout,int_fields=3,real_fields=3)
       call destroy_(FM_l)
       call destroy_(FM_s)
     STOP_TIMER("CRYSTAL:put_PND_sf")
      CHECK
   end subroutine

   subroutine put_asymmetric_unit_geometry(self,atom)
    CRYSTAL :: self
   ! Put out the asymmetric unit geometry.
      ATOMVEC(:), optional :: atom
      INT :: n
      STR(STR_SIZE) :: symbol
      STACK("CRYSTAL:put_asymmetric_unit_geometry")
      START_TIMER("CRYSTAL:put_asymmetric_unit_geometry")
      ENSURE(associated(self%asymmetric_unit_geometry),"CRYSTAL:put_asymmetric_unit_geometry ... no asymmetric unit atoms")
      call flush_(stdout)
      call text_(stdout,"Crystal asymmetric unit cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"ID",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do n = 1,self%n_asymmetric_unit_atoms
         call put_(stdout,n)
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(n))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=TRUE)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%asymmetric_unit_geometry(1,n))
         call put_(stdout,self%asymmetric_unit_geometry(2,n))
         call put_(stdout,self%asymmetric_unit_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)
     STOP_TIMER("CRYSTAL:put_asymmetric_unit_geometry")
      CHECK
   end subroutine

   subroutine put_unit_cell_geometry(self,atom)
    CRYSTAL :: self
   ! Put out the unit_cell_geometry.
      ATOMVEC(:), optional :: atom
      INT :: n,u
      STR(STR_SIZE) :: symbol
      STACK("CRYSTAL:put_unit_cell_geometry")
      START_TIMER("CRYSTAL:put_unit_cell_geometry")
      ENSURE(associated(self%unit_cell_geometry),"CRYSTAL:put_unit_cell_geometry ... no unit_cell_geometry")
      if (present(atom)) then
      ENSURE(size(atom)>=self%n_asymmetric_unit_atoms,"CRYSTAL:put_unit_cell_geometry ... atom array too small")
      end if
      call flush_(stdout)
      call text_(stdout,"Crystal unit cell geometry:")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"ID",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      do n = 1,self%n_unit_cell_atoms
         call put_(stdout,n)
         u = self%atom_for_unit_cell_atom(n)
         if (present(atom)) then
            symbol = trim(chemical_symbol_(atom(u))) // " (" // trim(to_str_(n)) // ")"
            call put_(stdout,symbol,int_width=TRUE)
         else
            call put_(stdout,n)
         end if
         call put_(stdout,self%unit_cell_geometry(1,n))
         call put_(stdout,self%unit_cell_geometry(2,n))
         call put_(stdout,self%unit_cell_geometry(3,n))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)
     STOP_TIMER("CRYSTAL:put_unit_cell_geometry")
      CHECK
   end subroutine

   subroutine put_CX(self,label)
    CRYSTAL :: self
   ! Output some information for the Crystal Explorer program.
       STR(STR_SIZE) :: label
       STACK("CRYSTAL:put_CX")
       START_TIMER("CRYSTAL:put_CX")
       call put_CX_(self%unitcell,label)
     STOP_TIMER("CRYSTAL:put_CX")
      CHECK
   end subroutine

!  *******************
!  Tests for existence
!  *******************

   function asymmetric_unit_exists(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if the asymmetric unit geometry information exists
     BIN :: res
     STACK("CRYSTAL:asymmetric_unit_exists")
     START_TIMER("CRYSTAL:asymmetric_unit_exists")
     res = associated(self%asymmetric_unit_geometry)
     STOP_TIMER("CRYSTAL:asymmetric_unit_exists")
      CHECK
   end function

   function unit_cell_geometry_exists(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if the unit_cell_geometry information exists
     BIN :: res
     STACK("CRYSTAL:unit_cell_geometry_exists")
     START_TIMER("CRYSTAL:unit_cell_geometry_exists")
     res = associated(self%unit_cell_geometry)
     STOP_TIMER("CRYSTAL:unit_cell_geometry_exists")
      CHECK
   end function

   function fragment_data_exists(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if a fragment information exists
     BIN :: res
     STACK("CRYSTAL:fragment_data_exists")
     START_TIMER("CRYSTAL:fragment_data_exists")
     res = associated(self%fragment_geometry)
     STOP_TIMER("CRYSTAL:fragment_data_exists")
      CHECK
   end function

   function reflection_data_exists(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if reflection data exists
     BIN :: res
     STACK("CRYSTAL:reflection_data_exists")
     START_TIMER("CRYSTAL:reflection_data_exists")
     res = associated(self%reflections)
     STOP_TIMER("CRYSTAL:reflection_data_exists")
      CHECK
   end function

   function have_F_exp(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if have some experimental structure factors
      BIN :: res
      STACK("CRYSTAL:have_F_exp")
      START_TIMER("CRYSTAL:have_F_exp")
      res = FALSE
      if (associated(self%reflections)) res = have_F_exp_(self%reflections)
     STOP_TIMER("CRYSTAL:have_F_exp")
      CHECK
   end function

   function have_F_calc(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if have some calculated structure factors
      BIN :: res
      STACK("CRYSTAL:have_F_calc")
      START_TIMER("CRYSTAL:have_F_calc")
      res = FALSE
      if (associated(self%reflections)) res = have_F_calc_(self%reflections)
     STOP_TIMER("CRYSTAL:have_F_calc")
      CHECK
   end function

   function have_F_pred(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if have some predicted structure factors
      BIN :: res
      STACK("CRYSTAL:have_F_pred")
      START_TIMER("CRYSTAL:have_F_pred")
      res = FALSE
      if (associated(self%reflections)) res = have_F_pred_(self%reflections)
     STOP_TIMER("CRYSTAL:have_F_pred")
      CHECK
   end function

   function have_F_sigma(self) result(res)
    CRYSTAL :: self
   ! Return TRUE if have some structure factor errors
      BIN :: res
      STACK("CRYSTAL:have_F_sigma")
      START_TIMER("CRYSTAL:have_F_sigma")
      res = FALSE
      if (associated(self%reflections)) res = have_F_sigma_(self%reflections)
     STOP_TIMER("CRYSTAL:have_F_sigma")
      CHECK
   end function

!*******************************************************************************
!                     Inherited reflection routines
!*******************************************************************************

   function n_refl(self) result(res)
    CRYSTAL :: self
   ! The number of reflections
     INT :: res
     STACK("CRYSTAL:n_refl")
     START_TIMER("CRYSTAL:n_refl")
     ENSURE(associated(self%reflections),"CRYSTAL:n_refl ... no reflection data")
     res = n_refl_(self%reflections)
     STOP_TIMER("CRYSTAL:n_refl")
      CHECK
   end function

   function F_calc(self) result(res)
    CRYSTAL :: self
   ! The calculated structure factors
     CPXVEC(size(self%reflections)) :: res
     STACK("CRYSTAL:F_calc")
     START_TIMER("CRYSTAL:F_calc")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_calc ... no reflection data")
     res = self%reflections%F_calc
     STOP_TIMER("CRYSTAL:F_calc")
      CHECK
   end function

   function F_pred(self) result(res)
    CRYSTAL :: self
   ! The predicted structure factors
     REALVEC(size(self%reflections)) :: res
     STACK("CRYSTAL:F_pred")
     START_TIMER("CRYSTAL:F_pred")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_pred ... no reflection data")
     res = self%reflections%F_pred
     STOP_TIMER("CRYSTAL:F_pred")
      CHECK
   end function

   function F_sigma(self) result(res)
    CRYSTAL :: self
   ! The structure factor errors
     REALVEC(size(self%reflections)) :: res
     STACK("CRYSTAL:F_sigma")
     START_TIMER("CRYSTAL:F_sigma")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_sigma ... no reflection data")
     res = self%reflections%F_sigma
     STOP_TIMER("CRYSTAL:F_sigma")
      CHECK
   end function

   function F_exp(self) result(res)
    CRYSTAL :: self
   ! The experimental structure factors
     REALVEC(size(self%reflections)) :: res
     STACK("CRYSTAL:F_exp")
     START_TIMER("CRYSTAL:F_exp")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_exp ... no reflection data")
     res = self%reflections%F_exp
     STOP_TIMER("CRYSTAL:F_exp")
      CHECK
   end function

   subroutine set_F_calc(self,F_calc)
    CRYSTAL :: self
   ! Set the calculated structure factors
     CPXVEC(:), IN :: F_calc
     STACK("CRYSTAL:set_F_calc")
     START_TIMER("CRYSTAL:set_F_calc")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:set_F_calc ... no reflection data")
     call set_F_calc_(self%reflections,F_calc)
     STOP_TIMER("CRYSTAL:set_F_calc")
      CHECK
   end subroutine

   function F_chi2(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:F_chi2")
     START_TIMER("CRYSTAL:F_chi2")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_chi2 ... no reflection data")
     res = F_chi2_(self%reflections)
     STOP_TIMER("CRYSTAL:F_chi2")
      CHECK
   end function

   function F_goodness_of_fit(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:F_goodness_of_fit")
     START_TIMER("CRYSTAL:F_goodness_of_fit")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_goodness_of_fit ... no reflection data")
     res = F_goodness_of_fit_(self%reflections)
     STOP_TIMER("CRYSTAL:F_goodness_of_fit")
      CHECK
   end function

   function F_r_factor(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:F_r_factor")
     START_TIMER("CRYSTAL:F_r_factor")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_r_factor ... no reflection data")
     res = F_r_factor_(self%reflections)
     STOP_TIMER("CRYSTAL:F_r_factor")
      CHECK
   end function

   function F_weighted_r_factor(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:F_weighted_r_factor")
     START_TIMER("CRYSTAL:F_weighted_r_factor")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:F_weighted_r_factor ... no reflection data")
     res = F_weighted_r_factor_(self%reflections)
     STOP_TIMER("CRYSTAL:F_weighted_r_factor")
      CHECK
   end function

   function I_chi2(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:I_chi2")
     START_TIMER("CRYSTAL:I_chi2")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:I_chi2 ... no reflection data")
     res = I_chi2_(self%reflections)
     STOP_TIMER("CRYSTAL:I_chi2")
      CHECK
   end function

   function I_goodness_of_fit(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:I_goodness_of_fit")
     START_TIMER("CRYSTAL:I_goodness_of_fit")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:I_goodness_of_fit ... no reflection data")
     res = I_goodness_of_fit_(self%reflections)
     STOP_TIMER("CRYSTAL:I_goodness_of_fit")
      CHECK
   end function

   function I_r_factor(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:I_r_factor")
     START_TIMER("CRYSTAL:I_r_factor")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:I_r_factor ... no reflection data")
     res = I_r_factor_(self%reflections)
     STOP_TIMER("CRYSTAL:I_r_factor")
      CHECK
   end function

   function I_weighted_r_factor(self) result(res)
    CRYSTAL :: self
   ! Returns data
     IN :: self
     REAL :: res
     STACK("CRYSTAL:I_weighted_r_factor")
     START_TIMER("CRYSTAL:I_weighted_r_factor")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:I_weighted_r_factor ... no reflection data")
     res = I_weighted_r_factor_(self%reflections)
     STOP_TIMER("CRYSTAL:I_weighted_r_factor")
      CHECK
   end function

   subroutine simulate_new_F_exp(self)
    CRYSTAL :: self
   ! Simulate a new experiment by adding normally distributed experimental
   ! errors the the F_exp.
     REALVEC(:), PTR :: F_exp
     INT :: n
     REAL :: chi2,z
     STACK("CRYSTAL:simulate_new_F_exp")
     START_TIMER("CRYSTAL:simulate_new_F_exp")
     ENSURE(reflection_data_exists_(self),"CRYSTAL:simulate_new_F_exp ... no reflection data")
     ENSURE(have_F_exp_(self),"CRYSTAL:simulate_new_F_exp ... no experimental structure factors")
     ENSURE(have_F_sigma_(self),"CRYSTAL:simulate_new_F_exp ... no experimental structure factor errors")
     call flush_(stdout)
     call text_(stdout,"Adding simulated errors to F_exp using F_sigma.")
     call create_(F_exp,n_refl_(self))
     F_exp = self%reflections%F_exp
     call simulate_new_F_exp_(self%reflections)
     chi2=ZERO
     do n=1,n_refl_(self)
       z=(F_exp(n)-self%reflections(n)%F_exp)/self%reflections(n)%F_sigma
       chi2 = chi2 + z*z
     end do
     chi2 = chi2 / max(n_refl_(self) - 1,1)
     call text_(stdout,"chi^2 of old F_exp to new F_exp is " // trim(to_str_(chi2)))
     call flush_(stdout)
     call dash_(stdout,real_fields=4)
     call put_(stdout,"F_exp (old)")
     call put_(stdout,"F_exp (new)")
     call put_(stdout,"F_sigma")
     call put_(stdout,"dF/sigma")
     call flush_(stdout)
     call dash_(stdout,real_fields=4)
     do n=1,n_refl_(self)
       z=(F_exp(n)-self%reflections(n)%F_exp)/self%reflections(n)%F_sigma
       call put_(stdout,F_exp(n))
       call put_(stdout,self%reflections(n)%F_exp)
       call put_(stdout,self%reflections(n)%F_sigma)
       call put_(stdout,z)
       call flush_(stdout)
     end do
     call dash_(stdout,real_fields=4)
     call destroy_(F_exp)
     STOP_TIMER("CRYSTAL:simulate_new_F_exp")
      CHECK
   end subroutine

   subroutine make_asymmetric_geometry(self)
    CRYSTAL :: self
   ! Set the asymmetric geometry array from the ".unique_fragment_atom" info.
   ! This should not be done if an ".asymmetric_unit_geometry" already exists!
     STACK("CRYSTAL:make_asymmetric_geometry")
     START_TIMER("CRYSTAL:make_asymmetric_geometry")
     ENSURE(NOT associated(self%asymmetric_unit_geometry),"CRYSTAL:make_asymmetric_geometry ... asymmetric unit already exists")
     ENSURE(associated(self%unique_fragment_atom),"CRYSTAL:make_asymmetric_geometry ... unique_fragment_atom list does not exist")
     call create_(self%asymmetric_unit_geometry,3,self%n_unique_fragment_atoms)
     self%asymmetric_unit_geometry = self%fragment_geometry(:,self%unique_fragment_atom)
     self%n_asymmetric_unit_atoms = self%n_unique_fragment_atoms
     STOP_TIMER("CRYSTAL:make_asymmetric_geometry")
      UNSTACK
   end subroutine

end
