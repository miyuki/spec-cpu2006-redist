!---------------------------------------------------------------------------
!
!  ROBY:
!
!  Read and evaluate Roby atom data for population and bond analysis.
!  This module has been completely rewritten from Chris Taylor's version,
!  based on Steve Wolff's checked version.
!
! Copyright (C) Chris Taylor, UWA, 1999.
! Copyright (C) Stephen Wolff, UWA, 2000-2001.
! Copyright (C) Dylan Jayatilaka, 2001
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
! $Id: roby.foo,v 1.2.2.4 2004/04/21 09:12:56 reaper Exp $
!---------------------------------------------------------------------------

module ROBY_MODULE

#  include "roby.use"

   implicit none

#  include "macros"
#  include "roby.int"


contains

!  *******************
!  Allocation routines
!  *******************

   subroutine create(self,mol,S)
    ROBY :: self
   ! Create space for the object
     PTR :: self
     MOL, optional :: mol
      REALMAT(:,:), PTR, optional :: S
     STACK("ROBY:create")
     START_TIMER("ROBY:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(ROBY_SIZE)
     call nullify_ptr_part_(self)
     call set_defaults_(self,mol,S)
     STOP_TIMER("ROBY:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    ROBY :: self
   ! Destroy space for an SCF type
     PTR :: self
     STACK("ROBY:destroy")
     START_TIMER("ROBY:destroy")
     if (NOT associated(self)) then; STOP_TIMER("ROBY:destroy") UNSTACK return; end if
     call nullify_ptr_data_(self)
     call destroy_ptr_part_(self)
     DELETE_MEMORY(ROBY_SIZE)
     deallocate(self)
     STOP_TIMER("ROBY:destroy")
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

   subroutine nullify_ptr_part(self)
    ROBY :: self
   ! Nullify the pointer parts of self
      STACK("ROBY:nullify_ptr_part")
      START_TIMER("ROBY:nullify_ptr_part")
      nullify(self%n1)
      nullify(self%n2)
      nullify(self%bond_index)
      nullify(self%percent_covalency)
      nullify(self%gould_charge)
      nullify(self%cruickshank_charge)
      nullify(self%summed_n2)
      nullify(self%summed_n3)
      nullify(self%atom_list)
      nullify(self%atom_group)
      nullify(self%atom_a)
      nullify(self%atom_b)
      nullify(self%atom_ab)
      nullify(self%theta_C)
      nullify(self%eval_C)
      nullify(self%theta_I)
      nullify(self%eval_I)
      nullify(self%pop_C)
      nullify(self%pop_I)
      nullify(self%pop_A)
      nullify(self%pop_B)
      nullify(self%covalent_index)
      nullify(self%ionic_index)
      nullify(self%proportion_a)
      nullify(self%pair)
      nullify(self%rho)
      nullify(self%overlap_matrix)
      nullify(self%atom)
     STOP_TIMER("ROBY:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine nullify_ptr_data(self)
    ROBY :: self
   ! Nullify the pointer data that is supposed to come from outside this
   ! module, so that this data is *not* accidentally destroyed
      STACK("ROBY:nullify_ptr_data")
      START_TIMER("ROBY:nullify_ptr_data")
      nullify(self%rho)
      nullify(self%atom)
     STOP_TIMER("ROBY:nullify_ptr_data")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    ROBY :: self
   ! Destroy the pointer parts of self
      STACK("ROBY:destroy_ptr_part")
      START_TIMER("ROBY:destroy_ptr_part")
      call destroy_(self%n1)
      call destroy_(self%n2)
      call destroy_(self%bond_index)
      call destroy_(self%percent_covalency)
      call destroy_(self%gould_charge)
      call destroy_(self%cruickshank_charge)
      call destroy_(self%summed_n2)
      call destroy_(self%summed_n3)
      call destroy_(self%atom_list)
      call destroy_(self%atom_group)
      call destroy_(self%atom_a)
      call destroy_(self%atom_b)
      call destroy_(self%atom_ab)
      call destroy_(self%theta_C)
      call destroy_(self%eval_C)
      call destroy_(self%theta_I)
      call destroy_(self%eval_I)
      call destroy_(self%pop_C)
      call destroy_(self%pop_I)
      call destroy_(self%pop_A)
      call destroy_(self%pop_B)
      call destroy_(self%covalent_index)
      call destroy_(self%ionic_index)
      call destroy_(self%proportion_a)
      call destroy_(self%pair)
      call destroy_(self%rho)
      call destroy_(self%overlap_matrix)
      call destroy_(self%atom)
     STOP_TIMER("ROBY:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine set_defaults(self,mol,S)
    ROBY :: self
   ! Set default SCF data values
     MOL, optional :: mol
     REALMAT(:,:), PTR, optional :: S
     REAL :: val
     STACK("ROBY:set_defaults")
     START_TIMER("ROBY:set_defaults")
     self%roby_kind = "bond_and_charge_analysis"
     val = ROBY_COVALENT_CUTOFF; self%covalent_cutoff = from_units_(val,"degree")
     val = ROBY_IONIC_CUTOFF;    self%ionic_cutoff    = from_units_(val,"degree")
     val = ROBY_ZERO_CUTOFF;     self%zero_cutoff     = from_units_(val,"degree")
     val = ROBY_PI_ON_2_CUTOFF;  self%pi_on_2_cutoff  = from_units_(val,"degree")
     self%occupied_ANO_cutoff = ROBY_OCCUPIED_ANO_CUTOFF
     self%output_theta_info   = ROBY_OUTPUT_THETA_INFO
     if (present(mol)) then
     self%charge =  mol%charge
     self%mult   =  mol%mult
     self%atom   => mol%atom
     self%rho    => mol%density_matrix
     end if
     if (present(S)) &
     self%overlap_matrix => S
     STOP_TIMER("ROBY:set_defaults")
      CHECK
   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    ROBY :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("ROBY:read_keywords")
     START_TIMER("ROBY:read_keywords")
     ENSURE(next_item_(stdin)=="{","ROBY:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("ROBY:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    ROBY :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("ROBY:process_keyword")
      START_TIMER("ROBY:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}                      ")  ! exit case
         case ("analyse_all_atom_pairs="); call read_analyse_all_atom_pairs_(self)
         case ("atom_groups=           "); call read_atom_groups_(self)
         case ("atom_list=             "); call read_atom_list_(self)
         case ("bond_scale_factors=    "); call read_bond_scale_factor_(self)
         case ("covalent_cutoff=       "); call read_covalent_cutoff_(self)
         case ("ionic_cutoff=          "); call read_ionic_cutoff_(self)
         case ("kind=                  "); call read_kind_(self)
         case ("output_theta_info=     "); call read_output_theta_info_(self)
         case ("pi_on_2_cutoff=        "); call read_pi_on_2_cutoff_(self)
         case ("zero_cutoff=           "); call read_zero_cutoff_(self)
         case default;           allocate(tonto%known_keywords(11))
         tonto%known_keywords(1) = "}                      "
         tonto%known_keywords(2) = "analyse_all_atom_pairs="
         tonto%known_keywords(3) = "atom_groups=           "
         tonto%known_keywords(4) = "atom_list=             "
         tonto%known_keywords(5) = "bond_scale_factors=    "
         tonto%known_keywords(6) = "covalent_cutoff=       "
         tonto%known_keywords(7) = "ionic_cutoff=          "
         tonto%known_keywords(8) = "kind=                  "
         tonto%known_keywords(9) = "output_theta_info=     "
         tonto%known_keywords(10) = "pi_on_2_cutoff=        "
         tonto%known_keywords(11) = "zero_cutoff=           "
         call unknown_(tonto,word,"ROBY:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ROBY:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    ROBY :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("ROBY:read_units")
      START_TIMER("ROBY:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("ROBY:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    ROBY :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("ROBY:read_junk")
      START_TIMER("ROBY:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("ROBY:read_junk")
      CHECK
   end subroutine

   subroutine finalize(self)
    ROBY :: self
   ! Make sure the input satisfies sanity checks and generate any other
   ! missing data
     BIN :: tmp
     STACK("ROBY:finalize")
     START_TIMER("ROBY:finalize")
     ENSURE(self%roby_kind/=" ","ROBY:finalize ... no calculation kind specified")
     ENSURE(associated(self%atom),"ROBY:finalize ... no atom data supplied")
     tmp = NOT(associated(self%atom_list) AND associated(self%atom_group))
     ENSURE(tmp,"ROBY:finalize ... atom_list, atom_group both used")
     STOP_TIMER("ROBY:finalize")
      UNSTACK
   end subroutine

   subroutine read_kind(self)
    ROBY :: self
   ! Read the SCF type
      STACK("ROBY:read_kind")
      START_TIMER("ROBY:read_kind")
      call read_(stdin,self%roby_kind)
      select case (self%roby_kind)
         case("atom_bond_analysis     ")
         case("atom_shared_population ")
         case("atom_pair_populations  ")
         case("atom_populations       ")
         case("group_bond_analysis    ")
         case("group_shared_population")
         case("group_pair_populations ")
         case("group_populations      ")
         case default;    allocate(tonto%known_keywords(8))
         tonto%known_keywords(1) = "atom_bond_analysis     "
         tonto%known_keywords(2) = "atom_shared_population "
         tonto%known_keywords(3) = "atom_pair_populations  "
         tonto%known_keywords(4) = "atom_populations       "
         tonto%known_keywords(5) = "group_bond_analysis    "
         tonto%known_keywords(6) = "group_shared_population"
         tonto%known_keywords(7) = "group_pair_populations "
         tonto%known_keywords(8) = "group_populations      "
         call unknown_(tonto,self%roby_kind,"ROBY:read_kind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ROBY:read_kind")
      CHECK
   end subroutine

   subroutine read_atom_list(self)
    ROBY :: self
   ! Read the list of atoms indices defining a Roby group
      STACK("ROBY:read_atom_list")
      START_TIMER("ROBY:read_atom_list")
      call read_ptr_(stdin,self%atom_list)
     STOP_TIMER("ROBY:read_atom_list")
      UNSTACK
   end subroutine

   subroutine read_atom_groups(self)
    ROBY :: self
   ! Read the list of atoms indices defining a Roby group
      STACK("ROBY:read_atom_groups")
      START_TIMER("ROBY:read_atom_groups")
      call read_ptr_(stdin,self%atom_group)
     STOP_TIMER("ROBY:read_atom_groups")
      UNSTACK
   end subroutine

   subroutine read_covalent_cutoff(self)
    ROBY :: self
   ! Angles (in radians) greater than this one inputted are ignored when
   ! calculating the covalent bond index
      STACK("ROBY:read_covalent_cutoff")
      START_TIMER("ROBY:read_covalent_cutoff")
      call read_(stdin,self%covalent_cutoff)
     STOP_TIMER("ROBY:read_covalent_cutoff")
      CHECK
   end subroutine

   subroutine read_ionic_cutoff(self)
    ROBY :: self
   ! Angles (in radians) greater than this one inputted are ignored when
   ! calculating the ionic bond index
      STACK("ROBY:read_ionic_cutoff")
      START_TIMER("ROBY:read_ionic_cutoff")
      call read_(stdin,self%ionic_cutoff)
     STOP_TIMER("ROBY:read_ionic_cutoff")
      CHECK
   end subroutine

   subroutine read_pi_on_2_cutoff(self)
    ROBY :: self
   ! Angles (in radians) greater than this one inputted are not used to
   ! calculate the bond index
      STACK("ROBY:read_pi_on_2_cutoff")
      START_TIMER("ROBY:read_pi_on_2_cutoff")
      call read_(stdin,self%pi_on_2_cutoff)
      self%ionic_cutoff    = self%pi_on_2_cutoff
      self%covalent_cutoff = self%pi_on_2_cutoff
     STOP_TIMER("ROBY:read_pi_on_2_cutoff")
      CHECK
   end subroutine

   subroutine read_zero_cutoff(self)
    ROBY :: self
   ! Angles (in radians) *less* than this one inputted are regareded as zero
      STACK("ROBY:read_zero_cutoff")
      START_TIMER("ROBY:read_zero_cutoff")
      call read_(stdin,self%zero_cutoff)
     STOP_TIMER("ROBY:read_zero_cutoff")
      CHECK
   end subroutine

   subroutine read_output_theta_info(self)
    ROBY :: self
   ! Read in a switch which tells whether to output detailed theta-subspace
   ! bond information
      STACK("ROBY:read_output_theta_info")
      START_TIMER("ROBY:read_output_theta_info")
      call read_(stdin,self%output_theta_info)
     STOP_TIMER("ROBY:read_output_theta_info")
      CHECK
   end subroutine

   subroutine read_analyse_all_atom_pairs(self)
    ROBY :: self
   ! Read in a switch which tells whether to bond-analyse all atoms
   ! pairs or not
      STACK("ROBY:read_analyse_all_atom_pairs")
      START_TIMER("ROBY:read_analyse_all_atom_pairs")
      call read_(stdin,self%analyse_all_atom_pairs)
     STOP_TIMER("ROBY:read_analyse_all_atom_pairs")
      CHECK
   end subroutine

   subroutine read_bond_scale_factor(self)
    ROBY :: self
   ! Read in a bond scale factor used to multiply the sum of the
   ! Bragg-Slater radii for two atoms, to determine a distance cutoff
   ! within which atoms are regarded to be bonded
      STACK("ROBY:read_bond_scale_factor")
      START_TIMER("ROBY:read_bond_scale_factor")
      call read_(stdin,self%bond_scale_factor)
     STOP_TIMER("ROBY:read_bond_scale_factor")
      CHECK
   end subroutine

!  *******************
!  Information methods
!  *******************

   function is_homoleptic(self) result(res)
    ROBY :: self
   ! Returns TRUE if the calculation data is "homoleptic" or not
   ! i.e. involving groups of atoms
      BIN :: res
      STACK("ROBY:is_homoleptic")
      START_TIMER("ROBY:is_homoleptic")
      res = NOT associated(self%atom_list)
     STOP_TIMER("ROBY:is_homoleptic")
      CHECK
   end function

   function skip_pair(self,a,b) result(res)
    ROBY :: self
   ! Returns TRUE if the bond index calculation should skip the pair of groups
   ! (a,b). For calculations which are homoleptic, this always returns FALSE, but
   ! for calculations which are defined by an atom_list, this returns TRUE only if
   ! .analyse_all_atom_pairs is set to FALSE and the atoms are regarded as bonded
   ! according to the .bond_scale_factor distance cutoff.
      INT :: a,b
      BIN :: res
      STACK("ROBY:skip_pair")
      START_TIMER("ROBY:skip_pair")
      if      (is_homoleptic_(self)) then;          res = FALSE
      else if (self%analyse_all_atom_pairs) then; res = FALSE
      else
         res = NOT bonded_(self%atom,a,b,self%bond_scale_factor)
      end if
     STOP_TIMER("ROBY:skip_pair")
      CHECK
   end function

   function n_bf(self,group) result(res)
    ROBY :: self
   ! Return the number of basis functions in the atom group
   ! whose indices are given in "group"; but if "group" is
   ! not present return the dimension of the full overlap matrix.
      IN :: self
      INTVEC(:), optional :: group
      INT :: res
      STACK("ROBY:n_bf")
      START_TIMER("ROBY:n_bf")
      ENSURE(associated(self%atom),"ROBY:n_bf ... No atom information")
      ENSURE(associated(self%overlap_matrix),"ROBY:n_bf ... No overlap matrix")
      if (present(group)) then; res = n_bf_(self%atom(group))
      else;                     res = size(self%overlap_matrix,1)
      end if
     STOP_TIMER("ROBY:n_bf")
      CHECK
   end function

   function n_bf_a(self) result(res)
    ROBY :: self
   ! Return the number of basis functions in the .atom_a group
      IN :: self
      INT :: res
      STACK("ROBY:n_bf_a")
      START_TIMER("ROBY:n_bf_a")
      ENSURE(associated(self%atom),"ROBY:n_bf_a ... No atom infpormation")
      ENSURE(associated(self%atom_a),"ROBY:n_bf_a ... No atom A group")
      res = n_bf_(self,self%atom_a)
     STOP_TIMER("ROBY:n_bf_a")
      CHECK
   end function

   function n_bf_b(self) result(res)
    ROBY :: self
   ! Return the number of basis functions in the .atom_b group
      IN :: self
      INT :: res
      STACK("ROBY:n_bf_b")
      START_TIMER("ROBY:n_bf_b")
      ENSURE(associated(self%atom),"ROBY:n_bf_b ... No atom infpormation")
      ENSURE(associated(self%atom_b),"ROBY:n_bf_b ... No atom B group")
      res = n_bf_(self,self%atom_b)
     STOP_TIMER("ROBY:n_bf_b")
      CHECK
   end function

   function n_bf_ab(self) result(res)
    ROBY :: self
   ! Return the number of basis functions in the .atom_b group
      IN :: self
      INT :: res
      STACK("ROBY:n_bf_ab")
      START_TIMER("ROBY:n_bf_ab")
      ENSURE(associated(self%atom),"ROBY:n_bf_ab ... No atom infpormation")
      ENSURE(associated(self%atom_ab),"ROBY:n_bf_ab ... No atom AB group")
      res = n_bf_(self,self%atom_ab)
     STOP_TIMER("ROBY:n_bf_ab")
      CHECK
   end function

   function n_group(self) result(res)
    ROBY :: self
   ! Return the number of roby atom groups
      INT :: res
   STACK("ROBY:n_group")
   START_TIMER("ROBY:n_group")
   ENSURE(associated(self%atom_group),"ROBY:n_group ... no atom groups!")
      res = size(self%atom_group)
     STOP_TIMER("ROBY:n_group")
      CHECK
   end function

   function no_of_occupied_ANOs(self,group,tol) result(res)
    ROBY :: self
   ! Return the number of occupied atomic natural orbitals for
   ! the group of atoms whose indices are in "group"
      INTVEC(:) :: group
      REAL, optional :: tol
      INT :: res
      REAL :: eps
   STACK("ROBY:no_of_occupied_ANOs")
   START_TIMER("ROBY:no_of_occupied_ANOs")
   ENSURE(associated(self%atom),"ROBY:no_of_occupied_ANOs ... no atom information")
      eps = self%occupied_ANO_cutoff
      if (present(tol)) eps = tol
      res = no_of_occupied_ANOs_(self%atom(group),tol=eps)
     STOP_TIMER("ROBY:no_of_occupied_ANOs")
      CHECK
   end function

   function numbered_chemical_symbols(self,group) result(res)
    ROBY :: self
   ! Return a list of numbered chemical symbols for a "group" of atoms
   ! .atom(group)
      INTVEC(:) :: group
      STRVEC(STR_SIZE,:), PTR :: res
      STACK("ROBY:numbered_chemical_symbols")
      START_TIMER("ROBY:numbered_chemical_symbols")
      call create_(res,size(group))
      res = numbered_chemical_symbols_(self%atom(group(:)))
     STOP_TIMER("ROBY:numbered_chemical_symbols")
      UNSTACK
   end function

! ************
! Main methods
! ************

   subroutine atom_populations(self)
    ROBY :: self
   ! EValuate and print out atom populations
   STACK("ROBY:atom_populations")
   START_TIMER("ROBY:atom_populations")
   ENSURE(associated(self%atom_list),"ROBY:atom_populations ... no atom list")
   ENSURE(NOT associated(self%atom_group),"ROBY:atom_populations ... do not use atom_group=")
     call make_single_atom_groups_(self)
     call group_populations_(self)
     STOP_TIMER("ROBY:atom_populations")
      UNSTACK
   end subroutine

   subroutine group_populations(self)
    ROBY :: self
   ! EValuate and print out atom-group populations
     STACK("ROBY:group_populations")
     START_TIMER("ROBY:group_populations")
     call make_populations_(self)
     call put_(self)
     call put_populations_(self)
     STOP_TIMER("ROBY:group_populations")
      UNSTACK
   end subroutine

   subroutine atom_pair_populations(self)
    ROBY :: self
   ! Evaluate and print out atom pair populations
   STACK("ROBY:atom_pair_populations")
   START_TIMER("ROBY:atom_pair_populations")
   ENSURE(associated(self%atom_list),"ROBY:atom_pair_populations ... no atom list")
   ENSURE(NOT associated(self%atom_group),"ROBY:atom_pair_populations ... do not use atom_group=")
     call make_single_atom_groups_(self)
     call group_pair_populations_(self)
     STOP_TIMER("ROBY:atom_pair_populations")
      UNSTACK
   end subroutine

   subroutine group_pair_populations(self)
    ROBY :: self
   ! Evaluate and print out atom-group pair populations
     STACK("ROBY:group_pair_populations")
     START_TIMER("ROBY:group_pair_populations")
     call make_pair_populations_(self)
     call put_(self)
     call put_pair_populations_(self)
     STOP_TIMER("ROBY:group_pair_populations")
      UNSTACK
   end subroutine

   subroutine atom_bond_analysis(self)
    ROBY :: self
   ! Do a Roby bond and Gould charge analysis.
   STACK("ROBY:atom_bond_analysis")
   START_TIMER("ROBY:atom_bond_analysis")
   ENSURE(associated(self%atom_list),"ROBY:atom_bond_analysis ... no atom list")
   ENSURE(NOT associated(self%atom_group),"ROBY:atom_bond_analysis ... do not use atom_group=")
      call make_single_atom_groups_(self)
      call group_bond_analysis_(self)
     STOP_TIMER("ROBY:atom_bond_analysis")
      UNSTACK
   end subroutine

   subroutine group_bond_analysis(self)
    ROBY :: self
   ! Do a Roby bond and Gould charge analysis.
      STACK("ROBY:group_bond_analysis")
      START_TIMER("ROBY:group_bond_analysis")
      call bond_analysis_(self)
      call charge_analysis_(self)
      call put_all_info_(self)
     STOP_TIMER("ROBY:group_bond_analysis")
      UNSTACK
   end subroutine

   subroutine atom_shared_population(self)
    ROBY :: self
   ! Do a Roby bond and Gould charge analysis.
   STACK("ROBY:atom_shared_population")
   START_TIMER("ROBY:atom_shared_population")
   ENSURE(associated(self%atom_list),"ROBY:atom_shared_population ... no atom list")
   ENSURE(NOT associated(self%atom_group),"ROBY:atom_shared_population ... do not use atom_group=")
      call make_single_atom_groups_(self)
      call group_shared_population_(self)
     STOP_TIMER("ROBY:atom_shared_population")
      UNSTACK
   end subroutine

   subroutine group_shared_population(self)
    ROBY :: self
   ! Do a Roby bond and Gould charge analysis.
   STACK("ROBY:group_shared_population")
   START_TIMER("ROBY:group_shared_population")
   ENSURE(associated(self%atom_list),"ROBY:group_shared_population ... no atom list")
      call make_shared_population_(self)
      call put_(self)
      call put_shared_population_(self)
     STOP_TIMER("ROBY:group_shared_population")
      UNSTACK
   end subroutine

! *********************************
! Roby population analysis routines
! *********************************

   subroutine make_single_atom_groups(self)
    ROBY :: self
   ! Make each Roby atom to be equal to a single atom formed from
   ! each element in the ".atom_list".
     INT :: n_atom,a
   STACK("ROBY:make_single_atom_groups")
   START_TIMER("ROBY:make_single_atom_groups")
   ENSURE(associated(self%atom_list),"ROBY:make_single_atom_groups ... no atom list")
   ENSURE(NOT associated(self%atom_group),"ROBY:make_single_atom_groups ... do not use atom_groups=")
     n_atom = size(self%atom_list)
     call create_(self%atom_group,n_atom)
     do a = 1,n_atom
        call create_(self%atom_group(a)%element,1)
        self%atom_group(a)%element(1) = self%atom_list(a)
     end do
     STOP_TIMER("ROBY:make_single_atom_groups")
      UNSTACK
   end subroutine

   subroutine make_populations(self)
    ROBY :: self
   ! Make the Roby populations for the defined atom groups, for a
   ! given density matrix "rho"
     INT :: n_group,a
   STACK("ROBY:make_populations")
   START_TIMER("ROBY:make_populations")
   ENSURE(associated(self%atom_group),"ROBY:make_populations ... no atom groups")
   ENSURE(associated(self%rho),"ROBY:make_populations ... no rho matrix")
     n_group = n_group_(self)
     call destroy_(self%n1)
     call create_(self%n1,n_group)
     do a = 1,n_group
       self%atom_a => self%atom_group(a)%element
       self%n1(a) = population_(self,self%atom_a)
       nullify(self%atom_a)
     end do
     STOP_TIMER("ROBY:make_populations")
      UNSTACK
   end subroutine

   subroutine make_pair_populations(self)
    ROBY :: self
   ! Make the pair Roby populations
     INT :: n_group,a,b
     REAL :: pop
     STACK("ROBY:make_pair_populations")
     START_TIMER("ROBY:make_pair_populations")
     n_group = n_group_(self)
     call destroy_(self%n2)
     call create_(self%n2,n_group,n_group)
     self%n2 = ZERO
     do a = 1,n_group
     do b = a+1,n_group
        if (skip_pair_(self,a,b)) cycle
        self%atom_a  => self%atom_group(a)%element
        self%atom_b  => self%atom_group(b)%element
        self%atom_ab => join_(self%atom_a,self%atom_b)
        pop = population_(self,self%atom_ab)
        self%n2(a,b) = pop
        self%n2(b,a) = pop
        nullify(self%atom_a)
        nullify(self%atom_b)
        call destroy_(self%atom_ab)
     end do
     end do
     STOP_TIMER("ROBY:make_pair_populations")
      UNSTACK
   end subroutine

   function population(self,group,ANO) result(res)
    ROBY :: self
   ! Return the total Roby population for the group of atoms whose indices are
   ! given in "group", i.e Tr (P_{group;ANO} rho), where "rho" is the density
   ! matrix for a molecule. If present, the columns of the "ANO" matrix are used
   ! to define the Roby projector instead of the atomic natural orbitals.
      INTVEC(:) :: group
      REALMAT(:,:), optional :: ANO
      REAL :: res
       REALMAT(:,:), PTR :: X
      INT :: n_gr
   STACK("ROBY:population")
   START_TIMER("ROBY:population")
   ENSURE(associated(self%rho),"ROBY:population ... no rho matrix")
   ENSURE(associated(self%overlap_matrix),"ROBY:population ... no overlap matrix")
      n_gr = n_bf_(self,group)
      call create_(X,n_gr,n_gr)
      call make_projection_matrix_(self,X,group,ANO)
      res = expectation_(self,X,group)
      call destroy_(X)
     STOP_TIMER("ROBY:population")
      CHECK
   end function

   function subpopulation(self,subgroup,group,ANO) result(res)
    ROBY :: self
   ! Return the Roby subpopulation for the subgroup of atoms whose indices are
   ! given in "subgroup", in the whole group of atoms whose indices are
   ! are given in "group", i.e
   !    Tr ( P_{subgroup} P_{group;ANO} P_{subgroup} rho )
   ! If present, the columns of the "ANO" matrix are used to define the middle
   ! Roby projector instead of the atomic natural orbitals. Note that the
   ! real atomic natural orbitals are always used for the P_{subgroup}
   ! projector.  This routine is useful for getting theta orbital populations.
      INTVEC(:) :: subgroup,group
      REALMAT(:,:), optional :: ANO
      REAL :: res
      REALMAT(:,:), PTR :: P,X,P_sub
      INT :: n_gr,n_sub
      STACK("ROBY:subpopulation")
      START_TIMER("ROBY:subpopulation")
      ENSURE(associated(self%rho),"ROBY:subpopulation ... no rho matrix")
      ENSURE(associated(self%overlap_matrix),"ROBY:subpopulation ... no overlap matrix")
      n_gr  = n_bf_(self,group)
      n_sub = n_bf_(self,subgroup)
      call create_(X,n_sub,n_sub)
      call create_(P,n_gr,n_gr)
      call create_(P_sub,n_sub,n_sub)
      call make_projection_matrix_(self,P,group,ANO)
      call make_projection_matrix_(self,P_sub,subgroup)
      call project_(self,P,P_sub,X,group,subgroup)
      call destroy_(P_sub)
      call destroy_(P)
      res = expectation_(self,X,subgroup)
      call destroy_(X)
     STOP_TIMER("ROBY:subpopulation")
      CHECK
   end function

! ********************************
! Roby multiple shared populations
! ********************************

   subroutine make_shared_population(self)
    ROBY :: self
   ! Evaluate the Roby shared population
     INT :: n_group,i
   STACK("ROBY:make_shared_population")
   START_TIMER("ROBY:make_shared_population")
   ENSURE(associated(self%atom_group),"ROBY:make_shared_population ... no atom group supplied")
     n_group = n_group_(self)
     call create_(self%n1,n_group)
     self%n_shared = shared_population_(self,(/(i,i=1,n_group)/), self%n1)
     STOP_TIMER("ROBY:make_shared_population")
      UNSTACK
   end subroutine

   function shared_population(self,group_list,pop_groups) result(res)
    ROBY :: self
   ! Returns the Roby shared population of the group of .atom_groups whose
   ! indices are in "group". If "pop_groups" is present, the total
   ! sub-group populations of every size from 1 to size(group) is returned
      INTVEC(:) :: group_list
      REALVEC(:), optional :: pop_groups
      REAL :: res
      REAL :: pop_k
      INT :: m, k, n_k, n
      INTMAT(:,:), PTR :: comb_mat
      REALVEC(:), PTR :: pop_group
   STACK("ROBY:shared_population")
   START_TIMER("ROBY:shared_population")
   ENSURE(associated(self%atom_group),"ROBY:shared_population ... no atom groups")
      n = size(group_list)
      call create_(pop_group,n)
      res = ZERO
      do k = 1,n
         n_k = choose_(n,k)
         call create_(comb_mat,k,n_k)
         comb_mat => combinations_of_length_(group_list,k)
         ! comb_mat is the matrix of all combinations of the groups
         ! of size k, where the groups are taken from the atom_group
         pop_k = 0
         do m = 1,n_k
            self%atom_ab => join_(self%atom_group,comb_mat(:,m))
            pop_k = pop_k + population_(self,self%atom_ab)
            call destroy_(self%atom_ab)
         end do
         pop_groups(k) = pop_k
         call destroy_(comb_mat)
         res = res - ((-1)**k)*pop_k
      end do
      if (present(pop_groups)) pop_groups = pop_group
      call destroy_(pop_group)
     STOP_TIMER("ROBY:shared_population")
      CHECK
   end function

   subroutine make_summed_pair_pops(self)
    ROBY :: self
   ! Evaluate the SUMMED Roby shared populations for groups of 2 atoms.
   ! On return, .summed_n2(a) = \sum_{b} s_{ab}, where s_{ab} is the pairwise
   ! shared population for atoms a, b.
     REAL :: shared_population
     INT :: n_group,a,b
     STACK("ROBY:make_summed_pair_pops")
     START_TIMER("ROBY:make_summed_pair_pops")
     ENSURE(associated(self%n1),"ROBY:make_summed_pair_pops ... no group populations")
     ENSURE(associated(self%n2),"ROBY:make_summed_pair_pops ... no group pair populations")
     n_group = n_group_(self)
     call create_(self%summed_n2,n_group)
     self%summed_n2 = ZERO
     do a = 1,n_group
     do b = a+1,n_group
        shared_population = self%n1(a) + self%n1(b) - self%n2(a,b)
        self%summed_n2(a) = self%summed_n2(a) + shared_population
        self%summed_n2(b) = self%summed_n2(b) + shared_population
     end do
     end do
     STOP_TIMER("ROBY:make_summed_pair_pops")
      UNSTACK
   end subroutine

   subroutine make_summed_triple_pops(self)
    ROBY :: self
   ! Evaluate the SUMMED Roby shared populations for groups of 3 atoms.
   ! On return, .summed_n3(a) = \sum_{bc} s_{abc}, where s_{abc} is the triple
   ! shared population for atoms a, b, and c.
     REAL :: shared_population
     INT :: n_group,a,b,c
     STACK("ROBY:make_summed_triple_pops")
     START_TIMER("ROBY:make_summed_triple_pops")
     n_group = n_group_(self)
     call create_(self%summed_n3,n_group)
     self%summed_n3 = ZERO
     do a = 1,n_group
     do b = a+1,n_group
     do c = b+1,n_group
        shared_population = shared_population_(self,(/a,b,c/))
        self%summed_n3(a) = self%summed_n3(a) + shared_population
        self%summed_n3(b) = self%summed_n3(b) + shared_population
        self%summed_n3(c) = self%summed_n3(c) + shared_population
     end do
     end do
     end do
     STOP_TIMER("ROBY:make_summed_triple_pops")
      UNSTACK
   end subroutine

! ******************************
! Roby-Gould bond index routines
! ******************************

   subroutine bond_analysis(self)
    ROBY :: self
   ! Do a Roby-Gould bond and charge analysis for the atom groups
   ! given in .atom_groups
     INT :: n_group,a,b
     REAL :: pcc
     STACK("ROBY:bond_analysis")
     START_TIMER("ROBY:bond_analysis")
     ENSURE(associated(self%atom_group),"ROBY:bond_analysis ... no atom group defined")
     n_group = n_group_(self)
     call make_populations_(self)
     call make_pair_populations_(self)
     call create_(self%bond_index,n_group,n_group)
     call create_(self%percent_covalency,n_group,n_group)
     do a = 1,n_group
     do b = 1,a-1
        if (skip_pair_(self,a,b)) cycle
        self%atom_a  => self%atom_group(a)%element
        self%atom_b  => self%atom_group(b)%element
        self%atom_ab => join_(self%atom_a,self%atom_b)
        call destroy_theta_info_(self)
        call make_theta_info_(self)
        self%bond_index(a,b) = gould_bond_index_(self,pcc)
        self%percent_covalency(a,b) = pcc
        if (self%output_theta_info) then
           call put_theta_info_(self)
           call put_theta_bond_info_(self)
        end if
        call destroy_theta_info_(self)
        nullify(self%atom_a)
        nullify(self%atom_b)
        call destroy_(self%atom_ab)
     end do
     end do
     STOP_TIMER("ROBY:bond_analysis")
      UNSTACK
   end subroutine

   function gould_bond_index(self,pcc) result(bond_index)
    ROBY :: self
   ! Calculates the Roby-Gould "bond_index", percentage covalent character
   ! "pcc", for atom groups ".atom_a" and ".atom_b".
      REAL :: pcc, bond_index
      INT :: n_ab,i
      REAL :: ionic_cutoff,covalent_cutoff,zero_cutoff,angle
      STACK("ROBY:gould_bond_index")
      START_TIMER("ROBY:gould_bond_index")
      ENSURE(associated(self%theta_angle),"ROBY:gould_bond_index ... no theta angles")
      ENSURE(associated(self%pop_C),"ROBY:gould_bond_index ... no covalent theta populations")
      ENSURE(associated(self%pop_I),"ROBY:gould_bond_index ... no ionic theta populations")
      ENSURE(associated(self%pair),"ROBY:gould_bond_index ... no pair array")
      n_ab = n_bf_ab_(self)
      ionic_cutoff    = to_units_(self%ionic_cutoff,"degree")
      covalent_cutoff = to_units_(self%covalent_cutoff,"degree")
      zero_cutoff     = to_units_(self%zero_cutoff,"degree")
      call create_(self%covalent_index,n_ab)
      call create_(self%ionic_index,n_ab)
      self%covalent_index = ZERO
      self%ionic_index    = ZERO
      pcc = ZERO
      bond_index = ZERO
      do i = 1,n_ab
        if (self%pair(i)<1 OR i<self%pair(i)) cycle
        angle = self%theta_angle(i)
        self%covalent_index(i) = (self%pop_C(i) - self%pop_C(self%pair(i)))/TWO
        self%ionic_index(i)    = (self%pop_I(i) - self%pop_I(self%pair(i)))/TWO
        if (angle <= covalent_cutoff AND angle >= zero_cutoff) then
           bond_index = bond_index + self%covalent_index(i)**2
           pcc = pcc + self%covalent_index(i)**2
        end if
        if (angle <= ionic_cutoff AND angle >= zero_cutoff) then
           bond_index = bond_index + self%ionic_index(i)**2
        end if
      end do
      pcc = 100 * (pcc/bond_index)
      bond_index = sqrt(bond_index)
     STOP_TIMER("ROBY:gould_bond_index")
      UNSTACK
   end function

   subroutine make_theta_info(self)
    ROBY :: self
   ! Calculates the Roby-Gould theta subspace information for atom groups
   ! ".atom_a" and ".atom_b", including theta subspace populations
      REALMAT(:,:), PTR :: C
      INT :: n_a,n_b,n_ab
      STACK("ROBY:make_theta_info")
      START_TIMER("ROBY:make_theta_info")
      ENSURE(associated(self%atom_a),"ROBY:make_theta_info ... no atom A group")
      ENSURE(associated(self%atom_b),"ROBY:make_theta_info ... no atom B group")
      ENSURE(associated(self%atom_ab),"ROBY:make_theta_info ... no atom AB group")
      n_a  = n_bf_a_(self)
      n_b  = n_bf_b_(self)
      n_ab = n_a + n_b
      call create_(C,n_ab,n_ab)                      ! Make the Roby operator
      call make_shared_operator_(self,C)
      call create_(self%theta_C,n_ab,n_ab)
      call create_(self%eval_C,n_ab)
      call diagonalise_V_AB_operator_(self,C,self%theta_C,self%eval_C)
      call create_(self%theta_angle,n_ab)
      call create_(self%pair,n_ab)                       ! Find +/- pairs covalent pairs
#ifndef NOGENERIC
      call find_pairs_(self%eval_C,self%pair,match_pair)
#else
      call find_pairs_(self%eval_C,self%pair,ROBY_match_pair)
#endif
      call make_theta_angles_(self)                       ! Make |cov-> from |cov+>
      call make_gould_covalent_orbitals_(self)            ! Make |cov-> from |cov+>
      call create_(self%theta_I,n_ab,n_ab)
      call create_(self%eval_I,n_ab)
      call make_gould_ionic_orbitals_(self)               ! Make |ion> pairs from |cov> pairs
      call create_(self%pop_C,n_ab)
      call create_(self%pop_I,n_ab)
      call make_theta_populations_(self,self%pop_C,self%theta_C) ! Make covalent theta populations
      call make_theta_populations_(self,self%pop_I,self%theta_I) ! Make ionic theta populations
      call destroy_(C)
     STOP_TIMER("ROBY:make_theta_info")
      UNSTACK
   end subroutine

   subroutine destroy_theta_info(self)
    ROBY :: self
   ! Delete all the temporary theta subspace information created by various bond
   ! index routines
      STACK("ROBY:destroy_theta_info")
      START_TIMER("ROBY:destroy_theta_info")
      call destroy_(self%ionic_index)
      call destroy_(self%covalent_index)
      call destroy_(self%pop_B)
      call destroy_(self%pop_A)
      call destroy_(self%pop_I)
      call destroy_(self%pop_C)
      call destroy_(self%eval_I)
      call destroy_(self%theta_I)
      call destroy_(self%pair)
      call destroy_(self%theta_angle)
      call destroy_(self%eval_C)
      call destroy_(self%theta_C)
     STOP_TIMER("ROBY:destroy_theta_info")
      UNSTACK
   end subroutine

   function match_pair(arg1,arg2) result(res)
   ! Function which returns zero if "arg1" and "arg2" are opposite.
   ! Used for matching purposes.
      REAL :: arg1,arg2,res
      STACK("ROBY:match_pair")
      START_TIMER("ROBY:match_pair")
      res = abs(arg1 + arg2)
     STOP_TIMER("ROBY:match_pair")
      CHECK
   end function

   subroutine make_theta_populations(self,pop,theta)
    ROBY :: self
   ! Make "pop", an array of "theta" populations for a pair of Roby atoms
   ! defined by indices in ".atom_a" and ".atom_b". "theta" itself is a matrix
   ! whose columns are coefficients on V_AB.
      REALVEC(:) :: pop
      REALMAT(:,:) :: theta
       INT :: i
   STACK("ROBY:make_theta_populations")
   START_TIMER("ROBY:make_theta_populations")
   ENSURE(associated(self%atom_ab),"ROBY:make_theta_populations ... No roby atom group AB")
   ENSURE(size(pop)==n_bf_ab_(self),"ROBY:make_theta_populations ... wrong size, pop")
      do i = 1,n_bf_ab_(self)
         pop(i) = population_(self,self%atom_ab,ANO=theta(:,i:i))
      end do
     STOP_TIMER("ROBY:make_theta_populations")
      CHECK
   end subroutine

   subroutine make_theta_atom_populations(self,pop_a,pop_b,theta)
    ROBY :: self
   ! Make "pop_a" and "pop_b", the "theta" populations of the Roby atoms
   ! ".atom_a" and ".atom_b". "theta" itself is a matrix whose columns are
   ! coefficients on V_AB.
      REALVEC(:) :: pop_a,pop_b
      REALMAT(:,:) :: theta
       INT :: i
   STACK("ROBY:make_theta_atom_populations")
   START_TIMER("ROBY:make_theta_atom_populations")
   ENSURE(size(pop_a)==n_bf_ab_(self),"ROBY:make_theta_atom_populations ... wrong size, theta_a_population")
   ENSURE(size(pop_b)==n_bf_ab_(self),"ROBY:make_theta_atom_populations ... wrong size, theta_b_population")
   ENSURE(associated(self%atom_a),"ROBY:make_theta_atom_populations ... No roby atom group A")
   ENSURE(associated(self%atom_b),"ROBY:make_theta_atom_populations ... No roby atom group B")
   ENSURE(associated(self%atom_ab),"ROBY:make_theta_atom_populations ... No roby atom group AB")
      do i = 1,n_bf_ab_(self)
         pop_a(i) = subpopulation_(self,self%atom_a,self%atom_ab,ANO=theta(:,i:i))
         pop_b(i) = subpopulation_(self,self%atom_b,self%atom_ab,ANO=theta(:,i:i))
      end do
     STOP_TIMER("ROBY:make_theta_atom_populations")
      CHECK
   end subroutine

   subroutine make_theta_angles(self)
    ROBY :: self
   ! Make the ".theta_angle" array from the Roby eigenvalues ".eval_C"
       INT :: i
      REAL :: value
   STACK("ROBY:make_theta_angles")
   START_TIMER("ROBY:make_theta_angles")
   ENSURE(associated(self%eval_C),"ROBY:make_theta_angles ... No roby eigenvalues")
   ENSURE(associated(self%theta_angle),"ROBY:make_theta_angles ... No theta_angle array")
      do i = 1,n_bf_ab_(self)
        value = abs(self%eval_C(i))
        value = arccos_(value)
        value = to_units_(value,"degree")
        self%theta_angle(i) = value
      end do
     STOP_TIMER("ROBY:make_theta_angles")
      CHECK
   end subroutine

   subroutine make_gould_covalent_orbitals(self)
    ROBY :: self
   ! Generate the gould covalent orbitals ".theta_C" in such a way that the
   ! negative eigenvector of each pair is generated explicitly from the positive
   ! eigenvector by constructing the |a> and |b> parts using projectors P_A and
   ! P_B. ".eval_C" are the eigenvalues corresponding to ".theta_C". ".pair" is
   ! defined so that the i-th eigenvector .theta_C(:,i) is paired with
   ! theta_C(:,pair(i)). The spaces V_A , V_B and V_AB are defined by
   ! ".atom_a", ".atom_b", and ".atom_ab". This routine is necessary in the case
   ! where there are degeneracies.
      REALMAT(:,:), PTR :: P_A,PA,P_B,PB
      REALVEC(:), PTR :: A,B
      INT :: n_a,n_b,n_ab,i
      REAL :: fac,costheta
      STACK("ROBY:make_gould_covalent_orbitals")
      START_TIMER("ROBY:make_gould_covalent_orbitals")
      ENSURE(associated(self%atom_a),"ROBY:make_gould_covalent_orbitals ... No roby atom A")
      ENSURE(associated(self%atom_b),"ROBY:make_gould_covalent_orbitals ... No roby atom B")
      ENSURE(associated(self%atom_ab),"ROBY:make_gould_covalent_orbitals ... No roby atom AB")
      ENSURE(associated(self%atom),"ROBY:make_gould_covalent_orbitals ... No atom information")
      ENSURE(associated(self%theta_C),"ROBY:make_gould_covalent_orbitals ... No theta_C array")
      ENSURE(size(self%theta_C,1)==n_bf_ab_(self),"ROBY:make_gould_covalent_orbitals ... wrong shape, theta_C")
      ENSURE(is_square_(self%theta_C),"ROBY:make_gould_covalent_orbitals ... wrong shape, theta_C")
      ENSURE(associated(self%eval_C),"ROBY:make_gould_covalent_orbitals ... No eval_C array")
      ENSURE(size(self%eval_C)==n_bf_ab_(self),"ROBY:make_gould_covalent_orbitals ... wrong shape, eval_I")
      ENSURE(associated(self%pair),"ROBY:make_gould_covalent_orbitals ... No pair array")
      ENSURE(size(self%pair)==n_bf_ab_(self),"ROBY:make_gould_covalent_orbitals ... wrong shape, pair")
      n_a  = n_bf_a_(self)
      n_b  = n_bf_b_(self)
      n_ab = n_bf_ab_(self)
      call create_(P_A,n_a,n_a); call create_(PA,n_a,n_ab)
      call create_(P_B,n_b,n_b); call create_(PB,n_b,n_ab)
      call create_(A,n_a)
      call create_(B,n_b)
      call make_projection_matrix_(self,P_A,self%atom_a)
      call make_projection_matrix_(self,P_B,self%atom_b)
      call right_overlap_transform_(self,P_A,PA,self%atom_a,self%atom_ab)
      call right_overlap_transform_(self,P_B,PB,self%atom_b,self%atom_ab)
      do i = 1,n_ab
         if (self%pair(i)<1) cycle
         if (self%eval_C(i)<self%eval_C(self%pair(i))) cycle
         costheta = self%eval_C(i)
         fac = sqrt(TWO+TWO*costheta)/(ONE+costheta)
         call to_product_of_(A,PA,self%theta_C(:,i)); A = fac*A
         call to_product_of_(B,PB,self%theta_C(:,i)); B = fac*B
         fac = ONE/sqrt(TWO-TWO*costheta)
         self%theta_C(    1:n_a ,self%pair(i)) =  fac*A
         self%theta_C(n_a+1:n_ab,self%pair(i)) = -fac*B
      end do
      call destroy_(B)
      call destroy_(A)
      call destroy_(PB); call destroy_(P_B)
      call destroy_(PA); call destroy_(P_A)
     STOP_TIMER("ROBY:make_gould_covalent_orbitals")
      CHECK
   end subroutine

   subroutine make_gould_ionic_orbitals(self)
    ROBY :: self
   ! Make the gould ionic orbitals ".theta_I" and eigenvalues ".eval_I"
   ! from the covalent orbitals ".theta_C" and covalent eigenvalues
   ! ".eval_C" which have been explicitly paired up as described by
   ! the ".pair" array. ALSO: the ".pair" array is modified to remove all
   ! those pairs which have zero sine and cosine eigenvalues.
      REAL :: fac,eps
      INT :: n_ab,i,j
      REALMAT(1,1) :: eval
      REALMAT(:,:), PTR :: ZZ,II
      STACK("ROBY:make_gould_ionic_orbitals")
      START_TIMER("ROBY:make_gould_ionic_orbitals")
      ENSURE(associated(self%theta_C),"ROBY:make_gould_ionic_orbitals ... no theta_C array")
      ENSURE(size(self%theta_C,1)==n_bf_ab_(self),"ROBY:make_gould_ionic_orbitals ... wrong shape, theta_C")
      ENSURE(is_square_(self%theta_C),"ROBY:make_gould_ionic_orbitals ... wrong shape, theta_C")
      ENSURE(associated(self%theta_I),"ROBY:make_gould_ionic_orbitals ... no theta_I array")
      ENSURE(size(self%theta_I,1)==n_bf_ab_(self),"ROBY:make_gould_ionic_orbitals ... wrong shape, theta_I")
      ENSURE(is_square_(self%theta_I),"ROBY:make_gould_ionic_orbitals ... wrong shape, theta_I")
      ENSURE(associated(self%eval_I),"ROBY:make_gould_ionic_orbitals ... no eval_I array")
      ENSURE(size(self%eval_I)==n_bf_ab_(self),"ROBY:make_gould_ionic_orbitals ... wrong shape, eval_I")
      ENSURE(associated(self%eval_C),"ROBY:make_gould_ionic_orbitals ... no eval_C array")
      ENSURE(size(self%eval_C)==n_bf_ab_(self),"ROBY:make_gould_ionic_orbitals ... wrong shape, eval_I")
      ENSURE(associated(self%pair),"ROBY:make_gould_ionic_orbitals ... no pair array")
      ENSURE(size(self%pair)==n_bf_ab_(self),"ROBY:make_gould_ionic_orbitals ... wrong shape, pair")
      n_ab = size(self%pair)
      eps = self%zero_cutoff
      call create_(ZZ,n_ab,n_ab)
      call create_(II,n_ab,n_ab)
      call make_ionic_operator_(self,II)
      call overlap_transform_(self,II,ZZ,self%atom_ab,self%atom_ab)
      call destroy_(II)
      self%theta_I = ZERO
      self%eval_I = ZERO
      do i = 1,n_ab
         if (self%pair(i)<1) cycle
         if (self%eval_C(i)<self%eval_C(self%pair(i))) cycle
         j = self%pair(i)
         fac = ONE/sqrt(TWO)
         self%theta_I(:,i) = fac*(self%theta_C(:,i)+self%theta_C(:,j)) ! I+
         self%theta_I(:,j) = fac*(self%theta_C(:,i)-self%theta_C(:,j)) ! I-
         call change_basis_(ZZ,eval,self%theta_I(:,i:i)); self%eval_I(i) = eval(1,1)
         call change_basis_(ZZ,eval,self%theta_I(:,j:j)); self%eval_I(j) = eval(1,1)
         if (is_zero_(self%eval_C(i),eps) AND is_zero_(self%eval_I(i),eps)) then
            self%pair(i) = 0
            self%pair(j) = 0
         end if
      end do
      call destroy_(ZZ)
     STOP_TIMER("ROBY:make_gould_ionic_orbitals")
      CHECK
   end subroutine

   subroutine charge_analysis(self)
    ROBY :: self
   ! Do a Roby-Cruickshank-Avramedes and Roby-Gould charge analysis.
   ! NOTE: This can only be called after a bond analysis because the atom
   ! proportions are required.
     INT :: n_group,a,b
     REAL :: rsa,rsb,charge_deficit
   STACK("ROBY:charge_analysis")
   START_TIMER("ROBY:charge_analysis")
   ENSURE(associated(self%atom_group),"ROBY:charge_analysis ... no atom groups")
   ENSURE(associated(self%n1),"ROBY:charge_analysis ... no atom populations")
     n_group = n_group_(self)
     call create_(self%proportion_a,n_group,n_group)
     self%proportion_a = ZERO
     do a = 1,n_group
     do b = 1,a-1
        if (skip_pair_(self,a,b)) cycle
        self%atom_a  => self%atom_group(a)%element
        self%atom_b  => self%atom_group(b)%element
        self%atom_ab => join_(self%atom_a,self%atom_b)
        call destroy_theta_info_(self)
        call make_theta_info_(self)
        call make_atom_proportions_(self,rsa,rsb) ! Store for later charge analysis
        self%proportion_a(a,b) = rsa
        self%proportion_a(b,a) = rsb
        if (self%output_theta_info) call put_theta_atom_pops_(self)
        call destroy_theta_info_(self)
        nullify(self%atom_a)
        nullify(self%atom_b)
        call destroy_(self%atom_ab)
     end do
     end do
     call make_populations_(self)
     call make_pair_populations_(self)
     call make_summed_pair_pops_(self)
     call make_summed_triple_pops_(self)
     call create_(self%gould_charge,n_group)       ! === Roby-Gould atomic charges ===
     do a = 1,n_group
        self%gould_charge(a) = sum_of_atomic_numbers_(self%atom(self%atom_group(a)%element)) &
                         - self%n1(a) + sum(self%proportion_a(:,a)) &
                         - self%summed_n3(a)/THREE
     end do
     charge_deficit = (self%charge - sum(self%gould_charge(:)))/n_group
     self%gould_charge = self%gould_charge + charge_deficit
     call create_(self%cruickshank_charge,n_group) ! === Cruickshank atomic charges ===
     do a = 1,n_group
        self%cruickshank_charge(a) = sum_of_atomic_numbers_(self%atom(self%atom_group(a)%element)) &
                               - self%n1(a) + self%summed_n2(a)/TWO - self%summed_n3(a)/THREE
     end do
     charge_deficit = (self%charge - sum(self%cruickshank_charge(:)))/n_group
     self%cruickshank_charge = self%cruickshank_charge + charge_deficit
     STOP_TIMER("ROBY:charge_analysis")
      UNSTACK
   end subroutine

   subroutine make_atom_proportions(self,rsa,rsb)
    ROBY :: self
   ! Make Gould's probabilistic proportionalities between atoms,
   ! ".proportion_a" and ".proportion_b" used to approtion charge
   ! between atoms, i.e. calculate
   ! rsa = \sum_{\theta} r^{\theta}_{A,AB} s^{\theta}_{AB}
   ! rsb = \sum_{\theta} r^{\theta}_{B,AB} s^{\theta}_{AB}
      REAL :: rsa, rsb
      INT :: n_ab,i
      REAL :: pop_a, pop_b, pop_ab, s_ab, ratio_a, ratio_b
      STACK("ROBY:make_atom_proportions")
      START_TIMER("ROBY:make_atom_proportions")
      ENSURE(associated(self%theta_C),"ROBY:make_atom_proportions ... no theta_C covalent orbitals")
      ENSURE(associated(self%eval_C),"ROBY:make_atom_proportions ... no eval_C covalent eigenvalues")
      ENSURE(size(self%theta_C,1)==n_bf_ab_(self),"ROBY:make_atom_proportions ... wrong shape, theta_C")
      n_ab = n_bf_ab_(self)
      call create_(self%pop_A,n_ab)
      call create_(self%pop_B,n_ab)   ! Make A & B theta popualtions
      call make_theta_atom_populations_(self,self%pop_A,self%pop_B,self%theta_C)
      rsa = ZERO
      rsb = ZERO
      do i = 1,n_ab
        if (self%pair(i)<1 OR i<self%pair(i)) cycle
        pop_a   = self%pop_A(i) + self%pop_A(self%pair(i))
        pop_b   = self%pop_B(i) + self%pop_B(self%pair(i))
        pop_ab  = self%pop_C(i) + self%pop_C(self%pair(i))
        ratio_a = pop_a/(pop_a + pop_b)
        ratio_b = pop_b/(pop_a + pop_b)
        s_ab    = pop_a + pop_b - pop_ab
        rsa     = rsa + ratio_a*s_ab
        rsb     = rsb + ratio_b*s_ab
      end do
     STOP_TIMER("ROBY:make_atom_proportions")
      UNSTACK
   end subroutine

! ***********************************
! Make and diagonalise Roby operators
! ***********************************

   subroutine make_projection_matrix(self,P,group,ANO)
    ROBY :: self
   ! Make the Roby projection matrix "P" in the AO basis made from the
   ! concatenated basis sets for each atom in "group". If present, use
   ! the columns of "ANO" as atomic orbital coefficients to form the
   ! projection operator instead of the actual atomic natural orbitals
      REALMAT(:,:) :: P
      INTVEC(:) :: group
      REALMAT(:,:), optional :: ANO
      REALMAT(:,:), PTR :: W,X,Y
      INT :: n_occ,n_bf
      STACK("ROBY:make_projection_matrix")
      START_TIMER("ROBY:make_projection_matrix")
      ENSURE(size(P,1)==n_bf_(self,group),"ROBY:make_projection_matrix ... wrong dimension, P ")
      ENSURE(is_square_(P),"ROBY:make_projection_matrix ... P is incorrectly dimensioned")
      if (present(ANO)) &
      ENSURE(size(ANO,1)==n_bf_(self,group),"ROBY:make_projection_matrix ... wrong dimension, ANO")
      n_bf = n_bf_(self,group)
      if (present(ANO)) then
         n_occ = size(ANO,2)
         call create_(W,n_bf,n_occ)               ! W = columns of ANO's
         W = ANO
      else
         n_occ = no_of_occupied_ANOs_(self,group)
         call create_(W,n_bf,n_occ)               ! W = columns of ANO's
         call make_ANO_matrix_(self,W,group)
      end if
      call create_(Y,n_bf,n_bf)                   ! Y = overlap matrix for group(:) basis
      call make_overlap_matrix_(self,Y,group,group)
      call create_(X,n_occ,n_occ)
      call change_basis_(Y,X,W)                   ! X = S in the ANO basis
      call destroy_(Y)
      call create_(Y,n_occ,n_occ)
      call to_inverse_of_(Y,X)                    ! Y = (ANO overlap matrix)^{-1}
      call destroy_(X)
      call back_transform_(Y,P,W)                 ! P = Y in the group(:) AO basis
      call destroy_(Y)
      call destroy_(W)
     STOP_TIMER("ROBY:make_projection_matrix")
      CHECK
   end subroutine

   subroutine make_ANO_matrix(self,ANO,group,tol)
    ROBY :: self
   ! Make the "ANO" matrix, comprised of columns of the occupied atomic
   ! natural orbitals, for each atom whose index appears in "group".
   ! If "tol" is present, use this cutoff to determine what is an occupied
   ! natural orbital.
      REALMAT(:,:) :: ANO
      INTVEC(:) :: group
      REAL, optional :: tol
      REAL :: eps
      INT :: n_gr,a,ra,b,n,n_bf,n_occ
      STACK("ROBY:make_ANO_matrix")
      START_TIMER("ROBY:make_ANO_matrix")
      ENSURE(size(ANO,1)==n_bf_(self,group),"ROBY:make_ANO_matrix ... wrong shape, ANO")
      ENSURE(size(ANO,2)==no_of_occupied_ANOs_(self,group,tol),"ROBY:make_ANO_matrix ... wrong shape, ANO")
      eps = self%occupied_ANO_cutoff
      if (present(tol)) eps = tol
      n_gr = size(group)
      ANO = ZERO
      b = 0; n = 0
      do a = 1,n_gr
         ra    = group(a)
         n_bf  = n_bf_(self%atom(ra))
         n_occ = no_of_occupied_NOs_(self%atom(ra),tol=eps)
         ANO(b+1:b+n_bf,n+1:n+n_occ) = self%atom(ra)%natural_orbitals%restricted(:,1:n_occ)
         b = b + n_bf
         n = n + n_occ
      end do
     STOP_TIMER("ROBY:make_ANO_matrix")
      CHECK
   end subroutine

   subroutine make_shared_operator(self,R)
    ROBY :: self
   ! constructs the roby_shared_operator R_AB = P_A + P_B - P_AB
   ! if spin_case is supplied then either the alpha or beta
   ! operator is constructed, depending on the value of spin_case
      REALMAT(:,:) :: R
      REALMAT(:,:), PTR :: P_A,P_B,P_AB
      INT :: n_a, n_b, n_ab
      STACK("ROBY:make_shared_operator")
      START_TIMER("ROBY:make_shared_operator")
      ENSURE(associated(self%atom_a),"ROBY:make_shared_operator ... No roby atom A")
      ENSURE(associated(self%atom_b),"ROBY:make_shared_operator ... No roby atom B")
      ENSURE(associated(self%atom_ab),"ROBY:make_shared_operator ... No roby atom AB")
      n_a  = n_bf_a_(self)
      n_b  = n_bf_b_(self)
      n_ab = n_bf_ab_(self)
      call create_(P_A,n_a,n_a)
      call create_(P_B,n_b,n_b)
      call create_(P_AB,n_ab,n_ab)
      call make_projection_matrix_(self,P_A ,self%atom_a)
      call make_projection_matrix_(self,P_B ,self%atom_b)
      call make_projection_matrix_(self,P_AB,self%atom_ab)
      R = ZERO
      R(    1:n_a ,     1:n_a ) =  P_A
      R(n_a+1:n_ab, n_a+1:n_ab) =  P_B
      R = R - P_AB
      call destroy_(P_AB)
      call destroy_(P_B)
      call destroy_(P_A)
     STOP_TIMER("ROBY:make_shared_operator")
      CHECK
   end subroutine

   subroutine make_ionic_operator(self,I)
    ROBY :: self
   ! constructs the roby/gould ionic operator I_AB = P_A - P_B
       REALMAT(:,:) :: I
      REALMAT(:,:), PTR :: P_B,P_A
      INT :: n_a, n_b, n_ab
   STACK("ROBY:make_ionic_operator")
   START_TIMER("ROBY:make_ionic_operator")
   ENSURE(associated(self%atom_a),"ROBY:make_ionic_operator ... No roby atom A")
   ENSURE(associated(self%atom_b),"ROBY:make_ionic_operator ... No roby atom B")
   ENSURE(associated(self%atom_ab),"ROBY:make_ionic_operator ... No roby atom AB")
      n_a  = n_bf_a_(self)
      n_b  = n_bf_b_(self)
      n_ab = n_bf_ab_(self)
      call create_(P_A,n_a,n_a)
      call create_(P_B,n_b,n_b)
      call make_projection_matrix_(self,P_A,self%atom_a)
      call make_projection_matrix_(self,P_B,self%atom_b)
      I = ZERO
      I(    1:n_a ,     1:n_a ) =  P_A
      I(n_a+1:n_ab, n_a+1:n_ab) = -P_B
      call destroy_(P_B); call destroy_(P_A)
     STOP_TIMER("ROBY:make_ionic_operator")
      CHECK
   end subroutine

   subroutine diagonalise_V_AB_operator(self,X,eigenvectors,eigenvalues)
    ROBY :: self
   ! For a given operator "X" (for example, C = cos theta or S= sin theta)
   ! in the concatenated AO basis of the atoms in .roby.atom_group,
   !    X = \sum_{i,j} |i> X_{ij} <j|, |i>,|j> in V_{AB},
   ! diagonalise and return the "eigenvectors" and "eigenvalues".
      REALMAT(:,:) :: X,eigenvectors
      REALVEC(:) :: eigenvalues
      REALMAT(:,:), PTR :: XX,XV, SS,SI,SH
      INT :: n_ab
      STACK("ROBY:diagonalise_V_AB_operator")
      START_TIMER("ROBY:diagonalise_V_AB_operator")
      ENSURE(associated(self%atom_a),"ROBY:diagonalise_V_AB_operator ... No roby atom A")
      ENSURE(associated(self%atom_b),"ROBY:diagonalise_V_AB_operator ... No roby atom B")
      ENSURE(associated(self%atom_ab),"ROBY:diagonalise_V_AB_operator ... No roby atom AB")
      n_ab = n_bf_ab_(self)
      call create_(XX,n_ab,n_ab); call create_(XV,n_ab,n_ab)
      call create_(SS,n_ab,n_ab); call create_(SI,n_ab,n_ab); call create_(SH,n_ab,n_ab)
      call make_overlap_matrix_(self,SS,self%atom_ab,self%atom_ab)
      call to_sqrt_(SH,SS)
      call to_inverse_of_(SI,SH)
      call back_transform_(X,XX,SH)
      call solve_eigenproblem_(XX,eigenvalues,XV)
      call to_product_of_(eigenvectors,SI,XV)
      call destroy_(SH); call destroy_(SI); call destroy_(SS)
      call destroy_(XV); call destroy_(XX)
     STOP_TIMER("ROBY:diagonalise_V_AB_operator")
      CHECK
   end subroutine

! ***************************
! Make roby projected density
! ***************************

   subroutine make_projected_density(self,rho,density,ANO)
    ROBY :: self
   ! Make the Roby-projected density matrix in the concatenated basis
   ! of AO functions of the atoms in .atom_ab and copy it into "rho",
   ! a .n_bf x .n_bf matrix where all other matrix elements not corresponding
   ! to basis functions in .atom_ab are made zero. If present, "density" is
   ! used instead of the restricted density matrix. If present, "ANO" are used
   ! for the Roby projection.
      REALMAT(:,:) :: rho,density
      REALMAT(:,:), optional :: ANO
      REALMAT(:,:), PTR :: D,P,rho_P
      INT :: n_bf
   STACK("ROBY:make_projected_density")
   START_TIMER("ROBY:make_projected_density")
   ENSURE(size(rho,1)==n_bf_(self),"ROBY:make_projected_density ... rho has wrong shape")
   ENSURE(is_square_(rho),"ROBY:make_projected_density ... rho has wrong shape")
   ENSURE(size(density,1)==n_bf_(self),"ROBY:make_projected_density ... wrong shape, density")
   ENSURE(is_square_(density),"ROBY:make_projected_density ... wrong shape, density")
   ENSURE(associated(self%atom_ab),"ROBY:make_projected_density ... No atom AB group")
      n_bf = n_bf_ab_(self)
      call create_(D,n_bf,n_bf)
      call overlap_transform_(self,density,D,col_atom=self%atom_ab)
      call create_(P,n_bf,n_bf)
      call make_projection_matrix_(self,P,self%atom_ab,ANO)
      call create_(rho_P,n_bf,n_bf)
      call change_basis_(D,rho_P,P) ! rho_P(small) = P_W(small) D(small) P_W(small)
      call AO_subspace_set_(self,rho,rho_P,A_row_atom=self%atom_ab,A_col_atom=self%atom_ab)
      call destroy_(rho_P)
      call destroy_(P)
      call destroy_(D)
     STOP_TIMER("ROBY:make_projected_density")
      CHECK
   end subroutine

! ***********************************************
! Low level Roby routines used all over the place
! ***********************************************

   function expectation(self,X,group,rho) result(res)
    ROBY :: self
   ! Get the expectation value of the operator matrix "X" over a certain
   ! "group" of atoms, i.e:
   !    res = Trace ( S(:,group) X S(group,:) rho )
   ! Note that "group" are the indices of the atoms whose basis functions
   ! are used to define the matrix "X".
       REALMAT(:,:) :: X
      INTVEC(:) :: group
      OPMATRIX, PTR, optional :: rho
      REAL :: res
      OPMATRIX, PTR :: density
       REALMAT(:,:), PTR :: W
      INT :: n_bf
   STACK("ROBY:expectation")
   START_TIMER("ROBY:expectation")
   ENSURE(size(X,1)==n_bf_(self,group),"ROBY:expectation ... wrong X dimension")
   ENSURE(is_square_(X),"ROBY:expectation ... X is not square")
      density => self%rho
      if (present(rho)) density => rho
      n_bf = n_bf_(self)
      call create_(W,n_bf,n_bf)
      call overlap_transform_(self,X,W,group)
      if (self%mult/=1) then
        res = trace_product_with_(density%alpha,W) &
            + trace_product_with_(density%beta,W)
      else
        res = trace_product_with_(density%restricted,W)
      end if
      call destroy_(W)
     STOP_TIMER("ROBY:expectation")
      CHECK
   end function

   subroutine project(self,X,P,Y,row_atom,col_atom)
    ROBY :: self
   ! Do: Y = P^T S(col_atom,row_atom) X S(row_atom,col_atom) P
   ! where S(col_atom,row_atom) is the AO subspace section of the full
   ! overlap matrix specified by the basis functions on the atom indices
   ! in "row_atom" and "col_atom", and "P" is a matrix
      REALMAT(:,:) :: X,P,Y
      INTVEC(:) :: row_atom,col_atom
       REALMAT(:,:), PTR :: W
   STACK("ROBY:project")
   START_TIMER("ROBY:project")
   ENSURE(size(X,1)==n_bf_(self,row_atom),"ROBY:project ... wrong X dimension")
   ENSURE(size(Y,1)==n_bf_(self,col_atom),"ROBY:project ... wrong Y dimension")
   ENSURE(size(Y,1)==size(P,1),"ROBY:project ... wrong P dimension")
   ENSURE(is_square_(X),"ROBY:project ... X is not square")
   ENSURE(is_square_(Y),"ROBY:project ... Y is not square")
   ENSURE(is_square_(P),"ROBY:project ... P is not square")
      call create_(W,size(Y,1),size(Y,2))
      call overlap_transform_(self,X,W,row_atom,col_atom)
      call change_basis_(W,Y,P)
      call destroy_(W)
     STOP_TIMER("ROBY:project")
      CHECK
   end subroutine

   subroutine overlap_transform(self,X,Y,row_atom,col_atom)
    ROBY :: self
   ! Do: Y = S(col_atom,row_atom) X S(row_atom,col_atom)
   ! where S(col_atom,row_atom) is the AO subspace section of the full
   ! overlap matrix specified by the basis functions on the atom indices
   ! in "row_atom" and "col_atom".
      REALMAT(:,:) :: X,Y
      INTVEC(:), optional :: row_atom, col_atom
      REALMAT(:,:), PTR :: SS
      INT :: n_row,n_col
      STACK("ROBY:overlap_transform")
      START_TIMER("ROBY:overlap_transform")
      n_row = n_bf_(self,row_atom)
      n_col = n_bf_(self,col_atom)
      call create_(SS,n_row,n_col)
      call make_overlap_matrix_(self,SS,row_atom,col_atom)
      call change_basis_(X,Y,SS)
      call destroy_(SS)
     STOP_TIMER("ROBY:overlap_transform")
      CHECK
   end subroutine

   subroutine right_overlap_transform(self,X,Y,row_atom,col_atom)
    ROBY :: self
   ! Do: Y = X S(row_atom,col_atom), where S(row_atom,col_atom) is the
   ! AO subspace section of the full overlap matrix specified by the
   ! basis functions on the atom indices in "row_atom" and "col_atom".
      REALMAT(:,:) :: X,Y
      INTVEC(:), optional :: row_atom, col_atom
      REALMAT(:,:), PTR :: SS
      INT :: n_row,n_col
      STACK("ROBY:right_overlap_transform")
      START_TIMER("ROBY:right_overlap_transform")
      n_row = n_bf_(self,row_atom)
      n_col = n_bf_(self,col_atom)
      call create_(SS,n_row,n_col)
      call make_overlap_matrix_(self,SS,row_atom,col_atom)
      call to_product_of_(Y,X,SS)
      call destroy_(SS)
     STOP_TIMER("ROBY:right_overlap_transform")
      CHECK
   end subroutine

   subroutine make_overlap_matrix(self,SS,row_atom,col_atom)
    ROBY :: self
   ! Make an overlap section "SS" from the basis functions of the atoms
   ! specified in "row_atom" and "col_atom".
      REALMAT(:,:) :: SS
      INTVEC(:), optional :: row_atom,col_atom
   STACK("ROBY:make_overlap_matrix")
   START_TIMER("ROBY:make_overlap_matrix")
   ENSURE(associated(self%overlap_matrix),"ROBY:make_overlap_matrix ... no overlap matrix")
   ENSURE(size(SS,1)==n_bf_(self,row_atom),"ROBY:make_overlap_matrix ... wrong shape, SS")
   ENSURE(size(SS,2)==n_bf_(self,col_atom),"ROBY:make_overlap_matrix ... wrong shape, SS")
      call AO_subspace_set_(self,SS,self%overlap_matrix,B_row_atom=row_atom,B_col_atom=col_atom)
     STOP_TIMER("ROBY:make_overlap_matrix")
      CHECK
   end subroutine

   subroutine AO_subspace_set(self,A,B,A_row_atom,A_col_atom,B_row_atom,B_col_atom)
    ROBY :: self
   ! If either "B_row_atom" or "B_col_atom" is present, then set "A" equal to
   ! the AO subspace blocks of "B" specified by the atom indices in "B_row_atom"
   ! and "B_col_atom". If either is missing, then copy the entire row or col, i.e.
   !    A(small) = B(B_row_atom,B_col_atom)
   ! If either "A_row_atom" or "A_col_atom" is present, then set the AO
   ! subspace blocks of "A" specified by the atom indices in "A_row_atom" and
   ! "A_col_atom" equal to "B". If either is missing then copy the entire row or
   ! column. Uncopied blocks are set to zero, i.e.
   !    A(A_row_atom,A_col_atom) = B(small)
      REALMAT(:,:) :: A,B
      INTVEC(:), optional :: A_row_atom,A_col_atom
      INTVEC(:), optional :: B_row_atom,B_col_atom
      INT :: n_bf,n_row_atoms,n_col_atoms, a1,a2, b1,b2
      INT :: i,b_i,f_i,l_i,n_i, j,b_j,f_j,l_j,n_j
      INTVEC(:), PTR :: first_basis_fn_for,last_basis_fn_for
      STACK("ROBY:AO_subspace_set")
      START_TIMER("ROBY:AO_subspace_set")
      n_bf = n_bf_(self)
      call make_atom_basis_fn_limits_(self%atom,first_basis_fn_for,last_basis_fn_for)
      if (present(B_row_atom) OR present(B_col_atom)) then
         ENSURE(size(B,1)==n_bf,"ROBY:AO_subspace_set ... B has wrong shape")
         ENSURE(size(B,2)==n_bf,"ROBY:AO_subspace_set ... B has wrong shape")
         if (present(B_row_atom) AND present(B_col_atom)) then
            n_row_atoms = size(B_row_atom)
            n_col_atoms = size(B_col_atom)
            a1 = n_bf_(self%atom(B_row_atom))
            a2 = n_bf_(self%atom(B_col_atom))
            ENSURE(size(A,1)==a1,"ROBY:AO_subspace_set ... A has wrong shape")
            ENSURE(size(A,2)==a2,"ROBY:AO_subspace_set ... A has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(B_row_atom(i))
               l_i = last_basis_fn_for( B_row_atom(i))
               n_i = n_bf_(self%atom(B_row_atom(i)))
               b_j = 0
               do j = 1,n_col_atoms
                  f_j = first_basis_fn_for(B_col_atom(j))
                  l_j = last_basis_fn_for( B_col_atom(j))
                  n_j = n_bf_(self%atom(B_col_atom(j)))
                  A(b_i+1:b_i+n_i,b_j+1:b_j+n_j) = B(f_i:l_i,f_j:l_j)
                  b_j = b_j + n_j
               end do
               b_i = b_i + n_i
            end do
         else if (present(B_row_atom)) then
            n_row_atoms = size(B_row_atom)
            a1 = n_bf_(self%atom(B_row_atom))
            a2 = n_bf_(self)
            ENSURE(size(A,1)==a1,"ROBY:AO_subspace_set ... A has wrong shape")
            ENSURE(size(A,2)==a2,"ROBY:AO_subspace_set ... A has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(B_row_atom(i));
               l_i = last_basis_fn_for( B_row_atom(i))
               n_i = n_bf_(self%atom(B_row_atom(i)))
               A(b_i+1:b_i+n_i,:) = B(f_i:l_i,:)
               b_i = b_i + n_i
            end do
         else if (present(B_col_atom)) then
            n_col_atoms = size(B_col_atom)
            a1 = n_bf_(self)
            a2 = n_bf_(self%atom(B_col_atom))
            ENSURE(size(A,1)==a1,"ROBY:AO_subspace_set ... A has wrong shape")
            ENSURE(size(A,2)==a2,"ROBY:AO_subspace_set ... A has wrong shape")
            b_j = 0
            do j = 1,n_col_atoms
               f_j = first_basis_fn_for(B_col_atom(j));
               l_j = last_basis_fn_for( B_col_atom(j))
               n_j = n_bf_(self%atom(B_col_atom(j)))
               A(:,b_j+1:b_j+n_j) = B(:,f_j:l_j)
               b_j = b_j + n_j
            end do
         end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else if (present(A_row_atom) OR present(A_col_atom)) then
         ENSURE(size(A,1)==n_bf_(self),"ROBY:AO_subspace_set ... A has wrong shape")
         ENSURE(size(A,2)==n_bf_(self),"ROBY:AO_subspace_set ... A has wrong shape")
         A = ZERO
         if (present(A_row_atom) AND present(A_col_atom)) then
            n_row_atoms = size(A_row_atom)
            n_col_atoms = size(A_col_atom)
            b1 = n_bf_(self%atom(A_row_atom))
            b2 = n_bf_(self%atom(A_col_atom))
            ENSURE(size(B,1)==b1,"ROBY:AO_subspace_set ... B has wrong shape")
            ENSURE(size(B,2)==b2,"ROBY:AO_subspace_set ... B has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(A_row_atom(i))
               l_i = last_basis_fn_for( A_row_atom(i))
               n_i = n_bf_(self%atom(A_row_atom(i)))
               b_j = 0
               do j = 1,n_col_atoms
                  f_j = first_basis_fn_for(A_col_atom(j))
                  l_j = last_basis_fn_for( A_col_atom(j))
                  n_j = n_bf_(self%atom(A_col_atom(j)))
                  A(f_i:l_i,f_j:l_j) = B(b_i+1:b_i+n_i,b_j+1:b_j+n_j)
                  b_j = b_j + n_j
               end do
               b_i = b_i + n_i
            end do
         else if (present(A_row_atom)) then
            n_row_atoms = size(A_row_atom)
            b1 = n_bf_(self%atom(A_row_atom))
            b2 = n_bf_(self)
            ENSURE(size(B,1)==b1,"ROBY:AO_subspace_set ... B has wrong shape")
            ENSURE(size(B,2)==b2,"ROBY:AO_subspace_set ... B has wrong shape")
            b_i = 0
            do i = 1,n_row_atoms
               f_i = first_basis_fn_for(A_row_atom(i));
               l_i = last_basis_fn_for( A_row_atom(i))
               n_i = n_bf_(self%atom(A_row_atom(i)))
               A(f_i:l_i,:) = B(b_i+1:b_i+n_i,:)
               b_i = b_i + n_i
            end do
         else if (present(A_col_atom)) then
            n_col_atoms = size(A_col_atom)
            b1 = n_bf_(self%atom(A_col_atom))
            b2 = n_bf_(self)
            ENSURE(size(B,1)==b1,"ROBY:AO_subspace_set ... B has wrong shape")
            ENSURE(size(B,2)==b2,"ROBY:AO_subspace_set ... B has wrong shape")
            b_j = 0
            do j = 1,n_col_atoms
               f_j = first_basis_fn_for(A_col_atom(j));
               l_j = last_basis_fn_for( A_col_atom(j))
               n_j = n_bf_(self%atom(A_col_atom(j)))
               A(:,f_j:l_j) = B(:,b_j+1:b_j+n_j)
               b_j = b_j + n_j
            end do
         end if
      else
         DIE("ROBY:AO_subspace_set ... must specify A or B row atoms")
      end if
      call destroy_(last_basis_fn_for)
      call destroy_(first_basis_fn_for)
     STOP_TIMER("ROBY:AO_subspace_set")
      CHECK
   end subroutine

! **************
! Output methods
! **************

   subroutine put(self)
    ROBY :: self
   ! Put to stdout the Roby atom data
      INT :: i
      STRVEC(STR_SIZE,:), PTR :: symbol
   STACK("ROBY:put")
   START_TIMER("ROBY:put")
   ENSURE(associated(self%atom),"ROBY:put ... no atom data")
      call flush_(stdout)
      call text_(stdout,"Roby population analysis information:",flush=2)
      call show_(stdout,"Kind of calculation     = ",self%roby_kind)
      call show_(stdout,"Homoleptic calculation? = ",is_homoleptic_(self))
      if (is_homoleptic_(self)) then
         do i = 1,n_group_(self)
            symbol => numbered_chemical_symbols_(self,self%atom_group(i)%element)
            call show_(stdout,"Atom group "//trim(to_str_(i))//"            = ",symbol)
            call destroy_(symbol)
         end do
      end if
      if (associated(self%atom_list)) then
         symbol => numbered_chemical_symbols_(self,self%atom_list)
         call show_(stdout,"Atom list               = ",symbol)
         call show_(stdout,"No. of atoms            = ",size(symbol))
         call destroy_(symbol)
      end if
      call text_(stdout,"Cutoff's in degrees:")
      call show_(stdout,"Covalent index cutoff   = ",to_units_(self%covalent_cutoff,"degree"))
      call show_(stdout,"Ionic bond index cutoff = ",to_units_(self%ionic_cutoff,"degree"))
      call show_(stdout,"Zero angle cutoff       = ",to_units_(self%zero_cutoff,"degree"))
      call show_(stdout,"Pi/2 cutoff             = ",to_units_(self%pi_on_2_cutoff,"degree"))
      call show_(stdout,"Occupied ANO cutoff     = ",self%occupied_ANO_cutoff)
      call show_(stdout,"Analyse all atom pairs? = ",self%analyse_all_atom_pairs)
      call show_(stdout,"Bond scale factor       = ",self%bond_scale_factor)
      call show_(stdout,"Output theta info?      = ",self%output_theta_info)
      call show_(stdout,"Molecule charge         = ",self%charge)
      call show_(stdout,"Molecule multiplicity   = ",self%mult)
     STOP_TIMER("ROBY:put")
      CHECK
   end subroutine

   subroutine put_all_info(self)
    ROBY :: self
   ! Put to stdout the results of a complete bond analysis
      STACK("ROBY:put_all_info")
      START_TIMER("ROBY:put_all_info")
      call put_(self)
      call put_populations_(self)
      call put_pair_populations_(self)
      call put_bond_indices_(self)
      call put_charges_(self)
      call put_dipole_moments_(self)
     STOP_TIMER("ROBY:put_all_info")
      CHECK
   end subroutine

   subroutine put_populations(self)
    ROBY :: self
   ! Put to stdout the only Roby atom populations
      INT :: a,n_group
      BIN :: homoleptic
      STR(STR_SIZE) :: symb
      STRVEC(STR_SIZE,:), PTR :: symbol
   STACK("ROBY:put_populations")
   START_TIMER("ROBY:put_populations")
   ENSURE(associated(self%n1),"ROBY:put_populations ... no atom populations exist")
   ENSURE(associated(self%atom),"ROBY:put_populations ... no atom data")
   ENSURE(associated(self%atom_group),"ROBY:put_populations ... no atom group data")
      n_group = n_group_(self)
      homoleptic = is_homoleptic_(self)
      if (NOT homoleptic) symbol => numbered_chemical_symbols_(self,self%atom_list)
      call flush_(stdout)
      call text_(stdout,"Roby atom populations:",flush=2)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Population")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do a = 1,n_group
        if (homoleptic) then; symb = to_str_(a)
        else                ; symb = symbol(a)
        end if
        call put_(stdout,symb,int_width=TRUE)
        call put_(stdout,self%n1(a))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      if (NOT homoleptic) call destroy_(symbol)
     STOP_TIMER("ROBY:put_populations")
      CHECK
   end subroutine

   subroutine put_pair_populations(self)
    ROBY :: self
   ! Put to stdout the Roby atom pair populations, including the shared
   ! population
      INT :: a,b,n_group
      BIN :: homoleptic
      STR(STR_SIZE) :: symb_a,symb_b
      REAL :: shared
      STRVEC(STR_SIZE,:), PTR :: symbol
   STACK("ROBY:put_pair_populations")
   START_TIMER("ROBY:put_pair_populations")
   ENSURE(associated(self%n1),"ROBY:put_pair_populations ... no populations exist")
   ENSURE(associated(self%n2),"ROBY:put_pair_populations ... no pair populations exist")
   ENSURE(associated(self%atom),"ROBY:put_pair_populations ... no atom data")
   ENSURE(associated(self%atom_group),"ROBY:put_pair_populations ... no atom group data")
      n_group = n_group_(self)
      homoleptic = is_homoleptic_(self)
      if (NOT homoleptic) symbol => numbered_chemical_symbols_(self,self%atom_list)
      call flush_(stdout)
      call text_(stdout,"Roby pair populations:",flush=2)
      call dash_(stdout,int_fields=2,real_fields=4)
      call put_(stdout,"Atom_a",int_width=TRUE)
      call put_(stdout,"Atom_b",int_width=TRUE)
      call put_(stdout,"n_a")
      call put_(stdout,"n_b")
      call put_(stdout,"n_ab")
      call put_(stdout,"s_ab")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=4)
      do a = 1,n_group
      do b = 1,a-1
        if (homoleptic) then; symb_a = to_str_(a)
        else                ; symb_a = symbol(a)
        end if
        if (homoleptic) then; symb_b = to_str_(b)
        else                ; symb_b = symbol(b)
        end if
        call put_(stdout,symb_a,int_width=TRUE)
        call put_(stdout,symb_b,int_width=TRUE)
        call put_(stdout,self%n1(a))
        call put_(stdout,self%n1(b))
        call put_(stdout,self%n2(a,b))
        shared = self%n1(a) + self%n1(b) - self%n2(a,b)
        call put_(stdout,shared)
        call flush_(stdout)
      end do
      end do
      call dash_(stdout,int_fields=2,real_fields=4)
      if (NOT homoleptic) call destroy_(symbol)
     STOP_TIMER("ROBY:put_pair_populations")
      CHECK
   end subroutine

   subroutine put_bond_indices(self)
    ROBY :: self
   ! Put to stdout the Roby bond indices, including the shared populations
      INT :: a,b,n_group
      BIN :: homoleptic
      STR(STR_SIZE) :: symb_a,symb_b
      REAL :: shared
      STRVEC(STR_SIZE,:), PTR :: symbol
   STACK("ROBY:put_bond_indices")
   START_TIMER("ROBY:put_bond_indices")
   ENSURE(associated(self%n1),"ROBY:put_bond_indices ... no populations exist")
   ENSURE(associated(self%n2),"ROBY:put_bond_indices ... no pair populations exist")
   ENSURE(associated(self%bond_index),"ROBY:put_bond_indices ... no bond indices exist")
   ENSURE(associated(self%percent_covalency),"ROBY:put_bond_indices ... no covalencies")
   ENSURE(associated(self%atom_group),"ROBY:put_bond_indices ... no atom group data")
      n_group = n_group_(self)
      homoleptic = is_homoleptic_(self)
      if (NOT homoleptic) symbol => numbered_chemical_symbols_(self,self%atom_list)
      call flush_(stdout)
      call text_(stdout,"Roby bond indices and populations:",flush=2)
      call dash_(stdout,int_fields=2,real_fields=6)
      call put_(stdout,"Atom_a",int_width=TRUE)
      call put_(stdout,"Atom_b",int_width=TRUE)
      call put_(stdout,"n_a")
      call put_(stdout,"n_b")
      call put_(stdout,"n_ab")
      call put_(stdout,"s_ab")
      call put_(stdout,"Bond Index")
      call put_(stdout,"% Covalency")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      do a = 1,n_group
      do b = 1,a-1
        if (homoleptic) then; symb_a = to_str_(a)
        else                ; symb_a = symbol(a)
        end if
        if (homoleptic) then; symb_b = to_str_(b)
        else                ; symb_b = symbol(b)
        end if
        call put_(stdout,symb_a,int_width=TRUE)
        call put_(stdout,symb_b,int_width=TRUE)
        call put_(stdout,self%n1(a))
        call put_(stdout,self%n1(b))
        call put_(stdout,self%n2(a,b))
        shared = self%n1(a) + self%n1(b) - self%n2(a,b)
        call put_(stdout,shared)
        call put_(stdout,self%bond_index(a,b))
        call put_(stdout,self%percent_covalency(a,b))
        call flush_(stdout)
      end do
      end do
      call dash_(stdout,int_fields=2,real_fields=6)
      if (NOT homoleptic) call destroy_(symbol)
     STOP_TIMER("ROBY:put_bond_indices")
      CHECK
   end subroutine

   subroutine put_charges(self)
    ROBY :: self
   ! Put to stdout the group charges.
      INT :: a,n_group
      BIN :: homoleptic
      STR(STR_SIZE) :: symb
      STRVEC(STR_SIZE,:), PTR :: symbol
   STACK("ROBY:put_charges")
   START_TIMER("ROBY:put_charges")
   ENSURE(associated(self%gould_charge),"ROBY:put_charges ... no Roby-Gould charges")
   ENSURE(associated(self%cruickshank_charge),"ROBY:put_charges ... no Cruickshank charges")
   ENSURE(associated(self%atom),"ROBY:put_charges ... no atom data")
   ENSURE(associated(self%atom_group),"ROBY:put_charges ... no atom group data")
      n_group = n_group_(self)
      homoleptic = is_homoleptic_(self)
      if (NOT homoleptic) symbol => numbered_chemical_symbols_(self,self%atom_list)
      call flush_(stdout)
      call text_(stdout,"Group charges:",flush=2)
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Roby-Gould")
      call put_(stdout,"Cruickshank")
      call put_(stdout,"Sum_b  Sab/2")
      call put_(stdout,"Sum_bc Sabc/3")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do a = 1,n_group
        if (homoleptic) then; symb = to_str_(a)
        else                ; symb = symbol(a)
        end if
        call put_(stdout,symb,int_width=TRUE)
        call put_(stdout,self%gould_charge(a))
        call put_(stdout,self%cruickshank_charge(a))
        call put_(stdout,self%summed_n2(a)/TWO)
        call put_(stdout,self%summed_n3(a)/THREE)
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
      if (NOT homoleptic) call destroy_(symbol)
     STOP_TIMER("ROBY:put_charges")
      CHECK
   end subroutine

   subroutine put_dipole_moments(self)
    ROBY :: self
   ! Dipole moments are calculated from the charges
      INT :: a,n_group
      REALVEC(3) :: gd,cd
   STACK("ROBY:put_dipole_moments")
   START_TIMER("ROBY:put_dipole_moments")
   ENSURE(associated(self%gould_charge),"ROBY:put_dipole_moments ... no Roby-Gould charges")
   ENSURE(associated(self%cruickshank_charge),"ROBY:put_dipole_moments ... no Cruickshank charges")
   ENSURE(associated(self%atom),"ROBY:put_dipole_moments ... no atom data")
      n_group = n_group_(self)
      gd = ZERO
      cd = ZERO
      do a = 1,n_group
         gd(:) = gd(:) + self%gould_charge(a)*self%atom(a)%pos(:)
         cd(:) = cd(:) + self%cruickshank_charge(a)*self%atom(a)%pos(:)
      end do
      ! convert from AU to Debyes
      gd = gd * conversion_factor_(("debye"))
      cd = cd * conversion_factor_(("debye"))
      call flush_(stdout)
      call text_(stdout,"Dipole moments (in DEBYE):",flush=2)
      call dash_(stdout,int_fields=1,real_fields=5)
      call put_(stdout,"Charge type")
      call put_(stdout,"X")
      call put_(stdout,"Y")
      call put_(stdout,"Z")
      call put_(stdout,"Magnitude")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      call put_(stdout,"Roby-Gould")
      call put_(stdout,gd(1))
      call put_(stdout,gd(2))
      call put_(stdout,gd(3))
      call put_(stdout,sqrt(gd(1)**2 + gd(2)**2 + gd(3)**2))
      call flush_(stdout)
      call put_(stdout,"Cruickshank")
      call put_(stdout,cd(1))
      call put_(stdout,cd(2))
      call put_(stdout,cd(3))
      call put_(stdout,sqrt(cd(1)**2 + cd(2)**2 + cd(3)**2))
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
     STOP_TIMER("ROBY:put_dipole_moments")
      CHECK
   end subroutine

   subroutine put_numbered_chemical_symbols(self,group,name)
    ROBY :: self
   ! Output atom labels for the atom indices given in "group"
      INTVEC(:) :: group
      STR(*), optional :: name
      STRVEC(STR_SIZE,:), PTR :: symbol
       INT :: a
      STR(STR_SIZE) :: label
      STACK("ROBY:put_numbered_chemical_symbols")
      START_TIMER("ROBY:put_numbered_chemical_symbols")
      label = " "
      if (present(name)) label = name
      symbol => numbered_chemical_symbols_(self,group)
      call put_(stdout,"ATOM "//trim(label)//" =")
      do a = 1,size(symbol)
         call put_(stdout,symbol(a),int_width=TRUE)
      end do
      call flush_(stdout)
      call destroy_(symbol)
     STOP_TIMER("ROBY:put_numbered_chemical_symbols")
      CHECK
   end subroutine

   subroutine put_theta_info(self)
    ROBY :: self
   ! Output Theta-space information
      INT :: i,n_ab,angle
      STACK("ROBY:put_theta_info")
      START_TIMER("ROBY:put_theta_info")
      ENSURE(associated(self%atom_a),"ROBY:put_theta_info ... No Roby atom A data")
      ENSURE(associated(self%atom_b),"ROBY:put_theta_info ... No Roby atom B data")
      ENSURE(associated(self%atom_ab),"ROBY:put_theta_info ... No Roby atom AB data")
      n_ab = n_bf_ab_(self)
      call flush_(stdout)
      call text_(stdout,"=== ALL ROBY EIGENVALUES:")
      call flush_(stdout)
      call text_(stdout,"Here all the eigenvalues are output in the order they")
      call text_(stdout,"come out of the diagonalization routine. Next to the ")
      call text_(stdout,"eigenvalues are the theta subspace angles. The pair  ")
      call text_(stdout,"index shows which eigenvalues have been paired       ")
      call flush_(stdout)
      call put_numbered_chemical_symbols_(self,self%atom_a,"(A)")
      call put_numbered_chemical_symbols_(self,self%atom_b,"(B)")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=2)
      call put_(stdout,"Eigvec",int_width=TRUE)
      call put_(stdout,"cos theta")
      call put_(stdout,"sin theta")
      call put_(stdout,"theta/dg",int_width=TRUE)
      call put_(stdout,"pair",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=2)
      do i = 1,n_ab
        call put_(stdout,i)
        call put_(stdout,self%eval_C(i))
        call put_(stdout,self%eval_I(i))
        angle = nint(self%theta_angle(i))
        call put_(stdout,angle)
        call put_(stdout,self%pair(i))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=2)
      call flush_(stdout)
     STOP_TIMER("ROBY:put_theta_info")
      CHECK
   end subroutine

   subroutine put_theta_bond_info(self)
    ROBY :: self
   ! Output Theta-space bond information
      INT :: i,n_ab,angle
      STACK("ROBY:put_theta_bond_info")
      START_TIMER("ROBY:put_theta_bond_info")
      n_ab = n_bf_ab_(self)
      call flush_(stdout)
      call text_(stdout,"=== ROBY BOND INDEX INFORMATION:")
      call flush_(stdout)
      call put_numbered_chemical_symbols_(self,self%atom_a,"(A)")
      call put_numbered_chemical_symbols_(self,self%atom_b,"(B)")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      call put_(stdout,"Spaces",int_width=TRUE)
      call put_(stdout,"THETA/dg",int_width=TRUE)
      call put_(stdout,"C+ pop")
      call put_(stdout,"C- pop")
      call put_(stdout,"C index")
      call put_(stdout,"I+ pop")
      call put_(stdout,"I- pop")
      call put_(stdout,"I index")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=6)
      do i = 1,n_ab
        if (self%pair(i)<1 OR i<self%pair(i)) cycle
        call put_(stdout,"("//trim(to_str_(i))//","//trim(to_str_(self%pair(i)))//")",int_width=TRUE)
        angle = nint(self%theta_angle(i))
        call put_(stdout,angle)
        call put_(stdout,self%pop_C(i))
        call put_(stdout,self%pop_C(self%pair(i)))
        call put_(stdout,self%covalent_index(i))
        call put_(stdout,self%pop_I(i))
        call put_(stdout,self%pop_I(self%pair(i)))
        call put_(stdout,self%ionic_index(i))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=6)
      call flush_(stdout)
     STOP_TIMER("ROBY:put_theta_bond_info")
      CHECK
   end subroutine

   subroutine put_theta_atom_pops(self)
    ROBY :: self
   ! Output Roby angles and populations
      INT :: n_ab,i,angle
      STACK("ROBY:put_theta_atom_pops")
      START_TIMER("ROBY:put_theta_atom_pops")
      ENSURE(associated(self%atom_a),"ROBY:put_theta_atom_pops ... No Roby atom A data")
      ENSURE(associated(self%atom_b),"ROBY:put_theta_atom_pops ... No Roby atom B data")
      ENSURE(associated(self%atom_ab),"ROBY:put_theta_atom_pops ... No Roby atom AB data")
      n_ab = n_bf_ab_(self)
      call flush_(stdout)
      call text_(stdout,"=== ROBY ANGLES AND POPULATAIONS:")
      call flush_(stdout)
      call text_(stdout,"Here are the populations for the orbitals in the previous table.")
      call text_(stdout,"The +/- indicates the sign of the eigenvalue.")
      call flush_(stdout)
      call put_numbered_chemical_symbols_(self,self%atom_a,"(A)")
      call put_numbered_chemical_symbols_(self,self%atom_b,"(B)")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call put_(stdout,"EIGVEC",int_width=TRUE)
      call put_(stdout,"THETA/dg",int_width=TRUE)
      call put_(stdout,"POP (A)")
      call put_(stdout,"POP (B)")
      call put_(stdout,"TOTAL")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=3)
      call flush_(stdout)
      do i = 1, n_ab
        call put_(stdout,i)
        angle = nint(self%theta_angle(i))
        call put_(stdout,angle)
        call put_(stdout,self%pop_A(i))
        call put_(stdout,self%pop_B(i))
        call put_(stdout,self%pop_A(i)+self%pop_B(i))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=3)
     STOP_TIMER("ROBY:put_theta_atom_pops")
      CHECK
   end subroutine

   subroutine put_shared_population(self)
    ROBY :: self
   ! Put to stdout the multiple shared Roby atom populations
      INT :: a,n_group
      STRVEC(STR_SIZE,:), PTR :: symbol
      STACK("ROBY:put_shared_population")
      START_TIMER("ROBY:put_shared_population")
      ENSURE(associated(self%n1),"ROBY:put_shared_population ... no subgroup population totals exist")
      ENSURE(associated(self%atom),"ROBY:put_shared_population ... no atom data")
      ENSURE(associated(self%atom_group),"ROBY:put_shared_population ... no atom group data")
      n_group = n_group_(self)
      call flush_(stdout)
      call text_(stdout,"Roby multiple shared population:",flush=2)
      call show_(stdout,"Multiple shared population =",self%n_shared)
      call text_(stdout,"Contributions by subgroup size:",flush=2)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"Subgrp size",int_width=TRUE)
      call put_(stdout,"Subgrp Pop.")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do a = 1,n_group
        call put_(stdout,symbol(a),int_width=TRUE)
        call put_(stdout,self%n1(a))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
      call destroy_(symbol)
     STOP_TIMER("ROBY:put_shared_population")
      CHECK
   end subroutine

end
