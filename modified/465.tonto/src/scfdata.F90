!---------------------------------------------------------------------------
!
!  SCFDATA: Store SCF data and deal with iteration control ......
!
! Copyright (C) Daniel Grimwood, 1998
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
! $Id: scfdata.foo,v 1.67.2.9 2004/04/21 09:12:56 reaper Exp $
!---------------------------------------------------------------------------

module SCFDATA_MODULE

#  include "scfdata.use"

   implicit none

#  include "macros"
#  include "scfdata.int"


contains

!  *******************
!  Allocation routines
!  *******************

   subroutine create(self)
    SCFDATA :: self
   ! Create space for an SCF type
     PTR :: self
     STACK("SCFDATA:create")
     START_TIMER("SCFDATA:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(SCFDATA_SIZE)
     call nullify_ptr_part_(self)
     call set_defaults_(self)
     STOP_TIMER("SCFDATA:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SCFDATA :: self
   ! Destroy space for an SCF type
     PTR :: self
     STACK("SCFDATA:destroy")
     START_TIMER("SCFDATA:destroy")
     if (associated(self)) then
       call destroy_ptr_part_(self)
       DELETE_MEMORY(SCFDATA_SIZE)
       deallocate(self)
     end if
     STOP_TIMER("SCFDATA:destroy")
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
    SCFDATA :: self
   ! Nullify the pointer parts of self
      STACK("SCFDATA:nullify_ptr_part")
      START_TIMER("SCFDATA:nullify_ptr_part")
      call nullify_ptr_part_(self%diis)
     STOP_TIMER("SCFDATA:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SCFDATA :: self
   ! Destroy the pointer parts of self
      STACK("SCFDATA:destroy_ptr_part")
      START_TIMER("SCFDATA:destroy_ptr_part")
      call destroy_ptr_part_(self%diis)
     STOP_TIMER("SCFDATA:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine create_copy(self,scfdata)
    SCFDATA :: self
   ! Create a copy of the "scfdata" object
      PTR :: self
      SCFDATA :: scfdata
      STACK("SCFDATA:create_copy")
      START_TIMER("SCFDATA:create_copy")
      call create_(self)
      call copy_(self,scfdata)
     STOP_TIMER("SCFDATA:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,scfdata)
    SCFDATA :: self
   ! Make a copy of the "scfdata" object
      SCFDATA :: scfdata
      STACK("SCFDATA:copy")
      START_TIMER("SCFDATA:copy")
      self = scfdata
      call copy_(self%diis,scfdata%diis)
     STOP_TIMER("SCFDATA:copy")
      UNSTACK
   end subroutine

   subroutine delete_diis_archives(self)
    SCFDATA :: self
   ! Delete the DIIS archives on disk
     STACK("SCFDATA:delete_diis_archives")
     START_TIMER("SCFDATA:delete_diis_archives")
     call delete_archives_(self%diis)
     STOP_TIMER("SCFDATA:delete_diis_archives")
      CHECK
   end subroutine

   subroutine cleanup_diis(self)
    SCFDATA :: self
   ! Delete the DIIS archives on disk, and restore the DIIS object so it can be
   ! reused.  Does not go back to the default DIIS parameters.
     STACK("SCFDATA:cleanup_diis")
     START_TIMER("SCFDATA:cleanup_diis")
     call cleanup_(self%diis)
     STOP_TIMER("SCFDATA:cleanup_diis")
      CHECK
   end subroutine

!  ************
!  Set routines
!  ************

   subroutine set_defaults(self)
    SCFDATA :: self
   ! Set default SCF data values
     STACK("SCFDATA:set_defaults")
     START_TIMER("SCFDATA:set_defaults")
     self%scf_kind                  = " " ! Purposely set blank so diis comes later
     self%dft_exchange              = "slater"
     self%dft_correlation           = "lyp"
     self%dft_non_local_exchange    = FALSE
     self%dft_non_local_correlation = TRUE
     self%initial_density         = "core"
     self%initial_mos             = " "
     self%nuclear_energy          = ZERO
     self%kinetic_energy          = ZERO
     self%energy                  = ZERO
     self%old_energy              = ZERO
     self%dft_energy_correction   = ZERO
     self%difference              = ZERO
     self%convergence             = SCFDATA_CONVERGENCE             ! TOL(4)
     self%diis_convergence        = SCFDATA_DIIS_CONVERGENCE        ! TOL(4)
     self%diis_start_iteration    = SCFDATA_DIIS_START_ITERATION    ! 3
     self%using_rough_convergence = SCFDATA_USING_ROUGH_CONVERGENCE ! TRUE
     self%rough_convergence       = SCFDATA_ROUGH_CONVERGENCE       ! TOL(2)
     self%rough_diis_convergence  = SCFDATA_ROUGH_DIIS_CONVERGENCE  ! TOL(2)
     self%iteration               = 0
     self%total_iterations        = 0
     self%lambda_iteration        = 0
     self%min_iterations          = SCFDATA_MIN_ITERATIONS
     self%max_iterations          = SCFDATA_MAX_ITERATIONS
     self%lambda                  = 0
     self%lambda_max              = 0
     self%lambda_step             = 1
     self%fit_value               = ZERO
     self%old_fit_value           = ZERO
     self%F_chi2                  = ZERO
     self%old_F_chi2              = ZERO
     self%F_gof                   = ZERO
     self%F_r_factor              = ZERO
     self%F_weighted_r_factor     = ZERO
     self%test                    = FALSE
     self%direct                  = FALSE
     self%using_delta_build       = FALSE
     self%using_fock_diis         = TRUE
     self%using_MO_diis           = FALSE
     self%using_damping           = SCFDATA_USING_DAMPING ! TRUE
     self%using_level_shift       = SCFDATA_USING_LEVEL_SHIFT ! TRUE
     self%using_camp_king         = FALSE
     self%camp_king_iters         = 0
     self%using_dynamic_damping   = FALSE
     self%dynamic_damp_factor     = ZERO
     call set_defaults_(self%diis)
     self%diis_error              = ZERO
     self%old_diis_error          = ZERO
     self%using_diis_auto_start   = FALSE
     self%damp_finish             = SCFDATA_DIIS_START_ITERATION
     self%damp_factor             = SCFDATA_DAMP_FACTOR
     self%level_shift             = SCFDATA_LEVEL_SHIFT
     self%level_shift_finish      = SCFDATA_DIIS_START_ITERATION
     self%output                  = TRUE
     self%nddo                    = FALSE
     self%nudo                    = FALSE
     self%pie                     = FALSE
     self%using_bl_term           = TRUE
     self%using_bs_term           = TRUE
     self%using_bs_t_term         = TRUE
     self%using_aa_term           = TRUE
     self%using_1e_sl_term        = TRUE
     self%using_1e_srxa_term      = TRUE
     self%using_2e_sl_term        = TRUE
     self%using_1e_zora_term      = FALSE
     self%sl_1e_factor            = ONE
     self%sl_2e_factor            = ONE
     self%eri_limit               = SCFDATA_ERI_LIMIT
     self%old_eri_cutoff          = SCFDATA_ERI_LIMIT
     self%quantization_axis       = (/ ZERO, ZERO, ONE /)
     self%group                   = FALSE
     self%using_MO_gradient_update= FALSE
     self%MO_gradient_stepsize    = 0.01d0
     self%max_update_stepsize     = 0.01d0
     STOP_TIMER("SCFDATA:set_defaults")
      CHECK
   end subroutine

   subroutine set(self,nuclear_energy,energy,kinetic_energy,dft_energy_correction,crystal)
    SCFDATA :: self
   ! Set various parts of the scfdata type.
     REAL, IN, optional :: nuclear_energy,energy,kinetic_energy,dft_energy_correction
     CRYSTAL, PTR, optional :: crystal
     STACK("SCFDATA:set")
     START_TIMER("SCFDATA:set")
     if (present(nuclear_energy))        self%nuclear_energy = nuclear_energy
     if (present(dft_energy_correction)) self%dft_energy_correction = dft_energy_correction
     if (present(crystal)) then
     if (associated(crystal)) then
       if (associated(crystal%reflections)) then
         self%old_F_chi2 = self%F_chi2
         self%F_chi2 = F_chi2_(crystal)
         self%F_gof = F_goodness_of_fit_(crystal)
         self%F_r_factor = F_r_factor_(crystal)
         self%F_weighted_r_factor = F_weighted_r_factor_(crystal)
       end if
     end if
     end if
     if (present(energy)) then
       self%energy = energy
       if (fitting_(self)) self%fit_value = energy + self%lambda * self%F_chi2
     end if
     if (present(kinetic_energy))    self%kinetic_energy = kinetic_energy
     STOP_TIMER("SCFDATA:set")
      CHECK
   end subroutine

   subroutine reset(self,nuclear_energy,energy,kinetic_energy,dft_energy_correction,crystal)
    SCFDATA :: self
   ! Reset SCF energy and iteration data , but don't change
   ! nuclear_energy, convergence, direct, or max_it options
      REAL, IN, optional :: nuclear_energy,energy,kinetic_energy,dft_energy_correction
      CRYSTAL, PTR, optional :: crystal
     STACK("SCFDATA:reset")
     START_TIMER("SCFDATA:reset")
     self%energy                = ZERO
     self%fit_value             = ZERO
     self%old_fit_value         = ZERO
     self%kinetic_energy        = ZERO
     self%old_energy            = ZERO
     self%dft_energy_correction = ZERO
     self%iteration             = 0
     self%total_iterations      = 0
     self%lambda_iteration      = 0
     self%F_chi2                = ZERO
     self%F_gof                 = ZERO
     self%F_r_factor            = ZERO
     self%F_weighted_r_factor   = ZERO
     self%old_eri_cutoff        = eri_cutoff_(self)
     call set_(self,nuclear_energy,energy,kinetic_energy,dft_energy_correction,crystal)
     if (present(energy)) self%difference = energy
     if (present(crystal)) self%old_fit_value = ZERO
     if (present(dft_energy_correction)) self%dft_energy_correction = dft_energy_correction
     STOP_TIMER("SCFDATA:reset")
      CHECK
   end subroutine

   subroutine update_lambda(self)
    SCFDATA :: self
   ! Increments lambda by lambda_step
     STACK("SCFDATA:update_lambda")
     START_TIMER("SCFDATA:update_lambda")
     self%lambda           = self%lambda + self%lambda_step
     self%lambda_iteration = self%lambda_iteration + 1
     self%iteration        = 0
     self%old_fit_value    = self%old_energy + self%lambda * self%old_F_chi2
     self%fit_value        = self%energy + self%lambda * self%F_chi2
     STOP_TIMER("SCFDATA:update_lambda")
      CHECK
   end subroutine

   subroutine update(self,energy,kinetic_energy,dft_energy_correction,crystal)
    SCFDATA :: self
   ! Update the SCF energy and/or chi2 and increment iteration and any other
   ! data of use in the SCF calculation.
     REAL, IN, optional :: energy,kinetic_energy,dft_energy_correction
     CRYSTAL, PTR, optional :: crystal
     STACK("SCFDATA:update")
     START_TIMER("SCFDATA:update")
     self%old_fit_value = self%fit_value
     self%old_energy = self%energy
     call set_(self,energy=energy,kinetic_energy=kinetic_energy, &
                dft_energy_correction=dft_energy_correction,crystal=crystal)
     self%iteration = self%iteration + 1
     self%total_iterations = self%total_iterations + 1
     self%difference = self%energy - self%old_energy
     ! For updating the fock matrix in incremental builds
     self%old_eri_cutoff = eri_cutoff_(self)
     STOP_TIMER("SCFDATA:update")
      CHECK
   end subroutine

   subroutine set_diis_error(self,err)
    SCFDATA :: self
   ! Set the DIIS error and the starting iteration for automatic DIIS start
     REAL, IN :: err
     BIN :: set_start
     STACK("SCFDATA:set_diis_error")
     START_TIMER("SCFDATA:set_diis_error")
     self%old_diis_error = self%diis_error
     self%diis_error = err
     set_start = self%using_diis_auto_start AND &
                 self%diis_start_iteration == self%max_iterations AND &
                (self%diis_error<self%rough_diis_convergence)
!     if (set_start) .diis.set_start(.iteration)
     STOP_TIMER("SCFDATA:set_diis_error")
      CHECK
   end subroutine

   function spinorbital_kind(self,scf_kind) result(res)
    SCFDATA :: self
   ! Return the kind of spinorbitals used in a particular "scf_kind"
   ! (i.e. for the fock_matrix and density_matrix, but not neccesarily the mo's)
     STR(*), optional :: scf_kind
     STR(STR_SIZE) :: res
     STR(STR_SIZE) :: s_kind
     STACK("SCFDATA:spinorbital_kind")
     START_TIMER("SCFDATA:spinorbital_kind")
     s_kind = self%scf_kind
     if (present(scf_kind)) s_kind = scf_kind
     select case (s_kind)
       case("rhf","rdft","restricted_hartree_fock"); res = "restricted"
       case("rohf", "restricted_open_shell_hartree_fock"); res = "unrestricted"
       case("uhf","udft","unrestricted_hartree_fock");    res = "unrestricted"
       case("ghf","general_hartree_fock");         res = "general"
       case("rchf","crhf", &
            "restricted_complex_hartree_fock", &
            "complex_restricted_hartree_fock");    res = "restricted_complex"
       case("uchf","cuhf", &
            "unrestricted_complex_hartree_fock", &
            "complex_unrestricted_hartree_fock");  res = "unrestricted_complex"
       case("gchf","cghf", &
            "general_complex_hartree_fock", &
            "complex_general_hartree_fock");       res = "general_complex"
       case("xray_rhf", &
            "xray_restricted_hartree_fock");       res = "restricted"
       case("xray_rks");                           res = "restricted"
       case("xray_rdft");                          res = "restricted"
       case("xray_udft");                          res = "unrestricted"
       case("rdvpt","restricted_dvpt");            res = "restricted"
       case("noninteracting-group-rhf");           res = "restricted"
       case default; DIE("SCFDATA:spinorbital_kind ... unknown scf kind")
     end select
     STOP_TIMER("SCFDATA:spinorbital_kind")
      CHECK
   end function

   function molecular_orbital_kind(self,scf_kind) result(res)
    SCFDATA :: self
   ! Return the kind of spinorbitals used for the molecular orbitals in a
   ! particular "scf_kind"
     STR(*), optional :: scf_kind
     STR(STR_SIZE) :: res
     STR(STR_SIZE) :: s_kind
      STACK("SCFDATA:molecular_orbital_kind")
      START_TIMER("SCFDATA:molecular_orbital_kind")
      s_kind = self%scf_kind
     if (present(scf_kind)) s_kind = scf_kind
     select case (s_kind)
       case("rhf","rdft","restricted_hartree_fock"); res = "restricted"
       case("rohf", &
            "restricted_open_shell_hartree_fock"); res = "restricted"
       case("uhf","udft", "unrestricted_hartree_fock");    res = "unrestricted"
       case("ghf","general_hartree_fock");         res = "general"
       case("rchf","crhf", &
            "restricted_complex_hartree_fock", &
            "complex_restricted_hartree_fock");    res = "restricted_complex"
       case("uchf","cuhf", &
            "unrestricted_complex_hartree_fock", &
            "complex_unrestricted_hartree_fock");  res = "unrestricted_complex"
       case("gchf","cghf", &
            "general_complex_hartree_fock", &
            "complex_general_hartree_fock");       res = "general_complex"
       case("xray_rhf", &
            "xray_restricted_hartree_fock");       res = "restricted"
       case("xray_rks");                           res = "restricted"
       case("xray_rdft");                          res = "restricted"
       case("xray_udft");                          res = "unrestricted"
       case("rdvpt","restricted_dvpt");            res = "restricted"
       case("noninteracting-group-rhf");           res = "restricted"
       case default; DIE("SCFDATA:molecular_orbital_kind ... unknown scf kind")
     end select
     STOP_TIMER("SCFDATA:molecular_orbital_kind")
      CHECK
   end function

   function orbital_energy_kind(self,scf_kind) result(res)
    SCFDATA :: self
   ! Return the kind of vectors used for the orbital energies in a
   ! particular "scf_kind"
      STR(*), optional :: scf_kind
      STR(STR_SIZE) :: res
      STACK("SCFDATA:orbital_energy_kind")
      START_TIMER("SCFDATA:orbital_energy_kind")
      res = orbital_energies_kind_(self,scf_kind)
     STOP_TIMER("SCFDATA:orbital_energy_kind")
      CHECK
   end function

   function orbital_energies_kind(self,scf_kind) result(res)
    SCFDATA :: self
   ! Return the kind of vectors used for the orbital energies in a
   ! particular "scf_kind"
      STR(*), optional :: scf_kind
      STR(STR_SIZE) :: res
      STR(STR_SIZE) :: s_kind
      STACK("SCFDATA:orbital_energies_kind")
      START_TIMER("SCFDATA:orbital_energies_kind")
      s_kind = self%scf_kind
      if (present(scf_kind)) s_kind = scf_kind
      select case (s_kind)
         case("rhf","rdft","restricted_hartree_fock");      res = "restricted"
         case("rohf","restricted_open_shell_hartree_fock"); res = "restricted"
         case("uhf","udft", "unrestricted_hartree_fock");   res = "unrestricted"
         case("ghf","general_hartree_fock");                res = "general"
         case("rchf","crhf", &
              "restricted_complex_hartree_fock", &
              "complex_restricted_hartree_fock");           res = "restricted"
         case("uchf","cuhf", &
              "unrestricted_complex_hartree_fock", &
              "complex_unrestricted_hartree_fock");         res = "unrestricted"
         case("gchf","cghf", &
              "general_complex_hartree_fock", &
              "complex_general_hartree_fock");              res = "general"
         case("xray_rhf","xray_restricted_hartree_fock");   res = "restricted"
         case("xray_rks");                                  res = "restricted"
         case("xray_rdft");                                 res = "restricted"
         case("xray_udft");                                 res = "unrestricted"
         case("rdvpt","restricted_dvpt");                   res = "restricted"
         case("noninteracting-group-rhf");                  res = "restricted"
         case default; DIE("SCFDATA:orbital_energies_kind ... unknown scf kind")
      end select
     STOP_TIMER("SCFDATA:orbital_energies_kind")
      CHECK
   end function

   function number_kind(self) result(res)
    SCFDATA :: self
   ! Return the kind of numbers used for a particular "kind" of scf calculation
      STR(STR_SIZE) :: res
      STACK("SCFDATA:number_kind")
      START_TIMER("SCFDATA:number_kind")
      select case (self%scf_kind)
         case("rhf","rdft","rohf","uhf","udft", "ghf");    res = "real"
         case("rchf","uchf","gchf");                       res = "complex"
         case("restricted_hartree_fock");                  res = "real"
         case("restricted_open_shell_hartree_fock");       res = "real"
         case("unrestricted_hartree_fock");                res = "real"
         case("general_hartree_fock");                     res = "real"
         case("restricted_complex_hartree_fock");          res = "complex"
         case("unrestricted_complex_hartree_fock");        res = "complex"
         case("general_complex_hartree_fock");             res = "complex"
         case("xray_rhf","xray_restricted_hartree_fock");  res = "real"
         case("xray_rks");                                 res = "real"
         case("xray_rdft","xray_udft");                    res = "real"
         case("rdvpt","restricted_dvpt");                  res = "real"
         case("noninteracting-group-rhf");                 res = "real"
         case default; DIE("SCFDATA:number_kind ... unknown scf kind")
      end select
     STOP_TIMER("SCFDATA:number_kind")
      CHECK
   end function

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    SCFDATA :: self
   ! Read data from "stdin" using keyword style input.
      STR(STR_SIZE) :: word
      STACK("SCFDATA:read_keywords")
      START_TIMER("SCFDATA:read_keywords")
      call read_(stdin,word)
   ENSURE(word=="{","SCFDATA:read_keywords ... expecting an open bracket symbol, {")
      call set_defaults_(self)
      read_loop: do             ! Loop over keywords
         call read_(stdin,word)
         if (word=="}")         exit read_loop
         if (reverted_(stdin))    exit read_loop
         call process_keyword_(self,word)
      end do read_loop
      call finalize_(self)
     STOP_TIMER("SCFDATA:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SCFDATA :: self
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("SCFDATA:process_keyword")
      START_TIMER("SCFDATA:process_keyword")
      word = keyword
      call to_lower_case_(word)
      if (includes_(word," ")) then
         call redirect_(stdin,(/word/))
         call read_keywords_(self)
         call revert_(stdin)
      else
         select case (word)
            case ("}                       ")  ! exit case
            case ("1e_sl_factor=           "); call read_(stdin,self%sl_1e_factor)
            case ("2e_sl_factor=           "); call read_(stdin,self%sl_2e_factor)
            case ("camp-king=              "); call read_(stdin,self%using_camp_king)
            case ("convergence=            "); call read_convergence_(self)
            case ("converge=               "); call read_convergence_(self)
            case ("damp_factor=            "); call read_(stdin,self%damp_factor)
            case ("damp_finish=            "); call read_(stdin,self%damp_finish)
            case ("dft_correlation=        "); call read_dft_correlation_(self)
            case ("dft_exchange=           "); call read_dft_exchange_(self)
            case ("diis=                   "); call read_fock_diis_(self)
            case ("diis_auto_start=        "); call read_diis_auto_start_(self)
            case ("diis_convergence=       "); call read_diis_convergence_(self)
            case ("diis_keep=              "); call read_diis_keep_(self)
            case ("diis_start=             "); call read_diis_start_(self)
            case ("direct=                 "); call read_direct_(self)
            case ("dynamic_damping=        "); call read_(stdin,self%using_dynamic_damping)
            case ("eri_cutoff=             "); call read_eri_limit_(self)
            case ("eri_limit=              "); call read_eri_limit_(self)
            case ("fock_diis=              "); call read_fock_diis_(self)
            case ("guess=                  "); call read_initial_density_(self)
            case ("initial_density=        "); call read_initial_density_(self)
            case ("initial_guess=          "); call read_initial_density_(self)
            case ("initial_lambda=         "); call read_(stdin,self%lambda)
            case ("initial_mos=            "); call read_initial_mos_(self)
            case ("kind=                   "); call read_kind_(self)
            case ("lambda_max=             "); call read_(stdin,self%lambda_max)
            case ("lambda_min=             "); call read_(stdin,self%lambda)
            case ("lambda_step=            "); call read_(stdin,self%lambda_step)
            case ("level_shift=            "); call read_(stdin,self%level_shift)
            case ("level_shift_finish=     "); call read_(stdin,self%level_shift_finish)
            case ("min_iterations=         "); call read_min_iterations_(self)
            case ("min_it=                 "); call read_min_iterations_(self)
            case ("max_iterations=         "); call read_max_iterations_(self)
            case ("max_it=                 "); call read_max_iterations_(self)
            case ("max_update_stepsize=    "); call read_(stdin,self%max_update_stepsize)
            case ("mo_diis=                "); call read_mo_diis_(self)
            case ("mo_gradient_stepsize=   "); call read_(stdin,self%MO_gradient_stepsize)
            case ("nddo=                   "); call read_(stdin,self%nddo)
            case ("nudo=                   "); call read_(stdin,self%nudo)
            case ("output=                 "); call read_(stdin,self%output)
            case ("pie=                    "); call read_(stdin,self%pie)
            case ("quantization_axis=      "); call read_(stdin,self%quantization_axis)
            case ("rough_convergence=      "); call read_rough_convergence_(self)
            case ("rough_diis_convergence= "); call read_rough_diis_convergence_(self)
            case ("scf_kind=               "); call read_kind_(self)
            case ("scf_type=               "); call read_kind_(self)
            case ("sl_1e_factor=           "); call read_(stdin,self%sl_1e_factor)
            case ("sl_2e_factor=           "); call read_(stdin,self%sl_2e_factor)
            case ("test=                   "); call read_(stdin,self%test)
            case ("use_1e_sl_term=         "); call read_(stdin,self%using_1e_sl_term)
            case ("use_1e_s(rxa)_term=     "); call read_(stdin,self%using_1e_srxa_term)
            case ("use_1e_zora_term=       "); call read_(stdin,self%using_1e_zora_term)
            case ("use_2e_sl_term=         "); call read_(stdin,self%using_2e_sl_term)
            case ("use_aa_term=            "); call read_(stdin,self%using_aa_term)
            case ("use_bl_term=            "); call read_(stdin,self%using_bl_term)
            case ("use_bs_term=            "); call read_(stdin,self%using_bs_term)
            case ("use_bs_t_term=          "); call read_(stdin,self%using_bs_t_term)
            case ("use_damping=            "); call read_(stdin,self%using_damping)
            case ("use_delta_build=        "); call read_delta_build_(self)
            case ("use_diis=               "); call read_fock_diis_(self)
            case ("use_fock_diis=          "); call read_fock_diis_(self)
            case ("use_mo_diis=            "); call read_MO_diis_(self)
            case ("use_mo_gradient_update= "); call read_MO_gradient_update_(self)
            case ("use_level_shifting=     "); call read_(stdin,self%using_level_shift)
            case ("use_level_shift=        "); call read_(stdin,self%using_level_shift)
            case ("use_rough_convergence=  "); call read_(stdin,self%using_rough_convergence)
            case default;           allocate(tonto%known_keywords(67))
            tonto%known_keywords(1) = "}                       "
            tonto%known_keywords(2) = "1e_sl_factor=           "
            tonto%known_keywords(3) = "2e_sl_factor=           "
            tonto%known_keywords(4) = "camp-king=              "
            tonto%known_keywords(5) = "convergence=            "
            tonto%known_keywords(6) = "converge=               "
            tonto%known_keywords(7) = "damp_factor=            "
            tonto%known_keywords(8) = "damp_finish=            "
            tonto%known_keywords(9) = "dft_correlation=        "
            tonto%known_keywords(10) = "dft_exchange=           "
            tonto%known_keywords(11) = "diis=                   "
            tonto%known_keywords(12) = "diis_auto_start=        "
            tonto%known_keywords(13) = "diis_convergence=       "
            tonto%known_keywords(14) = "diis_keep=              "
            tonto%known_keywords(15) = "diis_start=             "
            tonto%known_keywords(16) = "direct=                 "
            tonto%known_keywords(17) = "dynamic_damping=        "
            tonto%known_keywords(18) = "eri_cutoff=             "
            tonto%known_keywords(19) = "eri_limit=              "
            tonto%known_keywords(20) = "fock_diis=              "
            tonto%known_keywords(21) = "guess=                  "
            tonto%known_keywords(22) = "initial_density=        "
            tonto%known_keywords(23) = "initial_guess=          "
            tonto%known_keywords(24) = "initial_lambda=         "
            tonto%known_keywords(25) = "initial_mos=            "
            tonto%known_keywords(26) = "kind=                   "
            tonto%known_keywords(27) = "lambda_max=             "
            tonto%known_keywords(28) = "lambda_min=             "
            tonto%known_keywords(29) = "lambda_step=            "
            tonto%known_keywords(30) = "level_shift=            "
            tonto%known_keywords(31) = "level_shift_finish=     "
            tonto%known_keywords(32) = "min_iterations=         "
            tonto%known_keywords(33) = "min_it=                 "
            tonto%known_keywords(34) = "max_iterations=         "
            tonto%known_keywords(35) = "max_it=                 "
            tonto%known_keywords(36) = "max_update_stepsize=    "
            tonto%known_keywords(37) = "mo_diis=                "
            tonto%known_keywords(38) = "mo_gradient_stepsize=   "
            tonto%known_keywords(39) = "nddo=                   "
            tonto%known_keywords(40) = "nudo=                   "
            tonto%known_keywords(41) = "output=                 "
            tonto%known_keywords(42) = "pie=                    "
            tonto%known_keywords(43) = "quantization_axis=      "
            tonto%known_keywords(44) = "rough_convergence=      "
            tonto%known_keywords(45) = "rough_diis_convergence= "
            tonto%known_keywords(46) = "scf_kind=               "
            tonto%known_keywords(47) = "scf_type=               "
            tonto%known_keywords(48) = "sl_1e_factor=           "
            tonto%known_keywords(49) = "sl_2e_factor=           "
            tonto%known_keywords(50) = "test=                   "
            tonto%known_keywords(51) = "use_1e_sl_term=         "
            tonto%known_keywords(52) = "use_1e_s(rxa)_term=     "
            tonto%known_keywords(53) = "use_1e_zora_term=       "
            tonto%known_keywords(54) = "use_2e_sl_term=         "
            tonto%known_keywords(55) = "use_aa_term=            "
            tonto%known_keywords(56) = "use_bl_term=            "
            tonto%known_keywords(57) = "use_bs_term=            "
            tonto%known_keywords(58) = "use_bs_t_term=          "
            tonto%known_keywords(59) = "use_damping=            "
            tonto%known_keywords(60) = "use_delta_build=        "
            tonto%known_keywords(61) = "use_diis=               "
            tonto%known_keywords(62) = "use_fock_diis=          "
            tonto%known_keywords(63) = "use_mo_diis=            "
            tonto%known_keywords(64) = "use_mo_gradient_update= "
            tonto%known_keywords(65) = "use_level_shifting=     "
            tonto%known_keywords(66) = "use_level_shift=        "
            tonto%known_keywords(67) = "use_rough_convergence=  "
            call unknown_(tonto,word,"SCFDATA:process_keyword")
            deallocate(tonto%known_keywords)
         end select
      end if
     STOP_TIMER("SCFDATA:process_keyword")
      UNSTACK
   end subroutine

   subroutine finalize(self)
    SCFDATA :: self
   ! Make sure the input satisfies sanity checks and generate
   ! any other missing data
   STACK("SCFDATA:finalize")
   START_TIMER("SCFDATA:finalize")
   ENSURE(self%scf_kind/=" ","SCFDATA:finalize ... no scf kind specified")
   ENSURE(self%max_iterations>=self%min_iterations,"SCFDATA:finalize ... max_it must be greater than min_it!")
      if (self%initial_mos/=" ")   self%initial_density = "--using MO's--"
      if (self%using_1e_zora_term) self%using_1e_sl_term = FALSE
      if (self%using_1e_sl_term)   self%using_1e_zora_term = FALSE
      if (self%scf_kind=="noninteracting-group-rhf") self%group = TRUE
      if (NOT self%direct)         self%using_rough_convergence = FALSE
      if (self%using_fock_diis OR self%using_MO_diis) then
         ENSURE(self%diis%keep>0,"SCFDATA:finalize ... DIIS_keep must be > 0 for MO_gradient_update")
      end if
      if (self%using_MO_diis) then
         call set_archive_name_(self%diis,"DIIS_molecular_orbitals")
      else if (self%using_fock_diis) then
         call set_archive_name_(self%diis,"DIIS_fock_matrix")
      end if
      if (self%using_diis_auto_start) self%diis_start_iteration = self%max_iterations
     STOP_TIMER("SCFDATA:finalize")
      CHECK
   end subroutine

   subroutine read_dft_exchange(self)
    SCFDATA :: self
   ! Read the SCF type
      STACK("SCFDATA:read_dft_exchange")
      START_TIMER("SCFDATA:read_dft_exchange")
      call read_(stdin,self%dft_exchange)
      call to_lower_case_(self%dft_exchange)
      select case (self%dft_exchange)
         case("none                              "); self%dft_non_local_exchange=FALSE
         case("slater                            "); self%dft_non_local_exchange=FALSE
         case("xalpha                            "); self%dft_non_local_exchange=FALSE
         case("becke88                           "); self%dft_non_local_exchange=TRUE
         case("gill96                            "); self%dft_non_local_exchange=TRUE
         case default;    allocate(tonto%known_keywords(5))
         tonto%known_keywords(1) = "none                              "
         tonto%known_keywords(2) = "slater                            "
         tonto%known_keywords(3) = "xalpha                            "
         tonto%known_keywords(4) = "becke88                           "
         tonto%known_keywords(5) = "gill96                            "
         call unknown_(tonto,self%dft_exchange,"SCFDATA:read_dft_exchange")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("SCFDATA:read_dft_exchange")
      CHECK
   end subroutine

   subroutine read_dft_correlation(self)
    SCFDATA :: self
   ! Read the SCF type
      STACK("SCFDATA:read_dft_correlation")
      START_TIMER("SCFDATA:read_dft_correlation")
      call read_(stdin,self%dft_correlation)
      call to_lower_case_(self%dft_correlation)
      select case (self%dft_correlation)
         case("none                              "); self%dft_non_local_correlation=FALSE
         case("vwn                               "); self%dft_non_local_correlation=FALSE
         case("lyp                               "); self%dft_non_local_correlation=TRUE
         case default;    allocate(tonto%known_keywords(3))
         tonto%known_keywords(1) = "none                              "
         tonto%known_keywords(2) = "vwn                               "
         tonto%known_keywords(3) = "lyp                               "
         call unknown_(tonto,self%dft_correlation,"SCFDATA:read_dft_correlation")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("SCFDATA:read_dft_correlation")
      CHECK
   end subroutine

   subroutine read_kind(self)
    SCFDATA :: self
   ! Read the SCF type
      STACK("SCFDATA:read_kind")
      START_TIMER("SCFDATA:read_kind")
      call read_(stdin,self%scf_kind)
      select case (self%scf_kind)
         case("rhf                               ")
         case("rdft                              ")
         case("udft                              ")
         case("restricted_hartree_fock           ")
         case("xray_rhf                          ")
         case("xray_rdft                         ")
         case("xray_udft                         ")
         case("rohf                              ")
         case("restricted_open_shell_hartree_fock")
         case("uhf                               ")
         case("unrestricted_hartree_fock         ")
         case("ghf                               ")
         case("general_hartree_fock              ")
         case("rchf                              ")
         case("restricted_complex_hartree_fock   ")
         case("uchf                              ")
         case("unrestricted_complex_hartree_fock ")
         case("gchf                              ")
         case("general_complex_hartree_fock      ")
         case("noninteracting-group-rhf          ")
         case default;    allocate(tonto%known_keywords(20))
         tonto%known_keywords(1) = "rhf                               "
         tonto%known_keywords(2) = "rdft                              "
         tonto%known_keywords(3) = "udft                              "
         tonto%known_keywords(4) = "restricted_hartree_fock           "
         tonto%known_keywords(5) = "xray_rhf                          "
         tonto%known_keywords(6) = "xray_rdft                         "
         tonto%known_keywords(7) = "xray_udft                         "
         tonto%known_keywords(8) = "rohf                              "
         tonto%known_keywords(9) = "restricted_open_shell_hartree_fock"
         tonto%known_keywords(10) = "uhf                               "
         tonto%known_keywords(11) = "unrestricted_hartree_fock         "
         tonto%known_keywords(12) = "ghf                               "
         tonto%known_keywords(13) = "general_hartree_fock              "
         tonto%known_keywords(14) = "rchf                              "
         tonto%known_keywords(15) = "restricted_complex_hartree_fock   "
         tonto%known_keywords(16) = "uchf                              "
         tonto%known_keywords(17) = "unrestricted_complex_hartree_fock "
         tonto%known_keywords(18) = "gchf                              "
         tonto%known_keywords(19) = "general_complex_hartree_fock      "
         tonto%known_keywords(20) = "noninteracting-group-rhf          "
         call unknown_(tonto,self%scf_kind,"SCFDATA:read_kind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("SCFDATA:read_kind")
      CHECK
   end subroutine

   subroutine read_initial_density(self)
    SCFDATA :: self
   ! Read the initial density guess
      STACK("SCFDATA:read_initial_density")
      START_TIMER("SCFDATA:read_initial_density")
      call read_(stdin,self%initial_density)
      select case (self%initial_density)
         case("core                ")
         case("fock                ")
         case("atom                ")
         case("group               ")
         case("restricted          ")
         case("unrestricted        ")
         case("general             ")
         case("restricted_complex  ")
         case("complex_unrestricted")
         case("unrestricted_complex")
         case("general_complex     ")
         case("complex_general     ")
         case default;  allocate(tonto%known_keywords(12))
         tonto%known_keywords(1) = "core                "
         tonto%known_keywords(2) = "fock                "
         tonto%known_keywords(3) = "atom                "
         tonto%known_keywords(4) = "group               "
         tonto%known_keywords(5) = "restricted          "
         tonto%known_keywords(6) = "unrestricted        "
         tonto%known_keywords(7) = "general             "
         tonto%known_keywords(8) = "restricted_complex  "
         tonto%known_keywords(9) = "complex_unrestricted"
         tonto%known_keywords(10) = "unrestricted_complex"
         tonto%known_keywords(11) = "general_complex     "
         tonto%known_keywords(12) = "complex_general     "
         call unknown_(tonto,self%initial_density,"SCFDATA:read_initial_density")
         deallocate(tonto%known_keywords)
      end select
      ! User inputted guesses are usually converged, so stay accurate.
    ! .using_rough_convergence = FALSE
    ! WARN("Rough convergence switched off by initial_density= option")
     STOP_TIMER("SCFDATA:read_initial_density")
      CHECK
   end subroutine

   subroutine read_initial_mos(self)
    SCFDATA :: self
   ! Read the initial density guess
      STACK("SCFDATA:read_initial_mos")
      START_TIMER("SCFDATA:read_initial_mos")
      call read_(stdin,self%initial_mos)
      select case (self%initial_mos)
         case("restricted          ")
         case("unrestricted        ")
         case("general             ")
         case("restricted_complex  ")
         case("complex_unrestricted")
         case("unrestricted_complex")
         case("general_complex     ")
         case("complex_general     ")
         case default;   allocate(tonto%known_keywords(8))
         tonto%known_keywords(1) = "restricted          "
         tonto%known_keywords(2) = "unrestricted        "
         tonto%known_keywords(3) = "general             "
         tonto%known_keywords(4) = "restricted_complex  "
         tonto%known_keywords(5) = "complex_unrestricted"
         tonto%known_keywords(6) = "unrestricted_complex"
         tonto%known_keywords(7) = "general_complex     "
         tonto%known_keywords(8) = "complex_general     "
         call unknown_(tonto,self%initial_mos,"SCFDATA:read_initial_mos")
         deallocate(tonto%known_keywords)
      end select
      ! User inputted guesses are usually converged, so stay accurate.
      if (self%using_rough_convergence) then
        self%using_rough_convergence = FALSE
        WARN("SCFDATA:read_initial_mos ... Rough convergence switched off by initial_mos= option")
      end if
     STOP_TIMER("SCFDATA:read_initial_mos")
      CHECK
   end subroutine

   subroutine read_min_iterations(self)
    SCFDATA :: self
   ! Read the minimum no. of SCF interations
      STACK("SCFDATA:read_min_iterations")
      START_TIMER("SCFDATA:read_min_iterations")
      call read_(stdin,self%min_iterations)
   ENSURE(self%min_iterations>=0,"SCFDATA:read_min_iterations ... min_iteration must be non-negative")
     STOP_TIMER("SCFDATA:read_min_iterations")
      CHECK
   end subroutine

   subroutine read_max_iterations(self)
    SCFDATA :: self
   ! Read the maximum no. of SCF interations
      STACK("SCFDATA:read_max_iterations")
      START_TIMER("SCFDATA:read_max_iterations")
      call read_(stdin,self%max_iterations)
   ENSURE(self%max_iterations>=0,"SCFDATA:read_max_iterations ... must be non-negative")
   ENSURE(self%max_iterations>=self%min_iterations,"SCFDATA:read_max_iterations ... smaller than min_iterations!")
     STOP_TIMER("SCFDATA:read_max_iterations")
      CHECK
   end subroutine

   subroutine read_convergence(self)
    SCFDATA :: self
   ! Read the SCF convergence criteria
      STACK("SCFDATA:read_convergence")
      START_TIMER("SCFDATA:read_convergence")
      call read_(stdin,self%convergence)
   ENSURE(self%convergence>0,"SCFDATA:read_convergence ... convergence must be positive")
      WARN_IF(self%convergence<TOL(11),"SCFDATA:read_convergence ... convergence may be too small")
     STOP_TIMER("SCFDATA:read_convergence")
      CHECK
   end subroutine

   subroutine read_rough_convergence(self)
    SCFDATA :: self
   ! Read the rough SCF convergence criteria
      STACK("SCFDATA:read_rough_convergence")
      START_TIMER("SCFDATA:read_rough_convergence")
      call read_(stdin,self%rough_convergence)
   ENSURE(self%rough_convergence>0,"SCFDATA:read_rough_convergence ... must be positive")
   ENSURE(self%rough_convergence>self%convergence,"SCFDATA:read_rough_convergence ... smaller than convergence!")
      WARN_IF(self%rough_convergence<TOL(11),"SCFDATA:read_rough_convergence ... may be too small")
     STOP_TIMER("SCFDATA:read_rough_convergence")
      CHECK
   end subroutine

   subroutine read_fock_diis(self)
    SCFDATA :: self
   ! Read whether to use DIIS for fock matrix
      STACK("SCFDATA:read_fock_diis")
      START_TIMER("SCFDATA:read_fock_diis")
      call read_(stdin,self%using_fock_diis)
      if (self%using_fock_diis) self%using_MO_diis = FALSE
     STOP_TIMER("SCFDATA:read_fock_diis")
      CHECK
   end subroutine

   subroutine read_MO_diis(self)
    SCFDATA :: self
   ! Read whether to use DIIS for molecular orbitals
      STACK("SCFDATA:read_MO_diis")
      START_TIMER("SCFDATA:read_MO_diis")
      call read_(stdin,self%using_MO_diis)
      if (self%using_MO_diis) self%using_fock_diis = FALSE
     STOP_TIMER("SCFDATA:read_MO_diis")
      CHECK
   end subroutine

   subroutine read_diis_convergence(self)
    SCFDATA :: self
   ! Read the DIIS SCF convergence criteria
      STACK("SCFDATA:read_diis_convergence")
      START_TIMER("SCFDATA:read_diis_convergence")
      call read_(stdin,self%diis_convergence)
   ENSURE(self%diis_convergence>0,"SCFDATA:read_diis_convergence ... must be positive")
      WARN_IF(self%diis_convergence<TOL(11),"SCFDATA:read_diis_convergence ... may be too small")
     STOP_TIMER("SCFDATA:read_diis_convergence")
      CHECK
   end subroutine

   subroutine read_diis_keep(self)
    SCFDATA :: self
   ! Read the number of DIIS vectors to keep
     INT :: i
     STACK("SCFDATA:read_diis_keep")
     START_TIMER("SCFDATA:read_diis_keep")
     call read_(stdin,i)
     call set_keep_(self%diis,i)
     STOP_TIMER("SCFDATA:read_diis_keep")
      CHECK
   end subroutine

   subroutine read_diis_start(self)
    SCFDATA :: self
   ! Read when to start DIIS
     INT :: i
     STACK("SCFDATA:read_diis_start")
     START_TIMER("SCFDATA:read_diis_start")
     call read_(stdin,i)
     self%diis_start_iteration = i
     STOP_TIMER("SCFDATA:read_diis_start")
      CHECK
   end subroutine

   subroutine read_diis_auto_start(self)
    SCFDATA :: self
   ! Read whether to start DIIS automatically based on the diis error
      STACK("SCFDATA:read_diis_auto_start")
      START_TIMER("SCFDATA:read_diis_auto_start")
      call read_(stdin,self%using_diis_auto_start)
     self%diis_start_iteration = self%max_iterations
     STOP_TIMER("SCFDATA:read_diis_auto_start")
      CHECK
   end subroutine

   subroutine read_rough_diis_convergence(self)
    SCFDATA :: self
   ! Read the rough DIIS SCF convergence criteria
      STACK("SCFDATA:read_rough_diis_convergence")
      START_TIMER("SCFDATA:read_rough_diis_convergence")
      call read_(stdin,self%rough_diis_convergence)
   ENSURE(self%rough_diis_convergence>0,"SCFDATA:read_rough_diis_convergence ... must be positive")
   ENSURE(self%rough_diis_convergence>self%diis_convergence,"SCFDATA:read_rough_diis_convergence ... too small")
   WARN_IF(self%rough_diis_convergence<TOL(11),"SCFDATA:read_rough_diis_convergence ... may be too small")
     STOP_TIMER("SCFDATA:read_rough_diis_convergence")
      CHECK
   end subroutine

   subroutine read_direct(self)
    SCFDATA :: self
   ! Read whether to use direct SCF or not
      STACK("SCFDATA:read_direct")
      START_TIMER("SCFDATA:read_direct")
      call read_(stdin,self%direct)
      if (self%direct) self%using_delta_build = TRUE
     STOP_TIMER("SCFDATA:read_direct")
      CHECK
   end subroutine

   subroutine read_delta_build(self)
    SCFDATA :: self
   ! Read whether to use incremental fock build
      STACK("SCFDATA:read_delta_build")
      START_TIMER("SCFDATA:read_delta_build")
      call read_(stdin,self%using_delta_build)
     STOP_TIMER("SCFDATA:read_delta_build")
      CHECK
   end subroutine

   subroutine read_MO_gradient_update(self)
    SCFDATA :: self
   ! Read whether to an MO gradient update method
      STACK("SCFDATA:read_MO_gradient_update")
      START_TIMER("SCFDATA:read_MO_gradient_update")
      call read_(stdin,self%using_MO_gradient_update)
     STOP_TIMER("SCFDATA:read_MO_gradient_update")
      CHECK
   end subroutine

   subroutine read_eri_limit(self)
    SCFDATA :: self
   ! Read the ERI cutoff limit
      STACK("SCFDATA:read_eri_limit")
      START_TIMER("SCFDATA:read_eri_limit")
      call read_(stdin,self%eri_limit)
     STOP_TIMER("SCFDATA:read_eri_limit")
      CHECK
   end subroutine

!  *****
!  Tests
!  *****

   function scf_done(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the scf procedure is done
      BIN :: res
      STACK("SCFDATA:scf_done")
      START_TIMER("SCFDATA:scf_done")
      res = (converged_(self) OR exceeded_max_it_(self)) AND exceeded_min_it_(self)
     STOP_TIMER("SCFDATA:scf_done")
      CHECK
   end function

   function converged(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the SCFDATA appears to be converged
      BIN :: res
      STACK("SCFDATA:converged")
      START_TIMER("SCFDATA:converged")
      res = diff_converged_(self) AND diis_converged_(self) &
          AND NOT apply_rough_convergence_(self) ! must use full accuracy integrals
     STOP_TIMER("SCFDATA:converged")
      CHECK
   end function

   function roughly_converged(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the SCFDATA is roughly converged
      BIN :: res
     STACK("SCFDATA:roughly_converged")
     START_TIMER("SCFDATA:roughly_converged")
     res = abs(self%difference) < self%rough_convergence AND &
           abs(self%diis_error) < self%rough_diis_convergence
     STOP_TIMER("SCFDATA:roughly_converged")
      CHECK
   end function

   function apply_rough_convergence(self) result(res)
    SCFDATA :: self
   ! Return TRUE if applying rough integral convergence this iteration
     BIN :: res
     STACK("SCFDATA:apply_rough_convergence")
     START_TIMER("SCFDATA:apply_rough_convergence")
     res = self%using_rough_convergence AND self%direct
     res = res AND (NOT roughly_converged_(self)) AND self%lambda_iteration==0
     STOP_TIMER("SCFDATA:apply_rough_convergence")
      CHECK
   end function

   function diff_converged(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the energy difference has converged
      BIN :: res
      STACK("SCFDATA:diff_converged")
      START_TIMER("SCFDATA:diff_converged")
      if (fitting_(self)) then
        res = abs(self%fit_value-self%old_fit_value) < self%convergence
      else
        res = abs(self%difference) < self%convergence
      end if
     STOP_TIMER("SCFDATA:diff_converged")
      CHECK
   end function

   function diis_converged(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the gradient/DIIS error has converged
      BIN :: res
      STACK("SCFDATA:diis_converged")
      START_TIMER("SCFDATA:diis_converged")
      res = abs(self%diis_error) < self%diis_convergence
     STOP_TIMER("SCFDATA:diis_converged")
      CHECK
   end function

   function exceeded_max_it(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the SCFDATA has exceeded the maximum iterations
      BIN :: res
      STACK("SCFDATA:exceeded_max_it")
      START_TIMER("SCFDATA:exceeded_max_it")
      res = self%iteration >= self%max_iterations
     STOP_TIMER("SCFDATA:exceeded_max_it")
      CHECK
   end function

   function exceeded_min_it(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the SCFDATA has exceeded the minimum iterations
      BIN :: res
      STACK("SCFDATA:exceeded_min_it")
      START_TIMER("SCFDATA:exceeded_min_it")
      res = self%iteration >= self%min_iterations
      if (fitting_(self)) res = res AND (self%iteration > 1)
     STOP_TIMER("SCFDATA:exceeded_min_it")
      CHECK
   end function

   function exceeded_lambda_max(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the SCFDATA has exceeded the maximum lambda
      BIN :: res
      STACK("SCFDATA:exceeded_lambda_max")
      START_TIMER("SCFDATA:exceeded_lambda_max")
      res = self%lambda > (ONE+TOL(10)) * self%lambda_max
                         ! TOL(10) allows for roundoff errors
      if (self%lambda_step < TOL(10)) res = TRUE
     STOP_TIMER("SCFDATA:exceeded_lambda_max")
      CHECK
   end function

   function eri_cutoff(self) result(res)
    SCFDATA :: self
   ! Return a value to eliminate small integrals in direct SCF calculations
     REAL :: res
     STACK("SCFDATA:eri_cutoff")
     START_TIMER("SCFDATA:eri_cutoff")
     if (apply_rough_convergence_(self)) then
        res = SCFDATA_ERI_LIMIT_ROUGH
     else
        res = self%eri_limit
     end if
     STOP_TIMER("SCFDATA:eri_cutoff")
      CHECK
   end function

   function eri_cutoff_altered(self) result(res)
    SCFDATA :: self
   ! Return TRUE if the eri_cutoff has changed since the last .update.
   ! This function is needed for recreating the fock matrix where
   ! incremental builds are used.
      BIN :: res
      STACK("SCFDATA:eri_cutoff_altered")
      START_TIMER("SCFDATA:eri_cutoff_altered")
      res = self%old_eri_cutoff/=eri_cutoff_(self)
     STOP_TIMER("SCFDATA:eri_cutoff_altered")
      CHECK
   end function

   function do_delta_build(self) result(res)
    SCFDATA :: self
   ! Return TRUE if a delta fock matrix build is allowed (assuming that the old
   ! fock matrix and old density matrix are available)
      BIN :: res
      STACK("SCFDATA:do_delta_build")
      START_TIMER("SCFDATA:do_delta_build")
      res = self%using_delta_build AND NOT eri_cutoff_altered_(self)
      res = res AND NOT includes_(self%scf_kind,"dft")
     STOP_TIMER("SCFDATA:do_delta_build")
      CHECK
   end function

!  **********************
!  DIIS tests. Be careful
!  **********************

   function apply_fock_diis(self) result(res)
    SCFDATA :: self
   ! Return TRUE if DIIS extrapolation is to be used for extrapolating
   ! the fock matrix (this is the default). Currently the only alternative
   ! is gradient extrapolation of the orbitals.
      BIN :: res
      STACK("SCFDATA:apply_fock_diis")
      START_TIMER("SCFDATA:apply_fock_diis")
      res = apply_diis_(self) AND self%using_fock_diis
     STOP_TIMER("SCFDATA:apply_fock_diis")
      CHECK
   end function

   function apply_MO_diis(self) result(res)
    SCFDATA :: self
   ! Return TRUE if DIIS extrapolation is to be used for extrapolating
   ! the fock matrix (this is the default). Currently the only alternative
   ! is gradient extrapolation of the orbitals.
      BIN :: res
      STACK("SCFDATA:apply_MO_diis")
      START_TIMER("SCFDATA:apply_MO_diis")
      res = apply_diis_(self) AND self%using_MO_diis
     STOP_TIMER("SCFDATA:apply_MO_diis")
      CHECK
   end function

   function using_diis(self) result(res)
    SCFDATA :: self
   ! Return TRUE if DIIS extrapolation is to be used
      BIN :: res
      STACK("SCFDATA:using_diis")
      START_TIMER("SCFDATA:using_diis")
      res = self%using_fock_diis OR self%using_MO_diis
     STOP_TIMER("SCFDATA:using_diis")
      CHECK
   end function

   function diis_used(self) result(res)
    SCFDATA :: self
   ! Return TRUE if DIIS extrapolation has *really* been used this iteration
   ! (The first time doesn't really count, see apply_diis below for that case)
      BIN :: res
      STACK("SCFDATA:diis_used")
      START_TIMER("SCFDATA:diis_used")
      if (NOT using_diis_(self)) then
         res = FALSE
      else
         res = (self%total_iterations > self%diis_start_iteration) AND self%diis%keep > 1
      end if
     STOP_TIMER("SCFDATA:diis_used")
      CHECK
   end function

   function apply_diis(self) result(res)
    SCFDATA :: self
   ! Return TRUE if DIIS extrapolation must be applied this iteration,
   ! or has been applied this iteration.
      BIN :: res
      STACK("SCFDATA:apply_diis")
      START_TIMER("SCFDATA:apply_diis")
      if (NOT using_diis_(self)) then
         res = FALSE
      else
         res = (self%total_iterations >= self%diis_start_iteration) AND self%diis%keep > 1
      end if
!      ! Do not have diis at lambda=0 between lambda increments.
!      if (.fitting) then
!        if (.iteration==0 AND .lambda_iteration>0) res = FALSE
!      end
     STOP_TIMER("SCFDATA:apply_diis")
      CHECK
   end function

   function apply_camp_king(self) result(res)
    SCFDATA :: self
   ! Return TRUE if Camp-King converger is to be used this iteration
     BIN :: res
!    res = .using_camp_king AND (.diis_error > .old_diis_error)
     STACK("SCFDATA:apply_camp_king")
     START_TIMER("SCFDATA:apply_camp_king")
     res = self%using_camp_king AND spinorbital_kind_(self) == "restricted"
     STOP_TIMER("SCFDATA:apply_camp_king")
      CHECK
   end function

   function apply_dynamic_damping(self) result(res)
    SCFDATA :: self
   ! Return TRUE if Camp-King converger is to be used this iteration
     BIN :: res
     STACK("SCFDATA:apply_dynamic_damping")
     START_TIMER("SCFDATA:apply_dynamic_damping")
     res = self%using_dynamic_damping AND NOT apply_damping_(self)
     STOP_TIMER("SCFDATA:apply_dynamic_damping")
      CHECK
   end function

   function apply_damping(self) result(res)
    SCFDATA :: self
   ! Return TRUE if density matrix damping is to be applied this iteration
      BIN :: res
      STACK("SCFDATA:apply_damping")
      START_TIMER("SCFDATA:apply_damping")
      res = self%using_damping AND self%iteration < self%damp_finish
     STOP_TIMER("SCFDATA:apply_damping")
      CHECK
   end function

   function apply_level_shifting(self) result(res)
    SCFDATA :: self
   ! Return TRUE if level shifting must be applied this iteration
      BIN :: res
      STACK("SCFDATA:apply_level_shifting")
      START_TIMER("SCFDATA:apply_level_shifting")
      res = NOT is_zero_(self%level_shift) &
            AND self%using_level_shift AND self%iteration < self%level_shift_finish
     STOP_TIMER("SCFDATA:apply_level_shifting")
      CHECK
   end function

   function fitting(self) result(res)
    SCFDATA :: self
   ! Return true if we are fitting the wavefunction.
     BIN :: res
     STACK("SCFDATA:fitting")
     START_TIMER("SCFDATA:fitting")
     select case (self%scf_kind)
       case("xray_rhf","xray_restricted_hartree_fock");   res = TRUE
       case("xray_rks");                                  res = TRUE
       case("xray_rdft");                                 res = TRUE
       case("xray_udft");                                 res = TRUE
       case default;                                      res = FALSE
     end select
     STOP_TIMER("SCFDATA:fitting")
      CHECK
   end function

 !  ***************
 !  Output routines
 !  ***************

   subroutine put_banner(self)
    SCFDATA :: self
   ! Prints out the nuclear energy and initial guess energy.
     STACK("SCFDATA:put_banner")
     START_TIMER("SCFDATA:put_banner")
     if (NOT self%output) then; STOP_TIMER("SCFDATA:put_banner") CHECK return; end if
     call flush_(stdout)
     call text_(stdout,"***************")
     call text_(stdout,"SCF calculation")
     call text_(stdout,"***************")
     call flush_(stdout)
     call put_summary_(self)
     STOP_TIMER("SCFDATA:put_banner")
      CHECK
   end subroutine

   subroutine put_summary(self)
    SCFDATA :: self
   ! Prints out a summary of what is stored in the scfdata object.
     REALVEC(3) :: q
     BIN :: real_width
     STACK("SCFDATA:put_summary")
     START_TIMER("SCFDATA:put_summary")
     real_width = TRUE
     call show_(stdout,"SCF kind                    = ", self%scf_kind)
     if (includes_(self%scf_kind,"dft")) then
     call show_(stdout,"DFT Exchange                = ", self%dft_exchange)
     call show_(stdout,"DFT Correlation             = ", self%dft_correlation)
     end if
     call show_(stdout,"Direct                      = ", self%direct,real_width)
     call set_real_style_(stdout,"e")
     call show_(stdout,"Integral cutoff             = ", self%eri_limit)
     call set_real_style_(stdout,"f")
     if (self%nddo) &
     call show_(stdout,"NDDO                        = ", self%nddo,real_width)
     call show_(stdout,"ZORA (1 electron) terms     = ", self%using_1e_zora_term,real_width)
     call flush_(stdout)
     call text_(stdout,"Initial guess options:")
     call flush_(stdout)
     call show_(stdout,"Initial density             = ", self%initial_density)
     call show_(stdout,"Initial MO's                = ", self%initial_mos)
     call flush_(stdout)
     call text_(stdout,"Initial guess energies:")
     call flush_(stdout)
     call show_(stdout,"Nuclear Energy              = ", self%nuclear_energy)
     call show_(stdout,"SCF Energy                  = ", self%energy)
     call show_(stdout,"Kinetic Energy              = ", self%kinetic_energy)
     if (includes_(self%scf_kind,"dft")) then
     call show_(stdout,"DFT Energy                  = ", self%energy+self%dft_energy_correction)
     end if
     call flush_(stdout)
     call text_(stdout,"SCF termination criteria:")
     call flush_(stdout)
     call show_(stdout,"Convergence                 = ", self%convergence)
     call show_(stdout,"Gradient/DIIS convergence   = ", self%diis_convergence)
     call show_(stdout,"Minimum iterations          = ", self%min_iterations,real_width)
     call show_(stdout,"Maximum iterations          = ", self%max_iterations,real_width)
     call flush_(stdout)
     call text_(stdout,"Convergence acceleration options:")
     call flush_(stdout)
     call show_(stdout,"Using Rough Convergence     = ", self%using_rough_convergence,real_width)
     if (self%using_rough_convergence) then
     call show_(stdout,"Rough Convergence           = ", self%rough_convergence)
     call show_(stdout,"Rough DIIS Convergence      = ", self%rough_diis_convergence)
     end if
     call show_(stdout,"Using level shift           = ", self%using_level_shift,real_width)
     if (self%using_level_shift) then
     call show_(stdout,"Level shift                 = ", self%level_shift)
     call show_(stdout,"Level shift  quits at       = ", self%level_shift_finish,real_width)
     end if
     call show_(stdout,"Using density damping       = ", self%using_damping,real_width)
     if (self%using_damping) then
     call show_(stdout,"Damping factor              = ", self%damp_factor)
     call show_(stdout,"Damping quits at            = ", self%damp_finish,real_width)
     end if
     call show_(stdout,"Using MO gradient update    = ", self%using_MO_gradient_update,real_width)
     if (self%using_MO_gradient_update) then
     call show_(stdout,"MO gradient stepsize        = ", self%MO_gradient_stepsize)
     call show_(stdout,"Maximum update stepsize     = ", self%max_update_stepsize)
     end if
     if (self%using_dynamic_damping) then
     call show_(stdout,"Using Dynamic Damping       = ", self%using_dynamic_damping,real_width)
     end if
     if (self%using_camp_king) then
     call show_(stdout,"Using Camp-King             = ", self%using_camp_king)
     end if
     call show_(stdout,"Using DIIS                  = ", using_diis_(self),real_width)
     call show_(stdout,"Using Fock DIIS?            = ", self%using_fock_diis,real_width)
     call show_(stdout,"Using MO DIIS?              = ", self%using_MO_diis,real_width)
     if (using_diis_(self)) then
     call show_(stdout,"DIIS archive root name      = ", self%diis%archive%root_name)
     call show_(stdout,"DIIS no. to keep            = ", self%diis%keep,real_width)
     call show_(stdout,"DIIS automatic start?       = ", self%using_diis_auto_start,real_width)
     if (NOT self%using_diis_auto_start) then
     call show_(stdout,"DIIS start iteration        = ", self%diis_start_iteration,real_width)
     end if
     end if
     if (spinorbital_kind_(self)=="general_complex") then
       call flush_(stdout)
       call text_(stdout,"Magnetic/Relativistic terms:")
       call flush_(stdout)
       q = self%quantization_axis
       call show_(stdout,"Quantization axis           = ", q(1),q(2),q(3))
       call show_(stdout,"ZORA (1 electron) terms     = ", self%using_1e_zora_term)
       call show_(stdout,"Using B:L term              = ", self%using_bl_term)
       call show_(stdout,"Using B:S term              = ", self%using_bs_term)
       call show_(stdout,"Using B:S T term            = ", self%using_bs_t_term)
       call show_(stdout,"Using A:A term              = ", self%using_aa_term)
       call show_(stdout,"Using 1e S:L term           = ", self%using_1e_sl_term)
       call show_(stdout,"Using 1e S:(rxA) term       = ", self%using_1e_srxa_term)
       call show_(stdout,"Using 2e S:L term           = ", self%using_2e_sl_term)
       call show_(stdout,"Factor for 1e S:L term      = ", self%sl_1e_factor)
       call show_(stdout,"Factor for 2e S:L term      = ", self%sl_2e_factor)
     end if
     if (fitting_(self)) then
       call flush_(stdout)
       call text_(stdout,"Experimental wavefunction parameters:")
       call flush_(stdout)
       call show_(stdout,"Lambda fitting parameter    = ", self%lambda)
       call show_(stdout,"Lambda max                  = ", self%lambda_max)
       call show_(stdout,"Lambda step                 = ", self%lambda_step)
       call put_crystal_(self)
     end if
     STOP_TIMER("SCFDATA:put_summary")
      CHECK
   end subroutine

   subroutine put_table_head(self)
    SCFDATA :: self
   ! Prints out the table head for an SCF calculation
     INT :: fields
     STACK("SCFDATA:put_table_head")
     START_TIMER("SCFDATA:put_table_head")
     if (NOT self%output) then; STOP_TIMER("SCFDATA:put_table_head") CHECK return; end if
     fields = 3
     if (fitting_(self))     fields = fields + 2
     call flush_(stdout)
     call dash_(stdout,real_fields=fields,int_fields=1)
     call put_(stdout,"Iter",int_width=TRUE)
     if (fitting_(self)) then
       call put_(stdout,"lambda")
       call put_(stdout,"F_chi2")
     end if
     call put_(stdout,"Energy")
     #ifndef SPEC_CPU
     !SPEC: Next two fields not appropriate for SPEC; see comment in "put_results"
     if (fitting_(self)) then
       call put_(stdout,"d(E+l*chi2)")
     else
       call put_(stdout,"difference")
     end if
     call put_(stdout,"Gradient/DIIS")
     #endif
     call flush_(stdout)
     call dash_(stdout,real_fields=fields,int_fields=1)
     STOP_TIMER("SCFDATA:put_table_head")
      CHECK
   end subroutine

   subroutine put_results(self)
    SCFDATA :: self
   ! Print out the results for the current iteration. This routine must be
   ! compatible with put_banner. This routine must be called at iteration 0.
     INT :: fields,n,i,margin_pos
     BIN, save :: diis_converged,diff_converged,damping_on,damping_off
     BIN, save :: level_on,level_off,diis_on,diis_up,rough_off
     STRVEC(STR_SIZE,20) :: info
     STACK("SCFDATA:put_results")
     START_TIMER("SCFDATA:put_results")
     if (NOT self%output) then; STOP_TIMER("SCFDATA:put_results") CHECK return; end if
     ! This is the table head ...
     if (self%iteration == 0) then
        if (self%lambda_iteration == 0) call put_table_head_(self)
        diff_converged = FALSE
        rough_off = FALSE
        diis_converged = FALSE
        damping_on = FALSE
        damping_off = FALSE
        level_on = FALSE
        level_off = FALSE
        diis_on = FALSE
        diis_up = FALSE
 !       if (.lambda_iteration > 0) return
     end if
     call put_(stdout,self%iteration)
     fields = 3
     if (fitting_(self)) then
        fields = fields + 2
        call put_(stdout,self%lambda)
        call put_(stdout,self%F_chi2)
     end if
     ! This is the important info ...
     call put_(stdout,self%energy)
     #ifndef SPEC_CPU
     !SPEC: but, Daniel Grimwood has commented that SPEC shouldn't be doing a comparison on
     !SPEC: the next two fields, and that the easiest solution is to not print them.
     if (fitting_(self)) then
       call put_(stdout,self%fit_value-self%old_fit_value)
     else
       call put_(stdout,self%difference)
     end if
     call put_(stdout,self%diis_error)
     #endif
     ! Margin notes ...
     if (NOT scf_done_(self)) then
        info = " "
        n = 0
        if (NOT diff_converged AND diff_converged_(self) AND self%iteration>0) then
          n = n + 1
          info(n) = " *Difference has converged"
          diff_converged = TRUE
        end if
        if (NOT diis_converged AND diis_converged_(self)) then
          n = n + 1
          info(n) = " *Gradient has converged"
          diis_converged = TRUE
        end if
        if (NOT rough_off AND NOT apply_rough_convergence_(self) AND self%using_rough_convergence) then
           n = n + 1
           info(n) = " *Increasing integral accuracy"
           rough_off = TRUE
        end if
        if (apply_damping_(self) AND NOT damping_on) then
           n = n + 1
           info(n) = " *Damping on"
           damping_on = TRUE
        else if (NOT apply_damping_(self) AND NOT damping_off) then
           n = n + 1
           info(n) = " *Damping off"
           damping_off = TRUE
        end if
        if (apply_level_shifting_(self) AND NOT level_on) then
           n = n + 1
           info(n) = " *Levelshift on"
           level_on = TRUE
        else if (NOT apply_level_shifting_(self) AND NOT level_off) then
           n = n + 1
           info(n) = " *Levelshift off"
           level_off = TRUE
        end if
        if ((using_diis_(self) AND NOT diis_on AND apply_diis_(self))) then
           n = n + 1
           info(n) = " *DIIS on"
           diis_on = TRUE
        end if
        if (NOT diis_up AND apply_diis_(self) AND self%diis%n_vec==self%diis%keep) then
           n = n + 1
           info(n) = " *DIIS subspace saturated"
           diis_up = TRUE
        end if
        if (self%camp_king_iters>0) then
           n = n + 1
           info(n) = " *Camp-King iterations = " // trim(to_str_(self%camp_king_iters))
        end if
        if (self%using_dynamic_damping AND self%dynamic_damp_factor > TOL(7)) then
           n = n + 1
           info(n) = " *damp factor = " // &
                     trim(to_str_no_zeros_(self%dynamic_damp_factor,"f8.6"))
        end if
        margin_pos = stdout%buffer%item_end
        do i = 1,n
          if (i>1) then
          call flush_(stdout)
          call tab_(stdout,width=margin_pos)
          end if
          call put_text_(stdout,trim(info(i)))
        end do
        call flush_(stdout)
     ! This is the table foot ...
     else
        call put_table_foot_(self)
     end if
     STOP_TIMER("SCFDATA:put_results")
      CHECK
   end subroutine

   subroutine put_table_foot(self)
    SCFDATA :: self
   ! Prints out the table foot for an SCF calculation, after convergence
   ! or not as the case may be
     INT :: fields
     STACK("SCFDATA:put_table_foot")
     START_TIMER("SCFDATA:put_table_foot")
     if (NOT self%output) then; STOP_TIMER("SCFDATA:put_table_foot") CHECK return; end if
     fields = 3
     if (fitting_(self))     fields = fields + 2
     call flush_(stdout)
     call dash_(stdout,real_fields=fields,int_fields=1)
     if (converged_(self)) then; call text_(stdout,"* * * SCF has converged * * *",flush=1)
     else;                 call text_(stdout,"* * * SCF has not converged * * *",flush=1)
     end if
     call dash_(stdout,real_fields=fields,int_fields=1)
     call show_(stdout,"SCF Energy                  = ", self%energy)
     call show_(stdout,"Kinetic Energy              = ", self%kinetic_energy)
     if (includes_(self%scf_kind,"dft")) then
     call show_(stdout,"DFT Energy                  = ", self%energy+self%dft_energy_correction)
     end if
     if (fitting_(self)) call put_crystal_(self)
     call dash_(stdout,real_fields=fields,int_fields=1)
     STOP_TIMER("SCFDATA:put_table_foot")
      CHECK
   end subroutine

   subroutine put_crystal(self)
    SCFDATA :: self
   ! Prints out the crystal structure factor statistics.
     STACK("SCFDATA:put_crystal")
     START_TIMER("SCFDATA:put_crystal")
     call show_(stdout,"Chi^2 in F                  = ", self%F_chi2)
     call show_(stdout,"Goodness of fit in F        = ", self%F_gof)
     call show_(stdout,"R factor in F               = ", self%F_r_factor)
     call show_(stdout,"Weighted R factor in F      = ", self%F_weighted_r_factor)
     STOP_TIMER("SCFDATA:put_crystal")
      CHECK
   end subroutine

   subroutine put_constrained_scf_results(self,out)
    SCFDATA :: self
   ! Outputs SCF information to a file, which is useful for constrained
   ! Hartree-Fock methods where you want to view the effects of the constraint
   ! Lagrange multiplier.
     TEXTFILE :: out
     STACK("SCFDATA:put_constrained_scf_results")
     START_TIMER("SCFDATA:put_constrained_scf_results")
     call put_(out,self%lambda)
     call put_(out,self%F_chi2)
     call put_(out,self%energy)
     call put_(out,self%kinetic_energy)
     call put_(out,self%F_weighted_r_factor)
     STOP_TIMER("SCFDATA:put_constrained_scf_results")
      CHECK
   end subroutine

end
