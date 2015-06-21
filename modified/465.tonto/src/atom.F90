!-------------------------------------------------------------------------------
!
! ATOM: can be a quantum mechanical atom, with a basis set
!       or a molecular mechanical atom with a force field potential
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
! $Id: atom.foo,v 1.33.2.17 2003/11/13 05:34:14 reaper Exp $
!-------------------------------------------------------------------------------

module ATOM_MODULE

#  include "atom.use"

   implicit none

#  include "macros"
#  include "atom.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

!  ***************
!  Data statements
!  ***************

   !  The Periodic Table

   ARRAY(STR(2),103) :: element_symbols

   data element_symbols/ &
    "H ",                                                                                "He", &
    "Li","Be",                                                  "B ","C ","N ","O ","F ","Ne", &
    "Na","Mg",                                                  "Al","Si","P ","S ","Cl","Ar", &
    "K ","Ca","Sc","Ti","V ","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As","Se","Br","Kr", &
    "Rb","Sr","Y ","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","Sn","Sb","Te","I ","Xe", &
    "Cs","Ba","La",     "Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu", &
                   "Hf","Ta","W ","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po","At","Rn", &
    "Fr","Ra","Ac",     "Th","Pa","U ","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md","No","Lr"/

   ARRAY(STR(12),103) :: element_names

   data element_names/ &
    "Hydrogen    ","Helium      ", &
    "Lithium     ","Beryllium   ", &
    "Boron       ","Carbon      ","Nitrogen    ","Oxygen      ","Fluorine    ","Neon        ", &
    "Sodium      ","Magnesium   ", &
    "Aluminium   ","Silicon     ","Phosphorous ","Sulfur      ","Chlorine    ","Argon       ", &
    "Potassium   ","Calcium     ", &
    "Scandium    ","Titanium    ","Vanadium    ","Chromium    ","Manganese   ", &
    "Iron        ","Cobalt      ","Nickel      ","Copper      ","Zinc        ", &
    "Gallium     ","Germanium   ","Arsenic     ","Selinium    ","Bromine     ","Krypton     ", &
    "Rubidium    ","Strontium   ", &
    "Yttrium     ","Zirconium   ","Niobium     ","Molybdenum  ","Technitium  ", &
    "Ruthenium   ","Rhodium     ","Palladium   ","Silver      ","Cadmium     ", &
    "Indium      ","Tin         ","Antimony    ","Tellurium   ","Iodine      ","Xenon       ", &
    "Cesium      ","Barium      ", &
    "Lanthanum   ", &
    "Cerium      ","Praseodymium","Neodymium   ","Promethium  ","Samarium    ","Europium    ","Gadolinium  ", &
    "Terbium     ","Dysprosium  ","Holmium     ","Erbium      ","Thulium     ","Ytterbium   ","Lutetium    ", &
                   "Haffnium    ","Tantalum    ","Tangsten    ","Rhenium     ", &
    "Osmium      ","Iridium     ","Platinum    ","Gold        ","Mercury     ", &
    "Thallium    ","Lead        ","Bismuth     ","Polonium    ","Astatine    ","Radon       ", &
    "Francium    ","Radium      ", &
    "Actinium    ", &
    "Thorium     ","Protactinium","Uranium     ","Neptunium   ","Plutonium   ","Americium   ","Curium      ", &
    "Berkellium  ","Californium ","Einsteinium ","Fermium     ","Mendelevium ","Nobelium    ","Lawrencium  "/

   !  Bragg-Slater radii taken from Aaron Lee's code

   REALVEC(54) :: bragg_slater_radii

   data bragg_slater_radii/ &
      0.35d0,0.35d0,                                           &
      1.45d0,1.05d0,0.85d0,0.70d0,0.65d0,0.60d0,0.50d0,0.45d0, &
      1.80d0,1.50d0,1.25d0,1.10d0,1.00d0,1.00d0,1.00d0,1.00d0, &
      2.20d0,1.80d0,                                           &
      1.60d0,1.40d0,1.35d0,1.40d0,1.40d0,                      &
      1.40d0,1.35d0,1.35d0,1.35d0,1.35d0,                      &
                    1.30d0,1.25d0,1.15d0,1.15d0,1.15d0,1.15d0, &
      1.30d0,1.30d0,                                           &
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      &
      1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,                      &
                    1.30d0,1.30d0,1.30d0,1.30d0,1.30d0,1.30d0  /

   !  Abundance weighted atomic masses taken from the WWW to be the same as Turbomol

   REALVEC(92) :: atomic_masses

   data atomic_masses/ &
       1.007970d0,   4.002600d0, &
       6.939000d0,   9.012200d0,  10.811000d0,  12.011150d0,  14.006700d0,  15.999400d0,  18.998400d0,  20.183000d0, &
      22.989800d0,  24.312000d0,  26.981500d0,  28.086000d0,  30.973800d0,  32.064000d0,  35.453000d0,  39.948000d0, &
      39.102000d0,  40.080000d0, &
                    44.956000d0,  47.900000d0,  50.942000d0,  51.996000d0,  54.938000d0, &
                    55.850000d0,  58.933200d0,  58.710000d0,  63.540000d0,  65.370000d0, &
                                  69.720000d0,  72.590000d0,  74.921600d0,  78.960000d0,  79.909000d0,  83.800000d0, &
      85.470000d0,  87.620000d0,  &
                    88.905000d0,  91.220000d0,  92.906000d0,  95.940000d0,  99.000000d0, &
                   101.070000d0, 102.905000d0, 106.400000d0, 107.870000d0, 112.400000d0, &
                                 114.820000d0, 118.690000d0, 121.750000d0, 127.600000d0, 126.904000d0, 131.300000d0, &
     132.905000d0, 137.330000d0, &
                   138.910000d0, 140.115000d0, 140.908000d0, 144.240000d0, 146.920000d0, 150.360000d0, 151.965000d0, &
                   157.250000d0, 158.925000d0, 162.500000d0, 164.930000d0, 167.260000d0, 168.930000d0, 173.040000d0, &
                   174.970000d0, 178.490000d0, 180.950000d0, 183.850000d0, 186.210000d0, &
                   190.200000d0, 192.220000d0, 195.080000d0, 196.070000d0, 200.590000d0, &
                                 204.380000d0, 207.200000d0, 208.980000d0, 208.980000d0, 209.990000d0, 222.020000d0, &
     223.020000d0, 226.030000d0, &
                   227.030000d0, 232.040000d0, 231.040000d0, 238.030000d0/

   !  Abundance-weighted coherent neutron scattering lengths in fm taken from:
   !  International Tables for Crystallography, Vol. C, 1992, pp 384-391

   REALVEC(95) :: neutron_scattering_lengths

   data neutron_scattering_lengths/ &
      -3.7390,  3.2600, &
      -1.9000,  7.7900,  5.3000,  6.6460,  9.3600,  5.8030,  5.6540,  4.5470, &
       3.6300,  5.3750,  3.4490,  4.1490,  5.1300,  2.8470,  9.5770,  1.9090, &
       3.7100,  4.9000, &
               12.2900, -3.4380,  -.3824,  3.6350, -3.7300, &
                9.5400,  2.5000, 10.3000,  7.7180,  5.6890, &
                         7.2879,  8.1929,  6.5800,  7.9700,  6.7950,  7.8000, &
       7.0800,  7.0200, &
                7.7500,  7.1600,  7.0540,  6.9500,  6.8000, &
                7.2100,  5.8800,  5.9100,  5.9220,  5.1000, &
                         4.0650,  6.2257,  5.5700,  5.8000,  5.2800,  4.8500, &
       5.4200,  5.0600, &
                8.2400,  4.8400,  4.4500,  7.6900, 12.6000,  4.2000,  6.7300, &
                9.5000,  7.3800, 16.9000,  8.0800,  8.0300,  7.0700, 12.4100, &
                7.2100,  7.7700,  6.9100,  4.7700,  9.2000, &
               11.0000, 10.6000,  9.6000,  7.6300, 12.6920, &
                         8.7760,  9.4017,  8.5307,  0.0000,  0.0000,  0.0000, &
       0.0000, 10.0000, &
                0.0000, 10.6300,  9.1000,  8.4170, 10.5500, 14.1000,  8.3000/

   ! X-ray dispersion correction factors.
   ! First element is f', second is f".  Cr wavelength = 2.291A.

   CPXVEC(92) :: dispersion_correction_Cr

   data dispersion_correction_Cr/ &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), &
    (  0.0,  0.1), (  0.0,  0.1), (  0.1,  0.1), (  0.1,  0.2), (  0.1,  0.3), &
    (  0.2,  0.4), (  0.2,  0.5), (  0.2,  0.6), (  0.3,  0.8), (  0.3,  0.9), &
    (  0.3,  1.2), (  0.3,  1.4), (  0.2,  1.7), (  0.0,  2.1), ( -0.2,  2.6), &
    ( -0.7,  3.1), ( -1.7,  3.7), ( -4.4,  0.5), ( -2.2,  0.6), ( -1.8,  0.7), &
    ( -1.6,  0.8), ( -1.4,  0.9), ( -1.2,  1.1), ( -1.1,  1.2), ( -1.0,  1.4), &
    ( -0.9,  1.6), ( -0.8,  1.8), ( -0.7,  2.1), ( -0.7,  2.3), ( -0.7,  2.5), &
    ( -0.7,  2.8), ( -0.7,  3.2), ( -0.7,  3.6), ( -0.7,  3.9), ( -0.8,  4.3), &
    ( -0.8,  4.8), ( -0.9,  5.3), ( -1.1,  5.9), ( -1.2,  6.4), ( -1.4,  6.9), &
    ( -1.7,  7.5), ( -2.0,  8.2), ( -2.3,  8.8), ( -2.8,  9.5), ( -3.3, 10.3), &
    ( -4.0, 11.1), ( -5.0, 11.9), ( -7.1, 13.1), ( -9.0, 10.0), (-12.0, 11.0), &
    (-11.0,  8.0), (-14.0,  3.0), (-10.0,  3.0), ( -9.0,  3.0), ( -8.0,  4.0), &
    ( -7.0,  4.0), ( -7.0,  4.0), ( -6.0,  5.0), ( -6.0,  5.0), ( -6.0,  6.0), &
    ( -6.0,  6.0), ( -5.0,  7.0), ( -5.0,  7.0), ( -5.0,  8.0), ( -5.0,  8.0), &
    ( -5.0,  9.0), ( -5.0,  9.0), ( -5.0, 10.0), ( -5.0, 10.0), ( -5.0, 11.0), &
    ( -5.0, 12.0), ( -5.0, 13.0), ( -5.0, 14.0), ( -5.0, 14.0), ( -5.0, 15.0), &
    ( -5.0, 16.0), ( -6.0, 17.0), ( -6.0, 18.0), ( -7.0, 19.0), ( -8.0, 20.0), &
    ( -9.0, 22.0), (-10.0, 23.0), (-11.0, 24.0), (-12.0, 26.0), (-13.0, 27.0), &
    (-15.0, 28.0), (-17.0, 27.0)/

   ! X-ray dispersion correction factors.
   ! First element is f', second is f".  Cu wavelength = 1.542A.

   CPXVEC(92) :: dispersion_correction_Cu

   data dispersion_correction_Cu/ &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.1), (  0.0,  0.1), (  0.1,  0.1), &
    (  0.1,  0.2), (  0.1,  0.2), (  0.1,  0.3), (  0.2,  0.4), (  0.2,  0.5), &
    (  0.3,  0.6), (  0.3,  0.7), (  0.3,  0.8), (  0.3,  1.0), (  0.3,  1.3), &
    (  0.3,  1.5), (  0.2,  1.8), (  0.1,  2.2), ( -0.1,  2.5), ( -0.5,  2.9), &
    ( -1.1,  3.3), ( -2.2,  3.8), ( -3.1,  0.5), ( -2.1,  0.6), ( -1.7,  0.7), &
    ( -1.5,  0.8), ( -1.3,  0.9), ( -1.2,  1.0), ( -1.1,  1.1), ( -1.0,  1.3), &
    ( -1.0,  1.5), ( -0.9,  1.7), ( -0.8,  1.8), ( -0.8,  2.0), ( -0.7,  2.2), &
    ( -0.7,  2.5), ( -0.6,  2.7), ( -0.6,  3.0), ( -0.6,  3.3), ( -0.6,  3.6), &
    ( -0.6,  3.9), ( -0.6,  4.3), ( -0.7,  4.6), ( -0.8,  5.0), ( -0.9,  5.4), &
    ( -1.0,  5.8), ( -1.1,  6.2), ( -1.3,  6.7), ( -1.6,  7.2), ( -1.9,  7.7), &
    ( -2.3,  8.3), ( -2.7,  8.9), ( -3.1,  9.6), ( -3.6, 10.2), ( -4.4, 10.9), &
    ( -5.3, 11.5), ( -6.7, 12.4), ( -9.0, 10.2), (-12.0, 11.2), (-11.0,  7.0), &
    (-10.0,  8.0), (-13.0,  3.0), ( -9.0,  3.0), ( -8.0,  4.0), ( -8.0,  4.0), &
    ( -7.0,  4.0), ( -7.0,  4.0), ( -6.0,  5.0), ( -6.0,  5.0), ( -6.0,  5.0), &
    ( -6.0,  6.0), ( -6.0,  6.0), ( -5.0,  7.0), ( -5.0,  7.0), ( -5.0,  8.0), &
    ( -5.0,  8.0), ( -5.0,  9.0), ( -5.0,  9.0), ( -5.0, 10.0), ( -5.0, 10.0), &
    ( -5.0, 11.0), ( -5.0, 11.0), ( -5.0, 12.0), ( -5.0, 12.0), ( -5.0, 13.0), &
    ( -5.0, 14.0), ( -5.0, 15.0)/

   ! X-ray dispersion correction factors.
   ! First element is f', second is f".  Mo wavelength = 0.7107A.

   CPXVEC(92) :: dispersion_correction_Mo

   data dispersion_correction_Mo/ &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0),  &
    (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0), (  0.0,  0.0),  &
    (  0.0,  0.1), (  0.0,  0.1), (  0.1,  0.1), (  0.1,  0.1), (  0.1,  0.1),  &
    (  0.1,  0.2), (  0.1,  0.2), (  0.1,  0.2), (  0.2,  0.3), (  0.2,  0.3),  &
    (  0.2,  0.4), (  0.2,  0.5), (  0.3,  0.6), (  0.3,  0.7), (  0.3,  0.8),  &
    (  0.3,  0.9), (  0.3,  1.0), (  0.3,  1.1), (  0.3,  1.3), (  0.3,  1.5),  &
    (  0.2,  1.6), (  0.2,  1.8), (  0.1,  2.0), ( -0.1,  2.2), ( -0.3,  2.4),  &
    ( -0.6,  2.7), ( -0.9,  3.0), ( -1.4,  3.4), ( -2.3,  3.7), ( -2.8,  0.6),  &
    ( -2.1,  0.6), ( -1.7,  0.7), ( -1.5,  0.8), ( -1.3,  0.8), ( -1.2,  0.9),  &
    ( -1.1,  1.0), ( -1.0,  1.1), ( -0.9,  1.3), ( -0.8,  1.4), ( -0.8,  1.5),  &
    ( -0.8,  1.6), ( -0.7,  1.8), ( -0.7,  1.9), ( -0.6,  2.1), ( -0.6,  2.3),  &
    ( -0.6,  2.5), ( -0.5,  2.7), ( -0.5,  2.9), ( -0.5,  3.1), ( -0.5,  3.3),  &
    ( -0.5,  3.5), ( -0.5,  3.7), ( -0.5,  3.9), ( -0.6,  4.1), ( -0.6,  4.3),  &
    ( -0.7,  4.7), ( -0.7,  5.0), ( -0.7,  5.3), ( -0.8,  5.6), ( -0.8,  5.9),  &
    ( -0.9,  6.1), ( -1.0,  6.4), ( -1.1,  6.7), ( -1.3,  7.1), ( -1.5,  7.5),  &
    ( -1.7,  7.9), ( -2.0,  8.3), ( -2.2,  8.7), ( -2.5,  9.2), ( -2.9,  9.7),  &
    ( -3.5, 10.2), ( -4.1, 10.7), ( -4.8, 11.1), ( -5.5, 11.7), ( -7.0,  9.0),  &
    ( -8.0, 10.0), ( -8.0,  7.0), ( -7.0,  7.0), ( -7.0,  8.0), ( -7.0,  7.0),  &
    ( -7.0,  8.0), ( -8.0,  8.0)/

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self)
    ATOM :: self
   ! Create an atom
      PTR :: self
      STACK("ATOM:create")
      START_TIMER("ATOM:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(ATOM_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("ATOM:create")
      UNSTACK
   end subroutine

   subroutine create_copy(self,atom)
    ATOM :: self
   ! Create a copy of atom.
     ATOM :: atom
     PTR :: self
     STACK("ATOM:create_copy")
     START_TIMER("ATOM:create_copy")
     call create_(self)
     call copy_(self,atom)
     STOP_TIMER("ATOM:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,atom)
    ATOM :: self
   ! Make self a copy of atom.  WARNING: the basis part is not explicitly
   ! copied, so be careful with destroy operations.
     ATOM :: atom
     STACK("ATOM:copy")
     START_TIMER("ATOM:copy")
     self = atom
     ! if (atom.basis.created) .basis.create_copy(atom.basis)
     ! if (atom.slaterbasis.created) .coppensbasis.create_copy(atom.coppensbasis)
     ! if (atom.coppensbasis.created) .coppensbasis.create_copy(atom.coppensbasis)
     if (associated(atom%density_matrix)) call create_copy_(self%density_matrix,atom%density_matrix)
     if (associated(atom%natural_orbitals)) call create_copy_(self%natural_orbitals,atom%natural_orbitals)
     if (associated(atom%occupation_numbers)) call create_copy_(self%occupation_numbers,atom%occupation_numbers)
     STOP_TIMER("ATOM:copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    ATOM :: self
   ! Destroy an atom
      PTR :: self
      STACK("ATOM:destroy")
      START_TIMER("ATOM:destroy")
      if (NOT associated(self)) then; STOP_TIMER("ATOM:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      deallocate(self)
      DELETE_MEMORY(ATOM_SIZE)
     STOP_TIMER("ATOM:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    ATOM :: self
   ! Nullify the pointer parts of the atom
      STACK("ATOM:nullify_ptr_part")
      START_TIMER("ATOM:nullify_ptr_part")
      nullify(self%basis)
      nullify(self%slaterbasis)
      nullify(self%coppensbasis)
      nullify(self%density_matrix)
      nullify(self%natural_orbitals)
      nullify(self%occupation_numbers)
     STOP_TIMER("ATOM:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    ATOM :: self
   ! Destroy the pointer parts of an atom
      STACK("ATOM:destroy_ptr_part")
      START_TIMER("ATOM:destroy_ptr_part")
      call destroy_(self%basis)
      call destroy_(self%slaterbasis)
      call destroy_(self%coppensbasis)
      call destroy_(self%density_matrix)
      call destroy_(self%natural_orbitals)
      call destroy_(self%occupation_numbers)
     STOP_TIMER("ATOM:destroy_ptr_part")
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
    ATOM :: self
   ! Set a default atom
      STACK("ATOM:set_defaults")
      START_TIMER("ATOM:set_defaults")
      self%label = "?"
      self%atomic_number = 0
      self%pos = (/ ZERO,ZERO,ZERO /)
      self%U_iso = ZERO
      self%thermal_tensor = ZERO
      self%basis_label = " "
      self%axis_system = "cartesian"
      self%thermal_axis_system = "cartesian"
      self%energy = ZERO
      self%group  = 0
      ! MM/protien defaults
      self%sequence_number = 0
      self%residue_atom_name = "?"
      self%residue_name = "UNK"
      self%mm_forcefield_name = "?"
      self%mm_atom_type = "?"
      self%mm_charge = ZERO
      self%restraining_position = (/ ZERO,ZERO,ZERO /)
      self%restraining_force_constant = ZERO
      self%site_occupancy = ONE
     STOP_TIMER("ATOM:set_defaults")
      CHECK
   end subroutine

   subroutine set_label_and_atomic_number(self,label)
    ATOM :: self
   ! Set an ATOM "label" and extract the atomic number from it.
      STR(*) :: label
      INT :: lensym,z
      STR(STR_SIZE) :: symbol
      BIN :: error
      STACK("ATOM:set_label_and_atomic_number")
      START_TIMER("ATOM:set_label_and_atomic_number")
      self%label = label
      if (is_int_(label)) then    ! The label must be the atomic number
         z = to_int_(label)
         self%atomic_number = z
      else                      ! First part of the label is the element symbol
         lensym = scan(label,"0123456789 ")-1
         error = lensym>2 OR lensym==0
         ENSURE(NOT error,"ATOM:set_label_and_atomic_number ... unacceptable atom symbol, "// trim(label))
         symbol = label(1:lensym)
         call to_upper_case_(symbol(1:1))
         call to_lower_case_(symbol(2:2))
         if (any(element_symbols==symbol(1:2))) then
            do z = 1,size(element_symbols)
               if (element_symbols(z)==symbol(1:2)) exit
            end do
           self%atomic_number = z
         else
            DIE("ATOM:set_label_and_atomic_number ... unknown element symbol: "//trim(symbol))
         end if
      end if
     STOP_TIMER("ATOM:set_label_and_atomic_number")
      CHECK
   end subroutine

   subroutine set_coppensbasis_label(self,label)
    ATOM :: self
   ! Set the coppensbasis "label".
      STR(*) :: label
      STACK("ATOM:set_coppensbasis_label")
      START_TIMER("ATOM:set_coppensbasis_label")
      if (NOT associated(self%coppensbasis)) call create_(self%coppensbasis)
      call set_label_(self%coppensbasis,label)
     STOP_TIMER("ATOM:set_coppensbasis_label")
      CHECK
   end subroutine

!  ************
!  I/O Routines
!  ************

   subroutine read_keywords(self)
    ATOM :: self
   ! Read data from "stdin" using keyword style input.
     STR(STR_SIZE) :: keyword
   STACK("ATOM:read_keywords")
   START_TIMER("ATOM:read_keywords")
   ENSURE(next_item_(stdin)=="{","ATOM:read_keywords ... expecting an open bracket symbol, {")
     call read_(stdin,keyword)         ! Read opening brace
     do                          ! Loop over keywords
       call read_(stdin,keyword)
       if (keyword=="}")   exit  ! Exit on closing brace
       if (reverted_(stdin)) exit  ! Exit if internal file reverted
       call process_keyword_(self,keyword)
     end do
     STOP_TIMER("ATOM:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    ATOM :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("ATOM:process_keyword")
     START_TIMER("ATOM:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}                            ")  ! exit case
       case ("axis_system=                 "); call read_axis_system_(self)
       case ("basis=                       "); call read_basis_(self)
       case ("basis_label=                 "); call read_basis_label_(self)
       case ("coppensbasis=                "); call read_coppensbasis_(self)
       case ("group=                       "); call read_group_(self)
       case ("junk=                        "); call read_junk_(self)
       case ("label=                       "); call read_label_(self)
       case ("mm_atom_type=                "); call read_mm_atom_type_(self)
       case ("mm_charge=                   "); call read_mm_charge_(self)
       case ("mm_forcefield_name=          "); call read_mm_forcefield_name_(self)
       case ("pdb_input_line=              "); call read_pdb_input_line_(self)
       case ("pos=                         "); call read_pos_(self)
       case ("position=                    "); call read_pos_(self)
       case ("put                          "); call put_(self)
       case ("residue_atom_name=           "); call read_residue_atom_name_(self)
       case ("residue_name=                "); call read_residue_name_(self)
       case ("restraining_position=        "); call read_restraining_position_(self)
       case ("restraining_force_constant=  "); call read_restraining_force_(self)
       case ("sequence_number=             "); call read_sequence_number_(self)
       case ("site_occupancy=              "); call read_site_occupancy_(self)
       case ("slaterbasis=                 "); call read_slaterbasis_(self)
       case ("thermal_axis_system=         "); call read_thermal_axis_system_(self)
       case ("thermal_tensor=              "); call read_thermal_tensor_(self)
       case ("u_iso=                       "); call read_U_iso_(self)
       case ("u_tensor=                    "); call read_thermal_tensor_(self)
       case ("units=                       "); call read_units_(self)
       ! These are only for making custom tables for the list type
       case ("---For custom tables---      ");
       case ("flush                        "); call flush_(stdout)
       case ("put_atomic_number            "); call put_(stdout,self%atomic_number)
       case ("put_bragg_slater_radius      "); call put_(stdout,bragg_slater_radius_(self))
       case ("put_chemical_symbol          "); call put_(stdout,chemical_symbol_(self),int_width=TRUE)
       case ("put_column_number            "); call put_(stdout,column_number_(self))
       case ("put_ground_state_multiplicity"); call put_(stdout,ground_state_multiplicity_(self))
       case ("put_label                    "); call put_(stdout,self%label,int_width=TRUE)
       case ("put_mass                     "); call put_(stdout,mass_(self))
       case ("put_mean_neutron_number      "); call put_(stdout,mean_neutron_number_(self))
       case ("put_neutron_scattering_length"); call put_(stdout,neutron_scattering_length_(self))
       case ("put_period_block             "); call put_(stdout,period_block_(self))
       case ("put_period_number            "); call put_(stdout,period_number_(self))
       case ("put_pos                      "); call put_(stdout,self%pos(1))
                                               call put_(stdout,self%pos(2))
                                               call put_(stdout,self%pos(3))
       case ("put_position                 "); call put_(stdout,self%pos(1))
                                               call put_(stdout,self%pos(2))
                                               call put_(stdout,self%pos(3))
       case ("put_thermal_tensor           "); call put_thermal_tensor_(self)
       case  default;                        allocate(tonto%known_keywords(43))
       tonto%known_keywords(1) = "}                            "
       tonto%known_keywords(2) = "axis_system=                 "
       tonto%known_keywords(3) = "basis=                       "
       tonto%known_keywords(4) = "basis_label=                 "
       tonto%known_keywords(5) = "coppensbasis=                "
       tonto%known_keywords(6) = "group=                       "
       tonto%known_keywords(7) = "junk=                        "
       tonto%known_keywords(8) = "label=                       "
       tonto%known_keywords(9) = "mm_atom_type=                "
       tonto%known_keywords(10) = "mm_charge=                   "
       tonto%known_keywords(11) = "mm_forcefield_name=          "
       tonto%known_keywords(12) = "pdb_input_line=              "
       tonto%known_keywords(13) = "pos=                         "
       tonto%known_keywords(14) = "position=                    "
       tonto%known_keywords(15) = "put                          "
       tonto%known_keywords(16) = "residue_atom_name=           "
       tonto%known_keywords(17) = "residue_name=                "
       tonto%known_keywords(18) = "restraining_position=        "
       tonto%known_keywords(19) = "restraining_force_constant=  "
       tonto%known_keywords(20) = "sequence_number=             "
       tonto%known_keywords(21) = "site_occupancy=              "
       tonto%known_keywords(22) = "slaterbasis=                 "
       tonto%known_keywords(23) = "thermal_axis_system=         "
       tonto%known_keywords(24) = "thermal_tensor=              "
       tonto%known_keywords(25) = "u_iso=                       "
       tonto%known_keywords(26) = "u_tensor=                    "
       tonto%known_keywords(27) = "units=                       "
       tonto%known_keywords(28) = "---For custom tables---      "
       tonto%known_keywords(29) = "flush                        "
       tonto%known_keywords(30) = "put_atomic_number            "
       tonto%known_keywords(31) = "put_bragg_slater_radius      "
       tonto%known_keywords(32) = "put_chemical_symbol          "
       tonto%known_keywords(33) = "put_column_number            "
       tonto%known_keywords(34) = "put_ground_state_multiplicity"
       tonto%known_keywords(35) = "put_label                    "
       tonto%known_keywords(36) = "put_mass                     "
       tonto%known_keywords(37) = "put_mean_neutron_number      "
       tonto%known_keywords(38) = "put_neutron_scattering_length"
       tonto%known_keywords(39) = "put_period_block             "
       tonto%known_keywords(40) = "put_period_number            "
       tonto%known_keywords(41) = "put_pos                      "
       tonto%known_keywords(42) = "put_position                 "
       tonto%known_keywords(43) = "put_thermal_tensor           "
       call unknown_(tonto,word,"ATOM:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    ATOM :: self
   ! Read a string which describes the units to be used
      STACK("ATOM:read_units")
      START_TIMER("ATOM:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("ATOM:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    ATOM :: self
   ! Read in a junk string, useful for ignoring a field
      STACK("ATOM:read_junk")
      START_TIMER("ATOM:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("ATOM:read_junk")
      CHECK
   end subroutine

   subroutine read_group(self)
    ATOM :: self
   ! Read the index which describes the group the atom belongs to.
      STACK("ATOM:read_group")
      START_TIMER("ATOM:read_group")
      call read_(stdin,self%group)
     STOP_TIMER("ATOM:read_group")
      CHECK
   end subroutine

   subroutine read_pos(self)
    ATOM :: self
   ! Read in the position
      STACK("ATOM:read_pos")
      START_TIMER("ATOM:read_pos")
      call read_(stdin,self%pos)
     STOP_TIMER("ATOM:read_pos")
      CHECK
   end subroutine

   subroutine read_label(self)
    ATOM :: self
   ! Read an ATOM label.
      STACK("ATOM:read_label")
      START_TIMER("ATOM:read_label")
      call set_label_and_atomic_number_(self,next_str_(stdin))  ! Input atom label
     STOP_TIMER("ATOM:read_label")
      CHECK
   end subroutine

   subroutine read_residue_atom_name(self)
    ATOM :: self
   ! Read the atom name that this atom belongs to a residue (case sensitive).
      STACK("ATOM:read_residue_atom_name")
      START_TIMER("ATOM:read_residue_atom_name")
      call read_(stdin,self%residue_atom_name)
     STOP_TIMER("ATOM:read_residue_atom_name")
      CHECK
   end subroutine

   subroutine read_residue_name(self)
    ATOM :: self
   ! Read the residue name that this atom belongs to.
      STACK("ATOM:read_residue_name")
      START_TIMER("ATOM:read_residue_name")
      call read_(stdin,self%residue_name)
     STOP_TIMER("ATOM:read_residue_name")
      CHECK
   end subroutine

   subroutine read_pdb_input_line(self)
    ATOM :: self
   ! Read a line in the PDB input style
      STR(STR_SIZE) :: word
      STACK("ATOM:read_pdb_input_line")
      START_TIMER("ATOM:read_pdb_input_line")
      call read_(stdin,word)
      call to_lower_case_(word)
      ENSURE(word=="atom","ATOM:read_pdb_input_line ... PDB line does not begin with 'atom'")
      call read_residue_atom_name_(self)
      call read_residue_name_(self)
      call read_sequence_number_(self)
      call read_pos_(self)
      call read_junk_(self)
      call read_junk_(self)
      call read_label_(self)
     STOP_TIMER("ATOM:read_pdb_input_line")
      CHECK
   end subroutine

   subroutine read_sequence_number(self)
    ATOM :: self
   ! Read the sequence number of the residue in the molecule
      STACK("ATOM:read_sequence_number")
      START_TIMER("ATOM:read_sequence_number")
      call read_(stdin,self%sequence_number)
     STOP_TIMER("ATOM:read_sequence_number")
      CHECK
   end subroutine

   subroutine read_mm_atom_type(self)
    ATOM :: self
   ! Read the atom type potential, used to define the force field potential
      STACK("ATOM:read_mm_atom_type")
      START_TIMER("ATOM:read_mm_atom_type")
      call read_(stdin,self%mm_atom_type)
     STOP_TIMER("ATOM:read_mm_atom_type")
      CHECK
   end subroutine

   subroutine read_mm_charge(self)
    ATOM :: self
   ! Read the MM charge on this atom
      STACK("ATOM:read_mm_charge")
      START_TIMER("ATOM:read_mm_charge")
      call read_(stdin,self%mm_charge)
     STOP_TIMER("ATOM:read_mm_charge")
      CHECK
   end subroutine

   subroutine read_mm_forcefield_name(self)
    ATOM :: self
   ! Read the MM forcefield name for this atom
      STACK("ATOM:read_mm_forcefield_name")
      START_TIMER("ATOM:read_mm_forcefield_name")
      call read_(stdin,self%mm_forcefield_name)
     STOP_TIMER("ATOM:read_mm_forcefield_name")
      CHECK
   end subroutine

   subroutine read_restraining_position(self)
    ATOM :: self
   ! Read the restraining position
      STACK("ATOM:read_restraining_position")
      START_TIMER("ATOM:read_restraining_position")
      call read_(stdin,self%restraining_position)
     STOP_TIMER("ATOM:read_restraining_position")
      CHECK
   end subroutine

   subroutine read_restraining_force(self)
    ATOM :: self
   ! Read the restrain force constant for this atom
      STACK("ATOM:read_restraining_force")
      START_TIMER("ATOM:read_restraining_force")
      call read_(stdin,self%restraining_force_constant)
     STOP_TIMER("ATOM:read_restraining_force")
      CHECK
   end subroutine

   subroutine read_site_occupancy(self)
    ATOM :: self
   ! Read the site occupancy, used mainly for crystallographic applications.
      STACK("ATOM:read_site_occupancy")
      START_TIMER("ATOM:read_site_occupancy")
      call read_(stdin,self%site_occupancy)
     STOP_TIMER("ATOM:read_site_occupancy")
      CHECK
   end subroutine

   subroutine read_axis_system(self)
    ATOM :: self
   ! Read a string which describes the axis system. Currently allowed is
   ! "cartesian" or "crystal". NOTE: .thermal_axis_system is changed too.
      STACK("ATOM:read_axis_system")
      START_TIMER("ATOM:read_axis_system")
      call read_(stdin,self%axis_system)
      self%thermal_axis_system = self%axis_system
      select case (self%axis_system)
        case ("cartesian")
        case ("crystal  ")
        case default
          allocate(tonto%known_keywords(2))
          tonto%known_keywords(1) = "cartesian"
          tonto%known_keywords(2) = "crystal  "
          call unknown_(tonto,self%axis_system,"ATOM:read_axis_system")
          deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ATOM:read_axis_system")
      CHECK
   end subroutine

   subroutine set_axis_system(self,word)
    ATOM :: self
   ! Read a string which describes the axis system. Currently allowed is
   ! "cartesian" or "crystal". NOTE: .thermal_axis_system is changed too.
     STR(*), IN :: word
     STACK("ATOM:set_axis_system")
     START_TIMER("ATOM:set_axis_system")
     self%axis_system = word
     self%thermal_axis_system = self%axis_system
     select case (self%axis_system)
       case ("cartesian")
       case ("crystal  ")
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,self%axis_system,"ATOM:set_axis_system")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:set_axis_system")
      CHECK
   end subroutine

   subroutine read_thermal_axis_system(self)
    ATOM :: self
   ! Read a string which describes the thermal tensor axis system.
   ! Currently allowed, is "cartesian" or "crystal".
      STACK("ATOM:read_thermal_axis_system")
      START_TIMER("ATOM:read_thermal_axis_system")
      call read_(stdin,self%thermal_axis_system)
      select case (self%thermal_axis_system)
        case ("cartesian")
        case ("crystal  ")
        case default
          allocate(tonto%known_keywords(2))
          tonto%known_keywords(1) = "cartesian"
          tonto%known_keywords(2) = "crystal  "
          call unknown_(tonto,self%thermal_axis_system,"ATOM:read_thermal_axis_system")
          deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ATOM:read_thermal_axis_system")
      CHECK
   end subroutine

   subroutine set_thermal_axis_system(self,word)
    ATOM :: self
   ! Read a string which describes the thermal tensor axis system.
   ! Currently allowed, is "cartesian" or "crystal".
     STR(*), IN :: word
     STACK("ATOM:set_thermal_axis_system")
     START_TIMER("ATOM:set_thermal_axis_system")
     self%thermal_axis_system = word
     select case (self%thermal_axis_system)
       case ("cartesian")
       case ("crystal  ")
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,self%thermal_axis_system,"ATOM:set_thermal_axis_system")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:set_thermal_axis_system")
      CHECK
   end subroutine

   subroutine read_basis(self)
    ATOM :: self
   ! Read the basis set from "stdin"
      STACK("ATOM:read_basis")
      START_TIMER("ATOM:read_basis")
      call create_(self%basis)
      call read_keywords_(self%basis)
     STOP_TIMER("ATOM:read_basis")
      UNSTACK
   end subroutine

   subroutine read_basis_label(self)
    ATOM :: self
   ! Read the basis set label from "stdin" which will be used to match a basis
   ! set to ...
      STACK("ATOM:read_basis_label")
      START_TIMER("ATOM:read_basis_label")
      call read_(stdin,self%basis_label)
     STOP_TIMER("ATOM:read_basis_label")
      UNSTACK
   end subroutine

   subroutine read_slaterbasis(self)
    ATOM :: self
   ! Read the slaterbasis set from "stdin"
      STACK("ATOM:read_slaterbasis")
      START_TIMER("ATOM:read_slaterbasis")
      call create_(self%slaterbasis)
      call read_keywords_(self%slaterbasis)
     STOP_TIMER("ATOM:read_slaterbasis")
      UNSTACK
   end subroutine

   subroutine read_coppensbasis(self)
    ATOM :: self
   ! Read the coppensbasis set from "stdin"
      STACK("ATOM:read_coppensbasis")
      START_TIMER("ATOM:read_coppensbasis")
      call create_(self%coppensbasis)
      call read_keywords_(self%coppensbasis)
     STOP_TIMER("ATOM:read_coppensbasis")
      UNSTACK
   end subroutine

   subroutine read_U_iso(self)
    ATOM :: self
   ! Read the isotropic thermal parameters from "stdin".  NOTE: units are
   ! Bohr^2, not Angstrom^2.
      STACK("ATOM:read_U_iso")
      START_TIMER("ATOM:read_U_iso")
      call read_(stdin,self%U_iso)
     STOP_TIMER("ATOM:read_U_iso")
      CHECK
   end subroutine

   subroutine read_thermal_tensor(self)
    ATOM :: self
   ! Read thermal parameters from "stdin". These are assumed to be in the
   ! cartesian axis system, and in bohr^2.  The expansion of the thermal
   ! smearing temperature factor term is:
   !              TF = exp ( -2\pi^2 U_{ij} h_i h_j a^*_i a^*_j )
   ! where (h) are the miller indices and (a^*) are the reciprocal lattice
   ! constants in angstrom^{-2}.
      REALVEC(6) :: tensor
      STACK("ATOM:read_thermal_tensor")
      START_TIMER("ATOM:read_thermal_tensor")
      call read_(stdin,tensor)
      self%thermal_tensor(1,1) = tensor(1) ! Units must be bohr^2
      self%thermal_tensor(2,2) = tensor(2) ! unless over-ridden by read_units=
      self%thermal_tensor(3,3) = tensor(3)
      self%thermal_tensor(1,2) = tensor(4)
      self%thermal_tensor(1,3) = tensor(5)
      self%thermal_tensor(2,3) = tensor(6)
      self%thermal_tensor(2,1) = self%thermal_tensor(1,2)
      self%thermal_tensor(3,1) = self%thermal_tensor(1,3)
      self%thermal_tensor(3,2) = self%thermal_tensor(2,3)
     STOP_TIMER("ATOM:read_thermal_tensor")
      CHECK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    ATOM :: self
   ! Read the "keys".
     STACK("ATOM:read_keys")
     START_TIMER("ATOM:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("ATOM:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    ATOM :: self
   ! Process each of the words in the "keys" list.
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
   STACK("ATOM:process_keys")
   START_TIMER("ATOM:process_keys")
   ENSURE(associated(keys),"ATOM:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"ATOM:process_keys ... no matching closing brace, }")
            internal => keys(k:k+l-1)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call revert_(stdin)
            k = k+l-1
         else if (includes_(keyword," ")) then
            internal => split_(keyword)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call destroy_(internal)
            call revert_(stdin)
         else
            call process_keyword_(self,keyword)
         end if
         if (k==n_key) exit
      end do
     STOP_TIMER("ATOM:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    ATOM :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
      STACK("ATOM:keys_created")
      START_TIMER("ATOM:keys_created")
      res = associated(keys)
     STOP_TIMER("ATOM:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    ATOM :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
     STACK("ATOM:set_keys")
     START_TIMER("ATOM:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("ATOM:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    ATOM :: self
   ! This is for destroying the "keys" externally.
     STACK("ATOM:clear_keys")
     START_TIMER("ATOM:clear_keys")
     if (NOT associated(keys)) then; STOP_TIMER("ATOM:clear_keys") CHECK return; end if
     call destroy_(keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     STOP_TIMER("ATOM:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    ATOM :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STACK("ATOM:put_table_footer")
     START_TIMER("ATOM:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("ATOM:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    ATOM :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
   STACK("ATOM:put_table_header")
   START_TIMER("ATOM:put_table_header")
   ENSURE(associated(keys),"ATOM:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_label   "); call put_(stdout,"label",int_width=TRUE)
           case ("put_n_shells"); call put_(stdout,"n_shells",int_width=TRUE)
           case ("put_n_bf    "); call put_(stdout,"n_bf",int_width=TRUE)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=TRUE)
           case ("flush       "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("ATOM:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("ATOM:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    ATOM :: self
   ! Return the table width in characters, based on "keys".  Note that not all
   ! keywords need to contribute to the banner - if a keyword is not recognised,
   ! then it is skipped.
     INT :: res
     STR(STR_SIZE) :: word
     INT :: int_dash,real_dash,k
     STACK("ATOM:table_width")
     START_TIMER("ATOM:table_width")
     ENSURE(associated(keys),"ATOM:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           "); ! exit surrounding loop
         case ("put_label   "); int_dash = int_dash + 1
         case ("put_n_shells"); int_dash = int_dash + 1
         case ("put_n_bf    "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("ATOM:table_width")
      CHECK
   end function

!  *******************
!  Axis change methods
!  *******************

   subroutine resolve_axis_system(self,crystal)
    ATOM :: self
   ! Change the axis system for the position and thermal tensors to
   ! "cartesian". "crystal" holds the axis system information.
     CRYSTAL, IN :: crystal
     STACK("ATOM:resolve_axis_system")
     START_TIMER("ATOM:resolve_axis_system")
     if (self%axis_system=="crystal")         call position_from_(self,crystal)
     if (self%thermal_axis_system=="crystal") call thermal_tensor_from_(self,crystal)
     STOP_TIMER("ATOM:resolve_axis_system")
      CHECK
   end subroutine

   subroutine change_axis_system_to(self,axiskind,crystal)
    ATOM :: self
   ! Change the axis system for the position and thermal tensors
   ! to "axiskind", either crystal or cartesian.
     STR(*), IN :: axiskind
     CRYSTAL, IN :: crystal
     STACK("ATOM:change_axis_system_to")
     START_TIMER("ATOM:change_axis_system_to")
     select case (axiskind)
       case ("cartesian"); call position_from_(self,crystal); call thermal_tensor_from_(self,crystal)
       case ("crystal  "); call position_to_(self,crystal);   call thermal_tensor_to_(self,crystal)
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,axiskind,"ATOM:change_axis_system_to")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:change_axis_system_to")
      CHECK
   end subroutine

   subroutine change_thermal_axis_system_to(self,axiskind,crystal)
    ATOM :: self
   ! Change the axis system for the thermal tensors to "axiskind",
   ! either crystal or cartesian.
     STR(*), IN :: axiskind
     CRYSTAL, IN :: crystal
     STACK("ATOM:change_thermal_axis_system_to")
     START_TIMER("ATOM:change_thermal_axis_system_to")
     select case (axiskind)
       case ("cartesian"); call thermal_tensor_from_(self,crystal)
       case ("crystal  "); call thermal_tensor_to_(self,crystal)
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "cartesian"
         tonto%known_keywords(2) = "crystal  "
         call unknown_(tonto,axiskind,"ATOM:change_thermal_axis_system_to")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:change_thermal_axis_system_to")
      CHECK
   end subroutine

   subroutine thermal_tensor_from(self,crystal)
    ATOM :: self
   ! Change thermal parameters in the crystal axis system to the
   ! cartesian axis system
     CRYSTAL, IN :: crystal
     STACK("ATOM:thermal_tensor_from")
     START_TIMER("ATOM:thermal_tensor_from")
     select case (self%thermal_axis_system)
       case ("crystal  ")
         if (self%U_iso>ZERO) then ! convert U_iso first
            self%thermal_tensor(1,1) = self%U_iso
            self%thermal_tensor(2,2) = self%U_iso
            self%thermal_tensor(3,3) = self%U_iso
            self%thermal_tensor(1,2) = self%U_iso * cos(gamma_star_(crystal%unitcell))
            self%thermal_tensor(1,3) = self%U_iso * cos(beta_star_(crystal%unitcell))
            self%thermal_tensor(2,3) = self%U_iso * cos(alpha_star_(crystal%unitcell))
         end if
         call change_basis_(self%thermal_tensor,crystal%unitcell%direct_U_matrix)
         self%thermal_axis_system = "cartesian"
       case ("cartesian")
         ! do nothing
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%thermal_axis_system,"ATOM:thermal_tensor_from")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:thermal_tensor_from")
      CHECK
   end subroutine

   subroutine thermal_tensor_to(self,crystal)
    ATOM :: self
   ! Change thermal parameters in the cartesian axis system to the
   ! crystal axis system
     CRYSTAL, IN :: crystal
     STACK("ATOM:thermal_tensor_to")
     START_TIMER("ATOM:thermal_tensor_to")
     select case (self%thermal_axis_system)
       case ("crystal  ")
         ! do nothing
       case ("cartesian")
         call change_basis_(self%thermal_tensor,crystal%unitcell%reciprocal_U_matrix)
         self%thermal_axis_system = "crystal"
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%thermal_axis_system,"ATOM:thermal_tensor_to")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:thermal_tensor_to")
      CHECK
   end subroutine

   subroutine position_from(self,crystal)
    ATOM :: self
   ! Change atom positions in the crystal axis system to the
   ! cartesian axis system
     CRYSTAL, IN :: crystal
     STACK("ATOM:position_from")
     START_TIMER("ATOM:position_from")
     select case (self%axis_system)
       case ("crystal  ")
         self%pos = matmul(crystal%unitcell%direct_matrix,self%pos)
         self%axis_system = "cartesian"
       case ("cartesian")
         ! do nothing
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%axis_system,"ATOM:position_from")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:position_from")
      CHECK
   end subroutine

   subroutine position_to(self,crystal)
    ATOM :: self
   ! Change atom positions in the cartesian axis system to the
   ! the crystal axis system
     CRYSTAL, IN :: crystal
     STACK("ATOM:position_to")
     START_TIMER("ATOM:position_to")
     select case (self%axis_system)
       case ("crystal  ")
         ! do nothing
       case ("cartesian")
         self%pos = matmul(crystal%unitcell%inverse_matrix,self%pos)
         self%axis_system = "crystal"
       case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "crystal  "
         tonto%known_keywords(2) = "cartesian"
         call unknown_(tonto,self%axis_system,"ATOM:position_to")
         deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOM:position_to")
      CHECK
   end subroutine

!  ************************
!  Basis resolution methods
!  ************************

!   resolve_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by matching the basis set label with one of the
!   ! labels from the basis set vector "basis". If "clobber" is present and TRUE
!   ! (the default situation), then the matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already assigned, it is
!   ! not pointer assigned. If present, "found" is set TRUE if the basis set has
!   ! been resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: BASISVEC*
!      clobber,found :: BIN, optional
!      b :: INT
!      check :: BINVEC*
!      force,fnd :: BIN
!   ENSURE(basis.created, "no basis set")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.basis.destroyed) then
!         if (present(found)) found = FALSE
!         return
!      end
!      if (NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      check.create(basis.dim)
!      check = basis.label==.basis.label
!      b = check.index_of_first_true_element
!      check.destroy
!      fnd = b>0
!      if (fnd) then
!      !  .basis.destroy     ! don't destroy
!         .basis => basis(b) ! NOTE : this is a pointer assign NOT COPY
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.basis.label))
!      end
!   end
!
!   resolve_library_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by first looking in the "basis" list, and then (if
!   ! needed) looking in a basis set library file. The appropriate basis set
!   ! library files are obtained from the basis set qualifier -- the part after
!   ! the colon in the atom basis set label. For example, if the atom basis set
!   ! label is "H:DZP", then the qualifier is "DZP" and the routine looks in
!   ! library file basis_sets/"DZP" for a matching basis set. If found, the basis
!   ! set is appended to "basis". If "clobber" is present and TRUE (the default
!   ! situation), then the matched basis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise if the matching basis set is already associated, it is not
!   ! pointer assigned.  If present, "found" is set TRUE if the basis set has
!   ! been resolved, or false otherwise. If "found" is not present, and a match
!   ! has not been found, an error is generated
!   ! NOTE: this should probably not be used ... .basis should be filled first
!   ! with the right bases and then they should be resolved in one hit.
!      basis :: BASISVEC*
!      clobber,found :: BIN, optional
!      i :: INT
!      basis_label,basis_kind,library :: STR
!      force,fnd :: BIN
!   ENSURE(.basis.created,"no basis set")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.basis.created AND NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      if (basis.created) .resolve_basis(basis,clobber=TRUE,found=fnd)
!      if (fnd) return
!      basis_label = .basis.label                 ! Look for this <<<<<<<
!      if (basis_label.includes(":")) then        ! look in library directory
!         i = basis_label.index_of_substring(":")
!         if (i>0) then
!            basis_kind = basis_label(i+1:)
!            if (basis_kind/=" ") then
!               library = basis.library_directory(basis_kind)
!               basis.read_library_data(library,[basis_label])
!               ! .basis.destroy  ! don't destroy ????
!               .basis => basis(basis.dim)  ! NOTE : this is a pointer assign NOT COPY
!               fnd = TRUE
!            end
!         end
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.basis.label))
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,found) ::: leaky
!   ! Resolve the basis set by first making a standard basis label, by joining
!   ! the atom chemical symbol with the :"suffix" string, and then trying to find
!   ! a match with one of the "basis" set vector labels.  If "clobber" is present
!   ! and FALSE, then only an unassociated .basis is resolved.  If "found" is
!   ! present and it is set TRUE if there was a match, otherwise FALSE; and if it
!   ! is not present an error is generated if no match is found.
!      basis :: BASISVEC*
!      suffix :: STR(*)
!      clobber,found :: BIN, optional
!      label :: STR
!      b :: INT
!      force,fnd :: BIN
!   ENSURE(basis.created,"no basis")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.basis.created AND NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      label = .library_basis_label(suffix)
!      fnd = FALSE
!      do b = 1,basis.dim
!         if (basis(b).label.same_as(label,ignore_case=TRUE)) then
!            fnd = TRUE
!            exit
!         end
!      end
!      if (fnd) then
!      !  .basis.destroy     ! don't destroy
!         .basis => basis(b) ! NOTE : this is a pointer assign NOT COPY
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.basis.label))
!      end
!   end
!
!   resolve_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by matching the basis set label with one of the
!   ! labels from the basis set vector "basis". If "clobber" is present and TRUE
!   ! (the default situation), then the matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already assigned, it is
!   ! not pointer assigned. If present, "found" is set TRUE if the basis set has
!   ! been resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: SLATERBASISVEC*
!      clobber,found :: BIN, optional
!      b :: INT
!      check :: BINVEC*
!      force,fnd :: BIN
!   ENSURE(basis.created, "no Coppens basis set")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.slaterbasis.destroyed) then
!         if (present(found)) found = FALSE
!         return
!      end
!      if (.slaterbasis.created AND NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      check.create(basis.dim)
!      check = basis.label==.slaterbasis.label
!      b = check.index_of_first_true_element
!      check.destroy
!      fnd = b>0
!      if (fnd) then
!      !  .slaterbasis.destroy     ! don't destroy
!         .slaterbasis => basis(b) ! NOTE : this is a pointer assign NOT COPY
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.slaterbasis.label))
!      end
!   end
!
!   resolve_library_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by first looking in the "basis" list, and then (if
!   ! needed) looking in a basis set library file. The appropriate basis set
!   ! library files are obtained from the basis set qualifier -- the part after
!   ! the colon in the atom basis set label. For example, if the atom basis set
!   ! label is "H:DZP", then the qualifier is "DZP" and the routine looks in
!   ! library file basis_sets/"DZP" for a matching basis set. If found, the basis
!   ! set is appended to "basis". If "clobber" is present and TRUE (the default
!   ! situation), then the matched basis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise if the matching basis set is already associated, it is not
!   ! pointer assigned.  If present, "found" is set TRUE if the basis set has
!   ! been resolved, or false otherwise. If "found" is not present, and a match
!   ! has not been found, an error is generated
!      basis :: SLATERBASISVEC*
!      clobber,found :: BIN, optional
!      i :: INT
!      basis_label,basis_kind,library :: STR
!      force,fnd :: BIN
!   ENSURE(.slaterbasis.created,"no Slater basis set")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.slaterbasis.created AND NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      if (basis.created) .resolve_basis(basis,clobber=TRUE,found=fnd)
!      if (fnd) return
!      basis_label = .slaterbasis.label                 ! Look for this <<<<<<<
!      if (basis_label.includes(":")) then        ! look in library directory
!         i = basis_label.index_of_substring(":")
!         if (i>0) then
!            basis_kind = basis_label(i+1:)
!            if (basis_kind/=" ") then
!               library = basis.library_directory(basis_kind)
!               basis.read_library_data(library,[basis_label])
!               ! .slaterbasis.destroy  ! don't destroy ????
!               .slaterbasis => basis(basis.dim)  ! NOTE : this is a pointer assign NOT COPY
!               fnd = TRUE
!            end
!         end
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.slaterbasis.label))
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,found) ::: leaky
!   ! Resolve the basis set by first making a standard basis label, by joining
!   ! the atom chemical symbol with the :"suffix" string, and then trying to find
!   ! a match with one of the "basis" set vector labels.  If "clobber" is present
!   ! and FALSE, then only an unassociated .slaterbasis is resolved.  If "found" is
!   ! present and it is set TRUE if there was a match, otherwise FALSE; and if it
!   ! is not present an error is generated if no match is found.
!      basis :: SLATERBASISVEC*
!      suffix :: STR(*)
!      clobber,found :: BIN, optional
!      label :: STR
!      b :: INT
!      force,fnd :: BIN
!   ENSURE(basis.created,"no Coppens basis")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.slaterbasis.created AND NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      label = .library_basis_label(suffix)
!      fnd = FALSE
!      do b = 1,basis.dim
!         if (basis(b).label.same_as(label,ignore_case=TRUE)) then
!            fnd = TRUE
!            exit
!         end
!      end
!      if (fnd) then
!      !  .slaterbasis.destroy     ! don't destroy
!         .slaterbasis => basis(b) ! NOTE : this is a pointer assign NOT COPY
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.slaterbasis.label))
!      end
!   end

!   resolve_basis(basis,clobber,found) 
!   ! Resolve the basis set by matching the basis set label with one of the
!   ! labels from the basis set vector "basis". If "clobber" is present and TRUE
!   ! (the default situation), then the matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already assigned, it is
!   ! not pointer assigned. If present, "found" is set TRUE if the basis set has
!   ! been resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: COPPENSBASISVEC*
!      clobber,found :: BIN, optional
!      b :: INT
!      check :: BINVEC*
!      force,fnd :: BIN
!   ENSURE(basis.created, "no Coppens basis set")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      stdout.text("1==")
!      if (.coppensbasis.destroyed) then
!         if (present(found)) found = FALSE
!         return
!      end
!      stdout.text("2==")
!      if (.coppensbasis.created AND NOT force) then
!      if (.coppensbasis.label/=" ") then
!         if (present(found)) found = TRUE
!         return
!      end
!      end
!      stdout.text("2==")
!      check.create(basis.dim)
!      check = basis.label==.coppensbasis.label
!      b = check.index_of_first_true_element
!      check.destroy
!      fnd = b>0
!      stdout.show("coppensbasis label =",.coppensbasis.label)
!      stdout.show("found              =",fnd)
!      if (fnd) then
!      !  .coppensbasis.destroy     ! don't destroy
!         .coppensbasis => basis(b) ! NOTE : this is a pointer assign NOT COPY
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.coppensbasis.label))
!      end
!   end

!   set_basis(basis,clobber) 
!   ! Set the .coppensbasis to be "basis". If "clobber" is present and TRUE (the
!   ! default situation), then .coppensbasis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise it is not pointer assigned. 
!      basis :: COPPENSBASIS
!      clobber :: BIN, optional
!      force :: BIN
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.coppensbasis.created AND NOT force) then
!      if (.coppensbasis.label/=" ") then
!         return
!      end
!      end
!      .coppensbasis => basis ! NOTE : this is a pointer assign NOT COPY
!   end

!   resolve_library_basis(basis,clobber,found) ::: leaky
!   ! Resolve the basis set by first looking in the "basis" list, and then (if
!   ! needed) looking in a basis set library file. The appropriate basis set
!   ! library files are obtained from the basis set qualifier -- the part after
!   ! the colon in the atom basis set label. For example, if the atom basis set
!   ! label is "H:DZP", then the qualifier is "DZP" and the routine looks in
!   ! library file basis_sets/"DZP" for a matching basis set. If found, the basis
!   ! set is appended to "basis". If "clobber" is present and TRUE (the default
!   ! situation), then the matched basis is pointer assigned to the matching
!   ! element in "basis" irrespective of whether it is already associated;
!   ! otherwise if the matching basis set is already associated, it is not
!   ! pointer assigned.  If present, "found" is set TRUE if the basis set has
!   ! been resolved, or false otherwise. If "found" is not present, and a match
!   ! has not been found, an error is generated
!      basis :: COPPENSBASISVEC*
!      clobber,found :: BIN, optional
!      i :: INT
!      basis_label,basis_kind,library :: STR
!      force,fnd :: BIN
!   ENSURE(.coppensbasis.created,"no Coppens basis set")
!      force = TRUE
!      if (present(clobber)) force = clobber
!      if (.coppensbasis.created AND NOT force) then
!         if (present(found)) found = TRUE
!         return
!      end
!      if (basis.created) .resolve_basis(basis,clobber=TRUE,found=fnd)
!      if (fnd) return
!      basis_label = .coppensbasis.label                 ! Look for this <<<<<<<
!      if (basis_label.includes(":")) then        ! look in library directory
!         i = basis_label.index_of_substring(":")
!         if (i>0) then
!            basis_kind = basis_label(i+1:)
!            if (basis_kind/=" ") then
!               library = basis.library_directory(basis_kind)
!               basis.read_library_data(library,[basis_label])
!               ! .coppensbasis.destroy  ! don't destroy ?
!               .coppensbasis => basis(basis.dim)  ! NOTE : this is a pointer assign NOT COPY
!               fnd = TRUE
!            end
!         end
!      end
!      if (present(found)) then; found = fnd
!      else; ENSURE(fnd,"unknown basis, "// trim(.coppensbasis.label))
!      end
!   end

   subroutine resolve_basis(self,basis,suffix,found)
    ATOM :: self
   ! Resolve the .basis by firstly trying to matching the ".basis_label"
   ! with one of the labels from "basis". If that fails, and "suffix" is present
   ! and not blank, then a library basis label is generated from the "suffix"
   ! and we try and match again to one of the labels in "basis". If present,
   ! "found" is set TRUE if the basis set has been resolved, or FALSE otherwise.
      BASISVEC(:), PTR :: basis
      STR(*), optional :: suffix
      BIN, optional :: found
      BIN :: fnd
   STACK("ATOM:resolve_basis")
   START_TIMER("ATOM:resolve_basis")
   ENSURE(associated(basis),"ATOM:resolve_basis ... no basis")
      fnd = FALSE
      if (self%basis_label/=" ") then
         call resolve_by_label_(self%basis,self%basis_label,basis,clobber=TRUE,found=fnd)
      end if
      if (NOT fnd AND present(suffix)) then
      if (suffix/=" ") then
         call resolve_by_label_(self%basis,library_basis_label_(self,suffix),basis,clobber=TRUE,found=fnd)
      end if
      end if
      if (present(found)) found = fnd
     STOP_TIMER("ATOM:resolve_basis")
      CHECK
   end subroutine

   subroutine resolve_basis_1(self,basis,suffix,found)
    ATOM :: self
   ! Resolve the .slaterbasis by firstly trying to matching the ".basis_label"
   ! with one of the labels from "basis". If that fails, and "suffix" is present
   ! and not blank, then a library basis label is generated from the "suffix"
   ! and we try and match again to one of the labels in "basis". If present,
   ! "found" is set TRUE if the basis set has been resolved, or FALSE otherwise.
      SLATERBASISVEC(:), PTR :: basis
      STR(*), optional :: suffix
      BIN, optional :: found
      BIN :: fnd
   STACK("ATOM:resolve_basis_1")
   START_TIMER("ATOM:resolve_basis_1")
   ENSURE(associated(basis),"ATOM:resolve_basis_1 ... no basis")
      fnd = FALSE
      if (self%basis_label/=" ") then
         call resolve_by_label_(self%slaterbasis,self%basis_label,basis,clobber=TRUE,found=fnd)
      end if
      if (NOT fnd AND present(suffix)) then
      if (suffix/=" ") then
         call resolve_by_label_(self%slaterbasis,library_basis_label_(self,suffix),basis,clobber=TRUE,found=fnd)
      end if
      end if
      if (present(found)) found = fnd
     STOP_TIMER("ATOM:resolve_basis_1")
      CHECK
   end subroutine

   subroutine resolve_basis_2(self,basis,suffix,found)
    ATOM :: self
   ! Resolve the .coppensbasis by firstly trying to matching the ".basis_label"
   ! with one of the labels from "basis". If that fails, and "suffix" is present
   ! and not blank, then a library basis label is generated from the "suffix"
   ! and we try and match again to one of the labels in "basis". If present,
   ! "found" is set TRUE if the basis set has been resolved, or FALSE otherwise.
      COPPENSBASISVEC(:), PTR :: basis
      STR(*), optional :: suffix
      BIN, optional :: found
      BIN :: fnd
   STACK("ATOM:resolve_basis_2")
   START_TIMER("ATOM:resolve_basis_2")
   ENSURE(associated(basis),"ATOM:resolve_basis_2 ... no basis")
      fnd = FALSE
      if (self%basis_label/=" ") then
         call resolve_by_label_(self%coppensbasis,self%basis_label,basis,clobber=TRUE,found=fnd)
      end if
      if (NOT fnd AND present(suffix)) then
      if (suffix/=" ") then
         call resolve_by_label_(self%coppensbasis,library_basis_label_(self,suffix),basis,clobber=TRUE,found=fnd)
      end if
      end if
      if (present(found)) found = fnd
     STOP_TIMER("ATOM:resolve_basis_2")
      CHECK
   end subroutine

!   resolve_by_basis_label(basis,label,clobber,found)
!   ! Resolve the .coppensbasis set by matching the ".basis_label" with one of
!   ! the labels from "basis". IF "label" is present it is used instead of
!   ! ".basis_label". If "clobber" is present and TRUE (the default situation),
!   ! then .coppensbasis is pointer assigned to the matching element in "basis"
!   ! irrespective of whether it is already associated; otherwise it is not
!   ! pointer assigned. If present, "found" is set TRUE if the basis set has been
!   ! resolved (or was already resolved if clobber was not set), or false
!   ! otherwise. If "found" is not present, and a match has not been found, an
!   ! error is generated
!      basis :: COPPENSBASISVEC*
!      label :: STR
!      clobber,found :: BIN, optional
!   ENSURE(basis.created,"no Coppens basis")
!      .coppensbasis.resolve_by_label(label,basis,clobber,found)
!   end
!
!   resolve_by_basis_suffix(basis,suffix,clobber,found) 
!   ! Resolve the basis set by first making a standard basis label, by joining
!   ! the atom chemical symbol with the ":suffix" string, and then trying to find
!   ! a match with one of the "basis" set vector labels.  If "clobber" is present
!   ! and FALSE, then only an unassociated .coppensbasis is resolved.  If "found" is
!   ! present and it is set TRUE if there was a match, otherwise FALSE; and if it
!   ! is not present an error is generated if no match is found.
!      basis :: COPPENSBASISVEC*
!      suffix :: STR(*)
!      clobber,found :: BIN, optional
!      label :: STR
!   ENSURE(basis.created,"no Coppens basis")
!      label = .library_basis_label(suffix)
!      .coppensbasis.resolve_by_label(label,basis,clobber,found)
!   end

!  ***************
!  Inquiry methods
!  ***************

   function same_kind_as(self,atom) result(res)
    ATOM :: self
   ! Return TRUE if self is the same kind of atom as "atom". The position and
   ! label are not compared since the same kind of atom can have a different
   ! position and label.  To check if the basis set is the same, only the
   ! label is used.
      ATOM :: atom
      BIN :: res
      BIN :: bases_created,coppens_created,bases_destroyed,coppens_destroyed
      BIN :: mixed
      STACK("ATOM:same_kind_as")
      START_TIMER("ATOM:same_kind_as")
      bases_created = associated(self%basis) AND associated(atom%basis)
      coppens_created = associated(self%coppensbasis) AND associated(atom%coppensbasis)
      bases_destroyed = NOT associated(self%basis) AND NOT associated(atom%basis)
      coppens_destroyed = NOT associated(self%coppensbasis) AND NOT associated(atom%coppensbasis)
      mixed = NOT (bases_created OR bases_destroyed) OR &
              NOT (coppens_created OR coppens_destroyed) 
      if (mixed) then
         res = FALSE
      else
         res = self%atomic_number==atom%atomic_number 
         if (bases_created)   res = res AND self%basis%label==atom%basis%label 
         if (coppens_created) res = res AND self%coppensbasis%label==atom%coppensbasis%label 
      end if
     STOP_TIMER("ATOM:same_kind_as")
      CHECK
   end function

   PURE function no_of_shells(self) result(res)
    ATOM :: self
   ! Return the no of shells
      IN :: self
      INT :: res
   
      res = no_of_shells_(self%basis)
     STOP_TIMER("ATOM:no_of_shells")
   end function

   PURE function n_shell(self) result(res)
    ATOM :: self
   ! Return the no of shells
      IN :: self
      INT :: res
   
      res = self%basis%n_shell
     STOP_TIMER("ATOM:n_shell")
   end function

   PURE function no_of_basis_functions(self) result(res)
    ATOM :: self
   ! Evaluate and return the no. of basis functions
      IN :: self
      INT :: res
   
      res = no_of_basis_functions_(self%basis)
     STOP_TIMER("ATOM:no_of_basis_functions")
   end function

   PURE function n_bf(self) result(res)
    ATOM :: self
   ! Return the no. of basis functions
      IN :: self
      INT :: res
   
      res = self%basis%n_bf
     STOP_TIMER("ATOM:n_bf")
   end function

   PURE function no_of_primitives(self) result(res)
    ATOM :: self
   ! Return the no of primitives for this atom
      IN :: self
      INT :: res
   
      res = no_of_primitives_(self%basis)
     STOP_TIMER("ATOM:no_of_primitives")
   end function

   PURE function n_prim(self) result(res)
    ATOM :: self
   ! Return the no of primitives for this atom
      IN :: self
      INT :: res
   
      res = self%basis%n_prim
     STOP_TIMER("ATOM:n_prim")
   end function

   function no_of_occupied_NOs(self,axiskind,tol) result(res)
    ATOM :: self
   ! Returns the number of non-zero occupied natural orbitals. For this purpose,
   ! zero is defined to be "tol" if present, or TOL(7) otherwise
      IN :: self
      STR(*), optional, IN :: axiskind
      REAL, optional, IN :: tol
      INT :: res
      STACK("ATOM:no_of_occupied_NOs")
      START_TIMER("ATOM:no_of_occupied_NOs")
      ENSURE(associated(self%occupation_numbers),"ATOM:no_of_occupied_NOs ... no occupation numbers")
      res = no_of_occupied_(self%occupation_numbers,axiskind,tol)
     STOP_TIMER("ATOM:no_of_occupied_NOs")
      CHECK
   end function

   function chemical_symbol(self,Z) result(res)
    ATOM :: self
   ! Return the chemical symbol for this atom. If "Z" is present then the symbol
   ! returned is the one for the atom with atomic number "Z".
      INT, IN, optional :: Z
      STR(2) :: res
      INT :: atomic_number
      STACK("ATOM:chemical_symbol")
      START_TIMER("ATOM:chemical_symbol")
      atomic_number = self%atomic_number
      if (present(Z)) atomic_number = Z
      if (atomic_number<1 OR atomic_number>103) then; res = "??"
      else;                  res = element_symbols(atomic_number)
      end if
     STOP_TIMER("ATOM:chemical_symbol")
      CHECK
   end function

   function chemical_name(self) result(res)
    ATOM :: self
   ! Return the chemical name for this atom
      STR(12) :: res
      STACK("ATOM:chemical_name")
      START_TIMER("ATOM:chemical_name")
      if (self%atomic_number<1 OR self%atomic_number>103) then; res = "??"
      else;                  res = element_names(self%atomic_number)
      end if
     STOP_TIMER("ATOM:chemical_name")
      CHECK
   end function

   function mass(self) result(res)
    ATOM :: self
   ! Return the atomic mass for this atom
      REAL :: res
      STACK("ATOM:mass")
      START_TIMER("ATOM:mass")
      if (self%atomic_number<1 OR self%atomic_number>92) then; res = ZERO
      else;                  res = atomic_masses(self%atomic_number)
      end if
     STOP_TIMER("ATOM:mass")
      CHECK
   end function

   function mean_neutron_number(self) result(res)
    ATOM :: self
   ! Return the average (abundance weighted) number of neutrons for this atom,
   ! calculated roughly by subtracting the number of protons from the atomic mass.
      REAL :: res
      STACK("ATOM:mean_neutron_number")
      START_TIMER("ATOM:mean_neutron_number")
      if (self%atomic_number<1 OR self%atomic_number>92) then; res = ZERO
      else;                  res = mass_(self) - self%atomic_number
      end if
     STOP_TIMER("ATOM:mean_neutron_number")
      CHECK
   end function

   function bragg_slater_radius(self) result(res)
    ATOM :: self
   ! Return the Bragg-Slater radius for this atom
      REAL :: res
      STACK("ATOM:bragg_slater_radius")
      START_TIMER("ATOM:bragg_slater_radius")
      ENSURE(self%atomic_number > 0,"ATOM:bragg_slater_radius ... atomic number less than 1")
      if (self%atomic_number>54) then; res = ZERO
      else;                  res = bragg_slater_radii(self%atomic_number)
      end if
     STOP_TIMER("ATOM:bragg_slater_radius")
      CHECK
   end function

   function neutron_scattering_length(self) result(res)
    ATOM :: self
   ! Return the neutron scattering length for this atom
      REAL :: res
      STACK("ATOM:neutron_scattering_length")
      START_TIMER("ATOM:neutron_scattering_length")
      if (self%atomic_number<1 OR self%atomic_number>95) then; res = ZERO
      else;            res = neutron_scattering_lengths(self%atomic_number)
      end if
     STOP_TIMER("ATOM:neutron_scattering_length")
      CHECK
   end function

   function period_number(self,Z) result(p)
    ATOM :: self
   ! Return the period (i.e. row) on which the atom lies.
   ! If "Z" is present it is used as the atomic number.
       INT, optional :: Z
       INT :: p
      INT :: atomic_number,noble,n
      STACK("ATOM:period_number")
      START_TIMER("ATOM:period_number")
      atomic_number = self%atomic_number
      if (present(Z)) atomic_number = Z
      p = 1
      if (atomic_number<1) then; STOP_TIMER("ATOM:period_number") CHECK return; end if
      noble = 0
      do
         n = (p+2)/2
         noble = noble + 2*n**2
         if (atomic_number <= noble) exit
         p = p + 1
      end do
     STOP_TIMER("ATOM:period_number")
      CHECK
   end function

   function column_number(self,Z) result(col)
    ATOM :: self
   ! Return the period column (i.e. row) on which the atom lies.
   ! If "Z" is present it is used as the atomic number.
       INT, optional :: Z
      INT :: col
      INT :: atomic_number,p,noble,n
      STACK("ATOM:column_number")
      START_TIMER("ATOM:column_number")
      atomic_number = self%atomic_number
      if (present(Z)) atomic_number = Z
      p = 1
      if (atomic_number<1) then; STOP_TIMER("ATOM:column_number") CHECK return; end if
      noble = 0
      do
         n = (p+2)/2
         noble = noble + 2*n**2
         if (atomic_number <= noble) exit
         p = p + 1
      end do
      noble = noble - 2*n**2
      col = atomic_number - noble
     STOP_TIMER("ATOM:column_number")
      CHECK
   end function

   function period_block(self,Z) result(b)
    ATOM :: self
   ! Return the period block character in which the atom lies.
   ! If "Z" is present it is used as the atomic number.
       INT, optional :: Z
       STR(1) :: b
      INT :: p,col
      STACK("ATOM:period_block")
      START_TIMER("ATOM:period_block")
      p   = period_number_(self,Z)
      col = column_number_(self,Z)
      if (p<4) then
         select case (col)
            case (1:2);   b = "s"
            case (3:8);   b = "p"
         end select
      else if (p<6) then
         select case (col)
            case (1:2);   b = "s"
            case (3:12);  b = "d"
            case (13:18); b = "p"
         end select
      else if (p<8) then
         select case (col)
            case (1:2);   b = "s"
            case (3:16);  b = "f"
            case (17:26); b = "d"
            case (27:32); b = "p"
         end select
      else
         DIE("ATOM:period_block ... cannot assign for period "// trim(to_str_(p)))
      end if
     STOP_TIMER("ATOM:period_block")
      CHECK
   end function

   function ground_state_multiplicity(self,Z) result(mult)
    ATOM :: self
   ! Return the ground state multiplicity for this atom according to Hunds rule
   ! (Note this is not neccesarily the real ground state, esp. for Cu)
   ! If "Z" is present it is used as the atomic number.
       INT, optional :: Z
      INT :: mult
      INT :: p,col
      STACK("ATOM:ground_state_multiplicity")
      START_TIMER("ATOM:ground_state_multiplicity")
      p   = period_number_(self,Z)
      col = column_number_(self,Z)
      if (p<4) then
         select case (col)
            case (2,8);   mult = 1
            case (1,3,7); mult = 2
            case (4,6);   mult = 3
            case (5);     mult = 4
         end select
      else if (p<6) then
         select case (col)
            case (2,12,18);      mult = 1
            case (1,3,11,13,17); mult = 2
            case (4,10,14,16);   mult = 3
            case (5,9,15);       mult = 4
            case (6,8);          mult = 5
            case (7);            mult = 6
         end select
      else if (p<8) then
         select case (col)
            case (2,16,26,32);         mult = 1
            case (1,3,15,17,25,27,31); mult = 2
            case (4,14,18,24,28,30);   mult = 3
            case (5,13,19,23,29);      mult = 4
            case (6,12,20,22);         mult = 5
            case (7,11,21);            mult = 6
            case (8,10);               mult = 7
            case (9);                  mult = 8
         end select
      else
         DIE("ATOM:ground_state_multiplicity ... cannot assign for period "// trim(to_str_(p)))
      end if
     STOP_TIMER("ATOM:ground_state_multiplicity")
      CHECK
   end function

   function dispersion_correction(self,wavelength) result(res)
    ATOM :: self
   ! The dispersion correction which best matches the wavelength.
     IN :: self
     REAL :: wavelength
     CPX :: res
     STACK("ATOM:dispersion_correction")
     START_TIMER("ATOM:dispersion_correction")
     if (self%atomic_number<1 OR self%atomic_number>92) then
       res = ZERO
     else
       if (wavelength < 2) then
         res = dispersion_correction_Mo( self%atomic_number ) ! 0.71A/1.34au
       else if (wavelength < 3.5) then
         res = dispersion_correction_Cu( self%atomic_number ) ! 1.542A/2.91au
       else
         res = dispersion_correction_Cr( self%atomic_number ) ! 2.29A/4.33au
       end if
     end if
     STOP_TIMER("ATOM:dispersion_correction")
      CHECK
   end function

   function library_basis_label(self,suffix) result(label)
    ATOM :: self
   ! Return a library basis set label by appending "suffix" to the
   ! chemical symbol.
      STR(*) :: suffix
      STR(STR_SIZE) :: label
      STR(STR_SIZE) :: symbol
      STACK("ATOM:library_basis_label")
      START_TIMER("ATOM:library_basis_label")
      symbol = chemical_symbol_(self)
      select case (suffix)
         case ("Coppens")
            if (self%atomic_number>55) then
               select case (self%atomic_number)
                  case (56);     symbol = "Sr"
                  case (57:71);  symbol = "Y"
                  case (72:87);  symbol = chemical_symbol_(self,self%atomic_number-32)
                  case (88);     symbol = "Sr"
                  case (89:103); symbol = "Y"
               end select
               WARN("ATOM:library_basis_label ... Replaced basis for atom "//trim(chemical_symbol_(self))//" with that for atom "//trim(symbol))
            end if
         case default
      end select
      label = trim(symbol)//":"//trim(suffix)
     STOP_TIMER("ATOM:library_basis_label")
      UNSTACK
   end function

   function has_ANO_data(self) result(res)
    ATOM :: self
   ! Return TRUE if the ANO data exists for the atom.
      BIN :: res
      STACK("ATOM:has_ANO_data")
      START_TIMER("ATOM:has_ANO_data")
      res = associated(self%natural_orbitals) AND associated(self%occupation_numbers)
     STOP_TIMER("ATOM:has_ANO_data")
      CHECK
   end function

   function has_basis(self) result(res)
    ATOM :: self
   ! Return TRUE if the basis exists
      BIN :: res
      STACK("ATOM:has_basis")
      START_TIMER("ATOM:has_basis")
      res = associated(self%basis)
     STOP_TIMER("ATOM:has_basis")
      CHECK
   end function

   function has_basis_label(self) result(res)
    ATOM :: self
   ! Return TRUE if the basis label exists and is not blank
      BIN :: res
      STACK("ATOM:has_basis_label")
      START_TIMER("ATOM:has_basis_label")
      if (NOT associated(self%basis)) then;       res = FALSE
      else if (self%basis%label==" ") then; res = FALSE
      else;                             res = TRUE
      end if
     STOP_TIMER("ATOM:has_basis_label")
      CHECK
   end function

   function min_basis_exponent(self) result(res)
    ATOM :: self
   ! Return the minimum exponent in the basis.
     REAL :: res
     STACK("ATOM:min_basis_exponent")
     START_TIMER("ATOM:min_basis_exponent")
     if (associated(self%coppensbasis)) then
       res = min_exponent_(self%coppensbasis)
     else if (associated(self%basis)) then
       res = min_exponent_(self%basis)
     else
       DIE("ATOM:min_basis_exponent ... no basis")
     end if
     STOP_TIMER("ATOM:min_basis_exponent")
      CHECK
   end function

!  *************************
!  Density plotting routines
!  *************************

   subroutine make_density_grid(self,density_grid,pt)
    ATOM :: self
   ! Work out the electron "density_grid" on "pt" using ".natural orbitals" and
   ! the ".occupation_numbers" vector.
      IN :: self
      REALVEC(:), OUT :: density_grid
      REALMAT(:,:), IN :: pt
   STACK("ATOM:make_density_grid")
   START_TIMER("ATOM:make_density_grid")
   ENSURE(size(density_grid)==size(pt,1),"ATOM:make_density_grid ... inconsistent number of points")
      if      (associated(self%coppensbasis)) then
         call make_density_grid_(self%coppensbasis,density_grid,pt,self%pos)
      else if (associated(self%slaterbasis)) then
         call make_density_grid_(self%slaterbasis,density_grid,pt,self%pos)
      else if (associated(self%basis) AND associated(self%natural_orbitals)) then
         if (number_kind_(self%natural_orbitals) == "real") then
            call make_density_grid_r_(self,density_grid,pt)
         else
            call make_density_grid_c_(self,density_grid,pt)
         end if
      else
         DIE("ATOM:make_density_grid ... Can't made density grid")
      end if
      if (NOT same_as_(self%site_occupancy,ONE)) &
         density_grid = self%site_occupancy*density_grid
     STOP_TIMER("ATOM:make_density_grid")
      CHECK
   end subroutine

   subroutine make_density_grid_r(self,density_grid,pt)
    ATOM :: self
   ! Make the "density_grid" for the supplied points "pt" from restricted real
   ! natural orbitals
     IN :: self
     REALVEC(:), OUT :: density_grid
     REALMAT(:,:), IN :: pt
     REALVEC(:), PTR :: NO
     INT :: n_occ,n
   STACK("ATOM:make_density_grid_r")
   START_TIMER("ATOM:make_density_grid_r")
   ENSURE(size(pt,2)==3,"ATOM:make_density_grid_r ... wrong dimension for points array")
   ENSURE(size(density_grid)==size(pt,1),"ATOM:make_density_grid_r ... inconsistent number of points")
   ENSURE(created_(self%natural_orbitals,"restricted"),"ATOM:make_density_grid_r ... no restricted NO's")
   ENSURE(created_(self%occupation_numbers,"restricted"),"ATOM:make_density_grid_r ... no occupation numbers")
     density_grid = ZERO
     call create_(NO,size(pt,1))
     n_occ = no_of_occupied_NOs_(self)
     do n = 1,n_occ
       call make_orbital_grid_r_(self,NO,self%natural_orbitals%restricted(:,n),pt,self%pos)
       density_grid = density_grid &
                    + self%occupation_numbers%restricted(n)*NO*NO
     end do
     call destroy_(NO)
     STOP_TIMER("ATOM:make_density_grid_r")
      CHECK
   end subroutine

   subroutine make_density_grid_c(self,density_grid,pt)
    ATOM :: self
   ! Make the "density_grid" for the supplied points "pt" from restricted
   ! complex natural orbitals.
      REALVEC(:) :: density_grid
      REALMAT(:,:) :: pt
      CPXVEC(:), PTR :: NO
      INT :: n_occ,n
   STACK("ATOM:make_density_grid_c")
   START_TIMER("ATOM:make_density_grid_c")
   ENSURE(size(pt,2)==3,"ATOM:make_density_grid_c ... wrong dimension for points array")
   ENSURE(size(density_grid)==size(pt,1),"ATOM:make_density_grid_c ... inconsistent number of points")
   ENSURE(created_(self%natural_orbitals,"restricted_complex"),"ATOM:make_density_grid_c ... no restricted NO's")
   ENSURE(created_(self%occupation_numbers,"restricted"),"ATOM:make_density_grid_c ... no occupation numbers")
      density_grid = ZERO
      call create_(NO,size(pt,1))
      n_occ = no_of_occupied_NOs_(self)
      do n = 1,n_occ
         call make_orbital_grid_c_(self,NO,self%natural_orbitals%restricted_complex(:,n),pt,self%pos)
         density_grid = density_grid &
                      + self%occupation_numbers%restricted(n)*conjg(NO)*NO
      end do
      call destroy_(NO)
     STOP_TIMER("ATOM:make_density_grid_c")
      CHECK
   end subroutine

   subroutine make_orbital_grid_r(self,g,orb,pt,pos,square)
    ATOM :: self
   ! Evaluate the orbital density grid "g" for *one* AO-basis coefficient
   ! orbital vector "orb" on a set of grid points "pt" for an atom at position
   ! "pos". If "square" is present and TRUE, the square of the orbital density
   ! is returned.
      REALVEC(:), OUT :: g
      REALVEC(:), IN :: orb
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
      BIN, optional, IN :: square
      BIN :: sq
      SHELL1 :: sh
      REALMAT(:,:), PTR :: sh_grid
      INT :: n_pt,f,l,s
   STACK("ATOM:make_orbital_grid_r")
   START_TIMER("ATOM:make_orbital_grid_r")
   ENSURE(associated(self%basis),"ATOM:make_orbital_grid_r ... no basis set")
   ENSURE(no_of_basis_functions_(self%basis)==size(orb),"ATOM:make_orbital_grid_r ... incorrect suize, orb")
      sq = FALSE
      if (present(square)) sq = square
      n_pt = size(pt,1)
      g = ZERO
      l = 0
      do s = 1,n_shell_(self)
         call set_(sh,self%basis%shell(s),pos)
         f = l + 1
         l = f + sh%n_comp - 1
         call create_(sh_grid,n_pt,sh%n_comp)
         call make_grid_(sh,sh_grid,pt)
         g = g + matmul(sh_grid,orb(f:l))
         call destroy_(sh_grid)
      end do
      if (sq) g = g*g
     STOP_TIMER("ATOM:make_orbital_grid_r")
      CHECK
   end subroutine

   subroutine make_orbital_grid_c(self,g,orb,pt,pos,square)
    ATOM :: self
   ! Evaluate the orbital density grid "g" for *one* AO-basis coefficient
   ! orbital vector "orb" on a set of grid points "pt" for an atom at position
   ! "pos". If "square" is present and TRUE, the square of the orbital density
   ! is returned.
      IN :: self
      CPXVEC(:), OUT :: g
      CPXVEC(:), IN :: orb
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
      BIN, optional, IN :: square
      BIN :: sq
      SHELL1 :: sh
      REALMAT(:,:), PTR :: sh_grid
      INT :: n_pt,f,l,s
   STACK("ATOM:make_orbital_grid_c")
   START_TIMER("ATOM:make_orbital_grid_c")
   ENSURE(associated(self%basis),"ATOM:make_orbital_grid_c ... no basis set")
   ENSURE(no_of_basis_functions_(self%basis)==size(orb),"ATOM:make_orbital_grid_c ... incorrect suize, orb")
      sq = FALSE
      if (present(square)) sq = square
      n_pt = size(pt,1)
      g = ZERO
      l = 0
      do s = 1,n_shell_(self)
         call set_(sh,self%basis%shell(s),pos)
         f = l + 1
         l = f + sh%n_comp - 1
         call create_(sh_grid,n_pt,sh%n_comp)
         call make_grid_(sh,sh_grid,pt)
         g = g + matmul(sh_grid,orb(f:l))
         call destroy_(sh_grid)
      end do
      if (sq) g = conjg(g)*g
     STOP_TIMER("ATOM:make_orbital_grid_c")
      CHECK
   end subroutine

   function density_at_radius(self,R) result(res)
    ATOM :: self
   ! Work out the electron at radius "R".
      REAL :: R,res
      REALVEC(1) :: density_grid
      REALMAT(3,1) :: pt
      STACK("ATOM:density_at_radius")
      START_TIMER("ATOM:density_at_radius")
      if (associated(self%coppensbasis) AND associated(self%coppensbasis%orbital)) then
         res = density_at_radius_(self%coppensbasis,R)
      else if (associated(self%basis)) then
         pt(:,1) = self%pos + (/R,ZERO,ZERO/)
         if (number_kind_(self%natural_orbitals) == "real") then
            call make_density_grid_r_(self,density_grid,pt)
         else
            call make_density_grid_c_(self,density_grid,pt)
         end if
         res = density_grid(1)
      end if
     STOP_TIMER("ATOM:density_at_radius")
      CHECK
   end function

!  ***************************
!  Basis shell access routines
!  ***************************

   subroutine get_shell_limits(self,first,last)
    ATOM :: self
   ! Get the indices of first and last basis functions in a particular shell
   ! "s", first(s) and last(s), respectively.
      INTVEC(:), PTR :: first,last
      INT :: n_shell,f,l,s
      STACK("ATOM:get_shell_limits")
      START_TIMER("ATOM:get_shell_limits")
      n_shell = self%basis%n_shell
      nullify(first); call create_(first,n_shell)
      nullify(last);  call create_(last,n_shell)
      l = 0
      do s = 1,n_shell
         f = l + 1
         l = f + self%basis%shell(s)%n_comp - 1
         first(s) = f
         last(s)  = l
      end do
     STOP_TIMER("ATOM:get_shell_limits")
      UNSTACK
   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    ATOM :: self
   ! Put out the atom information to file "stdout"
     STACK("ATOM:put")
     START_TIMER("ATOM:put")
     call flush_(stdout)
     call show_(stdout,"Label                  =",trim(self%label))
     call show_(stdout,"Atomic No.             =",self%atomic_number)
     call show_(stdout,"Chemical symbol        =",trim(chemical_symbol_(self)))
     call show_(stdout,"Atom coordinates       =",self%pos(1),self%pos(2),self%pos(3))
     if (self%sequence_number/=0) call put_mm_info_(self)
     if (associated(self%basis)) call put_(self%basis)
     if (associated(self%coppensbasis)) call put_(self%coppensbasis)
   ! if (.interpolator.created) .interpolator.put
     STOP_TIMER("ATOM:put")
      CHECK
   end subroutine

   subroutine put_mm_info(self)
    ATOM :: self
   ! Put out the MM/protien part of the atom information to file "stdout"
     STACK("ATOM:put_mm_info")
     START_TIMER("ATOM:put_mm_info")
     call flush_(stdout)
     call show_(stdout,"Residue atom name      =",trim(self%residue_atom_name))
     call show_(stdout,"Residue name           =",trim(self%residue_name))
     call show_(stdout,"Sequence number        =",self%sequence_number)
     call show_(stdout,"MM forcefield name     =",self%mm_forcefield_name)
     call show_(stdout,"MM atom type           =",self%mm_atom_type)
     call show_(stdout,"MM charge              =",self%mm_charge)
     call show_(stdout,"Restraining position   =",self%restraining_position)
     call show_(stdout,"Restraining force      =",self%restraining_force_constant)
     STOP_TIMER("ATOM:put_mm_info")
      CHECK
   end subroutine

   subroutine put_thermal_tensor(self)
    ATOM :: self
   ! Output the thermal tensor.  Does not put a header or carriage return.
   ! Only outputs the 6 independent ones, not all 9.
     STACK("ATOM:put_thermal_tensor")
     START_TIMER("ATOM:put_thermal_tensor")
     call put_(stdout,self%thermal_tensor(1,1))
     call put_(stdout,self%thermal_tensor(2,2))
     call put_(stdout,self%thermal_tensor(3,3))
     call put_(stdout,self%thermal_tensor(1,2))
     call put_(stdout,self%thermal_tensor(1,3))
     call put_(stdout,self%thermal_tensor(2,3))
     STOP_TIMER("ATOM:put_thermal_tensor")
      CHECK
   end subroutine

   subroutine put_natural_orbitals(self)
    ATOM :: self
   ! Put out the current associated molecular orbitals to file "out"
      STACK("ATOM:put_natural_orbitals")
      START_TIMER("ATOM:put_natural_orbitals")
      call flush_(stdout)
      call text_(stdout,"Natural orbital occupations:")
      call put_(stdout, self%occupation_numbers, format="column")
      call flush_(stdout)
      call text_(stdout,"Natural orbitals:")
      call put_(stdout, self%natural_orbitals)
     STOP_TIMER("ATOM:put_natural_orbitals")
      CHECK
   end subroutine

end
