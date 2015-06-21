!-------------------------------------------------------------------------------
!
! COPPENSORBITAL: used to describe contracted slater orbitals fitted to
! relativistic Hartree Fock calculations, as described by Coppens and Liu, and
! Macch and Coppens..
!
! Copyright (C) Mark Hore, Dylan Jayatilaka, 2002
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
! $Id: coppensorbital.foo,v 1.2.2.9 2004/04/21 09:12:54 reaper Exp $
!-------------------------------------------------------------------------------
module COPPENSORBITAL_MODULE

#  include "coppensorbital.use"

   implicit none

#  include "macros"
#  include "coppensorbital.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

   REAL, private :: values_cutoff = COPPENSORBITAL_VALUES_CUTOFF

contains

! ***************************
! Create and destroy routines
! ***************************

   subroutine create(self)
    COPPENSORBITAL :: self
   ! Create an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("COPPENSORBITAL:create")
      START_TIMER("COPPENSORBITAL:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(COPPENSORBITAL_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("COPPENSORBITAL:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    COPPENSORBITAL :: self
   ! Destroy an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("COPPENSORBITAL:destroy")
      START_TIMER("COPPENSORBITAL:destroy")
      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)
        DELETE_MEMORY(COPPENSORBITAL_SIZE)
      end if
     STOP_TIMER("COPPENSORBITAL:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    COPPENSORBITAL :: self
   ! Nullify the pointer parts of the atomvec
      STACK("COPPENSORBITAL:nullify_ptr_part")
      START_TIMER("COPPENSORBITAL:nullify_ptr_part")
      nullify(self%n)
      nullify(self%c)
      nullify(self%z)
     STOP_TIMER("COPPENSORBITAL:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    COPPENSORBITAL :: self
   ! Destroy the pointer parts
      STACK("COPPENSORBITAL:destroy_ptr_part")
      START_TIMER("COPPENSORBITAL:destroy_ptr_part")
      call destroy_(self%n)
      call destroy_(self%c)
      call destroy_(self%z)
     STOP_TIMER("COPPENSORBITAL:destroy_ptr_part")
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
    COPPENSORBITAL :: self
   ! Set default values
      STACK("COPPENSORBITAL:set_defaults")
      START_TIMER("COPPENSORBITAL:set_defaults")
      self%orb_kind = "?"
      self%occupancy = ZERO
      self%n_fun = 0
     STOP_TIMER("COPPENSORBITAL:set_defaults")
      CHECK
   end subroutine

   subroutine copy(self,c)
    COPPENSORBITAL :: self
   ! Create a copy of c
       COPPENSORBITAL :: c
       STACK("COPPENSORBITAL:copy")
       START_TIMER("COPPENSORBITAL:copy")
       self = c
       if (associated(c%n)) call create_copy_(self%n,c%n)
       if (associated(c%c)) call create_copy_(self%c,c%c)
       if (associated(c%z)) call create_copy_(self%z,c%z)
     STOP_TIMER("COPPENSORBITAL:copy")
      UNSTACK
   end subroutine

   function ex(self,i) result(res)
    COPPENSORBITAL :: self
   ! Return the exponent of the "i" th slater orbital
       INT :: i
       REAL :: res
       STACK("COPPENSORBITAL:ex")
       START_TIMER("COPPENSORBITAL:ex")
       res = self%z(i)
     STOP_TIMER("COPPENSORBITAL:ex")
      CHECK
   end function

   function cc(self,i) result(res)
    COPPENSORBITAL :: self
   ! Return the contraction coefficient of the "i" th slater orbital
       INT :: i
       REAL :: res
       STACK("COPPENSORBITAL:cc")
       START_TIMER("COPPENSORBITAL:cc")
       res = self%c(i)
     STOP_TIMER("COPPENSORBITAL:cc")
      CHECK
   end function

! ***********
! I/O methods
! ***********

   recursive subroutine read_keywords(self)
    COPPENSORBITAL :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("COPPENSORBITAL:read_keywords")
     START_TIMER("COPPENSORBITAL:read_keywords")
     ENSURE(next_item_(stdin)=="{","COPPENSORBITAL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("COPPENSORBITAL:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    COPPENSORBITAL :: self
   ! Process command "keyword". Data is inputted from "stdin", unless "keyword"
   ! is a sequence of blank separated strings.  In this case, the sequence is
   ! processed as if it were a separate file.
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("COPPENSORBITAL:process_keyword")
     START_TIMER("COPPENSORBITAL:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                          ")  ! exit case
        case ("c=                         "); call read_c_(self)
        case ("kind=                      "); call read_kind_(self)
        case ("kind,occupancy,num,n,c,z=  "); call read_kind_occ_num_n_c_z_(self)
        case ("n=                         "); call read_n_(self)
        case ("n,c,z=                     "); call read_n_c_z_(self)
        case ("n,c,z*=                    "); call read_n_c_z_ptr_(self)
        case ("n_fun=                     "); call read_n_fun_(self)
        case ("num=                       "); call read_n_fun_(self)
        case ("occupancy=                 "); call read_occupancy_(self)
        case ("values_cutoff=             "); call read_values_cutoff_(self)
        case ("z=                         "); call read_z_(self)
        case  default;                        allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "}                          "
        tonto%known_keywords(2) = "c=                         "
        tonto%known_keywords(3) = "kind=                      "
        tonto%known_keywords(4) = "kind,occupancy,num,n,c,z=  "
        tonto%known_keywords(5) = "n=                         "
        tonto%known_keywords(6) = "n,c,z=                     "
        tonto%known_keywords(7) = "n,c,z*=                    "
        tonto%known_keywords(8) = "n_fun=                     "
        tonto%known_keywords(9) = "num=                       "
        tonto%known_keywords(10) = "occupancy=                 "
        tonto%known_keywords(11) = "values_cutoff=             "
        tonto%known_keywords(12) = "z=                         "
        call unknown_(tonto,word,"COPPENSORBITAL:process_keyword")
        deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("COPPENSORBITAL:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    COPPENSORBITAL :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("COPPENSORBITAL:read_units")
      START_TIMER("COPPENSORBITAL:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("COPPENSORBITAL:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    COPPENSORBITAL :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("COPPENSORBITAL:read_junk")
      START_TIMER("COPPENSORBITAL:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("COPPENSORBITAL:read_junk")
      CHECK
   end subroutine

   subroutine read_kind(self)
    COPPENSORBITAL :: self
   ! Read in the orbital kind ("1s", "2s", "2p" ....)
      STACK("COPPENSORBITAL:read_kind")
      START_TIMER("COPPENSORBITAL:read_kind")
      call read_(stdin,self%orb_kind)
     STOP_TIMER("COPPENSORBITAL:read_kind")
      CHECK
   end subroutine

   subroutine read_occupancy(self)
    COPPENSORBITAL :: self
   ! Read in the orbital occupancy
      STACK("COPPENSORBITAL:read_occupancy")
      START_TIMER("COPPENSORBITAL:read_occupancy")
      call read_(stdin,self%occupancy)
     STOP_TIMER("COPPENSORBITAL:read_occupancy")
      CHECK
   end subroutine

   subroutine read_n_fun(self)
    COPPENSORBITAL :: self
   ! Read in the number of contracted functions.
      STACK("COPPENSORBITAL:read_n_fun")
      START_TIMER("COPPENSORBITAL:read_n_fun")
      call read_(stdin,self%n_fun)
     STOP_TIMER("COPPENSORBITAL:read_n_fun")
      CHECK
   end subroutine

   subroutine read_n(self)
    COPPENSORBITAL :: self
   ! Read in the "n" quantum numbers. NOTE: n_fun must already have been input.
      ENSURE(self%n_fun>0,"COPPENSORBITAL:read_n ... n_fun is negative; use n_fun= before this command")
      STACK("COPPENSORBITAL:read_n")
      START_TIMER("COPPENSORBITAL:read_n")
      call destroy_(self%n)
      call create_(self%n,self%n_fun)
      call read_(stdin,self%n)
     STOP_TIMER("COPPENSORBITAL:read_n")
      UNSTACK
   end subroutine

   subroutine read_c(self)
    COPPENSORBITAL :: self
   ! Read in the "c" contraction coefficients. NOTE: n_fun must already have
   ! been input.
      ENSURE(self%n_fun>0,"COPPENSORBITAL:read_c ... n_fun is negative; use n_fun= before this command")
      STACK("COPPENSORBITAL:read_c")
      START_TIMER("COPPENSORBITAL:read_c")
      call destroy_(self%c)
      call create_(self%c,self%n_fun)
      call read_(stdin,self%c)
     STOP_TIMER("COPPENSORBITAL:read_c")
      UNSTACK
   end subroutine

   subroutine read_z(self)
    COPPENSORBITAL :: self
   ! Read in the "z" slater function exponents. NOTE: n_fun must already have
   ! been input.
      ENSURE(self%n_fun>0,"COPPENSORBITAL:read_z ... n_fun is negative; use n_fun= before this command")
      STACK("COPPENSORBITAL:read_z")
      START_TIMER("COPPENSORBITAL:read_z")
      call destroy_(self%z)
      call create_(self%z,self%n_fun)
      call read_(stdin,self%z)
     STOP_TIMER("COPPENSORBITAL:read_z")
      UNSTACK
   end subroutine

   subroutine read_n_c_z(self)
    COPPENSORBITAL :: self
   ! Read in the "n", "c" and "z" vectors. NOTE: .n_fun must previously have been inputted.
      INT :: i
      REAL :: val
      ENSURE(self%n_fun>0,"COPPENSORBITAL:read_n_c_z ... n_fun is negative; use n_fun= before this command")
      STACK("COPPENSORBITAL:read_n_c_z")
      START_TIMER("COPPENSORBITAL:read_n_c_z")
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_fun)
      call create_(self%c,self%n_fun)
      call create_(self%z,self%n_fun)
      do i = 1,self%n_fun
         call read_(stdin,val)
         ENSURE(is_int_(val),"COPPENSORBITAL:read_n_c_z ... n number is not integer")
         self%n(i) = val
         call read_(stdin,self%c(i))
         call read_(stdin,self%z(i))
      end do
     STOP_TIMER("COPPENSORBITAL:read_n_c_z")
      UNSTACK
   end subroutine

   subroutine read_n_c_z_ptr(self)
    COPPENSORBITAL :: self
   ! Read in the "n", "c" and "z" vectors. NOTE: it is not necessary to read in
   ! n_fun, it will be defined from the vector length.
      REALVEC(:), PTR :: tmp
      INT :: i,k
      STACK("COPPENSORBITAL:read_n_c_z_ptr")
      START_TIMER("COPPENSORBITAL:read_n_c_z_ptr")
      call read_(stdin,tmp)
      ENSURE(mod(size(tmp),3)==0,"COPPENSORBITAL:read_n_c_z_ptr ... number of data not divisible by three")
      self%n_fun = size(tmp)/3
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_fun)
      call create_(self%c,self%n_fun)
      call create_(self%z,self%n_fun)
      k = 1
      do i = 1,3
         ENSURE(is_int_(tmp(k)),"COPPENSORBITAL:read_n_c_z_ptr ... n number is not integer")
         self%n(k) = tmp(k)
         self%c(k) = tmp(k+1)
         self%z(k) = tmp(k+2)
         k = k + 3
      end do
      call destroy_(tmp)
     STOP_TIMER("COPPENSORBITAL:read_n_c_z_ptr")
      UNSTACK
   end subroutine

   subroutine read_kind_occ_num_n_c_z(self)
    COPPENSORBITAL :: self
   ! Read in everything: the "kind", "occupnacy", "n_fun", and "n", "c" and "z".
      STACK("COPPENSORBITAL:read_kind_occ_num_n_c_z")
      START_TIMER("COPPENSORBITAL:read_kind_occ_num_n_c_z")
      call read_kind_(self)
      call read_occupancy_(self)
      call read_n_fun_(self)
      call read_n_c_z_(self)
     STOP_TIMER("COPPENSORBITAL:read_kind_occ_num_n_c_z")
      UNSTACK
   end subroutine

   subroutine read_values_cutoff(self)
    COPPENSORBITAL :: self
   ! Read in a cutoff below which values of the the coppensorbital on a grid are
   ! set to zero.
      STACK("COPPENSORBITAL:read_values_cutoff")
      START_TIMER("COPPENSORBITAL:read_values_cutoff")
      call read_(stdin,values_cutoff)
     STOP_TIMER("COPPENSORBITAL:read_values_cutoff")
      CHECK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    COPPENSORBITAL :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("COPPENSORBITAL:read_keys")
     START_TIMER("COPPENSORBITAL:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("COPPENSORBITAL:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    COPPENSORBITAL :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("COPPENSORBITAL:process_keys")
      START_TIMER("COPPENSORBITAL:process_keys")
      ENSURE(associated(keys),"COPPENSORBITAL:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"COPPENSORBITAL:process_keys ... no matching closing brace, }")
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
     STOP_TIMER("COPPENSORBITAL:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    COPPENSORBITAL :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("COPPENSORBITAL:keys_created")
      START_TIMER("COPPENSORBITAL:keys_created")
      res = associated(keys)
     STOP_TIMER("COPPENSORBITAL:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    COPPENSORBITAL :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("COPPENSORBITAL:set_keys")
     START_TIMER("COPPENSORBITAL:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("COPPENSORBITAL:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    COPPENSORBITAL :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("COPPENSORBITAL:clear_keys")
     START_TIMER("COPPENSORBITAL:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("COPPENSORBITAL:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    COPPENSORBITAL :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("COPPENSORBITAL:put_table_footer")
     START_TIMER("COPPENSORBITAL:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("COPPENSORBITAL:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    COPPENSORBITAL :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
     STACK("COPPENSORBITAL:put_table_header")
     START_TIMER("COPPENSORBITAL:put_table_header")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush        "); call flush_(stdout); exit
           case ("put_kind     "); call put_(stdout,"kind",int_width=TRUE)
           case ("put_occ      "); call put_(stdout,"Occ.",int_width=TRUE)
           case ("put_occupancy"); call put_(stdout,"Occ.",int_width=TRUE)
           case ("put_n_fun    "); call put_(stdout,"n_fun",int_width=TRUE)
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("COPPENSORBITAL:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("COPPENSORBITAL:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    COPPENSORBITAL :: self
   ! Return how wide a table is, based on "keys".  Note that not all keywords
   ! need to contribute to the banner - any unrecognised keyword is skipped.
     INT :: res
     STR(STR_SIZE) :: word
     INT :: int_dash,real_dash,k
     STACK("COPPENSORBITAL:table_width")
     START_TIMER("COPPENSORBITAL:table_width")
     int_dash = 0
     real_dash = 0
     ENSURE(associated(keys),"COPPENSORBITAL:table_width ... no keywords")
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("put_kind     "); int_dash = int_dash + 1
         case ("put_occ      "); int_dash = int_dash + 1
         case ("put_occupancy"); int_dash = int_dash + 1
         case ("put_n_fun    "); int_dash = int_dash + 1
         case ("flush        "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("COPPENSORBITAL:table_width")
      CHECK
   end function

! *******
! Methods
! *******

   function same_as(self,b) result(res)
    COPPENSORBITAL :: self
   ! Return TRUE if "self" is the same as "b".
      IN :: self
      COPPENSORBITAL, IN :: b
      BIN :: res
      STACK("COPPENSORBITAL:same_as")
      START_TIMER("COPPENSORBITAL:same_as")
      res = self%orb_kind == b%orb_kind AND self%occupancy == b%occupancy &
            AND same_as_(self%n,b%n) AND same_as_(self%c,b%c) AND same_as_(self%z,b%z)
     STOP_TIMER("COPPENSORBITAL:same_as")
      CHECK
   end function

   function density_at_radius(self,R) result(res)
    COPPENSORBITAL :: self
   ! Returns the value of the coppens orbital at radius "R".
      REAL, IN :: R
      REAL :: res
      STACK("COPPENSORBITAL:density_at_radius")
      START_TIMER("COPPENSORBITAL:density_at_radius")
      res = value_at_radius_(self,R)
      res = self%occupancy*res*res
     STOP_TIMER("COPPENSORBITAL:density_at_radius")
      CHECK
   end function

   function densities_at_radii(self,R) result(res)
    COPPENSORBITAL :: self
   ! Returns the values of the coppens density at all the radii "R".
      REALVEC(:), IN :: R
      REALVEC(size(R)) :: res
      STACK("COPPENSORBITAL:densities_at_radii")
      START_TIMER("COPPENSORBITAL:densities_at_radii")
      res = values_at_radii_(self,R)
      res = self%occupancy*res*res
     STOP_TIMER("COPPENSORBITAL:densities_at_radii")
      CHECK
   end function

   function value_at_radius(self,R) result(res)
    COPPENSORBITAL :: self
   ! Returns the value of the coppens orbital at radius "R".
      REAL, IN :: R
      REAL :: res
      INTVEC(:), PTR :: nm1
      STACK("COPPENSORBITAL:value_at_radius")
      START_TIMER("COPPENSORBITAL:value_at_radius")
      res = ZERO
      call create_(nm1,size(self%n))
      nm1 = self%n - 1
      res = sum(self%c * R**nm1 * exp(-self%z*R))
      call destroy_(nm1)
     STOP_TIMER("COPPENSORBITAL:value_at_radius")
      CHECK
   end function

   function values_at_radii(self,R) result(res)
    COPPENSORBITAL :: self
   ! Returns the values of the coppens orbital at all the radii "R".
      REALVEC(:), IN :: R
      REALVEC(size(R)) :: res
      INTVEC(:), PTR :: nm1
      INT :: i
      STACK("COPPENSORBITAL:values_at_radii")
      START_TIMER("COPPENSORBITAL:values_at_radii")
      call create_(nm1,size(self%n))
      nm1 = self%n - 1
      do i = 1,size(R)
         res(i) = sum(self%c * R(i)**nm1 * exp(-self%z*R(i)) )
      end do
      call destroy_(nm1)
     STOP_TIMER("COPPENSORBITAL:values_at_radii")
      CHECK
   end function

   subroutine unnormalise(self)
    COPPENSORBITAL :: self
   ! Set the value of the coefficient to correspond to un-normalised Slater
   ! functions -- assuming they are normalised. This saves in computation time.
      INT :: i,n2
      STACK("COPPENSORBITAL:unnormalise")
      START_TIMER("COPPENSORBITAL:unnormalise")
      do i = 1,self%n_fun
        n2 = 2*self%n(i)
        self%c(i) = self%c(i) * sqrt(TWO*self%z(i)/factorial_(n2)) * (TWO*self%z(i))**self%n(i)
      end do
     STOP_TIMER("COPPENSORBITAL:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    COPPENSORBITAL :: self
   ! Set the value of the coefficient to correspond to normalised slater
   ! functions --- assuming they are w.r.t. unnormalised functions. 
      INT :: i,n2
      STACK("COPPENSORBITAL:renormalise")
      START_TIMER("COPPENSORBITAL:renormalise")
      do i = 1,self%n_fun
        n2 = 2*self%n(i)
        self%c(i) = self%c(i) / (sqrt(TWO*self%z(i)/factorial_(n2)) * (TWO*self%z(i))**self%n(i))
      end do
     STOP_TIMER("COPPENSORBITAL:renormalise")
      CHECK
   end subroutine

   function values_at_points(self,pt,pos) result(res)
    COPPENSORBITAL :: self
   ! Make the orbital values on the series of points specified in "pt", assuming
   ! the orbital is at point "pos"; and put the results in "res".
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
      REALVEC(size(pt,1)) :: res
      REALVEC(:), PTR :: R
      REAL :: x,y,z
      INT :: n,n_pt
      STACK("COPPENSORBITAL:values_at_points")
      START_TIMER("COPPENSORBITAL:values_at_points")
      ENSURE(size(pt,2)==3,"COPPENSORBITAL:values_at_points ... incorrect second dimension, pt array")
      n_pt = size(pt,1)
      call create_(R,n_pt)
      do n = 1,n_pt
         x = pt(n,1) - pos(1)
         y = pt(n,2) - pos(2)
         z = pt(n,3) - pos(3)
         R(n) = sqrt(x*x + y*y + z*z)
      end do
      res = values_at_radii_(self,R)
      call destroy_(R)
     STOP_TIMER("COPPENSORBITAL:values_at_points")
      CHECK
   end function

   subroutine make_values_at_opt(self,pt,pos,grid)
    COPPENSORBITAL :: self
   ! Make the orbital values on the series of points specified in "pt", assuming
   ! the orbital is at point "pos"; and put the results in the array "grid".
   ! This is an optimised version.
      IN :: self
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
      REALVEC(:), OUT :: grid
      REALVEC(:), PTR :: tmp,n_val
      REAL :: x,y,z,R,R2,pos1,pos2,pos3
      REAL :: cutoff,cutoff1,cutoff2,cutoff3,cutoff4,minz,maxn1divminz
      INT :: n,n_pt,i,n2,maxn1
      STACK("COPPENSORBITAL:make_values_at_opt")
      START_TIMER("COPPENSORBITAL:make_values_at_opt")
      ENSURE(size(grid)==size(pt,1),"COPPENSORBITAL:make_values_at_opt ... grid size wrong!")

      ! The cutoff of the orbital value.  Values smaller than this are set to
      ! zero.
      cutoff = values_cutoff
      call create_(n_val,size(self%n))
      n_val = self%n - ONE

      pos1 = pos(1)
      pos2 = pos(2)
      pos3 = pos(3)
      call create_(tmp,self%n_fun)
      do i = 1,self%n_fun
        n2 = 2*self%n(i)
        tmp(i) = self%c(i) * sqrt(TWO*self%z(i)/factorial_(n2)) * (TWO*self%z(i))**self%n(i)
      end do

      maxn1 = maxval(self%n)-1
      minz =  minval(self%z)
      maxn1divminz = maxn1/minz

      ! cutoff1 <= maxn1*log(R) - minz*R.  (Strict test).
      cutoff1 = log(cutoff/(self%n_fun*maxval(tmp)))
      cutoff2 = cutoff1/minz

      ! Looser yet strict test.
      ! log(R) < R, substitute into strict test.
      cutoff3 = cutoff1/(maxn1-minz)
      ! Do the test on R^2, so avoid sqrt below where possible.
      cutoff4 = cutoff3*cutoff3

      n_pt = size(pt,1)
      do n = 1,n_pt
         x = pt(n,1) - pos1
         y = pt(n,2) - pos2
         z = pt(n,3) - pos3
         R2 = x*x + y*y + z*z

         ! Do maxn==1 as special case.
         if (maxn1==0) then

           ! Do test using R2.
           if (cutoff4 < R2) then
             grid(n) = ZERO
           else if (R2 == ZERO) then
             grid(n) = sum(ZERO**n_val(:) * tmp(:))
           else
             ! Do test using R.
             R = sqrt(R2)
             if (cutoff2 > -R) then
               grid(n) = ZERO
             else
               grid(n) = sum(R**n_val(:) * exp(-self%z(:)*R) * tmp(:))
             end if
           end if

         ! not maxn==1 special case.
         else

           ! Do test using R2.
           if (cutoff4 < R2) then
             grid(n) = ZERO
           else if (R2 == ZERO) then
             grid(n) = sum(tmp(:))
           else
             ! Do test using R.
             R = sqrt(R2)
             if (cutoff2 > maxn1divminz*log(R)-R) then
               grid(n) = ZERO
             else
               grid(n) = sum(R**n_val(:) * exp(-self%z(:)*R) * tmp(:))
             end if
           end if
         end if

      end do
      call destroy_(tmp)
      call destroy_(n_val)
     STOP_TIMER("COPPENSORBITAL:make_values_at_opt")
      CHECK
   end subroutine

   subroutine put(self)
    COPPENSORBITAL :: self
   ! Put the orbital information to "stdout"
      STACK("COPPENSORBITAL:put")
      START_TIMER("COPPENSORBITAL:put")
      call flush_(stdout)
      call show_(stdout,"Kind        = ",self%orb_kind)
      call show_(stdout,"Occupancy   = ",self%occupancy)
      call show_(stdout,"No. of funs = ",self%n_fun)
      call put_table_(self)
     STOP_TIMER("COPPENSORBITAL:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    COPPENSORBITAL :: self
   ! Put the orbital information to "stdout" in table format
      INT :: i
      STACK("COPPENSORBITAL:put_table")
      START_TIMER("COPPENSORBITAL:put_table")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"N",int_width=TRUE)
      call put_(stdout,"Exponent")
      call put_(stdout,"Coeff.")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      do i = 1,self%n_fun
         call put_(stdout,i)
         call put_(stdout,self%n(i))
         call put_(stdout,self%z(i))
         call put_(stdout,self%c(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=2)
     STOP_TIMER("COPPENSORBITAL:put_table")
      CHECK
   end subroutine

end
