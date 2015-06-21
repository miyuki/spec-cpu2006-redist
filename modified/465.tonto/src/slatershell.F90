!-------------------------------------------------------------------------------
!
! SLATERSHELL: used to describe contracted slater shells.
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
! $Id: slatershell.foo,v 1.1.2.5 2004/04/21 09:12:56 reaper Exp $
!-------------------------------------------------------------------------------

module SLATERSHELL_MODULE

#  include "slatershell.use"

   implicit none

#  include "macros"
#  include "slatershell.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

contains

! ***************************
! Create and destroy routines
! ***************************

   subroutine create(self)
    SLATERSHELL :: self
   ! Create an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("SLATERSHELL:create")
      START_TIMER("SLATERSHELL:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(SLATERSHELL_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SLATERSHELL:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SLATERSHELL :: self
   ! Destroy an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("SLATERSHELL:destroy")
      START_TIMER("SLATERSHELL:destroy")
      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)
        DELETE_MEMORY(SLATERSHELL_SIZE)
      end if
     STOP_TIMER("SLATERSHELL:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SLATERSHELL :: self
   ! Nullify the pointer parts of the atomvec
      STACK("SLATERSHELL:nullify_ptr_part")
      START_TIMER("SLATERSHELL:nullify_ptr_part")
      nullify(self%n)
      nullify(self%z)
      nullify(self%c)
      nullify(self%orb_kind)
      nullify(self%occupancy)
     STOP_TIMER("SLATERSHELL:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SLATERSHELL :: self
   ! Destroy the pointer parts
      STACK("SLATERSHELL:destroy_ptr_part")
      START_TIMER("SLATERSHELL:destroy_ptr_part")
      call destroy_(self%n)
      call destroy_(self%z)
      call destroy_(self%c)
      call destroy_(self%orb_kind)
      call destroy_(self%occupancy)
     STOP_TIMER("SLATERSHELL:destroy_ptr_part")
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
    SLATERSHELL :: self
   ! Set default values
      STACK("SLATERSHELL:set_defaults")
      START_TIMER("SLATERSHELL:set_defaults")
      self%l = 0
      self%n_orb = 0
      self%n_prim = 0
     STOP_TIMER("SLATERSHELL:set_defaults")
      CHECK
   end subroutine

   subroutine copy(self,c)
    SLATERSHELL :: self
   ! Create a copy of c
       SLATERSHELL :: c
       STACK("SLATERSHELL:copy")
       START_TIMER("SLATERSHELL:copy")
       self = c
       if (associated(c%n)) call create_copy_(self%n,c%n)
       if (associated(c%z)) call create_copy_(self%z,c%z)
       if (associated(c%c)) call create_copy_(self%c,c%c)
       if (associated(c%orb_kind)) call create_copy_(self%orb_kind,c%orb_kind)
       if (associated(c%occupancy)) call create_copy_(self%occupancy,c%occupancy)
     STOP_TIMER("SLATERSHELL:copy")
      UNSTACK
   end subroutine

   subroutine set_n_comp(self)
    SLATERSHELL :: self
   ! Set the number of components
      STACK("SLATERSHELL:set_n_comp")
      START_TIMER("SLATERSHELL:set_n_comp")
      self%n_comp = 2*self%l+1
     STOP_TIMER("SLATERSHELL:set_n_comp")
      CHECK
   end subroutine

! ***********
! I/O methods
! ***********

   recursive subroutine read_keywords(self)
    SLATERSHELL :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("SLATERSHELL:read_keywords")
     START_TIMER("SLATERSHELL:read_keywords")
     ENSURE(next_item_(stdin)=="{","SLATERSHELL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("SLATERSHELL:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SLATERSHELL :: self
   ! Process command "keyword". Data is inputted from "stdin", unless "keyword"
   ! is a sequence of blank separated strings.  In this case, the sequence is
   ! processed as if it were a separate file.
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("SLATERSHELL:process_keyword")
     START_TIMER("SLATERSHELL:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
        case ("}                          ")  ! exit case
        case ("c=                         "); call read_c_(self)
        case ("junk=                      "); call read_junk_(self)
        case ("kind=                      "); call read_kind_(self)
        case ("l=                         "); call read_l_(self)
        case ("l_int=                     "); call read_l_int_(self)
        case ("l_chr=                     "); call read_l_chr_(self)
        case ("l,kind,n,z,c*=             "); call read_l_kind_n_z_c_ptr_(self)
        case ("n=                         "); call read_n_(self)
        case ("n_prim=                    "); call read_n_prim_(self)
        case ("n_orb=                     "); call read_n_orb_(self)
        case ("n,z,c*=                    "); call read_n_z_c_ptr_(self)
        case ("occupancy=                 "); call read_occupancy_(self)
        case ("units=                     "); call read_units_(self)
        case ("z=                         "); call read_z_(self)
        case  default;                        allocate(tonto%known_keywords(15))
        tonto%known_keywords(1) = "}                          "
        tonto%known_keywords(2) = "c=                         "
        tonto%known_keywords(3) = "junk=                      "
        tonto%known_keywords(4) = "kind=                      "
        tonto%known_keywords(5) = "l=                         "
        tonto%known_keywords(6) = "l_int=                     "
        tonto%known_keywords(7) = "l_chr=                     "
        tonto%known_keywords(8) = "l,kind,n,z,c*=             "
        tonto%known_keywords(9) = "n=                         "
        tonto%known_keywords(10) = "n_prim=                    "
        tonto%known_keywords(11) = "n_orb=                     "
        tonto%known_keywords(12) = "n,z,c*=                    "
        tonto%known_keywords(13) = "occupancy=                 "
        tonto%known_keywords(14) = "units=                     "
        tonto%known_keywords(15) = "z=                         "
        call unknown_(tonto,word,"SLATERSHELL:process_keyword")
        deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("SLATERSHELL:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    SLATERSHELL :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("SLATERSHELL:read_units")
      START_TIMER("SLATERSHELL:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("SLATERSHELL:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    SLATERSHELL :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("SLATERSHELL:read_junk")
      START_TIMER("SLATERSHELL:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("SLATERSHELL:read_junk")
      CHECK
   end subroutine

   subroutine read_l(self)
    SLATERSHELL :: self
   ! Read in the l symbol
   ! The following code is inherited from SHELL
      STR(STR_SIZE) :: word
      STACK("SLATERSHELL:read_l")
      START_TIMER("SLATERSHELL:read_l")
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if
     STOP_TIMER("SLATERSHELL:read_l")
      CHECK
   end subroutine

   subroutine read_l_int(self)
    SLATERSHELL :: self
   ! Read in the l integer
   ! The following code is inherited from SHELL
      STR(STR_SIZE) :: word
      STACK("SLATERSHELL:read_l_int")
      START_TIMER("SLATERSHELL:read_l_int")
      call read_(stdin,word)
      ENSURE(is_int_(word),"SLATERSHELL:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)
      call set_n_comp_(self)
     STOP_TIMER("SLATERSHELL:read_l_int")
      CHECK
   end subroutine

   subroutine read_l_chr(self)
    SLATERSHELL :: self
   ! Read in the l symbol
   ! The following code is inherited from SHELL
      STR(STR_SIZE) :: word
      STR(1) :: l_c
      INT :: l
      STACK("SLATERSHELL:read_l_chr")
      START_TIMER("SLATERSHELL:read_l_chr")
      call read_(stdin,word)
      ENSURE(len_trim(word)==1,"SLATERSHELL:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s");   l = 0
         case ("p");   l = 1
         case ("d");   l = 2
         case ("f");   l = 3
         case ("g");   l = 4
         case default; l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l
      call set_n_comp_(self)
     STOP_TIMER("SLATERSHELL:read_l_chr")
      CHECK
   end subroutine

   subroutine read_n_prim(self)
    SLATERSHELL :: self
   ! Read in the number of contraction coefficients
      STACK("SLATERSHELL:read_n_prim")
      START_TIMER("SLATERSHELL:read_n_prim")
      call read_(stdin,self%n_prim)
      ENSURE(self%n_prim>0,"SLATERSHELL:read_n_prim ... n_prim must be positive")
     STOP_TIMER("SLATERSHELL:read_n_prim")
      CHECK
   end subroutine

   subroutine read_n_orb(self)
    SLATERSHELL :: self
   ! Read in the number of generally contracted orbitals
      STACK("SLATERSHELL:read_n_orb")
      START_TIMER("SLATERSHELL:read_n_orb")
      call read_(stdin,self%n_orb)
      ENSURE(self%n_orb>0,"SLATERSHELL:read_n_orb ... n_orb must be positive")
     STOP_TIMER("SLATERSHELL:read_n_orb")
      CHECK
   end subroutine

   subroutine read_n(self)
    SLATERSHELL :: self
   ! Read in the "n" quantum numbers. NOTE: n_prim must already have been input.
   ENSURE(self%n_prim>0,"SLATERSHELL:read_n ... n_prim not set; use n_prim= before this command")
      STACK("SLATERSHELL:read_n")
      START_TIMER("SLATERSHELL:read_n")
      call destroy_(self%n)
      call create_(self%n,self%n_prim)
      call read_(stdin,self%n)
     STOP_TIMER("SLATERSHELL:read_n")
      UNSTACK
   end subroutine

   subroutine read_c(self)
    SLATERSHELL :: self
   ! Read in the "c" contraction coefficients. NOTE: n_orb must already have
   ! been input.
   ENSURE(self%n_prim>0,"SLATERSHELL:read_c ... n_prim not set; use n_prim= before this command")
   ENSURE(self%n_orb>0,"SLATERSHELL:read_c ... n_orb not set; use n_orb= before this command")
      STACK("SLATERSHELL:read_c")
      START_TIMER("SLATERSHELL:read_c")
      call destroy_(self%c)
      call create_(self%c,self%n_prim,self%n_orb)
      call read_(stdin,self%c)
     STOP_TIMER("SLATERSHELL:read_c")
      UNSTACK
   end subroutine

   subroutine read_z(self)
    SLATERSHELL :: self
   ! Read in the "z" slater function exponents. NOTE: n_prim must already have
   ! been input.
   ENSURE(self%n_prim>0,"SLATERSHELL:read_z ... n_prim not set; use n_prim= before this command")
      STACK("SLATERSHELL:read_z")
      START_TIMER("SLATERSHELL:read_z")
      call destroy_(self%z)
      call create_(self%z,self%n_prim)
      call read_(stdin,self%z)
     STOP_TIMER("SLATERSHELL:read_z")
      UNSTACK
   end subroutine

   subroutine read_kind(self)
    SLATERSHELL :: self
   ! Read in the orbital kind ("1s", "2s", "2p" ....); also set zero
   ! occupancies, if kinds are not set
      STACK("SLATERSHELL:read_kind")
      START_TIMER("SLATERSHELL:read_kind")
      if (associated(self%orb_kind)) call destroy_(self%orb_kind)
      call read_ptr_(stdin,self%orb_kind)
      if (self%n_orb>0) then; ENSURE(self%n_orb==size(self%orb_kind),"SLATERSHELL:read_kind ... n_orb and kind length inconsistent")
      else;               self%n_orb = size(self%orb_kind)
      end if
      if (associated(self%occupancy)) then
         WARN_IF(self%n_orb/=size(self%occupancy),"SLATERSHELL:read_kind ... n_orb and occupancy length inconsistent")
      else
         call create_(self%occupancy,self%n_orb)
         self%occupancy = 0
      end if
     STOP_TIMER("SLATERSHELL:read_kind")
      UNSTACK
   end subroutine

   subroutine read_occupancy(self)
    SLATERSHELL :: self
   ! Read in the occupancies; also set blank kinds, if kinds are not set.
      STACK("SLATERSHELL:read_occupancy")
      START_TIMER("SLATERSHELL:read_occupancy")
      if (associated(self%occupancy)) call destroy_(self%occupancy)
      call read_ptr_(stdin,self%occupancy)
      if (self%n_orb>0) then; ENSURE(self%n_orb==size(self%orb_kind),"SLATERSHELL:read_occupancy ... n_orb and kind length inconsistent")
      else;               self%n_orb = size(self%orb_kind)
      end if
      if (associated(self%orb_kind)) then
         WARN_IF(self%n_orb/=size(self%orb_kind),"SLATERSHELL:read_occupancy ... n_orb and kind length inconsistent")
      else
         call create_(self%orb_kind,self%n_orb)
         self%orb_kind = " "
      end if
     STOP_TIMER("SLATERSHELL:read_occupancy")
      UNSTACK
   end subroutine

   subroutine read_l_kind_n_z_c_ptr(self)
    SLATERSHELL :: self
   ! Read in everything: l, kind, and then n,z,c in a table, as in an Ajit
   ! Thakkar table
      STACK("SLATERSHELL:read_l_kind_n_z_c_ptr")
      START_TIMER("SLATERSHELL:read_l_kind_n_z_c_ptr")
      call read_l_chr_(self)
      call read_kind_(self)
      call read_n_z_c_ptr_(self)
     STOP_TIMER("SLATERSHELL:read_l_kind_n_z_c_ptr")
      UNSTACK
   end subroutine

   subroutine read_n_z_c_ptr(self)
    SLATERSHELL :: self
   ! Read in the "n", "z" and then "c" coefficients, across a line.
      REALVEC(:), PTR :: tmp
      INT :: i,k
   ENSURE(self%n_orb>0,"SLATERSHELL:read_n_z_c_ptr ... n_orb not set; use n_orb= before this command")
   STACK("SLATERSHELL:read_n_z_c_ptr")
   START_TIMER("SLATERSHELL:read_n_z_c_ptr")
   ENSURE(self%n_prim==0,"SLATERSHELL:read_n_z_c_ptr ... n_prim already defined!")
      call read_ptr_(stdin,tmp)
      ENSURE(mod(size(tmp),(self%n_orb+2))==0,"SLATERSHELL:read_n_z_c_ptr ... number of data not divisible by n_orb")
      self%n_prim =  size(tmp)/(self%n_orb+2)
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_prim)
      call create_(self%z,self%n_prim)
      call create_(self%c,self%n_prim,self%n_orb)
      k = 0
      do i = 1,self%n_prim
         ENSURE(is_int_(tmp(k+1)),"SLATERSHELL:read_n_z_c_ptr ... n number is not integer")
         self%n(i)   = tmp(k+1)
         self%z(i)   = tmp(k+2)
         self%c(i,:) = tmp(k+2+1:k+2+self%n_orb)
         k = k + 2 + self%n_orb 
      end do
      call destroy_(tmp)
     STOP_TIMER("SLATERSHELL:read_n_z_c_ptr")
      UNSTACK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    SLATERSHELL :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("SLATERSHELL:read_keys")
     START_TIMER("SLATERSHELL:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("SLATERSHELL:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    SLATERSHELL :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("SLATERSHELL:process_keys")
      START_TIMER("SLATERSHELL:process_keys")
      ENSURE(associated(keys),"SLATERSHELL:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"SLATERSHELL:process_keys ... no matching closing brace, }")
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
     STOP_TIMER("SLATERSHELL:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    SLATERSHELL :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("SLATERSHELL:keys_created")
      START_TIMER("SLATERSHELL:keys_created")
      res = associated(keys)
     STOP_TIMER("SLATERSHELL:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    SLATERSHELL :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("SLATERSHELL:set_keys")
     START_TIMER("SLATERSHELL:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("SLATERSHELL:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    SLATERSHELL :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("SLATERSHELL:clear_keys")
     START_TIMER("SLATERSHELL:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("SLATERSHELL:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    SLATERSHELL :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("SLATERSHELL:put_table_footer")
     START_TIMER("SLATERSHELL:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("SLATERSHELL:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    SLATERSHELL :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
     STACK("SLATERSHELL:put_table_header")
     START_TIMER("SLATERSHELL:put_table_header")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush            "); call flush_(stdout); exit
           case ("put_l            "); call put_(stdout,"l",int_width=TRUE)
           case ("put_n_prim       "); call put_(stdout,"n_prim",int_width=TRUE)
           case ("put_n_orb        "); call put_(stdout,"n_orb",int_width=TRUE)
           case ("put_configuration"); call put_(stdout,"Config.")
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("SLATERSHELL:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("SLATERSHELL:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    SLATERSHELL :: self
   ! Return how wide a table is, based on "keys".  Note that not all keywords
   ! need to contribute to the banner - any unrecognised keyword is skipped.
     INT :: res
     STR(STR_SIZE) :: word
     INT :: int_dash,real_dash,k
     STACK("SLATERSHELL:table_width")
     START_TIMER("SLATERSHELL:table_width")
     int_dash = 0
     real_dash = 0
     ENSURE(associated(keys),"SLATERSHELL:table_width ... no keywords")
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("put_l            "); int_dash = int_dash + 1
         case ("put_n_prim       "); int_dash = int_dash + 1
         case ("put_n_orb        "); int_dash = int_dash + 1
         case ("put_configuration"); real_dash = real_dash + 1
         case ("flush            "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("SLATERSHELL:table_width")
      CHECK
   end function

! *******
! Methods
! *******

   function same_as(self,b) result(res)
    SLATERSHELL :: self
   ! Return TRUE if "self" is the same as "b".
      IN :: self
      SLATERSHELL, IN :: b
      BIN :: res
   STACK("SLATERSHELL:same_as")
   START_TIMER("SLATERSHELL:same_as")
   ENSURE(associated(self%n),"SLATERSHELL:same_as ... no n quantum numbers")
   ENSURE(associated(self%c),"SLATERSHELL:same_as ... no contraction coefficients")
   ENSURE(associated(self%z),"SLATERSHELL:same_as ... no exponents")
   ENSURE(associated(b%n),"SLATERSHELL:same_as ... no n quantum numbers to compare to")
   ENSURE(associated(b%c),"SLATERSHELL:same_as ... no contraction coefficients to compare to")
   ENSURE(associated(b%z),"SLATERSHELL:same_as ... no exponents to compare to")
      res = self%l == b%l &
            AND same_as_(self%n,b%n) &
            AND same_as_(self%c,b%c) &
            AND same_as_(self%z,b%z)
      if (res AND associated(self%orb_kind))  res = all(self%orb_kind==b%orb_kind)
      if (res AND associated(self%occupancy)) res = all(self%occupancy==b%occupancy)
     STOP_TIMER("SLATERSHELL:same_as")
      CHECK
   end function

   function l_chr(self) result(res)
    SLATERSHELL :: self
   ! Return a character representation for the angular mtm
      STR(1) :: res
   ! The following code is inherited from SHELL
      INT :: l
      STACK("SLATERSHELL:l_chr")
      START_TIMER("SLATERSHELL:l_chr")
      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            DIE_IF(l>23,"SLATERSHELL:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select
     STOP_TIMER("SLATERSHELL:l_chr")
      CHECK
   end function

   PURE function no_of_basis_functions(self) result(res)
    SLATERSHELL :: self
   ! Work out and return the TOTAL number of generally contracted basis
   ! functions , counting the agular part also.
      IN :: self
      INT :: res
      res = self%n_comp*self%n_orb
     STOP_TIMER("SLATERSHELL:no_of_basis_functions")
   end function

   PURE function no_of_primitives(self) result(res)
    SLATERSHELL :: self
   ! Work out and return the TOTAL number of primitives, counting the angular
   ! part also.
      IN :: self
      INT :: res
      res = self%n_comp*self%n_prim
     STOP_TIMER("SLATERSHELL:no_of_primitives")
   end function

   function density_value_at_radius(self,R) result(res)
    SLATERSHELL :: self
   ! Returns the value of the coppens orbital at radius "R".
   ! SOme work can be save if the prefactor array is made.
      REAL, IN :: R
      REAL :: res
      INT :: i
      REAL :: orb
      REALVEC(:), PTR :: val
      STACK("SLATERSHELL:density_value_at_radius")
      START_TIMER("SLATERSHELL:density_value_at_radius")
      call create_(val,self%n_prim)
      val = R**(self%n-1) * exp(-self%z*R) ! save this
      res = ZERO
      do i = 1,self%n_orb
         orb = sum(self%c(:,i) * val)
         res = res + self%occupancy(i)*orb*orb
      end do
      call destroy_(val)
     STOP_TIMER("SLATERSHELL:density_value_at_radius")
      CHECK
   end function

   function density_values_at_radii(self,R) result(res)
    SLATERSHELL :: self
   ! Returns the values of the coppens density at all the radii "R".
      REALVEC(:), IN :: R
      REALVEC(size(R)) :: res
      INT :: i,p
      REAL :: orb
      REALVEC(:), PTR :: val
      STACK("SLATERSHELL:density_values_at_radii")
      START_TIMER("SLATERSHELL:density_values_at_radii")
      call create_(val,self%n_prim)
      do p = 1,size(R)
         val = R(p)**(self%n-1) * exp(-self%z*R(p)) ! save this
         res(p) = ZERO
         do i = 1,self%n_orb
            orb = sum(self%c(:,i) * val)
            res(p) = res(p) + self%occupancy(i)*orb*orb
         end do
      end do
      call destroy_(val)
     STOP_TIMER("SLATERSHELL:density_values_at_radii")
      CHECK
   end function

   function density_values_at_points(self,pt,pos) result(res)
    SLATERSHELL :: self
   ! Make the orbital density values on the series of points specified in "pt",
   ! assuming the orbital is at point "pos"; and put the results in "res".
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
      REALVEC(size(pt,1)) :: res
      REALVEC(:), PTR :: R
      REAL :: x,y,z
      INT :: n,n_pt
   STACK("SLATERSHELL:density_values_at_points")
   START_TIMER("SLATERSHELL:density_values_at_points")
   ENSURE(size(pt,2)==3,"SLATERSHELL:density_values_at_points ... incorrect second dimension, pt array")
      n_pt = size(pt,1)
      call create_(R,n_pt)
      do n = 1,n_pt
         x = pt(n,1) - pos(1)
         y = pt(n,2) - pos(2)
         z = pt(n,3) - pos(3)
         R(n) = sqrt(x*x + y*y + z*z)
      end do
      res = density_values_at_radii_(self,R)
      call destroy_(R)
     STOP_TIMER("SLATERSHELL:density_values_at_points")
      CHECK
   end function

   subroutine put(self)
    SLATERSHELL :: self
   ! Put the orbital information to "stdout"
      INT :: i
      STACK("SLATERSHELL:put")
      START_TIMER("SLATERSHELL:put")
      call flush_(stdout)
      call show_(stdout,"L quantum no.     = ",self%l)
      call show_(stdout,"No. of orbitals   = ",self%n_orb)
      call show_(stdout,"No. of primitives = ",self%n_prim)
      call put_table_(self)
     STOP_TIMER("SLATERSHELL:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    SLATERSHELL :: self
   ! Put the orbital information to "stdout" in table format
      INT :: i,j
      STACK("SLATERSHELL:put_table")
      START_TIMER("SLATERSHELL:put_table")
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=self%n_orb+1)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"N",int_width=TRUE)
      call put_(stdout,"Exponent")
      if (associated(self%orb_kind) AND associated(self%occupancy)) then
         do j = 1,self%n_orb
            call put_(stdout,trim(self%orb_kind(j))//"("//trim(to_str_(self%occupancy(j)))//")")
         end do
      else if (associated(self%orb_kind)) then
         do j = 1,self%n_orb
            call put_(stdout,trim(self%orb_kind(j)))
         end do
      else
         do j = 1,self%n_orb
            call put_(stdout,"Orb. "//trim(to_str_(j)))
         end do
      end if
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=self%n_orb+1)
      do i = 1,self%n_prim
         call put_(stdout,i)
         call put_(stdout,self%n(i))
         call put_(stdout,self%z(i))
         do j = 1,self%n_orb
            call put_(stdout,self%c(i,j))
         end do
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=2,real_fields=self%n_orb+1)
     STOP_TIMER("SLATERSHELL:put_table")
      CHECK
   end subroutine

end
