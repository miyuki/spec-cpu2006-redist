!-------------------------------------------------------------------------------
!
! REFLECTION: Reflection data for crystals
!
! Copyright (C) Daniel Grimwood, 2000
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
! $Id: reflection.foo,v 1.3.2.2 2003/11/13 05:36:07 reaper Exp $
!-------------------------------------------------------------------------------

module REFLECTION_MODULE

#  include "reflection.use"

   implicit none

#  include "macros"
#  include "reflection.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

contains

!*******************************************************************************
!                        Create and Destroy Routines
!*******************************************************************************

   subroutine create(self)
    REFLECTION :: self
   ! Create the object
      PTR :: self
      STACK("REFLECTION:create")
      START_TIMER("REFLECTION:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(REFLECTION_SIZE)
      call set_defaults_(self)
     STOP_TIMER("REFLECTION:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REFLECTION :: self
   ! Destroy the object
      PTR :: self
      STACK("REFLECTION:destroy")
      START_TIMER("REFLECTION:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REFLECTION:destroy") UNSTACK return; end if
      DELETE_MEMORY(REFLECTION_SIZE)
      deallocate(self)
     STOP_TIMER("REFLECTION:destroy")
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

   subroutine create_copy(self,r)
    REFLECTION :: self
   ! Create a copy of the reflection "r"
      PTR :: self
      REFLECTION :: r
      STACK("REFLECTION:create_copy")
      START_TIMER("REFLECTION:create_copy")
      call create_(self)
      call copy_(self,r)
     STOP_TIMER("REFLECTION:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,r)
    REFLECTION :: self
   ! Make a copy of the reflection "r"
      REFLECTION :: r
      STACK("REFLECTION:copy")
      START_TIMER("REFLECTION:copy")
      self = r
     STOP_TIMER("REFLECTION:copy")
      CHECK
   end subroutine

   subroutine set_defaults(self)
    REFLECTION :: self
   ! Set up a default object
     STACK("REFLECTION:set_defaults")
     START_TIMER("REFLECTION:set_defaults")
     self%h = 0
     self%k = 0
     self%l = 0
     self%F_exp = ZERO
     self%F_calc = ZERO
     self%F_pred = ZERO
     self%F_sigma = ZERO
     self%I_exp = ZERO
     self%I_pred = ZERO
     self%I_sigma = ZERO
     STOP_TIMER("REFLECTION:set_defaults")
      CHECK
   end subroutine


!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    REFLECTION :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("REFLECTION:read_keywords")
     START_TIMER("REFLECTION:read_keywords")
     ENSURE(next_item_(stdin)=="{","REFLECTION:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("REFLECTION:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    REFLECTION :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("REFLECTION:process_keyword")
     START_TIMER("REFLECTION:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}           "); ! exit surrounding loop
       case ("add_f_calc= "); call add_F_calc_(self)
       case ("add_f_exp=  "); call add_F_exp_(self)
       case ("add_f_pred= "); call add_F_pred_(self)
       case ("add_f_sigma="); call add_F_sigma_(self)
       case ("add_i_exp=  "); call add_I_exp_(self)
       case ("add_i_pred= "); call add_I_pred_(self)
       case ("add_i_sigma="); call add_I_sigma_(self)
       case ("f_calc=     "); call read_F_calc_(self)
       case ("f_exp=      "); call read_F_exp_(self)
       case ("f_pred=     "); call read_F_pred_(self)
       case ("f_sigma=    "); call read_F_sigma_(self)
       case ("h=          "); call read_h_(self)
       case ("indices=    "); call read_indices_(self)
       case ("i_exp=      "); call read_I_exp_(self)
       case ("i_pred=     "); call read_I_pred_(self)
       case ("i_sigma=    "); call read_I_sigma_(self)
       case ("junk=       "); call read_junk_(self)
       case ("k=          "); call read_k_(self)
       case ("l=          "); call read_l_(self)
       case ("units=      "); call read_units_(self)
       ! These are only for making custom tables for the list type
       case ("flush       "); call flush_(stdout)
       case ("put_f_calc  "); call put_(stdout,self%F_calc)
       case ("put_f_exp   "); call put_(stdout,self%F_exp)
       case ("put_f_phase "); call put_(stdout,F_phase_(self))
       case ("put_f_pred  "); call put_(stdout,self%F_pred)
       case ("put_f_sigma "); call put_(stdout,self%F_sigma)
       case ("put_f_z     "); call put_(stdout,F_z_(self))
       case ("put_f_z2    "); call put_(stdout,F_z2_(self))
       case ("put_h       "); call put_(stdout,self%h)
       case ("put_i_exp   "); call put_(stdout,self%I_exp)
       case ("put_i_pred  "); call put_(stdout,self%I_pred)
       case ("put_i_sigma "); call put_(stdout,self%I_sigma)
       case ("put_i_z     "); call put_(stdout,I_z_(self))
       case ("put_indices "); call put_(stdout,self%h); call put_(stdout,self%k); call put_(stdout,self%l)
       case ("put_k       "); call put_(stdout,self%k)
       case ("put_l       "); call put_(stdout,self%l)
       case default;         allocate(tonto%known_keywords(37))
       tonto%known_keywords(1) = "}           "
       tonto%known_keywords(2) = "add_f_calc= "
       tonto%known_keywords(3) = "add_f_exp=  "
       tonto%known_keywords(4) = "add_f_pred= "
       tonto%known_keywords(5) = "add_f_sigma="
       tonto%known_keywords(6) = "add_i_exp=  "
       tonto%known_keywords(7) = "add_i_pred= "
       tonto%known_keywords(8) = "add_i_sigma="
       tonto%known_keywords(9) = "f_calc=     "
       tonto%known_keywords(10) = "f_exp=      "
       tonto%known_keywords(11) = "f_pred=     "
       tonto%known_keywords(12) = "f_sigma=    "
       tonto%known_keywords(13) = "h=          "
       tonto%known_keywords(14) = "indices=    "
       tonto%known_keywords(15) = "i_exp=      "
       tonto%known_keywords(16) = "i_pred=     "
       tonto%known_keywords(17) = "i_sigma=    "
       tonto%known_keywords(18) = "junk=       "
       tonto%known_keywords(19) = "k=          "
       tonto%known_keywords(20) = "l=          "
       tonto%known_keywords(21) = "units=      "
       tonto%known_keywords(22) = "flush       "
       tonto%known_keywords(23) = "put_f_calc  "
       tonto%known_keywords(24) = "put_f_exp   "
       tonto%known_keywords(25) = "put_f_phase "
       tonto%known_keywords(26) = "put_f_pred  "
       tonto%known_keywords(27) = "put_f_sigma "
       tonto%known_keywords(28) = "put_f_z     "
       tonto%known_keywords(29) = "put_f_z2    "
       tonto%known_keywords(30) = "put_h       "
       tonto%known_keywords(31) = "put_i_exp   "
       tonto%known_keywords(32) = "put_i_pred  "
       tonto%known_keywords(33) = "put_i_sigma "
       tonto%known_keywords(34) = "put_i_z     "
       tonto%known_keywords(35) = "put_indices "
       tonto%known_keywords(36) = "put_k       "
       tonto%known_keywords(37) = "put_l       "
       call unknown_(tonto,word,"REFLECTION:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("REFLECTION:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    REFLECTION :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("REFLECTION:read_units")
      START_TIMER("REFLECTION:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("REFLECTION:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    REFLECTION :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("REFLECTION:read_junk")
      START_TIMER("REFLECTION:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("REFLECTION:read_junk")
      CHECK
   end subroutine

   subroutine read_h(self)
    REFLECTION :: self
   ! Read in the "h" Miller index
      STACK("REFLECTION:read_h")
      START_TIMER("REFLECTION:read_h")
      call read_int_(stdin,self%h)
     STOP_TIMER("REFLECTION:read_h")
      CHECK
   end subroutine

   subroutine read_k(self)
    REFLECTION :: self
   ! Read in the "k" Miller index
      STACK("REFLECTION:read_k")
      START_TIMER("REFLECTION:read_k")
      call read_int_(stdin,self%k)
     STOP_TIMER("REFLECTION:read_k")
      CHECK
   end subroutine

   subroutine read_l(self)
    REFLECTION :: self
   ! Read in the "l" Miller index
      STACK("REFLECTION:read_l")
      START_TIMER("REFLECTION:read_l")
      call read_int_(stdin,self%l)
     STOP_TIMER("REFLECTION:read_l")
      CHECK
   end subroutine

   subroutine read_indices(self)
    REFLECTION :: self
   ! Read in the h k l Miller indices as a triple
      STACK("REFLECTION:read_indices")
      START_TIMER("REFLECTION:read_indices")
      call read_int_(stdin,self%h)
      call read_int_(stdin,self%k)
      call read_int_(stdin,self%l)
     STOP_TIMER("REFLECTION:read_indices")
      CHECK
   end subroutine

   subroutine read_F_exp(self)
    REFLECTION :: self
   ! Read in the experimentally determined structure factor
      STACK("REFLECTION:read_F_exp")
      START_TIMER("REFLECTION:read_F_exp")
      call read_(stdin,self%F_exp)
     STOP_TIMER("REFLECTION:read_F_exp")
      CHECK
   end subroutine

   subroutine add_F_exp(self)
    REFLECTION :: self
   ! Read in an increment to add to .F_exp
      REAL :: tmp
      STACK("REFLECTION:add_F_exp")
      START_TIMER("REFLECTION:add_F_exp")
      call read_(stdin,tmp)
      self%F_exp = self%F_exp + tmp
     STOP_TIMER("REFLECTION:add_F_exp")
      CHECK
   end subroutine

   subroutine read_F_calc(self)
    REFLECTION :: self
   ! Read in a calculated (complex) structure factor
      STACK("REFLECTION:read_F_calc")
      START_TIMER("REFLECTION:read_F_calc")
      call read_(stdin,self%F_calc)
     STOP_TIMER("REFLECTION:read_F_calc")
      CHECK
   end subroutine

   subroutine add_F_calc(self)
    REFLECTION :: self
   ! Read in a *complex* increment to add to .F_calc
      CPX :: tmp
      STACK("REFLECTION:add_F_calc")
      START_TIMER("REFLECTION:add_F_calc")
      call read_(stdin,tmp)
      self%F_calc = self%F_calc + tmp
     STOP_TIMER("REFLECTION:add_F_calc")
      CHECK
   end subroutine

   subroutine read_F_pred(self)
    REFLECTION :: self
   ! Read in a predicted (real) structure factor
      STACK("REFLECTION:read_F_pred")
      START_TIMER("REFLECTION:read_F_pred")
      call read_(stdin,self%F_pred)
     STOP_TIMER("REFLECTION:read_F_pred")
      CHECK
   end subroutine

   subroutine add_F_pred(self)
    REFLECTION :: self
   ! Read in an increment to add to .F_pred
      REAL :: tmp
      STACK("REFLECTION:add_F_pred")
      START_TIMER("REFLECTION:add_F_pred")
      call read_(stdin,tmp)
      self%F_pred = self%F_pred + tmp
     STOP_TIMER("REFLECTION:add_F_pred")
      CHECK
   end subroutine

   subroutine read_F_sigma(self)
    REFLECTION :: self
   ! Read in the experimentally determined standard devaiation in
   ! the structure factor
      STACK("REFLECTION:read_F_sigma")
      START_TIMER("REFLECTION:read_F_sigma")
      call read_(stdin,self%F_sigma)
     STOP_TIMER("REFLECTION:read_F_sigma")
      CHECK
   end subroutine

   subroutine add_F_sigma(self)
    REFLECTION :: self
   ! Read in an increment to add to .F_sigma
      REAL :: tmp
      STACK("REFLECTION:add_F_sigma")
      START_TIMER("REFLECTION:add_F_sigma")
      call read_(stdin,tmp)
      self%F_sigma = self%F_sigma + tmp
     STOP_TIMER("REFLECTION:add_F_sigma")
      CHECK
   end subroutine

   subroutine read_I_exp(self)
    REFLECTION :: self
   ! Read in the experimentally determined intensity
      STACK("REFLECTION:read_I_exp")
      START_TIMER("REFLECTION:read_I_exp")
      call read_(stdin,self%I_exp)
     STOP_TIMER("REFLECTION:read_I_exp")
      CHECK
   end subroutine

   subroutine add_I_exp(self)
    REFLECTION :: self
   ! Read in an increment to add to .I_exp
      REAL :: tmp
      STACK("REFLECTION:add_I_exp")
      START_TIMER("REFLECTION:add_I_exp")
      call read_(stdin,tmp)
      self%I_exp = self%I_exp + tmp
     STOP_TIMER("REFLECTION:add_I_exp")
      CHECK
   end subroutine

   subroutine read_I_pred(self)
    REFLECTION :: self
   ! Read in a predicted intensity
      STACK("REFLECTION:read_I_pred")
      START_TIMER("REFLECTION:read_I_pred")
      call read_(stdin,self%I_pred)
     STOP_TIMER("REFLECTION:read_I_pred")
      CHECK
   end subroutine

   subroutine add_I_pred(self)
    REFLECTION :: self
   ! Read in an increment to add to .I_pred
      REAL :: tmp
      STACK("REFLECTION:add_I_pred")
      START_TIMER("REFLECTION:add_I_pred")
      call read_(stdin,tmp)
      self%I_pred = self%I_pred + tmp
     STOP_TIMER("REFLECTION:add_I_pred")
      CHECK
   end subroutine

   subroutine read_I_sigma(self)
    REFLECTION :: self
   ! Read in an experimentally determined standard deviation in the
   ! observed intensity
      STACK("REFLECTION:read_I_sigma")
      START_TIMER("REFLECTION:read_I_sigma")
      call read_(stdin,self%I_sigma)
     STOP_TIMER("REFLECTION:read_I_sigma")
      CHECK
   end subroutine

   subroutine add_I_sigma(self)
    REFLECTION :: self
   ! Read in an increment to add to .I_sigma
      REAL :: tmp
      STACK("REFLECTION:add_I_sigma")
      START_TIMER("REFLECTION:add_I_sigma")
      call read_(stdin,tmp)
      self%I_sigma = self%I_sigma + tmp
     STOP_TIMER("REFLECTION:add_I_sigma")
      CHECK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    REFLECTION :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("REFLECTION:read_keys")
     START_TIMER("REFLECTION:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("REFLECTION:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    REFLECTION :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("REFLECTION:process_keys")
      START_TIMER("REFLECTION:process_keys")
      ENSURE(associated(keys),"REFLECTION:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"REFLECTION:process_keys ... no matching closing brace, }")
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
     STOP_TIMER("REFLECTION:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    REFLECTION :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("REFLECTION:keys_created")
      START_TIMER("REFLECTION:keys_created")
      res = associated(keys)
     STOP_TIMER("REFLECTION:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    REFLECTION :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("REFLECTION:set_keys")
     START_TIMER("REFLECTION:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("REFLECTION:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    REFLECTION :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("REFLECTION:clear_keys")
     START_TIMER("REFLECTION:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("REFLECTION:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    REFLECTION :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("REFLECTION:put_table_footer")
     START_TIMER("REFLECTION:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("REFLECTION:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    REFLECTION :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
     STACK("REFLECTION:put_table_header")
     START_TIMER("REFLECTION:put_table_header")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush       "); call flush_(stdout); exit
           case ("put_indices "); call put_(stdout,"h",int_width=TRUE)
                                  call put_(stdout,"k",int_width=TRUE)
                                  call put_(stdout,"l",int_width=TRUE)
           case ("put_h       "); call put_(stdout,"h",int_width=TRUE)
           case ("put_k       "); call put_(stdout,"k",int_width=TRUE)
           case ("put_l       "); call put_(stdout,"l",int_width=TRUE)
           case ("put_f_calc  "); call put_(stdout,"F_calc",width=2*stdout%real_width)
           case ("put_f_exp   "); call put_(stdout,"F_exp")
           case ("put_f_pred  "); call put_(stdout,"F_pred")
           case ("put_f_sigma "); call put_(stdout,"F_sigma")
           case ("put_f_phase "); call put_(stdout,"F_phase")
           case ("put_f_z     "); call put_(stdout,"F_z")
           case ("put_f_z2    "); call put_(stdout,"F_z2")
           case ("put_i_exp   "); call put_(stdout,"I_exp")
           case ("put_i_pred  "); call put_(stdout,"I_pred")
           case ("put_i_sigma "); call put_(stdout,"I_sigma")
           case ("put_i_z     "); call put_(stdout,"I_z")
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("REFLECTION:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("REFLECTION:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    REFLECTION :: self
   ! Return the table width in characters, based on "keys".  Note that not all
   ! keywords need to contribute to the banner - if a keyword is not recognised,
   ! then it is skipped.
     INT :: res
     STR(STR_SIZE) :: word
     INT :: int_dash,real_dash,k
     STACK("REFLECTION:table_width")
     START_TIMER("REFLECTION:table_width")
     ENSURE(associated(keys),"REFLECTION:table_width ... no keywords")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("flush       "); exit
         case ("put_indices "); int_dash = int_dash + 3
         case ("put_h       "); int_dash = int_dash + 1
         case ("put_k       "); int_dash = int_dash + 1
         case ("put_l       "); int_dash = int_dash + 1
         case ("put_f_calc  "); real_dash = real_dash + 2
         case ("put_f_exp   "); real_dash = real_dash + 1
         case ("put_f_pred  "); real_dash = real_dash + 1
         case ("put_f_sigma "); real_dash = real_dash + 1
         case ("put_f_phase "); real_dash = real_dash + 1
         case ("put_f_z     "); real_dash = real_dash + 1
         case ("put_f_z2    "); real_dash = real_dash + 1
         case ("put_i_exp   "); real_dash = real_dash + 1
         case ("put_i_pred  "); real_dash = real_dash + 1
         case ("put_i_sigma "); real_dash = real_dash + 1
         case ("put_i_z     "); real_dash = real_dash + 1
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("REFLECTION:table_width")
      CHECK
   end function

!*******************************************************************************
!                           Enquiry Routines
!*******************************************************************************

   PURE function F_z(self) result(res)
    REFLECTION :: self
   ! Return the z statistic for the predicted structure factor.
     IN :: self
     REAL :: res
     res = sign(ONE,real(self%F_calc)) * (self%F_pred - self%F_exp) / self%F_sigma
     STOP_TIMER("REFLECTION:F_z")
   end function

   PURE function F_z2(self) result(res)
    REFLECTION :: self
   ! Return the z**2 statistic for the predicted structure factor.
     IN :: self
     REAL :: res
     REAL :: z
     z = (self%F_pred - self%F_exp) / self%F_sigma
     res = z*z
     STOP_TIMER("REFLECTION:F_z2")
   end function

   PURE function F_r(self) result(res)
    REFLECTION :: self
   ! Return the r factor for the predicted structure factor.
     IN :: self
     REAL :: res
     res = (self%F_pred - self%F_exp) / self%F_exp
     STOP_TIMER("REFLECTION:F_r")
   end function

   PURE function I_z(self) result(res)
    REFLECTION :: self
   ! Return the z statistic for the predicted intensity.
     IN :: self
     REAL :: res
     res = (self%I_pred - self%I_exp) / self%I_sigma
     STOP_TIMER("REFLECTION:I_z")
   end function

   PURE function F_phase(self) result(res)
    REFLECTION :: self
   ! Return the phase angle of the complex structure factor.
     IN :: self
     REAL :: res
     res = atan2(aimag(self%F_calc),real(self%F_calc))
     STOP_TIMER("REFLECTION:F_phase")
   end function
end
