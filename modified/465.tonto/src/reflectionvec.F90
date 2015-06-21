!---------------------------------------------------------------------------
!
!  REFLECTIONVEC: a vector of crystal reflection data
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
! $Id: reflectionvec.foo,v 1.11.2.3 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REFLECTIONVEC_MODULE

#  include "reflectionvec.use"

   implicit none

#  include "macros"
#  include "reflectionvec.int"


   REFLECTIONVEC(:), PTR, private :: saved_self DEFAULT_NULL

contains

!*******************************************************************************
!                           Create and Destroy Routines
!*******************************************************************************

   subroutine create(self,dim)
    REFLECTIONVEC(:) :: self
   ! Create space for a reflection vector
     PTR :: self
     INT, IN :: dim
     STACK("REFLECTIONVEC:create")
     START_TIMER("REFLECTIONVEC:create")
     nullify(self)
     allocate(self(dim))
     ADD_MEMORY(dim*REFLECTION_SIZE)
     call set_defaults_(self)
     STOP_TIMER("REFLECTIONVEC:create")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    REFLECTIONVEC(:) :: self
   ! Create a replica copy of "vec".
      REFLECTIONVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("REFLECTIONVEC:create_copy")
      START_TIMER("REFLECTIONVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("REFLECTIONVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    REFLECTIONVEC(:) :: self
   ! Copy "vec".
      REFLECTIONVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("REFLECTIONVEC:copy")
      START_TIMER("REFLECTIONVEC:copy")
      ENSURE(size(self)==size(vec),"REFLECTIONVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("REFLECTIONVEC:copy")
      CHECK
   end subroutine

   subroutine destroy(self)
    REFLECTIONVEC(:) :: self
   ! Destroy space for a reflection vector
     PTR :: self
     INT :: dim
     STACK("REFLECTIONVEC:destroy")
     START_TIMER("REFLECTIONVEC:destroy")
     if (NOT associated(self)) then; STOP_TIMER("REFLECTIONVEC:destroy") UNSTACK return; end if
     dim = size(self)
     DELETE_MEMORY(size(self)*REFLECTION_SIZE)
     deallocate(self)
     STOP_TIMER("REFLECTIONVEC:destroy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    REFLECTIONVEC(:) :: self
   ! Set defaults
      INT :: n
     STACK("REFLECTIONVEC:set_defaults")
     START_TIMER("REFLECTIONVEC:set_defaults")
     do n = 1,size(self)
       call set_defaults_(self(n))
     end do
     STOP_TIMER("REFLECTIONVEC:set_defaults")
      CHECK
   end subroutine

!*******************************************************************************
!                           Data changing routines
!*******************************************************************************

   subroutine set_indices(self,h,k,l)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     INTVEC(:), IN :: h,k,l
     REFLECTION, PTR :: ref
     INT :: n
     STACK("REFLECTIONVEC:set_indices")
     START_TIMER("REFLECTIONVEC:set_indices")
     do n=1,n_refl_(self)
       ref => self(n)
       ref%h       = h(n)
       ref%k       = k(n)
       ref%l       = l(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_indices")
      CHECK
   end subroutine

   subroutine set_F_calc(self,F_calc)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     CPXVEC(:), IN :: F_calc
      INT :: n
     STACK("REFLECTIONVEC:set_F_calc")
     START_TIMER("REFLECTIONVEC:set_F_calc")
     do n=1,n_refl_(self)
       self(n)%F_calc = F_calc(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_F_calc")
      CHECK
   end subroutine

   subroutine set_F_pred(self,F_pred)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     REALVEC(:), IN :: F_pred
      INT :: n
     STACK("REFLECTIONVEC:set_F_pred")
     START_TIMER("REFLECTIONVEC:set_F_pred")
     do n=1,n_refl_(self)
       self(n)%F_pred = F_pred(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_F_pred")
      CHECK
   end subroutine

   subroutine set_F_exp(self,F_exp)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     REALVEC(:), IN :: F_exp
      INT :: n
     STACK("REFLECTIONVEC:set_F_exp")
     START_TIMER("REFLECTIONVEC:set_F_exp")
     do n=1,n_refl_(self)
       self(n)%F_exp = F_exp(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_F_exp")
      CHECK
   end subroutine

   subroutine set_F_sigma(self,F_sigma)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     REALVEC(:), IN :: F_sigma
      INT :: n
     STACK("REFLECTIONVEC:set_F_sigma")
     START_TIMER("REFLECTIONVEC:set_F_sigma")
     do n=1,n_refl_(self)
       self(n)%F_sigma = F_sigma(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_F_sigma")
      CHECK
   end subroutine

   subroutine set_I_pred(self,I_pred)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     REALVEC(:), IN :: I_pred
      INT :: n
     STACK("REFLECTIONVEC:set_I_pred")
     START_TIMER("REFLECTIONVEC:set_I_pred")
     do n=1,n_refl_(self)
       self(n)%I_pred = I_pred(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_I_pred")
      CHECK
   end subroutine

   subroutine set_I_exp(self,I_exp)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     REALVEC(:), IN :: I_exp
      INT :: n
     STACK("REFLECTIONVEC:set_I_exp")
     START_TIMER("REFLECTIONVEC:set_I_exp")
     do n=1,n_refl_(self)
       self(n)%I_exp = I_exp(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_I_exp")
      CHECK
   end subroutine

   subroutine set_I_sigma(self,I_sigma)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from the vector.
     target :: self
     REALVEC(:), IN :: I_sigma
      INT :: n
     STACK("REFLECTIONVEC:set_I_sigma")
     START_TIMER("REFLECTIONVEC:set_I_sigma")
     do n=1,n_refl_(self)
       self(n)%I_sigma = I_sigma(n)
     end do
     STOP_TIMER("REFLECTIONVEC:set_I_sigma")
      CHECK
   end subroutine

   subroutine set(self,ref)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data from.
     target :: self
     REFLECTIONVEC(:), target :: ref
     REFLECTION, PTR :: ref1,self1
      INT :: n
     STACK("REFLECTIONVEC:set")
     START_TIMER("REFLECTIONVEC:set")
     do n=1,n_refl_(self)
       self1 => self(n)
       ref1  => ref(n)
       self1%h       = ref1%h
       self1%k       = ref1%k
       self1%l       = ref1%l
       self1%F_exp   = ref1%F_exp
       self1%F_pred  = ref1%F_pred
       self1%F_calc  = ref1%F_calc
       self1%F_sigma = ref1%F_sigma
       self1%I_exp   = ref1%I_exp
       self1%I_pred  = ref1%I_pred
       self1%I_sigma = ref1%I_sigma
     end do
     STOP_TIMER("REFLECTIONVEC:set")
      CHECK
   end subroutine

   subroutine scale_F_pred(self,fac)
    REFLECTIONVEC(:) :: self
   ! Scale the predicted structure factors
     INOUT :: self
     REAL, IN :: fac
      INT :: n
     STACK("REFLECTIONVEC:scale_F_pred")
     START_TIMER("REFLECTIONVEC:scale_F_pred")
     do n=1,size(self)
       self(n)%F_pred = self(n)%F_pred * fac
     end do
     STOP_TIMER("REFLECTIONVEC:scale_F_pred")
      CHECK
   end subroutine

   subroutine scale_F_calc(self,fac)
    REFLECTIONVEC(:) :: self
   ! Scale the calculated structure factors
     INOUT :: self
     REAL, IN :: fac
      INT :: n
     STACK("REFLECTIONVEC:scale_F_calc")
     START_TIMER("REFLECTIONVEC:scale_F_calc")
     do n=1,size(self)
       self(n)%F_calc = self(n)%F_calc * fac
     end do
     STOP_TIMER("REFLECTIONVEC:scale_F_calc")
      CHECK
   end subroutine

   subroutine scale_F_exp(self,fac)
    REFLECTIONVEC(:) :: self
   ! Scale the experimental structure factors
     INOUT :: self
     REAL, IN :: fac
      INT :: n
     STACK("REFLECTIONVEC:scale_F_exp")
     START_TIMER("REFLECTIONVEC:scale_F_exp")
     do n=1,size(self)
       self(n)%F_exp = self(n)%F_exp * fac
     end do
     STOP_TIMER("REFLECTIONVEC:scale_F_exp")
      CHECK
   end subroutine

   subroutine scale_F_sigma(self,fac)
    REFLECTIONVEC(:) :: self
   ! Scale the structure factor errors
     INOUT :: self
     REAL, IN :: fac
      INT :: n
     STACK("REFLECTIONVEC:scale_F_sigma")
     START_TIMER("REFLECTIONVEC:scale_F_sigma")
     do n=1,size(self)
       self(n)%F_sigma = self(n)%F_sigma * fac
     end do
     STOP_TIMER("REFLECTIONVEC:scale_F_sigma")
      CHECK
   end subroutine

!*******************************************************************************
!                             Enquiry Routines
!*******************************************************************************

!   created result(res)
!   ! Returns true if self has been created
!     self :: PTR
!     res :: BIN
!     res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!     self :: PTR
!     res :: BIN
!     res = NOT associated(self)
!   end

   PURE function n_refl(self) result(res)
    REFLECTIONVEC(:) :: self
   ! The number of reflections
     IN :: self
     INT :: res
     res = size(self)
     STOP_TIMER("REFLECTIONVEC:n_refl")
   end function

   PURE function have_F_calc(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any calculated structure factors
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%F_calc) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_F_calc")
   end function

   PURE function have_F_pred(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any predicted structure factors
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%F_pred) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_F_pred")
   end function

   PURE function have_F_exp(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any experimental structure factors
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%F_exp) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_F_exp")
   end function

   PURE function have_F_sigma(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any errors in the structure factors
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%F_sigma) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_F_sigma")
   end function

   PURE function have_I_pred(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any predicted intensities
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%I_pred) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_I_pred")
   end function

   PURE function have_I_exp(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any experimental intensities
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%I_exp) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_I_exp")
   end function

   PURE function have_I_sigma(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have any errors in the intensities
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (abs(self(n)%I_sigma) > TOL(10)) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_I_sigma")
   end function

   PURE function have_indices(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Whether we have the Miller indices
     IN :: self
     BIN :: res
      INT :: n
     res = FALSE
     do n=1,size(self)
       if (self(n)%h /= 0 OR self(n)%k /= 0 OR self(n)%l /= 0) then
         res = TRUE
         exit
       end if
     end do
     STOP_TIMER("REFLECTIONVEC:have_indices")
   end function

   PURE function indices(self,n) result(res)
    REFLECTIONVEC(:) :: self
   ! Return the miller indices of reflection n.
     IN :: self
      INT, IN :: n
     INTVEC(3) :: res
     res = (/ self(n)%h, self(n)%k, self(n)%l /)
     STOP_TIMER("REFLECTIONVEC:indices")
   end function

!*******************************************************************************
!                              Statistical Routines
!*******************************************************************************

   PURE function F_chi2(self) result(res)
    REFLECTIONVEC(:) :: self
   ! chi2 for the structure factors
     IN :: self
     REAL :: res
     REAL :: z
     INT :: n
     
     
     
     res = ZERO
     do n=1,size(self)
       z = F_z_(self(n))
       res = res + z*z
     end do
     res = res / max(n_refl_(self) - 1,1)
     STOP_TIMER("REFLECTIONVEC:F_chi2")
   end function

   PURE function I_chi2(self) result(res)
    REFLECTIONVEC(:) :: self
   ! chi2 for the intensities
     IN :: self
     REAL :: res
     REAL :: z
     INT :: n
     res = ZERO
     
     
     
     do n=1,size(self)
       z = I_z_(self(n))
       res = res + z*z
     end do
     res = res / max(n_refl_(self) - 1,1)
     STOP_TIMER("REFLECTIONVEC:I_chi2")
   end function

   PURE function F_goodness_of_fit(self) result(res)
    REFLECTIONVEC(:) :: self
   ! goodness_of_fit for the structure factors
     IN :: self
     REAL :: res
     res = sqrt(F_chi2_(self))
     STOP_TIMER("REFLECTIONVEC:F_goodness_of_fit")
   end function

   PURE function I_goodness_of_fit(self) result(res)
    REFLECTIONVEC(:) :: self
   ! goodness_of_fit for the intensities
     IN :: self
     REAL :: res
     res = sqrt(I_chi2_(self))
     STOP_TIMER("REFLECTIONVEC:I_goodness_of_fit")
   end function

   PURE function F_r_factor(self) result(res)
    REFLECTIONVEC(:) :: self
   ! r factor for the structure factors
     IN :: self
     REAL :: res
     REAL :: top,bot
     
     
     
     INT :: n
     top = ZERO
     bot = ZERO
     do n=1,size(self)
       top = top + abs(self(n)%F_pred - self(n)%F_exp)
       bot = bot + abs(self(n)%F_exp)
     end do
     res = top / bot
     STOP_TIMER("REFLECTIONVEC:F_r_factor")
   end function

   PURE function I_r_factor(self) result(res)
    REFLECTIONVEC(:) :: self
   ! r factor for the intensities
     IN :: self
     REAL :: res
     REAL :: top,bot
     INT :: n
     
     
     
     top = ZERO
     bot = ZERO
     do n=1,size(self)
       top = top + abs(self(n)%I_pred - self(n)%I_exp)
       bot = bot + abs(self(n)%I_exp)
     end do
     res = top / bot
     STOP_TIMER("REFLECTIONVEC:I_r_factor")
   end function

   PURE function F_weighted_r_factor(self) result(res)
    REFLECTIONVEC(:) :: self
   ! weighted r factor for the structure factors
     IN :: self
     REAL :: res
     REAL :: top,bot,z,b
     INT :: n
     
     
     
     top = ZERO
     bot = ZERO
     do n=1,size(self)
       z = F_z_(self(n))
       b = self(n)%F_exp / self(n)%F_sigma
       top = top + z*z
       bot = bot + b*b
     end do
     res = sqrt(top / bot)
     STOP_TIMER("REFLECTIONVEC:F_weighted_r_factor")
   end function

   PURE function I_weighted_r_factor(self) result(res)
    REFLECTIONVEC(:) :: self
   ! weighted r factor for the intensities
     IN :: self
     REAL :: res
     REAL :: top,bot,z,b
     INT :: n
     
     
     
     top = ZERO
     bot = ZERO
     do n=1,size(self)
       z = I_z_(self(n))
       b = self(n)%I_exp / self(n)%I_sigma
       top = top + z*z
       bot = bot + b*b
     end do
     res = sqrt(top / bot)
     STOP_TIMER("REFLECTIONVEC:I_weighted_r_factor")
   end function

   PURE function F_calc(self) result(res)
    REFLECTIONVEC(:) :: self
   ! return the calculated structure factors
     IN :: self
     CPXVEC(size(self)) :: res
     res(:) = self(:)%F_calc
     STOP_TIMER("REFLECTIONVEC:F_calc")
   end function

   PURE function F_pred(self) result(res)
    REFLECTIONVEC(:) :: self
   ! return the predicted structure factors
     IN :: self
     REALVEC(size(self)) :: res
     res(:) = self(:)%F_pred
     STOP_TIMER("REFLECTIONVEC:F_pred")
   end function

   PURE function F_exp(self) result(res)
    REFLECTIONVEC(:) :: self
   ! return the experimental structure factors
     IN :: self
     REALVEC(size(self)) :: res
     res(:) = self(:)%F_exp
     STOP_TIMER("REFLECTIONVEC:F_exp")
   end function

   PURE function I_pred(self) result(res)
    REFLECTIONVEC(:) :: self
   ! return the predicted intensities
     IN :: self
     REALVEC(size(self)) :: res
     res(:) = self(:)%I_pred
     STOP_TIMER("REFLECTIONVEC:I_pred")
   end function

   PURE function F_sigma(self) result(res)
    REFLECTIONVEC(:) :: self
   ! return the errors in the experimental structure factors
     IN :: self
     REALVEC(size(self)) :: res
     res(:) = self(:)%F_sigma
     STOP_TIMER("REFLECTIONVEC:F_sigma")
   end function

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    REFLECTIONVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     STACK("REFLECTIONVEC:read_list_keywords")
     START_TIMER("REFLECTIONVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","REFLECTIONVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("REFLECTIONVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    REFLECTIONVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("REFLECTIONVEC:process_list_keyword")
     START_TIMER("REFLECTIONVEC:process_list_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("altered_data= "); call read_altered_data_(self)
       case("append_data=  "); call read_append_data_(self)
       case("data=         "); call read_data_(self)
       case("do            "); call read_keywords_(self)
       case("keys=         "); call read_keys_(self)
       case("new_data=     "); call destroy_(self); call read_data_(self)
       case("process_keys  "); call process_keys_(self)
       case("put_keys_table"); call put_keys_table_(self)
       case("redirect      "); call redirect_(self)
       case("revert        "); call revert_(self)
       case default;           call move_to_previous_item_(stdin)
                               call read_data_(self,ignore_braces)
     end select
     STOP_TIMER("REFLECTIONVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    REFLECTIONVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("REFLECTIONVEC:read_data")
     START_TIMER("REFLECTIONVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","REFLECTIONVEC:read_data ... expecting open bracket symbol, {")
        call read_(stdin,word) ! move past open brace
     end if
     length = data_length_(self)
     if (associated(self)) then
        message = "No. of data items in new and old data lists do not match: " &
                  // "new = "//trim(to_str_(length))//", old = "//trim(to_str_(size(self)))
        ENSURE(length==size(self),message)
     else
        call create_(self,length)
     end if
     call process_keys_(self)
     if (NOT present(ignore_braces)) then
        call read_(stdin,word) ! read last brace
        ENSURE(word=="}","REFLECTIONVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("REFLECTIONVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    REFLECTIONVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
   ! The following code is inherited from OBJECTVEC
     REFLECTION, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("REFLECTIONVEC:data_length")
     START_TIMER("REFLECTIONVEC:data_length")
     ENSURE(next_item_(stdin)/="}","REFLECTIONVEC:data_length ... empty data list!")
     call read_(stdin,word)
     length = 0
     line = line_number_(stdin)
     item = previous_line_item_(stdin)
     do
       call move_to_previous_item_(stdin)
       call create_(tmp)
       call process_keys_(tmp)
       call destroy_(tmp)
       length = length + 1
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}") exit
       if (at_end_of_file_(stdin)) exit
     end do
     call move_to_line_(stdin,line)
     call move_to_line_item_(stdin,item)
     STOP_TIMER("REFLECTIONVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    REFLECTIONVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     INT :: s
     STACK("REFLECTIONVEC:read_altered_data")
     START_TIMER("REFLECTIONVEC:read_altered_data")
     ENSURE(associated(self),"REFLECTIONVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","REFLECTIONVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"REFLECTIONVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<=size(self),"REFLECTIONVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"REFLECTIONVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("REFLECTIONVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine read_append_data(self)
    REFLECTIONVEC(:) :: self
   ! Read in a set of data to append to an existing set.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
   STACK("REFLECTIONVEC:read_append_data")
   START_TIMER("REFLECTIONVEC:read_append_data")
   ENSURE(associated(self),"REFLECTIONVEC:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","REFLECTIONVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)
     STOP_TIMER("REFLECTIONVEC:read_append_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    REFLECTIONVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     REFLECTION :: tmp
     INT :: s
     STACK("REFLECTIONVEC:process_keys")
     START_TIMER("REFLECTIONVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("REFLECTIONVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    REFLECTIONVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
   ! The following code is inherited from OBJECTVEC
      REFLECTION :: tmp
      STACK("REFLECTIONVEC:keys_created")
      START_TIMER("REFLECTIONVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("REFLECTIONVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    REFLECTIONVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECTVEC
     REFLECTION :: tmp
     STACK("REFLECTIONVEC:set_keys")
     START_TIMER("REFLECTIONVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("REFLECTIONVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    REFLECTIONVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     REFLECTION :: tmp
     STACK("REFLECTIONVEC:clear_keys")
     START_TIMER("REFLECTIONVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("REFLECTIONVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    REFLECTIONVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      REFLECTION :: tmp
      STACK("REFLECTIONVEC:read_keys")
      START_TIMER("REFLECTIONVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("REFLECTIONVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    REFLECTIONVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STACK("REFLECTIONVEC:put_keys_table")
     START_TIMER("REFLECTIONVEC:put_keys_table")
     ENSURE(keys_created_(self),"REFLECTIONVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("REFLECTIONVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    REFLECTIONVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      REFLECTION :: tmp
      STACK("REFLECTIONVEC:put_table_header")
      START_TIMER("REFLECTIONVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("REFLECTIONVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    REFLECTIONVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      REFLECTION :: tmp
      STACK("REFLECTIONVEC:put_table_footer")
      START_TIMER("REFLECTIONVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("REFLECTIONVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    REFLECTIONVEC(:) :: self
   ! Redirect input
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("REFLECTIONVEC:redirect")
     START_TIMER("REFLECTIONVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("REFLECTIONVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    REFLECTIONVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("REFLECTIONVEC:revert")
     START_TIMER("REFLECTIONVEC:revert")
     call revert_(stdin)
     STOP_TIMER("REFLECTIONVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    REFLECTIONVEC(:) :: self
   ! Read in and process normal (non list-type) keywords from "stdin".
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("REFLECTIONVEC:read_keywords")
     START_TIMER("REFLECTIONVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","REFLECTIONVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("REFLECTIONVEC:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    REFLECTIONVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("REFLECTIONVEC:process_keyword")
     START_TIMER("REFLECTIONVEC:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}") ! do nothing.
       case("add_random_error         "); call add_random_error_(self)
       case("put                      "); call put_(self)
       case("put_f_exp_data           "); call put_F_exp_data_(self)
       case("put_f_pred_data          "); call put_F_pred_data_(self)
       case("put_f_stats              "); call put_F_stats_(self)
       case("put_i_exp_data           "); call put_I_exp_data_(self)
       case("put_i_stats              "); call put_I_stats_(self)
       case("put_intensity_data       "); call put_intensity_data_(self)
       case("put_structure_factor_data"); call put_structure_factor_data_(self)
       case("redirect                 "); call redirect_(self)
       case("revert                   "); call revert_(self)
       case("simulate_new_f_exp       "); call simulate_new_F_exp_(self)
       case default;     allocate(tonto%known_keywords(13))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "add_random_error         "
       tonto%known_keywords(3) = "put                      "
       tonto%known_keywords(4) = "put_f_exp_data           "
       tonto%known_keywords(5) = "put_f_pred_data          "
       tonto%known_keywords(6) = "put_f_stats              "
       tonto%known_keywords(7) = "put_i_exp_data           "
       tonto%known_keywords(8) = "put_i_stats              "
       tonto%known_keywords(9) = "put_intensity_data       "
       tonto%known_keywords(10) = "put_structure_factor_data"
       tonto%known_keywords(11) = "redirect                 "
       tonto%known_keywords(12) = "revert                   "
       tonto%known_keywords(13) = "simulate_new_f_exp       "
       call unknown_(tonto,word,"REFLECTIONVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("REFLECTIONVEC:process_keyword")
      CHECK
   end subroutine

   subroutine shrink(self,dim)
    REFLECTIONVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     REFLECTIONVEC(:), PTR :: old
     INT :: n
     STACK("REFLECTIONVEC:shrink")
     START_TIMER("REFLECTIONVEC:shrink")
     ENSURE(associated(self),"REFLECTIONVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"REFLECTIONVEC:shrink ... dim too large")
     ENSURE(dim>=0,"REFLECTIONVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then; STOP_TIMER("REFLECTIONVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)
     STOP_TIMER("REFLECTIONVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    REFLECTIONVEC(:) :: self
   ! Expand the vector "self" to "dim". New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     REFLECTIONVEC(:), PTR :: old
     INT :: old_dim
     STACK("REFLECTIONVEC:expand")
     START_TIMER("REFLECTIONVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"REFLECTIONVEC:expand ... dim not large enough")
        ENSURE(dim>=0,"REFLECTIONVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if
     STOP_TIMER("REFLECTIONVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    REFLECTIONVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     REFLECTIONVEC(:), IN :: v
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("REFLECTIONVEC:append")
     START_TIMER("REFLECTIONVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)
     STOP_TIMER("REFLECTIONVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    REFLECTIONVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     REFLECTION, IN :: value
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("REFLECTIONVEC:append_1")
     START_TIMER("REFLECTIONVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)
     STOP_TIMER("REFLECTIONVEC:append_1")
      UNSTACK
   end subroutine

!*******************************************************************************
!                                  Output routines
!*******************************************************************************

   subroutine put(self)
    REFLECTIONVEC(:) :: self
   ! Output the reflection data.
     PTR :: self
     STACK("REFLECTIONVEC:put")
     START_TIMER("REFLECTIONVEC:put")
     call flush_(stdout)
     call text_(stdout,"Reflection data:")
     call flush_(stdout)
     call show_(stdout,"Number of reflections is ",size(self))
     call flush_(stdout)
     if (have_F_pred_(self)) then
       if (have_F_exp_(self)) then
         call put_structure_factor_data_(self)
       else
         call put_F_pred_data_(self)
       end if
     else
       if (have_F_exp_(self)) call put_F_exp_data_(self)
     end if
     if (have_I_pred_(self)) then
       if (have_I_exp_(self)) then
         call put_intensity_data_(self)
       else
         call put_I_pred_data_(self)
       end if
     else
       if (have_I_exp_(self)) call put_I_exp_data_(self)
     end if
     STOP_TIMER("REFLECTIONVEC:put")
      CHECK
   end subroutine

   subroutine put_structure_factor_data(self)
    REFLECTIONVEC(:) :: self
   ! Output the structure factor data
     PTR :: self
     STACK("REFLECTIONVEC:put_structure_factor_data")
     START_TIMER("REFLECTIONVEC:put_structure_factor_data")
     call put_F_stats_(self)
     call set_keys_(self,(/"put_indices", &
                "put_F_calc ", &
                "put_F_pred ", &
                "put_F_exp  ", &
                "put_F_sigma", &
                "flush      "/) )
     call put_keys_table_(self)
     STOP_TIMER("REFLECTIONVEC:put_structure_factor_data")
      CHECK
   end subroutine

   subroutine put_F_pred_data(self)
    REFLECTIONVEC(:) :: self
   ! Output only the predicted structure factor data
     PTR :: self
     STACK("REFLECTIONVEC:put_F_pred_data")
     START_TIMER("REFLECTIONVEC:put_F_pred_data")
     call set_keys_(self,(/"put_indices", &
                "put_F_calc ", &
                "put_F_pred ", &
                "flush      "/) )
     call put_keys_table_(self)
     STOP_TIMER("REFLECTIONVEC:put_F_pred_data")
      CHECK
   end subroutine

   subroutine put_F_exp_data(self)
    REFLECTIONVEC(:) :: self
   ! Output only the experimental structure factor data
     PTR :: self
     STACK("REFLECTIONVEC:put_F_exp_data")
     START_TIMER("REFLECTIONVEC:put_F_exp_data")
     call set_keys_(self,(/"put_indices", &
                "put_F_exp  ", &
                "put_F_sigma", &
                "flush      "/) )
     call put_keys_table_(self)
     STOP_TIMER("REFLECTIONVEC:put_F_exp_data")
      CHECK
   end subroutine

   subroutine put_intensity_data(self)
    REFLECTIONVEC(:) :: self
   ! Output the structure factor data
     PTR :: self
     STACK("REFLECTIONVEC:put_intensity_data")
     START_TIMER("REFLECTIONVEC:put_intensity_data")
     call put_I_stats_(self)
     call set_keys_(self,(/"put_indices", &
                "put_I_pred ", &
                "put_I_exp  ", &
                "put_I_sigma", &
                "flush      "/) )
     call put_keys_table_(self)
     STOP_TIMER("REFLECTIONVEC:put_intensity_data")
      CHECK
   end subroutine

   subroutine put_I_pred_data(self)
    REFLECTIONVEC(:) :: self
   ! Output only the predicted structure factor data
     PTR :: self
     STACK("REFLECTIONVEC:put_I_pred_data")
     START_TIMER("REFLECTIONVEC:put_I_pred_data")
     call set_keys_(self,(/"put_indices", &
                "put_I_calc ", &
                "put_I_pred ", &
                "flush      "/) )
     call put_keys_table_(self)
     STOP_TIMER("REFLECTIONVEC:put_I_pred_data")
      CHECK
   end subroutine

   subroutine put_I_exp_data(self)
    REFLECTIONVEC(:) :: self
   ! Output only the experimental intensity data
     PTR :: self
     STACK("REFLECTIONVEC:put_I_exp_data")
     START_TIMER("REFLECTIONVEC:put_I_exp_data")
     call set_keys_(self,(/"put_indices", &
                "put_I_exp  ", &
                "put_I_sigma", &
                "flush      "/) )
     call put_keys_table_(self)
     STOP_TIMER("REFLECTIONVEC:put_I_exp_data")
      CHECK
   end subroutine

   subroutine put_F_stats(self)
    REFLECTIONVEC(:) :: self
   ! Output the structure factor goodness of fit statistics
     REAL :: chi2
     STACK("REFLECTIONVEC:put_F_stats")
     START_TIMER("REFLECTIONVEC:put_F_stats")
     call flush_(stdout)
     if (NOT have_F_pred_(self)) &
     call text_(stdout,"Note that the F_pred are all set to zero!")
     chi2 = F_chi2_(self)
     call text_(stdout,"Goodness of fit parameters based on Structure Factors:",flush=2)
     call show_(stdout,"R factor                    =", F_r_factor_(self))
     call show_(stdout,"Weighted R factor           =", F_weighted_r_factor_(self))
     call show_(stdout,"chi**2                      =", chi2)
     call show_(stdout,"Goodness of fit             =", sqrt(chi2))
     STOP_TIMER("REFLECTIONVEC:put_F_stats")
      CHECK
   end subroutine

   subroutine put_I_stats(self)
    REFLECTIONVEC(:) :: self
   ! Output the intensity goodness of fit statistics
     REAL :: chi2
     STACK("REFLECTIONVEC:put_I_stats")
     START_TIMER("REFLECTIONVEC:put_I_stats")
     chi2 = I_chi2_(self)
     call flush_(stdout)
     call text_(stdout,"Goodness of fit parameters based on Intensities:",flush=2)
     call show_(stdout,"R factor                    =", I_r_factor_(self))
     call show_(stdout,"Weighted R factor           =", I_weighted_r_factor_(self))
     call show_(stdout,"chi**2                      =", chi2)
     call show_(stdout,"Goodness of fit             =", sqrt(chi2))
     STOP_TIMER("REFLECTIONVEC:put_I_stats")
      CHECK
   end subroutine

!*******************************************************************************
!                                    QQ plots
!*******************************************************************************

   subroutine put_F_qq_plot(self,name)
    REFLECTIONVEC(:) :: self
   ! Output a qq plot to the text file.
   ! It is a plot of the experimental quantile vs expected quantile.
     STR(STR_SIZE), optional :: name
     ARCHIVE :: arch
     REALMAT(:,:), PTR :: grid
     STACK("REFLECTIONVEC:put_F_qq_plot")
     START_TIMER("REFLECTIONVEC:put_F_qq_plot")
     call create_(grid,n_refl_(self),2)
     call make_F_qq_plot_grid_(self,grid)
     call set_(arch,root_name=name,name="qq_plot",format="ascii")
     call write_(arch,grid,order="by_row")
     call close_(arch)
     call destroy_(grid)
     STOP_TIMER("REFLECTIONVEC:put_F_qq_plot")
      CHECK
   end subroutine

   subroutine put_labelled_F_qq_plot(self,name)
    REFLECTIONVEC(:) :: self
   ! Output a qq plot to the text file.
   ! It is a plot of the experimental quantile vs expected quantile.
     STR(STR_SIZE), optional :: name
     TEXTFILE, PTR :: tf
     REALMAT(:,:), PTR :: grid
     INTMAT(:,:), PTR :: hkl
      INT :: n
     STACK("REFLECTIONVEC:put_labelled_F_qq_plot")
     START_TIMER("REFLECTIONVEC:put_labelled_F_qq_plot")
     call create_(hkl,n_refl_(self),3)
     do n=1,n_refl_(self)
       hkl(n,:) = indices_(self,n)
     end do
     call create_(grid,n_refl_(self),2)
     call make_F_qq_plot_grid_(self,grid,hkl)
     call create_(tf,trim(name) // ":qq_plot_labelled")
     call open_(tf,for="write")
     call set_use_labels_(tf,FALSE)
     do n=1,n_refl_(self)
       call put_(tf,grid(n,1))
       call put_(tf,grid(n,2))
       call put_(tf,hkl(n,1))
       call put_(tf,hkl(n,2))
       call put_(tf,hkl(n,3))
       call flush_(tf)
     end do
     call close_(tf)
     call destroy_(tf)
     call destroy_(grid)
     call destroy_(hkl)
     STOP_TIMER("REFLECTIONVEC:put_labelled_F_qq_plot")
      CHECK
   end subroutine

   subroutine make_F_qq_plot_grid(self,grid,hkl)
    REFLECTIONVEC(:) :: self
   ! Make the grid for the Q-Q plot, which is a plot of the deviations X-Y
   ! versus the expected deviations, assuming that the expected devaitions
   ! are normally distributed. grid(1,:) contains the expected deviation d0j,
   ! grid(2,:) contains actual deviation dj.
     target :: self
     REALMAT(:,:), target :: grid
     INTMAT(:,:), optional :: hkl
     REALVEC(:), PTR :: d,e
     REFLECTION, PTR :: ref
     REAL :: p
     INT :: n_refl,i,j
     STACK("REFLECTIONVEC:make_F_qq_plot_grid")
     START_TIMER("REFLECTIONVEC:make_F_qq_plot_grid")
     n_refl = size(self)
     ENSURE(size(grid,1)==n_refl,"REFLECTIONVEC:make_F_qq_plot_grid ... grid wrong size")
     ENSURE(size(grid,2)==2,"REFLECTIONVEC:make_F_qq_plot_grid ... grid wrong size")

     e => grid(:,1) ! theoretical z's
     d => grid(:,2) ! calculated z's

     do i=1,n_refl
       ref => self(i)
       d(i)  = F_z_(ref) * sign(ONE,real(ref%F_calc))
     end do

    ! sort array from lowest z to highest
     if (present(hkl)) then
       ENSURE(size(hkl,1)==n_refl,"REFLECTIONVEC:make_F_qq_plot_grid ... size of index array incorrect")
       ENSURE(size(hkl,2)==3,"REFLECTIONVEC:make_F_qq_plot_grid ... size of index array incorrect")
       do i=1,n_refl-1
         do j=i+1,n_refl
           if (d(j) < d(i)) then
             call swap_elements_(d,i,j)
             call swap_columns_(hkl,i,j)
           end if
         end do
       end do
     else
       call sort_(d)
     end if

     do j=1,n_refl                    ! expected quantile, d0j
       p = (TWO*(n_refl-j)+ONE)/(TWO*n_refl) ! cumulative probability
       e(n_refl-j+1) = z_from_p_(p)
     end do

     STOP_TIMER("REFLECTIONVEC:make_F_qq_plot_grid")
      CHECK
   end subroutine

   subroutine simulate_new_F_exp(self)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data to be the experimental reflection data plus an
   ! error.  The errors are normally distributed, with the experimental errors
   ! being their standard deviations.
     INT :: n
     REAL :: dF
     STACK("REFLECTIONVEC:simulate_new_F_exp")
     START_TIMER("REFLECTIONVEC:simulate_new_F_exp")
     call flush_(stdout)
     call text_(stdout,"*************************************************")
     call text_(stdout,"adding normally distributed random error to F_exp")
     call text_(stdout,"*************************************************")
     do n=1,n_refl_(self)
       call to_random_normal_(dF) ! dF has mean zero, standard deviation one
       self(n)%F_exp = self(n)%F_exp + dF*self(n)%F_sigma
     end do
     STOP_TIMER("REFLECTIONVEC:simulate_new_F_exp")
      CHECK
   end subroutine

   subroutine add_random_error(self)
    REFLECTIONVEC(:) :: self
   ! Set the reflection data to be the experimental reflection data plus an
   ! error.  The errors are normally distributed, with the experimental errors
   ! being their standard deviations multiplied by a scale factor which is read
   ! from stdin.
     INT :: n
     REAL :: dF,scale
     STACK("REFLECTIONVEC:add_random_error")
     START_TIMER("REFLECTIONVEC:add_random_error")
     call flush_(stdout)
     call text_(stdout,"*************************************************")
     call text_(stdout,"adding normally distributed random error to F_exp")
     call text_(stdout,"*************************************************")
     call read_(stdin,scale)
     call show_(stdout,"normal distribution scaled by ",scale)
     do n=1,n_refl_(self)
       call to_random_normal_(dF) ! dF has mean zero, standard deviation one
       dF=dF*scale
       self(n)%F_exp = self(n)%F_exp + dF*self(n)%F_sigma
     end do
     STOP_TIMER("REFLECTIONVEC:add_random_error")
      CHECK
   end subroutine

end
