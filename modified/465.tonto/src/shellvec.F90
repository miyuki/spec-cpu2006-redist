!---------------------------------------------------------------------------
!
!  SHELLVEC: A vector of gaussian SHELL data
!
! Copyright (C) Dylan Jayatilaka, 1998
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
! $Id: shellvec.foo,v 1.19.2.5 2003/11/13 05:33:21 reaper Exp $
!---------------------------------------------------------------------------

module SHELLVEC_MODULE

#  include "shellvec.use"

   implicit none

#  include "macros"
#  include "shellvec.int"


   SHELLVEC(:), PTR, private :: saved_self DEFAULT_NULL

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    SHELLVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT :: dim
      STACK("SHELLVEC:create")
      START_TIMER("SHELLVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*SHELL_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("SHELLVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SHELLVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SHELLVEC:destroy")
      START_TIMER("SHELLVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SHELLVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*SHELL_SIZE)
      deallocate(self)
     STOP_TIMER("SHELLVEC:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SHELLVEC(:) :: self
   ! Nullify the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SHELLVEC:nullify_ptr_part")
      START_TIMER("SHELLVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("SHELLVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SHELLVEC(:) :: self
   ! Destroy the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SHELLVEC:destroy_ptr_part")
      START_TIMER("SHELLVEC:destroy_ptr_part")
      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("SHELLVEC:destroy_ptr_part")
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

   subroutine create_copy(self,vec)
    SHELLVEC(:) :: self
   ! Create a replica copy of "vec".
      SHELLVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SHELLVEC:create_copy")
      START_TIMER("SHELLVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("SHELLVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    SHELLVEC(:) :: self
   ! Copy "vec".
      SHELLVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SHELLVEC:copy")
      START_TIMER("SHELLVEC:copy")
      ENSURE(size(self)==size(vec),"SHELLVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("SHELLVEC:copy")
      UNSTACK
   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    SHELLVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     STACK("SHELLVEC:read_list_keywords")
     START_TIMER("SHELLVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","SHELLVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("SHELLVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    SHELLVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("SHELLVEC:process_list_keyword")
     START_TIMER("SHELLVEC:process_list_keyword")
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
     STOP_TIMER("SHELLVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    SHELLVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("SHELLVEC:read_data")
     START_TIMER("SHELLVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","SHELLVEC:read_data ... expecting open bracket symbol, {")
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
        ENSURE(word=="}","SHELLVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("SHELLVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    SHELLVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
   ! The following code is inherited from OBJECTVEC
     SHELL, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("SHELLVEC:data_length")
     START_TIMER("SHELLVEC:data_length")
     ENSURE(next_item_(stdin)/="}","SHELLVEC:data_length ... empty data list!")
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
     STOP_TIMER("SHELLVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    SHELLVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     INT :: s
     STACK("SHELLVEC:read_altered_data")
     START_TIMER("SHELLVEC:read_altered_data")
     ENSURE(associated(self),"SHELLVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","SHELLVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"SHELLVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<=size(self),"SHELLVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"SHELLVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("SHELLVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine read_append_data(self)
    SHELLVEC(:) :: self
   ! Read in a set of data to append to an existing set.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
   STACK("SHELLVEC:read_append_data")
   START_TIMER("SHELLVEC:read_append_data")
   ENSURE(associated(self),"SHELLVEC:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","SHELLVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)
     STOP_TIMER("SHELLVEC:read_append_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    SHELLVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     SHELL :: tmp
     INT :: s
     STACK("SHELLVEC:process_keys")
     START_TIMER("SHELLVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("SHELLVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    SHELLVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
   ! The following code is inherited from OBJECTVEC
      SHELL :: tmp
      STACK("SHELLVEC:keys_created")
      START_TIMER("SHELLVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("SHELLVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    SHELLVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECTVEC
     SHELL :: tmp
     STACK("SHELLVEC:set_keys")
     START_TIMER("SHELLVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("SHELLVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    SHELLVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     SHELL :: tmp
     STACK("SHELLVEC:clear_keys")
     START_TIMER("SHELLVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("SHELLVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    SHELLVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SHELL :: tmp
      STACK("SHELLVEC:read_keys")
      START_TIMER("SHELLVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("SHELLVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    SHELLVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STACK("SHELLVEC:put_keys_table")
     START_TIMER("SHELLVEC:put_keys_table")
     ENSURE(keys_created_(self),"SHELLVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("SHELLVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    SHELLVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SHELL :: tmp
      STACK("SHELLVEC:put_table_header")
      START_TIMER("SHELLVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("SHELLVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    SHELLVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SHELL :: tmp
      STACK("SHELLVEC:put_table_footer")
      START_TIMER("SHELLVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("SHELLVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    SHELLVEC(:) :: self
   ! Redirect input
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("SHELLVEC:redirect")
     START_TIMER("SHELLVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("SHELLVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    SHELLVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("SHELLVEC:revert")
     START_TIMER("SHELLVEC:revert")
     call revert_(stdin)
     STOP_TIMER("SHELLVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    SHELLVEC(:) :: self
   ! Read in and process normal (non list-type) keywords from "stdin".
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("SHELLVEC:read_keywords")
     START_TIMER("SHELLVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","SHELLVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("SHELLVEC:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SHELLVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("SHELLVEC:process_keyword")
     START_TIMER("SHELLVEC:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}") ! do nothing.
       case("put         "); call put_(self)
       case("redirect    "); call redirect_(self)
       case("revert      "); call revert_(self)
       case default;       allocate(tonto%known_keywords(4))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "put         "
       tonto%known_keywords(3) = "redirect    "
       tonto%known_keywords(4) = "revert      "
       call unknown_(tonto,word,"SHELLVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("SHELLVEC:process_keyword")
      UNSTACK
   end subroutine

!*******************************************************************************

   function maximum_l_value(self) result(res)
    SHELLVEC(:) :: self
   ! Return the maximum l value in the shell vector
      INT :: res
      INT :: n
      STACK("SHELLVEC:maximum_l_value")
      START_TIMER("SHELLVEC:maximum_l_value")
      res = 0
      do n = 1,size(self)
         res = max(res,self(n)%l)
      end do
     STOP_TIMER("SHELLVEC:maximum_l_value")
      CHECK
   end function

   function same_as(self,sh) result(same)
    SHELLVEC(:) :: self
   ! Return TRUE if the shell vector set "self" is the same as "sh".
      SHELLVEC(:) :: sh
      BIN :: same
       INT :: i
      STACK("SHELLVEC:same_as")
      START_TIMER("SHELLVEC:same_as")
      if (n_shell_(self) /= n_shell_(sh)) then; same = FALSE; STOP_TIMER("SHELLVEC:same_as") CHECK return
      end if
      same = TRUE
      do i = 1,n_shell_(self)
         same = same AND same_as_(self(i),sh(i))
         if (NOT same) exit
      end do
     STOP_TIMER("SHELLVEC:same_as")
      CHECK
   end function

   subroutine shrink(self,dim)
    SHELLVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     SHELLVEC(:), PTR :: old
     INT :: n
     STACK("SHELLVEC:shrink")
     START_TIMER("SHELLVEC:shrink")
     ENSURE(associated(self),"SHELLVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"SHELLVEC:shrink ... dim too large")
     ENSURE(dim>=0,"SHELLVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then; STOP_TIMER("SHELLVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)
     STOP_TIMER("SHELLVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    SHELLVEC(:) :: self
   ! Expand the vector "self" to "dim". New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     SHELLVEC(:), PTR :: old
     INT :: old_dim
     STACK("SHELLVEC:expand")
     START_TIMER("SHELLVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"SHELLVEC:expand ... dim not large enough")
        ENSURE(dim>=0,"SHELLVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if
     STOP_TIMER("SHELLVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    SHELLVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     SHELLVEC(:), IN :: v
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("SHELLVEC:append")
     START_TIMER("SHELLVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)
     STOP_TIMER("SHELLVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    SHELLVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     SHELL, IN :: value
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("SHELLVEC:append_1")
     START_TIMER("SHELLVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)
     STOP_TIMER("SHELLVEC:append_1")
      UNSTACK
   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   PURE function n_shell(self) result(res)
    SHELLVEC(:) :: self
   ! Return the size of the shell vector
      IN :: self
      INT :: res
      res = size(self)
     STOP_TIMER("SHELLVEC:n_shell")
   end function

   PURE function no_of_shells(self) result(res)
    SHELLVEC(:) :: self
   ! Work out and return the number of shells 
      IN :: self
      INT :: res
      res = size(self)
     STOP_TIMER("SHELLVEC:no_of_shells")
   end function

   PURE function no_of_basis_functions(self) result(res)
    SHELLVEC(:) :: self
   ! Work out and return the number of basis functions 
      IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = res + self(i)%n_comp
      end do
     STOP_TIMER("SHELLVEC:no_of_basis_functions")
   end function

   PURE function no_of_primitives(self) result(res)
    SHELLVEC(:) :: self
   ! Work out and return the number of primitives 
      IN :: self
      INT :: res
      INT :: i
      res = 0
      do i= 1,size(self)
         res = res + self(i)%n_comp*self(i)%n_cc
      end do
     STOP_TIMER("SHELLVEC:no_of_primitives")
   end function

   PURE function min_exponent(self) result(res)
    SHELLVEC(:) :: self
   ! Return the minimum exponent in the basis.
     IN :: self
     REAL :: res
     REAL :: tmp
     INT :: i
     res = ZERO
     do i= 1,n_shell_(self)
       tmp = minval(self(i)%ex)
       if (tmp < res) res = tmp
     end do
     STOP_TIMER("SHELLVEC:min_exponent")
   end function

   subroutine unnormalise(self)
    SHELLVEC(:) :: self
   ! Take the normalisation factors out of the primitives, assuming
   ! that the contraction coeff's refer to normalised basis functions
       INT :: i
      STACK("SHELLVEC:unnormalise")
      START_TIMER("SHELLVEC:unnormalise")
      do i= 1,n_shell_(self)
         call unnormalise_(self(i))
      end do
     STOP_TIMER("SHELLVEC:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    SHELLVEC(:) :: self
   ! Put back in the normalisation factors of the primitives, assuming
   ! that the contraction coeff's refer to unnormalised basis functions
       INT :: i
      STACK("SHELLVEC:renormalise")
      START_TIMER("SHELLVEC:renormalise")
      do i= 1,n_shell_(self)
         call renormalise_(self(i))
      end do
     STOP_TIMER("SHELLVEC:renormalise")
      CHECK
   end subroutine

   subroutine put(self)
    SHELLVEC(:) :: self
   ! Put the shellvec information to "stdout"
      INT :: i
      STACK("SHELLVEC:put")
      START_TIMER("SHELLVEC:put")
      call flush_(stdout)
      call show_(stdout,"No. of shells   = ",n_shell_(self))
      call show_(stdout,"Maximum l value = ",maximum_l_value_(self))
      call flush_(stdout)
      call text_(stdout,"Shell list:")
      do i = 1,n_shell_(self)
         call put_(self(i))
      end do
     STOP_TIMER("SHELLVEC:put")
      CHECK
   end subroutine

end
