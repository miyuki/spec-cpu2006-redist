!---------------------------------------------------------------------------
!
! SLATERSHELLVEC: SLATERSHELL vectors
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
! $Id: slatershellvec.foo,v 1.1.2.3 2003/10/13 06:22:46 reaper Exp $
!---------------------------------------------------------------------------

module SLATERSHELLVEC_MODULE

#  include "slatershellvec.use"

   implicit none

#  include "macros"
#  include "slatershellvec.int"


   SLATERSHELLVEC(:), PTR, private :: saved_self DEFAULT_NULL

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    SLATERSHELLVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT :: dim
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERSHELLVEC:create")
      START_TIMER("SLATERSHELLVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*SLATERSHELL_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SLATERSHELLVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SLATERSHELLVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERSHELLVEC:destroy")
      START_TIMER("SLATERSHELLVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SLATERSHELLVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*SLATERSHELL_SIZE)
      deallocate(self)
     STOP_TIMER("SLATERSHELLVEC:destroy")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    SLATERSHELLVEC(:) :: self
   ! Create a replica copy of "vec".
      SLATERSHELLVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERSHELLVEC:create_copy")
      START_TIMER("SLATERSHELLVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("SLATERSHELLVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    SLATERSHELLVEC(:) :: self
   ! Copy "vec".
      SLATERSHELLVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SLATERSHELLVEC:copy")
      START_TIMER("SLATERSHELLVEC:copy")
      ENSURE(size(self)==size(vec),"SLATERSHELLVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("SLATERSHELLVEC:copy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SLATERSHELLVEC(:) :: self
   ! Nullify the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SLATERSHELLVEC:nullify_ptr_part")
      START_TIMER("SLATERSHELLVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("SLATERSHELLVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SLATERSHELLVEC(:) :: self
   ! Destroy the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SLATERSHELLVEC:destroy_ptr_part")
      START_TIMER("SLATERSHELLVEC:destroy_ptr_part")
      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("SLATERSHELLVEC:destroy_ptr_part")
      UNSTACK
   end subroutine

!   created result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!   end

!   destroyed result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has *not* been created
!      self :: PTR
!      res :: BIN
!   end

   subroutine set_defaults(self)
    SLATERSHELLVEC(:) :: self
   ! Set default values
   ! The following code is inherited from OBJECTVEC
       INT :: n
      STACK("SLATERSHELLVEC:set_defaults")
      START_TIMER("SLATERSHELLVEC:set_defaults")
      do n = 1,size(self)
        call set_defaults_(self(n))
      end do
     STOP_TIMER("SLATERSHELLVEC:set_defaults")
      CHECK
   end subroutine

   subroutine set_saved_self(self)
    SLATERSHELLVEC(:) :: self
   ! Set saved_self
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERSHELLVEC:set_saved_self")
      START_TIMER("SLATERSHELLVEC:set_saved_self")
      saved_self => self
     STOP_TIMER("SLATERSHELLVEC:set_saved_self")
      CHECK
   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   subroutine read_list_keywords(self)
    SLATERSHELLVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     STACK("SLATERSHELLVEC:read_list_keywords")
     START_TIMER("SLATERSHELLVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","SLATERSHELLVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("SLATERSHELLVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    SLATERSHELLVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("SLATERSHELLVEC:process_list_keyword")
     START_TIMER("SLATERSHELLVEC:process_list_keyword")
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
     STOP_TIMER("SLATERSHELLVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    SLATERSHELLVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("SLATERSHELLVEC:read_data")
     START_TIMER("SLATERSHELLVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","SLATERSHELLVEC:read_data ... expecting open bracket symbol, {")
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
        ENSURE(word=="}","SLATERSHELLVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("SLATERSHELLVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    SLATERSHELLVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
   ! The following code is inherited from OBJECTVEC
     SLATERSHELL, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("SLATERSHELLVEC:data_length")
     START_TIMER("SLATERSHELLVEC:data_length")
     ENSURE(next_item_(stdin)/="}","SLATERSHELLVEC:data_length ... empty data list!")
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
     STOP_TIMER("SLATERSHELLVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    SLATERSHELLVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     INT :: s
     STACK("SLATERSHELLVEC:read_altered_data")
     START_TIMER("SLATERSHELLVEC:read_altered_data")
     ENSURE(associated(self),"SLATERSHELLVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","SLATERSHELLVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"SLATERSHELLVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<=size(self),"SLATERSHELLVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"SLATERSHELLVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("SLATERSHELLVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine read_append_data(self)
    SLATERSHELLVEC(:) :: self
   ! Read in a set of data to append to an existing set.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
   STACK("SLATERSHELLVEC:read_append_data")
   START_TIMER("SLATERSHELLVEC:read_append_data")
   ENSURE(associated(self),"SLATERSHELLVEC:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","SLATERSHELLVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)
     STOP_TIMER("SLATERSHELLVEC:read_append_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    SLATERSHELLVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     SLATERSHELL :: tmp
     INT :: s
     STACK("SLATERSHELLVEC:process_keys")
     START_TIMER("SLATERSHELLVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("SLATERSHELLVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
   ! The following code is inherited from OBJECTVEC
      SLATERSHELL :: tmp
      STACK("SLATERSHELLVEC:keys_created")
      START_TIMER("SLATERSHELLVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("SLATERSHELLVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    SLATERSHELLVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECTVEC
     SLATERSHELL :: tmp
     STACK("SLATERSHELLVEC:set_keys")
     START_TIMER("SLATERSHELLVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("SLATERSHELLVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    SLATERSHELLVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     SLATERSHELL :: tmp
     STACK("SLATERSHELLVEC:clear_keys")
     START_TIMER("SLATERSHELLVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("SLATERSHELLVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    SLATERSHELLVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SLATERSHELL :: tmp
      STACK("SLATERSHELLVEC:read_keys")
      START_TIMER("SLATERSHELLVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("SLATERSHELLVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    SLATERSHELLVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STACK("SLATERSHELLVEC:put_keys_table")
     START_TIMER("SLATERSHELLVEC:put_keys_table")
     ENSURE(keys_created_(self),"SLATERSHELLVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("SLATERSHELLVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    SLATERSHELLVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SLATERSHELL :: tmp
      STACK("SLATERSHELLVEC:put_table_header")
      START_TIMER("SLATERSHELLVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("SLATERSHELLVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    SLATERSHELLVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SLATERSHELL :: tmp
      STACK("SLATERSHELLVEC:put_table_footer")
      START_TIMER("SLATERSHELLVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("SLATERSHELLVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    SLATERSHELLVEC(:) :: self
   ! Redirect input
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("SLATERSHELLVEC:redirect")
     START_TIMER("SLATERSHELLVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("SLATERSHELLVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    SLATERSHELLVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("SLATERSHELLVEC:revert")
     START_TIMER("SLATERSHELLVEC:revert")
     call revert_(stdin)
     STOP_TIMER("SLATERSHELLVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    SLATERSHELLVEC(:) :: self
   ! Read in and process normal (non list-type) keywords from "stdin".
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("SLATERSHELLVEC:read_keywords")
     START_TIMER("SLATERSHELLVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","SLATERSHELLVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("SLATERSHELLVEC:read_keywords")
      CHECK
   end subroutine

   subroutine process_keyword(self,keyword)
    SLATERSHELLVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("SLATERSHELLVEC:process_keyword")
     START_TIMER("SLATERSHELLVEC:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}") ! do nothing.
       case("put               "); call put_(self)
       case("redirect          "); call redirect_(self)
       case("revert            "); call revert_(self)
       case default;               allocate(tonto%known_keywords(4))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "put               "
       tonto%known_keywords(3) = "redirect          "
       tonto%known_keywords(4) = "revert            "
       call unknown_(tonto,word,"SLATERSHELLVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("SLATERSHELLVEC:process_keyword")
      CHECK
   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   function maximum_l_value(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Return the maximum l value in the shell vector
      INT :: res
      STACK("SLATERSHELLVEC:maximum_l_value")
      START_TIMER("SLATERSHELLVEC:maximum_l_value")
      res = maxval(self%l)
     STOP_TIMER("SLATERSHELLVEC:maximum_l_value")
      CHECK
   end function

   function maximum_n_value(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Returns the maximum orbital n value
     IN :: self
     INT :: res
     INT :: i
     STACK("SLATERSHELLVEC:maximum_n_value")
     START_TIMER("SLATERSHELLVEC:maximum_n_value")
     res = maxval(self(1)%n)
     do i = 2,size(self)
        res = max(maxval(self(i)%n),res)
     end do
     STOP_TIMER("SLATERSHELLVEC:maximum_n_value")
      CHECK
   end function

   PURE function min_exponent(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Return the minimum exponent in the basis.
     IN :: self
     REAL :: res
     INT :: i
     res = minval(self(1)%z)
     do i= 2,size(self)
       res = min(minval(self(i)%z),res)
     end do
     STOP_TIMER("SLATERSHELLVEC:min_exponent")
   end function

   function same_as(self,sh) result(same)
    SLATERSHELLVEC(:) :: self
   ! Return TRUE if the shell vector set "self" is the same as "sh".
      SLATERSHELLVEC(:) :: sh
      BIN :: same
      INT :: i
      STACK("SLATERSHELLVEC:same_as")
      START_TIMER("SLATERSHELLVEC:same_as")
      if (n_shell_(self) /= n_shell_(sh)) then
         same = FALSE
      else
         same = TRUE
         do i = 1,n_shell_(self)
            same = same AND same_as_(self(i),sh(i))
            if (NOT same) exit
         end do
      end if
     STOP_TIMER("SLATERSHELLVEC:same_as")
      CHECK
   end function

   PURE function n_shell(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Return the size of the shell vector
      IN :: self
      INT :: res
      res = size(self)
     STOP_TIMER("SLATERSHELLVEC:n_shell")
   end function

   PURE function no_of_shells(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Work out and return the number of shells 
      IN :: self
      INT :: res
      res = size(self)
     STOP_TIMER("SLATERSHELLVEC:no_of_shells")
   end function

   PURE function no_of_basis_functions(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Work out and return the number of basis functions 
      IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self)
         res = res + no_of_basis_functions_(self(i))
      end do
     STOP_TIMER("SLATERSHELLVEC:no_of_basis_functions")
   end function

   PURE function no_of_primitives(self) result(res)
    SLATERSHELLVEC(:) :: self
   ! Work out and return the number of primitives 
      IN :: self
      INT :: res
      INT :: i
      res = 0
      do i= 1,size(self)
         res = res + no_of_primitives_(self(i))
      end do
     STOP_TIMER("SLATERSHELLVEC:no_of_primitives")
   end function

!*******************************************************************************

   subroutine shrink(self,dim)
    SLATERSHELLVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     SLATERSHELLVEC(:), PTR :: old
     INT :: n
     STACK("SLATERSHELLVEC:shrink")
     START_TIMER("SLATERSHELLVEC:shrink")
     ENSURE(associated(self),"SLATERSHELLVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"SLATERSHELLVEC:shrink ... dim too large")
     ENSURE(dim>=0,"SLATERSHELLVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then; STOP_TIMER("SLATERSHELLVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)
     STOP_TIMER("SLATERSHELLVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    SLATERSHELLVEC(:) :: self
   ! Expand the vector "self" to "dim". New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     SLATERSHELLVEC(:), PTR :: old
     INT :: old_dim
     STACK("SLATERSHELLVEC:expand")
     START_TIMER("SLATERSHELLVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"SLATERSHELLVEC:expand ... dim not large enough")
        ENSURE(dim>=0,"SLATERSHELLVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if
     STOP_TIMER("SLATERSHELLVEC:expand")
      CHECK
   end subroutine

   subroutine append(self,v)
    SLATERSHELLVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     SLATERSHELLVEC(:), IN :: v
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("SLATERSHELLVEC:append")
     START_TIMER("SLATERSHELLVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)
     STOP_TIMER("SLATERSHELLVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    SLATERSHELLVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     SLATERSHELL, IN :: value
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("SLATERSHELLVEC:append_1")
     START_TIMER("SLATERSHELLVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)
     STOP_TIMER("SLATERSHELLVEC:append_1")
      UNSTACK
   end subroutine

   subroutine put(self)
    SLATERSHELLVEC(:) :: self
   ! Output the list information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("SLATERSHELLVEC:put")
      START_TIMER("SLATERSHELLVEC:put")
      do b = 1,size(self)
         call put_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("SLATERSHELLVEC:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    SLATERSHELLVEC(:) :: self
   ! Output table information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("SLATERSHELLVEC:put_table")
      START_TIMER("SLATERSHELLVEC:put_table")
      do b = 1,size(self)
         call put_table_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("SLATERSHELLVEC:put_table")
      CHECK
   end subroutine

   function density_value_at_radius(R) result(res)
   ! Return the total coppens density values at the radial value "R".
     REAL :: R,res
     INT :: n
     SLATERSHELLVEC(:), PTR :: self
     STACK("SLATERSHELLVEC:density_value_at_radius")
     START_TIMER("SLATERSHELLVEC:density_value_at_radius")
     self => saved_self
     res = ZERO ! Work out radial density here
     do n = 1,size(self)
        res = res + density_value_at_radius_(self(n),R)
     end do
     STOP_TIMER("SLATERSHELLVEC:density_value_at_radius")
      CHECK
   end function

   function density_at_radius(self,R) result(res)
    SLATERSHELLVEC(:) :: self
   ! Return the total coppens density values at the radial value "R".
     REAL :: R,res
     INT :: n
     STACK("SLATERSHELLVEC:density_at_radius")
     START_TIMER("SLATERSHELLVEC:density_at_radius")
     res = ZERO ! Work out radial density here
     do n = 1,size(self)
        res = res + density_value_at_radius_(self(n),R)
     end do
     STOP_TIMER("SLATERSHELLVEC:density_at_radius")
      CHECK
   end function

   function densities_at_radii(self,R) result(res)
    SLATERSHELLVEC(:) :: self
   ! Make the total coppens density values at the radial values "R".
     REALVEC(:) :: R
     REALVEC(size(R)) :: res
     INT :: n
     STACK("SLATERSHELLVEC:densities_at_radii")
     START_TIMER("SLATERSHELLVEC:densities_at_radii")
     res = ZERO ! Work out radial density here
     do n = 1,size(self)
        res = res + density_values_at_radii_(self(n),R)
     end do
     STOP_TIMER("SLATERSHELLVEC:densities_at_radii")
      CHECK
   end function

end
