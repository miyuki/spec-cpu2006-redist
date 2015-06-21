!---------------------------------------------------------------------------
!
! COPPENSORBITALVEC: COPPENSORBITAL vectors
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
! $Id: coppensorbitalvec.foo,v 1.2.2.7 2003/10/13 06:22:45 reaper Exp $
!---------------------------------------------------------------------------

module COPPENSORBITALVEC_MODULE

#  include "coppensorbitalvec.use"

   implicit none

#  include "macros"
#  include "coppensorbitalvec.int"


   COPPENSORBITALVEC(:), PTR, private :: saved_self DEFAULT_NULL

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    COPPENSORBITALVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT :: dim
   ! The following code is inherited from OBJECTVEC
      STACK("COPPENSORBITALVEC:create")
      START_TIMER("COPPENSORBITALVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*COPPENSORBITAL_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("COPPENSORBITALVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    COPPENSORBITALVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("COPPENSORBITALVEC:destroy")
      START_TIMER("COPPENSORBITALVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("COPPENSORBITALVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*COPPENSORBITAL_SIZE)
      deallocate(self)
     STOP_TIMER("COPPENSORBITALVEC:destroy")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    COPPENSORBITALVEC(:) :: self
   ! Create a replica copy of "vec".
      COPPENSORBITALVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("COPPENSORBITALVEC:create_copy")
      START_TIMER("COPPENSORBITALVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("COPPENSORBITALVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    COPPENSORBITALVEC(:) :: self
   ! Copy "vec".
      COPPENSORBITALVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("COPPENSORBITALVEC:copy")
      START_TIMER("COPPENSORBITALVEC:copy")
      ENSURE(size(self)==size(vec),"COPPENSORBITALVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("COPPENSORBITALVEC:copy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    COPPENSORBITALVEC(:) :: self
   ! Nullify the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("COPPENSORBITALVEC:nullify_ptr_part")
      START_TIMER("COPPENSORBITALVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("COPPENSORBITALVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    COPPENSORBITALVEC(:) :: self
   ! Destroy the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("COPPENSORBITALVEC:destroy_ptr_part")
      START_TIMER("COPPENSORBITALVEC:destroy_ptr_part")
      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("COPPENSORBITALVEC:destroy_ptr_part")
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
    COPPENSORBITALVEC(:) :: self
   ! Set default values
   ! The following code is inherited from OBJECTVEC
       INT :: n
      STACK("COPPENSORBITALVEC:set_defaults")
      START_TIMER("COPPENSORBITALVEC:set_defaults")
      do n = 1,size(self)
        call set_defaults_(self(n))
      end do
     STOP_TIMER("COPPENSORBITALVEC:set_defaults")
      CHECK
   end subroutine

   subroutine set_saved_self(self)
    COPPENSORBITALVEC(:) :: self
   ! Set saved_self
      PTR :: self
      STACK("COPPENSORBITALVEC:set_saved_self")
      START_TIMER("COPPENSORBITALVEC:set_saved_self")
      saved_self => self
     STOP_TIMER("COPPENSORBITALVEC:set_saved_self")
      CHECK
   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   subroutine read_list_keywords(self)
    COPPENSORBITALVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     STACK("COPPENSORBITALVEC:read_list_keywords")
     START_TIMER("COPPENSORBITALVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","COPPENSORBITALVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("COPPENSORBITALVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    COPPENSORBITALVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("COPPENSORBITALVEC:process_list_keyword")
     START_TIMER("COPPENSORBITALVEC:process_list_keyword")
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
     STOP_TIMER("COPPENSORBITALVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    COPPENSORBITALVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("COPPENSORBITALVEC:read_data")
     START_TIMER("COPPENSORBITALVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","COPPENSORBITALVEC:read_data ... expecting open bracket symbol, {")
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
        ENSURE(word=="}","COPPENSORBITALVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("COPPENSORBITALVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    COPPENSORBITALVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
   ! The following code is inherited from OBJECTVEC
     COPPENSORBITAL, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("COPPENSORBITALVEC:data_length")
     START_TIMER("COPPENSORBITALVEC:data_length")
     ENSURE(next_item_(stdin)/="}","COPPENSORBITALVEC:data_length ... empty data list!")
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
     STOP_TIMER("COPPENSORBITALVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    COPPENSORBITALVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     INT :: s
     STACK("COPPENSORBITALVEC:read_altered_data")
     START_TIMER("COPPENSORBITALVEC:read_altered_data")
     ENSURE(associated(self),"COPPENSORBITALVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","COPPENSORBITALVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"COPPENSORBITALVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<=size(self),"COPPENSORBITALVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"COPPENSORBITALVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("COPPENSORBITALVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine read_append_data(self)
    COPPENSORBITALVEC(:) :: self
   ! Read in a set of data to append to an existing set.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
   STACK("COPPENSORBITALVEC:read_append_data")
   START_TIMER("COPPENSORBITALVEC:read_append_data")
   ENSURE(associated(self),"COPPENSORBITALVEC:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","COPPENSORBITALVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)
     STOP_TIMER("COPPENSORBITALVEC:read_append_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    COPPENSORBITALVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     COPPENSORBITAL :: tmp
     INT :: s
     STACK("COPPENSORBITALVEC:process_keys")
     START_TIMER("COPPENSORBITALVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("COPPENSORBITALVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    COPPENSORBITALVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
   ! The following code is inherited from OBJECTVEC
      COPPENSORBITAL :: tmp
      STACK("COPPENSORBITALVEC:keys_created")
      START_TIMER("COPPENSORBITALVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("COPPENSORBITALVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    COPPENSORBITALVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECTVEC
     COPPENSORBITAL :: tmp
     STACK("COPPENSORBITALVEC:set_keys")
     START_TIMER("COPPENSORBITALVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("COPPENSORBITALVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    COPPENSORBITALVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     COPPENSORBITAL :: tmp
     STACK("COPPENSORBITALVEC:clear_keys")
     START_TIMER("COPPENSORBITALVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("COPPENSORBITALVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    COPPENSORBITALVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      COPPENSORBITAL :: tmp
      STACK("COPPENSORBITALVEC:read_keys")
      START_TIMER("COPPENSORBITALVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("COPPENSORBITALVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    COPPENSORBITALVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STACK("COPPENSORBITALVEC:put_keys_table")
     START_TIMER("COPPENSORBITALVEC:put_keys_table")
     ENSURE(keys_created_(self),"COPPENSORBITALVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("COPPENSORBITALVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    COPPENSORBITALVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      COPPENSORBITAL :: tmp
      STACK("COPPENSORBITALVEC:put_table_header")
      START_TIMER("COPPENSORBITALVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("COPPENSORBITALVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    COPPENSORBITALVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      COPPENSORBITAL :: tmp
      STACK("COPPENSORBITALVEC:put_table_footer")
      START_TIMER("COPPENSORBITALVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("COPPENSORBITALVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    COPPENSORBITALVEC(:) :: self
   ! Redirect input
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("COPPENSORBITALVEC:redirect")
     START_TIMER("COPPENSORBITALVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("COPPENSORBITALVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    COPPENSORBITALVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("COPPENSORBITALVEC:revert")
     START_TIMER("COPPENSORBITALVEC:revert")
     call revert_(stdin)
     STOP_TIMER("COPPENSORBITALVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    COPPENSORBITALVEC(:) :: self
   ! Read in and process normal (non list-type) keywords from "stdin".
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("COPPENSORBITALVEC:read_keywords")
     START_TIMER("COPPENSORBITALVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","COPPENSORBITALVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("COPPENSORBITALVEC:read_keywords")
      CHECK
   end subroutine

   subroutine process_keyword(self,keyword)
    COPPENSORBITALVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("COPPENSORBITALVEC:process_keyword")
     START_TIMER("COPPENSORBITALVEC:process_keyword")
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
       call unknown_(tonto,word,"COPPENSORBITALVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("COPPENSORBITALVEC:process_keyword")
      CHECK
   end subroutine

!*******************************************************************************

   subroutine shrink(self,dim)
    COPPENSORBITALVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     COPPENSORBITALVEC(:), PTR :: old
     INT :: n
     STACK("COPPENSORBITALVEC:shrink")
     START_TIMER("COPPENSORBITALVEC:shrink")
     ENSURE(associated(self),"COPPENSORBITALVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"COPPENSORBITALVEC:shrink ... dim too large")
     ENSURE(dim>=0,"COPPENSORBITALVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then; STOP_TIMER("COPPENSORBITALVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)
     STOP_TIMER("COPPENSORBITALVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    COPPENSORBITALVEC(:) :: self
   ! Expand the vector "self" to "dim". New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     COPPENSORBITALVEC(:), PTR :: old
     INT :: old_dim
     STACK("COPPENSORBITALVEC:expand")
     START_TIMER("COPPENSORBITALVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"COPPENSORBITALVEC:expand ... dim not large enough")
        ENSURE(dim>=0,"COPPENSORBITALVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if
     STOP_TIMER("COPPENSORBITALVEC:expand")
      CHECK
   end subroutine

   subroutine append(self,v)
    COPPENSORBITALVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     COPPENSORBITALVEC(:), IN :: v
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("COPPENSORBITALVEC:append")
     START_TIMER("COPPENSORBITALVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)
     STOP_TIMER("COPPENSORBITALVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    COPPENSORBITALVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     COPPENSORBITAL, IN :: value
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("COPPENSORBITALVEC:append_1")
     START_TIMER("COPPENSORBITALVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)
     STOP_TIMER("COPPENSORBITALVEC:append_1")
      UNSTACK
   end subroutine

   subroutine put(self)
    COPPENSORBITALVEC(:) :: self
   ! Output the list information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("COPPENSORBITALVEC:put")
      START_TIMER("COPPENSORBITALVEC:put")
      do b = 1,size(self)
         call put_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("COPPENSORBITALVEC:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    COPPENSORBITALVEC(:) :: self
   ! Output table information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("COPPENSORBITALVEC:put_table")
      START_TIMER("COPPENSORBITALVEC:put_table")
      do b = 1,size(self)
         call put_table_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("COPPENSORBITALVEC:put_table")
      CHECK
   end subroutine

   function n_orbitals(self) result(res)
    COPPENSORBITALVEC(:) :: self
   ! Return the number of orbitals
      INT :: res
      STACK("COPPENSORBITALVEC:n_orbitals")
      START_TIMER("COPPENSORBITALVEC:n_orbitals")
      res = size(self)
     STOP_TIMER("COPPENSORBITALVEC:n_orbitals")
      CHECK
   end function

   function maximum_orbital_n_value(self) result(res)
    COPPENSORBITALVEC(:) :: self
   ! Returns the maximum orbital n value
     IN :: self
     INT :: res
     INT :: i
     STACK("COPPENSORBITALVEC:maximum_orbital_n_value")
     START_TIMER("COPPENSORBITALVEC:maximum_orbital_n_value")
     res = 0
     do i = 1,size(self)
        res = max(maxval(self(i)%n),res)
     end do
     STOP_TIMER("COPPENSORBITALVEC:maximum_orbital_n_value")
      CHECK
   end function

   function same_as(self,vec) result(same)
    COPPENSORBITALVEC(:) :: self
   ! Return TRUE if the orbital vector "self" is the same as "vec".
      COPPENSORBITALVEC(:) :: vec
      BIN :: same
      INT :: i
      STACK("COPPENSORBITALVEC:same_as")
      START_TIMER("COPPENSORBITALVEC:same_as")
      if (size(self) /= size(vec)) then
         same = FALSE
         STOP_TIMER("COPPENSORBITALVEC:same_as") CHECK return
      else
         same = TRUE
         do i = 1,size(self)
            same = same AND same_as_(self(i),vec(i))
            if (NOT same) exit
         end do
      end if
     STOP_TIMER("COPPENSORBITALVEC:same_as")
      CHECK
   end function

   function density_value_at_radius(R) result(res)
   ! Return the total coppens density values at the radial value "R".
     REAL :: R,res
     INT :: n
     COPPENSORBITALVEC(:), PTR :: self
     STACK("COPPENSORBITALVEC:density_value_at_radius")
     START_TIMER("COPPENSORBITALVEC:density_value_at_radius")
     self => saved_self
     res = ZERO ! Work out radial density here
     do n = 1,size(self)
        res = res + density_at_radius_(self(n),R)
     end do
     STOP_TIMER("COPPENSORBITALVEC:density_value_at_radius")
      CHECK
   end function

   function density_at_radius(self,R) result(res)
    COPPENSORBITALVEC(:) :: self
   ! Return the total coppens density values at the radial value "R".
     REAL :: R,res
     INT :: n
     STACK("COPPENSORBITALVEC:density_at_radius")
     START_TIMER("COPPENSORBITALVEC:density_at_radius")
     res = ZERO ! Work out radial density here
     do n = 1,size(self)
        res = res + density_at_radius_(self(n),R)
     end do
     STOP_TIMER("COPPENSORBITALVEC:density_at_radius")
      CHECK
   end function

   function densities_at_radii(self,R) result(res)
    COPPENSORBITALVEC(:) :: self
   ! Make the total coppens density values at the radial values "R".
     REALVEC(:) :: R
     REALVEC(size(R)) :: res
     INT :: n
     STACK("COPPENSORBITALVEC:densities_at_radii")
     START_TIMER("COPPENSORBITALVEC:densities_at_radii")
     res = ZERO ! Work out radial density here
     do n = 1,size(self)
        res = res + densities_at_radii_(self(n),R)
     end do
     STOP_TIMER("COPPENSORBITALVEC:densities_at_radii")
      CHECK
   end function

   subroutine unnormalise(self)
    COPPENSORBITALVEC(:) :: self
   ! Set the value of the orbital coefficients to correspond to un-normalised
   ! Slater functions -- assuming they are normalised. This saves computation.
      INT :: i
      STACK("COPPENSORBITALVEC:unnormalise")
      START_TIMER("COPPENSORBITALVEC:unnormalise")
      do i = 1,size(self)
         call unnormalise_(self(i))
      end do
     STOP_TIMER("COPPENSORBITALVEC:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    COPPENSORBITALVEC(:) :: self
   ! Set the value of the orbitals coefficients to correspond to normalised
   ! Slater functions --- assuming they are w.r.t. unnormalised functions. 
      INT :: i
      STACK("COPPENSORBITALVEC:renormalise")
      START_TIMER("COPPENSORBITALVEC:renormalise")
      do i = 1,size(self)
         call renormalise_(self(i))
      end do
     STOP_TIMER("COPPENSORBITALVEC:renormalise")
      CHECK
   end subroutine

end
