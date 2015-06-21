!---------------------------------------------------------------------------
!
! BASISVEC: BASIS vectors
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
! $Id: basisvec.foo,v 1.24.2.5 2003/10/01 15:35:48 dylan Exp $
!---------------------------------------------------------------------------

module BASISVEC_MODULE

#  include "basisvec.use"

   implicit none

#  include "macros"
#  include "basisvec.int"


   BASISVEC(:), PTR, private :: saved_self DEFAULT_NULL
   STR(STR_SIZE), private :: basis_set_library_directory = "./basis_sets"

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    BASISVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT :: dim
   ! The following code is inherited from OBJECTVEC
      STACK("BASISVEC:create")
      START_TIMER("BASISVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*BASIS_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("BASISVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    BASISVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("BASISVEC:destroy")
      START_TIMER("BASISVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("BASISVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*BASIS_SIZE)
      deallocate(self)
     STOP_TIMER("BASISVEC:destroy")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    BASISVEC(:) :: self
   ! Create a replica copy of "vec".
      BASISVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("BASISVEC:create_copy")
      START_TIMER("BASISVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("BASISVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    BASISVEC(:) :: self
   ! Copy "vec".
      BASISVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("BASISVEC:copy")
      START_TIMER("BASISVEC:copy")
      ENSURE(size(self)==size(vec),"BASISVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("BASISVEC:copy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    BASISVEC(:) :: self
   ! Nullify the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("BASISVEC:nullify_ptr_part")
      START_TIMER("BASISVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("BASISVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    BASISVEC(:) :: self
   ! Destroy the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("BASISVEC:destroy_ptr_part")
      START_TIMER("BASISVEC:destroy_ptr_part")
      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("BASISVEC:destroy_ptr_part")
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
    BASISVEC(:) :: self
   ! Set default values
   ! The following code is inherited from OBJECTVEC
       INT :: n
      STACK("BASISVEC:set_defaults")
      START_TIMER("BASISVEC:set_defaults")
      do n = 1,size(self)
        call set_defaults_(self(n))
      end do
     STOP_TIMER("BASISVEC:set_defaults")
      CHECK
   end subroutine

   function library_directory(self,basis_set_kind) result(res)
    BASISVEC(:) :: self
   ! Return the basis set library directory for a particular "basis_set_kind".
     PTR :: self
     STR(*) :: basis_set_kind
     STR(STR_SIZE) :: res
     STACK("BASISVEC:library_directory")
     START_TIMER("BASISVEC:library_directory")
     res = trim(basis_set_library_directory)//"/"//trim(basis_set_kind)
     STOP_TIMER("BASISVEC:library_directory")
      CHECK
   end function

   subroutine set_library_directory(self)
    BASISVEC(:) :: self
   ! Read the directory name for where the basis set libraries are stored.
     STACK("BASISVEC:set_library_directory")
     START_TIMER("BASISVEC:set_library_directory")
     call read_(stdin,basis_set_library_directory)
     STOP_TIMER("BASISVEC:set_library_directory")
      CHECK
   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    BASISVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     STACK("BASISVEC:read_list_keywords")
     START_TIMER("BASISVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","BASISVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("BASISVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    BASISVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("BASISVEC:process_list_keyword")
     START_TIMER("BASISVEC:process_list_keyword")
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
     STOP_TIMER("BASISVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    BASISVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("BASISVEC:read_data")
     START_TIMER("BASISVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","BASISVEC:read_data ... expecting open bracket symbol, {")
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
        ENSURE(word=="}","BASISVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("BASISVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    BASISVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
   ! The following code is inherited from OBJECTVEC
     BASIS, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("BASISVEC:data_length")
     START_TIMER("BASISVEC:data_length")
     ENSURE(next_item_(stdin)/="}","BASISVEC:data_length ... empty data list!")
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
     STOP_TIMER("BASISVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    BASISVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     INT :: s
     STACK("BASISVEC:read_altered_data")
     START_TIMER("BASISVEC:read_altered_data")
     ENSURE(associated(self),"BASISVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","BASISVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"BASISVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<=size(self),"BASISVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"BASISVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("BASISVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine read_append_data(self)
    BASISVEC(:) :: self
   ! Read in a set of data to append to an existing set.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
   STACK("BASISVEC:read_append_data")
   START_TIMER("BASISVEC:read_append_data")
   ENSURE(associated(self),"BASISVEC:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","BASISVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)
     STOP_TIMER("BASISVEC:read_append_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    BASISVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     BASIS :: tmp
     INT :: s
     STACK("BASISVEC:process_keys")
     START_TIMER("BASISVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("BASISVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    BASISVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
   ! The following code is inherited from OBJECTVEC
      BASIS :: tmp
      STACK("BASISVEC:keys_created")
      START_TIMER("BASISVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("BASISVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    BASISVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECTVEC
     BASIS :: tmp
     STACK("BASISVEC:set_keys")
     START_TIMER("BASISVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("BASISVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    BASISVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     BASIS :: tmp
     STACK("BASISVEC:clear_keys")
     START_TIMER("BASISVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("BASISVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    BASISVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      BASIS :: tmp
      STACK("BASISVEC:read_keys")
      START_TIMER("BASISVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("BASISVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    BASISVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STACK("BASISVEC:put_keys_table")
     START_TIMER("BASISVEC:put_keys_table")
     ENSURE(keys_created_(self),"BASISVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("BASISVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    BASISVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      BASIS :: tmp
      STACK("BASISVEC:put_table_header")
      START_TIMER("BASISVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("BASISVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    BASISVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      BASIS :: tmp
      STACK("BASISVEC:put_table_footer")
      START_TIMER("BASISVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("BASISVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    BASISVEC(:) :: self
   ! Redirect input
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("BASISVEC:redirect")
     START_TIMER("BASISVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("BASISVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    BASISVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("BASISVEC:revert")
     START_TIMER("BASISVEC:revert")
     call revert_(stdin)
     STOP_TIMER("BASISVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   recursive subroutine read_keywords(self)
    BASISVEC(:) :: self
   ! Read data from "stdin" using keyword style input.
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("BASISVEC:read_keywords")
     START_TIMER("BASISVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","BASISVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("BASISVEC:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    BASISVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("BASISVEC:process_keyword")
     START_TIMER("BASISVEC:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}") ! do nothing.
       case("library_directory="); call read_library_directory_(self)
       case("put               "); call put_(self)
       case("redirect          "); call redirect_(self)
       case("revert            "); call revert_(self)
       case default;               allocate(tonto%known_keywords(5))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "library_directory="
       tonto%known_keywords(3) = "put               "
       tonto%known_keywords(4) = "redirect          "
       tonto%known_keywords(5) = "revert            "
       call unknown_(tonto,word,"BASISVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("BASISVEC:process_keyword")
      CHECK
   end subroutine

   subroutine read_library_directory(self)
    BASISVEC(:) :: self
   ! Read the directory name for where the basis set libraries are stored.
     PTR :: self
     STACK("BASISVEC:read_library_directory")
     START_TIMER("BASISVEC:read_library_directory")
     call read_(stdin,basis_set_library_directory)
     STOP_TIMER("BASISVEC:read_library_directory")
      CHECK
   end subroutine

!   read_library_data(library,labels) ::: leaky
!   ! Open the file named "library", read the basis sets which have the right
!   ! "labels", and assign them to the basis vector.  The file must begin with a
!   ! "{" symbol and must have at least one "data=" list-type keyword following
!   ! the open bracket. NOTE: Self is created.
!     self :: PTR
!     library :: STR, IN
!     labels :: STRVEC, IN
!     word :: STR
!     basis :: ELEMENT_TYPE*
!     found,i,n_labels,N :: INT
!     stdin.redirect(library)
!     ENSURE(stdin.next_item=="{","expecting open bracket symbol, {")
!     stdin.read(word)            ! Read the open bracket
!     do                          ! Loop over input list-type keywords
!       stdin.read(word)
!       if (word=="}")      exit
!       if (word=="data=")  exit
!       .process_list_keyword(word)
!     end
!     ENSURE(.keys_created,"no keys= supplied")
!     ENSURE(word=="data=","no data= list keyword")
!     ENSURE(stdin.next_item=="{","expecting open bracket symbol, {")
!     stdin.read(word)            ! move past open brace
!     n_labels = size(labels)     ! Explicitly process data= label
!     if (.created) then
!        N = .n_basis
!        .expand(N+n_labels)
!     else
!        N = 0
!        .create(n_labels)
!     end
!     found = 0
!     do                          ! Try to match all basis labels
!       basis.create
!       basis.process_keys
!       do i = 1,n_labels
!          if (basis.label.same_as(labels(i),ignore_case=TRUE)) then
!             found = found + 1
!             self(N+i).copy(basis)
!          end
!       end
!       basis.destroy
!       if (found==n_labels) exit ! All bases found
!       ENSURE(stdin.next_item/="}","missing basis sets in library")
!     end
!     stdin.revert
!   end

   subroutine read_library_data(self,library,labels,n_unique_labels)
    BASISVEC(:) :: self
   ! Open the file named "library", read the basis sets which have the right
   ! "labels", and assign them to the basis vector.  The file must begin with a
   ! "{" symbol and must have at least one "data=" list-type keyword following
   ! the open bracket. NOTE: Self is created or expanded by the number of unique
   ! labels in "labels" -- those which cannot be found already in "self".
     PTR :: self
     STR(STR_SIZE), IN :: library
     STRVEC(STR_SIZE,:), IN :: labels
     INT, OUT, optional :: n_unique_labels
     STRVEC(STR_SIZE,:), PTR :: unique_labels
     STR(STR_SIZE) :: word
     BASIS, PTR :: basis
     INT :: found,i,u,n_labels,N
     STACK("BASISVEC:read_library_data")
     START_TIMER("BASISVEC:read_library_data")
     if (NOT associated(self)) then
        N = 0
        n_labels = size(labels)    ! All labels are unique
        call create_copy_(unique_labels,labels)
        if (n_labels>0) call create_(self,n_labels)
     else                        
        N = n_basis_(self)
        n_labels = size(labels)    ! Find and add only unique labels
        call create_(unique_labels,n_labels)
        u = 0
        do i = 1,n_labels
           if (any(self(:)%label==labels(i))) cycle
           u = u + 1
           unique_labels(u) = labels(i)
        end do
        n_labels = u
        call shrink_(unique_labels,n_labels)
        if (n_labels>0) call expand_(self,N+n_labels)
     end if
     if (present(n_unique_labels)) n_unique_labels = n_labels
     if (n_labels==0) then
        call destroy_(unique_labels)
        STOP_TIMER("BASISVEC:read_library_data") UNSTACK return
     end if
     call redirect_(stdin,library)
     ENSURE(next_item_(stdin)=="{","BASISVEC:read_library_data ... expecting open bracket symbol, {")
     call read_(stdin,word)            ! Read the open bracket
     do                          ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (word=="data=")  exit
       call process_list_keyword_(self,word)
     end do
     ENSURE(keys_created_(self),"BASISVEC:read_library_data ... no keys= supplied")
     ENSURE(word=="data=","BASISVEC:read_library_data ... no data= list keyword")
     ENSURE(next_item_(stdin)=="{","BASISVEC:read_library_data ... expecting open bracket symbol, {")
     call read_(stdin,word)            ! move past open brace
     found = 0                   ! Explicitly process data= keyword
     do                          ! Try to match all basis labels
       call create_(basis)
       call process_keys_(basis)
       do i = 1,n_labels
          if (same_as_(basis%label,unique_labels(i),ignore_case=TRUE)) then
             found = found + 1
             call copy_(self(N+i),basis)
          end if
       end do
       call destroy_(basis)
       if (found==n_labels) exit ! All bases found
       ENSURE(next_item_(stdin)/="}","BASISVEC:read_library_data ... missing basis sets in library")
     end do
     call revert_(stdin)
     call destroy_(unique_labels)
     STOP_TIMER("BASISVEC:read_library_data")
      UNSTACK
   end subroutine

!*******************************************************************************

   subroutine shrink(self,dim)
    BASISVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     BASISVEC(:), PTR :: old
     INT :: n
     STACK("BASISVEC:shrink")
     START_TIMER("BASISVEC:shrink")
     ENSURE(associated(self),"BASISVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"BASISVEC:shrink ... dim too large")
     ENSURE(dim>=0,"BASISVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then; STOP_TIMER("BASISVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)
     STOP_TIMER("BASISVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    BASISVEC(:) :: self
   ! Expand the vector "self" to "dim". New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     BASISVEC(:), PTR :: old
     INT :: old_dim
     STACK("BASISVEC:expand")
     START_TIMER("BASISVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"BASISVEC:expand ... dim not large enough")
        ENSURE(dim>=0,"BASISVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if
     STOP_TIMER("BASISVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    BASISVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     BASISVEC(:), IN :: v
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("BASISVEC:append")
     START_TIMER("BASISVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)
     STOP_TIMER("BASISVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    BASISVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     BASIS, IN :: value
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("BASISVEC:append_1")
     START_TIMER("BASISVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)
     STOP_TIMER("BASISVEC:append_1")
      UNSTACK
   end subroutine

   subroutine unnormalise(self)
    BASISVEC(:) :: self
   ! Take the normalisation factors out of the primitives, assuming
   ! that the contraction coeff's refer to normalised basis functions
       INT :: b
      STACK("BASISVEC:unnormalise")
      START_TIMER("BASISVEC:unnormalise")
      do b = 1,size(self)
         call unnormalise_(self(b))
      end do
     STOP_TIMER("BASISVEC:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    BASISVEC(:) :: self
   ! Put back in the normalisation factors of the primitives, assuming
   ! that the contraction coeff's refer to unnormalised basis functions
       INT :: b
      STACK("BASISVEC:renormalise")
      START_TIMER("BASISVEC:renormalise")
      do b = 1,size(self)
         call renormalise_(self(b))
      end do
     STOP_TIMER("BASISVEC:renormalise")
      CHECK
   end subroutine

   subroutine put(self)
    BASISVEC(:) :: self
   ! Output the list information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("BASISVEC:put")
      START_TIMER("BASISVEC:put")
      do b = 1,size(self)
         call put_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("BASISVEC:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    BASISVEC(:) :: self
   ! Output table information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("BASISVEC:put_table")
      START_TIMER("BASISVEC:put_table")
      do b = 1,size(self)
         call put_table_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("BASISVEC:put_table")
      CHECK
   end subroutine

   function n_basis(self) result(res)
    BASISVEC(:) :: self
   ! Return the number of basis sets
      INT :: res
      STACK("BASISVEC:n_basis")
      START_TIMER("BASISVEC:n_basis")
      res = size(self)
     STOP_TIMER("BASISVEC:n_basis")
      CHECK
   end function

   function maximum_basis_set_l_value(self) result(res)
    BASISVEC(:) :: self
   ! Returns the maximum basis set l value
     IN :: self
     INT :: res
     INT :: i,j,l
     STACK("BASISVEC:maximum_basis_set_l_value")
     START_TIMER("BASISVEC:maximum_basis_set_l_value")
     res = 0
     do i = 1,size(self)
       do j = 1,size(self(i)%shell)
         l = self(i)%shell(j)%l
         res = max(l,res)
       end do
     end do
     STOP_TIMER("BASISVEC:maximum_basis_set_l_value")
      CHECK
   end function

end

