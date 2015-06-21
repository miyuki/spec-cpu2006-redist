!---------------------------------------------------------------------------
!
! SLATERBASISVEC: SLATERBASIS vectors
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
! $Id: slaterbasisvec.foo,v 1.1.2.3 2003/10/01 15:35:49 dylan Exp $
!---------------------------------------------------------------------------

module SLATERBASISVEC_MODULE

#  include "slaterbasisvec.use"

   implicit none

#  include "macros"
#  include "slaterbasisvec.int"


   SLATERBASISVEC(:), PTR, private :: saved_self DEFAULT_NULL
   STR(STR_SIZE), private :: basis_set_library_directory = "./basis_sets"

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    SLATERBASISVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT :: dim
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERBASISVEC:create")
      START_TIMER("SLATERBASISVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*SLATERBASIS_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SLATERBASISVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SLATERBASISVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERBASISVEC:destroy")
      START_TIMER("SLATERBASISVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SLATERBASISVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*SLATERBASIS_SIZE)
      deallocate(self)
     STOP_TIMER("SLATERBASISVEC:destroy")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    SLATERBASISVEC(:) :: self
   ! Create a replica copy of "vec".
      SLATERBASISVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SLATERBASISVEC:create_copy")
      START_TIMER("SLATERBASISVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("SLATERBASISVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    SLATERBASISVEC(:) :: self
   ! Copy "vec".
      SLATERBASISVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SLATERBASISVEC:copy")
      START_TIMER("SLATERBASISVEC:copy")
      ENSURE(size(self)==size(vec),"SLATERBASISVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("SLATERBASISVEC:copy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SLATERBASISVEC(:) :: self
   ! Nullify the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SLATERBASISVEC:nullify_ptr_part")
      START_TIMER("SLATERBASISVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("SLATERBASISVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SLATERBASISVEC(:) :: self
   ! Destroy the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SLATERBASISVEC:destroy_ptr_part")
      START_TIMER("SLATERBASISVEC:destroy_ptr_part")
      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("SLATERBASISVEC:destroy_ptr_part")
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
    SLATERBASISVEC(:) :: self
   ! Set default values
   ! The following code is inherited from OBJECTVEC
       INT :: n
      STACK("SLATERBASISVEC:set_defaults")
      START_TIMER("SLATERBASISVEC:set_defaults")
      do n = 1,size(self)
        call set_defaults_(self(n))
      end do
     STOP_TIMER("SLATERBASISVEC:set_defaults")
      CHECK
   end subroutine

   function library_directory(self,basis_set_kind) result(res)
    SLATERBASISVEC(:) :: self
   ! Return the basis set library directory for a particular "basis_set_kind".
     PTR :: self
     STR(*) :: basis_set_kind
     STR(STR_SIZE) :: res
   ! The following code is inherited from BASISVEC
     STACK("SLATERBASISVEC:library_directory")
     START_TIMER("SLATERBASISVEC:library_directory")
     res = trim(basis_set_library_directory)//"/"//trim(basis_set_kind)
     STOP_TIMER("SLATERBASISVEC:library_directory")
      CHECK
   end function

   subroutine set_library_directory(self)
    SLATERBASISVEC(:) :: self
   ! Read the directory name for where the basis set libraries are stored.
   ! The following code is inherited from BASISVEC
     STACK("SLATERBASISVEC:set_library_directory")
     START_TIMER("SLATERBASISVEC:set_library_directory")
     call read_(stdin,basis_set_library_directory)
     STOP_TIMER("SLATERBASISVEC:set_library_directory")
      CHECK
   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    SLATERBASISVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     STACK("SLATERBASISVEC:read_list_keywords")
     START_TIMER("SLATERBASISVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("SLATERBASISVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    SLATERBASISVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("SLATERBASISVEC:process_list_keyword")
     START_TIMER("SLATERBASISVEC:process_list_keyword")
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
     STOP_TIMER("SLATERBASISVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    SLATERBASISVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("SLATERBASISVEC:read_data")
     START_TIMER("SLATERBASISVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_data ... expecting open bracket symbol, {")
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
        ENSURE(word=="}","SLATERBASISVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("SLATERBASISVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    SLATERBASISVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
   ! The following code is inherited from OBJECTVEC
     SLATERBASIS, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("SLATERBASISVEC:data_length")
     START_TIMER("SLATERBASISVEC:data_length")
     ENSURE(next_item_(stdin)/="}","SLATERBASISVEC:data_length ... empty data list!")
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
     STOP_TIMER("SLATERBASISVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    SLATERBASISVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: word
     INT :: s
     STACK("SLATERBASISVEC:read_altered_data")
     START_TIMER("SLATERBASISVEC:read_altered_data")
     ENSURE(associated(self),"SLATERBASISVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"SLATERBASISVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<=size(self),"SLATERBASISVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"SLATERBASISVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("SLATERBASISVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine read_append_data(self)
    SLATERBASISVEC(:) :: self
   ! Read in a set of data to append to an existing set.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
   STACK("SLATERBASISVEC:read_append_data")
   START_TIMER("SLATERBASISVEC:read_append_data")
   ENSURE(associated(self),"SLATERBASISVEC:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_append_data ... expecting open bracket symbol: {")
     nullify(saved_self)
     call read_data_(saved_self)
     call append_(self,saved_self)
     call destroy_(saved_self)
     STOP_TIMER("SLATERBASISVEC:read_append_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    SLATERBASISVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     SLATERBASIS :: tmp
     INT :: s
     STACK("SLATERBASISVEC:process_keys")
     START_TIMER("SLATERBASISVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("SLATERBASISVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    SLATERBASISVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
   ! The following code is inherited from OBJECTVEC
      SLATERBASIS :: tmp
      STACK("SLATERBASISVEC:keys_created")
      START_TIMER("SLATERBASISVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("SLATERBASISVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    SLATERBASISVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECTVEC
     SLATERBASIS :: tmp
     STACK("SLATERBASISVEC:set_keys")
     START_TIMER("SLATERBASISVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("SLATERBASISVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    SLATERBASISVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     SLATERBASIS :: tmp
     STACK("SLATERBASISVEC:clear_keys")
     START_TIMER("SLATERBASISVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("SLATERBASISVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    SLATERBASISVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SLATERBASIS :: tmp
      STACK("SLATERBASISVEC:read_keys")
      START_TIMER("SLATERBASISVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("SLATERBASISVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    SLATERBASISVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
   ! The following code is inherited from OBJECTVEC
     STACK("SLATERBASISVEC:put_keys_table")
     START_TIMER("SLATERBASISVEC:put_keys_table")
     ENSURE(keys_created_(self),"SLATERBASISVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("SLATERBASISVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    SLATERBASISVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SLATERBASIS :: tmp
      STACK("SLATERBASISVEC:put_table_header")
      START_TIMER("SLATERBASISVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("SLATERBASISVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    SLATERBASISVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      SLATERBASIS :: tmp
      STACK("SLATERBASISVEC:put_table_footer")
      START_TIMER("SLATERBASISVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("SLATERBASISVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    SLATERBASISVEC(:) :: self
   ! Redirect input
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("SLATERBASISVEC:redirect")
     START_TIMER("SLATERBASISVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("SLATERBASISVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    SLATERBASISVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("SLATERBASISVEC:revert")
     START_TIMER("SLATERBASISVEC:revert")
     call revert_(stdin)
     STOP_TIMER("SLATERBASISVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   recursive subroutine read_keywords(self)
    SLATERBASISVEC(:) :: self
   ! Read data from "stdin" using keyword style input.
     PTR :: self
   ! The following code is inherited from BASISVEC
     STR(STR_SIZE) :: word
     STACK("SLATERBASISVEC:read_keywords")
     START_TIMER("SLATERBASISVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("SLATERBASISVEC:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SLATERBASISVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
   ! The following code is inherited from BASISVEC
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("SLATERBASISVEC:process_keyword")
     START_TIMER("SLATERBASISVEC:process_keyword")
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
       call unknown_(tonto,word,"SLATERBASISVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("SLATERBASISVEC:process_keyword")
      CHECK
   end subroutine

   subroutine read_library_directory(self)
    SLATERBASISVEC(:) :: self
   ! Read the directory name for where the basis set libraries are stored.
     PTR :: self
   ! The following code is inherited from BASISVEC
     STACK("SLATERBASISVEC:read_library_directory")
     START_TIMER("SLATERBASISVEC:read_library_directory")
     call read_(stdin,basis_set_library_directory)
     STOP_TIMER("SLATERBASISVEC:read_library_directory")
      CHECK
   end subroutine

   subroutine read_library_data(self,library,labels,n_unique_labels)
    SLATERBASISVEC(:) :: self
   ! Open the file named "library", read the basis sets which have the right
   ! "labels", and assign them to the basis vector.  The file must begin with a
   ! "{" symbol and must have at least one "data=" list-type keyword following
   ! the open bracket. NOTE: Self is created or expanded by the number of unique
   ! labels in "labels" -- those which cannot be found already in "self".
     PTR :: self
     STR(STR_SIZE), IN :: library
     STRVEC(STR_SIZE,:), IN :: labels
     INT, OUT, optional :: n_unique_labels
   ! The following code is inherited from BASISVEC
     STRVEC(STR_SIZE,:), PTR :: unique_labels
     STR(STR_SIZE) :: word
     SLATERBASIS, PTR :: basis
     INT :: found,i,u,n_labels,N
     STACK("SLATERBASISVEC:read_library_data")
     START_TIMER("SLATERBASISVEC:read_library_data")
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
        STOP_TIMER("SLATERBASISVEC:read_library_data") UNSTACK return
     end if
     call redirect_(stdin,library)
     ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_library_data ... expecting open bracket symbol, {")
     call read_(stdin,word)            ! Read the open bracket
     do                          ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (word=="data=")  exit
       call process_list_keyword_(self,word)
     end do
     ENSURE(keys_created_(self),"SLATERBASISVEC:read_library_data ... no keys= supplied")
     ENSURE(word=="data=","SLATERBASISVEC:read_library_data ... no data= list keyword")
     ENSURE(next_item_(stdin)=="{","SLATERBASISVEC:read_library_data ... expecting open bracket symbol, {")
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
       ENSURE(next_item_(stdin)/="}","SLATERBASISVEC:read_library_data ... missing basis sets in library")
     end do
     call revert_(stdin)
     call destroy_(unique_labels)
     STOP_TIMER("SLATERBASISVEC:read_library_data")
      UNSTACK
   end subroutine

!   read_library_data(library,labels,n_unique_labels) ::: leaky
!   ! Open the file named "library", read the basis sets which have the right
!   ! "labels", and assign them to the basis vector.  The file must begin with a
!   ! "{" symbol and must have at least one "data=" list-type keyword following
!   ! the open bracket. NOTE: Self is created or expanded by the number of unique
!   ! labels in "labels" -- those which cannot be found already in "self".
!     self :: PTR
!     library :: STR, IN
!     labels :: STRVEC, IN
!     n_unique_labels :: INT, OUT, optional
!     unique_labels :: STRVEC*
!     word :: STR
!     basis :: ELEMENT_TYPE*
!     found,i,u,n_labels,N :: INT
!     if (NOT .created) then
!        N = 0
!        n_labels = labels.dim    ! All labels are unique
!        unique_labels.create_copy(labels)
!        if (n_labels>0) .create(n_labels)
!     else                        
!        N = .n_basis
!        n_labels = labels.dim    ! Find and add only unique labels
!        unique_labels.create(n_labels)
!        u = 0
!        do i = 1,n_labels
!           if (any(self(:).label==labels(i))) cycle
!           u = u + 1
!           unique_labels(u) = labels(i)
!        end
!        n_labels = u
!        unique_labels.shrink(n_labels)
!        if (n_labels>0) .expand(N+n_labels)
!     end
!     if (present(n_unique_labels)) n_unique_labels = n_labels
!     if (n_labels==0) then
!        unique_labels.destroy
!        return
!     end
!     stdout.text("unique_labels")
!     stdout.put(unique_labels,"column")
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
!     found = 0                   ! Explicitly process data= keyword
!     do                          ! Try to match all basis labels
!       basis.create
!       basis.process_keys
!       stdout.show("basis label =",basis.label)
!       do i = 1,n_labels
!          if (basis.label.same_as(unique_labels(i),ignore_case=TRUE)) then
!             found = found + 1
!             self(N+i).copy(basis)
!          end
!       end
!       basis.destroy
!       if (found==n_labels) exit ! All bases found
!       ENSURE(stdin.next_item/="}","missing basis sets in library")
!     end
!     stdin.revert
!     unique_labels.destroy
!   end

!*******************************************************************************

   subroutine shrink(self,dim)
    SLATERBASISVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     SLATERBASISVEC(:), PTR :: old
     INT :: n
     STACK("SLATERBASISVEC:shrink")
     START_TIMER("SLATERBASISVEC:shrink")
     ENSURE(associated(self),"SLATERBASISVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"SLATERBASISVEC:shrink ... dim too large")
     ENSURE(dim>=0,"SLATERBASISVEC:shrink ... dim must be non-negative")
     if (dim==size(self)) then; STOP_TIMER("SLATERBASISVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call destroy_(old)
     STOP_TIMER("SLATERBASISVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    SLATERBASISVEC(:) :: self
   ! Expand the vector "self" to "dim". New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from OBJECTVEC
     SLATERBASISVEC(:), PTR :: old
     INT :: old_dim
     STACK("SLATERBASISVEC:expand")
     START_TIMER("SLATERBASISVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"SLATERBASISVEC:expand ... dim not large enough")
        ENSURE(dim>=0,"SLATERBASISVEC:expand ... dim must be non-negative")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        call copy_(self(1:old_dim),old)
        call destroy_(old)
     end if
     STOP_TIMER("SLATERBASISVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    SLATERBASISVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     SLATERBASISVEC(:), IN :: v
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("SLATERBASISVEC:append")
     START_TIMER("SLATERBASISVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     call copy_(self(dim+1:),v)
     STOP_TIMER("SLATERBASISVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    SLATERBASISVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     SLATERBASIS, IN :: value
   ! The following code is inherited from OBJECTVEC
     INT :: dim
     STACK("SLATERBASISVEC:append_1")
     START_TIMER("SLATERBASISVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call copy_(self(dim+1),value)
     STOP_TIMER("SLATERBASISVEC:append_1")
      UNSTACK
   end subroutine

   subroutine put(self)
    SLATERBASISVEC(:) :: self
   ! Output the list information
   ! The following code is inherited from OBJECTVEC
      INT :: b
      STACK("SLATERBASISVEC:put")
      START_TIMER("SLATERBASISVEC:put")
      do b = 1,size(self)
         call put_(self(b))
         call flush_(stdout)
      end do
     STOP_TIMER("SLATERBASISVEC:put")
      CHECK
   end subroutine

   function n_basis(self) result(res)
    SLATERBASISVEC(:) :: self
   ! Return the number of basis sets
      INT :: res
      STACK("SLATERBASISVEC:n_basis")
      START_TIMER("SLATERBASISVEC:n_basis")
      res = size(self)
   ! The following code is inherited from BASISVEC
     STOP_TIMER("SLATERBASISVEC:n_basis")
      CHECK
   end function

end

