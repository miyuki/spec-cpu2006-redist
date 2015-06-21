!---------------------------------------------------------------------------
!
! ATOMVEC: ATOM vectors
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
! $Id: atomvec.foo,v 1.63.2.21 2003/11/13 05:34:14 reaper Exp $
!---------------------------------------------------------------------------

module ATOMVEC_MODULE

#  include "atomvec.use"

   implicit none

#  include "macros"
#  include "atomvec.int"


   ! Index information arrays; the strange "4" stands for "for" and prevents a
   ! name clash with a procedure of the same name.

   BIN, private :: index_info_created DEFAULT(FALSE)
   INTVEC(:), PTR, private :: atom_4_shell DEFAULT_NULL
   INTVEC(:), PTR, private :: atom_shell_4_shell DEFAULT_NULL
   INTVEC(:), PTR, private :: first_shell_4_atom DEFAULT_NULL
   INTVEC(:), PTR, private :: first_basis_fn_4_shell DEFAULT_NULL
    INTVEC(:), PTR, private :: last_basis_fn_4_shell DEFAULT_NULL
   INTVEC(:), PTR, private :: first_basis_fn_4_atom DEFAULT_NULL
    INTVEC(:), PTR, private :: last_basis_fn_4_atom DEFAULT_NULL

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    ATOMVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT :: dim
      STACK("ATOMVEC:create")
      START_TIMER("ATOMVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*ATOM_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("ATOMVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    ATOMVEC(:) :: self
   ! Destroy space for object
      PTR :: self
      STACK("ATOMVEC:destroy")
      START_TIMER("ATOMVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("ATOMVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*ATOM_SIZE)
      deallocate(self)
     STOP_TIMER("ATOMVEC:destroy")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    ATOMVEC(:) :: self
   ! Create a replica copy of "vec".
      ATOMVEC(:), IN :: vec
      PTR :: self
      STACK("ATOMVEC:create_copy")
      START_TIMER("ATOMVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("ATOMVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    ATOMVEC(:) :: self
   ! Copy "vec". Pointer parts are replicated.
      ATOMVEC(:), IN :: vec
      INT :: a
      STACK("ATOMVEC:copy")
      START_TIMER("ATOMVEC:copy")
      ENSURE(size(self)==size(vec),"ATOMVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("ATOMVEC:copy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    ATOMVEC(:) :: self
   ! Nullify the pointer parts of self
      INT :: a
      STACK("ATOMVEC:nullify_ptr_part")
      START_TIMER("ATOMVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("ATOMVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine nullify_bases(self)
    ATOMVEC(:) :: self
   ! Nullify the bases 
      STACK("ATOMVEC:nullify_bases")
      START_TIMER("ATOMVEC:nullify_bases")
      call nullify_basis_part_(self)
      call nullify_slaterbasis_part_(self)
      call nullify_coppensbasis_part_(self)
     STOP_TIMER("ATOMVEC:nullify_bases")
      CHECK
   end subroutine

   subroutine nullify_basis_part(self)
    ATOMVEC(:) :: self
   ! Nullify the basis parts of self
      INT :: a
      STACK("ATOMVEC:nullify_basis_part")
      START_TIMER("ATOMVEC:nullify_basis_part")
      do a = 1,size(self)
         nullify(self(a)%basis)
      end do
     STOP_TIMER("ATOMVEC:nullify_basis_part")
      CHECK
   end subroutine

   subroutine nullify_slaterbasis_part(self)
    ATOMVEC(:) :: self
   ! Nullify the slaterbasis parts of self
      INT :: a
      STACK("ATOMVEC:nullify_slaterbasis_part")
      START_TIMER("ATOMVEC:nullify_slaterbasis_part")
      do a = 1,size(self)
         nullify(self(a)%slaterbasis)
      end do
     STOP_TIMER("ATOMVEC:nullify_slaterbasis_part")
      CHECK
   end subroutine

   subroutine nullify_coppensbasis_part(self)
    ATOMVEC(:) :: self
   ! Nullify the coppensbasis parts of self
      INT :: a
      STACK("ATOMVEC:nullify_coppensbasis_part")
      START_TIMER("ATOMVEC:nullify_coppensbasis_part")
      do a = 1,size(self)
         nullify(self(a)%coppensbasis)
      end do
     STOP_TIMER("ATOMVEC:nullify_coppensbasis_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    ATOMVEC(:) :: self
   ! Destroy the pointer parts of self
      INT :: a
      ! avoid double destroying
      STACK("ATOMVEC:destroy_ptr_part")
      START_TIMER("ATOMVEC:destroy_ptr_part")
      if (coppens_interpolators_exist_(self)) call destroy_coppens_interpolators_(self) 
      if (slater_interpolators_exist_(self))  call destroy_slater_interpolators_(self) 
      call nullify_bases_(self)
      do a = 1,size(self) ! Now we can safely destroy everything .....
         call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("ATOMVEC:destroy_ptr_part")
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
    ATOMVEC(:) :: self
   ! Set default values
       INT :: n
      STACK("ATOMVEC:set_defaults")
      START_TIMER("ATOMVEC:set_defaults")
      do n = 1,size(self)
        call set_defaults_(self(n))
      end do
     STOP_TIMER("ATOMVEC:set_defaults")
      CHECK
   end subroutine

   subroutine set_labels_and_atomic_numbers(self,labels)
    ATOMVEC(:) :: self
   ! Set "labels" for the atoms and also their atomic numbers.
      STRVEC(len=*,:) :: labels
      INT :: n
   STACK("ATOMVEC:set_labels_and_atomic_numbers")
   START_TIMER("ATOMVEC:set_labels_and_atomic_numbers")
   ENSURE(size(labels)==size(self),"ATOMVEC:set_labels_and_atomic_numbers ... wrong length for labels")
      do n = 1,n_atom_(self)
        call set_label_and_atomic_number_(self(n),labels(n))
      end do
     STOP_TIMER("ATOMVEC:set_labels_and_atomic_numbers")
      CHECK
   end subroutine

   subroutine set_coppensbasis_labels(self,labels)
    ATOMVEC(:) :: self
   ! Set the .coppensbasis "labels".
      STRVEC(len=*,:) :: labels
      INT :: n
   STACK("ATOMVEC:set_coppensbasis_labels")
   START_TIMER("ATOMVEC:set_coppensbasis_labels")
   ENSURE(size(labels)==size(self),"ATOMVEC:set_coppensbasis_labels ... wrong length for labels")
      do n = 1,n_atom_(self)
        call set_coppensbasis_label_(self(n),labels(n))
      end do
     STOP_TIMER("ATOMVEC:set_coppensbasis_labels")
      CHECK
   end subroutine

   subroutine shrink(self,dim)
    ATOMVEC(:) :: self
   ! Shrink the atomvec to dimension "dim", retaining contents.
     PTR :: self
     INT, IN :: dim
     ATOMVEC(:), PTR :: old
     INT :: n
     STACK("ATOMVEC:shrink")
     START_TIMER("ATOMVEC:shrink")
     ENSURE(associated(self),"ATOMVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"ATOMVEC:shrink ... dim too large")
     if (dim==size(self)) then; STOP_TIMER("ATOMVEC:shrink") CHECK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       call copy_(self(n),old(n))
     end do
     call nullify_basis_part_(old)
     call destroy_(old)
     STOP_TIMER("ATOMVEC:shrink")
      CHECK
   end subroutine

! ***********************
! List-based I/O Routines
! ***********************

   recursive subroutine read_list_keywords(self)
    ATOMVEC(:) :: self
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("ATOMVEC:read_list_keywords")
     START_TIMER("ATOMVEC:read_list_keywords")
     ENSURE(next_item_(stdin)=="{","ATOMVEC:read_list_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input list-type keywords
       call read_(stdin,word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_list_keyword_(self,word)
     end do
     STOP_TIMER("ATOMVEC:read_list_keywords")
      UNSTACK
   end subroutine

   subroutine process_list_keyword(self,keyword)
    ATOMVEC(:) :: self
   ! Process a list-type "keyword", common to all list-type objects.
     PTR :: self
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     BIN :: ignore_braces
     STACK("ATOMVEC:process_list_keyword")
     START_TIMER("ATOMVEC:process_list_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("altered_data= "); call read_altered_data_(self)
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
     STOP_TIMER("ATOMVEC:process_list_keyword")
      UNSTACK
   end subroutine

   subroutine read_data(self,ignore_braces)
    ATOMVEC(:) :: self
   ! Process the keywords list to read data or commands. If "ignore_braces" is
   ! present then the opening and closing braces, which are normally required,
   ! are ignored.
     PTR :: self
     BIN, optional :: ignore_braces
     STR(STR_SIZE) :: word,message
     INT :: length
     STACK("ATOMVEC:read_data")
     START_TIMER("ATOMVEC:read_data")
     if (NOT present(ignore_braces)) then
        ENSURE(next_item_(stdin)=="{","ATOMVEC:read_data ... expecting open bracket symbol, {")
        call read_(stdin,word) ! move past open brace
     end if
     length = data_length_(self)
     if (associated(self)) then
        message = "No. of data items in new and old data lists do not match: "// &
                  "new = "//trim(to_str_(length))//", old = "//trim(to_str_(size(self)))
        ENSURE(length==size(self),message)
     else
        call create_(self,length)
     end if
     call process_keys_(self)
     if (NOT present(ignore_braces)) then
        call read_(stdin,word) ! read last brace
        ENSURE(word=="}","ATOMVEC:read_data ... expecting close bracket symbol, }")
     end if
     STOP_TIMER("ATOMVEC:read_data")
      UNSTACK
   end subroutine

   function data_length(self) result(length)
    ATOMVEC(:) :: self
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
     PTR :: self
     INT :: length
     ATOM, PTR :: tmp
     STR(STR_SIZE) :: word
     INT :: line,item
     STACK("ATOMVEC:data_length")
     START_TIMER("ATOMVEC:data_length")
     ENSURE(next_item_(stdin)/="}","ATOMVEC:data_length ... empty data list!")
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
     STOP_TIMER("ATOMVEC:data_length")
      CHECK
   end function

   subroutine read_altered_data(self)
    ATOMVEC(:) :: self
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
     PTR :: self
     STR(STR_SIZE) :: word
     INT :: s
     STACK("ATOMVEC:read_altered_data")
     START_TIMER("ATOMVEC:read_altered_data")
     ENSURE(associated(self),"ATOMVEC:read_altered_data ... list does not exist yet")
     ENSURE(next_item_(stdin)=="{","ATOMVEC:read_altered_data ... expecting open bracket symbol: {")
     call read_(stdin,word)
     read_loop: do
        call read_(stdin,word)
        if (word=="}") exit read_loop
        ENSURE(is_int_(word),"ATOMVEC:read_altered_data ... expecting integer list-element index")
        s = to_int_(word)
        ENSURE(s<size(self),"ATOMVEC:read_altered_data ... list-element too large")
        ENSURE(s>0,"ATOMVEC:read_altered_data ... list-element must be positive")
        call process_keys_(self(s))
     end do read_loop
     STOP_TIMER("ATOMVEC:read_altered_data")
      UNSTACK
   end subroutine

   subroutine process_keys(self)
    ATOMVEC(:) :: self
   ! Process the "keys" on each element of the list.
     PTR :: self
     ATOM :: tmp
     INT :: s
     STACK("ATOMVEC:process_keys")
     START_TIMER("ATOMVEC:process_keys")
     if (associated(self)) then
        do s = 1,size(self)
           call process_keys_(self(s))
        end do
     else ! for embedded keywords
        call process_keys_(tmp)
     end if
     STOP_TIMER("ATOMVEC:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the list-element keys are created.
      PTR :: self
      BIN :: res
      ATOM :: tmp
      STACK("ATOMVEC:keys_created")
      START_TIMER("ATOMVEC:keys_created")
      res = keys_created_(tmp)
     STOP_TIMER("ATOMVEC:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    ATOMVEC(:) :: self
   ! This is for setting the "keys" externally.
     PTR :: self
     STRVEC(len=*,:) :: the_keys
     ATOM :: tmp
     STACK("ATOMVEC:set_keys")
     START_TIMER("ATOMVEC:set_keys")
     call set_keys_(tmp,the_keys)
     STOP_TIMER("ATOMVEC:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    ATOMVEC(:) :: self
   ! This is for destroying the "keys" externally.
     PTR :: self
     ATOM :: tmp
     STACK("ATOMVEC:clear_keys")
     START_TIMER("ATOMVEC:clear_keys")
     call clear_keys_(tmp)
     STOP_TIMER("ATOMVEC:clear_keys")
      CHECK
   end subroutine

   subroutine read_keys(self)
    ATOMVEC(:) :: self
   ! Read a new set of keys
      PTR :: self
      ATOM :: tmp
      STACK("ATOMVEC:read_keys")
      START_TIMER("ATOMVEC:read_keys")
      call read_keys_(tmp)
     STOP_TIMER("ATOMVEC:read_keys")
      CHECK
   end subroutine

   subroutine put_keys_table(self)
    ATOMVEC(:) :: self
   ! Output a generic table based on the "keys"
     PTR :: self
     STACK("ATOMVEC:put_keys_table")
     START_TIMER("ATOMVEC:put_keys_table")
     ENSURE(keys_created_(self),"ATOMVEC:put_keys_table ... no keys")
     call put_table_header_(self)
     call process_keys_(self)
     call put_table_footer_(self)
     STOP_TIMER("ATOMVEC:put_keys_table")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    ATOMVEC(:) :: self
   ! Put out a table header based on "keys"
      PTR :: self
      ATOM :: tmp
      STACK("ATOMVEC:put_table_header")
      START_TIMER("ATOMVEC:put_table_header")
      call put_table_header_(tmp)
     STOP_TIMER("ATOMVEC:put_table_header")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    ATOMVEC(:) :: self
   ! Put out a table footer based on "keys"
      PTR :: self
      ATOM :: tmp
      STACK("ATOMVEC:put_table_footer")
      START_TIMER("ATOMVEC:put_table_footer")
      call put_table_footer_(tmp)
     STOP_TIMER("ATOMVEC:put_table_footer")
      CHECK
   end subroutine

   subroutine redirect(self)
    ATOMVEC(:) :: self
   ! Redirect input
     PTR :: self
     STACK("ATOMVEC:redirect")
     START_TIMER("ATOMVEC:redirect")
     call redirect_(stdin,next_str_(stdin))
     STOP_TIMER("ATOMVEC:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    ATOMVEC(:) :: self
   ! Revert back to previous stdin file
     PTR :: self
     STACK("ATOMVEC:revert")
     START_TIMER("ATOMVEC:revert")
     call revert_(stdin)
     STOP_TIMER("ATOMVEC:revert")
      UNSTACK
   end subroutine

! ***************************
! Non-list based I/O routines
! ***************************

   subroutine read_keywords(self)
    ATOMVEC(:) :: self
   ! Read in and process normal (non list-type) keywords from "stdin".
     PTR :: self
     STR(STR_SIZE) :: word
     STACK("ATOMVEC:read_keywords")
     START_TIMER("ATOMVEC:read_keywords")
     ENSURE(next_item_(stdin)=="{","ATOMVEC:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("ATOMVEC:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    ATOMVEC(:) :: self
   ! Process a normal (non list-type) "keyword".
     PTR :: self
     STR(STR_SIZE) :: keyword
     STR(STR_SIZE) :: word
     STACK("ATOMVEC:process_keyword")
     START_TIMER("ATOMVEC:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case("}") ! do nothing.
       case("put                    "); call put_(self)
       case("put_bond_angle_table   "); call put_bond_angle_table_(self)
       case("put_bond_length_table  "); call put_bond_length_table_(self)
       case("put_coord_info         "); call put_coord_info_(self)
       case("put_coordinates        "); call put_coordinates_(self)
       case("put_mm_info            "); call put_mm_info_(self)
       case("put_thermal_tensors    "); call put_thermal_tensors_(self)
       case("put_torsion_angle_table"); call put_torsion_angle_table_(self)
       case("put_restraint_atoms    "); call put_restraint_atoms_(self)
       case("read_cif               "); call read_CIF_(self)
       case("redirect               "); call redirect_(self)
       case("revert                 "); call revert_(self)
       case default;               allocate(tonto%known_keywords(13))
       tonto%known_keywords(1) = "}"
       tonto%known_keywords(2) = "put                    "
       tonto%known_keywords(3) = "put_bond_angle_table   "
       tonto%known_keywords(4) = "put_bond_length_table  "
       tonto%known_keywords(5) = "put_coord_info         "
       tonto%known_keywords(6) = "put_coordinates        "
       tonto%known_keywords(7) = "put_mm_info            "
       tonto%known_keywords(8) = "put_thermal_tensors    "
       tonto%known_keywords(9) = "put_torsion_angle_table"
       tonto%known_keywords(10) = "put_restraint_atoms    "
       tonto%known_keywords(11) = "read_cif               "
       tonto%known_keywords(12) = "redirect               "
       tonto%known_keywords(13) = "revert                 "
       call unknown_(tonto,word,"ATOMVEC:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("ATOMVEC:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_CIF(self)
    ATOMVEC(:) :: self
   ! Read information from a Crystallographic Information File whose name is
   ! read from "stdin".
      PTR :: self
      CIF, PTR :: cif
      BIN :: found
      STR(STR_SIZE) :: name
      STACK("ATOMVEC:read_CIF")
      START_TIMER("ATOMVEC:read_CIF")
      call read_(stdin,name)
      call create_(cif,name)
      call open_(cif)
      call find_crystal_data_block_(cif,found)
      ENSURE(found,"ATOMVEC:read_CIF ... no crystal data block found") 
      call read_CIF_(self,cif)
      call destroy_(cif)
     STOP_TIMER("ATOMVEC:read_CIF")
      UNSTACK
   end subroutine

   subroutine read_CIF_1(self,cif)
    ATOMVEC(:) :: self
   ! Read information from a Crystallographic Information File, "cif".
   ! NOTE: self is destroyed and created from this file!
      PTR :: self
      CIF :: cif
      BIN :: fs,fc,fo,fu,fl
      STRVEC(STR_SIZE,:), PTR :: IDs,labels
      INT :: i,ind
      REALMAT(:,:), PTR :: pos,U
      ! Read the site labels
      STACK("ATOMVEC:read_CIF_1")
      START_TIMER("ATOMVEC:read_CIF_1")
      call read_looped_item_(cif,"_atom_site_label",labels,fs)
      if (NOT fs) &
      call read_looped_item_(cif,"_atom_site_type_symbol",labels,fs)
      ENSURE(fs,"ATOMVEC:read_CIF_1 ... no atom site label information in CIF file")
      ! Read the site coordinates, and possibly occupancies
      call find_looped_item_(cif,"_atom_site_occupancy",fo)
      if (fo) then
         call create_(IDs,4)
         IDs = (/"_atom_site_fract_x  ", &
                "_atom_site_fract_y  ", &
                "_atom_site_fract_z  ", &
                "_atom_site_occupancy"/)
         call read_looped_items_(cif,IDs,pos,found=fc)
      else
         call create_(IDs,3)
         IDs = (/"_atom_site_fract_x  ", &
                "_atom_site_fract_y  ", &
                "_atom_site_fract_z  "/)
         call read_looped_items_(cif,IDs,pos,found=fc)
      end if
      ENSURE(fc,"ATOMVEC:read_CIF_1 ... no atom coordinate information in CIF file")
      ! Assign the CIF info to the atom list
      call destroy_(self)
      call create_(self,size(labels))
      call set_labels_and_atomic_numbers_(self,labels)
      self(:)%pos(1) = pos(1,:)
      self(:)%pos(2) = pos(2,:)
      self(:)%pos(3) = pos(3,:)
      self(:)%axis_system = "crystal"
      if (fo) self(:)%site_occupancy = pos(4,:)
      call destroy_(pos)
      call destroy_(IDs)
      call destroy_(labels)
      ! Now read U tensor if it is there ...
      call create_(IDs,6)
      IDs = (/"_atom_site_aniso_U_11", &
             "_atom_site_aniso_U_22", &
             "_atom_site_aniso_U_33", &
             "_atom_site_aniso_U_12", &
             "_atom_site_aniso_U_13", &
             "_atom_site_aniso_U_23"/)
      call read_looped_item_(cif,"_atom_site_aniso_label",labels,fl)
      call read_looped_items_(cif,IDs,U,found=fu)
      if (NOT fl OR NOT fu) then; STOP_TIMER("ATOMVEC:read_CIF_1") UNSTACK return; end if
      ! Match the labels and assign the U tensors
      do i = 1,size(labels)
         ind = index_of_(self(:)%label,labels(i))
         ENSURE(ind>0,"ATOMVEC:read_CIF_1 ... label "//trim(labels(i))//" cant be found")
         self(ind)%thermal_tensor(1,1) = U(1,i)
         self(ind)%thermal_tensor(2,2) = U(2,i)
         self(ind)%thermal_tensor(3,3) = U(3,i)
         self(ind)%thermal_tensor(1,2) = U(4,i)
         self(ind)%thermal_tensor(2,1) = U(4,i)
         self(ind)%thermal_tensor(1,3) = U(5,i)
         self(ind)%thermal_tensor(3,1) = U(5,i)
         self(ind)%thermal_tensor(2,3) = U(6,i)
         self(ind)%thermal_tensor(3,2) = U(6,i)
      end do
      self(:)%thermal_axis_system = "crystal"
      call destroy_(U)
      call destroy_(labels)
      call destroy_(IDs)
     STOP_TIMER("ATOMVEC:read_CIF_1")
      UNSTACK
   end subroutine

!*******************************************************************************

!   resolve_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by matching the atom basis set label
!   ! with the labels from the basis set vector "basis". If "clobber" is present
!   ! and TRUE (the default situation), then any matched basis is pointer
!   ! assigned to the matching element in "basis" irrespective of whether it is
!   ! already associated; otherwise if the matching basis set is already
!   ! associated, it is not pointer assigned. If "resolve_all" is present and TRUE
!   ! (the default) an error is generated if all the basis sets are not resolved;
!   ! the default is that "resolve_all is FALSE.
!      basis :: BASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      ENSURE(basis.created,"no basis set")
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis(basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_library_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by first looking in the "basis" list,
!   ! and then (if needed) looking in a basis set library file. The appropriate
!   ! basis set library files are obtained from the basis set qualifier -- the
!   ! part after the colon in the atom basis set label. For example, if the atom
!   ! basis set label is "H:DZP", then the qualifier is "DZP" and the routine
!   ! looks in library file basis_sets/"DZP" for a matching basis set. If found,
!   ! the basis set is appended to "basis". If "clobber" is present and TRUE (the
!   ! default situation), then any matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already associated, it
!   ! is not pointer assigned. If "resolve_all" is present and TRUE (the default)
!   ! an error is generated if all the basis sets are not resolved; the default
!   ! is that "resolve_all is FALSE.
!      basis :: BASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_library_basis(basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,resolve_all) ::: leaky
!   ! Match the basis set labels from the given basis set vector "basis"
!   ! with atom basis set labels contructed in a standard way by joining
!   ! the (lower case) atom chemical symbol with the -"suffix" string.
!   ! If "clobber" is present and FALSE, then any basis which is already associated
!   ! is not resolved even though there may be a matching entry. If "resolve_all"
!   ! is present and FALSE, then it is not an error if all the basis sets are
!   ! nopt resolved.
!      basis :: BASISVEC*
!      suffix :: STR(*)
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis_suffix(basis,suffix,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by matching the atom basis set label
!   ! with the labels from the basis set vector "basis". If "clobber" is present
!   ! and TRUE (the default situation), then any matched basis is pointer
!   ! assigned to the matching element in "basis" irrespective of whether it is
!   ! already associated; otherwise if the matching basis set is already
!   ! associated, it is not pointer assigned. If "resolve_all" is present and TRUE
!   ! (the default) an error is generated if all the basis sets are not resolved;
!   ! the default is that "resolve_all is FALSE.
!      basis :: SLATERBASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      ENSURE(basis.created,"no basis set")
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis(basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_library_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by first looking in the "basis" list,
!   ! and then (if needed) looking in a basis set library file. The appropriate
!   ! basis set library files are obtained from the basis set qualifier -- the
!   ! part after the colon in the atom basis set label. For example, if the atom
!   ! basis set label is "H:DZP", then the qualifier is "DZP" and the routine
!   ! looks in library file basis_sets/"DZP" for a matching basis set. If found,
!   ! the basis set is appended to "basis". If "clobber" is present and TRUE (the
!   ! default situation), then any matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already associated, it
!   ! is not pointer assigned. If "resolve_all" is present and TRUE (the default)
!   ! an error is generated if all the basis sets are not resolved; the default
!   ! is that "resolve_all is FALSE.
!      basis :: SLATERBASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_library_basis(basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end
!
!   resolve_basis_suffix(basis,suffix,clobber,resolve_all) ::: leaky
!   ! Match the basis set labels from the given basis set vector "basis"
!   ! with atom basis set labels contructed in a standard way by joining
!   ! the (lower case) atom chemical symbol with the -"suffix" string.
!   ! If "clobber" is present and FALSE, then any basis which is already associated
!   ! is not resolved even though there may be a matching entry. If "resolve_all"
!   ! is present and FALSE, then it is not an error if all the basis sets are
!   ! nopt resolved.
!      basis :: SLATERBASISVEC*
!      suffix :: STR(*)
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_basis_suffix(basis,suffix,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

!   resolve_library_bases(basis,clobber,resolve_all) ::: leaky
!   ! Resolve the basis sets for each atom by first looking in the "basis" list,
!   ! and then (if needed) looking in a basis set library file. The appropriate
!   ! basis set library files are obtained from the basis set qualifier -- the
!   ! part after the colon in the atom basis set label. For example, if the atom
!   ! basis set label is "H:DZP", then the qualifier is "DZP" and the routine
!   ! looks in library file basis_sets/"DZP" for a matching basis set. If found,
!   ! the basis set is appended to "basis". If "clobber" is present and TRUE (the
!   ! default situation), then any matched basis is pointer assigned to the
!   ! matching element in "basis" irrespective of whether it is already
!   ! associated; otherwise if the matching basis set is already associated, it
!   ! is not pointer assigned. If "resolve_all" is present and TRUE (the default)
!   ! an error is generated if all the basis sets are not resolved; the default
!   ! is that "resolve_all is FALSE.
!      basis :: COPPENSBASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_library_basis(basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

!   resolve_bases_by_labels(labels,basis,clobber,resolve_all) 
!   ! Resolve the basis sets for each atom self(a), by pointer assigning to the
!   ! element in "basis" which has the same label as "labels(a)". If "clobber" is
!   ! present and TRUE (the default situation), then any matched basis is pointer
!   ! assigned to the matching element in "basis" irrespective of whether it is
!   ! already associated; otherwise if the matching basis set is already
!   ! associated, it is not pointer assigned. If "resolve_all" is present and
!   ! TRUE (the default) an error is generated if all the basis sets are not
!   ! resolved; the default is that "resolve_all is FALSE.
!      labels :: STRVEC
!      basis :: COPPENSBASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!   ENSURE(basis.created,"no basis set")
!   ENSURE(labels.dim==.n_atom,"wrong number of labels")
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_by_basis_label(labels(a),basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

!   resolve_bases_by_suffix(suffix,basis,clobber,resolve_all) 
!   ! Resolve the basis sets for each atom self(a), by pointer assigning to the
!   ! element in "basis" which has a label constructed in a standard way, by
!   ! joining the atom chemical symbol with the -"suffix" string.  If "clobber"
!   ! is present and FALSE, then any basis which is already associated is not
!   ! resolved even though there may be a matching entry. If "resolve_all" is
!   ! present and FALSE, then it is not an error if all the basis sets are nopt
!   ! resolved.
!      suffix :: STR(*)
!      basis :: COPPENSBASISVEC*
!      clobber,resolve_all :: BIN, optional
!      a :: INT
!      found,find_all :: BIN
!      find_all = TRUE
!      if (present(resolve_all)) find_all = resolve_all
!      do a = 1,.n_atom
!         self(a).resolve_by_basis_suffix(suffix,basis,clobber,found)
!         if (find_all) then
!           ENSURE(found,"unknown basis, atom "// trim(a.to_str))
!         end
!      end
!   end

   subroutine resolve_bases(self,basis,suffix)
    ATOMVEC(:) :: self
   ! Resolve the basis sets for each atom -- by pointer assigning its basis to
   ! the element in "basis" which matches either the atoms .basis_label, or else
   ! matches a label constructed in a standard way, by joining the atom chemical
   ! symbol with the ":suffix" string.  
      BASISVEC(:), PTR :: basis
      STR(*), optional :: suffix
      INT :: a
      STACK("ATOMVEC:resolve_bases")
      START_TIMER("ATOMVEC:resolve_bases")
      do a = 1,n_atom_(self)
         call resolve_basis_(self(a),basis,suffix)
      end do
     STOP_TIMER("ATOMVEC:resolve_bases")
      CHECK
   end subroutine

   subroutine resolve_bases_1(self,basis,suffix)
    ATOMVEC(:) :: self
   ! Resolve the basis sets for each atom -- by pointer assigning its basis to
   ! the element in "basis" which matches either the atoms .basis_label, or else
   ! matches a label constructed in a standard way, by joining the atom chemical
   ! symbol with the ":suffix" string.  
      SLATERBASISVEC(:), PTR :: basis
      STR(*), optional :: suffix
      INT :: a
      STACK("ATOMVEC:resolve_bases_1")
      START_TIMER("ATOMVEC:resolve_bases_1")
      do a = 1,n_atom_(self)
         call resolve_basis_(self(a),basis,suffix)
      end do
     STOP_TIMER("ATOMVEC:resolve_bases_1")
      CHECK
   end subroutine

   subroutine resolve_bases_2(self,basis,suffix)
    ATOMVEC(:) :: self
   ! Resolve the basis sets for each atom -- by pointer assigning its basis to
   ! the element in "basis" which matches either the atoms .basis_label, or else
   ! matches a label constructed in a standard way, by joining the atom chemical
   ! symbol with the ":suffix" string.  
      COPPENSBASISVEC(:), PTR :: basis
      STR(*), optional :: suffix
      INT :: a
      STACK("ATOMVEC:resolve_bases_2")
      START_TIMER("ATOMVEC:resolve_bases_2")
      do a = 1,n_atom_(self)
         call resolve_basis_(self(a),basis,suffix)
      end do
     STOP_TIMER("ATOMVEC:resolve_bases_2")
      CHECK
   end subroutine

   subroutine resolve_axis_system(self,crystal)
    ATOMVEC(:) :: self
   ! Change the atom axis systems to cartesian, from crystal, if required.
      CRYSTAL, IN :: crystal
       INT :: a
      STACK("ATOMVEC:resolve_axis_system")
      START_TIMER("ATOMVEC:resolve_axis_system")
      do a = 1,n_atom_(self)
         call resolve_axis_system_(self(a),crystal)
      end do
     STOP_TIMER("ATOMVEC:resolve_axis_system")
      CHECK
   end subroutine

   subroutine change_axis_system_to(self,axiskind,crystal)
    ATOMVEC(:) :: self
   ! Change the axis system "axiskind" for all atoms to or from "cartesian" and
   ! "crystal".
      STR(*) :: axiskind
      CRYSTAL, IN :: crystal
       INT :: a
      STACK("ATOMVEC:change_axis_system_to")
      START_TIMER("ATOMVEC:change_axis_system_to")
      do a = 1,n_atom_(self)
         call change_axis_system_to_(self(a),axiskind,crystal)
      end do
     STOP_TIMER("ATOMVEC:change_axis_system_to")
      CHECK
   end subroutine

   subroutine change_thermal_axis_system_to(self,axiskind,crystal)
    ATOMVEC(:) :: self
   ! Change the thermal tensor axis system "axiskind" for all atoms to or from
   ! "cartesian" and "crystal".
      STR(*) :: axiskind
      CRYSTAL, IN :: crystal
       INT :: a
      STACK("ATOMVEC:change_thermal_axis_system_to")
      START_TIMER("ATOMVEC:change_thermal_axis_system_to")
      do a = 1,n_atom_(self)
         call change_thermal_axis_system_to_(self(a),axiskind,crystal)
      end do
     STOP_TIMER("ATOMVEC:change_thermal_axis_system_to")
      CHECK
   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    ATOMVEC(:) :: self
   ! Output atom information, without full basis set info
      STACK("ATOMVEC:put")
      START_TIMER("ATOMVEC:put")
      call flush_(stdout)
      call show_(stdout,"Chemical Formula       =",trim(chemical_formula_(self)))
      call show_(stdout,"No of atoms            =",size(self))
      call show_(stdout,"No of electrons        =",n_e_(self))
      if (has_residue_names_(self)) then; call put_mm_info_(self)
      else;                         call put_coord_info_(self)
      end if
      if (has_restraints_(self))          call put_restraint_atoms_(self)
     STOP_TIMER("ATOMVEC:put")
      CHECK
   end subroutine

   subroutine put_coord_info(self,all)
    ATOMVEC(:) :: self
   ! Output atom coordinate information, including bond lengths, angles,
   ! without full basis set info
     BIN, IN, optional :: all
      STACK("ATOMVEC:put_coord_info")
      START_TIMER("ATOMVEC:put_coord_info")
      call put_coordinates_(self)
      call put_bond_length_table_(self,all)
      call put_bond_angle_table_(self,all)
      call put_torsion_angle_table_(self,all)
     STOP_TIMER("ATOMVEC:put_coord_info")
      CHECK
   end subroutine

   subroutine put_coordinates(self)
    ATOMVEC(:) :: self
   ! Output the atom coordinate information
      STACK("ATOMVEC:put_coordinates")
      START_TIMER("ATOMVEC:put_coordinates")
      if (bases_are_all_labeled_(self) OR coppensbases_are_all_labeled_(self)) then
        call put_coords_with_basis_label_(self)
      else
        call put_coords_without_basis_label_(self)
      end if
     STOP_TIMER("ATOMVEC:put_coordinates")
      CHECK
   end subroutine

   subroutine put_coords_with_basis_label(self)
    ATOMVEC(:) :: self
   ! Output the atom coordinates information, including the basis label,
   ! but not the entire basis set.
      target :: self
      INT :: i
      STR(STR_SIZE) :: label
      BIN :: coppens
      STACK("ATOMVEC:put_coords_with_basis_label")
      START_TIMER("ATOMVEC:put_coords_with_basis_label")
      ENSURE(bases_are_all_labeled_(self) OR coppensbases_are_all_labeled_(self),"ATOMVEC:put_coords_with_basis_label ... no bases")
      coppens = coppensbases_are_all_labeled_(self)
      call flush_(stdout)
      call text_(stdout,"Atom list information:")
      call dash_(stdout,int_fields=3,real_fields=4)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"ID",int_width=TRUE)
      call put_(stdout,"Z",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"Basis")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=4)
      do i = 1,n_atom_(self)
         call put_(stdout,i)
         call put_(stdout,self(i)%label,int_width=TRUE)
         call put_(stdout,self(i)%atomic_number)
         call put_(stdout,self(i)%pos(1))
         call put_(stdout,self(i)%pos(2))
         call put_(stdout,self(i)%pos(3))
         if (coppens) then; label = self(i)%coppensbasis%label
         else;              label = self(i)%basis%label
         end if
         call put_(stdout,label)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=4)
     STOP_TIMER("ATOMVEC:put_coords_with_basis_label")
      CHECK
   end subroutine

   subroutine put_coords_without_basis_label(self)
    ATOMVEC(:) :: self
   ! Output the atom coordinates information, without basis set label
       INT :: i
      STACK("ATOMVEC:put_coords_without_basis_label")
      START_TIMER("ATOMVEC:put_coords_without_basis_label")
      call flush_(stdout)
      call text_(stdout,"Atom list information:")
      call dash_(stdout,int_fields=3,real_fields=3)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"ID",int_width=TRUE)
      call put_(stdout,"Z",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      do i = 1,n_atom_(self)
         call put_(stdout,i)
         call put_(stdout,self(i)%label,int_width=TRUE)
         call put_(stdout,self(i)%atomic_number)
         call put_(stdout,self(i)%pos(1))
         call put_(stdout,self(i)%pos(2))
         call put_(stdout,self(i)%pos(3))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=3)
     STOP_TIMER("ATOMVEC:put_coords_without_basis_label")
      CHECK
   end subroutine

   function list_symbol(self,a) result(res)
    ATOMVEC(:) :: self
   ! Return the chemical symbol and atomvec number of atom "a".
   ! e.g., "Na (3)"
     INT :: a
     STR(STR_SIZE) :: res
     STACK("ATOMVEC:list_symbol")
     START_TIMER("ATOMVEC:list_symbol")
     res =  trim(chemical_symbol_(self(a))) // " (" // trim(to_str_(a)) // ")"
     STOP_TIMER("ATOMVEC:list_symbol")
      CHECK
   end function

   subroutine put_bond_length_table(self,all)
    ATOMVEC(:) :: self
   ! Output the bond length table. If "all" is present and true, put out all
   ! possible bond lengths
      BIN, IN, optional :: all
      BIN :: print_all_bonds
      INTVECVEC(:), PTR :: atom_kind
      INT :: n_k,k,l,kk,ll,a,b
      STR(STR_SIZE) :: symbol_a,symbol_b
      REAL :: r_ab
      STACK("ATOMVEC:put_bond_length_table")
      START_TIMER("ATOMVEC:put_bond_length_table")
      print_all_bonds = FALSE
      if (present(all)) print_all_bonds = all
      if (n_atom_(self)<2) then; STOP_TIMER("ATOMVEC:put_bond_length_table") CHECK return; end if
      if (no_of_bonds_(self)<1 AND NOT print_all_bonds) then; STOP_TIMER("ATOMVEC:put_bond_length_table") CHECK return; end if
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      call flush_(stdout)
      call text_(stdout,"Bond lengths:",flush=2)
      call show_(stdout,"No. of independent bonds  =",no_of_bonds_(self))
      call flush_(stdout)
      call dash_(stdout,int_fields=2,real_fields=2)
      call put_(stdout,"Atom_a",int_width=TRUE)
      call put_(stdout,"Atom_b",int_width=TRUE)
      call put_(stdout,"r_ab/au")
      call put_(stdout,"r_ab/Angstrom",flush=1)
      call dash_(stdout,int_fields=2,real_fields=2)
      do k = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         a = atom_kind(k)%element(kk)
         symbol_a = list_symbol_(self,a)
         do l = 1,n_k
         do ll = 1,size(atom_kind(l)%element)
            b = atom_kind(l)%element(ll)
            symbol_b = list_symbol_(self,b)
            if (a>=b) cycle
            if (NOT print_all_bonds AND NOT bonded_(self,a,b)) cycle
            r_ab = bond_distance_(self,a,b)
            call put_(stdout,symbol_a,int_width=TRUE)
            call put_(stdout,symbol_b,int_width=TRUE)
            call put_(stdout,r_ab)
            call put_(stdout,r_ab*ANGSTROM_PER_BOHR)
            call flush_(stdout)
         end do
         end do
      end do
      end do
      call destroy_(atom_kind)
      call dash_(stdout,int_fields=2,real_fields=2)
     STOP_TIMER("ATOMVEC:put_bond_length_table")
      CHECK
   end subroutine

   subroutine put_bond_angle_table(self,all)
    ATOMVEC(:) :: self
   ! Output the bond length table. If "all" is present and true, put out all
   ! possible angles
      BIN, IN, optional :: all
      BIN :: print_all_angles
      INTVECVEC(:), PTR :: atom_kind
      INT :: n_k,k,l,m,kk,ll,mm,a,b,c
      STR(STR_SIZE) :: symbol_a,symbol_b,symbol_c
      REAL :: theta_abc
      STACK("ATOMVEC:put_bond_angle_table")
      START_TIMER("ATOMVEC:put_bond_angle_table")
      print_all_angles = FALSE
      if (present(all)) print_all_angles = all
      if (n_atom_(self)<3 ) then; STOP_TIMER("ATOMVEC:put_bond_angle_table") CHECK return; end if
      if (no_of_angles_(self)<1 AND NOT print_all_angles) then; STOP_TIMER("ATOMVEC:put_bond_angle_table") CHECK return; end if
      if (no_of_angles_(self)>100 AND NOT print_all_angles) then; STOP_TIMER("ATOMVEC:put_bond_angle_table") CHECK return; end if
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      call flush_(stdout)
      call text_(stdout,"Bond angles (b the central atom):",flush=2)
      call show_(stdout,"No. of independent angles =",no_of_angles_(self))
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=1)
      call put_(stdout,"Atom_a",int_width=TRUE)
      call put_(stdout,"Atom_b",int_width=TRUE)
      call put_(stdout,"Atom_c",int_width=TRUE)
      call put_(stdout,"Angle_abc/degrees",flush=1)
      call dash_(stdout,int_fields=3,real_fields=1)
      do k = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         a = atom_kind(k)%element(kk)
         symbol_a = list_symbol_(self,a)
         do l = 1,n_k
         do ll = 1,size(atom_kind(l)%element)
            b = atom_kind(l)%element(ll)
            if (a==b) cycle
            if (NOT print_all_angles AND NOT bonded_(self,a,b)) cycle
            symbol_b = list_symbol_(self,b)
            do m = 1,n_k
            do mm = 1,size(atom_kind(m)%element)
               c = atom_kind(m)%element(mm)
               if (b==c OR c==a) cycle
               if (NOT print_all_angles AND NOT bonded_(self,b,c)) cycle
               symbol_c = list_symbol_(self,c)
               theta_abc = bond_angle_(self,a,b,c,degrees=TRUE)
               call put_(stdout,symbol_a,int_width=TRUE)
               call put_(stdout,symbol_b,int_width=TRUE)
               call put_(stdout,symbol_c,int_width=TRUE)
               call put_(stdout,theta_abc)
               call flush_(stdout)
            end do
            end do
         end do
         end do
      end do
      end do
      call destroy_(atom_kind)
      call dash_(stdout,int_fields=3,real_fields=1)
     STOP_TIMER("ATOMVEC:put_bond_angle_table")
      CHECK
   end subroutine

   subroutine put_torsion_angle_table(self,all)
    ATOMVEC(:) :: self
   ! Output the torsion angle table. If "all" is present and true, put out all
   ! possible angles
      BIN, IN, optional :: all
      BIN :: print_all_angles,abc_colinear,bcd_colinear
      INTVECVEC(:), PTR :: atom_kind
      INT :: n_k,k,l,m,n,kk,ll,mm,nn,a,b,c,d
      STR(STR_SIZE) :: symbol_a,symbol_b,symbol_c,symbol_d
      REAL :: theta_abcd
      STACK("ATOMVEC:put_torsion_angle_table")
      START_TIMER("ATOMVEC:put_torsion_angle_table")
      print_all_angles = FALSE
      if (present(all)) print_all_angles = all
      if (n_atom_(self)<4) then; STOP_TIMER("ATOMVEC:put_torsion_angle_table") CHECK return; end if
      if (no_of_torsion_angles_(self)<1 AND NOT print_all_angles) then; STOP_TIMER("ATOMVEC:put_torsion_angle_table") CHECK return; end if
      if (no_of_angles_(self)>100 AND NOT print_all_angles) then; STOP_TIMER("ATOMVEC:put_torsion_angle_table") CHECK return; end if
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      call flush_(stdout)
      call text_(stdout,"Torsion angles (looking down b->c):",flush=2)
      call show_(stdout,"No. of independent angles =",no_of_torsion_angles_(self))
      call flush_(stdout)
      call dash_(stdout,int_fields=4,real_fields=1)
      call put_(stdout,"Atom_a",int_width=TRUE)
      call put_(stdout,"Atom_b",int_width=TRUE)
      call put_(stdout,"Atom_c",int_width=TRUE)
      call put_(stdout,"Atom_d",int_width=TRUE)
      call put_(stdout,"Angle_abcd/degrees",flush=1)
      call dash_(stdout,int_fields=4,real_fields=1)
      do k = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         a = atom_kind(k)%element(kk)
         symbol_a = list_symbol_(self,a)
         do l = 1,n_k
         do ll = 1,size(atom_kind(l)%element)
            b = atom_kind(l)%element(ll)
            if (a==b) cycle
            if (NOT print_all_angles AND NOT bonded_(self,a,b)) cycle
            symbol_b = list_symbol_(self,b)
            do m = 1,n_k
            do mm = 1,size(atom_kind(m)%element)
               c = atom_kind(m)%element(mm)
               if (a==c OR b==c) cycle
               if (NOT print_all_angles AND NOT bonded_(self,b,c)) cycle
               symbol_c = list_symbol_(self,c)
               do n = 1,n_k
               do nn = 1,size(atom_kind(n)%element)
                  d = atom_kind(n)%element(nn)
                  if (NOT print_all_angles AND NOT bonded_(self,c,d)) cycle
                  if (a==d OR b==d OR c==d) cycle
                  symbol_d = list_symbol_(self,d)
                  theta_abcd = torsion_angle_(self,a,b,c,d,abc_colinear, &
                                            bcd_colinear,degrees=TRUE)
                  call put_(stdout,symbol_a,int_width=TRUE)
                  call put_(stdout,symbol_b,int_width=TRUE)
                  call put_(stdout,symbol_c,int_width=TRUE)
                  call put_(stdout,symbol_d,int_width=TRUE)
                  if (abc_colinear AND bcd_colinear) then
                     call put_(stdout,"a-b-c-d colinear")
                  else if (abc_colinear) then
                     call put_(stdout,"a-b-c colinear")
                  else if (bcd_colinear) then
                     call put_(stdout,"b-c-d colinear")
                  else
                     call put_(stdout,theta_abcd)
                  end if
                  call flush_(stdout)
               end do
               end do
            end do
            end do
         end do
         end do
      end do
      end do
      call destroy_(atom_kind)
      call dash_(stdout,int_fields=4,real_fields=1)
     STOP_TIMER("ATOMVEC:put_torsion_angle_table")
      CHECK
   end subroutine

   subroutine put_thermal_tensors(self)
    ATOMVEC(:) :: self
   ! Output the thermal tensors.
      INT :: i
      STACK("ATOMVEC:put_thermal_tensors")
      START_TIMER("ATOMVEC:put_thermal_tensors")
      call flush_(stdout)
      call text_(stdout,"Thermal tensors in cartesian coordinates/(bohr^2):")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"U11")
      call put_(stdout,"U22")
      call put_(stdout,"U33")
      call put_(stdout,"U12")
      call put_(stdout,"U13")
      call put_(stdout,"U23")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      do i = 1,size(self)
        call put_(stdout,i)
        call put_thermal_tensor_(self(i))
        call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=6)
      call flush_(stdout)
     STOP_TIMER("ATOMVEC:put_thermal_tensors")
      CHECK
   end subroutine

   subroutine put_mm_info(self)
    ATOMVEC(:) :: self
   ! Output a table of the residue names, sequence numbers, charges, but
   ! no basis sets. This is in PDB input format.
      INT :: i
      STACK("ATOMVEC:put_mm_info")
      START_TIMER("ATOMVEC:put_mm_info")
      call flush_(stdout)
      call text_(stdout,"Residue information:",flush=1)
      call dash_(stdout,width=42)
      call put_(stdout," ",width=4)
      call put_(stdout,"#",width=7)
      call put_(stdout,"Name",width=4)
      call put_(stdout,"Residue",width=5)
      call put_(stdout,"Sequence",width=6)
      call put_(stdout,"x",width=3)
      call put_(stdout,"y",width=3)
      call put_(stdout,"z",width=3)
      call put_(stdout,"Charge",width=3)
      call put_(stdout,"Element",width=4)
      call flush_(stdout)
      call dash_(stdout,width=42)
      call dash_(stdout,int_fields=6,real_fields=5)
      do i = 1,size(self)
         call put_(stdout,"ATOM",width=4)
         call put_(stdout,i,width=7)
         call put_(stdout,self(i)%residue_atom_name,width=4)
         call put_(stdout,self(i)%residue_name,width=5)
         call put_(stdout,self(i)%sequence_number,width=6)
         call put_(stdout,self(i)%pos(1),width=8,precision=3)
         call put_(stdout,self(i)%pos(2),width=8,precision=3)
         call put_(stdout,self(i)%pos(3),width=8,precision=3)
         call put_(stdout,self(i)%mm_charge,width=7,precision=3)
         call put_(stdout,self(i)%label,width=4)
         call flush_(stdout)
      end do
      call dash_(stdout,width=42)
     STOP_TIMER("ATOMVEC:put_mm_info")
      CHECK
   end subroutine

   subroutine put_restraint_atoms(self)
    ATOMVEC(:) :: self
   ! Output a table of the atom names, residue names and restraint atom
   ! information
      INT :: i
      STACK("ATOMVEC:put_restraint_atoms")
      START_TIMER("ATOMVEC:put_restraint_atoms")
      call text_(stdout,"Restraint atoms:",flush=1)
      call save_(stdout)
      call set_int_width_(stdout,9)
      call set_real_width_(stdout,9)
      call set_real_precision_(stdout,3)
      call dash_(stdout,int_fields=5,real_fields=3)
      call put_(stdout," ",int_width=TRUE)
      call put_(stdout,"Atom",int_width=TRUE)
      call put_(stdout,"Residue",int_width=TRUE)
      call put_(stdout,"Sequence")
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"Force",int_width=TRUE)
      call flush_(stdout)
      call dash_(stdout,int_fields=5,real_fields=3)
      do i = 1,size(self)
         if (is_zero_(self(i)%restraining_force_constant)) cycle
         call put_(stdout," ",int_width=TRUE)
         call put_(stdout,self(i)%residue_atom_name,int_width=TRUE)
         call put_(stdout,self(i)%residue_name,int_width=TRUE)
         call put_(stdout,self(i)%sequence_number)
         call put_(stdout,self(i)%restraining_position(1))
         call put_(stdout,self(i)%restraining_position(2))
         call put_(stdout,self(i)%restraining_position(3))
         call put_(stdout,self(i)%restraining_force_constant)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=5,real_fields=3)
      call unsave_(stdout)
     STOP_TIMER("ATOMVEC:put_restraint_atoms")
      CHECK
   end subroutine

   subroutine put_vrml(self,out)
    ATOMVEC(:) :: self
   ! Put vrml version of the atomvec to the file in object "out".
     TEXTFILE :: out
     STACK("ATOMVEC:put_vrml")
     START_TIMER("ATOMVEC:put_vrml")
     call set_real_precision_(out,5)
     call set_real_width_(out,12)

     call text_(stdout,"Generating VRML atoms...")
     call put_vrml_header_(self,out)
     call put_vrml_atoms_(self,out)
     call put_vrml_bonds_(self,out)
     call text_(stdout,"done VRML atoms")
     call flush_(stdout)
     STOP_TIMER("ATOMVEC:put_vrml")
      CHECK
   end subroutine

   subroutine put_vrml_header(self,out)
    ATOMVEC(:) :: self
   ! Put vrml header, and prototype bond and spheres, to the file.
     TEXTFILE :: out
     STACK("ATOMVEC:put_vrml_header")
     START_TIMER("ATOMVEC:put_vrml_header")
     call text_(out,"PROTO Atom " // achar(91))
     call text_(out,"   field SFColor col 1 0 0")
     call text_(out,"   field SFFloat rad 1")
     call text_(out,"   field SFVec3f pos 0 0 0")
     call text_(out,achar(93))
     call text_(out,"{")
     call text_(out,"   Transform {")
     call text_(out,"      translation IS pos")
     call text_(out,"      children " // achar(91))
     call text_(out,"         Shape {")
     call text_(out,"            appearance Appearance {")
     call text_(out,"               material Material {")
     call text_(out,"                  diffuseColor IS col")
     call text_(out,"               }")
     call text_(out,"            }")
     call text_(out,"            geometry Sphere {")
     call text_(out,"              radius IS rad")
     call text_(out,"            }")
     call text_(out,"         }")
     call text_(out,"      " // achar(93))
     call text_(out,"   }")
     call text_(out,"}")
     call flush_(out)
     call text_(out,"PROTO Bond " // achar(91))
     call text_(out,"   field SFColor col 1 0 0")
     call text_(out,"   field SFFloat hgt 1")
     call text_(out,"   field SFVec3f pos 0 0 0")
     call text_(out,"   field SFRotation rot 1 0 0 0")
     call text_(out,achar(93))
     call text_(out,"{")
     call text_(out,"   Transform {")
     call text_(out,"      translation IS pos")
     call text_(out,"      rotation IS rot")
     call text_(out,"      children " // achar(91))
     call text_(out,"         Shape {")
     call text_(out,"            appearance Appearance {")
     call text_(out,"               material Material {")
     call text_(out,"                 diffuseColor IS col")
     call text_(out,"               }")
     call text_(out,"            }")
     call text_(out,"            geometry Cylinder {")
     call text_(out,"               radius 0.304245979")
     call text_(out,"               height IS hgt")
     call text_(out,"               top    FALSE")
     call text_(out,"               bottom FALSE")
     call text_(out,"            }")
     call text_(out,"         }")
     call text_(out,"      " // achar(93))
     call text_(out,"   }")
     call text_(out,"}")
     call flush_(out)
     STOP_TIMER("ATOMVEC:put_vrml_header")
      CHECK
   end subroutine

   subroutine put_vrml_atoms(self,out)
    ATOMVEC(:) :: self
   ! Put spheres for the atoms to view as vrml.
     TEXTFILE :: out
     REALVEC(3) :: colour
     STR(2) :: label
     REAL :: radius
     INT :: n,Z
     STACK("ATOMVEC:put_vrml_atoms")
     START_TIMER("ATOMVEC:put_vrml_atoms")
     do n=1,n_atom_(self)
       Z = self(n)%atomic_number
       label = chemical_symbol_(self(n))
       select case (label)
         case ("H ");                     colour = (/191,196,192/)
         case ("He","Rn");                colour = (/88,196,160/)
         case ("Li","Be","Na","Mg");      colour = (/144,149,145/)
         case ("B ");                     colour = (/187,4,187/)
         case ("C ");                     colour = (/160,80,17/)
         case ("N ","Al");                colour = (/126,169,176/)
         case ("Si");                     colour = (/192,172,137/)
         case ("S ");                     colour = (/192,165,0/)
         case ("Cl");                     colour = (/0,165,0/)
         case ("O ","Ca","Ge","As");      colour = (/192,12,8/)
         case ("Se","Br","Sr","I ");      colour = (/192,12,8/)
         case default;                    colour = (/192,148,25/)
       end select
       select case (Z)
         case (1:54);     radius = bragg_slater_radius_(self(n))
         case default;    radius = 1.30d0
       end select
       colour = colour / 256
       call text_(out,"Atom {")
       call text_(out," col " // trim(to_str_(colour,"f15.8",separator=", ")))
       call text_(out," pos " // trim(to_str_(self(n)%pos,"f15.8",separator=", ")))
       call text_(out," rad " // trim(to_str_(radius,"f15.8")))
       call text_(out,"}")
     end do
     STOP_TIMER("ATOMVEC:put_vrml_atoms")
      CHECK
   end subroutine

   subroutine put_vrml_bonds(self,out)
    ATOMVEC(:) :: self
   ! Put bonds for the atoms to view as vrml.
     TEXTFILE :: out
     REALVEC(3) :: col_a,col_b,posa,posb,pos1,pos2,AB,centre
     REALVEC(4) :: rot
     REAL :: hgta,hgtb,rada,radb
     INT :: a,b
     STACK("ATOMVEC:put_vrml_bonds")
     START_TIMER("ATOMVEC:put_vrml_bonds")
     do a = 1,n_atom_(self)
       do b = 1,a-1
         if (bonded_(self,a,b)) then
           posa = self(a)%pos
           posb = self(b)%pos
           AB = posb-posa
           call normalise_(AB)
           rada = bragg_slater_radius_(self(a))
           radb = bragg_slater_radius_(self(b))
           centre = HALF*(posa+rada*AB + posb-radb*AB)
           hgta = norm_((centre - posa))
           hgtb = norm_((posb - centre))
           pos1 = HALF*(centre + posa)
           pos2 = HALF*(centre + posb)
           rot(4) = PI
           rot(1:3) = AB + (/ZERO,ONE,ZERO/)
           call normalise_(rot(1:3))
           select case (chemical_symbol_(self(a)))
             case ("H ");                     col_a = (/191,196,192/)
             case ("He","Rn");                col_a = (/88,196,160/)
             case ("Li","Be","Na","Mg");      col_a = (/144,149,145/)
             case ("B ");                     col_a = (/187,4,187/)
             case ("C ");                     col_a = (/160,80,17/)
             case ("N ","Al");                col_a = (/126,169,176/)
             case ("Si");                     col_a = (/192,172,137/)
             case ("S ");                     col_a = (/192,165,0/)
             case ("Cl");                     col_a = (/0,165,0/)
             case ("O ","Ca","Ge","As");      col_a = (/192,12,8/)
             case ("Se","Br","Sr","I ");      col_a = (/192,12,8/)
             case default;                    col_a = (/192,148,25/)
           end select
           select case (chemical_symbol_(self(b)))
             case ("H ");                     col_b = (/191,196,192/)
             case ("He","Rn");                col_b = (/88,196,160/)
             case ("Li","Be","Na","Mg");      col_b = (/144,149,145/)
             case ("B ");                     col_b = (/187,4,187/)
             case ("C ");                     col_b = (/160,80,17/)
             case ("N ","Al");                col_b = (/126,169,176/)
             case ("Si");                     col_b = (/192,172,137/)
             case ("S ");                     col_b = (/192,165,0/)
             case ("Cl");                     col_b = (/0,165,0/)
             case ("O ","Ca","Ge","As");      col_b = (/192,12,8/)
             case ("Se","Br","Sr","I ");      col_b = (/192,12,8/)
             case default;                    col_b = (/192,148,25/)
           end select
           col_a = col_a / 256
           col_b = col_b / 256
           call text_(out,"Bond {")
           call text_(out," col " //  trim(to_str_(col_a,"f15.8",separator=", ")))
           call text_(out," pos " //  trim(to_str_(pos1,"f15.8",separator=", ")))
           call text_(out," rot " //  trim(to_str_(rot,"f15.8",separator=", ")))
           call text_(out," hgt " //  trim(to_str_(hgta,"f15.8")))
           call text_(out,"}")
           call text_(out,"Bond {")
           call text_(out," col " //  trim(to_str_(col_b,"f15.8",separator=", ")))
           call text_(out," pos " //  trim(to_str_(pos2,"f15.8",separator=", ")))
           call text_(out," rot " //  trim(to_str_(rot,"f15.8",separator=", ")))
           call text_(out," hgt " //  trim(to_str_(hgtb,"f15.8")))
           call text_(out,"}")
         end if
       end do
     end do
     STOP_TIMER("ATOMVEC:put_vrml_bonds")
      CHECK
   end subroutine

!  *************************
!  Geometry unit conversions
!  *************************

   subroutine convert_from_angstrom(self)
    ATOMVEC(:) :: self
   ! Convert atom positions to A.U. from Angstroms
       INT :: a
      STACK("ATOMVEC:convert_from_angstrom")
      START_TIMER("ATOMVEC:convert_from_angstrom")
      do a = 1,n_atom_(self)
         self(a)%pos = self(a)%pos*BOHR_PER_ANGSTROM
      end do
     STOP_TIMER("ATOMVEC:convert_from_angstrom")
      CHECK
   end subroutine

   subroutine convert_to_angstrom(self)
    ATOMVEC(:) :: self
   ! Convert atom positions to Angstroms from A.U.
       INT :: a
      STACK("ATOMVEC:convert_to_angstrom")
      START_TIMER("ATOMVEC:convert_to_angstrom")
      do a = 1,n_atom_(self)
         self(a)%pos = self(a)%pos*ANGSTROM_PER_BOHR
      end do
     STOP_TIMER("ATOMVEC:convert_to_angstrom")
      CHECK
   end subroutine

   subroutine convert_from_crystal(self,crystal)
    ATOMVEC(:) :: self
   ! Convert atom positions to A.U. from fractional crystal coordinates
      CRYSTAL :: crystal
      STACK("ATOMVEC:convert_from_crystal")
      START_TIMER("ATOMVEC:convert_from_crystal")
      call rotate_(self,crystal%unitcell%direct_matrix)
     STOP_TIMER("ATOMVEC:convert_from_crystal")
      CHECK
   end subroutine

   subroutine convert_to_crystal(self,crystal)
    ATOMVEC(:) :: self
   ! Convert atom positions to fractional crystal coordinates from A.U.
      CRYSTAL :: crystal
      STACK("ATOMVEC:convert_to_crystal")
      START_TIMER("ATOMVEC:convert_to_crystal")
      call rotate_(self,crystal%unitcell%inverse_matrix)
     STOP_TIMER("ATOMVEC:convert_to_crystal")
      CHECK
   end subroutine

   subroutine get_coordinates(self,coord)
    ATOMVEC(:) :: self
   ! Get the atom coordinates in a matrix object "coord"
      REALMAT(:,:) :: coord
       INT :: a
      STACK("ATOMVEC:get_coordinates")
      START_TIMER("ATOMVEC:get_coordinates")
      if (size(coord,1)==3) then
         do a = 1,n_atom_(self)
            coord(:,a) = self(a)%pos
         end do
      else if (size(coord,2)==3) then
         do a = 1,n_atom_(self)
            coord(a,:) = self(a)%pos
         end do
      else
         DIE("ATOMVEC:get_coordinates ... wrong shape for coordinate matrix")
      end if
     STOP_TIMER("ATOMVEC:get_coordinates")
      CHECK
   end subroutine

   subroutine get_mean_neutron_numbers(self,NN)
    ATOMVEC(:) :: self
   ! Get the atom coordinates in a matrix object "coord"
      REALVEC(:) :: NN
       INT :: a
      STACK("ATOMVEC:get_mean_neutron_numbers")
      START_TIMER("ATOMVEC:get_mean_neutron_numbers")
      do a = 1,n_atom_(self)
         NN(a) = mean_neutron_number_(self(a))
      end do
     STOP_TIMER("ATOMVEC:get_mean_neutron_numbers")
      CHECK
   end subroutine

!  *******************
!  Information methods
!  *******************

   function sum_of_atomic_numbers(self) result(res)
    ATOMVEC(:) :: self
   ! Return the sum of the atomic numbers
      REAL :: res
      STACK("ATOMVEC:sum_of_atomic_numbers")
      START_TIMER("ATOMVEC:sum_of_atomic_numbers")
      res = sum(self(:)%atomic_number)
     STOP_TIMER("ATOMVEC:sum_of_atomic_numbers")
      CHECK
   end function

   function atomic_numbers(self) result(res)
    ATOMVEC(:) :: self
   ! Return the atomic numbers as a vector
      REALVEC(size(self)) :: res
      STACK("ATOMVEC:atomic_numbers")
      START_TIMER("ATOMVEC:atomic_numbers")
      res = self(:)%atomic_number
     STOP_TIMER("ATOMVEC:atomic_numbers")
      CHECK
   end function

   function nuclear_energy(self) result(res)
    ATOMVEC(:) :: self
   ! Return the nuclear repulsion energy
      REAL :: res
      INT :: i,j,qi,qj
      REALVEC(3) :: radius
      STACK("ATOMVEC:nuclear_energy")
      START_TIMER("ATOMVEC:nuclear_energy")
      res = ZERO
      do i=1,n_atom_(self)
         qi = self(i)%atomic_number
         do j=1,i-1
            qj = self(j)%atomic_number
            radius =  self(j)%pos - self(i)%pos
            res = res + qi * qj / sqrt( dot_(radius,radius) )
         end do
      end do
     STOP_TIMER("ATOMVEC:nuclear_energy")
      CHECK
   end function

   function nuclear_energy_1(self,atoms) result(res)
    ATOMVEC(:) :: self
   ! Return the nuclear repulsion energy felt by the group of atoms "a" in
   ! the field of all the nuclei in "self"
      INTVEC(:) :: atoms
      REAL :: res
      INT :: a, i, j,qi,qj, n_atoms
      REALVEC(3) :: radius
      STACK("ATOMVEC:nuclear_energy_1")
      START_TIMER("ATOMVEC:nuclear_energy_1")
      res = ZERO
      n_atoms = size(atoms)
      do i=1,n_atoms
       a = atoms(i)
       qi = self(a)%atomic_number
       do j=1,n_atom_(self)
        if (any(atoms==j)) cycle
        qj = self(j)%atomic_number
        radius =  self(j)%pos - self(a)%pos
        res = res + qi * qj / sqrt( dot_(radius,radius) )
       end do
      end do
      res = res + nuclear_energy_(self(atoms))
     STOP_TIMER("ATOMVEC:nuclear_energy_1")
      CHECK
   end function

   function nuclear_energy_2(self,atoms,nuclei) result(res)
    ATOMVEC(:) :: self
   ! Return the nuclear repulsion energy felt by the group of atoms "a" in
   ! the field of all the nuclei in "nuclei"
      INTVEC(:) :: atoms, nuclei
      REAL :: res
      INT :: a, i, j,k,qi,qj, n_atoms, n_field
      REALVEC(3) :: radius
      STACK("ATOMVEC:nuclear_energy_2")
      START_TIMER("ATOMVEC:nuclear_energy_2")
      res = ZERO
      n_atoms = size(atoms)
      n_field = size(nuclei)
      do i=1,n_atoms
       a = atoms(i)
       qi = self(a)%atomic_number
       do k=1,n_field
        j = nuclei(k)
        if (any(atoms==j)) cycle
        qj = self(j)%atomic_number
        radius =  self(j)%pos - self(a)%pos
        res = res + qi * qj / sqrt( dot_(radius,radius) )
       end do
      end do
      res = res + nuclear_energy_(self(atoms))
     STOP_TIMER("ATOMVEC:nuclear_energy_2")
      CHECK
   end function

   function chemical_formula(self) result(res)
    ATOMVEC(:) :: self
   ! Return the chemical formula for the molecule, as a string, in alphabetical
   ! order of elements
      STR(STR_SIZE) :: res
      STRVEC(STR_SIZE,:), PTR :: symbol
      INT :: a,na
      STACK("ATOMVEC:chemical_formula")
      START_TIMER("ATOMVEC:chemical_formula")
      call create_(symbol,size(self))
      do a = 1,n_atom_(self)
         symbol(a) = chemical_symbol_(self(a))
      end do
      call quick_sort_(symbol)
      res = " "
      a = 1
      do
         na = count(symbol==symbol(a))
         res = trim(res) // trim(symbol(a))
         if (na>1) &
         res = trim(res) // trim(to_str_(na))
         a = a + na
         if (a>n_atom_(self)) exit
      end do
      call destroy_(symbol)
     STOP_TIMER("ATOMVEC:chemical_formula")
      CHECK
   end function

   function centre_of_mass(self) result(centre)
    ATOMVEC(:) :: self
   ! Return the centre of mass
      REALVEC(3) :: centre
      INT :: a
      REAL :: mw
      STACK("ATOMVEC:centre_of_mass")
      START_TIMER("ATOMVEC:centre_of_mass")
      mw = ONE/molecular_weight_(self)
      centre = ZERO
      do a = 1,n_atom_(self)
         centre = centre + self(a)%pos*mass_(self(a))*mw
      end do
     STOP_TIMER("ATOMVEC:centre_of_mass")
      CHECK
   end function

   subroutine move_origin_to_centre_of_mass(self)
    ATOMVEC(:) :: self
   ! Move the origin to the centre of mass
      REALVEC(3) :: com
      STACK("ATOMVEC:move_origin_to_centre_of_mass")
      START_TIMER("ATOMVEC:move_origin_to_centre_of_mass")
      com = centre_of_mass_(self)
      call translate_(self,-com)
     STOP_TIMER("ATOMVEC:move_origin_to_centre_of_mass")
      CHECK
   end subroutine

   function reduced_mass(self) result(mu)
    ATOMVEC(:) :: self
   ! Return the centre of mass
      REAL :: mu
      INT :: a
      STACK("ATOMVEC:reduced_mass")
      START_TIMER("ATOMVEC:reduced_mass")
      mu = ZERO
      do a = 1,n_atom_(self)
         mu = mu + ONE / mass_(self(a))
      end do
      mu = ONE/mu
     STOP_TIMER("ATOMVEC:reduced_mass")
      CHECK
   end function

   subroutine make_inertia_tensor(self,it)
    ATOMVEC(:) :: self
   ! Make the moment of inertia tensor wrt the centre of mass
      REALMAT(3,3) :: it
      REALMAT(3,3) :: m
      REALVEC(3) :: com,r
      REAL :: trace
      INT :: a
      STACK("ATOMVEC:make_inertia_tensor")
      START_TIMER("ATOMVEC:make_inertia_tensor")
      com = centre_of_mass_(self)
      it = ZERO
      do a = 1, n_atom_(self)
         r = self(a)%pos - com
         m = spread(r,dim=1,ncopies=3)*spread(r,dim=2,ncopies=3)
         m = mass_(self(a)) * m
         trace = trace_(m)
         m = -m
         call add_to_diagonal_(m,trace)
         it = it + m
      end do
     STOP_TIMER("ATOMVEC:make_inertia_tensor")
      CHECK
   end subroutine

   subroutine make_principal_moments(self,pm,pa)
    ATOMVEC(:) :: self
   ! Make the principal moments "pm" and principal axes "pm" wrt the centre of
   ! mass.  The principal axes are made to be right handed.
   ! - For sperical tops, the local x,y,z axes are the pricipal axes
   ! - For symmetric tops, the C axis is unique
   ! - For asymmetric tops, the pricipal axes are aligned close to the local
   ! x,y,z axes
      REALVEC(3) :: pm
      REALMAT(3,3) :: pa
      REALMAT(3,3) :: it
      STACK("ATOMVEC:make_principal_moments")
      START_TIMER("ATOMVEC:make_principal_moments")
      call make_inertia_tensor_(self,it)
      call solve_eigenproblem_(it,pm,pa)
      where (pm<TOL(6))
        pm = ZERO                               ! Small moments set to zero
      end where
      if (is_spherical_top_(self,pm)) then           ! For spherical tops, principal
            call to_unit_matrix_(pa)                   ! axes are x,y,z
      else if (is_symmetric_top_(self,pm)) then      ! For symmetric tops C axis is
         if (abs(pm(1)-pm(3)) < TOL(6)) then    ! unique
            call swap_columns_(pa,2,3)
            call swap_elements_(pm,2,3)
         else if (abs(pm(2)-pm(3)) < TOL(6)) then
            call swap_columns_(pa,1,3)
            call swap_elements_(pm,1,3)
         end if
      else                                      ! For asymmetric tops, principal
         if (abs(pa(1,2)) > abs(pa(1,1))) then  ! axes are close to x,y,z
            call swap_columns_(pa,1,2)
            call swap_elements_(pm,1,2)
         end if
         if (abs(pa(1,3)) > abs(pa(1,1))) then
            call swap_columns_(pa,1,3)
            call swap_elements_(pm,1,3)
         end if
         if (abs(pa(2,3)) > abs(pa(2,2))) then
            call swap_columns_(pa,2,3)
            call swap_elements_(pm,2,3)
         end if
      end if
      if (determinant_(pa)>ZERO) then; STOP_TIMER("ATOMVEC:make_principal_moments") CHECK return; end if! Ensure principal axes are right handed
      if (abs(pm(1)-pm(2)) < TOL(6)) then
         call swap_elements_(pm,1,2)
         call swap_columns_(pa,1,2)
      else if (abs (pm(2)-pm(3)) < TOL(6)) then
         call swap_elements_(pm,2,3)
         call swap_columns_(pa,2,3)
      else
         pa(1:3,3) = -pa(1:3,3)
      end if
     STOP_TIMER("ATOMVEC:make_principal_moments")
      CHECK
   end subroutine

   function is_linear(self,pm) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the geometry is linear.
   ! Needs principal moments of inertia "pm"
      BIN :: res
      REALVEC(3) :: pm
      STACK("ATOMVEC:is_linear")
      START_TIMER("ATOMVEC:is_linear")
      res = any(pm==ZERO)
     STOP_TIMER("ATOMVEC:is_linear")
      CHECK
   end function

   function is_spherical_top(self,pm) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the geometry is a spherical top
   ! Needs principal moments of inertia "pm"
      BIN :: res
      REALVEC(3) :: pm
      STACK("ATOMVEC:is_spherical_top")
      START_TIMER("ATOMVEC:is_spherical_top")
      res = no_of_same_principal_moments_(self,pm)==3
     STOP_TIMER("ATOMVEC:is_spherical_top")
      CHECK
   end function

   function is_symmetric_top(self,pm) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the geometry is a symmetric top
   ! Needs principal moments of inertia "pm"
      BIN :: res
      REALVEC(3) :: pm
      STACK("ATOMVEC:is_symmetric_top")
      START_TIMER("ATOMVEC:is_symmetric_top")
      res = no_of_same_principal_moments_(self,pm)==1
     STOP_TIMER("ATOMVEC:is_symmetric_top")
      CHECK
   end function

   function is_prolate_top(self,pm) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the geometry is a prolate top
   ! Needs principal moments of inertia "pm" after alignment
      BIN :: res
      REALVEC(3) :: pm
      STACK("ATOMVEC:is_prolate_top")
      START_TIMER("ATOMVEC:is_prolate_top")
      res = is_symmetric_top_(self,pm) AND (pm(3)<pm(1))
     STOP_TIMER("ATOMVEC:is_prolate_top")
      CHECK
   end function

   function is_oblate_top(self,pm) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the geometry is a oblate top
   ! Needs principal moments of inertia "pm" after alignment
      BIN :: res
      REALVEC(3) :: pm
      STACK("ATOMVEC:is_oblate_top")
      START_TIMER("ATOMVEC:is_oblate_top")
      res = is_symmetric_top_(self,pm) AND (pm(3)>pm(1))
     STOP_TIMER("ATOMVEC:is_oblate_top")
      CHECK
   end function

   function is_asymmetric_top(self,pm) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if the geometry is a asymmetric top
   ! Needs principal moments of inertia "pm"
      BIN :: res
      REALVEC(3) :: pm
      STACK("ATOMVEC:is_asymmetric_top")
      START_TIMER("ATOMVEC:is_asymmetric_top")
      res = no_of_same_principal_moments_(self,pm)==0
     STOP_TIMER("ATOMVEC:is_asymmetric_top")
      CHECK
   end function

   function no_of_same_principal_moments(self,pm) result(same)
    ATOMVEC(:) :: self
   ! Return the number of "same" pairs of principal moments of inertia.
   ! Needs principal moments of inertia "pm"
      INT :: same
      REALVEC(3) :: pm
      INT :: i,j
      STACK("ATOMVEC:no_of_same_principal_moments")
      START_TIMER("ATOMVEC:no_of_same_principal_moments")
      same = 0
      do i = 1,3
      do j = 1,i-1
         if (abs(pm(i)-pm(j))<=TOL(6)) then
            same = same + 1
         end if
      end do
      end do
     STOP_TIMER("ATOMVEC:no_of_same_principal_moments")
      CHECK
   end function

   function has_sequence_numbers(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if any atom in the list has a non zero sequence number
      BIN :: res
      STACK("ATOMVEC:has_sequence_numbers")
      START_TIMER("ATOMVEC:has_sequence_numbers")
      res = any(self(:)%sequence_number > 1)
     STOP_TIMER("ATOMVEC:has_sequence_numbers")
      CHECK
   end function

   function has_residue_names(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if any atom in the list has a residue name different from "UNK"
      BIN :: res
      STACK("ATOMVEC:has_residue_names")
      START_TIMER("ATOMVEC:has_residue_names")
      res = any(self(:)%residue_name /= "UNK" )
     STOP_TIMER("ATOMVEC:has_residue_names")
      CHECK
   end function

   function has_restraints(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if any atom in the list has a restrained position
   ! or restrained force constants
      BIN :: res
      STACK("ATOMVEC:has_restraints")
      START_TIMER("ATOMVEC:has_restraints")
      res = any(self(:)%restraining_force_constant/=ZERO)
     STOP_TIMER("ATOMVEC:has_restraints")
      CHECK
   end function

   function molecular_weight(self) result(res)
    ATOMVEC(:) :: self
   ! Return the molceular weight for this atomvec
      REAL :: res
      INT :: a
      STACK("ATOMVEC:molecular_weight")
      START_TIMER("ATOMVEC:molecular_weight")
      res = ZERO
      do a = 1,n_atom_(self)
         res = res + mass_(self(a))
      end do
     STOP_TIMER("ATOMVEC:molecular_weight")
      CHECK
   end function

   function centre_of_atoms(self,axes) result(centre)
    ATOMVEC(:) :: self
   ! Return the centroid of the atom positions in "centre". If "axes" is present
   ! then the "centre" is expressed with respect to the new "axes", where the
   ! columns of "axes" are the coordinates of the new axes in terms of the old.
      REALVEC(3) :: centre
      REALMAT(3,3), optional :: axes
      INT :: a
      STACK("ATOMVEC:centre_of_atoms")
      START_TIMER("ATOMVEC:centre_of_atoms")
      centre = ZERO
      do a = 1,n_atom_(self)
         centre = centre + self(a)%pos
      end do
      centre = centre/n_atom_(self)
      if (present(axes)) centre = matmul(transpose(axes),centre)
     STOP_TIMER("ATOMVEC:centre_of_atoms")
      CHECK
   end function

   function bounding_cube_width(self) result(width)
    ATOMVEC(:) :: self
   ! Return "width" which is a width of a side of a cube in which the molecule
   ! nicely sits.
   ! Suitable for generating plot widths.
      REAL :: width
      REALVEC(3) :: centre,dist
      REAL :: length
      INT :: a
      STACK("ATOMVEC:bounding_cube_width")
      START_TIMER("ATOMVEC:bounding_cube_width")
      width = ZERO
      centre = centre_of_atoms_(self)
      do a = 1,n_atom_(self)
         dist = self(a)%pos - centre
         length = norm_(dist) + bragg_slater_radius_(self(a))*BOHR_PER_ANGSTROM*TWO
         length = max(length, norm_(dist)*TWO)
         width = max(width,length)
      end do
      width = TWO*width
     STOP_TIMER("ATOMVEC:bounding_cube_width")
      CHECK
   end function

   function bounding_box(self,axes) result(box)
    ATOMVEC(:) :: self
   ! Return "box" which are three widths of a side of a box in which the molecule
   ! nicely sits. If "axes" is present, the "box" coordinates are expressed in
   ! terms of the new "axes", where the columns of "axes" are the coordinates of
   ! the new axes in terms of the old. These "axes" may be (typically) the
   ! principal moment axes. This routine is suitable for generating plot widths.
      REALMAT(3,3), optional :: axes
      REALVEC(3) :: box
      REALVEC(3) :: centre,dist
      INT :: a
      STACK("ATOMVEC:bounding_box")
      START_TIMER("ATOMVEC:bounding_box")
      box = ZERO
      centre = centre_of_atoms_(self)
      do a = 1,n_atom_(self)
         dist = self(a)%pos - centre
         dist = abs(dist)
         if (present(axes)) then
         dist = matmul(transpose(axes),dist) ! coordinates in new axis system
         dist = abs(dist)
         end if
         dist = dist + bragg_slater_radius_(self(a))*BOHR_PER_ANGSTROM*TWO
         box  = max(box,dist)
      end do
      box = FOUR*box
     STOP_TIMER("ATOMVEC:bounding_box")
      CHECK
   end function

   subroutine make_shape_tensor(self,st)
    ATOMVEC(:) :: self
   ! Make the shape tensor "st" wrt the centre of atoms. This is the same as the
   ! moment of inertia tensor except that each atom is assumed to have unit mass.
      REALMAT(3,3) :: st
      REALMAT(3,3) :: m
      REALVEC(3) :: c,r
      INT :: a
      STACK("ATOMVEC:make_shape_tensor")
      START_TIMER("ATOMVEC:make_shape_tensor")
      c = centre_of_atoms_(self)
      st = ZERO
      do a = 1, n_atom_(self)
         r = self(a)%pos - c
         m = spread(r,dim=1,ncopies=3)*spread(r,dim=2,ncopies=3)
         call add_to_diagonal_(m,-trace_(m))
         st = st - m
      end do
     STOP_TIMER("ATOMVEC:make_shape_tensor")
      CHECK
   end subroutine

   subroutine make_shape_moments(self,sm,sa)
    ATOMVEC(:) :: self
   ! Make the shape moments "sm" and principal shape axes "sa" wrt the centre of
   ! atoms. The shape axes are the same as the principal moment of inertia axes
   ! except that each atom is assumed to have unit mass. The principal shape
   ! axes are made right handed.
      REALVEC(3) :: sm
      REALMAT(3,3) :: sa
      REALMAT(3,3) :: st
      STACK("ATOMVEC:make_shape_moments")
      START_TIMER("ATOMVEC:make_shape_moments")
      call make_shape_tensor_(self,st)
      call solve_eigenproblem_(st,sm,sa)
      if (abs(sa(1,2)) > abs(sa(1,1))) then ! make axes are close to x,y,z
         call swap_columns_(sa,1,2)
         call swap_elements_(sm,1,2)
      end if
      if (abs(sa(1,3)) > abs(sa(1,1))) then
         call swap_columns_(sa,1,3)
         call swap_elements_(sm,1,3)
      end if
      if (abs(sa(2,3)) > abs(sa(2,2))) then
         call swap_columns_(sa,2,3)
         call swap_elements_(sm,2,3)
      end if
      if (sa(1,1)<ZERO)        sa(:,1) = -sa(:,1)  ! make axes +ve
      if (sa(2,2)<ZERO)        sa(:,2) = -sa(:,2)
      if (determinant_(sa)<ZERO) sa(:,3) = -sa(:,3)
     STOP_TIMER("ATOMVEC:make_shape_moments")
      CHECK
   end subroutine

   subroutine get_geometry(self,g)
    ATOMVEC(:) :: self
   ! Return the geometry "g" in a matrix
      IN :: self
      REALMAT(:,:), OUT :: g
      INT :: n_atom,n
      STACK("ATOMVEC:get_geometry")
      START_TIMER("ATOMVEC:get_geometry")
      if (size(g,1)==3 AND size(g,2)==n_atom_(self)) then
         n_atom = size(g,2)
         do n = 1,n_atom
            g(:,n) = self(n)%pos
         end do
      else if (size(g,1)==n_atom_(self) AND size(g,2)==3) then
         n_atom = size(g,1)
         do n = 1,n_atom
            g(n,:) = self(n)%pos
         end do
      else
         DIE("ATOMVEC:get_geometry ... incorrectly sized geometry array")
      end if
     STOP_TIMER("ATOMVEC:get_geometry")
      CHECK
   end subroutine

   subroutine get_geometry_vector(self,g)
    ATOMVEC(:) :: self
   ! Return the geometry "g" as a flat vector where the xyz positions increment
   ! fastest, useful for optimisations
      REALVEC(:) :: g
      INT :: k,i
      STACK("ATOMVEC:get_geometry_vector")
      START_TIMER("ATOMVEC:get_geometry_vector")
      ENSURE(size(g)==3*n_atom_(self),"ATOMVEC:get_geometry_vector ... wrong size, g")
      k = 0
      do i = 1,n_atom_(self)
        g(k+1) = self(i)%pos(1)
        g(k+2) = self(i)%pos(2)
        g(k+3) = self(i)%pos(3)
        k = k + 3
      end do
     STOP_TIMER("ATOMVEC:get_geometry_vector")
      CHECK
   end subroutine

   subroutine set_geometry_from_vector(self,g)
    ATOMVEC(:) :: self
   ! Set the geometry from "g", a flat vector, where the xyz positions
   ! increment fastest
      REALVEC(:) :: g
      INT :: k,i
      STACK("ATOMVEC:set_geometry_from_vector")
      START_TIMER("ATOMVEC:set_geometry_from_vector")
      ENSURE(size(g)==3*n_atom_(self),"ATOMVEC:set_geometry_from_vector ... wrong size, g")
      k = 0
      do i = 1,n_atom_(self)
        self(i)%pos(1) = g(k+1)
        self(i)%pos(2) = g(k+2)
        self(i)%pos(3) = g(k+3)
        k = k + 3
      end do
     STOP_TIMER("ATOMVEC:set_geometry_from_vector")
      CHECK
   end subroutine

   function geometry(self) result(g)
    ATOMVEC(:) :: self
   ! Return the geometry "g" in a (3 x .n_atom) matrix
       REALMAT(3,size(self)) :: g
      INT :: n_atom,n
      STACK("ATOMVEC:geometry")
      START_TIMER("ATOMVEC:geometry")
      n_atom = size(self)
      do n = 1,n_atom
         g(:,n) = self(n)%pos
      end do
     STOP_TIMER("ATOMVEC:geometry")
      CHECK
   end function

   function geometry_vector(self) result(g)
    ATOMVEC(:) :: self
   ! Return the geometry "g" as a flat vector where the xyz positions increment
   ! fastest, useful for optimisations
      REALVEC(3*size(self)) :: g
      INT :: k,i
      STACK("ATOMVEC:geometry_vector")
      START_TIMER("ATOMVEC:geometry_vector")
      k = 0
      do i = 1,n_atom_(self)
        g(k+1) = self(i)%pos(1)
        g(k+2) = self(i)%pos(2)
        g(k+3) = self(i)%pos(3)
        k = k + 3
      end do
     STOP_TIMER("ATOMVEC:geometry_vector")
      CHECK
   end function

   function nuclear_dipole_moment(self) result(res)
    ATOMVEC(:) :: self
   ! Return the dipole moment obtained from the nuclear charges
      REALVEC(3) :: res
       INT :: a
      STACK("ATOMVEC:nuclear_dipole_moment")
      START_TIMER("ATOMVEC:nuclear_dipole_moment")
      res = ZERO
      do a = 1,n_atom_(self)
         res = res + self(a)%atomic_number*self(a)%pos(:)
      end do
     STOP_TIMER("ATOMVEC:nuclear_dipole_moment")
      CHECK
   end function

   function nuclear_quadrupole_moment(self) result(res)
    ATOMVEC(:) :: self
   ! Return the quadrupole moment obtained from the nuclear charges
   ! as a vector, in the order: xx, yy, zz, xy, xz, yz
      REALVEC(6) :: res
       REAL :: Z
       INT :: a
      STACK("ATOMVEC:nuclear_quadrupole_moment")
      START_TIMER("ATOMVEC:nuclear_quadrupole_moment")
      res = ZERO
      do a = 1,n_atom_(self)
         Z = self(a)%atomic_number
         res(1) = res(1) + Z*self(a)%pos(1)*self(a)%pos(1)
         res(2) = res(2) + Z*self(a)%pos(2)*self(a)%pos(2)
         res(3) = res(3) + Z*self(a)%pos(3)*self(a)%pos(3)
         res(4) = res(4) + Z*self(a)%pos(1)*self(a)%pos(2)
         res(5) = res(5) + Z*self(a)%pos(1)*self(a)%pos(3)
         res(6) = res(6) + Z*self(a)%pos(2)*self(a)%pos(3)
      end do
     STOP_TIMER("ATOMVEC:nuclear_quadrupole_moment")
      CHECK
   end function

   function nuclear_octupole_moment(self) result(res)
    ATOMVEC(:) :: self
   ! Return the octupole moment obtained from the nuclear charges as a vector,
   ! in the order: xxx, yyy, zzz, xxy, xxz, yyx, yyz, zzx, zzy, xyz
      REALVEC(10) :: res
      REAL :: Z
      INT :: a
      STACK("ATOMVEC:nuclear_octupole_moment")
      START_TIMER("ATOMVEC:nuclear_octupole_moment")
      res = ZERO
      do a = 1,n_atom_(self)
         Z = self(a)%atomic_number
         res(1)  = res(1)  + Z*self(a)%pos(1)*self(a)%pos(1)*self(a)%pos(1)
         res(2)  = res(2)  + Z*self(a)%pos(2)*self(a)%pos(2)*self(a)%pos(2)
         res(3)  = res(3)  + Z*self(a)%pos(3)*self(a)%pos(3)*self(a)%pos(3)
         res(4)  = res(4)  + Z*self(a)%pos(1)*self(a)%pos(1)*self(a)%pos(2)
         res(5)  = res(5)  + Z*self(a)%pos(1)*self(a)%pos(1)*self(a)%pos(3)
         res(6)  = res(6)  + Z*self(a)%pos(2)*self(a)%pos(2)*self(a)%pos(1)
         res(7)  = res(7)  + Z*self(a)%pos(2)*self(a)%pos(2)*self(a)%pos(3)
         res(8)  = res(8)  + Z*self(a)%pos(3)*self(a)%pos(3)*self(a)%pos(1)
         res(9)  = res(9)  + Z*self(a)%pos(3)*self(a)%pos(3)*self(a)%pos(2)
         res(10) = res(10) + Z*self(a)%pos(1)*self(a)%pos(2)*self(a)%pos(3)
      end do
     STOP_TIMER("ATOMVEC:nuclear_octupole_moment")
      CHECK
   end function

   function nuclear_E_field_at_nuclei(self) result(res)
    ATOMVEC(:) :: self
   ! Return the nuclear contribution to the electric fields
   ! at the nuclei as a (3 x .n_atom) array
      REALMAT(3,size(self)) :: res
      REAL :: Z,r
      INT :: a,b
      REALVEC(3) :: ab
      STACK("ATOMVEC:nuclear_E_field_at_nuclei")
      START_TIMER("ATOMVEC:nuclear_E_field_at_nuclei")
      res = ZERO
      do a = 1,n_atom_(self)
         do b = 1,n_atom_(self)
            if (b==a) cycle
            Z  = self(b)%atomic_number
            ab = self(a)%pos - self(b)%pos
            r  = norm_(ab)
            res(:,a) = res(:,a) - Z*ab/(r*r*r)
         end do
      end do
     STOP_TIMER("ATOMVEC:nuclear_E_field_at_nuclei")
      CHECK
   end function

   function nuclear_EFG_at_nuclei(self) result(res)
    ATOMVEC(:) :: self
   ! Return the nuclear contribution to the electric fields gradient (EFG)
   ! at the nuclei as a (6 x .n_atom) array
      REALMAT(6,size(self)) :: res
      REAL :: Z,r,r3,r5
      INT :: a,b
      REALVEC(3) :: ab
      STACK("ATOMVEC:nuclear_EFG_at_nuclei")
      START_TIMER("ATOMVEC:nuclear_EFG_at_nuclei")
      res = ZERO
      do a = 1,n_atom_(self)
         do b = 1,n_atom_(self)
            if (b==a) cycle
            Z  = self(b)%atomic_number
            ab = self(a)%pos - self(b)%pos
            r  = norm_(ab)
            r3 = r*r*r
            r5 = r3*r*r
            res(1,a) = res(1,a) + Z * (THREE*ab(1)*ab(1)/r5 - ONE/r3)
            res(2,a) = res(2,a) + Z * (THREE*ab(2)*ab(2)/r5 - ONE/r3)
            res(3,a) = res(3,a) + Z * (THREE*ab(3)*ab(3)/r5 - ONE/r3)
            res(4,a) = res(4,a) + Z * (THREE*ab(1)*ab(2)/r5)
            res(5,a) = res(5,a) + Z * (THREE*ab(1)*ab(3)/r5)
            res(6,a) = res(6,a) + Z * (THREE*ab(2)*ab(3)/r5)
         end do
      end do
     STOP_TIMER("ATOMVEC:nuclear_EFG_at_nuclei")
      CHECK
   end function

   function has_all_ANO_data(self) result(has)
    ATOMVEC(:) :: self
   ! Return TRUE if all atom ANO data exists
      IN :: self
      BIN :: has
      INT :: a
      STACK("ATOMVEC:has_all_ANO_data")
      START_TIMER("ATOMVEC:has_all_ANO_data")
      has = TRUE
      do a = 1,n_atom_(self)
         has = has AND has_ANO_data_(self(a))
         if (NOT has) exit
      end do
     STOP_TIMER("ATOMVEC:has_all_ANO_data")
      CHECK
   end function

!  ************************
!  Atom information methods
!  ************************

   function chemical_symbols(self) result(res)
    ATOMVEC(:) :: self
   ! Return an array of the chemical symbols for each atom
      STRVEC(STR_SIZE,size(self)) :: res
       INT :: a
      STACK("ATOMVEC:chemical_symbols")
      START_TIMER("ATOMVEC:chemical_symbols")
      do a = 1,n_atom_(self)
         res(a) = chemical_symbol_(self(a))
      end do
     STOP_TIMER("ATOMVEC:chemical_symbols")
      CHECK
   end function

   function numbered_chemical_symbols(self) result(res)
    ATOMVEC(:) :: self
   ! Return an array of the chemical symbols for each atom with a number
   ! at the end in brackets
      STRVEC(STR_SIZE,size(self)) :: res
       INT :: a
      STACK("ATOMVEC:numbered_chemical_symbols")
      START_TIMER("ATOMVEC:numbered_chemical_symbols")
      do a = 1,n_atom_(self)
         res(a) = chemical_symbol_(self(a))
         res(a) = trim(res(a))//"("//trim(to_str_(a))//")"
      end do
     STOP_TIMER("ATOMVEC:numbered_chemical_symbols")
      CHECK
   end function

   function basis_labels(self) result(labels)
    ATOMVEC(:) :: self
   ! Return a list of basis set "labels". Missing labels are returned blank.
      STRVEC(STR_SIZE,:), PTR :: labels
      INT :: i
      STACK("ATOMVEC:basis_labels")
      START_TIMER("ATOMVEC:basis_labels")
      call create_(labels,size(self))
      do i = 1,size(self)
         if (self(i)%basis_label/=" ") then; labels(i) = self(i)%basis_label
         else;                               labels(i) = " "
         end if
      end do
     STOP_TIMER("ATOMVEC:basis_labels")
      UNSTACK
   end function

   function library_basis_labels(self,suffix) result(labels)
    ATOMVEC(:) :: self
   ! Return a list of library basis set labels. The label is either the atoms
   ! own .basis_label (if it contains the colon character, the indicator of a
   ! library basis set), or else it is the atoms element name with ":suffix"
   ! appended to it. Only a unique list of basis labels is returned.
      STR(STR_SIZE) :: suffix
      STRVEC(STR_SIZE,:), PTR :: labels
      INT :: i
      STACK("ATOMVEC:library_basis_labels")
      START_TIMER("ATOMVEC:library_basis_labels")
      call create_(labels,size(self))
      do i = 1,size(self)
         if (includes_(self(i)%basis_label,":")) then
            labels(i) = self(i)%basis_label
         else
            labels(i) = library_basis_label_(self(i),suffix)
         end if
      end do
      call remove_repetitions_(labels)
     STOP_TIMER("ATOMVEC:library_basis_labels")
      UNSTACK
   end function

   function groups_defined(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if atom groups have been defined
      BIN :: res
      STACK("ATOMVEC:groups_defined")
      START_TIMER("ATOMVEC:groups_defined")
      if (any(self(:)%group>0)) then; res = TRUE
      else;                           res = FALSE
      end if
     STOP_TIMER("ATOMVEC:groups_defined")
      CHECK
   end function

   function atom_index_from_pos(self,pos) result(res)
    ATOMVEC(:) :: self
   ! Return the index of atom from its position "pos"
      REALVEC(3) :: pos
      INT :: res
      INT :: a
      BIN :: found
      STACK("ATOMVEC:atom_index_from_pos")
      START_TIMER("ATOMVEC:atom_index_from_pos")
      found = FALSE
      do a = 1,n_atom_(self)
         found = same_as_(pos,self(a)%pos)
         if (found) exit
      end do
      res = a
      ENSURE(found,"ATOMVEC:atom_index_from_pos ... no atom for this position")
     STOP_TIMER("ATOMVEC:atom_index_from_pos")
      CHECK
   end function

   subroutine make_atom_kind_count(self,cnt,n_kind)
    ATOMVEC(:) :: self
   ! Return an array "cnt" in which each element represents an atom, and the
   ! value of that array element is a count of the number of atoms of that kind.
   ! Later atoms (i.e. array elements) which are of the same kind as earlier
   ! atoms have a count of zero. The number of different kinds is returned in
   ! "n_kind".
      INTVEC(:), PTR :: cnt
      INT :: n_kind
      INT :: n,l,n_atom
      STACK("ATOMVEC:make_atom_kind_count")
      START_TIMER("ATOMVEC:make_atom_kind_count")
      n_atom = n_atom_(self)
      nullify(cnt); call create_(cnt,n_atom)
      cnt = 1
      do n = 1,n_atom
         if (cnt(n)==0) cycle
         do l = n+1,n_atom
            if (cnt(l)==0) cycle
            if ( same_kind_of_atoms_(self,l,n) ) then
               cnt(n) = cnt(n)+1
               cnt(l) = 0
            end if
         end do
      end do
      n_kind = n_atom_(self) - count(cnt==0)
     STOP_TIMER("ATOMVEC:make_atom_kind_count")
      UNSTACK
   end subroutine

   subroutine make_atom_kind_list(self,atom_kind)
    ATOMVEC(:) :: self
   ! Make the atom kind list ... atom_kind(k).element(c) is the c-th atom of the
   ! same kind as atom k, which is a unique kind.
      INTVECVEC(:), PTR :: atom_kind
      INTVEC(:), PTR :: cnt
      INT :: n,l,k,c,n_atom
      STACK("ATOMVEC:make_atom_kind_list")
      START_TIMER("ATOMVEC:make_atom_kind_list")
      n_atom = n_atom_(self)
      call make_atom_kind_count_(self,cnt,n)
      nullify(atom_kind)
      call create_(atom_kind,n)
      k = 0
      do n = 1,n_atom
         if (cnt(n)==0) cycle
         k = k+1
         c = 1
         call create_(atom_kind(k)%element,cnt(n))
         atom_kind(k)%element(c) = n
         do l = n+1,n_atom
            if ( same_kind_of_atoms_(self,l,n) ) then
               c = c+1
               atom_kind(k)%element(c) = l
            end if
         end do
      end do
      call destroy_(cnt)
     STOP_TIMER("ATOMVEC:make_atom_kind_list")
      UNSTACK
   end subroutine

   subroutine make_atom_kind_list_1(self,atom_kind,n_kind)
    ATOMVEC(:) :: self
   ! Make a different atom kind list ... atom_kind(k) is the kind index of the
   ! k-th atom. (Same effect as the make_atom_kind_map routine).
      INTVEC(:) :: atom_kind
      INT :: n_kind
      INT :: n_atom,n,l
   STACK("ATOMVEC:make_atom_kind_list_1")
   START_TIMER("ATOMVEC:make_atom_kind_list_1")
   ENSURE(size(atom_kind)==size(self),"ATOMVEC:make_atom_kind_list_1 ... atom_kind is incorrectly dimensioned")
      n_atom = size(self)
      atom_kind = (/ (n, n=1,n_atom) /)
      n_kind = 0
      do n = 1,n_atom
         if (atom_kind(n)<=n_kind) cycle
         n_kind = n_kind + 1
         atom_kind(n) = n_kind
         do l = n+1,n_atom
            if (atom_kind(l)<=n_kind) cycle
            if ( NOT same_kind_of_atoms_(self,l,n) ) cycle
            atom_kind(l) = n_kind
         end do
      end do
     STOP_TIMER("ATOMVEC:make_atom_kind_list_1")
      CHECK
   end subroutine

   subroutine make_unique_atom_list(self,unique_atom)
    ATOMVEC(:) :: self
   ! "unique_atom(k)" is the index of the first atom which represents all the
   ! the atoms which are of the same kind, k.
      INTVEC(:) :: unique_atom
      INTVEC(:), PTR :: atom_kind
      INT :: n_kind,k,pos
      STACK("ATOMVEC:make_unique_atom_list")
      START_TIMER("ATOMVEC:make_unique_atom_list")
      call create_(atom_kind,size(self))
      call make_atom_kind_list_(self,atom_kind,n_kind)
      ENSURE(size(unique_atom)==n_kind,"ATOMVEC:make_unique_atom_list ... unique atom incorrectly dimensioned")
      do k = 1,n_kind
         pos = index_of_value_(atom_kind,k)
         ENSURE(pos/=0,"ATOMVEC:make_unique_atom_list ... atom kind "// trim(to_str_(k)) //" does not exist!")
         unique_atom(k) = pos
      end do
      call destroy_(atom_kind)
     STOP_TIMER("ATOMVEC:make_unique_atom_list")
      UNSTACK
   end subroutine

   subroutine make_unique_atom_list_1(self,unique_atom,atom_kind,n_kind)
    ATOMVEC(:) :: self
   ! "unique_atom(k)" is the index of the first atom which represents all the
   ! the atoms which are of the same kind, k. The atom kinds for each atom "a"
   ! are given by "atom_kind(a)" ...
      INTVEC(:), PTR :: unique_atom,atom_kind
      INT :: n_kind
      INT :: k,pos
      STACK("ATOMVEC:make_unique_atom_list_1")
      START_TIMER("ATOMVEC:make_unique_atom_list_1")
      call create_(atom_kind,size(self))
      call make_atom_kind_list_(self,atom_kind,n_kind)
      call create_(unique_atom,n_kind)
      do k = 1,n_kind
         pos = index_of_value_(atom_kind,k)
         ENSURE(pos/=0,"ATOMVEC:make_unique_atom_list_1 ... atom kind "// trim(to_str_(k)) //" does not exist!")
         unique_atom(k) = pos
      end do
     STOP_TIMER("ATOMVEC:make_unique_atom_list_1")
      UNSTACK
   end subroutine

   subroutine make_atom_kind_map(self,map)
    ATOMVEC(:) :: self
   ! Make an atom kind array "map", where map(a) is the unique atom kind
   ! corresponding to atom index a.
      INTVEC(:), PTR :: map
      INTVECVEC(:), PTR :: atom_kind
      INT :: n_k,k,kk
      STACK("ATOMVEC:make_atom_kind_map")
      START_TIMER("ATOMVEC:make_atom_kind_map")
      call create_(map,size(self))
      call make_atom_kind_list_(self,atom_kind)
      n_k = size(atom_kind)
      do k  = 1,n_k
      do kk = 1,size(atom_kind(k)%element)
         map(atom_kind(k)%element(kk)) = k
      end do
      end do
      call destroy_(atom_kind)
     STOP_TIMER("ATOMVEC:make_atom_kind_map")
      CHECK
   end subroutine

!  *************************
!  Shell information methods
!  *************************

   subroutine get_shell_limits(self,s,first,last)
    ATOMVEC(:) :: self
   ! Get the shell function limits "first" and "last" for atomvec shell
   ! number "s"
      INT :: s,first,last
      INT :: a,as,n,ss
      STACK("ATOMVEC:get_shell_limits")
      START_TIMER("ATOMVEC:get_shell_limits")
      ss = 0; last = 0
      atom_loop: do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         do as = 1,n
            ss = ss + 1
            first = last+1
            last  = first + self(a)%basis%shell(as)%n_comp - 1
            if (s==ss) exit atom_loop
         end do
      end do atom_loop
     STOP_TIMER("ATOMVEC:get_shell_limits")
      CHECK
   end subroutine

   subroutine get_shell_limits_1(self,first,last)
    ATOMVEC(:) :: self
   ! Get the shell function limit vectors "first" and "last" for corresponding
   ! to the vector of atomvec shell number
      INTVEC(:), PTR :: first,last
      INT :: a,as,n,ss,f,l
      STACK("ATOMVEC:get_shell_limits_1")
      START_TIMER("ATOMVEC:get_shell_limits_1")
      nullify(first); call create_(first,n_shell_(self))
      nullify(last);  call create_(last,n_shell_(self))
      ss = 0; l = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         do as = 1,n
            ss = ss + 1
            f = l + 1
            l = f + self(a)%basis%shell(as)%n_comp - 1
            first(ss) = f
            last(ss)  = l
         end do
      end do
     STOP_TIMER("ATOMVEC:get_shell_limits_1")
      UNSTACK
   end subroutine

   subroutine make_atom_basis_fn_limits(self,first,last)
    ATOMVEC(:) :: self
   ! Get the first and last basis functions for the atoms
      INTVEC(:), PTR :: first,last
      INT :: a,as,n,l
      STACK("ATOMVEC:make_atom_basis_fn_limits")
      START_TIMER("ATOMVEC:make_atom_basis_fn_limits")
      nullify(first); call create_(first,size(self))
      nullify(last);  call create_(last,size(self))
      l = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         first(a) = l + 1
         do as = 1,n
            l = l + self(a)%basis%shell(as)%n_comp
         end do
         last(a) = l
      end do
     STOP_TIMER("ATOMVEC:make_atom_basis_fn_limits")
      UNSTACK
   end subroutine

!  ******************************
!  Atom-shell information methods
!  ******************************

   function atom_for_shell(self,s) result(a)
    ATOMVEC(:) :: self
   ! Return the *atom* number "a" corresponding to the
   ! *atomvec* shell number "s"
      INT :: a,s
      INT :: ss,n
      STACK("ATOMVEC:atom_for_shell")
      START_TIMER("ATOMVEC:atom_for_shell")
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         ss = ss + n
         if (s<=ss) exit
      end do
     STOP_TIMER("ATOMVEC:atom_for_shell")
      CHECK
   end function

   function atom_for_shell_1(self) result(res)
    ATOMVEC(:) :: self
   ! Return a vector of *atom* numbers corresponding to the
   ! vector of *atomvec* shell numbers
      INTVEC(:), PTR :: res
      INT :: a,ss,n
      STACK("ATOMVEC:atom_for_shell_1")
      START_TIMER("ATOMVEC:atom_for_shell_1")
      nullify(res); call create_(res,n_shell_(self))
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         res(ss+1:ss+n) = a
         ss = ss + n
      end do
     STOP_TIMER("ATOMVEC:atom_for_shell_1")
      UNSTACK
   end function

   function atom_shell_for_shell(self,s) result(as)
    ATOMVEC(:) :: self
   ! Return the *atom* shell number "as" corresponding to the
   ! *atomvec* shell number "s"
      INT :: as,s
      INT :: a,ss,n
      STACK("ATOMVEC:atom_shell_for_shell")
      START_TIMER("ATOMVEC:atom_shell_for_shell")
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         ss = ss + n
         if (s<=ss) exit
      end do
      as = s - ss + n
     STOP_TIMER("ATOMVEC:atom_shell_for_shell")
      CHECK
   end function

   function atom_shell_for_shell_1(self) result(res)
    ATOMVEC(:) :: self
   ! Return a vector of *atom* shell numbers corresponding to the
   ! *atomvec* shell number vector
      INTVEC(:), PTR :: res
      INT :: a,ss,n,as
      STACK("ATOMVEC:atom_shell_for_shell_1")
      START_TIMER("ATOMVEC:atom_shell_for_shell_1")
      nullify(res); call create_(res,n_shell_(self))
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         do as = 1,n
            res(ss+as) = as
         end do
         ss = ss + n
      end do
     STOP_TIMER("ATOMVEC:atom_shell_for_shell_1")
      UNSTACK
   end function

   function first_shell_for_atom(self,a) result(res)
    ATOMVEC(:) :: self
   ! Return the index of the first shell for atom "a" in the atomvec
      INT :: a,res
      INT :: at,n
      STACK("ATOMVEC:first_shell_for_atom")
      START_TIMER("ATOMVEC:first_shell_for_atom")
      res = 1
      do at = 1,(a-1)
         n = self(a)%basis%n_shell
         res = res + n
      end do
     STOP_TIMER("ATOMVEC:first_shell_for_atom")
      CHECK
   end function

   subroutine make_shell_for_atom_limits(self,first,last)
    ATOMVEC(:) :: self
   ! Return the indices of the first and last shell for each atom in the atomvec
      INTVEC(:) :: first,last
      INT :: ss,a,n
      STACK("ATOMVEC:make_shell_for_atom_limits")
      START_TIMER("ATOMVEC:make_shell_for_atom_limits")
      ss = 0
      do a = 1,n_atom_(self)
         first(a) = ss + 1
         n = self(a)%basis%n_shell
         ss = ss + n
         last(a) = ss
      end do
     STOP_TIMER("ATOMVEC:make_shell_for_atom_limits")
      CHECK
   end subroutine

   function first_shell_for_atom_1(self) result(res)
    ATOMVEC(:) :: self
   ! Return the indices of the first shell for an atom in the atomvec
      INTVEC(:), PTR :: res
      INT :: ss,a,n
      STACK("ATOMVEC:first_shell_for_atom_1")
      START_TIMER("ATOMVEC:first_shell_for_atom_1")
      nullify(res); call create_(res,size(self))
      ss = 0
      do a = 1,n_atom_(self)
         n = self(a)%basis%n_shell
         res(a) = ss + 1
         ss = ss + n
      end do
     STOP_TIMER("ATOMVEC:first_shell_for_atom_1")
      UNSTACK
   end function

   function same_kind_of_atoms(self,a,b) result(res)
    ATOMVEC(:) :: self
   ! Return true if atoms "a" and "b" are the same kind
      INT :: a,b
      BIN :: res
      STACK("ATOMVEC:same_kind_of_atoms")
      START_TIMER("ATOMVEC:same_kind_of_atoms")
      res = same_kind_as_(self(a),self(b))
     STOP_TIMER("ATOMVEC:same_kind_of_atoms")
      CHECK
   end function

   function bonded(self,a,b,scale_factor) result(res)
    ATOMVEC(:) :: self
   ! Return true if atoms "a" and "b" are bonded atoms.
   ! If present, "scale_factor" is used to determine a multiple
   ! of the sum of the Bragg-Slater radii within which the atoms
   ! are regarded to be bonded.
      INT :: a,b
      REAL, optional :: scale_factor
      BIN :: res
      REAL :: bond_max,fac
      STACK("ATOMVEC:bonded")
      START_TIMER("ATOMVEC:bonded")
      fac = ATOMVEC_BONDED_SCALE_FACTOR
      if (present(scale_factor)) fac = scale_factor
      bond_max = fac*(bragg_slater_radius_(self(a)) + bragg_slater_radius_(self(b)))
      bond_max = bond_max*BOHR_PER_ANGSTROM
      res = are_nearby_(self,a,b,bond_max)
     STOP_TIMER("ATOMVEC:bonded")
      CHECK
   end function

   function are_nearby(self,a,b,dist) result(res)
    ATOMVEC(:) :: self
   ! Return whether atoms "a" and "b" are nearby, i.e. within length "dist".
      INT :: a,b
      REAL :: dist
      BIN :: res
      REALVEC(3) :: tmp
      REAL :: r2
      STACK("ATOMVEC:are_nearby")
      START_TIMER("ATOMVEC:are_nearby")
      tmp = self(a)%pos - self(b)%pos
      tmp = abs(tmp)
      if      (tmp(1)>dist) then
         res = FALSE
      else if (tmp(2)>dist) then
         res = FALSE
      else if (tmp(3)>dist) then
         res = FALSE
      else
         r2 = dot_product(tmp,tmp)
         res = (r2 < dist*dist)
      end if
     STOP_TIMER("ATOMVEC:are_nearby")
      CHECK
   end function

   function connected(self,a,b,c,d) result(res)
    ATOMVEC(:) :: self
   ! Return true if atoms "a" "b" "c" and "d" are connected together
      INT :: a,b,c,d
      BIN :: res
      STACK("ATOMVEC:connected")
      START_TIMER("ATOMVEC:connected")
      res = bonded_(self,a,b) OR bonded_(self,a,c) OR bonded_(self,a,d)
      res = res AND ( bonded_(self,b,a) OR bonded_(self,b,c) OR bonded_(self,b,d) )
      res = res AND ( bonded_(self,c,a) OR bonded_(self,c,b) OR bonded_(self,c,d) )
      res = res AND ( bonded_(self,d,a) OR bonded_(self,d,b) OR bonded_(self,d,c) )
     STOP_TIMER("ATOMVEC:connected")
      CHECK
   end function

   function no_of_bonds(self) result(res)
    ATOMVEC(:) :: self
   ! Return the number of bonded atoms
      INT :: res
      INT :: a,b
      STACK("ATOMVEC:no_of_bonds")
      START_TIMER("ATOMVEC:no_of_bonds")
      res = 0
      do a = 1,n_atom_(self)
      do b = 1,a-1
         if (bonded_(self,a,b)) res = res + 1
      end do
      end do
     STOP_TIMER("ATOMVEC:no_of_bonds")
      CHECK
   end function

!   bond_distance(a,b) result(res)
!   ! Return the bond distance between atoms "a" and "b"
!      a,b :: INT
!      res :: REAL
!      res = self(a).pos.distance_to(self(b).pos)
!   end

   function bond_distance(self,a,b) result(res)
    ATOMVEC(:) :: self
   ! Return the bond distance between atoms "a" and "b"
      INT :: a,b
      REAL :: res
      REALVEC(3) :: tmp
      STACK("ATOMVEC:bond_distance")
      START_TIMER("ATOMVEC:bond_distance")
      tmp = self(a)%pos - self(b)%pos
      res = sqrt(dot_product(tmp,tmp))
!      res = self(a).pos.distance_to(self(b).pos)
     STOP_TIMER("ATOMVEC:bond_distance")
      CHECK
   end function

   function no_of_angles(self) result(res)
    ATOMVEC(:) :: self
   ! Return the number of angles within bond contact distance
      INT :: res
      INT :: n,a,b,c
      STACK("ATOMVEC:no_of_angles")
      START_TIMER("ATOMVEC:no_of_angles")
      n = n_atom_(self)
      res = 0
      do a = 1,n
      do b = 1,n
         if (a==b) cycle
         if (NOT bonded_(self,a,b)) cycle
         do c = 1,n
            if (a==c OR b==c) cycle
            if (NOT bonded_(self,b,c)) cycle
            res = res + 1
         end do
      end do
      end do
      res = res/2
     STOP_TIMER("ATOMVEC:no_of_angles")
      CHECK
   end function

   function bond_angle(self,a,b,c,degrees) result(res)
    ATOMVEC(:) :: self
   ! Return the bond angle between atoms "a" , "b" and "c".
   ! The central atom is "b".
   ! If "degrees" is present and TRUE, result is in degrees.
      INT :: a,b,c
      BIN, optional :: degrees
      REAL :: res
      REALVEC(3) :: rba,rbc
      BIN :: change
      STACK("ATOMVEC:bond_angle")
      START_TIMER("ATOMVEC:bond_angle")
      rba = self(a)%pos - self(b)%pos
      rbc = self(c)%pos - self(b)%pos
      call normalise_(rba)
      call normalise_(rbc)
      res = dot_(rba,rbc)
      res = arccos_(res)
      change = FALSE
      if (present (degrees)) change = degrees
      if (change) res = (180/PI)*res
     STOP_TIMER("ATOMVEC:bond_angle")
      CHECK
   end function

   function no_of_torsion_angles(self) result(res)
    ATOMVEC(:) :: self
   ! Return the number of torsion angles within bond contact distance
      INT :: res
      INT :: n,a,b,c,d
      STACK("ATOMVEC:no_of_torsion_angles")
      START_TIMER("ATOMVEC:no_of_torsion_angles")
      n = n_atom_(self)
      res = 0
      do a = 1,n
      do b = 1,n
         if (a==b) cycle
         if (NOT bonded_(self,a,b)) cycle
         do c = 1,n
            if (a==c OR b==c) cycle
            if (NOT bonded_(self,b,c)) cycle
            do d = 1,n
               if (a==d OR b==d OR c==d) cycle
               if (NOT bonded_(self,c,d)) cycle
               res = res + 1
            end do
         end do
      end do
      end do
      res = res/2
     STOP_TIMER("ATOMVEC:no_of_torsion_angles")
      CHECK
   end function

   function torsion_angle(self,a,b,c,d,abc_colinear,bcd_colinear,degrees) result(res)
    ATOMVEC(:) :: self
   ! Return the torsion angle between atoms "a", "b", "c" and "d". The atoms are
   ! assumed connected like a--b--c--d and the angle returned is that between
   ! vectors (a-b) and (d-c) i.e. the torsion angle looking down the b--c bond.
   ! If "degrees" is present and TRUE, result is in degrees.
   ! NOTE *** If the result is -ONE, either a--b--c or b--c--d are colinear, and
   ! the variables "abc_colinear" and "bcd_colinear" are set.
      INT :: a,b,c,d
      BIN, optional :: abc_colinear,bcd_colinear
      BIN, optional :: degrees
      REAL :: res
      BIN :: change
      REALVEC(3) :: tcd,tba,rba,rcd,rbc
      STACK("ATOMVEC:torsion_angle")
      START_TIMER("ATOMVEC:torsion_angle")
      if (present(abc_colinear)) abc_colinear = FALSE
      if (present(bcd_colinear)) bcd_colinear = FALSE
      rba = self(a)%pos - self(b)%pos
      rcd = self(d)%pos - self(c)%pos
      rbc = self(c)%pos - self(b)%pos
      call to_cross_product_(tcd,rcd,rbc)
      call to_cross_product_(tba,rba,rbc)
      res = ZERO
      if (abs( norm_(tba))<TOL(5)) then
         res = -ONE
         if (present(abc_colinear)) abc_colinear = TRUE
      end if
      if (abs( norm_(tcd))<TOL(5)) then
         res = -ONE
         if (present(bcd_colinear)) bcd_colinear = TRUE
      end if
      if (res<0) then; STOP_TIMER("ATOMVEC:torsion_angle") CHECK return; end if
      call normalise_(tba)
      call normalise_(tcd)
      res = dot_(tba,tcd)
      res = arccos_(res)
      change = FALSE
      if (present (degrees)) change = degrees
      if (change) res = (180/PI)*res
     STOP_TIMER("ATOMVEC:torsion_angle")
      CHECK
   end function

!  ************************
!  Size information methods
!  ************************

   PURE function n_atom(self) result(res)
    ATOMVEC(:) :: self
   ! Return the number of atoms in the atom vector
      IN :: self
      INT :: res
      res = size(self)
     STOP_TIMER("ATOMVEC:n_atom")
   end function

   PURE function n_e(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of electrons in the atomvec assuming
   ! that it is neutrally charged.
      IN :: self
      INT :: res
       INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + self(a)%atomic_number
      end do
     STOP_TIMER("ATOMVEC:n_e")
   end function

   PURE function no_of_shells(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of gaussian shells in the basis set for the
   ! molecule
      IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_shells_(self(a))
      end do
     STOP_TIMER("ATOMVEC:no_of_shells")
   end function

   PURE function n_shell(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of gaussian shells in the basis set for the
   ! molecule
      IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + n_shell_(self(a))
      end do
     STOP_TIMER("ATOMVEC:n_shell")
   end function

   PURE function n_shell_pairs(self) result(res)
    ATOMVEC(:) :: self
   ! Return the number of shell pairs in the basis set for the molecule
      IN :: self
      INT :: res
      INT :: n_shell
      
      n_shell = n_shell_(self)
      res = n_shell*(n_shell+1)/2
     STOP_TIMER("ATOMVEC:n_shell_pairs")
   end function

   PURE function n_shell_for_atom(self,i) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of gaussian shells in the basis set for the
   ! molecule
      IN :: self
       INT, IN :: i
      INT :: res
      res = self(i)%basis%n_shell
     STOP_TIMER("ATOMVEC:n_shell_for_atom")
   end function

   PURE function no_of_basis_functions(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of basis functions in the concatenated
   ! basis set for the atom list.
      IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_basis_functions_(self(a))
      end do
     STOP_TIMER("ATOMVEC:no_of_basis_functions")
   end function

   PURE function n_bf(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of basis functions in the concatenated
   ! basis set for the atom list.
      IN :: self
      INT :: res
       INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + n_bf_(self(a))
      end do
     STOP_TIMER("ATOMVEC:n_bf")
   end function

   PURE function no_of_primitives(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of primitives in the basis set for the
   ! molecule
      IN :: self
      INT :: res
       INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_primitives_(self(a))
      end do
     STOP_TIMER("ATOMVEC:no_of_primitives")
   end function

   PURE function n_prim(self) result(res)
    ATOMVEC(:) :: self
   ! Work out and return the number of primitives in the basis set for the
   ! molecule
      IN :: self
      INT :: res
       INT :: a
      res = 0
      do a = 1,n_atom_(self)
         res = res + n_prim_(self(a))
      end do
     STOP_TIMER("ATOMVEC:n_prim")
   end function

   function no_of_occupied_ANOs(self,ANOkind,tol) result(res)
    ATOMVEC(:) :: self
   ! Returns the number of non-zero occupied atomic natural orbitals. For this
   ! purpose, zero is defined to be "tol" if present, or TOL(7) otherwise
      STR(STR_SIZE), optional :: ANOkind
      REAL, optional :: tol
      INT :: res
      INT :: a
      STACK("ATOMVEC:no_of_occupied_ANOs")
      START_TIMER("ATOMVEC:no_of_occupied_ANOs")
      ENSURE(associated(self(1)%occupation_numbers),"ATOMVEC:no_of_occupied_ANOs ... no occupation numbers")
      res = 0
      do a = 1,n_atom_(self)
         res = res + no_of_occupied_NOs_(self(a),ANOkind,tol)
      end do
     STOP_TIMER("ATOMVEC:no_of_occupied_ANOs")
      CHECK
   end function

!  *********************
!  Basis set information
!  *********************

   function bases_all_exist(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if *all* basis sets are associated. NOTE: do not confuse this
   ! with the routine .basis_are_resolved, which is probably what you want.
      BIN :: res
      INT :: a
      STACK("ATOMVEC:bases_all_exist")
      START_TIMER("ATOMVEC:bases_all_exist")
      res = TRUE
      do a = 1,n_atom_(self)
         if (NOT associated(self(a)%basis)) then
            res = FALSE
            exit
         end if
      end do
     STOP_TIMER("ATOMVEC:bases_all_exist")
      CHECK
   end function

   function bases_are_all_unlabeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all basis set labels do not exist (or are blank)
      BIN :: res
      INT :: a
      STACK("ATOMVEC:bases_are_all_unlabeled")
      START_TIMER("ATOMVEC:bases_are_all_unlabeled")
      if (NOT bases_all_exist_(self)) then
         res = TRUE
      else
         res = TRUE
         do a = 1,n_atom_(self)
            if (self(a)%basis%label/=" ") then
               res = FALSE
               exit
            end if
         end do
      end if
     STOP_TIMER("ATOMVEC:bases_are_all_unlabeled")
      CHECK
   end function

   function bases_are_all_labeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all basis set labels exist (i.e. are not blank)
      BIN :: res
      INT :: a
      STACK("ATOMVEC:bases_are_all_labeled")
      START_TIMER("ATOMVEC:bases_are_all_labeled")
      if (NOT bases_all_exist_(self)) then
         res = FALSE
      else
         res = TRUE
         do a = 1,n_atom_(self)
            if (self(a)%basis%label==" ") then
               res = FALSE
               exit
            end if
         end do
      end if
     STOP_TIMER("ATOMVEC:bases_are_all_labeled")
      CHECK
   end function

   function bases_are_part_labeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if only some basis set labels exist (i.e. are not blank)
      BIN :: res
      STACK("ATOMVEC:bases_are_part_labeled")
      START_TIMER("ATOMVEC:bases_are_part_labeled")
      if (NOT bases_all_exist_(self)) then
         res = FALSE
      else
         res = NOT bases_are_all_labeled_(self)   &
           AND NOT bases_are_all_unlabeled_(self)
      end if
     STOP_TIMER("ATOMVEC:bases_are_part_labeled")
      CHECK
   end function

   function bases_are_resolved(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all basis sets are associated AND their shell list parts are
   ! also associated.
      BIN :: res
      INT :: a
      STACK("ATOMVEC:bases_are_resolved")
      START_TIMER("ATOMVEC:bases_are_resolved")
      res = TRUE
      do a = 1,n_atom_(self)
         if (NOT associated(self(a)%basis)) then
            res = FALSE
            exit
         else if (NOT associated(self(a)%basis%shell)) then
            res = FALSE
            exit
         end if
      end do
     STOP_TIMER("ATOMVEC:bases_are_resolved")
      CHECK
   end function

   function slaterbases_all_exist(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if *all* slaterbasis sets are associated. NOTE: do not confuse
   ! this with the routine .slaterbasis_are_resolved, which is probably what you
   ! want.
      BIN :: res
      INT :: a
      STACK("ATOMVEC:slaterbases_all_exist")
      START_TIMER("ATOMVEC:slaterbases_all_exist")
      res = TRUE
      do a = 1,n_atom_(self)
         if (NOT associated(self(a)%slaterbasis)) then
            res = FALSE
            exit
         end if
      end do
     STOP_TIMER("ATOMVEC:slaterbases_all_exist")
      CHECK
   end function

   function slaterbases_are_all_unlabeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all slaterbasis set labels do not exist (or are blank)
      BIN :: res
      INT :: a
      STACK("ATOMVEC:slaterbases_are_all_unlabeled")
      START_TIMER("ATOMVEC:slaterbases_are_all_unlabeled")
      if (NOT slaterbases_all_exist_(self)) then
         res = TRUE
      else
         res = TRUE
         do a = 1,n_atom_(self)
            if (self(a)%slaterbasis%label/=" ") then
               res = FALSE
               exit
            end if
         end do
      end if
     STOP_TIMER("ATOMVEC:slaterbases_are_all_unlabeled")
      CHECK
   end function

   function slaterbases_are_all_labeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all slaterbasis set labels exist (i.e. are not blank)
      BIN :: res
      INT :: a
      STACK("ATOMVEC:slaterbases_are_all_labeled")
      START_TIMER("ATOMVEC:slaterbases_are_all_labeled")
      if (NOT slaterbases_all_exist_(self)) then
         res = FALSE
      else
         res = TRUE
         do a = 1,n_atom_(self)
            if (self(a)%slaterbasis%label==" ") then
               res = FALSE
               exit
            end if
         end do
      end if
     STOP_TIMER("ATOMVEC:slaterbases_are_all_labeled")
      CHECK
   end function

   function slaterbases_are_part_labeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if only some slaterbasis set labels exist (i.e. are not blank)
      BIN :: res
      STACK("ATOMVEC:slaterbases_are_part_labeled")
      START_TIMER("ATOMVEC:slaterbases_are_part_labeled")
      if (NOT slaterbases_all_exist_(self)) then
         res = FALSE
      else
         res = NOT slaterbases_are_all_labeled_(self)   &
           AND NOT slaterbases_are_all_unlabeled_(self)
      end if
     STOP_TIMER("ATOMVEC:slaterbases_are_part_labeled")
      CHECK
   end function

   function slaterbases_are_resolved(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all slaterbasis sets are associated AND their shell list parts are
   ! also associated.
      BIN :: res
      INT :: a
      STACK("ATOMVEC:slaterbases_are_resolved")
      START_TIMER("ATOMVEC:slaterbases_are_resolved")
      res = TRUE
      do a = 1,n_atom_(self)
         if (NOT associated(self(a)%slaterbasis)) then
            res = FALSE
            exit
         else if (NOT associated(self(a)%slaterbasis%shell)) then
            res = FALSE
            exit
         end if
      end do
     STOP_TIMER("ATOMVEC:slaterbases_are_resolved")
      CHECK
   end function

   function coppensbases_all_exist(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if *all* coppens basis sets are associated. NOTE: do not
   ! confuse this with the routine .basis_are_resolved, which is probably what
   ! you want.
      BIN :: res
      INT :: a
      STACK("ATOMVEC:coppensbases_all_exist")
      START_TIMER("ATOMVEC:coppensbases_all_exist")
      res = TRUE
      do a = 1,n_atom_(self)
         if (NOT associated(self(a)%coppensbasis)) then
            res = FALSE
            exit
         end if
      end do
     STOP_TIMER("ATOMVEC:coppensbases_all_exist")
      CHECK
   end function

   function coppensbases_are_all_unlabeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all coppens basis set labels do not exist (or are blank)
      BIN :: res
      INT :: a
      STACK("ATOMVEC:coppensbases_are_all_unlabeled")
      START_TIMER("ATOMVEC:coppensbases_are_all_unlabeled")
      if (NOT coppensbases_all_exist_(self)) then
         res = TRUE
      else
         res = TRUE
         do a = 1,n_atom_(self)
            if (self(a)%coppensbasis%label/=" ") then
               res = FALSE
               exit
            end if
         end do
      end if
     STOP_TIMER("ATOMVEC:coppensbases_are_all_unlabeled")
      CHECK
   end function

   function coppensbases_are_all_labeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all coppens basis set labels exist (i.e. are not blank)
      BIN :: res
      INT :: a
      STACK("ATOMVEC:coppensbases_are_all_labeled")
      START_TIMER("ATOMVEC:coppensbases_are_all_labeled")
      if (NOT coppensbases_all_exist_(self)) then
         res = FALSE
      else
         res = TRUE
         do a = 1,n_atom_(self)
            if (self(a)%coppensbasis%label==" ") then
               res = FALSE
               exit
            end if
         end do
      end if
     STOP_TIMER("ATOMVEC:coppensbases_are_all_labeled")
      CHECK
   end function

   function coppensbases_are_part_labeled(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if only some coppens basis set labels exist (i.e. are not blank)
      BIN :: res
      STACK("ATOMVEC:coppensbases_are_part_labeled")
      START_TIMER("ATOMVEC:coppensbases_are_part_labeled")
      if (NOT coppensbases_all_exist_(self)) then
         res = FALSE
      else
         res = NOT coppensbases_are_all_labeled_(self)   &
           AND NOT coppensbases_are_all_unlabeled_(self)
      end if
     STOP_TIMER("ATOMVEC:coppensbases_are_part_labeled")
      CHECK
   end function

   function coppensbases_are_resolved(self) result(res)
    ATOMVEC(:) :: self
   ! Return TRUE if all coppens basis sets are associated AND their shell list
   ! parts are also associated.
      BIN :: res
      INT :: a
      STACK("ATOMVEC:coppensbases_are_resolved")
      START_TIMER("ATOMVEC:coppensbases_are_resolved")
      res = TRUE
      do a = 1,n_atom_(self)
         if (NOT associated(self(a)%coppensbasis)) then
            res = FALSE
            exit
         else if (NOT associated(self(a)%coppensbasis%orbital)) then
            res = FALSE
            exit
         end if
      end do
     STOP_TIMER("ATOMVEC:coppensbases_are_resolved")
      CHECK
   end function

   subroutine get_distance_from(self,atomvec,distance,t1,t2)
    ATOMVEC(:) :: self
   ! Calculates the shortest distance between an atom in self and one in
   ! atomvec.  Will set the distance to zero if calculated to less than 10^-6.
   ! If present, t1 and t2 are the indices of the two closest atoms.
     ATOMVEC(:), IN :: atomvec
     IN :: self
     REAL :: distance
     INT, optional :: t1,t2
     REAL :: dist
     INT :: i,j,dim1,dim2
     REALVEC(3) :: difference

     STACK("ATOMVEC:get_distance_from")
     START_TIMER("ATOMVEC:get_distance_from")
     dim1 = n_atom_(self)
     dim2 = size(atomvec)
     ENSURE(present(t1) EQV present(t2),"ATOMVEC:get_distance_from ... need 0 or 2 optional arguments")

     ! Do the first pair explicitly to set a starting distance.
     ! We also work with distance^2 until the end - saves computation.
     difference = self(1)%pos(:) - atomvec(1)%pos(:)
     distance = dot_product(difference,difference)

     do i=1,dim1
       do j=1,dim2
         difference = self(i)%pos(:) - atomvec(j)%pos(:)
         dist = dot_product(difference,difference)
         if (dist < TOL(6)) dist = ZERO
         if (dist < distance) then
           distance = dist
           if (present(t1)) then
             t1=i; t2=j
           end if
         end if
       end do
     end do

     distance = sqrt(distance)
     STOP_TIMER("ATOMVEC:get_distance_from")
      CHECK
   end subroutine

   subroutine get_distance_from_1(self,pos,distance,t1)
    ATOMVEC(:) :: self
   ! Calculates the shortest distance of "pos" to an atom in self.
   ! If present, t1 is the index of the closest atom.
     REALVEC(3), IN :: pos
     IN :: self
     REAL :: distance
     INT, optional :: t1
     REAL :: dist
     REALVEC(3) :: difference
      INT :: i

     ! Do the first pair explicitly to set a starting distance.
     ! We also work with distance^2 until the end - saves computation.
     STACK("ATOMVEC:get_distance_from_1")
     START_TIMER("ATOMVEC:get_distance_from_1")
     difference = self(1)%pos(:) - pos(:)
     distance = dot_product(difference,difference)

     do i = 1, n_atom_(self)
       difference = self(i)%pos(:) - pos(:)
       dist = dot_product(difference,difference)
       if (dist < TOL(6)) dist = ZERO
       if (dist < distance) then
         distance = dist
         if (present(t1)) t1=i
       end if
     end do

     distance = sqrt(distance)
     STOP_TIMER("ATOMVEC:get_distance_from_1")
      CHECK
   end subroutine

   function same_as(self,atomvec) result(res)
    ATOMVEC(:) :: self
   ! Returns true if the two atomvecs contain the same atoms, though maybe in a
   ! different order.  Checks atomic number and position of each atom, but not
   ! the basis sets.
     ATOMVEC(:), IN :: atomvec
     BIN :: res
     BINVEC(size(self)) :: matched
     BIN :: match_pos,match_kind,match
     INT :: n,q,dim
     STACK("ATOMVEC:same_as")
     START_TIMER("ATOMVEC:same_as")
     res = FALSE
     dim = n_atom_(self)
     if (dim/=size(atomvec)) then; STOP_TIMER("ATOMVEC:same_as") CHECK return; end if! different number of atoms in each.
     matched = FALSE
     do n = 1, dim
       match=FALSE
       do q = 1, dim
         match_kind =  (self(n)%atomic_number == atomvec(q)%atomic_number)
         match_pos  =  same_as_(self(n)%pos, atomvec(q)%pos, TOL(3) )
         if (match_pos AND match_kind AND (NOT matched(q))) then
           matched(q) = TRUE
           match = TRUE
           exit
         end if
       end do
       if (NOT match) then; STOP_TIMER("ATOMVEC:same_as") CHECK return; end if! atom n doesn't have a match.
     end do
     do q = 1, dim           ! If not all of q are matched then atomvecs not same.
       if (NOT matched(q)) then; STOP_TIMER("ATOMVEC:same_as") CHECK return; end if
     end do
     res = TRUE
     STOP_TIMER("ATOMVEC:same_as")
      CHECK
   end function

!  *************
!  Crystal stuff
!  *************

   subroutine seitz_multiply(self,seitz)
    ATOMVEC(:) :: self
   ! Self is operated on by the seitz matrix.
   ! Self must be in fractional coordinates.
     INOUT :: self
     REALMAT(:,:), IN :: seitz
     STACK("ATOMVEC:seitz_multiply")
     START_TIMER("ATOMVEC:seitz_multiply")
     call rotate_(self,seitz(1:3,1:3))
     call rotate_thermal_(self,seitz(1:3,1:3))
     call translate_(self,seitz(1:3,4))
     STOP_TIMER("ATOMVEC:seitz_multiply")
      CHECK
   end subroutine

   subroutine translate(self,vector)
    ATOMVEC(:) :: self
   ! Translate self by vector.
     INOUT :: self
     REALVEC(3), IN :: vector
      INT :: n
     STACK("ATOMVEC:translate")
     START_TIMER("ATOMVEC:translate")
     do n=1,n_atom_(self)
       self(n)%pos = self(n)%pos + vector
     end do
     STOP_TIMER("ATOMVEC:translate")
      CHECK
   end subroutine

   subroutine rotate(self,matrix)
    ATOMVEC(:) :: self
   ! Rotate self by the rotation matrix
     INOUT :: self
     REALMAT(3,3), IN :: matrix
      INT :: n
     STACK("ATOMVEC:rotate")
     START_TIMER("ATOMVEC:rotate")
     do n=1,n_atom_(self)
       self(n)%pos            = matmul(matrix,self(n)%pos)
     end do
     STOP_TIMER("ATOMVEC:rotate")
      CHECK
   end subroutine

   subroutine rotate_thermal(self,matrix)
    ATOMVEC(:) :: self
   ! Rotate self by the rotation matrix
     INOUT :: self
     REALMAT(3,3), IN :: matrix
      INT :: n
     STACK("ATOMVEC:rotate_thermal")
     START_TIMER("ATOMVEC:rotate_thermal")
     do n=1,n_atom_(self)
       call change_basis_(self(n)%thermal_tensor,matrix)
     end do
     STOP_TIMER("ATOMVEC:rotate_thermal")
      CHECK
   end subroutine

   subroutine thermal_tensor_to(self,crystal)
    ATOMVEC(:) :: self
   ! Convert all thermal tensors from cartesians to crystal coordinates.
     CRYSTAL, IN :: crystal
      INT :: n
     STACK("ATOMVEC:thermal_tensor_to")
     START_TIMER("ATOMVEC:thermal_tensor_to")
     do n=1,n_atom_(self)
       call thermal_tensor_to_(self(n),crystal)
     end do
     STOP_TIMER("ATOMVEC:thermal_tensor_to")
      CHECK
   end subroutine

   subroutine thermal_tensor_from(self,crystal)
    ATOMVEC(:) :: self
   ! Convert all thermal tensors from crystal coordinates to cartesians.
     CRYSTAL, IN :: crystal
      INT :: n
     STACK("ATOMVEC:thermal_tensor_from")
     START_TIMER("ATOMVEC:thermal_tensor_from")
     do n=1,n_atom_(self)
       call thermal_tensor_from_(self(n),crystal)
     end do
     STOP_TIMER("ATOMVEC:thermal_tensor_from")
      CHECK
   end subroutine

   subroutine ensure_in_unitcell(self,crystal)
    ATOMVEC(:) :: self
   ! Translate the position of self to be in the unitcell.
   ! WARNING: Does this work -- dylan ?
     INOUT :: self
     CRYSTAL, IN :: crystal
     REALVEC(3) :: translation,coa_cart,coa_frac,trans_int
     STACK("ATOMVEC:ensure_in_unitcell")
     START_TIMER("ATOMVEC:ensure_in_unitcell")
     coa_cart = centre_of_atoms_(self)
     coa_frac = matmul(crystal%unitcell%inverse_matrix,coa_cart)   ! fractionals
     trans_int = floor(coa_frac + TOL(6))                ! round to lattice vector
     translation = matmul(crystal%unitcell%inverse_matrix,translation) !cartesians
     call translate_(self, - translation )
     STOP_TIMER("ATOMVEC:ensure_in_unitcell")
      CHECK
   end subroutine

   function default_multiplicity(self) result(res)
    ATOMVEC(:) :: self
   ! Return the default multiplicity for an atomvec/molecule.
     IN :: self
     REAL :: res
     STACK("ATOMVEC:default_multiplicity")
     START_TIMER("ATOMVEC:default_multiplicity")
     if (n_atom_(self)==1) then
        res = ground_state_multiplicity_(self(1))
     else
        res = mod(n_e_(self),2) + 1
     end if
     STOP_TIMER("ATOMVEC:default_multiplicity")
      CHECK
   end function

!  *********
!  Integrals
!  *********

   subroutine make_nuclear_matrix(self,Z)
    ATOMVEC(:) :: self
   ! Calculate the nuclear attraction matrix "Z" for the atoms in the list.
     target :: self
     REALMAT(:,:) :: Z
     ATOM, PTR :: atom
     REALMAT(:,:), PTR :: Z_c
     INT :: q,c,fa,la,fb,lb
     SHELL2 :: sh
     STACK("ATOMVEC:make_nuclear_matrix")
     START_TIMER("ATOMVEC:make_nuclear_matrix")
     ENSURE(bases_are_resolved_(self),"ATOMVEC:make_nuclear_matrix ... no basis set")
     ENSURE(is_square_(Z),"ATOMVEC:make_nuclear_matrix ... Z is not square")
     ENSURE(size(Z,1)==n_bf_(self),"ATOMVEC:make_nuclear_matrix ... wrong size, Z")
     call make_index_info_(self)
     Z = ZERO
     do q = 1,n_shell_pairs_(self)
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
        do c = 1,n_atom_(self)
           atom => self(c)
           call get_nuc_(sh,Z_c,mass_(atom),atom%pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom%atomic_number * Z_c
        end do
        call destroy_(Z_c)
        call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)
     call destroy_index_info_(self)
     STOP_TIMER("ATOMVEC:make_nuclear_matrix")
      CHECK
   end subroutine

   subroutine make_nuclear_matrix_1(self,Z,nuclei)
    ATOMVEC(:) :: self
   ! Calculate the nuclear attraction matrix "Z" for the basis functions on all
   ! atoms in the list, but only for the positive nuclei specified in the
   ! "nuclei" list.
     target :: self
     REALMAT(:,:) :: Z
     INTVEC(:) :: nuclei
     ATOM, PTR :: atom
     REALMAT(:,:), PTR :: Z_c
     INT :: q,c,fa,la,fb,lb
     SHELL2 :: sh
     STACK("ATOMVEC:make_nuclear_matrix_1")
     START_TIMER("ATOMVEC:make_nuclear_matrix_1")
     ENSURE(bases_are_resolved_(self),"ATOMVEC:make_nuclear_matrix_1 ... no basis set")
     ENSURE(is_square_(Z),"ATOMVEC:make_nuclear_matrix_1 ... Z is not square")
     ENSURE(size(Z,1)==n_bf_(self),"ATOMVEC:make_nuclear_matrix_1 ... wrong size, Z")
     call make_index_info_(self)
     Z = ZERO
     do q = 1,n_shell_pairs_(self)
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        call create_(Z_c,sh%a%n_comp,sh%b%n_comp)
        do c = 1,size(nuclei)
           atom => self(nuclei(c))
           call get_nuc_(sh,Z_c,mass_(atom),atom%pos)
           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom%atomic_number * Z_c
        end do
        call destroy_(Z_c)
        call destroy_ptr_part_(sh)
     end do
     call symmetric_reflect_(Z)
     call destroy_index_info_(self)
     STOP_TIMER("ATOMVEC:make_nuclear_matrix_1")
      CHECK
   end subroutine

!  ***********
!  Shell pairs
!  ***********

   subroutine get_shell_pair(self,shell,index,fa,la,fb,lb)
    ATOMVEC(:) :: self
   ! Get the SHELL2 object "shell" correponding to the pair index "index"
   ! Also return the basis function start indices "fa", "la", etc ...
      INT, IN :: index
      SHELL2, OUT :: shell
      INT, OUT :: fa,la,fb,lb
      INT :: a,b,aa,sa,bb,sb
      STACK("ATOMVEC:get_shell_pair")
      START_TIMER("ATOMVEC:get_shell_pair")
      ENSURE(index_info_created,"ATOMVEC:get_shell_pair ... no index information")
      a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
      b  = index - a*(a-1)/2
      fa = first_basis_fn_4_shell(a) ! These are module variables
      fb = first_basis_fn_4_shell(b)
      la = last_basis_fn_4_shell(a)
      lb = last_basis_fn_4_shell(b)
      aa = atom_4_shell(a)
      bb = atom_4_shell(b)
      sa = atom_shell_4_shell(a)
      sb = atom_shell_4_shell(b)
      call copy_(shell,self(aa)%basis%shell(sa), self(bb)%basis%shell(sb), &
                 self(aa)%pos, self(bb)%pos )
     STOP_TIMER("ATOMVEC:get_shell_pair")
      UNSTACK
   end subroutine

   subroutine get_shell_pair_1(self,shell,index,fa,la,fb,lb,atom_a,atom_b)
    ATOMVEC(:) :: self
   ! Get the SHELL2 object "shell" correponding to the pair index "index"
   ! Also return the basis function start indices "fa", "la", etc ...
   ! Plus the atoms the shells are located on, "atom_a" and "atom_b".
     INT, IN :: index
     SHELL2, OUT :: shell
     INT, OUT :: fa,la,fb,lb,atom_a,atom_b
     INT :: a,b,sa,sb
     STACK("ATOMVEC:get_shell_pair_1")
     START_TIMER("ATOMVEC:get_shell_pair_1")
     ENSURE(index_info_created,"ATOMVEC:get_shell_pair_1 ... no index information")
     a  = (1+int(sqrt(8.0d0*index-7.0d0)))/2
     b  = index - a*(a-1)/2
     fa = first_basis_fn_4_shell(a)
     fb = first_basis_fn_4_shell(b)
     la = last_basis_fn_4_shell(a)
     lb = last_basis_fn_4_shell(b)
     atom_a = atom_4_shell(a)
     atom_b = atom_4_shell(b)
     sa = atom_shell_4_shell(a)
     sb = atom_shell_4_shell(b)
     call copy_(shell,self(atom_a)%basis%shell(sa), self(atom_b)%basis%shell(sb), &
                self(atom_a)%pos, self(atom_b)%pos )
     STOP_TIMER("ATOMVEC:get_shell_pair_1")
      UNSTACK
   end subroutine

   subroutine make_index_info(self)
    ATOMVEC(:) :: self
   ! Define a vector of atom numbers corresponding to the molecule
   ! basis set shell numbers; also define a vector of atom shell numbers
   ! corresponding to the molecule basis set shell number
     STACK("ATOMVEC:make_index_info")
     START_TIMER("ATOMVEC:make_index_info")
     atom_4_shell       => atom_for_shell_(self)
     atom_shell_4_shell => atom_shell_for_shell_(self)
     first_shell_4_atom => first_shell_for_atom_(self)
     call get_shell_limits_(self,first_basis_fn_4_shell, last_basis_fn_4_shell)
     call make_atom_basis_fn_limits_(self,first_basis_fn_4_atom,last_basis_fn_4_atom)
     index_info_created = TRUE
     STOP_TIMER("ATOMVEC:make_index_info")
      UNSTACK
   end subroutine

   subroutine destroy_index_info(self)
    ATOMVEC(:) :: self
   ! Destroythe private index information. There may be problems with this for
   ! compilers without default initialisation ...
     STACK("ATOMVEC:destroy_index_info")
     START_TIMER("ATOMVEC:destroy_index_info")
     call destroy_(atom_4_shell)
     call destroy_(atom_shell_4_shell)
     call destroy_(first_shell_4_atom)
     call destroy_(first_basis_fn_4_shell)
     call destroy_(last_basis_fn_4_shell)
     call destroy_(first_basis_fn_4_atom)
     call destroy_(last_basis_fn_4_atom)
     index_info_created = FALSE
     STOP_TIMER("ATOMVEC:destroy_index_info")
      UNSTACK
   end subroutine

   subroutine make_coppens_interpolators(self)
    ATOMVEC(:) :: self
   ! Make a unique set of coppensbasis orbital density interpolators.
   ! NOTE: This requires careful destroying.
      INTVEC(:), PTR :: unique_atom,atom_kind
      INT :: n_kind,k,u,j
      STACK("ATOMVEC:make_coppens_interpolators")
      START_TIMER("ATOMVEC:make_coppens_interpolators")
      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind   ! Get interpolators only for unique atoms
        u = unique_atom(k)
        call make_interpolator_(self(u)%coppensbasis)
      ! self(u).coppensbasis.interpolator.put
        do j = u+1,size(self) ! ... pointer assign the rest
           if (atom_kind(j)/=k) cycle
           self(j)%coppensbasis%interpolator => self(u)%coppensbasis%interpolator
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)
     STOP_TIMER("ATOMVEC:make_coppens_interpolators")
      UNSTACK
   end subroutine

   subroutine destroy_coppens_interpolators(self)
    ATOMVEC(:) :: self
   ! Destroy the coppens interpolators for each atom. NOTE: This assumes that
   ! they were created only by the routine "make_coppens_interpolators".
      INTVEC(:), PTR :: unique_atom,atom_kind
      INT :: n_kind,k,u,j
      STACK("ATOMVEC:destroy_coppens_interpolators")
      START_TIMER("ATOMVEC:destroy_coppens_interpolators")
      if (NOT coppens_interpolators_exist_(self)) then; STOP_TIMER("ATOMVEC:destroy_coppens_interpolators") UNSTACK return; end if
      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind   ! Destroy interpolators only for unique atoms
        u = unique_atom(k)
        call destroy_(self(u)%coppensbasis%interpolator)
        do j = u+1,size(self) ! ... nullify the rest
           if (atom_kind(j)/=k) cycle
           nullify(self(j)%coppensbasis%interpolator)
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)
     STOP_TIMER("ATOMVEC:destroy_coppens_interpolators")
      UNSTACK
   end subroutine

   function coppens_interpolators_exist(self) result(res)
    ATOMVEC(:) :: self
   ! Returns TRUE if all the interpolators exist. If so, it is assumed they were
   ! created by make_coppens_interpolators.
      BIN :: res
      INT :: i
      STACK("ATOMVEC:coppens_interpolators_exist")
      START_TIMER("ATOMVEC:coppens_interpolators_exist")
      res = TRUE
      do i = 1,size(self)
         if (associated(self(i)%coppensbasis)) then
            if (associated(self(i)%coppensbasis%interpolator)) then
               cycle
            end if
         end if
         res = FALSE
         exit
      end do
     STOP_TIMER("ATOMVEC:coppens_interpolators_exist")
      CHECK
   end function

   subroutine make_slater_interpolators(self)
    ATOMVEC(:) :: self
   ! Make a unique set of slaterbasis orbital density interpolators.
   ! NOTE: This requires careful destroying.
      INTVEC(:), PTR :: unique_atom,atom_kind
      INT :: n_kind,k,u,j
      STACK("ATOMVEC:make_slater_interpolators")
      START_TIMER("ATOMVEC:make_slater_interpolators")
      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind   ! Get interpolators only for unique atoms
        u = unique_atom(k)
        call make_interpolator_(self(u)%slaterbasis)
      ! self(u).slaterbasis.interpolator.put
        do j = u+1,size(self) ! ... pointer assign the rest
           if (atom_kind(j)/=k) cycle
           self(j)%slaterbasis%interpolator => self(u)%slaterbasis%interpolator
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)
     STOP_TIMER("ATOMVEC:make_slater_interpolators")
      UNSTACK
   end subroutine

   subroutine destroy_slater_interpolators(self)
    ATOMVEC(:) :: self
   ! Destroy the slater interpolators for each atom. NOTE: This assumes that
   ! they were created only by the routine "make_slater_interpolators".
      INTVEC(:), PTR :: unique_atom,atom_kind
      INT :: n_kind,k,u,j
      STACK("ATOMVEC:destroy_slater_interpolators")
      START_TIMER("ATOMVEC:destroy_slater_interpolators")
      if (NOT slater_interpolators_exist_(self)) then; STOP_TIMER("ATOMVEC:destroy_slater_interpolators") UNSTACK return; end if
      call make_unique_atom_list_(self,unique_atom,atom_kind,n_kind)
      do k = 1,n_kind   ! Destroy interpolators only for unique atoms
        u = unique_atom(k)
        call destroy_(self(u)%slaterbasis%interpolator)
        do j = u+1,size(self) ! ... nullify the rest
           if (atom_kind(j)/=k) cycle
           nullify(self(j)%slaterbasis%interpolator)
        end do
      end do
      call destroy_(unique_atom)
      call destroy_(atom_kind)
     STOP_TIMER("ATOMVEC:destroy_slater_interpolators")
      UNSTACK
   end subroutine

   function slater_interpolators_exist(self) result(res)
    ATOMVEC(:) :: self
   ! Returns TRUE if all the interpolators exist. If so, it is assumed they were
   ! created by make_slater_interpolators.
      BIN :: res
      INT :: i
      STACK("ATOMVEC:slater_interpolators_exist")
      START_TIMER("ATOMVEC:slater_interpolators_exist")
      res = TRUE
      do i = 1,size(self)
         if (associated(self(i)%slaterbasis)) then
            if (associated(self(i)%slaterbasis%interpolator)) then
               cycle
            end if
         end if
         res = FALSE
         exit
      end do
     STOP_TIMER("ATOMVEC:slater_interpolators_exist")
      CHECK
   end function

end
