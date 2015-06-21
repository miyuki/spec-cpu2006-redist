!-------------------------------------------------------------------------------
!
! CIF: An object for processing Crystallographic Information Files (CIF).
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
! $Id: cif.foo,v 1.2.2.8 2003/11/11 02:41:45 dylan Exp $
!-------------------------------------------------------------------------------

module CIF_MODULE

#  include "cif.use"

   implicit none

#  include "macros"
#  include "cif.int"


contains

   subroutine create(self,name)
    CIF :: self
   ! Create the object
     PTR :: self
     STR(*), optional :: name
     STACK("CIF:create")
     START_TIMER("CIF:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(CIF_SIZE)
     call nullify_ptr_part_(self) 
     call set_defaults_(self,name)
     STOP_TIMER("CIF:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CIF :: self
   ! Destroy the object
      PTR :: self
      STACK("CIF:destroy")
      START_TIMER("CIF:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CIF:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      deallocate(self)
      DELETE_MEMORY(CIF_SIZE)
     STOP_TIMER("CIF:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    CIF :: self
   ! Nullify the pointer parts
      STACK("CIF:nullify_ptr_part")
      START_TIMER("CIF:nullify_ptr_part")
      nullify(self%file)
     STOP_TIMER("CIF:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    CIF :: self
   ! Destroy the pointer parts
      STACK("CIF:destroy_ptr_part")
      START_TIMER("CIF:destroy_ptr_part")
      call destroy_(self%file)
     STOP_TIMER("CIF:destroy_ptr_part")
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

   subroutine create_copy(self,cif)
    CIF :: self
   ! Create a copy.
      PTR :: self
      CIF, IN :: cif
      STACK("CIF:create_copy")
      START_TIMER("CIF:create_copy")
      call create_(self)
      call copy_(self,cif)
     STOP_TIMER("CIF:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,cif)
    CIF :: self
   ! Set self to be a copy of "cif"
      CIF, IN :: cif
      STACK("CIF:copy")
      START_TIMER("CIF:copy")
      self = cif
      if (associated(cif%file)) call create_copy_(self%file,cif%file)
     STOP_TIMER("CIF:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self,name)
    CIF :: self
   ! Set up defaults, especially the CIF file "name" if it is present.
      STR(*), optional :: name
      STACK("CIF:set_defaults")
      START_TIMER("CIF:set_defaults")
      self%start_of_data = 1
      self%end_of_data = 0
      self%data_block_found = FALSE
      if (present(name)) call create_(self%file,name)
     STOP_TIMER("CIF:set_defaults")
      UNSTACK
   end subroutine

   subroutine open(self)
    CIF :: self
   ! Create and open the CIF
      STACK("CIF:open")
      START_TIMER("CIF:open")
      ENSURE(associated(self%file),"CIF:open ... file not created")
    ! .file.quote_chars = "'" ! disallow ", it is used as an axis symbol !
      call open_(self%file,for="read")
     STOP_TIMER("CIF:open")
      CHECK
   end subroutine

   subroutine close(self)
    CIF :: self
   ! Close the CIF, and destroy it
      STACK("CIF:close")
      START_TIMER("CIF:close")
      ENSURE(associated(self%file),"CIF:close ... file not created")
      call close_(self%file)
     STOP_TIMER("CIF:close")
      CHECK
   end subroutine

!  *****************
!  Find/read methods
!  *****************

   subroutine find_data_block(self,block_name,found)
    CIF :: self
   ! Read and find the start of the data block with name "block_name", starting
   ! from ".start_of_data", and store its ".data_block_name". If present "found"
   ! is set TRUE if the bblock is found, else FALSE; if it is not present and
   ! the data block is not found then it is an error.
      STR(*), IN :: block_name
      BIN, optional, OUT :: found
      STACK("CIF:find_data_block")
      START_TIMER("CIF:find_data_block")
      ENSURE(associated(self%file),"CIF:find_data_block ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_data_block ... CIF file has not been opened")
      do
         call find_next_data_block_(self)
         if (present(found)) then
            found = self%data_block_found
            if (NOT found) then; STOP_TIMER("CIF:find_data_block") CHECK return; end if
         else
            ENSURE(self%data_block_found,"CIF:find_data_block ... cant find data block with name " // trim(block_name))
         end if
         if (self%data_block_name==block_name) exit
         self%start_of_data = self%end_of_data 
      end do
     STOP_TIMER("CIF:find_data_block")
      CHECK
   end subroutine

   subroutine find_next_data_block(self)
    CIF :: self
   ! Read through and find the next data block, and store its ".data_block_name",
   ! ".start_of_data", and ".end_of_data" line numbers. The search begins from
   ! line ".start_of_data". The routine returns if no data block can be found
   ! and in that case ".data_block_found" is set to FALSE.
      STACK("CIF:find_next_data_block")
      START_TIMER("CIF:find_next_data_block")
      ENSURE(associated(self%file),"CIF:find_next_data_block ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_next_data_block ... CIF file has not been opened")
      call find_start_of_data_block_(self)
      if (NOT self%data_block_found) then; STOP_TIMER("CIF:find_next_data_block") CHECK return; end if
      call find_end_of_data_block_(self)
     STOP_TIMER("CIF:find_next_data_block")
      CHECK
   end subroutine

   subroutine find_start_of_data_block(self)
    CIF :: self
   ! Read through and find a data block, and store its ".data_block_name",
   ! ".start_of_data". The search begins from line ".start_of_data". The
   ! ".data_block_found" is set to TRUE if the block is found, otherwise it is
   ! set FALSE and the routine returns. The cursor is left after the starting
   ! data block token, or at the starting position if nothing is found.
      STACK("CIF:find_start_of_data_block")
      START_TIMER("CIF:find_start_of_data_block")
      ENSURE(associated(self%file),"CIF:find_start_of_data_block ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_start_of_data_block ... CIF file has not been opened")
      call look_for_item_(self%file,"data_", &
                          from=self%start_of_data, &
                          head_match=TRUE, &
                          found=self%data_block_found)
      if (NOT self%data_block_found) then; STOP_TIMER("CIF:find_start_of_data_block") CHECK return; end if
      call move_to_previous_item_(self%file)
      call read_(self%file,self%data_block_name)
      self%data_block_name = self%data_block_name(6:) ! without data_ at start
      self%start_of_data = line_number_(self%file)
     STOP_TIMER("CIF:find_start_of_data_block")
      CHECK
   end subroutine

   subroutine find_end_of_data_block(self)
    CIF :: self
   ! Read through and find the end of a data block, starting from the first line
   ! after ".start_of_data", and store this end in ".end_of_data". The cursor is
   ! left at the end of the data block (possibly at the end of the file if there
   ! was no explicit block terminator found). NOTE: There is always an ending to
   ! a data block.
      STACK("CIF:find_end_of_data_block")
      START_TIMER("CIF:find_end_of_data_block")
      ENSURE(associated(self%file),"CIF:find_end_of_data_block ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_end_of_data_block ... CIF file has not been opened")
      call look_for_item_(self%file,"_eof", return_cursor=FALSE, &
                                  from=self%start_of_data+1, &
                                  end_tokens=(/"data_","_eof "/))
      if (self%file%no_of_lines>0) then; self%end_of_data = self%file%no_of_lines
      else;                          self%end_of_data = line_number_(self%file)
      end if
     STOP_TIMER("CIF:find_end_of_data_block")
      CHECK
   end subroutine

   subroutine find_crystal_data_block(self,found)
    CIF :: self
   ! Read through and find the first acceptable crystal data block, starting
   ! from ".start_of_data", and store its ".data_block_name". It is an error if
   ! a data block is not found; except in the case when "found" is present, in
   ! which case it is set TRUE if the data block exists, otherwise FALSE.
      BIN, OUT, optional :: found
      BINVEC(4) :: there
      INT :: first,last
      STACK("CIF:find_crystal_data_block")
      START_TIMER("CIF:find_crystal_data_block")
      ENSURE(associated(self%file),"CIF:find_crystal_data_block ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_crystal_data_block ... CIF file has not been opened")
      do
         call find_next_data_block_(self)
         if (present(found)) then
            found = self%data_block_found
            if (NOT found) then; STOP_TIMER("CIF:find_crystal_data_block") CHECK return; end if
         else
            ENSURE(self%data_block_found,"CIF:find_crystal_data_block ... cant find valid data block in file: "//trim(self%file%name))
         end if
         first = self%start_of_data
         last  = self%end_of_data
         call look_for_item_(self%file,"_symmetry_space_group_name",from=first,until=last,found=there(1))
         call look_for_item_(self%file,"_cell_length",from=first,until=last,found=there(2))
         call look_for_item_(self%file,"_atom_site_label",from=first,until=last,found=there(3))
         if (NOT there(3)) &
         call look_for_item_(self%file,"_atom_site_type_symbol",from=first,until=last,found=there(3))
         call look_for_item_(self%file,"_atom_site_fract",from=first,until=last,found=there(4))
         if (all(there)) exit ! This is really a crystal info data block
         self%start_of_data = self%end_of_data 
      end do
      if (present(found)) found = TRUE
     STOP_TIMER("CIF:find_crystal_data_block")
      CHECK
   end subroutine

   subroutine find_item(self,ID,found)
    CIF :: self
   ! Find a (non-looped) data item with identifier "ID" in the current data
   ! block, and leave the file cursor positioned just after it. If present,
   ! "found" is set TRUE if "ID" is found, else it is set FALSE and the routine
   ! returns without an error. NOTE: it is *not* checked that "ID" is a true
   ! non-looped item. 
      STR(*), IN :: ID
      BIN, OUT, optional :: found
      BIN :: fnd
      STACK("CIF:find_item")
      START_TIMER("CIF:find_item")
      ENSURE(associated(self%file),"CIF:find_item ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_item ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:find_item ... no data block found")
      ENSURE(self%end_of_data>self%start_of_data,"CIF:find_item ... no data block found")
      call look_for_item_(self%file,ID,from=self%start_of_data,until=self%end_of_data,found=fnd,exact_match=TRUE)
      if (present(found)) then
         found = fnd
         if (NOT found) then; STOP_TIMER("CIF:find_item") CHECK return; end if
      else if (NOT fnd) then
         DIE("CIF:find_item ... data item "//trim(ID)//" not found")
      end if
     STOP_TIMER("CIF:find_item")
      CHECK
   end subroutine

   subroutine read_item(self,ID,item,itemvec,found)
    CIF :: self
   ! Read a non-looped STR data item with string identifier "ID" from the data
   ! block.  The result is put in "item" if it is a single string, or "itemvec"
   ! if it is a multi-line semicolon delimited paragraph. If present, "found" is
   ! set TRUE if "ID" is found.
      STR(*), IN :: ID
      STR(STR_SIZE), OUT :: item
      STRVEC(STR_SIZE,:), PTR :: itemvec
      BIN, OUT, optional :: found
      STR(STR_SIZE) :: semicolon
      STACK("CIF:read_item")
      START_TIMER("CIF:read_item")
      ENSURE(associated(self%file),"CIF:read_item ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:read_item ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:read_item ... no data block found")
      call find_item_(self,ID,found)
      if (present(found)) then
        if (NOT found) then; STOP_TIMER("CIF:read_item") UNSTACK return; end if
      end if
      call read_(self%file,item) ! read the item
      semicolon = ";"
      if (item==semicolon AND self%file%buffer%item_start==1) then
       ! quote_chars = .file.quote_chars     ! switch off quotes
       ! .file.set_quote_chars(" ")
         call create_(itemvec,0)
         item = rest_of_line_(self%file)
         if (item/=" ") call append_(itemvec,item)
         do
           call read_(self%file,item)
           if (item==semicolon AND self%file%buffer%item_start==1) exit
           call append_(itemvec,self%file%buffer%string)
           call read_line_(self%file)
         end do
         ENSURE(size(itemvec)>0,"CIF:read_item ... zero length string")
         if (size(itemvec)==1) then
            item = itemvec(1)
            call destroy_(itemvec)
         else
            item = " "
         end if
       ! .file.set_quote_chars(quote_chars)  ! put back quotes
      end if
     STOP_TIMER("CIF:read_item")
      UNSTACK
   end subroutine

   subroutine read_item_1(self,ID,real,error,found)
    CIF :: self
   ! Read a non-looped REAL data item with string identifier "ID" from the data
   ! block.  The result is put in "real". If present, andy "error" associated
   ! with "real" is also read. If present, "found" is set TRUE if "ID" is found.
      STR(*) :: ID
      REAL :: real
      REAL, optional :: error
      BIN, OUT, optional :: found
      STACK("CIF:read_item_1")
      START_TIMER("CIF:read_item_1")
      ENSURE(associated(self%file),"CIF:read_item_1 ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:read_item_1 ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:read_item_1 ... no data block found")
      call find_item_(self,ID,found)
      if (present(found)) then
        if (NOT found) then; STOP_TIMER("CIF:read_item_1") CHECK return; end if
      end if
      if (present(error)) then; call read_(self%file,real,error)
      else;                     call read_(self%file,real)
      end if
     STOP_TIMER("CIF:read_item_1")
      CHECK
   end subroutine

   subroutine read_item_2(self,ID,int,found)
    CIF :: self
   ! Read a non-looped INT data item with string identifier "ID" from the data block.
   ! The result is put in "int". If present, "found" is set TRUE if "ID" is found.
      STR(*) :: ID
      INT :: int
      BIN, OUT, optional :: found
      STACK("CIF:read_item_2")
      START_TIMER("CIF:read_item_2")
      ENSURE(associated(self%file),"CIF:read_item_2 ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:read_item_2 ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:read_item_2 ... no data block found")
      call find_item_(self,ID,found)
      if (present(found)) then
        if (NOT found) then; STOP_TIMER("CIF:read_item_2") CHECK return; end if
      end if
      call read_(self%file,int)
     STOP_TIMER("CIF:read_item_2")
      CHECK
   end subroutine

   subroutine find_looped_item(self,ID,found,n_item,n_data)
    CIF :: self
   ! Find a looped data item with identifier "ID" in the current data block, and
   ! leave the file cursor just before the actual looped data.  If present,
   ! "found" is set TRUE if "ID" is found, else it is set FALSE and the routine
   ! returns without an error. If present, "n_item" is set to the position
   ! number of the item in the looped list, and "n_data" is set to the number of
   ! data elements in the looped list.
      STR(len=*) :: ID
      BIN, OUT, optional :: found
      INT, OUT, optional :: n_item,n_data
      BIN :: fnd
      STR(STR_SIZE) :: word
      INT :: n_dat,n_itm
      STACK("CIF:find_looped_item")
      START_TIMER("CIF:find_looped_item")
      ENSURE(associated(self%file),"CIF:find_looped_item ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:find_looped_item ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:find_looped_item ... no data block found")
      ENSURE(ID(1:1)=="_","CIF:find_looped_item ... ID list is not a looped datum")
      call look_for_item_(self%file,ID,from=self%start_of_data, &
                             until=self%end_of_data, &
                             found=fnd, &
                             exact_match=TRUE)
      if (present(found)) then
         found = fnd
         if (NOT found) then; STOP_TIMER("CIF:find_looped_item") CHECK return; end if
      else if (NOT fnd) then
         DIE("CIF:find_looped_item ... data item "//trim(ID)//" not found")
      end if
      call look_backwards_for_item_(self%file,"loop_",until=self%start_of_data,found=fnd)
      if (present(found)) then
         found = fnd
         if (NOT found) then; STOP_TIMER("CIF:find_looped_item") CHECK return; end if
      else if (NOT fnd) then
         DIE("CIF:find_looped_item ... data item "//trim(ID)//" is not looped")
      end if
      n_dat = 0
      do
         call read_(self%file,word) ! read first looped data item
         if (ID==word) n_itm = n_dat + 1
         if (word(1:1)/="_") exit ! this is an actual datum
         n_dat = n_dat + 1
      end do
      call move_to_previous_item_(self%file)
      if (present(n_data)) n_data = n_dat
      if (present(n_item)) n_item = n_itm
     STOP_TIMER("CIF:find_looped_item")
      CHECK
   end subroutine

   subroutine read_looped_item(self,ID,ivec,found)
    CIF :: self
   ! Read a looped REALVEC data item with string identifier "ID" from the data block.
   ! The result is put in "ivec". NOTE: "ivec" is created. If present,
   ! "found" is set TRUE if "ID" is found, else it is set FALSE and the routine
   ! returns without an error.
      STR(len=*) :: ID
      INTVEC(:), PTR :: ivec
      BIN, OUT, optional :: found
      INT :: n_data,n_item,i
      STR(STR_SIZE) :: word
      INT :: val
      STACK("CIF:read_looped_item")
      START_TIMER("CIF:read_looped_item")
      ENSURE(associated(self%file),"CIF:read_looped_item ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:read_looped_item ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:read_looped_item ... no data block found")
      ENSURE(ID(1:1)=="_","CIF:read_looped_item ... ID list is not a looped datum")
      call find_looped_item_(self,ID,found,n_item,n_data)
      if (present(found)) then
        if (NOT found) then; STOP_TIMER("CIF:read_looped_item") UNSTACK return; end if
      end if
      call create_(ivec,0)
      do
         do i = 1,n_item-1
            call skip_next_item_(self%file)
         end do
         call read_(self%file,val)
         call append_(ivec,val)
         do i = n_item+1,n_data
            call skip_next_item_(self%file)
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do
     STOP_TIMER("CIF:read_looped_item")
      UNSTACK
   end subroutine

   subroutine read_looped_item_1(self,ID,vec,error,found)
    CIF :: self
   ! Read a looped REALVEC data item with string identifier "ID" from the data block.
   ! The result is put in "vec" and the associated errors are placed in "error".
   ! NOTE: "vec" and "err" are created. If present, "found" is set TRUE if "ID"
   ! is found, else it is set FALSE and the routine returns without an error.
      STR(len=*) :: ID
      REALVEC(:), PTR :: vec
      REALVEC(:), PTR, optional :: error
      BIN, OUT, optional :: found
      INT :: n_data,n_item,i
      STR(STR_SIZE) :: word
      REAL :: val,err
      STACK("CIF:read_looped_item_1")
      START_TIMER("CIF:read_looped_item_1")
      ENSURE(associated(self%file),"CIF:read_looped_item_1 ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:read_looped_item_1 ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:read_looped_item_1 ... no data block found")
      ENSURE(ID(1:1)=="_","CIF:read_looped_item_1 ... ID list is not have a looped datum")
      call find_looped_item_(self,ID,found,n_item,n_data)
      if (present(found)) then
        if (NOT found) then; STOP_TIMER("CIF:read_looped_item_1") UNSTACK return; end if
      end if
      call create_(vec,0)
      if (present(error)) call create_(error,0)
      do
         do i = 1,n_item-1
            call skip_next_item_(self%file)
         end do
         if (present(error)) then ! read both value and error
            call read_(self%file,val,err)
            call append_(vec,val)
            call append_(error,err)
         else                     ! read only value
            call read_(self%file,val,err)
            call append_(vec,val)
         end if
         do i = n_item+1,n_data
            call skip_next_item_(self%file)
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do
     STOP_TIMER("CIF:read_looped_item_1")
      UNSTACK
   end subroutine

   subroutine read_looped_item_2(self,ID,strvec,found)
    CIF :: self
   ! Read a looped REALVEC data item with string identifier "ID" from the data block.
   ! The result is put in "strvec". NOTE: "strvec" is created. If present,
   ! "found" is set TRUE if "ID" is found, else it is set FALSE and the routine
   ! returns without an error.
      STR(len=*) :: ID
      STRVEC(STR_SIZE,:), PTR :: strvec
      BIN, OUT, optional :: found
      INT :: n_data,n_item,i
      STR(STR_SIZE) :: word
      STACK("CIF:read_looped_item_2")
      START_TIMER("CIF:read_looped_item_2")
      ENSURE(associated(self%file),"CIF:read_looped_item_2 ... CIF file has not been created")
      ENSURE(is_open_(self%file),"CIF:read_looped_item_2 ... CIF file has not been opened")
      ENSURE(self%start_of_data>0,"CIF:read_looped_item_2 ... no data block found")
      ENSURE(ID(1:1)=="_","CIF:read_looped_item_2 ... ID list is not have a looped datum")
      call find_looped_item_(self,ID,found,n_item,n_data)
      if (present(found)) then
        if (NOT found) then; STOP_TIMER("CIF:read_looped_item_2") UNSTACK return; end if
      end if
      call create_(strvec,0)
      do
         do i = 1,n_item-1
            call skip_next_item_(self%file)
         end do
         call read_(self%file,word)
         call append_(strvec,word)
         do i = n_item+1,n_data
            call skip_next_item_(self%file)
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
      end do
     STOP_TIMER("CIF:read_looped_item_2")
      UNSTACK
   end subroutine

   subroutine find_looped_items(self,ID,found,ID_pos,ID_ind,n_labels)
    CIF :: self
   ! Find a set of looped data items all in the same loop, with identifiers
   ! "ID", in the current data block, and leave the file cursor just before the
   ! actual looped data.  If present, "found" is set TRUE if "ID" is found, else
   ! it is set FALSE and the routine returns without an error. 
      STRVEC(len=*,:) :: ID
      BIN, OUT, optional :: found
      INTVEC(:), OUT, optional :: ID_pos,ID_ind
      INT, OUT, optional :: n_labels
      BIN :: fnd
      STR(STR_SIZE) :: word
      INT :: label,item
   STACK("CIF:find_looped_items")
   START_TIMER("CIF:find_looped_items")
   ENSURE(associated(self%file),"CIF:find_looped_items ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:find_looped_items ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:find_looped_items ... no data block found")
   ENSURE(size(ID)>0,"CIF:find_looped_items ... no items in ID list")
   ENSURE(all(ID(:)(1:1)=="_"),"CIF:find_looped_items ... ID list does not have a looped datum")
   ENSURE(NOT has_repetitions_(ID),"CIF:find_looped_items ... ID list has repetitions")
      if (present(ID_pos)) then
      ENSURE(size(ID)==size(ID_pos),"CIF:find_looped_items ... ID and ID_pos are inconsistent")
      end if
      if (present(ID_ind)) then
      ENSURE(size(ID)==size(ID_ind),"CIF:find_looped_items ... ID and ID_ind are inconsistent")
      end if
      call look_for_any_item_(self%file,ID,from=self%start_of_data, &
                              until=self%end_of_data, &
                              found=fnd, &
                              exact_match=TRUE)
      if (present(found)) then
         found = fnd
         if (NOT found) then; STOP_TIMER("CIF:find_looped_items") CHECK return; end if
      else if (NOT fnd) then
         DIE("CIF:find_looped_items ... no data items found")
      end if
      call look_backwards_for_item_(self%file,"loop_",until=self%start_of_data,found=fnd)
      if (present(found)) then
         found = fnd
         if (NOT found) then; STOP_TIMER("CIF:find_looped_items") CHECK return; end if
      else if (NOT fnd) then
         DIE("CIF:find_looped_items ... at least one data item is not looped")
      end if
      item = 0
      label = 0
      do
         call read_(self%file,word) ! read looped data item
         if (any(ID==word)) then       ! This is an ID
            item = item + 1            ! The item number; this is not the ID index
            label = label + 1          ! The label number in the looped list
            if (present(ID_pos)) ID_pos(item) = label
            if (present(ID_ind)) ID_ind(item) = index_of_(ID,word)
         else if (word(1:1)=="_") then ! This is a looped data descriptor
            label = label + 1
         else                          ! This is not a looped descriptor
            exit
         end if
      end do
      fnd = item==size(ID) AND NOT has_repetitions_(ID_ind)
      if (present(found)) then; found = fnd
      else if (NOT fnd) then; DIE("CIF:find_looped_items ... not all data items found")
      end if
      call move_to_previous_item_(self%file)
      if (present(n_labels)) n_labels = label
     STOP_TIMER("CIF:find_looped_items")
      CHECK
   end subroutine

   subroutine read_looped_items(self,ID,mat,error,found)
    CIF :: self
   ! Read a looped set of data items "mat" with string identifiers "ID" from the
   ! data block.  The the associated errors are placed in "error".  NOTE: "mat"
   ! and "err" are created. If present, "found" is set TRUE if all the "ID" are
   ! found, else it is set FALSE and the routine returns without an error.
      STRVEC(len=*,:) :: ID
      REALMAT(:,:), PTR :: mat
      REALMAT(:,:), PTR, optional :: error
      BIN, OUT, optional :: found
      INTVEC(size(ID)) :: ID_pos,ID_ind
      INT :: n_labels,n,item,i,ind
      STR(STR_SIZE) :: word
      REAL :: val,err
   STACK("CIF:read_looped_items")
   START_TIMER("CIF:read_looped_items")
   ENSURE(associated(self%file),"CIF:read_looped_items ... CIF file has not been created")
   ENSURE(is_open_(self%file),"CIF:read_looped_items ... CIF file has not been opened")
   ENSURE(self%start_of_data>0,"CIF:read_looped_items ... no data block found")
      call find_looped_items_(self,ID,found,ID_pos,ID_ind,n_labels)
      if (present(found)) then
         if (NOT found) then; STOP_TIMER("CIF:read_looped_items") UNSTACK return; end if
      end if
      call create_(mat,size(ID),1)
      if (present(error)) call create_(error,size(ID),1)
      n = 1
      do ! loop over all the n "looped" data
         item = 1
         do i = 1,n_labels
            if (item>size(ID)) then
               call skip_next_item_(self%file)
            else if (i/=ID_pos(item)) then
               call skip_next_item_(self%file)
            else
               ind = ID_ind(item)
               call read_(self%file,val,err)
               ! stdout.text(.file.buffer.string.trim)
               ! stdout.text(.file.buffer.cursor_pointer)
               mat(ind,n) = val
               if (present(error)) error(ind,n) = err
               item = item + 1 ! next ID label
            end if
         end do
         if (at_end_of_file_(self%file)) exit
         word = next_item_(self%file)
         if (word(1:1)=="_") exit
         if (word(1:5)=="loop_") exit
         if (word(1:5)=="data_") exit
         if (word(1:5)=="save_") exit
         if (word(1:7)=="global_") exit
         n = n + 1
         call expand_columns_(mat,n)
         if (present(error)) call expand_columns_(error,n)
      end do
     STOP_TIMER("CIF:read_looped_items")
      UNSTACK
   end subroutine

end
