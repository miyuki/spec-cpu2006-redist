!---------------------------------------------------------------------------
!
!  FILE : Unformatted sequential advancing I/O, for fast mass data storage
!
!  NOTE: If ascii files are required consider using TEXTFILE.
!
!  A file is referred to only by a STR name. Unit numbers are not required.
!  Creation of a file object does not lead to creation of the actual file
!  on the computer, it merely creates the label for the file.
!  The actual file may already exist. If it does not exist, then an
!  FILE_open command will bring it into existence. Otherwise, FILE_open
!  will open the existing file. Closing a file does not delete the actual
!  file unless specifically requested.
!
!  When reading or writing an object to the file, it is assumed that
!  each object occupied one abstract "record". After a recor5d is written,
!  it may not be overwritten without destroying all data objects in the
!  following records. It is recommended that multiple data objects which
!  are logically related be stored in different files with appropriate names
!  for each file which indicate the relationship of the data objects
!  within them.
!
!  Strings are regarded as type STR for purposes of output to the file.
!
!  If the read or write statements give a segmentation fault for large
!  arrays or matrices, try increasing your stack size.
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
! $Id: file.foo,v 1.13.2.6 2003/11/13 05:34:39 reaper Exp $
!---------------------------------------------------------------------------

module FILE_MODULE

#  include "file.use"

   implicit none

#  include "macros"
#  include "file.int"


   public get_; interface get_
      module procedure read_str
      module procedure read_real
      module procedure read_cpx
      module procedure read_int
      module procedure read_bin
      module procedure read_intvec
      module procedure read_realvec
      module procedure read_cpxvec
      module procedure read_realmat
      module procedure read_realmat3
      module procedure read_realmat4
      module procedure read_intmat
      module procedure read_cpxmat
      module procedure read_cpxmat3
      module procedure read_cpxmat4
      module procedure read_cpxmat5
   end interface

   public read_; interface read_
      module procedure read_str
      module procedure read_real
      module procedure read_cpx
      module procedure read_int
      module procedure read_bin
      module procedure read_intvec
      module procedure read_realvec
      module procedure read_cpxvec
      module procedure read_realmat
      module procedure read_realmat3
      module procedure read_realmat4
      module procedure read_intmat
      module procedure read_cpxmat
      module procedure read_cpxmat3
      module procedure read_cpxmat4
      module procedure read_cpxmat5
   end interface

   public write_; interface write_
      module procedure write_int
      module procedure write_real
      module procedure write_cpx
      module procedure write_str
      module procedure write_bin
      module procedure write_vec
      module procedure write_intvec
      module procedure write_cpxvec
      module procedure write_mat
      module procedure write_mat3
      module procedure write_mat4
      module procedure write_intmat
      module procedure write_cpxmat
      module procedure write_cpxmat3
      module procedure write_cpxmat4
      module procedure write_cpxmat5
   end interface

   public put_; interface put_
      module procedure write_int
      module procedure write_real
      module procedure write_cpx
      module procedure write_str
      module procedure write_bin
      module procedure write_vec
      module procedure write_intvec
      module procedure write_cpxvec
      module procedure write_mat
      module procedure write_mat3
      module procedure write_mat4
      module procedure write_intmat
      module procedure write_cpxmat
      module procedure write_cpxmat3
      module procedure write_cpxmat4
      module procedure write_cpxmat5
   end interface

   public write_buffered_; interface write_buffered_
    module procedure write_buffered_real
    module procedure write_buffered_int
    module procedure write_buffered_cpx
    module procedure write_buffered_vec
    module procedure write_buffered_intvec
    module procedure write_buffered_cpxvec
    module procedure write_buffered_mat
    module procedure write_buffered_intmat
    module procedure write_buffered_cpxmat
    module procedure write_buffered_mat3
    module procedure write_buffered_cpxmat3
    module procedure write_buffered_mat4
    module procedure write_buffered_cpxmat4
    module procedure write_buffered_cpxmat5
  end interface

   public read_buffered_; interface read_buffered_
    module procedure read_buffered_real
    module procedure read_buffered_int
    module procedure read_buffered_cpx
    module procedure read_buffered_vec
    module procedure read_buffered_intvec
    module procedure read_buffered_cpxvec
    module procedure read_buffered_mat
    module procedure read_buffered_intmat
    module procedure read_buffered_cpxmat
    module procedure read_buffered_mat3
    module procedure read_buffered_cpxmat3
    module procedure read_buffered_mat4
    module procedure read_buffered_cpxmat4
    module procedure read_buffered_cpxmat5
  end interface

contains

!  *****************************
!  File creation type operations
!  *****************************

   subroutine create(self,name)
    FILE :: self
   ! Create a file-label object. Does not open the file.  Sets the filename if
   ! present.
      PTR :: self
      STR(*), optional :: name
      UNITNUMBER :: unit
      STACK("FILE:create")
      START_TIMER("FILE:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(FILE_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
      if (present(name)) then
        self%name = name
        call get_(unit,self%unit)
      end if
     STOP_TIMER("FILE:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    FILE :: self
   ! Destroy a file-label object
      PTR :: self
      UNITNUMBER :: unit
      STACK("FILE:destroy")
      START_TIMER("FILE:destroy")
      if (NOT associated(self)) then; STOP_TIMER("FILE:destroy") UNSTACK return; end if
      if (is_open_(self) AND unit_used_(self)) call close_(self)
      call free_(unit,self%unit)
      call destroy_ptr_part_(self)
      DELETE_MEMORY(FILE_SIZE)
      deallocate(self)
     STOP_TIMER("FILE:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    FILE :: self
   ! Nullify the pointer parts
      STACK("FILE:nullify_ptr_part")
      START_TIMER("FILE:nullify_ptr_part")
      nullify( self%real_buffer )
      nullify( self%int_buffer )
      nullify( self%cpx_buffer )
     STOP_TIMER("FILE:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    FILE :: self
   ! Destroy pointer parts
      STACK("FILE:destroy_ptr_part")
      START_TIMER("FILE:destroy_ptr_part")
      call destroy_(self%real_buffer)
      call destroy_(self%int_buffer)
      call destroy_(self%cpx_buffer)
     STOP_TIMER("FILE:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine create_copy(self,f)
    FILE :: self
   ! Create a copy a file "f"
      PTR :: self
       FILE :: f
      STACK("FILE:create_copy")
      START_TIMER("FILE:create_copy")
      call create_(self)
      call copy_(self,f)
     STOP_TIMER("FILE:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,f)
    FILE :: self
   ! Copy a file "f"
      FILE :: f
      STACK("FILE:copy")
      START_TIMER("FILE:copy")
      self = f
      if (associated(f%int_buffer)) &
         call create_copy_(self%int_buffer,f%int_buffer)
      if (associated(f%real_buffer)) &
         call create_copy_(self%real_buffer,f%real_buffer)
      if (associated(f%cpx_buffer)) &
         call create_copy_(self%cpx_buffer,f%cpx_buffer)
     STOP_TIMER("FILE:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    FILE :: self
   ! Set up defaults
      STACK("FILE:set_defaults")
      START_TIMER("FILE:set_defaults")
      self%record = 1
      self%io_status = 0
      self%file_status = "unknown"
      self%action      = "readwrite"
      self%buffered    = FALSE
      self%real_buffer_pos = 1
      self%int_buffer_pos = 1
      self%cpx_buffer_pos = 1
     STOP_TIMER("FILE:set_defaults")
      CHECK
   end subroutine

   subroutine open(self,for,buffered,type)
    FILE :: self
   ! Open the file. The file object must already be created.
   ! If present, "for" indicated whether the file is "read_write", "read-only"
   ! or "write-only"
   ! If present, "buffered" indicated whether the file is to be buffered, and
   ! the "type" of the buffer must also be specified as "real" or "int"
     STR(*), optional :: for
     BIN,    optional :: buffered
     STR(*), optional :: type
     STACK("FILE:open")
     START_TIMER("FILE:open")
     if (present(for)) then
        select case (for)
           case("readwrite ","read-write") ;              self%action = "readwrite"
           case("read      ","reading   ","read-only ") ; self%action = "read"
           case("write     ","writing   ","write-only") ; self%action = "write"
           case default; allocate(tonto%known_keywords(8))
           tonto%known_keywords(1) = "readwrite "
           tonto%known_keywords(2) = "read-write"
           tonto%known_keywords(3) = "read      "
           tonto%known_keywords(4) = "reading   "
           tonto%known_keywords(5) = "read-only "
           tonto%known_keywords(6) = "write     "
           tonto%known_keywords(7) = "writing   "
           tonto%known_keywords(8) = "write-only"
           call unknown_(tonto,for,"FILE:open")
           deallocate(tonto%known_keywords)
        end select
     end if
     self%file_status = "new"
     if (exists_(self)) self%file_status = "old"
     if (do_io_(tonto_parallel)) then
       open( unit   = self%unit,       &
           file   = self%name,         &
           status = self%file_status,  &
           access = "sequential",  &
           form   = "unformatted", & ! <---------
           action = self%action,       &
           iostat = self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     ENSURE(self%io_status==0,"FILE:open ... error opening "//trim(self%file_status)//" file "//self%name)
     if (present(buffered)) self%buffered = buffered
     if (self%buffered) then
        ENSURE(present(type),"FILE:open ... type of buffer must be specified")
        ENSURE(self%action/="readwrite","FILE:open ... buffer must be either read-only or write-only")
        select case (type)
           case("real")
             call create_(self%real_buffer,FILE_BUFFER_LENGTH)
             if (self%action == "read") call get_real_buffer_(self)
           case("int")
             call create_(self%int_buffer,FILE_BUFFER_LENGTH)
             if (self%action == "read") call get_int_buffer_(self)
           case("cpx")
             call create_(self%cpx_buffer,FILE_BUFFER_LENGTH)
             if (self%action == "read") call get_cpx_buffer_(self)
           case default
             DIE("FILE:open ... unknown buffer type, "//trim(type))
        end select
     end if
     STOP_TIMER("FILE:open")
      UNSTACK
   end subroutine

   subroutine close(self)
    FILE :: self
   ! Close the file
     STACK("FILE:close")
     START_TIMER("FILE:close")
     WARN_IF(NOT is_open_(self),"FILE:close ... file is not open")
     if (self%action=="write") then
       if (self%real_buffer_pos > 1) call flush_real_buffer_(self)
       if (self%int_buffer_pos > 1) call flush_int_buffer_(self)
       if (self%cpx_buffer_pos > 1) call flush_cpx_buffer_(self)
     end if
     call destroy_ptr_part_(self)
     if (do_io_(tonto_parallel)) then
       close(unit=self%unit,iostat=self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     STOP_TIMER("FILE:close")
      UNSTACK
   end subroutine

   subroutine close_and_delete(self)
    FILE :: self
   ! Close the file and delete it from the file system
     STACK("FILE:close_and_delete")
     START_TIMER("FILE:close_and_delete")
     WARN_IF(NOT is_open_(self),"FILE:close_and_delete ... file is not open")
     if (self%action=="write") then
       if (self%real_buffer_pos > 1) call flush_real_buffer_(self)
       if (self%int_buffer_pos > 1) call flush_int_buffer_(self)
       if (self%cpx_buffer_pos > 1) call flush_cpx_buffer_(self)
     end if
     call destroy_ptr_part_(self)
     if (do_io_(tonto_parallel)) then
      close(unit=self%unit,status="delete",iostat=self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     STOP_TIMER("FILE:close_and_delete")
      UNSTACK
   end subroutine

   subroutine delete(self)
    FILE :: self
   ! Delete the file from the file system
      STACK("FILE:delete")
      START_TIMER("FILE:delete")
      if (NOT is_open_(self)) call open_(self)
      call close_and_delete_(self)
     STOP_TIMER("FILE:delete")
      CHECK
   end subroutine

!  **************************
!  Data input type operations
!  **************************

   subroutine read_str(self,value)
    FILE :: self
   ! Read a default str from the file into variable "value"
     STR(*) :: value
     STACK("FILE:read_str")
     START_TIMER("FILE:read_str")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) value
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,value,0)
     ENSURE(self%io_status==0,"FILE:read_str ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     STOP_TIMER("FILE:read_str")
      CHECK
   end subroutine

   subroutine read_int(self,value)
    FILE :: self
   ! Read an integer from the file into variable "value"
     INT :: value
     STACK("FILE:read_int")
     START_TIMER("FILE:read_int")
     if (self%buffered) then
       call read_buffered_int_(self,value)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) value
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,value,0)
       ENSURE(self%io_status==0,"FILE:read_int ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_int")
      CHECK
   end subroutine

   subroutine read_real(self,value)
    FILE :: self
   ! Read a real from the file into variable "value"
     REAL :: value
     STACK("FILE:read_real")
     START_TIMER("FILE:read_real")
     if (self%buffered) then
       call read_buffered_real_(self,value)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) value
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,value,0)
       ENSURE(self%io_status==0,"FILE:read_real ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_real")
      CHECK
   end subroutine

   subroutine read_cpx(self,value)
    FILE :: self
   ! Read a complex REAL from the file into variable "value"
     CPX :: value
     STACK("FILE:read_cpx")
     START_TIMER("FILE:read_cpx")
     if (self%buffered) then
       call read_buffered_cpx_(self,value)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) value
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,value,0)
       ENSURE(self%io_status==0,"FILE:read_cpx ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_cpx")
      CHECK
   end subroutine

   subroutine read_bin(self,value)
    FILE :: self
   ! Read a logical from the file into variable "value"
     BIN :: value
     STACK("FILE:read_bin")
     START_TIMER("FILE:read_bin")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) value
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,value,0)
     ENSURE(self%io_status==0,"FILE:read_bin ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     STOP_TIMER("FILE:read_bin")
      CHECK
   end subroutine

   subroutine read_realvec(self,vec)
    FILE :: self
   ! Read a vector from the file into variable "vec"
     REALVEC(:) :: vec
     STACK("FILE:read_realvec")
     START_TIMER("FILE:read_realvec")
     if (self%buffered) then
       call read_buffered_vec_(self,vec)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) vec
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,vec,0)
       ENSURE(self%io_status==0,"FILE:read_realvec ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_realvec")
      CHECK
   end subroutine

   subroutine read_intvec(self,vec)
    FILE :: self
   ! Read a vector from the file into variable "vec"
     INTVEC(:) :: vec
     STACK("FILE:read_intvec")
     START_TIMER("FILE:read_intvec")
     if (self%buffered) then
       call read_buffered_intvec_(self,vec)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) vec
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,vec,0)
       ENSURE(self%io_status==0,"FILE:read_intvec ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_intvec")
      CHECK
   end subroutine

   subroutine read_cpxvec(self,vec)
    FILE :: self
   ! Read a vector from the file into variable "vec"
     CPXVEC(:) :: vec
     STACK("FILE:read_cpxvec")
     START_TIMER("FILE:read_cpxvec")
     if (self%buffered) then
       call read_buffered_cpxvec_(self,vec)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) vec
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,vec,0)
       ENSURE(self%io_status==0,"FILE:read_cpxvec ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_cpxvec")
      CHECK
   end subroutine

   subroutine read_realmat(self,mat)
    FILE :: self
   ! Read a matrix from the file into variable "mat"
     REALMAT(:,:) :: mat
     STACK("FILE:read_realmat")
     START_TIMER("FILE:read_realmat")
     if (self%buffered) then
       call read_buffered_mat_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_realmat ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_realmat")
      CHECK
   end subroutine

   subroutine read_intmat(self,mat)
    FILE :: self
   ! Read an integer matrix from the file into variable "mat"
     INTMAT(:,:) :: mat
     STACK("FILE:read_intmat")
     START_TIMER("FILE:read_intmat")
     if (self%buffered) then
       call read_buffered_intmat_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_intmat ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_intmat")
      CHECK
   end subroutine

   subroutine read_cpxmat(self,mat)
    FILE :: self
   ! Read a complex matrix from the file into variable "mat"
     CPXMAT(:,:) :: mat
     STACK("FILE:read_cpxmat")
     START_TIMER("FILE:read_cpxmat")
     if (self%buffered) then
       call read_buffered_cpxmat_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_cpxmat ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_cpxmat")
      CHECK
   end subroutine

   subroutine read_realmat3(self,mat)
    FILE :: self
   ! Read a matrix from the file into variable "mat"
     REALMAT3(:,:,:) :: mat
     STACK("FILE:read_realmat3")
     START_TIMER("FILE:read_realmat3")
     if (self%buffered) then
       call read_buffered_mat3_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_realmat3 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_realmat3")
      CHECK
   end subroutine

   subroutine read_cpxmat3(self,mat)
    FILE :: self
   ! Read a complex matrix from the file into variable "mat"
     CPXMAT3(:,:,:) :: mat
     STACK("FILE:read_cpxmat3")
     START_TIMER("FILE:read_cpxmat3")
     if (self%buffered) then
       call read_buffered_cpxmat3_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_cpxmat3 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_cpxmat3")
      CHECK
   end subroutine

   subroutine read_realmat4(self,mat)
    FILE :: self
   ! Read a matrix from the file into variable "mat"
     REALMAT4(:,:,:,:) :: mat
     STACK("FILE:read_realmat4")
     START_TIMER("FILE:read_realmat4")
     if (self%buffered) then
       call read_buffered_mat4_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_realmat4 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_realmat4")
      CHECK
   end subroutine

   subroutine read_cpxmat4(self,mat)
    FILE :: self
   ! Read a complex matrix from the file into variable "mat"
     CPXMAT4(:,:,:,:) :: mat
     STACK("FILE:read_cpxmat4")
     START_TIMER("FILE:read_cpxmat4")
     if (self%buffered) then
       call read_buffered_cpxmat4_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_cpxmat4 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_cpxmat4")
      CHECK
   end subroutine

   subroutine read_cpxmat5(self,mat)
    FILE :: self
   ! Read a complex matrix from the file into variable "mat"
     CPXMAT5(:,:,:,:,:) :: mat
     STACK("FILE:read_cpxmat5")
     START_TIMER("FILE:read_cpxmat5")
     if (self%buffered) then
       call read_buffered_cpxmat5_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       ENSURE(self%io_status==0,"FILE:read_cpxmat5 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if
     STOP_TIMER("FILE:read_cpxmat5")
      CHECK
   end subroutine

!  ********************
!  Data output routines
!  ********************

   subroutine write_int(self,value)
    FILE :: self
   ! Write an integer to the file as a record
      INT :: value
      STACK("FILE:write_int")
      START_TIMER("FILE:write_int")
      if (self%buffered) then
         call write_buffered_int_(self,value)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) value
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_int ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_int")
      CHECK
   end subroutine

   subroutine write_real(self,value)
    FILE :: self
   ! Write a REAL to the file as a record
      REAL :: value
      STACK("FILE:write_real")
      START_TIMER("FILE:write_real")
      if (self%buffered) then
         call write_buffered_real_(self,value)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) value
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_real ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_real")
      CHECK
   end subroutine

   subroutine write_cpx(self,value)
    FILE :: self
   ! Write a cpx to the file as a record
      CPX :: value
      STACK("FILE:write_cpx")
      START_TIMER("FILE:write_cpx")
      if (self%buffered) then
         call write_buffered_cpx_(self,value)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) value
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_cpx ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_cpx")
      CHECK
   end subroutine

   subroutine write_str(self,value)
    FILE :: self
   ! Write a str to the file as a record
      STR(*) :: value
      STR(len(value)) :: str1
      STACK("FILE:write_str")
      START_TIMER("FILE:write_str")
      str1 = value
      if (do_io_(tonto_parallel)) then
        write(unit=self%unit,iostat=self%io_status) str1
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      ENSURE(self%io_status==0,"FILE:write_str ... write error, error="// trim(to_str_(self%io_status)))
      self%record = self%record + 1
     STOP_TIMER("FILE:write_str")
      CHECK
   end subroutine

   subroutine write_bin(self,value)
    FILE :: self
   ! Write a bin to the file as a record
      BIN :: value
      STACK("FILE:write_bin")
      START_TIMER("FILE:write_bin")
      if (do_io_(tonto_parallel)) then
        write(unit=self%unit,iostat=self%io_status) value
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      ENSURE(self%io_status==0,"FILE:write_bin ... write error, error="// trim(to_str_(self%io_status)))
      self%record = self%record + 1
     STOP_TIMER("FILE:write_bin")
      CHECK
   end subroutine

   subroutine write_vec(self,vec)
    FILE :: self
   ! Write a vector to the file as a single record
      REALVEC(:) :: vec
      STACK("FILE:write_vec")
      START_TIMER("FILE:write_vec")
      if (self%buffered) then
         call write_buffered_vec_(self,vec)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) vec
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_vec ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_vec")
      CHECK
   end subroutine

   subroutine write_intvec(self,vec)
    FILE :: self
   ! Write a vector to the file as a single record
      INTVEC(:), IN :: vec
      STACK("FILE:write_intvec")
      START_TIMER("FILE:write_intvec")
      if (self%buffered) then
         call write_buffered_intvec_(self,vec)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) vec
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_intvec ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_intvec")
      CHECK
   end subroutine

   subroutine write_cpxvec(self,vec)
    FILE :: self
   ! Write a vector to the file as a single record
      CPXVEC(:) :: vec
      STACK("FILE:write_cpxvec")
      START_TIMER("FILE:write_cpxvec")
      if (self%buffered) then
         call write_buffered_cpxvec_(self,vec)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) vec
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_cpxvec ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_cpxvec")
      CHECK
   end subroutine

   subroutine write_intmat(self,mat)
    FILE :: self
   ! Write a matrix to the file as a single record
      INTMAT(:,:) :: mat
      STACK("FILE:write_intmat")
      START_TIMER("FILE:write_intmat")
      if (self%buffered) then
         call write_buffered_intmat_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_intmat ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_intmat")
      CHECK
   end subroutine

   subroutine write_mat(self,mat)
    FILE :: self
   ! Write a matrix to the file as a single record
      REALMAT(:,:) :: mat
      STACK("FILE:write_mat")
      START_TIMER("FILE:write_mat")
      if (self%buffered) then
         call write_buffered_mat_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_mat ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_mat")
      CHECK
   end subroutine

   subroutine write_mat3(self,mat)
    FILE :: self
   ! Write a matrix to the file as a single record
      REALMAT3(:,:,:) :: mat
      STACK("FILE:write_mat3")
      START_TIMER("FILE:write_mat3")
      if (self%buffered) then
         call write_buffered_mat3_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_mat3 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_mat3")
      CHECK
   end subroutine

   subroutine write_mat4(self,mat)
    FILE :: self
   ! Write a matrix to the file as a single record
      REALMAT4(:,:,:,:) :: mat
      STACK("FILE:write_mat4")
      START_TIMER("FILE:write_mat4")
      if (self%buffered) then
        call write_buffered_mat4_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_mat4 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_mat4")
      CHECK
   end subroutine

   subroutine write_cpxmat(self,mat)
    FILE :: self
   ! Write a complex matrix to the file as a single record
      CPXMAT(:,:) :: mat
      STACK("FILE:write_cpxmat")
      START_TIMER("FILE:write_cpxmat")
      if (self%buffered) then
         call write_buffered_cpxmat_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_cpxmat ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_cpxmat")
      CHECK
   end subroutine

   subroutine write_cpxmat3(self,mat)
    FILE :: self
   ! Write a complex 3d matrix to the file as a single record
      CPXMAT3(:,:,:) :: mat
      STACK("FILE:write_cpxmat3")
      START_TIMER("FILE:write_cpxmat3")
      if (self%buffered) then
         call write_buffered_cpxmat3_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_cpxmat3 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_cpxmat3")
      CHECK
   end subroutine

   subroutine write_cpxmat4(self,mat)
    FILE :: self
   ! Write a complex 4d matrix to the file as a single record
      CPXMAT4(:,:,:,:) :: mat
      STACK("FILE:write_cpxmat4")
      START_TIMER("FILE:write_cpxmat4")
      if (self%buffered) then
         call write_buffered_cpxmat4_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_cpxmat4 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_cpxmat4")
      CHECK
   end subroutine

   subroutine write_cpxmat5(self,mat)
    FILE :: self
   ! Write a complex 5d matrix to the file as a single record
      CPXMAT5(:,:,:,:,:) :: mat
      STACK("FILE:write_cpxmat5")
      START_TIMER("FILE:write_cpxmat5")
      if (self%buffered) then
         call write_buffered_cpxmat5_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        ENSURE(self%io_status==0,"FILE:write_cpxmat5 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if
     STOP_TIMER("FILE:write_cpxmat5")
      CHECK
   end subroutine

!  **********************************
!  File repositioning type operations
!  **********************************

  subroutine rewind(self)
    FILE :: self
  ! Rewind the file
    STACK("FILE:rewind")
    START_TIMER("FILE:rewind")
    if (do_io_(tonto_parallel)) then
      rewind(unit=self%unit,iostat=self%io_status)
    end if
    call broadcast_(tonto_parallel,self%io_status,0)
    ENSURE(self%io_status==0,"FILE:rewind ... rewind error, error="// trim(to_str_(self%io_status)))
    self%record = 1
    STOP_TIMER("FILE:rewind")
     CHECK
  end subroutine

  subroutine backspace(self)
    FILE :: self
  ! Backspace the file one record of the file. Backspacing before record 1 has
  ! no effect.
    STACK("FILE:backspace")
    START_TIMER("FILE:backspace")
    if (do_io_(tonto_parallel)) then
      backspace(unit=self%unit,iostat=self%io_status)
    end if
    call broadcast_(tonto_parallel,self%io_status,0)
    ENSURE(self%io_status==0,"FILE:backspace ... backspace error, error="// trim(to_str_(self%io_status)))
    self%record = max(1,self%record-1)
    STOP_TIMER("FILE:backspace")
     CHECK
  end subroutine

   subroutine skip(self)
    FILE :: self
   ! Skip over the next record of the file. An error is generated if at the end
   ! of the file
     STACK("FILE:skip")
     START_TIMER("FILE:skip")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     ENSURE(self%io_status==0,"FILE:skip ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     STOP_TIMER("FILE:skip")
      CHECK
   end subroutine

   subroutine move_to_end(self)
    FILE :: self
   ! Move to the end of the file, say for adding more data
      STACK("FILE:move_to_end")
      START_TIMER("FILE:move_to_end")
      do
        if (do_io_(tonto_parallel)) then
          read(unit=self%unit,iostat=self%io_status)
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        if (self%io_status/=0) exit
        self%record = self%record + 1
      end do
     STOP_TIMER("FILE:move_to_end")
      CHECK
   end subroutine

   subroutine move_to_position(self,pos)
    FILE :: self
   ! Move to a particular record position in the file
      INT :: pos
      STACK("FILE:move_to_position")
      START_TIMER("FILE:move_to_position")
      call move_to_record_(self,pos)
     STOP_TIMER("FILE:move_to_position")
      CHECK
   end subroutine

   subroutine move_to_record(self,rec)
    FILE :: self
   ! Move to a particular record position in the file
      INT :: rec
      STACK("FILE:move_to_record")
      START_TIMER("FILE:move_to_record")
      ENSURE(rec>=1,"FILE:move_to_record ... can't move to non-positive record")
      if (rec<position_(self)) then
         do
            call backspace_(self)
            if (position_(self)==rec) exit
         end do
      else if (rec>position_(self)) then
         do
            call skip_(self)
            if (position_(self)==rec) exit
         end do
      end if
     STOP_TIMER("FILE:move_to_record")
      CHECK
   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   function exists(self,name) result(res)
    FILE :: self
   ! Returns true if the file exists on the file system.  If present, "name" is
   ! used, otherwise .name is used.
      BIN :: res
      STR(*), IN, optional :: name
      STACK("FILE:exists")
      START_TIMER("FILE:exists")
      if (present(name)) then
        if (do_io_(tonto_parallel)) then
          inquire(file=name,exist=res)
        end if
      else
        if (do_io_(tonto_parallel)) then
          inquire(file=self%name,exist=res)
        end if
      end if
      call broadcast_(tonto_parallel,res,0)
     STOP_TIMER("FILE:exists")
      CHECK
   end function

   function is_open(self) result(res)
    FILE :: self
   ! Returns true if the file has been opened
      BIN :: res
      ! inquire(unit=.unit,opened=res)
      STACK("FILE:is_open")
      START_TIMER("FILE:is_open")
      if (do_io_(tonto_parallel)) then
        inquire(file=self%name,opened=res)
      end if
      call broadcast_(tonto_parallel,res,0)
     STOP_TIMER("FILE:is_open")
      CHECK
   end function

   function unit_used(self) result(res)
    FILE :: self
   ! Returns true if the file unit is in use
      BIN :: res
      STACK("FILE:unit_used")
      START_TIMER("FILE:unit_used")
      if (do_io_(tonto_parallel)) then
        inquire(unit=self%unit,opened=res)
      end if
      call broadcast_(tonto_parallel,res,0)
     STOP_TIMER("FILE:unit_used")
      CHECK
   end function

!   created result(res)
!   ! Returns true if the file object has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if the file object has *not* been created
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   function position(self) result(res)
    FILE :: self
   ! Return record position of the file
      INT :: res
      STACK("FILE:position")
      START_TIMER("FILE:position")
      res = self%record
     STOP_TIMER("FILE:position")
      CHECK
   end function

   function is_for_reading(self) result(res)
    FILE :: self
   ! Return whether the file is opened for reading.
     BIN :: res
     STR(5) :: reading
     STACK("FILE:is_for_reading")
     START_TIMER("FILE:is_for_reading")
     if (do_io_(tonto_parallel)) then
       inquire(unit=self%unit,read=reading)
     end if
     call broadcast_(tonto_parallel,reading,0)
     if (trim(reading) == "YES") then
       res = TRUE
     else
       res = FALSE
     end if
     STOP_TIMER("FILE:is_for_reading")
      CHECK
   end function

!   is_real_buffered result(res)
!   ! Returns true if the file is using real buffering
!      res :: BIN
!      res = associated(.real_buffer)
!   end

!   is_int_buffered result(res)
!   ! Returns true if the file is using INT buffering
!      res :: BIN
!      res = associated(.int_buffer)
!   end

!***************************************************
! Buffered routines
!***************************************************

   subroutine flush_real_buffer(self)
    FILE :: self
   ! Writes the real_buffer to disk, and positions the pointer at the start of
   ! the buffer.
     STACK("FILE:flush_real_buffer")
     START_TIMER("FILE:flush_real_buffer")
     if (do_io_(tonto_parallel)) then
       write(unit=self%unit,iostat=self%io_status) self%real_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     ENSURE(self%io_status==0,"FILE:flush_real_buffer ... write error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%real_buffer_pos = 1
     STOP_TIMER("FILE:flush_real_buffer")
      CHECK
   end subroutine

   subroutine flush_int_buffer(self)
    FILE :: self
   ! Writes the int_buffer to disk, and positions the pointer at the start of
   ! the buffer.
     STACK("FILE:flush_int_buffer")
     START_TIMER("FILE:flush_int_buffer")
     if (do_io_(tonto_parallel)) then
       write(unit=self%unit,iostat=self%io_status) self%int_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     ENSURE(self%io_status==0,"FILE:flush_int_buffer ... write error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%int_buffer_pos = 1
     STOP_TIMER("FILE:flush_int_buffer")
      CHECK
   end subroutine

   subroutine flush_cpx_buffer(self)
    FILE :: self
   ! Writes the cpx_buffer to disk, and positions the pointer at the start of
   ! the buffer.
     STACK("FILE:flush_cpx_buffer")
     START_TIMER("FILE:flush_cpx_buffer")
     if (do_io_(tonto_parallel)) then
       write(unit=self%unit,iostat=self%io_status) self%cpx_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     ENSURE(self%io_status==0,"FILE:flush_cpx_buffer ... write error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%cpx_buffer_pos = 1
     STOP_TIMER("FILE:flush_cpx_buffer")
      CHECK
   end subroutine

   subroutine get_real_buffer(self)
    FILE :: self
   ! Reads the real_buffer from disk, and positions the pointer at the start of
   ! the buffer.
     STACK("FILE:get_real_buffer")
     START_TIMER("FILE:get_real_buffer")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) self%real_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,self%real_buffer,0)
     ENSURE(self%io_status==0,"FILE:get_real_buffer ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%real_buffer_pos = 1
     STOP_TIMER("FILE:get_real_buffer")
      CHECK
   end subroutine

   subroutine get_int_buffer(self)
    FILE :: self
   ! Reads the int_buffer from disk, and positions the pointer at the start of
   ! the buffer.
     STACK("FILE:get_int_buffer")
     START_TIMER("FILE:get_int_buffer")
     ENSURE( associated(self%int_buffer),"FILE:get_int_buffer ... buffer not created")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) self%int_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,self%int_buffer,0)
     ENSURE(self%io_status==0,"FILE:get_int_buffer ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%int_buffer_pos = 1
     STOP_TIMER("FILE:get_int_buffer")
      CHECK
   end subroutine

   subroutine get_cpx_buffer(self)
    FILE :: self
   ! Reads the cpx_buffer from disk, and positions the pointer at the start of
   ! the buffer.
     STACK("FILE:get_cpx_buffer")
     START_TIMER("FILE:get_cpx_buffer")
     ENSURE( associated(self%cpx_buffer),"FILE:get_cpx_buffer ... buffer not created")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) self%cpx_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,self%cpx_buffer,0)
     ENSURE(self%io_status==0,"FILE:get_cpx_buffer ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%cpx_buffer_pos = 1
     STOP_TIMER("FILE:get_cpx_buffer")
      CHECK
   end subroutine

   subroutine write_buffered_real(self,the_real)
    FILE :: self
   ! Writes "the_real" to the buffer, and writes buffer to disk when full.
     REAL, IN :: the_real
     STACK("FILE:write_buffered_real")
     START_TIMER("FILE:write_buffered_real")
     ENSURE( associated(self%real_buffer),"FILE:write_buffered_real ... buffer not created")
     self%real_buffer( self%real_buffer_pos ) = the_real
     self%real_buffer_pos = self%real_buffer_pos + 1
     if ( self%real_buffer_pos > FILE_BUFFER_LENGTH) call flush_real_buffer_(self)
     STOP_TIMER("FILE:write_buffered_real")
      CHECK
   end subroutine

   subroutine read_buffered_real(self,the_real)
    FILE :: self
   ! Reads "the_real" from the buffer, and reads buffer from disk when empty.
     REAL, OUT :: the_real
     STACK("FILE:read_buffered_real")
     START_TIMER("FILE:read_buffered_real")
     if ( self%real_buffer_pos > FILE_BUFFER_LENGTH) call get_real_buffer_(self)
     the_real = self%real_buffer( self%real_buffer_pos )
     self%real_buffer_pos = self%real_buffer_pos + 1
     STOP_TIMER("FILE:read_buffered_real")
      CHECK
   end subroutine

   subroutine write_buffered_int(self,the_int)
    FILE :: self
   ! Writes "the_int" to the buffer, and writes buffer to disk when full.
     INT, IN :: the_int
     STACK("FILE:write_buffered_int")
     START_TIMER("FILE:write_buffered_int")
     self%int_buffer( self%int_buffer_pos ) = the_int
     self%int_buffer_pos = self%int_buffer_pos + 1
     if ( self%int_buffer_pos > FILE_BUFFER_LENGTH) call flush_int_buffer_(self)
     STOP_TIMER("FILE:write_buffered_int")
      CHECK
   end subroutine

   subroutine read_buffered_int(self,the_int)
    FILE :: self
   ! Reads the integer from the buffer, and reads buffer from disk when empty.
     INT, OUT :: the_int
     STACK("FILE:read_buffered_int")
     START_TIMER("FILE:read_buffered_int")
     if ( self%int_buffer_pos > FILE_BUFFER_LENGTH) call get_int_buffer_(self)
     the_int = self%int_buffer( self%int_buffer_pos )
     self%int_buffer_pos = self%int_buffer_pos + 1
     STOP_TIMER("FILE:read_buffered_int")
      CHECK
   end subroutine

   subroutine write_buffered_cpx(self,the_cpx)
    FILE :: self
   ! Writes "the_cpx" to the buffer, and writes buffer to disk when full.
     CPX, IN :: the_cpx
     STACK("FILE:write_buffered_cpx")
     START_TIMER("FILE:write_buffered_cpx")
     ENSURE( associated(self%cpx_buffer),"FILE:write_buffered_cpx ... buffer not created")
     self%cpx_buffer( self%cpx_buffer_pos ) = the_cpx
     self%cpx_buffer_pos = self%cpx_buffer_pos + 1
     if ( self%cpx_buffer_pos > FILE_BUFFER_LENGTH) call flush_cpx_buffer_(self)
     STOP_TIMER("FILE:write_buffered_cpx")
      CHECK
   end subroutine

   subroutine read_buffered_cpx(self,the_cpx)
    FILE :: self
   ! Reads "the_cpx" from the buffer, and reads buffer from disk when empty.
     CPX, OUT :: the_cpx
     STACK("FILE:read_buffered_cpx")
     START_TIMER("FILE:read_buffered_cpx")
     if ( self%cpx_buffer_pos > FILE_BUFFER_LENGTH) call get_cpx_buffer_(self)
     the_cpx = self%cpx_buffer( self%cpx_buffer_pos )
     self%cpx_buffer_pos = self%cpx_buffer_pos + 1
     STOP_TIMER("FILE:read_buffered_cpx")
      CHECK
   end subroutine

   subroutine write_buffered_vec(self,the_vec)
    FILE :: self
   ! Writes the vector to the buffer, and writes buffer to disk when full.
     REALVEC(:), IN :: the_vec
     INT :: vec_length,vec_pos,buffer_top,vec_top,put_length
     STACK("FILE:write_buffered_vec")
     START_TIMER("FILE:write_buffered_vec")
     vec_length = size(the_vec)
     vec_pos=1
     do
       put_length=min(FILE_BUFFER_LENGTH - self%real_buffer_pos + 1,vec_length-vec_pos+1)
       buffer_top = self%real_buffer_pos + put_length - 1
       vec_top = vec_pos + put_length - 1
       self%real_buffer( self%real_buffer_pos:buffer_top) = the_vec(vec_pos:vec_top)
       self%real_buffer_pos = buffer_top + 1
       vec_pos = vec_top + 1
       if ( self%real_buffer_pos > FILE_BUFFER_LENGTH) call flush_real_buffer_(self)
       if (vec_pos > vec_length) exit
     end do
     STOP_TIMER("FILE:write_buffered_vec")
      CHECK
   end subroutine

   subroutine read_buffered_vec(self,the_vec)
    FILE :: self
   ! Reads the vector from the buffer, and reads buffer from disk when empty.
     REALVEC(:), OUT :: the_vec
     INT :: vec_length,vec_pos,buffer_top,vec_top,get_length
     STACK("FILE:read_buffered_vec")
     START_TIMER("FILE:read_buffered_vec")
     vec_length = size(the_vec)
     vec_pos=1
     do
       if ( self%real_buffer_pos > FILE_BUFFER_LENGTH) call get_real_buffer_(self)
       get_length=min(FILE_BUFFER_LENGTH - self%real_buffer_pos + 1,vec_length-vec_pos+1)
       buffer_top = self%real_buffer_pos + get_length - 1
       vec_top = vec_pos + get_length - 1
       the_vec(vec_pos:vec_top) = self%real_buffer( self%real_buffer_pos:buffer_top)
       self%real_buffer_pos = buffer_top + 1
       vec_pos = vec_top + 1
       if (vec_pos > vec_length) exit
     end do
     STOP_TIMER("FILE:read_buffered_vec")
      CHECK
   end subroutine

   subroutine write_buffered_intvec(self,the_intvec)
    FILE :: self
   ! Writes the intvector to the buffer, and writes buffer to disk when full.
     INTVEC(:), IN :: the_intvec
     INT :: intvec_length,intvec_pos,buffer_top,intvec_top,put_length
     STACK("FILE:write_buffered_intvec")
     START_TIMER("FILE:write_buffered_intvec")
     intvec_length = size(the_intvec)
     intvec_pos=1
     do
       put_length=min(FILE_BUFFER_LENGTH - self%int_buffer_pos + 1,intvec_length-intvec_pos+1)
       buffer_top = self%int_buffer_pos + put_length - 1
       intvec_top = intvec_pos + put_length - 1
       self%int_buffer( self%int_buffer_pos:buffer_top) = the_intvec(intvec_pos:intvec_top)
       self%int_buffer_pos = buffer_top + 1
       intvec_pos = intvec_top + 1
       if ( self%int_buffer_pos > FILE_BUFFER_LENGTH) call flush_int_buffer_(self)
       if (intvec_pos > intvec_length) exit
     end do
     STOP_TIMER("FILE:write_buffered_intvec")
      CHECK
   end subroutine

   subroutine read_buffered_intvec(self,the_intvec)
    FILE :: self
   ! Reads the intvector from the buffer, and reads buffer from disk when empty.
     INTVEC(:), OUT :: the_intvec
     INT :: intvec_length,intvec_pos,buffer_top,intvec_top,get_length
     STACK("FILE:read_buffered_intvec")
     START_TIMER("FILE:read_buffered_intvec")
     intvec_length = size(the_intvec)
     intvec_pos=1
     do
       if ( self%int_buffer_pos > FILE_BUFFER_LENGTH) call get_int_buffer_(self)
       get_length=min(FILE_BUFFER_LENGTH - self%int_buffer_pos + 1,intvec_length-intvec_pos+1)
       buffer_top = self%int_buffer_pos + get_length - 1
       intvec_top = intvec_pos + get_length - 1
       the_intvec(intvec_pos:intvec_top) = self%int_buffer( self%int_buffer_pos:buffer_top)
       self%int_buffer_pos = buffer_top + 1
       intvec_pos = intvec_top + 1
       if (intvec_pos > intvec_length) exit
     end do
     STOP_TIMER("FILE:read_buffered_intvec")
      CHECK
   end subroutine

   subroutine write_buffered_cpxvec(self,the_cpxvec)
    FILE :: self
   ! Writes the cvector to the buffer, and writes buffer to disk when full.
     CPXVEC(:), IN :: the_cpxvec
     INT :: cpxvec_length,cpxvec_pos,buffer_top,cpxvec_top,put_length
     STACK("FILE:write_buffered_cpxvec")
     START_TIMER("FILE:write_buffered_cpxvec")
     cpxvec_length = size(the_cpxvec)
     cpxvec_pos=1
     do
       put_length=min(FILE_BUFFER_LENGTH - self%cpx_buffer_pos + 1,cpxvec_length-cpxvec_pos+1)
       buffer_top = self%cpx_buffer_pos + put_length - 1
       cpxvec_top = cpxvec_pos + put_length - 1
       self%cpx_buffer( self%cpx_buffer_pos:buffer_top) = the_cpxvec(cpxvec_pos:cpxvec_top)
       self%cpx_buffer_pos = buffer_top + 1
       cpxvec_pos = cpxvec_top + 1
       if ( self%cpx_buffer_pos > FILE_BUFFER_LENGTH) call flush_cpx_buffer_(self)
       if (cpxvec_pos > cpxvec_length) exit
     end do
     STOP_TIMER("FILE:write_buffered_cpxvec")
      CHECK
   end subroutine

   subroutine read_buffered_cpxvec(self,the_cpxvec)
    FILE :: self
   ! Reads the cvector from the buffer, and reads buffer from disk when empty.
     CPXVEC(:), OUT :: the_cpxvec
     INT :: cpxvec_length,cpxvec_pos,buffer_top,cpxvec_top,get_length
     STACK("FILE:read_buffered_cpxvec")
     START_TIMER("FILE:read_buffered_cpxvec")
     cpxvec_length = size(the_cpxvec)
     cpxvec_pos=1
     do
       if ( self%cpx_buffer_pos > FILE_BUFFER_LENGTH) call get_cpx_buffer_(self)
       get_length=min(FILE_BUFFER_LENGTH - self%cpx_buffer_pos + 1,cpxvec_length-cpxvec_pos+1)
       buffer_top = self%cpx_buffer_pos + get_length - 1
       cpxvec_top = cpxvec_pos + get_length - 1
       the_cpxvec(cpxvec_pos:cpxvec_top) = self%cpx_buffer( self%cpx_buffer_pos:buffer_top)
       self%cpx_buffer_pos = buffer_top + 1
       cpxvec_pos = cpxvec_top + 1
       if (cpxvec_pos > cpxvec_length) exit
     end do
     STOP_TIMER("FILE:read_buffered_cpxvec")
      CHECK
   end subroutine

   subroutine write_buffered_mat(self,mat)
    FILE :: self
   ! Writes the mat to the buffer, and writes buffer to disk when full.
     REALMAT(:,:), IN :: mat
     INT :: b,bsize
     STACK("FILE:write_buffered_mat")
     START_TIMER("FILE:write_buffered_mat")
     bsize=size(mat,2)
     do b=1,bsize
       call write_buffered_vec_(self,mat(:,b))
     end do
     STOP_TIMER("FILE:write_buffered_mat")
      CHECK
   end subroutine

   subroutine read_buffered_mat(self,mat)
    FILE :: self
   ! Reads the mat from the buffer.
     REALMAT(:,:), OUT :: mat
     INT :: b,bsize
     STACK("FILE:read_buffered_mat")
     START_TIMER("FILE:read_buffered_mat")
     bsize=size(mat,2)
     do b=1,bsize
       call read_buffered_vec_(self,mat(:,b))
     end do
     STOP_TIMER("FILE:read_buffered_mat")
      CHECK
   end subroutine

   subroutine write_buffered_intmat(self,mat)
    FILE :: self
   ! Writes the intmat to the buffer, and writes buffer to disk when full.
     INTMAT(:,:), IN :: mat
     INT :: a,b,asize,bsize
     STACK("FILE:write_buffered_intmat")
     START_TIMER("FILE:write_buffered_intmat")
     asize=size(mat,1);    bsize=size(mat,2)
     do a=1,asize
       do b=1,bsize
         call write_buffered_int_(self,mat(a,b))
       end do
     end do
     STOP_TIMER("FILE:write_buffered_intmat")
      CHECK
   end subroutine

   subroutine read_buffered_intmat(self,mat)
    FILE :: self
   ! Reads the intmat from the buffer.
     INTMAT(:,:), OUT :: mat
     INT :: a,b,asize,bsize
     STACK("FILE:read_buffered_intmat")
     START_TIMER("FILE:read_buffered_intmat")
     asize=size(mat,1);    bsize=size(mat,2)
     do a=1,asize
       do b=1,bsize
         call read_buffered_int_(self,mat(a,b))
       end do
     end do
     STOP_TIMER("FILE:read_buffered_intmat")
      CHECK
   end subroutine

   subroutine write_buffered_cpxmat(self,mat)
    FILE :: self
   ! Writes the cpxmat to the buffer, and writes buffer to disk when full.
     CPXMAT(:,:), IN :: mat
     INT :: b,bsize
     STACK("FILE:write_buffered_cpxmat")
     START_TIMER("FILE:write_buffered_cpxmat")
     bsize=size(mat,2)
     do b=1,bsize
       call write_buffered_cpxvec_(self,mat(:,b))
     end do
     STOP_TIMER("FILE:write_buffered_cpxmat")
      CHECK
   end subroutine

   subroutine read_buffered_cpxmat(self,mat)
    FILE :: self
   ! Reads the cpxmat from the buffer.
     CPXMAT(:,:), OUT :: mat
     INT :: b,bsize
     STACK("FILE:read_buffered_cpxmat")
     START_TIMER("FILE:read_buffered_cpxmat")
     bsize=size(mat,2)
     do b=1,bsize
       call read_buffered_cpxvec_(self,mat(:,b))
     end do
     STOP_TIMER("FILE:read_buffered_cpxmat")
      CHECK
   end subroutine

   subroutine write_buffered_mat3(self,mat)
    FILE :: self
   ! Writes the mat3 to the buffer, and writes buffer to disk when full.
     REALMAT3(:,:,:), IN :: mat
     INT :: b,c,bsize,csize
     STACK("FILE:write_buffered_mat3")
     START_TIMER("FILE:write_buffered_mat3")
     bsize=size(mat,2);     csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call write_buffered_vec_(self,mat(:,b,c))
       end do
     end do
     STOP_TIMER("FILE:write_buffered_mat3")
      CHECK
   end subroutine

   subroutine read_buffered_mat3(self,mat)
    FILE :: self
   ! Reads the mat3 from the buffer.
     REALMAT3(:,:,:), OUT :: mat
     INT :: b,c,bsize,csize
     STACK("FILE:read_buffered_mat3")
     START_TIMER("FILE:read_buffered_mat3")
     bsize=size(mat,2);     csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call read_buffered_vec_(self,mat(:,b,c))
       end do
     end do
     STOP_TIMER("FILE:read_buffered_mat3")
      CHECK
   end subroutine

   subroutine write_buffered_cpxmat3(self,mat)
    FILE :: self
   ! Writes the cpxmat3 to the buffer, and writes buffer to disk when full.
     CPXMAT3(:,:,:), IN :: mat
     INT :: b,c,bsize,csize
     STACK("FILE:write_buffered_cpxmat3")
     START_TIMER("FILE:write_buffered_cpxmat3")
     bsize=size(mat,2);    csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call write_buffered_cpxvec_(self,mat(:,b,c))
       end do
     end do
     STOP_TIMER("FILE:write_buffered_cpxmat3")
      CHECK
   end subroutine

   subroutine read_buffered_cpxmat3(self,mat)
    FILE :: self
   ! Reads the cpxmat3 from the buffer.
     CPXMAT3(:,:,:), OUT :: mat
     INT :: b,c,bsize,csize
     STACK("FILE:read_buffered_cpxmat3")
     START_TIMER("FILE:read_buffered_cpxmat3")
     bsize=size(mat,2);     csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call read_buffered_cpxvec_(self,mat(:,b,c))
       end do
     end do
     STOP_TIMER("FILE:read_buffered_cpxmat3")
      CHECK
   end subroutine

   subroutine write_buffered_mat4(self,mat)
    FILE :: self
   ! Writes the mat4 to the buffer, and writes buffer to disk when full.
     REALMAT4(:,:,:,:), IN :: mat
     INT :: b,c,d,bsize,csize,dsize
     STACK("FILE:write_buffered_mat4")
     START_TIMER("FILE:write_buffered_mat4")
     bsize=size(mat,2);     csize=size(mat,3);    dsize=size(mat,4)
     do d=1,dsize
       do c=1,csize
         do b=1,bsize
           call write_buffered_vec_(self,mat(:,b,c,d))
         end do
       end do
     end do
     STOP_TIMER("FILE:write_buffered_mat4")
      CHECK
   end subroutine

   subroutine read_buffered_mat4(self,mat)
    FILE :: self
   ! Reads the mat4 from the buffer.
     REALMAT4(:,:,:,:), OUT :: mat
     INT :: b,c,d,bsize,csize,dsize
     STACK("FILE:read_buffered_mat4")
     START_TIMER("FILE:read_buffered_mat4")
     bsize=size(mat,2);     csize=size(mat,3);    dsize=size(mat,4)
     do d=1,dsize
       do c=1,csize
         do b=1,bsize
           call read_buffered_vec_(self,mat(:,b,c,d))
         end do
       end do
     end do
     STOP_TIMER("FILE:read_buffered_mat4")
      CHECK
   end subroutine

   subroutine write_buffered_cpxmat4(self,mat)
    FILE :: self
   ! Writes the mat4 to the buffer, and writes buffer to disk when full.
     CPXMAT4(:,:,:,:), IN :: mat
     INT :: a,b,c,d,asize,bsize,csize,dsize
     STACK("FILE:write_buffered_cpxmat4")
     START_TIMER("FILE:write_buffered_cpxmat4")
     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             call write_buffered_cpx_(self,mat(a,b,c,d))
           end do
         end do
       end do
     end do
     STOP_TIMER("FILE:write_buffered_cpxmat4")
      CHECK
   end subroutine

   subroutine read_buffered_cpxmat4(self,mat)
    FILE :: self
   ! Reads the mat4 from the buffer.
     CPXMAT4(:,:,:,:), OUT :: mat
     INT :: a,b,c,d,asize,bsize,csize,dsize
     STACK("FILE:read_buffered_cpxmat4")
     START_TIMER("FILE:read_buffered_cpxmat4")
     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             call read_buffered_cpx_(self,mat(a,b,c,d))
           end do
         end do
       end do
     end do
     STOP_TIMER("FILE:read_buffered_cpxmat4")
      CHECK
   end subroutine

   subroutine write_buffered_cpxmat5(self,mat)
    FILE :: self
   ! Writes the mat5 to the buffer, and writes buffer to disk when full.
     CPXMAT5(:,:,:,:,:), IN :: mat
     INT :: a,b,c,d,e,asize,bsize,csize,dsize,esize
     STACK("FILE:write_buffered_cpxmat5")
     START_TIMER("FILE:write_buffered_cpxmat5")
     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     esize=size(mat,5)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             do e=1,esize
               call write_buffered_cpx_(self,mat(a,b,c,d,e))
             end do
           end do
         end do
       end do
     end do
     STOP_TIMER("FILE:write_buffered_cpxmat5")
      CHECK
   end subroutine

   subroutine read_buffered_cpxmat5(self,mat)
    FILE :: self
   ! Reads the mat5 from the buffer.
     CPXMAT5(:,:,:,:,:), OUT :: mat
     INT :: a,b,c,d,e,asize,bsize,csize,dsize,esize
     STACK("FILE:read_buffered_cpxmat5")
     START_TIMER("FILE:read_buffered_cpxmat5")
     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     esize=size(mat,5)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             do e=1,esize
               call read_buffered_cpx_(self,mat(a,b,c,d,e))
             end do
           end do
         end do
       end do
     end do
     STOP_TIMER("FILE:read_buffered_cpxmat5")
      CHECK
   end subroutine

end
