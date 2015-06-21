!-------------------------------------------------------------------------------
!
! ARCHIVE: For archiving tonto objects to disk
!
! A polymorphic file object which can store ascii or binary format.
! Archives with the string "ascii" somewhere in their name are assumed to be
! ascii files, otherwise they are binary files.
!
! An archive can't be used for input and output at the same time.
!
! Read and write routines are used to deal with entire single objects.
! The archive is closed after the requested IO operation.
!
! If the above behaviour is not what is needed, the archive can be opened
! for multiple object I/O in one archive (say, a list of integral blocks)
! and you can deal by hand with appropriate component of the archive.
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
! $Id: archive.foo,v 1.22.2.9 2003/10/15 09:19:36 reaper Exp $
!-------------------------------------------------------------------------------

module ARCHIVE_MODULE

#  include "archive.use"

   implicit none

#  include "macros"
#  include "archive.int"


   public read_; interface read_
      module procedure read_int
      module procedure read_real
      module procedure read_vec
      module procedure read_cvec
      module procedure read_mat
      module procedure read_mat3
      module procedure read_mat4
      module procedure read_cmat
      module procedure read_cmat3
      module procedure read_cmat4
      module procedure read_cmat5
      module procedure read_vec_vec
      module procedure read_mat_vec
      module procedure read_opvector
      module procedure read_opmatrix
   end interface

   public write_; interface write_
      module procedure write_int
      module procedure write_real
      module procedure write_vec
      module procedure write_cvec
      module procedure write_mat
      module procedure write_mat3
      module procedure write_mat4
      module procedure write_cmat
      module procedure write_cmat3
      module procedure write_cmat4
      module procedure write_cmat5
      module procedure write_vec_mat
      module procedure write_mat_vec
      module procedure write_opvector
      module procedure write_opmatrix
   end interface

contains

   subroutine create(self,root_name,name,genre,format)
    ARCHIVE :: self
   ! Create an archive object with main name "root_name" and sub name "name".
   ! "genre" is used to identify components of OPMATRIX and OPVECTOR objects.
   ! "format" is used to identify file format (e.g. ascii). The default is
   ! binary.
      PTR :: self
      STR(*), optional :: root_name,name
      STR(*), optional :: genre,format
      STACK("ARCHIVE:create")
      START_TIMER("ARCHIVE:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(ARCHIVE_SIZE)
      call nullify_ptr_part_(self)
      call set_(self,root_name,name,genre,format)
     STOP_TIMER("ARCHIVE:create")
      UNSTACK
   end subroutine

   subroutine copy(self,archive)
    ARCHIVE :: self
   ! Make a copy
     ARCHIVE :: archive
     STACK("ARCHIVE:copy")
     START_TIMER("ARCHIVE:copy")
     self = archive
     call nullify_ptr_part_(self)
     if (associated(archive%file)) call create_copy_(self%file,archive%file)
     STOP_TIMER("ARCHIVE:copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    ARCHIVE :: self
   ! Destroy an opmatrix object
      PTR :: self
      STACK("ARCHIVE:destroy")
      START_TIMER("ARCHIVE:destroy")
      if ( NOT associated(self)) then; STOP_TIMER("ARCHIVE:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      deallocate(self)
      DELETE_MEMORY(ARCHIVE_SIZE)
     STOP_TIMER("ARCHIVE:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    ARCHIVE :: self
   ! Nullify the pointer parts of the opmatrix object
      STACK("ARCHIVE:nullify_ptr_part")
      START_TIMER("ARCHIVE:nullify_ptr_part")
      nullify(self%file)
      nullify(self%textfile)
     STOP_TIMER("ARCHIVE:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    ARCHIVE :: self
   ! Destroy the pointer parts of the opmatrix object
      STACK("ARCHIVE:destroy_ptr_part")
      START_TIMER("ARCHIVE:destroy_ptr_part")
      call destroy_(self%file)
      call destroy_(self%textfile)
     STOP_TIMER("ARCHIVE:destroy_ptr_part")
      UNSTACK
   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has not been created
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   subroutine set_defaults(self)
    ARCHIVE :: self
   ! Set up a default archive object
      STACK("ARCHIVE:set_defaults")
      START_TIMER("ARCHIVE:set_defaults")
      self%root_name = "unknown"
      self%name      = "unknown"
      self%genre      = " " ! generic genre
      self%format    = " " ! assume binary format
     STOP_TIMER("ARCHIVE:set_defaults")
      CHECK
   end subroutine

   subroutine set(self,root_name,name,genre,format)
    ARCHIVE :: self
   ! Reset an archive to have main name "root_name", sub name "name"
   ! "genre" (if present) is used to identify components of OPMATRIX and OPVECTOR
   ! objects.
   ! "format" (if present) is used to identify file format (e.g. ascii). The
   ! default is binary.
   ! Otherwise use defaults.
      STR(*), optional :: root_name,name
      STR(*), optional :: genre,format
      STACK("ARCHIVE:set")
      START_TIMER("ARCHIVE:set")
      call set_defaults_(self)
      if (present(root_name)) self%root_name = root_name
      if (present(name))      self%name      = name
      if (present(genre))      self%genre      = genre
      if (present(format))    self%format    = format
     STOP_TIMER("ARCHIVE:set")
      CHECK
   end subroutine

   subroutine set_root_name(self,root_name)
    ARCHIVE :: self
   ! Set an archive to have sub name "name".
      STR(*) :: root_name
      STACK("ARCHIVE:set_root_name")
      START_TIMER("ARCHIVE:set_root_name")
      call destroy_ptr_part_(self)
      self%root_name = root_name
     STOP_TIMER("ARCHIVE:set_root_name")
      CHECK
   end subroutine

   subroutine set_root(self,root_name)
    ARCHIVE :: self
   ! Set an archive to have sub name "name".
      STR(*) :: root_name
      STACK("ARCHIVE:set_root")
      START_TIMER("ARCHIVE:set_root")
      call destroy_ptr_part_(self)
      self%root_name = root_name
     STOP_TIMER("ARCHIVE:set_root")
      CHECK
   end subroutine

   subroutine set_name(self,name)
    ARCHIVE :: self
   ! Set an archive to have sub name "name".
      STR(*) :: name
      STACK("ARCHIVE:set_name")
      START_TIMER("ARCHIVE:set_name")
      call destroy_ptr_part_(self)
      self%name = name
     STOP_TIMER("ARCHIVE:set_name")
      CHECK
   end subroutine

   subroutine set_genre(self,genre)
    ARCHIVE :: self
   ! Set an archive to have genre "genre".
      STR(*) :: genre
      STACK("ARCHIVE:set_genre")
      START_TIMER("ARCHIVE:set_genre")
      call destroy_ptr_part_(self)
      self%genre = genre
     STOP_TIMER("ARCHIVE:set_genre")
      CHECK
   end subroutine

   subroutine set_format(self,format)
    ARCHIVE :: self
   ! Set an archive to have format "format".
      STR(*) :: format
      STACK("ARCHIVE:set_format")
      START_TIMER("ARCHIVE:set_format")
      call destroy_ptr_part_(self)
      self%format = format
     STOP_TIMER("ARCHIVE:set_format")
      CHECK
   end subroutine

   function file_name(self,genre) result(res)
    ARCHIVE :: self
   ! Return the archive file name from string ".root_name" by prepending the
   ! archive header ".name". If present "genre" is also appended. If present,
   ! ".format" is also appended.
      STR(*), optional :: genre
      STR(STR_SIZE) :: res
      STR(STR_SIZE) :: k
      STACK("ARCHIVE:file_name")
      START_TIMER("ARCHIVE:file_name")
      res = trim(self%root_name) // "." // self%name
      k = self%genre
      if (present(genre)) k = genre
      if (k/=" ")       res = trim(res) // "," // trim(k)
      if (self%format/=" ") res = trim(res) // "," // self%format
     STOP_TIMER("ARCHIVE:file_name")
      CHECK
   end function

   function is_a_text_file(self) result(res)
    ARCHIVE :: self
   ! Return true if the file is a text file
      BIN :: res
      STACK("ARCHIVE:is_a_text_file")
      START_TIMER("ARCHIVE:is_a_text_file")
      res = includes_(self%format,"ascii")
     STOP_TIMER("ARCHIVE:is_a_text_file")
      CHECK
   end function

   function exists(self,genre) result(res)
    ARCHIVE :: self
   ! Return TRUE if the archive exists in some form on disk.
      STR(*), optional :: genre
      BIN :: res
      STACK("ARCHIVE:exists")
      START_TIMER("ARCHIVE:exists")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         res = exists_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         res = exists_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:exists")
      CHECK
   end function

   subroutine open(self,for,buffered,type,genre)
    ARCHIVE :: self
   ! Open archive file
      STR(*) :: for
      STR(*), optional :: type
      BIN, optional :: buffered
      STR(*), optional :: genre
      STACK("ARCHIVE:open")
      START_TIMER("ARCHIVE:open")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call open_(self%textfile,for)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for,buffered,type)
      end if
     STOP_TIMER("ARCHIVE:open")
      UNSTACK
   end subroutine

   subroutine close(self)
    ARCHIVE :: self
   ! Close *and* destroy the archive file part.
      STACK("ARCHIVE:close")
      START_TIMER("ARCHIVE:close")
      if (associated(self%textfile)) then
          call close_(self%textfile)
          call destroy_(self%textfile)
      end if
      if (associated(self%file)) then
          call close_(self%file)
          call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:close")
      UNSTACK
   end subroutine

   subroutine delete(self,genre)
    ARCHIVE :: self
   ! Delete archive file if it exists, *and* destroy the archive file part
      STR(*), optional :: genre
      STACK("ARCHIVE:delete")
      START_TIMER("ARCHIVE:delete")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         if (exists_(self%textfile)) call delete_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         if (exists_(self%file)) call delete_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:delete")
      CHECK
   end subroutine

   subroutine delete_all_genres(self)
    ARCHIVE :: self
   ! Delete all genres of archive file, if they exists.
      STACK("ARCHIVE:delete_all_genres")
      START_TIMER("ARCHIVE:delete_all_genres")
      call delete_(self)
      call delete_(self,"restricted")
      call delete_(self,"unrestricted")
      call delete_(self,"alpha")
      call delete_(self,"beta")
      call delete_(self,"general")
      call delete_(self,"restricted_complex")
      call delete_(self,"unrestricted_complex")
      call delete_(self,"alpha_complex")
      call delete_(self,"beta_complex")
      call delete_(self,"general_complex")
      call delete_(self,"complex_restricted")
      call delete_(self,"complex_unrestricted")
      call delete_(self,"complex_alpha")
      call delete_(self,"complex_beta")
      call delete_(self,"complex_general")
     STOP_TIMER("ARCHIVE:delete_all_genres")
      CHECK
   end subroutine

!  ************************************
!  Read routines: read an entire object
!  ************************************

   subroutine read_int(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, vector "item".
      INT :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_int")
      START_TIMER("ARCHIVE:read_int")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_int ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_int ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="int")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_int")
      CHECK
   end subroutine

   subroutine read_real(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, vector "item".
      REAL :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_real")
      START_TIMER("ARCHIVE:read_real")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_real ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_real ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_real")
      CHECK
   end subroutine

   subroutine read_vec(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, vector "item".
      REALVEC(:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_vec")
      START_TIMER("ARCHIVE:read_vec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_vec ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_vec ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_vec")
      CHECK
   end subroutine

   subroutine read_cvec(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, complex vector "item".
      CPXVEC(:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_cvec")
      START_TIMER("ARCHIVE:read_cvec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_cvec ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_cvec ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_cvec")
      CHECK
   end subroutine

   subroutine read_mat(self,item,genre,order)
    ARCHIVE :: self
   ! Read from the archive, matrix "item". "order" is the input order for text
   ! files
      REALMAT(:,:) :: item
      STR(*), optional :: genre,order
      STACK("ARCHIVE:read_mat")
      START_TIMER("ARCHIVE:read_mat")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_mat ... no text archive "// trim(self%textfile%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item,order)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_mat ... no binary archive "// trim(self%file%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_mat")
      CHECK
   end subroutine

   subroutine read_mat3(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, matrix "item". "order" is the input order for text
   ! files
      REALMAT3(:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_mat3")
      START_TIMER("ARCHIVE:read_mat3")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_mat3 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_mat3 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_mat3")
      CHECK
   end subroutine

   subroutine read_mat4(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, matrix "item". "order" is the input order for text
   ! files
      REALMAT4(:,:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_mat4")
      START_TIMER("ARCHIVE:read_mat4")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_mat4 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_mat4 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_mat4")
      CHECK
   end subroutine

   subroutine read_cmat(self,item,genre,order)
    ARCHIVE :: self
   ! Read from the archive, complex matrix "item". "order" is the input order
   ! for text files
      CPXMAT(:,:) :: item
      STR(*), optional :: genre,order
      STACK("ARCHIVE:read_cmat")
      START_TIMER("ARCHIVE:read_cmat")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_cmat ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item,order)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_cmat ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_cmat")
      CHECK
   end subroutine

   subroutine read_cmat3(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, matrix "item". "order" is the input order for text
   ! files
      CPXMAT3(:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_cmat3")
      START_TIMER("ARCHIVE:read_cmat3")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_cmat3 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_cmat3 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_cmat3")
      CHECK
   end subroutine

   subroutine read_cmat4(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, matrix "item". "order" is the input order for text
   ! files
      CPXMAT4(:,:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_cmat4")
      START_TIMER("ARCHIVE:read_cmat4")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_cmat4 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_cmat4 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_cmat4")
      CHECK
   end subroutine

   subroutine read_cmat5(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive, matrix "item". "order" is the input order for text
   ! files
      CPXMAT5(:,:,:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:read_cmat5")
      START_TIMER("ARCHIVE:read_cmat5")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_cmat5 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         ENSURE(exists_(self%file),"ARCHIVE:read_cmat5 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=TRUE,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_cmat5")
      CHECK
   end subroutine

   subroutine read_vec_vec(self,item1,item2)
    ARCHIVE :: self
   ! Read from the archive, vector "item1" and matrix "item2"
      REALVEC(:) :: item1
      REALMAT(:,:) :: item2
      STACK("ARCHIVE:read_vec_vec")
      START_TIMER("ARCHIVE:read_vec_vec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_vec_vec ... no text archive "// trim(file_name_(self)))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item1)
         call read_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         if (NOT exists_(self%file)) &
            DIE("ARCHIVE:read_vec_vec ... no binary archive "// trim(file_name_(self)))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item1)
         call read_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_vec_vec")
      CHECK
   end subroutine

   subroutine read_mat_vec(self,item1,item2)
    ARCHIVE :: self
   ! Read from the archive, matrix "item1" and vector "item2"
      REALMAT(:,:) :: item1
      REALVEC(:) :: item2
      STACK("ARCHIVE:read_mat_vec")
      START_TIMER("ARCHIVE:read_mat_vec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         ENSURE(exists_(self%textfile),"ARCHIVE:read_mat_vec ... no text archive "// trim(file_name_(self)))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item1)
         call read_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         if (NOT exists_(self%file)) &
            DIE("ARCHIVE:read_mat_vec ... no binary archive "// trim(file_name_(self)))
         call open_(self%file,for="read-only",buffered=TRUE,type="real")
         call read_(self%file,item1)
         call read_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:read_mat_vec")
      CHECK
   end subroutine

   subroutine read_opvector(self,item,genre)
    ARCHIVE :: self
   ! Read from the archive operator vector "item". If present, "genre" is the
   ! genre of "item" to be read in.
      OPVECTOR :: item
      STR(*), optional :: genre
      STR(STR_SIZE) :: itemgenre
      STACK("ARCHIVE:read_opvector")
      START_TIMER("ARCHIVE:read_opvector")
      if (present(genre) ) then;  itemgenre = genre
      else if (self%genre/=" ") then; itemgenre = self%genre
      else;                      itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted")
            call destroy_(item,"restricted")
            call create_(item,"restricted")
            call read_(self, item%restricted,genre="restricted")
         case ("unrestricted")
            call destroy_(item,"unrestricted")
            call create_(item,"unrestricted")
            call read_(self, item%alpha,genre="alpha")
            call read_(self, item%beta,genre="beta")
         case ("alpha")
            call destroy_(item,"alpha")
            call create_(item,"alpha")
            call read_(self, item%alpha,genre="alpha")
         case ("beta")
            call destroy_(item,"beta")
            call create_(item,"beta")
            call read_(self, item%beta,genre="beta")
         case ("general")
            call destroy_(item,"general")
            call create_(item,"general")
            call read_(self, item%general,genre="general")
         case default
            DIE("ARCHIVE:read_opvector ... unknown genre, "//trim(itemgenre))
      end select
!      if (.is_a_text_file) .write(item,itemgenre)
     STOP_TIMER("ARCHIVE:read_opvector")
      UNSTACK
   end subroutine

   subroutine read_opmatrix(self,item,genre,order)
    ARCHIVE :: self
   ! Read from the archive operator matrix "item". If present, "genre" is the
   ! genre of "item" to be read in. "order" indicated the input order for ascii
   ! files.
      OPMATRIX :: item
      STR(*), optional :: genre,order
      STR(STR_SIZE) :: itemgenre
      STACK("ARCHIVE:read_opmatrix")
      START_TIMER("ARCHIVE:read_opmatrix")
      if (present(genre))   then; itemgenre = genre
      else if (self%genre/=" ") then; itemgenre = self%genre
      else;                      itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted")
            call destroy_(item,"restricted")
            call create_(item,"restricted")
            call read_(self, item%restricted,"restricted",order)
         case ("unrestricted")
            call destroy_(item,"unrestricted")
            call create_(item,"unrestricted")
            call read_(self, item%alpha,"alpha",order)
            call read_(self, item%beta,"beta",order)
         case ("alpha")
            call destroy_(item,"alpha")
            call create_(item,"alpha")
            call read_(self, item%alpha,"alpha",order)
         case ("beta")
            call destroy_(item,"beta")
            call create_(item,"beta")
            call read_(self, item%beta,"beta",order)
         case ("general")
            call destroy_(item,"general")
            call create_(item,"general")
            call read_(self, item%general,"general",order)
         case ("restricted_complex","complex_restricted")
            call destroy_(item,"restricted_complex")
            call create_(item,"restricted_complex")
            call read_(self, item%restricted_complex,"restricted_complex",order)
         case ("unrestricted_complex","complex_unrestricted")
            call destroy_(item,"unrestricted_complex")
            call create_(item,"unrestricted_complex")
            call read_(self, item%alpha_complex,"alpha_complex",order)
            call read_(self, item%beta_complex,"beta_complex",order)
         case ("alpha_complex","complex_alpha")
            call destroy_(item,"alpha_complex")
            call create_(item,"alpha_complex")
            call read_(self, item%alpha_complex,"alpha_complex",order)
         case ("beta_complex","complex_beta")
            call destroy_(item,"beta_complex")
            call create_(item,"beta_complex")
            call read_(self, item%beta_complex,"beta_complex",order)
         case ("general_complex","complex_general")
            call destroy_(item,"general_complex")
            call create_(item,"general_complex")
            call read_(self, item%general_complex,"general_complex",order)
         case default
            DIE("ARCHIVE:read_opmatrix ... unknown genre, "//trim(itemgenre))
      end select
!      if (.is_a_text_file) .write(item,genre,order)
     STOP_TIMER("ARCHIVE:read_opmatrix")
      UNSTACK
   end subroutine

!  **************
!  Write routines
!  **************

   subroutine write_int(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, matrix "item".
      INT :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_int")
      START_TIMER("ARCHIVE:write_int")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="int")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_int")
      CHECK
   end subroutine

   subroutine write_real(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, matrix "item".
      REAL :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_real")
      START_TIMER("ARCHIVE:write_real")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_real")
      CHECK
   end subroutine

   subroutine write_vec(self,item,genre,format)
    ARCHIVE :: self
   ! Write to the archive, vector "item". "format" is the output format wanted
   ! for text files
      REALVEC(:) :: item
      STR(*), optional :: genre,format
      STR(STR_SIZE) :: fmt
      STACK("ARCHIVE:write_vec")
      START_TIMER("ARCHIVE:write_vec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         fmt = "row"; if (present(format)) fmt = format
         call put_(self%textfile,item,fmt)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_vec")
      CHECK
   end subroutine

   subroutine write_cvec(self,item,genre,format)
    ARCHIVE :: self
   ! Write to the archive, complex vector "item". "format" is the output format
   ! wanted for text files
      CPXVEC(:) :: item
      STR(*), optional :: genre,format
      STR(STR_SIZE) :: fmt
      STACK("ARCHIVE:write_cvec")
      START_TIMER("ARCHIVE:write_cvec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         fmt = "row"; if (present(format)) fmt = format
         call put_(self%textfile,item,fmt)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_cvec")
      CHECK
   end subroutine

   subroutine write_mat(self,item,genre,order)
    ARCHIVE :: self
   ! Write to the archive, matrix "item". "order" is the output format wanted
   ! for text files
      REALMAT(:,:) :: item
      STR(*), optional :: genre,order
      STR(STR_SIZE) :: ord
      STACK("ARCHIVE:write_mat")
      START_TIMER("ARCHIVE:write_mat")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         ord = "by_row"; if (present(order)) ord = order
         call put_(self%textfile,item,ord)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_mat")
      CHECK
   end subroutine

   subroutine write_mat3(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, matrix "item".
      REALMAT3(:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_mat3")
      START_TIMER("ARCHIVE:write_mat3")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_mat3")
      CHECK
   end subroutine

   subroutine write_mat4(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, matrix "item".
      REALMAT4(:,:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_mat4")
      START_TIMER("ARCHIVE:write_mat4")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_mat4")
      CHECK
   end subroutine

   subroutine write_cmat(self,item,genre,order)
    ARCHIVE :: self
   ! Write to the archive, complex matrix "item". "order" is the output format
   ! wanted for text files
      CPXMAT(:,:) :: item
      STR(*), optional :: genre,order
      STR(STR_SIZE) :: ord
      STACK("ARCHIVE:write_cmat")
      START_TIMER("ARCHIVE:write_cmat")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         ord = "by_row"; if (present(order)) ord = order
         call put_(self%textfile,item,ord)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_cmat")
      CHECK
   end subroutine

   subroutine write_cmat3(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, complex matrix "item".
      CPXMAT3(:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_cmat3")
      START_TIMER("ARCHIVE:write_cmat3")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_cmat3")
      CHECK
   end subroutine

   subroutine write_cmat4(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, complex matrix "item".
      CPXMAT4(:,:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_cmat4")
      START_TIMER("ARCHIVE:write_cmat4")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_cmat4")
      CHECK
   end subroutine

   subroutine write_cmat5(self,item,genre)
    ARCHIVE :: self
   ! Write to the archive, complex matrix "item".
      CPXMAT5(:,:,:,:,:) :: item
      STR(*), optional :: genre
      STACK("ARCHIVE:write_cmat5")
      START_TIMER("ARCHIVE:write_cmat5")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=TRUE,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_cmat5")
      CHECK
   end subroutine

   subroutine write_vec_mat(self,item1,item2)
    ARCHIVE :: self
   ! Write to the archive, vector "item1" and matrix "item2".
      REALVEC(:) :: item1
      REALMAT(:,:) :: item2
      STACK("ARCHIVE:write_vec_mat")
      START_TIMER("ARCHIVE:write_vec_mat")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item1)
         call put_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item1)
         call write_buffered_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_vec_mat")
      CHECK
   end subroutine

   subroutine write_mat_vec(self,item1,item2)
    ARCHIVE :: self
   ! Write to the archive, matrix "item1" and vector "item2".
      REALMAT(:,:) :: item1
      REALVEC(:) :: item2
      STACK("ARCHIVE:write_mat_vec")
      START_TIMER("ARCHIVE:write_mat_vec")
      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,FALSE)
         call put_(self%textfile,item1)
         call put_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         call open_(self%file,for="write-only",buffered=TRUE,type="real")
         call write_buffered_(self%file,item1)
         call write_buffered_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if
     STOP_TIMER("ARCHIVE:write_mat_vec")
      CHECK
   end subroutine

   subroutine write_opvector(self,item,genre,order)
    ARCHIVE :: self
   ! Write to the archive operator vector "item". "order" indicated the output
   ! order for ascii files.
      OPVECTOR :: item
      STR(*), optional :: genre,order
      STR(STR_SIZE) :: itemgenre
      STACK("ARCHIVE:write_opvector")
      START_TIMER("ARCHIVE:write_opvector")
      if (present(genre)) then; itemgenre = genre
      else;                    itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted  "); call write_(self, item%restricted,"restricted",order)
         case ("unrestricted"); call write_(self, item%alpha,"alpha",order)
                                call write_(self, item%beta,"beta",order)
         case ("alpha       "); call write_(self, item%alpha,"alpha",order)
         case ("beta        "); call write_(self, item%beta,"beta",order)
         case ("general     "); call write_(self, item%general,"general",order)
         case default;          allocate(tonto%known_keywords(5))
         tonto%known_keywords(1) = "restricted  "
         tonto%known_keywords(2) = "unrestricted"
         tonto%known_keywords(3) = "alpha       "
         tonto%known_keywords(4) = "beta        "
         tonto%known_keywords(5) = "general     "
         call unknown_(tonto,itemgenre,"ARCHIVE:write_opvector")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("ARCHIVE:write_opvector")
      CHECK
   end subroutine

   subroutine write_opmatrix(self,item,genre,order)
    ARCHIVE :: self
   ! Write to the archive operator vector "item". "order" indicates the output
   ! order for ascii files.
      OPMATRIX :: item
      STR(*), optional :: genre,order
      STR(STR_SIZE) :: itemgenre
      STACK("ARCHIVE:write_opmatrix")
      START_TIMER("ARCHIVE:write_opmatrix")
      if (present(genre)) then; itemgenre = genre
      else;                    itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted");           call write_(self,item%restricted,"restricted",order)
         case ("unrestricted");         call write_(self,item%alpha,"alpha",order)
                                        call write_(self,item%beta,"beta",order)
         case ("alpha");                call write_(self,item%alpha,"alpha",order)
         case ("beta");                 call write_(self,item%beta,"beta",order)
         case ("general");              call write_(self,item%general,"general",order)
         case ("restricted_complex");   call write_(self,item%restricted_complex,"restricted_complex",order)
         case ("unrestricted_complex"); call write_(self,item%alpha_complex,"alpha_complex",order)
                                        call write_(self,item%beta_complex,"beta_complex",order)
         case ("alpha_complex");        call write_(self,item%alpha_complex,"alpha_complex",order)
         case ("beta_complex");         call write_(self,item%beta_complex,"beta_complex",order)
         case ("general_complex");      call write_(self,item%general_complex,"general_complex",order)
         case default;                  DIE("ARCHIVE:write_opmatrix ... unknown genre, "//trim(itemgenre))
      end select
     STOP_TIMER("ARCHIVE:write_opmatrix")
      CHECK
   end subroutine

!  **********************
!  Write gnuplot routines
!  **********************

   subroutine write_gnuplot(self,density,n_x,n_y,n_z)
    ARCHIVE :: self
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in gnuplot format (suitable for an splot).
      REALVEC(:) :: density
      INT :: n_x,n_y,n_z
      INT :: n,x,y,z
      STACK("ARCHIVE:write_gnuplot")
      START_TIMER("ARCHIVE:write_gnuplot")
      ENSURE(is_a_text_file_(self),"ARCHIVE:write_gnuplot ... must open a ascii file")
      call open_(self,for="writing")
      call set_real_style_(self%textfile,"e")
      call set_real_width_(self%textfile,30)
      call set_real_precision_(self%textfile,15)
      n = 1
      do z = 1,n_z
         if (n_z>1) call show_(self%textfile," z = ",z)
         do y = 1,n_y
         do x = 1,n_x
            call put_(self%textfile,density(n),flush=1)
            n = n+1
         end do
         call flush_(self%textfile)
         end do
      end do
      call close_(self)
     STOP_TIMER("ARCHIVE:write_gnuplot")
      CHECK
   end subroutine

   subroutine write_gnuplot_1(self,j,n_x,n_y,n_z,norm,normalise)
    ARCHIVE :: self
   ! Write a current density vector "j" corresponding to a grid with dimensions
   ! "n_x", "n_y", "n_z" in gnuplot format (suitable for an splot).
   ! If "norm" is present and TRUE write the norm of the vector field
   ! If "normalise" is present and TRUE write the normalised of the vector field
       REALMAT(:,:) :: j
      INT :: n_x,n_y,n_z
      BIN, optional :: norm,normalise
      INT :: n,x,y,z
      BIN :: write_norm,write_normalised
       REALVEC(3) :: v
   STACK("ARCHIVE:write_gnuplot_1")
   START_TIMER("ARCHIVE:write_gnuplot_1")
   ENSURE(is_a_text_file_(self),"ARCHIVE:write_gnuplot_1 ... must open a ascii file")
      call open_(self,for="writing")
      call set_real_style_(self%textfile,"e")
      call set_real_width_(self%textfile,30)
      call set_real_precision_(self%textfile,15)
      write_norm = FALSE; if (present(norm)) write_norm = norm
      write_normalised = FALSE; if (present(normalise)) write_normalised = normalise
      n = 1
      do z = 1,n_z
         if (n_z>1) then
            call put_(self%textfile," z = ")
            call put_(self%textfile,z)
            call flush_(self%textfile)
         end if
         do y = 1,n_y
         do x = 1,n_x
            v = j(n,:)
            if (write_norm) then
               call put_(self%textfile, norm_(v))
            else
               if (write_normalised) call normalise_(v)
               call put_(self%textfile,v(1))
               call put_(self%textfile,v(2))
               call put_(self%textfile,v(3))
            end if
            call flush_(self%textfile)
            n = n+1
         end do
         call flush_(self%textfile)
         end do
      end do
      call close_(self)
     STOP_TIMER("ARCHIVE:write_gnuplot_1")
      CHECK
   end subroutine

end
