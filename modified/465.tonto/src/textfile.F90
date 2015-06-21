!-----------------------------------------------------------------------
!
! TEXTFILE : Line-oriented formatted sequential advancing file
! input/output, including input from standard input, and output to
! standard output.
!
! The TEXTFILE object is a FILE with a BUFFER and a REALFMT formatting
! object. The input/output proceeds via a line buffer. It is forbidden
! to open a TEXTFILE simultaneously for input and output.
!
! There is a default internal standard input object, "stdin",
! and a default standard output object, "stdout".
!
! A line count is maintained to allow back-tracking to previous lines,
! and particular items on each line. This is useful for input.
!
! For output, items can be put in columns of a specified width, with
! double precision numbers having a specified precision and "style"
! (set using fortran conventions). This is useful for dynamic tables,
! which the user can change at run time. Rewind and backtracking
! are not allowed for output files.
!
! It is possible to redirect the input or output to a new file
! using the "redirect" command. The previous file can be recovered
! using "unsave", or it will revert back automatically to the previous
! file if the redirected file ends. It is also possibile to redirect
! input to an internal file. This is useful for processing a list of
! text as if it were a file. Output to an internal file is not allowed.
!
! Note that the input buffer is limited to size BSTR_SIZE, set in the
! "macros" file.
!
! The system information in "tonto" is updated whenever a I/O operation
! or a buffer operation is performed.
!
! Copyright (C) Dylan Jayatilaka, 1999
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
! $Id: textfile.foo,v 1.40.2.19 2004/04/21 09:47:21 reaper Exp $
!---------------------------------------------------------------------------

module TEXTFILE_MODULE

#  include "textfile.use"

   implicit none

#  include "macros"
#  include "textfile.int"


   public read_; interface read_
      module procedure read_str
      module procedure read_real_quantity
      module procedure read_formatted_real
      module procedure read_imprecise_real
      module procedure read_cpx
      module procedure read_int
      module procedure read_bin
      module procedure read_intvec
      module procedure read_binvec
      module procedure read_strvec
      module procedure read_realvec_quantity
      module procedure read_realvec_pair_quantities
      module procedure read_cpxvec
      module procedure read_intmat
      module procedure read_realmat_quantity
      module procedure read_cpxmat
      module procedure read_realmat3
      module procedure read_realmat4
      module procedure read_cpxmat3
      module procedure read_cpxmat4
      module procedure read_cpxmat5
      module procedure read_intvecvec
   end interface

   public read_ptr_; interface read_ptr_
      module procedure read_strvec_ptr
      module procedure read_binvec_ptr
      module procedure read_intvec_ptr
      module procedure read_realvec_quantity_ptr
      module procedure read_cpxvec_ptr
      module procedure read_realmat_ptr
      module procedure read_intmat_ptr
      module procedure read_intvecpxvec_ptr
   end interface

   public put_; interface put_
      module procedure put_info
      module procedure put_str
      module procedure put_int
      module procedure put_bin
      module procedure put_real
      module procedure put_cpx
      module procedure put_intvec
      module procedure put_strvec
      module procedure put_realvec
      module procedure put_cpxvec
      module procedure put_binvec
      module procedure put_intmat
      module procedure put_realmat
      module procedure put_realmat3
      module procedure put_realmat4
      module procedure put_cpxmat
      module procedure put_cpxmat3
      module procedure put_cpxmat4
      module procedure put_cpxmat5
      module procedure put_intvecvec
      module procedure put_opvector
      module procedure put_opmatrix
   end interface

   public show_; interface show_
      module procedure show_str
      module procedure show_int
      module procedure show_int_3
      module procedure show_bin
      module procedure show_real
      module procedure show_real_3
      module procedure show_binvec
      module procedure show_intvec
      module procedure show_realvec
      module procedure show_strvec
   end interface

   TEXTFILE, PTR, public :: stdin DEFAULT_NULL

   TEXTFILE, PTR, public :: stdout DEFAULT_NULL

contains

!  *****************************
!  File creation type operations
!  *****************************

   subroutine create_stdin(self)
    TEXTFILE :: self
   ! Create a the standard input file object, if needed.
   ! Return a pointer to it if already created
      PTR :: self
      STACK("TEXTFILE:create_stdin")
      START_TIMER("TEXTFILE:create_stdin")
      if (NOT associated(stdin)) then
         allocate(stdin)
         ADD_MEMORY(TEXTFILE_SIZE)
         call nullify_ptr_part_(stdin)
         stdin%name = "stdin"
         stdin%action = "read"
         stdin%record = 0
         stdin%unit = TEXTFILE_STDIN_UNIT
         stdin%io_status = 0
         stdin%ignore_end_of_file = FALSE
         stdin%no_of_lines = -1
         stdin%default_units = " "
         stdin%comment_chars = TEXTFILE_COMMENT_CHARS
         stdin%quote_chars   = TEXTFILE_QUOTE_CHARS
      end if
      call clear_(stdin%buffer)
      self => stdin
     STOP_TIMER("TEXTFILE:create_stdin")
      UNSTACK
   end subroutine

   subroutine create_stdout(self)
    TEXTFILE :: self
   ! Create a standard output file object.
   ! Return a pointer to it if already created
      PTR :: self
      STACK("TEXTFILE:create_stdout")
      START_TIMER("TEXTFILE:create_stdout")
      if (NOT associated(stdout)) then
         allocate(stdout)
         ADD_MEMORY(TEXTFILE_SIZE)
         call nullify_ptr_part_(stdout)
         stdout%name = "stdout"
         stdout%action = "write"
         stdout%record = 0
         stdout%unit = TEXTFILE_STDOUT_UNIT
         stdout%io_status = 0
         stdout%ignore_end_of_file = FALSE
         stdout%no_of_lines = -1
         stdout%default_units = " "
         call set_default_format_(stdout)
      end if
      call clear_(stdout%buffer)
      call put_margin_(stdout)
      self => stdout
     STOP_TIMER("TEXTFILE:create_stdout")
      UNSTACK
   end subroutine

   subroutine create(self,name)
    TEXTFILE :: self
   ! Create a textfile, and optionally set the name. Does not open the file.
      PTR :: self
      STR(*), optional :: name
      UNITNUMBER :: unit
      STACK("TEXTFILE:create")
      START_TIMER("TEXTFILE:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(TEXTFILE_SIZE)
      call nullify_ptr_part_(self)
      if (present(name)) then
        self%name = name
      else
        self%name = "unknown"
      end if
      self%action = "unknown"
      self%record = 0
      self%io_status = 0
      self%ignore_end_of_file = FALSE
      self%no_of_lines = -1
      self%default_units = " "
      self%comment_chars = TEXTFILE_COMMENT_CHARS
      self%quote_chars   = TEXTFILE_QUOTE_CHARS
      call get_(unit,self%unit) ! get a unique unit number
      call clear_(self%buffer)
     STOP_TIMER("TEXTFILE:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,internal)
    TEXTFILE :: self
   ! Create an internal textfile
      PTR :: self
      STRVEC(len=*,:), IN :: internal
      STACK("TEXTFILE:create_1")
      START_TIMER("TEXTFILE:create_1")
      ENSURE(NOT associated(self%internal),"TEXTFILE:create_1 ... internal file already exists")
      nullify(self)
      allocate(self)
      ADD_MEMORY(TEXTFILE_SIZE)
      call nullify_ptr_part_(self)
      self%name = "internal"
      self%record = 0
      self%io_status = 0
      self%ignore_end_of_file = FALSE
      self%no_of_lines = size(internal)
      self%default_units = " "
      self%comment_chars = TEXTFILE_COMMENT_CHARS
      self%quote_chars   = TEXTFILE_QUOTE_CHARS
      self%unit = 0
      call create_(self%internal,size(internal))
      self%internal = internal
      self%action = "read"    ! only read action allowed
      call read_line_(self)
     STOP_TIMER("TEXTFILE:create_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    TEXTFILE :: self
   ! Destroy a textfile
      PTR :: self
      UNITNUMBER :: unit
      STACK("TEXTFILE:destroy")
      START_TIMER("TEXTFILE:destroy")
      if (NOT associated(self)) then; STOP_TIMER("TEXTFILE:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      if (is_open_(self)) call close_(self)
      if (do_io_(tonto_parallel)) then
        call free_(unit,self%unit)
      end if
      DELETE_MEMORY(TEXTFILE_SIZE)
      deallocate(self)
     STOP_TIMER("TEXTFILE:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    TEXTFILE :: self
   ! Nullify the pointer parts of "self".
      STACK("TEXTFILE:nullify_ptr_part")
      START_TIMER("TEXTFILE:nullify_ptr_part")
      nullify(self%internal)
      nullify(self%saved)
     STOP_TIMER("TEXTFILE:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    TEXTFILE :: self
   ! Destroy the pointer parts of "self", including any saved parts,
   ! which are destroyed recursively.
      STACK("TEXTFILE:destroy_ptr_part")
      START_TIMER("TEXTFILE:destroy_ptr_part")
      call destroy_(self%internal)
      call destroy_(self%saved)
     STOP_TIMER("TEXTFILE:destroy_ptr_part")
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

   recursive subroutine create_copy(self,file)
    TEXTFILE :: self
   ! Create a copy of this textfile
      PTR :: self
      TEXTFILE :: file
      UNITNUMBER :: unit
      STACK("TEXTFILE:create_copy")
      START_TIMER("TEXTFILE:create_copy")
      call create_(self," ")
      if (do_io_(tonto_parallel)) then
        call free_(unit,self%unit)
      end if
      call copy_(self,file)
     STOP_TIMER("TEXTFILE:create_copy")
      UNSTACK
   end subroutine

   recursive subroutine copy(self,file)
    TEXTFILE :: self
   ! Make a copy of this textfile
      TEXTFILE :: file
      STACK("TEXTFILE:copy")
      START_TIMER("TEXTFILE:copy")
      self = file
      call copy_(self%buffer,file%buffer)
      if (associated(file%internal)) &
         call create_copy_(self%internal,file%internal)
      if (associated(file%saved)) &
         call create_copy_(self%saved,file%saved)
     STOP_TIMER("TEXTFILE:copy")
      UNSTACK
   end subroutine

!  **********************
!  Saving and redirection
!  **********************

   subroutine save(self)
    TEXTFILE :: self
   ! Saved everything about the current textfile "self" in ".saved"
   ! Do not open a new textfile. Can be used to save style settings.
      PTR :: self
      TEXTFILE, PTR :: saved
      STACK("TEXTFILE:save")
      START_TIMER("TEXTFILE:save")
      saved => self
      nullify(self)
      allocate(self)
      ADD_MEMORY(TEXTFILE_SIZE)
      self = saved            ! Keep all settings, even line number
      self%saved => saved
     STOP_TIMER("TEXTFILE:save")
      UNSTACK
   end subroutine

   subroutine unsave(self)
    TEXTFILE :: self
   ! Revert to previously saved textfile settings. Note that this
   ! is not the same as reverting to a previously saved textfile
      TEXTFILE, PTR :: saved
      STACK("TEXTFILE:unsave")
      START_TIMER("TEXTFILE:unsave")
      ENSURE(associated(self%saved),"TEXTFILE:unsave ... no previous settings")
      saved => self%saved
      self  = saved
      DELETE_MEMORY(TEXTFILE_SIZE)
      deallocate(saved)
     STOP_TIMER("TEXTFILE:unsave")
      UNSTACK
   end subroutine

   subroutine redirect(self,name)
    TEXTFILE :: self
   ! Save all the info for the current file in ".saved", and open a new
   ! textfile.  This is used for input, or output redirection. The new file
   ! retains the style settings of the saved file
      PTR :: self
      STR(*) :: name
      TEXTFILE, PTR :: saved
      STACK("TEXTFILE:redirect")
      START_TIMER("TEXTFILE:redirect")
      saved => self
      call create_(self,name)
      self%saved => saved
      call use_style_(self,saved)
      call open_(self,for=self%saved%action)
     STOP_TIMER("TEXTFILE:redirect")
      UNSTACK
   end subroutine

   subroutine redirect_1(self,internal)
    TEXTFILE :: self
   ! Save all the info for the current file in ".saved", and open a new
   ! internal textfile. This is used for *only* input redirection.
   ! The new file retains the style settings of the saved file
      PTR :: self
      STRVEC(len=*,:), IN :: internal
      TEXTFILE, PTR :: saved
      STACK("TEXTFILE:redirect_1")
      START_TIMER("TEXTFILE:redirect_1")
      saved => self
      nullify(self%internal)
      call create_(self,internal)
      self%saved => saved
      call use_style_(self,saved)
     STOP_TIMER("TEXTFILE:redirect_1")
      UNSTACK
   end subroutine

   subroutine use_style(self,saved)
    TEXTFILE :: self
   ! Revert to the previously saved style settings, including
   ! any default units.
      TEXTFILE :: saved
      STACK("TEXTFILE:use_style")
      START_TIMER("TEXTFILE:use_style")
      self%default_units = saved%default_units
      self%use_labels    = saved%use_labels
      self%margin_width  = saved%margin_width
      self%n_fields      = saved%n_fields
      self%int_width     = saved%int_width
      self%real_width     = saved%real_width
      self%real_precision = saved%real_precision
      self%real_style     = saved%real_style
      self%default_units = saved%default_units
     STOP_TIMER("TEXTFILE:use_style")
      CHECK
   end subroutine

   subroutine revert(self)
    TEXTFILE :: self
   ! Revert to the previously redirected textfile, but keep current style
   ! settings, including (for example) any default_units settings.
      TEXTFILE, PTR :: saved
      UNITNUMBER :: unit
      STACK("TEXTFILE:revert")
      START_TIMER("TEXTFILE:revert")
      ENSURE(associated(self%saved),"TEXTFILE:revert ... no previous settings")
      saved => self%saved
      if (associated(self%internal)) then
         call destroy_(self%internal)      ! Do not destroy .saved recursively ...
      else
         call close_(self)
         call free_(unit,self%unit)
      end if
      call use_style_(saved,self)
      self  = saved
      self%io_status = -1           ! Soft-ending
      DELETE_MEMORY(TEXTFILE_SIZE)
      deallocate(saved)         ! instead, just deallocate it
     STOP_TIMER("TEXTFILE:revert")
      UNSTACK
   end subroutine

!  ******************************
!  Opening, closing, and deletion
!  ******************************

   subroutine open(self)
    TEXTFILE :: self
   ! Open the textfile based on its action attribute
     STACK("TEXTFILE:open")
     START_TIMER("TEXTFILE:open")
     ENSURE(self%action/="unknown","TEXTFILE:open ... file has unknown action")
     call open_(self,for=self%action)
     STOP_TIMER("TEXTFILE:open")
      CHECK
   end subroutine

   subroutine open_1(self,for)
    TEXTFILE :: self
   ! Open the textfile "for" either "read" or "write".
     STR(*) :: for
     STACK("TEXTFILE:open_1")
     START_TIMER("TEXTFILE:open_1")
     select case (for)
        case("read      ","reading   ","read-only ") ; call open_for_read_(self)
        case("write     ","writing   ","write-only") ; call open_for_write_(self)
        case default; allocate(tonto%known_keywords(6))
        tonto%known_keywords(1) = "read      "
        tonto%known_keywords(2) = "reading   "
        tonto%known_keywords(3) = "read-only "
        tonto%known_keywords(4) = "write     "
        tonto%known_keywords(5) = "writing   "
        tonto%known_keywords(6) = "write-only"
        call unknown_(tonto,for,"TEXTFILE:open_1")
        deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("TEXTFILE:open_1")
      CHECK
   end subroutine

   subroutine open_for_read(self)
    TEXTFILE :: self
   ! Open the input file. The input file object must already be created
   ! The file is positioned at the first line.
      STACK("TEXTFILE:open_for_read")
      START_TIMER("TEXTFILE:open_for_read")
      ENSURE(NOT associated(self%internal),"TEXTFILE:open_for_read ... no need to open an internal file")
      ENSURE(exists_(self),"TEXTFILE:open_for_read ... opening new file "//trim(self%name)//" for read!")
      ENSURE(self%action=="unknown" OR self%action=="read","TEXTFILE:open_for_read ... not a readable file")
      self%action = "read"
      if (do_io_(tonto_parallel)) then
        open(unit=self%unit,        &
           file=trim(self%name),     &
           status="old",        &
           access="sequential", &
           form="formatted",    &
           iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      ENSURE(self%io_status==0,"TEXTFILE:open_for_read ... error opening old file "//trim(self%name))
      call rewind_(self)
     STOP_TIMER("TEXTFILE:open_for_read")
      CHECK
   end subroutine

   subroutine open_for_write(self)
    TEXTFILE :: self
   ! Open the output file associated with the output object
      STACK("TEXTFILE:open_for_write")
      START_TIMER("TEXTFILE:open_for_write")
      ENSURE(NOT associated(self%internal),"TEXTFILE:open_for_write ... no need to open an internal file")
      ENSURE(self%action=="unknown" OR self%action=="write","TEXTFILE:open_for_write ... not a writable file")
      self%action = "write"
      if (exists_(self)) then
                   call open_old_file_for_write_(self)
      else
                   call open_new_file_for_write_(self)
      end if
     STOP_TIMER("TEXTFILE:open_for_write")
      CHECK
   end subroutine

   subroutine open_old_file_for_write(self)
    TEXTFILE :: self
   ! Open an old output file for writing
      STACK("TEXTFILE:open_old_file_for_write")
      START_TIMER("TEXTFILE:open_old_file_for_write")
      ENSURE(NOT associated(self%internal),"TEXTFILE:open_old_file_for_write ... no need to open an internal file")
      ENSURE(exists_(self),"TEXTFILE:open_old_file_for_write ... not an existing file!")
      self%action = "write"
      if (do_io_(tonto_parallel)) then
        open(unit=self%unit,        &
           file=trim(self%name),     &
           status="old",        &
           access="sequential", &
           form="formatted",    &
           iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      ENSURE(self%io_status==0,"TEXTFILE:open_old_file_for_write ... error opening old file "//trim(self%name))
      call rewind_(self)
      call set_default_format_(self)
      call put_margin_(self)
     STOP_TIMER("TEXTFILE:open_old_file_for_write")
      CHECK
   end subroutine

   subroutine open_new_file_for_write(self)
    TEXTFILE :: self
   ! Open an new output file for writing
      STACK("TEXTFILE:open_new_file_for_write")
      START_TIMER("TEXTFILE:open_new_file_for_write")
      ENSURE(NOT associated(self%internal),"TEXTFILE:open_new_file_for_write ... no need to open an internal file")
      ENSURE(NOT exists_(self),"TEXTFILE:open_new_file_for_write ... output file exists!")
      self%action = "write"
      if (do_io_(tonto_parallel)) then
        open(unit=self%unit,        &
           file=trim(self%name),     &
           status="new",        &
           access="sequential", &
           form="formatted",    &
           iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      ENSURE(self%io_status==0,"TEXTFILE:open_new_file_for_write ... error opening new file "//trim(self%name))
      call rewind_(self)
      call set_default_format_(self)
      call put_margin_(self)
     STOP_TIMER("TEXTFILE:open_new_file_for_write")
      CHECK
   end subroutine

   subroutine close(self)
    TEXTFILE :: self
   ! Close the input file
      STACK("TEXTFILE:close")
      START_TIMER("TEXTFILE:close")
      close(unit=self%unit)
     STOP_TIMER("TEXTFILE:close")
      CHECK
   end subroutine

   subroutine close_and_delete(self)
    TEXTFILE :: self
   ! Close the input file and delete it from the file system
      STACK("TEXTFILE:close_and_delete")
      START_TIMER("TEXTFILE:close_and_delete")
      if (do_io_(tonto_parallel)) then
        close(unit=self%unit,status="delete")
      end if
     STOP_TIMER("TEXTFILE:close_and_delete")
      CHECK
   end subroutine

   subroutine delete(self)
    TEXTFILE :: self
   ! Delete the input file from the file system
      STACK("TEXTFILE:delete")
      START_TIMER("TEXTFILE:delete")
      if (NOT is_open_(self)) call open_(self,for="read")
      call close_and_delete_(self)
     STOP_TIMER("TEXTFILE:delete")
      CHECK
   end subroutine

!  ******************
!  Read style options
!  ******************

   subroutine read_keywords(self,in)
    TEXTFILE :: self
   ! Read the default output style parameters from another textfile, "in"
      TEXTFILE :: in
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:read_keywords")
      START_TIMER("TEXTFILE:read_keywords")
      ENSURE(in%action=="read",'file "in" is not an input file')
      call read_(in,word)
      ENSURE(word=="{","TEXTFILE:read_keywords ... expecting a {")
      read_loop: do          ! Loop over keywords
         call read_(in,word)
         call to_lower_case_(word)
         if (word=="}")      exit read_loop
         call process_keyword_(self,word,in)
      end do read_loop
     STOP_TIMER("TEXTFILE:read_keywords")
      CHECK
   end subroutine

   subroutine process_keyword(self,keyword,in)
    TEXTFILE :: self
   ! Process a command "keyword". Data is inputted from "in", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      STR(*) :: keyword
      TEXTFILE, target, optional :: in
      TEXTFILE, PTR :: input
      target :: self
      STR(STR_SIZE) :: word,style
      BIN :: bin
      INT :: val
      STACK("TEXTFILE:process_keyword")
      START_TIMER("TEXTFILE:process_keyword")
      word = keyword
      call to_lower_case_(word)
      if (present(in)) then; input => in
      else;                  input => self
      end if
      select case (word)
        case ("real_precision="); call read_(input,val);   call set_real_precision_(self,val)
        case ("real_style=    "); call read_(input,style); call set_real_style_(self,style)
        case ("real_width=    "); call read_(input,val);   call set_real_width_(self,val)
        case ("fields=        "); call read_(input,val);   call set_n_fields_(self,val)
        case ("field_width=   "); call read_(input,val);   call set_real_width_(self,val)
        case ("int_width=     "); call read_(input,val);   call set_int_width_(self,val)
        case ("labels=        "); call read_(input,bin);   call set_use_labels_(self,bin)
        case ("margin=        "); call read_(input,val);   call set_margin_(self,val)
        case ("n_fields=      "); call read_(input,val);   call set_n_fields_(self,val)
        case ("precision=     "); call read_(input,val);   call set_real_precision_(self,val)
        case ("style=         "); call read_(input,style); call set_real_style_(self,style)
        case ("use_labels=    "); call read_(input,bin);   call set_use_labels_(self,bin)
        case default;          allocate(tonto%known_keywords(12))
        tonto%known_keywords(1) = "real_precision="
        tonto%known_keywords(2) = "real_style=    "
        tonto%known_keywords(3) = "real_width=    "
        tonto%known_keywords(4) = "fields=        "
        tonto%known_keywords(5) = "field_width=   "
        tonto%known_keywords(6) = "int_width=     "
        tonto%known_keywords(7) = "labels=        "
        tonto%known_keywords(8) = "margin=        "
        tonto%known_keywords(9) = "n_fields=      "
        tonto%known_keywords(10) = "precision=     "
        tonto%known_keywords(11) = "style=         "
        tonto%known_keywords(12) = "use_labels=    "
        call unknown_(tonto,word,"TEXTFILE:process_keyword")
        deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:process_keyword")
      UNSTACK
   end subroutine

!  **********************************
!  Line repositioning type operations
!  **********************************

   recursive subroutine read_line(self)
    TEXTFILE :: self
   ! Read a line into the buffer. (The routine is recursive because there may
   ! need to be a reversion to a previously redirected file).
      STACK("TEXTFILE:read_line")
      START_TIMER("TEXTFILE:read_line")
      if (associated(self%internal)) then; call read_line_internal_(self)
      else;                        call read_line_external_(self)
      end if
     STOP_TIMER("TEXTFILE:read_line")
      UNSTACK
   end subroutine

   recursive subroutine read_line_internal(self)
    TEXTFILE :: self
   ! Read a line into the buffer from the internal file. If the file end, then
   ! this routine dies, UNLESS (1) .ignore_end_of_file is set, in which case the
   ! another succesive failure to read a line will generate an error (this
   ! prevents infinite read loops, and is also useful for testing whether at the
   ! end of a file), OR (2) there is a .saved file, in which case the current
   ! file reverts to the saved file, and an attempt is made to read from that
   ! saved file.
      STR(BSTR_SIZE) :: string
      BIN :: eliminate_specials
      STACK("TEXTFILE:read_line_internal")
      START_TIMER("TEXTFILE:read_line_internal")
      ENSURE(associated(self%internal),"TEXTFILE:read_line_internal ... no internal file")
      call update_system_info_(self)
      if (self%record<size(self%internal)) then      ! No errors.
         self%io_status = 0
         self%record = self%record+1
         string = self%internal(self%record)
         call set_(self%buffer,string,self%comment_chars,self%quote_chars,eliminate_specials)
      else                                   ! At end of file.
         self%io_status = 1
         if (self%ignore_end_of_file) then       ! Ignore end of file ... 
            self%ignore_end_of_file = FALSE      ! Next time don't ignore
         else                               
            if(associated(self%saved)) then          ! It's OK, go back to saved file
               call revert_(self)
               call read_line_(self)
            else
               DIE("TEXTFILE:read_line_internal ... unexpected end of file") ! Die if nothing saved
            end if
         end if
      end if
      call update_system_info_(self)
     STOP_TIMER("TEXTFILE:read_line_internal")
      UNSTACK
   end subroutine

   recursive subroutine read_line_external(self)
    TEXTFILE :: self
   ! Read a line into the buffer from the input file. If the file ends, this
   ! routine dies, UNLESS (1) .ignore_end_of_file is set, in which case another
   ! succesive failure to read a line will generate an error (this prevents
   ! infinite read loops, and is also useful for testing whether at the end of a
   ! file), OR (2) there is a .saved file, in which case the current file
   ! reverts to the saved file, and an attempt is made to read that saved file.
      INT :: fail
      STR(BSTR_SIZE) :: string
      BIN :: eliminate_specials
      STACK("TEXTFILE:read_line_external")
      START_TIMER("TEXTFILE:read_line_external")
      ENSURE(is_open_(self),"TEXTFILE:read_line_external ... file is not open")
      ENSURE(self%action=="read","TEXTFILE:read_line_external ... file does not have read action!")
      call update_system_info_(self)
      string = " "
      if (do_io_(tonto_parallel)) then
        fail = 2
        read(unit=self%unit,fmt="(a)",end=20,err=10) string
        fail = fail-1
20      fail = fail-1
10      continue
      end if
      call broadcast_(tonto_parallel,fail,0)
      call broadcast_(tonto_parallel,string,0)
      select case (fail)
         case (0)                               ! No errors.
            self%io_status = 0                      ! Read a line into the buffer.
            call set_(self%buffer,string,self%comment_chars,self%quote_chars,eliminate_specials) 
            self%record = self%record + 1
         case (1)                               ! At end of file.
            self%io_status = 1
            if (self%ignore_end_of_file) then       ! Ignore end of file ... 
               self%ignore_end_of_file = FALSE      ! Next time don't ignore
               if (self%no_of_lines<0) then
                  self%no_of_lines = self%record
                  self%record = self%record + 1
               else
                  self%record = self%no_of_lines + 1
               end if
            else                               
               if(associated(self%saved)) then          ! It's OK, go back to saved file
                  call revert_(self)
                  call read_line_(self)                    ! Try again ...
               else
                  DIE("TEXTFILE:read_line_external ... unexpected end of file") ! Die if nothing saved
               end if
            end if
         case (2)                               ! Some error, but not end of file.
            DIE("TEXTFILE:read_line_external ... read error")
      end select
      call update_system_info_(self)
     STOP_TIMER("TEXTFILE:read_line_external")
      UNSTACK
   end subroutine

   subroutine update_line(self)
    TEXTFILE :: self
   ! Get the next non-exhausted line if the current one is exhausted---and in
   ! this case, the buffer pointer is positioned before the first line item.
   ! This routine performs a reversion if there is a .saved file.
      STACK("TEXTFILE:update_line")
      START_TIMER("TEXTFILE:update_line")
      do
         if (not_exhausted_(self%buffer)) exit
         call read_line_(self)
         if (end_of_file_(self)) exit  
      end do
     STOP_TIMER("TEXTFILE:update_line")
      UNSTACK
   end subroutine

   subroutine revert_line(self)
    TEXTFILE :: self
   ! Revert back to the previous non-exhausted line in the input file if the
   ! current buffer pointer is before the first item; and in this case,
   ! the buffer pointer is repositioned *after* the last line item.
      STACK("TEXTFILE:revert_line")
      START_TIMER("TEXTFILE:revert_line")
      do
         if (next_line_item_(self)>1) exit
         call backspace_line_(self)
         call move_to_line_item_(self,last_line_item_(self)+1)
      end do
     STOP_TIMER("TEXTFILE:revert_line")
      CHECK
   end subroutine

   subroutine skip_next_item(self)
    TEXTFILE :: self
   ! Move to the next item in the input file.
      STACK("TEXTFILE:skip_next_item")
      START_TIMER("TEXTFILE:skip_next_item")
      call update_line_(self)
      call skip_item_(self%buffer)
     STOP_TIMER("TEXTFILE:skip_next_item")
      UNSTACK
   end subroutine

   subroutine move_to_previous_item(self)
    TEXTFILE :: self
   ! Move to the previous item in the input file. Backspace a line if required.
      STACK("TEXTFILE:move_to_previous_item")
      START_TIMER("TEXTFILE:move_to_previous_item")
      call revert_line_(self)
      call move_to_previous_item_on_line_(self)     ! move to the last read item
     STOP_TIMER("TEXTFILE:move_to_previous_item")
      CHECK
   end subroutine

   subroutine move_to_last_item_on_line(self)
    TEXTFILE :: self
   ! Move the cursor over to the beginning of the last item on the line
      INT :: item
      STACK("TEXTFILE:move_to_last_item_on_line")
      START_TIMER("TEXTFILE:move_to_last_item_on_line")
      item = last_line_item_(self)
      call move_to_line_item_(self,item)
     STOP_TIMER("TEXTFILE:move_to_last_item_on_line")
      CHECK
   end subroutine

   subroutine move_to_previous_item_on_line(self)
    TEXTFILE :: self
   ! Move the cursor over to the beginning of the previous item on the
      INT :: item
      STACK("TEXTFILE:move_to_previous_item_on_line")
      START_TIMER("TEXTFILE:move_to_previous_item_on_line")
      item = previous_line_item_(self)
      call move_to_line_item_(self,item)
     STOP_TIMER("TEXTFILE:move_to_previous_item_on_line")
      CHECK
   end subroutine

   subroutine move_to_line_item(self,number)
    TEXTFILE :: self
   ! Move the cursor over to the beginning of the item with index "number"
   ! on the current line. (More accurately: after the end of the previous item
   ! on the current line. So if there are "nitems" on the line you can move to
   ! "nitem+1" if you want to, and that will be the end of the line).
      INT :: number
       INT :: n
      STACK("TEXTFILE:move_to_line_item")
      START_TIMER("TEXTFILE:move_to_line_item")
      call update_system_info_(self)
      n = number
      call move_to_item_(self%buffer,n)
     STOP_TIMER("TEXTFILE:move_to_line_item")
      CHECK
   end subroutine

   subroutine look_for_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
    TEXTFILE :: self
   ! Scans through the file for a token which includes the given "item" (the
   ! token in the file must be a separate thing surrounded by whitespace, begin
   ! or end of line, or comment characters; whereas "item" may may match only a
   ! part of a given token). If a token matching "item" exists, the file cursor
   ! is left just after this token. If no match is found, the file is rewound to
   ! the initial line of the search.  If "from" is present then it is used as
   ! the start line for the search. If "until" is present, it is used as the
   ! last line of the search. If "end_tokens" is present then an exact match to
   ! any of these tokens indicates the end of search for "item".  If
   ! "exact_match" is present and TRUE, then "item" must match exactly the token
   ! in the file, instead of just being included in it. If "head_match" is
   ! present and TRUE, then occurs only if the characters at the head of the
   ! "item" string match the token in the file. only If "return_cursor" is
   ! present and FALSE, the cursor is not returned to the starting line of the
   ! search in the case where no match is found. If "found" is present, it is
   ! set TRUE when the item is found, else FALSE. 
      STR(*), IN :: item
      INT, IN, optional :: from,until
      STRVEC(len=*,:), IN, optional :: end_tokens
      BIN, IN, optional :: exact_match,head_match,return_cursor
      BIN, OUT, optional :: found
      INT :: start_record
      STR(STR_SIZE) :: word
      BIN :: fnd,exact,head,put_back,has_item,has_end,match
      INT :: i
      STACK("TEXTFILE:look_for_item")
      START_TIMER("TEXTFILE:look_for_item")
      self%ignore_end_of_file = TRUE
      fnd = FALSE
      start_record = self%record                ! Initialise switches and counters
      if (present(from)) then
         start_record = from
         if (from==1) then; call rewind_(self)         ! The following may yeild an EOF
         else;              call move_to_record_(self,from)
         end if
      end if
      put_back = TRUE; if (present(return_cursor)) put_back = return_cursor
      if (NOT end_of_file_(self)) then
        if (present(until)) then
        ENSURE(until>=start_record,"TEXTFILE:look_for_item ... ending line must be greater than starting line")
        end if
        exact = FALSE; if (present(exact_match)) exact = exact_match
        head = FALSE;  if (present(head_match))  head = head_match
        ENSURE(NOT (head AND exact),"TEXTFILE:look_for_item ... don't specify head *and* exact match")
        has_end = FALSE
        line_search: do                       ! Now do the search here ...
           has_item = includes_(self%buffer%string,item)
           if (present(end_tokens) AND self%record>start_record) &
           has_end = includes_any_in_(self%buffer%string,end_tokens)
           if (has_item OR has_end) then
              do i = 1,self%buffer%n_items
                 call get_next_item_(self,word)
            ! stdout.flush
            ! stdout.show("line    =",.record)
            ! stdout.show("   i    =",i)
            ! stdout.show("   word =",word)
            ! stdout.show("   item =",item)
                 if (has_item) then
                    if (head) then;           match = includes_(word,item,at_start=TRUE)
                    else if (exact) then;     match = word==item
                    else if (NOT exact) then; match = includes_(word,item)
                    end if
                    if (match) then           ! Found a match
                       fnd = TRUE; exit line_search
                    end if
                 end if
                 if (has_end) then
                    if (has_any_included_in_(end_tokens,word,at_start=TRUE)) exit line_search
                 end if
              end do
           end if
           if (present(until)) then
              if (self%record==until) exit line_search
           end if
           call read_line_(self)
           if (end_of_file_(self)) exit line_search
        end do line_search
      end if
      if (present(found)) found = fnd
      if (NOT fnd AND put_back) call move_to_record_(self,start_record)
      self%ignore_end_of_file = FALSE
     STOP_TIMER("TEXTFILE:look_for_item")
      CHECK
   end subroutine

   subroutine look_backwards_for_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
    TEXTFILE :: self
   ! Scans *backwards* through the file for a token which includes the given
   ! "item" (the token in the file must be a separate thing surrounded by
   ! whitespace, begin or end of line, or comment characters; whereas "item" may
   ! may match only a part of a given token). If a token matching "item" exists,
   ! the file cursor is left just after this token. If no match is found, the
   ! file is rewound to the initial line of the search.  If "from" is present
   ! then it is used as the start line for the search. If "until" is present, it
   ! is used as the last line of the search. If "end_tokens" is present then an
   ! exact match to any of these tokens indicates the end of search for "item".
   ! If "exact_match" is present and TRUE, then "item" must match exactly the
   ! token in the file, instead of just being included in it. If "head_match" is
   ! present and TRUE, then occurs only if the characters at the head of the
   ! "item" string match the token in the file. only If "return_cursor" is
   ! present and FALSE, the cursor is not returned to the starting line of the
   ! search in the case where no match is found. If "found" is present, it is
   ! set TRUE when the item is found, else FALSE. 
      STR(*), IN :: item
      INT, IN, optional :: from,until
      STRVEC(len=*,:), IN, optional :: end_tokens
      BIN, IN, optional :: exact_match,head_match,return_cursor
      BIN, OUT, optional :: found
      INT :: start_record,end_record
      STR(STR_SIZE) :: word
      BIN :: fnd,exact,head,put_back,has_item,has_end,match
      INT :: i
      STACK("TEXTFILE:look_backwards_for_item")
      START_TIMER("TEXTFILE:look_backwards_for_item")
      self%ignore_end_of_file = TRUE
      fnd = FALSE
      start_record = self%record                ! Initialise switches and counters
      if (present(from)) start_record = from
      call move_to_record_(self,start_record)         ! Possible EOF here ...
      put_back = TRUE; if (present(return_cursor)) put_back = return_cursor
      if (NOT end_of_file_(self)) then
        call move_to_last_item_on_line_(self)
        end_record = 1; if (present(until)) end_record = until
        ENSURE(end_record<=start_record,"TEXTFILE:look_backwards_for_item ... ending line must be smaller than starting line")
        exact = FALSE; if (present(exact_match)) exact = exact_match
        head = FALSE;  if (present(head_match))  head = head_match
        ENSURE(NOT (head AND exact),"TEXTFILE:look_backwards_for_item ... don't specify head *and* exact match")
        has_end = FALSE
        line_search: do                       ! Now do the search here ...
           has_item = includes_(self%buffer%string,item)
           if (present(end_tokens) AND self%record>start_record) &
           has_end = includes_any_in_(self%buffer%string,end_tokens)
           if (has_item OR has_end) then
              do i = self%buffer%n_items,1,-1     ! Look backwards ...
                 call move_to_line_item_(self,i)
                 call get_next_item_(self,word)
                 if (has_item) then
                    if (head) then;           match = includes_(word,item,at_start=TRUE)
                    else if (exact) then;     match = word==item
                    else if (NOT exact) then; match = includes_(word,item)
                    end if
                    if (match) then           ! Found a match
                       fnd = TRUE; exit line_search
                    end if
                 end if
                 if (has_end) then
                    if (has_any_included_in_(end_tokens,word,at_start=TRUE)) exit line_search
                 end if
              end do
           end if
           if (self%record==end_record) exit line_search
           call backspace_line_(self)                    ! Move to previous line
        end do line_search
      end if
      if (present(found)) found = fnd
      if (NOT fnd AND put_back) call move_to_record_(self,start_record)
      self%ignore_end_of_file = FALSE
     STOP_TIMER("TEXTFILE:look_backwards_for_item")
      CHECK
   end subroutine

   subroutine look_for_any_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
    TEXTFILE :: self
   ! Scans through the file for a token which includes the any of the given
   ! elements in the "item" list (the token in the file must be a separate thing
   ! surrounded by whitespace, begin or end of line, or comment characters;
   ! whereas "item" may may match only a part of a given token). If a token
   ! matching "item" exists, the file cursor is left just after this token. If
   ! no match is found, the file is rewound to the initial line of the search.
   ! If "from" is present then it is used as the start line for the search. If
   ! "until" is present, it is used as the last line of the search. If
   ! "end_tokens" is present then an exact match to any of these tokens
   ! indicates the end of search for "item".  If "exact_match" is present and
   ! TRUE, then "item" must match exactly the token in the file, instead of just
   ! being included in it. If "head_match" is present and TRUE, then occurs only
   ! if the characters at the head of the "item" string match the token in the
   ! file. only If "return_cursor" is present and FALSE, the cursor is not
   ! returned to the starting line of the search in the case where no match is
   ! found. If "found" is present, it is set TRUE when the item is found, else
   ! FALSE. 
      STRVEC(len=*,:), IN :: item
      INT, IN, optional :: from,until
      STRVEC(len=*,:), IN, optional :: end_tokens
      BIN, IN, optional :: exact_match,head_match,return_cursor
      BIN, OUT, optional :: found
      INT :: start_record
      STR(STR_SIZE) :: word
      BIN :: fnd,exact,head,put_back,has_item,has_end,match
      INT :: i,j
      STACK("TEXTFILE:look_for_any_item")
      START_TIMER("TEXTFILE:look_for_any_item")
      self%ignore_end_of_file = TRUE
      fnd = FALSE
      start_record = self%record                ! Initialise switches and counters
      if (present(from)) then
         start_record = from
         if (from==1) then; call rewind_(self)         ! The following may yeild an EOF
         else;              call move_to_record_(self,from)
         end if
      end if
      put_back = TRUE; if (present(return_cursor)) put_back = return_cursor
      if (NOT end_of_file_(self)) then
        if (present(until)) then
        ENSURE(until>=start_record,"TEXTFILE:look_for_any_item ... ending line must be greater than starting line")
        end if
        exact = FALSE; if (present(exact_match)) exact = exact_match
        head = FALSE;  if (present(head_match))  head = head_match
        ENSURE(NOT (head AND exact),"TEXTFILE:look_for_any_item ... don't specify head *and* exact match")
        has_end = FALSE
        line_search: do                       ! Now do the search here ...
           has_item = includes_any_in_(self%buffer%string,item)
           if (present(end_tokens) AND self%record>start_record) &
           has_end = includes_any_in_(self%buffer%string,end_tokens)
           if (has_item OR has_end) then
              do i = 1,self%buffer%n_items
                 call get_next_item_(self,word)
                 if (has_item) then
                    if (head) then
                       do j = 1,size(item)
                          match = includes_(word,item(j),at_start=TRUE)
                          if (match) exit
                       end do
                    else if (exact) then
                       do j = 1,size(item)
                          match = word==item(j)
                          if (match) exit
                       end do
                    else if (NOT exact) then
                       do j = 1,size(item)
                          match = includes_(word,item(j))
                          if (match) exit
                       end do
                    end if
                    if (match) then           ! Found a match
                       fnd = TRUE; exit line_search
                    end if
                 end if
                 if (has_end) then
                    if (has_any_included_in_(end_tokens,word,at_start=TRUE)) exit line_search
                 end if
              end do
           end if
           if (present(until)) then
              if (self%record==until) exit line_search
           end if
           call read_line_(self)
           if (end_of_file_(self)) exit line_search
        end do line_search
      end if
      if (present(found)) found = fnd
      if (NOT fnd AND put_back) call move_to_record_(self,start_record)
      self%ignore_end_of_file = FALSE
     STOP_TIMER("TEXTFILE:look_for_any_item")
      CHECK
   end subroutine

   subroutine look_for(self,item,from,until,first,found)
    TEXTFILE :: self
   ! Scans through the file for a line which includes string "item".  If there,
   ! the file record is left at the first line at which the match occured.  If
   ! no match is found, the file is rewound to the initial line before the
   ! search.  If "from" is present then it is used as the start line for the
   ! search, and if "item" is not found the file record is returned to that
   ! line.  If "until" is present then matches to these tokens are used to
   ! indicate the end of search condition for "item". If "first" is present and
   ! TRUE, then the item is matched only if it is the first non-blank token in
   ! the input, and likewise the search is terminated only when the "until"
   ! tokens are the first characters in the input. If "found" is present, it is
   ! set TRUE when the item is found, else FALSE.
      STR(*), IN :: item
      INT, IN, optional :: from
      STRVEC(len=*,:), IN, optional :: until
      BIN, IN, optional :: first
      BIN, OUT, optional :: found
      INT :: start_record
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:look_for")
      START_TIMER("TEXTFILE:look_for")
      start_record = self%record
      if (present(from)) then
         if (from==1) then
            call rewind_(self)
            start_record = 1
         else
            call move_to_record_(self,from)
            start_record = from
         end if
      end if
      if (present(found)) found = FALSE
      self%ignore_end_of_file = TRUE
      line_search: do
         word = trim_blanks_from_start_(self%buffer%string)
         if (is_included_in_(item,word,first)) then
            if (present(found)) found = TRUE
            exit line_search
         end if
         if (present(until) AND self%record>start_record) then
         if (has_any_included_in_(until,word,first)) then
            call move_to_record_(self,start_record)
            if (present(found)) found = FALSE
            exit line_search
         end if
         end if
         call read_line_(self)
         if (end_of_file_(self)) then
            call move_to_record_(self,start_record)
            if (present(found)) found = FALSE
            exit line_search
         end if
      end do line_search
      self%ignore_end_of_file = FALSE
     STOP_TIMER("TEXTFILE:look_for")
      CHECK
   end subroutine

   subroutine look_backwards_for(self,item,from,until,first,found)
    TEXTFILE :: self
   ! Scans backward through the file for a line which includes string "item".
   ! If there, the file record is left at the first line at which the match
   ! occured.  If no match is found, the file is rewound to the initial line
   ! before the search.  If "from" is present then it is used as the start line
   ! for the search, and if "item" is not found the file record is returned to
   ! that line.  If "until" is present then matches to these tokens are used to
   ! indicate the end of search condition for "item". If "first" is present and
   ! TRUE, then the item is matched only if it is the first non-blank token in
   ! the input, and likewise the search is terminated only when the "until"
   ! tokens are the first characters in the input. If "found" is present, it is
   ! set TRUE when the item is found, else FALSE.
      STR(*), IN :: item
      INT, IN, optional :: from
      STRVEC(len=*,:), IN, optional :: until
      BIN, IN, optional :: first
      BIN, OUT, optional :: found
      INT :: start_record
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:look_backwards_for")
      START_TIMER("TEXTFILE:look_backwards_for")
      start_record = self%record
      if (present(from)) then
         call move_to_record_(self,from)
         start_record = from
      end if
      if (present(found)) found = FALSE
      self%ignore_end_of_file = TRUE
      do
         word = trim_blanks_from_start_(self%buffer%string)
         if (is_included_in_(item,word,first)) then
            if (present(found)) found = TRUE
            exit
         end if
         if (self%record==1) then ! This is the start of the file
            call move_to_record_(self,start_record)
            if (present(found)) found = FALSE
            exit
         end if
         call backspace_line_(self)
         if (present(until) AND self%record<start_record) then
         if (has_any_included_in_(until,word,first)) then
            call move_to_record_(self,start_record)
            if (present(found)) found = FALSE
            exit
         end if
         end if
      end do
      self%ignore_end_of_file = FALSE
     STOP_TIMER("TEXTFILE:look_backwards_for")
      CHECK
   end subroutine

   function has_string(self,search) result(res)
    TEXTFILE :: self
   ! Returns TRUE if the file contains string "search". The file is returned
   ! to its original line number after this routine.
      STR(*) :: search
      BIN :: res
      INT :: update_record
      BIN :: found
      STACK("TEXTFILE:has_string")
      START_TIMER("TEXTFILE:has_string")
      update_record = self%record
      call rewind_(self)
      self%ignore_end_of_file = TRUE
      found = FALSE
      do
         if (is_included_in_(search,self%buffer%string)) then
           found = TRUE
           exit
         end if
         call read_line_(self)
         if (end_of_file_(self)) exit
      end do
      call move_to_record_(self,update_record)
      self%ignore_end_of_file = FALSE
      res = found
     STOP_TIMER("TEXTFILE:has_string")
      CHECK
   end function

   function rest_of_line(self) result(res)
    TEXTFILE :: self
   ! If there is anything left on the input line, then it is returned, else
   ! nothing is returned.  Record moves to the next line.
     INOUT :: self
     STR(STR_SIZE) :: res
     STACK("TEXTFILE:rest_of_line")
     START_TIMER("TEXTFILE:rest_of_line")
     ENSURE(self%action=="read","TEXTFILE:rest_of_line ... file does not have read action!")
     if (NOT empty_(self%buffer)) then
       res = self%buffer%string(self%buffer%item_end+1: )
       call read_line_(self)
     else
       res = " "
       call read_line_(self)
     end if
     STOP_TIMER("TEXTFILE:rest_of_line")
      CHECK
   end function

   subroutine rewind(self)
    TEXTFILE :: self
   ! Rewind the input file, reading the first line for read-only files.
      STACK("TEXTFILE:rewind")
      START_TIMER("TEXTFILE:rewind")
      if (associated(self%internal)) then; call rewind_internal_(self)
      else;                        call rewind_external_(self)
      end if
      select case (self%action)
         case("read      ","reading   ","read-only "); call read_line_(self)
         case("write     ","writing   ","write-only") 
         case default; allocate(tonto%known_keywords(6))
         tonto%known_keywords(1) = "read      "
         tonto%known_keywords(2) = "reading   "
         tonto%known_keywords(3) = "read-only "
         tonto%known_keywords(4) = "write     "
         tonto%known_keywords(5) = "writing   "
         tonto%known_keywords(6) = "write-only"
         call unknown_(tonto,self%action,"TEXTFILE:rewind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:rewind")
      CHECK
   end subroutine

   subroutine rewind_internal(self)
    TEXTFILE :: self
   ! Rewind the input file, but do not read a line.
      STACK("TEXTFILE:rewind_internal")
      START_TIMER("TEXTFILE:rewind_internal")
      self%record = 0
      self%io_status = 0
     STOP_TIMER("TEXTFILE:rewind_internal")
      CHECK
   end subroutine

   subroutine rewind_external(self)
    TEXTFILE :: self
   ! Rewind the input file, but do not read a line.
      STACK("TEXTFILE:rewind_external")
      START_TIMER("TEXTFILE:rewind_external")
      call update_system_info_(self)
      if (do_io_(tonto_parallel)) then
        rewind(unit=self%unit,iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      DIE_IF(self%io_status>0,"TEXTFILE:rewind_external ... rewind error")
      self%record = 0
      self%io_status = 0
     STOP_TIMER("TEXTFILE:rewind_external")
      CHECK
   end subroutine

   subroutine move_to_end(self)
    TEXTFILE :: self
   ! Move to the end of the input file
      STACK("TEXTFILE:move_to_end")
      START_TIMER("TEXTFILE:move_to_end")
      if (associated(self%internal)) then; call move_to_end_internal_(self)
      else;                        call move_to_end_external_(self)
      end if
     STOP_TIMER("TEXTFILE:move_to_end")
      CHECK
   end subroutine

   subroutine move_to_end_internal(self)
    TEXTFILE :: self
   ! Move to the end of the input file
      STACK("TEXTFILE:move_to_end_internal")
      START_TIMER("TEXTFILE:move_to_end_internal")
      self%record = size(self%internal)-1
      call read_line_(self)
     STOP_TIMER("TEXTFILE:move_to_end_internal")
      CHECK
   end subroutine

   subroutine move_to_end_external(self)
    TEXTFILE :: self
   ! Move to the end of the input file
      STACK("TEXTFILE:move_to_end_external")
      START_TIMER("TEXTFILE:move_to_end_external")
      call update_system_info_(self)
      do
        if (do_io_(tonto_parallel)) then
          read(unit=self%unit, fmt="()", iostat=self%io_status)
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        if (self%io_status/=0) exit
        self%record = self%record + 1
      end do
      if (do_io_(tonto_parallel)) then
        backspace(unit=self%unit,iostat=self%io_status)
        backspace(unit=self%unit,iostat=self%io_status)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      self%record = max(0,self%record-1)
      call read_line_(self)
     STOP_TIMER("TEXTFILE:move_to_end_external")
      CHECK
   end subroutine

   subroutine backspace_line(self)
    TEXTFILE :: self
   ! Reprocess previously input line
      STACK("TEXTFILE:backspace_line")
      START_TIMER("TEXTFILE:backspace_line")
      call move_to_record_(self,self%record-1)
     STOP_TIMER("TEXTFILE:backspace_line")
      CHECK
   end subroutine

   subroutine skip_line(self)
    TEXTFILE :: self
   ! Skip the next line in the input file
      STACK("TEXTFILE:skip_line")
      START_TIMER("TEXTFILE:skip_line")
      call move_to_record_(self,self%record+1)
     STOP_TIMER("TEXTFILE:skip_line")
      CHECK
   end subroutine

   subroutine move_to_line(self,line)
    TEXTFILE :: self
   ! Make sure that "line" was the last one processed
   ! in the input file
      INT :: line
      STACK("TEXTFILE:move_to_line")
      START_TIMER("TEXTFILE:move_to_line")
      ENSURE(line>=0,"TEXTFILE:move_to_line ... line number must be non-negative")
      call move_to_record_(self,line)
     STOP_TIMER("TEXTFILE:move_to_line")
      CHECK
   end subroutine

   subroutine move_to_record(self,rec)
    TEXTFILE :: self
   ! Move to the requested record "rec" in the input file.
   ! Remember: .record is the last processed record.
      INT :: rec
      STACK("TEXTFILE:move_to_record")
      START_TIMER("TEXTFILE:move_to_record")
      if (associated(self%internal)) then; call move_to_record_internal_(self,rec)
      else;                        call move_to_record_external_(self,rec)
      end if
     STOP_TIMER("TEXTFILE:move_to_record")
      CHECK
   end subroutine

   subroutine move_to_record_internal(self,rec)
    TEXTFILE :: self
   ! Move to the requested record "rec" in the internal file.
   ! Remember: .record is the last processed record.
      INT :: rec
      STACK("TEXTFILE:move_to_record_internal")
      START_TIMER("TEXTFILE:move_to_record_internal")
      ENSURE(rec>=0,"TEXTFILE:move_to_record_internal ... record number must be non-negative")
      ENSURE(rec<=size(self%internal),"TEXTFILE:move_to_record_internal ... record outside file range")
      self%record = rec-1
      call read_line_(self)
     STOP_TIMER("TEXTFILE:move_to_record_internal")
      CHECK
   end subroutine

   subroutine move_to_record_external(self,rec)
    TEXTFILE :: self
   ! Move to the requested record "rec" in the input file.
   ! Remember: .record is the last processed record.
      INT :: rec
      STACK("TEXTFILE:move_to_record_external")
      START_TIMER("TEXTFILE:move_to_record_external")
      ENSURE(rec>=0,"TEXTFILE:move_to_record_external ... record number must be non-negative")
      call update_system_info_(self)
      if (rec<(self%record+1)) then
         do
           if (do_io_(tonto_parallel)) then
             backspace(unit=self%unit,iostat=self%io_status)
           end if
           call broadcast_(tonto_parallel,self%io_status,0)
           DIE_IF(self%io_status>0,"TEXTFILE:move_to_record_external ... backspace error")
           self%record = self%record-1
           self%io_status = 0
           if (rec==(self%record+1)) exit
         end do
      else if (rec>(self%record+1)) then
         do
           if (do_io_(tonto_parallel)) then
             read(unit=self%unit,fmt="()",iostat=self%io_status)
           end if
           call broadcast_(tonto_parallel,self%io_status,0)
           DIE_IF(self%io_status>0,"TEXTFILE:move_to_record_external ... read error")
           self%record = self%record + 1
           if (rec==(self%record+1)) exit
         end do
      end if
      call read_line_(self)
     STOP_TIMER("TEXTFILE:move_to_record_external")
      CHECK
   end subroutine

!  **************************
!  Data input type operations
!  **************************

   subroutine get_next_item(self,word)
    TEXTFILE :: self
   ! Get the next item in the file
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:get_next_item")
      START_TIMER("TEXTFILE:get_next_item")
      call update_line_(self)
      call get_(self%buffer,word)
     STOP_TIMER("TEXTFILE:get_next_item")
      CHECK
   end subroutine

   recursive subroutine read_str(self,word)
    TEXTFILE :: self
   ! Read a str into "word".
     STR(*) :: word
     STR(STR_SIZE) :: the_word
     STACK("TEXTFILE:read_str")
     START_TIMER("TEXTFILE:read_str")
     ENSURE(self%action=="read","TEXTFILE:read_str ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,the_word)
     if (len_trim(the_word)>len(word)) then
        DIE("TEXTFILE:read_str ... Length of word is too long: " // trim(word))
     end if
     word = the_word
     STOP_TIMER("TEXTFILE:read_str")
      UNSTACK
   end subroutine

   subroutine read_real_quantity(self,value)
    TEXTFILE :: self
   ! Read a REAL quantity into "value". Will convert the value of the quantity
   ! from atomic units, if followed by a unit specifier.
   ! WARNING: You must *not* be at the end of file to use this routine, since
   ! it will try to read the next token after the REAL as a units specifier.
   ! (There will usually be at least a close brackets symbol "}" after this
   ! data in TONTO input files).
      REAL :: value
      STR(STR_SIZE) :: units
      BIN :: known_unit
      STACK("TEXTFILE:read_real_quantity")
      START_TIMER("TEXTFILE:read_real_quantity")
      ENSURE(self%action=="read","TEXTFILE:read_real_quantity ... file does not have read action!")
      call read_real_(self,value)
      if (NOT at_end_of_file_(self)) then
        call read_str_(self,units)                      ! Read possible unit string
        known_unit = is_known_unit_(units)   ! If unknown unit, move back
        if (NOT known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(value,units)              ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(value,self%default_units)     ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(value,self%default_units)     ! Convert from assumed default units
        self%default_units = " "
      end if
     STOP_TIMER("TEXTFILE:read_real_quantity")
      CHECK
   end subroutine

   subroutine read_real(self,value)
    TEXTFILE :: self
   ! Read a REAL into "value"
     REAL :: value
      STACK("TEXTFILE:read_real")
      START_TIMER("TEXTFILE:read_real")
      ENSURE(self%action=="read","TEXTFILE:read_real ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,value)
     STOP_TIMER("TEXTFILE:read_real")
      CHECK
   end subroutine

   subroutine read_formatted_real(self,value,format)
    TEXTFILE :: self
   ! Read a formatted REAL into "value". Does not check the end of line!
     REAL :: value
     STR(*), IN :: format
      STACK("TEXTFILE:read_formatted_real")
      START_TIMER("TEXTFILE:read_formatted_real")
      ENSURE(self%action=="read","TEXTFILE:read_formatted_real ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,value,format)
     STOP_TIMER("TEXTFILE:read_formatted_real")
      CHECK
   end subroutine

   subroutine read_imprecise_real(self,value,error)
    TEXTFILE :: self
   ! Read a REAL into "value" and also its associated "error", which appears in
   ! parentheses after it.
     REAL :: value,error
      STACK("TEXTFILE:read_imprecise_real")
      START_TIMER("TEXTFILE:read_imprecise_real")
      ENSURE(self%action=="read","TEXTFILE:read_imprecise_real ... file does not have read action!")
     call update_line_(self)
     call get_(self%buffer,value,error)
     STOP_TIMER("TEXTFILE:read_imprecise_real")
      CHECK
   end subroutine

   subroutine read_cpx(self,value)
    TEXTFILE :: self
   ! Read a cpx into "value"
      CPX :: value
    ! real,imag :: REAL
      STACK("TEXTFILE:read_cpx")
      START_TIMER("TEXTFILE:read_cpx")
      ENSURE(self%action=="read","TEXTFILE:read_cpx ... file does not have read action!")
      call update_line_(self)
    ! .buffer.get(real)
    ! .update_line
    ! .buffer.get(imag)
    ! value = cmplx(real,imag,kind=CPX_KIND)
      call get_(self%buffer,value)
     STOP_TIMER("TEXTFILE:read_cpx")
      CHECK
   end subroutine

   subroutine read_int(self,value)
    TEXTFILE :: self
   ! Read an integer into "value"
      INT :: value
      STACK("TEXTFILE:read_int")
      START_TIMER("TEXTFILE:read_int")
      ENSURE(self%action=="read","TEXTFILE:read_int ... file does not have read action!")
      WARN_IF(self%default_units/=" ","TEXTFILE:read_int ... default units ignored for integers")
      call update_line_(self)
      call get_(self%buffer,value)
     STOP_TIMER("TEXTFILE:read_int")
      CHECK
   end subroutine

   subroutine read_bin(self,value)
    TEXTFILE :: self
   ! Read a logical into "value"
      BIN :: value
      STACK("TEXTFILE:read_bin")
      START_TIMER("TEXTFILE:read_bin")
      ENSURE(self%action=="read","TEXTFILE:read_bin ... file does not have read action!")
      WARN_IF(self%default_units/=" ","TEXTFILE:read_bin ... default units ignored for logicals")
      call update_line_(self)
      call get_(self%buffer,value)
     STOP_TIMER("TEXTFILE:read_bin")
      CHECK
   end subroutine

   function list_length(self,ignore_opening_brace) result(res)
    TEXTFILE :: self
   ! Return the size of the list, by reading the input after an initial opening
   ! bracket "{" until a *matching* end bracket "}" token is found. If
   ! "ignore_opening_brace" is present, the initial curly bracket is not
   ! required. Line breaks are not significant. 
      BIN, optional :: ignore_opening_brace
      INT :: res
      INT :: line,item,n
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:list_length")
      START_TIMER("TEXTFILE:list_length")
      line = line_number_(self)
      item = next_line_item_(self) ! next one to be read
      if (NOT present(ignore_opening_brace)) then
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:list_length ... list does not begin with {")
      end if
      n = 0
      res = 0
      do
         call read_(self,word)
         if (word=="{") n = n + 1
         if (word=="}" AND n==0) exit
         if (word=="}" AND n>0) n = n - 1
         res = res+1
      end do
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)
     STOP_TIMER("TEXTFILE:list_length")
      UNSTACK
   end function

   function list_list_length(self) result(res)
    TEXTFILE :: self
   ! Return the size of an INTVECVEC list, by reading the input until an end
   ! bracket "}" token is found. Line breaks are not significant.
      INT :: res
      INT :: line,item
      STR(STR_SIZE) :: word
      STRVEC(STR_SIZE,:), PTR :: v
      STACK("TEXTFILE:list_list_length")
      START_TIMER("TEXTFILE:list_list_length")
      line = line_number_(self)
      item = next_line_item_(self)
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:list_list_length ... list does not begin with {")
      res = 0
      do
         call read_str_(self,word)
         if (word=="}") exit
         call move_to_previous_item_(self)
         call read_strvec_ptr_(self,v)
         res = res+1
      end do
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)
     STOP_TIMER("TEXTFILE:list_list_length")
      UNSTACK
   end function

   subroutine read_binvec(self,v)
    TEXTFILE :: self
   ! Read in a BIN vector sequentially. Line breaks are not significant.
      BINVEC(:) :: v
      INT :: dim,i
      STACK("TEXTFILE:read_binvec")
      START_TIMER("TEXTFILE:read_binvec")
      dim = size(v)
      do i = 1,dim
         call read_bin_(self,v(i))
      end do
     STOP_TIMER("TEXTFILE:read_binvec")
      CHECK
   end subroutine

   subroutine read_binvec_ptr(self,v,ignore_opening_brace)
    TEXTFILE :: self
   ! Read in a BIN vector pointer "v" sequentially. Line breaks are not
   ! significant. If "ignore_opening_brace" is present, the opening curly brace
   ! "{" is not required; however, the closing curly brace "}" is always
   ! required. The "v" vector pointer is created to be the right size.
      BIN, optional :: ignore_opening_brace
      BINVEC(:), PTR :: v
      INT :: dim
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:read_binvec_ptr")
      START_TIMER("TEXTFILE:read_binvec_ptr")
      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (NOT present(ignore_opening_brace)) then
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_binvec_ptr ... list does not begin with a {")
      end if
      call read_binvec_(self,v)
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_binvec_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_binvec_ptr")
      UNSTACK
   end subroutine

   subroutine read_strvec(self,v)
    TEXTFILE :: self
   ! Read in an string vector sequentially. Line breaks are not significant.
      STRVEC(STR_SIZE,:) :: v
      INT :: dim,i
      STACK("TEXTFILE:read_strvec")
      START_TIMER("TEXTFILE:read_strvec")
      dim = size(v)
      do i = 1,dim
         call read_str_(self,v(i))
      end do
     STOP_TIMER("TEXTFILE:read_strvec")
      CHECK
   end subroutine

   subroutine read_strvec_ptr(self,v,ignore_opening_brace)
    TEXTFILE :: self
   ! Read in a string vector pointer "v" sequentially. Line breaks are not
   ! significant. If "ignore_opening_brace" is present, the opening curly
   ! brace "{" is not required; however, the closing curly brace "}" is always
   ! required. The "v" vector pointer is created to be the right size.
      STRVEC(STR_SIZE,:), PTR :: v
      BIN, optional :: ignore_opening_brace
      INT :: dim
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:read_strvec_ptr")
      START_TIMER("TEXTFILE:read_strvec_ptr")
      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (NOT present(ignore_opening_brace)) then
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_strvec_ptr ... list does not begin with a {")
      end if
      call read_strvec_(self,v)
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_strvec_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_strvec_ptr")
      UNSTACK
   end subroutine

   subroutine read_intvec(self,v)
    TEXTFILE :: self
   ! Read in an integer vector sequentially. Line breaks are not significant.
      INTVEC(:) :: v
      INT :: dim,i
      STACK("TEXTFILE:read_intvec")
      START_TIMER("TEXTFILE:read_intvec")
      dim = size(v)
      do i = 1,dim
         call read_int_(self,v(i))
      end do
     STOP_TIMER("TEXTFILE:read_intvec")
      CHECK
   end subroutine

   subroutine read_intvec_ptr(self,v,ignore_opening_brace)
    TEXTFILE :: self
   ! Read in an integer vector pointer "v" sequentially. Line breaks are not
   ! significant.  If "ignore_opening_brace" is present, the opening curly brace
   ! "{" is not required; however, the closing curly brace "}" is always
   ! required. The "v" vector pointer is created to be the right size.  
   ! NOTE: this will not handle zero length INTVEC* arrays.
      INTVEC(:), PTR :: v
      BIN, optional :: ignore_opening_brace
      INT :: dim,f,l,i
      STR(STR_SIZE) :: first,word,last
      STACK("TEXTFILE:read_intvec_ptr")
      START_TIMER("TEXTFILE:read_intvec_ptr")
      if (NOT present(ignore_opening_brace)) then
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_intvec_ptr ... list does not begin with a {")
      end if
      call read_str_(self,first)      ! Read the first item
      call read_str_(self,word)
      if (word=="...") then ! this is a looping INTVEC ptr
         call read_str_(self,last)
         l = to_int_(last)
         f = to_int_(first)
         dim = l - f + 1
         call create_(v,abs(dim))
         if (dim>0) then; v = (/ (i, i=f,l)    /)
         else           ; v = (/ (i, i=f,l,-1) /)
         end if
      else
         call move_to_previous_item_(self)
         call move_to_previous_item_(self)
         if (NOT present(ignore_opening_brace)) &
         call move_to_previous_item_(self)
         dim = list_length_(self,ignore_opening_brace)
         call create_(v,dim)
         if (NOT present(ignore_opening_brace)) then
         call read_(self,word)
         ENSURE(word=="{","TEXTFILE:read_intvec_ptr ... list does not begin with a {")
         end if
         call read_intvec_(self,v)
      end if
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_intvec_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_intvec_ptr")
      UNSTACK
   end subroutine

   subroutine read_intvecvec(self,v)
    TEXTFILE :: self
   ! Read in an integer vector vector "v" sequentially. Line breaks are not
   ! significant. The "v" INTVECVEC is created to be the right size.
      INTVECVEC(:) :: v
      INT :: dim,i
      STACK("TEXTFILE:read_intvecvec")
      START_TIMER("TEXTFILE:read_intvecvec")
      dim = size(v)
      do i = 1,dim
         call read_intvec_ptr_(self,v(i)%element)
      end do
     STOP_TIMER("TEXTFILE:read_intvecvec")
      UNSTACK
   end subroutine

   subroutine read_intvecpxvec_ptr(self,v)
    TEXTFILE :: self
   ! Read in an integer vector vector pointer "v" sequentially. Line breaks
   ! are not significant. The "v" INTVECVEC* is created to be the right size.
      INTVECVEC(:), PTR :: v
      STR(STR_SIZE) :: word
      INT :: dim
      STACK("TEXTFILE:read_intvecpxvec_ptr")
      START_TIMER("TEXTFILE:read_intvecpxvec_ptr")
      dim = list_list_length_(self)
      call create_(v,dim)
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_intvecpxvec_ptr ... list does not begin with a {")
      call read_intvecvec_(self,v)
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_intvecpxvec_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_intvecpxvec_ptr")
      UNSTACK
   end subroutine

   subroutine read_realvec_quantity(self,v)
    TEXTFILE :: self
   ! Read in a vector sequentially. Line breaks are not significant.
   ! WARNING: You must *not* be at the end of file to use this routine, since
   ! it will try to read the next token after the vector as a units specifier.
   ! (There will usually be at least a close brackets symbol "}" after this
   ! data in TONTO input files).
      REALVEC(:) :: v
      STR(STR_SIZE) :: units
      BIN :: known_unit
      STACK("TEXTFILE:read_realvec_quantity")
      START_TIMER("TEXTFILE:read_realvec_quantity")
      call read_realvec_(self,v)
      if (NOT at_end_of_file_(self)) then
        call read_str_(self,units)                      ! Read possible unit string
        known_unit = is_known_unit_(units)   ! If unknown unit, move back
        if (NOT known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(v,units)              ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(v,self%default_units)     ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(v,self%default_units)     ! Convert from assumed default units
        self%default_units = " "
      end if
     STOP_TIMER("TEXTFILE:read_realvec_quantity")
      CHECK
   end subroutine

   subroutine read_realvec(self,v)
    TEXTFILE :: self
   ! Read in a vector sequentially. Line breaks are not significant.
      REALVEC(:) :: v
      INT :: dim,i
      STACK("TEXTFILE:read_realvec")
      START_TIMER("TEXTFILE:read_realvec")
      dim = size(v)
      do i = 1,dim
         call read_real_(self,v(i))
      end do
     STOP_TIMER("TEXTFILE:read_realvec")
      CHECK
   end subroutine

   subroutine read_realvec_ptr(self,v,ignore_opening_brace)
    TEXTFILE :: self
   ! Read in a vector pointer "v" sequentially. Line breaks are not significant.
   ! If "ignore_opening_brace" is present, the opening curly brace "{" is not
   ! required; however, the closing curly brace "}" is always required. The "v"
   ! vector pointer is created to be the right size.
      BIN, optional :: ignore_opening_brace
      REALVEC(:), PTR :: v
      INT :: dim
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:read_realvec_ptr")
      START_TIMER("TEXTFILE:read_realvec_ptr")
      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (NOT present(ignore_opening_brace)) then
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_realvec_ptr ... list does not begin with a {")
      end if
      call read_realvec_(self,v)
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_realvec_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_realvec_ptr")
      UNSTACK
   end subroutine

   subroutine read_realvec_quantity_ptr(self,v)
    TEXTFILE :: self
   ! Read in a vector quantity pointer "v" sequentially. The units specifier, if
   ! present, must follow the final curly bracket. Line breaks are not
   ! significant. The "v" vector pointer is created to be the right size.
      REALVEC(:), PTR :: v
      INT :: dim
      STR(STR_SIZE) :: units
      STR(STR_SIZE) :: word
      BIN :: known_unit
      STACK("TEXTFILE:read_realvec_quantity_ptr")
      START_TIMER("TEXTFILE:read_realvec_quantity_ptr")
      dim = list_length_(self)
      call create_(v,dim)
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_realvec_quantity_ptr ... list does not begin with a {")
      call read_realvec_(self,v)
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_realvec_quantity_ptr ... list does not end with a {")
      ! Read units part
      if (NOT at_end_of_file_(self)) then
        call read_str_(self,units)                      ! Read possible unit string
        known_unit = is_known_unit_(units)   ! If unknown unit, move back
        if (NOT known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(v,units)              ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(v,self%default_units)     ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(v,self%default_units)     ! Convert from assumed default units
        self%default_units = " "
      end if
     STOP_TIMER("TEXTFILE:read_realvec_quantity_ptr")
      UNSTACK
   end subroutine

   subroutine read_realvec_pair_quantities(self,v1,v2)
    TEXTFILE :: self
   ! Read in a pair of vectors sequentially, alternating. Line breaks are
   ! not significant.
      REALVEC(:) :: v1,v2
      INT :: dim,i
      STACK("TEXTFILE:read_realvec_pair_quantities")
      START_TIMER("TEXTFILE:read_realvec_pair_quantities")
      ENSURE(size(v1)==size(v2),"TEXTFILE:read_realvec_pair_quantities ... incompatible vectors")
      dim = size(v1)
      do i = 1,dim
         call read_(self,v1(i))
         call read_(self,v2(i))
      end do
     STOP_TIMER("TEXTFILE:read_realvec_pair_quantities")
      CHECK
   end subroutine

   subroutine read_cpxvec(self,v)
    TEXTFILE :: self
   ! Read in a complex vector sequentially. Line breaks are not significant.
      CPXVEC(:) :: v
      INT :: dim,i
      STACK("TEXTFILE:read_cpxvec")
      START_TIMER("TEXTFILE:read_cpxvec")
      dim = size(v)
      do i = 1,dim
         call read_cpx_(self,v(i))
      end do
     STOP_TIMER("TEXTFILE:read_cpxvec")
      CHECK
   end subroutine

   subroutine read_cpxvec_ptr(self,v,ignore_opening_brace)
    TEXTFILE :: self
   ! Read in a complex vector pointer "v" sequentially. Line breaks are not
   ! significant. If "ignore_opening_brace" is present, the opening curly
   ! brace "{" is not required; however, the closing curly brace "}" is always
   ! required. The "v" vector pointer is created to be the right size.
      CPXVEC(:), PTR :: v
      BIN, optional :: ignore_opening_brace
      INT :: dim
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:read_cpxvec_ptr")
      START_TIMER("TEXTFILE:read_cpxvec_ptr")
      dim = list_length_(self,ignore_opening_brace)
      call create_(v,dim)
      if (NOT present(ignore_opening_brace)) then
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_cpxvec_ptr ... list does not begin with a {")
      end if
      call read_cpxvec_(self,v)
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_cpxvec_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_cpxvec_ptr")
      UNSTACK
   end subroutine

   subroutine read_realmat_quantity(self,m,order)
    TEXTFILE :: self
   ! Read in a matrix sequentially by row (default) or by column. If a units
   ! string appears after the matrix, the elements are converted into atomic
   ! units or generic units. Line breaks are not significant.
   ! WARNING: You must *not* be at the end of file to use this routine, since
   ! it will try to read the next token after the matrix as a units specifier.
   ! (There will usually be at least a close brackets symbol "}" after this
   ! data in TONTO input files).
      REALMAT(:,:) :: m
      STR(*), optional :: order
      STR(STR_SIZE) :: units
      BIN :: known_unit
      STACK("TEXTFILE:read_realmat_quantity")
      START_TIMER("TEXTFILE:read_realmat_quantity")
      call read_realmat_(self,m,order)
      if (NOT at_end_of_file_(self)) then
        call read_str_(self,units)                      ! Read possible unit string
        known_unit = is_known_unit_(units)   ! If unknown unit, move back
        if (NOT known_unit) call move_to_previous_item_(self)
        if (known_unit) then
           call convert_from_(m,units)              ! Convert from known units
        else if (self%default_units/=" ") then
           call convert_from_(m,self%default_units)     ! Convert from assumed default units
           self%default_units = " "
        end if
      else if (self%default_units/=" ") then
        call convert_from_(m,self%default_units)     ! Convert from assumed default units
        self%default_units = " "
      end if
     STOP_TIMER("TEXTFILE:read_realmat_quantity")
      CHECK
   end subroutine

   subroutine read_intmat(self,m,order)
    TEXTFILE :: self
   ! Read in an integer complex matrix sequentially by row (default) or by
   ! column.  Line breaks are not significant.
      INTMAT(:,:) :: m
      STR(*), optional :: order
      INT :: dim1,dim2,i,j
      STR(STR_SIZE) :: print_order
      STACK("TEXTFILE:read_intmat")
      START_TIMER("TEXTFILE:read_intmat")
      dim1 = size(m,1)
      dim2 = size(m,2)
      print_order = "by_row"
      if (present(order)) print_order = order
      select case (print_order)
         case ("by_column   ","column_major")
            do j = 1,dim2
            do i = 1,dim1
              call read_int_(self,m(i,j))
            end do
            end do
         case ("by_row      ","row_major   ")
            do i = 1,dim1
            do j = 1,dim2
              call read_int_(self,m(i,j))
            end do
            end do
         case default
            allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "by_column   "
            tonto%known_keywords(2) = "column_major"
            tonto%known_keywords(3) = "by_row      "
            tonto%known_keywords(4) = "row_major   "
            call unknown_(tonto,print_order,"TEXTFILE:read_intmat")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:read_intmat")
      CHECK
   end subroutine

   subroutine read_intmat_ptr(self,v,n_rows,n_cols,order)
    TEXTFILE :: self
   ! Read in an integer matrix pointer "v" sequentially. Line breaks are not
   ! significant.  The "m" matrix pointer is created to be the right size.
   ! You must specify n_rows or n_cols in the calling routine, but not both.
      INTMAT(:,:), PTR :: v
      INT, IN, optional :: n_rows,n_cols
      STR(*), optional :: order
      INT :: dim,len
      STR(STR_SIZE) :: word
      STACK("TEXTFILE:read_intmat_ptr")
      START_TIMER("TEXTFILE:read_intmat_ptr")
      len = list_length_(self)
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_intmat_ptr ... list does not begin with a {")
      if (present(n_rows)) then
        ENSURE(NOT present(n_cols),"TEXTFILE:read_intmat_ptr ... subroutine incorrectly called.")
        ENSURE(mod(dim,n_rows)==0,"TEXTFILE:read_intmat_ptr ... number of matrix elements entered is incorrect")
        dim = len/n_rows
        call create_(v,n_rows,dim)
        call read_intmat_(self,v,order)
      else if (present(n_cols)) then
        ENSURE(mod(dim,n_cols)==0,"TEXTFILE:read_intmat_ptr ... number of matrix elements entered is incorrect")
        dim = len/n_cols
        call create_(v,dim,n_cols)
        call read_intmat_(self,v,order)
      else
        DIE("TEXTFILE:read_intmat_ptr ... subroutine incorrectly called.")
        dim = len ! if DIE macro not set, fall back to n_cols=1
        call create_(v,dim,1)
        call read_intmat_(self,v,order)
      end if
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_intmat_ptr ... list does not end with a {")
     STOP_TIMER("TEXTFILE:read_intmat_ptr")
      UNSTACK
   end subroutine

   subroutine read_realmat(self,m,order)
    TEXTFILE :: self
   ! Read in a complex matrix sequentially by row (default) or by column.
   ! Line breaks are not significant.
      REALMAT(:,:) :: m
      STR(*), optional :: order
      INT :: dim1,dim2,i,j
      STR(STR_SIZE) :: print_order
      STACK("TEXTFILE:read_realmat")
      START_TIMER("TEXTFILE:read_realmat")
      dim1 = size(m,1)
      dim2 = size(m,2)
      print_order = "by_row"
      if (present(order)) print_order = order
      select case (print_order)
         case ("by_column   ","column_major")
            do j = 1,dim2
            do i = 1,dim1
              call read_real_(self,m(i,j))
            end do
            end do
         case ("by_row      ","row_major   ")
            do i = 1,dim1
            do j = 1,dim2
              call read_real_(self,m(i,j))
            end do
            end do
         case default
            allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "by_column   "
            tonto%known_keywords(2) = "column_major"
            tonto%known_keywords(3) = "by_row      "
            tonto%known_keywords(4) = "row_major   "
            call unknown_(tonto,print_order,"TEXTFILE:read_realmat")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:read_realmat")
      CHECK
   end subroutine

   subroutine read_realmat_ptr(self,mat)
    TEXTFILE :: self
   ! Read in a matrix pointer "mat" sequentially. Line breaks are significant.
       REALMAT(:,:), PTR :: mat
       STR(STR_SIZE) :: word
       INTVEC(2) :: dim
       STACK("TEXTFILE:read_realmat_ptr")
       START_TIMER("TEXTFILE:read_realmat_ptr")
       call number_lines_col_(self,dim)
       call read_(self,word)
       ENSURE(word=="{","TEXTFILE:read_realmat_ptr ... matrix does not begin with a {")
       call create_(mat,dim(1),dim(2))
       call read_realmat_(self,mat)
       call read_(self,word)
       ENSURE(word=="}","TEXTFILE:read_realmat_ptr ... expecting close bracket symbol: }")
     STOP_TIMER("TEXTFILE:read_realmat_ptr")
      CHECK
   end subroutine

   subroutine number_lines_col(self,dim)
    TEXTFILE :: self
   ! Returns the number of lines which are not blank, and the number of items
   ! per line between an opening and closing brace. It is an error if there are
   ! a different number of items per line on every non-blank line.
      INTVEC(2) :: dim
      INT :: line,item,first_line
      STR(STR_SIZE) :: word
      INT :: dim1,dim2
      STACK("TEXTFILE:number_lines_col")
      START_TIMER("TEXTFILE:number_lines_col")
      item = next_line_item_(self)
      line = line_number_(self)
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:number_lines_col ... list does not begin with a {")
      dim1 = 0
      dim2 = 0
      call read_(self,word)                           ! first item of 1st line
      if (word == "}") then; STOP_TIMER("TEXTFILE:number_lines_col") CHECK return; end if
      first_line = line_number_(self) 
      dim1 = 1                              ! There is at least one row
      dim2 = n_line_items_(self)                  ! The number of columns
      dim(2) = dim2                         ! This (no. of items per line) shouln't change
      if (first_line==line) dim2 = dim2 - 1 ! Opening { is on ths same line as first item
      do
         call skip_line_(self)            
         if (n_line_items_(self)==0) cycle
         call read_(self,word)                        ! first item of next non-blank line
         if (word == "}") exit              ! End of matrix
         dim1 = dim1 + 1                    ! Add another row
         dim2 = n_line_items_(self)
         if (dim2>dim(2)) then
            dim2 = dim(2)
            call move_to_line_item_(self,dim2+1)      ! This item must be a closing }
            ENSURE(next_item_(self)=="}","TEXTFILE:number_lines_col ... different number of line items on succesive lines")
            exit
         else
            ENSURE(dim2==dim(2),"TEXTFILE:number_lines_col ... different number of line items on succesive lines")
         end if
      end do
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)
      dim = (/dim1,dim2/)
     STOP_TIMER("TEXTFILE:number_lines_col")
      CHECK
   end subroutine

   subroutine read_realmat3(self,mx)
    TEXTFILE :: self
   ! Read a REALMAT3 from the input buffer flat style
     REALMAT3(:,:,:) :: mx
     INT :: a,b,c,aub,bub,cub,tmp
     STACK("TEXTFILE:read_realmat3")
     START_TIMER("TEXTFILE:read_realmat3")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     WARN_IF(aub==0,"TEXTFILE:read_realmat3 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:read_realmat3 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:read_realmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_real_(self,mx(a,b,c))
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:read_realmat3")
      CHECK
   end subroutine

   subroutine read_realmat4(self,mx)
    TEXTFILE :: self
   ! Read a REALMAT4 from the input buffer flat style
     REALMAT4(:,:,:,:) :: mx
     INT :: a,b,c,d,aub,bub,cub,dub,tmp
     STACK("TEXTFILE:read_realmat4")
     START_TIMER("TEXTFILE:read_realmat4")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,3)
     WARN_IF(aub==0,"TEXTFILE:read_realmat4 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:read_realmat4 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:read_realmat4 ... zero sized 3rd dimension!")
     WARN_IF(dub==0,"TEXTFILE:read_realmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_real_(self,mx(a,b,c,d))
           end do
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:read_realmat4")
      CHECK
   end subroutine

   subroutine read_formatted_mat(self,m,format,items_per_line)
    TEXTFILE :: self
   ! Read a formatted matrix "m" with fortran "format" and a given
   ! number of "items_per_line".
      REALMAT(:,:), OUT :: m
     STR(*), IN :: format
     INT, IN :: items_per_line
     INT :: i,j,d,count
     STACK("TEXTFILE:read_formatted_mat")
     START_TIMER("TEXTFILE:read_formatted_mat")
     call move_to_item_(self%buffer,1)
     d = size(m,1)
     count=0
     do i=1,d
       do j=1,d
         call read_formatted_real_(self,m(i,j),format)
         count = count + 1
          if (count==items_per_line) then
            do
               call read_line_(self)
               if (NOT empty_(self%buffer)) exit
            end do
            count=0
          end if
       end do
     end do
     STOP_TIMER("TEXTFILE:read_formatted_mat")
      CHECK
   end subroutine

   subroutine read_cadpac_mat(self,m)
    TEXTFILE :: self
   ! Read in a matrix produced by the constraint part of cadpac.
     IN :: self
     REALMAT(:,:), OUT :: m
     STACK("TEXTFILE:read_cadpac_mat")
     START_TIMER("TEXTFILE:read_cadpac_mat")
     call read_formatted_mat_(self,m,"5D16.8",5)
     STOP_TIMER("TEXTFILE:read_cadpac_mat")
      CHECK
   end subroutine

   subroutine read_cpxmat(self,m,order)
    TEXTFILE :: self
   ! Read in a complex matrix sequentially by row (default) or by column.
   ! Line breaks are not significant.
      CPXMAT(:,:) :: m
      STR(*), optional :: order
      INT :: dim1,dim2,i,j
      STR(STR_SIZE) :: print_order
      STACK("TEXTFILE:read_cpxmat")
      START_TIMER("TEXTFILE:read_cpxmat")
      dim1 = size(m,1)
      dim2 = size(m,2)
      print_order = "by_row"
      if (present(order)) print_order = order
      select case (print_order)
         case ("by_column   ","column_major")
            do j = 1,dim2
            do i = 1,dim1
              call read_cpx_(self,m(i,j))
            end do
            end do
         case ("by_row      ","row_major   ")
            do i = 1,dim1
            do j = 1,dim2
              call read_cpx_(self,m(i,j))
            end do
            end do
         case default
            allocate(tonto%known_keywords(4))
            tonto%known_keywords(1) = "by_column   "
            tonto%known_keywords(2) = "column_major"
            tonto%known_keywords(3) = "by_row      "
            tonto%known_keywords(4) = "row_major   "
            call unknown_(tonto,print_order,"TEXTFILE:read_cpxmat")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:read_cpxmat")
      CHECK
   end subroutine

   subroutine read_cpxmat3(self,mx)
    TEXTFILE :: self
   ! Read a CPXMAT3 from the input buffer flat style
     CPXMAT3(:,:,:) :: mx
     INT :: a,b,c,aub,bub,cub,tmp
     STACK("TEXTFILE:read_cpxmat3")
     START_TIMER("TEXTFILE:read_cpxmat3")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     WARN_IF(aub==0,"TEXTFILE:read_cpxmat3 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:read_cpxmat3 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:read_cpxmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_int_(self,tmp)
           call read_cpx_(self,mx(a,b,c))
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:read_cpxmat3")
      CHECK
   end subroutine

   subroutine read_cpxmat4(self,mx)
    TEXTFILE :: self
   ! Read a CPXMAT4 from the input buffer flat style
     CPXMAT4(:,:,:,:) :: mx
     INT :: a,b,c,d,aub,bub,cub,dub,tmp
     STACK("TEXTFILE:read_cpxmat4")
     START_TIMER("TEXTFILE:read_cpxmat4")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     WARN_IF(aub==0,"TEXTFILE:read_cpxmat4 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:read_cpxmat4 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:read_cpxmat4 ... zero sized 3rd dimension!")
     WARN_IF(dub==0,"TEXTFILE:read_cpxmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_int_(self,tmp)
             call read_cpx_(self,mx(a,b,c,d))
           end do
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:read_cpxmat4")
      CHECK
   end subroutine

   subroutine read_cpxmat5(self,mx)
    TEXTFILE :: self
   ! Read a CPXMAT5 from the input buffer flat style
     CPXMAT5(:,:,:,:,:) :: mx
     INT :: a,b,c,d,e,aub,bub,cub,dub,eub,tmp
     STACK("TEXTFILE:read_cpxmat5")
     START_TIMER("TEXTFILE:read_cpxmat5")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     eub=size(mx,5)
     WARN_IF(aub==0,"TEXTFILE:read_cpxmat5 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:read_cpxmat5 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:read_cpxmat5 ... zero sized 3rd dimension!")
     WARN_IF(dub==0,"TEXTFILE:read_cpxmat5 ... zero sized 4th dimension!")
     WARN_IF(eub==0,"TEXTFILE:read_cpxmat5 ... zero sized 5th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             do e=1,eub
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_int_(self,tmp)
               call read_cpx_(self,mx(a,b,c,d,e))
             end do
           end do
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:read_cpxmat5")
      CHECK
   end subroutine

   function next_item(self) result(word)
    TEXTFILE :: self
   ! Read a str from the input file and return it but
   ! *do not advance the cursor*. Use next_str for that.
     STR(STR_SIZE) :: word
     STACK("TEXTFILE:next_item")
     START_TIMER("TEXTFILE:next_item")
     call read_str_(self,word)
     call move_to_previous_item_(self)
     STOP_TIMER("TEXTFILE:next_item")
      UNSTACK
   end function

   function next_str(self) result(word)
    TEXTFILE :: self
   ! Read a str from the input file and return it and advance the cursor
     STR(STR_SIZE) :: word
     STACK("TEXTFILE:next_str")
     START_TIMER("TEXTFILE:next_str")
     call read_str_(self,word)
     STOP_TIMER("TEXTFILE:next_str")
      UNSTACK
   end function

   function next_formatted_REAL(self,format) result(value)
    TEXTFILE :: self
   ! Read a formatted REAL from the input file and return it.
   ! Does not check the end of line!
     REAL :: value
     STR(*), IN :: format
     STACK("TEXTFILE:next_formatted_REAL")
     START_TIMER("TEXTFILE:next_formatted_REAL")
     call read_formatted_real_(self,value,format)
     STOP_TIMER("TEXTFILE:next_formatted_REAL")
      CHECK
   end function

!  ********************
!  Data output routines
!  ********************

   subroutine put_margin(self)
    TEXTFILE :: self
   ! Put a margin into the buffer of the output object
      STACK("TEXTFILE:put_margin")
      START_TIMER("TEXTFILE:put_margin")
      call put_(self%buffer,repeat(" ",self%margin_width))
     STOP_TIMER("TEXTFILE:put_margin")
      CHECK
   end subroutine

   subroutine flush(self)
    TEXTFILE :: self
   ! Flush the buffer to the output file
      UNITNUMBER :: unit
      STACK("TEXTFILE:flush")
      START_TIMER("TEXTFILE:flush")
      ENSURE(is_open_(self),"TEXTFILE:flush ... file is not open!")
      ENSURE(self%action=="write","TEXTFILE:flush ... file does not have write action!")
      if (do_io_(tonto_parallel)) then
        write(unit=self%unit,iostat=self%io_status,fmt='(a)') trim(self%buffer%string)
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      unit%unit = self%unit
      call flush_buffer_(unit)
      call clear_(self%buffer)
      call put_margin_(self)
      self%record = self%record + 1
     STOP_TIMER("TEXTFILE:flush")
      CHECK
   end subroutine

   subroutine flush_1(self,times)
    TEXTFILE :: self
   ! Flush the buffer multiple times to get extra carriage returns
     INT :: times
     INT :: i
     STACK("TEXTFILE:flush_1")
     START_TIMER("TEXTFILE:flush_1")
     do i=1,times
        call flush_(self)
     end do
     STOP_TIMER("TEXTFILE:flush_1")
      CHECK
   end subroutine

   subroutine tab(self,real_fields,int_fields,bin_fields,width)
    TEXTFILE :: self
   ! Tab across the specified number of fields in the output buffer
   ! "real_fields", "int_fields" and "bin_fields" refer to the number of
   ! real, integer and logical fields to tab; "width" is the width of spaces
   ! to tab.
      INT, optional :: real_fields,int_fields,bin_fields,width
      STACK("TEXTFILE:tab")
      START_TIMER("TEXTFILE:tab")
      if (present(real_fields)) &
         call put_(self%buffer,repeat(repeat(" ",self%real_width),real_fields))
      if (present(int_fields)) &
         call put_(self%buffer,repeat(repeat(" ",self%int_width),int_fields))
      if (present(bin_fields)) &
         call put_(self%buffer,repeat(repeat(" ",self%int_width),bin_fields))
      if (present(width)) &
         call put_(self%buffer,repeat(" ",width))
     STOP_TIMER("TEXTFILE:tab")
      CHECK
   end subroutine

   subroutine dash(self,real_fields,int_fields,bin_fields,width)
    TEXTFILE :: self
   ! Put a dashed line into the buffer. "real_fields", "int_fields" and
   ! "bin_fields" refer to the number of real, integer and logical fields
   ! to draw the line over; "width" is the width of characters to draw the
   ! dashed line.
      INT, optional :: real_fields,int_fields,bin_fields,width
      STACK("TEXTFILE:dash")
      START_TIMER("TEXTFILE:dash")
      call put_dash_(self,real_fields,int_fields,bin_fields,width,flush=1)
     STOP_TIMER("TEXTFILE:dash")
      CHECK
   end subroutine

   subroutine put_dash(self,real_fields,int_fields,bin_fields,width,flush)
    TEXTFILE :: self
   ! Put a dashed line into the buffer. "real_fields", "int_fields" and
   ! "bin_fields" refer to the number of real, integer and logical fields
   ! to draw the line over; "width" is the width of characters to draw the
   ! dashed line.
      INT, optional :: real_fields,int_fields,bin_fields,width,flush
      STACK("TEXTFILE:put_dash")
      START_TIMER("TEXTFILE:put_dash")
      if (present(real_fields)) &
         call put_(self%buffer,repeat(repeat("-",self%real_width),real_fields))
      if (present(int_fields)) &
         call put_(self%buffer,repeat(repeat("-",self%int_width),int_fields))
      if (present(bin_fields)) &
         call put_(self%buffer,repeat(repeat("-",self%int_width),bin_fields))
      if (present(width)) &
         call put_(self%buffer,repeat("-",width))
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_dash")
      CHECK
   end subroutine

   subroutine show_str(self,pretext,string,int_width)
    TEXTFILE :: self
   ! Put a formatted STR into the output buffer with descriptive "pretext".
      STR(*) :: pretext
      STR(*) :: string
      BIN, optional :: int_width
      STACK("TEXTFILE:show_str")
      START_TIMER("TEXTFILE:show_str")
      call put_text_(self,pretext)
      if (includes_(string," ")) then
        call put_str_(self,'"'//trim(string)//'"',int_width,flush=1)
      else
        call put_str_(self,string,int_width,flush=1)
      end if
     STOP_TIMER("TEXTFILE:show_str")
      CHECK
   end subroutine

   subroutine show_int(self,pretext,value,real_width)
    TEXTFILE :: self
   ! Put a formatted INT into the output buffer with descriptive "pretext".  If
   ! present, use "real_width" for the width of the field, instead of int_wdith.
      STR(*) :: pretext
      BIN, optional :: real_width
      INT :: value
      STACK("TEXTFILE:show_int")
      START_TIMER("TEXTFILE:show_int")
      call put_text_(self,pretext)
      call put_int_(self,value,real_width,flush=1)
     STOP_TIMER("TEXTFILE:show_int")
      CHECK
   end subroutine

   subroutine show_int_3(self,pretext,val1,val2,val3,real_width)
    TEXTFILE :: self
   ! Put three INT's "val1", "val2" and "val3" into the output buffer, with
   ! descriptive "pretext" before. If present, use "real_width" for all of them.
      STR(*) :: pretext
      INT :: val1,val2,val3
      BIN, optional :: real_width
      STACK("TEXTFILE:show_int_3")
      START_TIMER("TEXTFILE:show_int_3")
      call put_text_(self,pretext)
      call put_int_(self,val1,real_width)
      call put_int_(self,val2,real_width)
      call put_int_(self,val3,real_width,flush=1)
     STOP_TIMER("TEXTFILE:show_int_3")
      CHECK
   end subroutine

   subroutine show_bit_string(self,pretext,value,int_width,width)
    TEXTFILE :: self
   ! Put a formatted INT as a binary number into the output buffer,
   ! with descriptive "pretext".
      STR(*) :: pretext
      BIN, optional :: int_width
      INT, optional :: width
      INT :: value
      STACK("TEXTFILE:show_bit_string")
      START_TIMER("TEXTFILE:show_bit_string")
      call put_text_(self,pretext)
      call put_bit_string_(self,value,int_width,width,flush=1)
     STOP_TIMER("TEXTFILE:show_bit_string")
      CHECK
   end subroutine

   subroutine show_bin(self,pretext,value,real_width)
    TEXTFILE :: self
   ! Put a formatted BIN into the output buffer with descriptive "pretext".
      STR(*) :: pretext
      BIN, optional :: real_width
      BIN :: value
      STACK("TEXTFILE:show_bin")
      START_TIMER("TEXTFILE:show_bin")
      call put_text_(self,pretext)
      call put_bin_(self,value,real_width,flush=1)
     STOP_TIMER("TEXTFILE:show_bin")
      CHECK
   end subroutine

   subroutine show_real(self,pretext,value)
    TEXTFILE :: self
   ! Put a formatted REAL into the output buffer with descriptive "pretext".
      STR(*) :: pretext
      REAL :: value
      STACK("TEXTFILE:show_real")
      START_TIMER("TEXTFILE:show_real")
      call put_text_(self,pretext)
      call put_real_(self,value,flush=1)
     STOP_TIMER("TEXTFILE:show_real")
      CHECK
   end subroutine

   subroutine show_real_3(self,pretext,val1,val2,val3)
    TEXTFILE :: self
   ! Put three REAL's "val1", "val2" and "val3" into the output buffer, with
   ! descriptive "pretext" before.
      STR(*) :: pretext
      REAL :: val1,val2,val3
      STACK("TEXTFILE:show_real_3")
      START_TIMER("TEXTFILE:show_real_3")
      call put_text_(self,pretext)
      call put_real_(self,val1)
      call put_real_(self,val2)
      call put_real_(self,val3,flush=1)
     STOP_TIMER("TEXTFILE:show_real_3")
      CHECK
   end subroutine

   subroutine show_binvec(self,pretext,value)
    TEXTFILE :: self
   ! Put a formatted BINVEC "value" into the output buffer with descriptive
   ! "pretext".
      STR(*) :: pretext
      BINVEC(:) :: value
      BIN :: use_labels
      STACK("TEXTFILE:show_binvec")
      START_TIMER("TEXTFILE:show_binvec")
      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,FALSE)
      call put_binvec_(self,value)
      call set_use_labels_(self,use_labels)
   !  .flush
     STOP_TIMER("TEXTFILE:show_binvec")
      CHECK
   end subroutine

   subroutine show_intvec(self,pretext,value)
    TEXTFILE :: self
   ! Put a formatted INTVEC "value" into the output buffer with descriptive
   ! "pretext".
      STR(*) :: pretext
      INTVEC(:) :: value
      BIN :: use_labels
      STACK("TEXTFILE:show_intvec")
      START_TIMER("TEXTFILE:show_intvec")
      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,FALSE)
      call put_intvec_(self,value)
      call set_use_labels_(self,use_labels)
   !  .flush
     STOP_TIMER("TEXTFILE:show_intvec")
      CHECK
   end subroutine

   subroutine show_realvec(self,pretext,value)
    TEXTFILE :: self
   ! Put a formatted REAL "value" into the output buffer with descriptive
   ! "pretext".
      STR(*) :: pretext
      REALVEC(:) :: value
      BIN :: use_labels
      STACK("TEXTFILE:show_realvec")
      START_TIMER("TEXTFILE:show_realvec")
      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,FALSE)
      call put_realvec_(self,value)
      call set_use_labels_(self,use_labels)
   !  .flush
     STOP_TIMER("TEXTFILE:show_realvec")
      CHECK
   end subroutine

   subroutine show_strvec(self,pretext,value)
    TEXTFILE :: self
   ! Put a formatted REAL "value" into the output buffer with descriptive
   ! "pretext".
      STR(*) :: pretext
      STRVEC(STR_SIZE,:) :: value
      BIN :: use_labels
      STACK("TEXTFILE:show_strvec")
      START_TIMER("TEXTFILE:show_strvec")
      call put_text_(self,pretext)
      use_labels = self%use_labels
      call set_use_labels_(self,FALSE)
      call put_strvec_(self,value)
      call set_use_labels_(self,use_labels)
     STOP_TIMER("TEXTFILE:show_strvec")
      CHECK
   end subroutine

   subroutine text(self,string,real_width,int_width,flush)
    TEXTFILE :: self
   ! Put text into the output buffer as is and flush,
   ! unless an explicit flush is present
      STR(*) :: string
      BIN, optional :: real_width,int_width
      INT, optional :: flush
      STACK("TEXTFILE:text")
      START_TIMER("TEXTFILE:text")
      if (present(flush)) then
         call put_text_(self,string,real_width,int_width,flush)
      else
         call put_text_(self,string,real_width,int_width,flush=1)
      end if
     STOP_TIMER("TEXTFILE:text")
      CHECK
   end subroutine

   subroutine put_text(self,string,real_width,int_width,flush)
    TEXTFILE :: self
   ! Put text into the output buffer as is.
      STR(*) :: string
      BIN, optional :: real_width,int_width
      INT, optional :: flush
      STR(self%int_width) :: int_string
      STR(self%real_width) :: real_string
      STACK("TEXTFILE:put_text")
      START_TIMER("TEXTFILE:put_text")
      if (present(real_width) AND (len(string) <= self%real_width )) then
        real_string = string
        call put_(self%buffer, real_string )
      else if (present(int_width) AND (len(string) <= self%int_width )) then
        int_string = string
        call put_(self%buffer, int_string )
      else
        call put_(self%buffer, string )
      end if
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_text")
      CHECK
   end subroutine

   subroutine put_str(self,string,int_width,width,flush)
    TEXTFILE :: self
   ! Put a formatted STR into the output buffer; if too big, put as is.
   ! if present and TRUE, "int_width" says to use field width of int_width.
   ! if present, "width" is how wide the field should be, instead of real_width.
   ! if present, flush is how many times to flush the buffer.
      STR(*) :: string
      INT, optional :: flush
      BIN, optional :: int_width
      INT, optional :: width
      STR(STR_SIZE) :: form
      INT :: wid
      STACK("TEXTFILE:put_str")
      START_TIMER("TEXTFILE:put_str")
      ENSURE(NOT (present(int_width) AND present(width)),"TEXTFILE:put_str ... too many widths")
      wid = self%real_width
      if (present(int_width)) wid = self%int_width
      if (present(width))     wid = width
      if (len_trim(string)>wid) then
         call put_(self%buffer,trim(string))
      else if (len_trim(string)/=0) then
         form = "a" // trim( to_str_(wid))
         call put_(self%buffer,trim(string),form)
      end if
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_str")
      CHECK
   end subroutine

   subroutine put_unit(self,value,units)
    TEXTFILE :: self
   ! Put a formatted value and its units into the output buffer.
     INT, IN :: value
     STR(STR_SIZE), IN :: units
     STR(STR_SIZE) :: format
     STACK("TEXTFILE:put_unit")
     START_TIMER("TEXTFILE:put_unit")
     format = format_for_int_(self, nice_field_width_for_(self,value) )
     call put_(self%buffer,value,format)
     call put_text_(self, " " //  trim(units))
     if (value /= 1) call put_text_(self,"s")
     STOP_TIMER("TEXTFILE:put_unit")
      CHECK
   end subroutine

   subroutine put_int(self,value,real_width,width,flush)
    TEXTFILE :: self
   ! Put a formatted integer into the output buffer; the field width used is
   ! int_width, usually the length of the nondecimal part of a formatted REAL
   ! number. Can override the width and use real_width if "real_width" is set to
   ! TRUE.  Can override real_width by explicitly setting "width".
      INT :: value
      BIN, optional :: real_width
      INT, optional :: width
      INT, optional :: flush
      STR(STR_SIZE) :: format
      INT :: wide
      STACK("TEXTFILE:put_int")
      START_TIMER("TEXTFILE:put_int")
      ENSURE(NOT (present(real_width) AND present(width)),"TEXTFILE:put_int ... too many widths")
      wide = self%int_width
      if (present(real_width)) wide = self%real_width
      if (present(width))     wide = width
      format = format_for_int_(self,wide)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_int")
      CHECK
   end subroutine

!   put_int_with_zeros(int,width,flush)
!   !
!     int, width :: INT, IN
!     flush :: INT, optional
!     int_width :: INT
!     format :: STR
!     int_width = .nice_field_width_for(int)
!     if (int<0) then
!       .buffer.put("-")
!       format = .format_for_int(int_width-1)
!     else
!       format = .format_for_int(int_width)
!     end
!     .buffer.put(repeat("0",width-int_width))
!     .buffer.put(abs(int),format)
!     if (present(flush)) .flush(flush)
!   end

   subroutine put_bit_string(self,value,int_width,width,flush)
    TEXTFILE :: self
   ! Put a formatted integer as a bit string into the output buffer; the
   ! field width used is real_width. Can override real_width by explicitly
   ! setting "width".
      INT :: value
      BIN, optional :: int_width
      INT, optional :: width
      INT, optional :: flush
      STR(STR_SIZE) :: format
      INT :: wide
      STACK("TEXTFILE:put_bit_string")
      START_TIMER("TEXTFILE:put_bit_string")
      ENSURE(NOT (present(int_width) AND present(width)),"TEXTFILE:put_bit_string ... too many widths")
      wide = self%real_width
      if (present(int_width)) wide = self%int_width
      if (present(width))     wide = width
      format = format_for_bit_string_(self,wide)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_bit_string")
      CHECK
   end subroutine

   subroutine put_bin(self,value,real_width,flush)
    TEXTFILE :: self
   ! Put a formatted logical into the output buffer; can specify the width of
   ! the field if desired. Otherwise, the field with used is int_width, or
   ! the length of the nondecimal part of a formatted REAL number
      BIN :: value
      INT, optional :: flush
      BIN, optional :: real_width
      STR(STR_SIZE) :: format
      STACK("TEXTFILE:put_bin")
      START_TIMER("TEXTFILE:put_bin")
      format = format_for_bin_(self, self%int_width)
      if (present(real_width)) then
        if (real_width) format = format_for_bin_(self, self%real_width)
      end if
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_bin")
      CHECK
   end subroutine

   subroutine put_real(self,value,int_width,width,precision,flush)
    TEXTFILE :: self
   ! Put a formatted REAL into the output buffer. The field with used is
   ! .real_width unless "width" is given which explicitly overrides the default.
   ! Likewise, "precision" can be used to overide the default number of decimal
   ! places given in .real_precision.
      REAL :: value
      BIN, optional :: int_width
      INT, optional :: width, precision, flush
      STR(STR_SIZE) :: format
      INT :: wide,precise
      STACK("TEXTFILE:put_real")
      START_TIMER("TEXTFILE:put_real")
      ENSURE(NOT (present(int_width) AND present(width)),"TEXTFILE:put_real ... too many widths")
      ENSURE(NOT (present(int_width) AND present(precision)),"TEXTFILE:put_real ... int_width overrides precision")
      precise = self%real_precision
      if (present(precision)) precise = precision
      wide = self%real_width
      if (present(int_width)) then
         wide = self%int_width
         precise = 3
      end if
      if (present(width))     wide = width
      format = format_for_real_(self,self%real_style,wide,precise)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_real")
      CHECK
   end subroutine

   subroutine put_cpx(self,value,flush)
    TEXTFILE :: self
   ! Put a formatted CPX into the output buffer
   ! The field with used is int_width, or
   ! the length of the nondecimal part of a formatted REAL number
      CPX :: value
      INT, optional :: flush
      STR(STR_SIZE) :: format
      STACK("TEXTFILE:put_cpx")
      START_TIMER("TEXTFILE:put_cpx")
      format = format_for_real_(self, self%real_style, self%real_width, self%real_precision)
      call put_(self%buffer,value,format)
      if (present(flush)) call flush_(self,flush)
     STOP_TIMER("TEXTFILE:put_cpx")
      CHECK
   end subroutine

   subroutine put_binvec(self,vec,format)
    TEXTFILE :: self
   ! Put a formatted logical vector into the output buffer
      BINVEC(:) :: vec
      STR(*), optional :: format
      STR(STR_SIZE) :: fmt
      STACK("TEXTFILE:put_binvec")
      START_TIMER("TEXTFILE:put_binvec")
      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_binvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_binvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_binvec")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:put_binvec")
      CHECK
   end subroutine

   subroutine put_binvec_by_column(self,vec)
    TEXTFILE :: self
   ! Put a formatted logical vector into the output buffer
      BINVEC(:) :: vec
      INT :: dim,i
      STACK("TEXTFILE:put_binvec_by_column")
      START_TIMER("TEXTFILE:put_binvec_by_column")
      dim = size(vec)
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i)
         end if
         call put_bin_(self,vec(i),flush=1)
      end do
     STOP_TIMER("TEXTFILE:put_binvec_by_column")
      CHECK
   end subroutine

   subroutine put_binvec_by_row(self,vec)
    TEXTFILE :: self
   ! Put a formatted logical vector into the output buffer
      BINVEC(:) :: vec
      INT :: dim,block,nb,f,l,i,fields
      STACK("TEXTFILE:put_binvec_by_row")
      START_TIMER("TEXTFILE:put_binvec_by_row")
      dim = size(vec)
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call put_int_(self,i)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_bin_(self,vec(i))
         end do
         call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_binvec_by_row")
      CHECK
   end subroutine

   subroutine put_intvec(self,vec,format)
    TEXTFILE :: self
   ! Put a formatted integer vector into the output buffer
      INTVEC(:) :: vec
      STR(*), optional :: format
      STR(STR_SIZE) :: fmt
      STACK("TEXTFILE:put_intvec")
      START_TIMER("TEXTFILE:put_intvec")
      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_intvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_intvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_intvec")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:put_intvec")
      CHECK
   end subroutine

   subroutine put_intvec_by_column(self,vec)
    TEXTFILE :: self
   ! Put a formatted integer vector into the output buffer
      INTVEC(:) :: vec
      INT :: dim,i
      STACK("TEXTFILE:put_intvec_by_column")
      START_TIMER("TEXTFILE:put_intvec_by_column")
      dim = size(vec)
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i)
         end if
         call put_int_(self,vec(i),flush=1)
      end do
     STOP_TIMER("TEXTFILE:put_intvec_by_column")
      CHECK
   end subroutine

   subroutine put_intvec_by_row(self,vec)
    TEXTFILE :: self
   ! Put a formatted integer vector into the output buffer
      INTVEC(:) :: vec
      INT :: dim,block,nb,f,l,i,fields
      STACK("TEXTFILE:put_intvec_by_row")
      START_TIMER("TEXTFILE:put_intvec_by_row")
      dim = size(vec)
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call put_int_(self,i)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_int_(self,vec(i))
         end do
         call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_intvec_by_row")
      CHECK
   end subroutine

   subroutine put_strvec(self,vec,format)
    TEXTFILE :: self
   ! Put a formatted REAL vector into the output buffer
      STRVEC(STR_SIZE,:) :: vec
      STR(*), optional :: format
      STR(STR_SIZE) :: fmt
      STACK("TEXTFILE:put_strvec")
      START_TIMER("TEXTFILE:put_strvec")
      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_strvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_strvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_strvec")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:put_strvec")
      CHECK
   end subroutine

   subroutine put_strvec_by_column(self,vec)
    TEXTFILE :: self
   ! Put a formatted integer vector into the output buffer
      STRVEC(STR_SIZE,:) :: vec
      INT :: dim,i
      STACK("TEXTFILE:put_strvec_by_column")
      START_TIMER("TEXTFILE:put_strvec_by_column")
      dim = size(vec)
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i)
         end if
         call put_str_(self,vec(i))
         call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_strvec_by_column")
      CHECK
   end subroutine

   subroutine put_strvec_by_row(self,vec)
    TEXTFILE :: self
   ! Put a formatted string vector into the output buffer
      STRVEC(STR_SIZE,:) :: vec
      INT :: dim,block,nb,f,l,i,fields
      STACK("TEXTFILE:put_strvec_by_row")
      START_TIMER("TEXTFILE:put_strvec_by_row")
      dim = size(vec)
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(real_fields=1)
            do i = f,l
               call put_int_(self,i,real_width=TRUE)
            end do
            call flush_(self,2)
!           .tab(real_fields=1)
         end if
         do i = f,l
            call put_str_(self,vec(i))
         end do
         call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_strvec_by_row")
      CHECK
   end subroutine

   subroutine put_realvec(self,vec,format)
    TEXTFILE :: self
   ! Put a formatted REAL vector into the output buffer
      REALVEC(:) :: vec
      STR(*), optional :: format
      STR(STR_SIZE) :: fmt
      STACK("TEXTFILE:put_realvec")
      START_TIMER("TEXTFILE:put_realvec")
      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_realvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_realvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_realvec")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:put_realvec")
      CHECK
   end subroutine

   subroutine put_realvec_by_column(self,vec)
    TEXTFILE :: self
   ! Put a formatted integer vector into the output buffer
      REALVEC(:) :: vec
      INT :: dim,i
      STACK("TEXTFILE:put_realvec_by_column")
      START_TIMER("TEXTFILE:put_realvec_by_column")
      dim = size(vec)
      WARN_IF(dim==0,"TEXTFILE:put_realvec_by_column ... zero length vector!")
      do i = 1,dim
         if (self%use_labels) then
!           .put_int(i,real_width=TRUE)
            call put_int_(self,i)
         end if
         call put_real_(self,vec(i),flush=1)
      end do
     STOP_TIMER("TEXTFILE:put_realvec_by_column")
      CHECK
   end subroutine

   subroutine put_realvec_by_row(self,vec)
    TEXTFILE :: self
   ! Put a formatted REAL vector into the output buffer
      REALVEC(:) :: vec
      INT :: dim,block,nb,f,l,i,fields
      STACK("TEXTFILE:put_realvec_by_row")
      START_TIMER("TEXTFILE:put_realvec_by_row")
      dim = size(vec)
      WARN_IF(dim==0,"TEXTFILE:put_realvec_by_row ... zero length vector!")
      fields = self%n_fields
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
!         if (block>1) .flush
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call put_int_(self,i,real_width=TRUE)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_real_(self,vec(i))
         end do
         call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_realvec_by_row")
      CHECK
   end subroutine

   subroutine put_cpxvec(self,vec,format)
    TEXTFILE :: self
   ! Put a formatted REAL vector into the output buffer
      CPXVEC(:) :: vec
      STR(*), optional :: format
      STR(STR_SIZE) :: fmt
      STACK("TEXTFILE:put_cpxvec")
      START_TIMER("TEXTFILE:put_cpxvec")
      fmt = "row_wise"
      if (present(format)) fmt = format
      select case (fmt)
         case("by_row     ","row_wise   ","row        ")
            call put_cpxvec_by_row_(self,vec)
         case("by_column  ","column_wise","column     ")
            call put_cpxvec_by_column_(self,vec)
         case default
            allocate(tonto%known_keywords(6))
            tonto%known_keywords(1) = "by_row     "
            tonto%known_keywords(2) = "row_wise   "
            tonto%known_keywords(3) = "row        "
            tonto%known_keywords(4) = "by_column  "
            tonto%known_keywords(5) = "column_wise"
            tonto%known_keywords(6) = "column     "
            call unknown_(tonto,fmt,"TEXTFILE:put_cpxvec")
            deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("TEXTFILE:put_cpxvec")
      CHECK
   end subroutine

   subroutine put_cpxvec_by_column(self,vec)
    TEXTFILE :: self
   ! Put a formatted complex vector into the output buffer
      CPXVEC(:) :: vec
      INT :: dim,i
      STACK("TEXTFILE:put_cpxvec_by_column")
      START_TIMER("TEXTFILE:put_cpxvec_by_column")
      dim = size(vec)
      WARN_IF(dim==0,"TEXTFILE:put_cpxvec_by_column ... zero length vector!")
      do i = 1,dim
         if (self%use_labels) then
            call put_int_(self,i,real_width=TRUE)
         end if
         call put_cpx_(self,vec(i),flush=1)
      end do
     STOP_TIMER("TEXTFILE:put_cpxvec_by_column")
      CHECK
   end subroutine

   subroutine put_cpxvec_by_row(self,vec)
    TEXTFILE :: self
   ! Put a formatted REAL vector into the output buffer
      CPXVEC(:) :: vec
      INT :: dim,block,nb,f,l,i,fields
      STACK("TEXTFILE:put_cpxvec_by_row")
      START_TIMER("TEXTFILE:put_cpxvec_by_row")
      dim = size(vec)
      WARN_IF(dim==0,"TEXTFILE:put_cpxvec_by_row ... zero length vector!")
      fields = self%n_fields/2
      nb = (dim-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         if (self%use_labels) then
!           .tab(int_fields=1)
            do i = f,l
               call tab_(self,real_fields=1)
               call put_int_(self,i,real_width=TRUE)
            end do
            call flush_(self,2)
!           .tab(int_fields=1)
         end if
         do i = f,l
            call put_cpx_(self,vec(i))
         end do
         call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_cpxvec_by_row")
      CHECK
   end subroutine

   subroutine put_intmat(self,mx,order)
    TEXTFILE :: self
   ! Put a formatted integer matrix into the output buffer
   ! If "order" is present use unformatted output suitable for ASCII archive
      INTMAT(:,:) :: mx
      STR(*), optional :: order
      STACK("TEXTFILE:put_intmat")
      START_TIMER("TEXTFILE:put_intmat")
      if (NOT present(order)) then     ! formatted output
         call put_labelled_intmat_(self,mx)
      else                             ! unformatted output
         select case (order)
            case ("by_column  ","column_wise","column     ","transpose  ")
               call put_intmat_by_column_(self,mx)
            case ("by_row     ","row_wise   ","row        ","normal     ")
               call put_intmat_by_row_(self,mx)
            case default
               allocate(tonto%known_keywords(8))
               tonto%known_keywords(1) = "by_column  "
               tonto%known_keywords(2) = "column_wise"
               tonto%known_keywords(3) = "column     "
               tonto%known_keywords(4) = "transpose  "
               tonto%known_keywords(5) = "by_row     "
               tonto%known_keywords(6) = "row_wise   "
               tonto%known_keywords(7) = "row        "
               tonto%known_keywords(8) = "normal     "
               call unknown_(tonto,order,"TEXTFILE:put_intmat")
               deallocate(tonto%known_keywords)
         end select
      end if
     STOP_TIMER("TEXTFILE:put_intmat")
      CHECK
   end subroutine

   subroutine put_labelled_intmat(self,mx)
    TEXTFILE :: self
   ! Put a labeled REAL matrix into the output buffer by column
      INTMAT(:,:) :: mx
      INT :: i,j,block,nb,f,l,dim1,dim2,fields
      STACK("TEXTFILE:put_labelled_intmat")
      START_TIMER("TEXTFILE:put_labelled_intmat")
      WARN_IF(size(mx,1)==0,"TEXTFILE:put_labelled_intmat ... zero sized 1st dimension!")
      WARN_IF(size(mx,2)==0,"TEXTFILE:put_labelled_intmat ... zero sized 2nd dimension!")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      fields = self%n_fields
      nb = (dim2-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim2)
         if (block>1) call flush_(self)
         if (self%use_labels) then
            call tab_(self,int_fields=1)
            do j = f,l
               call put_int_(self,j)
            end do
            call flush_(self,2)
         end if
         do i = 1,dim1
            if (self%use_labels)     call put_int_(self,i)
            if (NOT self%use_labels) call tab_(self,int_fields=1)
            do j = f,l
               call put_int_(self,mx(i,j))
            end do
            call flush_(self)
         end do
      end do
     STOP_TIMER("TEXTFILE:put_labelled_intmat")
      CHECK
   end subroutine

   subroutine put_intmat_by_column(self,mx)
    TEXTFILE :: self
   ! Put a REAL matrix into the output buffer by column, without labels
      INTMAT(:,:) :: mx
      INT :: i,j,dim1,dim2
      STACK("TEXTFILE:put_intmat_by_column")
      START_TIMER("TEXTFILE:put_intmat_by_column")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      WARN_IF(dim1==0,"TEXTFILE:put_intmat_by_column ... zero sized 1st dimension!")
      WARN_IF(dim2==0,"TEXTFILE:put_intmat_by_column ... zero sized 2nd dimension!")
      do j = 1,dim2
      do i = 1,dim1
         call put_int_(self,mx(i,j))
         if(mod(i,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_intmat_by_column")
      CHECK
   end subroutine

   subroutine put_intmat_by_row(self,mx)
    TEXTFILE :: self
   ! Put a REAL matrix into the output buffer by row, without labels
      INTMAT(:,:) :: mx
      INT :: i,j,dim1,dim2
      STACK("TEXTFILE:put_intmat_by_row")
      START_TIMER("TEXTFILE:put_intmat_by_row")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      WARN_IF(dim1==0,"TEXTFILE:put_intmat_by_row ... zero sized 1st dimension!")
      WARN_IF(dim2==0,"TEXTFILE:put_intmat_by_row ... zero sized 2nd dimension!")
      do i = 1,dim1
      do j = 1,dim2
         call put_int_(self,mx(i,j))
         if(mod(j,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_intmat_by_row")
      CHECK
   end subroutine

   subroutine put_realmat(self,mx,order)
    TEXTFILE :: self
   ! Put a formatted REAL matrix into the output buffer
   ! If "order" is present use unformatted output suitable for ASCII archive
      REALMAT(:,:) :: mx
      STR(*), optional :: order
      STACK("TEXTFILE:put_realmat")
      START_TIMER("TEXTFILE:put_realmat")
      if (NOT present(order)) then
         call put_labelled_mat_(self,mx)
      else
         select case (order)
            case("by_column  ","column_wise","column     ","transpose  ")
               call put_realmat_by_column_(self,mx)
            case("by_row     ","row_wise   ","row        ","normal     ")
               call put_realmat_by_row_(self,mx)
            case default
               allocate(tonto%known_keywords(8))
               tonto%known_keywords(1) = "by_column  "
               tonto%known_keywords(2) = "column_wise"
               tonto%known_keywords(3) = "column     "
               tonto%known_keywords(4) = "transpose  "
               tonto%known_keywords(5) = "by_row     "
               tonto%known_keywords(6) = "row_wise   "
               tonto%known_keywords(7) = "row        "
               tonto%known_keywords(8) = "normal     "
               call unknown_(tonto,order,"TEXTFILE:put_realmat")
               deallocate(tonto%known_keywords)
         end select
      end if
     STOP_TIMER("TEXTFILE:put_realmat")
      CHECK
   end subroutine

   subroutine put_labelled_mat(self,mx)
    TEXTFILE :: self
   ! Put a labeled REAL matrix into the output buffer by column
      REALMAT(:,:) :: mx
      INT :: i,j,block,nb,f,l,dim1,dim2,fields
      STACK("TEXTFILE:put_labelled_mat")
      START_TIMER("TEXTFILE:put_labelled_mat")
      WARN_IF(size(mx,1)==0,"TEXTFILE:put_labelled_mat ... zero sized 1st dimension!")
      WARN_IF(size(mx,2)==0,"TEXTFILE:put_labelled_mat ... zero sized 2nd dimension!")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      fields = self%n_fields
      nb = (dim2-0.1)/fields+1
      do block = 1,nb
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim2)
         if (block>1) call flush_(self)
         if (self%use_labels) then
            call tab_(self,int_fields=1)
            do j = f,l
               call put_int_(self,j,real_width=TRUE)
            end do
            call flush_(self,2)
         end if
         do i = 1,dim1
            if (self%use_labels)     call put_int_(self,i)
            if (NOT self%use_labels) call tab_(self,int_fields=1)
            do j = f,l
               call put_real_(self,mx(i,j))
            end do
            call flush_(self)
         end do
      end do
     STOP_TIMER("TEXTFILE:put_labelled_mat")
      CHECK
   end subroutine

   subroutine put_realmat_by_column(self,mx)
    TEXTFILE :: self
   ! Put a REAL matrix into the output buffer by column, without labels
      REALMAT(:,:) :: mx
      INT :: i,j,dim1,dim2
      STACK("TEXTFILE:put_realmat_by_column")
      START_TIMER("TEXTFILE:put_realmat_by_column")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      WARN_IF(dim1==0,"TEXTFILE:put_realmat_by_column ... zero sized 1st dimension!")
      WARN_IF(dim2==0,"TEXTFILE:put_realmat_by_column ... zero sized 2nd dimension!")
      do j = 1,dim2
      do i = 1,dim1
         call put_real_(self,mx(i,j))
         if(mod(i,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_realmat_by_column")
      CHECK
   end subroutine

   subroutine put_realmat_by_row(self,mx)
    TEXTFILE :: self
   ! Put a REAL matrix into the output buffer by row, without labels
      REALMAT(:,:) :: mx
      INT :: i,j,dim1,dim2
      STACK("TEXTFILE:put_realmat_by_row")
      START_TIMER("TEXTFILE:put_realmat_by_row")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      WARN_IF(dim1==0,"TEXTFILE:put_realmat_by_row ... zero sized 1st dimension!")
      WARN_IF(dim2==0,"TEXTFILE:put_realmat_by_row ... zero sized 2nd dimension!")
      do i = 1,dim1
      do j = 1,dim2
         call put_real_(self,mx(i,j))
         if(mod(j,self%n_fields)==0) call flush_(self)
      end do
      call flush_(self)
      end do
     STOP_TIMER("TEXTFILE:put_realmat_by_row")
      CHECK
   end subroutine

   subroutine put_formatted_mat(self,mx,form)
    TEXTFILE :: self
   ! Put out a matrix suitable to be read in by other programs.
   ! Will overwrite the current buffer.
     REALMAT(:,:), IN :: mx
     STR(*), IN :: form
     INT :: columns,first,last,i,j,nbasis,count,old_margin
     STR(STR_SIZE) :: forma,formb
     STACK("TEXTFILE:put_formatted_mat")
     START_TIMER("TEXTFILE:put_formatted_mat")
     i = scan(form,"FfEe")
     call get_next_item_position_(form,first,last)
     DIE_IF( i==0,"TEXTFILE:put_formatted_mat ... incorrect format specification")
     forma=" "
     forma(1:i-first)=form(first:i-1)
     DIE_IF(NOT is_int_(forma),"TEXTFILE:put_formatted_mat ... incorrect format specification")
     columns = to_int_(forma)
     formb=" "
     formb(1:last-i+1) = form(i:last)
     call clear_(self%buffer)
     old_margin = self%margin_width
     self%margin_width = 0
     nbasis=size(mx,1)
     count=0
     do i=1,nbasis
       do j=1,nbasis
         call put_(self%buffer,mx(i,j), trim(formb))
         count=count+1
         if (count==columns) then
           call flush_(self)
           count=0
         end if
       end do
     end do
     if (NOT count == 0) call flush_(self)
     self%margin_width = old_margin
     STOP_TIMER("TEXTFILE:put_formatted_mat")
      CHECK
   end subroutine

   subroutine put_cpxmat(self,mx,order)
    TEXTFILE :: self
   ! Put a formatted CPX matrix into the output buffer
   ! If "order" is present use unformatted output suitable for ASCII archive
      CPXMAT(:,:) :: mx
      STR(*), optional :: order
      INT :: i,j,block,nb,f,l,dim1,dim2,fields
      STACK("TEXTFILE:put_cpxmat")
      START_TIMER("TEXTFILE:put_cpxmat")
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      WARN_IF(dim1==0,"TEXTFILE:put_cpxmat ... zero sized 1st dimension!")
      WARN_IF(dim2==0,"TEXTFILE:put_cpxmat ... zero sized 2nd dimension!")
      fields = self%n_fields/2
      if (NOT present(order)) then
         nb = (dim2-0.1)/fields+1
         do block = 1,nb
            f = 1+fields*(block-1)
            l = min(f+fields-1,dim2)
            if (block>1) call flush_(self)
            if (self%use_labels) then
               call tab_(self,int_fields=1)
               do j = f,l
                  call tab_(self,real_fields=1)
                  call put_int_(self,j,real_width=TRUE)
               end do
               call flush_(self,2)
            end if
            do i = 1,dim1
               if (self%use_labels)     call put_int_(self,i)
               if (NOT self%use_labels) call tab_(self,int_fields=1)
               do j = f,l
                  call put_cpx_(self,mx(i,j))
               end do
               call flush_(self)
            end do
         end do
      else
         select case (order)
            case ("by_column  ","column_wise","transpose  ")
               do j = 1,dim2
               do i = 1,dim1
                  call put_cpx_(self,mx(i,j))
                  if(mod(i,fields)==0) call flush_(self)
               end do
               call flush_(self)
               end do
            case ("by_row     ","row_wise   ","normal     ")
               do i = 1,dim1
               do j = 1,dim2
                  call put_cpx_(self,mx(i,j))
                 if(mod(j,fields)==0) call flush_(self)
               end do
               call flush_(self)
               end do
            case default
               allocate(tonto%known_keywords(6))
               tonto%known_keywords(1) = "by_column  "
               tonto%known_keywords(2) = "column_wise"
               tonto%known_keywords(3) = "transpose  "
               tonto%known_keywords(4) = "by_row     "
               tonto%known_keywords(5) = "row_wise   "
               tonto%known_keywords(6) = "normal     "
               call unknown_(tonto,order,"TEXTFILE:put_cpxmat")
               deallocate(tonto%known_keywords)
         end select
      end if
     STOP_TIMER("TEXTFILE:put_cpxmat")
      CHECK
   end subroutine

   subroutine put_realmat3(self,mx)
    TEXTFILE :: self
   ! Put a REALMAT3 into the output buffer flat style
     REALMAT3(:,:,:) :: mx
     INT :: a,b,c,aub,bub,cub
     STACK("TEXTFILE:put_realmat3")
     START_TIMER("TEXTFILE:put_realmat3")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     WARN_IF(aub==0,"TEXTFILE:put_realmat3 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:put_realmat3 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:put_realmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call put_int_(self,a)
           call put_int_(self,b)
           call put_int_(self,c)
           call put_real_(self,mx(a,b,c),flush=1)
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:put_realmat3")
      CHECK
   end subroutine

   subroutine put_realmat4(self,mx)
    TEXTFILE :: self
   ! Put a REALMAT4 into the output buffer flat style
     REALMAT4(:,:,:,:) :: mx
     INT :: a,b,c,d,aub,bub,cub,dub
     STACK("TEXTFILE:put_realmat4")
     START_TIMER("TEXTFILE:put_realmat4")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     WARN_IF(aub==0,"TEXTFILE:put_realmat4 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:put_realmat4 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:put_realmat4 ... zero sized 3rd dimension!")
     WARN_IF(dub==0,"TEXTFILE:put_realmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call put_int_(self,a)
             call put_int_(self,b)
             call put_int_(self,c)
             call put_int_(self,d)
             call put_real_(self,mx(a,b,c,d),flush=1)
           end do
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:put_realmat4")
      CHECK
   end subroutine

   subroutine put_cpxmat3(self,mx)
    TEXTFILE :: self
   ! Put a REALMAT3 into the output buffer flat style
     CPXMAT3(:,:,:) :: mx
     INT :: a,b,c,aub,bub,cub
     STACK("TEXTFILE:put_cpxmat3")
     START_TIMER("TEXTFILE:put_cpxmat3")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     WARN_IF(aub==0,"TEXTFILE:put_cpxmat3 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:put_cpxmat3 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:put_cpxmat3 ... zero sized 3rd dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           call put_int_(self,a)
           call put_int_(self,b)
           call put_int_(self,c)
           call put_cpx_(self,mx(a,b,c),flush=1)
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:put_cpxmat3")
      CHECK
   end subroutine

   subroutine put_cpxmat4(self,mx)
    TEXTFILE :: self
   ! Put a CPXMAT4 into the output buffer flat style
     CPXMAT4(:,:,:,:) :: mx
     INT :: a,b,c,d,aub,bub,cub,dub
     STACK("TEXTFILE:put_cpxmat4")
     START_TIMER("TEXTFILE:put_cpxmat4")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     WARN_IF(aub==0,"TEXTFILE:put_cpxmat4 ... zero sized 1st dimension!")
     WARN_IF(bub==0,"TEXTFILE:put_cpxmat4 ... zero sized 2nd dimension!")
     WARN_IF(cub==0,"TEXTFILE:put_cpxmat4 ... zero sized 3rd dimension!")
     WARN_IF(dub==0,"TEXTFILE:put_cpxmat4 ... zero sized 4th dimension!")
     do a=1,aub
       do b=1,bub
         do c=1,cub
           do d=1,dub
             call put_int_(self,a)
             call put_int_(self,b)
             call put_int_(self,c)
             call put_int_(self,d)
             call put_cpx_(self,mx(a,b,c,d),flush=1)
           end do
         end do
       end do
     end do
     STOP_TIMER("TEXTFILE:put_cpxmat4")
      CHECK
   end subroutine

   subroutine put_cpxmat5(self,mx)
    TEXTFILE :: self
   ! Put a CPXMAT5 into the output buffer flat style
     CPXMAT5(:,:,:,:,:) :: mx
     INT :: a,b,c,d,e,aub,bub,cub,dub,eub
     STACK("TEXTFILE:put_cpxmat5")
     START_TIMER("TEXTFILE:put_cpxmat5")
     aub=size(mx,1)
     bub=size(mx,2)
     cub=size(mx,3)
     dub=size(mx,4)
     eub=size(mx,5)
     do a=1,aub
     do b=1,bub
     do c=1,cub
     do d=1,dub
     do e=1,eub
        call put_int_(self,a)
        call put_int_(self,b)
        call put_int_(self,c)
        call put_int_(self,d)
        call put_int_(self,e)
        call put_cpx_(self,mx(a,b,c,d,e),flush=1)
     end do
     end do
     end do
     end do
     end do
     STOP_TIMER("TEXTFILE:put_cpxmat5")
      CHECK
   end subroutine

!   put_gaussian(g)
!   ! Output gaussian information
!       g :: GAUSSIAN
!      .flush
!      .show("L quantum number = ", g.l)
!      .show("Position         = ", g.pos(1), g.pos(2), g.pos(3))
!      .show("Exponent         = ", g.ex)
!   end

!   put_gaussian2(g)
!   ! Output gaussian information
!       g :: GAUSSIAN2
!      .flush
!      .text("Shell a:")
!      .show("L quantum number = ", g.a.l)
!      .show("Position         = ", g.a.pos(1), g.a.pos(2), g.a.pos(3))
!      .show("Exponent         = ", g.a.ex)
!      .text("Shell b:")
!      .show("L quantum number = ", g.b.l)
!      .show("Position         = ", g.b.pos(1), g.b.pos(2), g.b.pos(3))
!      .show("Exponent         = ", g.b.ex)
!   end

   subroutine put_intvecvec(self,vec)
    TEXTFILE :: self
   ! Put a integer vector list into the output buffer
      INTVECVEC(:) :: vec
      INT :: i
      BIN :: use_labels
      STACK("TEXTFILE:put_intvecvec")
      START_TIMER("TEXTFILE:put_intvecvec")
      use_labels = self%use_labels
      call set_use_labels_(self,FALSE)
      do i = 1,size(vec)
         call put_int_(self,i)
         call put_text_(self,":")
         call put_intvec_by_row_(self,vec(i)%element)
      end do
      call set_use_labels_(self,use_labels)
     STOP_TIMER("TEXTFILE:put_intvecvec")
      CHECK
   end subroutine

   subroutine put_opvector(self,vec,format)
    TEXTFILE :: self
   ! Outputs the opvector
     OPVECTOR, IN :: vec
     STR(*), optional :: format
     STACK("TEXTFILE:put_opvector")
     START_TIMER("TEXTFILE:put_opvector")
     if (associated( vec%restricted)) then
       call flush_(self)
       call put_text_(self,"restricted part:",flush=2)
       call put_realvec_(self, vec%restricted,format)
     end if
     if (associated( vec%alpha)) then
       call flush_(self)
       call put_text_(self,"alpha part:",flush=2)
       call put_realvec_(self, vec%alpha,format)
     end if
     if (associated( vec%beta)) then
       call flush_(self)
       call put_text_(self,"beta part:",flush=2)
       call put_realvec_(self, vec%beta,format)
     end if
     if (associated( vec%general)) then
       call flush_(self)
       call put_text_(self,"general part:",flush=2)
       call put_realvec_(self, vec%general,format)
     end if
     STOP_TIMER("TEXTFILE:put_opvector")
      CHECK
   end subroutine

   subroutine put_opmatrix(self,mat,order)
    TEXTFILE :: self
   ! Outputs the opmatrix
     OPMATRIX, IN :: mat
     STR(*), optional :: order
     STACK("TEXTFILE:put_opmatrix")
     START_TIMER("TEXTFILE:put_opmatrix")
     if (associated( mat%restricted)) then
       call flush_(self)
       call put_text_(self,"restricted part:",flush=2)
       call put_realmat_(self, mat%restricted,order)
     end if
     if (associated( mat%alpha)) then
       call flush_(self)
       call put_text_(self,"alpha part:",flush=2)
       call put_realmat_(self, mat%alpha,order)
     end if
     if (associated( mat%beta)) then
       call flush_(self)
       call put_text_(self,"beta part:",flush=2)
       call put_realmat_(self, mat%beta,order)
     end if
     if (associated( mat%general)) then
       call flush_(self)
       call put_text_(self,"general part:",flush=2)
       call put_realmat_(self, mat%general,order)
     end if
     if (associated( mat%restricted_complex)) then
       call flush_(self)
       call put_text_(self,"complex restricted part:",flush=2)
       call put_cpxmat_(self, mat%restricted_complex,order)
     end if
     if (associated( mat%alpha_complex)) then
       call flush_(self)
       call put_text_(self,"complex alpha part:",flush=2)
       call put_cpxmat_(self, mat%alpha_complex,order)
     end if
     if (associated( mat%beta_complex)) then
       call flush_(self)
       call put_text_(self,"beta part:",flush=2)
       call put_cpxmat_(self, mat%beta_complex,order)
     end if
     if (associated( mat%general_complex)) then
       call flush_(self)
       call put_text_(self,"complex general part:",flush=2)
       call put_cpxmat_(self, mat%general_complex,order)
     end if
     STOP_TIMER("TEXTFILE:put_opmatrix")
      CHECK
   end subroutine

!  ****************
!  Output self info
!  ****************

   subroutine put_info(self)
    TEXTFILE :: self
   ! Put all the available molecule information on file "out"
      STACK("TEXTFILE:put_info")
      START_TIMER("TEXTFILE:put_info")
      call show_(stdout,"Name       =",self%name)
      call show_(stdout,"I/O action =",self%action)
      call show_(stdout,"Unit       =",self%unit)
      call show_(stdout,"Line       =",self%record)
      call show_(stdout,"Buffer     =",trim(self%buffer%string))
      call show_(stdout,"Cursor pos =",trim(cursor_pointer_(self%buffer)))
     STOP_TIMER("TEXTFILE:put_info")
      CHECK
   end subroutine

!  ***************
!  Set fmt methods
!  ***************

   subroutine set_default_units(self,units)
    TEXTFILE :: self
   ! Set the .default_units to "units". This is reset back to 1
   ! after a particular number has been read and converted.
      STR(*) :: units
      STACK("TEXTFILE:set_default_units")
      START_TIMER("TEXTFILE:set_default_units")
      ENSURE(units==" " OR is_known_unit_(units),"TEXTFILE:set_default_units ... Unknown units!")
      self%default_units = units
     STOP_TIMER("TEXTFILE:set_default_units")
      CHECK
   end subroutine

   subroutine set_comment_chars(self,comment_chars)
    TEXTFILE :: self
   ! Set .comment_chars to "comment_chars".
      STR(*) :: comment_chars
      STACK("TEXTFILE:set_comment_chars")
      START_TIMER("TEXTFILE:set_comment_chars")
      self%comment_chars = comment_chars
    ! .buffer.set(comment_chars=comment_chars)
      self%buffer%comment_chars = comment_chars
     STOP_TIMER("TEXTFILE:set_comment_chars")
      CHECK
   end subroutine

   subroutine set_quote_chars(self,quote_chars)
    TEXTFILE :: self
   ! Set .quote_chars to "quote_chars".
      STR(*) :: quote_chars
      STACK("TEXTFILE:set_quote_chars")
      START_TIMER("TEXTFILE:set_quote_chars")
      self%quote_chars = quote_chars
    ! .buffer.set(quote_chars=quote_chars)
      self%buffer%quote_chars = quote_chars
     STOP_TIMER("TEXTFILE:set_quote_chars")
      CHECK
   end subroutine

   subroutine set_use_labels(self,use_labels)
    TEXTFILE :: self
   ! Set whether to use numbered column or row labels on matrix or vector output
      BIN :: use_labels
      STACK("TEXTFILE:set_use_labels")
      START_TIMER("TEXTFILE:set_use_labels")
      self%use_labels = use_labels
     STOP_TIMER("TEXTFILE:set_use_labels")
      CHECK
   end subroutine

   subroutine set_margin(self,margin_width)
    TEXTFILE :: self
   ! Set the width of the margin in the buffer. Takes effect at the next
   ! output line
      INT :: margin_width
      INT :: item_end
      STACK("TEXTFILE:set_margin")
      START_TIMER("TEXTFILE:set_margin")
      ENSURE(margin_width>=0,"TEXTFILE:set_margin ... margin width less than zero")
      ENSURE(margin_width<=STR_SIZE,"TEXTFILE:set_margin ... margin width too large")
      item_end=self%buffer%item_end ! to shorten the ENSURE line below.
      ENSURE(item_end<=self%margin_width,"TEXTFILE:set_margin ... set margin width only on empty buffers")
      self%margin_width = margin_width
      call clear_(self%buffer)
      call put_margin_(self)
     STOP_TIMER("TEXTFILE:set_margin")
      CHECK
   end subroutine

!  ************************
!  Inherited REALfmt methods
!  ************************

   subroutine set_default_format(self)
    TEXTFILE :: self
   ! Set the default settings for the REAL formatting object
   ! Extra functions added compared to inherited code
      STACK("TEXTFILE:set_default_format")
      START_TIMER("TEXTFILE:set_default_format")
      self%use_labels    = TEXTFILE_USE_LABELS
      self%margin_width  = TEXTFILE_MARGIN_WIDTH
      self%n_fields      = TEXTFILE_N_FIELDS
      self%int_width     = TEXTFILE_INT_WIDTH
      self%real_width     = TEXTFILE_REAL_WIDTH
      self%real_precision = TEXTFILE_REAL_PRECISION
      self%real_style     = TEXTFILE_REAL_STYLE
     STOP_TIMER("TEXTFILE:set_default_format")
      CHECK
   end subroutine

   subroutine set_int_width(self,width)
    TEXTFILE :: self
   ! Set the width of an integer in the format object
      INT :: width
      STACK("TEXTFILE:set_int_width")
      START_TIMER("TEXTFILE:set_int_width")
      ENSURE(width>=0,"TEXTFILE:set_int_width ... width less than zero")
      self%int_width = width
     STOP_TIMER("TEXTFILE:set_int_width")
      CHECK
   end subroutine

   subroutine set_real_width(self,width)
    TEXTFILE :: self
   ! Set the width in the realfmt format object
      INT :: width
      STACK("TEXTFILE:set_real_width")
      START_TIMER("TEXTFILE:set_real_width")
      ENSURE(width>=0,"TEXTFILE:set_real_width ... width less than zero")
      ENSURE(width>=self%real_precision,"TEXTFILE:set_real_width ... width smaller than precision")
      self%real_width = width
     STOP_TIMER("TEXTFILE:set_real_width")
      CHECK
   end subroutine

   subroutine set_n_fields(self,n_fields)
    TEXTFILE :: self
   ! Set the number of fields in the realfmt format object
      INT :: n_fields
      STACK("TEXTFILE:set_n_fields")
      START_TIMER("TEXTFILE:set_n_fields")
      ENSURE(n_fields>=1,"TEXTFILE:set_n_fields ... not enough fields")
      self%n_fields = n_fields
     STOP_TIMER("TEXTFILE:set_n_fields")
      CHECK
   end subroutine

   subroutine set_precision(self,precision)
    TEXTFILE :: self
   ! Set the precision required in the realfmt format object
      INT :: precision
      STACK("TEXTFILE:set_precision")
      START_TIMER("TEXTFILE:set_precision")
      call set_real_precision_(self,precision)
     STOP_TIMER("TEXTFILE:set_precision")
      CHECK
   end subroutine

   subroutine set_real_precision(self,precision)
    TEXTFILE :: self
   ! Set the precision required in the realfmt format object
      INT :: precision
      INT :: w
      STACK("TEXTFILE:set_real_precision")
      START_TIMER("TEXTFILE:set_real_precision")
      ENSURE(precision>=0,"TEXTFILE:set_real_precision ... precision less than zero")
      ENSURE(precision<=self%real_width,"TEXTFILE:set_real_precision ... precision greater than field width")
      w = self%int_width - (self%real_width-self%real_precision-1)
      WARN_IF(w>0,"TEXTFILE:set_real_precision ... width may be too small")
      self%real_precision = precision
     STOP_TIMER("TEXTFILE:set_real_precision")
      CHECK
   end subroutine

   subroutine set_real_style(self,real_style)
    TEXTFILE :: self
   ! Set the fortran format style string in the realfmt format object
      STR(*) :: real_style
      STR(2) :: style
      STACK("TEXTFILE:set_real_style")
      START_TIMER("TEXTFILE:set_real_style")
      style = adjustl(real_style)
      select case (style)
        case ("f","d","e","en","es")
        case default; allocate(tonto%known_keywords(5))
        tonto%known_keywords(1) = "f"
        tonto%known_keywords(2) = "d"
        tonto%known_keywords(3) = "e"
        tonto%known_keywords(4) = "en"
        tonto%known_keywords(5) = "es"
        call unknown_(tonto,style,"TEXTFILE:set_real_style")
        deallocate(tonto%known_keywords)
      end select
      self%real_style = style
     STOP_TIMER("TEXTFILE:set_real_style")
      CHECK
   end subroutine

   function nice_field_width_for(self,num) result(res)
    TEXTFILE :: self
   ! Return the field width that would look nice
     INT :: res
     INT, IN :: num
     REAL :: temp
     STACK("TEXTFILE:nice_field_width_for")
     START_TIMER("TEXTFILE:nice_field_width_for")
     res=0
     if (num<0) res=res+1
     temp=abs(num)
     do
       res=res+1
       temp=temp/10
       if (temp < 1.0d0) exit
     end do
     STOP_TIMER("TEXTFILE:nice_field_width_for")
      CHECK
   end function

   function format_for_real(self,style,width,precision) result(res)
    TEXTFILE :: self
   ! Format for a REAL string
      STR(STR_SIZE) :: res
      STR(STR_SIZE), IN :: style
      INT, IN :: width,precision
      INT :: n
      STACK("TEXTFILE:format_for_real")
      START_TIMER("TEXTFILE:format_for_real")
      res = trim(style) // trim( to_str_(width)) // "." // trim( to_str_(precision))
      if (trim(style)=="e" OR trim(style)=="E") then
        ! Max number of digits permitted for the exponent.
        n = max(ceiling(log10(huge(ONE))),width-precision-5)
        if (width-precision-n>=5) then
          res = trim(res) // "E" // trim(to_str_(n))
        end if
      end if
     STOP_TIMER("TEXTFILE:format_for_real")
      CHECK
   end function

   function format_for_int(self,width) result(res)
    TEXTFILE :: self
   ! Format for a INT string
      STR(STR_SIZE) :: res
      INT, IN :: width
      STACK("TEXTFILE:format_for_int")
      START_TIMER("TEXTFILE:format_for_int")
      res = "i" // trim( to_str_(width))
     STOP_TIMER("TEXTFILE:format_for_int")
      CHECK
   end function

   function format_for_bit_string(self,width) result(res)
    TEXTFILE :: self
   ! Format for a INT string
      STR(STR_SIZE) :: res
      INT, IN :: width
      STACK("TEXTFILE:format_for_bit_string")
      START_TIMER("TEXTFILE:format_for_bit_string")
      res = "b" // trim( to_str_(width))
     STOP_TIMER("TEXTFILE:format_for_bit_string")
      CHECK
   end function

   function format_for_bin(self,width) result(res)
    TEXTFILE :: self
   ! Format for a INT string
      STR(STR_SIZE) :: res
      INT, IN :: width
      STACK("TEXTFILE:format_for_bin")
      START_TIMER("TEXTFILE:format_for_bin")
      res = "l" // trim( to_str_(width))
     STOP_TIMER("TEXTFILE:format_for_bin")
      CHECK
   end function

!  ***************
!  Inquiry methods
!  ***************

   function exists(self,name) result(res)
    TEXTFILE :: self
   ! Returns true if the file exists on the file system.
   ! Uses "name" if present, otherwise ".name".
      STR(*), optional :: name
      BIN :: res
      STACK("TEXTFILE:exists")
      START_TIMER("TEXTFILE:exists")
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
     STOP_TIMER("TEXTFILE:exists")
      CHECK
   end function

   function is_open(self) result(res)
    TEXTFILE :: self
   ! Returns true if the file has been opened
      BIN :: res
      ! inquire(unit=.unit,opened=res)
      STACK("TEXTFILE:is_open")
      START_TIMER("TEXTFILE:is_open")
      if (self%unit==TEXTFILE_STDIN_UNIT OR self%unit==TEXTFILE_STDOUT_UNIT) then
        res = TRUE ! not needed for stdin, stdout
      else
        if (do_io_(tonto_parallel)) then
          inquire(file=self%name,opened=res)
        end if
      end if
      call broadcast_(tonto_parallel,res,0)
     STOP_TIMER("TEXTFILE:is_open")
      CHECK
   end function

   function unit_used(self) result(res)
    TEXTFILE :: self
   ! Returns true if the file unit is in use
      BIN :: res
      STACK("TEXTFILE:unit_used")
      START_TIMER("TEXTFILE:unit_used")
      if (do_io_(tonto_parallel)) then
        inquire(unit=self%unit,opened=res)
      end if
      call broadcast_(tonto_parallel,res,0)
     STOP_TIMER("TEXTFILE:unit_used")
      CHECK
   end function

   function next_line_item(self) result(res)
    TEXTFILE :: self
   ! Return the index of the next item to be processed on the line
      INT :: res
      STACK("TEXTFILE:next_line_item")
      START_TIMER("TEXTFILE:next_line_item")
      res = next_item_number_(self%buffer)
     STOP_TIMER("TEXTFILE:next_line_item")
      CHECK
   end function

   function previous_line_item(self) result(res)
    TEXTFILE :: self
   ! Return the index of the previous item to be processed on the line
      INT :: res
      STACK("TEXTFILE:previous_line_item")
      START_TIMER("TEXTFILE:previous_line_item")
      res = self%buffer%item_index
     STOP_TIMER("TEXTFILE:previous_line_item")
      CHECK
   end function

   function last_line_item(self) result(res)
    TEXTFILE :: self
   ! Return the index of the final item on the line
      INT :: res
      STACK("TEXTFILE:last_line_item")
      START_TIMER("TEXTFILE:last_line_item")
      res = self%buffer%n_items
     STOP_TIMER("TEXTFILE:last_line_item")
      CHECK
   end function

   function n_line_items(self) result(res)
    TEXTFILE :: self
   ! Return the number of items on the line
      INT :: res
      STACK("TEXTFILE:n_line_items")
      START_TIMER("TEXTFILE:n_line_items")
      res = self%buffer%n_items
     STOP_TIMER("TEXTFILE:n_line_items")
      CHECK
   end function

   function at_end_of_line(self) result(res)
    TEXTFILE :: self
   ! Return TRUE if at the end of the line
      BIN :: res
      STACK("TEXTFILE:at_end_of_line")
      START_TIMER("TEXTFILE:at_end_of_line")
      res = self%buffer%item_index==self%buffer%n_items
     STOP_TIMER("TEXTFILE:at_end_of_line")
      CHECK
   end function

   function line_number(self) result(res)
    TEXTFILE :: self
   ! Return the input file line number which is being processed
      INT :: res
      STACK("TEXTFILE:line_number")
      START_TIMER("TEXTFILE:line_number")
      res = self%record
     STOP_TIMER("TEXTFILE:line_number")
      CHECK
   end function

   function buffer_string(self) result(res)
    TEXTFILE :: self
   ! Put a string into the buffer
      STR(BSTR_SIZE) :: res
      STACK("TEXTFILE:buffer_string")
      START_TIMER("TEXTFILE:buffer_string")
      res = buffer_string_(self%buffer)
     STOP_TIMER("TEXTFILE:buffer_string")
      CHECK
   end function

   function end_of_file(self) result(res)
    TEXTFILE :: self
   ! See if .io_status>1, indicating the end of file has been found.
      BIN :: res
      STACK("TEXTFILE:end_of_file")
      START_TIMER("TEXTFILE:end_of_file")
      res = self%io_status>0
     STOP_TIMER("TEXTFILE:end_of_file")
      CHECK
   end function

   !reverted(reset) result(res)
   function reverted(self) result(res)
    TEXTFILE :: self
   ! See if .io_status==-1, indicating an internal file has ended (i.e. a soft
   ! ending).  NOTE: Unless the "reset" variable is present and FALSE, the
   ! .io_status variable is reset by default. So if you want the soft ending to
   ! be detectable by later routines, you must call this routine with
   ! "reset=FALSE".
    !  reset :: BIN, optional
      BIN :: res
    ! reset_io_status :: BIN
      STACK("TEXTFILE:reverted")
      START_TIMER("TEXTFILE:reverted")
      res = self%io_status==-1
    ! reset_io_status = TRUE
    ! if (present(reset)) reset_io_status = reset
    ! if (reset_io_status) .io_status = 0
     STOP_TIMER("TEXTFILE:reverted")
      CHECK
   end function

   function at_end_of_file(self) result(res)
    TEXTFILE :: self
   ! Read a line into the buffer from the input file and see if it is at the end
   ! of file. This is an explicit test, not just a viewing of ".io_status".
   ! NOTE: If all lines to the end of file are empty, then the result is also
   ! true.
      BIN :: res
      STR(STR_SIZE) :: word
      BIN :: ignore
      STACK("TEXTFILE:at_end_of_file")
      START_TIMER("TEXTFILE:at_end_of_file")
      ENSURE(self%action=="read","TEXTFILE:at_end_of_file ... file does not have read status")
      ignore = self%ignore_end_of_file ! Save this
      self%ignore_end_of_file = TRUE
      call read_str_(self,word)
      res = end_of_file_(self)
      self%io_status = 0
      call move_to_previous_item_(self)
      self%ignore_end_of_file = ignore ! Put back ignore state
     STOP_TIMER("TEXTFILE:at_end_of_file")
      CHECK
   end function

   function buffer_exhausted(self) result(res)
    TEXTFILE :: self
   ! Return whether the buffer is exhausted
     BIN :: res
       STACK("TEXTFILE:buffer_exhausted")
       START_TIMER("TEXTFILE:buffer_exhausted")
       res = exhausted_(self%buffer)
     STOP_TIMER("TEXTFILE:buffer_exhausted")
      CHECK
   end function

! ***************
! System routines
! ***************

   subroutine update_system_info(self)
    TEXTFILE :: self
   ! Lets the system know info about the file being read, in case of error.
     target :: self
     STACK("TEXTFILE:update_system_info")
     START_TIMER("TEXTFILE:update_system_info")
     tonto%io_file => self
     STOP_TIMER("TEXTFILE:update_system_info")
      CHECK
   end subroutine

end
