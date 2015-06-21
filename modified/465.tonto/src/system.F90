!-------------------------------------------------------------------------------
!
! SYSTEM: Contains system level routines, including memory management and error
! messages.  It allows for the graceful termination of a program.
!
! An error status is simply an integer, the value is set to 1 if the program
! terminates, or -1 if a warning condition is encountered.
!
! The memory part stores the total and maximum memory used (in bytes), and the
! the total and maximum number of blocks of memory allocated.
! A memory limit is also stored.  It is a fatal error to use more than the
! allocated limit.
!
! The file part contains the file name and record number of the last acessed
! file, in the event that this file should cause an error, the exact position
! will be known.
!
! A standard system object, "tonto", is provided to hold system information
! in the current program. In most cases it should not be neccesary to
! create any other system objects.
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
! $Id: system.foo,v 1.28.2.8 2003/10/09 09:16:34 reaper Exp $
!-------------------------------------------------------------------------------

module SYSTEM_MODULE

#  include "system.use"

#ifdef SERVICE_ROUTINES
   USE service_routines, only: flush
#endif
#ifdef F90_UNIX_IO
   USE f90_unix_io, only: flush
#endif

   implicit none

#  include "macros"
#  include "system.int"


#  undef STACK
#  undef UNSTACK
#  undef CHECK
#  undef START_TIMER
#  undef STOP_TIMER

#  define STACK(X)
#  define UNSTACK
#  define CHECK
#  define START_TIMER(X)
#  define STOP_TIMER(X)

#ifdef MPI
#include "mpif.h"
#endif

   SYSTEM, target, public, save :: tonto
   PARALLEL, public, save :: tonto_parallel

contains

!  *****************
!  Creation routines
!  *****************

   subroutine create(self,memory_limit)
    SYSTEM :: self
   !  Create a memory manager object with soft limit given in "limit"
      PTR :: self
      INT, optional :: memory_limit
      STACK("SYSTEM:create")
      START_TIMER("SYSTEM:create")
      allocate(self)
      call nullify_ptr_part_(self)
      call set_defaults_(self,memory_limit)
      call add_memory_(tonto,SYSTEM_SIZE) ! increment internal memory manager
     STOP_TIMER("SYSTEM:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SYSTEM :: self
   !  Destroy the memory manager object
      PTR :: self
      STACK("SYSTEM:destroy")
      START_TIMER("SYSTEM:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SYSTEM:destroy") UNSTACK return; end if
      deallocate(self)
      call delete_memory_(tonto,SYSTEM_SIZE)
     STOP_TIMER("SYSTEM:destroy")
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

   subroutine nullify_ptr_part(self)
    SYSTEM :: self
   ! Nullify the pointer parts of self
      INOUT :: self
      STACK("SYSTEM:nullify_ptr_part")
      START_TIMER("SYSTEM:nullify_ptr_part")
      nullify(self%io_file)
      nullify(self%call_stack)
      nullify(self%memory_used_stack)
      nullify(self%time_call_stack)
      nullify(self%time_strt_stack)
      nullify(self%time_for_routine)
      nullify(self%name_for_routine)
     STOP_TIMER("SYSTEM:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine set_defaults(self,memory_limit)
    SYSTEM :: self
   !  Set defaults
      INOUT :: self
      INT, optional :: memory_limit
      STACK("SYSTEM:set_defaults")
      START_TIMER("SYSTEM:set_defaults")
      self%error_status = 0
      self%error_output_unit = SYSTEM_ERROR_OUTPUT_UNIT
      self%memory_used = 0
      self%memory_blocks_used = 0
      self%max_memory_used = 0
      self%max_memory_blocks_used = 0
      self%memory_limit = SYSTEM_MEMORY_LIMIT
      self%memory_limit_exceeded = FALSE
      self%memory_leak_detected = FALSE
      self%memory_leak_level = 0
      self%memory_units = SYSTEM_MEMORY_UNITS
      self%stack_level = 0
      self%max_stack_level = 0
      self%stack_show_level = -1
      self%show_call_stack = FALSE
      ! Timing stuff ...
      self%time_stack_level = 0
      self%n_timed_routines = 0
      self%time_limit = ZERO
      if (present(memory_limit)) self%memory_limit = memory_limit
     STOP_TIMER("SYSTEM:set_defaults")
      CHECK
   end subroutine

   subroutine initialize(self)
    SYSTEM :: self
   !  Initialise the system object and set defaults
      STACK("SYSTEM:initialize")
      START_TIMER("SYSTEM:initialize")
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SYSTEM:initialize")
      CHECK
   end subroutine

   function unit_conversion_factor(self) result(res)
    SYSTEM :: self
   ! Change the units used to display the memory
      IN :: self
      INT :: res
      STACK("SYSTEM:unit_conversion_factor")
      START_TIMER("SYSTEM:unit_conversion_factor")
      select case(self%memory_units)
         case("Words");  res = 8
         case("Bytes");  res = 1
         case("MBytes"); res = 1000000
         case("MWords"); res = 8000000
      end select
     STOP_TIMER("SYSTEM:unit_conversion_factor")
      CHECK
   end function

!   reset_limit(limit)
!   !  Reset memory limit for the memory manager
!   !  "limit" is in bytes.
!      self :: INOUT
!      limit :: INT, IN
!      fac :: INT
!      fac = .unit_conversion_factor
!      .memory_limit = fac*limit
!   end

!  ****************
!  Error operations
!  ****************

!   reset_error_status
!   ! Reset the error flag
!      .error_status = 0
!   end

   subroutine set_error_output_unit(self,number)
    SYSTEM :: self
   ! Set the error unit "number"
      INT :: number
      STACK("SYSTEM:set_error_output_unit")
      START_TIMER("SYSTEM:set_error_output_unit")
      self%error_output_unit = number
     STOP_TIMER("SYSTEM:set_error_output_unit")
      CHECK
   end subroutine

   subroutine set_error_output_file(self,file)
    SYSTEM :: self
   ! Set the error output file to "file"
      TEXTFILE :: file
      STACK("SYSTEM:set_error_output_file")
      START_TIMER("SYSTEM:set_error_output_file")
      self%error_output_unit = file%unit
     STOP_TIMER("SYSTEM:set_error_output_file")
      CHECK
   end subroutine

!  **************
!  Error messages
!  **************

   subroutine die(self,message)
    SYSTEM :: self
   ! Set the error flag to 1 and terminate the program with a message
      STR(*) :: message
      STACK("SYSTEM:die")
      START_TIMER("SYSTEM:die")
      self%error_status = 1
      if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a)") &
         "Error in routine "// trim(message) ! message should include the routine name via foo
      end if
      call report_stack_see_info_(self)
      call report_io_file_info_(self)
      call report_keyword_info_(self)
      call report_stack_info_(self)
#ifdef MPI
      call MPI_abort(MPI_COMM_WORLD,self%error_status,tonto_parallel%mpi_status)
#endif
      stop
     STOP_TIMER("SYSTEM:die")
      CHECK
   end subroutine

   subroutine die_if(self,condition,message)
    SYSTEM :: self
   ! Set the error flag to 1 and terminate the program with a message
   ! provided "condition" is TRUE
      BIN :: condition
      STR(*) :: message
      STACK("SYSTEM:die_if")
      START_TIMER("SYSTEM:die_if")
      if (condition) call die_(self,message)
     STOP_TIMER("SYSTEM:die_if")
      CHECK
   end subroutine

   subroutine unknown(self,word,name,options)
    SYSTEM :: self
   ! Set the error flag to 1 and terminate the program with a message
   ! "Unknown option". The list of known keywords is dumped.
      STR(*), IN :: word
      STR(*), IN :: name
      STRVEC(len=*,:), IN :: options
      STACK("SYSTEM:unknown")
      START_TIMER("SYSTEM:unknown")
      self%error_status = 1
      if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a)") &
        "Error in routine "// trim(name) // " ... unknown option " // trim(word)
      end if
      call report_stack_see_info_(self)
      call report_io_file_info_(self)
      call report_keyword_info_(self,options)
      call report_stack_info_(self)
#ifdef MPI
      call MPI_abort(MPI_COMM_WORLD,self%error_status,tonto_parallel%mpi_status)
#endif
      stop
     STOP_TIMER("SYSTEM:unknown")
      CHECK
   end subroutine

   subroutine unknown_1(self,word,name)
    SYSTEM :: self
   ! Set the error flag to 1 and terminate the program with a message
   ! "Unknown option". The list of known keywords is dumped.
      STR(*), IN :: word
      STR(*), IN :: name
      STACK("SYSTEM:unknown_1")
      START_TIMER("SYSTEM:unknown_1")
      self%error_status = 1
      if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a)") &
        "Error in routine "// trim(name) // " ... unknown option " // trim(word)
      end if
      call report_stack_see_info_(self)
      call report_io_file_info_(self)
      if (associated(self%known_keywords)) call report_keyword_info_(self,self%known_keywords)
      call report_stack_info_(self)
#ifdef MPI
      call MPI_abort(MPI_COMM_WORLD,self%error_status,tonto_parallel%mpi_status)
#endif
      stop
     STOP_TIMER("SYSTEM:unknown_1")
      CHECK
   end subroutine

   subroutine warn(self,message,iostat,use_stack_name)
    SYSTEM :: self
   ! Set the error flag to -1 and issue a warning message.
   ! If "use_stack_name" is present, the call stacl routine name is used
   ! in the warning messsage, assuming that the call stack is associated.
      STR(*) :: message
      INT, IN, optional :: iostat
      BIN, IN, optional :: use_stack_name
      STR(STR_SIZE) :: name
      STACK("SYSTEM:warn")
      START_TIMER("SYSTEM:warn")
      self%error_status = -1
      if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
      end if
      if (present(use_stack_name)) then
         if (associated(self%call_stack)) then
            name = self%call_stack(self%stack_level)
            if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
              write(self%error_output_unit,"(a)") &
              "Warning in routine "// trim(name) // " ... " // trim(message)
            end if
         else
            if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
              write(self%error_output_unit,"(a)") &
              "Warning: " // trim(message)
            end if
         end if
      else
        if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
           write(self%error_output_unit,"(a)") &
           "Warning in routine "// trim(message) ! message should include the routine name
        end if
      end if
!      .report_stack_see_info
      if (present(iostat)) then
        if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
           write(self%error_output_unit,"(a,i4)") "Fortran error ",iostat
        end if
      end if
      call flush_buffer_(self)
     STOP_TIMER("SYSTEM:warn")
      CHECK
   end subroutine

   subroutine warn_if(self,condition,message,iostat)
    SYSTEM :: self
   ! If "condition" is true, issue a warning and continue, but set the error
   ! flag to -1 and
      BIN, IN :: condition
      STR(*), IN :: message
      INT, IN, optional :: iostat
      STACK("SYSTEM:warn_if")
      START_TIMER("SYSTEM:warn_if")
      if (condition) call warn_(self,message,iostat)
     STOP_TIMER("SYSTEM:warn_if")
      CHECK
   end subroutine

   subroutine ensure(self,condition,message)
    SYSTEM :: self
   ! Ensure "condition" is true, otherwise set the error flag to 1 and
   ! terminate the program with a "message"
      BIN, IN :: condition
      STR(*), IN :: message
      STACK("SYSTEM:ensure")
      START_TIMER("SYSTEM:ensure")
      if (NOT condition) call die_(self,message)
     STOP_TIMER("SYSTEM:ensure")
      CHECK
   end subroutine

   subroutine report_stack_see_info(self)
    SYSTEM :: self
   ! Report information about how to compile to see call stack management
   ! information
      INT :: unit
      STACK("SYSTEM:report_stack_see_info")
      START_TIMER("SYSTEM:report_stack_see_info")
      if (associated(self%call_stack)) then; STOP_TIMER("SYSTEM:report_stack_see_info") CHECK return; end if
      unit = self%error_output_unit
      if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
        write(unit,"(a)") " "
!        write(unit,"(a)") "To see a routine call stack locating this error more precisely,"
!        write(unit,"(a)") "try compiling with the USE_CALL_STACK_MANAGEMENT macro. See the"
!        write(unit,"(a)") "manual for more details"
      end if
     STOP_TIMER("SYSTEM:report_stack_see_info")
      CHECK
   end subroutine

!  *********************************************************
!  Call stack management, memory reporting and leak checking
!  *********************************************************

   PURE subroutine add_memory(self,used)
    SYSTEM :: self
   !  Add memory usage data to the memory manager
      INOUT :: self
      INT, IN :: used
      self%memory_used = self%memory_used + used
      self%max_memory_used = max(self%memory_used,self%max_memory_used)
      self%memory_blocks_used = self%memory_blocks_used + 1
      self%max_memory_blocks_used = max(self%memory_blocks_used,self%max_memory_blocks_used)
      self%memory_used_stack(1:self%stack_level) = self%memory_used_stack(1:self%stack_level) + used
     STOP_TIMER("SYSTEM:add_memory")
   end subroutine

   PURE subroutine delete_memory(self,used)
    SYSTEM :: self
   !  Delete memory usage data to the memory manager
      INOUT :: self
      INT, IN :: used
      self%memory_used = self%memory_used - used
      self%memory_blocks_used = self%memory_blocks_used - 1
      self%memory_used_stack(1:self%stack_level) = self%memory_used_stack(1:self%stack_level) - used
     STOP_TIMER("SYSTEM:delete_memory")
   end subroutine

   subroutine stack(self,routine_name)
    SYSTEM :: self
   ! Add another level to the call stack. Placed at the start of every
   ! non-pure routine. (Pure routines may not have I/O which this has).
      INOUT :: self
      STR(*), optional :: routine_name
      STACK("SYSTEM:stack")
      START_TIMER("SYSTEM:stack")
      self%stack_level = self%stack_level + 1
      call expand_all_stacks_(self)
      self%memory_used_stack(self%stack_level) = 0
      if (present(routine_name)) then
        self%call_stack(self%stack_level) = routine_name
      else
        self%call_stack(self%stack_level) = "Unknown routine"
      end if
      if (self%show_call_stack) then
        if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
          write(self%error_output_unit,"(a)") repeat("   ",self%stack_level)//trim(routine_name)//" {"
        end if
      end if
      call flush_buffer_(self)
      self%memory_leak_detected = FALSE ! reset any memory leaks
      if (self%stack_level<self%memory_leak_level) self%memory_leak_level = 0
      ! allow leak reports again at higher levels
     STOP_TIMER("SYSTEM:stack")
      CHECK
   end subroutine

   subroutine unstack(self)
    SYSTEM :: self
   ! Remove a level from the call level. Placed at the end of every non-pure
   ! routine, *including* leaky routines.  If the current level exceeds
   ! stack_show_level then a stack report is produced --- provided that
   ! show_call_stack is not set; but if it is set, then an indented stack report
   ! is made instead.
     INOUT :: self
     BIN :: report_stack,show_call_stack
     STR(STR_SIZE) :: routine_name
     INT :: mem
     STR(9) :: memory
     INT :: l
     STACK("SYSTEM:unstack")
     START_TIMER("SYSTEM:unstack")
     l = self%stack_level
     if (l<=0) then
        if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
          write(self%error_output_unit,*)
          write(self%error_output_unit,"(a,I2)") &
          "Warning in routine SYSTEM:unstack ... stack level is not positive, ", l
        end if
        call report_io_file_info_(self)
        call report_stack_info_(self,full_report=TRUE)
#ifdef MPI
        call MPI_abort(MPI_COMM_WORLD,self%error_status,tonto_parallel%mpi_status)
#endif
        stop
     else if (l>self%max_stack_level) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a,2I2)") &
        "Warning in routine SYSTEM:unstack ... stack level greater than max, ", &
        l,self%max_stack_level
#ifdef MPI
        call MPI_abort(MPI_COMM_WORLD,self%error_status,tonto_parallel%mpi_status)
#endif
        stop
     else
        show_call_stack =             & ! Show indented call stack if:
           self%show_call_stack           & ! ... switch was set
        AND l >=self%stack_show_level       ! ... stack is greater than check
        report_stack =                & ! Report tabular stack info if:
            l >= self%stack_show_level    & ! ... greater than check level
        AND self%stack_show_level > 0     & ! ... check level was set
        AND NOT show_call_stack         ! ... NOT doing indented view
        if (report_stack) then
           call report_stack_info_(self)
        else if (show_call_stack) then
           routine_name = self%call_stack(l)
           mem = self%memory_used_stack(l)/unit_conversion_factor_(self)
           if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
             write(memory,"(I9)") mem
           end if
           if (mem==0) then
             if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
               write(self%error_output_unit,"(a)") &
                repeat("   ",l)//trim(routine_name)//" } "//adjustl(memory)
             end if
           else
             if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
               write(self%error_output_unit,"(a)") &
                repeat("---",l)//trim(routine_name)//" } "//adjustl(memory)
             end if
           end if
        end if
     end if
     self%call_stack(l) = " "
     self%stack_level   = self%stack_level - 1
     call flush_buffer_(self)
     STOP_TIMER("SYSTEM:unstack")
      CHECK
   end subroutine

   subroutine check(self)
    SYSTEM :: self
   ! Check for memory leaks at this level. Placed at the end of every non-pure,
   ! non-leaky routine. A stack report is produced only if there is a leak, AND
   ! if the level is greater than the check_level set by start_leak_test (since
   ! for levels less or equal to than the check_level, unstack produces a stack
   ! report).
      STACK("SYSTEM:check")
      START_TIMER("SYSTEM:check")
      if (self%stack_level<=0) then
        if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
          write(self%error_output_unit,*)
          write(self%error_output_unit,"(a,I2)") &
          "Warning in routine SYSTEM:check ... stack level is not positive, ", &
          self%stack_level
        end if
        call report_io_file_info_(self)
        call report_stack_info_(self,full_report=TRUE)
#ifdef MPI
        call MPI_abort(MPI_COMM_WORLD,self%error_status,tonto_parallel%mpi_status)
#endif
        stop
      end if
      call check_exceeded_(self)
      call check_if_leaky_(self)
      call unstack_(self)
     STOP_TIMER("SYSTEM:check")
      CHECK
   end subroutine

   subroutine check_exceeded(self)
    SYSTEM :: self
   !  Checks whether memory limit is exceeded
      STR(STR_SIZE) :: name
      STACK("SYSTEM:check_exceeded")
      START_TIMER("SYSTEM:check_exceeded")
      name = self%call_stack(self%stack_level)
      if (self%memory_used>self%memory_limit AND NOT self%memory_limit_exceeded) then
         call warn_(self,"Memory limit exceeded in routine "//trim(name))
         call report_(self)
         self%memory_limit_exceeded = TRUE
      end if
     STOP_TIMER("SYSTEM:check_exceeded")
      CHECK
   end subroutine

   subroutine check_if_leaky(self)
    SYSTEM :: self
   ! Checks whether there is a memory leak, and if so produce a warning.
   ! A stack report is made only if the stack_show_level was not set,
   ! or if it was set, it the leak occurs below the stack_show_level
   ! (since otherwise the unstack routine will make a stack report).
      BIN :: produce_warning,produce_report
      INT :: l
      STACK("SYSTEM:check_if_leaky")
      START_TIMER("SYSTEM:check_if_leaky")
      l = self%stack_level
      produce_warning =  &               ! produce warning if:
         self%memory_used_stack(l) /= 0    & ! ... there is a leak at this level,
         AND NOT self%memory_leak_detected & ! ... it hasn't yet been seen,
         AND l > self%memory_leak_level      ! ... it wasn't reported already
      if (produce_warning) then
         if (self%memory_used_stack(l)>0) call warn_(self,"memory leak",use_stack_name=TRUE)
         if (self%memory_used_stack(l)<0) call warn_(self,"memory sink",use_stack_name=TRUE)
         self%memory_leak_detected = TRUE
         if (self%memory_leak_level==0) then
            self%memory_leak_level = self%stack_level - 1
         else
            self%memory_leak_level = min(self%memory_leak_level,self%stack_level-1)
         end if
      end if
      produce_report  =  &               ! produce stack report if:
         produce_warning               & ! ... there was a warning made AND
         AND (0 > self%stack_show_level    & ! ... stack level was not set OR
         OR   l < self%stack_show_level)     ! ... unstack makes no report
      if (produce_report) then
         call report_stack_info_(self)
      end if
     STOP_TIMER("SYSTEM:check_if_leaky")
      CHECK
   end subroutine

   subroutine ignore_memory_leak(self,memory_blocks_gained)
    SYSTEM :: self
   ! If called, this routine will reset any memory leak in the current
   ! procedure at the point of call. This is highly dangerous and should be used
   ! only in special cases when you are sure the leak can be tolerated.
   ! "memory_blocks_gained" is the number of blocks that were gained in the leak
   ! process: it is negative for a loss in memory, and positive for a gain in
   ! memory.
      INT :: memory_blocks_gained
      STACK("SYSTEM:ignore_memory_leak")
      START_TIMER("SYSTEM:ignore_memory_leak")
      if (NOT associated(self%memory_used_stack)) then; STOP_TIMER("SYSTEM:ignore_memory_leak") CHECK return; end if
      call delete_memory_(self,self%memory_used_stack(self%stack_level))
      self%memory_blocks_used = self%memory_blocks_used + 1 - memory_blocks_gained
     STOP_TIMER("SYSTEM:ignore_memory_leak")
      CHECK
   end subroutine

   subroutine start_show(self,depth,show_call_stack)
    SYSTEM :: self
   ! Start printing out the memory stack for all routines that are called (i.e.
   ! at the next level). This command is undone by the "end_show" routine. If
   ! "depth" is present, the show starts at the current level plus "depth".
   ! If "show_call_stack" is present and FALSE, then a tabular style output is
   ! shown rather than the default indented style.
      INT, optional :: depth
      BIN, optional :: show_call_stack
      STACK("SYSTEM:start_show")
      START_TIMER("SYSTEM:start_show")
      self%stack_show_level = self%stack_level + 1
      if (present(depth)) self%stack_show_level = self%stack_level + depth
      self%show_call_stack = TRUE
      if (present(show_call_stack)) self%show_call_stack = FALSE
     STOP_TIMER("SYSTEM:start_show")
      CHECK
   end subroutine

   subroutine end_show(self)
    SYSTEM :: self
   ! Ends memory leak testing at this level
      STACK("SYSTEM:end_show")
      START_TIMER("SYSTEM:end_show")
      self%stack_show_level = -1
      self%show_call_stack = FALSE
     STOP_TIMER("SYSTEM:end_show")
      CHECK
   end subroutine

   subroutine expand_all_stacks(self)
    SYSTEM :: self
   ! Expand both stacks to a length at least equal to ".stack_level".
      INT :: dim
      STACK("SYSTEM:expand_all_stacks")
      START_TIMER("SYSTEM:expand_all_stacks")
      dim = self%stack_level - self%max_stack_level
      if (dim>0) then
         call expand_int_(self,self%memory_used_stack,dim)
         call expand_str_(self,self%call_stack,dim)
         self%max_stack_level = self%stack_level
      end if
     STOP_TIMER("SYSTEM:expand_all_stacks")
      CHECK
   end subroutine

   subroutine report(self,out)
    SYSTEM :: self
   !  Report memory usage
     TEXTFILE, optional :: out
     INT :: unit,fac
     STACK("SYSTEM:report")
     START_TIMER("SYSTEM:report")
     if (self%max_memory_used /= 0) then
       unit = tonto%error_output_unit
       if (present(out)) unit = out%unit
       fac = unit_conversion_factor_(self)
#ifndef SPEC_CPU
       if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
         write(unit,'(/"SYSTEM: Memory usage report:"/)')
         write(unit,'("Memory used                = ",i9," ",a6)') self%memory_used/fac,self%memory_units
         write(unit,'("Maximum memory used        = ",i9," ",a6)') self%max_memory_used/fac,self%memory_units
         write(unit,'("Memory blocks used         = ",i9)') self%memory_blocks_used
         write(unit,'("Maximum memory blocks used = ",i9)') self%max_memory_blocks_used
         write(unit,'("Call stack level           = ",i9)') self%stack_level
         write(unit,'("Maximum call stack depth   = ",i9)') self%max_stack_level
       end if
#endif
       call report_stack_info_(self,out)
       call report_timing_info_(self) 
     end if
     call flush_buffer_(self)
     STOP_TIMER("SYSTEM:report")
      CHECK
   end subroutine

   subroutine report_stack_info(self,out,full_report)
    SYSTEM :: self
   ! Report memory stack usage. Use the unit number for file "out", if present.
   ! If present and TRUE, "full_report" requests a full stack output.
     TEXTFILE, optional :: out
     BIN, optional :: full_report
     INT :: unit,fac,l
     BIN :: full
     STACK("SYSTEM:report_stack_info")
     START_TIMER("SYSTEM:report_stack_info")
     if (self%max_stack_level /= 0) then
       unit = tonto%error_output_unit
       if (present(out)) unit = out%unit
       full = FALSE
       if (present(full_report)) full = full_report
       fac = unit_conversion_factor_(self)
       if (self%stack_level>0 AND associated(self%call_stack)) then
         if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
           write(unit,*)
           write(unit,'(a)') "Routine call stack:"
           write(unit,*)
           write(unit,'("   Call   Routine name        ",a37)') "Memory Used"
         end if
         do l = 1,self%stack_level
#ifndef SPEC_CPU
           if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
             write(unit,'(3x,i3,".",3x,a48,i9)') &
                         l,adjustl(self%call_stack(l)),self%memory_used_stack(l)/fac
           end if
#endif
         end do
       end if
       if (full AND self%max_stack_level>self%stack_level AND associated(self%call_stack)) then
           if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
             write(unit, &
           '("   ----------------------------------------------------------------")')
           end if
         do l = self%stack_level+1,self%max_stack_level
           if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
              write(unit,'(3x,i3,".",3x,a48,i9)') &
              l,adjustl(self%call_stack(l)),self%memory_used_stack(l)/fac
           end if
         end do
       end if
     end if
     call flush_buffer_(self)
     STOP_TIMER("SYSTEM:report_stack_info")
      CHECK
   end subroutine

   subroutine report_io_file_info(self)
    SYSTEM :: self
   ! Report info about the most recent open file.
     INT :: unit
     STR(BSTR_SIZE) :: cursor
     INT :: item_end
     STACK("SYSTEM:report_io_file_info")
     START_TIMER("SYSTEM:report_io_file_info")
     if (NOT associated(self%io_file)) then; STOP_TIMER("SYSTEM:report_io_file_info") CHECK return; end if
     unit = tonto%error_output_unit
     item_end = max(1,self%io_file%buffer%item_end)
     if (item_end>0) cursor = repeat("-",item_end-1)//"^"
     if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
       write(unit,*)
       write(unit,'("File name   = ",a)')  trim(self%io_file%name)
       write(unit,'("Line number = ",i4)') self%io_file%record
       write(unit,'("File buffer = ",a)')  trim(self%io_file%buffer%string)
     end if
     if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
       if (item_end>0) then
         write(unit,'("Cursor -------",a)')  trim(cursor)
       end if
     end if
     call flush_buffer_(self)
     STOP_TIMER("SYSTEM:report_io_file_info")
      CHECK
   end subroutine

   subroutine report_keyword_info(self,options)
    SYSTEM :: self
   ! Report info about the most recent keywords used
      STRVEC(len=*,:), optional :: options
      INT :: n
      INT :: unit
      STACK("SYSTEM:report_keyword_info")
      START_TIMER("SYSTEM:report_keyword_info")
      unit = tonto%error_output_unit
      if (present(options)) then
        if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
          write(unit,*)
          write(unit,'("Allowed keyword options:")')
          write(unit,*)
          do n = 1,size(options)
            write(unit,'("   ",a)') trim(options(n))
          end do
        end if
      end if
      call flush_buffer_(self)
     STOP_TIMER("SYSTEM:report_keyword_info")
      CHECK
   end subroutine

   subroutine start_timer(self,routine_name)
    SYSTEM :: self
   ! Start timing routine "routine_name". Placed at the start of every routine
   ! when profiling is requested. 
      STR(*) :: routine_name
      REAL :: start_time
      STACK("SYSTEM:start_timer")
      START_TIMER("SYSTEM:start_timer")
      self%time_stack_level = self%time_stack_level + 1
      call cpu_time(start_time)
      call expand_time_stacks_(self,routine_name,start_time) 
     STOP_TIMER("SYSTEM:start_timer")
      CHECK
   end subroutine

   subroutine stop_timer(self,routine_name)
    SYSTEM :: self
   ! Start the timer for routine "routine_name". Placed at the start of every
   ! routine profiling is requested. 
      STR(*) :: routine_name
      INT :: index,i
      REAL :: start_time,finish_time,elapsed_time,total_time,total
      ! Get the finishing time
      STACK("SYSTEM:stop_timer")
      START_TIMER("SYSTEM:stop_timer")
      call cpu_time(finish_time)
      ! Get the starting time and elapsed time
      start_time = self%time_strt_stack(self%time_stack_level)
      elapsed_time = finish_time - start_time
      ! Increment the elapsed time for the current routine
      index = index_for_routine_(self,routine_name)
      self%time_for_routine(index) = self%time_for_routine(index) + elapsed_time
      ! Remove this elapsed time from all the parent routines 
      do i = 1,self%time_stack_level-1
         index = self%time_call_stack(i) ! This is a parent routine index
         self%time_for_routine(index) = self%time_for_routine(index) - elapsed_time
      end do
      ! See whether to stop program if total time exceeded
      if (self%time_limit>ZERO) then
         ! Get total time: Add up total time so far
         total_time = ZERO
         do i = 1,self%n_timed_routines
            total = self%time_for_routine(i)
            if (total<ZERO) cycle ! These have not completed
            total_time = total_time + total
         end do
         ! Make a report and stop if total time exceeded 
         if (total_time>self%time_limit) then
            call report_timing_info_(self)
            stop
         end if
      end if
      ! Remove the routine from the call stack
      self%time_call_stack(self%time_stack_level) = 0
      self%time_stack_level = self%time_stack_level - 1
    ! write(*,*) "---- returning, time_stack_level=",.time_stack_level
     STOP_TIMER("SYSTEM:stop_timer")
      CHECK
   end subroutine

   subroutine expand_time_stacks(self,routine_name,start_time)
    SYSTEM :: self
   ! Expand all the time stacks (if required) by adding "routine_name" in the
   ! ".name_for_routine" stack, for example. Also add the starting time.
     STR(*) :: routine_name
     REAL :: start_time
     INT :: index,dim
     ! First expand the list of akll timed routines
     STACK("SYSTEM:expand_time_stacks")
     START_TIMER("SYSTEM:expand_time_stacks")
     index = index_for_routine_(self,routine_name)
     if (index==0) then  ! NEW routine. 
        dim = self%n_timed_routines
        if (associated(self%time_for_routine)) dim = dim - size(self%time_for_routine)
        if (dim>=0) then ! Not enough space? Expand stack ...
           call expand_real_(self,self%time_for_routine,dim+1)
           call expand_str_(self, self%name_for_routine,dim+1)
        end if
        index = self%n_timed_routines + 1
        self%n_timed_routines = index
        self%time_for_routine(index) = ZERO
        self%name_for_routine(index) = routine_name
        write(*,*) "NEW=",index,"name=",trim(routine_name)
     else
        write(*,*) "old=",index,"name=",trim(routine_name)
     end if
     ! Now expand the timed call stack ...
     dim = self%time_stack_level
     if (associated(self%time_call_stack)) dim = dim - size(self%time_call_stack)
     if (dim>0) then
        call expand_int_(self, self%time_call_stack,dim)
        call expand_real_(self,self%time_strt_stack,dim)
     end if
     self%time_call_stack(self%time_stack_level) = index
     self%time_strt_stack(self%time_stack_level) = start_time
     STOP_TIMER("SYSTEM:expand_time_stacks")
      UNSTACK
   end subroutine

   function index_for_routine(self,routine_name) result(index)
    SYSTEM :: self
   ! Return the "index" for routine "routine_name" in the ".name_for_routine"
   ! stack. 
     STR(*) :: routine_name
     INT :: index
     INT :: i
     STACK("SYSTEM:index_for_routine")
     START_TIMER("SYSTEM:index_for_routine")
     index = 0
     do i = 1,self%n_timed_routines
        if (self%name_for_routine(i)/=routine_name) cycle
        index = i
        exit
     end do
     STOP_TIMER("SYSTEM:index_for_routine")
      UNSTACK
   end function

   subroutine report_timing_info(self,out,full_report)
    SYSTEM :: self
   ! Report routine timing info i.e. a profile. If present, use the unit number for file
   ! "out", otherwise use "tonto.error_output_unit".  If present and TRUE,
   ! "full_report" requests a full stack output, otherwise only the top 20 are
   ! reported.
     TEXTFILE, optional :: out
     BIN, optional :: full_report
     BIN :: full
     INTVEC(:), PTR :: order
     REAL :: total_time,total
     INT :: unit,i,j,n_routine
     STACK("SYSTEM:report_timing_info")
     START_TIMER("SYSTEM:report_timing_info")
     if (self%n_timed_routines == 0) then; STOP_TIMER("SYSTEM:report_timing_info") CHECK return; end if
     if (tonto_parallel%rank > 0 AND tonto_parallel%do_parallel) then; STOP_TIMER("SYSTEM:report_timing_info") CHECK return; end if
     ! Process arguments ...
     unit = tonto%error_output_unit
     if (present(out)) unit = out%unit
     full = TRUE
     if (present(full_report)) full = full_report
     if (full) then; n_routine = self%n_timed_routines
     else;           n_routine = min(20,self%n_timed_routines)
     end if
     ! Set any negative times to zero and sort
     total_time = ZERO
     do i = 1,self%n_timed_routines
        total = self%time_for_routine(i)
        if (total<ZERO) self%time_for_routine(i) = ZERO 
        total_time = total_time + total
     end do
     allocate(order(self%n_timed_routines))
     call quick_sort_decreasing_(self,self%time_for_routine,order)
     write(unit,*)
     write(unit,'(a)') "Routine call stack:"
     write(unit,*)
     write(unit,'("   Call   ",a43,a7,a7)') "   Time","% total"
     do i = 1,n_routine
        j = order(i) 
        write(unit,'(3x,i3,".",3x,a43,f7.3,f7.3)') &
                         i, &
                         adjustl(self%name_for_routine(i)), &
                         self%time_for_routine(i), &
                         self%time_for_routine(i)*100d0/total_time
     end do
     deallocate(order)
     call flush_buffer_(self)
     STOP_TIMER("SYSTEM:report_timing_info")
      CHECK
   end subroutine

   subroutine flush_buffer(self,unit)
    SYSTEM :: self
   ! Flush the output
     INT, IN, optional :: unit
     INT :: f_unit
     STACK("SYSTEM:flush_buffer")
     START_TIMER("SYSTEM:flush_buffer")
     if (present(unit)) then
       f_unit = unit
     else
       f_unit = tonto%error_output_unit
     end if
     if (tonto_parallel%rank == 0 OR (NOT tonto_parallel%do_parallel)) then
#ifdef FLUSH
       call flush(f_unit)
#endif
     end if
     STOP_TIMER("SYSTEM:flush_buffer")
      CHECK
   end subroutine

! ************************************
! These would be inherited if possible
! ************************************

   subroutine expand_real(self,stack,dim)
    SYSTEM :: self
   ! Expands "stack" by amount "dim". Contents are retained.
   ! Elements added are set to zero.
     REALVEC(:), PTR :: stack
     INT, IN :: dim
     REALVEC(:), PTR :: old_stack
     INT :: n
   STACK("SYSTEM:expand_real")
   START_TIMER("SYSTEM:expand_real")
   ENSURE(dim>0,"SYSTEM:expand_real ... cannot expand stack by less than 1")
     if (NOT associated(stack)) then
      ! write(*,*) "NOT associated, dim=",dim
        allocate(stack(dim))
        stack = ZERO
     else
        n = size(stack)
        old_stack => stack
      ! write(*,*) "Associated, copying, n=",n," dim=",dim
        nullify(stack)
        allocate(stack(n+dim))
        stack(1:n) = old_stack
        stack(n+1:n+dim) = ZERO
        deallocate(old_stack)
     end if
     STOP_TIMER("SYSTEM:expand_real")
      UNSTACK
   end subroutine

   subroutine expand_int(self,stack,dim)
    SYSTEM :: self
   ! Expands "stack" by amount "dim". Contents are retained.
   ! Elements added are set to zero.
     INTVEC(:), PTR :: stack
     INT, IN :: dim
     INTVEC(:), PTR :: old_stack
     INT :: n
   STACK("SYSTEM:expand_int")
   START_TIMER("SYSTEM:expand_int")
   ENSURE(dim>0,"SYSTEM:expand_int ... cannot expand stack by less than 1")
     if (NOT associated(stack)) then
        allocate(stack(dim))
        stack = 0
     else
        n = size(stack)
        old_stack => stack
        nullify(stack)
        allocate(stack(n+dim))
        stack(1:n) = old_stack
        stack(n+1:n+dim) = 0
        deallocate(old_stack)
     end if
     STOP_TIMER("SYSTEM:expand_int")
      UNSTACK
   end subroutine

   subroutine expand_str(self,stack,dim)
    SYSTEM :: self
   ! Expands "stack" by amount "dim". Contents are retained.
   ! Elements added are set to blank.
     STRVEC(STR_SIZE,:), PTR :: stack
     INT, IN :: dim
     STRVEC(STR_SIZE,:), PTR :: old_stack
     INT :: n
   STACK("SYSTEM:expand_str")
   START_TIMER("SYSTEM:expand_str")
   ENSURE(dim>0,"SYSTEM:expand_str ... cannot expand stack by less than 1")
     if (NOT associated(stack)) then
        allocate(stack(dim))
        stack = " "
     else
        n = size(stack)
        old_stack => stack
        nullify(stack)
        allocate(stack(n+dim))
        stack(1:n) = old_stack
        stack(n+1:n+dim) = " "
        deallocate(old_stack)
     end if
     STOP_TIMER("SYSTEM:expand_str")
      UNSTACK
   end subroutine

   recursive subroutine quick_sort_decreasing(self,vec,indices)
    SYSTEM :: self
   ! Return the indices which sort vector from largest to smallest, i.e. on
   ! return "vec(indices)" is sorted. NOTE: vec is *not* sorted.
      REALVEC(:) :: vec
      INTVEC(:), INOUT :: indices
      INTVEC(:), PTR :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n, i, ns, ne, nl
      REAL :: chosen
      STACK("SYSTEM:quick_sort_decreasing")
      START_TIMER("SYSTEM:quick_sort_decreasing")
      if (size(indices)<=1) then; STOP_TIMER("SYSTEM:quick_sort_decreasing") CHECK return; end if
      n = size(indices)
      allocate(list(n)); list = (/(i,i=1,n)/)
      chosen = vec(1)
      ns = count(vec>chosen)
      nl = count(vec<chosen)
      ne = n - ns - nl
      allocate(small(ns)); allocate(small_indices(ns))
      allocate(equal(ne)); allocate(equal_indices(ne))
      allocate(large(nl)); allocate(large_indices(nl))
      small = pack(list,vec >chosen) ! indices of large vec elements
      equal = pack(list,vec==chosen) ! indices of equal vec elements
      large = pack(list,vec <chosen) ! indices of small vec elements
      small_indices = indices(small)
      equal_indices = indices(equal)
      large_indices = indices(large)
      if (ns>1) call quick_sort_decreasing_(self,vec(small),small_indices)
      if (nl>1) call quick_sort_decreasing_(self,vec(large),large_indices)
      indices(1:ns)       = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:)   = large_indices
      deallocate(large_indices); deallocate(large)
      deallocate(equal_indices); deallocate(equal)
      deallocate(small_indices); deallocate(small)
      deallocate(list)
     STOP_TIMER("SYSTEM:quick_sort_decreasing")
      CHECK
   end subroutine

end
