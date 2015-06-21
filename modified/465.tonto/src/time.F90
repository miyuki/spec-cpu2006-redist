!---------------------------------------------------------------------------
!
!  Time: timing routines, such as output date, time execution, etc.
!
! Copyright (C) Daniel Grimwood, 1998
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
! $Id: time.foo,v 1.16.2.5 2003/11/13 05:33:55 reaper Exp $
!---------------------------------------------------------------------------

module TIME_MODULE

#  include "time.use"

   implicit none

#  include "macros"
#  include "time.int"


#ifdef MPI
#include "mpif.h"
#endif

   TIME, public :: std_time

   public reset_; interface reset_
    module procedure start
  end interface

contains

   subroutine start(self)
    TIME :: self
   ! starts the counter
     STACK("TIME:start")
     START_TIMER("TIME:start")
     self%started   = current_(self)
     self%start_cpu = current_cpu_time_(self)
     self%stopped = 0
     STOP_TIMER("TIME:start")
      CHECK
   end subroutine

   subroutine stop(self)
    TIME :: self
   ! Stop the counter.
     STACK("TIME:stop")
     START_TIMER("TIME:stop")
     self%stopped  = current_(self)
     self%stop_cpu = current_cpu_time_(self)
     STOP_TIMER("TIME:stop")
      CHECK
   end subroutine

   function current(self) result(res)
    TIME :: self
   ! returns current time
     INTVEC(5) :: res
     INTVEC(8) :: time
     INT :: y,m,d,j
STACK("TIME:current")
START_TIMER("TIME:current")
#ifdef MPI
     call MPI_BARRIER(MPI_COMM_WORLD,tonto_parallel%mpi_status)
#endif
     call date_and_time(values=time)
     y = time(1)
     m = time(2)
     d = time(3)
     call to_julian_(self,d,m,y,j)
     res(1) = j       ! Julian day
     res(2) = time(5) ! hour
     res(3) = time(6) ! minute
     res(4) = time(7) ! second
     res(5) = time(8) ! millisecond
     STOP_TIMER("TIME:current")
      CHECK
   end function

   function current_cpu_time(self) result(res)
    TIME :: self
   ! What the current CPU_TIME is.
     REAL :: res
     REAL :: time
     STACK("TIME:current_cpu_time")
     START_TIMER("TIME:current_cpu_time")
     time = ZERO
#ifdef MPI
     call MPI_BARRIER(MPI_COMM_WORLD,tonto_parallel%mpi_status)
#endif
#ifndef LANGUAGE_FORTRAN_90
     call cpu_time(time)
#endif
     res = time
     STOP_TIMER("TIME:current_cpu_time")
      CHECK
   end function

   function elapsed(self) result(time)
    TIME :: self
   ! returns elapsed time
     INTVEC(5) :: time
     STACK("TIME:elapsed")
     START_TIMER("TIME:elapsed")
     time = subtract_times_(self, self%stopped , self%started)
     STOP_TIMER("TIME:elapsed")
      CHECK
   end function

   function elapsed_time_in_seconds(self) result(secs)
    TIME :: self
   ! returns elapsed time
     REAL :: secs
     INTVEC(5) :: time
     STACK("TIME:elapsed_time_in_seconds")
     START_TIMER("TIME:elapsed_time_in_seconds")
     time = subtract_times_(self, self%stopped , self%started)
     secs = time_to_seconds_(self,time)
     STOP_TIMER("TIME:elapsed_time_in_seconds")
      CHECK
   end function

! *********************************************************************
! return time strings.
! *********************************************************************

   function current_time(self) result(res)
    TIME :: self
   !
     STR(40) :: res
     STACK("TIME:current_time")
     START_TIMER("TIME:current_time")
     res = "The current time is " // time_to_str_(self, current_(self) ) // "."
     STOP_TIMER("TIME:current_time")
      CHECK
   end function

   function start_time(self) result(res)
    TIME :: self
   !
     STR(37) :: res
     STACK("TIME:start_time")
     START_TIMER("TIME:start_time")
     !SPEC res = "Timer started at " // time_to_str_(self, self%started ) // "."
     res = "Timer started at XX" // "."
     STOP_TIMER("TIME:start_time")
      CHECK
   end function

   function stop_time(self) result(res)
    TIME :: self
   !
     STR(37) :: res
     STACK("TIME:stop_time")
     START_TIMER("TIME:stop_time")
     res = "Timer stopped at " // time_to_str_(self, self%stopped ) // "."
     STOP_TIMER("TIME:stop_time")
      CHECK
   end function

   function time_taken(self,task) result(res)
    TIME :: self
   ! Returns time taken. "Time taken is ..."
   ! If task given, "Time taken for "task" is ..."
     STR(132) :: res
     STR(*), optional :: task
     STACK("TIME:time_taken")
     START_TIMER("TIME:time_taken")
     call stop_(self)
     if (present(task)) then
       !SPEC res = "Wall-clock time taken for " // trim(task) // " is " // &
       !SPEC       trim( elapsed_time_to_str_(self, elapsed_(self) )) // "."
       res = "Wall-clock time taken for " // trim(task) // " is " // &
              "XX."
     else
     res = "Wall-clock time taken is " // trim( elapsed_time_to_str_(self, elapsed_(self) )) // "."
     end if
     STOP_TIMER("TIME:time_taken")
      CHECK
   end function

   function cpu_time_taken(self,task) result(res)
    TIME :: self
   ! Returns cpu time taken. "CPU time taken is ... CPU seconds."
   ! If task given, "CPU time taken for "task" is ... CPU seconds."
     STR(*), optional :: task
     STR(STR_SIZE) :: res
     REAL :: time,time1
     STR(132) :: time_str
     STACK("TIME:cpu_time_taken")
     START_TIMER("TIME:cpu_time_taken")
     call stop_(self)
     time = self%stop_cpu - self%start_cpu
#ifdef MPI
     call MPI_ALLREDUCE(time,time1,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,tonto_parallel%mpi_status)
     time = time1
#endif
     write(time_str,'(f15.3)') time
     if (present(task)) then
       !SPEC res = "CPU time taken for " // trim(task) // " is " // &
       !SPEC       trim(adjustl(time_str)) // " CPU seconds."
       res = "CPU time taken for " // trim(task) // " is " // &
               "XX CPU seconds."
     else
       res = "CPU time taken is " // trim(adjustl(time_str)) // " CPU seconds."
     end if
     STOP_TIMER("TIME:cpu_time_taken")
      CHECK
   end function

   function elapsed_time_to_str(self,time) result(res)
    TIME :: self
   ! Returns the elapsed time as a string.
   ! Formatted as years, months, days, hours, min, sec, msec.
   ! If time is greater than a day, does not output min or smaller.
     INTVEC(5), IN :: time
     STR(STR_SIZE) :: res
     BIN :: comma
     STACK("TIME:elapsed_time_to_str")
     START_TIMER("TIME:elapsed_time_to_str")
     comma = FALSE
     res = " "
     res =              number_with_units_(self,time(1),"day",comma)
     res = trim(res) // number_with_units_(self,time(2),"hour",comma)
     if (time(1) == 0) then
       res = trim(res) // number_with_units_(self,time(3),"minute",comma)
       if (time(2) == 0) then
         res = trim(res) // number_with_units_(self,time(4),"second",comma)
         res = trim(res) // number_with_units_(self,time(5),"millisecond",comma)
       end if
     end if
     STOP_TIMER("TIME:elapsed_time_to_str")
      CHECK
   end function

   function time_to_str(self,time) result(res)
    TIME :: self
   ! Return a string containing the time.
   ! Formatted as DD/MM/YYYY HH:MM:SS
     INTVEC(5), IN :: time
     STR(19) :: res
     STR(2) :: day,month,hour,min,sec
     STR(4) :: year
     INT :: i,y,m,d
     STACK("TIME:time_to_str")
     START_TIMER("TIME:time_to_str")
     call from_julian_(self,d,m,y,time(1))
     res = " "
     write(day,fmt='(i2)') d
     write(month,fmt='(i2)') m
     write(year,fmt='(i4)') y
     write(hour,fmt='(i2)') time(2)
     write(min,fmt='(i2)') time(3)
     write(sec,fmt='(i2)') time(4)
     res = day //"/"// month //"/"// year //" "// hour //":"// min //":"// sec
     do i=1,19
       if (res(i:i) == " ") res(i:i) = "0"
     end do
     res(11:11) = " "
     STOP_TIMER("TIME:time_to_str")
      CHECK
   end function

! *********************************************************************
! private routines
! *********************************************************************

   function number_with_units(self,number,unit,comma) result(res)
    TIME :: self
   ! Returns the number with its units, if the number is non-zero.
   ! Inserts a comma if comma is true.  If the number is non-zero,
   ! then comma is set to true.
     IN :: self
     INT, IN :: number
     STR(*), IN :: unit
     BIN :: comma
     STR(STR_SIZE) :: res
     INT :: i
     STR(STR_SIZE) :: res1
     STACK("TIME:number_with_units")
     START_TIMER("TIME:number_with_units")
     res = " "
     if (number /= 0) then
       res1 = " "
       write(res1,'(i 20)') number
       res1 = adjustl(res1)
       i = 1
       if (comma) then           ! insert comma at start if needed.
         res(1:2) = ", "
         i = 3
       end if
       res(i:) = trim(res1) // " " // unit
       if (number /= 1) then     ! add "s" to "unit"s if number not 1.
         i=len_trim(res)+1
         res(i:i) = "s"
       end if
       comma = TRUE
     end if
     STOP_TIMER("TIME:number_with_units")
      CHECK
   end function

   function subtract_times(self,time1,time2) result(res)
    TIME :: self
   ! result = time1 - time2
     IN :: self
     INTVEC(5), IN :: time1,time2
     INTVEC(5) :: res
     REAL :: secs,secs1,secs2
     STACK("TIME:subtract_times")
     START_TIMER("TIME:subtract_times")
     secs1 = time_to_seconds_(self,time1)
     secs2 = time_to_seconds_(self,time2)
     secs = secs1 - secs2
     res = seconds_to_time_(self,secs)
     STOP_TIMER("TIME:subtract_times")
      CHECK
   end function

   function seconds_to_time(self,secs) result(res)
    TIME :: self
   ! Returns the number of days, hours, minutes, seconds, milliseconds from the
   ! number of seconds.
     IN :: self
     INTVEC(5) :: res
     REAL, IN :: secs
     REAL :: sex
     STACK("TIME:seconds_to_time")
     START_TIMER("TIME:seconds_to_time")
     sex = abs(secs)
     res(1) = floor( sex / 86400 ) ! days
     sex = sex - res(1) * 86400
     res(2) = floor( sex / 3600 )  ! hours
     sex = sex - res(2) * 3600
     res(3) = floor( sex / 60 )    ! minutes
     sex = sex - res(3) * 60
     res(4) = floor(sex)           ! seconds
     sex = sex - res(4)
     res(5) = 1000*sex             ! milliseconds
     if (secs < 0) res = - res
     STOP_TIMER("TIME:seconds_to_time")
      CHECK
   end function

   function time_to_seconds(self,time) result(res)
    TIME :: self
   ! Returns the number of seconds from the days, hours, minutes, seconds, and
   ! milliseconds.
     IN :: self
     INTVEC(5), IN :: time
     REAL :: res
     STACK("TIME:time_to_seconds")
     START_TIMER("TIME:time_to_seconds")
     res = 86400d0*time(1)+3600d0*time(2)+60d0*time(3)+time(4)+time(5)/1000d0
     STOP_TIMER("TIME:time_to_seconds")
      CHECK
   end function

   subroutine to_julian(self,d,m,y,j)
    TIME :: self
   ! Converts the days, months, years, into the Julian date.
     IN :: self
     INT, IN :: d,m,y
      INT, OUT :: j
     INT :: mm,yy,c
     STACK("TIME:to_julian")
     START_TIMER("TIME:to_julian")
     mm = m
     yy = y
     if (mm>2) then
       mm = mm - 3
     else
       mm = mm + 9
       yy = yy - 1
     end if
     c = yy / 100
     yy = yy - 100 * c
     j = (146097*c)/4 + (1461*yy)/4 + (153*mm+2)/5 + d + 1721119
     STOP_TIMER("TIME:to_julian")
      CHECK
   end subroutine

   subroutine from_julian(self,d,m,y,j)
    TIME :: self
   ! Converts the days, months, years, from the Julian date.
     IN :: self
      INT, IN :: j
     INT, OUT :: d,m,y
     INT :: jj
     STACK("TIME:from_julian")
     START_TIMER("TIME:from_julian")
     jj=j - 1721119
     y = (4*jj-1)/146097
     jj = 4*jj - 1 - 146097 * y
     d = jj/4
     jj = (4*d+3)/1461
     d = 4*d + 3 - 1461*jj
     d = (d+4)/4
     m = (5*d-3)/153
     d = 5*d - 3 - 153*m
     d = (d+5)/5
     y = 100*y + jj
     if (m<10) then
       m = m + 3
     else
       m = m - 9
       y = y + 1
     end if
     STOP_TIMER("TIME:from_julian")
      CHECK
   end subroutine

end
