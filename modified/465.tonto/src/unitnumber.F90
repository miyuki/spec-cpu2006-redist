!---------------------------------------------------------------------------
!
! UNITNUMBER:  Keeps a list of the unit numbers used for I/O.
!
! Whenever you use a new unit, use "get" to get a unique number for it.
! When finished with the unit, use "free" so that the number becomes
! available for later use.
!
! The numbers start at UNITNUMBER_STARTING_UNIT, not one.
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
! $Id: unitnumber.foo,v 1.10.2.1 2003/11/13 05:33:02 reaper Exp $
!
!---------------------------------------------------------------------------

module UNITNUMBER_MODULE

#  include "unitnumber.use"

   implicit none

#  include "macros"
#  include "unitnumber.int"


   INTVEC(:), PTR :: unitlist

contains

   subroutine get(self,unit)
    UNITNUMBER :: self
   ! Returns the next unused unit number, and updates the unitlist.
     INT, OUT :: unit
     INTVEC(:), PTR :: old
     INT :: oldlength
     STACK("UNITNUMBER:get")
     START_TIMER("UNITNUMBER:get")
     if (associated(unitlist)) then
       oldlength=size(unitlist)
       unit=UNITNUMBER_STARTING_UNIT-1
       do
         unit=unit+1
         if (any(unitlist == unit)) then
           cycle
         else
           exit
         end if
       end do
       ! expand the array by one.
       old => unitlist
       nullify(unitlist)
       allocate(unitlist(oldlength+1))
       unitlist(1:oldlength) = old
       unitlist(oldlength+1) = unit
     else
       allocate(unitlist(1))
       unit=UNITNUMBER_STARTING_UNIT
       unitlist(1)=unit
     end if
     STOP_TIMER("UNITNUMBER:get")
      UNSTACK
   end subroutine

   subroutine free(self,unit)
    UNITNUMBER :: self
   ! Removes the unit number from the unitlist.
     INT, IN :: unit
     INTVEC(:), PTR :: old
     INT :: oldlength,position,i
     STACK("UNITNUMBER:free")
     START_TIMER("UNITNUMBER:free")
     if (unit<UNITNUMBER_STARTING_UNIT) then; STOP_TIMER("UNITNUMBER:free") UNSTACK return; end if
     oldlength=size(unitlist)
     ENSURE(oldlength>0,"UNITNUMBER:free ... no unitnumber array")
     if (oldlength==1) then
       deallocate(unitlist)
     else
       do i=1,oldlength
         if (unitlist(i)==unit) then
           position=i
           exit
         else
           cycle
         end if
       end do
       do i=position,oldlength-1
         unitlist(i)=unitlist(i+1)
       end do
       ! shrink the array by 1.
       old => unitlist
       nullify(unitlist)
       allocate(unitlist(oldlength-1))
       unitlist = old(1:oldlength-1)
     end if
     STOP_TIMER("UNITNUMBER:free")
      UNSTACK
   end subroutine

   subroutine flush_buffer(self)
    UNITNUMBER :: self
   ! Flush the buffer if need be.
     STACK("UNITNUMBER:flush_buffer")
     START_TIMER("UNITNUMBER:flush_buffer")
     call flush_buffer_(tonto,self%unit)
     STOP_TIMER("UNITNUMBER:flush_buffer")
      CHECK
   end subroutine

end
