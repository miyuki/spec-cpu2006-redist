!-------------------------------------------------------------------------------
!
! SHELLPAIRVEC : a vector of pairs of gaussian SHELLPAIR's.
!
! This object is used to store precalculated information used in ERI integral
! evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
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
! $Id: shellpairvec.foo,v 1.2.2.1 2003/03/24 01:28:54 dylan Exp $
!-------------------------------------------------------------------------------

module SHELLPAIRVEC_MODULE

#  include "shellpairvec.use"

   implicit none

#  include "macros"
#  include "shellpairvec.int"


contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self,dim)
    SHELLPAIRVEC(:) :: self
   ! Create space for a the shellpair vector
      PTR :: self
      INT, IN :: dim
      STACK("SHELLPAIRVEC:create")
      START_TIMER("SHELLPAIRVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*SHELLPAIR_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("SHELLPAIRVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SHELLPAIRVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SHELLPAIRVEC:destroy")
      START_TIMER("SHELLPAIRVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SHELLPAIRVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*SHELLPAIR_SIZE)
      deallocate(self)
     STOP_TIMER("SHELLPAIRVEC:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SHELLPAIRVEC(:) :: self
   ! Nullify the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SHELLPAIRVEC:nullify_ptr_part")
      START_TIMER("SHELLPAIRVEC:nullify_ptr_part")
      do a = 1,size(self)
         call nullify_ptr_part_(self(a))
      end do
     STOP_TIMER("SHELLPAIRVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SHELLPAIRVEC(:) :: self
   ! Destroy the pointer parts of self
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SHELLPAIRVEC:destroy_ptr_part")
      START_TIMER("SHELLPAIRVEC:destroy_ptr_part")
      do a = 1,size(self)
        call destroy_ptr_part_(self(a))
      end do
     STOP_TIMER("SHELLPAIRVEC:destroy_ptr_part")
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

   subroutine create_copy(self,vec)
    SHELLPAIRVEC(:) :: self
   ! Create a replica copy of "vec".
      SHELLPAIRVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("SHELLPAIRVEC:create_copy")
      START_TIMER("SHELLPAIRVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("SHELLPAIRVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    SHELLPAIRVEC(:) :: self
   ! Copy "vec".
      SHELLPAIRVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("SHELLPAIRVEC:copy")
      START_TIMER("SHELLPAIRVEC:copy")
      ENSURE(size(self)==size(vec),"SHELLPAIRVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("SHELLPAIRVEC:copy")
      UNSTACK
   end subroutine

end
