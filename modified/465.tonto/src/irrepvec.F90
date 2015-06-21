!---------------------------------------------------------------------------
!
! IRREPVEC: For representing an array of irreps
!
! Copyright (C) Daniel Grimwood, 1999
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
! $Id: irrepvec.foo,v 1.4.2.1 2003/03/24 01:28:52 dylan Exp $
!---------------------------------------------------------------------------

module IRREPVEC_MODULE

#  include "irrepvec.use"

   implicit none

#  include "macros"
#  include "irrepvec.int"


contains

   subroutine create(self,dim)
    IRREPVEC(:) :: self
   ! Create an irrepvec
      PTR :: self
      INT :: dim
      STACK("IRREPVEC:create")
      START_TIMER("IRREPVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*IRREP_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("IRREPVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    IRREPVEC(:) :: self
   ! Destroy an irrepvec
      PTR :: self
      STACK("IRREPVEC:destroy")
      START_TIMER("IRREPVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("IRREPVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*IRREP_SIZE)
      deallocate(self)
     STOP_TIMER("IRREPVEC:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    IRREPVEC(:) :: self
   ! Nullify the pointer parts of an irrepvec
       INT :: i
      STACK("IRREPVEC:nullify_ptr_part")
      START_TIMER("IRREPVEC:nullify_ptr_part")
      do i = 1,size(self)
         call nullify_ptr_part_(self(i))
      end do
     STOP_TIMER("IRREPVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    IRREPVEC(:) :: self
   ! Destroy the pointer parts of an irrepvec
       INT :: i
      STACK("IRREPVEC:destroy_ptr_part")
      START_TIMER("IRREPVEC:destroy_ptr_part")
      do i = 1,size(self)
         call destroy_ptr_part_(self(i))
      end do
     STOP_TIMER("IRREPVEC:destroy_ptr_part")
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
    IRREPVEC(:) :: self
   ! Create a replica copy of "vec".
      IRREPVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("IRREPVEC:create_copy")
      START_TIMER("IRREPVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("IRREPVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    IRREPVEC(:) :: self
   ! Copy "vec".
      IRREPVEC(:), IN :: vec
   ! The following code is inherited from OBJECTVEC
      INT :: a
      STACK("IRREPVEC:copy")
      START_TIMER("IRREPVEC:copy")
      ENSURE(size(self)==size(vec),"IRREPVEC:copy ... vec size does not match")
      do a = 1,size(vec)
        call copy_(self(a),vec(a))
      end do
     STOP_TIMER("IRREPVEC:copy")
      UNSTACK
   end subroutine

end
