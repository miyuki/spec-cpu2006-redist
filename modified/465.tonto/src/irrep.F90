!---------------------------------------------------------------------------
!
! IRREP: For representing an irreducible representation of a pointgroup
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
! $Id: irrep.foo,v 1.6 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module IRREP_MODULE

#  include "irrep.use"

   implicit none

#  include "macros"
#  include "irrep.int"


contains

   subroutine create(self)
    IRREP :: self
   ! Create an irrep
      PTR :: self
      STACK("IRREP:create")
      START_TIMER("IRREP:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(IRREP_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("IRREP:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    IRREP :: self
   ! Destroy an irrep
      PTR :: self
      STACK("IRREP:destroy")
      START_TIMER("IRREP:destroy")
      if (NOT associated(self)) then; STOP_TIMER("IRREP:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(IRREP_SIZE)
      deallocate(self)
     STOP_TIMER("IRREP:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    IRREP :: self
   ! Nullify the pointer parts of an irrep
      STACK("IRREP:nullify_ptr_part")
      START_TIMER("IRREP:nullify_ptr_part")
      nullify(self%character)
      nullify(self%mat)
     STOP_TIMER("IRREP:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    IRREP :: self
   ! Destroy the pointer parts of an irrep
      STACK("IRREP:destroy_ptr_part")
      START_TIMER("IRREP:destroy_ptr_part")
      call destroy_(self%character)
      call destroy_(self%mat)
     STOP_TIMER("IRREP:destroy_ptr_part")
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

   subroutine create_copy(self,irrep)
    IRREP :: self
   ! Create a copy
      PTR :: self
      IRREP :: irrep
      STACK("IRREP:create_copy")
      START_TIMER("IRREP:create_copy")
      call create_(self)
      call copy_(self,irrep)
     STOP_TIMER("IRREP:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,irrep)
    IRREP :: self
   ! Copy an irrep
      IRREP :: irrep
      STACK("IRREP:copy")
      START_TIMER("IRREP:copy")
      self = irrep
      call nullify_ptr_part_(self)
      call create_copy_(self%character,irrep%character)
      call create_copy_(self%mat,irrep%mat)
     STOP_TIMER("IRREP:copy")
      UNSTACK
   end subroutine

end
