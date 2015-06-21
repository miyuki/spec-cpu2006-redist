!---------------------------------------------------------------------------
!
!  CPXMAT3: Complex 3 dimensional matrices
!
! Copyright (C) dylan jayatilaka, 1998
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
! $Id: cpxmat3.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module CPXMAT3_MODULE

#  include "cpxmat3.use"

   implicit none

#  include "macros"
#  include "cpxmat3.int"


contains

   subroutine create(self,dim1,dim2,dim3)
    CPXMAT3(:,:,:) :: self
   ! Create a cmat3 with the specified dimensions
      PTR :: self
      INT :: dim1,dim2,dim3
      STACK("CPXMAT3:create")
      START_TIMER("CPXMAT3:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3))
      ADD_MEMORY(dim1*dim2*dim3*CPX_SIZE)
     STOP_TIMER("CPXMAT3:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
    CPXMAT3(:,:,:) :: self
   ! Create a cmat3 with the specified bounds
      PTR :: self
      INT :: ub1,ub2,ub3,lb1,lb2,lb3
      STACK("CPXMAT3:create_1")
      START_TIMER("CPXMAT3:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*CPX_SIZE)
     STOP_TIMER("CPXMAT3:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2,bounds3)
    CPXMAT3(:,:,:) :: self
   ! Create a cmat3 with the specified bounds for each dimension
      PTR :: self
      INTVEC(2) :: bounds1,bounds2,bounds3
      STACK("CPXMAT3:create_2")
      START_TIMER("CPXMAT3:create_2")
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2), &
             bounds3(1),bounds3(2))
     STOP_TIMER("CPXMAT3:create_2")
      UNSTACK
   end subroutine

   subroutine create_3(self,bounds)
    CPXMAT3(:,:,:) :: self
   ! Create a cmat3 with the specified bounds for all dimensions
      PTR :: self
      INTMAT(3,2) :: bounds
      STACK("CPXMAT3:create_3")
      START_TIMER("CPXMAT3:create_3")
      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2), &
            bounds(3,1),bounds(3,2))
     STOP_TIMER("CPXMAT3:create_3")
      UNSTACK
   end subroutine

   subroutine create_copy(self,c)
    CPXMAT3(:,:,:) :: self
   ! Create a copy of matrix "c"
      PTR :: self
       CPXMAT3(:,:,:), PTR :: c
      STACK("CPXMAT3:create_copy")
      START_TIMER("CPXMAT3:create_copy")
      call create_(self,lbound(c,1),ubound(c,1), &
              lbound(c,2),ubound(c,2), &
              lbound(c,3),ubound(c,3)  )
      self = c
     STOP_TIMER("CPXMAT3:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CPXMAT3(:,:,:) :: self
   ! Destroy a cmat3
      PTR :: self
      STACK("CPXMAT3:destroy")
      START_TIMER("CPXMAT3:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CPXMAT3:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*CPX_SIZE)
      deallocate(self)
     STOP_TIMER("CPXMAT3:destroy")
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

end
