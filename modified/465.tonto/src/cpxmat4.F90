!---------------------------------------------------------------------------
!
!  CPXMAT4: Complex 4 dimensional matrices
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
! $Id: cpxmat4.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module CPXMAT4_MODULE

#  include "cpxmat4.use"

   implicit none

#  include "macros"
#  include "cpxmat4.int"


contains

   subroutine create(self,dim1,dim2,dim3,dim4)
    CPXMAT4(:,:,:,:) :: self
   ! Create a cmat4 with the specified dimensions
      PTR :: self
      INT :: dim1,dim2,dim3,dim4
      STACK("CPXMAT4:create")
      START_TIMER("CPXMAT4:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3,dim4))
      ADD_MEMORY(dim1*dim2*dim3*dim4*CPX_SIZE)
     STOP_TIMER("CPXMAT4:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4)
    CPXMAT4(:,:,:,:) :: self
   ! Create a cmat4 with the specified bounds
      PTR :: self
      INT, IN :: ub1,ub2,ub3,ub4
      INT, IN :: lb1,lb2,lb3,lb4
      STACK("CPXMAT4:create_1")
      START_TIMER("CPXMAT4:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*(ub4-lb4+1)*CPX_SIZE)
     STOP_TIMER("CPXMAT4:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2,bounds3,bounds4)
    CPXMAT4(:,:,:,:) :: self
   ! Create a cmat4 with the specified bounds for each dimension
      PTR :: self
      INTVEC(2), IN :: bounds1,bounds2,bounds3,bounds4
      STACK("CPXMAT4:create_2")
      START_TIMER("CPXMAT4:create_2")
      call create_(self,bounds1(1),bounds1(2), bounds2(1),bounds2(2), &
              bounds3(1),bounds3(2), bounds4(1),bounds4(2))
     STOP_TIMER("CPXMAT4:create_2")
      UNSTACK
   end subroutine

   subroutine create_3(self,bounds)
    CPXMAT4(:,:,:,:) :: self
   ! Create a cmat4 with the specified bounds for all dimensions
      PTR :: self
      INTMAT(4,2), IN :: bounds
      STACK("CPXMAT4:create_3")
      START_TIMER("CPXMAT4:create_3")
      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2), &
              bounds(3,1),bounds(3,2),bounds(4,1),bounds(4,2))
     STOP_TIMER("CPXMAT4:create_3")
      UNSTACK
   end subroutine

   subroutine create_copy(self,c)
    CPXMAT4(:,:,:,:) :: self
   ! Create a copy of matrix "c"
      PTR :: self
       CPXMAT4(:,:,:,:), PTR :: c
      STACK("CPXMAT4:create_copy")
      START_TIMER("CPXMAT4:create_copy")
      call create_(self,lbound(c,1),ubound(c,1), &
              lbound(c,2),ubound(c,2), &
              lbound(c,3),ubound(c,3), &
              lbound(c,4),ubound(c,4)  )
      self = c
     STOP_TIMER("CPXMAT4:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CPXMAT4(:,:,:,:) :: self
   ! Destroy a cmat4
      PTR :: self
      STACK("CPXMAT4:destroy")
      START_TIMER("CPXMAT4:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CPXMAT4:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*CPX_SIZE)
      deallocate(self)
     STOP_TIMER("CPXMAT4:destroy")
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
