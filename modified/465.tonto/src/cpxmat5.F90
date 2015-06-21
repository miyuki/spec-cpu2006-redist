!---------------------------------------------------------------------------
!
!  CPXMAT5: Complex 5 dimensional matrices
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
! $Id: cpxmat5.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module CPXMAT5_MODULE

#  include "cpxmat5.use"

   implicit none

#  include "macros"
#  include "cpxmat5.int"


contains

   subroutine create(self,dim1,dim2,dim3,dim4,dim5)
    CPXMAT5(:,:,:,:,:) :: self
   ! Create a cmat5 object
      PTR :: self
      INT :: dim1,dim2,dim3,dim4,dim5
      STACK("CPXMAT5:create")
      START_TIMER("CPXMAT5:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3,dim4,dim5))
      ADD_MEMORY(dim1*dim2*dim3*dim4*dim5*CPX_SIZE)
     STOP_TIMER("CPXMAT5:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4,lb5,ub5)
    CPXMAT5(:,:,:,:,:) :: self
   ! Create a cmat5 object with the specified bounds
      PTR :: self
      INT :: lb1,lb2,lb3,lb4,lb5
      INT :: ub1,ub2,ub3,ub4,ub5
      STACK("CPXMAT5:create_1")
      START_TIMER("CPXMAT5:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*(ub4-lb4+1)*(ub5-lb5+1)*CPX_SIZE)
     STOP_TIMER("CPXMAT5:create_1")
      UNSTACK
   end subroutine

   subroutine create_copy(self,c)
    CPXMAT5(:,:,:,:,:) :: self
   ! Create a copy of matrix "c"
      PTR :: self
       CPXMAT5(:,:,:,:,:), PTR :: c
      STACK("CPXMAT5:create_copy")
      START_TIMER("CPXMAT5:create_copy")
      call create_(self,lbound(c,1),ubound(c,1), &
              lbound(c,2),ubound(c,2), &
              lbound(c,3),ubound(c,3), &
              lbound(c,4),ubound(c,4), &
              lbound(c,5),ubound(c,5)  )
      self = c
     STOP_TIMER("CPXMAT5:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CPXMAT5(:,:,:,:,:) :: self
   ! Destroy a cmat5 object
      PTR :: self
      STACK("CPXMAT5:destroy")
      START_TIMER("CPXMAT5:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CPXMAT5:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*CPX_SIZE)
      deallocate(self)
     STOP_TIMER("CPXMAT5:destroy")
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
