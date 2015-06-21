!---------------------------------------------------------------------------
!
!  REALMAT5: 5 dimensional matrices
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
! $Id: realmat5.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT5_MODULE

#  include "realmat5.use"

   implicit none

#  include "macros"
#  include "realmat5.int"


contains

   subroutine create(self,dim1,dim2,dim3,dim4,dim5)
    REALMAT5(:,:,:,:,:) :: self
   ! Create a mat5 object
      PTR :: self
      INT :: dim1,dim2,dim3,dim4,dim5
      STACK("REALMAT5:create")
      START_TIMER("REALMAT5:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3,dim4,dim5))
      ADD_MEMORY(dim1*dim2*dim3*dim4*dim5*REAL_SIZE)
     STOP_TIMER("REALMAT5:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4,lb5,ub5)
    REALMAT5(:,:,:,:,:) :: self
   ! Create a mat5 object with the specified bounds
      PTR :: self
      INT :: lb1,lb2,lb3,lb4,lb5
      INT :: ub1,ub2,ub3,ub4,ub5
      STACK("REALMAT5:create_1")
      START_TIMER("REALMAT5:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4,lb5:ub5))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*(ub4-lb4+1)*(ub5-lb5+1)*REAL_SIZE)
     STOP_TIMER("REALMAT5:create_1")
      UNSTACK
   end subroutine

   subroutine create_copy(self,m)
    REALMAT5(:,:,:,:,:) :: self
   ! Create a copy of matrix "m"
      PTR :: self
       REALMAT5(:,:,:,:,:), PTR :: m
      STACK("REALMAT5:create_copy")
      START_TIMER("REALMAT5:create_copy")
      call create_(self,lbound(m,1),ubound(m,1), &
              lbound(m,2),ubound(m,2), &
              lbound(m,3),ubound(m,3), &
              lbound(m,4),ubound(m,4), &
              lbound(m,5),ubound(m,5)  )
      self = m
     STOP_TIMER("REALMAT5:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REALMAT5(:,:,:,:,:) :: self
   ! Destroy a mat5 object
      PTR :: self
      STACK("REALMAT5:destroy")
      START_TIMER("REALMAT5:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REALMAT5:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*REAL_SIZE)
      deallocate(self)
     STOP_TIMER("REALMAT5:destroy")
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
