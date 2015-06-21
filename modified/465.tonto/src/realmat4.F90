!---------------------------------------------------------------------------
!
!  REALMAT4: 4 dimensional matrices
!
! Copyright (C) Dylan Jayatilaka and Daniel Grimwood, 1998
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
! $Id: realmat4.foo,v 1.5 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT4_MODULE

#  include "realmat4.use"

   implicit none

#  include "macros"
#  include "realmat4.int"


contains

   subroutine create(self,dim1,dim2,dim3,dim4)
    REALMAT4(:,:,:,:) :: self
   ! Create a 4-d array
     PTR :: self
     INT, IN :: dim1,dim2,dim3,dim4
     STACK("REALMAT4:create")
     START_TIMER("REALMAT4:create")
     nullify(self)
     allocate(self(dim1,dim2,dim3,dim4))
     ADD_MEMORY(dim1*dim2*dim3*dim4*REAL_SIZE)
     STOP_TIMER("REALMAT4:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,bounds)
    REALMAT4(:,:,:,:) :: self
   ! Create a 4-d array with all upper "bounds" specified
     PTR :: self
     INTVEC(4), IN :: bounds
     STACK("REALMAT4:create_1")
     START_TIMER("REALMAT4:create_1")
     call create_(self,bounds(1),bounds(2),bounds(3),bounds(4))
     STOP_TIMER("REALMAT4:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,lb1,ub1,lb2,ub2,lb3,ub3,lb4,ub4)
    REALMAT4(:,:,:,:) :: self
   ! Create a 4-d array with all bound specified
     PTR :: self
     INT, IN :: lb1,lb2,lb3,lb4,ub1,ub2,ub3,ub4
     STACK("REALMAT4:create_2")
     START_TIMER("REALMAT4:create_2")
     nullify(self)
     allocate(self(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4))
     ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*(ub4-lb4+1)*REAL_SIZE)
     STOP_TIMER("REALMAT4:create_2")
      UNSTACK
   end subroutine

   subroutine create_3(self,bounds1,bounds2,bounds3,bounds4)
    REALMAT4(:,:,:,:) :: self
   ! Create a 4-d array with all pair bounds specified
     PTR :: self
     INTVEC(2), IN :: bounds1,bounds2,bounds3,bounds4
     STACK("REALMAT4:create_3")
     START_TIMER("REALMAT4:create_3")
     call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2), &
             bounds3(1),bounds3(2),bounds4(1),bounds4(2))
     STOP_TIMER("REALMAT4:create_3")
      UNSTACK
   end subroutine

   subroutine create_4(self,bounds)
    REALMAT4(:,:,:,:) :: self
   ! Create a 4-d array with all bounds specified
     PTR :: self
     INTMAT(4,2), IN :: bounds
     STACK("REALMAT4:create_4")
     START_TIMER("REALMAT4:create_4")
     call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2), &
             bounds(3,1),bounds(3,2),bounds(4,1),bounds(4,2))
     STOP_TIMER("REALMAT4:create_4")
      UNSTACK
   end subroutine

   subroutine create_copy(self,m)
    REALMAT4(:,:,:,:) :: self
   ! Create a copy of matrix "m"
      PTR :: self
       REALMAT4(:,:,:,:), PTR :: m
      STACK("REALMAT4:create_copy")
      START_TIMER("REALMAT4:create_copy")
      call create_(self,lbound(m,1),ubound(m,1), &
              lbound(m,2),ubound(m,2), &
              lbound(m,3),ubound(m,3), &
              lbound(m,4),ubound(m,4)  )
      self = m
     STOP_TIMER("REALMAT4:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REALMAT4(:,:,:,:) :: self
   ! Destroy the object
      PTR :: self
      STACK("REALMAT4:destroy")
      START_TIMER("REALMAT4:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REALMAT4:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*REAL_SIZE)
      deallocate(self)
     STOP_TIMER("REALMAT4:destroy")
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

   subroutine plus_product(self,a,b)
    REALMAT4(:,:,:,:) :: self
   ! The product of "b" with scalar "a" is added to "self".
      REAL, IN :: a
      REALMAT4(:,:,:,:), IN :: b
     INT :: d1,d2,d3,d4,dim1,dim2,dim3,dim4
     STACK("REALMAT4:plus_product")
     START_TIMER("REALMAT4:plus_product")
     dim1=size(self,1)
     dim2=size(self,2)
     dim3=size(self,3)
     dim4=size(self,4)
   ENSURE(size(self,1)==dim1,"REALMAT4:plus_product ... array dimension mismatch")
   ENSURE(size(self,2)==dim2,"REALMAT4:plus_product ... array dimension mismatch")
   ENSURE(size(self,3)==dim3,"REALMAT4:plus_product ... array dimension mismatch")
   ENSURE(size(self,4)==dim4,"REALMAT4:plus_product ... array dimension mismatch")
     do d4=1,dim4
       do d3=1,dim3
         do d2=1,dim2
           do d1=1,dim1
             self(d1,d2,d3,d4) = self(d1,d2,d3,d4) + a * b(d1,d2,d3,d4)
           end do
         end do
       end do
     end do
     STOP_TIMER("REALMAT4:plus_product")
      CHECK
   end subroutine

end
