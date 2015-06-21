!---------------------------------------------------------------------------
!
!  INTVECMAT3 : a 3-D matrix where each element is a vector of (possibly)
!  a different length
!
! Copyright (C) Dylan Jayatilaka, 2002
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
! $Id: intvecmat3.foo,v 1.2 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module INTVECMAT3_MODULE

#  include "intvecmat3.use"

   implicit none

#  include "macros"
#  include "intvecmat3.int"


contains

   subroutine create(self,dim1,dim2,dim3)
    INTVECMAT3(:,:,:) :: self
   ! Create space
      PTR :: self
      INT :: dim1,dim2,dim3
      STACK("INTVECMAT3:create")
      START_TIMER("INTVECMAT3:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3))
      ADD_MEMORY(dim1*dim2*dim3*PTR_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("INTVECMAT3:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,dim1,dim2,dim3,dimv)
    INTVECMAT3(:,:,:) :: self
   ! Create space
      PTR :: self
      INT :: dim1,dim2,dim3,dimv
      STACK("INTVECMAT3:create_1")
      START_TIMER("INTVECMAT3:create_1")
      nullify(self)
      allocate(self(dim1,dim2,dim3))
      ADD_MEMORY(dim1*dim2*dim3*PTR_SIZE)
      call create_vec_(self,dimv)
     STOP_TIMER("INTVECMAT3:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,dim1,dim2,dim3,dimv)
    INTVECMAT3(:,:,:) :: self
   ! Create space
      PTR :: self
      INT :: dim1,dim2,dim3
      INTVEC(2) :: dimv
      STACK("INTVECMAT3:create_2")
      START_TIMER("INTVECMAT3:create_2")
      nullify(self)
      allocate(self(dim1,dim2,dim3))
      ADD_MEMORY(dim1*dim2*dim3*PTR_SIZE)
      call create_vec_(self,dimv)
     STOP_TIMER("INTVECMAT3:create_2")
      UNSTACK
   end subroutine

   subroutine create_3(self,dim)
    INTVECMAT3(:,:,:) :: self
   ! Create space
      PTR :: self
      INTVEC(3) :: dim
      STACK("INTVECMAT3:create_3")
      START_TIMER("INTVECMAT3:create_3")
      nullify(self)
      allocate(self(dim(1),dim(2),dim(3)))
      ADD_MEMORY(dim(1)*dim(2)*dim(3)*PTR_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("INTVECMAT3:create_3")
      UNSTACK
   end subroutine

   subroutine create_vec(self,dimv)
    INTVECMAT3(:,:,:) :: self
   ! Allocate the "vec" pointer parts
      INT :: dimv
      INT :: i,j,k
      STACK("INTVECMAT3:create_vec")
      START_TIMER("INTVECMAT3:create_vec")
      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         call create_(self(i,j,k)%element,dimv)
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:create_vec")
      UNSTACK
   end subroutine

   subroutine create_vec_1(self,dimv)
    INTVECMAT3(:,:,:) :: self
   ! Allocate the "vec" pointer parts
      INTVEC(2) :: dimv
      INT :: i,j,k
      STACK("INTVECMAT3:create_vec_1")
      START_TIMER("INTVECMAT3:create_vec_1")
      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         call create_(self(i,j,k)%element,dimv)
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:create_vec_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTVECMAT3(:,:,:) :: self
   ! Destroy allocated space
      PTR :: self
      STACK("INTVECMAT3:destroy")
      START_TIMER("INTVECMAT3:destroy")
      if (NOT associated(self)) then; STOP_TIMER("INTVECMAT3:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*PTR_SIZE)
      deallocate(self)
     STOP_TIMER("INTVECMAT3:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    INTVECMAT3(:,:,:) :: self
   ! Nullify the pointer parts
      PTR :: self
      INT :: i,j,k
      STACK("INTVECMAT3:nullify_ptr_part")
      START_TIMER("INTVECMAT3:nullify_ptr_part")
      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         nullify(self(i,j,k)%element)
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    INTVECMAT3(:,:,:) :: self
   ! Destroy the pointer parts of an ivecvec
      PTR :: self
      INT :: i,j,k
      STACK("INTVECMAT3:destroy_ptr_part")
      START_TIMER("INTVECMAT3:destroy_ptr_part")
      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         call destroy_(self(i,j,k)%element)
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:destroy_ptr_part")
      UNSTACK
   end subroutine

!   created result (res)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result (res)
!   ! Returns true if self has *not* been created
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   subroutine create_copy(self,v)
    INTVECMAT3(:,:,:) :: self
   ! Create a copy of "v"
      PTR :: self
      INTVECMAT3(:,:,:) :: v
      STACK("INTVECMAT3:create_copy")
      START_TIMER("INTVECMAT3:create_copy")
      call create_(self,size(v,1),size(v,2),size(v,3))
      call copy_(self,v)
     STOP_TIMER("INTVECMAT3:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,v)
    INTVECMAT3(:,:,:) :: self
   ! Make a copy of "v"
      INTVECMAT3(:,:,:) :: v
      INT :: i,j,k
      STACK("INTVECMAT3:copy")
      START_TIMER("INTVECMAT3:copy")
      do i = 1,size(v,1)
      do j = 1,size(v,2)
      do k = 1,size(v,3)
         call create_copy_(self(i,j,k)%element,v(i,j,k)%element)
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:copy")
      UNSTACK
   end subroutine

   subroutine set_to(self,v)
    INTVECMAT3(:,:,:) :: self
   ! Set self to "v". Up to you to make sure they are compatible!
      INOUT :: self
      INTVECMAT3(:,:,:), IN :: v
      INT :: i,j,k
      STACK("INTVECMAT3:set_to")
      START_TIMER("INTVECMAT3:set_to")
      do i = 1,size(v,1)
      do j = 1,size(v,2)
      do k = 1,size(v,3)
         call set_to_(self(i,j,k)%element,v(i,j,k)%element)
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:set_to")
      CHECK
   end subroutine

   subroutine zero(self)
    INTVECMAT3(:,:,:) :: self
   ! Zero all elements
      INOUT :: self
      INT :: i,j,k
      STACK("INTVECMAT3:zero")
      START_TIMER("INTVECMAT3:zero")
      do i = 1,size(self,1)
      do j = 1,size(self,2)
      do k = 1,size(self,3)
         self(i,j,k)%element = 0
      end do
      end do
      end do
     STOP_TIMER("INTVECMAT3:zero")
      CHECK
   end subroutine

   function dim1(self) result(res)
    INTVECMAT3(:,:,:) :: self
   ! The first dimension of self
      INT :: res
      STACK("INTVECMAT3:dim1")
      START_TIMER("INTVECMAT3:dim1")
      res = size(self,1)
     STOP_TIMER("INTVECMAT3:dim1")
      CHECK
   end function

   function dim2(self) result(res)
    INTVECMAT3(:,:,:) :: self
   ! The second dimension of self
      INT :: res
      STACK("INTVECMAT3:dim2")
      START_TIMER("INTVECMAT3:dim2")
      res = size(self,2)
     STOP_TIMER("INTVECMAT3:dim2")
      CHECK
   end function

   function dim3(self) result(res)
    INTVECMAT3(:,:,:) :: self
   ! The third dimension of self
      INT :: res
      STACK("INTVECMAT3:dim3")
      START_TIMER("INTVECMAT3:dim3")
      res = size(self,3)
     STOP_TIMER("INTVECMAT3:dim3")
      CHECK
   end function

end
