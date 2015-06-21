!---------------------------------------------------------------------------
!
!  REALMAT3VEC: Vector of REALMAT3 matrices
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
! $Id: realmat3vec.foo,v 1.4.2.1 2003/10/13 06:22:46 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT3VEC_MODULE

#  include "realmat3vec.use"

   implicit none

#  include "macros"
#  include "realmat3vec.int"


contains

   subroutine create(self,dim)
    REALMAT3VEC(:) :: self
   ! Create a mat3vec
      PTR :: self
      INT :: dim
      STACK("REALMAT3VEC:create")
      START_TIMER("REALMAT3VEC:create")
      allocate(self(dim))
      ADD_MEMORY(dim*PTR_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("REALMAT3VEC:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb,ub)
    REALMAT3VEC(:) :: self
   ! Create a mat3vec
      PTR :: self
      INT :: lb,ub
      STACK("REALMAT3VEC:create_1")
      START_TIMER("REALMAT3VEC:create_1")
      ENSURE(ub>=lb,"REALMAT3VEC:create_1 ... upper bound must be greater than or equal to lower bound")
      allocate(self(lb:ub))
      ADD_MEMORY((ub-lb+1)*PTR_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("REALMAT3VEC:create_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REALMAT3VEC(:) :: self
   ! Destroy a mat3vec
      PTR :: self
      STACK("REALMAT3VEC:destroy")
      START_TIMER("REALMAT3VEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REALMAT3VEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*PTR_SIZE)
      deallocate(self)
     STOP_TIMER("REALMAT3VEC:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    REALMAT3VEC(:) :: self
   ! Nullify the pointer parts of a mat3vec
       INT :: i
      STACK("REALMAT3VEC:nullify_ptr_part")
      START_TIMER("REALMAT3VEC:nullify_ptr_part")
      do i = 1,size(self,1)
         nullify(self(i)%element)
      end do
     STOP_TIMER("REALMAT3VEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    REALMAT3VEC(:) :: self
   ! Destroy the pointer parts of a mat3vec
       INT :: i
      STACK("REALMAT3VEC:destroy_ptr_part")
      START_TIMER("REALMAT3VEC:destroy_ptr_part")
      do i = 1,size(self,1)
         call destroy_(self(i)%element)
      end do
     STOP_TIMER("REALMAT3VEC:destroy_ptr_part")
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

   subroutine make_gaussian_xyz_matrices(self,ptr)
    REALMAT3VEC(:) :: self
   ! Return representation matrices for the s, p, d, f, and g (l = 0 ... 4)
   ! xyz cartesian gaussian shell components from a list of the p xyz
   ! representation matrices.
   ! NOTE: nothing in self in pointer assigned, so it can be destroyed safely.
      PTR :: self
      REALMAT3(:,:,:) :: ptr
      INT :: order
      STACK("REALMAT3VEC:make_gaussian_xyz_matrices")
      START_TIMER("REALMAT3VEC:make_gaussian_xyz_matrices")
      ENSURE(size(ptr,1)==3,"REALMAT3VEC:make_gaussian_xyz_matrices ... wrong 1st dimension, self")
      ENSURE(size(ptr,2)==3,"REALMAT3VEC:make_gaussian_xyz_matrices ... wrong 1st dimension, self")
      ENSURE(size(ptr,3)>0,"REALMAT3VEC:make_gaussian_xyz_matrices ... no p-type representation matrices")
      order = size(ptr,3)
      call create_(self,0,4)
      call create_(self(0)%element,1,1,order)
      call create_(self(1)%element,3,3,order)
      ! Now assign the transformation matrices
      self(0)%element = ONE
      self(1)%element = ptr
      self(2)%element => gaussian_d_xyz_matrices_(ptr)
      self(3)%element => gaussian_f_xyz_matrices_(ptr)
      self(4)%element => gaussian_g_xyz_matrices_(ptr)
     STOP_TIMER("REALMAT3VEC:make_gaussian_xyz_matrices")
      UNSTACK
   end subroutine

end
