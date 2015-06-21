!---------------------------------------------------------------------------
!
!  REALMAT3: 3 dimensional matrices
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
! $Id: realmat3.foo,v 1.8.2.1 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REALMAT3_MODULE

#  include "realmat3.use"

   implicit none

#  include "macros"
#  include "realmat3.int"


contains

   subroutine create(self,dim1,dim2,dim3)
    REALMAT3(:,:,:) :: self
   ! Create a mat3 with the given dimensions
      PTR :: self
      INT :: dim1,dim2,dim3
      STACK("REALMAT3:create")
      START_TIMER("REALMAT3:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3))
      ADD_MEMORY(dim1*dim2*dim3*REAL_SIZE)
     STOP_TIMER("REALMAT3:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
    REALMAT3(:,:,:) :: self
   ! Create a mat3 with the given bounds
      PTR :: self
      INT, IN :: lb1,lb2,lb3,ub1,ub2,ub3
      STACK("REALMAT3:create_1")
      START_TIMER("REALMAT3:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*REAL_SIZE)
     STOP_TIMER("REALMAT3:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2,bounds3)
    REALMAT3(:,:,:) :: self
   ! Create a mat3 with the given bounds for each dimension
      PTR :: self
      INTVEC(2), IN :: bounds1,bounds2,bounds3
      STACK("REALMAT3:create_2")
      START_TIMER("REALMAT3:create_2")
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))
     STOP_TIMER("REALMAT3:create_2")
      UNSTACK
   end subroutine

   subroutine create_3(self,bounds)
    REALMAT3(:,:,:) :: self
   ! Create a mat3 with the given bounds for all dimensions
      PTR :: self
      INTMAT(3,2), IN :: bounds
      STACK("REALMAT3:create_3")
      START_TIMER("REALMAT3:create_3")
      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2), &
              bounds(3,1),bounds(3,2))
     STOP_TIMER("REALMAT3:create_3")
      UNSTACK
   end subroutine

   subroutine create_copy(self,m)
    REALMAT3(:,:,:) :: self
   ! Create a copy of matrix "m"
      PTR :: self
      REALMAT3(:,:,:), PTR :: m
      STACK("REALMAT3:create_copy")
      START_TIMER("REALMAT3:create_copy")
      call create_(self,lbound(m,1),ubound(m,1), &
              lbound(m,2),ubound(m,2), &
              lbound(m,3),ubound(m,3)  )
      self = m
     STOP_TIMER("REALMAT3:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REALMAT3(:,:,:) :: self
   ! Destroy at mat3 object
      PTR :: self
      STACK("REALMAT3:destroy")
      START_TIMER("REALMAT3:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REALMAT3:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*REAL_SIZE)
      deallocate(self)
     STOP_TIMER("REALMAT3:destroy")
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

   function dim1(self) result(res)
    REALMAT3(:,:,:) :: self
   ! Returns the first dimension of self
      INT :: res
      STACK("REALMAT3:dim1")
      START_TIMER("REALMAT3:dim1")
      res = size(self,1)
     STOP_TIMER("REALMAT3:dim1")
      CHECK
   end function

   function dim2(self) result(res)
    REALMAT3(:,:,:) :: self
   ! Returns the second dimension of self
      INT :: res
      STACK("REALMAT3:dim2")
      START_TIMER("REALMAT3:dim2")
      res = size(self,2)
     STOP_TIMER("REALMAT3:dim2")
      CHECK
   end function

   function dim3(self) result(res)
    REALMAT3(:,:,:) :: self
   ! Returns the thirs dimension of self
      INT :: res
      STACK("REALMAT3:dim3")
      START_TIMER("REALMAT3:dim3")
      res = size(self,3)
     STOP_TIMER("REALMAT3:dim3")
      CHECK
   end function

   PURE function is_same_shape_as(self,b) result(res)
    REALMAT3(:,:,:) :: self
   ! Returns TRUE if the matrix "b" has the same shape as self
      REALMAT3(:,:,:), IN :: b
      IN :: self
      BIN :: res
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2) AND size(self,3)==size(b,3)
     STOP_TIMER("REALMAT3:is_same_shape_as")
   end function

   subroutine set_to(self,b)
    REALMAT3(:,:,:) :: self
   ! Set "self" to "b"
      REALMAT3(:,:,:) :: b
   STACK("REALMAT3:set_to")
   START_TIMER("REALMAT3:set_to")
   ENSURE(is_same_shape_as_(self,b),"REALMAT3:set_to ... different shapes")
      self = b
     STOP_TIMER("REALMAT3:set_to")
      CHECK
   end subroutine

   subroutine set_to_1(self,b)
    REALMAT3(:,:,:) :: self
   ! Set "self" to vector "b" in standard order
      REALVEC(:) :: b
   STACK("REALMAT3:set_to_1")
   START_TIMER("REALMAT3:set_to_1")
   ENSURE(size(self)==size(b),"REALMAT3:set_to_1 ... different sizes")
      self = reshape(b,(/ size(self,1),size(self,2),size(self,3) /))
     STOP_TIMER("REALMAT3:set_to_1")
      CHECK
   end subroutine

   subroutine make_symmetric(self)
    REALMAT3(:,:,:) :: self
   ! Make the upper pyramid of "self" the same as the lower pyramid
      INT :: dim, i,j,k
      REAL :: val
   STACK("REALMAT3:make_symmetric")
   START_TIMER("REALMAT3:make_symmetric")
   ENSURE(size(self,1)==size(self,2),"REALMAT3:make_symmetric ... non-cube tensor")
   ENSURE(size(self,1)==size(self,3),"REALMAT3:make_symmetric ... non-cube tensor")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,i
      do k = 1,j
         val = self(i,j,k)
         self(i,k,j) = val
         self(j,i,k) = val
         self(j,k,i) = val
         self(k,i,j) = val
         self(k,j,i) = val
      end do
      end do
      end do
     STOP_TIMER("REALMAT3:make_symmetric")
      CHECK
   end subroutine

   subroutine transpose_12(self)
    REALMAT3(:,:,:) :: self
   ! Makes the matrix formed by the 1st and 2nd columns symmetrical.
     INT :: k
   STACK("REALMAT3:transpose_12")
   START_TIMER("REALMAT3:transpose_12")
   ENSURE(size(self,1)==size(self,2),"REALMAT3:transpose_12 ... non-square matrix")
     do k = 1,size(self,3)
       self(:,:,k) = transpose(self(:,:,k))
     end do
     STOP_TIMER("REALMAT3:transpose_12")
      CHECK
   end subroutine

   subroutine symmetric_reflect_12(self)
    REALMAT3(:,:,:) :: self
   ! Makes the matrix formed by the 1st and 2nd columns symmetrical.
     INT :: dim1,dim2,i,j
   STACK("REALMAT3:symmetric_reflect_12")
   START_TIMER("REALMAT3:symmetric_reflect_12")
   ENSURE(size(self,1)==size(self,2),"REALMAT3:symmetric_reflect_12 ... non-square matrix")
     dim1 = size(self,1)
     dim2 = size(self,2)
     do i = 1,dim1
     do j = 1,i-1
       self(j,i,:) = self(i,j,:)
     end do
     end do
     STOP_TIMER("REALMAT3:symmetric_reflect_12")
      CHECK
   end subroutine

   subroutine symmetric_reflect_23(self)
    REALMAT3(:,:,:) :: self
   ! Makes the matrix formed by the 2nd and 3rd columns symmetrical.
     INT :: dim,i,j
     STACK("REALMAT3:symmetric_reflect_23")
     START_TIMER("REALMAT3:symmetric_reflect_23")
     call check_square_23_(self)
     dim = size(self,2)
     do i = 1,dim
     do j = 1,i-1
       self(:,j,i) = self(:,i,j)
     end do
     end do
     STOP_TIMER("REALMAT3:symmetric_reflect_23")
      CHECK
   end subroutine

   subroutine to_tri_23(self,tr)
    REALMAT3(:,:,:) :: self
   ! Converts the matrix self to the lower triangle tr.
   ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
     IN :: self
     REALVEC(:) :: tr
     INT :: dim1,dim2,h,i,j,ij
   STACK("REALMAT3:to_tri_23")
   START_TIMER("REALMAT3:to_tri_23")
   ENSURE(size(tr)>=tri_size_23_(self),"REALMAT3:to_tri_23 ... tr array too small")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call check_square_23_(self)
     ij = 0
     do h = 1,dim1
       do i = 1,dim2
         do j = 1,i
           ij = ij+1
           tr(ij) = self(h,j,i)
         end do
       end do
     end do
     STOP_TIMER("REALMAT3:to_tri_23")
      CHECK
   end subroutine

   subroutine from_tri_23(self,tr)
    REALMAT3(:,:,:) :: self
   ! Converts the matrix self to the lower triangle tr.
   ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
     REALVEC(:), IN :: tr
     INT :: dim1,dim2,h,i,j,ij
   STACK("REALMAT3:from_tri_23")
   START_TIMER("REALMAT3:from_tri_23")
   ENSURE(size(tr)>=tri_size_23_(self),"REALMAT3:from_tri_23 ... tr array too small")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call check_square_23_(self)
     ij = 0
     do h = 1,dim1
       do i = 1,dim2
         do j = 1,i
           ij = ij+1
           self(h,j,i) = tr(ij)
           self(h,i,j) = tr(ij)
         end do
       end do
     end do
     STOP_TIMER("REALMAT3:from_tri_23")
      CHECK
   end subroutine

   function tri_size_23(self) result(ltr)
    REALMAT3(:,:,:) :: self
   ! Returns the size of the lower triangle needed to store the matrix self.
   ! Assumes the matrix formed by columns 2 and 3 is the symmetric one.
     IN :: self
     INT :: ltr
     INT :: dim1,dim2
     STACK("REALMAT3:tri_size_23")
     START_TIMER("REALMAT3:tri_size_23")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call check_square_23_(self)
     ltr = dim1*dim2*(dim2+1)/2
     STOP_TIMER("REALMAT3:tri_size_23")
      CHECK
   end function

   subroutine check_square_23(self)
    REALMAT3(:,:,:) :: self
   ! Checks to see that the matrix formed by the 2nd and 3rd columns is square.
     INT :: dim2,dim3
     STACK("REALMAT3:check_square_23")
     START_TIMER("REALMAT3:check_square_23")
     dim2 = size(self,2)
     dim3 = size(self,3)
     ENSURE(dim2==dim3,"REALMAT3:check_square_23 ... non-square 2nd and 3rd dimensions")
     STOP_TIMER("REALMAT3:check_square_23")
      CHECK
   end subroutine

   function gaussian_d_xyz_matrices(self) result(dtr)
    REALMAT3(:,:,:) :: self
   ! Return the representation matrices for d xyz products found in
   ! gaussian shells from the p xyz matrices.
      REALMAT3(:,:,:), PTR :: dtr
      INT :: n,order
   STACK("REALMAT3:gaussian_d_xyz_matrices")
   START_TIMER("REALMAT3:gaussian_d_xyz_matrices")
   ENSURE(size(self,1)==3,"REALMAT3:gaussian_d_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,2)==3,"REALMAT3:gaussian_d_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,3)>0,"REALMAT3:gaussian_d_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(dtr,6,6,order)
      do n = 1,order
         dtr(:,:,n) = gaussian_d_xyz_matrix_(self(:,:,n))
      end do
     STOP_TIMER("REALMAT3:gaussian_d_xyz_matrices")
      UNSTACK
   end function

   function gaussian_f_xyz_matrices(self) result(ftr)
    REALMAT3(:,:,:) :: self
   ! Return the representation matrices for f xyz products found in
   ! gaussian shells from the p xyz matrices
      REALMAT3(:,:,:), PTR :: ftr
      INT :: n,order
   STACK("REALMAT3:gaussian_f_xyz_matrices")
   START_TIMER("REALMAT3:gaussian_f_xyz_matrices")
   ENSURE(size(self,1)==3,"REALMAT3:gaussian_f_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,2)==3,"REALMAT3:gaussian_f_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,3)>0,"REALMAT3:gaussian_f_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(ftr,10,10,order)
      do n = 1,order
         ftr(:,:,n) = gaussian_f_xyz_matrix_(self(:,:,n))
      end do
     STOP_TIMER("REALMAT3:gaussian_f_xyz_matrices")
      UNSTACK
   end function

   function gaussian_g_xyz_matrices(self) result(gtr)
    REALMAT3(:,:,:) :: self
   ! Return the representation matrices for g xyz products found in
   ! gaussian shells from the p xyz matrices
      REALMAT3(:,:,:), PTR :: gtr
      INT :: n,order
   STACK("REALMAT3:gaussian_g_xyz_matrices")
   START_TIMER("REALMAT3:gaussian_g_xyz_matrices")
   ENSURE(size(self,1)==3,"REALMAT3:gaussian_g_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,2)==3,"REALMAT3:gaussian_g_xyz_matrices ... wrong 1st dimension, self")
   ENSURE(size(self,3)>0,"REALMAT3:gaussian_g_xyz_matrices ... no p type matrices")
      order = size(self,3)
      call create_(gtr,15,15,order)
      do n = 1,order
         gtr(:,:,n) = gaussian_g_xyz_matrix_(self(:,:,n))
      end do
     STOP_TIMER("REALMAT3:gaussian_g_xyz_matrices")
      UNSTACK
   end function

   subroutine similarity_transform_12(self,V)
    REALMAT3(:,:,:) :: self
   ! Do a similarity tranform on the first two indices of self, i.e.
   ! self(:,:,i) -> V self(:,:,i) V^-1
      REALMAT(:,:) :: V
      INT :: n
   STACK("REALMAT3:similarity_transform_12")
   START_TIMER("REALMAT3:similarity_transform_12")
   ENSURE(size(self,1)==size(self,2),"REALMAT3:similarity_transform_12 ... 1st two dimensions of self unequal")
   ENSURE(size(self,1)==size(V,1),"REALMAT3:similarity_transform_12 ... incompatible transform matrix, V")
   ENSURE(is_square_(V),"REALMAT3:similarity_transform_12 ... transform matrix not square")
   ENSURE(size(self,3)>0,"REALMAT3:similarity_transform_12 ... no p type matrices")
      do n = 1,size(self,3)
         call similarity_transform_(self(:,:,n),V)
      end do
     STOP_TIMER("REALMAT3:similarity_transform_12")
      CHECK
   end subroutine

end
