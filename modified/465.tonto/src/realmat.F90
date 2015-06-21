!-------------------------------------------------------------------------------
!
! REALMAT: Matrix operations ...
!
! Copyright (C) Dylan Jayatilaka, 1996
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
! $Id: realmat.foo,v 1.58.2.9 2003/11/13 05:36:07 reaper Exp $
!-------------------------------------------------------------------------------

module REALMAT_MODULE

#  include "realmat.use"

   implicit none

#  include "macros"
#  include "realmat.int"


   public trace_of_product_; interface trace_of_product_
     module procedure trace_product_with
   end interface

contains

   subroutine create(self,dim1,dim2)
    REALMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:create")
      START_TIMER("REALMAT:create")
      nullify(self)
      allocate(self(dim1,dim2))
      ADD_MEMORY(dim1*dim2*REAL_SIZE)
     STOP_TIMER("REALMAT:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    REALMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: lb1,ub1,lb2,ub2
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:create_1")
      START_TIMER("REALMAT:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*REAL_SIZE)
     STOP_TIMER("REALMAT:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2)
    REALMAT(:,:) :: self
   ! Create a matrix with the specified bounds for each dimension
      PTR :: self
      INTVEC(:), IN :: bounds1,bounds2
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:create_2")
      START_TIMER("REALMAT:create_2")
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))
     STOP_TIMER("REALMAT:create_2")
      UNSTACK
   end subroutine

   subroutine create_3(self,bounds)
    REALMAT(:,:) :: self
   ! Create a matrix with the given bounds for all dimensions
      PTR :: self
      INTMAT(2,2), IN :: bounds
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:create_3")
      START_TIMER("REALMAT:create_3")
      call create_(self,bounds(1,1),bounds(1,2),bounds(2,1),bounds(2,2))
     STOP_TIMER("REALMAT:create_3")
      UNSTACK
   end subroutine

   subroutine create_copy(self,matrix)
    REALMAT(:,:) :: self
   ! Create a replica copy of matrix
      PTR :: self
      REALMAT(:,:), IN :: matrix
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:create_copy")
      START_TIMER("REALMAT:create_copy")
      call create_(self,lbound(matrix,1),ubound(matrix,1), &
              lbound(matrix,2),ubound(matrix,2)  )
      self = matrix
     STOP_TIMER("REALMAT:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REALMAT(:,:) :: self
   ! Destroy the object
      PTR :: self
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:destroy")
      START_TIMER("REALMAT:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REALMAT:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*REAL_SIZE)
      deallocate(self)
     STOP_TIMER("REALMAT:destroy")
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

   PURE function is_square(self) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix is square
      IN :: self
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(self,2)
     STOP_TIMER("REALMAT:is_square")
   end function

   PURE function is_same_shape_as(self,b) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" has the same shape as self
      IN :: self
      REALMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2)
     STOP_TIMER("REALMAT:is_same_shape_as")
   end function

   PURE function is_transposed_shape_of(self,b) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" is the transposed shape of self
      IN :: self
      REALMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) AND size(self,2)==size(b,1)
     STOP_TIMER("REALMAT:is_transposed_shape_of")
   end function

   function all_in_range(self,range) result(res)
    REALMAT(:,:) :: self
   ! Return TRUE if all values of self are within the specified "range".
      REALVEC(2) :: range
      BIN :: res
      STACK("REALMAT:all_in_range")
      START_TIMER("REALMAT:all_in_range")
      res = all(range(1) <= self AND self <= range(2))
     STOP_TIMER("REALMAT:all_in_range")
      CHECK
   end function

   function in_range(self,range) result(res)
    REALMAT(:,:) :: self
   ! Return element ij as TRUE if self(i,j) is within the specified "range".
      REALVEC(2) :: range
      BINMAT(size(self,1),size(self,2)) :: res
      STACK("REALMAT:in_range")
      START_TIMER("REALMAT:in_range")
      res = (range(1) <= self AND self <= range(2))
     STOP_TIMER("REALMAT:in_range")
      CHECK
   end function

   function range(self) result(res)
    REALMAT(:,:) :: self
   ! Return the range (smallest and largest value) of self.
      REALVEC(2) :: res
      STACK("REALMAT:range")
      START_TIMER("REALMAT:range")
      res(1) = minval(self)
      res(2) = maxval(self)
     STOP_TIMER("REALMAT:range")
      CHECK
   end function

   subroutine shrink(self,dim1,dim2)
    REALMAT(:,:) :: self
   ! Shrinks self to dimension dim1xdim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
     REALMAT(:,:), PTR :: old
     STACK("REALMAT:shrink")
     START_TIMER("REALMAT:shrink")
     ENSURE(associated(self),"REALMAT:shrink ... matrix not allocated")
     ENSURE(dim1<=size(self,1),"REALMAT:shrink ... 1st dimension given is too large.")
     ENSURE(dim2<=size(self,2),"REALMAT:shrink ... 2nd dimension given is too large.")
     if (dim1==size(self,1) AND dim2==size(self,2)) then; STOP_TIMER("REALMAT:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)
     STOP_TIMER("REALMAT:shrink")
      UNSTACK
   end subroutine

   subroutine shrink_columns(self,dim2)
    REALMAT(:,:) :: self
   ! Shrinks columns of self to dimension dim2. Contents are retained.
     PTR :: self
     INT, IN :: dim2
   ! The following code is inherited from INTRINSICMAT
     REALMAT(:,:), PTR :: old
     INT :: dim1
     STACK("REALMAT:shrink_columns")
     START_TIMER("REALMAT:shrink_columns")
     ENSURE(associated(self),"REALMAT:shrink_columns ... matrix not allocated")
     ENSURE(dim2<=size(self,2),"REALMAT:shrink_columns ... 2nd dimension given is too large.")
     if (dim2==size(self,2)) then; STOP_TIMER("REALMAT:shrink_columns") UNSTACK return; end if
     dim1 = size(self,1)
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)
     STOP_TIMER("REALMAT:shrink_columns")
      UNSTACK
   end subroutine

   subroutine expand(self,dim1,dim2)
    REALMAT(:,:) :: self
   ! Expands self to dimension dim1xdim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
     REALMAT(:,:), PTR :: old
     INT :: old_size1,old_size2
     STACK("REALMAT:expand")
     START_TIMER("REALMAT:expand")
     if (NOT associated(self)) then
       call create_(self,0,0)
     else
     ENSURE(dim1>=size(self,1),"REALMAT:expand ... 1st dimension given is too small")
     ENSURE(dim2>=size(self,2),"REALMAT:expand ... 2nd dimension given is too small")
     end if
     old => self
     old_size1 = size(old,1)
     old_size2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(1:old_size1,1:old_size2)=old
     call destroy_(old)
     STOP_TIMER("REALMAT:expand")
      UNSTACK
   end subroutine

   subroutine expand_columns(self,dim2)
    REALMAT(:,:) :: self
   ! Expands the columns self to dim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim2
   ! The following code is inherited from INTRINSICMAT
     INT :: dim1,old_dim2
     REALMAT(:,:), PTR :: old
     STACK("REALMAT:expand_columns")
     START_TIMER("REALMAT:expand_columns")
     ENSURE( associated(self),"REALMAT:expand_columns ... matrix not allocated")
     ENSURE(dim2>=size(self,2),"REALMAT:expand_columns ... 2nd dimension given is too small")
     dim1 = size(self,1)
     old => self
     old_dim2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(:,1:old_dim2) = old
     call destroy_(old)
     STOP_TIMER("REALMAT:expand_columns")
      UNSTACK
   end subroutine

   subroutine append_column(self,col)
    REALMAT(:,:) :: self
   ! Append the column "col" onto the end of self.
     PTR :: self
     REALVEC(:) :: col
     INT :: old_dim2,new_dim2
   STACK("REALMAT:append_column")
   START_TIMER("REALMAT:append_column")
   ENSURE(associated(self),"REALMAT:append_column ... self not allocated")
   ENSURE(size(self,1)==size(col),"REALMAT:append_column ... 2nd dimension given is too small")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + 1
     call expand_columns_(self,new_dim2)
     self(:,new_dim2) = col
     STOP_TIMER("REALMAT:append_column")
      UNSTACK
   end subroutine

   subroutine append_columns(self,cols)
    REALMAT(:,:) :: self
   ! Append the columns "cols" onto the end of self.
     PTR :: self
     REALMAT(:,:) :: cols
   ! The following code is inherited from INTRINSICMAT
     INT :: old_dim2,new_dim2
     STACK("REALMAT:append_columns")
     START_TIMER("REALMAT:append_columns")
     ENSURE(associated(self),"REALMAT:append_columns ... self not allocated")
     ENSURE(size(self,1)==size(cols,1),"REALMAT:append_columns ... 1st dimension wrong, cols")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + size(cols,2)
     call expand_columns_(self,new_dim2)
     self(:,old_dim2+1:new_dim2) = cols
     STOP_TIMER("REALMAT:append_columns")
      UNSTACK
   end subroutine

   function determinant(self) result(res)
    REALMAT(:,:) :: self
   ! Return the determinant a 3x3 matrix
      IN :: self
      REAL :: res
   STACK("REALMAT:determinant")
   START_TIMER("REALMAT:determinant")
   ENSURE(is_square_(self),"REALMAT:determinant ... non-square matrix")
   ENSURE(size(self,1)<4,"REALMAT:determinant ... only works for up to size 3 matrices")
      select case (size(self,1))
          case (1)
             res = self(1,1)
          case (2)
             res = self(1,1)*self(2,2) - self(2,1)*self(1,2)
          case (3)
             res = self(1,3)*(self(2,1)*self(3,2) - self(3,1)*self(2,2)) &
                 + self(2,3)*(self(3,1)*self(1,2) - self(1,1)*self(3,2)) &
                 + self(3,3)*(self(1,1)*self(2,2) - self(2,1)*self(1,2))
      end select
     STOP_TIMER("REALMAT:determinant")
      CHECK
   end function

   function cofactor(self) result(res)
    REALMAT(:,:) :: self
   ! Return the cofactor fo a 3x3 matrix
      IN :: self
      REALMAT(:,:), PTR :: res
      STACK("REALMAT:cofactor")
      START_TIMER("REALMAT:cofactor")
      ENSURE(is_square_(self),"REALMAT:cofactor ... non-square matrix")
      ENSURE(size(self,1)==3,"REALMAT:cofactor ... only works for 3x3 matrices")
      nullify(res)
      call create_(res,3,3)
      res(1,1) =  (self(2,2)*self(3,3) - self(2,3)*self(3,2))
      res(1,2) = -(self(2,1)*self(3,3) - self(2,3)*self(3,1))
      res(1,3) =  (self(2,1)*self(3,2) - self(2,2)*self(3,1))
      res(2,1) = -(self(1,2)*self(3,3) - self(1,3)*self(3,2))
      res(2,2) =  (self(1,1)*self(3,3) - self(1,3)*self(3,1))
      res(2,3) = -(self(1,1)*self(3,2) - self(1,2)*self(3,1))
      res(3,1) =  (self(1,2)*self(2,3) - self(1,3)*self(2,2))
      res(3,2) = -(self(1,1)*self(2,3) - self(1,3)*self(2,1))
      res(3,3) =  (self(1,1)*self(2,2) - self(1,2)*self(2,1))
     STOP_TIMER("REALMAT:cofactor")
      CHECK
   end function

   function dot(self,l,r) result(res)
    REALMAT(:,:) :: self
   ! Multiply the matrix self by vector "l" on the left and vector "r" on the
   ! right ie:  res = l^T self r. Useful for non-unit metric dot_products.
     IN :: self
     REALVEC(:), IN :: l,r
     REAL :: res
     REALVEC(:), PTR :: w
     STACK("REALMAT:dot")
     START_TIMER("REALMAT:dot")
     ENSURE(size(self,1)==size(l),"REALMAT:dot ... wrong size, r")
     ENSURE(size(self,2)==size(r),"REALMAT:dot ... wrong size, r")
     call create_(w,size(l))
     call to_product_of_(w,self,r)
     res = dot_product(l,w)
     call destroy_(w)
     STOP_TIMER("REALMAT:dot")
      CHECK
   end function

   function dot_1(self,l,r) result(res)
    REALMAT(:,:) :: self
   ! Multiply the matrix self by vector "l" on the left and vector "r" on the
   ! right ie:  res = l^T self r. Useful for non-unit metric dot_products.
     IN :: self
     CPXVEC(:), IN :: l,r
     CPX :: res
     CPXVEC(:), PTR :: w
     STACK("REALMAT:dot_1")
     START_TIMER("REALMAT:dot_1")
     ENSURE(size(self,1)==size(l),"REALMAT:dot_1 ... wrong size, r")
     ENSURE(size(self,2)==size(r),"REALMAT:dot_1 ... wrong size, r")
     call create_(w,size(l))
     call to_product_of_(w,self,r)
     res = dot_product(l,w)
     call destroy_(w)
     STOP_TIMER("REALMAT:dot_1")
      CHECK
   end function

   subroutine rotate(self,v)
    REALMAT(:,:) :: self
   ! Rotate vector "v" by self
     REALVEC(:), INOUT :: v
     INT :: dim1,dim2,i,j
     REALVEC(:), PTR :: w
     REAL :: val
     STACK("REALMAT:rotate")
     START_TIMER("REALMAT:rotate")
     ENSURE(is_square_(self),"REALMAT:rotate ... incompatible arrays sizes")
     ENSURE(size(self,2)==size(v),"REALMAT:rotate ... incompatible arrays sizes")
     dim1 = size(self,1)
     dim2 = size(self,2)
     call create_(w,dim2)
     do i = 1,dim1
       val = ZERO
       do j = 1,dim2
         val = val + self(i,j) * v(j)
       end do
       w(i) = val
     end do
     v = w
     call destroy_(w)
     STOP_TIMER("REALMAT:rotate")
      CHECK
   end subroutine

   subroutine to_product_of(self,a,b,transpose_a,transpose_b)
    REALMAT(:,:) :: self
   ! Set "self" to the matrix product of "a" and "b". If present,
   ! "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! neeb to be transposed.
     INOUT :: self
     REALMAT(:,:), IN :: a, b
     BIN, optional, IN :: transpose_a, transpose_b
     BIN :: trans_a,trans_b
     INT :: dim1,dim2,i,j,opt
     STACK("REALMAT:to_product_of")
     START_TIMER("REALMAT:to_product_of")
     trans_a = FALSE;       trans_b = FALSE
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0) ! .to_product_an_bn
         ENSURE( size(self,1)==size(a,1),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(i,:)*b(:,j))
           end do
         end do
       case (1) ! .to_product_at_bn
         ENSURE( size(self,1)==size(a,2),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(:,i)*b(:,j))
           end do
         end do
       case (2) ! .to_product_an_bt
         ENSURE( size(self,1)==size(a,1),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(i,:)*b(j,:))
           end do
         end do
       case (3) ! .to_product_at_bt
         ENSURE( size(self,1)==size(a,2),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:to_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"REALMAT:to_product_of ... incompatible arrays")
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
           do j=1,dim2
             self(i,j) = sum(a(:,i)*b(j,:))
           end do
         end do
     end select
     STOP_TIMER("REALMAT:to_product_of")
      CHECK
   end subroutine

   subroutine to_product_of_1(self,a,b,dagger_a,dagger_b)
    REALMAT(:,:) :: self
   ! Set "self" to the complex matrix product of "a" and "b". If present,
   ! "dagger_a" and "dagger_b" can be set to TRUE if "a" and "b" need
   ! to be daggered. WARNING: the complex part is thrown away.
     INOUT :: self
     CPXMAT(:,:), IN :: a, b
     BIN, optional, IN :: dagger_a, dagger_b
     BIN :: dagg_a,dagg_b
     INT :: opt
     INT :: dima,dim1,dim2,i,j,k
     REAL :: temp
     STACK("REALMAT:to_product_of_1")
     START_TIMER("REALMAT:to_product_of_1")
     dagg_a = FALSE;        dagg_b = FALSE
     if (present(dagger_a)) dagg_a = dagger_a
     if (present(dagger_b)) dagg_b = dagger_b
     opt = 0
     if (dagg_a) opt = opt + 1
     if (dagg_b) opt = opt + 2
     select case (opt)
       case (0) ! .to_product_an_bn
         ENSURE( size(self,1)==size(a,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = temp
         end do
         end do
       case (1) ! .to_product_ad_bn
         ENSURE( size(self,1)==size(a,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + conjg(a(k,i)) * b(k,j)
           end do
           self(i,j) = temp
         end do
         end do
       case (2) ! .to_product_an_bd
         ENSURE( size(self,1)==size(a,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * conjg(b(j,k))
           end do
           self(i,j) = temp
         end do
         end do
       case (3) ! .to_product_ad_bd
         ENSURE( size(self,1)==size(a,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:to_product_of_1 ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"REALMAT:to_product_of_1 ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = temp ! conjugate not reqd
         end do
         end do
     end select
     STOP_TIMER("REALMAT:to_product_of_1")
      CHECK
   end subroutine

   subroutine to_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    REALMAT(:,:) :: self
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac".
   ! If present, "transpose_a" and "transpose_b" can be set to TRUE if "a"
   ! and "b" neeb to be transposed.
     INOUT :: self
     REAL, IN :: fac
     REALMAT(:,:), IN :: a, b
     BIN, optional, IN :: transpose_a, transpose_b
     BIN :: trans_a,trans_b
     INT :: opt
     INT :: dima,dim1,dim2,i,j,k
     REAL :: temp
     STACK("REALMAT:to_scaled_product_of")
     START_TIMER("REALMAT:to_scaled_product_of")
     trans_a = FALSE;          trans_b = FALSE
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0) ! .to_scaled_product_an_bn
         ENSURE( size(self,1)==size(a,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = fac * temp
         end do
         end do
       case (1) ! .to_scaled_product_at_bn
         ENSURE( size(self,1)==size(a,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(k,j)
           end do
           self(i,j) = fac * temp
         end do
         end do
       case (2) ! .to_scaled_product_an_bt
         ENSURE( size(self,1)==size(a,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(j,k)
           end do
           self(i,j) = fac * temp
         end do
         end do
       case (3) ! .to_scaled_product_at_bt
         ENSURE( size(self,1)==size(a,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:to_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"REALMAT:to_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = fac * temp
         end do
         end do
     end select
     STOP_TIMER("REALMAT:to_scaled_product_of")
      CHECK
   end subroutine

   subroutine plus_product_of(self,a,b,transpose_a,transpose_b)
    REALMAT(:,:) :: self
   ! Add to  "self" the matrix product of "a" and "b". If present,
   ! "transpose_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! neeb to be transposed.
     INOUT :: self
     REALMAT(:,:), IN :: a, b
     BIN, optional, IN :: transpose_a, transpose_b
     BIN :: trans_a,trans_b
     INT :: opt
     INT :: dima,dim1,dim2,i,j,k
     REAL :: temp
     STACK("REALMAT:plus_product_of")
     START_TIMER("REALMAT:plus_product_of")
     trans_a = FALSE;       trans_b = FALSE
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0) ! .plus_product_an_bn
         ENSURE( size(self,1)==size(a,1),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
       case (1) ! .plus_product_at_bn
         ENSURE( size(self,1)==size(a,2),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(k,j)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
       case (2) ! .plus_product_an_bt
         ENSURE( size(self,1)==size(a,1),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(j,k)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
       case (3) ! .plus_product_at_bt
         ENSURE( size(self,1)==size(a,2),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:plus_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"REALMAT:plus_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = self(i,j) + temp
         end do
         end do
     end select
     STOP_TIMER("REALMAT:plus_product_of")
      CHECK
   end subroutine

   subroutine plus_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    REALMAT(:,:) :: self
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac".
   ! If present, "transpose_a" and "transpose_b" can be set to TRUE if "a"
   ! and "b" need to be transposed.
     INOUT :: self
     REAL, IN :: fac
     REALMAT(:,:), IN :: a, b
     BIN, optional, IN :: transpose_a, transpose_b
     BIN :: trans_a,trans_b
     INT :: opt
     INT :: dima,dim1,dim2,i,j,k
     REAL :: temp
     STACK("REALMAT:plus_scaled_product_of")
     START_TIMER("REALMAT:plus_scaled_product_of")
     trans_a = FALSE;       trans_b = FALSE
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b
     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2
     select case (opt)
       case (0) ! .plus_scaled_product_an_bn
         ENSURE( size(self,1)==size(a,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(k,j)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
       case (1) ! .plus_scaled_product_at_bn
         ENSURE( size(self,1)==size(a,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(k,j)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
       case (2) ! .plus_scaled_product_an_bt
         ENSURE( size(self,1)==size(a,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,2)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,2)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(i,k) * b(j,k)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
       case (3) ! .plus_scaled_product_at_bt
         ENSURE( size(self,1)==size(a,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE( size(self,2)==size(b,1),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         ENSURE(size(a,1)==size(b,2),"REALMAT:plus_scaled_product_of ... incompatible arrays")
         dima = size(a,1)
         dim1 = size(self,1)
         dim2 = size(self,2)
         do i=1,dim1
         do j=1,dim2
           temp = ZERO
           do k=1,dima
             temp = temp + a(k,i) * b(j,k)
           end do
           self(i,j) = self(i,j) + fac * temp
         end do
         end do
     end select
     STOP_TIMER("REALMAT:plus_scaled_product_of")
      CHECK
   end subroutine

   subroutine plus_scaled(self,mat,fac)
    REALMAT(:,:) :: self
   ! Add the matrix "mat" scaled by "fac" to "self".
     INOUT :: self
     REAL, IN :: fac
     REALMAT(:,:), IN :: mat
     INT :: dim1,dim2,i,j
     STACK("REALMAT:plus_scaled")
     START_TIMER("REALMAT:plus_scaled")
     ENSURE(size(self,1)==size(mat,1),"REALMAT:plus_scaled ... incompatible arrays")
     ENSURE(size(self,2)==size(mat,2),"REALMAT:plus_scaled ... incompatible arrays")
     dim1 = size(self,1)
     dim2 = size(self,2)
     do j=1,dim2
       do i=1,dim1
         self(i,j) = self(i,j) + fac * mat(i,j)
       end do
     end do
     STOP_TIMER("REALMAT:plus_scaled")
      CHECK
   end subroutine

   subroutine minus_scaled(self,mat,fac)
    REALMAT(:,:) :: self
   ! Subtract the matrix "mat" scaled by "fac" from "self".
     INOUT :: self
     REAL, IN :: fac
     REALMAT(:,:), IN :: mat
     INT :: dim1,dim2,i,j
     STACK("REALMAT:minus_scaled")
     START_TIMER("REALMAT:minus_scaled")
     ENSURE(size(self,1)==size(mat,1),"REALMAT:minus_scaled ... incompatible arrays")
     ENSURE(size(self,2)==size(mat,2),"REALMAT:minus_scaled ... incompatible arrays")
     dim1 = size(self,1)
     dim2 = size(self,2)
     do j=1,dim2
       do i=1,dim1
         self(i,j) = self(i,j) - fac * mat(i,j)
       end do
     end do
     STOP_TIMER("REALMAT:minus_scaled")
      CHECK
   end subroutine

   subroutine to_product_with_diagonal(self,a,diag,transpose_a)
    REALMAT(:,:) :: self
   ! Set "self" to the matrix product of "a" with diagonal matrix "diag" (stored
   ! as a vector).  If present, "transpose_a" can be set to TRUE if "a" needs to
   ! be transposed.
      INOUT :: self
      REALMAT(:,:), IN :: a
      REALVEC(:), IN :: diag
      BIN, optional, IN :: transpose_a
      INT :: a1,a2,s1,s2,d1,i,j
      REAL :: temp
   STACK("REALMAT:to_product_with_diagonal")
   START_TIMER("REALMAT:to_product_with_diagonal")
   ENSURE(is_same_shape_as_(self,a),"REALMAT:to_product_with_diagonal ... incompatible dimensions")
      s1 = size(self,1); s2 = size(self,2)
      a1 = size(a,1);    a2 = size(a,2)
      d1 = size(diag)
      if (present(transpose_a)) then
         ENSURE(a1==d1,"REALMAT:to_product_with_diagonal ... incompatible dimensions")
         do j=1,s2
           temp = diag(j)
           do i=1,s1
             self(i,j) = a(j,i)*temp
           end do
         end do
      else
         ENSURE(a2==d1,"REALMAT:to_product_with_diagonal ... incompatible dimensions")
         do j=1,s2
           temp = diag(j)
           do i=1,s1
             self(i,j) = a(i,j)*temp
           end do
         end do
      end if
     STOP_TIMER("REALMAT:to_product_with_diagonal")
      CHECK
   end subroutine

   subroutine to_product_with_diagonal_1(self,dg,a,transpose_a)
    REALMAT(:,:) :: self
   ! Set "self" to the matrix product of diagonal matrix "dg" (stored as a
   ! vector) and "a".  If present, "transpose_a" can be set to TRUE if "a" needs
   ! to be transposed.
      INOUT :: self
      REALMAT(:,:), IN :: a
      REALVEC(:), IN :: dg
      BIN, optional, IN :: transpose_a
      INT :: a1,a2,s1,s2,d1,i,j
      REAL :: temp
      STACK("REALMAT:to_product_with_diagonal_1")
      START_TIMER("REALMAT:to_product_with_diagonal_1")
      ENSURE(is_same_shape_as_(self,a),"REALMAT:to_product_with_diagonal_1 ... incompatible dimensions")
      s1 = size(self,1); s2 = size(self,2)
      a1 = size(a,1);    a2 = size(a,2)
      d1 = size(dg)
      if (present(transpose_a)) then
         ENSURE(a2==d1,"REALMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i=1,s1
           temp = dg(i)
           do j=1,s2
             self(i,j) = temp*a(j,i)
           end do
         end do
      else
         ENSURE(a1==d1,"REALMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i=1,s1
           temp = dg(i)
           do j=1,s2
             self(i,j) = temp*a(i,j)
           end do
         end do
      end if
     STOP_TIMER("REALMAT:to_product_with_diagonal_1")
      CHECK
   end subroutine

! *********************
! Eigenproblem routines
! *********************

   subroutine solve_eigenproblem(self,eigenvalues,eigenvectors)
    REALMAT(:,:) :: self
   ! Solve the symmetric eigenproblem for "self", yeilding a vector of
   ! "eigenvalues" and a matrix of "eigenvectors"
      REALVEC(:) :: eigenvalues
      REALMAT(:,:) :: eigenvectors
      STACK("REALMAT:solve_eigenproblem")
      START_TIMER("REALMAT:solve_eigenproblem")
      call solve_symmetric_eigenproblem_(self,eigenvalues,eigenvectors)
     STOP_TIMER("REALMAT:solve_eigenproblem")
      CHECK
   end subroutine

!   solve_general_eigenproblem(eigenvalues,left,right,normalize)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "left" and "right" eigenvectors. If "normalize" is present
!   ! and FALSE, the left and right eigenvectors are not automatically
!   ! renormalized so that (left)^T (right) = 1
!      eigenvalues :: CPXVEC
!      left,right :: CPXMAT
!      normalize :: BIN, optional
!      er,ei,W :: REALVEC*
!      A,le,re :: SELF_TYPE*
!      i,dim,dimW, info :: INT
!      normalise :: BIN
!      dot :: REAL
!      ENSURE(.is_square,"non-square matrix")
!      ENSURE(size(eigenvalues)>=.dim1,"eigenvalue array too small")
!      ENSURE(size(left)>=size(self),"left eigenvector matrix too small")
!      ENSURE(size(right)>=size(self),"right eigenvector matrix too small")
!      dim = size(self,1)
!      normalise = TRUE
!      if (present(normalize)) normalise = normalize
!      if (self.is_symmetric) then
!         A.create(dim,dim)
!         er.create(dim)
!         .solve_symmetric_eigenproblem(er,A)
!         eigenvalues = er
!         right = A
!         left  = A
!         er.destroy
!         A.destroy
!      else
!         A.create(dim,dim)
!         er.create(dim); ei.create(dim)
!         le.create(dim,dim); re.create(dim,dim)
!         dimW = 8*dim
!         W.create(dimW)
!         A = self
!         ! Solve the eigenvalueproblem
!         call dgeev('V','V',dim,A,dim,er,ei,le,dim,re,dim,W,dimW,info)
!         ENSURE(info==0,"error, info="// trim(info.to_str))
!         ! Search for the complex eigenvalues/vectors
!         i = 1
!         do
!            if (NOT ei(i).is_zero(TOL(20))) then
!               eigenvalues(i)   = cmplx(er(i)  ,ei(i),  kind=CPX_KIND)
!               eigenvalues(i+1) = cmplx(er(i+1),ei(i+1),kind=CPX_KIND)
!               left(:,i)    = cmplx(le(:,i), le(:,i+1),kind=CPX_KIND)
!               left(:,i+1)  = cmplx(le(:,i),-le(:,i+1),kind=CPX_KIND)
!               right(:,i)   = cmplx(re(:,i), re(:,i+1),kind=CPX_KIND)
!               right(:,i+1) = cmplx(re(:,i),-re(:,i+1),kind=CPX_KIND)
!               i = i + 2
!            else
!               eigenvalues(i)   = cmplx(er(i)  , ZERO,  kind=CPX_KIND)
!               left(:,i)    = cmplx(le(:,i),ZERO,kind=CPX_KIND)
!               right(:,i)   = cmplx(re(:,i),ZERO,kind=CPX_KIND)
!               i = i + 1
!            end
!            if (i>dim) exit
!         end
!         W.destroy
!         re.destroy; le.destroy
!         ei.destroy; er.destroy
!         A.destroy
!      end
!      if (normalise) then
!         do i = 1,dim
!            dot = dot_product(left(:,i),right(:,i))
!            dot = ONE/sqrt(dot)
!            left(:,i)  = dot*left(:,i)
!            right(:,i) = dot*right(:,i)
!         end
!      end
!   end

   subroutine solve_symmetric_eigenproblem(self,eigenvalues,eigenvectors)
    REALMAT(:,:) :: self
   ! Solve the symmetric eigenproblem for "self", yeilding a vector of
   ! "eigenvalues" and a matrix of "eigenvectors"
      REALVEC(:) :: eigenvalues
      REALMAT(:,:) :: eigenvectors
STACK("REALMAT:solve_symmetric_eigenproblem")
START_TIMER("REALMAT:solve_symmetric_eigenproblem")
#ifdef ESSL
      call solve_symm_eigenproblem_ESSL_(self,eigenvalues,eigenvectors)
#else
      call solve_symm_eigenproblem_LAPACK_(self,eigenvalues,eigenvectors)
#endif
     STOP_TIMER("REALMAT:solve_symmetric_eigenproblem")
      CHECK
   end subroutine

   subroutine solve_symm_eigenproblem_ESSL(self,eigenvalues,eigenvectors)
    REALMAT(:,:) :: self
   ! Solve the symmetric eigenproblem for "self", yeilding a vector of
   ! "eigenvalues" and a matrix of "eigenvectors". ESSL version.
      REALVEC(:) :: eigenvalues
      REALMAT(:,:) :: eigenvectors
      REALVEC(:), PTR :: ap,W
      INT :: dim
      STACK("REALMAT:solve_symm_eigenproblem_ESSL")
      START_TIMER("REALMAT:solve_symm_eigenproblem_ESSL")
      ENSURE(is_square_(self),"REALMAT:solve_symm_eigenproblem_ESSL ... non-square matrix")
      ENSURE(size(eigenvalues)>=size(self,1),"REALMAT:solve_symm_eigenproblem_ESSL ... eigenvalue array too small")
      ENSURE(size(eigenvectors)>=size(self),"REALMAT:solve_symm_eigenproblem_ESSL ... eigenvector matrix too small")
      dim = size(self,1)
      call create_(ap,dim*(dim+1)/2)
      call compress_to_triangle_(self,ap)
      call create_(W,2*dim)
#ifdef ESSL
      call dspev(21,ap,eigenvalues,eigenvectors,dim,dim,W,2*dim)
#endif
      call destroy_(W)
      call destroy_(ap)
     STOP_TIMER("REALMAT:solve_symm_eigenproblem_ESSL")
      CHECK
   end subroutine

   subroutine solve_symm_eigenproblem_LAPACK(self,eigenvalues,eigenvectors)
    REALMAT(:,:) :: self
   ! Solve the symmetric eigenproblem for "self", yeilding a vector of
   ! "eigenvalues" and a matrix of "eigenvectors". LAPACK version.
      REALVEC(:) :: eigenvalues
      REALMAT(:,:) :: eigenvectors
      REALVEC(:), PTR :: W
      INT :: dim,fail,lwork
      STACK("REALMAT:solve_symm_eigenproblem_LAPACK")
      START_TIMER("REALMAT:solve_symm_eigenproblem_LAPACK")
      ENSURE(is_square_(self),"REALMAT:solve_symm_eigenproblem_LAPACK ... non-square matrix")
      ENSURE(size(eigenvalues)>=size(self,1),"REALMAT:solve_symm_eigenproblem_LAPACK ... eigenvalue array too small")
      ENSURE(size(eigenvectors)>=size(self),"REALMAT:solve_symm_eigenproblem_LAPACK ... eigenvector matrix too small")
      dim = size(self,1)
      lwork = max(dim*dim,3*dim-1)
      call create_(W,lwork)
      eigenvectors = self
      fail = 0
#ifndef ESSL
      call dsyev("V","L",dim,eigenvectors,dim,eigenvalues,W,lwork,fail)
#endif
   ENSURE(fail==0,"REALMAT:solve_symm_eigenproblem_LAPACK ... no solution, error found")
      call destroy_(W)
     STOP_TIMER("REALMAT:solve_symm_eigenproblem_LAPACK")
      CHECK
   end subroutine

   subroutine solve_linear_equation(self,rhs,solution)
    REALMAT(:,:) :: self
   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
   ! yeilding vector "solution" as the answer
      REALVEC(:) :: rhs, solution
STACK("REALMAT:solve_linear_equation")
START_TIMER("REALMAT:solve_linear_equation")
#ifdef ESSL
      call solve_linear_equation_ESSL_(self,rhs,solution)
#else
      call solve_linear_equation_LAPACK_(self,rhs,solution)
#endif
     STOP_TIMER("REALMAT:solve_linear_equation")
      CHECK
   end subroutine

   subroutine solve_linear_equation_ESSL(self,rhs,solution)
    REALMAT(:,:) :: self
   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
   ! yeilding vector "solution" as the answer. ESSL version
      REALVEC(:) :: rhs, solution
      REALMAT(:,:), PTR :: LU
      INTVEC(:), PTR :: pivot
      INT :: dim
      STACK("REALMAT:solve_linear_equation_ESSL")
      START_TIMER("REALMAT:solve_linear_equation_ESSL")
      ENSURE(is_square_(self),"REALMAT:solve_linear_equation_ESSL ... non-square matrix")
      ENSURE(size(rhs)==size(self,1),"REALMAT:solve_linear_equation_ESSL ... incompatible rhs")
      dim = size(rhs)
      call create_(LU,dim,dim)
      call create_(pivot,dim)
      LU = self
      solution = rhs
#ifdef ESSL
      call dgef(LU,dim,dim,pivot)
      call dges(LU,dim,dim,pivot,solution,0)
#endif
      call destroy_(pivot)
      call destroy_(LU)
     STOP_TIMER("REALMAT:solve_linear_equation_ESSL")
      CHECK
   end subroutine

   subroutine solve_linear_equation_LAPACK(self,rhs,solution)
    REALMAT(:,:) :: self
   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
   ! yeilding vector "solution" as the answer. LAPACK version.
      REALVEC(:) :: rhs, solution
      REALMAT(:,:), PTR :: LU
      INTVEC(:), PTR :: pivot
      INT :: dim,nrhs,err
      STACK("REALMAT:solve_linear_equation_LAPACK")
      START_TIMER("REALMAT:solve_linear_equation_LAPACK")
      ENSURE(is_square_(self),"REALMAT:solve_linear_equation_LAPACK ... non-square matrix")
      ENSURE(size(rhs)==size(self,1),"REALMAT:solve_linear_equation_LAPACK ... incompatible rhs")
      dim = size(rhs)
      nrhs = 1
      nullify(LU); call create_(LU,dim,dim)
      nullify(pivot); call create_(pivot,dim)
      LU = self
      solution = rhs
#ifndef ESSL
      call dgesv(dim,nrhs,LU,dim,pivot,solution,dim,err)
#endif
      ENSURE(err==0,"REALMAT:solve_linear_equation_LAPACK ... no solution, error found")
      call destroy_(pivot)
      call destroy_(LU)
     STOP_TIMER("REALMAT:solve_linear_equation_LAPACK")
      CHECK
   end subroutine

   subroutine solve_linear_equations(self,rhs,solution)
    REALMAT(:,:) :: self
   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
   ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
      REALMAT(:,:) :: rhs, solution
STACK("REALMAT:solve_linear_equations")
START_TIMER("REALMAT:solve_linear_equations")
#ifdef ESSL
      call solve_linear_equations_ESSL_(self,rhs,solution)
#else
      call solve_linear_equations_LAPACK_(self,rhs,solution)
#endif
     STOP_TIMER("REALMAT:solve_linear_equations")
      CHECK
   end subroutine

   subroutine solve_linear_equations_ESSL(self,rhs,solution)
    REALMAT(:,:) :: self
   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
   ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
   ! ESSL version.
      REALMAT(:,:) :: rhs, solution
      REALMAT(:,:), PTR :: LU
      INTVEC(:), PTR :: pivot
      INT :: dim1,nrhs
      STACK("REALMAT:solve_linear_equations_ESSL")
      START_TIMER("REALMAT:solve_linear_equations_ESSL")
      ENSURE(is_square_(self),"REALMAT:solve_linear_equations_ESSL ... non-square matrix")
      ENSURE(size(rhs,1)==size(self,2),"REALMAT:solve_linear_equations_ESSL ... rhs incompatible with coefficient matrix")
      ENSURE(nrhs>0,"REALMAT:solve_linear_equations_ESSL ... no rhs vectors")
      dim1 = size(rhs,1)
      nrhs = size(rhs,2)
      call create_(LU,dim1,dim1)
      call create_(pivot,dim1)
      LU = self
      solution = rhs
#ifdef ESSL
      call dgef(LU,dim1,dim1,pivot)
      call dgesm("N",LU,dim1,dim1,pivot,solution,dim1,nrhs)
#endif
      call destroy_(pivot)
      call destroy_(LU)
     STOP_TIMER("REALMAT:solve_linear_equations_ESSL")
      CHECK
   end subroutine

   subroutine solve_linear_equations_LAPACK(self,rhs,solution)
    REALMAT(:,:) :: self
   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS
   ! vectors, yeilding matrix "solution" as a matrix of solution vectors.
   ! LAPACK version
      REALMAT(:,:) :: rhs, solution
      REALMAT(:,:), PTR :: LU
      INTVEC(:), PTR :: pivot
      INT :: dim1,nrhs,err
      STACK("REALMAT:solve_linear_equations_LAPACK")
      START_TIMER("REALMAT:solve_linear_equations_LAPACK")
      ENSURE(is_square_(self),"REALMAT:solve_linear_equations_LAPACK ... non-square matrix")
      ENSURE(size(rhs,1)==size(self,2),"REALMAT:solve_linear_equations_LAPACK ... rhs incompatible with coefficient matrix")
      ENSURE(nrhs>0,"REALMAT:solve_linear_equations_LAPACK ... no rhs vectors")
      dim1 = size(rhs,1)
      nrhs = size(rhs,2)
      call create_(LU,dim1,dim1)
      call create_(pivot,dim1)
      LU = self
      solution = rhs
#ifndef ESSL
      call dgesv(dim1,nrhs,LU,dim1,pivot,solution,dim1,err)
#endif
      call destroy_(pivot)
      call destroy_(LU)
      ENSURE(err==0,"REALMAT:solve_linear_equations_LAPACK ... no solution, error found")
     STOP_TIMER("REALMAT:solve_linear_equations_LAPACK")
      CHECK
   end subroutine

!  Unused ESSL routines

!   solve_general_eigenproblem(eigenvalues,eigenvectors)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "eigenvectors"
!      eigenvalues :: CPXVEC
!      eigenvectors :: CPXMAT
!       W :: REALVEC*
!      dim1,dim2,dime,dimv :: INT
!      select :: BIN
!      dim1 = size(self,1)
!      dim2 = size(self,2)
!      dime = size(eigenvalues)
!      dimv = size(eigenvectors)
!      ENSURE(dim1==dim2,"non-square matrix")
!      ENSURE(dime>=dim1,"eigenvalue array too small")
!      ENSURE(dimv>=dim1*dim1,"eigenvector matrix too small")
!      W.create(2*dim1)
!      call dgeev(1,self,dim1,eigenvalues,eigenvectors,dim1,select,dim1,W,2*dim1)
!      W.destroy
!   end
!
!   solve_general_eigenproblem(eigenvalues,left,right,normalize)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "left" and "right" eigenvectors. If "normalize" is present
!   ! and FALSE, the left and right eigenvectors are not automatically
!   ! renormalized so that (left)^T (right) = 1.
!   ! NOTE : this routine fails if there are complex eigenvalues. Use the complex
!   ! routine in this case.
!      eigenvalues :: REALVEC, target
!      left,right :: SELF_TYPE, target
!      normalize :: BIN, optional
!      er,ei,W :: REALVEC*
!      A,le,re :: SELF_TYPE*
!      i,dim,dimW, info :: INT
!      normalise :: BIN
!      dot :: REAL
!      ENSURE(.is_square,"non-square matrix")
!      ENSURE(size(eigenvalues)>=.dim1,"eigenvalues array too small")
!      ENSURE(size(left)>=size(self),"left eigenvector matrix too small")
!      ENSURE(size(right)>=size(self),"right eigenvector matrix too small")
!      dim = size(self,1)
!      normalise = TRUE
!      if (present(normalize)) normalise = normalize
!      if (.is_symmetric) then
!         .solve_symmetric_eigenproblem(eigenvalues,right)
!         left = right
!      else
!         A.create(dim,dim)
!         ei.create(dim)
!         er => eigenvalues
!         le => left
!         re => right
!         dimW = 8*dim
!         W.create(dimW)
!         A = self
!         ! Solve the eigenvalueproblem
!         call dgeev('V','V',dim,A,dim,er,ei,le,dim,re,dim,W,dimW,info)
!         ENSURE(info==0,"error, info="// trim(info.to_str))
!         ! Search for the complex eigenvalues/vectors
!         do i = 1,dim
!            if (NOT ei(i).is_zero(TOL(20))) then
!               DIE("There are complex eigenvalues, use the complex routine")
!            end
!         end
!         W.destroy
!         ei.destroy
!         A.destroy
!      end
!      if (normalise) then
!         do i = 1,dim
!            dot = dot_product(left(:,i),right(:,i))
!            dot = ONE/sqrt(dot)
!            left(:,i)  = dot*left(:,i)
!            right(:,i) = dot*right(:,i)
!         end
!      end
!   end

   function trace(self) result(res)
    REALMAT(:,:) :: self
   ! Return the trace of self
      IN :: self
      REAL :: res
   ! The following code is inherited from INTRINSICMAT
      INT :: dim,i
      STACK("REALMAT:trace")
      START_TIMER("REALMAT:trace")
      ENSURE(size(self,1)==size(self,2),"REALMAT:trace ... non-square matrix")
      dim = size(self,1)
      res = ZERO
      do i = 1,dim
         res = res + self(i,i)
      end do
     STOP_TIMER("REALMAT:trace")
      CHECK
   end function

   function trace_product_with(self,b) result(res)
    REALMAT(:,:) :: self
   ! Return the trace of the product of "self" with matrix b.
      IN :: self
      REALMAT(:,:), IN :: b
      REAL :: res
   ! The following code is inherited from INTRINSICMAT
      INT :: i
      STACK("REALMAT:trace_product_with")
      START_TIMER("REALMAT:trace_product_with")
      ENSURE(is_transposed_shape_of_(self,b),"REALMAT:trace_product_with ... incompatible dimensions")
      res = ZERO
      do i = 1,size(self,1)
         res = res + sum( self(i,:)*b(:,i) )
      end do
     STOP_TIMER("REALMAT:trace_product_with")
      CHECK
   end function

   function trace_product_with_1(self,b,c,d) result(res)
    REALMAT(:,:) :: self
   ! Return the trace of the product of "self" with matrices "b" ... "d".
      REALMAT(:,:) :: b,c,d
      REAL :: res
      REALMAT(:,:), PTR :: W1,W2
      STACK("REALMAT:trace_product_with_1")
      START_TIMER("REALMAT:trace_product_with_1")
      call create_(W2,size(b,1),size(d,2))
      call create_(W1,size(c,1),size(d,2))
      call to_product_of_(W1,c,d)
      call to_product_of_(W2,b,W1)
      call destroy_(W1)
      call create_(W1, size(self,1),size(d,2))
      call to_product_of_(W1,self,W2)
      res = trace_(W1)
      call destroy_(W1)
      call destroy_(W2)
     STOP_TIMER("REALMAT:trace_product_with_1")
      CHECK
   end function

   function trace_product_with_2(self,b,c,d,e,f) result(res)
    REALMAT(:,:) :: self
   ! Return the trace of the product of "self" with matrices "b" ... "f".
      REALMAT(:,:) :: b,c,d,e,f
      REAL :: res
      REALMAT(:,:), PTR :: W1,W2
      STACK("REALMAT:trace_product_with_2")
      START_TIMER("REALMAT:trace_product_with_2")
      call create_(W1,size(e,1),size(f,2))
      call to_product_of_(W1,e,f)     ! e*f
      call create_(W2,size(d,1),size(f,2))
      call to_product_of_(W2,d,W1)    ! d*e*f
      call destroy_(W1); W1 => W2
      call create_(W2,size(c,1),size(f,2))
      call to_product_of_(W2,c,W1)    ! c*d*e*f
      call destroy_(W1); W1 => W2
      call create_(W2,size(b,1),size(f,2))
      call to_product_of_(W2,b,W1)    ! b*c*d*e*f
      call destroy_(W1); W1 => W2
      call create_(W2, size(self,1),size(f,2))
      call to_product_of_(W2,self,W1) ! self*b*c*d*e*f
      res = trace_(W2)
      call destroy_(W2)
      call destroy_(W1)
     STOP_TIMER("REALMAT:trace_product_with_2")
      CHECK
   end function

   function equals(self,b) result(res)
    REALMAT(:,:) :: self
   ! Check if the matrix is the same as "b".
      IN :: self
      REALMAT(:,:), IN :: b
      BIN :: res
      STACK("REALMAT:equals")
      START_TIMER("REALMAT:equals")
      res = same_as_(self,b)
     STOP_TIMER("REALMAT:equals")
      CHECK
   end function

   function same_as(self,b,eps,diff) result(res)
    REALMAT(:,:) :: self
   ! Check if the matrix is the same as "b", within "eps", and return the
   ! actual difference in "diff"
      IN :: self
      REALMAT(:,:), IN :: b
      REAL, IN, optional :: eps
      REAL, optional :: diff
      BIN :: res
      INT :: i
      REAL :: del,tolerance
   STACK("REALMAT:same_as")
   START_TIMER("REALMAT:same_as")
   ENSURE(is_same_shape_as_(self,b),"REALMAT:same_as ... incompatible dimensions")
      tolerance = REAL_EPSILON
      if (present(eps)) tolerance = eps
      del = ZERO
      do i = 1,size(self,2)
         del = del + sum( (self(:,i)-b(:,i))**2 )
      end do
      del = sqrt(del)
      res = FALSE
      if (del<tolerance) res=TRUE
      if (present(diff)) diff=del
     STOP_TIMER("REALMAT:same_as")
      CHECK
   end function

   function has_column(self,c,eps,col) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" has a column "c", with "eps" tolerance.
   ! If present, the matching column index "col" is also returned.
      IN :: self
      REALVEC(:), IN :: c
      REAL, optional :: eps
      INT, optional :: col
      BIN :: res
      INT :: n
   STACK("REALMAT:has_column")
   START_TIMER("REALMAT:has_column")
   ENSURE(size(c)==size(self,1),"REALMAT:has_column ... incompatible column size")
      res = FALSE
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c,eps)
         if (res) then
            if (present(col)) col = n
            exit
         end if
      end do
     STOP_TIMER("REALMAT:has_column")
      CHECK
   end function

   function column_index(self,c,eps) result(res)
    REALMAT(:,:) :: self
   ! The matching column index "col" is returned, if the column matches "col" to
   ! tolerance "tol".  "tol" is optional.
      IN :: self
      REALVEC(:), IN :: c
      INT :: res
      REAL, optional :: eps
      INT :: n
      STACK("REALMAT:column_index")
      START_TIMER("REALMAT:column_index")
      ENSURE(size(c)==size(self,1),"REALMAT:column_index ... incompatible column size")
      res = 0
      do n = 1,size(self,2)
         if (same_as_(self(:,n),c,eps)) then
            res = n
            exit
         end if
      end do
     STOP_TIMER("REALMAT:column_index")
      CHECK
   end function

   function is_diagonal(self) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is a diagonal matrix
      IN :: self
      BIN :: res
      INT :: dim,i,j
      BIN :: off_diagonal_is_zero
      STACK("REALMAT:is_diagonal")
      START_TIMER("REALMAT:is_diagonal")
      ENSURE(is_square_(self),"REALMAT:is_diagonal ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         off_diagonal_is_zero = is_zero_(self(i,j))
         if (off_diagonal_is_zero) cycle
         res = FALSE
         STOP_TIMER("REALMAT:is_diagonal") CHECK return
      end do
      end do
     STOP_TIMER("REALMAT:is_diagonal")
      CHECK
   end function

   function has_unit_diagonal(self) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" has 1's as diagonal elements
      IN :: self
      BIN :: res
      INT :: i
      BIN :: diagonal_is_one
      STACK("REALMAT:has_unit_diagonal")
      START_TIMER("REALMAT:has_unit_diagonal")
      ENSURE(is_square_(self),"REALMAT:has_unit_diagonal ... Non-square matrix")
      res = TRUE
      do i = 1,size(self,1)
         diagonal_is_one = is_zero_((ONE - self(i,i)))
         if (diagonal_is_one) cycle
         res = FALSE
         STOP_TIMER("REALMAT:has_unit_diagonal") CHECK return
      end do
     STOP_TIMER("REALMAT:has_unit_diagonal")
      CHECK
   end function

   function is_symmetric(self) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is a symmetric matrix
      IN :: self
      BIN :: res
      INT :: dim,i,j
      REAL :: diff
      STACK("REALMAT:is_symmetric")
      START_TIMER("REALMAT:is_symmetric")
      ENSURE(is_square_(self),"REALMAT:is_symmetric ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
      do j = 1,i-1
         diff = abs(self(i,j)-self(j,i))
         if (is_zero_(diff)) cycle
         res = FALSE
         STOP_TIMER("REALMAT:is_symmetric") CHECK return
      end do
      end do
     STOP_TIMER("REALMAT:is_symmetric")
      CHECK
   end function

   function is_unit_matrix(self) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is the unit matrix
      IN :: self
      BIN :: res
      INT :: dim,i
      BIN :: diagonal_is_one
      STACK("REALMAT:is_unit_matrix")
      START_TIMER("REALMAT:is_unit_matrix")
      ENSURE(is_square_(self),"REALMAT:is_unit_matrix ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
         diagonal_is_one = is_zero_((self(i,i)-ONE))
         if (diagonal_is_one) cycle
         res = FALSE
         exit
      end do
      if (res) res = is_diagonal_(self)
     STOP_TIMER("REALMAT:is_unit_matrix")
      CHECK
   end function

   function is_inversion_matrix(self) result(res)
    REALMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is an inversion matrix
   ! i.e. minus the unit matrix
      IN :: self
      BIN :: res
      INT :: dim,i
      BIN :: diagonal_is_minus_one
      STACK("REALMAT:is_inversion_matrix")
      START_TIMER("REALMAT:is_inversion_matrix")
      ENSURE(is_square_(self),"REALMAT:is_inversion_matrix ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
         diagonal_is_minus_one = is_zero_((self(i,i)+ONE))
         if (diagonal_is_minus_one) cycle
         res = FALSE
         exit
      end do
      if (res) res = is_diagonal_(self)
     STOP_TIMER("REALMAT:is_inversion_matrix")
      CHECK
   end function

   function sum_row_vectors(self) result(res)
    REALMAT(:,:) :: self
   ! Sum the row vectors (i.e. columns) in "self".
      REALVEC(size(self,2)) :: res
      INT :: j
      STACK("REALMAT:sum_row_vectors")
      START_TIMER("REALMAT:sum_row_vectors")
      do j = 1,size(self,2)
         res(j) = sum(self(:,j))
      end do
     STOP_TIMER("REALMAT:sum_row_vectors")
      CHECK
   end function

   function sum_column_vectors(self) result(res)
    REALMAT(:,:) :: self
   ! Sum the column vectors (i.e. rows) in "self".
      REALVEC(size(self,1)) :: res
      INT :: i
      STACK("REALMAT:sum_column_vectors")
      START_TIMER("REALMAT:sum_column_vectors")
      do i = 1,size(self,1)
         res(i) = sum(self(i,:))
      end do
     STOP_TIMER("REALMAT:sum_column_vectors")
      CHECK
   end function

   subroutine swap_columns(self,col1,col2)
    REALMAT(:,:) :: self
   ! Swap columns "col1" and "col2" of self
      INT, IN :: col1,col2
   ! The following code is inherited from INTRINSICMAT
      INT :: a1,a2,i
      REAL :: val
      STACK("REALMAT:swap_columns")
      START_TIMER("REALMAT:swap_columns")
      ENSURE(col1<=size(self,2) AND col2<=size(self,2),"REALMAT:swap_columns ... columns exceed dimesions")
      a1 = size(self,1)
      a2 = size(self,2)
      do i = 1,a1
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do
     STOP_TIMER("REALMAT:swap_columns")
      CHECK
   end subroutine

   subroutine swap_columns_1(self,list)
    REALMAT(:,:) :: self
   ! Sequentially swap all columns in a column "list",
   ! self(:,i)      = self(:,list(i))
   ! self(:,col(i)) = self(:,i)
      INTVEC(:), IN :: list
      INT :: l
      STACK("REALMAT:swap_columns_1")
      START_TIMER("REALMAT:swap_columns_1")
      ENSURE(maxval(list)<=size(self,2),"REALMAT:swap_columns_1 ... list value exceed column dimension")
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do
     STOP_TIMER("REALMAT:swap_columns_1")
      CHECK
   end subroutine

   function column_norms(self) result(res)
    REALMAT(:,:) :: self
   ! Return the norms of every column
      REALVEC(size(self,2)) :: res
      INT :: i
      STACK("REALMAT:column_norms")
      START_TIMER("REALMAT:column_norms")
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
     STOP_TIMER("REALMAT:column_norms")
      CHECK
   end function

   subroutine get_column_norms(self,res)
    REALMAT(:,:) :: self
   ! Return the norms of every column
      REALVEC(:) :: res
      INT :: i
   STACK("REALMAT:get_column_norms")
   START_TIMER("REALMAT:get_column_norms")
   ENSURE(size(res)==size(self,2),"REALMAT:get_column_norms ... wrong size, res array")
      do i = 1,size(self,2)
         res(i) = norm_(self(:,i))
      end do
     STOP_TIMER("REALMAT:get_column_norms")
      CHECK
   end subroutine

   subroutine get_column_dot_products(self,res)
    REALMAT(:,:) :: self
   ! Return the dot products of every column with itself.
   ! Goot for testing distances without using a sqrt.
      REALVEC(:) :: res
      INT :: i
   STACK("REALMAT:get_column_dot_products")
   START_TIMER("REALMAT:get_column_dot_products")
   ENSURE(size(res)==size(self,2),"REALMAT:get_column_dot_products ... wrong size, res array")
      do i = 1,size(self,2)
         res(i) = dot_product(self(:,i),self(:,i))
      end do
     STOP_TIMER("REALMAT:get_column_dot_products")
      CHECK
   end subroutine

   function index_of_minimum_column_norm(self,offset) result(res)
    REALMAT(:,:) :: self
   ! Return the column index of the column with the *minimum* norm. If present,
   ! "offset" is subtracted from every column beforehand, and then added back
   ! afterwards. This is useful for finding the index of the column with minimum
   ! distance to "offset", for a list of points held in "self".
      REALVEC(3), optional :: offset
      INT :: res
      INT :: i
      REAL :: val,tmp
      STACK("REALMAT:index_of_minimum_column_norm")
      START_TIMER("REALMAT:index_of_minimum_column_norm")
      if (present(offset)) &
         self = self - spread(offset,dim=2,ncopies=size(self,2))
      res = 1
      val = norm_(self(:,1))
      do i = 2,size(self,2)
         tmp = norm_(self(:,i))
         if (tmp>=val) cycle
         val = tmp
         res = i
      end do
      if (present(offset)) &
         self = self + spread(offset,dim=2,ncopies=size(self,2))
     STOP_TIMER("REALMAT:index_of_minimum_column_norm")
      CHECK
   end function

   function mean_column_vector(self) result(res)
    REALMAT(:,:) :: self
   ! Return the mean of the column vectors.
      REALVEC(size(self,1)) :: res
      STACK("REALMAT:mean_column_vector")
      START_TIMER("REALMAT:mean_column_vector")
      res = sum_column_vectors_(self)/size(self,2)
     STOP_TIMER("REALMAT:mean_column_vector")
      CHECK
   end function

   function max_abs_column_difference(self) result(res)
    REALMAT(:,:) :: self
   ! Return the maximum of the absolute difference between all the column vector
   ! pairs of the matrix.
      REALVEC(size(self,1)) :: res
      INT :: i,j,dim
      REALVEC(size(self,1)) :: diff,col_i,col_j
      STACK("REALMAT:max_abs_column_difference")
      START_TIMER("REALMAT:max_abs_column_difference")
      dim = size(self,2)
      diff = ZERO
      do i = 1,size(self,2)
         col_i = self(:,i)
         do j = 1,i-1
            col_j = self(:,j)
            diff = max(abs(col_i-col_j),diff)
         end do
      end do
      res = diff
     STOP_TIMER("REALMAT:max_abs_column_difference")
      CHECK
   end function

   subroutine set_to(self,b)
    REALMAT(:,:) :: self
   ! Set self to "b"
      REALMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:set_to")
      START_TIMER("REALMAT:set_to")
      ENSURE(is_same_shape_as_(self,b),"REALMAT:set_to ... incompatible shape")
      self = b
     STOP_TIMER("REALMAT:set_to")
      CHECK
   end subroutine

   subroutine plus(self,b)
    REALMAT(:,:) :: self
   ! Add to self the matrix "b"
      REALMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:plus")
      START_TIMER("REALMAT:plus")
      ENSURE(is_same_shape_as_(self,b),"REALMAT:plus ... incompatible shape")
      self = self+b
     STOP_TIMER("REALMAT:plus")
      CHECK
   end subroutine

   subroutine minus(self,b)
    REALMAT(:,:) :: self
   ! Subtract from self the matrix "b"
      REALMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("REALMAT:minus")
      START_TIMER("REALMAT:minus")
      ENSURE(is_same_shape_as_(self,b),"REALMAT:minus ... incompatible shape")
      self = self-b
     STOP_TIMER("REALMAT:minus")
      CHECK
   end subroutine

   subroutine to_scaled_mat(self,fac,b)
    REALMAT(:,:) :: self
   ! Set "self" to matrix "b" scaled by "fac"
      REALMAT(:,:), IN :: b
      REAL, IN :: fac
      STACK("REALMAT:to_scaled_mat")
      START_TIMER("REALMAT:to_scaled_mat")
      ENSURE(is_same_shape_as_(self,b),"REALMAT:to_scaled_mat ... different shapes")
      self = fac*b
     STOP_TIMER("REALMAT:to_scaled_mat")
      CHECK
   end subroutine

   subroutine plus_scaled_mat(self,fac,b)
    REALMAT(:,:) :: self
   ! Add to "self" matrix "b" scaled by "fac"
      REALMAT(:,:), IN :: b
      REAL, IN :: fac
      STACK("REALMAT:plus_scaled_mat")
      START_TIMER("REALMAT:plus_scaled_mat")
      ENSURE(is_same_shape_as_(self,b),"REALMAT:plus_scaled_mat ... different shapes")
      self = self+fac*b
     STOP_TIMER("REALMAT:plus_scaled_mat")
      CHECK
   end subroutine

   PURE subroutine zero_small_values(self,tol)
    REALMAT(:,:) :: self
   ! Zero elements of the matrix which are less than "tol" in magnitude
      INOUT :: self
      REAL, IN :: tol
      where (abs(self)<tol)
        self = ZERO
      end where
     STOP_TIMER("REALMAT:zero_small_values")
   end subroutine

   function is_zero(self,eps) result(res)
    REALMAT(:,:) :: self
   ! Return TRUE is "self" is the zero matrix, i.e. every element is zero.
   ! If present, "eps" is used to decide when a small number is zero.
      REAL, optional, IN :: eps
      BIN :: res
      INT :: dim1,dim2,i,j
      BIN :: ij_is_zero
      STACK("REALMAT:is_zero")
      START_TIMER("REALMAT:is_zero")
      dim1 = size(self,1)
      dim2 = size(self,2)
      res = TRUE
      do i = 1,dim1
      do j = 1,dim2
         ij_is_zero = is_zero_(self(i,j),eps)
         if (ij_is_zero) cycle
         res = FALSE
         exit
      end do
      end do
     STOP_TIMER("REALMAT:is_zero")
      CHECK
   end function

   subroutine change_basis(self,V)
    REALMAT(:,:) :: self
   ! Change the basis of "self" using vectors "V"; self = V^T self V
      REALMAT(:,:), IN :: V
      REALMAT(:,:), PTR :: W
      INT :: o1,v2
      STACK("REALMAT:change_basis")
      START_TIMER("REALMAT:change_basis")
      ENSURE( is_square_(self),"REALMAT:change_basis ... Non-square matrix")
      ENSURE(is_square_(V),"REALMAT:change_basis ... Non-square matrix")
      ENSURE(size(self,2)==size(V,1),"REALMAT:change_basis ... wrong shapes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,transpose_a=TRUE)
      call destroy_(W)
     STOP_TIMER("REALMAT:change_basis")
      CHECK
   end subroutine

   subroutine change_basis_1(self,new,V)
    REALMAT(:,:) :: self
   ! Change the basis of "self" using vectors "V", and place the result in
   ! "new".  new = V^T self V
      REALMAT(:,:) :: new,V
      REALMAT(:,:), PTR :: W
      INT :: o1,v2
      STACK("REALMAT:change_basis_1")
      START_TIMER("REALMAT:change_basis_1")
      ENSURE(   is_square_(self),"REALMAT:change_basis_1 ... Non-square matrix")
      ENSURE(is_square_(new),"REALMAT:change_basis_1 ... Non-square matrix")
      ENSURE(size(V,1)==   size(self,2),"REALMAT:change_basis_1 ... wrong shapes")
      ENSURE(size(V,2)==size(new,2),"REALMAT:change_basis_1 ... wrong shapes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,transpose_a=TRUE)
      call destroy_(W)
     STOP_TIMER("REALMAT:change_basis_1")
      CHECK
   end subroutine

   subroutine change_basis_2(self,new,L,R)
    REALMAT(:,:) :: self
   ! Change the basis of "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L^T self R
      REALMAT(:,:) :: new,L,R
      REALMAT(:,:), PTR :: W
      INT :: o1,r2
      STACK("REALMAT:change_basis_2")
      START_TIMER("REALMAT:change_basis_2")
      ENSURE(size(self,2)==size(R,1),"REALMAT:change_basis_2 ... incompatible sizes")
      ENSURE(size(self,1)==size(L,1),"REALMAT:change_basis_2 ... incompatible sizes")
      ENSURE(size(new,2)==size(R,2),"REALMAT:change_basis_2 ... incompatible sizes")
      ENSURE(size(new,1)==size(L,2),"REALMAT:change_basis_2 ... incompatible sizes")
      o1 = size(self,1)
      r2 = size(R,2)
      call create_(W,o1,r2)
      call to_product_of_(W,self,R)
      call to_product_of_(new,L,W,transpose_a=TRUE)
      call destroy_(W)
     STOP_TIMER("REALMAT:change_basis_2")
      CHECK
   end subroutine

   subroutine change_basis_3(self,L,R)
    REALMAT(:,:) :: self
   ! Change the basis of "self" using diagonal matrices "L" and "R" (stored as
   ! vectors).  self = L self R
      REALVEC(:) :: L,R
      REALMAT(:,:), PTR :: W
      INT :: l1,r1
      STACK("REALMAT:change_basis_3")
      START_TIMER("REALMAT:change_basis_3")
      ENSURE(size(self,1)==size(L) AND size(self,2)==size(R),"REALMAT:change_basis_3 ... incompatible sizes")
      l1 = size(L)
      r1 = size(R)
      call create_(W,l1,r1)
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)
     STOP_TIMER("REALMAT:change_basis_3")
      CHECK
   end subroutine

   subroutine change_basis_4(self,V)
    REALMAT(:,:) :: self
   ! Change the basis of "self" using diagonal matrix "V" (stored as a vectors).
   ! self = V self V
      REALVEC(:) :: V
      REALMAT(:,:), PTR :: W
      INT :: v1
      STACK("REALMAT:change_basis_4")
      START_TIMER("REALMAT:change_basis_4")
      ENSURE(is_square_(self),"REALMAT:change_basis_4 ... non-square matrix")
      ENSURE(size(self,1)==size(V),"REALMAT:change_basis_4 ... incompatible sizes")
      v1 = size(V)
      call create_(W,v1,v1)
      call to_product_with_diagonal_(W,self,V)
      call to_product_with_diagonal_(self,V,W)
      call destroy_(W)
     STOP_TIMER("REALMAT:change_basis_4")
      CHECK
   end subroutine

   subroutine back_transform(self,V)
    REALMAT(:,:) :: self
   ! Back transform "self" using vectors "V", and place the result in "self".
   ! self = V self V^T
      REALMAT(:,:) :: V
      REALMAT(:,:), PTR :: W
      INT :: o2,v1
      STACK("REALMAT:back_transform")
      START_TIMER("REALMAT:back_transform")
      ENSURE(is_square_(self),"REALMAT:back_transform ... non-square matrix")
      ENSURE(is_same_shape_as_(self,V),"REALMAT:back_transform ... incompatible shape")
      o2 = size(self,2)
      v1 = size(V,1)
      call create_(W,v1,o2)
      call to_product_of_(W,V,self)
      call to_product_of_(self,W,V,transpose_b=TRUE)
      call destroy_(W)
     STOP_TIMER("REALMAT:back_transform")
      CHECK
   end subroutine

   subroutine back_transform_1(self,new,V)
    REALMAT(:,:) :: self
   ! Back transform "self" using vectors "V", and place the result in "new".
   ! new = V self V^T
      REALMAT(:,:) :: new,V
      REALMAT(:,:), PTR :: W
      INT :: o2,v1
      STACK("REALMAT:back_transform_1")
      START_TIMER("REALMAT:back_transform_1")
      ENSURE(   is_square_(self),"REALMAT:back_transform_1 ... non-square matrix")
      ENSURE(is_square_(new),"REALMAT:back_transform_1 ... non-square matrix")
      ENSURE(size(V,2)==size(self,1),"REALMAT:back_transform_1 ... incompatible sizes")
      ENSURE(size(V,1)==size(new,1),"REALMAT:back_transform_1 ... incompatible sizes")
      o2 = size(self,2)
      v1 = size(V,1)
      call create_(W,v1,o2)
      call to_product_of_(W,V,self)
      call to_product_of_(new,W,V,transpose_b=TRUE)
      call destroy_(W)
     STOP_TIMER("REALMAT:back_transform_1")
      CHECK
   end subroutine

   subroutine back_transform_2(self,new,L,R)
    REALMAT(:,:) :: self
   ! Back transform "self" using left and right matrices "L" and "R"
   ! and place the result in "new", new = L self R^T
      REALMAT(:,:) :: new,L,R
      REALMAT(:,:), PTR :: W
      INT :: o1,r1
      STACK("REALMAT:back_transform_2")
      START_TIMER("REALMAT:back_transform_2")
      ENSURE(size(self,2)==size(R,2),"REALMAT:back_transform_2 ... incompatible sizes")
      ENSURE(size(self,1)==size(L,2),"REALMAT:back_transform_2 ... incompatible sizes")
      ENSURE(size(new,2)==size(R,1),"REALMAT:back_transform_2 ... incompatible sizes")
      ENSURE(size(new,1)==size(L,1),"REALMAT:back_transform_2 ... incompatible sizes")
      o1 = size(self,1)
      r1 = size(R,1)
      call create_(W,o1,r1)
      call to_product_of_(W,self,R,transpose_b=TRUE)
      call to_product_of_(new,L,W)
      call destroy_(W)
     STOP_TIMER("REALMAT:back_transform_2")
      CHECK
   end subroutine

   subroutine similarity_transform(self,V)
    REALMAT(:,:) :: self
   ! Do a similarity transform of "self" using vectors "V": self = V self V^-1
      REALMAT(:,:), IN :: V
      REALMAT(:,:), PTR :: V1,W
      INT :: n
   STACK("REALMAT:similarity_transform")
   START_TIMER("REALMAT:similarity_transform")
   ENSURE( is_square_(self),"REALMAT:similarity_transform ... Non-square matrix")
   ENSURE(is_square_(V),"REALMAT:similarity_transform ... Non-square matrix")
   ENSURE(size(self,1)==size(V,1),"REALMAT:similarity_transform ... wrong shapes")
      n = size(self,1)
      call create_(V1,n,n)
      call to_inverse_of_(V1,V)
      call create_(W,n,n)
      call to_product_of_(W,self,V1)
      call to_product_of_(self,V,W)
      call destroy_(W)
      call destroy_(V1)
     STOP_TIMER("REALMAT:similarity_transform")
      CHECK
   end subroutine

   subroutine compress_to_triangle(self,tr)
    REALMAT(:,:) :: self
   ! Converts the lower triangle of matrix self to the triangle "tr".
   ! using row order.
      IN :: self
      REALVEC(:) :: tr
      INT :: dim,i,j,ij
      STACK("REALMAT:compress_to_triangle")
      START_TIMER("REALMAT:compress_to_triangle")
      ENSURE(is_square_(self),"REALMAT:compress_to_triangle ... non-square matrix")
      ENSURE(size(tr)>=tri_size_(self),"REALMAT:compress_to_triangle ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            tr(ij+j) = self(i,j)
!            tr(ij+j) = self(j,i)
         end do
         ij = ij+i
      end do
     STOP_TIMER("REALMAT:compress_to_triangle")
      CHECK
   end subroutine

   subroutine uncompress_from_triangle(self,tr)
    REALMAT(:,:) :: self
   ! Converts the triangle "tr" into the symmetric matrix "self".
      REALVEC(:) :: tr
      REAL :: tmp
      INT :: dim,i,j,ij
      STACK("REALMAT:uncompress_from_triangle")
      START_TIMER("REALMAT:uncompress_from_triangle")
      ENSURE(is_square_(self),"REALMAT:uncompress_from_triangle ... non-square matrix")
      ENSURE(size(tr)>=tri_size_(self),"REALMAT:uncompress_from_triangle ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            tmp = tr(ij+j)
            self(j,i) = tmp
            self(i,j) = tmp
         end do
         ij = ij+i
      end do
     STOP_TIMER("REALMAT:uncompress_from_triangle")
      CHECK
   end subroutine

   subroutine from_diagonal(self,d)
    REALMAT(:,:) :: self
   ! Converts the diagonal vector "d" to matrix "self".
      REALVEC(:) :: d
      INT :: dim,i
      STACK("REALMAT:from_diagonal")
      START_TIMER("REALMAT:from_diagonal")
      ENSURE(is_square_(self),"REALMAT:from_diagonal ... non-square matrix")
      ENSURE(size(d)==size(self,1),"REALMAT:from_diagonal ... incompatibale diagonal length")
      dim  = size(d)
      self = ZERO
      do i = 1,dim
         self(i,i) = d(i)
      end do
     STOP_TIMER("REALMAT:from_diagonal")
      CHECK
   end subroutine

   function tri_size(self) result(ltr)
    REALMAT(:,:) :: self
   ! Returns the size of the lower triangle needed to store self.
      IN :: self
      INT :: ltr
      INT :: dim
      STACK("REALMAT:tri_size")
      START_TIMER("REALMAT:tri_size")
      ENSURE(is_square_(self),"REALMAT:tri_size ... non-square matrix")
      dim = size(self,1)
      ltr = dim*(dim+1)/2
     STOP_TIMER("REALMAT:tri_size")
      CHECK
   end function

   subroutine to_unit_matrix(self)
    REALMAT(:,:) :: self
   ! Set "self" to the unit matrix
      STACK("REALMAT:to_unit_matrix")
      START_TIMER("REALMAT:to_unit_matrix")
      call to_unit_mat_(self)
     STOP_TIMER("REALMAT:to_unit_matrix")
      CHECK
   end subroutine

   subroutine to_unit_mat(self)
    REALMAT(:,:) :: self
   ! Set "self" to the unit matrix
      INT :: i
   STACK("REALMAT:to_unit_mat")
   START_TIMER("REALMAT:to_unit_mat")
   ENSURE(is_square_(self),"REALMAT:to_unit_mat ... non-square matrix")
      self = ZERO
      do i = 1,size(self,1)
         self(i,i) = ONE
      end do
     STOP_TIMER("REALMAT:to_unit_mat")
      CHECK
   end subroutine

   subroutine set_diagonal(self,val)
    REALMAT(:,:) :: self
   ! Set the diagonal of "self" to "val"
      REAL :: val
      INT :: dim,i
      STACK("REALMAT:set_diagonal")
      START_TIMER("REALMAT:set_diagonal")
      ENSURE(is_square_(self),"REALMAT:set_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = val
      end do
     STOP_TIMER("REALMAT:set_diagonal")
      CHECK
   end subroutine

   subroutine add_to_diagonal(self,val)
    REALMAT(:,:) :: self
   ! Add "val" to the diagonal of "self"
      REAL :: val
      INT :: dim,i
      STACK("REALMAT:add_to_diagonal")
      START_TIMER("REALMAT:add_to_diagonal")
      ENSURE(is_square_(self),"REALMAT:add_to_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = self(i,i) + val
      end do
     STOP_TIMER("REALMAT:add_to_diagonal")
      CHECK
   end subroutine

   subroutine zero_diagonal(self)
    REALMAT(:,:) :: self
   ! Zero the diagonal elements of "self"
      INT :: dim,i
      STACK("REALMAT:zero_diagonal")
      START_TIMER("REALMAT:zero_diagonal")
      ENSURE(is_square_(self),"REALMAT:zero_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = ZERO
      end do
     STOP_TIMER("REALMAT:zero_diagonal")
      CHECK
   end subroutine

   subroutine zero_off_diagonal(self)
    REALMAT(:,:) :: self
   ! Zero the off diagonal elements of "self"
      INT :: dim,i,j
      STACK("REALMAT:zero_off_diagonal")
      START_TIMER("REALMAT:zero_off_diagonal")
      ENSURE(is_square_(self),"REALMAT:zero_off_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         self(i,j) = ZERO
      end do
      end do
     STOP_TIMER("REALMAT:zero_off_diagonal")
      CHECK
   end subroutine

   subroutine weight_diagonal(self,fac)
    REALMAT(:,:) :: self
   ! Weight the diagonal elements of "self" by "fac"
      REAL, IN :: fac
      INT :: dim,i
      STACK("REALMAT:weight_diagonal")
      START_TIMER("REALMAT:weight_diagonal")
      ENSURE(is_square_(self),"REALMAT:weight_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do
     STOP_TIMER("REALMAT:weight_diagonal")
      CHECK
   end subroutine

   subroutine get_diagonal(self,diag)
    REALMAT(:,:) :: self
   ! Get the diagonal elements of "self" in vector "diag"
      REALVEC(:) :: diag
      INT :: dim,i
      STACK("REALMAT:get_diagonal")
      START_TIMER("REALMAT:get_diagonal")
      ENSURE(size(diag)==min(size(self,1),size(self,2)),"REALMAT:get_diagonal ... diag vector is incompatible")
      dim  = size(diag)
      do i = 1,dim
         diag(i) = self(i,i)
      end do
     STOP_TIMER("REALMAT:get_diagonal")
      CHECK
   end subroutine

   function max_diagonal_element(self) result(res)
    REALMAT(:,:) :: self
   ! Get the maximum element on the diagonal of the matrix
      REAL :: res
      INT :: dim,i
      STACK("REALMAT:max_diagonal_element")
      START_TIMER("REALMAT:max_diagonal_element")
      ENSURE(min(size(self,1),size(self,2))>0,"REALMAT:max_diagonal_element ... cannot have zero sized dimensions")
      dim = min(size(self,1),size(self,2))
      res = self(1,1)
      do i = 2,dim
         res = max(self(i,i),res)
      end do
     STOP_TIMER("REALMAT:max_diagonal_element")
      CHECK
   end function

   function max_abs_diagonal_element(self) result(res)
    REALMAT(:,:) :: self
   ! Get the maximum absolute value of the diagonal elements of the self matrix
      REAL :: res
      INT :: dim,i
      STACK("REALMAT:max_abs_diagonal_element")
      START_TIMER("REALMAT:max_abs_diagonal_element")
      ENSURE(min(size(self,1),size(self,2))>0,"REALMAT:max_abs_diagonal_element ... cannot have zero sized dimensions")
      dim = min(size(self,1),size(self,2))
      res = abs(self(1,1))
      do i = 2,dim
         res = max(abs(self(i,i)),res)
      end do
     STOP_TIMER("REALMAT:max_abs_diagonal_element")
      CHECK
   end function

   subroutine symmetrize(self)
    REALMAT(:,:) :: self
   ! Set self to half of itself plus half its transpose, i.e.
   ! self = 1/2 (self + self^T)
      INT :: dim,i,j
      REAL :: val
   STACK("REALMAT:symmetrize")
   START_TIMER("REALMAT:symmetrize")
   ENSURE(is_square_(self),"REALMAT:symmetrize ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            val = HALF*(self(i,j)+self(j,i))
            self(i,j) = val
            self(j,i) = val
         end do
      end do
     STOP_TIMER("REALMAT:symmetrize")
      CHECK
   end subroutine

   subroutine antisymmetrize(self)
    REALMAT(:,:) :: self
   ! Set self to half of itself minus half its transpose, i.e.
   ! self = 1/2 (self - self^T)
      INT :: dim,i,j
      REAL :: val
   STACK("REALMAT:antisymmetrize")
   START_TIMER("REALMAT:antisymmetrize")
   ENSURE(is_square_(self),"REALMAT:antisymmetrize ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i
            val = HALF*(self(i,j)-self(j,i))
            self(i,j) =  val
            self(j,i) = -val
         end do
      end do
     STOP_TIMER("REALMAT:antisymmetrize")
      CHECK
   end subroutine

   subroutine symmetric_fold(self)
    REALMAT(:,:) :: self
   ! Add the upper triangle of "self" into the lower triangle
      INT :: dim,i,j
      STACK("REALMAT:symmetric_fold")
      START_TIMER("REALMAT:symmetric_fold")
      ENSURE(is_square_(self),"REALMAT:symmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)+self(j,i)
         end do
      end do
     STOP_TIMER("REALMAT:symmetric_fold")
      CHECK
   end subroutine

   subroutine antisymmetric_fold(self)
    REALMAT(:,:) :: self
   ! Subtract the upper triangle of "self" into the lower triangle
      INT :: dim,i,j
      STACK("REALMAT:antisymmetric_fold")
      START_TIMER("REALMAT:antisymmetric_fold")
      ENSURE(is_square_(self),"REALMAT:antisymmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)-self(j,i)
         end do
      end do
     STOP_TIMER("REALMAT:antisymmetric_fold")
      CHECK
   end subroutine

   subroutine symmetric_fold_to_tri(self,tr)
    REALMAT(:,:) :: self
   ! Add the upper triangle of "self" into the lower triangle and return
   ! the lower triangle "tr"
      REALVEC(:) :: tr
      INT :: dim,i,j,ij
      STACK("REALMAT:symmetric_fold_to_tri")
      START_TIMER("REALMAT:symmetric_fold_to_tri")
      ENSURE(is_square_(self),"REALMAT:symmetric_fold_to_tri ... non-square matrix")
      ENSURE(size(tr)>=tri_size_(self),"REALMAT:symmetric_fold_to_tri ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            ij = ij+1
            if (i==j) then
               tr(ij) = self(i,j)
            else
               tr(ij) = self(i,j)+self(i,j)
            end if
         end do
      end do
     STOP_TIMER("REALMAT:symmetric_fold_to_tri")
      CHECK
   end subroutine

   subroutine symmetric_reflect(self)
    REALMAT(:,:) :: self
   ! Make the upper triangle of "self" the same as the lower triangle
      INT :: dim,i,j
      STACK("REALMAT:symmetric_reflect")
      START_TIMER("REALMAT:symmetric_reflect")
      ENSURE(is_square_(self),"REALMAT:symmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j = 1,i-1
            self(j,i) = self(i,j)
         end do
      end do
     STOP_TIMER("REALMAT:symmetric_reflect")
      CHECK
   end subroutine

   subroutine antisymmetric_reflect(self)
    REALMAT(:,:) :: self
   ! Make the upper triangle of "self" the negative of the lower triangle
      INT :: dim,i,j
      STACK("REALMAT:antisymmetric_reflect")
      START_TIMER("REALMAT:antisymmetric_reflect")
      ENSURE(is_square_(self),"REALMAT:antisymmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j = 1,i-1
            self(j,i) = -self(i,j)
         end do
      end do
      do i = 1,dim
         self(i,i) = ZERO
      end do
     STOP_TIMER("REALMAT:antisymmetric_reflect")
      CHECK
   end subroutine

   subroutine schmidt_orthonormalise(self,S,scale)
    REALMAT(:,:) :: self
   ! Schmidt orthonormalise the column vectors in "self" using "S" as the
   ! metric. If "scale" is present, it is set to the product of the
   ! normalisation factors used to normalise each column after the Schmidt
   ! procedure.
     REALMAT(:,:), IN :: S
     REAL, optional :: scale
     target :: self
     INT :: dim,n,o
     REALVEC(:), PTR :: old,new
     INT :: j,k
     REAL :: proj,norm
     REALVEC(:), PTR :: T
     REAL :: fac
     STACK("REALMAT:schmidt_orthonormalise")
     START_TIMER("REALMAT:schmidt_orthonormalise")
     ENSURE(is_square_(S),"REALMAT:schmidt_orthonormalise ... metric S is not square")
     ENSURE(size(self,1)==size(S,1),"REALMAT:schmidt_orthonormalise ... incompatible metric S")
     ENSURE(NOT is_zero_(S),"REALMAT:schmidt_orthonormalise ... S is zero matrix")
     ENSURE(NOT is_zero_(self),"REALMAT:schmidt_orthonormalise ... self is zero matrix")
     if (present(scale)) scale = ONE

     if (present(scale)) then
       do n = 1,size(self,2)
          new => self(:,n)
          do o = 1,n-1
             old => self(:,o)
             fac = dot_(S,old,new)
             new = new - fac*old
          end do
          norm = dot_(S,new,new)
          ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"REALMAT:schmidt_orthonormalise ... linear dependence in vector "//to_str_(n))
          norm = ONE/sqrt(norm)
          new = new*norm
          scale = scale*norm
       end do
     end if

    dim = size(self,1)
    call create_(T,dim)
    do n=1,dim
      do j=1,dim
        T(j) = dot_product(self(:j,n),S(:j,j))
        T(1:j-1) = T(1:j-1) + self(j,n) * S(:j-1,j)
      end do
      do k=1,n-1
        proj = - dot_product(self(:,k),T)
        self(:,n) = self(:,n) + proj * self(:,k)
      end do
      do j=1,dim
        T(j) = dot_product(self(:j,n), S(:j,j))
        T(:j-1) = T(:j-1) + self(j,n) * S(:j-1,j)
      end do
      norm = dot_product(T,self(:,n))
      ENSURE(norm>TOL(10),"REALMAT:schmidt_orthonormalise ... linear dependence in vector " // to_str_(n))
      self(:,n) = self(:,n) / sqrt(norm)
    end do
    call destroy_(T)
     STOP_TIMER("REALMAT:schmidt_orthonormalise")
      CHECK
   end subroutine

   subroutine reverse_schmidt_orthonormalise(self,S)
    REALMAT(:,:) :: self
   ! Schmidt orthonormalise the column vectors in "self" using "S" as the
   ! metric.
     target :: self
     REALMAT(:,:) :: S
     REALVEC(:), PTR :: new,old,w
     REAL :: fac,norm
     INT :: dim,n,k
     STACK("REALMAT:reverse_schmidt_orthonormalise")
     START_TIMER("REALMAT:reverse_schmidt_orthonormalise")
     ENSURE(is_square_(self),"REALMAT:reverse_schmidt_orthonormalise ... non-square matrix")
     ENSURE(is_same_shape_as_(self,S),"REALMAT:reverse_schmidt_orthonormalise ... not same shape as S")
     ENSURE(NOT is_zero_(self),"REALMAT:reverse_schmidt_orthonormalise ... self is zero matrix")
     ENSURE(NOT is_zero_(self),"REALMAT:reverse_schmidt_orthonormalise ... self is zero matrix")
     dim = size(self,1)
     call create_(w,dim)
     do n = dim,1,-1
        new => self(:,n)
        do k = n-1,1,-1
           old => self(:,k)
           call to_product_of_(w,S,new)
           fac = dot_product(old,w)
           new = new - fac*old
        end do
        call to_product_of_(w,S,new)
        norm = dot_product(new,w)
        ENSURE(norm>TOL(10),"REALMAT:reverse_schmidt_orthonormalise ... linear dependence in vector " // to_str_(n))
        new = new/sqrt(norm)
     end do
     call destroy_(w)
     STOP_TIMER("REALMAT:reverse_schmidt_orthonormalise")
      CHECK
   end subroutine

   subroutine schmidt_orthonormalise_1(self)
    REALMAT(:,:) :: self
   ! Schmidt orthonormalise the column vectors in "self".
     target :: self
     REALVEC(:), PTR :: new,old
     REAL :: fac,norm
     INT :: n,k
     STACK("REALMAT:schmidt_orthonormalise_1")
     START_TIMER("REALMAT:schmidt_orthonormalise_1")
     ENSURE(size(self,1)>=size(self,2),"REALMAT:schmidt_orthonormalise_1 ... more vectors than dimension of vector space")
     do n = 1,size(self,2)
        new => self(:,n)
        do k = 1,n-1
           old => self(:,k)
           fac = dot_product(old,new)
           new = new - fac*old
        end do
        norm = dot_product(new,new)
        ENSURE(norm>TOL(10),"REALMAT:schmidt_orthonormalise_1 ... linear dependence in vector " // to_str_(n))
        new = new/sqrt(norm)
     end do
     STOP_TIMER("REALMAT:schmidt_orthonormalise_1")
      CHECK
   end subroutine

   subroutine reverse_schmidt_orthogonalise(self)
    REALMAT(:,:) :: self
   ! Schmidt orthonormalise the column vectors in "self" using unit metric.
      target :: self
      REALVEC(:), PTR :: new,old
      REAL :: fac,norm
      INT :: dim,n,k
      STACK("REALMAT:reverse_schmidt_orthogonalise")
      START_TIMER("REALMAT:reverse_schmidt_orthogonalise")
      ENSURE(is_square_(self),"REALMAT:reverse_schmidt_orthogonalise ... non square matrix")
      dim = size(self,1)
      do n = dim,1,-1
         new => self(:,n)
         do k = n-1,1,-1
            old => self(:,k)
            fac = dot_product(old,new)
            new = new - fac*old
         end do
         norm = dot_product(new,new)
         ENSURE(norm>TOL(10),"REALMAT:reverse_schmidt_orthogonalise ... linear dependence in vector " // to_str_(n))
         new = new/sqrt(norm)
      end do
     STOP_TIMER("REALMAT:reverse_schmidt_orthogonalise")
      CHECK
   end subroutine

   subroutine symmetrically_orthonormalise(self,S)
    REALMAT(:,:) :: self
   ! Symmetrically orthonormalise the column vectors in "self" using "S" as the
   ! metric.
     REALMAT(:,:), IN :: S
     REALMAT(:,:), PTR :: SS,SI
     INT :: dim
     STACK("REALMAT:symmetrically_orthonormalise")
     START_TIMER("REALMAT:symmetrically_orthonormalise")
     ENSURE(NOT is_zero_(self),"REALMAT:symmetrically_orthonormalise ... self is zero matrix")
     dim = size(self,2)
     call create_(SI,dim,dim)
     call create_(SS,dim,dim)
     call change_basis_(S,SS,self)
     call to_inverse_sqrt_(SI,SS)
     call destroy_(SS)
     call create_(SS,size(self,1),size(self,2))
     call to_product_of_(SS,self,SI)
     self = SS
     call destroy_(SS)
     call destroy_(SI)
     STOP_TIMER("REALMAT:symmetrically_orthonormalise")
      CHECK
   end subroutine

   subroutine reverse_column_order(self)
    REALMAT(:,:) :: self
   ! Reverse the order of the columns of self.
      REALVEC(:), PTR :: tmp
      INT :: n,n_col
      STACK("REALMAT:reverse_column_order")
      START_TIMER("REALMAT:reverse_column_order")
      n_col = size(self,2)
      call create_(tmp,size(self,1))
      do n=1,n_col/2
        tmp = self(:,n_col-n+1)
        self(:,n_col-n+1) = self(:,n)
        self(:,n) = tmp
      end do
      call destroy_(tmp)
     STOP_TIMER("REALMAT:reverse_column_order")
      CHECK
   end subroutine

   subroutine make_diagonally_dominant(self,permutation)
    REALMAT(:,:) :: self
   ! Rearrange the order of the columns of self so that the largest magnitude
   ! elements in each column occur along the diagonal. If "permutation" is
   ! present, it is a matrix which achieves this ordering, i.e. at the
   ! conclusion of the routine, self = self(:,permutation).
      INTVEC(:), optional :: permutation
      INTVEC(:), PTR :: perm
      BINMAT(:,:), PTR :: mask
      INT :: i,n
      INTVEC(2) :: loc
      STACK("REALMAT:make_diagonally_dominant")
      START_TIMER("REALMAT:make_diagonally_dominant")
      ENSURE(is_square_(self),"REALMAT:make_diagonally_dominant ... not square")
      if (present(permutation)) &
      ENSURE(size(permutation)==size(self,2),"REALMAT:make_diagonally_dominant ... wrong size, perm")
      n = size(self,2)
      call create_(perm,n)
      call create_(mask,n,n)
      mask = TRUE
      do i = 1,n
        loc = maxloc(abs(self),mask=mask)
        perm(loc(1))   = loc(2)
        mask(:,loc(2)) = FALSE ! eliminate this column next time
        mask(loc(1),:) = FALSE ! eliminate this row also
      ! write(*,*) " loc  =",loc
      ! write(*,*) " mask =",mask
      end do
      self = self(:,perm)
      if (present(permutation)) permutation = perm
      call destroy_(mask)
      call destroy_(perm)
     STOP_TIMER("REALMAT:make_diagonally_dominant")
      CHECK
   end subroutine

   subroutine to_sqrt(self,R)
    REALMAT(:,:) :: self
   ! self = sqrt(R), cannot have R=self
      REALMAT(:,:) :: R
      REALMAT(:,:), PTR :: evec
      REALVEC(:), PTR :: eval,veci,vecj
      INT :: d,i,j
      REAL :: temp
      STACK("REALMAT:to_sqrt")
      START_TIMER("REALMAT:to_sqrt")
      ENSURE(is_square_(self),"REALMAT:to_sqrt ... not square")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         if (temp <= ZERO) then
           WARN("REALMAT:to_sqrt ... non-positive eigenvalue, " // trim(to_str_(temp,"e15.8")))
         end if
         eval(i) = sqrt(abs(temp))
      end do
      do i=1,d
        veci => evec(i,:)
        do j=1,d
          vecj => evec(j,:)
          self(i,j) = sum(veci*eval*vecj)
        end do
      end do
      call destroy_(evec)
      call destroy_(eval)
     STOP_TIMER("REALMAT:to_sqrt")
      CHECK
   end subroutine

   subroutine to_inverse_sqrt(self,R)
    REALMAT(:,:) :: self
   ! self = sqrt(R)^(-1), cannot have R=self
      REALMAT(:,:) :: R
      REALMAT(:,:), PTR :: evec
      REALVEC(:), PTR :: eval,veci,vecj DEFAULT_NULL
      INT :: d,i,j
      STR(STR_SIZE) :: val
      REAL :: temp
      STACK("REALMAT:to_inverse_sqrt")
      START_TIMER("REALMAT:to_inverse_sqrt")
      ENSURE(is_square_(self),"REALMAT:to_inverse_sqrt ... not square")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         val = to_str_(temp,"e15.8")
         WARN_IF(temp<=ZERO,"REALMAT:to_inverse_sqrt ... non-positive eigenvalue, "// trim(val))
         eval(i) = ONE/sqrt(abs(temp))
      end do
      do i=1,d
        veci => evec(i,:)
        do j=1,d
          vecj => evec(j,:)
          self(i,j) = sum(veci*eval*vecj)
        end do
      end do
      call destroy_(evec)
      call destroy_(eval)
     STOP_TIMER("REALMAT:to_inverse_sqrt")
      CHECK
   end subroutine

   subroutine to_inverse_of(self,R)
    REALMAT(:,:) :: self
   ! self = (R)^(-1); can have R=self
       REALMAT(:,:) :: R
STACK("REALMAT:to_inverse_of")
START_TIMER("REALMAT:to_inverse_of")
#ifdef ESSL
      call to_inverse_of_ESSL_(self,R)
#else
      call to_inverse_of_LAPACK_(self,R)
#endif
     STOP_TIMER("REALMAT:to_inverse_of")
      CHECK
   end subroutine

   subroutine to_inverse_of_ESSL(self,R)
    REALMAT(:,:) :: self
   ! self = (R)^(-1); can have R=self. ESSL version.
   ! This ESSL version is untested.
      REALMAT(:,:) :: R
      REALMAT(:,:), PTR :: W
      INTVEC(:), PTR :: ipiv
      INT :: d,d2
STACK("REALMAT:to_inverse_of_ESSL")
START_TIMER("REALMAT:to_inverse_of_ESSL")
#ifdef ESSL
      REAL :: rcond
      REALVEC(2) :: det
#endif
      ENSURE(is_square_(self),"REALMAT:to_inverse_of_ESSL ... not square")
      ENSURE(is_same_shape_as_(self,R),"REALMAT:to_inverse_of_ESSL ... not same shape as R")
      d  = size(R,1)
      d2 = d*d
      self = R
      call create_(ipiv,d)
      call create_(W,d,d)
#ifdef ESSL
      call dgef(d,d,self,ipiv)
      W(:,1) = ipiv
      call dgeicd(self, d, d, 4, rcond, det, W, d2)
#endif
      call destroy_(W)
      call destroy_(ipiv)
     STOP_TIMER("REALMAT:to_inverse_of_ESSL")
      CHECK
   end subroutine

   subroutine to_inverse_of_LAPACK(self,R)
    REALMAT(:,:) :: self
   ! self = (R)^(-1); can have R=self. LAPACK version.
      REALMAT(:,:) :: R
      REALMAT(:,:), PTR :: W
      INTVEC(:), PTR :: ipiv
      INT :: d,d2,fail
      STACK("REALMAT:to_inverse_of_LAPACK")
      START_TIMER("REALMAT:to_inverse_of_LAPACK")
      ENSURE(is_square_(self),"REALMAT:to_inverse_of_LAPACK ... not square")
      ENSURE(is_same_shape_as_(self,R),"REALMAT:to_inverse_of_LAPACK ... not same shape as R")
      d  = size(R,1)
      d2 = d*d
      self = R
      call create_(ipiv,d)
      call create_(W,d,d)
      fail = 0
#ifndef ESSL
      call dgetrf(d,d,self,d,ipiv,fail)
      ENSURE(fail==0,"REALMAT:to_inverse_of_LAPACK ... failed LU factorisation")
      call dgetri(d,self,d,ipiv,W,d2,fail)
      ENSURE(fail==0,"REALMAT:to_inverse_of_LAPACK ... failed back substitution")
#endif
      call destroy_(ipiv)
      call destroy_(W)
     STOP_TIMER("REALMAT:to_inverse_of_LAPACK")
      CHECK
   end subroutine

   subroutine to_power_series_inverse_of(self,S,tol,max_it)
    REALMAT(:,:) :: self
   ! Set self to the power series inverse square root of "S".
   ! If "tol" is present, make sure that the maximum deviation from the exact
   ! answer is less than "tol" times the smallest element of "S". If "max_it"
   ! is present, use this as the maximum number of terms in the power series,
   ! before termination with an error.
      REALMAT(:,:) :: S
      REAL, optional :: tol
      INT, optional :: max_it
      REALVEC(:), PTR :: d
      INTVEC(:), PTR :: perm
      REALMAT(:,:), PTR :: W,X
      INT :: max_iter,n,k
      REAL :: eps
      STACK("REALMAT:to_power_series_inverse_of")
      START_TIMER("REALMAT:to_power_series_inverse_of")
      ENSURE(is_square_(S),"REALMAT:to_power_series_inverse_of ... S not square")
      ENSURE(is_same_shape_as_(self,S),"REALMAT:to_power_series_inverse_of ... wrong shape")
      eps = TOL(6)
      if (present(tol)) eps = tol
      max_iter = 100
      if (present(max_it)) max_iter = max_it
      n = size(S,1)
      call create_(perm,n)
      call create_(d,n)
      call create_(W,n,n)
      call create_(X,n,n)
      W = S
      call make_diagonally_dominant_(W,perm)
      call get_diagonal_(W,d)
      d = ONE/d
      call to_product_with_diagonal_(X,d,W)
      call to_unit_matrix_(self)
      X = X - self
      W = X
      self = self - W
      k = 1
      do
         k = k + 1
         W = matmul(X,W)
         if (is_odd_(k)) then; self = self - W
         else;               self = self + W
         end if
         if (maxval(abs(W)) < eps) exit
         ENSURE(k<=max_iter,"REALMAT:to_power_series_inverse_of ... power series too long")
       ! write(*,*) "k = ",k
       ! write(*,*) "W = ",W
       ! write(*,*) "s = ",self
      end do
      call to_product_with_diagonal_(X,self,d)
      self = X
      self(perm,:) = self
      call destroy_(X)
      call destroy_(W)
      call destroy_(d)
      call destroy_(perm)
     STOP_TIMER("REALMAT:to_power_series_inverse_of")
      CHECK
   end subroutine

   subroutine to_power_series_inv_sqrt_of(self,S,tol,max_it)
    REALMAT(:,:) :: self
   ! Set self to the inverse square root of "S", a matrix which is required to
   ! have a unit diagonal. The method uses a binomial power series expansion.
   ! If "tol" is present, make sure that the maximum deviation from the exact
   ! answer is less than "tol" times the smallest element of "S". If "max_it"
   ! is present, use this as the maximum number of terms in the power series,
   ! before termination with an error.
      REALMAT(:,:) :: S
      REAL, optional :: tol
      INT, optional :: max_it
      REALMAT(:,:), PTR :: W,X
      INT :: max_iter,n,k
      REAL :: eps,fac
      STACK("REALMAT:to_power_series_inv_sqrt_of")
      START_TIMER("REALMAT:to_power_series_inv_sqrt_of")
      ENSURE(has_unit_diagonal_(S),"REALMAT:to_power_series_inv_sqrt_of ... must have unit diagonal")
      ENSURE(is_square_(S),"REALMAT:to_power_series_inv_sqrt_of ... S not square")
      ENSURE(is_same_shape_as_(self,S),"REALMAT:to_power_series_inv_sqrt_of ... wrong shape")
      eps = TOL(6)
      if (present(tol)) eps = tol
      max_iter = 100
      if (present(max_it)) max_iter = max_it
      n = size(S,1)
      call to_unit_matrix_(self)
      S = S - self
      call create_(X,n,n)
      call create_(W,n,n)
      fac = -HALF
      X = fac*S
      W = X
      self = self + X
      k = 1
      do
         k   = k + 1
         fac = fac - ONE
         call to_scaled_product_of_(X,(fac/k),S,W)
         W = X
         self = self + X
         if (maxval(abs(X)) < eps) exit
         ENSURE(k<=max_iter,"REALMAT:to_power_series_inv_sqrt_of ... power series too long")
      end do
      call to_unit_matrix_(W)
      S = S + W
      call destroy_(W)
      call destroy_(X)
     STOP_TIMER("REALMAT:to_power_series_inv_sqrt_of")
      CHECK
   end subroutine

   subroutine to_exponential_of(self,X,tol)
    REALMAT(:,:) :: self
   ! Exponentiate the matrix "X" using a power series expansion, self = exp(X),
       REALMAT(:,:) :: X
      REAL, optional :: tol
      STACK("REALMAT:to_exponential_of")
      START_TIMER("REALMAT:to_exponential_of")
      call exponentiate_to_(X,self,tol)
     STOP_TIMER("REALMAT:to_exponential_of")
      CHECK
   end subroutine

   subroutine exponentiate_to(self,U,tol)
    REALMAT(:,:) :: self
   ! Exponentiate the matrix self using a power series expansion, U = exp(self),
   ! so that the maximum deviation from the exact answer is less than "tol"
   ! if present.
      REALMAT(:,:) :: U
      REAL, optional :: tol
      REALMAT(:,:), PTR :: W
      INT :: n,k
      REAL :: eps,fac
      INTVEC(2) :: ind
      STACK("REALMAT:exponentiate_to")
      START_TIMER("REALMAT:exponentiate_to")
      ENSURE(is_square_(U),"REALMAT:exponentiate_to ... U not square")
      ENSURE(is_same_shape_as_(self,U),"REALMAT:exponentiate_to ... wrong shape")
      eps = TOL(6)
      if (present(tol)) eps = tol
      n = size(U,1)
      call to_unit_matrix_(U)
      call create_(W,n,n)
      W = self
      U = U + W
      k = 1
      do
         k   = k+1
         fac = ONE/k
         W   = fac*self*W
         U   = U + W
         ind = maxloc(abs(W))
         if ( abs(W(ind(1),ind(2))) < eps ) exit
      end do
      call destroy_(W)
     STOP_TIMER("REALMAT:exponentiate_to")
      CHECK
   end subroutine

   subroutine antisymmetric_exponential(self,U, eval,evec)
    REALMAT(:,:) :: self
   ! Make unitary matrix U = exp(self) where "self" must be antisymmetric.
   ! Uses the formula:  exp A = V (cos P) V^t + V (sin P)/P V^t A
   !                        P = sqrt diag(eig(A^t A))
   ! (c) dylan jayatilaka, university of western australia, 1993
   ! Untested in TONTO.
      REALMAT(:,:) :: U
      REALMAT(:,:), PTR, optional :: evec
      REALVEC(:), PTR, optional :: eval
      REALMAT(:,:), PTR :: W
      INT :: dim1,dim2,dim,k
      REAL :: e,e2,cs,sn
      REALMAT(:,:), PTR :: v_k
      STACK("REALMAT:antisymmetric_exponential")
      START_TIMER("REALMAT:antisymmetric_exponential")
      ENSURE(is_square_(self),"REALMAT:antisymmetric_exponential ... self is a non-square matrix")
      ENSURE(is_same_shape_as_(self,U),"REALMAT:antisymmetric_exponential ... incompatible shapes")
      dim = size(self,1)
      if (NOT present(eval)) then
         call create_(eval,dim)
      else
         dim2 = size(eval)
         ENSURE(dim2>=dim,"REALMAT:antisymmetric_exponential ... eval too small")
      end if
      if (NOT present(evec)) then
         call create_(evec,dim,dim)
      else
         dim1 = size(evec,1)
         dim2 = size(evec,2)
         ENSURE(dim1==dim AND dim2==dim,"REALMAT:antisymmetric_exponential ... evec incompatible")
      end if
      call create_(W,dim,dim)

      U = matmul(self,self)   ! U = -self^t*self = self^2, makes U hermitian
      call solve_eigenproblem_(U,eval,evec) ! diagonalise U ...

      U = ZERO
      do k = 1,dim            ! do the exponential ... loop over eigenvalues ...
         e2 = eval(k)
         if (e2<0)  then
            e = sqrt(-e2); cs = cos(e);  sn = sin(e)/e;
         end if
         if (e2>0)  then
            e = sqrt(e2) ; cs = cosh(e); sn = sinh(e)/e
         end if
         if (e2==0) then
            cs = ONE    ; sn = ONE
         end if
         v_k => evec(1:dim,k:k)
         call to_product_of_(W,v_k,v_k,transpose_b=TRUE) ! V V^\dag part
         call plus_scaled_product_of_(U,sn,W,self)       ! sin part
         call plus_scaled_mat_(U,cs,W)                ! cos part
      end do
      if (NOT present(eval)) call destroy_(eval)
      if (NOT present(evec)) call destroy_(evec)
     STOP_TIMER("REALMAT:antisymmetric_exponential")
      CHECK
   end subroutine

!  ************************
!  Block returning routines
!  ************************

   function alpha_alpha(self) result(res)
    REALMAT(:,:) :: self
   ! return the alpha-alpha sector of the matrix
      TARGET :: self
      REALMAT(:,:), PTR :: res
       INT :: n
   STACK("REALMAT:alpha_alpha")
   START_TIMER("REALMAT:alpha_alpha")
   ENSURE(is_square_(self),"REALMAT:alpha_alpha ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:alpha_alpha ... uneven dimension")
      n = size(self,1)/2
      res => self(1:n,1:n)
     STOP_TIMER("REALMAT:alpha_alpha")
      CHECK
   end function

   function beta_alpha(self) result(res)
    REALMAT(:,:) :: self
   ! return the beta-alpha sector of the matrix
      TARGET :: self
      REALMAT(:,:), PTR :: res
       INT :: n
   STACK("REALMAT:beta_alpha")
   START_TIMER("REALMAT:beta_alpha")
   ENSURE(is_square_(self),"REALMAT:beta_alpha ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:beta_alpha ... uneven dimension")
      n = size(self,1)/2
      res => self(n+1:2*n,1:n)
     STOP_TIMER("REALMAT:beta_alpha")
      CHECK
   end function

   function alpha_beta(self) result(res)
    REALMAT(:,:) :: self
   ! return the alpha-beta sector of the matrix
      TARGET :: self
      REALMAT(:,:), PTR :: res
       INT :: n
   STACK("REALMAT:alpha_beta")
   START_TIMER("REALMAT:alpha_beta")
   ENSURE(is_square_(self),"REALMAT:alpha_beta ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:alpha_beta ... uneven dimension")
      n = size(self,1)/2
      res => self(1:n,n+1:2*n)
     STOP_TIMER("REALMAT:alpha_beta")
      CHECK
   end function

   function beta_beta(self) result(res)
    REALMAT(:,:) :: self
   ! return the beta-beta sector of the matrix
      TARGET :: self
      REALMAT(:,:), PTR :: res
       INT :: n
   STACK("REALMAT:beta_beta")
   START_TIMER("REALMAT:beta_beta")
   ENSURE(is_square_(self),"REALMAT:beta_beta ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:beta_beta ... uneven dimension")
      n = size(self,1)/2
      res => self(n+1:2*n,n+1:2*n)
     STOP_TIMER("REALMAT:beta_beta")
      CHECK
   end function

!  ***************
!  Set_to routines
!  ***************

   subroutine alpha_alpha_set_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Set the alpha-alpha sector of the matrix to "X"
       REALMAT(:,:), IN :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:alpha_alpha_set_to")
   START_TIMER("REALMAT:alpha_alpha_set_to")
   ENSURE(is_square_(self),"REALMAT:alpha_alpha_set_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:alpha_alpha_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = factor*X
      else;                      self(1:n,1:n) = X
      end if
     STOP_TIMER("REALMAT:alpha_alpha_set_to")
      CHECK
   end subroutine

   subroutine beta_alpha_set_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Set the beta-alpha sector of the matrix to "X"
       REALMAT(:,:), IN :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:beta_alpha_set_to")
   START_TIMER("REALMAT:beta_alpha_set_to")
   ENSURE(is_square_(self),"REALMAT:beta_alpha_set_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:beta_alpha_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = factor*X
      else;                      self(n+1:2*n,1:n) = X
      end if
     STOP_TIMER("REALMAT:beta_alpha_set_to")
      CHECK
   end subroutine

   subroutine alpha_beta_set_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Set the alpha-beta sector of the matrix to "X"
       REALMAT(:,:), IN :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:alpha_beta_set_to")
   START_TIMER("REALMAT:alpha_beta_set_to")
   ENSURE(is_square_(self),"REALMAT:alpha_beta_set_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:alpha_beta_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = factor*X
      else;                      self(1:n,n+1:2*n) = X
      end if
     STOP_TIMER("REALMAT:alpha_beta_set_to")
      CHECK
   end subroutine

   subroutine beta_beta_set_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Set the beta-beta sector of the matrix to "X"
       REALMAT(:,:), IN :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:beta_beta_set_to")
   START_TIMER("REALMAT:beta_beta_set_to")
   ENSURE(is_square_(self),"REALMAT:beta_beta_set_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:beta_beta_set_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = factor*X
      else;                      self(n+1:2*n,n+1:2*n) = X
      end if
     STOP_TIMER("REALMAT:beta_beta_set_to")
      CHECK
   end subroutine

!  ***************
!  Put_to routines
!  ***************

   subroutine alpha_alpha_put_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Put the alpha-alpha sector of the matrix to "X"
       REALMAT(:,:), OUT :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:alpha_alpha_put_to")
   START_TIMER("REALMAT:alpha_alpha_put_to")
   ENSURE(is_square_(self),"REALMAT:alpha_alpha_put_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:alpha_alpha_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,1:n)
      else;                      X = self(1:n,1:n)
      end if
     STOP_TIMER("REALMAT:alpha_alpha_put_to")
      CHECK
   end subroutine

   subroutine beta_alpha_put_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Put the beta-alpha sector of the matrix to "X"
       REALMAT(:,:), OUT :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:beta_alpha_put_to")
   START_TIMER("REALMAT:beta_alpha_put_to")
   ENSURE(is_square_(self),"REALMAT:beta_alpha_put_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:beta_alpha_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,1:n)
      else;                      X = self(n+1:2*n,1:n)
      end if
     STOP_TIMER("REALMAT:beta_alpha_put_to")
      CHECK
   end subroutine

   subroutine alpha_beta_put_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Put the alpha-beta sector of the matrix to "X"
       REALMAT(:,:), OUT :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:alpha_beta_put_to")
   START_TIMER("REALMAT:alpha_beta_put_to")
   ENSURE(is_square_(self),"REALMAT:alpha_beta_put_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:alpha_beta_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,n+1:2*n)
      else;                      X = self(1:n,n+1:2*n)
      end if
     STOP_TIMER("REALMAT:alpha_beta_put_to")
      CHECK
   end subroutine

   subroutine beta_beta_put_to(self,X,factor)
    REALMAT(:,:) :: self
   ! Put the beta-beta sector of the matrix to "X"
       REALMAT(:,:), OUT :: X
      REAL, optional, IN :: factor
       INT :: n
   STACK("REALMAT:beta_beta_put_to")
   START_TIMER("REALMAT:beta_beta_put_to")
   ENSURE(is_square_(self),"REALMAT:beta_beta_put_to ... non-square matrix")
   ENSURE(is_even_(size(self,1)),"REALMAT:beta_beta_put_to ... uneven dimension")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,n+1:2*n)
      else;                      X = self(n+1:2*n,n+1:2*n)
      end if
     STOP_TIMER("REALMAT:beta_beta_put_to")
      CHECK
   end subroutine

   subroutine convert_to(self,units)
    REALMAT(:,:) :: self
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      INOUT :: self
      STR(*), IN :: units
      REAL :: factor
   STACK("REALMAT:convert_to")
   START_TIMER("REALMAT:convert_to")
   ENSURE(is_known_unit_(units),"REALMAT:convert_to ... unknown units, " // units)
      factor = conversion_factor_(units)
      self = self * factor
     STOP_TIMER("REALMAT:convert_to")
      CHECK
   end subroutine

   subroutine convert_from(self,units)
    REALMAT(:,:) :: self
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      INOUT :: self
      STR(*), IN :: units
      REAL :: factor
   STACK("REALMAT:convert_from")
   START_TIMER("REALMAT:convert_from")
   ENSURE(is_known_unit_(units),"REALMAT:convert_from ... unknown units, " // units)
      factor = ONE/(conversion_factor_(units))
      self = self * factor
     STOP_TIMER("REALMAT:convert_from")
      CHECK
   end subroutine

   subroutine to_transpose(self)
    REALMAT(:,:) :: self
   ! Self becomes its own transpose.
     INOUT :: self
     INT :: i,j,dim
     REAL :: tmp
   STACK("REALMAT:to_transpose")
   START_TIMER("REALMAT:to_transpose")
   ENSURE(is_square_(self),"REALMAT:to_transpose ... non-square matrix")
     dim = size(self,1)
     ! do it element by element, otherwise the intrinsic routine can run out
     ! of stack space
     ! Also, only loop over the triangle, otherwise you end up where you started
     ! from.
     do i=1,dim
       do j=1,i
         tmp       = self(i,j)
         self(i,j) = self(j,i)
         self(j,i) = tmp
       end do
     end do
     STOP_TIMER("REALMAT:to_transpose")
      CHECK
   end subroutine

   subroutine to_transpose_1(self,a)
    REALMAT(:,:) :: self
   ! Self becomes the transpose of "a"
     OUT :: self
      REALMAT(:,:), IN :: a
     INT :: i,j,dim
   STACK("REALMAT:to_transpose_1")
   START_TIMER("REALMAT:to_transpose_1")
   ENSURE(is_square_(self),"REALMAT:to_transpose_1 ... non-square matrix")
   ENSURE(is_same_shape_as_(self,a),"REALMAT:to_transpose_1 ... different shapes")
     dim = size(self,1)
     ! do it element by element, otherwise the intrinsic routine can run out
     ! of stack space
     do i=1,dim
       do j=1,dim
         self(i,j) = a(j,i)
       end do
     end do
     STOP_TIMER("REALMAT:to_transpose_1")
      CHECK
   end subroutine

   subroutine make_corresponding_orbitals(self,left,right,theta,p)
    REALMAT(:,:) :: self
   ! This algorithm from Camp and King, J. Chem Phys. Vol 75(1), pp 268-274.
   ! p is the dimenstion of the first block of the partitioned matrices.
   ! Works best if "left" and "right" matrices are nonzero.
     target :: self
     REALMAT(:,:), target :: left,right
     REALVEC(:), OUT :: theta
      INT, IN :: p
     REALMAT(:,:), PTR :: Vp,Vq,Wp,Wq,M,MWq,Hq,Up,Uq
     REALVEC(:), PTR :: lambda
     INT :: minpq,q,n
   STACK("REALMAT:make_corresponding_orbitals")
   START_TIMER("REALMAT:make_corresponding_orbitals")
   ENSURE(is_square_(self),"REALMAT:make_corresponding_orbitals ... non-square matrix")
   ENSURE(is_same_shape_as_(self,right),"REALMAT:make_corresponding_orbitals ... right is incompatible")
   ENSURE(is_same_shape_as_(self,left),"REALMAT:make_corresponding_orbitals ... left is incompatible")
   ENSURE(size(theta)==min(size(self,1),size(self,1)-p),"REALMAT:make_corresponding_orbitals ... theta has wrong size")
     n = size(self,1)
     q = n - p
     minpq = min(p,q)
     ! I've only tested this for q>p.  Suspect p>q does not work.
     Vp => left(:p,:p)
     Vq => left(p+1:,p+1:)
     Wp => right(:p,:p)
     Wq => right(p+1:,p+1:)
     M  => self(:p,p+1:)
     Up => self(:p,:p)
     Uq => self(p+1:,p+1:)
     right(:p,p+1:)=ZERO
     right(p+1:,:p)=ZERO
     left(:p,p+1:)=ZERO
     left(p+1:,:p)=ZERO
     call zero_small_values_(self,TOL(10))

     call create_(lambda,q)                       ! get eigenvalues and Wq.
     call create_(Hq,q,q)
     call to_product_of_(Hq,M,M,transpose_a=TRUE)
     call solve_eigenproblem_(Hq,lambda,Wq)
     call destroy_(Hq)

     call reverse_order_(lambda)                   ! get rotation angles, largest first.
     theta = lambda(:minpq)
     call destroy_(lambda)
     call zero_small_values_(theta,TOL(10))

   ENSURE(minval(theta)>=ZERO,"REALMAT:make_corresponding_orbitals ... eigenvalues less than zero!")
   ENSURE(maxval(theta)<=ONE,"REALMAT:make_corresponding_orbitals ... eigenvalues more than one!")
     theta = min(theta,ONE)
     theta = max(theta,ZERO)

     call zero_small_values_(Wq,TOL(10))          ! get Vp
     call reverse_column_order_(Wq)
     call create_(MWq,p,q)
     call to_product_of_(MWq,M,Wq)
     Vp = MWq(:p,:p)
     call destroy_(MWq)
     call schmidt_orthonormalise_(Vp,theta)

     call to_product_of_(Vq,Uq,Wq)                   ! get Vq
     call reverse_schmidt_orthogonalise_(Vq)

     call to_product_of_(Wp,Up,Vp,transpose_a=TRUE)  ! get Wp
     call reverse_schmidt_orthogonalise_(Wp)

     theta = sqrt(theta)
     theta = asin(theta)
     STOP_TIMER("REALMAT:make_corresponding_orbitals")
      CHECK
   end subroutine

   subroutine schmidt_orthonormalise_2(self,lambda)
    REALMAT(:,:) :: self
   ! Schmidt orthonormalise the column vectors in "self".
   ! If the eigenvalue (lambda) of a vector is less than a cutoff, then that
   ! vector is chosen to be an orthonormal component.
   ! Eigenvalues must be sorted largest to smallest.
     target :: self
     REALVEC(:), IN :: lambda
     REALVEC(:), PTR :: new,old
     REAL :: fac,norm
     INT :: dim1,dim2,n,k,x,y,j
   STACK("REALMAT:schmidt_orthonormalise_2")
   START_TIMER("REALMAT:schmidt_orthonormalise_2")
   ENSURE(size(lambda)>=size(self,2),"REALMAT:schmidt_orthonormalise_2 ... not enough eigenvalues")
   ENSURE(size(self,1)>=size(self,2),"REALMAT:schmidt_orthonormalise_2 ... more vectors than dimension of vector space")
     dim1 = size(self,1)
     dim2 = size(self,2)

     y=dim2+1   ! y is set to the first vanishing eigenvalue.
     do x=1,dim2
       if (lambda(x)<TOL(10)) then
         y=x
         exit
       end if
     end do

     do n = 1,y-1  ! the usual Schmidt orthogonalisation.
        new => self(:,n)
        do k = 1,n-1
           old => self(:,k)
           fac = dot_product(old,new)
           new = new - fac*old
        end do
        norm = dot_product(new,new)
        ENSURE(norm>TOL(10),"REALMAT:schmidt_orthonormalise_2 ... linear dependence in vector " // to_str_(n))
        new = new * (ONE/sqrt(norm))
     end do

     do n = y,dim2                       ! make up some orthogonal vectors for
       do j=1,dim1                       ! the vanishing eigenvalues.
         new => self(:,n)
         new = ZERO
         new(j) = ONE
         do k = 1,n-1
            old => self(:,k)
            fac = dot_product(old,new)
            new = new - fac*old
         end do
         norm = dot_product(new,new)
         if (norm>TOL(10)) then  ! we have found an orthogonal vector
           new = new * (ONE/sqrt(norm))
           exit
         else                   ! try another
           DIE_IF(j==dim1,"REALMAT:schmidt_orthonormalise_2 ... cannot find an orthogonal vector")
         end if
       end do
     end do
     STOP_TIMER("REALMAT:schmidt_orthonormalise_2")
      CHECK
   end subroutine

!  **************************************************
!  Gaussian function rotation representation matrices
!  *************************************************

   function gaussian_d_xyz_matrix(self) result(dtr)
    REALMAT(:,:) :: self
   ! Return the representation matrix for a d xyz product found in gaussian shells
   ! from a p-type xyz matrix.
      REALMAT(6,6) :: dtr
      INT :: j,i1,i2
      REAL :: sqrt3
      INTVEC(6) :: d1  = (/1,2,3,1,1,2/)
      INTVEC(6) :: d2  = (/1,2,3,2,3,3/)
   STACK("REALMAT:gaussian_d_xyz_matrix")
   START_TIMER("REALMAT:gaussian_d_xyz_matrix")
   ENSURE(is_square_(self),"REALMAT:gaussian_d_xyz_matrix ... self not square")
   ENSURE(size(self,1)==3,"REALMAT:gaussian_d_xyz_matrix ... wrong size, self")
      sqrt3  = sqrt(THREE)
      do j = 1,6
         i1=d1(j)
         i2=d2(j)
         dtr(1,j)  = self(1,i1)*self(1,i2)
         dtr(2,j)  = self(2,i1)*self(2,i2)
         dtr(3,j)  = self(3,i1)*self(3,i2)
         dtr(4,j)  = self(1,i1)*self(2,i2) &
                   + self(2,i1)*self(1,i2)
         dtr(5,j)  = self(1,i1)*self(3,i2) &
                   + self(3,i1)*self(1,i2)
         dtr(6,j)  = self(2,i1)*self(3,i2) &
                   + self(3,i1)*self(2,i2)
      end do
      dtr(1:6,4:6) = dtr(1:6,4:6)*sqrt3 ! Put in correct normalization for old primitives
      dtr(4:6,1:6) = dtr(4:6,1:6)/sqrt3 ! Put in wrong   normalization for new primitives
     STOP_TIMER("REALMAT:gaussian_d_xyz_matrix")
      CHECK
   end function

   function gaussian_f_xyz_matrix(self) result(ftr)
    REALMAT(:,:) :: self
   ! Return the representation matrix for an f xyz product found in gaussian
   ! shells from a p-type xyz matrix
      REALMAT(10,10) :: ftr
      INT :: j,i1,i2,i3
      REAL :: sqrt5,sqrt15
      INTVEC(10) :: f1 = (/1,2,3,1,1,2,2,3,3,1/)
      INTVEC(10) :: f2 = (/1,2,3,1,1,2,2,3,3,2/)
      INTVEC(10) :: f3 = (/1,2,3,2,3,1,3,1,2,3/)
   STACK("REALMAT:gaussian_f_xyz_matrix")
   START_TIMER("REALMAT:gaussian_f_xyz_matrix")
   ENSURE(is_square_(self),"REALMAT:gaussian_f_xyz_matrix ... self not square")
   ENSURE(size(self,1)==3,"REALMAT:gaussian_f_xyz_matrix ... wrong size, self")
      sqrt5  = sqrt(FIVE)
      sqrt15 = sqrt(15d0)
      do j = 1,10
         i1=f1(j)
         i2=f2(j)
         i3=f3(j)
         ftr(1,j)  = self(1,i1)*self(1,i2)*self(1,i3)
         ftr(2,j)  = self(2,i1)*self(2,i2)*self(2,i3)
         ftr(3,j)  = self(3,i1)*self(3,i2)*self(3,i3)
         ftr(4,j)  = self(1,i1)*self(1,i2)*self(2,i3) &
                   + self(1,i1)*self(2,i2)*self(1,i3) &
                   + self(2,i1)*self(1,i2)*self(1,i3)
         ftr(5,j)  = self(1,i1)*self(1,i2)*self(3,i3) &
                   + self(1,i1)*self(3,i2)*self(1,i3) &
                   + self(3,i1)*self(1,i2)*self(1,i3)
         ftr(6,j)  = self(1,i1)*self(2,i2)*self(2,i3) &
                   + self(2,i1)*self(1,i2)*self(2,i3) &
                   + self(2,i1)*self(2,i2)*self(1,i3)
         ftr(7,j)  = self(3,i1)*self(2,i2)*self(2,i3) &
                   + self(2,i1)*self(3,i2)*self(2,i3) &
                   + self(2,i1)*self(2,i2)*self(3,i3)
         ftr(8,j)  = self(1,i1)*self(3,i2)*self(3,i3) &
                   + self(3,i1)*self(1,i2)*self(3,i3) &
                   + self(3,i1)*self(3,i2)*self(1,i3)
         ftr(9,j)  = self(2,i1)*self(3,i2)*self(3,i3) &
                   + self(3,i1)*self(2,i2)*self(3,i3) &
                   + self(3,i1)*self(3,i2)*self(2,i3)
         ftr(10,j) = self(1,i1)*self(2,i2)*self(3,i3) &
                   + self(1,i1)*self(3,i2)*self(2,i3) &
                   + self(2,i1)*self(1,i2)*self(3,i3) &
                   + self(2,i1)*self(3,i2)*self(1,i3) &
                   + self(3,i1)*self(1,i2)*self(2,i3) &
                   + self(3,i1)*self(2,i2)*self(1,i3)
      end do
      ftr(1:10, 4:9 ) = ftr(1:10, 4:9 )*sqrt5
      ftr(1:10,10:10) = ftr(1:10,10:10)*sqrt15
      ftr(4:9 , 1:10) = ftr(4:9 , 1:10)/sqrt5
      ftr(10:10,1:10) = ftr(10:10,1:10)/sqrt15
     STOP_TIMER("REALMAT:gaussian_f_xyz_matrix")
      CHECK
   end function

   function gaussian_g_xyz_matrix(self) result(gtr)
    REALMAT(:,:) :: self
   ! Return the representation matrix for a g xyz product found in gaussian
   ! shells from a p-type xyz matrix
      REALMAT(15,15) :: gtr
      INT :: j,i1,i2,i3,i4
      REAL :: sqrt7,sqrt35,sqrt353
      INTVEC(15) :: g1 = (/1,2,3,1,1,2,2,3,3,1,1,2,1,2,3/)
      INTVEC(15) :: g2 = (/1,2,3,1,1,2,2,3,3,1,1,2,1,2,3/)
      INTVEC(15) :: g3 = (/1,2,3,1,1,2,2,3,3,2,3,3,2,1,1/)
      INTVEC(15) :: g4 = (/1,2,3,2,3,1,3,1,2,2,3,3,3,3,2/)
   STACK("REALMAT:gaussian_g_xyz_matrix")
   START_TIMER("REALMAT:gaussian_g_xyz_matrix")
   ENSURE(is_square_(self),"REALMAT:gaussian_g_xyz_matrix ... self not square")
   ENSURE(size(self,1)==3,"REALMAT:gaussian_g_xyz_matrix ... wrong size, self")
      sqrt7  = sqrt(SEVEN)
      sqrt35 = sqrt(35d0)         ! = sqrt(35)
      sqrt353= sqrt35/sqrt(THREE) ! = sqrt(35)/sqrt(3)
      do j = 1,15
         i1=g1(j)
         i2=g2(j)
         i3=g3(j)
         i4=g4(j)
         gtr(1,j)  = self(1,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(2,j)  = self(2,i1)*self(2,i2)*self(2,i3)*self(2,i4)
         gtr(3,j)  = self(3,i1)*self(3,i2)*self(3,i3)*self(3,i4)
         gtr(4,j)  = self(1,i1)*self(1,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(1,i2)*self(2,i3)*self(1,i4) &
                   + self(1,i1)*self(2,i2)*self(1,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(5,j)  = self(1,i1)*self(1,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(1,i2)*self(3,i3)*self(1,i4) &
                   + self(1,i1)*self(3,i2)*self(1,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(1,i3)*self(1,i4)
         gtr(6,j)  = self(1,i1)*self(2,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(1,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(1,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(2,i3)*self(1,i4)
         gtr(7,j)  = self(3,i1)*self(2,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(3,i2)*self(2,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(3,i3)*self(2,i4) &
                   + self(2,i1)*self(2,i2)*self(2,i3)*self(3,i4)
         gtr(8,j)  = self(1,i1)*self(3,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(1,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(1,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(3,i3)*self(1,i4)
         gtr(9,j)  = self(2,i1)*self(3,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(2,i2)*self(3,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(2,i3)*self(3,i4) &
                   + self(3,i1)*self(3,i2)*self(3,i3)*self(2,i4)
         gtr(10,j) = self(1,i1)*self(1,i2)*self(2,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(1,i3)*self(2,i4) &
                   + self(2,i1)*self(1,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(2,i2)*self(1,i3)*self(1,i4)
         gtr(11,j) = self(1,i1)*self(1,i2)*self(3,i3)*self(3,i4) &
                   + self(1,i1)*self(3,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(3,i2)*self(3,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(1,i3)*self(3,i4) &
                   + self(3,i1)*self(1,i2)*self(3,i3)*self(1,i4) &
                   + self(3,i1)*self(3,i2)*self(1,i3)*self(1,i4)
         gtr(12,j) = self(2,i1)*self(2,i2)*self(3,i3)*self(3,i4) &
                   + self(2,i1)*self(3,i2)*self(2,i3)*self(3,i4) &
                   + self(2,i1)*self(3,i2)*self(3,i3)*self(2,i4) &
                   + self(3,i1)*self(2,i2)*self(2,i3)*self(3,i4) &
                   + self(3,i1)*self(2,i2)*self(3,i3)*self(2,i4) &
                   + self(3,i1)*self(3,i2)*self(2,i3)*self(2,i4)
         gtr(13,j) = self(1,i1)*self(1,i2)*self(2,i3)*self(3,i4) &
                   + self(1,i1)*self(1,i2)*self(3,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(2,i2)*self(3,i3)*self(1,i4) &
                   + self(1,i1)*self(3,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(3,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(1,i3)*self(3,i4) &
                   + self(2,i1)*self(1,i2)*self(3,i3)*self(1,i4) &
                   + self(2,i1)*self(3,i2)*self(1,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(1,i3)*self(2,i4) &
                   + self(3,i1)*self(1,i2)*self(2,i3)*self(1,i4) &
                   + self(3,i1)*self(2,i2)*self(1,i3)*self(1,i4)
         gtr(14,j) = self(2,i1)*self(2,i2)*self(1,i3)*self(3,i4) &
                   + self(2,i1)*self(2,i2)*self(3,i3)*self(1,i4) &
                   + self(2,i1)*self(1,i2)*self(2,i3)*self(3,i4) &
                   + self(2,i1)*self(1,i2)*self(3,i3)*self(2,i4) &
                   + self(2,i1)*self(3,i2)*self(2,i3)*self(1,i4) &
                   + self(2,i1)*self(3,i2)*self(1,i3)*self(2,i4) &
                   + self(1,i1)*self(2,i2)*self(2,i3)*self(3,i4) &
                   + self(1,i1)*self(2,i2)*self(3,i3)*self(2,i4) &
                   + self(1,i1)*self(3,i2)*self(2,i3)*self(2,i4) &
                   + self(3,i1)*self(2,i2)*self(2,i3)*self(1,i4) &
                   + self(3,i1)*self(2,i2)*self(1,i3)*self(2,i4) &
                   + self(3,i1)*self(1,i2)*self(2,i3)*self(2,i4)
         gtr(15,j) = self(3,i1)*self(3,i2)*self(1,i3)*self(2,i4) &
                   + self(3,i1)*self(3,i2)*self(2,i3)*self(1,i4) &
                   + self(3,i1)*self(1,i2)*self(3,i3)*self(2,i4) &
                   + self(3,i1)*self(1,i2)*self(2,i3)*self(3,i4) &
                   + self(3,i1)*self(2,i2)*self(3,i3)*self(1,i4) &
                   + self(3,i1)*self(2,i2)*self(1,i3)*self(3,i4) &
                   + self(1,i1)*self(3,i2)*self(3,i3)*self(2,i4) &
                   + self(1,i1)*self(3,i2)*self(2,i3)*self(3,i4) &
                   + self(1,i1)*self(2,i2)*self(3,i3)*self(3,i4) &
                   + self(2,i1)*self(3,i2)*self(3,i3)*self(1,i4) &
                   + self(2,i1)*self(3,i2)*self(1,i3)*self(3,i4) &
                   + self(2,i1)*self(1,i2)*self(3,i3)*self(3,i4)
      end do
      gtr(1:15, 4:9 ) = gtr(1:15, 4:9 )*sqrt7
      gtr(1:15,10:12) = gtr(1:15,10:12)*sqrt353
      gtr(1:15,13:15) = gtr(1:15,13:15)*sqrt35
      gtr(4:9 , 1:15) = gtr(4:9 , 1:15)/sqrt7
      gtr(10:12,1:15) = gtr(10:12,1:15)/sqrt353
      gtr(13:15,1:15) = gtr(13:15,1:15)/sqrt35
     STOP_TIMER("REALMAT:gaussian_g_xyz_matrix")
      CHECK
   end function

   PURE subroutine make_enclosing_sphere(self,pos,radius)
    REALMAT(:,:) :: self
   ! Determine the position and radius of a sphere that encloses all points in
   ! the grid.
     IN :: self
     REAL, OUT :: radius
     REALVEC(3), OUT :: pos
     REALVEC(3) :: diff
     REAL :: dist
     INT :: n,n_pts
     
     n_pts = size(self,2)
 
     ! Get the centre of the sphere.  Should use a better algorithm than just the
     ! average.
     pos = ZERO
     do n = 1,n_pts
       pos = pos + self(:,n)
     end do
     pos = pos / n_pts
 
     ! The radius is the distance to the furthest point.
     radius = 0
     do n = 1,n_pts
       diff = self(:,n) - pos
       dist = dot_product(diff,diff)
       if (dist > radius) radius = dist
     end do
     radius = sqrt(radius)
 
     STOP_TIMER("REALMAT:make_enclosing_sphere")
   end subroutine

end
