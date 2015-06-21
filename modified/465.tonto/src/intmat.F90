!---------------------------------------------------------------------------
!
!  INTMAT: Integer matrix operations ...
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
! $Id: intmat.foo,v 1.8.2.2 2003/11/13 05:35:09 reaper Exp $
!---------------------------------------------------------------------------

module INTMAT_MODULE

#  include "intmat.use"

   implicit none

#  include "macros"
#  include "intmat.int"


contains

   subroutine create(self,dim1,dim2)
    INTMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:create")
      START_TIMER("INTMAT:create")
      nullify(self)
      allocate(self(dim1,dim2))
      ADD_MEMORY(dim1*dim2*INT_SIZE)
     STOP_TIMER("INTMAT:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    INTMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: lb1,ub1,lb2,ub2
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:create_1")
      START_TIMER("INTMAT:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*INT_SIZE)
     STOP_TIMER("INTMAT:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2)
    INTMAT(:,:) :: self
   ! Create a matrix with the specified bounds for each dimension
      PTR :: self
      INTVEC(:), IN :: bounds1,bounds2
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:create_2")
      START_TIMER("INTMAT:create_2")
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))
     STOP_TIMER("INTMAT:create_2")
      UNSTACK
   end subroutine

   subroutine create_copy(self,matrix)
    INTMAT(:,:) :: self
   ! Create a replica copy of matrix
      PTR :: self
      INTMAT(:,:), IN :: matrix
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:create_copy")
      START_TIMER("INTMAT:create_copy")
      call create_(self,lbound(matrix,1),ubound(matrix,1), &
              lbound(matrix,2),ubound(matrix,2)  )
      self = matrix
     STOP_TIMER("INTMAT:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTMAT(:,:) :: self
   ! Destroy the object
      PTR :: self
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:destroy")
      START_TIMER("INTMAT:destroy")
      if (NOT associated(self)) then; STOP_TIMER("INTMAT:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*INT_SIZE)
      deallocate(self)
     STOP_TIMER("INTMAT:destroy")
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

   function is_square(self) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix is square
      IN :: self
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:is_square")
      START_TIMER("INTMAT:is_square")
      res = size(self,1)==size(self,2)
     STOP_TIMER("INTMAT:is_square")
      CHECK
   end function

   function is_diagonal(self) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is a diagonal matrix
      IN :: self
      BIN :: res
      INT :: dim,i,j
      BIN :: off_diagonal_is_zero
      STACK("INTMAT:is_diagonal")
      START_TIMER("INTMAT:is_diagonal")
      ENSURE(is_square_(self),"INTMAT:is_diagonal ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         off_diagonal_is_zero = self(i,j)==0
         if (off_diagonal_is_zero) cycle
         res = FALSE
         STOP_TIMER("INTMAT:is_diagonal") CHECK return
      end do
      end do
     STOP_TIMER("INTMAT:is_diagonal")
      CHECK
   end function

   function has_unit_diagonal(self) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" has 1's as diagonal elements
      IN :: self
      BIN :: res
      INT :: i
      BIN :: diagonal_is_one
      STACK("INTMAT:has_unit_diagonal")
      START_TIMER("INTMAT:has_unit_diagonal")
      ENSURE(is_square_(self),"INTMAT:has_unit_diagonal ... Non-square matrix")
      res = TRUE
      do i = 1,size(self,1)
         diagonal_is_one = (1 - self(i,i))==0
         if (diagonal_is_one) cycle
         res = FALSE
         exit
      end do
     STOP_TIMER("INTMAT:has_unit_diagonal")
      CHECK
   end function

   function is_unit_matrix(self) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is the unit matrix
      IN :: self
      BIN :: res
      STACK("INTMAT:is_unit_matrix")
      START_TIMER("INTMAT:is_unit_matrix")
      ENSURE(is_square_(self),"INTMAT:is_unit_matrix ... Non-square matrix")
      res = is_diagonal_(self) AND has_unit_diagonal_(self)
     STOP_TIMER("INTMAT:is_unit_matrix")
      CHECK
   end function

   function is_inversion_matrix(self) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is an inversion matrix
   ! i.e. minus the unit matrix
      IN :: self
      BIN :: res
      INT :: dim,i
      BIN :: diagonal_is_minus_one
      STACK("INTMAT:is_inversion_matrix")
      START_TIMER("INTMAT:is_inversion_matrix")
      ENSURE(is_square_(self),"INTMAT:is_inversion_matrix ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
         diagonal_is_minus_one = (self(i,i)+1)==0
         if (diagonal_is_minus_one) cycle
         res = FALSE
         exit
      end do
      if (res) res = is_diagonal_(self)
     STOP_TIMER("INTMAT:is_inversion_matrix")
      CHECK
   end function

   function is_symmetric(self) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" is a symmetric matrix
      IN :: self
      BIN :: res
      INT :: dim,i,j
      INT :: diff
      STACK("INTMAT:is_symmetric")
      START_TIMER("INTMAT:is_symmetric")
      ENSURE(is_square_(self),"INTMAT:is_symmetric ... Non-square matrix")
      dim = size(self,1)
      res = TRUE
      do i = 1,dim
      do j = 1,i-1
         diff = abs(self(i,j)-self(j,i))
         if (diff==0) cycle
         res = FALSE
         STOP_TIMER("INTMAT:is_symmetric") CHECK return
      end do
      end do
     STOP_TIMER("INTMAT:is_symmetric")
      CHECK
   end function

   PURE function is_same_shape_as(self,b) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" has the same shape as self
      IN :: self
      INTMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2)
     STOP_TIMER("INTMAT:is_same_shape_as")
   end function

   PURE function is_transposed_shape_of(self,b) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" is the transposed shape of self
      IN :: self
      INTMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) AND size(self,2)==size(b,1)
     STOP_TIMER("INTMAT:is_transposed_shape_of")
   end function

   function equals(self,b) result(res)
    INTMAT(:,:) :: self
   ! Check if the matrix is the same as "b".
      IN :: self
      INTMAT(:,:), IN :: b
      BIN :: res
      STACK("INTMAT:equals")
      START_TIMER("INTMAT:equals")
      res = same_as_(self,b)
     STOP_TIMER("INTMAT:equals")
      CHECK
   end function

   function same_as(self,b) result(res)
    INTMAT(:,:) :: self
   ! Check if the matrix is the same as "b". 
      IN :: self
      INTMAT(:,:), IN :: b
      BIN :: res
      INT :: i,del
      STACK("INTMAT:same_as")
      START_TIMER("INTMAT:same_as")
      ENSURE(is_same_shape_as_(self,b),"INTMAT:same_as ... incompatible dimensions")
      res =  TRUE
      do i = 1,size(self,2)
         del = sum( abs(self(:,i)-b(:,i)) )
         if (del==0) cycle
         res = FALSE
         exit
      end do
     STOP_TIMER("INTMAT:same_as")
      CHECK
   end function

   function has_column(self,c,col) result(res)
    INTMAT(:,:) :: self
   ! Returns TRUE if the matrix "self" has a column "c". If present, the
   ! first matching column index "col" is also returned.
      IN :: self
      INTVEC(:), IN :: c
      INT, optional :: col
      BIN :: res
      INT :: n
      STACK("INTMAT:has_column")
      START_TIMER("INTMAT:has_column")
      ENSURE(size(c)==size(self,1),"INTMAT:has_column ... incompatible column size")
      res = FALSE
      do n = 1,size(self,2)
         res = same_as_(self(:,n),c)
         if (NOT res) cycle
         if (present(col)) col = n
         exit
      end do
     STOP_TIMER("INTMAT:has_column")
      CHECK
   end function

   function all_in_range(self,range) result(res)
    INTMAT(:,:) :: self
   ! Return TRUE if all values of self are within the specified "range".
      INTVEC(2) :: range
      BIN :: res
      STACK("INTMAT:all_in_range")
      START_TIMER("INTMAT:all_in_range")
      res = all(range(1) <= self AND self <= range(2))
     STOP_TIMER("INTMAT:all_in_range")
      CHECK
   end function

   function in_range(self,range) result(res)
    INTMAT(:,:) :: self
   ! Return element ij as TRUE if self(i,j) is within the specified "range".
      INTVEC(2) :: range
      BINMAT(size(self,1),size(self,2)) :: res
      STACK("INTMAT:in_range")
      START_TIMER("INTMAT:in_range")
      res = (range(1) <= self AND self <= range(2))
     STOP_TIMER("INTMAT:in_range")
      CHECK
   end function

   function range(self) result(res)
    INTMAT(:,:) :: self
   ! Return the range (smallest and largest value) of self.
   ! NOTE: Returns a real.
      REALVEC(2) :: res
      STACK("INTMAT:range")
      START_TIMER("INTMAT:range")
      res(1) = minval(self)
      res(2) = maxval(self)
     STOP_TIMER("INTMAT:range")
      CHECK
   end function

   subroutine shrink(self,dim1,dim2)
    INTMAT(:,:) :: self
   ! Shrinks self to dimension dim1xdim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
     INTMAT(:,:), PTR :: old
     STACK("INTMAT:shrink")
     START_TIMER("INTMAT:shrink")
     ENSURE(associated(self),"INTMAT:shrink ... matrix not allocated")
     ENSURE(dim1<=size(self,1),"INTMAT:shrink ... 1st dimension given is too large.")
     ENSURE(dim2<=size(self,2),"INTMAT:shrink ... 2nd dimension given is too large.")
     if (dim1==size(self,1) AND dim2==size(self,2)) then; STOP_TIMER("INTMAT:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)
     STOP_TIMER("INTMAT:shrink")
      UNSTACK
   end subroutine

   subroutine shrink_columns(self,dim2)
    INTMAT(:,:) :: self
   ! Shrinks columns of self to dimension dim2. Contents are retained.
     PTR :: self
     INT, IN :: dim2
   ! The following code is inherited from INTRINSICMAT
     INTMAT(:,:), PTR :: old
     INT :: dim1
     STACK("INTMAT:shrink_columns")
     START_TIMER("INTMAT:shrink_columns")
     ENSURE(associated(self),"INTMAT:shrink_columns ... matrix not allocated")
     ENSURE(dim2<=size(self,2),"INTMAT:shrink_columns ... 2nd dimension given is too large.")
     if (dim2==size(self,2)) then; STOP_TIMER("INTMAT:shrink_columns") UNSTACK return; end if
     dim1 = size(self,1)
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)
     STOP_TIMER("INTMAT:shrink_columns")
      UNSTACK
   end subroutine

   subroutine expand(self,dim1,dim2)
    INTMAT(:,:) :: self
   ! Expands self to dimension dim1xdim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
     INTMAT(:,:), PTR :: old
     INT :: old_size1,old_size2
     STACK("INTMAT:expand")
     START_TIMER("INTMAT:expand")
     if (NOT associated(self)) then
       call create_(self,0,0)
     else
     ENSURE(dim1>=size(self,1),"INTMAT:expand ... 1st dimension given is too small")
     ENSURE(dim2>=size(self,2),"INTMAT:expand ... 2nd dimension given is too small")
     end if
     old => self
     old_size1 = size(old,1)
     old_size2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(1:old_size1,1:old_size2)=old
     call destroy_(old)
     STOP_TIMER("INTMAT:expand")
      UNSTACK
   end subroutine

   subroutine expand_columns(self,dim2)
    INTMAT(:,:) :: self
   ! Expands the columns self to dim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim2
   ! The following code is inherited from INTRINSICMAT
     INT :: dim1,old_dim2
     INTMAT(:,:), PTR :: old
     STACK("INTMAT:expand_columns")
     START_TIMER("INTMAT:expand_columns")
     ENSURE( associated(self),"INTMAT:expand_columns ... matrix not allocated")
     ENSURE(dim2>=size(self,2),"INTMAT:expand_columns ... 2nd dimension given is too small")
     dim1 = size(self,1)
     old => self
     old_dim2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(:,1:old_dim2) = old
     call destroy_(old)
     STOP_TIMER("INTMAT:expand_columns")
      UNSTACK
   end subroutine

   subroutine append_column(self,col)
    INTMAT(:,:) :: self
   ! Append the column "col" onto the end of self.
     PTR :: self
     INTVEC(:) :: col
     INT :: old_dim2,new_dim2
     STACK("INTMAT:append_column")
     START_TIMER("INTMAT:append_column")
     ENSURE(associated(self),"INTMAT:append_column ... self not allocated")
     ENSURE(size(self,1)==size(col),"INTMAT:append_column ... 2nd dimension given is too small")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + 1
     call expand_columns_(self,new_dim2)
     self(:,new_dim2) = col
     STOP_TIMER("INTMAT:append_column")
      UNSTACK
   end subroutine

   subroutine append_columns(self,cols)
    INTMAT(:,:) :: self
   ! Append the columns "cols" onto the end of self.
     PTR :: self
     INTMAT(:,:) :: cols
   ! The following code is inherited from INTRINSICMAT
     INT :: old_dim2,new_dim2
     STACK("INTMAT:append_columns")
     START_TIMER("INTMAT:append_columns")
     ENSURE(associated(self),"INTMAT:append_columns ... self not allocated")
     ENSURE(size(self,1)==size(cols,1),"INTMAT:append_columns ... 1st dimension wrong, cols")
     old_dim2 = size(self,2)
     new_dim2 = size(self,2) + size(cols,2)
     call expand_columns_(self,new_dim2)
     self(:,old_dim2+1:new_dim2) = cols
     STOP_TIMER("INTMAT:append_columns")
      UNSTACK
   end subroutine

   subroutine to_unit_matrix(self)
    INTMAT(:,:) :: self
   ! Set self to the unit matrix
      INT :: dim,i
      STACK("INTMAT:to_unit_matrix")
      START_TIMER("INTMAT:to_unit_matrix")
      ENSURE(is_square_(self),"INTMAT:to_unit_matrix ... non-square matrix")
      dim = size(self,1)
      self = 0
      do i = 1,dim
         self(i,i) = 1
      end do
     STOP_TIMER("INTMAT:to_unit_matrix")
      CHECK
   end subroutine

   subroutine to_inverse_matrix(self)
    INTMAT(:,:) :: self
   ! Set self to the negative unit matrix
      INT :: dim,i
      STACK("INTMAT:to_inverse_matrix")
      START_TIMER("INTMAT:to_inverse_matrix")
      ENSURE(is_square_(self),"INTMAT:to_inverse_matrix ... non-square matrix")
      dim = size(self,1)
      self = 0
      do i = 1,dim
         self(i,i) = -1
      end do
     STOP_TIMER("INTMAT:to_inverse_matrix")
      CHECK
   end subroutine

   subroutine set_to(self,b)
    INTMAT(:,:) :: self
   ! Set self to "b"
      INTMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:set_to")
      START_TIMER("INTMAT:set_to")
      ENSURE(is_same_shape_as_(self,b),"INTMAT:set_to ... incompatible shape")
      self = b
     STOP_TIMER("INTMAT:set_to")
      CHECK
   end subroutine

   subroutine plus(self,b)
    INTMAT(:,:) :: self
   ! Add to self the matrix "b"
      INTMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:plus")
      START_TIMER("INTMAT:plus")
      ENSURE(is_same_shape_as_(self,b),"INTMAT:plus ... incompatible shape")
      self = self+b
     STOP_TIMER("INTMAT:plus")
      CHECK
   end subroutine

   subroutine minus(self,b)
    INTMAT(:,:) :: self
   ! Subtract from self the matrix "b"
      INTMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("INTMAT:minus")
      START_TIMER("INTMAT:minus")
      ENSURE(is_same_shape_as_(self,b),"INTMAT:minus ... incompatible shape")
      self = self-b
     STOP_TIMER("INTMAT:minus")
      CHECK
   end subroutine

   subroutine to_scaled_imat(self,fac,b)
    INTMAT(:,:) :: self
   ! Set self to the scaled matrix "b"
      INTMAT(:,:), IN :: b
      INT, IN :: fac
      STACK("INTMAT:to_scaled_imat")
      START_TIMER("INTMAT:to_scaled_imat")
      ENSURE(is_same_shape_as_(self,b),"INTMAT:to_scaled_imat ... incompatible shapes")
      self = fac*b
     STOP_TIMER("INTMAT:to_scaled_imat")
      CHECK
   end subroutine

   subroutine plus_scaled_imat(self,fac,b)
    INTMAT(:,:) :: self
   ! Add to self the scaled matrix "b"
      INTMAT(:,:), IN :: b
      INT, IN :: fac
      STACK("INTMAT:plus_scaled_imat")
      START_TIMER("INTMAT:plus_scaled_imat")
      ENSURE(is_same_shape_as_(self,b),"INTMAT:plus_scaled_imat ... incompatible shapes")
      self = self+fac*b
     STOP_TIMER("INTMAT:plus_scaled_imat")
      CHECK
   end subroutine

   subroutine to_product_of(self,a,b,transpose_a,transpose_b)
    INTMAT(:,:) :: self
   ! Set self to the product of "a" and "b"
      INTMAT(:,:), IN :: a, b
      BIN, optional, IN :: transpose_a, transpose_b
      STACK("INTMAT:to_product_of")
      START_TIMER("INTMAT:to_product_of")
      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = matmul(transpose(a),transpose(b))
        else
          self = matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = matmul(a,transpose(b))
        else
          self = matmul(a,b)
        end if
      end if
     STOP_TIMER("INTMAT:to_product_of")
      CHECK
   end subroutine

   subroutine to_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    INTMAT(:,:) :: self
   ! Set self to the scaled product of "a" and "b"
      INTMAT(:,:), IN :: a, b
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a, transpose_b
      STACK("INTMAT:to_scaled_product_of")
      START_TIMER("INTMAT:to_scaled_product_of")
      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = fac*matmul(transpose(a),transpose(b))
        else
          self = fac*matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = fac*matmul(a,transpose(b))
        else
          self = fac*matmul(a,b)
        end if
      end if
     STOP_TIMER("INTMAT:to_scaled_product_of")
      CHECK
   end subroutine

   subroutine plus_product_of(self,a,b,transpose_a,transpose_b)
    INTMAT(:,:) :: self
   ! Add to self the product of "a" and "b"
      INTMAT(:,:), IN :: a, b
      BIN, optional, IN :: transpose_a, transpose_b
      STACK("INTMAT:plus_product_of")
      START_TIMER("INTMAT:plus_product_of")
      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = self+matmul(transpose(a),transpose(b))
        else
          self = self+matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = self+matmul(a,transpose(b))
        else
          self = self+matmul(a,b)
        end if
      end if
     STOP_TIMER("INTMAT:plus_product_of")
      CHECK
   end subroutine

   subroutine plus_scaled_product_of(self,fac,a,b,transpose_a,transpose_b)
    INTMAT(:,:) :: self
   ! Add to self the scaled product of "a" and "b"
      INTMAT(:,:), IN :: a, b
      REAL, IN :: fac
      BIN, optional, IN :: transpose_a, transpose_b
      STACK("INTMAT:plus_scaled_product_of")
      START_TIMER("INTMAT:plus_scaled_product_of")
      if (present(transpose_a)) then
        if (present(transpose_b)) then
          self = self+fac*matmul(transpose(a),transpose(b))
        else
          self = self+fac*matmul(transpose(a),b)
        end if
      else
        if (present(transpose_b)) then
          self = self+fac*matmul(a,transpose(b))
        else
          self = self+fac*matmul(a,b)
        end if
      end if
     STOP_TIMER("INTMAT:plus_scaled_product_of")
      CHECK
   end subroutine

   function trace(self) result(res)
    INTMAT(:,:) :: self
   ! Return the trace of self
      IN :: self
      INT :: res
   ! The following code is inherited from INTRINSICMAT
      INT :: dim,i
      STACK("INTMAT:trace")
      START_TIMER("INTMAT:trace")
      ENSURE(size(self,1)==size(self,2),"INTMAT:trace ... non-square matrix")
      dim = size(self,1)
      res = ZERO
      do i = 1,dim
         res = res + self(i,i)
      end do
     STOP_TIMER("INTMAT:trace")
      CHECK
   end function

   function trace_product_with(self,b) result(res)
    INTMAT(:,:) :: self
   ! Return the trace of the product of "self" with matrix b.
      IN :: self
      INTMAT(:,:), IN :: b
      INT :: res
   ! The following code is inherited from INTRINSICMAT
      INT :: i
      STACK("INTMAT:trace_product_with")
      START_TIMER("INTMAT:trace_product_with")
      ENSURE(is_transposed_shape_of_(self,b),"INTMAT:trace_product_with ... incompatible dimensions")
      res = ZERO
      do i = 1,size(self,1)
         res = res + sum( self(i,:)*b(:,i) )
      end do
     STOP_TIMER("INTMAT:trace_product_with")
      CHECK
   end function

   function sum_row_vectors(self) result(res)
    INTMAT(:,:) :: self
   ! Sum the row vectors (i.e. columns) in "self".
      INTVEC(size(self,2)) :: res
      INT :: j
      STACK("INTMAT:sum_row_vectors")
      START_TIMER("INTMAT:sum_row_vectors")
      do j = 1,size(self,2)
         res(j) = sum(self(:,j))
      end do
     STOP_TIMER("INTMAT:sum_row_vectors")
      CHECK
   end function

   function sum_column_vectors(self) result(res)
    INTMAT(:,:) :: self
   ! Sum the column vectors (i.e. rows) in "self".
      INTVEC(size(self,1)) :: res
      INT :: i
      STACK("INTMAT:sum_column_vectors")
      START_TIMER("INTMAT:sum_column_vectors")
      do i = 1,size(self,1)
         res(i) = sum(self(i,:))
      end do
     STOP_TIMER("INTMAT:sum_column_vectors")
      CHECK
   end function

   subroutine swap_columns(self,col1,col2)
    INTMAT(:,:) :: self
   ! Swap columns "col1" and "col2" in "self".
      INT :: col1,col2
      INT :: i
      REAL :: val
   STACK("INTMAT:swap_columns")
   START_TIMER("INTMAT:swap_columns")
   ENSURE(col1<=size(self,1) AND col2<=size(self,2),"INTMAT:swap_columns ... columns exceed dimesions")
      if (col1==col2) then; STOP_TIMER("INTMAT:swap_columns") CHECK return; end if
      do i = 1,size(self,1)
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do
     STOP_TIMER("INTMAT:swap_columns")
      CHECK
   end subroutine

   subroutine swap_columns_1(self,list)
    INTMAT(:,:) :: self
   ! Sequentially swap all columns in a column "list",
   ! self(:,i)       = self(:,list(i))
   ! self(:,list(i)) = self(:,i)
      INTVEC(:), IN :: list
      INT :: l
   STACK("INTMAT:swap_columns_1")
   START_TIMER("INTMAT:swap_columns_1")
   ENSURE(maxval(list)<=size(self,2),"INTMAT:swap_columns_1 ... list value exceed column dimension")
      do l = 1,size(list)
         call swap_columns_(self,l,list(l))
      end do
     STOP_TIMER("INTMAT:swap_columns_1")
      CHECK
   end subroutine

   subroutine reverse_column_order(self)
    INTMAT(:,:) :: self
   ! Reverse the order of the columns of self.
      REALVEC(:), PTR :: tmp
      INT :: n,n_col
      STACK("INTMAT:reverse_column_order")
      START_TIMER("INTMAT:reverse_column_order")
      n_col = size(self,2)
      call create_(tmp,size(self,1))
      do n=1,n_col/2
        tmp = self(:,n_col-n+1)
        self(:,n_col-n+1) = self(:,n)
        self(:,n) = tmp
      end do
      call destroy_(tmp)
     STOP_TIMER("INTMAT:reverse_column_order")
      CHECK
   end subroutine

   function column_norms(self) result(res)
    INTMAT(:,:) :: self
   ! Return the norms of every column
      REALVEC(size(self,2)) :: res
      INT :: i
      STACK("INTMAT:column_norms")
      START_TIMER("INTMAT:column_norms")
      do i = 1,size(self,2)
         res(i) = sqrt(real( sum(self(:,i)*self(:,i)) ,kind=REAL_KIND))
      end do
     STOP_TIMER("INTMAT:column_norms")
      CHECK
   end function

   subroutine get_column_norms(self,res)
    INTMAT(:,:) :: self
   ! Return the norms of every column
      REALVEC(:) :: res
      INT :: i
   STACK("INTMAT:get_column_norms")
   START_TIMER("INTMAT:get_column_norms")
   ENSURE(size(res)==size(self,2),"INTMAT:get_column_norms ... wrong size, res array")
      do i = 1,size(self,2)
         res(i) = sqrt(real( sum(self(:,i)*self(:,i)) ,kind=REAL_KIND))
      end do
     STOP_TIMER("INTMAT:get_column_norms")
      CHECK
   end subroutine

   function max_abs_column_difference(self) result(res)
    INTMAT(:,:) :: self
   ! Return the maximum of the absolute difference between all the column vector
   ! pairs of the matrix.
      INTVEC(size(self,1)) :: res
      INT :: i,j,dim
      INTVEC(size(self,1)) :: diff,col_i,col_j
      STACK("INTMAT:max_abs_column_difference")
      START_TIMER("INTMAT:max_abs_column_difference")
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
     STOP_TIMER("INTMAT:max_abs_column_difference")
      CHECK
   end function

   function mean_column_vector(self) result(res)
    INTMAT(:,:) :: self
   ! Return the mean of the column vectors.
      REALVEC(size(self,1)) :: res
      STACK("INTMAT:mean_column_vector")
      START_TIMER("INTMAT:mean_column_vector")
      res = float(sum_column_vectors_(self))/size(self,2)
     STOP_TIMER("INTMAT:mean_column_vector")
      CHECK
   end function

   subroutine compress_to_triangle(self,tr)
    INTMAT(:,:) :: self
   ! Converts the lower triangle of matrix self to the triangle "tr".
   ! using row order.
      IN :: self
      INTVEC(:) :: tr
      INT :: dim,i,j,ij
      STACK("INTMAT:compress_to_triangle")
      START_TIMER("INTMAT:compress_to_triangle")
      ENSURE(is_square_(self),"INTMAT:compress_to_triangle ... non-square matrix")
      ENSURE(size(tr)>=tri_size_(self),"INTMAT:compress_to_triangle ... triangle array too small")
      dim = size(self,1)
      ij = 0
      do i = 1,dim
         do j = 1,i
            tr(ij+j) = self(j,i)
         end do
         ij = ij+i
      end do
     STOP_TIMER("INTMAT:compress_to_triangle")
      CHECK
   end subroutine

   subroutine uncompress_from_triangle(self,tr)
    INTMAT(:,:) :: self
   ! Converts the triangle "tr" into the symmetric matrix "self".
      INTVEC(:) :: tr
      REAL :: tmp
      INT :: dim,i,j,ij
      STACK("INTMAT:uncompress_from_triangle")
      START_TIMER("INTMAT:uncompress_from_triangle")
      ENSURE(is_square_(self),"INTMAT:uncompress_from_triangle ... non-square matrix")
      ENSURE(size(tr)>=tri_size_(self),"INTMAT:uncompress_from_triangle ... triangle array too small")
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
     STOP_TIMER("INTMAT:uncompress_from_triangle")
      CHECK
   end subroutine

   subroutine from_diagonal(self,d)
    INTMAT(:,:) :: self
   ! Converts the diagonal vector "d" to matrix "self".
      INTVEC(:) :: d
      INT :: dim,i
      STACK("INTMAT:from_diagonal")
      START_TIMER("INTMAT:from_diagonal")
      ENSURE(is_square_(self),"INTMAT:from_diagonal ... non-square matrix")
      ENSURE(size(d)==size(self,1),"INTMAT:from_diagonal ... incompatibale diagonal length")
      dim  = size(d)
      self = ZERO
      do i = 1,dim
         self(i,i) = d(i)
      end do
     STOP_TIMER("INTMAT:from_diagonal")
      CHECK
   end subroutine

   function tri_size(self) result(ltr)
    INTMAT(:,:) :: self
   ! Returns the size of the lower triangle needed to store self.
      IN :: self
      INT :: ltr
      INT :: dim
      STACK("INTMAT:tri_size")
      START_TIMER("INTMAT:tri_size")
      ENSURE(is_square_(self),"INTMAT:tri_size ... non-square matrix")
      dim = size(self,1)
      ltr = dim*(dim+1)/2
     STOP_TIMER("INTMAT:tri_size")
      CHECK
   end function

   subroutine set_diagonal(self,val)
    INTMAT(:,:) :: self
   ! Set the diagonal of "self" to "val"
      INT :: val
      INT :: dim,i
      STACK("INTMAT:set_diagonal")
      START_TIMER("INTMAT:set_diagonal")
      ENSURE(is_square_(self),"INTMAT:set_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = val
      end do
     STOP_TIMER("INTMAT:set_diagonal")
      CHECK
   end subroutine

   subroutine add_to_diagonal(self,val)
    INTMAT(:,:) :: self
   ! Add "val" to the diagonal of "self"
      INT :: val
      INT :: dim,i
      STACK("INTMAT:add_to_diagonal")
      START_TIMER("INTMAT:add_to_diagonal")
      ENSURE(is_square_(self),"INTMAT:add_to_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = self(i,i) + val
      end do
     STOP_TIMER("INTMAT:add_to_diagonal")
      CHECK
   end subroutine

   subroutine zero_diagonal(self)
    INTMAT(:,:) :: self
   ! Zero the diagonal elements of "self"
      INT :: dim,i
      STACK("INTMAT:zero_diagonal")
      START_TIMER("INTMAT:zero_diagonal")
      ENSURE(is_square_(self),"INTMAT:zero_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = 0
      end do
     STOP_TIMER("INTMAT:zero_diagonal")
      CHECK
   end subroutine

   subroutine zero_off_diagonal(self)
    INTMAT(:,:) :: self
   ! Zero the off diagonal elements of "self"
      INT :: dim,i,j
      STACK("INTMAT:zero_off_diagonal")
      START_TIMER("INTMAT:zero_off_diagonal")
      ENSURE(is_square_(self),"INTMAT:zero_off_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,dim
         if (i==j) cycle
         self(i,j) = 0
      end do
      end do
     STOP_TIMER("INTMAT:zero_off_diagonal")
      CHECK
   end subroutine

   subroutine weight_diagonal(self,fac)
    INTMAT(:,:) :: self
   ! Weight the diagonal elements of "self" by "fac"
      INT, IN :: fac
      INT :: dim,i
      STACK("INTMAT:weight_diagonal")
      START_TIMER("INTMAT:weight_diagonal")
      ENSURE(is_square_(self),"INTMAT:weight_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do
     STOP_TIMER("INTMAT:weight_diagonal")
      CHECK
   end subroutine

   subroutine get_diagonal(self,diag)
    INTMAT(:,:) :: self
   ! Get the diagonal elements of "self" in vector "diag"
      INTVEC(:) :: diag
      INT :: dim,i
      STACK("INTMAT:get_diagonal")
      START_TIMER("INTMAT:get_diagonal")
      ENSURE(size(diag)==min(size(self,1),size(self,2)),"INTMAT:get_diagonal ... diag vector is incompatible")
      dim  = size(diag)
      do i = 1,dim
         diag(i) = self(i,i)
      end do
     STOP_TIMER("INTMAT:get_diagonal")
      CHECK
   end subroutine

   function max_diagonal_element(self) result(res)
    INTMAT(:,:) :: self
   ! Get the maximum element on the diagonal of the matrix
      INT :: res
      INT :: dim,i
      STACK("INTMAT:max_diagonal_element")
      START_TIMER("INTMAT:max_diagonal_element")
      ENSURE(min(size(self,1),size(self,2))>0,"INTMAT:max_diagonal_element ... cannot have zero sized dimensions")
      dim = min(size(self,1),size(self,2))
      res = self(1,1)
      do i = 2,dim
         res = max(self(i,i),res)
      end do
     STOP_TIMER("INTMAT:max_diagonal_element")
      CHECK
   end function

   function max_abs_diagonal_element(self) result(res)
    INTMAT(:,:) :: self
   ! Get the maximum absolute value of the diagonal elements of the self matrix
      INT :: res
      INT :: dim,i
      STACK("INTMAT:max_abs_diagonal_element")
      START_TIMER("INTMAT:max_abs_diagonal_element")
      ENSURE(min(size(self,1),size(self,2))>0,"INTMAT:max_abs_diagonal_element ... cannot have zero sized dimensions")
      dim = min(size(self,1),size(self,2))
      res = abs(self(1,1))
      do i = 2,dim
         res = max(abs(self(i,i)),res)
      end do
     STOP_TIMER("INTMAT:max_abs_diagonal_element")
      CHECK
   end function

   subroutine symmetric_fold(self)
    INTMAT(:,:) :: self
   ! Add the upper triangle of "self" into the lower triangle
      INT :: dim,i,j
   STACK("INTMAT:symmetric_fold")
   START_TIMER("INTMAT:symmetric_fold")
   ENSURE(is_square_(self),"INTMAT:symmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)+self(j,i)
         end do
      end do
     STOP_TIMER("INTMAT:symmetric_fold")
      CHECK
   end subroutine

   subroutine antisymmetric_fold(self)
    INTMAT(:,:) :: self
   ! Subtract the upper triangle of "self" into the lower triangle
      INT :: dim,i,j
   STACK("INTMAT:antisymmetric_fold")
   START_TIMER("INTMAT:antisymmetric_fold")
   ENSURE(is_square_(self),"INTMAT:antisymmetric_fold ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j=1,i-1
            self(i,j) = self(i,j)-self(j,i)
         end do
      end do
     STOP_TIMER("INTMAT:antisymmetric_fold")
      CHECK
   end subroutine

   subroutine symmetric_reflect(self)
    INTMAT(:,:) :: self
   ! Set the upper half of self to the lower half
      INT :: dim,i,j
   STACK("INTMAT:symmetric_reflect")
   START_TIMER("INTMAT:symmetric_reflect")
   ENSURE(is_square_(self),"INTMAT:symmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
      do j = 1,i-1
         self(j,i) = self(i,j)
      end do
      end do
     STOP_TIMER("INTMAT:symmetric_reflect")
      CHECK
   end subroutine

   subroutine antisymmetric_reflect(self)
    INTMAT(:,:) :: self
   ! Set the upper half of self to the negative of the lower half.
   ! The diagonals are set to zero
      INT :: dim,i,j
   STACK("INTMAT:antisymmetric_reflect")
   START_TIMER("INTMAT:antisymmetric_reflect")
   ENSURE(is_square_(self),"INTMAT:antisymmetric_reflect ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         do j = 0,i-1
            self(j,i) = -self(i,j)
         end do
      end do
      do i = 1,dim
         self(i,i) = 0
      end do
     STOP_TIMER("INTMAT:antisymmetric_reflect")
      CHECK
   end subroutine

   subroutine make_diagonally_dominant(self,permutation)
    INTMAT(:,:) :: self
   ! Rearrange the order of the columns of self so that the largest magnitude
   ! elements in each column occur along the diagonal. If "permutation" is
   ! present, it is a matrix which achieves this ordering, i.e. at the
   ! conclusion of the routine, self = self(:,permutation).
      INTVEC(:), optional :: permutation
      INTVEC(:), PTR :: perm
      BINMAT(:,:), PTR :: mask
      INT :: i,n
      INTVEC(2) :: loc
   STACK("INTMAT:make_diagonally_dominant")
   START_TIMER("INTMAT:make_diagonally_dominant")
   ENSURE(is_square_(self),"INTMAT:make_diagonally_dominant ... not square")
      if (present(permutation)) &
   ENSURE(size(permutation)==size(self,2),"INTMAT:make_diagonally_dominant ... wrong size, perm")
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
     STOP_TIMER("INTMAT:make_diagonally_dominant")
      CHECK
   end subroutine

   subroutine to_gaussian_xyz_powers(self,l_max)
    INTMAT(:,:) :: self
   ! Make "self(1:3,i)", the three xyz powers of all cartesian gaussian
   ! functions "i" of angular momentum up to "l_max", where "i" is the standard
   ! lexical index of the cartesian gaussian. (This routine essentially defines
   ! the standard order).  The shape of "self" is: [3, l_max.n_comp_sum ].
     OUT :: self
     INT, IN :: l_max
     INT :: i,L,a,b,c
   STACK("INTMAT:to_gaussian_xyz_powers")
   START_TIMER("INTMAT:to_gaussian_xyz_powers")
   ENSURE(size(self,1)==3,"INTMAT:to_gaussian_xyz_powers ... wrong 1st dimension, self")
   ENSURE(size(self,2)==n_comp_sum_(l_max),"INTMAT:to_gaussian_xyz_powers ... wrong 2nd dimension, self")
     i = 1          ! This is the total lexical index
     do L = 0,l_max ! Loop over all shells with momentum L
                    ! Loop over powers a, b, c
       do a = L,floor((L+2)*THIRD),-1
         do b = min(L-a,a),floor((L-a+1)*HALF),-1
           c = L-a-b
           if (a==b AND b==c) then
             self(:,i)   = (/a,a,a/)
             i = i+1
           else if (a>b AND b==c) then
             self(:,i)   = (/a,b,b/)
             self(:,i+1) = (/b,a,b/)
             self(:,i+2) = (/b,b,a/)
             i = i+3
           else if (a==b AND b>c) then
             self(:,i)   = (/a,a,c/)
             self(:,i+1) = (/a,c,a/)
             self(:,i+2) = (/c,a,a/)
             i = i+3
           else
             self(:,i)   = (/a,b,c/)
             self(:,i+1) = (/a,c,b/)
             self(:,i+2) = (/b,a,c/)
             self(:,i+3) = (/c,a,b/)
             self(:,i+4) = (/b,c,a/)
             self(:,i+5) = (/c,b,a/)
             i = i+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INTMAT:to_gaussian_xyz_powers")
      CHECK
   end subroutine

   subroutine to_gaussian_xyz_powers_1(self,l_max,index)
    INTMAT(:,:) :: self
   ! Make "self(1:3,i)", the three xyz powers of all cartesian gaussian
   ! functions "i" of angular momentum up to "l_max", where "i" is the standard
   ! lexical index of the cartesian gaussian. (This routine essentially defines
   ! the standard order).  The shape of "self" is: [3, l_max.n_comp_sum ].
   ! Array "index" maps the three xyz powers of each cartesian gaussian back to
   ! its lexical index *within a shell of the same angular momentum* i.e. not
   ! the total lexical index. NOTE: "index" has lower bounds of 0, and so is
   ! passed as a pointer.
     OUT :: self
     INT, IN :: l_max
     INTMAT3(:,:,:), PTR :: index
     INT :: i,L,k,a,b,c
   STACK("INTMAT:to_gaussian_xyz_powers_1")
   START_TIMER("INTMAT:to_gaussian_xyz_powers_1")
   ENSURE(size(self,1)==3,"INTMAT:to_gaussian_xyz_powers_1 ... wrong 1st dimension, self")
   ENSURE(size(self,2)==n_comp_sum_(l_max),"INTMAT:to_gaussian_xyz_powers_1 ... wrong 2nd dimension, self")
   ENSURE(lbound(index,1)==0,"INTMAT:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   ENSURE(lbound(index,2)==0,"INTMAT:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   ENSURE(lbound(index,3)==0,"INTMAT:to_gaussian_xyz_powers_1 ... wrong lower bound, index")
   ENSURE(ubound(index,1)==l_max,"INTMAT:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
   ENSURE(ubound(index,2)==l_max,"INTMAT:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
   ENSURE(ubound(index,3)==l_max,"INTMAT:to_gaussian_xyz_powers_1 ... wrong upper bound, index")
     i = 1              ! This is the total lexical index
     do L = 0,l_max     ! Loop over all shells with momentum L
       k = 1            ! This is the local shell lexical index
       !                  Loop over powers a, b, c
       do a = L,floor((L+2)*THIRD),-1
         do b = min(L-a,a),floor((L-a+1)*HALF),-1
           c = L-a-b
           if (a==b AND b==c) then
             self(:,i)   = (/a,a,a/)
             index(a,a,a) = k
             i = i+1
             k = k+1
           else if (a>b AND b==c) then
             self(:,i)   = (/a,b,b/)
             self(:,i+1) = (/b,a,b/)
             self(:,i+2) = (/b,b,a/)
             index(a,b,b) = k
             index(b,a,b) = k+1
             index(b,b,a) = k+2
             i = i+3
             k = k+3
           else if (a==b AND b>c) then
             self(:,i)   = (/a,a,c/)
             self(:,i+1) = (/a,c,a/)
             self(:,i+2) = (/c,a,a/)
             index(a,a,c) = k
             index(a,c,a) = k+1
             index(c,a,a) = k+2
             i = i+3
             k = k+3
           else
             self(:,i)   = (/a,b,c/)
             self(:,i+1) = (/a,c,b/)
             self(:,i+2) = (/b,a,c/)
             self(:,i+3) = (/c,a,b/)
             self(:,i+4) = (/b,c,a/)
             self(:,i+5) = (/c,b,a/)
             index(a,b,c) = k
             index(a,c,b) = k+1
             index(b,a,c) = k+2
             index(c,a,b) = k+3
             index(b,c,a) = k+4
             index(c,b,a) = k+5
             i = i+6
             k = k+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INTMAT:to_gaussian_xyz_powers_1")
      CHECK
   end subroutine

   subroutine bin_XY_data(self,X,Y,bin_side_length,data_count)
    INTMAT(:,:) :: self
   ! Set self to a matrix whose ij-th element contains the number of data points
   ! [X(k),Y(k)] which lie in the ij-th bin. A bin is simply a range of values
   ! of side length "bin_side_length" covering the set of points from
   ! [X_min,Y_min] to [X_max,Y_max]. The dimension of self is calculated within
   ! this routine. If "data_count" is present, then the bin count associated
   ! with each data item [X(k),Y(k)] is returned in an array.
      PTR :: self
      REALVEC(:), IN :: X,Y
      REAL, IN :: bin_side_length
      INTVEC(:), PTR, optional :: data_count
      REAL :: X_min,X_max,X_mid,X_ran
      REAL :: Y_min,Y_max,Y_mid,Y_ran
      REALVEC(2) :: X_range,Y_range
      INT :: dim,n_X,n_Y,i,j,k
   STACK("INTMAT:bin_XY_data")
   START_TIMER("INTMAT:bin_XY_data")
   ENSURE(size(X)==size(Y),"INTMAT:bin_XY_data ... incompatible data points")
      dim = size(X)
      X_min = minval(X); Y_min = minval(Y)
      X_max = maxval(X); Y_max = maxval(Y)
      X_mid = HALF*(X_min+X_max)
      Y_mid = HALF*(Y_min+Y_max)
      X_ran = X_max-X_min
      Y_ran = Y_max-Y_min
      n_X = ceiling(X_ran/bin_side_length)
      n_Y = ceiling(Y_ran/bin_side_length)
      X_min = X_mid - (n_X/TWO)*bin_side_length
      Y_min = Y_mid - (n_Y/TWO)*bin_side_length
      X_ran = X_ran/n_X; Y_ran = Y_ran/n_Y
      ! Now do the binning ...
      call create_(self,n_X,n_Y)
      if (present(data_count)) call create_(data_count,dim)
      do i = 1,n_X
      do j = 1,n_Y
         X_range(1) = X_min + (i-1)*bin_side_length
         X_range(2) = X_range(1)  + bin_side_length
         Y_range(1) = Y_min + (j-1)*bin_side_length
         Y_range(2) = Y_range(1)  + bin_side_length
         self(i,j) = count(in_range_(X,X_range) AND in_range_(Y,Y_range))
         if (present(data_count)) then
         do k = 1,dim
            if (is_in_range_(X(k),X_range) AND is_in_range_(Y(k),Y_range)) then
            data_count(k) = self(i,j)
            end if
         end do
         end if
      end do
      end do
     STOP_TIMER("INTMAT:bin_XY_data")
      UNSTACK
   end subroutine

end
