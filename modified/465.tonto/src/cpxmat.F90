!---------------------------------------------------------------------------
!
!  complex matrix operations :: CPXMAT ...
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
! $Id: cpxmat.foo,v 1.17.2.2 2003/11/13 05:34:39 reaper Exp $
!---------------------------------------------------------------------------

module CPXMAT_MODULE

#  include "cpxmat.use"

   implicit none

#  include "macros"
#  include "cpxmat.int"


   REAL, private :: tol5 = 1.0d-5
!   tol10 :: REAL, private = 1.0d-10

contains

   subroutine create(self,dim1,dim2)
    CPXMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
      STACK("CPXMAT:create")
      START_TIMER("CPXMAT:create")
      nullify(self)
      allocate(self(dim1,dim2))
      ADD_MEMORY(dim1*dim2*CPX_SIZE)
     STOP_TIMER("CPXMAT:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    CPXMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: lb1,ub1,lb2,ub2
   ! The following code is inherited from INTRINSICMAT
      STACK("CPXMAT:create_1")
      START_TIMER("CPXMAT:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*CPX_SIZE)
     STOP_TIMER("CPXMAT:create_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CPXMAT(:,:) :: self
   ! Destroy the object
      PTR :: self
   ! The following code is inherited from INTRINSICMAT
      STACK("CPXMAT:destroy")
      START_TIMER("CPXMAT:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CPXMAT:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*CPX_SIZE)
      deallocate(self)
     STOP_TIMER("CPXMAT:destroy")
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
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix is square
      IN :: self
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(self,2)
     STOP_TIMER("CPXMAT:is_square")
   end function

   function is_symmetric(self,tol) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix is symmetric. You can set the tolerance "tol"
   ! for comparison.
      BIN :: res
      REAL, optional :: tol
      STACK("CPXMAT:is_symmetric")
      START_TIMER("CPXMAT:is_symmetric")
      res = is_square_(self) AND same_as_(self,transpose(self),tol)
     STOP_TIMER("CPXMAT:is_symmetric")
      CHECK
   end function

   function is_antisymmetric(self,tol) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix is symmetric. You can set the tolerance "tol"
   ! for comparison.
      BIN :: res
      REAL, optional :: tol
      STACK("CPXMAT:is_antisymmetric")
      START_TIMER("CPXMAT:is_antisymmetric")
      res = is_square_(self) AND same_as_(self,-transpose(self),tol)
     STOP_TIMER("CPXMAT:is_antisymmetric")
      CHECK
   end function

   function is_hermitian(self,tol) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix is hermitian. You can set the tolerance "tol"
   ! for comparison.
      IN :: self
      BIN :: res
      REAL, optional :: tol
      STACK("CPXMAT:is_hermitian")
      START_TIMER("CPXMAT:is_hermitian")
      res = is_square_(self) AND same_as_(self,transpose(conjg(self)),tol)
     STOP_TIMER("CPXMAT:is_hermitian")
      CHECK
   end function

   function is_antihermitian(self,tol) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix is antihermitian. You can set the tolerance
   ! "tol" for comparison.
      IN :: self
      BIN :: res
      REAL, optional :: tol
      STACK("CPXMAT:is_antihermitian")
      START_TIMER("CPXMAT:is_antihermitian")
      res = is_square_(self) AND same_as_(self,-transpose(conjg(self)),tol)
     STOP_TIMER("CPXMAT:is_antihermitian")
      CHECK
   end function

   PURE function is_same_shape_as(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" has the same shape as self
      IN :: self
      CPXMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2)
     STOP_TIMER("CPXMAT:is_same_shape_as")
   end function

   PURE function is_same_shape_as_1(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" has the same shape as self
      IN :: self
      REALMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2)
     STOP_TIMER("CPXMAT:is_same_shape_as_1")
   end function

   PURE function is_transposed_shape_of(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" is the transposed shape of self
      IN :: self
      CPXMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) AND size(self,2)==size(b,1)
     STOP_TIMER("CPXMAT:is_transposed_shape_of")
   end function

   PURE function is_transposed_shape_of_1(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" is the transposed shape of self
      IN :: self
      REALMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      res = size(self,1)==size(b,2) AND size(self,2)==size(b,1)
     STOP_TIMER("CPXMAT:is_transposed_shape_of_1")
   end function

   function is_zero(self,eps) result(res)
    CPXMAT(:,:) :: self
   ! Return TRUE is "self" is the zero matrix, i.e. every element is zero.
   ! If present, "eps" is used to decide when a small number is zero.
      REAL, optional, IN :: eps
      BIN :: res
      INT :: dim1,dim2,i,j
      REAL :: re,im
      STACK("CPXMAT:is_zero")
      START_TIMER("CPXMAT:is_zero")
      dim1 = size(self,1)
      dim2 = size(self,2)
      res = TRUE
      do i = 1,dim1
      do j = 1,dim2
         re = real(self(i,j))
         im = aimag(self(i,j))
         if (is_zero_(re,eps) AND is_zero_(im,eps)) cycle
         res = FALSE
         exit
      end do
      end do
     STOP_TIMER("CPXMAT:is_zero")
      CHECK
   end function

   function dot(self,l,r) result(res)
    CPXMAT(:,:) :: self
   ! Multiply the matrix self by the complex conjugate of vector "l" on the left
   ! and vector "r" on the right ie:  res = l^\dagger self r. Useful for
   ! non-unit metric dot_products.
     CPXVEC(:), IN :: l,r
     CPX :: res
     CPXVEC(:), PTR :: w
     STACK("CPXMAT:dot")
     START_TIMER("CPXMAT:dot")
     ENSURE(size(self,1)==size(l),"CPXMAT:dot ... wrong size, r")
     ENSURE(size(self,2)==size(r),"CPXMAT:dot ... wrong size, r")
     call create_(w,size(l))
     call to_product_of_(w,self,r)
     res = dot_product(l,w)
     call destroy_(w)
     STOP_TIMER("CPXMAT:dot")
      CHECK
   end function

   subroutine to_product_of(self,a,b,dagger_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Set "self" to the matrix product of "a" and "b". If present,
   ! "dagger_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! needs to be daggerred.
      CPXMAT(:,:) :: a, b
      BIN, optional :: dagger_a, dagger_b
      BIN :: dagg_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:to_product_of")
      START_TIMER("CPXMAT:to_product_of")
      dagg_a = FALSE;       dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case (0) ! .to_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case (1) ! .to_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case (2) ! .to_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
        case (3) ! .to_product_ad_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:to_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:to_product_of")
      CHECK
   end subroutine

   subroutine to_product_of_1(self,a,b,dagger_a,transpose_b)
    CPXMAT(:,:) :: self
   ! Set "self" to the product of complex matrix "a" and real matrix "b".
   ! If present, "dagger_a" and "transpose_b" can be set to TRUE
   ! if "a" and "b" needs to be daggerred or transposed.
      CPXMAT(:,:) :: a
      REALMAT(:,:) :: b
      BIN, optional :: dagger_a, transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:to_product_of_1")
      START_TIMER("CPXMAT:to_product_of_1")
      dagg_a = FALSE;           trans_b = FALSE
      if (present(dagger_a))    dagg_a  = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0) ! .to_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(1) ! .to_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(2) ! .to_product_an_bt
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(j,k)
            end do
            self(i,j) = temp
          end do
          end do
        case(3) ! .to_product_ad_bt
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:to_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:to_product_of_1")
      CHECK
   end subroutine

   subroutine to_product_of_2(self,a,b,transpose_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Set "self" to the product of real matrix "a" and complex matrix "b".
   ! If present, "transpose_a" and "dagger_b" can be set to TRUE
   ! if "a" and "b" needs to be daggerred or transposed.
      REALMAT(:,:) :: a
      CPXMAT(:,:) :: b
      BIN, optional :: transpose_a, dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:to_product_of_2")
      START_TIMER("CPXMAT:to_product_of_2")
      trans_a = FALSE;          dagg_b  = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0) ! .to_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(1) ! .to_product_at_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(k,i) * b(k,j)
            end do
            self(i,j) = temp
          end do
          end do
        case(2) ! .to_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
        case(3) ! .to_product_at_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:to_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:to_product_of_2")
      CHECK
   end subroutine

   subroutine plus_product_of(self,a,b,dagger_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Add to "self" the matrix product of "a" and "b". If present, "dagger_a"
   ! and "dagger_b" can be set to TRUE if "a" and "b" neeb to be daggerd.
      CPXMAT(:,:) :: a, b
      BIN, optional :: dagger_a, dagger_b
      BIN :: dagg_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:plus_product_of")
      START_TIMER("CPXMAT:plus_product_of")
      dagg_a = FALSE;        dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case(0) ! .plus_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(1) ! .plus_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(2) ! .plus_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(3) ! .plus_product_ad_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:plus_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:plus_product_of")
      CHECK
   end subroutine

   subroutine plus_product_of_1(self,a,b,dagger_a,transpose_b)
    CPXMAT(:,:) :: self
   ! Add to "self" the matrix product of "a" and real matrix "b".
   ! If present, "dagger_a" and "transpose_b" can be set to TRUE
   ! if "a" and "b" need to be daggered or transposed
      CPXMAT(:,:) :: a
      REALMAT(:,:) :: b
      BIN, optional :: dagger_a, transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:plus_product_of_1")
      START_TIMER("CPXMAT:plus_product_of_1")
      dagg_a = FALSE;           trans_b = FALSE
      if (present(dagger_a))    dagg_a = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0) ! .plus_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(1) ! .plus_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(2) ! .plus_product_an_bt
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * b(j,k)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(3) ! .plus_product_ad_bt
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:plus_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:plus_product_of_1")
      CHECK
   end subroutine

   subroutine plus_product_of_2(self,a,b,transpose_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Add to "self" the product of real matrix "a" and complex matrix "b".
   ! If present, "tranpose_a" and "dagger_b" can be set to TRUE if
   ! "a" and "b" need to be transposed or daggerd.
      REALMAT(:,:) :: a
      CPXMAT(:,:) :: b
      BIN, optional :: transpose_a, dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:plus_product_of_2")
      START_TIMER("CPXMAT:plus_product_of_2")
      trans_a = FALSE;          dagg_b  = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0) ! .plus_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(1) ! .plus_product_at_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(k,i) * b(k,j)
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(2) ! .plus_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
        case(3) ! .plus_product_at_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:plus_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:plus_product_of_2")
      CHECK
   end subroutine

   subroutine to_scaled_product_of(self,fac,a,b,dagger_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Set "self" to the matrix product of "a" and "b" scaled by "fac". If
   ! present, "dagger_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! need to be daggerred.
      REAL :: fac
      CPXMAT(:,:) :: a, b
      BIN, optional :: dagger_a, dagger_b
      BIN :: dagg_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:to_scaled_product_of")
      START_TIMER("CPXMAT:to_scaled_product_of")
      dagg_a = FALSE;        dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case(0) ! .to_scaled_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(1) ! .to_scaled_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(2) ! .to_scaled_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(3) ! .to_scaled_product_ad_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:to_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:to_scaled_product_of")
      CHECK
   end subroutine

   subroutine to_scaled_product_of_1(self,fac,a,b,dagger_a,transpose_b)
    CPXMAT(:,:) :: self
   ! Set "self" to the matrix product of "a" and real matrix "b" scaled by "fac".
   ! If present, "dagger_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be daggerred or transposed.
      REAL :: fac
      CPXMAT(:,:) :: a
      REALMAT(:,:) :: b
      BIN, optional :: dagger_a, transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:to_scaled_product_of_1")
      START_TIMER("CPXMAT:to_scaled_product_of_1")
      dagg_a = FALSE;           trans_b = FALSE
      if (present(dagger_a))    dagg_a  = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0) ! .to_scaled_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(1) ! .to_scaled_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(2) ! .to_scaled_product_an_bt
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + a(i,k) * b(j,k)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(3) ! .to_scaled_product_ad_bt
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:to_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              self(i,j) = self(i,j) + conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = fac*temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:to_scaled_product_of_1")
      CHECK
   end subroutine

   subroutine to_scaled_product_of_2(self,fac,a,b,transpose_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Set "self" to the product of real matrix "a" and complex "b" scaled by "fac".
   ! If present, "transpose_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! need to be transposed or daggerred.
      REAL :: fac
      REALMAT(:,:) :: a
      CPXMAT(:,:) :: b
      BIN, optional :: transpose_a, dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:to_scaled_product_of_2")
      START_TIMER("CPXMAT:to_scaled_product_of_2")
      trans_a = FALSE;          dagg_b  = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0) ! .to_scaled_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(1) ! .to_scaled_product_at_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(k,i) * b(k,j)
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(2) ! .to_scaled_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
        case(3) ! .to_scaled_product_at_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:to_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = fac*temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:to_scaled_product_of_2")
      CHECK
   end subroutine

   subroutine plus_scaled_product_of(self,fac,a,b,dagger_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Add to "self" the matrix product of "a" and "b" scaled by "fac". If
   ! present, "dagger_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! need to be daggerred.
      REAL :: fac
      CPXMAT(:,:) :: a, b
      BIN, optional :: dagger_a, dagger_b
      BIN :: dagg_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:plus_scaled_product_of")
      START_TIMER("CPXMAT:plus_scaled_product_of")
      dagg_a = FALSE;        dagg_b = FALSE
      if (present(dagger_a)) dagg_a = dagger_a
      if (present(dagger_b)) dagg_b = dagger_b
      opt = 0
      if (dagg_a) opt = opt + 1
      if (dagg_b) opt = opt + 2
      select case (opt)
        case(0) ! .plus_scaled_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(1) ! .plus_scaled_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(2) ! .plus_scaled_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(3) ! .plus_scaled_product_ad_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:plus_scaled_product_of ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:plus_scaled_product_of")
      CHECK
   end subroutine

   subroutine plus_scaled_product_of_1(self,fac,a,b,dagger_a,transpose_b)
    CPXMAT(:,:) :: self
   ! Add to "self" the matrix product of "a" and real matrix "b" scaled by "fac".
   ! If present, "dagger_a" and "transpose_b" can be set to TRUE if "a" and "b"
   ! need to be daggerred or transposed.
      REAL :: fac
      CPXMAT(:,:) :: a
      REALMAT(:,:) :: b
      BIN, optional :: dagger_a, transpose_b
      BIN :: dagg_a,trans_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:plus_scaled_product_of_1")
      START_TIMER("CPXMAT:plus_scaled_product_of_1")
      dagg_a = FALSE;           trans_b = FALSE
      if (present(dagger_a))    dagg_a  = dagger_a
      if (present(transpose_b)) trans_b = transpose_b
      opt = 0
      if (dagg_a)  opt = opt + 1
      if (trans_b) opt = opt + 2
      select case (opt)
        case(0) ! .plus_scaled_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(1) ! .plus_scaled_product_ad_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(2) ! .plus_scaled_product_an_bt
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(j,k)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(3) ! .plus_scaled_product_ad_bt
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:plus_scaled_product_of_1 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * conjg(a(k,i)) * b(j,k)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:plus_scaled_product_of_1")
      CHECK
   end subroutine

   subroutine plus_scaled_product_of_2(self,fac,a,b,transpose_a,dagger_b)
    CPXMAT(:,:) :: self
   ! Add to "self" the product of real matrix "a" and complex "b" scaled by "fac".
   ! If present, "transpose_a" and "dagger_b" can be set to TRUE if "a" and "b"
   ! need to be transposed or daggerred.
      REAL :: fac
      REALMAT(:,:) :: a
      CPXMAT(:,:) :: b
      BIN, optional :: transpose_a, dagger_b
      BIN :: trans_a,dagg_b
      INT :: opt
      INT :: dima,dim1,dim2,i,j,k
      CPX :: temp
      STACK("CPXMAT:plus_scaled_product_of_2")
      START_TIMER("CPXMAT:plus_scaled_product_of_2")
      trans_a = FALSE;          dagg_b  = FALSE
      if (present(transpose_a)) trans_a = transpose_a
      if (present(dagger_b))    dagg_b  = dagger_b
      opt = 0
      if (trans_a) opt = opt + 1
      if (dagg_b)  opt = opt + 2
      select case (opt)
        case(0) ! .plus_scaled_product_an_bn
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(i,k) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(1) ! .plus_scaled_product_at_bn
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(k,i) * b(k,j)
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(2) ! .plus_scaled_product_an_bd
          ENSURE( size(self,1)==size(a,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,2)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,2)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(i,k) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
        case(3) ! .plus_scaled_product_at_bd
          ENSURE( size(self,1)==size(a,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE( size(self,2)==size(b,1),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          ENSURE(size(a,1)==size(b,2),"CPXMAT:plus_scaled_product_of_2 ... incompatible shapes")
          dima = size(a,1)
          dim1 = size(self,1)
          dim2 = size(self,2)
          do i=1,dim1
          do j=1,dim2
            temp = (ZERO,ZERO)
            do k=1,dima
              temp = temp + fac * a(k,i) * conjg(b(j,k))
            end do
            self(i,j) = self(i,j) + fac*temp
          end do
          end do
      end select
     STOP_TIMER("CPXMAT:plus_scaled_product_of_2")
      CHECK
   end subroutine

   subroutine to_product_with_diagonal(self,a,diag,dagger_a)
    CPXMAT(:,:) :: self
   ! set self to the product of "a" and with diagonal "diag"
      CPXMAT(:,:) :: a
      REALVEC(:) :: diag
      BIN, optional :: dagger_a
      INT :: a1,a2,s1,s2,d1,i,j
      REAL :: diag_j
      STACK("CPXMAT:to_product_with_diagonal")
      START_TIMER("CPXMAT:to_product_with_diagonal")
      ENSURE(is_same_shape_as_(self,a),"CPXMAT:to_product_with_diagonal ... incompatible dimensions")
      s1 = size(self,1); s2 = size(self,2)
      a1 = size(a,1);    a2 = size(a,2)
      d1 = size(diag)
      if (present(dagger_a)) then
         ENSURE(a1==d1,"CPXMAT:to_product_with_diagonal ... incompatible dimensions")
         do j = 1,s2
           diag_j = diag(j)
           do i = 1,s1
             self(i,j) = conjg(a(j,i))*diag_j
           end do
         end do
      else
         ENSURE(a2==d1,"CPXMAT:to_product_with_diagonal ... incompatible dimensions")
         do j = 1,s2
           diag_j = diag(j)
           do i = 1,s1
             self(i,j) = a(i,j)*diag_j
           end do
         end do
      end if
     STOP_TIMER("CPXMAT:to_product_with_diagonal")
      CHECK
   end subroutine

   subroutine to_product_with_diagonal_1(self,dg,a,dagger_a)
    CPXMAT(:,:) :: self
   ! set self to the product of diagonal "dg" with "a"
      CPXMAT(:,:) :: a
      REALVEC(:) :: dg
      BIN, optional :: dagger_a
      INT :: a1,a2,s1,s2,d1,i,j
      REAL :: dg_i
      STACK("CPXMAT:to_product_with_diagonal_1")
      START_TIMER("CPXMAT:to_product_with_diagonal_1")
      ENSURE(is_same_shape_as_(self,a),"CPXMAT:to_product_with_diagonal_1 ... incompatible dimensions")
      s1 = size(self,1);  s2 = size(self,2)
      a1 = size(a,1); a2 = size(a,2)
      d1 = size(dg)
      if (present(dagger_a)) then
         ENSURE(a2==d1,"CPXMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,s1
           dg_i = dg(i)
           do j = 1,s2
             self(i,j) = dg_i*conjg(a(j,i))
           end do
         end do
      else
         ENSURE(a1==d1,"CPXMAT:to_product_with_diagonal_1 ... incompatible dimensions")
         do i = 1,s1
           dg_i = dg(i)
           do j = 1,s2
             self(i,j) = dg_i*a(i,j)
           end do
         end do
      end if
     STOP_TIMER("CPXMAT:to_product_with_diagonal_1")
      CHECK
   end subroutine

   function trace(self) result(res)
    CPXMAT(:,:) :: self
   ! Return the trace of self
      IN :: self
      CPX :: res
   ! The following code is inherited from INTRINSICMAT
      INT :: dim,i
      STACK("CPXMAT:trace")
      START_TIMER("CPXMAT:trace")
      ENSURE(size(self,1)==size(self,2),"CPXMAT:trace ... non-square matrix")
      dim = size(self,1)
      res = ZERO
      do i = 1,dim
         res = res + self(i,i)
      end do
     STOP_TIMER("CPXMAT:trace")
      CHECK
   end function

! These next few commented routines are unused ESSL routines.

!   solve_general_eigenproblem(eigenvalues,eigenvectors)
!   ! Solve the eigenproblem for "self", yeilding a vector of "eigenvalues" and
!   ! a matrix of "eigenvectors"
!      eigenvalues :: CPXVEC
!      eigenvectors :: CPXMAT
!       W :: REALVEC*
!      dim1,dim2,dime,dimv :: INT
!      select :: BIN
!      dim1 = .dim1
!      dim2 = .dim2
!      dime = size(eigenvalues)
!      dimv = size(eigenvectors)
!      ENSURE(dim1==dim2,"non-square matrix")
!      ENSURE(dime>=dim1,"supplied eigenvalue array too small")
!      ENSURE(dimv>=dim1*dim1,"supplied eigenvector matrix too small")
!      W.create(3*dim1)
!      call zgeev(1,self,dim1,eigenvalues,eigenvectors,dim1,select,dim1,W,3*dim1)
!      W.destroy
!   end
!
!   solve_linear_equations(rhs,solution)
!   ! Solve the linear equations posed by "self", with "rhs" as the RHS vector,
!   ! yeilding vector "solution" as the answer
!      rhs, solution :: CPXVEC
!      err,nrhs :: INT
!      LU :: CPXMAT*
!      pivot :: INTVEC*
!      dim,dim1,dim2 :: INT
!      dim1 = .dim1
!      dim2 = .dim2
!      ENSURE(dim1==dim2,"non-square matrix")
!      dim = size(rhs)
!      ENSURE(dim==dim1,"incompatible rhs")
!      nrhs = 1
!      LU.create(dim,dim)
!      pivot.create(dim)
!      LU = self
!      solution = rhs
!      call zgef(LU,dim,dim,pivot)
!      call zges(LU,dim,dim,pivot,solution,0)
!      pivot.destroy
!      LU.destroy
!   end
!
!   solve_linear_equations(rhs,solution)
!   ! Solve the linear equations posed by "self", with "rhs" as a matrix of RHS vectors,
!   ! yeilding matrix "solution" as a matrix of solution vectors.
!      rhs, solution :: CPXMAT
!      err,nrhs :: INT
!      LU :: CPXMAT*
!      pivot :: INTVEC*
!      dim1,dim2 :: INT
!      dim1 = .dim1
!      dim2 = .dim2
!      ENSURE(dim1==dim2,"non-square matrix")
!      dim1 = size(rhs,1)
!      nrhs = size(rhs,2)
!      ENSURE(dim1==dim2,"rhs incompatible with coefficient matrix")
!      ENSURE(nrhs>0,"no rhs vectors")
!      LU.create(dim1,dim1)
!      pivot.create(dim1)
!      LU = self
!      solution = rhs
!      call zgef(LU,dim1,dim1,pivot)
!      call zgesm("N",LU,dim1,dim1,pivot,solution,dim1,nrhs)
!      pivot.destroy
!      LU.destroy
!   end

   subroutine solve_eigenproblem(self,eigenvalues,eigenvectors,routine)
    CPXMAT(:,:) :: self
   ! Solve the hermitian eigenvalue problem for self
      REALVEC(:) :: eigenvalues
      CPXMAT(:,:) :: eigenvectors
      STR(*), optional :: routine
STACK("CPXMAT:solve_eigenproblem")
START_TIMER("CPXMAT:solve_eigenproblem")
#ifdef ESSL
      call solve_eigenproblem_ESSL_(self,eigenvalues,eigenvectors,routine)
#else
      call solve_eigenproblem_LAPACK_(self,eigenvalues,eigenvectors,routine)
#endif
     STOP_TIMER("CPXMAT:solve_eigenproblem")
      CHECK
   end subroutine

   subroutine solve_eigenproblem_ESSL(self,eigenvalues,eigenvectors,routine)
    CPXMAT(:,:) :: self
   ! Solve the hermitian eigenvalue problem for self. ESSL version.
      REALVEC(:) :: eigenvalues
      CPXMAT(:,:) :: eigenvectors
      STR(*), optional :: routine
      CPXVEC(:), PTR :: ap
      REALVEC(:), PTR :: RW
      INT :: dim
      STACK("CPXMAT:solve_eigenproblem_ESSL")
      START_TIMER("CPXMAT:solve_eigenproblem_ESSL")
      ENSURE(is_square_(self),"CPXMAT:solve_eigenproblem_ESSL ... non-square matrix")
      ENSURE(size(eigenvalues)>=size(self,1),"CPXMAT:solve_eigenproblem_ESSL ... supplied eigenvalue array too small")
      ENSURE(size(eigenvectors)>=size(self),"CPXMAT:solve_eigenproblem_ESSL ... supplied eigenvector array too small")
      ENSURE(NOT present(routine),"CPXMAT:solve_eigenproblem_ESSL ... routine specifier not allowed in ESSL version")
      dim = size(self,1)
      call create_(ap,dim*(dim+1)/2)
      call compress_to_triangle_(self,ap)
      call create_(RW,4*dim)
#ifdef ESSL
      call zhpev(21,ap,eigenvalues,eigenvectors,dim,dim,RW,4*dim)
#endif
      call destroy_(RW)
      call destroy_(ap)
     STOP_TIMER("CPXMAT:solve_eigenproblem_ESSL")
      CHECK
   end subroutine

   subroutine solve_eigenproblem_LAPACK(self,eigenvalues,eigenvectors,routine)
    CPXMAT(:,:) :: self
   ! Solve the hermitian eigenvalue problem for self. LAPACK version.
      REALVEC(:) :: eigenvalues
      CPXMAT(:,:) :: eigenvectors
      STR(*), optional :: routine
      STR(STR_SIZE) :: rout
      CPXVEC(:), PTR :: ap,W
      REALVEC(:), PTR :: RW
      INT :: dim,fail,info
      STACK("CPXMAT:solve_eigenproblem_LAPACK")
      START_TIMER("CPXMAT:solve_eigenproblem_LAPACK")
      ENSURE(is_square_(self),"CPXMAT:solve_eigenproblem_LAPACK ... non-square matrix")
      ENSURE(size(eigenvalues)>=size(self,1),"CPXMAT:solve_eigenproblem_LAPACK ... supplied eigenvalue array too small")
      ENSURE(size(eigenvectors)>=size(self),"CPXMAT:solve_eigenproblem_LAPACK ... supplied eigenvector array too small")
      rout = "zheev"
      if (present(routine)) rout = routine
      dim = size(self,1)
      select case (rout)
        case ("zheev")
          call create_(W,dim*dim)
          call create_(RW,3*dim)
          call set_to_(eigenvectors,self)
          fail = 0
#ifndef ESSL
          call zheev("V","L",dim,eigenvectors,dim,eigenvalues,W,dim*dim,RW,fail)
#endif
          call destroy_(RW)
          call destroy_(W)
        case ("zhpev")
          call create_(ap,dim*(dim+1)/2)
          call compress_to_triangle_(self,ap)
          call create_(W,2*dim)
          call create_(RW,3*dim)
#ifndef ESSL
          call zhpev("V","U",dim,ap,eigenvalues,eigenvectors,dim,W,RW,info)
#endif
          call destroy_(RW)
          call destroy_(W)
          call destroy_(ap)
      end select
     STOP_TIMER("CPXMAT:solve_eigenproblem_LAPACK")
      CHECK
   end subroutine

   function trace_of_product_with(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Return the trace of the product of self with "b"
      CPXMAT(:,:), IN :: b
      CPX :: res
      STACK("CPXMAT:trace_of_product_with")
      START_TIMER("CPXMAT:trace_of_product_with")
      res = trace_of_product_(self,b)
     STOP_TIMER("CPXMAT:trace_of_product_with")
      CHECK
   end function

   function trace_of_product(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Return the trace of the product of self with "b"
      IN :: self
      CPXMAT(:,:), IN :: b
      CPX :: res
      INT :: a1,a2,i,j
      STACK("CPXMAT:trace_of_product")
      START_TIMER("CPXMAT:trace_of_product")
      ENSURE(is_transposed_shape_of_(self,b),"CPXMAT:trace_of_product ... incompatible shape")
      a1 = size(self,1)
      a2 = size(self,2)
      res = ZERO
      if (a1==1 AND a2==1) then
        res = self(1,1)*b(1,1)
      else
        res = ZERO
        do i = 1,a1
          do j = 1,a2
            res = res + self(i,j)*b(j,i)
          end do
        end do
      end if
     STOP_TIMER("CPXMAT:trace_of_product")
      CHECK
   end function

   function trace_of_product_with_1(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Return the trace of the product of self with real matrix "b"
      IN :: self
      REALMAT(:,:), IN :: b
      CPX :: res
      STACK("CPXMAT:trace_of_product_with_1")
      START_TIMER("CPXMAT:trace_of_product_with_1")
      res = trace_of_product_(self,b)
     STOP_TIMER("CPXMAT:trace_of_product_with_1")
      CHECK
   end function

   function trace_of_product_1(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Return the trace of the product of self with real matrix "b"
      IN :: self
      REALMAT(:,:), IN :: b
      CPX :: res
      INT :: a1,a2,i,j
      STACK("CPXMAT:trace_of_product_1")
      START_TIMER("CPXMAT:trace_of_product_1")
      ENSURE(is_transposed_shape_of_(self,b),"CPXMAT:trace_of_product_1 ... incompatible shape")
      a1 = size(self,1)
      a2 = size(self,2)
      if (a1==1 AND a2==1) then
        res = self(1,1)*b(1,1)
      else
        res = ZERO
        do i = 1,a1
          do j = 1,a2
            res = res + self(i,j)*b(j,i)
          end do
        end do
      end if
     STOP_TIMER("CPXMAT:trace_of_product_1")
      CHECK
   end function

   function equals(self,b) result(res)
    CPXMAT(:,:) :: self
   ! Check if the matrix is the same as "b".
      IN :: self
      CPXMAT(:,:), IN :: b
      BIN :: res
      STACK("CPXMAT:equals")
      START_TIMER("CPXMAT:equals")
      res = same_as_(self,b)
     STOP_TIMER("CPXMAT:equals")
      CHECK
   end function

   function same_as(self,b, tol) result(res)
    CPXMAT(:,:) :: self
   ! Return TRUE if self is the same as "b:", within tolerance "tol", if
   ! provided, or 10^-5 if not.
      CPXMAT(:,:) :: b
      REAL, optional :: tol
      BIN :: res
      REAL :: diff
      INT :: a1,a2,i,j
      REAL :: tolerance
      STACK("CPXMAT:same_as")
      START_TIMER("CPXMAT:same_as")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:same_as ... incompatible shape")
      a1 = size(self,1)
      a2 = size(self,2)
      if (present(tol))     tolerance = tol
      if (NOT present(tol)) tolerance = tol5
      diff = ZERO
      do i = 1,a1
      do j = 1,a2
         diff = diff + abs(self(j,i)-b(j,i))**2
      end do
      end do
      diff = sqrt(diff)
      res = FALSE
      if (diff<tolerance) res=TRUE
     STOP_TIMER("CPXMAT:same_as")
      CHECK
   end function

   subroutine swap_columns(self,col1,col2)
    CPXMAT(:,:) :: self
   ! Swap columns "col1" and "col2" of self
      INT, IN :: col1,col2
   ! The following code is inherited from INTRINSICMAT
      INT :: a1,a2,i
      CPX :: val
      STACK("CPXMAT:swap_columns")
      START_TIMER("CPXMAT:swap_columns")
      ENSURE(col1<=size(self,2) AND col2<=size(self,2),"CPXMAT:swap_columns ... columns exceed dimesions")
      a1 = size(self,1)
      a2 = size(self,2)
      do i = 1,a1
         val = self(i,col1)
         self(i,col1) = self(i,col2)
         self(i,col2) = val
      end do
     STOP_TIMER("CPXMAT:swap_columns")
      CHECK
   end subroutine

   subroutine set_to(self,b)
    CPXMAT(:,:) :: self
   ! Set self to "b"
      CPXMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("CPXMAT:set_to")
      START_TIMER("CPXMAT:set_to")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:set_to ... incompatible shape")
      self = b
     STOP_TIMER("CPXMAT:set_to")
      CHECK
   end subroutine

   subroutine plus(self,b)
    CPXMAT(:,:) :: self
   ! Add to self the matrix "b"
      CPXMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("CPXMAT:plus")
      START_TIMER("CPXMAT:plus")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:plus ... incompatible shape")
      self = self+b
     STOP_TIMER("CPXMAT:plus")
      CHECK
   end subroutine

   subroutine minus(self,b)
    CPXMAT(:,:) :: self
   ! Subtract from self the matrix "b"
      CPXMAT(:,:), IN :: b
   ! The following code is inherited from INTRINSICMAT
      STACK("CPXMAT:minus")
      START_TIMER("CPXMAT:minus")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:minus ... incompatible shape")
      self = self-b
     STOP_TIMER("CPXMAT:minus")
      CHECK
   end subroutine

   subroutine to_scaled_mat(self,fac,b)
    CPXMAT(:,:) :: self
   ! Set self to the scaled matrix "b"
      CPXMAT(:,:), IN :: b
      CPX, IN :: fac
      STACK("CPXMAT:to_scaled_mat")
      START_TIMER("CPXMAT:to_scaled_mat")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:to_scaled_mat ... incompatible shape")
      self = fac*b
     STOP_TIMER("CPXMAT:to_scaled_mat")
      CHECK
   end subroutine

   subroutine to_scaled_mat_1(self,fac,b)
    CPXMAT(:,:) :: self
   ! Set self to the scaled matrix "b"
      CPXMAT(:,:), IN :: b
      REAL, IN :: fac
      STACK("CPXMAT:to_scaled_mat_1")
      START_TIMER("CPXMAT:to_scaled_mat_1")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:to_scaled_mat_1 ... incompatible shape")
      self = fac*b
     STOP_TIMER("CPXMAT:to_scaled_mat_1")
      CHECK
   end subroutine

   subroutine plus_scaled_mat(self,fac,b)
    CPXMAT(:,:) :: self
   ! Add to self the scaled matrix "b"
      CPXMAT(:,:), IN :: b
      CPX, IN :: fac
      STACK("CPXMAT:plus_scaled_mat")
      START_TIMER("CPXMAT:plus_scaled_mat")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:plus_scaled_mat ... incompatible shape")
      self = self+fac*b
     STOP_TIMER("CPXMAT:plus_scaled_mat")
      CHECK
   end subroutine

   subroutine plus_scaled_mat_1(self,fac,b)
    CPXMAT(:,:) :: self
   ! Add to self the scaled matrix "b"
      CPXMAT(:,:), IN :: b
      REAL, IN :: fac
      STACK("CPXMAT:plus_scaled_mat_1")
      START_TIMER("CPXMAT:plus_scaled_mat_1")
      ENSURE(is_same_shape_as_(self,b),"CPXMAT:plus_scaled_mat_1 ... incompatible shape")
      self = self+fac*b
     STOP_TIMER("CPXMAT:plus_scaled_mat_1")
      CHECK
   end subroutine

   subroutine change_basis(self,V)
    CPXMAT(:,:) :: self
   ! Change the basis of self using matrix V, i.e. self = V^dagger self V
      CPXMAT(:,:) :: V
      CPXMAT(:,:), PTR :: W
      INT :: o1,v2
      STACK("CPXMAT:change_basis")
      START_TIMER("CPXMAT:change_basis")
      ENSURE( is_square_(self),"CPXMAT:change_basis ... non-square matrix")
      ENSURE(is_square_(V),"CPXMAT:change_basis ... new basis not square")
      ENSURE(size(self,1)==size(V,1),"CPXMAT:change_basis ... incompatible sizes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(self,V,W,dagger_a=TRUE)
   !  W = matmul(self,V)
   !  self = matmul(transpose(conjg(V)),W)
      call destroy_(W)
     STOP_TIMER("CPXMAT:change_basis")
      CHECK
   end subroutine

   subroutine change_basis_1(self,L,R)
    CPXMAT(:,:) :: self
   ! Change the basis of self using diagonals L and R, i.e. self = L^T self R
      REALVEC(:) :: L,R
      CPXMAT(:,:), PTR :: W
      INT :: l1,r1
      STACK("CPXMAT:change_basis_1")
      START_TIMER("CPXMAT:change_basis_1")
      ENSURE(size(self,1)==size(L),"CPXMAT:change_basis_1 ... incompatible sizes")
      ENSURE(size(self,2)==size(R),"CPXMAT:change_basis_1 ... incompatible sizes")
      l1 = size(L)
      r1 = size(R)
      call create_(W,l1,r1)
      call to_product_with_diagonal_(W,self,R)
      call to_product_with_diagonal_(self,L,W)
      call destroy_(W)
     STOP_TIMER("CPXMAT:change_basis_1")
      CHECK
   end subroutine

   subroutine change_basis_2(self,new,V)
    CPXMAT(:,:) :: self
   ! Set new = V^T self V
      CPXMAT(:,:), OUT :: new
      CPXMAT(:,:), IN :: V
      CPXMAT(:,:), PTR :: W
      INT :: o1,v2
      STACK("CPXMAT:change_basis_2")
      START_TIMER("CPXMAT:change_basis_2")
      ENSURE(   is_square_(self),"CPXMAT:change_basis_2 ... non-square matrix")
      ENSURE(is_square_(new),"CPXMAT:change_basis_2 ... new basis not square")
      ENSURE(size(V,1)==size(self,2),"CPXMAT:change_basis_2 ... incompatible sizes")
      ENSURE(size(V,2)==size(new,2),"CPXMAT:change_basis_2 ... incompatible sizes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,dagger_a=TRUE)
      call destroy_(W)
     STOP_TIMER("CPXMAT:change_basis_2")
      CHECK
   end subroutine

   subroutine change_basis_3(self,new,V)
    CPXMAT(:,:) :: self
   ! Set new = V^T self V, V a real matrix
      IN :: self
      CPXMAT(:,:), INOUT :: new
      REALMAT(:,:), IN :: V
      CPXMAT(:,:), PTR :: W
      INT :: o1,v2
      STACK("CPXMAT:change_basis_3")
      START_TIMER("CPXMAT:change_basis_3")
      ENSURE(   is_square_(self),"CPXMAT:change_basis_3 ... non-square matrix")
      ENSURE(is_square_(new),"CPXMAT:change_basis_3 ... new basis not square")
      ENSURE(size(V,1)==size(self,2),"CPXMAT:change_basis_3 ... incompatible sizes")
      ENSURE(size(V,2)==size(new,2),"CPXMAT:change_basis_3 ... incompatible sizes")
      o1 = size(self,1)
      v2 = size(V,2)
      call create_(W,o1,v2)
      call to_product_of_(W,self,V)
      call to_product_of_(new,V,W,transpose_a=TRUE)
      call destroy_(W)
     STOP_TIMER("CPXMAT:change_basis_3")
      CHECK
   end subroutine

   subroutine back_transform(self,new,V)
    CPXMAT(:,:) :: self
   ! Back transform "self" using vectors "V", and place the result in "new",
   ! new = V self V^dagger
      CPXMAT(:,:) :: new,V
      CPXMAT(:,:), PTR :: W
      INT :: v1,o2
      STACK("CPXMAT:back_transform")
      START_TIMER("CPXMAT:back_transform")
      ENSURE(   is_square_(self),"CPXMAT:back_transform ... non-square matrix")
      ENSURE(is_square_(new),"CPXMAT:back_transform ... new basis not square")
      ENSURE(size(V,1)==size(new,1),"CPXMAT:back_transform ... incompatible sizes")
      ENSURE(size(V,2)==size(self,1),"CPXMAT:back_transform ... incompatible sizes")
      v1 = size(V,1)
      o2 = size(self,2)
      call create_(W,v1,o2)
      call to_product_of_(W,V,self) ! W = V self
      call to_product_of_(new,W,V,dagger_b=TRUE)  ! new = V self V^dagger
      call destroy_(W)
     STOP_TIMER("CPXMAT:back_transform")
      CHECK
   end subroutine

   subroutine compress_to_square(self,sq)
    CPXMAT(:,:) :: self
   ! Compresses the hermitian matrix self to vector "sq". First comes the
   ! lower half of the real part, then lower half of the imaginary part.
      IN :: self
      REALVEC(:) :: sq
      INT :: dim1,i,j,ij
      STACK("CPXMAT:compress_to_square")
      START_TIMER("CPXMAT:compress_to_square")
      ENSURE(is_square_(self),"CPXMAT:compress_to_square ... non-square matrix")
      ENSURE(size(sq)>=size(self),"CPXMAT:compress_to_square ... sq array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            sq(ij+j) = real(self(i,j),kind=REAL_KIND)
         end do
         ij = ij+i
      end do
      do i = 1,dim1
         do j = 1,i-1
            sq(ij+j) = aimag(self(i,j))
         end do
         ij = ij+i-1
      end do
     STOP_TIMER("CPXMAT:compress_to_square")
      CHECK
   end subroutine

   subroutine uncompress_from_square(self,sq)
    CPXMAT(:,:) :: self
   ! Uncompress the vector "sq" to a hermitian matrix assuming the lower half
   ! of the real part comes first, then the lower half of the imaginary part.
      REALVEC(:) :: sq
      INT :: dim1,i,j,ij
      STACK("CPXMAT:uncompress_from_square")
      START_TIMER("CPXMAT:uncompress_from_square")
      ENSURE(is_square_(self),"CPXMAT:uncompress_from_square ... non-square matrix")
      ENSURE(size(sq)>=size(self),"CPXMAT:uncompress_from_square ... sq array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            self(i,j) = sq(ij+j)
            self(j,i) = sq(ij+j)
         end do
         ij = ij+i
      end do
      do i = 1,dim1
         do j = 1,i-1
            self(i,j) = self(i,j) + cmplx(ZERO,sq(ij+j),kind=CPX_KIND)
            self(j,i) = self(j,i) - cmplx(ZERO,sq(ij+j),kind=CPX_KIND)
         end do
         ij = ij+i-1
      end do
     STOP_TIMER("CPXMAT:uncompress_from_square")
      CHECK
   end subroutine

   subroutine compress_to_triangle(self,tr)
    CPXMAT(:,:) :: self
   ! Converts the upper triangle of matrix self to the triangle "tr".
      IN :: self
      CPXVEC(:) :: tr
      INT :: dim1,i,j,ij
      STACK("CPXMAT:compress_to_triangle")
      START_TIMER("CPXMAT:compress_to_triangle")
      ENSURE(is_square_(self),"CPXMAT:compress_to_triangle ... non-square matrix")
      ENSURE(size(tr)>=size(self,1)*(size(self,1)+1)/2,"CPXMAT:compress_to_triangle ... tr array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            tr(ij+j) = self(j,i)
         end do
         ij = ij+i
      end do
     STOP_TIMER("CPXMAT:compress_to_triangle")
      CHECK
   end subroutine

   subroutine uncompress_from_triangle(self,tr)
    CPXMAT(:,:) :: self
   ! Converts the triangle "tr" into the hermitian matrix "self".
   ! WARNING: won't work for symmetric matrices
      CPXVEC(:) :: tr
      INT :: dim1,i,j,ij
      STACK("CPXMAT:uncompress_from_triangle")
      START_TIMER("CPXMAT:uncompress_from_triangle")
      ENSURE(is_square_(self),"CPXMAT:uncompress_from_triangle ... non-square matrix")
      ENSURE(size(tr)>=tri_size_(self),"CPXMAT:uncompress_from_triangle ... tr array too small")
      dim1 = size(self,1)
      ij = 0
      do i = 1,dim1
         do j = 1,i
            self(j,i) =       tr(ij+j)
            self(i,j) = conjg(tr(ij+j))
         end do
         ij = ij+i
      end do
     STOP_TIMER("CPXMAT:uncompress_from_triangle")
      CHECK
   end subroutine

   subroutine from_diagonal(self,d)
    CPXMAT(:,:) :: self
   ! Converts the diagonal vector "d" to matrix "self".
      REALVEC(:) :: d
      INT :: dim,i
      STACK("CPXMAT:from_diagonal")
      START_TIMER("CPXMAT:from_diagonal")
      ENSURE(is_square_(self),"CPXMAT:from_diagonal ... non-square matrix")
      ENSURE(size(d)==size(self,1),"CPXMAT:from_diagonal ... wrong diagonal length")
      dim  = size(d)
      self = ZERO
      do i = 1,dim
         self(i,i) = d(i)
      end do
     STOP_TIMER("CPXMAT:from_diagonal")
      CHECK
   end subroutine

   function tri_size(self) result(ltr)
    CPXMAT(:,:) :: self
   ! Returns the size of the lower triangle needed to store self.
      IN :: self
      INT :: ltr
      INT :: dim1
      STACK("CPXMAT:tri_size")
      START_TIMER("CPXMAT:tri_size")
      ENSURE(is_square_(self),"CPXMAT:tri_size ... non-square matrix")
      dim1 = size(self,1)
      ltr = dim1*(dim1+1)/2
     STOP_TIMER("CPXMAT:tri_size")
      CHECK
   end function

   subroutine to_unit_mat(self)
    CPXMAT(:,:) :: self
   ! Set self to the unit matrix
      INT :: dim,i
      STACK("CPXMAT:to_unit_mat")
      START_TIMER("CPXMAT:to_unit_mat")
      ENSURE(is_square_(self),"CPXMAT:to_unit_mat ... non-square matrix")
      dim = size(self,1)
      self = (0.0,0.0)
      do i = 1,dim
         self(i,i) = (1.0,0.0)
      end do
     STOP_TIMER("CPXMAT:to_unit_mat")
      CHECK
   end subroutine

   subroutine weight_diagonal(self,fac)
    CPXMAT(:,:) :: self
   ! Weight the diagonals of self by "fac"
      CPX :: fac
      INT :: dim,i
      STACK("CPXMAT:weight_diagonal")
      START_TIMER("CPXMAT:weight_diagonal")
      ENSURE(is_square_(self),"CPXMAT:weight_diagonal ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do
     STOP_TIMER("CPXMAT:weight_diagonal")
      CHECK
   end subroutine

   subroutine weight_diagonal_1(self,fac)
    CPXMAT(:,:) :: self
   ! Weight the diagonals of self by "fac"
      REAL :: fac
      INT :: dim,i
      STACK("CPXMAT:weight_diagonal_1")
      START_TIMER("CPXMAT:weight_diagonal_1")
      ENSURE(is_square_(self),"CPXMAT:weight_diagonal_1 ... non-square matrix")
      dim = size(self,1)
      do i = 1,dim
         self(i,i) = fac*self(i,i)
      end do
     STOP_TIMER("CPXMAT:weight_diagonal_1")
      CHECK
   end subroutine

   subroutine get_diagonal(self,diag)
    CPXMAT(:,:) :: self
   ! Return the diagonals of self in "diag"
      CPXVEC(:) :: diag
      INT :: dim,i
      STACK("CPXMAT:get_diagonal")
      START_TIMER("CPXMAT:get_diagonal")
      ENSURE(size(diag)==min(size(self,1),size(self,2)),"CPXMAT:get_diagonal ... diag vector is incompatible")
      dim  = size(diag)
      do i = 1,dim
         diag(i) = self(i,i)
      end do
     STOP_TIMER("CPXMAT:get_diagonal")
      CHECK
   end subroutine

   subroutine hermitian_fold(self)
    CPXMAT(:,:) :: self
   ! Add the hermitian conjugate of the upper half of the
   ! matrix into its lower half.
      INT :: dim1,i,j
      STACK("CPXMAT:hermitian_fold")
      START_TIMER("CPXMAT:hermitian_fold")
      ENSURE(is_square_(self),"CPXMAT:hermitian_fold ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j=1,i-1
            self(i,j) = self(i,j)+conjg(self(j,i))
         end do
         self(i,i) = real(self(i,i),kind=REAL_KIND)
      end do
     STOP_TIMER("CPXMAT:hermitian_fold")
      CHECK
   end subroutine

   subroutine antihermitian_fold(self)
    CPXMAT(:,:) :: self
   ! Subtract the hermitian conjugate of the upper half of the
   ! matrix into its lower half.
      INT :: dim1,i,j
      STACK("CPXMAT:antihermitian_fold")
      START_TIMER("CPXMAT:antihermitian_fold")
      ENSURE(is_square_(self),"CPXMAT:antihermitian_fold ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j=1,i-1
            self(i,j) = self(i,j)-conjg(self(j,i))
         end do
         self(i,i) = ZERO
      end do
     STOP_TIMER("CPXMAT:antihermitian_fold")
      CHECK
   end subroutine

   subroutine make_hermitian(self)
    CPXMAT(:,:) :: self
   ! Make the upper half of self hermitian with respect to the lower half
      INT :: dim1,i,j
      STACK("CPXMAT:make_hermitian")
      START_TIMER("CPXMAT:make_hermitian")
      ENSURE(is_square_(self),"CPXMAT:make_hermitian ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = real(self(i,i),kind=REAL_KIND)
      end do
     STOP_TIMER("CPXMAT:make_hermitian")
      CHECK
   end subroutine

   subroutine hermitian_reflect(self)
    CPXMAT(:,:) :: self
   ! Make the upper half of self hermitian with respect
   ! to the lower half
      INT :: dim1,i,j
      STACK("CPXMAT:hermitian_reflect")
      START_TIMER("CPXMAT:hermitian_reflect")
      ENSURE(is_square_(self),"CPXMAT:hermitian_reflect ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = real(self(i,i),kind=REAL_KIND)
      end do
     STOP_TIMER("CPXMAT:hermitian_reflect")
      CHECK
   end subroutine

   subroutine make_antihermitian(self)
    CPXMAT(:,:) :: self
   ! Make the upper half of self anti-hermitian with respect
   ! to the lower half
      INT :: dim1,i,j
      STACK("CPXMAT:make_antihermitian")
      START_TIMER("CPXMAT:make_antihermitian")
      ENSURE(is_square_(self),"CPXMAT:make_antihermitian ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = -conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = ZERO
      end do
     STOP_TIMER("CPXMAT:make_antihermitian")
      CHECK
   end subroutine

   subroutine antihermitian_reflect(self)
    CPXMAT(:,:) :: self
   ! Make the upper half of self anti-hermitian with respect
   ! to the lower half
      INT :: dim1,i,j
      STACK("CPXMAT:antihermitian_reflect")
      START_TIMER("CPXMAT:antihermitian_reflect")
      ENSURE(is_square_(self),"CPXMAT:antihermitian_reflect ... non-square matrix")
      dim1 = size(self,1)
      do i = 1,dim1
         do j = 1,i-1
            self(j,i) = -conjg(self(i,j))
         end do
      end do
      do i = 1,dim1
         self(i,i) = ZERO
      end do
     STOP_TIMER("CPXMAT:antihermitian_reflect")
      CHECK
   end subroutine

!  ************************
!  Block returning routines
!  ************************

   function alpha_alpha(self) result(res)
    CPXMAT(:,:) :: self
   ! return the alpha-alpha sector of the matrix
      TARGET :: self
      CPXMAT(:,:), PTR :: res
      INT :: n
      STACK("CPXMAT:alpha_alpha")
      START_TIMER("CPXMAT:alpha_alpha")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha ... not even-dimensioned")
      n = size(self,1)/2
      res => self(1:n,1:n)
     STOP_TIMER("CPXMAT:alpha_alpha")
      CHECK
   end function

   function beta_alpha(self) result(res)
    CPXMAT(:,:) :: self
   ! return the beta-alpha sector of the matrix
      TARGET :: self
      CPXMAT(:,:), PTR :: res
      INT :: n
      STACK("CPXMAT:beta_alpha")
      START_TIMER("CPXMAT:beta_alpha")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha ... not even-dimensioned")
      n = size(self,1)/2
      res => self(n+1:2*n,1:n)
     STOP_TIMER("CPXMAT:beta_alpha")
      CHECK
   end function

   function alpha_beta(self) result(res)
    CPXMAT(:,:) :: self
   ! return the alpha-beta sector of the matrix
      TARGET :: self
      CPXMAT(:,:), PTR :: res
      INT :: n
      STACK("CPXMAT:alpha_beta")
      START_TIMER("CPXMAT:alpha_beta")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta ... not even-dimensioned")
      n = size(self,1)/2
      res => self(1:n,n+1:2*n)
     STOP_TIMER("CPXMAT:alpha_beta")
      CHECK
   end function

   function beta_beta(self) result(res)
    CPXMAT(:,:) :: self
   ! return the beta-beta sector of the matrix
      TARGET :: self
      CPXMAT(:,:), PTR :: res
      INT :: n
      STACK("CPXMAT:beta_beta")
      START_TIMER("CPXMAT:beta_beta")
      ENSURE(is_square_(self),"CPXMAT:beta_beta ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta ... not even-dimensioned")
      n = size(self,1)/2
      res => self(n+1:2*n,n+1:2*n)
     STOP_TIMER("CPXMAT:beta_beta")
      CHECK
   end function

!  ***************
!  Set_to routines
!  ***************

   subroutine alpha_alpha_set_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-alpha sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_set_to")
      START_TIMER("CPXMAT:alpha_alpha_set_to")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_set_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = factor*X
      else;                      self(1:n,1:n) = X
      end if
     STOP_TIMER("CPXMAT:alpha_alpha_set_to")
      CHECK
   end subroutine

   subroutine alpha_alpha_set_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-alpha sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_set_to_1")
      START_TIMER("CPXMAT:alpha_alpha_set_to_1")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_set_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = factor*X
     STOP_TIMER("CPXMAT:alpha_alpha_set_to_1")
      CHECK
   end subroutine

   subroutine alpha_alpha_set_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-alpha sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_set_to_2")
      START_TIMER("CPXMAT:alpha_alpha_set_to_2")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_set_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = factor*X
      else;                      self(1:n,1:n) = X
      end if
     STOP_TIMER("CPXMAT:alpha_alpha_set_to_2")
      CHECK
   end subroutine

   subroutine alpha_alpha_set_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-alpha sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_set_to_3")
      START_TIMER("CPXMAT:alpha_alpha_set_to_3")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_set_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = factor*X
     STOP_TIMER("CPXMAT:alpha_alpha_set_to_3")
      CHECK
   end subroutine

   subroutine beta_alpha_set_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-alpha sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_set_to")
      START_TIMER("CPXMAT:beta_alpha_set_to")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_set_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = factor*X
      else;                      self(n+1:2*n,1:n) = X
      end if
     STOP_TIMER("CPXMAT:beta_alpha_set_to")
      CHECK
   end subroutine

   subroutine beta_alpha_set_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-alpha sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_set_to_1")
      START_TIMER("CPXMAT:beta_alpha_set_to_1")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_set_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = factor*X
     STOP_TIMER("CPXMAT:beta_alpha_set_to_1")
      CHECK
   end subroutine

   subroutine beta_alpha_set_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-alpha sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_set_to_2")
      START_TIMER("CPXMAT:beta_alpha_set_to_2")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_set_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = factor*X
      else;                      self(n+1:2*n,1:n) = X
      end if
     STOP_TIMER("CPXMAT:beta_alpha_set_to_2")
      CHECK
   end subroutine

   subroutine beta_alpha_set_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-alpha sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_set_to_3")
      START_TIMER("CPXMAT:beta_alpha_set_to_3")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_set_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = factor*X
     STOP_TIMER("CPXMAT:beta_alpha_set_to_3")
      CHECK
   end subroutine

   subroutine alpha_beta_set_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-beta sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_set_to")
      START_TIMER("CPXMAT:alpha_beta_set_to")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_set_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = factor*X
      else;                      self(1:n,n+1:2*n) = X
      end if
     STOP_TIMER("CPXMAT:alpha_beta_set_to")
      CHECK
   end subroutine

   subroutine alpha_beta_set_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-beta sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_set_to_1")
      START_TIMER("CPXMAT:alpha_beta_set_to_1")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_set_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = factor*X
     STOP_TIMER("CPXMAT:alpha_beta_set_to_1")
      CHECK
   end subroutine

   subroutine alpha_beta_set_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-beta sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_set_to_2")
      START_TIMER("CPXMAT:alpha_beta_set_to_2")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_set_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = factor*X
      else;                      self(1:n,n+1:2*n) = X
      end if
     STOP_TIMER("CPXMAT:alpha_beta_set_to_2")
      CHECK
   end subroutine

   subroutine alpha_beta_set_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the alpha-beta sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_set_to_3")
      START_TIMER("CPXMAT:alpha_beta_set_to_3")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_set_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = factor*X
     STOP_TIMER("CPXMAT:alpha_beta_set_to_3")
      CHECK
   end subroutine

   subroutine beta_beta_set_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-beta sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_set_to")
      START_TIMER("CPXMAT:beta_beta_set_to")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_set_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_set_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = factor*X
      else;                      self(n+1:2*n,n+1:2*n) = X
      end if
     STOP_TIMER("CPXMAT:beta_beta_set_to")
      CHECK
   end subroutine

   subroutine beta_beta_set_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-beta sector of the matrix to "X"
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_set_to_1")
      START_TIMER("CPXMAT:beta_beta_set_to_1")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_set_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_set_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = factor*X
     STOP_TIMER("CPXMAT:beta_beta_set_to_1")
      CHECK
   end subroutine

   subroutine beta_beta_set_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-beta sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_set_to_2")
      START_TIMER("CPXMAT:beta_beta_set_to_2")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_set_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_set_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = factor*X
      else;                      self(n+1:2*n,n+1:2*n) = X
      end if
     STOP_TIMER("CPXMAT:beta_beta_set_to_2")
      CHECK
   end subroutine

   subroutine beta_beta_set_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Set the beta-beta sector of the matrix to "X"
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_set_to_3")
      START_TIMER("CPXMAT:beta_beta_set_to_3")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_set_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_set_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = factor*X
     STOP_TIMER("CPXMAT:beta_beta_set_to_3")
      CHECK
   end subroutine

!  ***************
!  Put_to routines
!  ***************

   subroutine alpha_alpha_put_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-alpha sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_put_to")
      START_TIMER("CPXMAT:alpha_alpha_put_to")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_put_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,1:n)
      else;                      X = self(1:n,1:n)
      end if
     STOP_TIMER("CPXMAT:alpha_alpha_put_to")
      CHECK
   end subroutine

   subroutine alpha_alpha_put_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-alpha sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      REAL :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_put_to_1")
      START_TIMER("CPXMAT:alpha_alpha_put_to_1")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_put_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,1:n)
     STOP_TIMER("CPXMAT:alpha_alpha_put_to_1")
      CHECK
   end subroutine

   subroutine alpha_alpha_put_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-alpha sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_put_to_2")
      START_TIMER("CPXMAT:alpha_alpha_put_to_2")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_put_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,1:n)
      else;                      X = self(1:n,1:n)
      end if
     STOP_TIMER("CPXMAT:alpha_alpha_put_to_2")
      CHECK
   end subroutine

   subroutine alpha_alpha_put_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-alpha sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_put_to_3")
      START_TIMER("CPXMAT:alpha_alpha_put_to_3")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_put_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,1:n)
     STOP_TIMER("CPXMAT:alpha_alpha_put_to_3")
      CHECK
   end subroutine

   subroutine beta_alpha_put_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-alpha sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_put_to")
      START_TIMER("CPXMAT:beta_alpha_put_to")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_put_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,1:n)
      else;                      X = self(n+1:2*n,1:n)
      end if
     STOP_TIMER("CPXMAT:beta_alpha_put_to")
      CHECK
   end subroutine

   subroutine beta_alpha_put_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-alpha sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_put_to_1")
      START_TIMER("CPXMAT:beta_alpha_put_to_1")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_put_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,1:n)
     STOP_TIMER("CPXMAT:beta_alpha_put_to_1")
      CHECK
   end subroutine

   subroutine beta_alpha_put_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-alpha sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_put_to_2")
      START_TIMER("CPXMAT:beta_alpha_put_to_2")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_put_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,1:n)
      else;                      X = self(n+1:2*n,1:n)
      end if
     STOP_TIMER("CPXMAT:beta_alpha_put_to_2")
      CHECK
   end subroutine

   subroutine beta_alpha_put_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-alpha sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_put_to_3")
      START_TIMER("CPXMAT:beta_alpha_put_to_3")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_put_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,1:n)
     STOP_TIMER("CPXMAT:beta_alpha_put_to_3")
      CHECK
   end subroutine

   subroutine alpha_beta_put_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-beta sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_put_to")
      START_TIMER("CPXMAT:alpha_beta_put_to")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_put_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,n+1:2*n)
      else;                      X = self(1:n,n+1:2*n)
      end if
     STOP_TIMER("CPXMAT:alpha_beta_put_to")
      CHECK
   end subroutine

   subroutine alpha_beta_put_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-beta sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_put_to_1")
      START_TIMER("CPXMAT:alpha_beta_put_to_1")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_put_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,n+1:2*n)
     STOP_TIMER("CPXMAT:alpha_beta_put_to_1")
      CHECK
   end subroutine

   subroutine alpha_beta_put_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-beta sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_put_to_2")
      START_TIMER("CPXMAT:alpha_beta_put_to_2")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_put_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(1:n,n+1:2*n)
      else;                      X = self(1:n,n+1:2*n)
      end if
     STOP_TIMER("CPXMAT:alpha_beta_put_to_2")
      CHECK
   end subroutine

   subroutine alpha_beta_put_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the alpha-beta sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_put_to_3")
      START_TIMER("CPXMAT:alpha_beta_put_to_3")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_put_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(1:n,n+1:2*n)
     STOP_TIMER("CPXMAT:alpha_beta_put_to_3")
      CHECK
   end subroutine

   subroutine beta_beta_put_to(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-beta sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_put_to")
      START_TIMER("CPXMAT:beta_beta_put_to")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_put_to ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_put_to ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,n+1:2*n)
      else;                      X = self(n+1:2*n,n+1:2*n)
      end if
     STOP_TIMER("CPXMAT:beta_beta_put_to")
      CHECK
   end subroutine

   subroutine beta_beta_put_to_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-beta sector of the matrix to "X"
      CPXMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_put_to_1")
      START_TIMER("CPXMAT:beta_beta_put_to_1")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_put_to_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_put_to_1 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,n+1:2*n)
     STOP_TIMER("CPXMAT:beta_beta_put_to_1")
      CHECK
   end subroutine

   subroutine beta_beta_put_to_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-beta sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_put_to_2")
      START_TIMER("CPXMAT:beta_beta_put_to_2")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_put_to_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_put_to_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; X = factor*self(n+1:2*n,n+1:2*n)
      else;                      X = self(n+1:2*n,n+1:2*n)
      end if
     STOP_TIMER("CPXMAT:beta_beta_put_to_2")
      CHECK
   end subroutine

   subroutine beta_beta_put_to_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Put the beta-beta sector of the matrix to "X"
      REALMAT(:,:), OUT :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_put_to_3")
      START_TIMER("CPXMAT:beta_beta_put_to_3")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_put_to_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_put_to_3 ... not even-dimensioned")
      n = size(self,1)/2
      X = factor*self(n+1:2*n,n+1:2*n)
     STOP_TIMER("CPXMAT:beta_beta_put_to_3")
      CHECK
   end subroutine

!  *************
!  plus routines
!  *************

   subroutine alpha_alpha_plus(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-alpha sector of the matrix
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_plus")
      START_TIMER("CPXMAT:alpha_alpha_plus")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_plus ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = self(1:n,1:n) + factor*X
      else;                      self(1:n,1:n) = self(1:n,1:n) + X
      end if
     STOP_TIMER("CPXMAT:alpha_alpha_plus")
      CHECK
   end subroutine

   subroutine alpha_alpha_plus_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-alpha sector of the matrix
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_plus_1")
      START_TIMER("CPXMAT:alpha_alpha_plus_1")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_plus_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n) + factor*X
     STOP_TIMER("CPXMAT:alpha_alpha_plus_1")
      CHECK
   end subroutine

   subroutine alpha_alpha_plus_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-alpha sector of the matrix
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_plus_2")
      START_TIMER("CPXMAT:alpha_alpha_plus_2")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_plus_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,1:n) = self(1:n,1:n) + factor*X
      else;                      self(1:n,1:n) = self(1:n,1:n) + X
      end if
     STOP_TIMER("CPXMAT:alpha_alpha_plus_2")
      CHECK
   end subroutine

   subroutine alpha_alpha_plus_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-alpha sector of the matrix
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_alpha_plus_3")
      START_TIMER("CPXMAT:alpha_alpha_plus_3")
      ENSURE(is_square_(self),"CPXMAT:alpha_alpha_plus_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_alpha_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,1:n) = self(1:n,1:n) + factor*X
     STOP_TIMER("CPXMAT:alpha_alpha_plus_3")
      CHECK
   end subroutine

   subroutine beta_alpha_plus(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-alpha sector of the matrix
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_plus")
      START_TIMER("CPXMAT:beta_alpha_plus")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_plus ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X
      else;                      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + X
      end if
     STOP_TIMER("CPXMAT:beta_alpha_plus")
      CHECK
   end subroutine

   subroutine beta_alpha_plus_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-alpha sector of the matrix
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_plus_1")
      START_TIMER("CPXMAT:beta_alpha_plus_1")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_plus_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X
     STOP_TIMER("CPXMAT:beta_alpha_plus_1")
      CHECK
   end subroutine

   subroutine beta_alpha_plus_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-alpha sector of the matrix
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_plus_2")
      START_TIMER("CPXMAT:beta_alpha_plus_2")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_plus_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X
      else;                      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + X
      end if
     STOP_TIMER("CPXMAT:beta_alpha_plus_2")
      CHECK
   end subroutine

   subroutine beta_alpha_plus_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-alpha sector of the matrix
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_alpha_plus_3")
      START_TIMER("CPXMAT:beta_alpha_plus_3")
      ENSURE(is_square_(self),"CPXMAT:beta_alpha_plus_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_alpha_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,1:n) = self(n+1:2*n,1:n) + factor*X
     STOP_TIMER("CPXMAT:beta_alpha_plus_3")
      CHECK
   end subroutine

   subroutine alpha_beta_plus(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-beta sector of the matrix
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_plus")
      START_TIMER("CPXMAT:alpha_beta_plus")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_plus ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X
      else;                      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + X
      end if
     STOP_TIMER("CPXMAT:alpha_beta_plus")
      CHECK
   end subroutine

   subroutine alpha_beta_plus_1(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-beta sector of the matrix
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_plus_1")
      START_TIMER("CPXMAT:alpha_beta_plus_1")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_plus_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X
     STOP_TIMER("CPXMAT:alpha_beta_plus_1")
      CHECK
   end subroutine

   subroutine alpha_beta_plus_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-beta sector of the matrix
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_plus_2")
      START_TIMER("CPXMAT:alpha_beta_plus_2")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_plus_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X
      else;                      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + X
      end if
     STOP_TIMER("CPXMAT:alpha_beta_plus_2")
      CHECK
   end subroutine

   subroutine alpha_beta_plus_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the alpha-beta sector of the matrix
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:alpha_beta_plus_3")
      START_TIMER("CPXMAT:alpha_beta_plus_3")
      ENSURE(is_square_(self),"CPXMAT:alpha_beta_plus_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:alpha_beta_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(1:n,n+1:2*n) = self(1:n,n+1:2*n) + factor*X
     STOP_TIMER("CPXMAT:alpha_beta_plus_3")
      CHECK
   end subroutine

   subroutine beta_beta_plus(self,X, factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-beta sector of the matrix
      CPXMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_plus")
      START_TIMER("CPXMAT:beta_beta_plus")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_plus ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_plus ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X
      else;                      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + X
      end if
     STOP_TIMER("CPXMAT:beta_beta_plus")
      CHECK
   end subroutine

   subroutine beta_beta_plus_1(self,X, factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-beta sector of the matrix
      CPXMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_plus_1")
      START_TIMER("CPXMAT:beta_beta_plus_1")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_plus_1 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_plus_1 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X
     STOP_TIMER("CPXMAT:beta_beta_plus_1")
      CHECK
   end subroutine

   subroutine beta_beta_plus_2(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-beta sector of the matrix
      REALMAT(:,:), IN :: X
      CPX, IN, optional :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_plus_2")
      START_TIMER("CPXMAT:beta_beta_plus_2")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_plus_2 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_plus_2 ... not even-dimensioned")
      n = size(self,1)/2
      if (present(factor)) then; self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X
      else;                      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + X
      end if
     STOP_TIMER("CPXMAT:beta_beta_plus_2")
      CHECK
   end subroutine

   subroutine beta_beta_plus_3(self,X,factor)
    CPXMAT(:,:) :: self
   ! Add "X" to the beta-beta sector of the matrix
      REALMAT(:,:), IN :: X
      REAL, IN :: factor
      INT :: n
      STACK("CPXMAT:beta_beta_plus_3")
      START_TIMER("CPXMAT:beta_beta_plus_3")
      ENSURE(is_square_(self),"CPXMAT:beta_beta_plus_3 ... non-square matrix")
      ENSURE(is_even_(size(self,1)),"CPXMAT:beta_beta_plus_3 ... not even-dimensioned")
      n = size(self,1)/2
      self(n+1:2*n,n+1:2*n) = self(n+1:2*n,n+1:2*n) + factor*X
     STOP_TIMER("CPXMAT:beta_beta_plus_3")
      CHECK
   end subroutine

   subroutine schmidt_orthonormalise(self,S,scale)
    CPXMAT(:,:) :: self
   ! Schmidt ortyhonormalise the column vectors in "self" using "S" as the
   ! metric.  If "scale" is present, it is set to the product of the
   ! normalisation factors used to normalise each column after the Schmidt
   ! procedure.
     target :: self
     REAL, optional :: scale
     REALMAT(:,:), IN :: S
     INT :: dim,n,o
     CPX :: fac
     REAL :: norm
     CPXVEC(:), PTR :: new,old
     STACK("CPXMAT:schmidt_orthonormalise")
     START_TIMER("CPXMAT:schmidt_orthonormalise")
     ENSURE( is_square_(self),"CPXMAT:schmidt_orthonormalise ... non-square matrix")
     ENSURE(is_square_(S),"CPXMAT:schmidt_orthonormalise ... non-square matrix")
     ENSURE(size(self,1)==size(S,1),"CPXMAT:schmidt_orthonormalise ... matrices not same size")
     ENSURE(NOT is_zero_(self),"CPXMAT:schmidt_orthonormalise ... self is zero matrix")
     if (present(scale)) scale = ONE
     dim = size(self,1)
     do n = 1,dim
        new => self(:,n)
        do o = 1,n-1
           old => self(:,o)
           fac = dot_(S,old,new)
           new = new - fac*old
        end do
        norm = dot_(S,new,new)
        ENSURE(norm>MAT_LINEAR_DEPENDENCE_TOL,"CPXMAT:schmidt_orthonormalise ... linear dependence in vector "//to_str_(n))
        norm = 1/sqrt(norm)
        new = new*norm
        if (present(scale)) scale = scale*norm
     end do
     STOP_TIMER("CPXMAT:schmidt_orthonormalise")
      CHECK
   end subroutine

   subroutine symmetrically_orthonormalise(self,S)
    CPXMAT(:,:) :: self
   ! Symmetrically orthonormalise the column vectors in "self" using "S" as the
   ! metric.
     REALMAT(:,:), IN :: S
     CPXMAT(:,:), PTR :: SS,SI
     INT :: dim
     STACK("CPXMAT:symmetrically_orthonormalise")
     START_TIMER("CPXMAT:symmetrically_orthonormalise")
     ENSURE(is_square_(self),"CPXMAT:symmetrically_orthonormalise ... non-square matrix")
     ENSURE(is_same_shape_as_(self,S),"CPXMAT:symmetrically_orthonormalise ... non-square matrix")
     dim = size(S,1)
     call create_(SS,dim,dim)
     call create_(SI,dim,dim)
     SS = S
     call change_basis_(SS,self)
     call to_inverse_sqrt_(SI,SS)
     call to_product_of_(SS,self,SI)
     self = SS
     call destroy_(SI)
     call destroy_(SS)
     STOP_TIMER("CPXMAT:symmetrically_orthonormalise")
      CHECK
   end subroutine

   subroutine to_sqrt(self,R)
    CPXMAT(:,:) :: self
   ! self = sqrt(R), cannot have R=self
      CPXMAT(:,:) :: R
      CPXMAT(:,:), PTR :: evec
      REALVEC(:), PTR :: eval
      CPXVEC(:), PTR :: veci,vecj
      INT :: d,i,j
      REAL :: temp
      STACK("CPXMAT:to_sqrt")
      START_TIMER("CPXMAT:to_sqrt")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         if (temp <= ZERO) then
           WARN("CPXMAT:to_sqrt ... non-positive eigenvalue, " // trim(to_str_(temp,"e15.8")))
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
     STOP_TIMER("CPXMAT:to_sqrt")
      CHECK
   end subroutine

   subroutine to_inverse_sqrt(self,R)
    CPXMAT(:,:) :: self
   ! self = sqrt(R)^(-1), cannot have R=self
      CPXMAT(:,:) :: R
      CPXMAT(:,:), PTR :: evec
      REALVEC(:), PTR :: eval
      CPXVEC(:), PTR :: veci,vecj
      INT :: d,i,j
      STR(STR_SIZE) :: val
      REAL :: temp
      STACK("CPXMAT:to_inverse_sqrt")
      START_TIMER("CPXMAT:to_inverse_sqrt")
      d = size(R,1)
      call create_(eval,d)
      call create_(evec,d,d)
      call solve_eigenproblem_(R,eval,evec)
      do i = 1,d
         temp = eval(i)
         val = to_str_(temp,"e15.8")
         WARN_IF(temp<=ZERO,"CPXMAT:to_inverse_sqrt ... non-positive eigenvalue, "// trim(val))
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
     STOP_TIMER("CPXMAT:to_inverse_sqrt")
      CHECK
   end subroutine

end
