!---------------------------------------------------------------------------
!
!  REALVEC: Vector operations ...
!
! Copyright (C) Dylan Jayatilaka, 1997
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
! $Id: realvec.foo,v 1.38.2.7 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REALVEC_MODULE

#  include "realvec.use"

   implicit none

#  include "macros"
#  include "realvec.int"


contains

   subroutine create(self,dim)
    REALVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
      STACK("REALVEC:create")
      START_TIMER("REALVEC:create")
      ENSURE(dim>=0,"REALVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*REAL_SIZE)
     STOP_TIMER("REALVEC:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb,ub)
    REALVEC(:) :: self
   ! Create the vector with lower bound "lb", upper bound "ub"
      PTR :: self
      INT, IN :: lb,ub
      STACK("REALVEC:create_1")
      START_TIMER("REALVEC:create_1")
      nullify(self)
      allocate(self(lb:ub))
      ADD_MEMORY((ub-lb+1)*REAL_SIZE)
   ! The following code is inherited from INTRINSICVEC
     STOP_TIMER("REALVEC:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds)
    REALVEC(:) :: self
   ! Create the vector with "bounds"
      PTR :: self
      INTVEC(2), IN :: bounds
      STACK("REALVEC:create_2")
      START_TIMER("REALVEC:create_2")
      call create_(self,bounds(1),bounds(2))
   ! The following code is inherited from OBJECTVEC
     STOP_TIMER("REALVEC:create_2")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    REALVEC(:) :: self
   ! Destroy space for object
      PTR :: self
      STACK("REALVEC:destroy")
      START_TIMER("REALVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("REALVEC:destroy") UNSTACK return; end if
      deallocate(self)
      DELETE_MEMORY(size(self)*REAL_SIZE)
     STOP_TIMER("REALVEC:destroy")
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

   subroutine create_copy(self,vec)
    REALVEC(:) :: self
   ! Create a replica copy of vec.
      REALVEC(:), IN :: vec
      PTR :: self
      STACK("REALVEC:create_copy")
      START_TIMER("REALVEC:create_copy")
      call create_(self,size(vec))
      self = vec
   ! The following code is inherited from INTRINSICVEC
     STOP_TIMER("REALVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    REALVEC(:) :: self
   ! Copy a vector "vec".
      REALVEC(:), IN :: vec
      STACK("REALVEC:copy")
      START_TIMER("REALVEC:copy")
      self = vec
     STOP_TIMER("REALVEC:copy")
      CHECK
   end subroutine

   subroutine shrink(self,dim)
    REALVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     REALVEC(:), PTR :: old
     INT :: n
     STACK("REALVEC:shrink")
     START_TIMER("REALVEC:shrink")
     ENSURE(associated(self),"REALVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"REALVEC:shrink ... dim too large")
     if (dim==size(self)) then; STOP_TIMER("REALVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       self(n) = old(n)
     end do
     call destroy_(old)
     STOP_TIMER("REALVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    REALVEC(:) :: self
   ! Expand self to dimension dim. New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     REALVEC(:), PTR :: old
     INT :: old_dim
     STACK("REALVEC:expand")
     START_TIMER("REALVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"REALVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if
     STOP_TIMER("REALVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    REALVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     REALVEC(:), IN :: v
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("REALVEC:append")
     START_TIMER("REALVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v
     STOP_TIMER("REALVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    REALVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     REAL, IN :: value
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("REALVEC:append_1")
     START_TIMER("REALVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value
     STOP_TIMER("REALVEC:append_1")
      UNSTACK
   end subroutine

   function join(self,v) result(res)
    REALVEC(:) :: self
   ! Yield a vector which is the concatenation of "self" and "v"
     REALVEC(:), IN :: v
     REALVEC(:), PTR :: res
   ! The following code is inherited from INTRINSICVEC
     INT :: dim, dim_v
     STACK("REALVEC:join")
     START_TIMER("REALVEC:join")
     dim   = size(self)
     dim_v = size(v)
     call create_(res,dim+dim_v)
     res(    1:dim      ) = self
     res(dim+1:dim+dim_v) = v
     STOP_TIMER("REALVEC:join")
      UNSTACK
   end function

   function join_1(self,v1,v2) result(res)
    REALVEC(:) :: self
   ! Yield a vector which is the concatenation of "self" and "v1" and "v2"
     REALVEC(:), IN :: v1,v2
     REALVEC(:), PTR :: res
   ! The following code is inherited from INTRINSICVEC
     INT :: dim, dim_v1, dim_v2
     STACK("REALVEC:join_1")
     START_TIMER("REALVEC:join_1")
     dim    = size(self)
     dim_v1 = size(v1)
     dim_v2 = size(v2)
     call create_(res,dim+dim_v1+dim_v2)
     res(           1:dim              ) = self
     res(dim+       1:dim+dim_v1       ) = v1
     res(dim+dim_v1+1:dim+dim_v1+dim_v2) = v2
     STOP_TIMER("REALVEC:join_1")
      UNSTACK
   end function

!*******************************************************************************

   subroutine to_product_of(self,mat,vec,transpose)
    REALVEC(:) :: self
   ! Set "self" to the product of the matrix "mat" and vector "vec". If present,
   ! "transpose" can be set to TRUE if the matrix needs to be transposed.
      REALMAT(:,:), IN :: mat
      REALVEC(:), IN :: vec
      BIN, optional :: transpose
      INT :: i,k
      BIN :: trans
      REAL :: temp
      STACK("REALVEC:to_product_of")
      START_TIMER("REALVEC:to_product_of")
      trans = FALSE
      if (present(transpose)) trans = transpose
      if (trans) then
        ENSURE(size(mat,2)==size(self),"REALVEC:to_product_of ... array dimensions do not agree")
        ENSURE(size(mat,1)==size(vec),"REALVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(1,i) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = temp
        end do
      else
        ENSURE(size(mat,1)==size(self),"REALVEC:to_product_of ... array dimensions do not agree")
        ENSURE(size(mat,2)==size(vec),"REALVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(i,1) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = temp
        end do
      end if
     STOP_TIMER("REALVEC:to_product_of")
      CHECK
   end subroutine

   subroutine to_product_of_1(self,mat_a,mat_b,transpose_a,transpose_b)
    REALVEC(:) :: self
   ! Set "self" to the matrix product of "mat_a" and "mat_b". If present,
   ! "transpose_a" and "transpose_b" can be set to TRUE if "mat_a" and "mat_b"
   ! neeb to be transposed.
     REALMAT(:,:) :: mat_a, mat_b
     BIN, optional :: transpose_a, transpose_b
     INT :: dim_a1,dim_a2,dim_b1,dim_b2,dim1,i,k,opt
     BIN :: trans_a,trans_b
     REAL :: temp

     STACK("REALVEC:to_product_of_1")
     START_TIMER("REALVEC:to_product_of_1")
     trans_a = FALSE;       trans_b = FALSE
     if (present(transpose_a)) trans_a = transpose_a
     if (present(transpose_b)) trans_b = transpose_b

     dim_a1 = size(mat_a,1);           dim_a2 = size(mat_a,2)
     dim_b1 = size(mat_b,1);           dim_b2 = size(mat_b,2)
     dim1   = size(self,1)

     opt = 0
     if (trans_a) opt = opt + 1
     if (trans_b) opt = opt + 2

     select case (opt)
       case (0)
         ENSURE(dim1==dim_a1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a2==dim_b1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b2==1,"REALVEC:to_product_of_1 ... b array dimensions incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (1)
         ENSURE(dim1==dim_a2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a1==dim_b1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b2==1,"REALVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (2)
         ENSURE(dim1==dim_a1,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a2==dim_b2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b1==1,"REALVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(1,k)
           end do
           self(i) = temp
         end do
       case (3)
         ENSURE(dim1==dim_a2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a1==dim_b2,"REALVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b1==1,"REALVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(1,k)
           end do
           self(i) = temp
         end do
     end select
     STOP_TIMER("REALVEC:to_product_of_1")
      CHECK
   end subroutine

   subroutine plus_product_of(self,mat,vec,transpose)
    REALVEC(:) :: self
   ! Add to "self" the product of the matrix and vector. If present,
   ! "transpose" can be set to TRUE if the matrix needs to be transposed.
      REALMAT(:,:), IN :: mat
      REALVEC(:), IN :: vec
      BIN, optional :: transpose
      INT :: dim_a1,dim_a2,dim_b1,dim1,i,k
      BIN :: trans
      REAL :: temp
      STACK("REALVEC:plus_product_of")
      START_TIMER("REALVEC:plus_product_of")
      trans = FALSE
      if (present(transpose)) trans = transpose
      dim_a1 = size(mat,1);   dim_a2 = size(mat,2)
      dim_b1 = size(vec,1)
      dim1   = size(self)
      if (trans) then
        ENSURE(dim1==dim_a2,"REALVEC:plus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a1,"REALVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      else
        ENSURE(dim1==dim_a1,"REALVEC:plus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a2,"REALVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(i,1) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      end if
     STOP_TIMER("REALVEC:plus_product_of")
      CHECK
   end subroutine

   subroutine minus_product_of(self,mat,vec,transpose)
    REALVEC(:) :: self
   ! Subtract from "self" the product of the matrix and vector. If present,
   ! "transpose" can be set to TRUE if the matrix needs to be transposed.
      REALMAT(:,:), IN :: mat
      REALVEC(:), IN :: vec
      BIN, optional :: transpose
      INT :: dim_a1,dim_a2,dim_b1,dim1,i,k
      BIN :: trans
      REAL :: temp
      STACK("REALVEC:minus_product_of")
      START_TIMER("REALVEC:minus_product_of")
      trans = FALSE
      if (present(transpose)) trans = transpose
      dim_a1 = size(mat,1);   dim_a2 = size(mat,2)
      dim_b1 = size(vec,1)
      dim1   = size(self)
      if (trans) then
        ENSURE(dim1==dim_a2,"REALVEC:minus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a1,"REALVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      else
        ENSURE(dim1==dim_a1,"REALVEC:minus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a2,"REALVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(i,1) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      end if
     STOP_TIMER("REALVEC:minus_product_of")
      CHECK
   end subroutine

   PURE function dot(self,b) result(res)
    REALVEC(:) :: self
   ! Return the dot product with "b"
      IN :: self
      REALVEC(:), IN :: b
      REAL :: res
   ! The following code is inherited from INTRINSICVEC
      res = dot_product(self,b)
     STOP_TIMER("REALVEC:dot")
   end function

   PURE function cross(self,b) result(res)
    REALVEC(:) :: self
   ! Return the cross product of "self" and "b".
      IN :: self
      REALVEC(:), IN :: b
      REALVEC(3) :: res
   ! The following code is inherited from INTRINSICVEC
      
      res(1) = self(2)*b(3) - b(2)*self(3)
      res(2) = self(3)*b(1) - b(3)*self(1)
      res(3) = self(1)*b(2) - b(1)*self(2)
     STOP_TIMER("REALVEC:cross")
   end function

   PURE subroutine to_cross_product(self,a,b)
    REALVEC(:) :: self
   ! Set the vector to the cross product of "a" and "b".
      INOUT :: self
      REALVEC(:), IN :: a,b
   ! The following code is inherited from INTRINSICVEC
      
      self(1) = a(2)*b(3) - b(2)*a(3)
      self(2) = a(3)*b(1) - b(3)*a(1)
      self(3) = a(1)*b(2) - b(1)*a(2)
     STOP_TIMER("REALVEC:to_cross_product")
   end subroutine

   PURE subroutine reverse_order(self)
    REALVEC(:) :: self
   ! Reverse the order of the elements of self
     INOUT :: self
   ! The following code is inherited from OBJECTVEC
     INT :: n,dim
     dim = size(self)
     do n = 1,dim/2
       call swap_elements_(self,n,dim-n+1)
     end do
     STOP_TIMER("REALVEC:reverse_order")
   end subroutine

   function equals(self,b,tol) result(res)
    REALVEC(:) :: self
   ! Return true if "self" is the same as "b", within range "tol" if supplied
      IN :: self
      REALVEC(:), IN :: b
      REAL, optional, IN :: tol
      BIN :: res
      STACK("REALVEC:equals")
      START_TIMER("REALVEC:equals")
      res = same_as_(self,b,tol)
     STOP_TIMER("REALVEC:equals")
      CHECK
   end function

   function same_as(self,b,tol) result(res)
    REALVEC(:) :: self
   ! Return true if "self" is the same as "b", within range "tol" if supplied
      IN :: self
      REALVEC(:), IN :: b
      REAL, optional, IN :: tol
      BIN :: res
      REAL :: tolerance
      REALVEC(size(b)) :: tmp
      STACK("REALVEC:same_as")
      START_TIMER("REALVEC:same_as")
      if (size(self)/=size(b)) then
         res = FALSE
      else
         tolerance = REAL_EPSILON
         if (present(tol)) tolerance = tol
         tmp = self - b
         tmp = abs(tmp)
         if (any(tmp>tolerance)) then
            res = FALSE
         else
            res = dot_product(tmp,tmp)<tolerance*tolerance
         end if
      end if
     STOP_TIMER("REALVEC:same_as")
      CHECK
   end function

   function are_all_equal(self,tol) result(res)
    REALVEC(:) :: self
   ! Return TRUE if "self" contains qall the same elements, to within precision
   ! "tol", if supplied.
      IN :: self
      REAL, optional, IN :: tol
      BIN :: res
      INT :: i
      STACK("REALVEC:are_all_equal")
      START_TIMER("REALVEC:are_all_equal")
      res = TRUE
      do i = 2,size(self)
         if (equals_(self(1),self(i),tol)) cycle
         res = FALSE
         exit
      end do
     STOP_TIMER("REALVEC:are_all_equal")
      CHECK
   end function

   PURE subroutine to_scaled_vec(self,fac,b)
    REALVEC(:) :: self
   ! Set the vector to "b" scaled by "fac"
      INOUT :: self
      REALVEC(:), IN :: b
      REAL, IN :: fac
   ! The following code is inherited from INTRINSICVEC
      self = fac*b
     STOP_TIMER("REALVEC:to_scaled_vec")
   end subroutine

   PURE subroutine plus_scaled_vec(self,fac,b)
    REALVEC(:) :: self
   ! Add a vector "b" scaled by "fac" to "self"
      INOUT :: self
      REALVEC(:), IN :: b
      REAL, IN :: fac
   ! The following code is inherited from INTRINSICVEC
      self = self + fac*b
     STOP_TIMER("REALVEC:plus_scaled_vec")
   end subroutine

   PURE subroutine minus(self,b,mask)
    REALVEC(:) :: self
   ! Subtract vector "b" from "self"
      INOUT :: self
      REALVEC(:), IN :: b
      BINVEC(:), IN, optional :: mask
   ! The following code is inherited from INTRINSICVEC
       INT :: i
      if (NOT present(mask)) then
         self = self - b
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i) - b(i)
         end do
      end if
     STOP_TIMER("REALVEC:minus")
   end subroutine

   PURE subroutine plus(self,b,mask)
    REALVEC(:) :: self
   ! Add vector "b" to "self"
      INOUT :: self
      BINVEC(:), IN, optional :: mask
      REALVEC(:), IN :: b
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      
      if (NOT present(mask)) then
         self = self + b
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i) + b(i)
         end do
      end if
     STOP_TIMER("REALVEC:plus")
   end subroutine

   PURE subroutine set_to(self,b)
    REALVEC(:) :: self
   ! Set the vector to "b". See also the "copy" routine.
      INOUT :: self
      REALVEC(:), IN :: b
   ! The following code is inherited from INTRINSICVEC
      self = b
     STOP_TIMER("REALVEC:set_to")
   end subroutine

   PURE subroutine swap_elements(self,e1,e2)
    REALVEC(:) :: self
   ! Swap elements "e1" and "e2" in "self".
      INOUT :: self
      INT, IN :: e1,e2
   ! The following code is inherited from INTRINSICVEC
      REAL :: val
      
      
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
     STOP_TIMER("REALVEC:swap_elements")
   end subroutine

   PURE function index_of_first_nonzero_value(self,eps) result(res)
    REALVEC(:) :: self
   ! Returns the index of the first nonzero component of self.
     IN :: self
     REAL, IN, optional :: eps
     INT :: res
     INT :: i
     res=0
     do i=1,size(self)
       if (NOT is_zero_(self(i),eps)) then
         res=i
         exit
       end if
     end do
     STOP_TIMER("REALVEC:index_of_first_nonzero_value")
   end function

   function index_of_first_zero_value(self) result(res)
    REALVEC(:) :: self
   ! Returns the index of the first zero component of self.
     IN :: self
     INT :: res
     INT :: i
     STACK("REALVEC:index_of_first_zero_value")
     START_TIMER("REALVEC:index_of_first_zero_value")
     res=0
     do i=1,size(self)
       if (is_zero_(self(i))) then
         res=i
         exit
       end if
     end do
     STOP_TIMER("REALVEC:index_of_first_zero_value")
      CHECK
   end function

   PURE function index_of_value(self,val,eps) result(pos)
    REALVEC(:) :: self
   ! Returns the first index in "self" which has the value "val", or 0 if "val"
   ! is not present in the array. If present, "eps" is used to test equality
   ! with "val".
     IN :: self
     REAL, IN :: val
     REAL, optional, IN :: eps
     INT :: pos
     INT :: i
     pos = 0
     do i = 1,size(self)
        if (same_as_(self(i),val,eps)) then
           pos = i
           exit
        end if
     end do
     STOP_TIMER("REALVEC:index_of_value")
   end function

   PURE subroutine chop_large_values(self,val)
    REALVEC(:) :: self
   ! Set all values in the self whose absolute value is larger than "val" to
   ! "val" times the sign of the original value.
      INOUT :: self
      REAL, IN :: val
   ! The following code is inherited from INTRINSICVEC
      INT :: dim,i
      REAL :: bb,ba,sign
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         if (bb==ZERO) cycle
         ba = abs(bb)
         sign = bb/ba
         self(i) = sign*min(val,ba)
     end do
     STOP_TIMER("REALVEC:chop_large_values")
   end subroutine

!   maximum result (val) ::: pure
!   ! Return the maximum value in the vector
!      self :: IN
!      val :: ELEMENT_TYPE
!      val = maxval(self)
!   end

!   minimum result (val) ::: pure
!   ! Return the minimum value in the vector
!      self :: IN
!      val :: ELEMENT_TYPE
!      val = minval(self)
!   end

   PURE function index_of_maximum(self) result(ind)
    REALVEC(:) :: self
   ! Return the index of the maximum in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      ind = maxval(maxloc(self))
     STOP_TIMER("REALVEC:index_of_maximum")
   end function

   PURE function index_of_minimum(self) result(ind)
    REALVEC(:) :: self
   ! Return the index of the minimum in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      ind = minval(minloc(self))
     STOP_TIMER("REALVEC:index_of_minimum")
   end function

   PURE function is_zero(self,eps) result(res)
    REALVEC(:) :: self
   ! Return true if the vector is zero (within "eps", if supplied)
      IN :: self
      REAL, IN, optional :: eps
      BIN :: res
      REAL :: val
      val = norm_(self)
      res = is_zero_(val,eps)
     STOP_TIMER("REALVEC:is_zero")
   end function

   PURE function all_in_range(self,range) result(res)
    REALVEC(:) :: self
   ! Return TRUE if all values of self are within the specified "range".
      IN :: self
      REALVEC(2), IN :: range
      BIN :: res
      res = all(range(1) <= self AND self <= range(2))
   ! The following code is inherited from INTRINSICVEC
     STOP_TIMER("REALVEC:all_in_range")
   end function

   PURE function in_range(self,range) result(res)
    REALVEC(:) :: self
   ! Return element i as TRUE if self(i) is within the specified "range".
      IN :: self
      REALVEC(2), IN :: range
      BINVEC(size(self)) :: res
      res = (range(1) <= self AND self <= range(2))
   ! The following code is inherited from INTRINSICVEC
     STOP_TIMER("REALVEC:in_range")
   end function

   PURE function range(self) result(res)
    REALVEC(:) :: self
   ! Return the range (smallest and largest value) of self.
      IN :: self
      REALVEC(2) :: res
      res(1) = minval(self)
      res(2) = maxval(self)
   ! The following code is inherited from INTRINSICVEC
     STOP_TIMER("REALVEC:range")
   end function

   function is_z_axis(self,eps) result(res)
    REALVEC(:) :: self
   ! Return true if the vector is set to the z-axis (within "eps", if supplied)
      REAL, optional :: eps
      BIN :: res
      STACK("REALVEC:is_z_axis")
      START_TIMER("REALVEC:is_z_axis")
      ENSURE(size(self)==3,"REALVEC:is_z_axis ... must supply a 3 dimensional vector!")
      res = is_zero_(self(1),eps)
      res = is_zero_(self(2),eps) AND res
      res = is_zero_((ONE-self(3)),eps) AND res
     STOP_TIMER("REALVEC:is_z_axis")
      CHECK
   end function

   PURE function largest_value(self) result(maxval)
    REALVEC(:) :: self
   ! Return the maximum absolute value in the vector
      IN :: self
      REAL :: maxval
      REAL :: val
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      maxval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) maxval = val
      end do
     STOP_TIMER("REALVEC:largest_value")
   end function

   PURE function smallest_value(self) result(minval)
    REALVEC(:) :: self
   ! Return minimum absolute value in the vector
      IN :: self
      REAL :: minval
      REAL :: val
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      minval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) minval = val
      end do
     STOP_TIMER("REALVEC:smallest_value")
   end function

   PURE function index_of_largest_value(self) result(ind)
    REALVEC(:) :: self
   ! Return the index "ind" of the largest absolute value in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      REAL :: maxval,val
      maxval = abs(self(1))
      ind = 1
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) then
          maxval = val
          ind = i
        end if
      end do
     STOP_TIMER("REALVEC:index_of_largest_value")
   end function

   PURE function index_of_smallest_value(self) result(ind)
    REALVEC(:) :: self
   ! Return the index "ind" of the smallest value in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      REAL :: minval,val
      minval = abs(self(1))
      ind = 1
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) then
          minval = val
          ind = i
        end if
      end do
     STOP_TIMER("REALVEC:index_of_smallest_value")
   end function

   PURE function no_of_elements_larger_than(self,tol) result(res)
    REALVEC(:) :: self
   ! Return the number of elements larger than "tol".
      IN :: self
      REAL, IN :: tol
      INT :: res
   ! The following code is inherited from INTRINSICVEC
      res = count(self>tol)
     STOP_TIMER("REALVEC:no_of_elements_larger_than")
   end function

   PURE subroutine normalising_factors(self,l)
    REALVEC(:) :: self
   ! Return the normalising factors for cartesian gaussian basis functions used
   ! in quantum chemical calculations.
     INT, IN :: l
     OUT :: self
     INTMAT(:,:), allocatable :: xyz_momenta
     REAL :: fac
     INT :: i,ncomp
     
     
     if (l<2) then
       self = ONE
     else if (l==2) then
       self(1)=ONE
       self(2)=ONE
       self(3)=ONE
       self(4)=sqrt(THREE)
       self(5)=sqrt(THREE)
       self(6)=sqrt(THREE)
     else
       ncomp=n_comp_(l)
       allocate(xyz_momenta(3,ncomp))
       call make_gaussian_xyz_powers_(l,xyz_momenta)
       do i=1,ncomp
         fac = double_factorial_(l) / &
               (double_factorial_(xyz_momenta(1,i))   &
               *double_factorial_(xyz_momenta(2,i))   &
               *double_factorial_(xyz_momenta(3,i)))
         self(i)=sqrt(fac)
       end do
       deallocate(xyz_momenta)
     end if
     STOP_TIMER("REALVEC:normalising_factors")
   end subroutine

   PURE function norm(self) result(res)
    REALVEC(:) :: self
   ! Return the norm of the vector
      IN :: self
      REAL :: res
      res = sqrt(sum(self*self))
     STOP_TIMER("REALVEC:norm")
   end function

   subroutine normalise(self)
    REALVEC(:) :: self
   ! Normalise the vector
      INOUT :: self
      STACK("REALVEC:normalise")
      START_TIMER("REALVEC:normalise")
      self = self/norm_(self)
     STOP_TIMER("REALVEC:normalise")
      CHECK
   end subroutine

   function distance_to(self,b) result(res)
    REALVEC(:) :: self
   ! Return the distance to "b" if supplied
      REALVEC(:), IN :: b
      REAL :: res
      REAL :: temp
      INT :: i
      STACK("REALVEC:distance_to")
      START_TIMER("REALVEC:distance_to")
      ENSURE(size(self)==size(b),"REALVEC:distance_to ... incompatible dimensions")
      res = ZERO
      do i = 1,size(self)
         temp = self(i)-b(i)
         res = res + temp*temp
      end do
      res = sqrt(res)
     STOP_TIMER("REALVEC:distance_to")
      CHECK
   end function

   PURE subroutine zero_small_values(self,tol)
    REALVEC(:) :: self
   ! Zero elements of the vector which are less than "tol" in magnitude
      INOUT :: self
      REAL, IN :: tol
      where (abs(self)<tol)
        self = ZERO
      end where
     STOP_TIMER("REALVEC:zero_small_values")
   end subroutine

   PURE function mean(self) result(res)
    REALVEC(:) :: self
   ! Return the mean of the vector
     IN :: self
     REAL :: res
     res = sum(self)/size(self)
     STOP_TIMER("REALVEC:mean")
   end function

   PURE function variance(self) result(res)
    REALVEC(:) :: self
   ! Return the variance of the vector from its mean
     IN :: self
     REAL :: res
     REAL :: mean
     mean = mean_(self)
     res = (sum(self*self))/size(self)-mean*mean
     STOP_TIMER("REALVEC:variance")
   end function

   PURE function standard_deviation(self) result(res)
    REALVEC(:) :: self
   ! Return the standard deviation of the vector from its mean
     IN :: self
     REAL :: res
     REAL :: variance
     variance = variance_(self)
     if (variance/=ZERO) res = sqrt(variance)
     STOP_TIMER("REALVEC:standard_deviation")
   end function

   function arcsinh(self) result(res)
    REALVEC(:) :: self
   ! Return the arcsinh of self, where self is a vector of any real numbers.
      IN :: self
      REALVEC(size(self)) :: res
      STACK("REALVEC:arcsinh")
      START_TIMER("REALVEC:arcsinh")
      res = log(self + sqrt(ONE+self*self))
     STOP_TIMER("REALVEC:arcsinh")
      CHECK
   end function

   function alpha(self) result(res)
    REALVEC(:) :: self
   ! return the alpha sector of the vector
      TARGET :: self
      REALVEC(:), PTR :: res
      INT :: n
      STACK("REALVEC:alpha")
      START_TIMER("REALVEC:alpha")
      ENSURE(is_even_(size(self)),"REALVEC:alpha ... self is not even-dimensioned")
      n = size(self)/2
      res => self(1:n)
     STOP_TIMER("REALVEC:alpha")
      CHECK
   end function

   function beta(self) result(res)
    REALVEC(:) :: self
   ! return the beta sector of the vector
      TARGET :: self
      REALVEC(:), PTR :: res
      INT :: n
      STACK("REALVEC:beta")
      START_TIMER("REALVEC:beta")
      ENSURE(is_even_(size(self)),"REALVEC:beta ... self is not even-dimensioned")
      n = size(self)/2
      res => self(n+1:2*n)
     STOP_TIMER("REALVEC:beta")
      CHECK
   end function

   subroutine set_alpha(self,X)
    REALVEC(:) :: self
   ! Set the alpha sector of the vector
      REALVEC(:) :: X
      INT :: n
      STACK("REALVEC:set_alpha")
      START_TIMER("REALVEC:set_alpha")
      ENSURE(is_even_(size(self)),"REALVEC:set_alpha ... self is not even-dimensioned")
      n = size(self)/2
      self(1:n) = X
     STOP_TIMER("REALVEC:set_alpha")
      CHECK
   end subroutine

   subroutine set_beta(self,X)
    REALVEC(:) :: self
   ! Set the beta sector of the vector
      REALVEC(:) :: X
      INT :: n
      STACK("REALVEC:set_beta")
      START_TIMER("REALVEC:set_beta")
      ENSURE(is_even_(size(self)),"REALVEC:set_beta ... self is not even-dimensioned")
      n = size(self)/2
      self(n+1:2*n) = X
     STOP_TIMER("REALVEC:set_beta")
      CHECK
   end subroutine

!   integrate(a,b,accuracy) result(res) ::: recursive, functional
!   ! Integrate the vector valued scalar function "self" between the limits
!   ! "a" and "b" using adaptive trapezoidal rule with Simpsons approximation.
!   ! If present, "accuracy" is the required accuracy of the integral.
!      interface
!         self(x) result(res)
!             x :: REAL
!            res :: SELF_TYPE*
!         end
!      end
!      a,b :: REAL
!      accuracy :: REAL, optional
!      res :: SELF_TYPE*
!       n :: INT
!      same :: BIN
!      tol,h,m :: REAL
!      fa,fb,fm,one_trap,two_trap,left,right :: SELF_TYPE,PTR
!      tol = TOL(6)
!      if (present(accuracy)) tol = accuracy
!      h  = b-a
!      m  = (a+b)/TWO
!      fa => self(a)
!      fb => self(b)
!      fm => self(m)
!      n = size(fa)
!      one_trap.create(n)
!      two_trap.create(n)
!      one_trap = h*(fa+fb)/TWO
!      two_trap = h*(fa+TWO*fm+fb)/FOUR
!      fm.destroy
!      fb.destroy
!      fa.destroy
!      res.create(n)
!      res = abs(one_trap-two_trap)
!      same = maxval(res) < THREE*tol
!      if (same) then
!         res = (FOUR*two_trap - one_trap)/THREE
!         two_trap.destroy
!         one_trap.destroy
!      else
!         two_trap.destroy
!         one_trap.destroy
!         left  => .integrate(a,m,tol/TWO)
!         right => .integrate(m,b,tol/TWO)
!         res = left + right
!         right.destroy
!         left.destroy
!      end
!   end

   subroutine seitz_multiply(self,seitz)
    REALVEC(:) :: self
   ! Self is operated on by the seitz matrix.  Self must be in fractional
   !  coordinates.
     INOUT :: self
     REALMAT(:,:), IN :: seitz
     STACK("REALVEC:seitz_multiply")
     START_TIMER("REALVEC:seitz_multiply")
     ENSURE(size(seitz,1)==4,"REALVEC:seitz_multiply ... seitz matrix must be 4x4")
     ENSURE(size(seitz,2)==4,"REALVEC:seitz_multiply ... seitz matrix must be 4x4")
     ENSURE(size(self)==3,"REALVEC:seitz_multiply ... vector not of dimension 3")
     call rotate_(self,seitz(1:3,1:3))
     call translate_(self,seitz(4,1:3))
     STOP_TIMER("REALVEC:seitz_multiply")
      CHECK
   end subroutine

   subroutine translate(self,vector)
    REALVEC(:) :: self
   ! Translate self by vector.
     INOUT :: self
     REALVEC(:), IN :: vector
   ! The following code is inherited from INTRINSICVEC
     STACK("REALVEC:translate")
     START_TIMER("REALVEC:translate")
     ENSURE(size(self)==size(vector),"REALVEC:translate ... vectors not of same dimension")
     self = self + vector
     STOP_TIMER("REALVEC:translate")
      CHECK
   end subroutine

   subroutine rotate(self,matrix)
    REALVEC(:) :: self
   ! Rotate self by the rotation matrix
     INOUT :: self
     REALMAT(:,:), IN :: matrix
     STACK("REALVEC:rotate")
     START_TIMER("REALVEC:rotate")
     ENSURE(size(matrix,1)==size(matrix,2),"REALVEC:rotate ... matrix must be square")
     ENSURE(size(matrix,2)==size(self),"REALVEC:rotate ... matrix and vector dimensions inconsistent")
     self = matmul(matrix,self)
     STOP_TIMER("REALVEC:rotate")
      CHECK
   end subroutine

   subroutine rotate_by(self,matrix)
    REALVEC(:) :: self
   ! Rotate self by the rotation matrix, treating self as a column vector
     INOUT :: self
     REALMAT(:,:), IN :: matrix
     STACK("REALVEC:rotate_by")
     START_TIMER("REALVEC:rotate_by")
     ENSURE(size(matrix,1)==size(matrix,2),"REALVEC:rotate_by ... matrix must be square")
     ENSURE(size(matrix,2)==size(self),"REALVEC:rotate_by ... matrix and vector dimensions inconsistent")
     self = matmul(matrix,self)
     STOP_TIMER("REALVEC:rotate_by")
      CHECK
   end subroutine

   function outer_product(self,b) result(res)
    REALVEC(:) :: self
   ! Returns the outer product of self with b.
     IN :: self
     REALVEC(:), IN :: b
     REALMAT(size(b),size(self)) :: res
     STACK("REALVEC:outer_product")
     START_TIMER("REALVEC:outer_product")
     res = spread(self,2,size(b)) * spread(b,1,size(self))
     STOP_TIMER("REALVEC:outer_product")
      CHECK
   end function

   PURE subroutine sort(self,decreasing_order)
    REALVEC(:) :: self
   ! Sort array "self" from lowest to highest, using simple insertion sort.  If
   ! "decreasing_order" is present and TRUE sort from highest to lowest instead.
     INOUT :: self
     BIN, IN, optional :: decreasing_order
   ! The following code is inherited from OBJECTVEC
     INT :: i,j,n
     BIN :: lowest_first
     lowest_first = TRUE
     if (present(decreasing_order)) lowest_first = NOT decreasing_order
     n = size(self)
     if (lowest_first) then
       do i=1,n-1
       do j=i+1,n
          if (self(j) < self(i)) call swap_elements_(self,i,j)
       end do
       end do
     else
       do i=1,n-1
       do j=i+1,n
          if (self(j) > self(i)) call swap_elements_(self,i,j)
       end do
       end do
     end if
     STOP_TIMER("REALVEC:sort")
   end subroutine

   subroutine quick_sort(self,decreasing_order)
    REALVEC(:) :: self
   ! Sort the vector into increasing order.If "decreasing_order" is present and
   ! TRUE, the vector is sorted from largest to smallest
      IN :: self
      BIN, optional, IN :: decreasing_order
   ! The following code is inherited from OBJECTVEC
      BIN :: decreasing
      STACK("REALVEC:quick_sort")
      START_TIMER("REALVEC:quick_sort")
      decreasing = FALSE
      if (present(decreasing_order)) decreasing = decreasing_order
      if (NOT decreasing) then; call quick_sort_increasing_(self)
      else;                     call quick_sort_decreasing_(self)
      end if
     STOP_TIMER("REALVEC:quick_sort")
      CHECK
   end subroutine

   recursive subroutine quick_sort_increasing(self)
    REALVEC(:) :: self
   ! Sort the vector into order, smallest to largest
   ! The following code is inherited from OBJECTVEC
      REALVEC(:), PTR :: smaller,larger
      INT :: n, ns, ne, nl
      REAL :: chosen
      STACK("REALVEC:quick_sort_increasing")
      START_TIMER("REALVEC:quick_sort_increasing")
      if (size(self)<=1) then; STOP_TIMER("REALVEC:quick_sort_increasing") CHECK return; end if
      n = size(self)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n - ns - nl
      call create_(smaller,ns)
      call create_(larger,nl)
      smaller = pack(self,self<chosen)
      larger  = pack(self,self>chosen)
      call quick_sort_(smaller)
      call quick_sort_(larger)
      self(1:ns)       = smaller
      self(ns+1:ns+ne) = chosen
      self(ns+ne+1:)   = larger
      call destroy_(larger)
      call destroy_(smaller)
     STOP_TIMER("REALVEC:quick_sort_increasing")
      CHECK
   end subroutine

   recursive subroutine quick_sort_decreasing(self)
    REALVEC(:) :: self
   ! Sort the vector into order, largest to smallest
   ! The following code is inherited from OBJECTVEC
      REALVEC(:), PTR :: smaller,larger
      INT :: n, ns, ne, nl
      REAL :: chosen
      STACK("REALVEC:quick_sort_decreasing")
      START_TIMER("REALVEC:quick_sort_decreasing")
      if (size(self)<=1) then; STOP_TIMER("REALVEC:quick_sort_decreasing") CHECK return; end if
      n = size(self)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n - ns - nl
      call create_(smaller,ns)
      call create_(larger,nl)
      smaller = pack(self,self<chosen)
      larger  = pack(self,self>chosen)
      call quick_sort_(smaller)
      call quick_sort_(larger)
      self(1:nl)       = larger
      self(nl+1:nl+ne) = chosen
      self(nl+ne+1:)   = smaller
      call destroy_(larger)
      call destroy_(smaller)
     STOP_TIMER("REALVEC:quick_sort_decreasing")
      CHECK
   end subroutine

   subroutine quick_sort_1(self,indices,decreasing_order)
    REALVEC(:) :: self
   ! Return the "indices" which sort self from smallest to largest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
   ! If "decreasing_order" is present and TRUE, the indices are sorted from
   ! largest to smallest
      IN :: self
      INTVEC(:), INOUT :: indices
      BIN, optional, IN :: decreasing_order
   ! The following code is inherited from OBJECTVEC
      BIN :: decreasing
      INT :: i
      STACK("REALVEC:quick_sort_1")
      START_TIMER("REALVEC:quick_sort_1")
      ENSURE(size(indices)==size(self),"REALVEC:quick_sort_1 ... wrong size, indices")
      decreasing = FALSE
      if (present(decreasing_order)) decreasing = decreasing_order
      indices = (/(i,i=1,size(self))/) ! initialise indices
      if (NOT decreasing) then; call quick_sort_increasing_(self,indices)
      else;                     call quick_sort_decreasing_(self,indices)
      end if
     STOP_TIMER("REALVEC:quick_sort_1")
      CHECK
   end subroutine

   recursive subroutine quick_sort_increasing_1(self,indices)
    REALVEC(:) :: self
   ! Return the indices which sort vector from smallest to largest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      IN :: self
      INTVEC(:), INOUT :: indices
   ! The following code is inherited from OBJECTVEC
      INTVEC(:), PTR :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n, i, ns, ne, nl
      REAL :: chosen
      STACK("REALVEC:quick_sort_increasing_1")
      START_TIMER("REALVEC:quick_sort_increasing_1")
      if (size(indices)<=1) then; STOP_TIMER("REALVEC:quick_sort_increasing_1") CHECK return; end if
      n = size(indices)
      call create_(list,n); list = (/(i,i=1,n)/)
      chosen = self(1)
      ns = count(self<chosen)
      nl = count(self>chosen)
      ne = n - ns - nl
      call create_(small,ns); call create_(small_indices,ns)
      call create_(equal,ne); call create_(equal_indices,ne)
      call create_(large,nl); call create_(large_indices,nl)
      small = pack(list,self <chosen) ! indices of small self elements
      equal = pack(list,self==chosen) ! indices of equal self elements
      large = pack(list,self >chosen) ! indices of large self elements
      small_indices = indices(small)
      equal_indices = indices(equal)
      large_indices = indices(large)
      if (ns>1) call quick_sort_increasing_(self(small),small_indices)
      if (nl>1) call quick_sort_increasing_(self(large),large_indices)
      indices(1:ns)       = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:)   = large_indices
      call destroy_(large_indices); call destroy_(large)
      call destroy_(equal_indices); call destroy_(equal)
      call destroy_(small_indices); call destroy_(small)
      call destroy_(list)
     STOP_TIMER("REALVEC:quick_sort_increasing_1")
      CHECK
   end subroutine

   recursive subroutine quick_sort_decreasing_1(self,indices)
    REALVEC(:) :: self
   ! Return the indices which sort vector from largest to smallest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      IN :: self
      INTVEC(:), INOUT :: indices
   ! The following code is inherited from OBJECTVEC
      INTVEC(:), PTR :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n, i, ns, ne, nl
      REAL :: chosen
      STACK("REALVEC:quick_sort_decreasing_1")
      START_TIMER("REALVEC:quick_sort_decreasing_1")
      if (size(indices)<=1) then; STOP_TIMER("REALVEC:quick_sort_decreasing_1") CHECK return; end if
      n = size(indices)
      call create_(list,n); list = (/(i,i=1,n)/)
      chosen = self(1)
      ns = count(self>chosen)
      nl = count(self<chosen)
      ne = n - ns - nl
      call create_(small,ns); call create_(small_indices,ns)
      call create_(equal,ne); call create_(equal_indices,ne)
      call create_(large,nl); call create_(large_indices,nl)
      small = pack(list,self >chosen) ! indices of large self elements
      equal = pack(list,self==chosen) ! indices of equal self elements
      large = pack(list,self <chosen) ! indices of small self elements
      small_indices = indices(small)
      equal_indices = indices(equal)
      large_indices = indices(large)
      if (ns>1) call quick_sort_decreasing_(self(small),small_indices)
      if (nl>1) call quick_sort_decreasing_(self(large),large_indices)
      indices(1:ns)       = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:)   = large_indices
      call destroy_(large_indices); call destroy_(large)
      call destroy_(equal_indices); call destroy_(equal)
      call destroy_(small_indices); call destroy_(small)
      call destroy_(list)
     STOP_TIMER("REALVEC:quick_sort_decreasing_1")
      CHECK
   end subroutine

   subroutine find_opposite_pairs(self,pair,min,max)
    REALVEC(:) :: self
   ! "pair(i)" returns the location of the element which has the opposite
   ! value to self(i) *and* is also negative, i.e. self(pair(i)) = -self(i) < 0.
   ! If no such match can be found, pair(i) is set to 0.
   ! unless self(i) is greater than max, in which case pair(i) is set to -1.
      INTVEC(:) :: pair
      REAL :: min,max
      INT :: i,j
      STACK("REALVEC:find_opposite_pairs")
      START_TIMER("REALVEC:find_opposite_pairs")
      pair = 0
      do i = 1,size(self)
         if      (abs(self(i))<min) then; pair(i) =  0
         else if (abs(self(i))>max) then
             if (self(i)<0)  pair(i) = -1
             if (self(i)>0)  pair(i) = -2
         else
            do j = 1,i-1
               if (any(pair==j)) cycle
               if (abs(self(j)+self(i))<min) then
                  if (self(i)>0) then; pair(i)=j
                  else;                pair(i)=0
                  end if
               end if
            end do
         end if
      end do
     STOP_TIMER("REALVEC:find_opposite_pairs")
      CHECK
   end subroutine

   subroutine find_pairs(self,pair,match_function,tol)
    REALVEC(:) :: self
   ! Given a vector "self" find pairs of values self(i) and self(pair(i))
   ! where: match_function(self(i),self(pair(i))) = 0. If no such pair can be
   ! found, then pair(i) is set to 0. If more than one match is found then the
   ! first match from the start of the list is the one that is paired.
      INTVEC(:) :: pair
      interface
         function match_function(arg1,arg2) result(res)
            REAL :: arg1,arg2,res
         end function
      end interface
      REAL, optional :: tol
      INT :: i,j,dim
      REAL :: val
      STACK("REALVEC:find_pairs")
      START_TIMER("REALVEC:find_pairs")
      dim = size(self)
      pair = 0
      do i = 1,dim
      do j = 1,dim
         val = match_function(self(i),self(j))
         if (is_zero_(val,tol) AND self(i)>self(j) AND NOT any(pair==j)) then
            pair(i) = j
            pair(j) = i
            exit
         end if
      end do
      end do
     STOP_TIMER("REALVEC:find_pairs")
      CHECK
   end subroutine

   subroutine convert_to(self,units)
    REALVEC(:) :: self
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      INOUT :: self
      STR(*), IN :: units
      REAL :: factor
   STACK("REALVEC:convert_to")
   START_TIMER("REALVEC:convert_to")
   ENSURE(is_known_unit_(units),"REALVEC:convert_to ... unknown units, " // units)
      factor = conversion_factor_(units)
      self = self * factor
     STOP_TIMER("REALVEC:convert_to")
      CHECK
   end subroutine

   subroutine convert_from(self,units)
    REALVEC(:) :: self
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      INOUT :: self
      STR(*), IN :: units
      REAL :: factor
   STACK("REALVEC:convert_from")
   START_TIMER("REALVEC:convert_from")
   ENSURE(is_known_unit_(units),"REALVEC:convert_from ... unknown units, " // units)
      factor = ONE/(conversion_factor_(units))
      self = self * factor
     STOP_TIMER("REALVEC:convert_from")
      CHECK
   end subroutine

   function to_str(self,format,separator) result(string)
    REALVEC(:) :: self
   ! Change self to a "string" using default format.
     STR(*), optional :: format
     STR(*), optional :: separator
     STR(STR_SIZE) :: string
     STR(STR_SIZE) :: str1,str2
     INT :: n
     STACK("REALVEC:to_str")
     START_TIMER("REALVEC:to_str")
     string = " "
     do n = 1,size(self)
       str1 = to_str_(self(n),format)
       ENSURE(len_trim(string) + len_trim(str1) < len(string),"REALVEC:to_str ... string too long")
       if (present(separator)) then; str2 = trim(string) // separator // trim(str1)
       else;                         str2 = trim(string) //    " "    // trim(str1)
       end if
       string = str2
     end do
     STOP_TIMER("REALVEC:to_str")
      CHECK
   end function

! *********************
! Root finding routines
! *********************

   subroutine bracket_root(self,z,direction,x1,x2,factor,val,max_it)
   ! Given a vector function self(z), an initial point "z" and a "direction",
   ! and initial distances "x1" and "x2" along "direction" from "z", bracket
   ! a root of self along "direction" by expansion. If "factor" is present it is
   ! used as the (linear) interval expansion factor. If "val" is present the
   ! values "x1" and "x2" will bracket the value x where self(x) = val.
   ! If "max_it" is present then it is the number of times the interval is
   ! expanded.
      interface
         function self(z) result(res)
             REALVEC(:) :: z
            REAL :: res
         end function
      end interface
      REALVEC(:) :: z,direction
      REAL :: x1,x2
      REAL, optional :: factor,val
      INT, optional :: max_it
      INT :: j
      REAL :: f1,f2
      REAL :: fac = 1.6d0
      REAL :: iso = 0.0d0
      INT :: maxit = 500
      STACK("REALVEC:bracket_root")
      START_TIMER("REALVEC:bracket_root")
      ENSURE(x1/=x2,"REALVEC:bracket_root ... non-zero range (x1,x2) required")
      if (present(factor)) fac = factor
      if (present(val))    iso = val
      if (present(max_it)) maxit = max_it
      if (x1>x2) call swap_with_(x1,x2)
      f1 = self(z + x1*direction) - iso
      f2 = self(z + x2*direction) - iso
      do j = 1,maxit
         if (f1*f2<ZERO) then; STOP_TIMER("REALVEC:bracket_root") CHECK return; end if
         if (abs(f1)<abs(f2)) then
            x1 = x1 + fac*(x1-x2)
            f1 = self(z + x1*direction) - iso
         else
            x2 = x2 + fac*(x2-x1)
            f2 = self(z + x2*direction) - iso
         end if
      end do
      DIE("REALVEC:bracket_root ... Exceeded maximum number of iterations")
     STOP_TIMER("REALVEC:bracket_root")
      CHECK
   end subroutine

   subroutine find_root_brent(self,z,direction,x1,x2,tol,root,val,max_it)
   ! Given a vector function self(x), an initial point "z", a "direction", and
   ! initial values "x1" and "x2" along this "direction" which bracket a root
   ! of self, return the "root" of self along "direction" to a precision "tol"
   ! using Brent's method. The point "z" is reset to be the vector point
   ! corresponding to this root. If "val" present, then root is set so that
   ! self(root) = val, i.e.  root is set top be an iso-value of self. If
   ! "max_it" is present it is set to the maximum number of iterations.
      interface
         function self(z) result(res)
             REALVEC(:) :: z
            REAL :: res
         end function
      end interface
      REALVEC(:) :: z,direction
      REAL :: x1,x2,tol
      REAL, optional :: root,val
      INT, optional :: max_it
      INT :: iter
      REAL :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      INT :: maxit = 100
      REAL :: iso = ZERO
      REAL :: eps = TOL(8)
      STACK("REALVEC:find_root_brent")
      START_TIMER("REALVEC:find_root_brent")
      if (present(max_it)) maxit = max_it
      if (present(val))    iso = val
      a  = x1
      b  = x2
      fa = self(z + a*direction) - iso
      fb = self(z + b*direction) - iso
      ENSURE((fa>0 AND fb<0) OR (fa<0 AND fb>0),"REALVEC:find_root_brent ... root is not bracketed")
      c = b
      fc = fb
      do iter = 1,maxit
         if ((fb>0 AND fc>0) OR (fb<0 AND fc<0)) then
            c = a    ! Rename a,b,c and adjust bounding interval d
            fc = fa
            d = b - a
            e = d
         end if
         if (abs(fc)<abs(fb)) then
            a  = b ; b  = c ; c  = a
            fa = fb; fb = fc; fc = fa
         end if
         ! Test convergence
         tol1 = TWO*eps*abs(b)+HALF*tol
         xm = HALF*(c-b)
         if (abs(xm)<=tol1 OR fb==ZERO) then
            if (present(root)) root = b
            z = z + b*direction
            STOP_TIMER("REALVEC:find_root_brent") CHECK return
         end if
         if (abs(e)>=tol1 AND abs(fa)>abs(fb)) then
            s = fb/fa ! Attempt inverse quadratic interpolation
            if (a==c) then
               p = TWO*xm*s
               q = ONE - s
            else
               q = fa/fc
               r = fb/fc
               p = s*(TWO*xm*q*(q-r)-(b-a)*(r-ONE))
               q = (q-ONE)*(r-ONE)*(s-ONE)
            end if
            if (p>ZERO) q = -q
            p = abs(p)
            if (TWO*p<min(THREE*xm*q-abs(tol1*q),abs(e*q))) then
               e = d  ! Accept interpolation
               d = p/q
            else
               d = xm ! Interpolation failed, use bisection
               e = d
            end if
         else
            d = xm    ! Bounds decreasing too slowly, use bisection
            e = d
         end if
         a = b; fa = fb
         if (abs(d)> tol1) then; b = b + d
         else;                   b = b+sign(tol1,xm)
         end if
         fb = self(z + b*direction) - iso
      end do
      DIE("REALVEC:find_root_brent ... maximum iterations exceeded")
     STOP_TIMER("REALVEC:find_root_brent")
      CHECK
   end subroutine

   subroutine find_isopoint(self,z,direction,isovalue,tol,x1,x2)
   ! Given an initial point "z" and a "direction" to search along, return the
   ! isopoint "z" where the function "self" has value "isovalue", i.e. where
   ! self(p) = isovalue. "tol" is the accuracy to which "z" is determined.
   ! If present, "x1" and "x2" are an initial interval along "direction" from
   ! "p" where the isopoint is suspected to lie.
      REALVEC(:) :: z,direction
      interface
         function self(z) result(res)
            REALVEC(:) :: z
            REAL :: res
         end function
      end interface
      REAL :: isovalue,tol
      REAL, optional :: x1,x2
      REAL :: y1,y2
      STACK("REALVEC:find_isopoint")
      START_TIMER("REALVEC:find_isopoint")
      y1 = ZERO; if (present(x1)) y1 = x1
      y2 = ONE;  if (present(x2)) y2 = x2
      call bracket_root_(self,z,direction,y1,y2,val=isovalue)
      call find_root_brent_(self,z,direction,y1,y2,tol,val=isovalue)
     STOP_TIMER("REALVEC:find_isopoint")
      CHECK
   end subroutine

! *********************
! Minimisation routines
! *********************

   subroutine minimise(self,new_direction,gradient,old_val,old_gradient,hessian)
    REALVEC(:) :: self
   ! Move the vector "self" to a position closer to the minimum, based on the
   ! gradient.  Uses the BFGS scheme.  Doesn't check for convergence, just does
   ! one iteration.
     REALVEC(:), OUT :: new_direction
     REALVEC(:), IN :: gradient
     REALVEC(:), INOUT :: old_val, old_gradient
     REALMAT(:,:), INOUT :: hessian
     REALVEC(:), PTR :: dx,dg,hdg,u
     REAL :: dxdg,dghdg,dghdg_inv
     INT :: dim
     STACK("REALVEC:minimise")
     START_TIMER("REALVEC:minimise")
     ENSURE(size(self)==size(new_direction),"REALVEC:minimise ... vector size mismatch")
     ENSURE(size(self)==size(gradient),"REALVEC:minimise ... vector size mismatch")
     ENSURE(size(self)==size(old_val),"REALVEC:minimise ... vector size mismatch")
     ENSURE(size(self)==size(old_gradient),"REALVEC:minimise ... vector size mismatch")
     ENSURE(size(self)==size(hessian,1),"REALVEC:minimise ... incorrect dimension for hessian matrix")
     ENSURE(size(self)==size(hessian,2),"REALVEC:minimise ... incorrect dimension for hessian matrix")
     dim = size(self)
     call create_(dx,dim); call create_(dg,dim); call create_(hdg,dim); call create_(u,dim)
     dg = gradient - old_gradient
     dx = self - old_val
     call to_product_of_(hdg,hessian,dg)
     dxdg = ONE / dot_product(dx,dg)
     dghdg = dot_product(dg,hdg)
     dghdg_inv = ONE / dghdg
     u  = dxdg * dx - dghdg_inv * hdg
     hessian = hessian + dxdg * outer_product_(dx,dx) - &
               dghdg_inv * outer_product_(hdg,hdg) + dghdg * outer_product_(u,u)
     call to_product_of_(dx,hessian,gradient)
     old_val = self
     old_gradient = gradient
     new_direction = - dx
     call destroy_(dx); call destroy_(dg); call destroy_(hdg); call destroy_(u)
     STOP_TIMER("REALVEC:minimise")
      CHECK
   end subroutine

   subroutine minimise_BFGS(self,dself,p,fret,tol,gtol,step,max_it)
   ! Use the Broyden-Fletcher-Goldfarb-Shanno method to minimise a vector
   ! function self(p) with derivative function dself(p) starting from
   ! an initial point "p", returning the minimum point in "p", to a
   ! tolerance "gtol" in the gradient. If "step" is present it determines
   ! the size of the initial step in the line minimisation along the
   ! gradient direction. If "max_it" is present, set that to be the
   ! maximum iterations
      interface
         function self(p) result(res)
             REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      interface
         function dself(p) result(res)
             REALVEC(:) :: p
            REALVEC(size(p)) :: res
         end function
      end interface
      REALVEC(:) :: p
      REAL :: fret,tol,gtol
      REAL, optional :: step
      INT, optional, IN :: max_it
      INT :: n,iter,i,itmax
      REAL :: fac,fad,fae,fp,sumdg,sumxi,eps, stp,this_stp
      REALVEC(:), PTR :: g,dg,hdg,pnew,xi
      REALMAT(:,:), PTR :: hessian
      BIN :: fail
      STACK("REALVEC:minimise_BFGS")
      START_TIMER("REALVEC:minimise_BFGS")
      eps = tol/FOUR
      stp = ONE
      if (present(step)) stp = step
      itmax = 200
      if (present(max_it)) itmax = max_it
      n = size(p)
      call create_(g,n)
      call create_(dg,n)
      call create_(hdg,n)
      call create_(pnew,n)
      call create_(xi,n)
      allocate(hessian(n,n))
      fp = self(p)
      g = dself(p)
      hessian = ZERO
      do i = 1,n
         hessian(i,i) = ONE
      end do
      xi = -g
      iter = 0
      fail = FALSE
      do
         iter = iter + 1
         pnew = p
         xi = (stp/norm_(xi)) * xi ! This is new
         call line_minimise_from_(self,pnew,xi,fret,tol,fp,this_stp)
         stp = stp*sqrt(abs(this_stp))
         fp = fret
         xi = pnew - p
         p = pnew
         if (largest_value_(xi)<tol) exit
         dg = g
         g = dself(p)
         if (largest_value_(g)<gtol) exit
         dg = g - dg
         call to_product_of_(hdg,hessian,dg)
         fac = sum(dg*xi)
         fae = sum(dg*hdg)
         sumdg = sum(dg*dg)
         sumxi = sum(xi*xi)
         if (fac**2>eps*sumdg*sumxi) then
            fac = ONE/fac
            fad = ONE/fae
            dg = fac*xi - fad*hdg
            hessian = hessian + fac * outer_product_(xi,xi)   &
                              - fad * outer_product_(hdg,hdg) &
                              + fae * outer_product_(dg,dg)
            call minus_product_of_(xi,hessian,g)
         end if
         if (i > itmax) fail = TRUE
      end do
      DIE_IF(fail,"REALVEC:minimise_BFGS ... exceeded allowed iterations")
      deallocate(hessian)
      call destroy_(xi)
      call destroy_(pnew)
      call destroy_(hdg)
      call destroy_(dg)
      call destroy_(g)
     STOP_TIMER("REALVEC:minimise_BFGS")
      CHECK
   end subroutine

   subroutine minimise_FRPR(self,dself,p,fret,tol,ftol,algorithm,step)
   ! Use the Fletcher-Reeves-Polak-Ribiere method to minimise a vector
   ! function self(p) with derivative function dself(p) starting from
   ! an initial point "p", returning the minimum point in "p", to a
   ! tolerance "tol" and the minimum function value in "fret" to a
   ! tolerance "ftol". "algorithm" (if present) can be set to
   ! "Polak-Ribiere" (default) or "Fletcher-Reeves".
      interface
         function self(p) result(res)
            REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      interface
         function dself(p) result(res)
            REALVEC(:) :: p
            REALVEC(size(p)) :: res
         end function
      end interface
      REALVEC(:) :: p
      REAL :: fret,tol,ftol
      REAL, optional :: step
      STR(STR_SIZE), optional :: algorithm
      INT :: itmax = 200
      INT :: n,iter
!      eps :: REAL = TOL(10)
      REAL :: stp,this_stp,dgg,fp,gam,gg,dtol
      REALVEC(:), PTR :: g,h,xi
      STR(STR_SIZE) :: alg
      BIN :: fail
      STACK("REALVEC:minimise_FRPR")
      START_TIMER("REALVEC:minimise_FRPR")
      alg = "Polak-Ribiere"
      if (present(algorithm)) alg = algorithm
      stp = ONE
      if (present(step)) stp = step
      n = size(p)
      call create_(g,n)
      call create_(h,n)
      call create_(xi,n)
      fp = self(p)
      fret = fp
      xi = dself(p)
      g = -xi
      h = g
      xi = h
      iter = 0
      fail = FALSE
      do
         iter = iter + 1
         dtol = largest_value_(xi)
         if (dtol<tol) exit
         xi = (stp/norm_(xi)) * xi ! This is new
         call line_minimise_from_(self,p,xi,fret,tol,fp,this_stp)
         stp = stp*sqrt(abs(this_stp))
       ! if (TWO*abs(fret-fp)<=ftol*(abs(fret)+abs(fp)+eps)) exit
         if (abs(fret-fp)<=ftol AND dtol<tol) exit
         fail = iter>=itmax
         if (fail) exit
       ! fp = self(p)
         fp = fret
         xi = dself(p)
         gg = ZERO
         dgg = ZERO
         gg = sum(g*g)
         select case (alg)
            case("Polak-Ribiere  "); dgg = sum((xi+g)*xi)
            case("Fletcher-Reeves"); dgg = sum(xi*xi)
            case default;        allocate(tonto%known_keywords(2))
            tonto%known_keywords(1) = "Polak-Ribiere  "
            tonto%known_keywords(2) = "Fletcher-Reeves"
            call unknown_(tonto,alg,"REALVEC:minimise_FRPR")
            deallocate(tonto%known_keywords)
         end select
         if (gg==ZERO) exit
         gam = dgg/gg
         g = -xi
         h = g + gam*h
         xi = h
      end do
      DIE_IF(fail,"REALVEC:minimise_FRPR ... exceeded allowed iterations")
      call destroy_(xi)
      call destroy_(h)
      call destroy_(g)
     STOP_TIMER("REALVEC:minimise_FRPR")
      CHECK
   end subroutine

   subroutine minimise_powell(self,p,directions,fret,tol,ftol)
   ! Use Powell's method to minimise a vector function self(p) starting from
   ! an initial point "p" along the initial (columns of) "directions", returning
   ! the minimum point in "p", the minimum function value in "fret" to a
   ! tolerance "ftol", and "tol" in the vector coordinates "p".
      interface
         function self(p) result(res)
             REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      REALVEC(:) :: p
      REALMAT(:,:) :: directions
      REAL :: fret,tol,ftol
      INT :: itmax = 200
      INT :: n,iter,i,ibig
      REAL :: del,fp,fptt,t
      REALVEC(:), PTR :: pt,ptt,xit
      BIN :: fail
      STACK("REALVEC:minimise_powell")
      START_TIMER("REALVEC:minimise_powell")
      ENSURE(size(p)==size(directions,1),"REALVEC:minimise_powell ... incompatible initial data")
      ENSURE(size(p)==size(directions,2),"REALVEC:minimise_powell ... incompatible initial data")
      n = size(p)
      call create_(pt,n)
      call create_(ptt,n)
      call create_(xit,n)
      fret = self(p)
      pt = p                                    ! Save initial point
      iter = 0
      fail = FALSE
      do                                        ! Iteration loop
         iter = iter + 1
         fp = fret
         ibig = 0
         del = 0
         do i = 1,n                             ! Loop over all directions in set
            xit = directions(:,i)               ! Get direction
            fptt = fret
            call line_minimise_from_(self,p,xit,fret,tol) ! Minimize along direction
            if (abs(fptt-fret)>del) then        ! Save if largest decrease so far
               del = abs(fptt-fret)
               ibig = i
            end if
         end do
         if (TWO*abs(fp-fret)<=ftol*(abs(fp)+abs(fret))) exit
         fail = iter>=itmax
         if (fail) exit
         ptt = TWO*p - pt                       ! Construct extrapolated point and the
         xit = p - pt                           ! average direction moved; save old
         pt = p                                 ! starting point
         fptt = self(ptt)
         if (fptt>=fp) cycle
         t = TWO*(fp-TWO*fret+fptt)*(fp-fret-del)**2-del*(fp-fptt)**2
         if (t>=0) cycle
         call line_minimise_from_(self,p,xit,fret,tol)    ! Move to minimum of the new direction
         directions(:,ibig) = directions(:,n)
         directions(:,n) = xit
      end do
      DIE_IF(fail,"REALVEC:minimise_powell ... exceeded allowed iterations")
      call destroy_(xit)
      call destroy_(ptt)
      call destroy_(pt)
     STOP_TIMER("REALVEC:minimise_powell")
      CHECK
   end subroutine

   subroutine line_minimise_from(self,p,direction,fret,tol,f0,del)
   ! Given a vector function self(x), minimise from point "p" along "direction".
   ! Return the minimum point in "p" and the minimum value "fret", with an
   ! accuracy "tol". If "f0" is present it is used as the value of the function
   ! "self" at p, f0 = self(p). If "del" is present it is set to the length
   ! along "direction" where p achieved its minimum, useful for monitoring the
   ! step size.
      interface
         function self(p) result(res)
            REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      REALVEC(:) :: p,direction
      REAL :: fret,tol
      REAL, optional :: f0,del
      REAL :: a,x,b,fa,fx,fb,xmin
      STACK("REALVEC:line_minimise_from")
      START_TIMER("REALVEC:line_minimise_from")
      a = ZERO
      x = ONE
      call bracket_minimum_(self,p,direction,a,x,b,fa,fx,fb,f0)
      call minimise_brent_(self,p,direction,a,x,b,xmin,fret,tol,fx)
      if (present(del)) del = xmin
     STOP_TIMER("REALVEC:line_minimise_from")
      CHECK
   end subroutine

   subroutine bracket_minimum(self,p,direction,a,b,c,fa,fb,fc,fa0,fb0)
   ! Given a vector function self(p), an initial point "p" and a "direction",
   ! and initial distances "a" and "b" along "direction" from "p", search in
   ! the downhill direction and return new distances "a", "b" and "c" along
   ! "direction" from "p" that bracket a minimum of the function self, and
   ! return the values of the function "fa", "fb", and "fc" at these points.
   ! NOTE: "c" is not used initially.
   ! If present, "fa0" is the initial value of self at a, fa0 = self(a).
   ! If present, "fb0" is the initial value of self at b, fb0 = self(b).
      interface
         function self(p) result(res)
             REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      REALVEC(:) :: p,direction
      REAL :: a,b,c,fa,fb,fc
      REAL, optional :: fa0,fb0
      REAL :: gold = 1.618034
      REAL :: glimit = 100
      REAL :: tiny = 1.0d-20
      REAL :: fu,q,r,u,ulim
      INT :: iter
      BIN :: fail
      STACK("REALVEC:bracket_minimum")
      START_TIMER("REALVEC:bracket_minimum")
      ENSURE(size(p)==size(direction),"REALVEC:bracket_minimum ... incompatible vectors")
      if (present(fa0)) then; fa = fa0
      else;                   fa = self(p + a*direction)
      end if
      if (present(fb0)) then; fb = fb0
      else;                   fb = self(p + b*direction)
      end if
      if (fb>fa) then
        call swap_with_(a,b)
        call swap_with_(fa,fb)
      end if
      c  = b + gold*(b-a)
      fc = self(p + c*direction)
      fail = TRUE
      do iter = 1, 100
         if (fb<fc) then
           fail = FALSE
           exit                           ! bracket found
         end if
         r = (b-a)*(fb-fc)                ! get u by parabolic extrapolation
         q = (b-c)*(fb-fa)
         u = b - ((b-c)*q-(b-a)*r)/(TWO*sign(max(abs(q-r),tiny),q-r))
         ulim = b + glimit*(c-b)
         if ((b-u)*(u-c)>ZERO) then       ! Parabolic u lies between b and c
            fu = self(p + u*direction)
            if (fu<fc) then               ! got a minimum between b and c
               a = b; fa = fb
               b = u; fb = fu
               fail = FALSE
               exit
            else if (fu>fb) then          ! got a minimum between a and u
               c = u; fc = fu
               fail = FALSE
               exit
            end if
            u = c + gold*(c-b)            ! parabolic fit no use, so magnify
            fu = self(p + u*direction)
         else if ((c-u)*(u-ulim)>0) then ! Fit is between c and its allowed limit
            fu = self(p + u*direction)
            if (fu<fc) then
               b = c; fb = fc
               c = u; fc = fu
               u = c + gold*(c-b)
               fu = self(p + u*direction)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            fu = self(p + u*direction)
         else
            u = c + gold*(c-b)            ! magnify
            fu = self(p + u*direction)
         end if
         a = b; fa = fb
         b = c; fb = fc
         c = u; fc = fu
      end do
      WARN_IF(fail,"REALVEC:bracket_minimum ... exceeded maximum iterations")
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(fa,fc)
      end if
     STOP_TIMER("REALVEC:bracket_minimum")
      CHECK
   end subroutine

   subroutine minimise_golden(self,p,direction,a,b,c,xmin,f,tol)
   ! Given a vector function self(p), an initial point "p" and a "direction",
   ! and initial distances "a", "b" and "c" along "direction" from "p" which
   ! bracket a minimum in function self, return the minimum point "p" and its
   ! distance "xmin" along the "direction" from "p", and also the value "f" at
   ! the minimum to a precision "tol", using the golden section search method.
      interface
         function self(p) result(res)
             REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      REALVEC(:) :: p,direction
      REAL :: a,b,c,xmin,f,tol
       REAL :: r = 0.618033399
      REAL :: s,f1,f2,x0,x1,x2,x3
      STACK("REALVEC:minimise_golden")
      START_TIMER("REALVEC:minimise_golden")
      ENSURE(size(p)==size(direction),"REALVEC:minimise_golden ... incompatible vectors")
      s = ONE - r
      x0 = a
      x3 = c
      if (abs(c-b)>abs(b-a)) then
         x1 = b; x2 = b + s*(c-b)
      else
         x2 = b; x1 = b - s*(b-a)
      end if
      f1 = self(p + x1*direction)
      f2 = self(p + x2*direction)
      do
         if (abs(x3-x0)<=tol*(abs(x1)+abs(x2))) exit
         if (f2<f1) then
            x0 = x1
            x1 = x2
            x2 = r*x1 + s*x3
            f1 = f2
            f2 = self(p + x2*direction)
         else
            x3 = x2
            x2 = x1
            x1 = r*x2 + s*x0
            f2 = f1
            f1 = self(p + x1*direction)
         end if
      end do
      if (f1<f2) then; f = f1; xmin = x1; p = p + x1*direction
      else;            f = f2; xmin = x2; p = p + x2*direction
      end if
     STOP_TIMER("REALVEC:minimise_golden")
      CHECK
   end subroutine

   subroutine minimise_brent(self,p,direction,a,b,c,xmin,f,tol,fb_in)
   ! Given a vector function self(x), an initial point "p", a "direction", and
   ! initial distances "a", "b" and "c" along "direction" from "p" which
   ! bracket a minimum in function self, return the minimum point "p" and its
   ! distance "xmin" along the "direction" from "p", and also the value "f" at
   ! the minimum to a precision "tol", using Brent's search method.
      interface
         function self(p) result(res)
             REALVEC(:) :: p
            REAL :: res
         end function
      end interface
      REALVEC(:) :: p,direction
      REAL :: a,b,c,xmin,f,tol
      REAL, IN, optional :: fb_in
      INT :: itmax = 100
      REAL :: cgold = 0.3819660d0
      REAL :: zeps = TOL(10)
      REAL :: d,e,etemp,fu,fv,fw,fx,pp,qq,r,tol1,tol2,u,v,w,x,xm
      INT :: iter,iters
      BIN :: fail
      STACK("REALVEC:minimise_brent")
      START_TIMER("REALVEC:minimise_brent")
      ENSURE(size(p)==size(direction),"REALVEC:minimise_brent ... incompatible vectors")
      if (a>c) call swap_with_(a,c)
      v = b
      w = b
      x = b
      if (present(fb_in)) then
        fx = fb_in
      else
        fx = self(p + x*direction)
      end if
      fv = fx
      fw = fx
      e = ZERO
      fail = TRUE
      b = c
      iters=0
      do iter = 1,itmax
         xm = HALF*(a+b)
         tol1 = tol*abs(x) + zeps
         tol2 = TWO*tol1
         if (abs(x-xm)<(tol2-HALF*(b-a))) then
            fail = FALSE
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(fx-fv)
            qq = (x-v)*(fx-fw)
            pp = (x-v)*qq - (x-w)*r
            qq = TWO*(qq-r)
            if (qq>ZERO) pp = -pp
            qq = abs(qq)
            etemp = e
            e = d
            if (abs(pp)>=abs(HALF*qq*etemp) OR pp<=qq*(a-x) OR pp>=qq*(b-x)) then
              if (x>=xm) then; e = a-x
              else;            e = b-x
              end if
              d = cgold*e
            else
              d = pp/qq
              u = x + d
              if ((u-a)<tol2 OR (b-u)<tol2) d = sign(tol1,xm-x)
            end if
         else
           if (x>=xm) then; e = a-x
           else;            e = b-x
           end if
           d = cgold*e
         end if
         if (abs(d)>=tol1) then; u = x + d
         else;                   u = x + sign(tol1,d)
         end if
         fu = self(p + u*direction)
         if (fu<=fx) then
            if (u>=x) then; a = x
            else;           b = x
            end if
            v = w; fv = fw
            w = x; fw = fx
            x = u; fx = fu
         else
            if (u<x) then; a = u
            else;          b = u
            end if
            if (fu<=fw OR w==x) then
               v = w; fv = fw
               w = u; fw = fu
            else if (fu<=fv OR v==x OR v==w) then
               v = u; fv = fu
            end if
         end if
         iters=iters+1
      end do
      DIE_IF(fail,"REALVEC:minimise_brent ... maximum iterations exceeded")
      f = fx
      xmin = x
      p = p + x*direction
     STOP_TIMER("REALVEC:minimise_brent")
      CHECK
   end subroutine

end
