!---------------------------------------------------------------------------
!
!  INTVEC : Integer vector operations ...
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
! $Id: intvec.foo,v 1.14.2.3 2003/11/06 10:06:14 dylan Exp $
!---------------------------------------------------------------------------

module INTVEC_MODULE

#  include "intvec.use"

   implicit none

#  include "macros"
#  include "intvec.int"


contains

   subroutine create(self,dim)
    INTVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:create")
      START_TIMER("INTVEC:create")
      ENSURE(dim>=0,"INTVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*INT_SIZE)
     STOP_TIMER("INTVEC:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb,ub)
    INTVEC(:) :: self
   ! Create the vector with lower bound "lb", upper bound "ub"
      PTR :: self
      INT, IN :: lb,ub
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:create_1")
      START_TIMER("INTVEC:create_1")
      nullify(self)
      allocate(self(lb:ub))
      ADD_MEMORY((ub-lb+1)*INT_SIZE)
     STOP_TIMER("INTVEC:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds)
    INTVEC(:) :: self
   ! Create the vector with "bounds"
      PTR :: self
      INTVEC(2), IN :: bounds
   ! The following code is inherited from OBJECTVEC
      STACK("INTVEC:create_2")
      START_TIMER("INTVEC:create_2")
      call create_(self,bounds(1),bounds(2))
     STOP_TIMER("INTVEC:create_2")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:destroy")
      START_TIMER("INTVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("INTVEC:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*INT_SIZE)
      deallocate(self)
     STOP_TIMER("INTVEC:destroy")
      UNSTACK
   end subroutine

!   created result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!   end

!   destroyed result(res) ::: get_from(OBJECTVEC)
!   ! Returns true if self has *not* been created
!      self :: PTR
!      res :: BIN
!   end

   subroutine create_copy(self,vec)
    INTVEC(:) :: self
   ! Create a replica copy of vec.
      INTVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:create_copy")
      START_TIMER("INTVEC:create_copy")
      call create_(self,size(vec))
      self = vec
     STOP_TIMER("INTVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    INTVEC(:) :: self
   ! Copy "vec".
      INTVEC(:), IN :: vec
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:copy")
      START_TIMER("INTVEC:copy")
      ENSURE(size(self)==size(vec),"INTVEC:copy ... vec size does not match")
      self = vec
     STOP_TIMER("INTVEC:copy")
      CHECK
   end subroutine

   subroutine shrink(self,dim)
    INTVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     INTVEC(:), PTR :: old
     INT :: n
     STACK("INTVEC:shrink")
     START_TIMER("INTVEC:shrink")
     ENSURE(associated(self),"INTVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"INTVEC:shrink ... dim too large")
     if (dim==size(self)) then; STOP_TIMER("INTVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       self(n) = old(n)
     end do
     call destroy_(old)
     STOP_TIMER("INTVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    INTVEC(:) :: self
   ! Expand self to dimension dim. New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     INTVEC(:), PTR :: old
     INT :: old_dim
     STACK("INTVEC:expand")
     START_TIMER("INTVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"INTVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if
     STOP_TIMER("INTVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    INTVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     INTVEC(:), IN :: v
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("INTVEC:append")
     START_TIMER("INTVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v
     STOP_TIMER("INTVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    INTVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     INT, IN :: value
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("INTVEC:append_1")
     START_TIMER("INTVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value
     STOP_TIMER("INTVEC:append_1")
      UNSTACK
   end subroutine

   subroutine append_only_if_unique(self,value)
    INTVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end, but
   ! only if the "value" is unique
     PTR :: self
     INT, IN :: value
   ! The following code is inherited from INTRINSICVEC
     STACK("INTVEC:append_only_if_unique")
     START_TIMER("INTVEC:append_only_if_unique")
     if (any(self==value)) then; STOP_TIMER("INTVEC:append_only_if_unique") UNSTACK return; end if
     call append_(self,value)
     STOP_TIMER("INTVEC:append_only_if_unique")
      UNSTACK
   end subroutine

   subroutine remove_repetitions(self)
    INTVEC(:) :: self
   ! Sort through the vector and remove repeated elements which come later in
   ! the list.  NOTE: the vector may shrink
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      INTVEC(:), PTR :: unique
      INT :: i,n
   STACK("INTVEC:remove_repetitions")
   START_TIMER("INTVEC:remove_repetitions")
   ENSURE(associated(self),"INTVEC:remove_repetitions ... no vector")
      if (size(self)==1) then; STOP_TIMER("INTVEC:remove_repetitions") UNSTACK return; end if
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         if (any(self(i)==unique(1:n))) cycle
         n = n + 1
         unique(n) = self(i)
      end do
      call destroy_(self)
      call create_(self,n)
      self = unique(1:n)
      call destroy_(unique)
     STOP_TIMER("INTVEC:remove_repetitions")
      UNSTACK
   end subroutine

   function has_repetitions(self) result(res)
    INTVEC(:) :: self
   ! Return TRUE if self has at least one repeated element.
      BIN :: res
   ! The following code is inherited from INTRINSICVEC
      INTVEC(:), PTR :: unique
      INT :: i,n
      STACK("INTVEC:has_repetitions")
      START_TIMER("INTVEC:has_repetitions")
      res = FALSE
      if (size(self)==1) then; STOP_TIMER("INTVEC:has_repetitions") CHECK return; end if
      call create_(unique,size(self))
      n = 1
      unique(1) = self(1)
      do i = 2,size(self)
         if (any(self(i)==unique(1:n))) then
            res = TRUE
         else
            n = n + 1
            unique(n) = self(i)
         end if
      end do
      call destroy_(unique)
     STOP_TIMER("INTVEC:has_repetitions")
      CHECK
   end function

   function join(self,v) result(res)
    INTVEC(:) :: self
   ! Yield a vector which is the concatenation of "self" and "v"
     INTVEC(:), IN :: v
     INTVEC(:), PTR :: res
   ! The following code is inherited from INTRINSICVEC
     INT :: dim, dim_v
     STACK("INTVEC:join")
     START_TIMER("INTVEC:join")
     dim   = size(self)
     dim_v = size(v)
     call create_(res,dim+dim_v)
     res(    1:dim      ) = self
     res(dim+1:dim+dim_v) = v
     STOP_TIMER("INTVEC:join")
      UNSTACK
   end function

   function join_1(self,v1,v2) result(res)
    INTVEC(:) :: self
   ! Yield a vector which is the concatenation of "self" and "v1" and "v2"
     INTVEC(:), IN :: v1,v2
     INTVEC(:), PTR :: res
   ! The following code is inherited from INTRINSICVEC
     INT :: dim, dim_v1, dim_v2
     STACK("INTVEC:join_1")
     START_TIMER("INTVEC:join_1")
     dim    = size(self)
     dim_v1 = size(v1)
     dim_v2 = size(v2)
     call create_(res,dim+dim_v1+dim_v2)
     res(           1:dim              ) = self
     res(dim+       1:dim+dim_v1       ) = v1
     res(dim+dim_v1+1:dim+dim_v1+dim_v2) = v2
     STOP_TIMER("INTVEC:join_1")
      UNSTACK
   end function

   subroutine to_product_of(self,mat,vec,transpose)
    INTVEC(:) :: self
   ! Set "self" to the product of the matrix "mat" and vector "vec". If present,
   ! "transpose" can be set to TRUE if the matrix needs to be transposed.
      INTMAT(:,:), IN :: mat
      INTVEC(:), IN :: vec
      BIN, optional :: transpose
      INT :: i,k
      BIN :: trans
      REAL :: temp
      STACK("INTVEC:to_product_of")
      START_TIMER("INTVEC:to_product_of")
      trans = FALSE
      if (present(transpose)) trans = transpose
      if (trans) then
        ENSURE(size(mat,2)==size(self),"INTVEC:to_product_of ... array dimensions do not agree")
        ENSURE(size(mat,1)==size(vec),"INTVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(1,i) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = temp
        end do
      else
        ENSURE(size(mat,1)==size(self),"INTVEC:to_product_of ... array dimensions do not agree")
        ENSURE(size(mat,2)==size(vec),"INTVEC:to_product_of ... array dimensions do not agree")
        do i = 1,size(self)
          temp = mat(i,1) * vec(1)
          do k=2,size(vec)
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = temp
        end do
      end if
     STOP_TIMER("INTVEC:to_product_of")
      CHECK
   end subroutine

   subroutine to_product_of_1(self,mat_a,mat_b,transpose_a,transpose_b)
    INTVEC(:) :: self
   ! Set "self" to the matrix product of "mat_a" and "mat_b". If present,
   ! "transpose_a" and "transpose_b" can be set to TRUE if "mat_a" and "mat_b"
   ! neeb to be transposed.
     INTMAT(:,:) :: mat_a, mat_b
     BIN, optional :: transpose_a, transpose_b
     INT :: dim_a1,dim_a2,dim_b1,dim_b2,dim1,i,k,opt
     BIN :: trans_a,trans_b
     REAL :: temp

     STACK("INTVEC:to_product_of_1")
     START_TIMER("INTVEC:to_product_of_1")
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
         ENSURE(dim1==dim_a1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a2==dim_b1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b2==1,"INTVEC:to_product_of_1 ... b array dimensions incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (1)
         ENSURE(dim1==dim_a2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a1==dim_b1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b2==1,"INTVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(k,1)
           end do
           self(i) = temp
         end do
       case (2)
         ENSURE(dim1==dim_a1,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a2==dim_b2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b1==1,"INTVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a2
             temp = temp + mat_a(i,k) * mat_b(1,k)
           end do
           self(i) = temp
         end do
       case (3)
         ENSURE(dim1==dim_a2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_a1==dim_b2,"INTVEC:to_product_of_1 ... array dimensions do not agree")
         ENSURE(dim_b1==1,"INTVEC:to_product_of_1 ... b array dimension incorrect")
         do i=1,dim1
           temp = ZERO
           do k=1,dim_a1
             temp = temp + mat_a(k,i) * mat_b(1,k)
           end do
           self(i) = temp
         end do
     end select
     STOP_TIMER("INTVEC:to_product_of_1")
      CHECK
   end subroutine

   subroutine plus_product_of(self,mat,vec,transpose)
    INTVEC(:) :: self
   ! Add to "self" the product of the matrix and vector. If present,
   ! "transpose" can be set to TRUE if the matrix needs to be transposed.
      INTMAT(:,:), IN :: mat
      INTVEC(:), IN :: vec
      BIN, optional :: transpose
      INT :: dim_a1,dim_a2,dim_b1,dim1,i,k
      BIN :: trans
      REAL :: temp
      STACK("INTVEC:plus_product_of")
      START_TIMER("INTVEC:plus_product_of")
      trans = FALSE
      if (present(transpose)) trans = transpose
      dim_a1 = size(mat,1);   dim_a2 = size(mat,2)
      dim_b1 = size(vec,1)
      dim1   = size(self)
      if (trans) then
        ENSURE(dim1==dim_a2,"INTVEC:plus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a1,"INTVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      else
        ENSURE(dim1==dim_a1,"INTVEC:plus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a2,"INTVEC:plus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(i,1) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = self(i) + temp
        end do
      end if
     STOP_TIMER("INTVEC:plus_product_of")
      CHECK
   end subroutine

   subroutine minus_product_of(self,mat,vec,transpose)
    INTVEC(:) :: self
   ! Subtract from "self" the product of the matrix and vector. If present,
   ! "transpose" can be set to TRUE if the matrix needs to be transposed.
      INTMAT(:,:), IN :: mat
      INTVEC(:), IN :: vec
      BIN, optional :: transpose
      INT :: dim_a1,dim_a2,dim_b1,dim1,i,k
      BIN :: trans
      REAL :: temp
      STACK("INTVEC:minus_product_of")
      START_TIMER("INTVEC:minus_product_of")
      trans = FALSE
      if (present(transpose)) trans = transpose
      dim_a1 = size(mat,1);   dim_a2 = size(mat,2)
      dim_b1 = size(vec,1)
      dim1   = size(self)
      if (trans) then
        ENSURE(dim1==dim_a2,"INTVEC:minus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a1,"INTVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(1,i) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(k,i) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      else
        ENSURE(dim1==dim_a1,"INTVEC:minus_product_of ... array dimensions do not agree")
        ENSURE(dim_b1==dim_a2,"INTVEC:minus_product_of ... array dimensions do not agree")
        do i=1,dim1
          temp = mat(i,1) * vec(1)
          do k=2,dim_b1
            temp = temp + mat(i,k) * vec(k)
          end do
          self(i) = self(i) - temp
        end do
      end if
     STOP_TIMER("INTVEC:minus_product_of")
      CHECK
   end subroutine

   PURE function dot(self,b) result(res)
    INTVEC(:) :: self
   ! Return the dot product with "b"
      IN :: self
      INTVEC(:), IN :: b
      INT :: res
   ! The following code is inherited from INTRINSICVEC
      res = dot_product(self,b)
     STOP_TIMER("INTVEC:dot")
   end function

   PURE function norm(self) result(res)
    INTVEC(:) :: self
   ! Return the norm of the vector
      IN :: self
      REAL :: res
      res = sqrt(real(sum(self*self),kind=REAL_KIND))
     STOP_TIMER("INTVEC:norm")
   end function

   PURE function cross(self,b) result(res)
    INTVEC(:) :: self
   ! Return the cross product of "self" and "b".
      IN :: self
      INTVEC(:), IN :: b
      INTVEC(3) :: res
   ! The following code is inherited from INTRINSICVEC
      
      res(1) = self(2)*b(3) - b(2)*self(3)
      res(2) = self(3)*b(1) - b(3)*self(1)
      res(3) = self(1)*b(2) - b(1)*self(2)
     STOP_TIMER("INTVEC:cross")
   end function

   PURE subroutine to_cross_product(self,a,b)
    INTVEC(:) :: self
   ! Set the vector to the cross product of "a" and "b".
      INOUT :: self
      INTVEC(:), IN :: a,b
   ! The following code is inherited from INTRINSICVEC
      
      self(1) = a(2)*b(3) - b(2)*a(3)
      self(2) = a(3)*b(1) - b(3)*a(1)
      self(3) = a(1)*b(2) - b(1)*a(2)
     STOP_TIMER("INTVEC:to_cross_product")
   end subroutine

   PURE subroutine reverse_order(self)
    INTVEC(:) :: self
   ! Reverse the order of the elements of self
     INOUT :: self
   ! The following code is inherited from OBJECTVEC
     INT :: n,dim
     dim = size(self)
     do n = 1,dim/2
       call swap_elements_(self,n,dim-n+1)
     end do
     STOP_TIMER("INTVEC:reverse_order")
   end subroutine

   function equals(self,b) result(res)
    INTVEC(:) :: self
   ! Return true if "self" is the same as "b".
      IN :: self
      INTVEC(:), IN :: b
      BIN :: res
      STACK("INTVEC:equals")
      START_TIMER("INTVEC:equals")
      res = same_as_(self,b)
     STOP_TIMER("INTVEC:equals")
      CHECK
   end function

   function same_as(self,b) result(res)
    INTVEC(:) :: self
   ! Return true if "self" is the same as "b".
      IN :: self
      INTVEC(:), IN :: b
      BIN :: res
      STACK("INTVEC:same_as")
      START_TIMER("INTVEC:same_as")
      if (size(self)/=size(b)) then; res = FALSE
      else;                  res = all(self==b)
      end if
     STOP_TIMER("INTVEC:same_as")
      CHECK
   end function

   PURE subroutine to_scaled_vec(self,fac,b)
    INTVEC(:) :: self
   ! Set the vector to "b" scaled by "fac"
      INOUT :: self
      INTVEC(:), IN :: b
      INT, IN :: fac
   ! The following code is inherited from INTRINSICVEC
      self = fac*b
     STOP_TIMER("INTVEC:to_scaled_vec")
   end subroutine

   PURE subroutine plus_scaled_vec(self,fac,b)
    INTVEC(:) :: self
   ! Add a vector "b" scaled by "fac" to "self"
      INOUT :: self
      INTVEC(:), IN :: b
      INT, IN :: fac
   ! The following code is inherited from INTRINSICVEC
      self = self + fac*b
     STOP_TIMER("INTVEC:plus_scaled_vec")
   end subroutine

   PURE subroutine minus(self,b,mask)
    INTVEC(:) :: self
   ! Subtract vector "b" from "self"
      INOUT :: self
      INTVEC(:), IN :: b
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
     STOP_TIMER("INTVEC:minus")
   end subroutine

   PURE subroutine plus(self,b,mask)
    INTVEC(:) :: self
   ! Add vector "b" to "self"
      INOUT :: self
      BINVEC(:), IN, optional :: mask
      INTVEC(:), IN :: b
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      
      if (NOT present(mask)) then
         self = self + b
      else
         do i = 1,size(self)
            if (mask(i)) self(i) = self(i) + b(i)
         end do
      end if
     STOP_TIMER("INTVEC:plus")
   end subroutine

   PURE subroutine set_to(self,b)
    INTVEC(:) :: self
   ! Set the vector to "b". See also the "copy" routine.
      INOUT :: self
      INTVEC(:), IN :: b
   ! The following code is inherited from INTRINSICVEC
      self = b
     STOP_TIMER("INTVEC:set_to")
   end subroutine

   PURE subroutine swap_elements(self,e1,e2)
    INTVEC(:) :: self
   ! Swap elements "e1" and "e2" in "self".
      INOUT :: self
      INT, IN :: e1,e2
   ! The following code is inherited from INTRINSICVEC
      INT :: val
      
      
      val = self(e1)
      self(e1) = self(e2)
      self(e2) = val
     STOP_TIMER("INTVEC:swap_elements")
   end subroutine

   PURE function index_of_first_nonzero_value(self) result(res)
    INTVEC(:) :: self
   ! Returns the index of the first nonzero component of self.
     IN :: self
     INT :: res
     INT :: i
     res=0
     do i=1,size(self)
       if (self(i)/=0) then
         res=i
         exit
       end if
     end do
     STOP_TIMER("INTVEC:index_of_first_nonzero_value")
   end function

   PURE function index_of_first_zero_value(self) result(res)
    INTVEC(:) :: self
   ! Returns the index of the first zero component of self.
     IN :: self
     INT :: res
     INT :: i
     res=0
     do i=1,size(self)
       if (self(i)==0) then
         res=i
         exit
       end if
     end do
     STOP_TIMER("INTVEC:index_of_first_zero_value")
   end function

   PURE function index_of_value(self,val) result(pos)
    INTVEC(:) :: self
   ! Returns the first index in "self" which has the value "val", or 0 if "val"
   ! is not present in the array.
     IN :: self
     INT, IN :: val
     INT :: pos
     INT :: i
     pos = 0
     do i = 1,size(self)
        if (self(i)/=val) cycle
        pos = i
        exit
     end do
     STOP_TIMER("INTVEC:index_of_value")
   end function

   PURE subroutine chop_large_values(self,val)
    INTVEC(:) :: self
   ! Set all values in the self whose absolute value is larger than "val" to
   ! "val" times the sign of the original value.
      INOUT :: self
      INT, IN :: val
   ! The following code is inherited from INTRINSICVEC
      INT :: dim,i
      INT :: bb,ba,sign
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         if (bb==ZERO) cycle
         ba = abs(bb)
         sign = bb/ba
         self(i) = sign*min(val,ba)
     end do
     STOP_TIMER("INTVEC:chop_large_values")
   end subroutine

   PURE function index_of_maximum(self) result(ind)
    INTVEC(:) :: self
   ! Return the index of the maximum in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      ind = maxval(maxloc(self))
     STOP_TIMER("INTVEC:index_of_maximum")
   end function

   PURE function index_of_minimum(self) result(ind)
    INTVEC(:) :: self
   ! Return the index of the minimum in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      ind = minval(minloc(self))
     STOP_TIMER("INTVEC:index_of_minimum")
   end function

   function is_zero(self) result(res)
    INTVEC(:) :: self
   ! Return true if the vector is zero
      BIN :: res
      STACK("INTVEC:is_zero")
      START_TIMER("INTVEC:is_zero")
      res = all(self==0)
     STOP_TIMER("INTVEC:is_zero")
      CHECK
   end function

   function all_in_range(self,range) result(res)
    INTVEC(:) :: self
   ! Return TRUE if all values of self are within the specified "range".
      IN :: self
      INTVEC(2), IN :: range
      BIN :: res
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:all_in_range")
      START_TIMER("INTVEC:all_in_range")
      res = all(range(1) <= self AND self <= range(2))
     STOP_TIMER("INTVEC:all_in_range")
      CHECK
   end function

   function in_range(self,range) result(res)
    INTVEC(:) :: self
   ! Return element i as TRUE if self(i) is within the specified "range".
      IN :: self
      INTVEC(2), IN :: range
      BINVEC(size(self)) :: res
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:in_range")
      START_TIMER("INTVEC:in_range")
      res = (range(1) <= self AND self <= range(2))
     STOP_TIMER("INTVEC:in_range")
      CHECK
   end function

   function range(self) result(res)
    INTVEC(:) :: self
   ! Return the range (smallest and largest value) of self.
      IN :: self
      INTVEC(2) :: res
   ! The following code is inherited from INTRINSICVEC
      STACK("INTVEC:range")
      START_TIMER("INTVEC:range")
      res(1) = minval(self)
      res(2) = maxval(self)
     STOP_TIMER("INTVEC:range")
      CHECK
   end function

   function is_z_axis(self) result(res)
    INTVEC(:) :: self
   ! Return true if the vector is set to the z-axis
      BIN :: res
      STACK("INTVEC:is_z_axis")
      START_TIMER("INTVEC:is_z_axis")
      ENSURE(size(self)==3,"INTVEC:is_z_axis ... must supply a 3 dimensional vector!")
      res = self(1) == 0
      res = self(2) == 0 AND res
      res = (1-self(3)) == 0 AND res
     STOP_TIMER("INTVEC:is_z_axis")
      CHECK
   end function

   PURE function largest_value(self) result(maxval)
    INTVEC(:) :: self
   ! Return the maximum absolute value in the vector
      IN :: self
      INT :: maxval
   ! The following code is inherited from INTRINSICVEC
      INT :: val
      INT :: i
      maxval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) maxval = val
      end do
     STOP_TIMER("INTVEC:largest_value")
   end function

   PURE function smallest_value(self) result(minval)
    INTVEC(:) :: self
   ! Return minimum absolute value in the vector
      IN :: self
      INT :: minval
   ! The following code is inherited from INTRINSICVEC
      INT :: val
      INT :: i
      minval = abs(self(1))
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) minval = val
      end do
     STOP_TIMER("INTVEC:smallest_value")
   end function

   PURE function index_of_largest_value(self) result(ind)
    INTVEC(:) :: self
   ! Return the index "ind" of the largest absolute value in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      INT :: maxval,val
      maxval = abs(self(1))
      ind = 1
      do i = 2,size(self)
        val = abs(self(i))
        if (val > maxval) then
          maxval = val
          ind = i
        end if
      end do
     STOP_TIMER("INTVEC:index_of_largest_value")
   end function

   PURE function index_of_smallest_value(self) result(ind)
    INTVEC(:) :: self
   ! Return the index "ind" of the smallest value in the vector
      IN :: self
      INT :: ind
   ! The following code is inherited from INTRINSICVEC
      INT :: i
      INT :: minval,val
      minval = abs(self(1))
      ind = 1
      do i = 2,size(self)
        val = abs(self(i))
        if (val < minval) then
          minval = val
          ind = i
        end if
      end do
     STOP_TIMER("INTVEC:index_of_smallest_value")
   end function

   PURE function no_of_elements_larger_than(self,tol) result(res)
    INTVEC(:) :: self
   ! Return the number of elements larger than "tol".
      IN :: self
      INT, IN :: tol
      INT :: res
   ! The following code is inherited from INTRINSICVEC
      res = count(self>tol)
     STOP_TIMER("INTVEC:no_of_elements_larger_than")
   end function

   PURE subroutine sort(self,decreasing_order)
    INTVEC(:) :: self
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
     STOP_TIMER("INTVEC:sort")
   end subroutine

   subroutine quick_sort(self,decreasing_order)
    INTVEC(:) :: self
   ! Sort the vector into increasing order.If "decreasing_order" is present and
   ! TRUE, the vector is sorted from largest to smallest
      IN :: self
      BIN, optional, IN :: decreasing_order
   ! The following code is inherited from OBJECTVEC
      BIN :: decreasing
      STACK("INTVEC:quick_sort")
      START_TIMER("INTVEC:quick_sort")
      decreasing = FALSE
      if (present(decreasing_order)) decreasing = decreasing_order
      if (NOT decreasing) then; call quick_sort_increasing_(self)
      else;                     call quick_sort_decreasing_(self)
      end if
     STOP_TIMER("INTVEC:quick_sort")
      CHECK
   end subroutine

   recursive subroutine quick_sort_increasing(self)
    INTVEC(:) :: self
   ! Sort the vector into order, smallest to largest
   ! The following code is inherited from OBJECTVEC
      INTVEC(:), PTR :: smaller,larger
      INT :: n, ns, ne, nl
      INT :: chosen
      STACK("INTVEC:quick_sort_increasing")
      START_TIMER("INTVEC:quick_sort_increasing")
      if (size(self)<=1) then; STOP_TIMER("INTVEC:quick_sort_increasing") CHECK return; end if
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
     STOP_TIMER("INTVEC:quick_sort_increasing")
      CHECK
   end subroutine

   recursive subroutine quick_sort_decreasing(self)
    INTVEC(:) :: self
   ! Sort the vector into order, largest to smallest
   ! The following code is inherited from OBJECTVEC
      INTVEC(:), PTR :: smaller,larger
      INT :: n, ns, ne, nl
      INT :: chosen
      STACK("INTVEC:quick_sort_decreasing")
      START_TIMER("INTVEC:quick_sort_decreasing")
      if (size(self)<=1) then; STOP_TIMER("INTVEC:quick_sort_decreasing") CHECK return; end if
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
     STOP_TIMER("INTVEC:quick_sort_decreasing")
      CHECK
   end subroutine

   subroutine quick_sort_1(self,indices,decreasing_order)
    INTVEC(:) :: self
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
      STACK("INTVEC:quick_sort_1")
      START_TIMER("INTVEC:quick_sort_1")
      ENSURE(size(indices)==size(self),"INTVEC:quick_sort_1 ... wrong size, indices")
      decreasing = FALSE
      if (present(decreasing_order)) decreasing = decreasing_order
      indices = (/(i,i=1,size(self))/) ! initialise indices
      if (NOT decreasing) then; call quick_sort_increasing_(self,indices)
      else;                     call quick_sort_decreasing_(self,indices)
      end if
     STOP_TIMER("INTVEC:quick_sort_1")
      CHECK
   end subroutine

   recursive subroutine quick_sort_increasing_1(self,indices)
    INTVEC(:) :: self
   ! Return the indices which sort vector from smallest to largest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      IN :: self
      INTVEC(:), INOUT :: indices
   ! The following code is inherited from OBJECTVEC
      INTVEC(:), PTR :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n, i, ns, ne, nl
      INT :: chosen
      STACK("INTVEC:quick_sort_increasing_1")
      START_TIMER("INTVEC:quick_sort_increasing_1")
      if (size(indices)<=1) then; STOP_TIMER("INTVEC:quick_sort_increasing_1") CHECK return; end if
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
     STOP_TIMER("INTVEC:quick_sort_increasing_1")
      CHECK
   end subroutine

   recursive subroutine quick_sort_decreasing_1(self,indices)
    INTVEC(:) :: self
   ! Return the indices which sort vector from largest to smallest, i.e. on
   ! return "self(indices)" is sorted. NOTE: self is *not* sorted.
      IN :: self
      INTVEC(:), INOUT :: indices
   ! The following code is inherited from OBJECTVEC
      INTVEC(:), PTR :: list,small,equal,large,small_indices,equal_indices,large_indices
      INT :: n, i, ns, ne, nl
      INT :: chosen
      STACK("INTVEC:quick_sort_decreasing_1")
      START_TIMER("INTVEC:quick_sort_decreasing_1")
      if (size(indices)<=1) then; STOP_TIMER("INTVEC:quick_sort_decreasing_1") CHECK return; end if
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
     STOP_TIMER("INTVEC:quick_sort_decreasing_1")
      CHECK
   end subroutine

   function to_str(self,format,separator) result(string)
    INTVEC(:) :: self
   ! Change self to a "string" using default format.
     STR(*), optional :: format
     STR(*), optional :: separator
     STR(STR_SIZE) :: string
   ! The following code is inherited from OBJECTVEC
     STR(STR_SIZE) :: str1,str2
     INT :: n
     STACK("INTVEC:to_str")
     START_TIMER("INTVEC:to_str")
     string = " "
     do n = 1,size(self)
       str1 = to_str_(self(n),format)
       ENSURE(len_trim(string) + len_trim(str1) < len(string),"INTVEC:to_str ... string too long")
       if (present(separator)) then; str2 = trim(string) // separator // trim(str1)
       else;                         str2 = trim(string) //    " "    // trim(str1)
       end if
       string = str2
     end do
     STOP_TIMER("INTVEC:to_str")
      CHECK
   end function

   recursive function combinations_of_length(self,k) result(C)
    INTVEC(:) :: self
   ! Returns the combination matrix "C" of all p distinct combinations
   ! C(:,p) of the elements in self(:) of length "k"
     INT :: k
     INTMAT(:,:), PTR :: C
     INT :: s,s_k,s1_k1,s1_k
     INTMAT(:,:), PTR :: L,R
     STACK("INTVEC:combinations_of_length")
     START_TIMER("INTVEC:combinations_of_length")
     ENSURE(k<=size(self),"INTVEC:combinations_of_length ... k is too large")
     ENSURE(k>0,"INTVEC:combinations_of_length ... k must be positive")
     s     = size(self)
     s_k   = nint(    choose_(s,k),  kind=INT_KIND)
     s1_k  = nint(choose_((s-1),k),  kind=INT_KIND)
     s1_k1 = nint(choose_((s-1),k-1),kind=INT_KIND)
     allocate(C(k,s1_k))
     if (k==1) &
       C(1  ,:)             = self(:)
     if ((s>k) AND (k/=1)) then
       C(1  ,      1:s1_k1) = self(1)
       L                   => combinations_of_length_(self(2:),k-1)
       C(2:k,      1:s1_k1) = L
       deallocate(L)
       R                   => combinations_of_length_(self(2:),k)
       C(:  ,s1_k1+1: s_k ) = R
       deallocate(R)
     end if
     if (k==s) &
       C(:  ,1)             = self(:)
     STOP_TIMER("INTVEC:combinations_of_length")
      UNSTACK
   end function

   subroutine to_pair_vec_from(self,v1,v2)
    INTVEC(:) :: self
   ! If "v1" and "v2" are vectors which contain some common elements, set "self"
   ! so that self(i)=j implies that v1(i)=v2(j). Further, self(i)=0 that implies
   ! that the vector element i in "v1" is unpaired with any in "v2".
     INOUT :: self
     INTVEC(:) :: v1, v2
     INT :: i, j
   STACK("INTVEC:to_pair_vec_from")
   START_TIMER("INTVEC:to_pair_vec_from")
   ENSURE(size(v1)==size(v2),"INTVEC:to_pair_vec_from ... vectors are not compatible sizes")
     self = ZERO
     do i = 1, size(v1)
        do j = 1, size(v2)
           if ((v1(i)==v2(j)) AND (NOT(any(self==j)))) self(i)=j
        end do
     end do
     STOP_TIMER("INTVEC:to_pair_vec_from")
      CHECK
   end subroutine

   PURE function no_of_unique_elements(self) result(res)
    INTVEC(:) :: self
   ! Return the number of unique elements in the vector.
      IN :: self
      INT :: res
      INT :: n,i
      BIN :: same
      res = 1
      do n = 2,size(self)
         same = FALSE
         do i = 1,n-1
            if (self(n)==self(i)) then
               same = TRUE
               exit
            end if
         end do
         if (NOT same) res = res + 1
      end do
     STOP_TIMER("INTVEC:no_of_unique_elements")
   end function

   subroutine bin_XY_data(self,X,Y,bin_side_length)
    INTVEC(:) :: self
   ! Return a vector of the same length as "X" and "Y", whose k-th element
   ! contains the number of data points [X(k),Y(k)] which lie in the k-th bin.
   ! A bin is simply a range of values (a square, in fact) of side length
   ! "bin_side_length" covering the total set of data points from [X_min,Y_min]
   ! to [X_max,Y_max], where X_min and X_max are the minimum and maximum data
   ! values in X (and likewise for Y).
      REALVEC(:), IN :: X,Y
      REAL, IN :: bin_side_length
      REAL :: X_min,X_max,X_mid,X_ran
      REAL :: Y_min,Y_max,Y_mid,Y_ran
      REALVEC(2) :: X_range,Y_range
      INT :: n_X,n_Y,i,j,c,k
   STACK("INTVEC:bin_XY_data")
   START_TIMER("INTVEC:bin_XY_data")
   ENSURE(size(X)==size(self),"INTVEC:bin_XY_data ... incompatible data points")
   ENSURE(size(X)==size(Y),"INTVEC:bin_XY_data ... incompatible data points")
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
      do i = 1,n_X
      do j = 1,n_Y
         X_range(1) = X_min + (i-1)*bin_side_length
         X_range(2) = X_range(1)  + bin_side_length
         Y_range(1) = Y_min + (j-1)*bin_side_length
         Y_range(2) = Y_range(1)  + bin_side_length
         c = count(X_range(1)<=X AND X<=X_range(2) AND Y_range(1)<=Y AND Y<=Y_range(2))
         do k = 1,size(self)
            if (is_in_range_(X(k),X_range) AND is_in_range_(Y(k),Y_range)) then
            self(k) = c
            end if
         end do
      end do
      end do
     STOP_TIMER("INTVEC:bin_XY_data")
      CHECK
   end subroutine

end
