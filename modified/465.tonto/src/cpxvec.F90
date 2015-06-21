!---------------------------------------------------------------------------
!
!  CPXVEC: Complex vector operations ...
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
! $Id: cpxvec.foo,v 1.7.2.2 2003/11/13 05:34:39 reaper Exp $
!---------------------------------------------------------------------------

module CPXVEC_MODULE

#  include "cpxvec.use"

   implicit none

#  include "macros"
#  include "cpxvec.int"


contains

   subroutine create(self,dim)
    CPXVEC(:) :: self
   ! Create space for object
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      INT, IN :: dim
      STACK("CPXVEC:create")
      START_TIMER("CPXVEC:create")
      ENSURE(dim>=0,"CPXVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*CPX_SIZE)
     STOP_TIMER("CPXVEC:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    CPXVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      STACK("CPXVEC:destroy")
      START_TIMER("CPXVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("CPXVEC:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*CPX_SIZE)
      deallocate(self)
     STOP_TIMER("CPXVEC:destroy")
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
    CPXVEC(:) :: self
   ! Create a replica copy of vec.
      CPXVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      STACK("CPXVEC:create_copy")
      START_TIMER("CPXVEC:create_copy")
      call create_(self,size(vec))
      self = vec
     STOP_TIMER("CPXVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    CPXVEC(:) :: self
   ! Copy "vec".
      CPXVEC(:), IN :: vec
   ! The following code is inherited from INTRINSICVEC
      STACK("CPXVEC:copy")
      START_TIMER("CPXVEC:copy")
      ENSURE(size(self)==size(vec),"CPXVEC:copy ... vec size does not match")
      self = vec
     STOP_TIMER("CPXVEC:copy")
      UNSTACK
   end subroutine

   function dim(self) result(res)
    CPXVEC(:) :: self
   ! Returns the size of self as a one dimensional array
      INT :: res
      STACK("CPXVEC:dim")
      START_TIMER("CPXVEC:dim")
      res = size(self)
     STOP_TIMER("CPXVEC:dim")
      CHECK
   end function

   subroutine shrink(self,dim)
    CPXVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     CPXVEC(:), PTR :: old
     INT :: n
     STACK("CPXVEC:shrink")
     START_TIMER("CPXVEC:shrink")
     ENSURE(associated(self),"CPXVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"CPXVEC:shrink ... dim too large")
     if (dim==size(self)) then; STOP_TIMER("CPXVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       self(n) = old(n)
     end do
     call destroy_(old)
     STOP_TIMER("CPXVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    CPXVEC(:) :: self
   ! Expand self to dimension dim. New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     CPXVEC(:), PTR :: old
     INT :: old_dim
     STACK("CPXVEC:expand")
     START_TIMER("CPXVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"CPXVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if
     STOP_TIMER("CPXVEC:expand")
      UNSTACK
   end subroutine

   function join(self,v) result(res)
    CPXVEC(:) :: self
   ! Yield a vector which is the concatenation of "self" and "v"
     CPXVEC(:), IN :: v
     CPXVEC(:), PTR :: res
   ! The following code is inherited from INTRINSICVEC
     INT :: dim, dim_v
     STACK("CPXVEC:join")
     START_TIMER("CPXVEC:join")
     dim   = size(self)
     dim_v = size(v)
     call create_(res,dim+dim_v)
     res(    1:dim      ) = self
     res(dim+1:dim+dim_v) = v
     STOP_TIMER("CPXVEC:join")
      UNSTACK
   end function

   function join_1(self,v1,v2) result(res)
    CPXVEC(:) :: self
   ! Yield a vector which is the concatenation of "self" and "v1" and "v2"
     CPXVEC(:), IN :: v1,v2
     CPXVEC(:), PTR :: res
   ! The following code is inherited from INTRINSICVEC
     INT :: dim, dim_v1, dim_v2
     STACK("CPXVEC:join_1")
     START_TIMER("CPXVEC:join_1")
     dim    = size(self)
     dim_v1 = size(v1)
     dim_v2 = size(v2)
     call create_(res,dim+dim_v1+dim_v2)
     res(           1:dim              ) = self
     res(dim+       1:dim+dim_v1       ) = v1
     res(dim+dim_v1+1:dim+dim_v1+dim_v2) = v2
     STOP_TIMER("CPXVEC:join_1")
      UNSTACK
   end function

   subroutine to_product_of(self,a,b,dagger_a)
    CPXVEC(:) :: self
   ! Set "self" to the product of matrix "a" and vector "b". If present,
   ! "dagger_a" can be set to TRUE if "a" needs to be daggered.
       CPXMAT(:,:), IN :: a
       CPXVEC(:), IN :: b
      BIN, optional :: dagger_a
      INT :: a1,a2, i,j
      BIN :: ta
      STACK("CPXVEC:to_product_of")
      START_TIMER("CPXVEC:to_product_of")
      a1 = size(a,1); a2 = size(a,2)
      ta = FALSE
      if (present(dagger_a)) ta = dagger_a
      if (ta) then
         self = ZERO
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + conjg(a(j,i))*b(j)
         end do
         end do
      else
         self = ZERO
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if
     STOP_TIMER("CPXVEC:to_product_of")
      CHECK
   end subroutine

   subroutine to_product_of_1(self,a,b,transpose_a)
    CPXVEC(:) :: self
   ! Set "self" to the product of matrix "a" and vector "b". If present,
   ! "transpose_a" can be set to TRUE if "a" needs to be transposed.
       REALMAT(:,:), IN :: a
       CPXVEC(:), IN :: b
      BIN, optional :: transpose_a
      INT :: a1,a2, i,j
      BIN :: ta
      STACK("CPXVEC:to_product_of_1")
      START_TIMER("CPXVEC:to_product_of_1")
      a1 = size(a,1); a2 = size(a,2)
      ta = FALSE
      if (present(transpose_a)) ta = transpose_a
      if (ta) then
         self = ZERO
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + a(j,i)*b(j)
         end do
         end do
      else
         self = ZERO
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if
     STOP_TIMER("CPXVEC:to_product_of_1")
      CHECK
   end subroutine

   subroutine plus_product_of(self,a,b,dagger_a)
    CPXVEC(:) :: self
   ! Set "self" to the product of matrix "a" and vector "b". If present,
   ! "dagger_a" can be set to TRUE if "a" needs to be daggered.
       CPXMAT(:,:), IN :: a
       CPXVEC(:), IN :: b
      BIN, optional :: dagger_a
      INT :: a1,a2, i,j
      BIN :: ta
      STACK("CPXVEC:plus_product_of")
      START_TIMER("CPXVEC:plus_product_of")
      a1 = size(a,1); a2 = size(a,2)
      ta = FALSE
      if (present(dagger_a)) ta = dagger_a
      if (ta) then
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + conjg(a(j,i))*b(j)
         end do
         end do
      else
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if
     STOP_TIMER("CPXVEC:plus_product_of")
      CHECK
   end subroutine

   subroutine plus_product_of_1(self,a,b,transpose_a)
    CPXVEC(:) :: self
   ! Set "self" to the product of matrix "a" and vector "b". If present,
   ! "transpose_a" can be set to TRUE if "a" needs to be transposed.
       REALMAT(:,:), IN :: a
       CPXVEC(:), IN :: b
      BIN, optional :: transpose_a
      INT :: a1,a2, i,j
      BIN :: ta
      STACK("CPXVEC:plus_product_of_1")
      START_TIMER("CPXVEC:plus_product_of_1")
      a1 = size(a,1); a2 = size(a,2)
      ta = FALSE
      if (present(transpose_a)) ta = transpose_a
      if (ta) then
         do i = 1,a2
         do j = 1,a1
            self(i) = self(i) + a(j,i)*b(j)
         end do
         end do
      else
         do i = 1,a1
         do j = 1,a2
            self(i) = self(i) + a(i,j)*b(j)
         end do
         end do
      end if
     STOP_TIMER("CPXVEC:plus_product_of_1")
      CHECK
   end subroutine

   PURE function dot(self,b) result(res)
    CPXVEC(:) :: self
   ! Return the dot product with "b"
      IN :: self
      CPXVEC(:), IN :: b
      CPX :: res
   ! The following code is inherited from INTRINSICVEC
      res = dot_product(self,b)
     STOP_TIMER("CPXVEC:dot")
   end function

   PURE subroutine to_cross_product(self,a,b)
    CPXVEC(:) :: self
   ! Set the vector to the cross product of "a" and "b".
      INOUT :: self
      CPXVEC(:), IN :: a,b
   ! The following code is inherited from INTRINSICVEC
      
      self(1) = a(2)*b(3) - b(2)*a(3)
      self(2) = a(3)*b(1) - b(3)*a(1)
      self(3) = a(1)*b(2) - b(1)*a(2)
     STOP_TIMER("CPXVEC:to_cross_product")
   end subroutine

   PURE function norm(self) result(val)
    CPXVEC(:) :: self
   ! Return the norm of self
      IN :: self
      CPX :: val
       INT :: i
      val = ZERO
      do i=1,size(self)
        val = val + self(i)*self(i)
      end do
      val = sqrt(val)
     STOP_TIMER("CPXVEC:norm")
   end function

   PURE subroutine chop_large_values(self,val)
    CPXVEC(:) :: self
   ! Set all values in the self whose absolute value is larger than "val" to
   ! "val" times the sign of the original value.
      INOUT :: self
      REAL, IN :: val
      INT :: dim,i
      REAL :: ba
      CPX :: bb,phase
      dim = size(self)
      do i = 1,dim
         bb = self(i)
         ba = abs(bb)
         if (ba==ZERO) then
            self(i) = ZERO
         else
            phase = bb/ba
            self(i) = phase*min(val,ba)
         end if
     end do
     STOP_TIMER("CPXVEC:chop_large_values")
   end subroutine

   PURE function largest_value(self) result(maxv)
    CPXVEC(:) :: self
   ! Return the largest value in self
      IN :: self
      REAL :: maxv
      REAL :: bb
      INT :: i
      maxv = abs(self(1))
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb > maxv) maxv = bb
      end do
!      res = dzamax(size(self),self,1) ! blas library
     STOP_TIMER("CPXVEC:largest_value")
   end function

   PURE function smallest_value(self) result(minv)
    CPXVEC(:) :: self
   ! Return the smallest value in self
      IN :: self
      REAL :: minv
      REAL :: bb
      INT :: i
      minv = abs(self(1))
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb < minv) minv = bb
      end do
!      res = dzamin(size(self),self,1) ! blas library
     STOP_TIMER("CPXVEC:smallest_value")
   end function

   PURE function index_of_largest_value(self) result(ind)
    CPXVEC(:) :: self
   ! Return the index of the largest value in self
      IN :: self
      INT :: ind
      INT :: i
      REAL :: maxv,bb
      maxv = abs(self(1))
      ind = 1
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb > maxv) then
          maxv = bb
          ind = i
        end if
      end do
!      ind = izamax(size(self),self,1) ! blas library
     STOP_TIMER("CPXVEC:index_of_largest_value")
   end function

   PURE function index_of_smallest_value(self) result(ind)
    CPXVEC(:) :: self
   ! Return the index of the smallest value in self
      IN :: self
      INT :: ind
      INT :: i
      REAL :: minv,bb
      minv = abs(self(1))
      ind = 1
      do i = 2,size(self)
        bb = abs(self(i))
        if (bb < minv) then
          minv = bb
          ind = i
        end if
      end do
!      ind = izamin(size(self),self,1) ! blas library
     STOP_TIMER("CPXVEC:index_of_smallest_value")
   end function

   function alpha(self) result(res)
    CPXVEC(:) :: self
   ! return the alpha sector of the vector
      TARGET :: self
      CPXVEC(:), PTR :: res
       INT :: n
   STACK("CPXVEC:alpha")
   START_TIMER("CPXVEC:alpha")
   ENSURE(is_even_(size(self)),"CPXVEC:alpha ... self is not even-dimensioned")
      n = size(self)/2
      res => self(1:n)
     STOP_TIMER("CPXVEC:alpha")
      CHECK
   end function

   function beta(self) result(res)
    CPXVEC(:) :: self
   ! return the beta sector of the vector
      TARGET :: self
      CPXVEC(:), PTR :: res
       INT :: n
   STACK("CPXVEC:beta")
   START_TIMER("CPXVEC:beta")
   ENSURE(is_even_(size(self)),"CPXVEC:beta ... self is not even-dimensioned")
      n = size(self)/2
      res => self(n+1:2*n)
     STOP_TIMER("CPXVEC:beta")
      CHECK
   end function

   subroutine set_alpha(self,X)
    CPXVEC(:) :: self
   ! Set the alpha sector of the vector
       CPXVEC(:) :: X
       INT :: n
   STACK("CPXVEC:set_alpha")
   START_TIMER("CPXVEC:set_alpha")
   ENSURE(is_even_(size(self)),"CPXVEC:set_alpha ... self is not even-dimensioned")
      n = size(self)/2
      self(1:n) = X
     STOP_TIMER("CPXVEC:set_alpha")
      CHECK
   end subroutine

   subroutine set_beta(self,X)
    CPXVEC(:) :: self
   ! Set the beta sector of the vector
       CPXVEC(:) :: X
       INT :: n
   STACK("CPXVEC:set_beta")
   START_TIMER("CPXVEC:set_beta")
   ENSURE(is_even_(size(self)),"CPXVEC:set_beta ... self is not even-dimensioned")
      n = size(self)/2
      self(n+1:2*n) = X
     STOP_TIMER("CPXVEC:set_beta")
      CHECK
   end subroutine

   subroutine set_alpha_1(self,X)
    CPXVEC(:) :: self
   ! Set the alpha sector of the vector
       REALVEC(:) :: X
       INT :: n
   STACK("CPXVEC:set_alpha_1")
   START_TIMER("CPXVEC:set_alpha_1")
   ENSURE(is_even_(size(self)),"CPXVEC:set_alpha_1 ... self is not even-dimensioned")
      n = size(self)/2
      self(1:n) = X
     STOP_TIMER("CPXVEC:set_alpha_1")
      CHECK
   end subroutine

   subroutine set_beta_1(self,X)
    CPXVEC(:) :: self
   ! Set the beta sector of the vector
       REALVEC(:) :: X
       INT :: n
   STACK("CPXVEC:set_beta_1")
   START_TIMER("CPXVEC:set_beta_1")
   ENSURE(is_even_(size(self)),"CPXVEC:set_beta_1 ... self is not even-dimensioned")
      n = size(self)/2
      self(n+1:2*n) = X
     STOP_TIMER("CPXVEC:set_beta_1")
      CHECK
   end subroutine

end
