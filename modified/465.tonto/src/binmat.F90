!---------------------------------------------------------------------------
!
!  BINMAT: Logical matrices
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
! $Id: binmat.foo,v 1.2 2003/02/19 07:48:56 reaper Exp $
!---------------------------------------------------------------------------

module BINMAT_MODULE

#  include "binmat.use"

   implicit none

#  include "macros"
#  include "binmat.int"


contains

   subroutine create(self,dim1,dim2)
    BINMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:create")
      START_TIMER("BINMAT:create")
      nullify(self)
      allocate(self(dim1,dim2))
      ADD_MEMORY(dim1*dim2*BIN_SIZE)
     STOP_TIMER("BINMAT:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2)
    BINMAT(:,:) :: self
   ! Create a matrix with the given dimensions
      PTR :: self
      INT, IN :: lb1,ub1,lb2,ub2
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:create_1")
      START_TIMER("BINMAT:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*BIN_SIZE)
     STOP_TIMER("BINMAT:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2)
    BINMAT(:,:) :: self
   ! Create a matrix with the specified bounds for each dimension
      PTR :: self
      INTVEC(:), IN :: bounds1,bounds2
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:create_2")
      START_TIMER("BINMAT:create_2")
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2))
     STOP_TIMER("BINMAT:create_2")
      UNSTACK
   end subroutine

   subroutine create_copy(self,matrix)
    BINMAT(:,:) :: self
   ! Create a replica copy of matrix
      PTR :: self
      BINMAT(:,:), IN :: matrix
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:create_copy")
      START_TIMER("BINMAT:create_copy")
      call create_(self,lbound(matrix,1),ubound(matrix,1), &
              lbound(matrix,2),ubound(matrix,2)  )
      self = matrix
     STOP_TIMER("BINMAT:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    BINMAT(:,:) :: self
   ! Destroy the object
      PTR :: self
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:destroy")
      START_TIMER("BINMAT:destroy")
      if (NOT associated(self)) then; STOP_TIMER("BINMAT:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*BIN_SIZE)
      deallocate(self)
     STOP_TIMER("BINMAT:destroy")
      UNSTACK
   end subroutine

   function is_square(self) result(res)
    BINMAT(:,:) :: self
   ! Returns TRUE if the matrix is square
      IN :: self
      BIN :: res
      STACK("BINMAT:is_square")
      START_TIMER("BINMAT:is_square")
      res = size(self,1)==size(self,2)
   ! The following code is inherited from INTRINSICMAT
     STOP_TIMER("BINMAT:is_square")
      CHECK
   end function

   function is_same_shape_as(self,b) result(res)
    BINMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" has the same shape as self
      IN :: self
      BINMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:is_same_shape_as")
      START_TIMER("BINMAT:is_same_shape_as")
      res = size(self,1)==size(b,1) AND size(self,2)==size(b,2)
     STOP_TIMER("BINMAT:is_same_shape_as")
      CHECK
   end function

   function is_transposed_shape_of(self,b) result(res)
    BINMAT(:,:) :: self
   ! Returns TRUE if the matrix "b" is the transposed shape of self
      IN :: self
      BINMAT(:,:), IN :: b
      BIN :: res
   ! The following code is inherited from INTRINSICMAT
      STACK("BINMAT:is_transposed_shape_of")
      START_TIMER("BINMAT:is_transposed_shape_of")
      res = size(self,1)==size(b,2) AND size(self,2)==size(b,1)
     STOP_TIMER("BINMAT:is_transposed_shape_of")
      CHECK
   end function

   subroutine shrink(self,dim1,dim2)
    BINMAT(:,:) :: self
   ! Shrinks self to dimension dim1xdim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
     BINMAT(:,:), PTR :: old
     STACK("BINMAT:shrink")
     START_TIMER("BINMAT:shrink")
     ENSURE(associated(self),"BINMAT:shrink ... matrix not allocated")
     ENSURE(dim1<=size(self,1),"BINMAT:shrink ... 1st dimension given is too large.")
     ENSURE(dim2<=size(self,2),"BINMAT:shrink ... 2nd dimension given is too large.")
     if (dim1==size(self,1) AND dim2==size(self,2)) then; STOP_TIMER("BINMAT:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim1,dim2)
     self=old(1:dim1,1:dim2)
     call destroy_(old)
     STOP_TIMER("BINMAT:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim1,dim2)
    BINMAT(:,:) :: self
   ! Expands self to dimension dim1xdim2.  Contents are retained.
     PTR :: self
     INT, IN :: dim1,dim2
   ! The following code is inherited from INTRINSICMAT
     BINMAT(:,:), PTR :: old
     INT :: old_size1,old_size2
     STACK("BINMAT:expand")
     START_TIMER("BINMAT:expand")
     if (NOT associated(self)) then
       call create_(self,0,0)
     else
     ENSURE(dim1>=size(self,1),"BINMAT:expand ... 1st dimension given is too small")
     ENSURE(dim2>=size(self,2),"BINMAT:expand ... 2nd dimension given is too small")
     end if
     old => self
     old_size1 = size(old,1)
     old_size2 = size(old,2)
     nullify(self)
     call create_(self,dim1,dim2)
     self(1:old_size1,1:old_size2)=old
     call destroy_(old)
     STOP_TIMER("BINMAT:expand")
      UNSTACK
   end subroutine

end
