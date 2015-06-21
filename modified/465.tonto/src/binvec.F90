!---------------------------------------------------------------------------
!
! BINVEC : Boolean vector operations ...
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
! $Id: binvec.foo,v 1.6.2.1 2003/03/24 01:28:52 dylan Exp $
!
!---------------------------------------------------------------------------

module BINVEC_MODULE

#  include "binvec.use"

  implicit none

#  include "macros"
#  include "binvec.int"


contains

   subroutine create(self,dim)
    BINVEC(:) :: self
   ! Create space for object
      PTR :: self
      INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
      STACK("BINVEC:create")
      START_TIMER("BINVEC:create")
      ENSURE(dim>=0,"BINVEC:create ... dimension of array not 1 or greater")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*BIN_SIZE)
     STOP_TIMER("BINVEC:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb,ub)
    BINVEC(:) :: self
   ! Create the vector with lower bound "lb", upper bound "ub"
      PTR :: self
      INT, IN :: lb,ub
   ! The following code is inherited from INTRINSICVEC
      STACK("BINVEC:create_1")
      START_TIMER("BINVEC:create_1")
      nullify(self)
      allocate(self(lb:ub))
      ADD_MEMORY((ub-lb+1)*BIN_SIZE)
     STOP_TIMER("BINVEC:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds)
    BINVEC(:) :: self
   ! Create the vector with "bounds"
      PTR :: self
      INTVEC(2), IN :: bounds
   ! The following code is inherited from OBJECTVEC
      STACK("BINVEC:create_2")
      START_TIMER("BINVEC:create_2")
      call create_(self,bounds(1),bounds(2))
     STOP_TIMER("BINVEC:create_2")
      UNSTACK
   end subroutine

   subroutine create_copy(self,vec)
    BINVEC(:) :: self
   ! Create a replica copy of vec.
      BINVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      STACK("BINVEC:create_copy")
      START_TIMER("BINVEC:create_copy")
      call create_(self,size(vec))
      self = vec
     STOP_TIMER("BINVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    BINVEC(:) :: self
   ! Copy "vec".
      BINVEC(:), IN :: vec
   ! The following code is inherited from INTRINSICVEC
      STACK("BINVEC:copy")
      START_TIMER("BINVEC:copy")
      ENSURE(size(self)==size(vec),"BINVEC:copy ... vec size does not match")
      self = vec
     STOP_TIMER("BINVEC:copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    BINVEC(:) :: self
   ! Destroy space for object
      PTR :: self
   ! The following code is inherited from INTRINSICVEC
      STACK("BINVEC:destroy")
      START_TIMER("BINVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("BINVEC:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*BIN_SIZE)
      deallocate(self)
     STOP_TIMER("BINVEC:destroy")
      UNSTACK
   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!     self :: PTR
!     res :: BIN
!     res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!     self :: PTR
!     res :: BIN
!     res = NOT associated(self)
!   end

   function n_true(self) result(res)
    BINVEC(:) :: self
   ! Returns the number of true items in self.
     INT :: res
     STACK("BINVEC:n_true")
     START_TIMER("BINVEC:n_true")
     res = count(self)
     STOP_TIMER("BINVEC:n_true")
      CHECK
   end function

   function n_false(self) result(res)
    BINVEC(:) :: self
   ! Returns the number of false items in self.
     INT :: res
     STACK("BINVEC:n_false")
     START_TIMER("BINVEC:n_false")
     res = size(self) - count(self)
     STOP_TIMER("BINVEC:n_false")
      CHECK
   end function

   function index_of_first_true_element(self) result(res)
    BINVEC(:) :: self
   ! Returns the index of the first true element in self, or zero if there is no
   ! true element.
     INT :: res
     INT :: i
     STACK("BINVEC:index_of_first_true_element")
     START_TIMER("BINVEC:index_of_first_true_element")
     res = 0
     do i = 1,size(self)
        if (self(i)) then
           res = i
           exit
        end if
     end do
     STOP_TIMER("BINVEC:index_of_first_true_element")
      CHECK
   end function

   subroutine shrink(self,dim)
    BINVEC(:) :: self
   ! Shrink self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     BINVEC(:), PTR :: old
     INT :: n
     STACK("BINVEC:shrink")
     START_TIMER("BINVEC:shrink")
     ENSURE(associated(self),"BINVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"BINVEC:shrink ... dim too large")
     if (dim==size(self)) then; STOP_TIMER("BINVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     do n=1,dim
       self(n) = old(n)
     end do
     call destroy_(old)
     STOP_TIMER("BINVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    BINVEC(:) :: self
   ! Expand self to dimension dim. New slots are left undefined.
     PTR :: self
     INT, IN :: dim
   ! The following code is inherited from INTRINSICVEC
     BINVEC(:), PTR :: old
     INT :: old_dim
     STACK("BINVEC:expand")
     START_TIMER("BINVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"BINVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(1:old_dim) = old
        call destroy_(old)
     end if
     STOP_TIMER("BINVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    BINVEC(:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     BINVEC(:), IN :: v
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("BINVEC:append")
     START_TIMER("BINVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v
     STOP_TIMER("BINVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    BINVEC(:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     BIN, IN :: value
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("BINVEC:append_1")
     START_TIMER("BINVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value
     STOP_TIMER("BINVEC:append_1")
      UNSTACK
   end subroutine

end
