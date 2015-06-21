!---------------------------------------------------------------------------
!
!  INTVECVEC : a matrix where each column is (possibly) a different length
!
! Copyright (C) Daniel Grimwood, 1998
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
! $Id: intvecvec.foo,v 1.4.2.3 2003/11/13 05:35:09 reaper Exp $
!---------------------------------------------------------------------------

module INTVECVEC_MODULE

#  include "intvecvec.use"

   implicit none

#  include "macros"
#  include "intvecvec.int"


contains

   subroutine create(self,dim)
    INTVECVEC(:) :: self
   ! Create space for a vector of integer vectors
      PTR :: self
      INT :: dim
      STACK("INTVECVEC:create")
      START_TIMER("INTVECVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*PTR_SIZE)
      call nullify_ptr_part_(self)
     STOP_TIMER("INTVECVEC:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,dim,dimv)
    INTVECVEC(:) :: self
   ! Create space for a vector of integer vectors
      PTR :: self
      INT :: dim,dimv
      STACK("INTVECVEC:create_1")
      START_TIMER("INTVECVEC:create_1")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*PTR_SIZE)
      call nullify_ptr_part_(self)
      call create_vec_(self,dimv)
     STOP_TIMER("INTVECVEC:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,dim,dimv)
    INTVECVEC(:) :: self
   ! Create space for a vector of integer vectors
      PTR :: self
      INT :: dim
      INTVEC(2) :: dimv
      STACK("INTVECVEC:create_2")
      START_TIMER("INTVECVEC:create_2")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*PTR_SIZE)
      call nullify_ptr_part_(self)
      call create_vec_(self,dimv)
     STOP_TIMER("INTVECVEC:create_2")
      UNSTACK
   end subroutine

   subroutine create_vec(self,dimv)
    INTVECVEC(:) :: self
   ! Allocate the "vec" pointer parts
      INT :: dimv
      INT :: i
      STACK("INTVECVEC:create_vec")
      START_TIMER("INTVECVEC:create_vec")
      do i = 1,size(self)
         call create_(self(i)%element,dimv)
      end do
     STOP_TIMER("INTVECVEC:create_vec")
      UNSTACK
   end subroutine

   subroutine create_vec_1(self,dimv)
    INTVECVEC(:) :: self
   ! Allocate the "vec" pointer parts
      INTVEC(2) :: dimv
      INT :: i
      STACK("INTVECVEC:create_vec_1")
      START_TIMER("INTVECVEC:create_vec_1")
      do i = 1,size(self)
         call create_(self(i)%element,dimv)
      end do
     STOP_TIMER("INTVECVEC:create_vec_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTVECVEC(:) :: self
   ! Destroy allocated space for ivec vector
      PTR :: self
      STACK("INTVECVEC:destroy")
      START_TIMER("INTVECVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("INTVECVEC:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(size(self)*PTR_SIZE)
      deallocate(self)
     STOP_TIMER("INTVECVEC:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    INTVECVEC(:) :: self
   ! Nullify the pointer parts of an ivecvec
      PTR :: self
      INT :: s
      STACK("INTVECVEC:nullify_ptr_part")
      START_TIMER("INTVECVEC:nullify_ptr_part")
      do s = 1,size(self)
         nullify(self(s)%element)
      end do
     STOP_TIMER("INTVECVEC:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    INTVECVEC(:) :: self
   ! Destroy the pointer parts of an ivecvec
      PTR :: self
       INT :: s
      STACK("INTVECVEC:destroy_ptr_part")
      START_TIMER("INTVECVEC:destroy_ptr_part")
      do s = 1,size(self)
         call destroy_(self(s)%element)
      end do
     STOP_TIMER("INTVECVEC:destroy_ptr_part")
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
    INTVECVEC(:) :: self
   ! Create a replica copy of "vec".
      INTVECVEC(:), IN :: vec
      PTR :: self
   ! The following code is inherited from OBJECTVEC
      STACK("INTVECVEC:create_copy")
      START_TIMER("INTVECVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("INTVECVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,v)
    INTVECVEC(:) :: self
   ! Make a copy of "v".
      INTVECVEC(:) :: v
      INT :: i
      STACK("INTVECVEC:copy")
      START_TIMER("INTVECVEC:copy")
      ENSURE(size(self)==size(v),"INTVECVEC:copy ... v has incompatible size")
      do i = 1,size(v)
         call create_copy_(self(i)%element,v(i)%element)
      end do
     STOP_TIMER("INTVECVEC:copy")
      UNSTACK
   end subroutine

   subroutine shrink(self,dim)
    INTVECVEC(:) :: self
   ! Shrinks self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
     INT :: i,old_size
     INTVECVEC(:), PTR :: old
     STACK("INTVECVEC:shrink")
     START_TIMER("INTVECVEC:shrink")
     ENSURE(associated(self),"INTVECVEC:shrink ... no self array")
     ENSURE(dim<=size(self),"INTVECVEC:shrink ... dim not small enough")
     if (dim==size(self)) then; STOP_TIMER("INTVECVEC:shrink") UNSTACK return; end if
     old => self
     old_size = size(old)
     nullify(self)
     call create_(self,dim)
     do i = 1,dim
        self(i)%element => old(i)%element
        nullify(old(i)%element)
     end do
     do i = dim+1,old_size
        call destroy_(old(i)%element)
     end do
     call destroy_(old)
     STOP_TIMER("INTVECVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    INTVECVEC(:) :: self
   ! Expands self to dimension dim.  Contents are retained.
   ! Note: pointer assignment is used to retain contents.
   ! Note: Elements which are added are nullified.
     PTR :: self
     INT, IN :: dim
     INTVECVEC(:), PTR :: old
     INT :: old_size,i
     STACK("INTVECVEC:expand")
     START_TIMER("INTVECVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
     else
        ENSURE(dim>=size(self),"INTVECVEC:expand ... dim not large enough")
        old => self
        old_size = size(old)
        nullify(self)
        call create_(self,dim)
        do i = 1,old_size
           self(i)%element => old(i)%element
           nullify(old(i)%element)
        end do
        call destroy_(old)
        do i = old_size+1,dim
           nullify(self(i)%element)
        end do
     end if
     STOP_TIMER("INTVECVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    INTVECVEC(:) :: self
   ! Expands self to the required dimension, and append the contents
   ! of encapsulated vector "v". Note: appended element parts are pointer
   ! assigned, not copied.
     PTR :: self
     INTVECVEC(:), IN :: v
     INT :: dim,i
     STACK("INTVECVEC:append")
     START_TIMER("INTVECVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     do i = 1,size(v)
        self(dim+i)%element => v(i)%element
     end do
     STOP_TIMER("INTVECVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    INTVECVEC(:) :: self
   ! Expands self to the required dimension, and append the single
   ! INTVEC "value" onto the end of self.
     PTR :: self
     INTVEC(:), IN :: value
     INT :: dim
     STACK("INTVECVEC:append_1")
     START_TIMER("INTVECVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     call append_(self(dim+1)%element,value)
     STOP_TIMER("INTVECVEC:append_1")
      UNSTACK
   end subroutine

   function join(self,list) result(res)
    INTVECVEC(:) :: self
   ! Join together the vectors in self whose indices are listed in "list".
      INTVEC(:) :: list
      INTVEC(:), PTR :: res
      INTVEC(:), PTR :: temp
      INT :: i,l
      STACK("INTVECVEC:join")
      START_TIMER("INTVECVEC:join")
      ENSURE(size(list)>=1,"INTVECVEC:join ... list must not have zero dimension")
      ENSURE(maxval(list)<=size(self),"INTVECVEC:join ... some elements of list are too large")
      ENSURE(minval(list)>=1,"INTVECVEC:join ... list elements must be +ve")
      l = list(1)
      call create_copy_(res,self(l)%element)
      do i = 2,size(list)
         l = list(i)
         temp => join_(res,self(l)%element)
         call destroy_(res)
         res => temp
      end do
     STOP_TIMER("INTVECVEC:join")
      UNSTACK
   end function

end
