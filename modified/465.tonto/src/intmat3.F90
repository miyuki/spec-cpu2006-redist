!---------------------------------------------------------------------------
!
!  INTMAT3: 3 dimensional integer matrices
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
! $Id: intmat3.foo,v 1.4 2003/02/19 07:48:57 reaper Exp $
!---------------------------------------------------------------------------

module INTMAT3_MODULE

#  include "intmat3.use"

   implicit none

#  include "macros"
#  include "intmat3.int"


contains

   subroutine create(self,dim1,dim2,dim3)
    INTMAT3(:,:,:) :: self
   ! Create the object with the desired bounds
      PTR :: self
      INT, IN :: dim1,dim2,dim3
      STACK("INTMAT3:create")
      START_TIMER("INTMAT3:create")
      nullify(self)
      allocate(self(dim1,dim2,dim3))
      ADD_MEMORY(dim1*dim2*dim3*INT_SIZE)
     STOP_TIMER("INTMAT3:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,lb1,ub1,lb2,ub2,lb3,ub3)
    INTMAT3(:,:,:) :: self
   ! Create the object with the desired bounds
      PTR :: self
      INT, IN :: lb1,lb2,lb3,ub1,ub2,ub3
      STACK("INTMAT3:create_1")
      START_TIMER("INTMAT3:create_1")
      nullify(self)
      allocate(self(lb1:ub1,lb2:ub2,lb3:ub3))
      ADD_MEMORY((ub1-lb1+1)*(ub2-lb2+1)*(ub3-lb3+1)*INT_SIZE)
     STOP_TIMER("INTMAT3:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,bounds1,bounds2,bounds3)
    INTMAT3(:,:,:) :: self
   ! Create the object with the desired bounds
      PTR :: self
      INTVEC(2) :: bounds1,bounds2,bounds3
      STACK("INTMAT3:create_2")
      START_TIMER("INTMAT3:create_2")
      call create_(self,bounds1(1),bounds1(2),bounds2(1),bounds2(2),bounds3(1),bounds3(2))
     STOP_TIMER("INTMAT3:create_2")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTMAT3(:,:,:) :: self
   ! Destroy the object
      PTR :: self
      STACK("INTMAT3:destroy")
      START_TIMER("INTMAT3:destroy")
      DELETE_MEMORY(size(self)*INT_SIZE)
      deallocate(self)
     STOP_TIMER("INTMAT3:destroy")
      UNSTACK
   end subroutine

   PURE subroutine make_index_of_components(self,components)
    INTMAT3(:,:,:) :: self
   ! Returns the index matrix corresponding to the components.
   ! Each dimension of self is size(components,2).
     INTMAT(:,:), IN :: components
     OUT :: self
     INT :: ind,a,b,c
     do ind = 1,size(components,2)
       a = components(1,ind)
       b = components(2,ind)
       c = components(3,ind)
       self(a+1,b+1,c+1) = ind
     end do
     STOP_TIMER("INTMAT3:make_index_of_components")
   end subroutine

   subroutine to_gaussian_xyz_indices(self,l_max)
    INTMAT3(:,:,:) :: self
   ! Make "self", which maps the three defining xyz powers of each cartesian
   ! gaussian, for all gaussians up to angular momnetum "l_max", back to its
   ! lexical index *within a shell of the same angular momentum* i.e. not the
   ! total lexical index. NOTE: "self" has lower bounds of 0, and so is passed
   ! in as a pointer.
     PTR :: self
     INT, IN :: l_max
     INT :: L,k,a,b,c
   STACK("INTMAT3:to_gaussian_xyz_indices")
   START_TIMER("INTMAT3:to_gaussian_xyz_indices")
   ENSURE(lbound(self,1)==0,"INTMAT3:to_gaussian_xyz_indices ... wrong lower bound")
   ENSURE(lbound(self,2)==0,"INTMAT3:to_gaussian_xyz_indices ... wrong lower bound")
   ENSURE(lbound(self,3)==0,"INTMAT3:to_gaussian_xyz_indices ... wrong lower bound")
   ENSURE(ubound(self,1)==l_max,"INTMAT3:to_gaussian_xyz_indices ... wrong upper bound")
   ENSURE(ubound(self,2)==l_max,"INTMAT3:to_gaussian_xyz_indices ... wrong upper bound")
   ENSURE(ubound(self,3)==l_max,"INTMAT3:to_gaussian_xyz_indices ... wrong upper bound")
     do L = 0,l_max  ! Loop over all shells with momentum L
       k = 1         ! This is the local shell lexical index
                     ! Loop over powers a, b, c
       do a = L,floor((L+2)*THIRD),-1
         do b = min(L-a,a),floor((L-a+1)*HALF),-1
           c = L-a-b
           if (a==b AND b==c) then
             self(a,a,a) = k
             k = k+1
           else if (a>b AND b==c) then
             self(a,b,b) = k
             self(b,a,b) = k+1
             self(b,b,a) = k+2
             k = k+3
           else if (a==b AND b>c) then
             self(a,a,c) = k
             self(a,c,a) = k+1
             self(c,a,a) = k+2
             k = k+3
           else
             self(a,b,c) = k
             self(a,c,b) = k+1
             self(b,a,c) = k+2
             self(c,a,b) = k+3
             self(b,c,a) = k+4
             self(c,b,a) = k+5
             k = k+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INTMAT3:to_gaussian_xyz_indices")
      CHECK
   end subroutine

end
