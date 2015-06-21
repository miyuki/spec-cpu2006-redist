!---------------------------------------------------------------------------
!
!  INT: methods which apply to integers
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
! $Id: int.foo,v 1.16.2.1 2003/11/13 05:35:09 reaper Exp $
!---------------------------------------------------------------------------

module INT_MODULE

#  include "int.use"

   implicit none

#  include "macros"
#  include "int.int"


contains

   subroutine plus(self,val)
    INT :: self
   ! Add "val" to self
      INOUT :: self
      INT :: val
      STACK("INT:plus")
      START_TIMER("INT:plus")
      self = self + val
     STOP_TIMER("INT:plus")
      CHECK
   end subroutine

   subroutine minus(self,val)
    INT :: self
   ! Subtract "val" to self
      INOUT :: self
      INT :: val
      STACK("INT:minus")
      START_TIMER("INT:minus")
      self = self - val
     STOP_TIMER("INT:minus")
      CHECK
   end subroutine

   subroutine times(self,val)
    INT :: self
   ! Multiply "val" by self
      INOUT :: self
      INT :: val
      STACK("INT:times")
      START_TIMER("INT:times")
      self = self * val
     STOP_TIMER("INT:times")
      CHECK
   end subroutine

   function is_even(self) result(res)
    INT :: self
   ! Return TRUE is self is an even number
      BIN :: res
      STACK("INT:is_even")
      START_TIMER("INT:is_even")
      res = mod(self,2)==0
     STOP_TIMER("INT:is_even")
      CHECK
   end function

   function is_odd(self) result(res)
    INT :: self
   ! Return TRUE is self is an odd number
      BIN :: res
      STACK("INT:is_odd")
      START_TIMER("INT:is_odd")
      res = mod(self,2)==1
     STOP_TIMER("INT:is_odd")
      CHECK
   end function

   function is_in_range(self,range) result(res)
    INT :: self
   ! Return TRUE if self is within the specified "range".
      INTVEC(2) :: range
      BIN :: res
      STACK("INT:is_in_range")
      START_TIMER("INT:is_in_range")
      res = range(1) <= self AND self <= range(2)
     STOP_TIMER("INT:is_in_range")
      CHECK
   end function

   function to_str(self,format) result(string)
    INT :: self
   ! Change self to a "string" using specified fortran "format".
      STR(*), optional :: format
      STR(STR_SIZE) :: string
      STACK("INT:to_str")
      START_TIMER("INT:to_str")
      string = " "
      if (present(format)) then; write(string,fmt="("//trim(format)//")") self
      else;                      write(string,fmt=*) self
      end if
      call left_justify_(string)
     STOP_TIMER("INT:to_str")
      CHECK
   end function

   function delta(self,j) result(res)
    INT :: self
   ! Kronecker delta function. Returns 1 if self==j, otherwise 0.
      IN :: self
      INT, IN :: j
      INT :: res
      STACK("INT:delta")
      START_TIMER("INT:delta")
      if (self==j) then; res = 1
      else;              res = 0
      end if
     STOP_TIMER("INT:delta")
      CHECK
   end function

   function raised_to(self,n) result(res)
    INT :: self
   ! Raise "self" to the power "n"
       INT :: n
      REAL :: res
       INT :: i
      STACK("INT:raised_to")
      START_TIMER("INT:raised_to")
      res = ONE
      do i = 1,abs(n)
         res = res*self
      end do
      if (n<0) res = ONE/res
     STOP_TIMER("INT:raised_to")
      CHECK
   end function

   ALWAYS_ELEMENTAL function factorial(self) result(res)
    INT :: self
   ! Return the factorial of the integer
   ! res cannot be int, as int(4) dies as 13!.
      IN :: self
      REAL :: res
       INT :: i
   !  ENSURE(self>=0,"must have non-negative argument")
      select case (self)
         case (0); res=1.0d0
         case (1); res=1.0d0
         case (2); res=2.0d0
         case (3); res=6.0d0
         case (4); res=24.0d0
         case (5); res=120.0d0
         case (6); res=720.0d0
         case (7); res=5040.0d0
         case (8); res=40320.0d0
         case (9); res=362880.0d0
         case default
           res = 3628800.0d0
           do i=11,self
             res = res * i
           end do
      end select
     STOP_TIMER("INT:factorial")
   end function

   ALWAYS_ELEMENTAL function double_factorial(self) result(res)
    INT :: self
   ! Return the double factorial of the integer
   ! res cannot be integer, as int(4) dies at 11!!.
      IN :: self
      REAL :: res
       INT :: i
   !  ENSURE(self>=0,"must have non-negative argument")
      select case (self)
         case (0); res=1.0d0
         case (1); res=1.0d0
         case (2); res=3.0d0
         case (3); res=15.0d0
         case (4); res=105.0d0
         case (5); res=945.0d0
         case (6); res=10395.0d0
         case (7); res=135135.0d0
         case (8); res=2027025.0d0
         case (9); res=34459425.0d0
         case default
           res=654729075.0d0
           do i=11,self
             res = res * (2 * i - 1)
           end do
      end select
     STOP_TIMER("INT:double_factorial")
   end function

   function permutation(self,n) result(res)
    INT :: self
   ! Return self!/n!
      IN :: self
       INT :: n
      REAL :: res
       INT :: i
   STACK("INT:permutation")
   START_TIMER("INT:permutation")
   ENSURE(self>=0,"INT:permutation ... must have non-negative argument")
   ENSURE(n   >=0,"INT:permutation ... must have non-negative n")
      res = ONE
      if (self>=n) then
         do i = n+1,self
            res = res * i
         end do
      else
         do i = self+1,n
            res = res * i
         end do
         res = ONE/res
      end if
     STOP_TIMER("INT:permutation")
      CHECK
   end function

   function permutation_1(self,n,m) result(res)
    INT :: self
   ! Return self!/(n!m!)
      IN :: self
      INT :: n,m
      REAL :: res
       INT :: i
   STACK("INT:permutation_1")
   START_TIMER("INT:permutation_1")
   ENSURE(self>=0,"INT:permutation_1 ... must have non-negative argument")
   ENSURE( n  >=0,"INT:permutation_1 ... must have non-negative n")
   ENSURE( m  >=0,"INT:permutation_1 ... must have non-negative m")
      res = permutation_(self,n)
      do i = 2,m
         res = res / i
      end do
     STOP_TIMER("INT:permutation_1")
      CHECK
   end function

   function choose(self,n) result(res)
    INT :: self
   ! Return self!/n!(self-n)! , self>=n
      IN :: self
      INT :: n
      REAL :: res
      STACK("INT:choose")
      START_TIMER("INT:choose")
      ENSURE(self>=0,"INT:choose ... must have non-negative argument")
      ENSURE(self>=n,"INT:choose ... must have self>=n")
      res = permutation_(self,n,self-n)
     STOP_TIMER("INT:choose")
      CHECK
   end function

   function sqrt_permutation(self,n) result(res)
    INT :: self
   ! Return sqrt(self!)/n! , self>n
      IN :: self
      INT, IN :: n
      REAL :: res
      REAL :: top,bot
      INT :: i
      STACK("INT:sqrt_permutation")
      START_TIMER("INT:sqrt_permutation")
      ENSURE(self>=0,"INT:sqrt_permutation ... must have non-negative argument")
      ENSURE(self>=n,"INT:sqrt_permutation ... must have self>=n")
      bot = ONE
      top = ONE
      do i = 1,n
         bot = bot * i
      end do
      do i = n+1,self
         top = top * i
      end do
      res = sqrt(top / bot)
     STOP_TIMER("INT:sqrt_permutation")
      CHECK
   end function

   function sqrt_permutation_1(self,n,m) result(res)
    INT :: self
   ! Return sqrt(self!)/(n!m!) , self>n, self>m
      IN :: self
      INT :: n,m
      REAL :: res
       INT :: i
   STACK("INT:sqrt_permutation_1")
   START_TIMER("INT:sqrt_permutation_1")
   ENSURE(self>=0,"INT:sqrt_permutation_1 ... must have non-negative argument")
   ENSURE(self>=n,"INT:sqrt_permutation_1 ... must have self>=n")
   ENSURE(self>=m,"INT:sqrt_permutation_1 ... must have self>=m")
      res = sqrt_permutation_(self,n)
      do i = 2,m
         res = res / i
      end do
     STOP_TIMER("INT:sqrt_permutation_1")
      CHECK
   end function

   ALWAYS_PURE function triangle(self) result(res)
    INT :: self
   ! The triangle number corresponding to seld
     IN :: self
     INT :: res
     res = self*(self+1)/2
     STOP_TIMER("INT:triangle")
   end function

   ALWAYS_PURE function triangle_number(self) result(res)
    INT :: self
   ! The triangle number corresponding to seld
     IN :: self
     INT :: res
     res = self*(self+1)/2
     STOP_TIMER("INT:triangle_number")
   end function

   function inverse_triangle_number(self) result(res)
    INT :: self
   ! Return the inverse triangle number of self.
     IN :: self
     INT :: res
   STACK("INT:inverse_triangle_number")
   START_TIMER("INT:inverse_triangle_number")
   ENSURE(self>=0,"INT:inverse_triangle_number ... self must be non-positive")
     res = (1+int(sqrt(EIGHT*self-SEVEN)))/2
     STOP_TIMER("INT:inverse_triangle_number")
      CHECK
   end function

   subroutine get_matrix_indices(self,row,column)
    INT :: self
   ! Assuming that "self" is an index into a flattened symmetric matrix stored
   ! as a vector (counting across the rows first, then down columns), set "row"
   ! and "column" to be the corresponding row and column indices of a square
   ! matrix, Clearly, row >= column always.
     IN :: self
     INT, OUT :: row,column
     STACK("INT:get_matrix_indices")
     START_TIMER("INT:get_matrix_indices")
     row    = inverse_triangle_number_(self)
     column = self - row*(row-1)/2
     STOP_TIMER("INT:get_matrix_indices")
      CHECK
   end subroutine

   ALWAYS_PURE function n_comp(self) result(res)
    INT :: self
   ! No. of cartesian components in a gaussian shell of momentum "self".
     IN :: self
     INT :: res
     res = (self+1)*(self+2)/2
     STOP_TIMER("INT:n_comp")
   end function

   ALWAYS_PURE function n_comp_sum(self) result(res)
    INT :: self
   ! No. of cartesian components up to shell with momentum "self".
     IN :: self
     INT :: res
     res = (self+1)*(self+2)*(self+3)/6
     STOP_TIMER("INT:n_comp_sum")
   end function

   ALWAYS_PURE function momentum_from_index(self) result(res)
    INT :: self
   ! Returns the total momentum of the cartesian component angular momentum
   ! vector with index "self".
     IN :: self
     INT :: res
     REAL :: mess
     mess = (81d0*real(self,kind=REAL_KIND) + &
                 sqrt(real(6561*self*self-27,kind=REAL_KIND)))**(0.333333333333333333d0)
     res = ceiling(1.0d0/mess + mess*(0.33333333333333333d0)-2.0000001d0)
     STOP_TIMER("INT:momentum_from_index")
   end function

   ALWAYS_PURE subroutine cartesian_components(self,res)
    INT :: self
   ! Returns the cartesian components of the angular momentum "self".
   ! Size of the result is [3,.n_comp].
     IN :: self
     INTMAT(3,(self+1)*(self+2)/2), OUT :: res
     INT :: a,b,c,ind
     ind=1
     do a = self,floor((self+2)/3.0),-1
       do b = min(self-a,a),floor((self-a+1)/2.0),-1
         c = self-a-b
         if (a==b AND b==c) then
           res(1:3,ind)   = (/a,a,a/)
           ind = ind+1
         else if (a>b AND b==c) then
           res(1:3,ind)   = (/a,b,b/)
           res(1:3,ind+1) = (/b,a,b/)
           res(1:3,ind+2) = (/b,b,a/)
           ind = ind+3
         else if (a==b AND b>c) then
           res(1:3,ind)   = (/a,a,c/)
           res(1:3,ind+1) = (/a,c,a/)
           res(1:3,ind+2) = (/c,a,a/)
           ind = ind+3
         else
           res(1:3,ind)   = (/a,b,c/)
           res(1:3,ind+1) = (/a,c,b/)
           res(1:3,ind+2) = (/b,a,c/)
           res(1:3,ind+3) = (/c,a,b/)
           res(1:3,ind+4) = (/b,c,a/)
           res(1:3,ind+5) = (/c,b,a/)
           ind = ind+6
         end if
       end do
     end do
     STOP_TIMER("INT:cartesian_components")
   end subroutine

   function hermite_polynomial(self,normalise) result(res)
    INT :: self
   ! Returns an array with the coefficients of the Hermite Polynomials.  First
   ! elements is for x^0, second for x^1, and so on.  The length of the result
   ! is (self+1).  Method used is equation 13.9 from Arfken and Weber 4th ed.
   ! If present and TRUE, "normalise" gives coefficients normalised for use
   ! in generating harmonic vibrational wavefunctions.
     IN :: self
     BIN, IN, optional :: normalise
     REALVEC(:), PTR :: res
     BIN :: norm
      INT :: n
     INT :: s,i,t
     REAL :: sign
   STACK("INT:hermite_polynomial")
   START_TIMER("INT:hermite_polynomial")
   ENSURE(self>=0,"INT:hermite_polynomial ... order must be 0 or greater")
     n = self
     norm = FALSE
     if (present(normalise)) norm = normalise
     allocate(res(n+1))
     t = 2
     res = ZERO
     if (norm) then
        sign = -ONE/sqrt(sqrt(PI))
        do s = 0,(n/2)
           sign = -sign
           i = n - 2*s
           res(i+1) = sign * sqrt( raised_to_(t,2*i-n)) * sqrt_permutation_(n,s,i)
        end do
     else
        sign = -ONE
        do s = 0,(n/2)
           sign = -sign
           i = n - 2*s
           res(i+1) = sign * raised_to_(t,i) * permutation_(n,s,i)
        end do
     end if
 !   select case (self)
 !     case (0); res = [   1.0d0
 !     case (1); res = [   0.0d0,   2.0d0
 !     case (2); res = [  -2.0d0,   0.0d0,   4.0d0
 !     case (3); res = [   0.0d0, -12.0d0,   0.0d0,   8.0d0
 !     case (4); res = [  12.0d0,   0.0d0, -48.0d0,   0.0d0,  16.0d0
 !     case (5); res = [   0.0d0, 120.0d0,   0.0d0,-160.0d0,   0.0d0, 32.0d0
 !     case (6); res = [-120.0d0,   0.0d0, 720.0d0,   0.0d0,-480.0d0,  0.0d0,  64.0d0
 !     case default;
 !        res(2:n+1) = TWO* (n-1).hermite_polynomial
 !        res(1:n-1) = res(1:n-1) - TWO*(n-1)* (n-2).hermite_polynomial
 !   end
     STOP_TIMER("INT:hermite_polynomial")
      CHECK
   end function

   ALWAYS_PURE function legendre_polynomials(self) result(res)
    INT :: self
   ! Returns an array with the coefficients of the Legendre Polynomials.  First
   ! elements is for x^0, second for x^1, and so on.  The length of the result
   ! is (self+1).  Method used is equation 12.64 from Arfken and Weber 4th ed.
     IN :: self
     REALVEC(self+1) :: res
      INT :: k
     REAL :: the_sign,self2
     select case (self)
       case (0); self2 = 1.0d0
       case (1); self2 = 2.0d0
       case (2); self2 = 4.0d0
       case default; self2 = 2.0d0**self
     end select
     res=0.0d0
     do k=0,self/2
       if (mod(k,2) == 0) then
         the_sign = 1.0d0
       else
         the_sign = -1.0d0
       end if
       res(self-2*k+1) = the_sign * factorial_((2*(self-k))) / &
                (self2 * factorial_(k) * factorial_((self-k)) * factorial_((self-2*k)))
     end do
     STOP_TIMER("INT:legendre_polynomials")
   end function

   ALWAYS_PURE function spherical_to_cartesians(self) result(res)
    INT :: self
   ! Returns the matrix which gives the cartesian to spherical conversion.
   ! Self is the angular momentum.
     IN :: self
     REALMAT(2*self+1,(self+1)*(self+2)/2) :: res
     REALMAT3(0:self,0:self,0:self) :: mat0
     CPXMAT3(0:self,0:self,0:self) :: plus,minus
     CPXMAT3(0:self,0:self,0:self) :: dx,dy,dz
     INTMAT(3,(self+1)*(self+2)/2) :: components
     REAL :: temp1,temp2,temp3
     INT :: start,i,j,array_pos,x,y,z,r_power,ind,m
     REALVEC(0:self) :: P_l_0

     P_l_0 = legendre_polynomials_(self)
     call cartesian_components_(self,components)
     do ind = 1,n_comp_(self)
       x = components(1,ind)
       y = components(2,ind)
       z = components(3,ind)
     end do
     mat0 = ZERO
     if (mod(self,2)==0) then
       start = 0
     else
       start = 1
     end if
     do array_pos = start,self,2
       r_power = (self - array_pos)/2
       temp1 = P_l_0(array_pos) * factorial_(r_power)
       do i = 0, r_power
         temp2 = temp1 / ( factorial_(i) )
         x = 2*i
         do j = 0, r_power-i
           y = 2*j
           z = 2*(r_power - i - j) + array_pos
           temp3 = temp2 / (factorial_(j) * factorial_((r_power-i-j)) )
           mat0(x,y,z) = mat0(x,y,z) + temp3
         end do
       end do
     end do
     do ind = 1,n_comp_(self)
       x = components(1,ind)
       y = components(2,ind)
       z = components(3,ind)
       res(1,ind) = mat0(x,y,z)
     end do

     plus = (1,1) * mat0
     minus =  (1,1) * mat0
     do m=1,self
       dx = eoshift(plus,1,(ZERO,ZERO),1)
       dy = eoshift(plus,1,(ZERO,ZERO),2)
       dz = eoshift(plus,1,(ZERO,ZERO),3)
       do i=0,self
         dx(i,:,:) = dx(i,:,:) * (i+1)
         dy(:,i,:) = dy(:,i,:) * (i+1)
         dz(:,:,i) = dz(:,:,i) * (i+1)
       end do
       plus = eoshift(dx,-1,(ZERO,ZERO),3) - eoshift(dz,-1,(ZERO,ZERO),1) &
         + (1,0) * (-eoshift(aimag(dy),-1,ZERO,3)+eoshift(aimag(dz),-1,ZERO,2)) &
         + (0,1) * (eoshift(real(dy),-1,ZERO,3)-eoshift(real(dz),-1,ZERO,2))

       dx = eoshift(minus,1,(ZERO,ZERO),1)
       dy = eoshift(minus,1,(ZERO,ZERO),2)
       dz = eoshift(minus,1,(ZERO,ZERO),3)
       do i=0,self
         dx(i,:,:) = dx(i,:,:) * (i+1)
         dy(:,i,:) = dy(:,i,:) * (i+1)
         dz(:,:,i) = dz(:,:,i) * (i+1)
       end do
       minus = - eoshift(dx,-1,(ZERO,ZERO),3) + eoshift(dz,-1,(ZERO,ZERO),1) &
         + (1,0) * (-eoshift(aimag(dy),-1,ZERO,3)+eoshift(aimag(dz),-1,ZERO,2)) &
         + (0,1) * (eoshift(real(dy),-1,ZERO,3)-eoshift(real(dz),-1,ZERO,2))

       temp1 = sqrt(real(self*(self+1)-(m-1)*(m),kind=REAL_KIND))
       plus = plus / temp1
       minus = minus / temp1
       do ind = 1,n_comp_(self)
         x = components(1,ind)
         y = components(2,ind)
         z = components(3,ind)
         res(2*m,ind)   = (-1)**m * real(plus(x,y,z)) + real(minus(x,y,z))
         res(2*m+1,ind) = (-1)**m * aimag(plus(x,y,z)) - aimag(minus(x,y,z))
       end do
     end do
     res(2:,:) = res(2:,:) / sqrt(TWO)
     STOP_TIMER("INT:spherical_to_cartesians")
   end function

   function assoc_legendre(self,m) result(res)
    INT :: self
   ! Returns d^m/dx^m(Pn(x)) for the associated Legendre polynomial, but does
   ! not multiply by (1-x^2)^(m/2).
      INT, IN :: m
     REALVEC(self+1) :: res
     INT :: i,j
     STACK("INT:assoc_legendre")
     START_TIMER("INT:assoc_legendre")
     res = legendre_polynomials_(self)
     do i=1,abs(m)
       do j=1,self+1
         res(j) = res(j) * (j-1)
       end do
       res = eoshift(res,1,ZERO)
     end do
     if (m < 0) then
       res = factorial_((self+m)) / factorial_((self-m)) * res
       if (mod(m,2)==1) res = - res
     end if
     STOP_TIMER("INT:assoc_legendre")
      CHECK
   end function

   function cos_coeff(self,comp) result(res)
    INT :: self
   ! Returns the coefficient of cos^comp(self-2*comp) sin^(2*comp)(phi) of the
   ! expansion of cos(self phi)
     IN :: self
     INT, IN :: comp
     INT :: res
     INT :: i,comp2,factor
     STACK("INT:cos_coeff")
     START_TIMER("INT:cos_coeff")
     if (self==0) then
       if (comp==0) then; res = 1
       else;              res = 0
       end if
     else if (mod(comp,2)==0 OR comp<0) then
       res = 0
     else
       comp2 = (comp - 1) / 2
       if (mod(comp2,2)==0) then; res = 1
       else;                      res = -1
       end if
       do i = abs(self) - 2 * comp2 + 1, abs(self)
         res = res * i
       end do
       factor=1
       do i = 1, comp2 - 1
         factor = factor * (2 * i + 1) * (i + 1)
       end do
       res = res / (2**comp2 * factor)
       if (self<0) res = - res
     end if
     STOP_TIMER("INT:cos_coeff")
      CHECK
   end function

   function sin_coeff(self,comp) result(res)
    INT :: self
   ! Returns the coefficient of cos^(self-2*comp-1)(phi) sin^(2*comp+1)(phi)
   ! of the expansion of sin(self * phi).
     IN :: self
     INT, IN :: comp
     INT :: res
     INT :: i,comp2,factor
     STACK("INT:sin_coeff")
     START_TIMER("INT:sin_coeff")
     if (mod(comp,2)==0 OR comp<0 OR self==0) then
       res = 0
     else
       comp2 = (comp - 1) / 2
       if (mod(comp2,2)==0) then; res = 1
       else;                      res = -1
       end if
       do i = abs(self) - 2 * comp2, abs(self)
         res = res * i
       end do
       factor=1
       do i = 1, comp2 - 1
         factor = factor * (2 * i + 1) * (i + 1)
       end do
       res = res / (2**comp2 * (2 * comp2 + 1) * factor)
     end if
     STOP_TIMER("INT:sin_coeff")
      CHECK
   end function

   recursive function associated_laguerre_polynomial(self,k) result(res)
    INT :: self
   ! Returns an array with the coefficients of the associated Laguerre
   ! polynomials, L_n^k (x) = (1/n!) exp(x) x^{-k) d^n/dx^n [exp(-x)
   ! x^{n+k}].  First elements is for x^0, second for x^1, and so on.
   ! The length of the result is (self+1).  The method is from the recursion
   ! expansion (13.44) in Arfken & Weber, Mathematical Methods for Physicists,
   ! 4th Edition.
     INT :: k
     REALVEC(self+1) :: res
     REALVEC(self) :: res1,res2
     INT :: n,n1,n2
     STACK("INT:associated_laguerre_polynomial")
     START_TIMER("INT:associated_laguerre_polynomial")
     ENSURE(self>=0,"INT:associated_laguerre_polynomial ... non positive self")
     n  = self
     n1 = n - 1
     n2 = n - 2
     select case (self)
       case (0); res = (/ 1 /)
       case (1); res = (/-1, k+1/)
       case default;
          res2 = associated_laguerre_polynomial_(n2,k)
          res1 = associated_laguerre_polynomial_(n1,k)
          res(1:n  ) =          - (n+k-1)   * res2
          res(1:n  ) = res(1:n) + (2*n+k-1) * res1
          res(2:n+1) = res(2:n+1) - res1
          res = res/n
     end select
     STOP_TIMER("INT:associated_laguerre_polynomial")
      CHECK
   end function

   function radial_laguerre_function(self,l,zeta) result(res)
    INT :: self
   ! Returns an array with the coefficients of the radial Laguerre function as
   ! defined in "Molecular Electronic Structure Theory" by Helgaker et al,
   ! equation (6.5.17) *except* that the factor r^l exp(-zeta*R) is set to one.
   ! Self=n is the principal quantum number.
     INT :: l
     REAL :: zeta
     REALVEC(:), PTR :: res
     INT :: n,n1,l1
     REAL :: fac
     STACK("INT:radial_laguerre_function")
     START_TIMER("INT:radial_laguerre_function")
     ENSURE(self>=0,"INT:radial_laguerre_function ... non positive self")
     n  = self
     n1 = n - l - 1
     l1 = 2*l + 2
     allocate(res(n1+1))
     res = associated_laguerre_polynomial_(n1,l1)
     fac = (TWO*zeta)**((THREE/TWO) + l) * sqrt(choose_(n1,n+l+1))
     res = fac * res
     STOP_TIMER("INT:radial_laguerre_function")
      UNSTACK
   end function

   ALWAYS_PURE subroutine make_gaussian_xyz_indices(self,nx,ny,nz)
    INT :: self
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum "self".
   ! (actually adds one to each power to avoid 0 as lower bound).
     IN :: self
     INTVEC(:), OUT :: nx,ny,nz
     call make_gaussian_xyz_indices_(self,nx,ny,nz,self)
     STOP_TIMER("INT:make_gaussian_xyz_indices")
   end subroutine

   ALWAYS_PURE subroutine make_gaussian_xyz_indices_1(self,nx,ny,nz,maxl)
    INT :: self
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum "self".
   ! (actually adds one to each power to avoid 0 as lower bound).
     IN :: self
     INT, IN :: maxl
     INTVEC(:), OUT :: nx,ny,nz
     INT :: a,b,c,i,j,a1,b1,c1
     i = 1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
          do b = min(j-a,a),floor((j-a+1)*HALF),-1
            c = j-a-b
            if (a==b AND b==c) then
              a1=a+1
              nx(i  ) = a1; ny(i  ) = a1; nz(i  ) = a1
              i = i+1
            else if (a>b AND b==c) then
              a1=a+1
              b1=b+1
              nx(i  ) = a1; ny(i  ) = b1; nz(i  ) = b1
              nx(i+1) = b1; ny(i+1) = a1; nz(i+1) = b1
              nx(i+2) = b1; ny(i+2) = b1; nz(i+2) = a1
              i = i+3
            else if (a==b AND b>c) then
              a1=a+1
              c1=c+1
              nx(i  ) = a1; ny(i  ) = a1; nz(i  ) = c1
              nx(i+1) = a1; ny(i+1) = c1; nz(i+1) = a1
              nx(i+2) = c1; ny(i+2) = a1; nz(i+2) = a1
              i = i+3
            else
              a1=a+1
              b1=b+1
              c1=c+1
              nx(i  ) = a1; ny(i  ) = b1; nz(i  ) = c1
              nx(i+1) = a1; ny(i+1) = c1; nz(i+1) = b1
              nx(i+2) = b1; ny(i+2) = a1; nz(i+2) = c1
              nx(i+3) = c1; ny(i+3) = a1; nz(i+3) = b1
              nx(i+4) = b1; ny(i+4) = c1; nz(i+4) = a1
              nx(i+5) = c1; ny(i+5) = b1; nz(i+5) = a1
              i = i+6
            end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_indices_1")
   end subroutine

   ALWAYS_PURE subroutine make_gaussian_xyz_indices_2(self,nx,ny,nz,index,maxl)
    INT :: self
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum "self".
   ! (actually adds one to each power to avoid 0 as lower bound).
     IN :: self
     INT, IN :: maxl
     INTVEC(:), OUT :: nx,ny,nz
     INTMAT3(:,:,:), OUT :: index
     INT :: a,b,c,i,j,a1,b1,c1
     i = 1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
          do b = min(j-a,a),floor((j-a+1)*HALF),-1
            c = j-a-b
            if (a==b AND b==c) then
              a1=a+1
              nx(i  ) = a1; ny(i  ) = a1; nz(i  ) = a1
              index(a1,a1,a1) = i
              i = i+1
            else if (a>b AND b==c) then
              a1=a+1
              b1=b+1
              nx(i  ) = a1; ny(i  ) = b1; nz(i  ) = b1
              nx(i+1) = b1; ny(i+1) = a1; nz(i+1) = b1
              nx(i+2) = b1; ny(i+2) = b1; nz(i+2) = a1
              index(a1,b1,b1) = i
              index(b1,a1,b1) = i+1
              index(b1,b1,a1) = i+2
              i = i+3
            else if (a==b AND b>c) then
              a1=a+1
              c1=c+1
              nx(i  ) = a1; ny(i  ) = a1; nz(i  ) = c1
              nx(i+1) = a1; ny(i+1) = c1; nz(i+1) = a1
              nx(i+2) = c1; ny(i+2) = a1; nz(i+2) = a1
              index(a1,a1,c1) = i
              index(a1,c1,a1) = i+1
              index(c1,a1,a1) = i+2
              i = i+3
            else
              a1=a+1
              b1=b+1
              c1=c+1
              nx(i  ) = a1; ny(i  ) = b1; nz(i  ) = c1
              nx(i+1) = a1; ny(i+1) = c1; nz(i+1) = b1
              nx(i+2) = b1; ny(i+2) = a1; nz(i+2) = c1
              nx(i+3) = c1; ny(i+3) = a1; nz(i+3) = b1
              nx(i+4) = b1; ny(i+4) = c1; nz(i+4) = a1
              nx(i+5) = c1; ny(i+5) = b1; nz(i+5) = a1
              index(a1,b1,c1) = i
              index(a1,c1,b1) = i+1
              index(b1,a1,c1) = i+2
              index(c1,a1,b1) = i+3
              index(b1,c1,a1) = i+4
              index(c1,b1,a1) = i+5
              i = i+6
            end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_indices_2")
   end subroutine

   PURE subroutine make_gaussian_xyz_powers(self,mat)
    INT :: self
   ! Returns the cartesian components of the angular momentum "self".
   ! Size of mat is [3,.n_comp].
     IN :: self
     INTMAT(:,:), OUT :: mat
     call make_gaussian_xyz_powers_(self,mat,self)
     STOP_TIMER("INT:make_gaussian_xyz_powers")
   end subroutine

   PURE subroutine make_gaussian_xyz_powers_1(self,mat,index)
    INT :: self
   ! Returns the cartesian components of the angular momentum "self".
   ! Size of mat is [3,.n_comp].
     IN :: self
     INTMAT(:,:), OUT :: mat
     INTMAT3(:,:,:), OUT :: index
     call make_gaussian_xyz_powers_(self,mat,self,index)
     STOP_TIMER("INT:make_gaussian_xyz_powers_1")
   end subroutine

   ALWAYS_PURE subroutine make_gaussian_xyz_powers_2(self,nx,ny,nz)
    INT :: self
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum "self".
     IN :: self
     INTVEC(:), OUT :: nx,ny,nz
     call make_gaussian_xyz_powers_(self,nx,ny,nz,self)
     STOP_TIMER("INT:make_gaussian_xyz_powers_2")
   end subroutine

   ALWAYS_PURE subroutine make_gaussian_xyz_powers_3(self,nx,ny,nz,maxl)
    INT :: self
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum "self".
     IN :: self
     INT, IN :: maxl
     INTVEC(:), OUT :: nx,ny,nz
     INT :: a,b,c,i,j
     i = 1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
         do b = min(j-a,a),floor((j-a+1)*HALF),-1
           c = j-a-b
           if (a==b AND b==c) then
             nx(i  ) = a; ny(i  ) = a; nz(i  ) = a
             i = i+1
           else if (a>b AND b==c) then
             nx(i  ) = a; ny(i  ) = b; nz(i  ) = b
             nx(i+1) = b; ny(i+1) = a; nz(i+1) = b
             nx(i+2) = b; ny(i+2) = b; nz(i+2) = a
             i = i+3
           else if (a==b AND b>c) then
             nx(i  ) = a; ny(i  ) = a; nz(i  ) = c
             nx(i+1) = a; ny(i+1) = c; nz(i+1) = a
             nx(i+2) = c; ny(i+2) = a; nz(i+2) = a
             i = i+3
           else
             nx(i  ) = a; ny(i  ) = b; nz(i  ) = c
             nx(i+1) = a; ny(i+1) = c; nz(i+1) = b
             nx(i+2) = b; ny(i+2) = a; nz(i+2) = c
             nx(i+3) = c; ny(i+3) = a; nz(i+3) = b
             nx(i+4) = b; ny(i+4) = c; nz(i+4) = a
             nx(i+5) = c; ny(i+5) = b; nz(i+5) = a
             i = i+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_powers_3")
   end subroutine

   ALWAYS_PURE subroutine make_gaussian_xyz_powers_4(self,nx,ny,nz,maxl,index)
    INT :: self
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum "self".
     IN :: self
     INT, IN :: maxl
     INTVEC(:), OUT :: nx,ny,nz
     INTMAT3(:,:,:), OUT :: index
     INT :: a,b,c,i,j,a1,b1,c1
     i = 1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
         do b = min(j-a,a),floor((j-a+1)*HALF),-1
           c = j-a-b
           if (a==b AND b==c) then
             a1=a+1
             nx(i  ) = a; ny(i  ) = a; nz(i  ) = a
             index(a1,a1,a1) = i
             i = i+1
           else if (a>b AND b==c) then
             a1=a+1;   b1=b+1
             nx(i  ) = a; ny(i  ) = b; nz(i  ) = b
             nx(i+1) = b; ny(i+1) = a; nz(i+1) = b
             nx(i+2) = b; ny(i+2) = b; nz(i+2) = a
             index(a1,b1,b1) = i
             index(b1,a1,b1) = i+1
             index(b1,b1,a1) = i+2
             i = i+3
           else if (a==b AND b>c) then
             a1=a+1;   c1=c+1
             nx(i  ) = a; ny(i  ) = a; nz(i  ) = c
             nx(i+1) = a; ny(i+1) = c; nz(i+1) = a
             nx(i+2) = c; ny(i+2) = a; nz(i+2) = a
             index(a1,a1,c1) = i
             index(a1,c1,a1) = i+1
             index(c1,a1,a1) = i+2
             i = i+3
           else
             a1=a+1;   b1=b+1;   c1=c+1
             nx(i  ) = a; ny(i  ) = b; nz(i  ) = c
             nx(i+1) = a; ny(i+1) = c; nz(i+1) = b
             nx(i+2) = b; ny(i+2) = a; nz(i+2) = c
             nx(i+3) = c; ny(i+3) = a; nz(i+3) = b
             nx(i+4) = b; ny(i+4) = c; nz(i+4) = a
             nx(i+5) = c; ny(i+5) = b; nz(i+5) = a
             index(a1,b1,c1) = i
             index(a1,c1,b1) = i+1
             index(b1,a1,c1) = i+2
             index(c1,a1,b1) = i+3
             index(b1,c1,a1) = i+4
             index(c1,b1,a1) = i+5
             i = i+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_powers_4")
   end subroutine

   PURE subroutine make_gaussian_xyz_powers_5(self,mat,maxl)
    INT :: self
   ! Returns the cartesian components of all angular momenta between "self"
   ! and "maxl".
   ! Size of mat is [3, maxl.n_comp_sum - (minl-1).n_comp_sum )].
     IN :: self
     INT, IN :: maxl
     INTMAT(:,:), OUT :: mat
     INT :: a,b,c,ind,j
     ind=1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
         do b = min(j-a,a),floor((j-a+1)*HALF),-1
           c = j-a-b
           if (a==b AND b==c) then
             mat(1:3,ind)   = (/a,a,a/)
             ind = ind+1
           else if (a>b AND b==c) then
             mat(1:3,ind)   = (/a,b,b/)
             mat(1:3,ind+1) = (/b,a,b/)
             mat(1:3,ind+2) = (/b,b,a/)
             ind = ind+3
           else if (a==b AND b>c) then
             mat(1:3,ind)   = (/a,a,c/)
             mat(1:3,ind+1) = (/a,c,a/)
             mat(1:3,ind+2) = (/c,a,a/)
             ind = ind+3
           else
             mat(1:3,ind)   = (/a,b,c/)
             mat(1:3,ind+1) = (/a,c,b/)
             mat(1:3,ind+2) = (/b,a,c/)
             mat(1:3,ind+3) = (/c,a,b/)
             mat(1:3,ind+4) = (/b,c,a/)
             mat(1:3,ind+5) = (/c,b,a/)
             ind = ind+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_powers_5")
   end subroutine

   PURE subroutine make_gaussian_xyz_powers_6(self,mat,maxl,index)
    INT :: self
   ! Returns the cartesian components of all angular momenta between "self"
   ! and "maxl".
   ! Size of mat is [3, maxl.n_comp_sum - (minl-1).n_comp_sum )].
     IN :: self
     INT, IN :: maxl
     INTMAT(:,:), OUT :: mat
     INTMAT3(:,:,:), OUT :: index
     INT :: a,b,c,i,j,a1,b1,c1
     i=1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
         do b = min(j-a,a),floor((j-a+1)*HALF),-1
           c = j-a-b
           if (a==b AND b==c) then
             a1=a+1
             mat(1:3,i)   = (/a,a,a/)
             index(a1,a1,a1) = i
             i = i+1
           else if (a>b AND b==c) then
             a1=a+1;   b1=b+1
             mat(1:3,i)   = (/a,b,b/)
             mat(1:3,i+1) = (/b,a,b/)
             mat(1:3,i+2) = (/b,b,a/)
             index(a1,b1,b1) = i
             index(b1,a1,b1) = i+1
             index(b1,b1,a1) = i+2
             i = i+3
           else if (a==b AND b>c) then
             a1=a+1;   c1=c+1
             mat(1:3,i)   = (/a,a,c/)
             mat(1:3,i+1) = (/a,c,a/)
             mat(1:3,i+2) = (/c,a,a/)
             index(a1,a1,c1) = i
             index(a1,c1,a1) = i+1
             index(c1,a1,a1) = i+2
             i = i+3
           else
             a1=a+1;   b1=b+1;   c1=c+1
             mat(1:3,i)   = (/a,b,c/)
             mat(1:3,i+1) = (/a,c,b/)
             mat(1:3,i+2) = (/b,a,c/)
             mat(1:3,i+3) = (/c,a,b/)
             mat(1:3,i+4) = (/b,c,a/)
             mat(1:3,i+5) = (/c,b,a/)
             index(a1,b1,c1) = i
             index(a1,c1,b1) = i+1
             index(b1,a1,c1) = i+2
             index(c1,a1,b1) = i+3
             index(b1,c1,a1) = i+4
             index(c1,b1,a1) = i+5
             i = i+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_powers_6")
   end subroutine

   PURE subroutine make_gaussian_xyz_powers_7(self,mat,maxl,index,first_nonzero)
    INT :: self
   ! Returns the cartesian components of all angular momenta between "self"
   ! and "maxl".
   ! Size of mat is [3, maxl.n_comp_sum - (minl-1).n_comp_sum )].
     IN :: self
     INT, IN :: maxl
     INTMAT(:,:), OUT :: mat
     INTMAT3(:,:,:), OUT :: index
     INTVEC(:), OUT :: first_nonzero
     INT :: a,b,c,i,j,a1,b1,c1,k
     i=1
     do j=self,maxl
       k=1
       do a = j,floor((j+2)*THIRD),-1
         do b = min(j-a,a),floor((j-a+1)*HALF),-1
           c = j-a-b
           if (a==b AND b==c) then
             a1=a+1
             mat(1:3,i)   = (/a,a,a/)
             index(a1,a1,a1) = k
             first_nonzero(i)=1
             i = i+1
             k = k+1
           else if (a>b AND b==c) then
             a1=a+1;   b1=b+1
             mat(1:3,i)   = (/a,b,b/)
             mat(1:3,i+1) = (/b,a,b/)
             mat(1:3,i+2) = (/b,b,a/)
             index(a1,b1,b1) = k
             index(b1,a1,b1) = k+1
             index(b1,b1,a1) = k+2
             first_nonzero(i)  =1
             first_nonzero(i+1)=2
             first_nonzero(i+2)=3
             if (b>0) then
               first_nonzero(i+1)=1
               first_nonzero(i+2)=1
             end if
             i = i+3
             k = k+3
           else if (a==b AND b>c) then
             a1=a+1;   c1=c+1
             mat(1:3,i)   = (/a,a,c/)
             mat(1:3,i+1) = (/a,c,a/)
             mat(1:3,i+2) = (/c,a,a/)
             index(a1,a1,c1) = k
             index(a1,c1,a1) = k+1
             index(c1,a1,a1) = k+2
             first_nonzero(i)  =1
             first_nonzero(i+1)=1
             first_nonzero(i+2)=2
             if (c>0) then
               first_nonzero(i+2)=1
             end if
             i = i+3
             k = k+3
           else
             a1=a+1;   b1=b+1;   c1=c+1
             mat(1:3,i)   = (/a,b,c/)
             mat(1:3,i+1) = (/a,c,b/)
             mat(1:3,i+2) = (/b,a,c/)
             mat(1:3,i+3) = (/c,a,b/)
             mat(1:3,i+4) = (/b,c,a/)
             mat(1:3,i+5) = (/c,b,a/)
             index(a1,b1,c1) = k
             index(a1,c1,b1) = k+1
             index(b1,a1,c1) = k+2
             index(c1,a1,b1) = k+3
             index(b1,c1,a1) = k+4
             index(c1,b1,a1) = k+5
             if (a>0) then
               first_nonzero(i)  =1
               first_nonzero(i+1)=1
               first_nonzero(i+2)=2
               first_nonzero(i+3)=2
               first_nonzero(i+4)=3
               first_nonzero(i+5)=3
             else
               first_nonzero(i:i+3)=3
               ! i+4,i+5 must have b or c nonzero.
             end if
             if (b>0) then
               first_nonzero(i+2)=1
               first_nonzero(i+4)=1
               first_nonzero(i+5)=2
             end if
             if (c>0) then
               first_nonzero(i+3)=1
               first_nonzero(i+5)=1
             end if
             i = i+6
             k = k+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_powers_7")
   end subroutine

   PURE subroutine make_gaussian_xyz_power_index(self,index,maxl)
    INT :: self
   ! Returns the cartesian components of all angular momenta between "self"
   ! and "maxl".
   ! Size of mat is [3, maxl.n_comp_sum - (minl-1).n_comp_sum )].
     IN :: self
     INT, IN :: maxl
     INTMAT3(:,:,:), OUT :: index
     INT :: a,b,c,i,j,a1,b1,c1
     i=1
     do j=self,maxl
       do a = j,floor((j+2)*THIRD),-1
         do b = min(j-a,a),floor((j-a+1)*HALF),-1
           c = j-a-b
           if (a==b AND b==c) then
             a1=a+1
             index(a1,a1,a1) = i
             i = i+1
           else if (a>b AND b==c) then
             a1=a+1;   b1=b+1
             index(a1,b1,b1) = i
             index(b1,a1,b1) = i+1
             index(b1,b1,a1) = i+2
             i = i+3
           else if (a==b AND b>c) then
             a1=a+1;   c1=c+1
             index(a1,a1,c1) = i
             index(a1,c1,a1) = i+1
             index(c1,a1,a1) = i+2
             i = i+3
           else
             a1=a+1;   b1=b+1;   c1=c+1
             index(a1,b1,c1) = i
             index(a1,c1,b1) = i+1
             index(b1,a1,c1) = i+2
             index(c1,a1,b1) = i+3
             index(b1,c1,a1) = i+4
             index(c1,b1,a1) = i+5
             i = i+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:make_gaussian_xyz_power_index")
   end subroutine

   subroutine get_gaussian_xyz_powers(self,power)
    INT :: self
   ! Make "power(1:3,i)", the three xyz powers of all cartesian gaussian
   ! functions "i" of angular momentum up to "self", where "i" is the standard
   ! lexical index of the cartesian gaussian. (This routine essentially defines
   ! the standard order).  The shape of "power" is: [3, .n_comp_sum ].
     REALMAT(:,:) :: power
     INT :: l_max,i,L,a,b,c
   STACK("INT:get_gaussian_xyz_powers")
   START_TIMER("INT:get_gaussian_xyz_powers")
   ENSURE(size(power,1)==3,"INT:get_gaussian_xyz_powers ... wrong 1st dimension, power")
   ENSURE(size(power,2)==n_comp_sum_(l_max),"INT:get_gaussian_xyz_powers ... wrong 2nd dimension, power")
     l_max = self
     i = 1          ! This is the total lexical index
     do L = 0,l_max ! Loop over all shells with momentum L
                    ! Loop over powers a, b, c
       do a = L,floor((L+2)*THIRD),-1
         do b = min(L-a,a),floor((L-a+1)*HALF),-1
           c = L-a-b
           if (a==b AND b==c) then
             power(:,i)   = (/a,a,a/)
             i = i+1
           else if (a>b AND b==c) then
             power(:,i)   = (/a,b,b/)
             power(:,i+1) = (/b,a,b/)
             power(:,i+2) = (/b,b,a/)
             i = i+3
           else if (a==b AND b>c) then
             power(:,i)   = (/a,a,c/)
             power(:,i+1) = (/a,c,a/)
             power(:,i+2) = (/c,a,a/)
             i = i+3
           else
             power(:,i)   = (/a,b,c/)
             power(:,i+1) = (/a,c,b/)
             power(:,i+2) = (/b,a,c/)
             power(:,i+3) = (/c,a,b/)
             power(:,i+4) = (/b,c,a/)
             power(:,i+5) = (/c,b,a/)
             i = i+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:get_gaussian_xyz_powers")
      CHECK
   end subroutine

   subroutine get_gaussian_xyz_indices(self,index)
    INT :: self
   ! Make "index", an array which maps the three defining xyz powers of a
   ! cartesian gaussian back to its lexical index *within a shell of the same
   ! angular momentum* i.e. not the total lexical index. The indices for all
   ! gaussians up to angular momentum "self" are generated. ***NOTE***
   ! "index" must have lower bounds of 0, and so is passed in as a pointer.
     REALMAT3(:,:,:), PTR :: index
     INT :: l_max,L,k,a,b,c
   STACK("INT:get_gaussian_xyz_indices")
   START_TIMER("INT:get_gaussian_xyz_indices")
   ENSURE(lbound(index,1)==0,"INT:get_gaussian_xyz_indices ... wrong lower bound")
   ENSURE(lbound(index,2)==0,"INT:get_gaussian_xyz_indices ... wrong lower bound")
   ENSURE(lbound(index,3)==0,"INT:get_gaussian_xyz_indices ... wrong lower bound")
   ENSURE(ubound(index,1)==l_max,"INT:get_gaussian_xyz_indices ... wrong upper bound")
   ENSURE(ubound(index,2)==l_max,"INT:get_gaussian_xyz_indices ... wrong upper bound")
   ENSURE(ubound(index,3)==l_max,"INT:get_gaussian_xyz_indices ... wrong upper bound")
     l_max = self
     do L = 0,l_max  ! Loop over all shells with momentum L
       k = 1         ! This is the local shell lexical index
                     ! Loop over powers a, b, c
       do a = L,floor((L+2)*THIRD),-1
         do b = min(L-a,a),floor((L-a+1)*HALF),-1
           c = L-a-b
           if (a==b AND b==c) then
             index(a,a,a) = k
             k = k+1
           else if (a>b AND b==c) then
             index(a,b,b) = k
             index(b,a,b) = k+1
             index(b,b,a) = k+2
             k = k+3
           else if (a==b AND b>c) then
             index(a,a,c) = k
             index(a,c,a) = k+1
             index(c,a,a) = k+2
             k = k+3
           else
             index(a,b,c) = k
             index(a,c,b) = k+1
             index(b,a,c) = k+2
             index(c,a,b) = k+3
             index(b,c,a) = k+4
             index(c,b,a) = k+5
             k = k+6
           end if
         end do
       end do
     end do
     STOP_TIMER("INT:get_gaussian_xyz_indices")
      CHECK
   end subroutine

   subroutine check_if_one_of(self,allowed)
    INT :: self
   ! Returns TRUE if "self" matches one of the integers in "allowed".
      INTVEC(:) :: allowed
      BIN :: ok
      STACK("INT:check_if_one_of")
      START_TIMER("INT:check_if_one_of")
      ok = any(self==allowed)
     STOP_TIMER("INT:check_if_one_of")
      CHECK
   end subroutine

   function bit_set(self,pos) result(res)
    INT :: self
   ! Sets all bits in "self" corresponding to bit positions given
   ! in vector "pos", and return the result "res"
      INTVEC(:), IN :: pos
      INT :: res
      INT :: i
   STACK("INT:bit_set")
   START_TIMER("INT:bit_set")
   ENSURE(minval(pos)>=0,"INT:bit_set ... some pos values too small")
   ENSURE(maxval(pos)<=bit_size(self),"INT:bit_set ... some pos values too large")
      res = self
      do i = 1,size(pos)
         res = ibset(res,pos(i))
      end do
     STOP_TIMER("INT:bit_set")
      CHECK
   end function

   function bit_test(self,pos) result(res)
    INT :: self
   ! Returns a logical vector which is true in element i if the correspondong
   ! bit of "self" at pos(i) is set.
      INTVEC(:), IN :: pos
      BINVEC(size(pos)) :: res
      INT :: i
   STACK("INT:bit_test")
   START_TIMER("INT:bit_test")
   ENSURE(minval(pos)>=0,"INT:bit_test ... some pos values too small")
   ENSURE(maxval(pos)<=bit_size(self),"INT:bit_test ... some pos values too large")
      do i = 1,size(pos)
         res(i) = btest(self,pos(i))
      end do
     STOP_TIMER("INT:bit_test")
      CHECK
   end function

end
