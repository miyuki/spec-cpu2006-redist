!---------------------------------------------------------------------------
!
!  REAL: methods which apply to double precision numbers
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
! $Id: real.foo,v 1.31.2.2 2003/11/13 05:36:07 reaper Exp $
!---------------------------------------------------------------------------

module REAL_MODULE

#  include "real.use"

   implicit none

#  include "macros"
#  include "real.int"


contains

   PURE subroutine plus(self,val)
    REAL :: self
   ! Add "val" to self
      INOUT :: self
      REAL, IN :: val
      self = self + val
     STOP_TIMER("REAL:plus")
   end subroutine

   PURE subroutine minus(self,val)
    REAL :: self
   ! Subtract "val" to self
      INOUT :: self
      REAL, IN :: val
      self = self - val
     STOP_TIMER("REAL:minus")
   end subroutine

   PURE subroutine times(self,val)
    REAL :: self
   ! Multiply "val" by self
      INOUT :: self
      REAL, IN :: val
      self = self * val
     STOP_TIMER("REAL:times")
   end subroutine

   PURE function to_str(self,format) result(string)
    REAL :: self
   ! Change self to a "string" using default format, or the specified fortran
   ! "format", if present.
      IN :: self
      STR(*), IN, optional :: format
      STR(STR_SIZE) :: string
      string = " "
      if (present(format)) then; write(string,fmt="("//trim(format)//")") self
      else;                      write(string,fmt=*) self
      end if
      call left_justify_(string)
     STOP_TIMER("REAL:to_str")
   end function

   PURE function to_str_no_zeros(self) result(string)
    REAL :: self
   ! Change self to a "string" but remove trailing zeros
      IN :: self
      STR(STR_SIZE) :: string
      INT :: i
      string = " "
      write(string,fmt=*) self
      string=adjustl(string)
      do i = len_trim(string),1,-1
          if (string(i:i)=="0") then
            string(i:i)=" "
          else if (string(i:i)==".") then
            string(i:i)=" "
            exit
          else
            exit
          end if
      end do
     STOP_TIMER("REAL:to_str_no_zeros")
   end function

   PURE function to_str_no_zeros_1(self,format) result(string)
    REAL :: self
   ! Change self to a "string" using specified format "format"
   ! but remove trailing zeros
      IN :: self
      STR(*), IN :: format
      STR(STR_SIZE) :: string
      INT :: i,n
      string = " "
      write(string,fmt="("//trim(format)//")") self
      string=adjustl(string)
      n = index(string,achar(46))
      if (n/=0) then
        do i = len_trim(string),n,-1
          if (string(i:i)=="0") then
            string(i:i)=" "
          else
            exit
          end if
        end do
      end if
     STOP_TIMER("REAL:to_str_no_zeros_1")
   end function

   PURE function equals(self,x,eps) result(res)
    REAL :: self
   ! Return TRUE is self is near enough to "x". If present, "eps"
   ! defines how close before the number is considered the same.
      IN :: self
      REAL, IN :: x
      REAL, IN, optional :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self-x) < tol
     STOP_TIMER("REAL:equals")
   end function

   PURE function same_as(self,x,eps) result(res)
    REAL :: self
   ! Return TRUE is self is near enough to "x". If present, "eps"
   ! defines how close before the number is considered the same.
      IN :: self
      REAL, IN :: x
      REAL, IN, optional :: eps
      BIN :: res
      res = equals_(self,x,eps)
     STOP_TIMER("REAL:same_as")
   end function

   PURE function is_zero(self,eps) result(res)
    REAL :: self
   ! Return TRUE if self is near enough zero. If present, "eps" defines how
   ! close to zero before the number is considered zero.
      IN :: self
      REAL, IN, optional :: eps
      BIN :: res
      REAL :: tol
      tol = REAL_EPSILON
      if (present(eps)) tol = abs(eps)
      res = abs(self) < tol
     STOP_TIMER("REAL:is_zero")
   end function

   PURE function is_int(self,eps) result(res)
    REAL :: self
   ! Return TRUE if self is near enough to an integer. If present, "eps" defines
   ! how close to zero the non-integer part is before the number is considered
   ! an integer.
      IN :: self
      REAL, IN, optional :: eps
      BIN :: res
      REAL :: val
      val = self - int(self)
      res = is_zero_(val,eps)
     STOP_TIMER("REAL:is_int")
   end function

   PURE function is_in_range(self,range) result(res)
    REAL :: self
   ! Return TRUE if self is within the specified "range".
      IN :: self
      REALVEC(2), IN :: range
      BIN :: res
      res = range(1) <= self AND self <= range(2)
     STOP_TIMER("REAL:is_in_range")
   end function

   PURE function raised_to(self,n) result(res)
    REAL :: self
   ! Raise "self" to the power "n"
      IN :: self
      INT, IN :: n
      REAL :: res
       INT :: i
      res = ONE
      do i = 1,abs(n)
         res = res*self
      end do
      if (n<0) res = ONE/res
     STOP_TIMER("REAL:raised_to")
   end function

   PURE function arccos(self) result(res)
    REAL :: self
   ! Return the arccosine for self. Corrects bug for numbers close to 1.
      IN :: self
      REAL :: res
      if (abs(abs(self)-ONE)<TOL(5)) then
         if (self<0) then; res = PI
         else;             res = ZERO
         end if
      else
         res = acos(self)
      end if
     STOP_TIMER("REAL:arccos")
   end function

   PURE function arcsin(self) result(res)
    REAL :: self
   ! Return the arcsine for self. Corrects bug for numbers close to 1.
      IN :: self
      REAL :: res
      if (abs(abs(self)-ONE)<TOL(5)) then
         if (self<0) then; res = -PI/TWO
         else;             res = +PI/TWO
         end if
      else
         res = asin(self)
      end if
     STOP_TIMER("REAL:arcsin")
   end function

   PURE function arcsinh(self) result(res)
    REAL :: self
   ! Return the arcsinh of self. Corrects bug for numbers close to 1.
   ! Note that self can be any real number.
      IN :: self
      REAL :: res
      res = log(self + sqrt(ONE+self*self))
     STOP_TIMER("REAL:arcsinh")
   end function

   function hermite_polynomial(self,n,normalise) result(res)
    REAL :: self
   ! Return "res", the value of the "n"th hermite polynomial H_n(self).
   ! If present and TRUE, "normalise" gives values normalised for use
   ! in generating harmonic vibrational wavefunctions. See below.
      IN :: self
      INT, IN :: n
      BIN, IN, optional :: normalise
      REAL :: res
      REALVEC(:), PTR :: coeff
      REAL :: x
      INT :: i
      STACK("REAL:hermite_polynomial")
      START_TIMER("REAL:hermite_polynomial")
      coeff => hermite_polynomial_(n,normalise)
      res = ZERO
      x = ONE
      do i = 0,n
         res = res + coeff(i+1)*x
         x = x*self
      end do
      deallocate(coeff)
     STOP_TIMER("REAL:hermite_polynomial")
      CHECK
   end function

   function harmonic_vibrational_fn(self,n) result(res)
    REAL :: self
   ! Return "res", the value of the "n"th harmonic vibrational wavefunction
   ! as a function of the dimensionless normal coordinate "self"
      IN :: self
      INT, IN :: n
      REAL :: res
      REAL :: q
      STACK("REAL:harmonic_vibrational_fn")
      START_TIMER("REAL:harmonic_vibrational_fn")
      q = self
   !  fac = sqrt( ONE/ (sqrt(PI) * 2**n * n.factorial) )
      res = hermite_polynomial_(q,n,normalise=TRUE)*exp(-HALF*q*q)
     STOP_TIMER("REAL:harmonic_vibrational_fn")
      CHECK
   end function

   recursive function integral(self,a,b,accuracy) result(res)
   ! Integrate the function "self" between the limits "a" and "b"
   ! using adaptive trapezoidal rule with Simpsons approximation acceleration.
   ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result(res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: a,b
      REAL, IN, optional :: accuracy
      REAL :: res
      STACK("REAL:integral")
      START_TIMER("REAL:integral")
      res = integrate_adaptive_trapezoid_(self,a,b,accuracy)
     STOP_TIMER("REAL:integral")
      CHECK
   end function

   recursive function integrate_adaptive_trapezoid(self,a,b,accuracy) result(res)
   ! Integrate the function "self" between the limits "a" and "b"
   ! using adaptive trapezoidal rule with Simpsons approximation acceleration.
   ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result(res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: a,b
      REAL, IN, optional :: accuracy
      REAL :: res
      REAL :: tol,h,m,fa,fb,fm,one_trap,two_trap,left,right
      STACK("REAL:integrate_adaptive_trapezoid")
      START_TIMER("REAL:integrate_adaptive_trapezoid")
      tol = TOL(6)
      if (present(accuracy)) tol = accuracy
      h  = b-a
      m  = (a+b)/TWO
      fa = self(a)
      fb = self(b)
      fm = self(m)
      one_trap = h*(fa+fb)/TWO
      two_trap = h*(fa+TWO*fm+fb)/FOUR
      if (abs(one_trap-two_trap)<THREE*tol) then
         res = (FOUR*two_trap - one_trap)/THREE
      else
         left  = integrate_adaptive_trapezoid_(self,a,m,tol/TWO)
         right = integrate_adaptive_trapezoid_(self,m,b,tol/TWO)
         res = left + right
      end if
     STOP_TIMER("REAL:integrate_adaptive_trapezoid")
      CHECK
   end function

   recursive function integrate_adaptive_simpson(self,a,b,accuracy) result(res)
   ! Integrate the function "self" between the limits "a" and "b"
   ! using adaptive Simpson rule with acceleration.
   ! If present, "accuracy" is the required accuracy of the integral.
      interface
         function self(x) result(res)
            REAL, IN :: x
            REAL :: res
         end function
      end interface
      REAL, IN :: a,b
      REAL, IN, optional :: accuracy
      REAL :: res
      REAL :: tol,h,m1,m2,m3,fa,fb,f1,f2,f3,s1,s2,left,right
      STACK("REAL:integrate_adaptive_simpson")
      START_TIMER("REAL:integrate_adaptive_simpson")
      tol = TOL(6)
      if (present(accuracy)) tol = accuracy
      h   = (b-a)/TWO
      m1  = (THREE*a+b)/FOUR
      m2  = (a+b)/TWO
      m3  = (a+THREE*b)/FOUR
      fa = self(a)
      fb = self(b)
      f1 = self(m1)
      f2 = self(m2)
      f3 = self(m3)
      s1 = h*(fa+FOUR*f2+fb)/THREE
      s2 = h*(fa+FOUR*f1+TWO*f2+FOUR*f3+fb)/SIX
      if (abs(s1-s2)<15.0d0*tol) then
         res = (16.0d0*s2 - s1)/15.0d0
      else
         left  = integrate_adaptive_simpson_(self,a,m2,tol/TWO)
         right = integrate_adaptive_simpson_(self,m2,b,tol/TWO)
         res = left + right
      end if
     STOP_TIMER("REAL:integrate_adaptive_simpson")
      CHECK
   end function

   PURE function fermi_population(self,E_fermi,temperature) result(res)
    REAL :: self
   ! Returns the population of the level with energy "self".
   ! Input energies in Hartrees, temperature in Kelvin!!!!!
     IN :: self
     REAL, IN :: E_fermi,temperature
     REAL :: res
     REAL :: de,temp,de_on_temp
     de = (self - E_fermi) * JOULE_PER_HARTREE
     temp = temperature * BOLTZMANN_SI
     de_on_temp = de/temp
     if (de_on_temp > 500.0d0) then
       res = ZERO
     else
       if (de_on_temp < (-500.0d0)) then
         res = ONE
       else
         res = ONE / (E**(de_on_temp) + ONE)
         if (res < 1d-20) res = ZERO
         if (res > (1 - 1d-20)) res = ONE
       end if
     end if
     STOP_TIMER("REAL:fermi_population")
   end function

   subroutine minimise_bisect(self,val,previous,kept,tolerance,delta,done)
    REAL :: self
   ! Returns new "self" which should be better than old "self".  Do at least
   ! twice to be sure.  Works by the bisection method.
   ! Dummy variable "done" is returned true if it has converged.
   ! "previous" s automatically created and destroyed.
   ! row 1 is for y, row 2 is for x, for y=f(x).
     REALMAT(:,:), PTR :: previous
     REAL, IN :: val,tolerance,delta
     INT, INOUT :: kept
     BIN, INOUT :: done
     REAL :: x1,x2,x3,y1,y2,y3

     STACK("REAL:minimise_bisect")
     START_TIMER("REAL:minimise_bisect")
     done = FALSE
     if (NOT associated(previous)) then
       allocate(previous(3,2))
       kept = 0
     else
       ENSURE(size(previous,1) == 3,"REAL:minimise_bisect ... matrix has wrong dimension")
       ENSURE(size(previous,2) == 2,"REAL:minimise_bisect ... matrix has wrong dimension")
       ENSURE(kept <= 3 AND kept >=0,"REAL:minimise_bisect ... invalid value for variable 'kept'")
     end if
     select case (kept)
       case (0);      kept = 1
         previous(:,1) = (/val ,ZERO,ZERO/)
         previous(:,2) = (/self,ZERO,ZERO/)
         self = self + delta
         done = FALSE
       case (1);      kept = 2
         x1 = previous(1,2);      y1 = previous(1,1)
         if (self<x1) then
           previous(:,1) = (/val ,y1,ZERO/)
           previous(:,2) = (/self,x1,ZERO/)
           self = self - (x1-self)
         else
           previous(:,1) = (/y1,val ,ZERO/)
           previous(:,2) = (/x1,self,ZERO/)
           self = self + (self-x1)
         end if
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         if (y1<y2) then
           self = x1 - delta
         else
           self = x2 + delta
         end if
         done = FALSE
       case (2);      kept = 3
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         if (self<x1) then
           previous(:,1) = (/val ,y1,y2/)
           previous(:,2) = (/self,x1,x2/)
         else if (self>x2) then
           previous(:,1) = (/y1,y2,val /)
           previous(:,2) = (/x1,x2,self/)
         else
           previous(:,1) = (/y1,val ,y2/)
           previous(:,2) = (/x1,self,x2/)
         end if
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         x3 = previous(3,2);      y3 = previous(3,1)

         if (y2<y1) then
           if (y2<y3) then
             if (y1<y3) then
               self = (x1+x2)/2
             else
               self = (x2+x3)/2
             end if
           else
             self = x3 + HALF*(x3-x1)
           end if
         else !y1<y2
           self = x1 - HALF*(x3-x1)
         end if

         x1 = abs(previous(1,2)-self)
         x2 = abs(previous(2,2)-self)
         x3 = abs(previous(3,2)-self)
         y1 = max(x1,x2,x3)
         if (y1 < tolerance) then
           done = TRUE
           deallocate(previous)
         end if
       case (3)
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         x3 = previous(3,2);      y3 = previous(3,1)
         if (self<x1) then
           previous(:,1) = (/val ,y1,y2/)
           previous(:,2) = (/self,x1,x2/)
         else if (self>x3) then
           previous(:,1) = (/y2,y3,val /)
           previous(:,2) = (/x2,x3,self/)
         else
           if (self<x2) then
             if (y1<y3) then
               previous(:,1) = (/y1,val ,y2/)
               previous(:,2) = (/x1,self,x2/)
             else
               previous(:,1) = (/y1,y2,val /)
               previous(:,2) = (/x1,x2,self/)
             end if
           else
             if (y1>y3) then
               previous(:,1) = (/y2,val ,y3/)
               previous(:,2) = (/x2,self,x3/)
             else
               previous(:,1) = (/y1,y2,val /)
               previous(:,2) = (/x1,x2,self/)
             end if
           end if
         end if
         x1 = previous(1,2);      y1 = previous(1,1)
         x2 = previous(2,2);      y2 = previous(2,1)
         x3 = previous(3,2);      y3 = previous(3,1)

         if (y2<y1) then
           if (y2<y3) then
             if (y1<y3) then
               self = (x1+x2)/2
             else
               self = (x2+x3)/2
             end if
           else
             self = x3 + HALF*(x3-x1)
           end if
         else !y1<y2
           self = x1 - HALF*(x3-x1)
         end if

         x1 = abs(previous(1,2)-self)
         x2 = abs(previous(2,2)-self)
         x3 = abs(previous(3,2)-self)
         y1 = max(x1,x2,x3)
         if (y1 < tolerance) then
           done = TRUE
           deallocate(previous)
         end if
     end select
     STOP_TIMER("REAL:minimise_bisect")
      UNSTACK
   end subroutine

   PURE function z_from_p(self) result(res)
    REAL :: self
   ! Produces the normal deviate Z corresponding to "self", a given lower
   ! tail area of P; Z is accurate to about 1 part in 10**16.
   ! Adapted from the Royal Statistical Society f77 routine "PPND16".
   ! algorithm AS241  appl. statist. (1988) vol 37, no 3.
     IN :: self
     REAL :: res
     REAL :: q,r
     q = self - HALF
     if (abs(q) < 0.425D0) then
       r = 0.180625D0 - q * q
       res = q * (((((((2.5090809287301226727D+3 * r + &
       3.3430575583588128105D+4) * r + 6.7265770927008700853D+4) * r + &
       4.5921953931549871457D+4) * r + 1.3731693765509461125D+4) * r + &
       1.9715909503065514427D+3) * r + 1.3314166789178437745D+2) * r + &
       3.3871328727963666080D0) / (((((((5.2264952788528545610D+3 * r + &
       2.8729085735721942674D+4) * r + 3.9307895800092710610D+4) * r + &
       2.1213794301586595867D+4) * r + 5.3941960214247511077D+3) * r + &
       6.8718700749205790830D+2) * r + 4.2313330701600911252D+1) * r + ONE)
     else
       if (q < ZERO) then
         r = self
       else
         r = ONE - self
       end if
       if (r < ZERO) then
         
         res = ZERO
       else
         r = sqrt(-log(r))
         if (r < FIVE) then
           r = r - 1.6D0
           res = (((((((7.74545014278341407640D-4 * r + &
           2.27238449892691845833D-2) * r + 2.41780725177450611770D-1) * r + &
           1.27045825245236838258D0) * r + 3.64784832476320460504D0) * r + &
           5.76949722146069140550D0) * r + 4.63033784615654529590D0) * r + &
           1.42343711074968357734D0) / (((((((1.05075007164441684324D-9 * r + &
           5.47593808499534494600D-4) * r + 1.51986665636164571966D-2) * r + &
           1.48103976427480074590D-1) * r + 6.89767334985100004550D-1) * r + &
           1.67638483018380384940D0) * r + 2.05319162663775882187D0) * r + ONE)
         else
           r = r - FIVE
           res = (((((((2.01033439929228813265D-7 * r + &
           2.71155556874348757815D-5) * r + 1.24266094738807843860D-3) * r + &
           2.65321895265761230930D-2) * r + 2.96560571828504891230D-1) * r + &
           1.78482653991729133580D0) * r + 5.46378491116411436990D0) * r + &
           6.65790464350110377720D0) / (((((((2.04426310338993978564D-15 * r + &
           1.42151175831644588870D-7) * r + 1.84631831751005468180D-5) * r + &
           7.86869131145613259100D-4) * r + 1.48753612908506148525D-2) * r + &
           1.36929880922735805310D-1) * r + 5.99832206555887937690D-1) * r + ONE)
         end if
         if (q < ZERO) res = - res
       end if
     end if
     STOP_TIMER("REAL:z_from_p")
   end function

!   s_from_p(p,mean)
!   ! Calculate the Poisson upper limit of cumulation "s" from the cumulation "p".
!   ! self is p, function returns s.
!       p,mean :: REAL, IN
!
!C      ..
!C      .. Scalar Arguments ..
!       REAL bound,p,s,xlam
!       INTEGER status,which
!C      ..
!C      .. Local Scalars ..
!       REAL fx,pp,ss,xxlam
!       LOGICAL qhi,qleft
!C      ..
!C      .. External Functions ..
!       REAL cumpoi
!       EXTERNAL cumpoi
!C      ..
!C      .. External Subroutines ..
!       EXTERNAL invr,stinvr
!
!       ENSURE(p>ZERO, "p not greater than zero")
!       ENSURE(p<ONE, "p not less than one")
!       ENSURE(xlam>ZERO,"xlam not > 0")
!
!           res = FIVE
!           CALL stinvr(ZERO,TEN**30,HALF,HALF,FIVE,TOL(4),TOL(4))
!           status = 0
!           CALL invr(status,res,fx,qleft,qhi)
!           do
!             if (status/=1) exit
!             fx = cumpoi(res,xlam) - p
!             CALL invr(status,res,fx,qleft,qhi)
!           end
!
!           if (status==-1) then
!             if (NOT (qleft)) then
!               status = 2
!               bound = inf
!             else
!               status = 1
!               bound = 0.0
!             end
!           end
!   end

   subroutine convert_to(self,units)
    REAL :: self
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      INOUT :: self
      STR(*), IN :: units
      REAL :: factor
      STACK("REAL:convert_to")
      START_TIMER("REAL:convert_to")
      ENSURE(is_known_unit_(units),"REAL:convert_to ... unknown units, " // units)
      factor = conversion_factor_(units)
      self = self * factor
     STOP_TIMER("REAL:convert_to")
      CHECK
   end subroutine

   function to_units(self,units) result(res)
    REAL :: self
   ! Convert the number "self" in atomic units or generic units to a
   ! new number in "units".
      IN :: self
      STR(*), IN :: units
      REAL :: res
      REAL :: factor
      STACK("REAL:to_units")
      START_TIMER("REAL:to_units")
      ENSURE(is_known_unit_(units),"REAL:to_units ... unknown units, " // units)
      factor = conversion_factor_(units)
      res = self * factor
     STOP_TIMER("REAL:to_units")
      CHECK
   end function

   subroutine convert_from(self,units)
    REAL :: self
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      INOUT :: self
      STR(*), IN :: units
      REAL :: factor
   STACK("REAL:convert_from")
   START_TIMER("REAL:convert_from")
   ENSURE(is_known_unit_(units),"REAL:convert_from ... unknown units, " // units)
      factor = ONE/(conversion_factor_(units))
      self = self * factor
     STOP_TIMER("REAL:convert_from")
      CHECK
   end subroutine

   function from_units(self,units) result(res)
    REAL :: self
   ! Convert the number "self" from "units" system to a new number
   ! in atomic units or generic units.  Returns "err" whether it was successful.
      IN :: self
      STR(*), IN :: units
      REAL :: res
      REAL :: factor
   STACK("REAL:from_units")
   START_TIMER("REAL:from_units")
   ENSURE(is_known_unit_(units),"REAL:from_units ... unknown units, " // units)
      factor = ONE/(conversion_factor_(units))
      res = self * factor
     STOP_TIMER("REAL:from_units")
      CHECK
   end function

   function is_convertible_to(self,unit) result(res)
    REAL :: self
   ! Return TRUE if "unit" is a known unit which can be used for conversion
   ! of "self".
      STR(*), IN :: unit
      BIN :: res
      STACK("REAL:is_convertible_to")
      START_TIMER("REAL:is_convertible_to")
      res = is_known_unit_(unit)
     STOP_TIMER("REAL:is_convertible_to")
      CHECK
   end function

   subroutine swap_with(self,x)
    REAL :: self
   ! Swap the value of "self" and "x"
       REAL :: x
      REAL :: keep
      STACK("REAL:swap_with")
      START_TIMER("REAL:swap_with")
      keep = self
      self = x
      x = keep
     STOP_TIMER("REAL:swap_with")
      CHECK
   end subroutine

! *********************
! Root finding routines
! *********************

   subroutine bracket_root(self,x1,x2,factor,max_it)
   ! Given a function self(x) and initial points "x1" and "x2", bracket a root
   ! of self(x) by expansion. If "factor" is present it is used as the (linear)
   ! interval expansion factor. If "max_it" is present then it is the number of
   ! times the interval is expanded.
      interface
         function self(x) result(res)
            REAL :: x,res
         end function
      end interface
      REAL :: x1,x2
      REAL, optional :: factor
      INT, optional :: max_it
      INT :: j,maxit
      REAL :: f1,f2,fac
      STACK("REAL:bracket_root")
      START_TIMER("REAL:bracket_root")
      ENSURE(x1/=x2,"REAL:bracket_root ... non-zero range (x1,x2) required")
      fac = 1.6d0
      if (present(factor)) fac = factor
      maxit = 50
      if (present(max_it)) maxit = max_it
      f1 = self(x1)
      f2 = self(x2)
      do j = 1,maxit
         if (f1*f2<ZERO) then; STOP_TIMER("REAL:bracket_root") CHECK return; end if
         if (abs(f1)<abs(f2)) then
            x1 = x1 + fac*(x1-x2)
            f1 = self(x1)
         else
            x2 = x2 + fac*(x2-x1)
            f2 = self(x2)
         end if
      end do
      DIE("REAL:bracket_root ... Exceeded maximum number of iterations")
     STOP_TIMER("REAL:bracket_root")
      CHECK
   end subroutine

   subroutine find_root_brent(self,x1,x2,root,tol,val,max_it)
   ! Given a function self(x) and initial points "x1" and "x2" which bracket a
   ! root of self, find the "root" to a precision "tol". If "val" present, then
   ! the root which makes self have this value is returned (an isovalue).
   ! If "max_it" is present it is set to the maximum number of iterations.
      interface
         function self(x) result(res)
            REAL :: x,res
         end function
      end interface
      REAL :: x1,x2,root,tol
      REAL, optional :: val
      INT, optional :: max_it
      INT :: iter
      REAL :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      INT :: maxit = 100
      REAL :: iso = ZERO
      REAL :: eps = TOL(8)
      STACK("REAL:find_root_brent")
      START_TIMER("REAL:find_root_brent")
      if (present(max_it)) maxit = max_it
      if (present(val))    iso = val
      a  = x1
      b  = x2
      fa = self(a) - iso
      fb = self(b) - iso
      ENSURE((fa>0 AND fb>0) OR (fa<0 AND fb<0),"REAL:find_root_brent ... root is not bracketed")
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
            root = b
            STOP_TIMER("REAL:find_root_brent") CHECK return
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
         fb = self(b) - iso
      end do
      DIE("REAL:find_root_brent ... maximum iterations exceeded")
     STOP_TIMER("REAL:find_root_brent")
      CHECK
   end subroutine

! *********************
! Minimization routines
! *********************

   subroutine bracket_minimum(self,a,b,c,fa,fb,fc)
   ! Given a function self(x) and initial points "a" and "b" search in
   ! the downhill direction and return points "a", "b" and "c" that bracket
   ! a minimum of the function, and return the value of the function
   ! "fa", "fb", and "fc" at these points. NOTE: "c" is not used initially.
      interface
         function self(x) result(res)
            REAL :: x,res
         end function
      end interface
      REAL :: a,b,c,fa,fb,fc
      REAL :: gold = 1.618034
      REAL :: glimit = 100
      REAL :: tiny = 1.0d-20
      REAL :: fu,q,r,u,ulim
      STACK("REAL:bracket_minimum")
      START_TIMER("REAL:bracket_minimum")
      fa = self(a)
      fb = self(b)
      if (fb>fa) then
        call swap_with_(a,b)
        call swap_with_(fa,fb)
      end if
      c  = b + gold*(b-a)
      fc = self(c)
      do
         if (fb<fc) exit                  ! bracket found
         r = (b-a)*(fb-fa)                ! get u by parabolic extrapolation
         q = (b-c)*(fb-fa)
         u = b - ((b-c)*q-(b-a)*r)/(TWO*sign(max(abs(q-r),tiny),q-r))
         ulim = b + glimit*(c-b)
         if ((b-u)*(u-c)>ZERO) then       ! Parabolic u lies between b and c
            fu = self(u)
            if (fu<fc) then               ! got a minimum between b and c
               a = b; fa = fb
               b = u; fb = fu
               exit
            else if (fu>fb) then          ! got a minimum between a and u
               c = u; fc = fu
               exit
            end if
            u = c + gold*(c-b)            ! parabolic fit no use, so magnify
            fu = self(u)
         else if ((c-u)*(u-ulim)>0) then ! Fit is between c and its allowed limit
            fu = self(u)
            if (fu<fc) then
               b = c; fb = fc
               c = u; fc = fu
               u = c + gold*(c-b)
               fu = self(u)
            end if
         else if ((u-ulim)*(ulim-c)>0) then
            u = ulim
            fu = self(u)
         else
            u = c + gold*(c-b)            ! magnify
            fu = self(u)
         end if
         a = b; fa = fb
         b = c; fb = fc
         c = u; fc = fu
      end do
      if (a>c) then
         call swap_with_(a,c)
         call swap_with_(fa,fc)
      end if
     STOP_TIMER("REAL:bracket_minimum")
      CHECK
   end subroutine

   subroutine minimise_golden(self,a,b,c,xmin,f,tol)
   ! Given a function self(x) and initial points "a", "b" and "c"
   ! which bracket a minimum, return the minimum point "xmin" and the
   ! value "f" at the minimum to a precision "tol" using the golden
   ! section search method.
      interface
         function self(x) result(res)
            REAL :: x,res
         end function
      end interface
      REAL :: a,b,c,xmin,f,tol
      REAL :: r = 0.618033399
      REAL :: s,f1,f2,x0,x1,x2,x3
      STACK("REAL:minimise_golden")
      START_TIMER("REAL:minimise_golden")
      s = ONE - r
      x0 = a
      x3 = c
      if (abs(c-b)>abs(b-a)) then
         x1 = b; x2 = b + s*(c-b)
      else
         x2 = b; x1 = b - s*(b-a)
      end if
      f1 = self(x1)
      f2 = self(x2)
      do
         if (abs(x3-x0)<=tol*(abs(x1)+abs(x2))) exit
         if (f2<f1) then
            x0 = x1
            x1 = x2
            x2 = r*x1 + s*x3
            f1 = f2
            f2 = self(x2)
         else
            x3 = x2
            x2 = x1
            x1 = r*x2 + s*x0
            f2 = f1
            f1 = self(x1)
         end if
      end do
      if (f1<f2) then; f = f1; xmin = x1
      else;            f = f2; xmin = x2
      end if
     STOP_TIMER("REAL:minimise_golden")
      CHECK
   end subroutine

   subroutine minimise_brent(self,a,b,c,xmin,f,tol)
   ! Given a function self(x) and initial points "a", "b" and "c"
   ! which bracket a minimum, return the minimum point "xmin" and the
   ! value "f" at the minimum to a precision "tol" using Brent's method.
      interface
         function self(x) result(res)
            REAL :: x,res
         end function
      end interface
      REAL :: a,b,c,xmin,f,tol
      INT :: itmax = 100
      REAL :: cgold = 0.3819660
      REAL :: zeps = TOL(10)
      REAL :: d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
      INT :: iter
      BIN :: failed
      STACK("REAL:minimise_brent")
      START_TIMER("REAL:minimise_brent")
      if (a>c) call swap_with_(a,c)
      v = b
      b = c
      w = v
      x = v
      fx = self(x)
      fv = fx
      fw = fx
      e = ZERO
      failed = TRUE
      do iter = 1,itmax
         xm = HALF*(a+b)
         tol1 = tol*abs(x) + zeps
         tol2 = TWO*tol1
         if (abs(x-xm)<(tol2-HALF*(b-a))) then
            failed = FALSE
            exit
         end if
         if (abs(e)>tol1) then
            r = (x-w)*(fx-fv)
            q = (x-v)*(fx-fw)
            p = (x-v)*q - (x-w)*r
            q = TWO*(q-r)
            if (q>ZERO) p = -p
            q = abs(q)
            etemp = e
            e = d
            if (abs(p)>=abs(HALF*q*etemp) OR p<=q*(a-x) OR p>=q*(b-x)) then
              if (x>=xm) then; e = a-x
              else;            e = b-x
              end if
              d = cgold*e
            else
              d = p/q
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
         fu = self(u)
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
      end do
      f = fx
      xmin = x
      ENSURE(NOT failed,"REAL:minimise_brent ... maximum iterations exceeded")
     STOP_TIMER("REAL:minimise_brent")
      CHECK
   end subroutine

   function test(self) result(res)
    REAL :: self
   !  A test function for minimising
      REAL :: res
      REAL :: x
      STACK("REAL:test")
      START_TIMER("REAL:test")
      x = self
      res = (x-1)*(x-1) + 1
     STOP_TIMER("REAL:test")
      CHECK
   end function

   subroutine to_random_normal(self)
    REAL :: self
   ! Set self to be a normal random number.
   ! From 488 in toms from Netlib.
   ! ALGORITHM APPEARED IN COMM. ACM, VOL. 17, NO. 12, P. 704.
     OUT :: self
      REALVEC(60) :: D = (/0.674489750,0.475859630,0.383771164, &
     0.328611323,0.291142827,0.263684322, &
     0.242508452,0.225567444,0.211634166,0.199924267,0.189910758,0.181225181, &
     0.173601400,0.166841909,0.160796729,0.155349717,0.150409384,0.145902577, &
     0.141770033,0.137963174,0.134441762,0.131172150,0.128125965,0.125279090, &
     0.122610883,0.120103560,0.117741707,0.115511892,0.113402349,0.111402720, &
     0.109503852,0.107697617,0.105976772,0.104334841,0.102766012,0.101265052, &
     0.099827234,0.098448282,0.097124309,0.095851778,0.094627461,0.093448407, &
     0.092311909,0.091215482,0.090156838,0.089133867,0.088144619,0.087187293, &
     0.086260215,0.085361834,0.084490706,0.083645487,0.082824924,0.082027847, &
     0.081253162,0.080499844,0.079766932,0.079053527,0.078358781,0.077681899/)
     REAL :: A,W,V
     REAL, SAVE :: U
     INT :: I,J,N
     BIN, SAVE :: first
     STACK("REAL:to_random_normal")
     START_TIMER("REAL:to_random_normal")
     DATA U /ZERO/
     DATA first /TRUE/

     ! Note that on the first call, self is returned as zero, so to get around
     ! this, we iterate twice.  On all subsequent calls, iterate only once.
     N=1
     if (first) N=2
     do J=1,N
       A = ZERO
       I = 0
       do
         U = U + U
         if (U < ONE) exit
         U = U - ONE
         I = I + 1
         A = A - D(I)
       end do
       outer : do
         W = D(I+1)*U
         V = W*(HALF*W-A)
         do
           call random_number(U)
           if (V<=U) exit outer
           call random_number(V)
           if (U<=V) exit
         end do
         U = (V-U)/(ONE-U)
       end do outer
       U = (U-V)/(ONE-V)
       U = U + U
       if (U >= ONE) then
         U = U - ONE
         self = W - A
       else
         self = A - W
       end if
     end do
     first=FALSE
     STOP_TIMER("REAL:to_random_normal")
      CHECK
   end subroutine

end

