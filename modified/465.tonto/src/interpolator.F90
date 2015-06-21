!------------------------------------------------------------------------------
!
! INTERPOLATOR:
!
! For interpolating values from a 1-D table.
!
! The module takes as input some "data_point"'s with their assigned "values".
! Any data value is then assigned a value by interpolating between the two
! nearest data points.
! 
! The module allows for even-spaced or uneven-spaced data. In the former case
! all the "data_point"'s are not required: only the first data point and the
! "spacing".
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
! $Id: interpolator.foo,v 1.1.2.8 2004/04/21 09:12:54 reaper Exp $
!
!------------------------------------------------------------------------------

module INTERPOLATOR_MODULE

#  include "interpolator.use"

   implicit none

#  include "macros"
#  include "interpolator.int"


contains

   subroutine create(self,spacing,first)
    INTERPOLATOR :: self
   ! Create the object
     PTR :: self
     REAL, optional :: spacing,first
     STACK("INTERPOLATOR:create")
     START_TIMER("INTERPOLATOR:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(INTERPOLATOR_SIZE)
     call nullify_ptr_part_(self)
     call set_defaults_(self,spacing,first)
     STOP_TIMER("INTERPOLATOR:create")
      UNSTACK
   end subroutine

   subroutine create_copy(self,object)
    INTERPOLATOR :: self
   ! Create a copy of object
     INTERPOLATOR :: object
     PTR :: self
   ! The following code is inherited from OBJECT
     STACK("INTERPOLATOR:create_copy")
     START_TIMER("INTERPOLATOR:create_copy")
     call create_(self)
     call copy_(self,object)
     STOP_TIMER("INTERPOLATOR:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,c)
    INTERPOLATOR :: self
   ! Copy the contents of "c" to self.
     INTERPOLATOR, IN :: c
     STACK("INTERPOLATOR:copy")
     START_TIMER("INTERPOLATOR:copy")
     self = c
     if (associated(c%data_point)) &
        call create_copy_(self%data_point,c%data_point)
     if (associated(c%data_value)) &
        call create_copy_(self%data_value,c%data_value)
     STOP_TIMER("INTERPOLATOR:copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTERPOLATOR :: self
   ! Destroy an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("INTERPOLATOR:destroy")
      START_TIMER("INTERPOLATOR:destroy")
      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)
        DELETE_MEMORY(INTERPOLATOR_SIZE)
      end if
     STOP_TIMER("INTERPOLATOR:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    INTERPOLATOR :: self
   ! Nullify the pointer parts
      STACK("INTERPOLATOR:nullify_ptr_part")
      START_TIMER("INTERPOLATOR:nullify_ptr_part")
      nullify(self%data_point)
      nullify(self%data_value)
     STOP_TIMER("INTERPOLATOR:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    INTERPOLATOR :: self
   ! Destroy the pointer parts
      STACK("INTERPOLATOR:destroy_ptr_part")
      START_TIMER("INTERPOLATOR:destroy_ptr_part")
      call destroy_(self%data_point)
      call destroy_(self%data_value)
     STOP_TIMER("INTERPOLATOR:destroy_ptr_part")
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

   subroutine set_defaults(self,spacing,first)
    INTERPOLATOR :: self
   ! Set up a defaults
      REAL, optional :: spacing,first
      STACK("INTERPOLATOR:set_defaults")
      START_TIMER("INTERPOLATOR:set_defaults")
      self%n_data = 0
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      self%spacing = ZERO
      self%finalised = FALSE
      if (present(spacing)) call set_spacing_(self,spacing)
      if (present(first))   call set_first_data_point_(self,first)
     STOP_TIMER("INTERPOLATOR:set_defaults")
      UNSTACK
   end subroutine

   subroutine set_spacing(self,spacing)
    INTERPOLATOR :: self
   ! Set the "spacing" of an even-spaced interpolator
      REAL :: spacing
   STACK("INTERPOLATOR:set_spacing")
   START_TIMER("INTERPOLATOR:set_spacing")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_spacing ... spacing must be +ve")
      self%spacing = spacing
     STOP_TIMER("INTERPOLATOR:set_spacing")
      UNSTACK
   end subroutine

   subroutine set_first_data_point(self,first)
    INTERPOLATOR :: self
   ! Set the "first" datum of an even-spaced interpolator
      REAL :: first
   STACK("INTERPOLATOR:set_first_data_point")
   START_TIMER("INTERPOLATOR:set_first_data_point")
   ENSURE(self%spacing>ZERO,"INTERPOLATOR:set_first_data_point ... spacing must be +ve")
      call destroy_(self%data_point)
      call create_(self%data_point,1)
      self%data_point(1) = first
     STOP_TIMER("INTERPOLATOR:set_first_data_point")
      UNSTACK
   end subroutine

   function is_even_spaced(self) result(res)
    INTERPOLATOR :: self
   ! Returns TRUE if the interpolator uses even spaced data points.
      BIN :: res
      STACK("INTERPOLATOR:is_even_spaced")
      START_TIMER("INTERPOLATOR:is_even_spaced")
      res = self%spacing > ZERO 
     STOP_TIMER("INTERPOLATOR:is_even_spaced")
      CHECK
   end function

   function first_data_point(self) result(res)
    INTERPOLATOR :: self
   ! Returns the first data point
      REAL :: res
   STACK("INTERPOLATOR:first_data_point")
   START_TIMER("INTERPOLATOR:first_data_point")
   ENSURE(self%finalised,"INTERPOLATOR:first_data_point ... Not finalised")
   ENSURE(self%n_data>0,"INTERPOLATOR:first_data_point ... No data")
   ENSURE(associated(self%data_point),"INTERPOLATOR:first_data_point ... No data")
      res = self%data_point(1)
     STOP_TIMER("INTERPOLATOR:first_data_point")
      CHECK
   end function

   function last_data_point(self) result(res)
    INTERPOLATOR :: self
   ! Returns the last data point
      REAL :: res
   STACK("INTERPOLATOR:last_data_point")
   START_TIMER("INTERPOLATOR:last_data_point")
   ENSURE(self%finalised,"INTERPOLATOR:last_data_point ... Not finalised")
   ENSURE(self%n_data>0,"INTERPOLATOR:last_data_point ... No data")
   ENSURE(associated(self%data_point),"INTERPOLATOR:last_data_point ... No data")
      res = self%data_point(self%n_data)
     STOP_TIMER("INTERPOLATOR:last_data_point")
      CHECK
   end function

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    INTERPOLATOR :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("INTERPOLATOR:read_keywords")
     START_TIMER("INTERPOLATOR:read_keywords")
     ENSURE(next_item_(stdin)=="{","INTERPOLATOR:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("INTERPOLATOR:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    INTERPOLATOR :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("INTERPOLATOR:process_keyword")
      START_TIMER("INTERPOLATOR:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}               ")  ! exit case
         case ("data_points=    "); call read_data_points_(self)
         case ("data_values=    "); call read_data_values_(self)
         case ("kind=           "); call read_kind_(self)
         case ("put             "); call put_(self)
         case ("spacing=        "); call read_spacing_(self)
         case ("units=          "); call read_units_(self)
         case default;         allocate(tonto%known_keywords(7))
         tonto%known_keywords(1) = "}               "
         tonto%known_keywords(2) = "data_points=    "
         tonto%known_keywords(3) = "data_values=    "
         tonto%known_keywords(4) = "kind=           "
         tonto%known_keywords(5) = "put             "
         tonto%known_keywords(6) = "spacing=        "
         tonto%known_keywords(7) = "units=          "
         call unknown_(tonto,word,"INTERPOLATOR:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("INTERPOLATOR:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    INTERPOLATOR :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("INTERPOLATOR:read_units")
      START_TIMER("INTERPOLATOR:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("INTERPOLATOR:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    INTERPOLATOR :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("INTERPOLATOR:read_junk")
      START_TIMER("INTERPOLATOR:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("INTERPOLATOR:read_junk")
      CHECK
   end subroutine

   subroutine read_kind(self)
    INTERPOLATOR :: self
   ! Read the kind of interpolation to use.
      STACK("INTERPOLATOR:read_kind")
      START_TIMER("INTERPOLATOR:read_kind")
      call read_(stdin,self%interp_kind)
      call to_lower_case_(self%interp_kind)
      select case (self%interp_kind)
         case ("linear")
         case ("logarithmic")
         case default; allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "linear"
         tonto%known_keywords(2) = "logarithmic"
         call unknown_(tonto,self%interp_kind,"INTERPOLATOR:read_kind")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("INTERPOLATOR:read_kind")
      CHECK
   end subroutine

   subroutine read_data_points(self)
    INTERPOLATOR :: self
   ! Read in the data points.
      STACK("INTERPOLATOR:read_data_points")
      START_TIMER("INTERPOLATOR:read_data_points")
      call read_ptr_(stdin,self%data_point)
      if (is_even_spaced_(self)) then
         ENSURE(size(self%data_point)==1,"INTERPOLATOR:read_data_points ... specify only first data point for even spaced interpolation")
      end if
     STOP_TIMER("INTERPOLATOR:read_data_points")
      UNSTACK
   end subroutine

   subroutine read_data_values(self)
    INTERPOLATOR :: self
   ! Read in the data values.
      STACK("INTERPOLATOR:read_data_values")
      START_TIMER("INTERPOLATOR:read_data_values")
      call read_ptr_(stdin,self%data_point)
     STOP_TIMER("INTERPOLATOR:read_data_values")
      UNSTACK
   end subroutine

   subroutine read_spacing(self)
    INTERPOLATOR :: self
   ! Read in the spacing
      REAL :: spacing
   STACK("INTERPOLATOR:read_spacing")
   START_TIMER("INTERPOLATOR:read_spacing")
   ENSURE(NOT associated(self%data_point) OR size(self%data_point)==1,"INTERPOLATOR:read_spacing ... can't have more than one data point")
      call read_(stdin,spacing)
      call set_spacing_(self,spacing)
     STOP_TIMER("INTERPOLATOR:read_spacing")
      UNSTACK
   end subroutine

   subroutine finalise(self)
    INTERPOLATOR :: self
   ! Check everything is OK after reading data. Saturation must exist but it is
   ! applied into the RGB triples and then destroyed.
      INTVEC(:), PTR :: order
      STACK("INTERPOLATOR:finalise")
      START_TIMER("INTERPOLATOR:finalise")
      ENSURE(associated(self%data_point),"INTERPOLATOR:finalise ...  no data_point's")
      ENSURE(associated(self%data_value),"INTERPOLATOR:finalise ...  no data_value's")
      if (is_even_spaced_(self)) then
         self%n_data = size(self%data_value)
         call set_even_spaced_data_points_(self)
      else
         ENSURE(size(self%data_point)==size(self%data_value),"INTERPOLATOR:finalise ... inconsistent data/value lengths")
         ENSURE(size(self%data_point)>2,"INTERPOLATOR:finalise ... not enough data_point's")
         self%n_data = size(self%data_point)
         call create_(order,self%n_data)
         ! Re-order from smallest to largest
         call quick_sort_(self%data_point,order)
         self%data_point = self%data_point(order)
         self%data_value = self%data_value(order)
         call destroy_(order)
      end if
      self%finalised = TRUE
     STOP_TIMER("INTERPOLATOR:finalise")
      UNSTACK
   end subroutine

   subroutine set_even_spaced_data_points(self)
    INTERPOLATOR :: self
   ! Set the ".data_point" to be even-spaced, assuming .n_data has been set:
   ! this can only be done when the ".data_point" array contains one element.
      INT :: i
      REAL :: p
   STACK("INTERPOLATOR:set_even_spaced_data_points")
   START_TIMER("INTERPOLATOR:set_even_spaced_data_points")
   ENSURE(is_even_spaced_(self),"INTERPOLATOR:set_even_spaced_data_points ... not even spaced")
   ENSURE(size(self%data_point)==1,"INTERPOLATOR:set_even_spaced_data_points ... data_point.dim/=1")
   ENSURE(self%n_data>0,"INTERPOLATOR:set_even_spaced_data_points ... there is no data")
      p = self%data_point(1)
      call destroy_(self%data_point)
      call create_(self%data_point,self%n_data)
      do i = 1,self%n_data
         self%data_point(i) = p
         p = p + self%spacing
      end do
     STOP_TIMER("INTERPOLATOR:set_even_spaced_data_points")
      UNSTACK
   end subroutine

   subroutine set_even_spaced_data_points_1(self,spacing,first,length)
    INTERPOLATOR :: self
   ! Set the ".data_point" to be even-spaced. In this routine .n_data must not
   ! have been set, nor ".data_values". Instead .n_data is worked out from the
   ! "first" data value, the "spacing", and the "length" of the interpolation
   ! region.
      REAL :: spacing,first,length
   STACK("INTERPOLATOR:set_even_spaced_data_points_1")
   START_TIMER("INTERPOLATOR:set_even_spaced_data_points_1")
   ENSURE(self%n_data==0,"INTERPOLATOR:set_even_spaced_data_points_1 ... there is already data")
   ENSURE(NOT associated(self%data_value),"INTERPOLATOR:set_even_spaced_data_points_1 ... there are already data_values")
      call set_spacing_(self,spacing)
      call set_first_data_point_(self,first)
      self%n_data = floor(length/spacing) + 1
      call set_even_spaced_data_points_(self)
     STOP_TIMER("INTERPOLATOR:set_even_spaced_data_points_1")
      UNSTACK
   end subroutine

   subroutine set_even_spaced_data(self,first,spacing,length,func,tol)
    INTERPOLATOR :: self
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending for a total "length", and with ".data_value"
   ! to be the corresponding values evaluated using monotonically decreasing
   ! function "func". The number of data values is worked out from where
   ! function "func" is greater than "tol". 
      REAL :: spacing,first,length,tol
      interface
         function func(point) result(value)
            REAL :: point
            REAL :: value
         end function
      end interface
      INT :: max_data
      INT :: i,s,n
      REAL :: p,val
   STACK("INTERPOLATOR:set_even_spaced_data")
   START_TIMER("INTERPOLATOR:set_even_spaced_data")
   ENSURE(self%n_data==0,"INTERPOLATOR:set_even_spaced_data ... there is already data")
   ENSURE(NOT associated(self%data_value),"INTERPOLATOR:set_even_spaced_data ... there are already data_values")
   ENSURE(NOT associated(self%data_point),"INTERPOLATOR:set_even_spaced_data ... there are already data_points")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_even_spaced_data ... spacing must be +ve")
      self%spacing = spacing
      max_data = floor((length-first)/spacing) + 1
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)
    ! ENSURE(func(first)>tol_max,"tol_max initially too small")
    ! ! Find first data_point
    ! p = first 
    ! do i = 1,max_data
    !    val = func(p)
    !    if (val<tol_max) exit
    !    p = p + spacing
    ! end
    ! ENSURE(val<tol_max,"tol_max finally too small")
    ! ! Find last data_point
    ! .data_point(1) = p - spacing
    ! .data_value(1) = val
    ! s = i
    ! n = 2
      p = first 
      s = 0
      n = 1
      do i = s+1,max_data
         val = func(p)
         self%data_point(n) = p
         self%data_value(n) = val
         if (val<tol) exit
         p = p + spacing
         n = n + 1
      end do
      ENSURE(val<tol,"INTERPOLATOR:set_even_spaced_data ... data table not large enough")
      self%n_data = n-1
      call shrink_(self%data_value,self%n_data)
      call shrink_(self%data_point,self%n_data)
      if (self%interp_kind=="logarithmic") then
         ENSURE(all(self%data_value>ZERO),"INTERPOLATOR:set_even_spaced_data ... -ve data_values cant be used with log interpolation")
         self%data_value = log(self%data_value)
      end if
      self%finalised = TRUE
     STOP_TIMER("INTERPOLATOR:set_even_spaced_data")
      UNSTACK
   end subroutine

!  ************
!  Set routines
!  ************

   subroutine set_data_points(self,points)
    INTERPOLATOR :: self
   ! Set the data "points". NOTE: Make sure that .finalise is called after all
   ! set routines.
      REALVEC(:) :: points
      STACK("INTERPOLATOR:set_data_points")
      START_TIMER("INTERPOLATOR:set_data_points")
      call destroy_(self%data_point)
      call create_copy_(self%data_point,points)
     STOP_TIMER("INTERPOLATOR:set_data_points")
      UNSTACK
   end subroutine

   subroutine set_data_values(self,values)
    INTERPOLATOR :: self
   ! Set the data "values". NOTE: Make sure that .finalise is called after all
   ! set routines.
      REALVEC(:) :: values
      STACK("INTERPOLATOR:set_data_values")
      START_TIMER("INTERPOLATOR:set_data_values")
      call destroy_(self%data_value)
      call create_copy_(self%data_value,values)
     STOP_TIMER("INTERPOLATOR:set_data_values")
      UNSTACK
   end subroutine

!  *****************
!  Values for points
!  *****************

   function value_for(self,point) result(res)
    INTERPOLATOR :: self
   ! Return the interpolated value for "point".
      IN :: self
      REAL, IN :: point
      REAL :: res
      REAL :: del,frac
      INT :: i1,i2
   STACK("INTERPOLATOR:value_for")
   START_TIMER("INTERPOLATOR:value_for")
   ENSURE(self%finalised,"INTERPOLATOR:value_for ... not finalised")
      if (is_even_spaced_(self)) then ! This is more efficient than below
         frac = (point - self%data_point(1))/self%spacing
         i1 = floor(frac)
         if (frac>i1) then
           i2 = i1+1
           if (-1<i1 AND i2<self%n_data) then
              del  = self%data_value(i1+2) - self%data_value(i2)
              res  = self%data_value(i2) + (frac-i1)*del
              if (self%interp_kind=="logarithmic") res = exp(res)
           else
              res = ZERO
           end if
         else
           if (-1<i1 AND i1<self%n_data) then
              res  = self%data_value(i1+1)
              if (self%interp_kind=="logarithmic") res = exp(res)
           else
              res = ZERO
           end if
         end if
      else
         if (is_in_range_(point,range_(self%data_point))) then
            i1 = count(self%data_point<=point) ! assuming .data is ordered !
            i1 = min(i1,self%n_data-1)
            i2 = i1 + 1
            frac = (point - self%data_point(i1))/(self%data_point(i2)-self%data_point(i1))
            del  = self%data_value(i2) - self%data_value(i1)
            res  = self%data_value(i1) + frac*del
            if (self%interp_kind=="logarithmic") res = exp(res)
         else
            res = ZERO 
         end if
      end if
     STOP_TIMER("INTERPOLATOR:value_for")
      CHECK
   end function

   function values_for(self,points) result(values)
    INTERPOLATOR :: self
   ! Return the interpolated "values" for a series of "points".
      IN :: self
      REALVEC(:), IN :: points
      REALVEC(size(points)) :: values
      REAL :: point
      REAL :: res
      REAL :: del,frac
      INT :: i1,i2
      INT :: i
      STACK("INTERPOLATOR:values_for")
      START_TIMER("INTERPOLATOR:values_for")
      ENSURE(self%finalised,"INTERPOLATOR:values_for ... not finalised")
      if (is_even_spaced_(self)) then ! This is more efficient than below
        do i = 1,size(points)
          point = points(i)

          frac = (point - self%data_point(1))/self%spacing
          i1 = floor(frac)
          if (frac>i1) then
            i2 = i1+1
            if (-1<i1 AND i2<self%n_data) then
              del  = self%data_value(i1+2) - self%data_value(i2)
              res  = self%data_value(i2) + (frac-i1)*del
              if (self%interp_kind=="logarithmic") res = exp(res)
              values(i) = res
            else
              values(i) = ZERO
            end if
          else
            if (-1<i1 AND i1<self%n_data) then
              res  = self%data_value(i1+1)
              if (self%interp_kind=="logarithmic") res = exp(res)
              values(i) = res
            else
              values(i) = ZERO
            end if
          end if
        end do
      else
        do i = 1,size(points)
          point = points(i)
          if (is_in_range_(point,range_(self%data_point))) then
            i1 = count(self%data_point<=point) ! assuming .data is ordered !
            i1 = min(i1,self%n_data-1)
            i2 = i1 + 1
            frac = (point - self%data_point(i1))/(self%data_point(i2)-self%data_point(i1))
            del  = self%data_value(i2) - self%data_value(i1)
            res  = self%data_value(i1) + frac*del
            if (self%interp_kind=="logarithmic") res = exp(res)
            values(i) = res
          else
            values(i) = ZERO
          end if
        end do
      end if
     STOP_TIMER("INTERPOLATOR:values_for")
      CHECK
   end function

   subroutine set_data_values_1(self,func)
    INTERPOLATOR :: self
   ! Set the ".data_values" from the function "func", which returns "values"
   ! from a set of given "points".
      interface
         subroutine func(points,values)
            REALVEC(:) :: points
            REALVEC(:) :: values
         end subroutine
      end interface
   STACK("INTERPOLATOR:set_data_values_1")
   START_TIMER("INTERPOLATOR:set_data_values_1")
   ENSURE(self%finalised,"INTERPOLATOR:set_data_values_1 ... not finalised")
   ENSURE(associated(self%data_point),"INTERPOLATOR:set_data_values_1 ... no data_points")
   ENSURE(size(self%data_point)==self%n_data,"INTERPOLATOR:set_data_values_1 ... wrong number of data_points")
   WARN_IF(associated(self%data_value),"INTERPOLATOR:set_data_values_1 ... data_values will be lost")
      call destroy_(self%data_value)
      call create_(self%data_value,self%n_data)
      call func(self%data_point,self%data_value)
     STOP_TIMER("INTERPOLATOR:set_data_values_1")
      UNSTACK
   end subroutine

   subroutine set_data_values_2(self,func_at,pos)
    INTERPOLATOR :: self
   ! Set the ".data_values" from the function "func_at", which returns "values"
   ! from a set of given "points", and an additional single "pos" as parameter.
      interface
         subroutine func_at(points,pos,values)
            REALVEC(:) :: points,values
            REALVEC(3) :: pos
         end subroutine
      end interface
      REALVEC(3) :: pos
   STACK("INTERPOLATOR:set_data_values_2")
   START_TIMER("INTERPOLATOR:set_data_values_2")
   ENSURE(self%finalised,"INTERPOLATOR:set_data_values_2 ... not finalised")
   ENSURE(associated(self%data_point),"INTERPOLATOR:set_data_values_2 ... no data_points")
   ENSURE(size(self%data_point)==self%n_data,"INTERPOLATOR:set_data_values_2 ... wrong number of data_points")
   WARN_IF(associated(self%data_value),"INTERPOLATOR:set_data_values_2 ... data_values will be lost")
      call destroy_(self%data_value)
      call create_(self%data_value,self%n_data)
      call func_at(self%data_point,pos,self%data_value)
     STOP_TIMER("INTERPOLATOR:set_data_values_2")
      UNSTACK
   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    INTERPOLATOR :: self
   ! Put the list of interpolating colours.
      INT :: i
   STACK("INTERPOLATOR:put")
   START_TIMER("INTERPOLATOR:put")
   ENSURE(self%finalised,"INTERPOLATOR:put ... not finalised")
      call flush_(stdout)
      call text_(stdout,"INTERPOLATOR info")
      call flush_(stdout)
      call show_(stdout,"No. of interpolating data points =",self%n_data)
      call show_(stdout,"Using even spaced data points?   =",is_even_spaced_(self))
      call flush_(stdout)
      if (is_even_spaced_(self)) then
         call show_(stdout,"Initial data value               =",self%data_point(1))
         call show_(stdout,"Last data value                  =",self%data_point(self%n_data))
         call show_(stdout,"Data point spacing               =",self%spacing)
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=1)
         call put_(stdout,"#",int_width=TRUE)
         call put_(stdout,"Value")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=1)
         do i = 1,self%n_data
            call put_(stdout,i)
            call put_(stdout,self%data_value(i))
            call flush_(stdout)
         end do
         call dash_(stdout,int_fields=1,real_fields=1)
      else
         call dash_(stdout,int_fields=1,real_fields=1)
         call put_(stdout,"#",int_width=TRUE)
         call put_(stdout,"Point")
         call put_(stdout,"Value")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=2)
         do i = 1,self%n_data
            call put_(stdout,i)
            call put_(stdout,self%data_point(i))
            call put_(stdout,self%data_value(i))
            call flush_(stdout)
         end do
         call dash_(stdout,int_fields=1,real_fields=2)
      end if
     STOP_TIMER("INTERPOLATOR:put")
      CHECK
   end subroutine

end
