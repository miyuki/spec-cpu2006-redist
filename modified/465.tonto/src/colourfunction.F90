!------------------------------------------------------------------------------
!
! COLOURFUNCTION:
!
! For generating an RGB colour triple from a given data value.
!
! The module takes as input a series of data values with their assigned RGB
! colours. Any data value is then assigned an RGB colour by interpolation from
! the colour table.
!
! The default data range is [0...1]. The colours assigned are:
! Blue-Green for the range [0...0.5
! Green-Red  for the range [0.5...1].
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
! $Id: colourfunction.foo,v 1.2.2.4 2003/09/18 05:28:23 reaper Exp $
!
!------------------------------------------------------------------------------

module COLOURFUNCTION_MODULE

#  include "colourfunction.use"

   implicit none

#  include "macros"
#  include "colourfunction.int"


contains

   subroutine create(self,range)
    COLOURFUNCTION :: self
   ! Create the object
     PTR :: self
     REALVEC(2), optional :: range
     STACK("COLOURFUNCTION:create")
     START_TIMER("COLOURFUNCTION:create")
     nullify(self)
     allocate(self)
     ADD_MEMORY(COLOURFUNCTION_SIZE)
     call nullify_ptr_part_(self)
     call set_defaults_(self,range)
     STOP_TIMER("COLOURFUNCTION:create")
      UNSTACK
   end subroutine

   subroutine create_copy(self,object)
    COLOURFUNCTION :: self
   ! Create a copy of object
     PTR :: self
     COLOURFUNCTION, IN :: object
     STACK("COLOURFUNCTION:create_copy")
     START_TIMER("COLOURFUNCTION:create_copy")
     call create_(self)
     call copy_(self,object)
     STOP_TIMER("COLOURFUNCTION:create_copy")
      UNSTACK
   end subroutine


   subroutine copy(self,c)
    COLOURFUNCTION :: self
   ! Copy the contents of "c" to self.
     COLOURFUNCTION, IN :: c
     STACK("COLOURFUNCTION:copy")
     START_TIMER("COLOURFUNCTION:copy")
     self%n_data = c%n_data
     call create_copy_(self%data,c%data)
     call create_copy_(self%RGB,c%RGB)
     STOP_TIMER("COLOURFUNCTION:copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    COLOURFUNCTION :: self
   ! Destroy an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("COLOURFUNCTION:destroy")
      START_TIMER("COLOURFUNCTION:destroy")
      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)
        DELETE_MEMORY(COLOURFUNCTION_SIZE)
      end if
     STOP_TIMER("COLOURFUNCTION:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    COLOURFUNCTION :: self
   ! Nullify the pointer parts
      STACK("COLOURFUNCTION:nullify_ptr_part")
      START_TIMER("COLOURFUNCTION:nullify_ptr_part")
      nullify(self%data)
      nullify(self%RGB)
     STOP_TIMER("COLOURFUNCTION:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    COLOURFUNCTION :: self
   ! Destroy the pointer parts
      STACK("COLOURFUNCTION:destroy_ptr_part")
      START_TIMER("COLOURFUNCTION:destroy_ptr_part")
      call destroy_(self%data)
      call destroy_(self%RGB)
     STOP_TIMER("COLOURFUNCTION:destroy_ptr_part")
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

   subroutine set_defaults(self,range)
    COLOURFUNCTION :: self
   ! Set up a defaults
      REALVEC(2), optional :: range
      STACK("COLOURFUNCTION:set_defaults")
      START_TIMER("COLOURFUNCTION:set_defaults")
      self%n_data = 3
      call destroy_(self%data)
      call create_(self%data,3)
      self%data     = (/ZERO,HALF, ONE/)
      call destroy_(self%RGB)
      call create_(self%RGB,3,3)
      self%RGB(:,1) = (/ZERO,ZERO, ONE/)
      self%RGB(:,2) = (/ZERO, ONE,ZERO/)
      self%RGB(:,3) = (/ ONE,ZERO,ZERO/)
      if (present(range)) call rescale_data_(self,range)
      self%finalised = TRUE
     STOP_TIMER("COLOURFUNCTION:set_defaults")
      UNSTACK
   end subroutine

   subroutine set_reverse_defaults(self,range)
    COLOURFUNCTION :: self
   ! Set up reversed defaults
      REALVEC(2), optional :: range
      STACK("COLOURFUNCTION:set_reverse_defaults")
      START_TIMER("COLOURFUNCTION:set_reverse_defaults")
      self%n_data = 3
      call destroy_(self%data)
      call create_(self%data,3)
      self%data     = (/ZERO,HALF, ONE/)
      call destroy_(self%RGB)
      call create_(self%RGB,3,3)
      self%RGB(:,1) = (/ ONE,ZERO,ZERO/)
      self%RGB(:,2) = (/ZERO, ONE,ZERO/)
      self%RGB(:,3) = (/ZERO,ZERO, ONE/)
      if (present(range)) call rescale_data_(self,range)
      self%finalised = TRUE
     STOP_TIMER("COLOURFUNCTION:set_reverse_defaults")
      CHECK
   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    COLOURFUNCTION :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("COLOURFUNCTION:read_keywords")
     START_TIMER("COLOURFUNCTION:read_keywords")
     ENSURE(next_item_(stdin)=="{","COLOURFUNCTION:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("COLOURFUNCTION:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    COLOURFUNCTION :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("COLOURFUNCTION:process_keyword")
      START_TIMER("COLOURFUNCTION:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}               ")  ! exit case
         case ("data_and_colour="); call read_data_and_colour_(self)
         case ("put             "); call put_(self)
         case ("units=          "); call read_units_(self)
         case default;         allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "}               "
         tonto%known_keywords(2) = "data_and_colour="
         tonto%known_keywords(3) = "put             "
         tonto%known_keywords(4) = "units=          "
         call unknown_(tonto,word,"COLOURFUNCTION:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("COLOURFUNCTION:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    COLOURFUNCTION :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("COLOURFUNCTION:read_units")
      START_TIMER("COLOURFUNCTION:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("COLOURFUNCTION:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    COLOURFUNCTION :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("COLOURFUNCTION:read_junk")
      START_TIMER("COLOURFUNCTION:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("COLOURFUNCTION:read_junk")
      CHECK
   end subroutine

   subroutine read_data_and_colour(self)
    COLOURFUNCTION :: self
   ! Read in the data-colour pair. The colour can be a text string of a known
   ! colour, an RGB 255 integer triple, or an RGB real triple. Only the latter
   ! is used and stored.
      STR(STR_SIZE) :: word
      COLOUR :: colour
      INT :: length,line,item,i
      REAL :: data
      STACK("COLOURFUNCTION:read_data_and_colour")
      START_TIMER("COLOURFUNCTION:read_data_and_colour")
      ENSURE(next_item_(stdin)=="{","COLOURFUNCTION:read_data_and_colour ... open bracket '{' expected")
      call read_(stdin,word)
      call read_(stdin,word)
      length = 0
      line = line_number_(stdin)
      item = previous_line_item_(stdin)
      do
         call move_to_previous_item_(stdin)
         call read_(stdin,data)
         call read_(colour)
         length = length + 1
         call read_(stdin,word)
         call to_lower_case_(word)
         if (word=="}") exit
      end do
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
      call destroy_(self%data)
      call create_(self%data,length)
      call destroy_(self%RGB)
      call create_(self%RGB,3,length)
      do i = 1,length
         call read_(stdin,data)
         call read_(colour)
         self%data(i)  = data
         self%RGB(:,i) = RGB_(colour)
      end do
      call read_(stdin,word)
      call to_lower_case_(word)
      ENSURE(word=="}","COLOURFUNCTION:read_data_and_colour ... expecting a }")
     STOP_TIMER("COLOURFUNCTION:read_data_and_colour")
      UNSTACK
   end subroutine

   subroutine finalise(self)
    COLOURFUNCTION :: self
   ! Check everything is OK after reading data. Saturation must exist but it is
   ! applied into the RGB triples and then destroyed.
      INTVEC(:), PTR :: order
   STACK("COLOURFUNCTION:finalise")
   START_TIMER("COLOURFUNCTION:finalise")
   ENSURE(associated(self%data),"COLOURFUNCTION:finalise ... no data values")
   ENSURE(associated(self%RGB),"COLOURFUNCTION:finalise ... no RGB values")
   ENSURE(size(self%data)==size(self%RGB,2),"COLOURFUNCTION:finalise ... incompatible #: data and RGB")
   ENSURE(all(self%RGB>=ZERO),"COLOURFUNCTION:finalise ... negative RGB values exist")
   ENSURE(all(self%RGB<=ONE),"COLOURFUNCTION:finalise ... some RGB values greater than 1")
   ENSURE(all(column_norms_(self%RGB)<=THREE),"COLOURFUNCTION:finalise ... some RGB norms larger than 3")
      self%n_data = size(self%data)
      call create_(order,self%n_data)
      ! Re-order from smallest to largest
      call quick_sort_(self%data,order)
      self%data       = self%data(order)
      self%RGB        = self%RGB(:,order)
      call destroy_(order)
      self%finalised = TRUE
     STOP_TIMER("COLOURFUNCTION:finalise")
      UNSTACK
   end subroutine

!  ************
!  Set routines
!  ************

   subroutine set_data(self,data)
    COLOURFUNCTION :: self
   ! Set the data. Make sure that .finalise is called after all set routines.
      REALVEC(:) :: data
      STACK("COLOURFUNCTION:set_data")
      START_TIMER("COLOURFUNCTION:set_data")
      call destroy_(self%data)
      call create_copy_(self%data,data)
     STOP_TIMER("COLOURFUNCTION:set_data")
      UNSTACK
   end subroutine

   subroutine set_RGB(self,RGB)
    COLOURFUNCTION :: self
   ! Set the RGB's. Make sure that .finalise is called after all set routines.
      REALMAT(:,:) :: RGB
      STACK("COLOURFUNCTION:set_RGB")
      START_TIMER("COLOURFUNCTION:set_RGB")
      call destroy_(self%RGB)
      call create_copy_(self%RGB,RGB)
     STOP_TIMER("COLOURFUNCTION:set_RGB")
      UNSTACK
   end subroutine

!  ****************
!  Colours for data
!  ****************

   function RGB_for(self,value,truncate_to_range) result(res)
    COLOURFUNCTION :: self
   ! Return the RGB triple corresponding to a certain "value".
      IN :: self
      REAL, IN :: value
      BIN, optional, IN :: truncate_to_range
      REALVEC(3) :: res
      REALVEC(3) :: colour
      REAL :: frac,thevalue
      INT :: i1,i2
      BIN :: truncate
      STACK("COLOURFUNCTION:RGB_for")
      START_TIMER("COLOURFUNCTION:RGB_for")
      ENSURE(self%finalised,"COLOURFUNCTION:RGB_for ... not finalised")
      thevalue = value
      truncate = TRUE
      if (present(truncate_to_range)) truncate = truncate_to_range
      if (NOT truncate) then
        ENSURE(is_in_range_(thevalue,range_(self%data)),"COLOURFUNCTION:RGB_for ... value out of range")
      else
        thevalue = min(thevalue,maxval(self%data))
        thevalue = max(thevalue,minval(self%data))
      end if
      i1 = count(self%data<=thevalue) ! assuming .data is ordered !
      i1 = min(i1,self%n_data-1)
      i2 = i1 + 1
      frac   = (thevalue - self%data(i1))/(self%data(i2)-self%data(i1))
      colour = self%RGB(:,i2) - self%RGB(:,i1)
      res    = self%RGB(:,i1) + frac*colour
     STOP_TIMER("COLOURFUNCTION:RGB_for")
      CHECK
   end function

   function RGB255_for(self,value) result(res)
    COLOURFUNCTION :: self
   ! Return the RGB255 triple corresponding to a certain "value".
      REAL :: value
      INTVEC(3) :: res
      REALVEC(3) :: colour
      REAL :: frac
      INT :: i1,i2
   STACK("COLOURFUNCTION:RGB255_for")
   START_TIMER("COLOURFUNCTION:RGB255_for")
   ENSURE(self%finalised,"COLOURFUNCTION:RGB255_for ... not finalised")
   ENSURE(is_in_range_(value,range_(self%data)),"COLOURFUNCTION:RGB255_for ... value out of range")
      i1 = count(self%data<=value) ! assuming .data is ordered !
      i1 = min(i1,self%n_data-1)
      i2 = i1 + 1
      frac   = (value - self%data(i1))/(self%data(i2)-self%data(i1))
      colour = self%RGB(:,i2) - self%RGB(:,i1)
      res    = nint(255*(self%RGB(:,i1) + frac*colour))
     STOP_TIMER("COLOURFUNCTION:RGB255_for")
      CHECK
   end function

   subroutine get_RGB_for(self,values,RGB)
    COLOURFUNCTION :: self
   ! Return the "RGB" triples corresponding to a set of "values".
      REALVEC(:) :: values
      REALMAT(:,:) :: RGB
      INT :: i,n
   STACK("COLOURFUNCTION:get_RGB_for")
   START_TIMER("COLOURFUNCTION:get_RGB_for")
   ENSURE(self%finalised,"COLOURFUNCTION:get_RGB_for ... not finalised")
   ENSURE(size(RGB,2)==size(values),"COLOURFUNCTION:get_RGB_for ... values and RGB are incompatible")
      n = size(values)
      do i = 1,n
         RGB(:,i) = RGB_for_(self,values(i))
      end do
     STOP_TIMER("COLOURFUNCTION:get_RGB_for")
      CHECK
   end subroutine

   subroutine get_RGB255_for(self,values,RGB255)
    COLOURFUNCTION :: self
   ! Return the "RGB255" triples corresponding to a set of "values".
      REALVEC(:) :: values
      INTMAT(:,:) :: RGB255
      INT :: i,n
   STACK("COLOURFUNCTION:get_RGB255_for")
   START_TIMER("COLOURFUNCTION:get_RGB255_for")
   ENSURE(self%finalised,"COLOURFUNCTION:get_RGB255_for ... not finalised")
   ENSURE(size(RGB255,2)==size(values),"COLOURFUNCTION:get_RGB255_for ... values and RGB255 are incompatible")
      n = size(values)
      do i = 1,n
         RGB255(:,i) = RGB255_for_(self,values(i))
      end do
     STOP_TIMER("COLOURFUNCTION:get_RGB255_for")
      CHECK
   end subroutine

   subroutine rescale_data(self,range)
    COLOURFUNCTION :: self
   ! Rescale the .data so that the lowest value corresponds to
   ! range(1), and the largest value corresponds to range(2).
      REALVEC(2) :: range
      REAL :: data1,del,frac
      INT :: i
   STACK("COLOURFUNCTION:rescale_data")
   START_TIMER("COLOURFUNCTION:rescale_data")
   ENSURE(associated(self%data),"COLOURFUNCTION:rescale_data ... no data")
   ENSURE(self%n_data>1,"COLOURFUNCTION:rescale_data ... not enough data")
      data1 = self%data(1)
      del   = self%data(self%n_data) - data1
      do i = 1,self%n_data
         frac = (self%data(i) - data1)/del
         self%data(i) = range(1) + frac*(range(2)-range(1))
      end do
     STOP_TIMER("COLOURFUNCTION:rescale_data")
      CHECK
   end subroutine

   subroutine set_default_colours(self,values)
    COLOURFUNCTION :: self
   ! Set up default colours for a set of "values".
      REALVEC(:) :: values
      STACK("COLOURFUNCTION:set_default_colours")
      START_TIMER("COLOURFUNCTION:set_default_colours")
      call rescale_data_(self,range_(values))
     STOP_TIMER("COLOURFUNCTION:set_default_colours")
      UNSTACK
   end subroutine

!  **************
!  Output methods
!  **************

   subroutine put(self)
    COLOURFUNCTION :: self
   ! Put the list of interpolating colours.
      INT :: i
      STACK("COLOURFUNCTION:put")
      START_TIMER("COLOURFUNCTION:put")
      call flush_(stdout)
      call text_(stdout,"Colourfunction data")
      call flush_(stdout)
      call show_(stdout,"No. of interpolating data points =",self%n_data)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"Data")
      call put_(stdout,"Red")
      call put_(stdout,"Green")
      call put_(stdout,"Blue")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do i = 1,self%n_data
         call put_(stdout,i)
         call put_(stdout,self%data(i))
         call put_(stdout,self%RGB(1,i))
         call put_(stdout,self%RGB(2,i))
         call put_(stdout,self%RGB(3,i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
     STOP_TIMER("COLOURFUNCTION:put")
      CHECK
   end subroutine

end
