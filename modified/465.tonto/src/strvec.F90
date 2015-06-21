!---------------------------------------------------------------------------
!
!  STRVEC: String vectors
!
!  Notes
!
!  Normally, a STR variable means a character string of length STR_SIZE.
!  However, in this module we use arbitrary length character strings.
!  (See also the STR module where the same tyhing is done).
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
! $Id: strvec.foo,v 1.11.2.3 2003/11/13 05:33:55 reaper Exp $
!---------------------------------------------------------------------------

module STRVEC_MODULE

#  include "strvec.use"

   implicit none

#  include "macros"
#  include "strvec.int"


!  Make strings arguments arbitrary length by default

#  undef  STR_SIZE
#  define STR_SIZE  *

contains

   subroutine create(self,dim)
    STRVEC(STR_SIZE,:) :: self
   ! Create space for a string vector
      PTR :: self
      INT :: dim
      STACK("STRVEC:create")
      START_TIMER("STRVEC:create")
      nullify(self)
      allocate(self(dim))
      ADD_MEMORY(dim*len(self(1))*CHR_SIZE)
     STOP_TIMER("STRVEC:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,v)
    STRVEC(STR_SIZE,:) :: self
   ! Create space for a string vector as assign "v"
      PTR :: self
      STRVEC(STR_SIZE,:) :: v
      INT :: dim
      STACK("STRVEC:create_1")
      START_TIMER("STRVEC:create_1")
      nullify(self)
      dim = size(v)
      allocate(self(dim))
      ADD_MEMORY(dim*len(self(1))*CHR_SIZE)
      self = v
!     do i = 1,dim            ! self = v is buggy on DEC
!        s = scan(v(i)," ")   ! when v is a constructor
!        self(i) = v(i)(1:s)
!     end
     STOP_TIMER("STRVEC:create_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    STRVEC(STR_SIZE,:) :: self
   ! Destroy space for a string vector
      PTR :: self
      STACK("STRVEC:destroy")
      START_TIMER("STRVEC:destroy")
      if (NOT associated(self)) then; STOP_TIMER("STRVEC:destroy") UNSTACK return; end if
      DELETE_MEMORY(size(self)*len(self(1))*CHR_SIZE)
      deallocate(self)
     STOP_TIMER("STRVEC:destroy")
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
    STRVEC(STR_SIZE,:) :: self
   ! Make a copy of "vec"
      PTR :: self
      STRVEC(STR_SIZE,:) :: vec
      STACK("STRVEC:create_copy")
      START_TIMER("STRVEC:create_copy")
      call create_(self,size(vec))
      call copy_(self,vec)
     STOP_TIMER("STRVEC:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,vec)
    STRVEC(STR_SIZE,:) :: self
   ! Make a copy of "vec"
      STRVEC(STR_SIZE,:) :: vec
      INT :: i
      STACK("STRVEC:copy")
      START_TIMER("STRVEC:copy")
      do i = 1,size(vec)
         call copy_(self(i),vec(i))
      end do
     STOP_TIMER("STRVEC:copy")
      CHECK
   end subroutine

   subroutine to_lower_case(self)
    STRVEC(STR_SIZE,:) :: self
   ! Change upper case charaters to lower case in all elements
       INT :: i
      STACK("STRVEC:to_lower_case")
      START_TIMER("STRVEC:to_lower_case")
      do i = 1,size(self)
         call to_lower_case_(self(i))
      end do
     STOP_TIMER("STRVEC:to_lower_case")
      CHECK
   end subroutine

   subroutine to_upper_case(self)
    STRVEC(STR_SIZE,:) :: self
   ! Change lower case charaters to upper case in all elements
       INT :: i
      STACK("STRVEC:to_upper_case")
      START_TIMER("STRVEC:to_upper_case")
      do i = 1,size(self)
         call to_upper_case_(self(i))
      end do
     STOP_TIMER("STRVEC:to_upper_case")
      CHECK
   end subroutine

   recursive subroutine quick_sort(self)
    STRVEC(STR_SIZE,:) :: self
   ! Sort the vector into fortran dictionary order
      STRVEC(len=len(self(1)),:), PTR :: smaller,larger
      INT :: n, ns, ne, nl
      STR(len(self(1))) :: chosen
      STACK("STRVEC:quick_sort")
      START_TIMER("STRVEC:quick_sort")
      n = size(self)
      if (n>=2) then
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
      end if
     STOP_TIMER("STRVEC:quick_sort")
      CHECK
   end subroutine

   subroutine shrink(self,dim)
    STRVEC(STR_SIZE,:) :: self
   ! Shrinks self to dimension dim.  Contents are retained.
     PTR :: self
     INT, IN :: dim
     STRVEC(len=len(self(1)),:), PTR :: old
   STACK("STRVEC:shrink")
   START_TIMER("STRVEC:shrink")
   ENSURE(associated(self),"STRVEC:shrink ... no self array")
   ENSURE(dim<=size(self),"STRVEC:shrink ... dim too large")
     if (dim==size(self)) then; STOP_TIMER("STRVEC:shrink") UNSTACK return; end if
     old => self
     nullify(self)
     call create_(self,dim)
     self=old(1:dim)
     call destroy_(old)
     STOP_TIMER("STRVEC:shrink")
      UNSTACK
   end subroutine

   subroutine expand(self,dim)
    STRVEC(STR_SIZE,:) :: self
   ! Expands self to dimension dim.  Contents are retained.
   ! Elements added are set to zero.
     PTR :: self
     INT, IN :: dim
     STRVEC(len=len(self(1)),:), PTR :: old
     INT :: old_dim
     STACK("STRVEC:expand")
     START_TIMER("STRVEC:expand")
     if (NOT associated(self)) then
        call create_(self,dim)
        self = " "
     else
        ENSURE(dim>=size(self),"STRVEC:expand ... dim not large enough")
        old => self
        old_dim = size(old)
        nullify(self)
        call create_(self,dim)
        self(        1:old_dim) = old
        self(old_dim+1:dim    ) = " "
        call destroy_(old)
     end if
     STOP_TIMER("STRVEC:expand")
      UNSTACK
   end subroutine

   subroutine append(self,v)
    STRVEC(STR_SIZE,:) :: self
   ! Expands self and appends the contents of vector "v".
     PTR :: self
     STRVEC(STR_SIZE,:), IN :: v
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("STRVEC:append")
     START_TIMER("STRVEC:append")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+size(v))
     self(dim+1:) = v
     STOP_TIMER("STRVEC:append")
      UNSTACK
   end subroutine

   subroutine append_1(self,value)
    STRVEC(STR_SIZE,:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end.
     PTR :: self
     STR(STR_SIZE), IN :: value
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("STRVEC:append_1")
     START_TIMER("STRVEC:append_1")
     if (NOT associated(self)) then; dim = 0
     else;                 dim = size(self)
     end if
     call expand_(self,dim+1)
     self(dim+1) = value
     STOP_TIMER("STRVEC:append_1")
      UNSTACK
   end subroutine

   subroutine append_only_if_unique(self,value)
    STRVEC(STR_SIZE,:) :: self
   ! Expands self by 1, and appends the single scalar "value" onto the end, but
   ! only if the "value" is unique
     PTR :: self
     STR(STR_SIZE), IN :: value
   ! The following code is inherited from INTRINSICVEC
     STACK("STRVEC:append_only_if_unique")
     START_TIMER("STRVEC:append_only_if_unique")
     if (any(self==value)) then; STOP_TIMER("STRVEC:append_only_if_unique") UNSTACK return; end if
     call append_(self,value)
     STOP_TIMER("STRVEC:append_only_if_unique")
      UNSTACK
   end subroutine

   subroutine remove_repetitions(self)
    STRVEC(STR_SIZE,:) :: self
   ! Sort through the vector and remove repeated elements which come later in
   ! the list.  NOTE: the vector may shrink
      PTR :: self
      STRVEC(len=len(self(1)),:), PTR :: unique
      INT :: i,n
   STACK("STRVEC:remove_repetitions")
   START_TIMER("STRVEC:remove_repetitions")
   ENSURE(associated(self),"STRVEC:remove_repetitions ... no vector")
      if (size(self)==1) then; STOP_TIMER("STRVEC:remove_repetitions") UNSTACK return; end if
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
     STOP_TIMER("STRVEC:remove_repetitions")
      UNSTACK
   end subroutine

   function has_repetitions(self) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return TRUE if self has at least one repeated element.
      BIN :: res
      STRVEC(len=len(self(1)),:), PTR :: unique
      INT :: i,n
      STACK("STRVEC:has_repetitions")
      START_TIMER("STRVEC:has_repetitions")
      res = FALSE
      if (size(self)==1) then; STOP_TIMER("STRVEC:has_repetitions") CHECK return; end if
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
     STOP_TIMER("STRVEC:has_repetitions")
      CHECK
   end function

   subroutine prepend(self,v)
    STRVEC(STR_SIZE,:) :: self
   ! Prepend the vector "v" to "self". "self" is expanded.
     PTR :: self
     STRVEC(STR_SIZE,:), IN :: v
   ! The following code is inherited from INTRINSICVEC
     INT :: dim,dimv
     STACK("STRVEC:prepend")
     START_TIMER("STRVEC:prepend")
     dim  = size(self)
     dimv = size(v)
     call expand_(self,dim+dimv)
     self(dimv+1:    ) = self(1:dim)
     self(     1:dimv) = v
     STOP_TIMER("STRVEC:prepend")
      UNSTACK
   end subroutine

   subroutine prepend_1(self,value)
    STRVEC(STR_SIZE,:) :: self
   ! Prepend an single "value" to "self". "self" is expanded.
     PTR :: self
     STR(STR_SIZE), IN :: value
   ! The following code is inherited from INTRINSICVEC
     INT :: dim
     STACK("STRVEC:prepend_1")
     START_TIMER("STRVEC:prepend_1")
     dim = size(self)
     call expand_(self,dim+1)
     self(2:) = self(1:dim)
     self(1 ) = value
     STOP_TIMER("STRVEC:prepend_1")
      UNSTACK
   end subroutine

   function has_any_included_in(self,string,at_start) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return TRUE if self has any element included in "string" which starts at
   ! the start of the "string", provided "at_start" is TRUE; otherwise
   ! returns TRUE even if the match was not at the start.
      STR(STR_SIZE), IN :: string
      BIN, optional :: at_start
      BIN :: res
      BIN :: first
      STACK("STRVEC:has_any_included_in")
      START_TIMER("STRVEC:has_any_included_in")
      first = FALSE
      if (present(at_start)) first = at_start
      if (first) then
         res = any(index(spread(string,1,size(self)),self) == 1)
      else
         res = any(index(spread(string,1,size(self)),self) /= 0)
      end if
     STOP_TIMER("STRVEC:has_any_included_in")
      CHECK
   end function

   function index_of_first_included_in(self,string) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return the index of the first element in self which is included in
   ! "string", or zero otherwise.
      STR(STR_SIZE), IN :: string
      INT :: res
      INT :: i
      STACK("STRVEC:index_of_first_included_in")
      START_TIMER("STRVEC:index_of_first_included_in")
      res = 0
      do i = 1,size(self)
         res = index(string,self(i))
         if (res>0) exit
      end do
     STOP_TIMER("STRVEC:index_of_first_included_in")
      CHECK
   end function

   function has_any_including(self,string) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return TRUE if self has any element which includes "string".
      STR(STR_SIZE), IN :: string
      BIN :: res
      STACK("STRVEC:has_any_including")
      START_TIMER("STRVEC:has_any_including")
      res = any(index(self,spread(string,1,size(self))) /= 0)
     STOP_TIMER("STRVEC:has_any_including")
      CHECK
   end function

   function includes(self,string,at_start) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return TRUE for a particular element, if that element of self includes "string".
   ! Returns FALSE if no element matches. If "at_start" is present and TRUE, then the
   ! result is TRUE only if the item matches at the start of the string.
      STR(STR_SIZE), IN :: string
      BIN, optional :: at_start
      BINVEC(size(self)) :: res
      BIN :: first
      STACK("STRVEC:includes")
      START_TIMER("STRVEC:includes")
      first = FALSE
      if (present(at_start)) first = at_start
      if (first) then
         res = index(self,spread(string,1,size(self))) == 1
      else
         res = index(self,spread(string,1,size(self))) /= 0
      end if
     STOP_TIMER("STRVEC:includes")
      CHECK
   end function

   function index_of_first_that_includes(self,string) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return the index of the first element of self that includes "string".
   ! Returns 0 if no element matches.
      STR(STR_SIZE), IN :: string
      INT :: res
      INT :: i
      STACK("STRVEC:index_of_first_that_includes")
      START_TIMER("STRVEC:index_of_first_that_includes")
      do i = 1,size(self)
         res = index(self(i),string)
         if (res==0) cycle
         res = i
         exit
      end do
     STOP_TIMER("STRVEC:index_of_first_that_includes")
      CHECK
   end function

   function index_of(self,string) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return the first index of the "string" in self.
   ! Returns 0 if no element matches.
      STR(STR_SIZE), IN :: string
      INT :: res
      INT :: i
      STACK("STRVEC:index_of")
      START_TIMER("STRVEC:index_of")
      res = 0
      do i = 1,size(self)
         if (self(i)/=string) cycle
         res = i
         exit
      end do
     STOP_TIMER("STRVEC:index_of")
      CHECK
   end function

   function index_of_matching_bracket(self,symbol) result(res)
    STRVEC(STR_SIZE,:) :: self
   ! Return the first index of the matching bracket "symbol" in self.
   ! The first element of self need not be an opening bracket symbol.
      STR(STR_SIZE), IN :: symbol
      INT :: res
      STRVEC(len=1,4) :: opening = (/"{","(","[","<"/)
      STRVEC(len=1,4) :: closing = (/"}",")","]",">"/)
      STR(1) :: op,cl
      INT :: i,s,n
      STACK("STRVEC:index_of_matching_bracket")
      START_TIMER("STRVEC:index_of_matching_bracket")
      ENSURE(any(symbol==opening),"STRVEC:index_of_matching_bracket ... unrecognised open bracket symbol")
      ENSURE(index_of_(self,symbol)>0,"STRVEC:index_of_matching_bracket ... no open bracket symbol in self")
      ENSURE(index_of_(self,symbol)<size(self),"STRVEC:index_of_matching_bracket ... open bracket is at end of self")
      op = symbol
      cl = closing(index_of_(opening,symbol))
      s = index_of_(self,op)
      n = 0
      do i = s+1,size(self)
         if (self(i)==op) n = n + 1
         if (self(i)==cl AND n==0) exit
         if (self(i)==cl AND n>0) n = n - 1
      end do
      ENSURE(n==0,"STRVEC:index_of_matching_bracket ... unmatching number of closing bracket symbols")
      res = i
     STOP_TIMER("STRVEC:index_of_matching_bracket")
      CHECK
   end function

end
