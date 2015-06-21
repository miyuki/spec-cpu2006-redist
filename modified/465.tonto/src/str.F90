!---------------------------------------------------------------------------
!
!  STR: Methods of dealing with arbitrary length character strings
!
!  Notes
!
!  Normally, a STR variable means a character string of length STR_SIZE.
!  However, in this module we use arbitrary length character strings.
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
! $Id: str.foo,v 1.16.2.11 2003/11/13 05:33:02 reaper Exp $
!
!---------------------------------------------------------------------------

module STR_MODULE

#  include "str.use"

   implicit none

#  include "macros"
#  include "str.int"


!  Make strings arguments arbitrary length by default

#  undef  STR_SIZE
#  define STR_SIZE  *

   public trim_; interface trim_
      module procedure trim_blanks_from_end
   end interface

   public scan_; interface scan_
      module procedure index_of_character_in
   end interface

   public verify_; interface verify_
      module procedure index_of_character_not_in
   end interface

   public adjustl_; interface adjustl_
      module procedure align_left
   end interface

   public adjustr_; interface adjustr_
      module procedure align_right
   end interface

contains

   subroutine create(self)
    STR(STR_SIZE) :: self
   ! Create space for a string variable
      PTR :: self
      STACK("STR:create")
      START_TIMER("STR:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(len(self)*CHR_SIZE)
      self = " "
     STOP_TIMER("STR:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    STR(STR_SIZE) :: self
   ! Destroy space for a string variable
      PTR :: self
      STACK("STR:destroy")
      START_TIMER("STR:destroy")
      if (NOT associated(self)) then; STOP_TIMER("STR:destroy") UNSTACK return; end if
      DELETE_MEMORY(len(self)*CHR_SIZE)
      deallocate(self)
     STOP_TIMER("STR:destroy")
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

   subroutine create_copy(self,s)
    STR(STR_SIZE) :: self
   ! Create a copy of "s"
      PTR :: self
      STR(STR_SIZE) :: s
      STACK("STR:create_copy")
      START_TIMER("STR:create_copy")
      call create_(self)
      call copy_(self,s)
     STOP_TIMER("STR:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,s)
    STR(STR_SIZE) :: self
   ! Make a copy of the string "s"
      STR(STR_SIZE) :: s
      STACK("STR:copy")
      START_TIMER("STR:copy")
      self = s
     STOP_TIMER("STR:copy")
      CHECK
   end subroutine

   function trim_blanks_from_end(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the trimmed version of "self"
      STR(len_trim(self)) :: res
      STACK("STR:trim_blanks_from_end")
      START_TIMER("STR:trim_blanks_from_end")
      res = trim(self)
     STOP_TIMER("STR:trim_blanks_from_end")
      CHECK
   end function

   function trim_blanks_from_start(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the trimmed version of "self"
      STR(len(self)) :: res
!      res = self(.index_of_character_not_in(" "):)
      STACK("STR:trim_blanks_from_start")
      START_TIMER("STR:trim_blanks_from_start")
      res = self(verify(self," "):)
     STOP_TIMER("STR:trim_blanks_from_start")
      CHECK
   end function

   function same_as(self,string,ignore_case) result(same)
    STR(STR_SIZE) :: self
   ! Test to see if the string is the same as another string
   ! If "ignore_case" is present and TRUE then case is ignored.
      STR(STR_SIZE), IN :: string
      BIN, optional :: ignore_case
      BIN :: same
      BIN :: ignore
      STR(len(self)) :: s1
      STR(len(string)) :: s2
      STACK("STR:same_as")
      START_TIMER("STR:same_as")
      ignore = FALSE
      if (present(ignore_case)) ignore = ignore_case
      if (ignore) then
         s1 = self;   call to_lower_case_(s1)
         s2 = string; call to_lower_case_(s2)
         same = s1==s2
      else
         same = self==string
      end if
     STOP_TIMER("STR:same_as")
      CHECK
   end function

   function n_items(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the number of items in the string
      INT :: res
      INT :: end,f,l
      STR(BSTR_SIZE) :: item
      STACK("STR:n_items")
      START_TIMER("STR:n_items")
      end = 0
      res = 0
      do
         item = " "
         call get_next_item_(self(end+1:),item,f,l)
         if (item==" ") exit
         end = end + l + 1
         res = res + 1
         if (end>len(self)) exit
      end do
     STOP_TIMER("STR:n_items")
      CHECK
   end function

   function item(self,n) result(res)
    STR(STR_SIZE) :: self
   ! Return the item no. "n" in the string.
      IN :: self
      INT, IN :: n
      STR(len(self)) :: res
      STACK("STR:item")
      START_TIMER("STR:item")
      call get_item_(self,n,res)
     STOP_TIMER("STR:item")
      CHECK
   end function

   subroutine get_item(self,n,item,position)
    STR(STR_SIZE) :: self
   ! Get the item no. "n" in the self string and put the result in "item". If
   ! "position" is present it is set to the index of the character after the end
   ! of item "n".
      IN :: self
      INT, IN :: n
      STR(STR_SIZE), OUT :: item
      INT, OUT, optional :: position
      STR(len(self)) :: word
      INT :: i,f,l
      STACK("STR:get_item")
      START_TIMER("STR:get_item")
      f = 1
      do i = 1,n
         call get_next_item_(self(f:),word,last=l)
         if (word==" ") exit
         f = f + l
      end do
      item = word
      if (present(position)) position=f
     STOP_TIMER("STR:get_item")
      CHECK
   end subroutine

   subroutine get_next_item(self,item,first,last,comment_chars,quote_chars)
    STR(STR_SIZE) :: self
   ! Get the first sequence of non-blank characters in the string (i.e. an
   ! "item") and (if present) the "first" and "last" character positions of the
   ! item in the self string.  If the first character of the word is a double
   ! quote then all text between it and the next double quote is treated as one
   ! item. If "quote_chars" is present and not blank, then any of these
   ! characters is regarded as a double quote; but if "quote_chars" is blank
   ! then no quotation characters are recognised. If "comment_chars" is present,
   ! the rest of the string following these comment characters is ignored.
      IN :: self
      STR(STR_SIZE), OUT :: item
      INT, OUT, optional :: first,last
      STR(STR_SIZE), optional :: comment_chars,quote_chars
      STR(16) :: quotes
      STR(1) :: quote
      STR(len(self)) :: word
      INT :: f,l
      STACK("STR:get_next_item")
      START_TIMER("STR:get_next_item")
      quotes = '"'
      if (present(quote_chars)) quotes = quote_chars
      call get_next_item_position_(self,f,l)
      if (f==0) then                          ! all characters are blanks
         word = " "
      else if (quotes/=" " AND scan(self(f:f),quotes)/=0) then ! quotes
         quote = self(f:f)
         l = quote_position_(self(f+1:),quote)
         DIE_IF(l==0,"STR:get_next_item ... unclosed quotes")
         l = f+l
         word = self(f+1:l-1)
      else if (present(comment_chars)) then   ! comments
         if (has_any_characters_in_(self(f:f),comment_chars)) then
            l = f-1
            word = " "
         else
            word = self(f:l)
         end if
      else
         word = self(f:l)
      end if
      item = word
      if(present(first)) first = f
      if(present(last))  last  = l
     STOP_TIMER("STR:get_next_item")
      CHECK
   end subroutine

   subroutine get_next_item_position(self,first,last)
    STR(STR_SIZE) :: self
   ! Get the first and last character positions of the first sequence of
   ! non-blank characters in the string (i.e. a "word")
      INT, OUT :: first,last
      STACK("STR:get_next_item_position")
      START_TIMER("STR:get_next_item_position")
      first = verify(self," ")
      last = scan(self(max(first,1):)//" "," ") - 1
      last = last + max(first,1) - 1
      last = max(last,0)
     STOP_TIMER("STR:get_next_item_position")
      CHECK
   end subroutine

   function split(self) result(res)
    STR(STR_SIZE) :: self
   ! Split the string into a vector of separate items.
      STRVEC(128,:), pointer :: res
      INT :: i,n
      STACK("STR:split")
      START_TIMER("STR:split")
      n = n_items_(self)
      allocate(res(n))
      ADD_MEMORY(len(res(1))*CHR_SIZE*n)
      do i = 1,n
         res(i) = item_(self,i)
      end do
     STOP_TIMER("STR:split")
      UNSTACK
   end function

   subroutine get_next_items(self,word)
    STR(STR_SIZE) :: self
   ! Get items 2 and after from self, i.e. all except the first item.
      STR(len(self)), OUT :: word
      INT :: l
      STACK("STR:get_next_items")
      START_TIMER("STR:get_next_items")
      call get_next_item_(self,word,last=l)
      word = adjustl(self(2+l:))
     STOP_TIMER("STR:get_next_items")
      CHECK
   end subroutine

   function quote_position(self,quote_chars) result(pos)
    STR(STR_SIZE) :: self
   ! Find the position of the first double quote character.
     INT :: pos
     STR(STR_SIZE), optional :: quote_chars
     STACK("STR:quote_position")
     START_TIMER("STR:quote_position")
     if (present(quote_chars)) then
       pos = scan(self,quote_chars)
     else
       pos = index(self,'"')
     end if
     STOP_TIMER("STR:quote_position")
      CHECK
   end function

   function index_of_matching(self,symbol) result(res)
    STR(STR_SIZE) :: self
   ! Return the first index of the matching "symbol" in self.  The first element
   ! of self need not be an opening bracket symbol. Returns zero if no match
   ! could be found, and also a warning.
      STR(STR_SIZE), IN :: symbol
      INT :: res
      STRVEC(len=1,6) :: opening = (/"'",'"',"{","(","[","<"/)
      STRVEC(len=1,6) :: closing = (/"'",'"',"}",")","]",">"/)
      STR(1) :: op,cl,c
      INT :: i,s,n
      STACK("STR:index_of_matching")
      START_TIMER("STR:index_of_matching")
      ENSURE(any(symbol==opening),"STR:index_of_matching ... unrecognised opening symbol")
      ENSURE(includes_(self,symbol),"STR:index_of_matching ... opening symbol cannot be found in self")
      op = symbol
      do i = 1,size(opening)
         if (op/=opening(i)) cycle
         exit
      end do
      cl = closing(i)
      s = scan(self,op)
      n = 0
      res = 0
      do i = s+1,len_trim(self)
         c = self(i:i)
         if      (c==op) then;           n = n + 1
         else if (c==cl AND n==0) then; res = i; exit
         else if (c==cl AND n >0) then;  n = n - 1
         end if
      end do
      WARN_IF(res==0,"STR:index_of_matching ... unmatching number of closing bracket symbols")
     STOP_TIMER("STR:index_of_matching")
      CHECK
   end function

   subroutine insert(self,string,position)
    STR(STR_SIZE) :: self
   ! Insert "string" at "position" into the self string.
      STR(STR_SIZE), IN :: string
      INT, IN :: position
      STR(len(self)) :: rest
      STACK("STR:insert")
      START_TIMER("STR:insert")
      rest = self(position:) 
      self(position:) = string
      self(position+len(string):) = rest
     STOP_TIMER("STR:insert")
      CHECK
   end subroutine

   function align_left(self) result(res)
    STR(STR_SIZE) :: self
   ! Return a string the same as "self" except with the first nonblank character
   ! aligned flush to the LHS of the string
      STR(len(self)) :: res
      STACK("STR:align_left")
      START_TIMER("STR:align_left")
      res = adjustl(self)
     STOP_TIMER("STR:align_left")
      CHECK
   end function

   PURE subroutine left_justify(self)
    STR(STR_SIZE) :: self
   ! Remove leftmost blank characters by shifting all characters to the left
      INOUT :: self
      self = adjustl(self)
     STOP_TIMER("STR:left_justify")
   end subroutine

   function align_right(self) result(res)
    STR(STR_SIZE) :: self
   ! Return a string the same as self except with the last nonblank character
   ! aligned flush to the RHS of the string
      STR(len(self)) :: res
      STACK("STR:align_right")
      START_TIMER("STR:align_right")
      res = adjustr(self)
     STOP_TIMER("STR:align_right")
      CHECK
   end function

   subroutine right_justify(self)
    STR(STR_SIZE) :: self
   ! Remove rightmost blank characters by shifting all characters to the right
      STACK("STR:right_justify")
      START_TIMER("STR:right_justify")
      self = adjustr(self)
     STOP_TIMER("STR:right_justify")
      CHECK
   end subroutine

   function index_of_substring(self,substring,backwards) result(ind)
    STR(STR_SIZE) :: self
   ! Return the starting index of a substring in the original string
      STR(STR_SIZE), IN :: substring
      BIN, optional :: backwards
      INT :: ind
!      ind = index(self,substring,backwards)
      STACK("STR:index_of_substring")
      START_TIMER("STR:index_of_substring")
      if (present(backwards)) then
        ind = index(self,substring,backwards)
      else
        ind = index(self,substring)
      end if
     STOP_TIMER("STR:index_of_substring")
      CHECK
   end function

   function is_included_in(self,string,at_start) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if self is included in "string". Trailing blanks in self are
   ! ignored. If "at_start" is present and TRUE then the result is TRUE only
   ! if self is included at the start of the string.
      STR(STR_SIZE), IN :: string
      BIN, IN, optional :: at_start
      BIN :: res
      INT :: ind
      STACK("STR:is_included_in")
      START_TIMER("STR:is_included_in")
      ind = index(string,trim(self))
      res = ind /= 0
      if (present(at_start)) then
      if (at_start) then
         res = ind == 1
      end if
      end if
     STOP_TIMER("STR:is_included_in")
      CHECK
   end function

   function is_included_in_any(self,strvec) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if self is included in any element of the string vector
   ! "strvec". Trailing blanks in self are *not* ignored.
      STRVEC(STR_SIZE,:), IN :: strvec
      BIN :: res
      STACK("STR:is_included_in_any")
      START_TIMER("STR:is_included_in_any")
      res = any(index(strvec,spread(self,1,size(strvec))) /= 0)
     STOP_TIMER("STR:is_included_in_any")
      CHECK
   end function

   function includes(self,string,at_start) result(res)
    STR(STR_SIZE) :: self
   ! Return true if self includes "string". Trailing blanks in self are ignored
   ! If "at_start" is present, the result is true only if "string" is the first
   ! part of self.
      STR(STR_SIZE), IN :: string
      BIN, IN, optional :: at_start
      BIN :: res
      INT :: ind
      STACK("STR:includes")
      START_TIMER("STR:includes")
      ind = index(trim(self),string)
      res = ind /= 0
      if (present(at_start)) then
      if (at_start) then
         res = ind == 1
      end if
      end if
     STOP_TIMER("STR:includes")
      CHECK
   end function

   function includes_any_in(self,strvec) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if self includes any element of the string vector "strvec".
   ! Trailing blanks in self are ignored.
      STRVEC(STR_SIZE,:), IN :: strvec
      BIN :: res
      INT :: i
      STACK("STR:includes_any_in")
      START_TIMER("STR:includes_any_in")
      res = FALSE
      do i = 1,size(strvec)
         res = index(trim(self),trim(strvec(i))) /= 0
         if (res) exit
      end do
     STOP_TIMER("STR:includes_any_in")
      CHECK
   end function

   function does_not_include(self,string) result(res)
    STR(STR_SIZE) :: self
   ! Return true if self does not include string. Traling blanks in self are ignored
      STR(STR_SIZE), IN :: string
      BIN :: res
      INT :: ind
      STACK("STR:does_not_include")
      START_TIMER("STR:does_not_include")
      ind = index(self(1:len_trim(self)),string)
      res = (ind==0)
     STOP_TIMER("STR:does_not_include")
      CHECK
   end function

   function has_any_characters_in(self,set) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if self has any of the characters in "set".
      STR(STR_SIZE), IN :: set
      BIN :: res
!     res = .index_of_character_in(set) /= 0
      STACK("STR:has_any_characters_in")
      START_TIMER("STR:has_any_characters_in")
      res = scan(self,set) /= 0
     STOP_TIMER("STR:has_any_characters_in")
      CHECK
   end function

   function has_all_characters_in(self,set) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if self has all of its characters in "set".
      STR(STR_SIZE), IN :: set
      BIN :: res
      STACK("STR:has_all_characters_in")
      START_TIMER("STR:has_all_characters_in")
      res = index_of_character_not_in_(self,set) == 0
     STOP_TIMER("STR:has_all_characters_in")
      CHECK
   end function

   function index_of_character_in(self,set,backwards) result(ind)
    STR(STR_SIZE) :: self
   ! In self, scan from left to right and return the index of the first
   ! character in "set". If backwards is present and true, scan from
   ! right to left
      STR(STR_SIZE), IN :: set
      BIN, optional :: backwards
      INT :: ind
!     ind = scan(self,set,backwards)
      STACK("STR:index_of_character_in")
      START_TIMER("STR:index_of_character_in")
      if (present(backwards)) then
        ind = scan(self,set,backwards)
      else
        ind = scan(self,set)
      end if
     STOP_TIMER("STR:index_of_character_in")
      CHECK
   end function

!   verify(set,backwards) result(ind)
!   ! In self, scan from left to right and return the index of the first
!   ! character *not* in "set". If backwards is present and true, scan from
!   ! right to left
!      set :: STR, IN
!      backwards :: BIN, optional
!      ind :: INT
!!      ind = verify(self,set,backwards)
!      if (present(backwards)) then
!        ind = verify(self,set,backwards)
!      else
!        ind = verify(self,set)
!      end
!   end

   function index_of_character_not_in(self,set,backwards) result(ind)
    STR(STR_SIZE) :: self
   ! In self, scan from left to right and return the index of the first
   ! character *not* in "set". If backwards is present and true, scan from
   ! right to left
      STR(STR_SIZE), IN :: set
      BIN, optional :: backwards
      INT :: ind
!      ind = verify(self,set,backwards)
      STACK("STR:index_of_character_not_in")
      START_TIMER("STR:index_of_character_not_in")
      if (present(backwards)) then
        ind = verify(self,set,backwards)
      else
        ind = verify(self,set)
      end if
     STOP_TIMER("STR:index_of_character_not_in")
      CHECK
   end function

   subroutine to_lower_case(self)
    STR(STR_SIZE) :: self
   ! Change upper case charaters to lower case in the original string
       INT :: i
      STACK("STR:to_lower_case")
      START_TIMER("STR:to_lower_case")
      do i = 1,len(self)
         if("A"<=self(i:i) AND self(i:i)<="Z") then
            self(i:i) = achar(iachar(self(i:i))+32)
         end if
      end do
     STOP_TIMER("STR:to_lower_case")
      CHECK
   end subroutine

   subroutine to_upper_case(self)
    STR(STR_SIZE) :: self
   ! Change lower case charaters to upper case in the original string
       INT :: i
      STACK("STR:to_upper_case")
      START_TIMER("STR:to_upper_case")
      do i = 1,len(self)
         if("a"<=self(i:i) AND self(i:i)<="z") then
            self(i:i) = achar(iachar(self(i:i))-32)
         end if
      end do
     STOP_TIMER("STR:to_upper_case")
      CHECK
   end subroutine

   subroutine replace(self,a,b)
    STR(STR_SIZE) :: self
   ! Replace all occurences of string "a" by "b". String "b" can be zero
   ! length, however, replacements only occur up to the last nonblank
   ! character in "self" i.e. up to len_trim(self).
      STR(STR_SIZE) :: a,b
      STR(len(self)) :: post
      INT :: len_a,len_b,i
      STACK("STR:replace")
      START_TIMER("STR:replace")
      ENSURE(len(a)>0,"STR:replace ... len(a) must be non-zero")
      len_a = len(a)
      len_b = len(b)
      i = 0
      do
         i = i + 1
         if (i+len_a-1>len_trim(self)) exit
         if (self(i:i+len_a-1)/=a)     cycle
         post = self(i+len_a:)
         if (len_b>0) self(i:i+len_b-1) = b
         ENSURE(i+len_b<len(self),"STR:replace ... replacement exceeds string length")
         self(i+len_b:) = post
         i = i + len_b - 1
      end do
     STOP_TIMER("STR:replace")
      CHECK
   end subroutine

   subroutine remove(self,a)
    STR(STR_SIZE) :: self
   ! Remove all occurences of "a" from "self".
      STR(STR_SIZE) :: a
      STACK("STR:remove")
      START_TIMER("STR:remove")
      call replace_(self,a,"")
     STOP_TIMER("STR:remove")
      CHECK
   end subroutine

   subroutine remove_blanks(self)
    STR(STR_SIZE) :: self
   ! Replace all blanks by moving all non-blank characters leftwards
      STACK("STR:remove_blanks")
      START_TIMER("STR:remove_blanks")
      call remove_(self," ")
     STOP_TIMER("STR:remove_blanks")
      CHECK
   end subroutine

   subroutine separate_all_characters(self)
    STR(STR_SIZE) :: self
   ! Separate all nonblank characters by one space
      INT :: i
      STACK("STR:separate_all_characters")
      START_TIMER("STR:separate_all_characters")
      i = 1
      do
         if (i>=len_trim(self)) exit
         if (self(i:i)==" ") then
            self(i:) = self(i+1:)
         else
            self(i+2:)    = self(i+1:)
            self(i+1:i+1) = " "
            i = i + 2
         end if
      end do
     STOP_TIMER("STR:separate_all_characters")
      CHECK
   end subroutine

   subroutine separate_before(self,characters)
    STR(STR_SIZE) :: self
   ! Separate the string by placing a space before each character that occurs in
   ! "characters".
      INOUT :: self
      STR(STR_SIZE) :: characters
      STR(1) :: thischar
      INT :: i,last
      STACK("STR:separate_before")
      START_TIMER("STR:separate_before")
      i = 1
      last = len(self)
      do
        if (i>len_trim(self)) exit
        thischar = self(i:i)
        if (includes_(characters,thischar)) then
            self(i+1:last) = self(i:last-1) ! move all along by one.
            self(i:i) = " "
            i = i + 1
        end if
        i = i + 1
      end do
     STOP_TIMER("STR:separate_before")
      CHECK
   end subroutine

   subroutine separate_after(self,characters)
    STR(STR_SIZE) :: self
   ! Separate the string by placing a space after each character that occurs in
   ! "characters".
      INOUT :: self
      STR(STR_SIZE) :: characters
      STR(1) :: thischar
      INT :: i,last
      STACK("STR:separate_after")
      START_TIMER("STR:separate_after")
      i = 1
      last = len(self)
      do
        if (i>=len_trim(self)-1) exit
        thischar = self(i:i)
        if (includes_(characters,thischar)) then
            self(i+2:last) = self(i+1:last-1) ! move all along by one.
            self(i+1:i+1) = " "
            i = i + 1
        end if
        i = i + 1
      end do
     STOP_TIMER("STR:separate_after")
      CHECK
   end subroutine

!  **********************
!  File name manipulation 
!  **********************

   function filename_head(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the head part of a file name, e.g. if self is "/home/file.c" it
   ! returnd the string "file". 
      STR(len(self)) :: res
      INT :: f,l
      STACK("STR:filename_head")
      START_TIMER("STR:filename_head")
      ENSURE(self/=" ","STR:filename_head ... string is blank!")
      l = index_of_character_in_(self,".",backwards=TRUE)
      if (l/=0) then
         l = l - 1
      else
         l = index_of_character_not_in_(self," ",backwards=TRUE)
      end if
      f = index_of_character_in_(self(:l),"/",backwards=TRUE)
      if (f==0) then
         f = 1
      else
         f = f + 1
      end if
      res = self(f:l)
     STOP_TIMER("STR:filename_head")
      CHECK
   end function

   function filename_tail(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the tail part of a file name, e.g. if self is "/home/file.c" it
   ! returnd the string "c". 
      STR(len(self)) :: res
      INT :: f,l
      STACK("STR:filename_tail")
      START_TIMER("STR:filename_tail")
      ENSURE(self/=" ","STR:filename_tail ... string is blank!")
      l = index_of_character_not_in_(self," ",backwards=TRUE)
      f = index_of_character_in_(self(:l),".",backwards=TRUE)
      ENSURE(f>0,"STR:filename_tail ... no dot in file name")
      f = f + 1
      res = self(f:l)
     STOP_TIMER("STR:filename_tail")
      CHECK
   end function

   function filename_directory(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the directory part of a file name, e.g. if self is "/home/file.c" it
   ! returnd the string "/home". 
      STR(len(self)) :: res
      INT :: l
      STACK("STR:filename_directory")
      START_TIMER("STR:filename_directory")
      ENSURE(self/=" ","STR:filename_directory ... string is blank!")
      l = index_of_character_in_(self,"/",backwards=TRUE)
      if (l == 0) then
         res = "."
      else if (l == 1) then
         res = "/"
      else
         l = l - 1
         res = self(:l)
      end if
     STOP_TIMER("STR:filename_directory")
      CHECK
   end function

!  *****************
!  Inquiry functions
!  *****************

   function is_real(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as a real number
      BIN :: res
      REAL :: value
      INT :: i,ios
      STACK("STR:is_real")
      START_TIMER("STR:is_real")
      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
        res = FALSE
      else
        read(unit=self,fmt=*,iostat=ios) value
        res = ios==0
      end if
     STOP_TIMER("STR:is_real")
      CHECK
   end function

   function is_cpx(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as a (fortran) complex number
      BIN :: res
      STACK("STR:is_cpx")
      START_TIMER("STR:is_cpx")
      res = is_a_true_cpx_(self) OR is_a_real_pair_(self)
     STOP_TIMER("STR:is_cpx")
      CHECK
   end function

   function is_a_true_cpx(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as a true (fortran) complex
   ! number
      BIN :: res
      CPX :: value
      INT :: i,ios
      STACK("STR:is_a_true_cpx")
      START_TIMER("STR:is_a_true_cpx")
      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
        res = FALSE
      else if ((self(1:1)=="(" OR self(1:2)=="-(") AND scan(self,",")>1) then
        read(unit=self,fmt=*,iostat=ios) value
        res = ios==0
      end if
     STOP_TIMER("STR:is_a_true_cpx")
      CHECK
   end function

   function is_a_real_pair(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as a pair of
   ! double precision numbers comprising a complex number
      BIN :: res
      INT :: i,ios
      REAL :: r,c
      STACK("STR:is_a_real_pair")
      START_TIMER("STR:is_a_real_pair")
      i = index_of_character_in_(self,"0123456789")
      if (i==0) then
        res = FALSE
      else
        read(unit=self,fmt=*,iostat=ios) r,c
        res = ios==0
      end if
     STOP_TIMER("STR:is_a_real_pair")
      CHECK
   end function

   function is_int(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as an integer number
      BIN :: res
      INT :: value,ios
      BIN :: char
      STACK("STR:is_int")
      START_TIMER("STR:is_int")
      char = NOT has_any_characters_in_(self(1:1),"0123456789-")
      if (char) then
         res = FALSE
      else
         read(unit=self,fmt=*,iostat=ios) value
         res = ios==0
      end if
     STOP_TIMER("STR:is_int")
      CHECK
   end function

   function is_a_true_int(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as an integer number
      BIN :: res
      INT :: value,ios
      STACK("STR:is_a_true_int")
      START_TIMER("STR:is_a_true_int")
      if (has_all_characters_in_(self,"0123456789- ")) then
         read(unit=self,fmt=*,iostat=ios) value
         res = ios==0
      else
         res = FALSE
      end if
     STOP_TIMER("STR:is_a_true_int")
      CHECK
   end function

   function is_bin(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string can be interpreted as a logical
      BIN :: res
      STR(len(self)) :: word
      STACK("STR:is_bin")
      START_TIMER("STR:is_bin")
      read(unit=self,fmt=*) word
      call to_lower_case_(word)
      select case (word)
         case("true", "t","on", "yes","y"); res = TRUE
         case("false","f","off","no", "n"); res = TRUE
         case default;                      res = FALSE
      end select
     STOP_TIMER("STR:is_bin")
      CHECK
   end function

   function is_imprecise_real(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns TRUE if the string can be interpreted as an imprecise double, i.e.
   ! a real fortran F "number" followed immediately (without intervening spaces)
   ! by a quoted "error" in parentheses. 
      BIN :: res
      INT :: f,l,p
      STACK("STR:is_imprecise_real")
      START_TIMER("STR:is_imprecise_real")
      f = index_of_substring_(self,"(")
      l = index_of_substring_(self,")")
      p = index_of_substring_(self,".")
      if (f==0 OR (l-f)<=1) then ! there is no error
         res = FALSE
      else
         res = is_real_(self(1:f-1)) AND is_real_(self(f+1:l-1))
      end if
     STOP_TIMER("STR:is_imprecise_real")
      CHECK
   end function

   function is_alphabetical(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string contains only alphabetical characters.
      BIN :: res
      STR(52) :: letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      STACK("STR:is_alphabetical")
      START_TIMER("STR:is_alphabetical")
      res = index_of_character_not_in_(self,letters) == 0
     STOP_TIMER("STR:is_alphabetical")
      CHECK
   end function

   function is_numeric(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string contains only digit characters.
      BIN :: res
      STR(10) :: digits = "0123456789"
      STACK("STR:is_numeric")
      START_TIMER("STR:is_numeric")
      res = index_of_character_not_in_(self,digits) == 0
     STOP_TIMER("STR:is_numeric")
      CHECK
   end function

   function is_alphanumeric(self) result(res)
    STR(STR_SIZE) :: self
   ! Returns true if the string contains only alphanumeric characters.
      BIN :: res
      STR(62) :: alphanumeric = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      STACK("STR:is_alphanumeric")
      START_TIMER("STR:is_alphanumeric")
      res = index_of_character_not_in_(self,alphanumeric) == 0
     STOP_TIMER("STR:is_alphanumeric")
      CHECK
   end function

!  **********************************
!  Conversion to value type variables
!  **********************************

   function to_real(self) result(value)
    STR(STR_SIZE) :: self
   ! Returns the real number corresponding to the first token string
      REAL :: value
      INT :: ios
      STACK("STR:to_real")
      START_TIMER("STR:to_real")
      read(unit=self,fmt=*,iostat=ios) value
      ENSURE(ios==0,"STR:to_real ... not a real number")
     STOP_TIMER("STR:to_real")
      CHECK
   end function

   function frac_to_real(self) result(value)
    STR(STR_SIZE) :: self
   ! Returns the real number corresponding to the first token string,
   ! represented as a fraction.
      IN :: self
      REAL :: value
      STR(len(self)) :: numerator,denominator,word1
      REAL :: num,denom
      INT :: ind,ios
      STACK("STR:frac_to_real")
      START_TIMER("STR:frac_to_real")
      word1 = self
      call remove_blanks_(word1)
      call separate_before_(word1,"/")
      call separate_after_(word1,"/")
      ind = index_of_character_in_(word1,"/")
      numerator = word1(1:ind-1)
      denominator = word1(ind+1:)
      read(unit=numerator,fmt=*,iostat=ios) num
      ENSURE(ios==0,trim(numerator) // " is not a real number")
      read(unit=denominator,fmt=*,iostat=ios) denom
      ENSURE(ios==0,trim(denominator) // " is not a real number")
      value = num/denom
     STOP_TIMER("STR:frac_to_real")
      CHECK
   end function

   function to_cpx(self) result(value)
    STR(STR_SIZE) :: self
   ! Returns the cpx number corresponding to the token string
   ! If a single token won't do, two tokens are inputted to see
   ! if it could be two real numbers in a row representing a cpx.
      CPX :: value
      REAL :: r,c
      STACK("STR:to_cpx")
      START_TIMER("STR:to_cpx")
      if (is_a_true_cpx_(self)) then
         read(unit=self,fmt=*) value
      else if (is_a_real_pair_(self)) then
         read(unit=self,fmt=*) r,c
         value = cmplx(r,c,kind=CPX_KIND)
      else
         DIE("STR:to_cpx ... Could not read complex number")
      end if
     STOP_TIMER("STR:to_cpx")
      CHECK
   end function

   function to_int(self) result(value)
    STR(STR_SIZE) :: self
   ! Returns the integer number corresponding to the first token string
      INT :: value
      INT :: ios
      STACK("STR:to_int")
      START_TIMER("STR:to_int")
      read(unit=self,fmt=*,iostat=ios) value
      ENSURE(ios==0,"STR:to_int ... not a real number")
     STOP_TIMER("STR:to_int")
      CHECK
   end function

   function to_bin(self) result(value)
    STR(STR_SIZE) :: self
   ! Returns the logical corresponding to the first token string
      BIN :: value
      STR(len(self)) :: word
      STACK("STR:to_bin")
      START_TIMER("STR:to_bin")
      word = self
      call to_lower_case_(word)
      select case (word)
         case("true", "t","on", "yes","y"); value=TRUE
         case("false","f","off","no", "n"); value=FALSE
         case default; DIE("STR:to_bin ... cant change to logical type, "//self)
      end select
     STOP_TIMER("STR:to_bin")
      CHECK
   end function

   subroutine to_imprecise_real(self,value,error)
    STR(STR_SIZE) :: self
   ! Returns the imprecise number corresponding to the first token, i.e. the
   ! real number "value" with a quoted "error" in parentheses immediately
   ! afterwards.  If the error is not present in the string it is assumed to be
   ! zero. This only works for "f" numbers.
      REAL :: value,error
      STR(len(self)) :: item,real_str,err_str
      INT :: f,l,p
      STACK("STR:to_imprecise_real")
      START_TIMER("STR:to_imprecise_real")
      call get_next_item_(self,item)
      f = index_of_substring_(item,"(")
      l = index_of_substring_(item,")")
      p = index_of_substring_(item,".")
      if (f==0 OR (l-f)<=1) then ! there is no error
         real_str = item
         err_str = "0"
      else
         real_str = item(1:f-1)
         err_str = item(f+1:l-1)
      end if
      ENSURE(is_real_(real_str),"STR:to_imprecise_real ... expected real number in input")
      ENSURE(is_real_(err_str),"STR:to_imprecise_real ... expected real number error in input")
      value = to_real_(real_str)
      error = to_real_(err_str)
      if (p>0) error = error * TEN**(-f+p+1)
     STOP_TIMER("STR:to_imprecise_real")
      CHECK
   end subroutine

!  ******************************
!  Conversion to self variables
!  ******************************

   subroutine from_int(self,value)
    STR(STR_SIZE) :: self
   ! Set the original string to the result from changing integer "value" to a
   ! string
      INT :: value
      STACK("STR:from_int")
      START_TIMER("STR:from_int")
      self = " "
      write(self,fmt=*) value
      call left_justify_(self)
     STOP_TIMER("STR:from_int")
      CHECK
   end subroutine

   subroutine from_bin(self,value)
    STR(STR_SIZE) :: self
   ! Set the original string to the result from changing logical "value" to a
   ! string
      BIN :: value
      STACK("STR:from_bin")
      START_TIMER("STR:from_bin")
      self = " "
      write(self,*) value
      call left_justify_(self)
     STOP_TIMER("STR:from_bin")
      CHECK
   end subroutine

   subroutine from_real(self,value)
    STR(STR_SIZE) :: self
   ! Set the original string to the result from changing real "value" to a
   ! string
      REAL :: value
      STACK("STR:from_real")
      START_TIMER("STR:from_real")
      self = " "
      write(self,*) value
      call left_justify_(self)
     STOP_TIMER("STR:from_real")
      CHECK
   end subroutine

   subroutine from_cpx(self,value)
    STR(STR_SIZE) :: self
   ! Set the original string to the result from changing cpx "value" to a string
      CPX :: value
      STACK("STR:from_cpx")
      START_TIMER("STR:from_cpx")
      self = " "
      write(self,*) value
      call left_justify_(self)
     STOP_TIMER("STR:from_cpx")
      CHECK
   end subroutine

!  ****************
!  Units conversion
!  ****************

   function is_known_unit(self) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if the string represents a known unit string
      BIN :: res
      STR(len(self)) :: word
      INT :: l
      STACK("STR:is_known_unit")
      START_TIMER("STR:is_known_unit")
      word = self
      call to_lower_case_(word)
      l = len_trim(word)
      if (word(l:l)=="s" AND l>1) word = word(1:l-1)
      res = is_one_of_(word,(/ &
              "debye           ", &
              "debye-angstrom  ", &
              "debye-angstrom^2", &
              "degree          ", &
              "bohr            ", &
              "angstrom        ", &
              "angstrom^2      ", &
              "meter           ", &
              "amu             ", &
              "wavenumber      ", &
              "ev              ", &
              "kelvin          ", &
              "joule           ", &
              "kilojoule       ", &
              "kjoule          ", &
              "kj              ", &
              "kcal/mol        " /))
     STOP_TIMER("STR:is_known_unit")
      CHECK
   end function

   function conversion_factor(self) result(res)
    STR(STR_SIZE) :: self
   ! Return the conversion factor which converts a value into the specified
   ! unit "self", assuming that the value has default units. In most cases
   ! the default units are atomic units, or radians for angles.
      REAL :: res
      STR(len(self)) :: word
      INT :: l
      STACK("STR:conversion_factor")
      START_TIMER("STR:conversion_factor")
      word = self
      call to_lower_case_(word)
      l = len_trim(word)
      if (word(l:l)=="s" AND l>1) word = word(1:l-1)
      select case (word)
        case ("debye           "); res = DEBYE_PER_AU
        case ("debye-angstrom  "); res = DEBYE_PER_AU*(ANGSTROM_PER_BOHR)
        case ("debye-angstrom^2"); res = DEBYE_PER_AU*(ANGSTROM_PER_BOHR)**2
        case ("degree          "); res = DEGREE_PER_RADIAN
        case ("bohr            "); res = BOHR_PER_BOHR
        case ("angstrom        "); res = ANGSTROM_PER_BOHR
        case ("angstrom^2      "); res = ANGSTROM_PER_BOHR**2
        case ("meter           "); res = ANGSTROM_PER_BOHR*METER_PER_ANGSTROM
        case ("amu             "); res = AMU_PER_MASS_OF_ELECTRON
        case ("wavenumber      "); res = WAVENUMBER_PER_HARTREE
        case ("ev              "); res = EV_PER_HARTREE
        case ("kelvin          "); res = KELVIN_PER_HARTREE
        case ("joule           "); res = JOULE_PER_HARTREE
        case ("kilojoule       "); res = KJOULE_PER_HARTREE
        case ("kjoule          "); res = KJOULE_PER_HARTREE
        case ("kj              "); res = KJOULE_PER_HARTREE
        case ("kcal/mol        "); res = KCALMOL_PER_HARTREE
        case default;        allocate(tonto%known_keywords(17))
        tonto%known_keywords(1) = "debye           "
        tonto%known_keywords(2) = "debye-angstrom  "
        tonto%known_keywords(3) = "debye-angstrom^2"
        tonto%known_keywords(4) = "degree          "
        tonto%known_keywords(5) = "bohr            "
        tonto%known_keywords(6) = "angstrom        "
        tonto%known_keywords(7) = "angstrom^2      "
        tonto%known_keywords(8) = "meter           "
        tonto%known_keywords(9) = "amu             "
        tonto%known_keywords(10) = "wavenumber      "
        tonto%known_keywords(11) = "ev              "
        tonto%known_keywords(12) = "kelvin          "
        tonto%known_keywords(13) = "joule           "
        tonto%known_keywords(14) = "kilojoule       "
        tonto%known_keywords(15) = "kjoule          "
        tonto%known_keywords(16) = "kj              "
        tonto%known_keywords(17) = "kcal/mol        "
        call unknown_(tonto,word,"STR:conversion_factor")
        deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("STR:conversion_factor")
      CHECK
   end function

   function is_one_of(self,allowed) result(res)
    STR(STR_SIZE) :: self
   ! Return TRUE if "self" is one of the strings in "allowed".
      STRVEC(STR_SIZE,:) :: allowed
      BIN :: res
      STACK("STR:is_one_of")
      START_TIMER("STR:is_one_of")
      res = any(self==allowed)
     STOP_TIMER("STR:is_one_of")
      CHECK
   end function

end
