!---------------------------------------------------------------------------
!
! INTVECINTVECHASH : 
!
! A hash table (or associative array) with INTVEC keys and INTVEC values.
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
! $Id: intvecintvechash.foo,v 1.2.2.1 2003/09/18 05:33:22 reaper Exp $
!---------------------------------------------------------------------------

module INTVECINTVECHASH_MODULE

#  include "intvecintvechash.use"

   implicit none

#  include "macros"
#  include "intvecintvechash.int"


   INT, save :: index_of_last_key

contains

   subroutine create(self)
    INTVECINTVECHASH :: self
   ! Create the hash object.
      PTR :: self
      STACK("INTVECINTVECHASH:create")
      START_TIMER("INTVECINTVECHASH:create")
      allocate(self)
      ADD_MEMORY(INTVECINTVECHASH_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("INTVECINTVECHASH:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,n_size,keydim,valdim)
    INTVECINTVECHASH :: self
   ! Create the hash with key array and value array with length "n_size".  The
   ! dimension of each key is "keydim". The dimension of each values is "valdim"
      PTR :: self
      INT :: n_size,keydim,valdim
      STACK("INTVECINTVECHASH:create_1")
      START_TIMER("INTVECINTVECHASH:create_1")
      call create_(self)
      self%n_keys = 0
      self%n_size = n_size
      call create_(self%keys,keydim,n_size)
      call create_(self%values,valdim,n_size)
     STOP_TIMER("INTVECINTVECHASH:create_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    INTVECINTVECHASH :: self
   ! Destroy the object.
      PTR :: self
      STACK("INTVECINTVECHASH:destroy")
      START_TIMER("INTVECINTVECHASH:destroy")
      if (NOT associated(self)) then; STOP_TIMER("INTVECINTVECHASH:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(INTVECINTVECHASH_SIZE)
      deallocate(self)
     STOP_TIMER("INTVECINTVECHASH:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    INTVECINTVECHASH :: self
   ! Nullify the pointer parts
     STACK("INTVECINTVECHASH:nullify_ptr_part")
     START_TIMER("INTVECINTVECHASH:nullify_ptr_part")
     nullify(self%keys)
     nullify(self%values)
     STOP_TIMER("INTVECINTVECHASH:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    INTVECINTVECHASH :: self
   ! Destroy the pointer parts
      STACK("INTVECINTVECHASH:destroy_ptr_part")
      START_TIMER("INTVECINTVECHASH:destroy_ptr_part")
      call destroy_(self%keys)
      call destroy_(self%values)
     STOP_TIMER("INTVECINTVECHASH:destroy_ptr_part")
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

   subroutine set_defaults(self)
    INTVECINTVECHASH :: self
   ! Set defaults
      STACK("INTVECINTVECHASH:set_defaults")
      START_TIMER("INTVECINTVECHASH:set_defaults")
      self%n_keys = 0
      self%n_size = 0
      self%reverse_search = FALSE
     STOP_TIMER("INTVECINTVECHASH:set_defaults")
      CHECK
   end subroutine

   subroutine set_reverse_search(self,value)
    INTVECINTVECHASH :: self
   ! Set the .reverse_search switch to "value". This determines whether the
   ! search through the keys arrays occurs in reverse order, which may be useful
   ! if you know that the most recent keys added are more likely to contain the
   ! values you are looking for.
      BIN :: value
      STACK("INTVECINTVECHASH:set_reverse_search")
      START_TIMER("INTVECINTVECHASH:set_reverse_search")
      self%reverse_search = value
     STOP_TIMER("INTVECINTVECHASH:set_reverse_search")
      CHECK
   end subroutine

   subroutine create_copy(self,hash)
    INTVECINTVECHASH :: self
   ! Create a copy of "hash"
      PTR :: self
      INTVECINTVECHASH :: hash
      STACK("INTVECINTVECHASH:create_copy")
      START_TIMER("INTVECINTVECHASH:create_copy")
      call create_(self)
      call copy_(self,hash)
     STOP_TIMER("INTVECINTVECHASH:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,hash)
    INTVECINTVECHASH :: self
   ! Make a "self" copy of "hash". NOTE: pointer parts are pointer copied.
      INTVECINTVECHASH :: hash
      STACK("INTVECINTVECHASH:copy")
      START_TIMER("INTVECINTVECHASH:copy")
      self = hash
      call create_copy_(self%keys,hash%keys)
      call create_copy_(self%values,hash%values)
     STOP_TIMER("INTVECINTVECHASH:copy")
      UNSTACK
   end subroutine

   subroutine shrink(self)
    INTVECINTVECHASH :: self
   ! Shrinks the size of the keys and values arrays to dimension ".n_keys",
   ! *only* when ".n_size" is bigger than the number of keys stored.  Contents
   ! are retained. This is used to save memory.
   STACK("INTVECINTVECHASH:shrink")
   START_TIMER("INTVECINTVECHASH:shrink")
   ENSURE(self%n_size>0,"INTVECINTVECHASH:shrink ... size of hash is 0")
     if (self%n_size<=self%n_keys) then; STOP_TIMER("INTVECINTVECHASH:shrink") UNSTACK return; end if
     call shrink_(self,self%n_keys)
     STOP_TIMER("INTVECINTVECHASH:shrink")
      UNSTACK
   end subroutine

   subroutine shrink_1(self,n_size)
    INTVECINTVECHASH :: self
   ! Shrinks the size of the keys and values arrays to dimension "n_size".
   ! Contents are retained, where possible.
     INT, IN :: n_size
   STACK("INTVECINTVECHASH:shrink_1")
   START_TIMER("INTVECINTVECHASH:shrink_1")
   ENSURE(self%n_size>0,"INTVECINTVECHASH:shrink_1 ... size of hash is 0")
     self%n_keys = min(self%n_keys,n_size)
     self%n_size = n_size
     call shrink_columns_(self%keys,n_size)
     call shrink_columns_(self%values,n_size)
     STOP_TIMER("INTVECINTVECHASH:shrink_1")
      UNSTACK
   end subroutine

   subroutine expand(self,n_size)
    INTVECINTVECHASH :: self
   ! Expands the size of the keys and values arrays to dimension "n_size".
   ! Contents are retained.  
     INT, IN :: n_size
   STACK("INTVECINTVECHASH:expand")
   START_TIMER("INTVECINTVECHASH:expand")
   ENSURE(n_size>self%n_size,"INTVECINTVECHASH:expand ... keys is already big enough")
   ENSURE(n_size>0,"INTVECINTVECHASH:expand ... keys and values not created")
     self%n_size = n_size
     call expand_columns_(self%keys,n_size)
     call expand_columns_(self%values,n_size)
     STOP_TIMER("INTVECINTVECHASH:expand")
      UNSTACK
   end subroutine

   subroutine append_pair(self,key,value)
    INTVECINTVECHASH :: self
   ! Append the "key" and the corresponding "value" to the hash table.
   ! If the .keys or .values arrays are too small, they are doubled in size.
     INTVEC(:), IN :: key
     INTVEC(:), IN :: value
     INT :: n
     STACK("INTVECINTVECHASH:append_pair")
     START_TIMER("INTVECINTVECHASH:append_pair")
     if (self%n_keys+1>self%n_size) call expand_(self,2*(self%n_keys+1))
     n = self%n_keys + 1
     self%n_keys = n
     self%keys(:,n) = key
     self%values(:,n) = value
     STOP_TIMER("INTVECINTVECHASH:append_pair")
      UNSTACK
   end subroutine

   subroutine set(self,key,value)
    INTVECINTVECHASH :: self
   ! Append the "key" and the corresponding "value" to the hash table.
   ! If the .keys or .values arrays are too small, they are doubled in size.
     INTVEC(:), IN :: key
     INTVEC(:), IN :: value
     STACK("INTVECINTVECHASH:set")
     START_TIMER("INTVECINTVECHASH:set")
     call append_pair_(self,key,value)
     STOP_TIMER("INTVECINTVECHASH:set")
      UNSTACK
   end subroutine

   subroutine append_pairs(self,keys,values)
    INTVECINTVECHASH :: self
   ! Append the "keys" and the corresponding "values" to the hash table.
   ! If the .keys or .values arrays are too small, they are doubled in size.
     INTMAT(:,:), IN :: keys
     INTMAT(:,:), IN :: values
     INT :: new,f,l
   STACK("INTVECINTVECHASH:append_pairs")
   START_TIMER("INTVECINTVECHASH:append_pairs")
   ENSURE(size(keys,2)==size(values),"INTVECINTVECHASH:append_pairs ... keys and values must have same length")
     new = size(keys,2)
     if (self%n_keys+new>self%n_size) call expand_(self,2*(self%n_keys+new))
     f = self%n_keys + 1
     l = self%n_keys + new
     self%n_keys = l
     self%keys(:,f:l) = keys
     self%values(:,f:l) = values
     STOP_TIMER("INTVECINTVECHASH:append_pairs")
      UNSTACK
   end subroutine

   subroutine set_1(self,keys,values)
    INTVECINTVECHASH :: self
   ! Append the "keys" and the corresponding "values" to the hash table.
   ! If the .keys or .values arrays are too small, they are doubled in size.
     INTMAT(:,:), IN :: keys
     INTMAT(:,:), IN :: values
   STACK("INTVECINTVECHASH:set_1")
   START_TIMER("INTVECINTVECHASH:set_1")
   ENSURE(size(keys,2)==size(values),"INTVECINTVECHASH:set_1 ... keys and values must have same length")
     call append_pairs_(self,keys,values)
     STOP_TIMER("INTVECINTVECHASH:set_1")
      UNSTACK
   end subroutine

   subroutine delete(self,key,has_key)
    INTVECINTVECHASH :: self
   ! Delete the "key" and the corresponding "value" from the hash table.  A
   ! fatal error occurs if the "key" is not there, unless "has_key" is present.
   ! If it is present, "has_key" is set to TRUE if element *was* there
   ! (after the delete operation it is not there!), otherwise it is set false.
   ! There is no change made to the size of the hash table.
     INTVEC(:), IN :: key
     BIN, OUT, optional :: has_key
     INT :: i
     STACK("INTVECINTVECHASH:delete")
     START_TIMER("INTVECINTVECHASH:delete")
     i = index_of_key_(self,key)
     if (i>0) then
        if (present(has_key)) has_key = TRUE
        call delete_item_(self,i)
     else
        if (present(has_key)) then; has_key = FALSE
        else;  DIE("INTVECINTVECHASH:delete ... no value exists for key = "//trim(to_str_(key)))
        end if
     end if
     STOP_TIMER("INTVECINTVECHASH:delete")
      CHECK
   end subroutine

   subroutine delete_item(self,index)
    INTVECINTVECHASH :: self
   ! Delete element "index" from the hash table.  No change is made to the size
   ! of the hash table.
     INT, IN :: index
     INT :: i,n
   STACK("INTVECINTVECHASH:delete_item")
   START_TIMER("INTVECINTVECHASH:delete_item")
   ENSURE(index>0,"INTVECINTVECHASH:delete_item ... index must be positive")
   ENSURE(index<=self%n_keys,"INTVECINTVECHASH:delete_item ... index must be less than number of keys")
     i = index
     n = self%n_keys - 1
     self%keys(:,i:n) = self%keys(:,i+1:self%n_keys)
     self%values(:,i:n) = self%values(:,i+1:self%n_keys)
     self%n_keys = n
     STOP_TIMER("INTVECINTVECHASH:delete_item")
      CHECK
   end subroutine

   function index_of_key(self,key) result(res)
    INTVECINTVECHASH :: self
   ! Returns the index of any key which matches "key" in the hash. If there is
   ! no match, then the result is 0.
     INTVEC(:), IN :: key
     INT :: res
     INT :: i
     STACK("INTVECINTVECHASH:index_of_key")
     START_TIMER("INTVECINTVECHASH:index_of_key")
     res = 0
     if (self%reverse_search) then
        do i = self%n_keys,1,-1
           if (any(key/=self%keys(:,i))) cycle
           res = i
           exit
        end do
     else
        do i = 1,self%n_keys
           if (any(key/=self%keys(:,i))) cycle
           res = i
           exit
        end do
     end if
     index_of_last_key = res
     STOP_TIMER("INTVECINTVECHASH:index_of_key")
      CHECK
   end function

   function has_key(self,key,index) result(res)
    INTVECINTVECHASH :: self
   ! Returns TRUE if the hash contains a specific "key". If present, "index" is
   ! set to the index of that key.
     INTVEC(:), IN :: key
     INT, OUT, optional :: index
     BIN :: res
     INT :: i
     STACK("INTVECINTVECHASH:has_key")
     START_TIMER("INTVECINTVECHASH:has_key")
     i = index_of_key_(self,key) 
     res = i > 0
     if (present(index)) index = i
     STOP_TIMER("INTVECINTVECHASH:has_key")
      CHECK
   end function

   function value_for_key(self,key,has_key,index) result(res)
    INTVECINTVECHASH :: self
   ! Returns the value corresponding to the hash "key". If "has_key" is not
   ! present, an error is generated if no matching "key" can be found in the
   ! hash; if it is present, it is set to TRUE if the the key is present, or
   ! FALSE otherwise. If "index" is present, then it is set to the value of the
   ! index of "key" in the hash table.
     INTVEC(:), IN :: key
     BIN, OUT, optional :: has_key
     INT, OUT, optional :: index
     INTVEC(size(self%values,1)) :: res
     INT :: i
     STACK("INTVECINTVECHASH:value_for_key")
     START_TIMER("INTVECINTVECHASH:value_for_key")
     i = index_of_key_(self,key)
     if (i>0) then
        if (present(has_key)) has_key = TRUE
        res = self%values(:,i)
     else
        if (present(has_key)) then; has_key = FALSE
        else; DIE("INTVECINTVECHASH:value_for_key ... no value exists for key = "//trim(to_str_(key)))
        end if
     end if
     if (present(index)) index = i
     STOP_TIMER("INTVECINTVECHASH:value_for_key")
      CHECK
   end function

   function value_for_item(self,index) result(res)
    INTVECINTVECHASH :: self
   ! Returns the "index"-th element in the hash table. 
     INT, IN :: index
     INTVEC(size(self%values,1)) :: res
   STACK("INTVECINTVECHASH:value_for_item")
   START_TIMER("INTVECINTVECHASH:value_for_item")
   ENSURE(index<=self%n_keys,"INTVECINTVECHASH:value_for_item ... index out of range")
   ENSURE(index>0,"INTVECINTVECHASH:value_for_item ... index out of range")
     res = self%values(:,index)
     STOP_TIMER("INTVECINTVECHASH:value_for_item")
      CHECK
   end function
      
end 
