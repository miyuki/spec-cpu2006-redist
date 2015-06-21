!-------------------------------------------------------------------------------
!
! BASIS: For gaussian basis sets
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
! $Id: basis.foo,v 1.17.2.8 2003/11/13 05:34:14 reaper Exp $
!
!-------------------------------------------------------------------------------

module BASIS_MODULE

#  include "basis.use"

   implicit none

#  include "macros"
#  include "basis.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

contains

!*******************************************************************************
!                             Create and Destroy Routines
!*******************************************************************************

   subroutine create(self)
    BASIS :: self
   ! Create a basis object
      PTR :: self
      STACK("BASIS:create")
      START_TIMER("BASIS:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(BASIS_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("BASIS:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    BASIS :: self
   ! Destroy a basis object
      PTR :: self
      STACK("BASIS:destroy")
      START_TIMER("BASIS:destroy")
      if (NOT associated(self)) then; STOP_TIMER("BASIS:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(BASIS_SIZE)
      deallocate(self)
     STOP_TIMER("BASIS:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    BASIS :: self
   ! Nullify the shell parts of self
      STACK("BASIS:nullify_ptr_part")
      START_TIMER("BASIS:nullify_ptr_part")
      nullify(self%shell)
     STOP_TIMER("BASIS:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    BASIS :: self
   ! Destroy the shell parts of self
      STACK("BASIS:destroy_ptr_part")
      START_TIMER("BASIS:destroy_ptr_part")
      call destroy_(self%shell)
     STOP_TIMER("BASIS:destroy_ptr_part")
      UNSTACK
   end subroutine

!   created result(res) ::: pure
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result(res) ::: pure
!   ! Returns true if self has *not* been created
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   subroutine create_copy(self,b)
    BASIS :: self
   ! Create a copy of the basis "b".
     BASIS, IN :: b
     PTR :: self
     STACK("BASIS:create_copy")
     START_TIMER("BASIS:create_copy")
     call create_(self)
     call copy_(self,b)
     STOP_TIMER("BASIS:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,b)
    BASIS :: self
   ! Copy a basis "b" to "self". Make sure pointer parts are first
   ! destroyed or nullified, as you want.
      BASIS, IN :: b
      STACK("BASIS:copy")
      START_TIMER("BASIS:copy")
      self = b
      if (associated(b%shell)) call create_copy_(self%shell,b%shell)
     STOP_TIMER("BASIS:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    BASIS :: self
   ! Create and set up a default basis set
      STACK("BASIS:set_defaults")
      START_TIMER("BASIS:set_defaults")
      self%label   = " "
      self%n_shell = 0
      self%n_bf    = 0
      self%n_prim  = 0
     STOP_TIMER("BASIS:set_defaults")
      CHECK
   end subroutine

   subroutine update(self)
    BASIS :: self
   ! Update the shell data, if it exists
      STACK("BASIS:update")
      START_TIMER("BASIS:update")
      if (NOT associated(self%shell)) then; STOP_TIMER("BASIS:update") CHECK return; end if
      self%n_shell = no_of_shells_(self)
      self%n_bf    = no_of_basis_functions_(self)
      self%n_prim  = no_of_primitives_(self)
     STOP_TIMER("BASIS:update")
      CHECK
   end subroutine

   subroutine set_label(self,label)
    BASIS :: self
   ! Set the basis label
      STR(STR_SIZE) :: label
      STACK("BASIS:set_label")
      START_TIMER("BASIS:set_label")
      self%label = label
     STOP_TIMER("BASIS:set_label")
      CHECK
   end subroutine

   subroutine resolve_by_label(self,label,basis,clobber,found)
    BASIS :: self
   ! Resolve "self" by pointer assigning it to the element in "basis" which has
   ! a label which matches "label". If "clobber" is present and TRUE (the
   ! default situation), then "self" is pointer assigned to the matching element
   ! in "basis" irrespective of whether it is already associated; otherwise it
   ! is not pointer assigned. If present, "found" is set TRUE if "self" is
   ! resolved (or was already resolved if clobber was not set), or false
   ! otherwise. If "found" is not present, and a match has not been found, an
   ! error is generated.
      PTR :: self
      STR(STR_SIZE) :: label
      BASISVEC(:), PTR :: basis
      BIN, optional :: clobber,found
      INT :: b
      BINVEC(:), PTR :: check
      BIN :: fnd
   STACK("BASIS:resolve_by_label")
   START_TIMER("BASIS:resolve_by_label")
   ENSURE(associated(basis),"BASIS:resolve_by_label ... no basis set")
      if (present(clobber)) then
      if (NOT clobber) then
      if (associated(self)) then
      if (self%label/=" ") then
         if (present(found)) found = TRUE
         STOP_TIMER("BASIS:resolve_by_label") CHECK return
      end if
      end if
      end if
      end if
      call create_(check,size(basis))
      check = basis%label==label
      b = index_of_first_true_element_(check)
      call destroy_(check)
      fnd = b>0
      if (fnd) self => basis(b) ! NOTE : this is a pointer assign NOT COPY
      if (present(found)) then; found = fnd
      else; ENSURE(fnd,"BASIS:resolve_by_label ... unknown basis label, "// trim(label))
      end if
     STOP_TIMER("BASIS:resolve_by_label")
      CHECK
   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    BASIS :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("BASIS:read_keywords")
     START_TIMER("BASIS:read_keywords")
     ENSURE(next_item_(stdin)=="{","BASIS:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("BASIS:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    BASIS :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("BASIS:process_keyword")
     START_TIMER("BASIS:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}            "); ! exit surrounding loop
       case ("gamess-us=   "); call read_gamess_us_(self)
       case ("label=       "); call read_label_(self)
       case ("shells=      "); call read_shell_(self)
       case ("units=       "); call read_units_(self)
       case ("put          "); call put_(self)
       case ("put_table    "); call put_table_(self)
       ! These are only for making custom tables for the list type
       case ("put_label    "); call put_(stdout,self%label,int_width=TRUE)
       case ("put_n_shells "); call put_(stdout,self%n_shell)
       case ("put_n_bf     "); call put_(stdout,self%n_bf)
       case ("put_n_prim   "); call put_(stdout,self%n_prim)
       case ("tonto-style= "); call read_tonto_style_(self)
       case ("flush        "); call flush_(stdout)
       case  default ;      allocate(tonto%known_keywords(13))
       tonto%known_keywords(1) = "}            "
       tonto%known_keywords(2) = "gamess-us=   "
       tonto%known_keywords(3) = "label=       "
       tonto%known_keywords(4) = "shells=      "
       tonto%known_keywords(5) = "units=       "
       tonto%known_keywords(6) = "put          "
       tonto%known_keywords(7) = "put_table    "
       tonto%known_keywords(8) = "put_label    "
       tonto%known_keywords(9) = "put_n_shells "
       tonto%known_keywords(10) = "put_n_bf     "
       tonto%known_keywords(11) = "put_n_prim   "
       tonto%known_keywords(12) = "tonto-style= "
       tonto%known_keywords(13) = "flush        "
       call unknown_(tonto,word,"BASIS:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("BASIS:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    BASIS :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("BASIS:read_units")
      START_TIMER("BASIS:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("BASIS:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    BASIS :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("BASIS:read_junk")
      START_TIMER("BASIS:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("BASIS:read_junk")
      CHECK
   end subroutine

   subroutine read_label(self)
    BASIS :: self
   ! Read only the basis label
      STACK("BASIS:read_label")
      START_TIMER("BASIS:read_label")
      call read_(stdin,self%label)
     STOP_TIMER("BASIS:read_label")
      CHECK
   end subroutine

   subroutine read_shell(self)
    BASIS :: self
   ! Read a shell
      STACK("BASIS:read_shell")
      START_TIMER("BASIS:read_shell")
      call read_list_keywords_(self%shell)
      call update_(self)
     STOP_TIMER("BASIS:read_shell")
      UNSTACK
   end subroutine

   subroutine read_tonto_style(self)
    BASIS :: self
   ! Create and read a tonto style basis set
      STACK("BASIS:read_tonto_style")
      START_TIMER("BASIS:read_tonto_style")
      call read_label_(self)
      call set_keys_(self%shell,(/"l_chr=","n_cc= ","ex,cc="/))
      call read_data_(self%shell)
      call update_(self)
     STOP_TIMER("BASIS:read_tonto_style")
      UNSTACK
   end subroutine

   subroutine read_gamess_us(self)
    BASIS :: self
   ! Create and read a GAMESS-US style basis set
      STRVEC(STR_SIZE,:), PTR :: the_keys
      STACK("BASIS:read_gamess_us")
      START_TIMER("BASIS:read_gamess_us")
      call read_label_(self)
      the_keys => split_(("l_chr= n_cc= junk,ex,cc="))
      call set_keys_(self%shell,the_keys)
      call destroy_(the_keys)
      call read_data_(self%shell)
      call update_(self)
     STOP_TIMER("BASIS:read_gamess_us")
      UNSTACK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    BASIS :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("BASIS:read_keys")
     START_TIMER("BASIS:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("BASIS:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    BASIS :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("BASIS:process_keys")
      START_TIMER("BASIS:process_keys")
      ENSURE(associated(keys),"BASIS:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"BASIS:process_keys ... no matching closing brace, }")
            internal => keys(k:k+l-1)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call revert_(stdin)
            k = k+l-1
         else if (includes_(keyword," ")) then
            internal => split_(keyword)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call destroy_(internal)
            call revert_(stdin)
         else
            call process_keyword_(self,keyword)
         end if
         if (k==n_key) exit
      end do
     STOP_TIMER("BASIS:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    BASIS :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("BASIS:keys_created")
      START_TIMER("BASIS:keys_created")
      res = associated(keys)
     STOP_TIMER("BASIS:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    BASIS :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("BASIS:set_keys")
     START_TIMER("BASIS:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("BASIS:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    BASIS :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("BASIS:clear_keys")
     START_TIMER("BASIS:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("BASIS:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    BASIS :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("BASIS:put_table_footer")
     START_TIMER("BASIS:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("BASIS:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    BASIS :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
     STACK("BASIS:put_table_header")
     START_TIMER("BASIS:put_table_header")
     ENSURE(associated(keys),"BASIS:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_label   "); call put_(stdout,"label",int_width=TRUE)
           case ("put_n_shells"); call put_(stdout,"n_shells",int_width=TRUE)
           case ("put_n_bf    "); call put_(stdout,"n_bf",int_width=TRUE)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=TRUE)
           case ("flush       "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("BASIS:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("BASIS:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    BASIS :: self
   ! Return the table width in characters, based on "keys".  Note that not all
   ! keywords need to contribute to the banner - if a keyword is not recognised,
   ! then it is skipped.
     INT :: res
     STR(STR_SIZE) :: word
     INT :: int_dash,real_dash,k
     STACK("BASIS:table_width")
     START_TIMER("BASIS:table_width")
     ENSURE(associated(keys),"BASIS:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           "); ! exit surrounding loop
         case ("put_label   "); int_dash = int_dash + 1
         case ("put_n_shells"); int_dash = int_dash + 1
         case ("put_n_bf    "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("BASIS:table_width")
      CHECK
   end function

!  *******
!  Methods
!  *******

   function same_as(self,b) result(res)
    BASIS :: self
   ! Return TRUE if the basis set "self" is the same as "b". Only the
   ! shell vector is compared to see if they are "really" the same.
      IN :: self
      BASIS, IN :: b
      BIN :: res
      STACK("BASIS:same_as")
      START_TIMER("BASIS:same_as")
      res = same_as_(self%shell,b%shell)
     STOP_TIMER("BASIS:same_as")
      CHECK
   end function

   function contraction_matrix(self) result(ccm)
    BASIS :: self
   ! Set the contraction coefficient matrix. Its dimensions are (.n_prim,.n_bf).
      IN :: self
      REALMAT(self%n_prim,self%n_bf) :: ccm
      INT :: b,p,n_p,n_b,i
      SHELL, PTR :: sh
      STACK("BASIS:contraction_matrix")
      START_TIMER("BASIS:contraction_matrix")
      p = 0                       ! no of primitives in all previous shells
      b = 0                       ! basis number
      ccm = ZERO
      do i = 1,self%n_shell           ! For segmented basis sets !
         sh => self%shell(i)
         n_b = sh%n_comp
         n_p = n_prim_(sh)
         call make_contraction_matrix_(sh,ccm(p+1:p+n_p,b+1:b+n_b))
         b = b + n_b
         p = p + n_p
      end do
     STOP_TIMER("BASIS:contraction_matrix")
      CHECK
   end function

   subroutine unnormalise(self)
    BASIS :: self
   ! Take the normalisation factors out of the primitives, assuming
   ! that the contraction coeff's refer to normalised basis functions
      STACK("BASIS:unnormalise")
      START_TIMER("BASIS:unnormalise")
      if (associated(self%shell)) call unnormalise_(self%shell)
     STOP_TIMER("BASIS:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    BASIS :: self
   ! Put back in the normalisation factors of the primitives, assuming
   ! that the contraction coeff's refer to unnormalised basis functions
      STACK("BASIS:renormalise")
      START_TIMER("BASIS:renormalise")
      if (associated(self%shell)) call renormalise_(self%shell)
     STOP_TIMER("BASIS:renormalise")
      CHECK
   end subroutine

!  ************
!  Put routines
!  ************

   subroutine put(self)
    BASIS :: self
   ! Put out the basis information to file "stdout"
      STACK("BASIS:put")
      START_TIMER("BASIS:put")
      call flush_(stdout)
      call show_(stdout,"Basis set : ",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call put_table_(self)
     STOP_TIMER("BASIS:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    BASIS :: self
   ! Put out the basis information to file "stdout"
      SHELL, PTR :: sh
      INT :: i,j,b,p
      STACK("BASIS:put_table")
      START_TIMER("BASIS:put_table")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=2)
      call put_(stdout,"Type",int_width=TRUE)
      call put_(stdout,"Fn",int_width=TRUE)
      call put_(stdout,"Prim",int_width=TRUE)
      call put_(stdout,"Exponent")
      call put_(stdout,"Contraction")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=2)
      b = 1
      p = 1
      do i = 1,self%n_shell
         sh => self%shell(i)
         do j = 1,sh%n_cc
            if (j==1) then
               call put_(stdout,l_chr_(sh),int_width=TRUE)
               call put_(stdout,b)
            else
               call tab_(stdout,int_fields=2)
            end if
            call put_(stdout,p)
            call put_(stdout,sh%ex(j))
            call put_(stdout,sh%cc(j))
            call flush_(stdout)
            p = p + sh%n_comp
         end do
         b = b + sh%n_comp
      end do
      call dash_(stdout,int_fields=3,real_fields=2)
     STOP_TIMER("BASIS:put_table")
      CHECK
   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   PURE function no_of_shells(self) result(res)
    BASIS :: self
   ! Work out and return the number of shells in the basis set
      IN :: self
      INT :: res
   
      res = no_of_shells_(self%shell)
     STOP_TIMER("BASIS:no_of_shells")
   end function

   PURE function no_of_basis_functions(self) result(res)
    BASIS :: self
   ! Work out and return the number of basis functions in the basis set
      IN :: self
      INT :: res
   
      res = no_of_basis_functions_(self%shell)
     STOP_TIMER("BASIS:no_of_basis_functions")
   end function

   PURE function no_of_primitives(self) result(res)
    BASIS :: self
   ! Work out and return the number of primitives in the basis set
      IN :: self
      INT :: res
   
      res = no_of_primitives_(self%shell)
     STOP_TIMER("BASIS:no_of_primitives")
   end function

   PURE function min_exponent(self) result(res)
    BASIS :: self
   ! Return the minimum exponent in the basis.
     IN :: self
     REAL :: res
   
     res = min_exponent_(self%shell)
     STOP_TIMER("BASIS:min_exponent")
   end function

end
