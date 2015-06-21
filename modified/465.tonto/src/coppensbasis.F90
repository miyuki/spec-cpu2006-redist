!-------------------------------------------------------------------------------
!
! COPPENSBASIS: For a Coppens style fitted relativistic atomic orbital basis.
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
! $Id: coppensbasis.foo,v 1.2.2.13 2004/04/21 09:12:54 reaper Exp $
!
!-------------------------------------------------------------------------------

module COPPENSBASIS_MODULE

#  include "coppensbasis.use"

   use COPPENSORBITALVEC_MODULE, only: density_value_at_radius

   implicit none

#  include "macros"
#  include "coppensbasis.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

contains

!****************************
! Create and Destroy Routines
!****************************

   subroutine create(self)
    COPPENSBASIS :: self
   ! Create an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("COPPENSBASIS:create")
      START_TIMER("COPPENSBASIS:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(COPPENSBASIS_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("COPPENSBASIS:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    COPPENSBASIS :: self
   ! Destroy an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("COPPENSBASIS:destroy")
      START_TIMER("COPPENSBASIS:destroy")
      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)
        DELETE_MEMORY(COPPENSBASIS_SIZE)
      end if
     STOP_TIMER("COPPENSBASIS:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    COPPENSBASIS :: self
   ! Nullify the pointer parts of self
      STACK("COPPENSBASIS:nullify_ptr_part")
      START_TIMER("COPPENSBASIS:nullify_ptr_part")
      nullify(self%orbital)
      nullify(self%interpolator)
     STOP_TIMER("COPPENSBASIS:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    COPPENSBASIS :: self
   ! Destroy the pointer parts of self
      STACK("COPPENSBASIS:destroy_ptr_part")
      START_TIMER("COPPENSBASIS:destroy_ptr_part")
      call destroy_(self%orbital)
      call destroy_(self%interpolator)
     STOP_TIMER("COPPENSBASIS:destroy_ptr_part")
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
    COPPENSBASIS :: self
   ! Create a copy of the basis "b".
     COPPENSBASIS, IN :: b
     PTR :: self
     STACK("COPPENSBASIS:create_copy")
     START_TIMER("COPPENSBASIS:create_copy")
     call create_(self)
     call copy_(self,b)
     STOP_TIMER("COPPENSBASIS:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,b)
    COPPENSBASIS :: self
   ! Copy a basis "b" to "self". Make sure pointer parts of self are first
   ! destroyed or nullified, as you want.
      COPPENSBASIS, IN :: b
      STACK("COPPENSBASIS:copy")
      START_TIMER("COPPENSBASIS:copy")
      self = b
      if (associated(b%orbital)) call create_copy_(self%orbital,b%orbital)
      if (associated(b%interpolator)) call create_copy_(self%interpolator,b%interpolator)
     STOP_TIMER("COPPENSBASIS:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    COPPENSBASIS :: self
   ! Create and set up a default basis set
      STACK("COPPENSBASIS:set_defaults")
      START_TIMER("COPPENSBASIS:set_defaults")
      self%label   = "?"
      self%n_orb   = 0
      self%n_prim  = 0
     STOP_TIMER("COPPENSBASIS:set_defaults")
      CHECK
   end subroutine

   subroutine update(self)
    COPPENSBASIS :: self
   ! Update the shell data, if it exists
      STACK("COPPENSBASIS:update")
      START_TIMER("COPPENSBASIS:update")
      if (NOT associated(self%orbital)) then; STOP_TIMER("COPPENSBASIS:update") CHECK return; end if
      self%n_orb   = no_of_orbitals_(self)
      self%n_prim  = no_of_primitives_(self)
     STOP_TIMER("COPPENSBASIS:update")
      CHECK
   end subroutine

   subroutine set_label(self,label)
    COPPENSBASIS :: self
   ! Set the basis label
      STR(STR_SIZE) :: label
      STACK("COPPENSBASIS:set_label")
      START_TIMER("COPPENSBASIS:set_label")
      self%label = label
     STOP_TIMER("COPPENSBASIS:set_label")
      CHECK
   end subroutine

   subroutine resolve_by_label(self,label,basis,clobber,found)
    COPPENSBASIS :: self
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
      COPPENSBASISVEC(:), PTR :: basis
      BIN, optional :: clobber,found
      INT :: b
      BINVEC(:), PTR :: check
      BIN :: fnd
   STACK("COPPENSBASIS:resolve_by_label")
   START_TIMER("COPPENSBASIS:resolve_by_label")
   ENSURE(associated(basis),"COPPENSBASIS:resolve_by_label ... no basis set")
      if (present(clobber)) then
      if (NOT clobber) then
      if (associated(self)) then
      if (self%label/=" ") then
         if (present(found)) found = TRUE
         STOP_TIMER("COPPENSBASIS:resolve_by_label") CHECK return
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
      else; ENSURE(fnd,"COPPENSBASIS:resolve_by_label ... unknown basis label, "// trim(label))
      end if
     STOP_TIMER("COPPENSBASIS:resolve_by_label")
      CHECK
   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    COPPENSBASIS :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("COPPENSBASIS:read_keywords")
     START_TIMER("COPPENSBASIS:read_keywords")
     ENSURE(next_item_(stdin)=="{","COPPENSBASIS:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("COPPENSBASIS:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    COPPENSBASIS :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("COPPENSBASIS:process_keyword")
     START_TIMER("COPPENSBASIS:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}            "); ! exit surrounding loop
       case ("label=       "); call read_label_(self)
       case ("orbitals=    "); call read_orbitals_(self)
       case ("put          "); call put_(self)
       case ("put_table    "); call put_table_(self)
       case ("tonto-style= "); call read_tonto_style_(self)
       case ("units=       "); call read_units_(self)
       ! These are only for making custom tables for the list type
       case ("put_label    "); call put_(stdout,self%label,int_width=TRUE)
       case ("put_n_orb    "); call put_(stdout,self%n_orb)
       case ("put_n_prim   "); call put_(stdout,self%n_prim)
       case ("flush        "); call flush_(stdout)
       case  default ;      allocate(tonto%known_keywords(11))
       tonto%known_keywords(1) = "}            "
       tonto%known_keywords(2) = "label=       "
       tonto%known_keywords(3) = "orbitals=    "
       tonto%known_keywords(4) = "put          "
       tonto%known_keywords(5) = "put_table    "
       tonto%known_keywords(6) = "tonto-style= "
       tonto%known_keywords(7) = "units=       "
       tonto%known_keywords(8) = "put_label    "
       tonto%known_keywords(9) = "put_n_orb    "
       tonto%known_keywords(10) = "put_n_prim   "
       tonto%known_keywords(11) = "flush        "
       call unknown_(tonto,word,"COPPENSBASIS:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("COPPENSBASIS:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    COPPENSBASIS :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("COPPENSBASIS:read_units")
      START_TIMER("COPPENSBASIS:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("COPPENSBASIS:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    COPPENSBASIS :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("COPPENSBASIS:read_junk")
      START_TIMER("COPPENSBASIS:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("COPPENSBASIS:read_junk")
      CHECK
   end subroutine

   subroutine read_label(self)
    COPPENSBASIS :: self
   ! Read only the basis label
      STACK("COPPENSBASIS:read_label")
      START_TIMER("COPPENSBASIS:read_label")
      call read_(stdin,self%label)
     STOP_TIMER("COPPENSBASIS:read_label")
      CHECK
   end subroutine

   subroutine read_orbitals(self)
    COPPENSBASIS :: self
   ! Read a list of orbitals
      STACK("COPPENSBASIS:read_orbitals")
      START_TIMER("COPPENSBASIS:read_orbitals")
      call read_list_keywords_(self%orbital)
      call update_(self)
     STOP_TIMER("COPPENSBASIS:read_orbitals")
      UNSTACK
   end subroutine

   subroutine read_tonto_style(self)
    COPPENSBASIS :: self
   ! Create and read a tonto style basis set
      STACK("COPPENSBASIS:read_tonto_style")
      START_TIMER("COPPENSBASIS:read_tonto_style")
      call read_label_(self)
      call set_keys_(self%orbital,(/"kind= ","occ=  ","n_fun=","n,c,z="/))
      call read_data_(self%orbital)
      call update_(self)
     STOP_TIMER("COPPENSBASIS:read_tonto_style")
      UNSTACK
   end subroutine

   subroutine put(self)
    COPPENSBASIS :: self
   ! Put out the basis information to file "stdout"
      STACK("COPPENSBASIS:put")
      START_TIMER("COPPENSBASIS:put")
      call flush_(stdout)
      call show_(stdout,"Coppens basis set : ",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"No. of orbitals        =",self%n_orb)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call put_table_(self)
     STOP_TIMER("COPPENSBASIS:put")
      CHECK
   end subroutine

   subroutine put_table(self)
    COPPENSBASIS :: self
   ! Put out the basis information to file "stdout"
      INT :: i,j
      STACK("COPPENSBASIS:put_table")
      START_TIMER("COPPENSBASIS:put_table")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      call put_(stdout,"Kind",int_width=TRUE)
      call put_(stdout,"Orb #",int_width=TRUE)
      call put_(stdout,"Prim",int_width=TRUE)
      call put_(stdout,"N",int_width=TRUE)
      call put_(stdout,"Exponent")
      call put_(stdout,"Coeff")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=3)
      do i = 1,self%n_orb
         do j = 1,self%orbital(i)%n_fun
            if (j==1) then
               call put_(stdout,self%orbital(i)%orb_kind,int_width=TRUE)
               call put_(stdout,i)
            else
               call tab_(stdout,int_fields=2)
            end if
            call put_(stdout,j)
            call put_(stdout,self%orbital(i)%n(j))
            call put_(stdout,self%orbital(i)%z(j))
            call put_(stdout,self%orbital(i)%c(j))
            call flush_(stdout)
         end do
      end do
      call dash_(stdout,int_fields=3,real_fields=3)
     STOP_TIMER("COPPENSBASIS:put_table")
      CHECK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    COPPENSBASIS :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("COPPENSBASIS:read_keys")
     START_TIMER("COPPENSBASIS:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("COPPENSBASIS:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    COPPENSBASIS :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("COPPENSBASIS:process_keys")
      START_TIMER("COPPENSBASIS:process_keys")
      ENSURE(associated(keys),"COPPENSBASIS:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"COPPENSBASIS:process_keys ... no matching closing brace, }")
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
     STOP_TIMER("COPPENSBASIS:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    COPPENSBASIS :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("COPPENSBASIS:keys_created")
      START_TIMER("COPPENSBASIS:keys_created")
      res = associated(keys)
     STOP_TIMER("COPPENSBASIS:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    COPPENSBASIS :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("COPPENSBASIS:set_keys")
     START_TIMER("COPPENSBASIS:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("COPPENSBASIS:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    COPPENSBASIS :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("COPPENSBASIS:clear_keys")
     START_TIMER("COPPENSBASIS:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("COPPENSBASIS:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    COPPENSBASIS :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("COPPENSBASIS:put_table_footer")
     START_TIMER("COPPENSBASIS:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("COPPENSBASIS:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    COPPENSBASIS :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
   STACK("COPPENSBASIS:put_table_header")
   START_TIMER("COPPENSBASIS:put_table_header")
   ENSURE(associated(keys),"COPPENSBASIS:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_label   "); call put_(stdout,"label",int_width=TRUE)
           case ("put_n_orb   "); call put_(stdout,"n_orb",int_width=TRUE)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=TRUE)
           case ("flush       "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("COPPENSBASIS:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("COPPENSBASIS:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    COPPENSBASIS :: self
   ! Return the table width in characters, based on "keys".  Note that not all
   ! keywords need to contribute to the banner - if a keyword is not recognised,
   ! then it is skipped.
     INT :: res
     STR(STR_SIZE) :: word
     INT :: int_dash,real_dash,k
     STACK("COPPENSBASIS:table_width")
     START_TIMER("COPPENSBASIS:table_width")
     ENSURE(associated(keys),"COPPENSBASIS:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           "); ! exit surrounding loop
         case ("put_label   "); int_dash = int_dash + 1
         case ("put_n_orb   "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("COPPENSBASIS:table_width")
      CHECK
   end function

!  *******
!  Methods
!  *******

   function same_as(self,b) result(res)
    COPPENSBASIS :: self
   ! Return TRUE if the basis set "self" is the same as "b". Only the
   ! orbital vector is compared to see if they are "really" the same.
      IN :: self
      COPPENSBASIS, IN :: b
      BIN :: res
      STACK("COPPENSBASIS:same_as")
      START_TIMER("COPPENSBASIS:same_as")
      res = same_as_(self%orbital,b%orbital)
     STOP_TIMER("COPPENSBASIS:same_as")
      CHECK
   end function

   PURE function no_of_orbitals(self) result(res)
    COPPENSBASIS :: self
   ! Work out and return the number of orbitals in the basis set
      IN :: self
      INT :: res
      if (associated(self%orbital)) then; res = size(self%orbital)
      else;                       res = 0
      end if
     STOP_TIMER("COPPENSBASIS:no_of_orbitals")
   end function

   PURE function no_of_primitives(self) result(res)
    COPPENSBASIS :: self
   ! Work out and return the number of primitive fitting functions in the basis ! set
      IN :: self
      INT :: res
      INT :: i
      res = 0
      if (NOT associated(self%orbital)) then; STOP_TIMER("COPPENSBASIS:no_of_primitives") return; end if
      do i = 1,no_of_orbitals_(self)
         res = res + self%orbital(i)%n_fun
      end do
     STOP_TIMER("COPPENSBASIS:no_of_primitives")
   end function

   function maximum_basis_n_value(self) result(res)
    COPPENSBASIS :: self
   ! Returns the maximum n value over all orbitals in every basis set of the
   ! vector
     IN :: self
     INT :: res
     STACK("COPPENSBASIS:maximum_basis_n_value")
     START_TIMER("COPPENSBASIS:maximum_basis_n_value")
     if (associated(self%orbital)) then; res = maximum_orbital_n_value_(self%orbital)
     else;                       res = 0
     end if
     STOP_TIMER("COPPENSBASIS:maximum_basis_n_value")
      CHECK
   end function

   PURE function min_exponent(self) result(res)
    COPPENSBASIS :: self
   ! Return the minimum exponent in the basis.
     IN :: self
     REAL :: res
     REAL :: tmp
     INT :: i
     
     res = ZERO
     do i= 1,self%n_orb
       tmp = minval(self%orbital(i)%z)
       if (tmp < res) res = tmp
     end do
     STOP_TIMER("COPPENSBASIS:min_exponent")
   end function

!  ******************
!  Density evaluation
!  ******************

   subroutine make_density_grid(self,density_grid,pt,pos)
    COPPENSBASIS :: self
   ! Work out the electron "density_grid" on a set of points "pt", assuming the
   ! orbitals are at position "pos".
      IN :: self
      REALVEC(:), OUT :: density_grid
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
   STACK("COPPENSBASIS:make_density_grid")
   START_TIMER("COPPENSBASIS:make_density_grid")
   ENSURE(size(density_grid)==size(pt,1),"COPPENSBASIS:make_density_grid ... inconsistent number of points")
      if (associated(self%interpolator)) then
         call make_interpolated_density_grid_(self,density_grid,pt,pos)
      else
         call make_normal_density_grid_(self,density_grid,pt,pos)
      end if
     STOP_TIMER("COPPENSBASIS:make_density_grid")
      CHECK
   end subroutine

   subroutine make_normal_density_grid(self,density_grid,pt,pos)
    COPPENSBASIS :: self
   ! Make the normal (uninterpolated) "density_grid" for the supplied points
   ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
   ! the orbital is at position "pos".
     IN :: self
     REALVEC(:), OUT :: density_grid
     REALMAT(:,:), IN :: pt
     REALVEC(3), IN :: pos
     REALVEC(:), PTR :: ORB
     INT :: n
   STACK("COPPENSBASIS:make_normal_density_grid")
   START_TIMER("COPPENSBASIS:make_normal_density_grid")
   ENSURE(size(pt,2)==3,"COPPENSBASIS:make_normal_density_grid ... wrong dimension for points array")
   ENSURE(size(density_grid)==size(pt,1),"COPPENSBASIS:make_normal_density_grid ... inconsistent number of points")
   ENSURE(associated(self%orbital),"COPPENSBASIS:make_normal_density_grid ... no orbital vector")
     call create_(ORB,size(pt,1))
     density_grid = ZERO
     do n = 1,size(self%orbital)
        call make_values_at_opt_(self%orbital(n),pt,pos,ORB)
        density_grid = density_grid  + self%orbital(n)%occupancy*ORB*ORB
     end do
     call destroy_(ORB)
     STOP_TIMER("COPPENSBASIS:make_normal_density_grid")
      CHECK
   end subroutine

   subroutine make_interpolated_density_grid(self,density_grid,pt,pos)
    COPPENSBASIS :: self
   ! Make the interpolated "density_grid" for the supplied points "pt" from the real
   ! slater atomic orbitals, as fitted by coppens, assuming the orbitals are at
   ! position "pos".
     IN :: self
     REALVEC(:), OUT :: density_grid
     REALMAT(:,:), IN :: pt
     REALVEC(3), IN :: pos
     REALVEC(:), PTR :: R
     INT :: n_pt,n
     REAL :: x, y, z
   STACK("COPPENSBASIS:make_interpolated_density_grid")
   START_TIMER("COPPENSBASIS:make_interpolated_density_grid")
   ENSURE(size(pt,2)==3,"COPPENSBASIS:make_interpolated_density_grid ... wrong dimension for points array")
   ENSURE(size(density_grid)==size(pt,1),"COPPENSBASIS:make_interpolated_density_grid ... inconsistent number of points")
   ENSURE(associated(self%interpolator),"COPPENSBASIS:make_interpolated_density_grid ... no coppens interpolator defined!")
     n_pt = size(pt,1)
     call create_(R,n_pt)
     do n = 1,n_pt
        x = pt(n,1) - pos(1)
        y = pt(n,2) - pos(2)
        z = pt(n,3) - pos(3)
        R(n) = sqrt(x*x + y*y + z*z)
     end do
     ! Now get the interpolated values
     density_grid = values_for_(self%interpolator,R)
     call destroy_(R)
     STOP_TIMER("COPPENSBASIS:make_interpolated_density_grid")
      UNSTACK
   end subroutine

   function density_at_radius(self,R) result(res)
    COPPENSBASIS :: self
   ! Work out the electron density at radius "R" from the orbitals.
      REAL :: R,res
      STACK("COPPENSBASIS:density_at_radius")
      START_TIMER("COPPENSBASIS:density_at_radius")
      if (associated(self%interpolator)) then
         res = value_for_(self%interpolator,R)
      else
         res = density_at_radius_(self%orbital,R)
      end if
     STOP_TIMER("COPPENSBASIS:density_at_radius")
      CHECK
   end function

   subroutine make_interpolator(self)
    COPPENSBASIS :: self
   ! Make the interpolator for the coppens atom density
   STACK("COPPENSBASIS:make_interpolator")
   START_TIMER("COPPENSBASIS:make_interpolator")
   ENSURE(associated(self%orbital),"COPPENSBASIS:make_interpolator ... no coppens orbitals defined!")
     call destroy_(self%interpolator)
     call create_(self%interpolator)
     self%interpolator%interp_kind = "logarithmic"
     call set_saved_self_(self%orbital) ! Used by function below
#ifdef NOGENERIC
     call set_even_spaced_data_(self%interpolator,first=ZERO,spacing=0.05d0,length=20.0d0, &
        func=COPPENSORBITALVEC_density_value_at_radius,tol=TOL(9))
#else
     call set_even_spaced_data_(self%interpolator,first=ZERO,spacing=0.05d0,length=20.0d0, &
        func=density_value_at_radius,tol=TOL(9))
#endif
     STOP_TIMER("COPPENSBASIS:make_interpolator")
      UNSTACK
   end subroutine

   subroutine unnormalise(self)
    COPPENSBASIS :: self
   ! Set the value of the orbital coefficients to correspond to un-normalised
   ! Slater functions -- assuming they are normalised. This saves computation.
      STACK("COPPENSBASIS:unnormalise")
      START_TIMER("COPPENSBASIS:unnormalise")
      if (associated(self%orbital)) call unnormalise_(self%orbital)
     STOP_TIMER("COPPENSBASIS:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    COPPENSBASIS :: self
   ! Set the value of the orbitals coefficients to correspond to normalised
   ! Slater functions --- assuming they are w.r.t. unnormalised functions. 
      STACK("COPPENSBASIS:renormalise")
      START_TIMER("COPPENSBASIS:renormalise")
      if (associated(self%orbital)) call renormalise_(self%orbital)
     STOP_TIMER("COPPENSBASIS:renormalise")
      CHECK
   end subroutine

end
