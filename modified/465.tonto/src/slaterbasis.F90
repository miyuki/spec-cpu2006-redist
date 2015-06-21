!-------------------------------------------------------------------------------
!
! SLATERBASIS: For Slater basis sets
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
! $Id: slaterbasis.foo,v 1.1.2.5 2004/04/21 09:12:56 reaper Exp $
!
!-------------------------------------------------------------------------------

module SLATERBASIS_MODULE

#  include "slaterbasis.use"

   use SLATERSHELLVEC_MODULE, only: density_value_at_radius

   implicit none

#  include "macros"
#  include "slaterbasis.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

contains

!*******************************************************************************
!                             Create and Destroy Routines
!*******************************************************************************

   subroutine create(self)
    SLATERBASIS :: self
   ! Create an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("SLATERBASIS:create")
      START_TIMER("SLATERBASIS:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(SLATERBASIS_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SLATERBASIS:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SLATERBASIS :: self
   ! Destroy an object
      PTR :: self
   ! The following code is inherited from OBJECT
      STACK("SLATERBASIS:destroy")
      START_TIMER("SLATERBASIS:destroy")
      if (associated(self)) then
        call destroy_ptr_part_(self)
        deallocate(self)
        DELETE_MEMORY(SLATERBASIS_SIZE)
      end if
     STOP_TIMER("SLATERBASIS:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SLATERBASIS :: self
   ! Nullify the shell parts of self
      STACK("SLATERBASIS:nullify_ptr_part")
      START_TIMER("SLATERBASIS:nullify_ptr_part")
      nullify(self%shell)
      nullify(self%interpolator)
     STOP_TIMER("SLATERBASIS:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SLATERBASIS :: self
   ! Destroy the shell parts of self
      STACK("SLATERBASIS:destroy_ptr_part")
      START_TIMER("SLATERBASIS:destroy_ptr_part")
      call destroy_(self%shell)
      call destroy_(self%interpolator)
     STOP_TIMER("SLATERBASIS:destroy_ptr_part")
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
    SLATERBASIS :: self
   ! Create a copy of the basis "b".
     SLATERBASIS, IN :: b
     PTR :: self
     STACK("SLATERBASIS:create_copy")
     START_TIMER("SLATERBASIS:create_copy")
     call create_(self)
     call copy_(self,b)
     STOP_TIMER("SLATERBASIS:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,b)
    SLATERBASIS :: self
   ! Copy a basis "b" to "self". Make sure pointer parts are first
   ! destroyed or nullified, as you want.
      SLATERBASIS, IN :: b
      STACK("SLATERBASIS:copy")
      START_TIMER("SLATERBASIS:copy")
      self = b
      if (associated(b%shell)) call create_copy_(self%shell,b%shell)
      if (associated(b%interpolator)) call create_copy_(self%interpolator,b%interpolator)
     STOP_TIMER("SLATERBASIS:copy")
      UNSTACK
   end subroutine

   subroutine set_defaults(self)
    SLATERBASIS :: self
   ! Create and set up a default basis set
   ! The following code is inherited from BASIS
      STACK("SLATERBASIS:set_defaults")
      START_TIMER("SLATERBASIS:set_defaults")
      self%label   = " "
      self%n_shell = 0
      self%n_bf    = 0
      self%n_prim  = 0
     STOP_TIMER("SLATERBASIS:set_defaults")
      CHECK
   end subroutine

   subroutine update(self)
    SLATERBASIS :: self
   ! Update the shell data, if it exists
   ! The following code is inherited from BASIS
      STACK("SLATERBASIS:update")
      START_TIMER("SLATERBASIS:update")
      if (NOT associated(self%shell)) then; STOP_TIMER("SLATERBASIS:update") CHECK return; end if
      self%n_shell = no_of_shells_(self)
      self%n_bf    = no_of_basis_functions_(self)
      self%n_prim  = no_of_primitives_(self)
     STOP_TIMER("SLATERBASIS:update")
      CHECK
   end subroutine

   subroutine set_label(self,label)
    SLATERBASIS :: self
   ! Set the basis label
      STR(STR_SIZE) :: label
      STACK("SLATERBASIS:set_label")
      START_TIMER("SLATERBASIS:set_label")
      self%label = label
     STOP_TIMER("SLATERBASIS:set_label")
      CHECK
   end subroutine

   subroutine resolve_by_label(self,label,basis,clobber,found)
    SLATERBASIS :: self
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
      SLATERBASISVEC(:), PTR :: basis
      BIN, optional :: clobber,found
      INT :: b
      BINVEC(:), PTR :: check
      BIN :: fnd
   STACK("SLATERBASIS:resolve_by_label")
   START_TIMER("SLATERBASIS:resolve_by_label")
   ENSURE(associated(basis),"SLATERBASIS:resolve_by_label ... no basis set")
      if (present(clobber)) then
      if (NOT clobber) then
      if (associated(self)) then
      if (self%label/=" ") then
         if (present(found)) found = TRUE
         STOP_TIMER("SLATERBASIS:resolve_by_label") CHECK return
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
      else; ENSURE(fnd,"SLATERBASIS:resolve_by_label ... unknown basis label, "// trim(label))
      end if
     STOP_TIMER("SLATERBASIS:resolve_by_label")
      CHECK
   end subroutine

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    SLATERBASIS :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("SLATERBASIS:read_keywords")
     START_TIMER("SLATERBASIS:read_keywords")
     ENSURE(next_item_(stdin)=="{","SLATERBASIS:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("SLATERBASIS:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SLATERBASIS :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("SLATERBASIS:process_keyword")
     START_TIMER("SLATERBASIS:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("-- Regular options --   ")
       case ("}                       "); ! exit surrounding loop
       case ("analyse_configuration   "); call analyse_configuration_(self)
       case ("configuration=          "); call read_configuration_(self)
       case ("label=                  "); call read_label_(self)
       case ("put                     "); call put_(self)
       case ("shells=                 "); call read_shell_(self)
       case ("tonto-style=            "); call read_tonto_style_(self)
       case ("units=                  "); call read_units_(self)
       ! These are only for making custom tables for the list type
       case ("-- Options for tables --")
       case ("flush                   "); call flush_(stdout)
       case ("put_label               "); call put_(stdout,self%label,int_width=TRUE)
       case ("put_n_shells            "); call put_(stdout,self%n_shell)
       case ("put_n_bf                "); call put_(stdout,self%n_bf)
       case ("put_n_prim              "); call put_(stdout,self%n_prim)
       case  default ;      allocate(tonto%known_keywords(15))
       tonto%known_keywords(1) = "-- Regular options --   "
       tonto%known_keywords(2) = "}                       "
       tonto%known_keywords(3) = "analyse_configuration   "
       tonto%known_keywords(4) = "configuration=          "
       tonto%known_keywords(5) = "label=                  "
       tonto%known_keywords(6) = "put                     "
       tonto%known_keywords(7) = "shells=                 "
       tonto%known_keywords(8) = "tonto-style=            "
       tonto%known_keywords(9) = "units=                  "
       tonto%known_keywords(10) = "-- Options for tables --"
       tonto%known_keywords(11) = "flush                   "
       tonto%known_keywords(12) = "put_label               "
       tonto%known_keywords(13) = "put_n_shells            "
       tonto%known_keywords(14) = "put_n_bf                "
       tonto%known_keywords(15) = "put_n_prim              "
       call unknown_(tonto,word,"SLATERBASIS:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("SLATERBASIS:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    SLATERBASIS :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("SLATERBASIS:read_units")
      START_TIMER("SLATERBASIS:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("SLATERBASIS:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    SLATERBASIS :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("SLATERBASIS:read_junk")
      START_TIMER("SLATERBASIS:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("SLATERBASIS:read_junk")
      CHECK
   end subroutine

   subroutine read_label(self)
    SLATERBASIS :: self
   ! Read only the basis label
   ! The following code is inherited from BASIS
      STACK("SLATERBASIS:read_label")
      START_TIMER("SLATERBASIS:read_label")
      call read_(stdin,self%label)
     STOP_TIMER("SLATERBASIS:read_label")
      CHECK
   end subroutine

   subroutine read_shell(self)
    SLATERBASIS :: self
   ! Read a shell
      STACK("SLATERBASIS:read_shell")
      START_TIMER("SLATERBASIS:read_shell")
      call read_list_keywords_(self%shell)
      call update_(self)
   ! The following code is inherited from BASIS
     STOP_TIMER("SLATERBASIS:read_shell")
      UNSTACK
   end subroutine

   subroutine read_configuration(self)
    SLATERBASIS :: self
   ! Read in the configuration string
      STACK("SLATERBASIS:read_configuration")
      START_TIMER("SLATERBASIS:read_configuration")
      call read_(stdin,self%configuration)
     STOP_TIMER("SLATERBASIS:read_configuration")
      CHECK
   end subroutine

   subroutine read_tonto_style(self)
    SLATERBASIS :: self
   ! Create and read a tonto style basis set
      STRVEC(STR_SIZE,:), PTR :: the_keys
      STACK("SLATERBASIS:read_tonto_style")
      START_TIMER("SLATERBASIS:read_tonto_style")
      call read_label_(self)
      call read_configuration_(self)
      the_keys => split_(("l_chr= kind= n,z,c*="))
      call set_keys_(self%shell,the_keys)
      call destroy_(the_keys)
      call read_data_(self%shell)
      call update_(self)
     STOP_TIMER("SLATERBASIS:read_tonto_style")
      UNSTACK
   end subroutine

   subroutine analyse_configuration(self)
    SLATERBASIS :: self
   ! Analyse the orbital configuration and set the shell occupancies
      STR(STR_SIZE) :: configuration,conf_kind
      STRVEC(STR_SIZE,:), PTR :: split,conf
      BINVEC(:), PTR :: match
      BIN :: found,keep
      INT :: i,j,s,occ,ind
   STACK("SLATERBASIS:analyse_configuration")
   START_TIMER("SLATERBASIS:analyse_configuration")
   ENSURE(self%configuration/=" ","SLATERBASIS:analyse_configuration ... no configuration")
   ENSURE(associated(self%shell),"SLATERBASIS:analyse_configuration ... no shells")
   ENSURE(shell_kinds_created_(self),"SLATERBASIS:analyse_configuration ... not all orbital kinds are there")
      ! Split configuration into separate fields
      configuration = self%configuration
      call replace_(configuration,"("," ")
      call replace_(configuration,")"," ")
      split => split_(configuration)
      i = 1
      do ! loop over shell/orbital configurations
         if (i>size(split)) exit
         ! Now get orbital configurations only
         keep = FALSE
         select case (split(i))
            case ("K");   conf => split_(("1S 2"))
            case ("L");   conf => split_(("2S 2 2P 6"))
            case ("M");   conf => split_(("3S 2 3P 6 3D 10"))
            case default; conf => split(i:i+1); keep = TRUE
         end select
         j = 1
         do ! Loop over orbital configurations 
            if (j>size(conf)) exit
            conf_kind = conf(j)
            occ  = to_int_(conf(j+1))
            ! Now find the shell with the correct orbital kind
            if (occ>0) then
            found = FALSE 
            do s = 1,size(self%shell)
               if (all(self%shell(s)%orb_kind/=conf_kind)) cycle
               call create_(match,self%shell(s)%n_orb)
               match = self%shell(s)%orb_kind==conf_kind
               ind = index_of_first_true_element_(match)
               call destroy_(match)
               ENSURE(associated(self%shell(s)%occupancy),"SLATERBASIS:analyse_configuration ... occupancies have not been created")
               self%shell(s)%occupancy(ind) = occ
               found = TRUE
               exit
            end do
            ENSURE(found,"SLATERBASIS:analyse_configuration ... orbital kind "//trim(conf_kind)//" not found")
            end if
            j = j + 2
         end do
         if (NOT keep) call destroy_(conf)
         i = i + 2
      end do
      call destroy_(split)
     STOP_TIMER("SLATERBASIS:analyse_configuration")
      CHECK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    SLATERBASIS :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("SLATERBASIS:read_keys")
     START_TIMER("SLATERBASIS:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("SLATERBASIS:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    SLATERBASIS :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("SLATERBASIS:process_keys")
      START_TIMER("SLATERBASIS:process_keys")
      ENSURE(associated(keys),"SLATERBASIS:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"SLATERBASIS:process_keys ... no matching closing brace, }")
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
     STOP_TIMER("SLATERBASIS:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    SLATERBASIS :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("SLATERBASIS:keys_created")
      START_TIMER("SLATERBASIS:keys_created")
      res = associated(keys)
     STOP_TIMER("SLATERBASIS:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    SLATERBASIS :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("SLATERBASIS:set_keys")
     START_TIMER("SLATERBASIS:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("SLATERBASIS:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    SLATERBASIS :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("SLATERBASIS:clear_keys")
     START_TIMER("SLATERBASIS:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("SLATERBASIS:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    SLATERBASIS :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("SLATERBASIS:put_table_footer")
     START_TIMER("SLATERBASIS:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("SLATERBASIS:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    SLATERBASIS :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k
     STACK("SLATERBASIS:put_table_header")
     START_TIMER("SLATERBASIS:put_table_header")
     ENSURE(associated(keys),"SLATERBASIS:put_table_header ... no keys")
     width = table_width_(self)
     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("put_configuration"); call put_(stdout,"Config.")
           case ("put_label        "); call put_(stdout,"label",int_width=TRUE)
           case ("put_n_shells     "); call put_(stdout,"n_shells",int_width=TRUE)
           case ("put_n_bf         "); call put_(stdout,"n_bf",int_width=TRUE)
           case ("put_n_prim       "); call put_(stdout,"n_prim",int_width=TRUE)
           case ("flush            "); call flush_(stdout); exit
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("SLATERBASIS:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("SLATERBASIS:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    SLATERBASIS :: self
   ! Return the table width in characters, based on "keys".  Note that not all
   ! keywords need to contribute to the banner - if a keyword is not recognised,
   ! then it is skipped.
     INT :: res
     INT :: int_dash,real_dash,k
     STR(STR_SIZE) :: word
     STACK("SLATERBASIS:table_width")
     START_TIMER("SLATERBASIS:table_width")
     ENSURE(associated(keys),"SLATERBASIS:table_width ... no keys")
     int_dash = 0
     real_dash = 0
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}                "); ! exit surrounding loop
         case ("put_configuration"); real_dash = real_dash + 1
         case ("put_label        "); int_dash = int_dash + 1
         case ("put_n_shells     "); int_dash = int_dash + 1
         case ("put_n_bf         "); int_dash = int_dash + 1
         case ("put_n_prim       "); int_dash = int_dash + 1
         case ("flush            "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("SLATERBASIS:table_width")
      CHECK
   end function

!  ***************
!  Inquiry methods
!  ***************

   function same_as(self,b) result(res)
    SLATERBASIS :: self
   ! Return TRUE if the basis set "self" is the same as "b". Only the
   ! shell vector is compared to see if they are "really" the same.
      IN :: self
      SLATERBASIS, IN :: b
      BIN :: res
   ! The following code is inherited from BASIS
      STACK("SLATERBASIS:same_as")
      START_TIMER("SLATERBASIS:same_as")
      res = same_as_(self%shell,b%shell)
     STOP_TIMER("SLATERBASIS:same_as")
      CHECK
   end function

   function max_n_orb(self) result(res)
    SLATERBASIS :: self
   ! Return the maximum number of contracted orbitals in any one shell.
      IN :: self
      INT :: res
      STACK("SLATERBASIS:max_n_orb")
      START_TIMER("SLATERBASIS:max_n_orb")
      res = maxval(self%shell%n_orb)
     STOP_TIMER("SLATERBASIS:max_n_orb")
      CHECK
   end function

   PURE function no_of_shells(self) result(res)
    SLATERBASIS :: self
   ! Work out and return the number of shells in the basis set
      IN :: self
      INT :: res
   ! The following code is inherited from BASIS
   
      res = no_of_shells_(self%shell)
     STOP_TIMER("SLATERBASIS:no_of_shells")
   end function

   PURE function no_of_basis_functions(self) result(res)
    SLATERBASIS :: self
   ! Work out and return the number of basis functions in the basis set
      IN :: self
      INT :: res
   ! The following code is inherited from BASIS
   
      res = no_of_basis_functions_(self%shell)
     STOP_TIMER("SLATERBASIS:no_of_basis_functions")
   end function

   PURE function no_of_primitives(self) result(res)
    SLATERBASIS :: self
   ! Work out and return the number of primitives in the basis set
      IN :: self
      INT :: res
   ! The following code is inherited from BASIS
   
      res = no_of_primitives_(self%shell)
     STOP_TIMER("SLATERBASIS:no_of_primitives")
   end function

   PURE function min_exponent(self) result(res)
    SLATERBASIS :: self
   ! Return the minimum exponent in the basis.
     IN :: self
     REAL :: res
   ! The following code is inherited from BASIS
   
     res = min_exponent_(self%shell)
     STOP_TIMER("SLATERBASIS:min_exponent")
   end function

   function shell_kinds_created(self) result(res)
    SLATERBASIS :: self
   ! Return TRUE if all the .shell.orb_kind vectors are there.
      IN :: self
      BIN :: res
      INT :: i
      STACK("SLATERBASIS:shell_kinds_created")
      START_TIMER("SLATERBASIS:shell_kinds_created")
      if (NOT associated(self%shell)) then
         res = FALSE
      else
         res = TRUE
         do i = 1,size(self%shell)
            if (associated(self%shell(i)%orb_kind)) cycle
            res = FALSE
            exit
         end do
      end if
     STOP_TIMER("SLATERBASIS:shell_kinds_created")
      UNSTACK
   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    SLATERBASIS :: self
   ! Put out the basis information to file "stdout"
      STACK("SLATERBASIS:put")
      START_TIMER("SLATERBASIS:put")
      call flush_(stdout)
      call show_(stdout,"Slater basis set : ",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"Configuration          =",self%configuration)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      if (associated(self%shell)) call put_(self%shell)
     STOP_TIMER("SLATERBASIS:put")
      CHECK
   end subroutine

!  ******************
!  Density evaluation
!  ******************

   subroutine make_density_grid(self,density_grid,pt,pos)
    SLATERBASIS :: self
   ! Work out the electron "density_grid" on a set of points "pt", assuming the
   ! orbitals are at position "pos".
      IN :: self
      REALVEC(:), OUT :: density_grid
      REALMAT(:,:), IN :: pt
      REALVEC(3), IN :: pos
   STACK("SLATERBASIS:make_density_grid")
   START_TIMER("SLATERBASIS:make_density_grid")
   ENSURE(size(density_grid)==size(pt,1),"SLATERBASIS:make_density_grid ... inconsistent number of points")
      if (associated(self%interpolator)) then
         call make_interpolated_density_grid_(self,density_grid,pt,pos)
      else
         call make_normal_density_grid_(self,density_grid,pt,pos)
      end if
     STOP_TIMER("SLATERBASIS:make_density_grid")
      CHECK
   end subroutine

   subroutine make_normal_density_grid(self,density_grid,pt,pos)
    SLATERBASIS :: self
   ! Make the normal (uninterpolated) "density_grid" for the supplied points
   ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
   ! the orbital is at position "pos".
     IN :: self
     REALVEC(:), OUT :: density_grid
     REALMAT(:,:), IN :: pt
     REALVEC(3), IN :: pos
     REALVEC(:), PTR :: R
     INT :: n_pt,n
     REAL :: x, y, z
   STACK("SLATERBASIS:make_normal_density_grid")
   START_TIMER("SLATERBASIS:make_normal_density_grid")
   ENSURE(size(pt,2)==3,"SLATERBASIS:make_normal_density_grid ... wrong dimension for points array")
   ENSURE(size(density_grid)==size(pt,1),"SLATERBASIS:make_normal_density_grid ... inconsistent number of points")
   ENSURE(associated(self%shell),"SLATERBASIS:make_normal_density_grid ... no shell vector")
     n_pt = size(pt,1)
     call create_(R,n_pt)
     do n = 1,n_pt
        x = pt(n,1) - pos(1)
        y = pt(n,2) - pos(2)
        z = pt(n,3) - pos(3)
        R(n) = sqrt(x*x + y*y + z*z)
     end do
     ! Now get the density values
     density_grid = densities_at_radii_(self%shell,R)
     call destroy_(R)
     STOP_TIMER("SLATERBASIS:make_normal_density_grid")
      CHECK
   end subroutine

   subroutine make_interpolated_density_grid(self,density_grid,pt,pos)
    SLATERBASIS :: self
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
   STACK("SLATERBASIS:make_interpolated_density_grid")
   START_TIMER("SLATERBASIS:make_interpolated_density_grid")
   ENSURE(size(pt,2)==3,"SLATERBASIS:make_interpolated_density_grid ... wrong dimension for points array")
   ENSURE(size(density_grid)==size(pt,1),"SLATERBASIS:make_interpolated_density_grid ... inconsistent number of points")
   ENSURE(associated(self%interpolator),"SLATERBASIS:make_interpolated_density_grid ... no interpolator defined!")
     n_pt = size(pt,1)
     call create_(R,n_pt)
     do n = 1,n_pt
        x = pt(n,1) - pos(1)
        y = pt(n,2) - pos(2)
        z = pt(n,3) - pos(3)
        R(n) = sqrt(x*x + y*y + z*z)
     end do
     ! Now get the interpolated density values
     density_grid = values_for_(self%interpolator,R)
     call destroy_(R)
     STOP_TIMER("SLATERBASIS:make_interpolated_density_grid")
      CHECK
   end subroutine

   subroutine make_interpolator(self)
    SLATERBASIS :: self
   ! Make the interpolator for the coppens atom density
   STACK("SLATERBASIS:make_interpolator")
   START_TIMER("SLATERBASIS:make_interpolator")
   ENSURE(associated(self%shell),"SLATERBASIS:make_interpolator ... no coppens orbitals defined!")
     call destroy_(self%interpolator)
     call create_(self%interpolator)
     self%interpolator%interp_kind = "logarithmic"
     call set_saved_self_(self%shell) ! Used by function below
#ifdef NOGENERIC
     call set_even_spaced_data_(self%interpolator,first=ZERO,spacing=0.05d0,length=20.0d0, &
        func=SLATERSHELLVEC_density_value_at_radius,tol=TOL(9))
#else
     call set_even_spaced_data_(self%interpolator,first=ZERO,spacing=0.05d0,length=20.0d0, &
        func=density_value_at_radius,tol=TOL(9))
#endif
     STOP_TIMER("SLATERBASIS:make_interpolator")
      UNSTACK
   end subroutine

end
