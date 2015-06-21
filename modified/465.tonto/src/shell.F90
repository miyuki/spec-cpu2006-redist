!-------------------------------------------------------------------------------
!
! SHELL: used to describe contracted cartesian gaussian shells.
!
! Copyright (C) Dylan Jayatilaka, 1997
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
! $Id: shell.foo,v 1.19.2.3 2003/11/13 05:33:21 reaper Exp $
!-------------------------------------------------------------------------------

module SHELL_MODULE

#  include "shell.use"

   implicit none

#  include "macros"
#  include "shell.int"


   STRVEC(STR_SIZE,:), PTR, private :: keys DEFAULT_NULL

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self)
    SHELL :: self
   ! Create a shell object
      PTR :: self
      STACK("SHELL:create")
      START_TIMER("SHELL:create")
      allocate(self)
      ADD_MEMORY(SHELL_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SHELL:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SHELL :: self
   ! Destroy a shell object
      PTR :: self
      STACK("SHELL:destroy")
      START_TIMER("SHELL:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SHELL:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(SHELL_SIZE)
      deallocate(self)
     STOP_TIMER("SHELL:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SHELL :: self
   ! Nullify the pointer part of a shell object
      STACK("SHELL:nullify_ptr_part")
      START_TIMER("SHELL:nullify_ptr_part")
      nullify(self%ex)
      nullify(self%cc)
     STOP_TIMER("SHELL:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SHELL :: self
   ! Destroy pointer part of a shell object
      STACK("SHELL:destroy_ptr_part")
      START_TIMER("SHELL:destroy_ptr_part")
      call destroy_(self%ex)
      call destroy_(self%cc)
     STOP_TIMER("SHELL:destroy_ptr_part")
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

   subroutine create_copy(self,shell)
    SHELL :: self
   ! Create a copy of "shell".
      PTR :: self
      SHELL :: shell
      STACK("SHELL:create_copy")
      START_TIMER("SHELL:create_copy")
      call create_(self)
      call copy_(self,shell)
     STOP_TIMER("SHELL:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,shell)
    SHELL :: self
   ! Copy a shell object. Make sure pointer parts are nullified or
   ! destroyed, as you like, before using this.
      SHELL, IN :: shell
      STACK("SHELL:copy")
      START_TIMER("SHELL:copy")
      self = shell
      call create_copy_(self%ex,shell%ex)
      call create_copy_(self%cc,shell%cc)
     STOP_TIMER("SHELL:copy")
      UNSTACK
   end subroutine

   subroutine set(self,shell)
    SHELL :: self
   ! Set a shell object
      SHELL :: shell
      STACK("SHELL:set")
      START_TIMER("SHELL:set")
      self = shell
     STOP_TIMER("SHELL:set")
      CHECK
   end subroutine

   subroutine set_defaults(self)
    SHELL :: self
   ! Set a default shell
      STACK("SHELL:set_defaults")
      START_TIMER("SHELL:set_defaults")
      self%l = 0
      self%n_cc = 0
      self%n_comp = 0
     STOP_TIMER("SHELL:set_defaults")
      CHECK
   end subroutine

   subroutine set_n_comp(self)
    SHELL :: self
   ! Set the number of components
      STACK("SHELL:set_n_comp")
      START_TIMER("SHELL:set_n_comp")
      self%n_comp = (self%l+1)*(self%l+2)/2
     STOP_TIMER("SHELL:set_n_comp")
      CHECK
   end subroutine

!   update ::: private
!   ! Update the shell data
!      DIE_IF(.ex.destroyed,"no expononents")
!      DIE_IF(.cc.destroyed,"no contractions")
!      ENSURE(stdin.default_units==" ","default units still set")
!   end

!   n_comp_sum result (res)
!   ! No. of cartesian components up to shell with momentum .l
!     res :: INT
!     res = (.l+1)*(.l+2)*(.l+3)/6
!   end

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    SHELL :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("SHELL:read_keywords")
     START_TIMER("SHELL:read_keywords")
     ENSURE(next_item_(stdin)=="{","SHELL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("SHELL:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SHELL :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
     STR(*), IN :: keyword
     STR(STR_SIZE) :: word
     STACK("SHELL:process_keyword")
     START_TIMER("SHELL:process_keyword")
     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}           "); ! exit surrounding loop
       case ("cc=         "); call read_cc_(self)
       case ("ex=         "); call read_ex_(self)
       case ("ex,cc=      "); call read_ex_cc_(self)
       case ("junk,ex,cc= "); call read_junk_ex_cc_(self)
       case ("l=          "); call read_l_(self)
       case ("l_chr=      "); call read_l_chr_(self)
       case ("l_int=      "); call read_l_int_(self)
       case ("n_cc=       "); call read_n_cc_(self)
       case ("put         "); call put_(self)
       case ("units=      "); call read_units_(self)
       ! These are only for making custom tables for the list type
       case ("flush       "); call flush_(stdout)
       case ("put_l       "); call put_(stdout,self%l)
       case ("put_l_chr   "); call put_(stdout,l_chr_(self),int_width=TRUE)
       case ("put_l_int   "); call put_(stdout,self%l)
       case ("put_n_cc    "); call put_(stdout,self%n_cc)
       case ("put_n_prim  "); call put_(stdout,n_prim_(self))
       case ("put_norm    "); call put_(stdout,norm_(self))
       case default;         allocate(tonto%known_keywords(18))
       tonto%known_keywords(1) = "}           "
       tonto%known_keywords(2) = "cc=         "
       tonto%known_keywords(3) = "ex=         "
       tonto%known_keywords(4) = "ex,cc=      "
       tonto%known_keywords(5) = "junk,ex,cc= "
       tonto%known_keywords(6) = "l=          "
       tonto%known_keywords(7) = "l_chr=      "
       tonto%known_keywords(8) = "l_int=      "
       tonto%known_keywords(9) = "n_cc=       "
       tonto%known_keywords(10) = "put         "
       tonto%known_keywords(11) = "units=      "
       tonto%known_keywords(12) = "flush       "
       tonto%known_keywords(13) = "put_l       "
       tonto%known_keywords(14) = "put_l_chr   "
       tonto%known_keywords(15) = "put_l_int   "
       tonto%known_keywords(16) = "put_n_cc    "
       tonto%known_keywords(17) = "put_n_prim  "
       tonto%known_keywords(18) = "put_norm    "
       call unknown_(tonto,word,"SHELL:process_keyword")
       deallocate(tonto%known_keywords)
     end select
     STOP_TIMER("SHELL:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    SHELL :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("SHELL:read_units")
      START_TIMER("SHELL:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("SHELL:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    SHELL :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("SHELL:read_junk")
      START_TIMER("SHELL:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("SHELL:read_junk")
      CHECK
   end subroutine

   subroutine read_l(self)
    SHELL :: self
   ! Read in the l symbol
      STR(STR_SIZE) :: word
      STACK("SHELL:read_l")
      START_TIMER("SHELL:read_l")
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if
     STOP_TIMER("SHELL:read_l")
      CHECK
   end subroutine

   subroutine read_l_int(self)
    SHELL :: self
   ! Read in the l integer
      STR(STR_SIZE) :: word
      STACK("SHELL:read_l_int")
      START_TIMER("SHELL:read_l_int")
      call read_(stdin,word)
      ENSURE(is_int_(word),"SHELL:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)
      call set_n_comp_(self)
     STOP_TIMER("SHELL:read_l_int")
      CHECK
   end subroutine

   subroutine read_l_chr(self)
    SHELL :: self
   ! Read in the l symbol
      STR(STR_SIZE) :: word
      STR(1) :: l_c
      INT :: l
      STACK("SHELL:read_l_chr")
      START_TIMER("SHELL:read_l_chr")
      call read_(stdin,word)
      ENSURE(len_trim(word)==1,"SHELL:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s");   l = 0
         case ("p");   l = 1
         case ("d");   l = 2
         case ("f");   l = 3
         case ("g");   l = 4
         case default; l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l
      call set_n_comp_(self)
     STOP_TIMER("SHELL:read_l_chr")
      CHECK
   end subroutine

   subroutine read_n_cc(self)
    SHELL :: self
   ! Read in the number of contraction coefficients
      STACK("SHELL:read_n_cc")
      START_TIMER("SHELL:read_n_cc")
      call read_(stdin,self%n_cc)
      ENSURE(self%n_cc>0,"SHELL:read_n_cc ... n_cc must be positive")
     STOP_TIMER("SHELL:read_n_cc")
      CHECK
   end subroutine

   subroutine read_ex(self)
    SHELL :: self
   ! Read in the exponents
      STACK("SHELL:read_ex")
      START_TIMER("SHELL:read_ex")
      ENSURE(self%n_cc>0,"SHELL:read_ex ... n_cc must be entered first")
      ENSURE(NOT associated(self%ex),"SHELL:read_ex ... ex already entered")
      call create_(self%ex,self%n_cc)
      call read_(stdin,self%ex)
     STOP_TIMER("SHELL:read_ex")
      UNSTACK
   end subroutine

   subroutine read_cc(self)
    SHELL :: self
   ! Read in the contraction coefficients
      STACK("SHELL:read_cc")
      START_TIMER("SHELL:read_cc")
      ENSURE(self%n_cc>0,"SHELL:read_cc ... n_cc must be entered first")
      ENSURE(NOT associated(self%cc),"SHELL:read_cc ... cc already entered")
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%cc)
     STOP_TIMER("SHELL:read_cc")
      UNSTACK
   end subroutine

   subroutine read_ex_cc(self)
    SHELL :: self
   ! Read in the exponents and contractions
      STACK("SHELL:read_ex_cc")
      START_TIMER("SHELL:read_ex_cc")
      ENSURE(self%n_cc>0,"SHELL:read_ex_cc ... n_cc must be entered first")
      ENSURE(NOT associated(self%ex),"SHELL:read_ex_cc ... ex already entered")
      ENSURE(NOT associated(self%cc),"SHELL:read_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%ex,self%cc)
     STOP_TIMER("SHELL:read_ex_cc")
      UNSTACK
   end subroutine

   subroutine read_junk_ex_cc(self)
    SHELL :: self
   ! Read in the exponents and contractions preceded by a junk string
      INT :: i
      STACK("SHELL:read_junk_ex_cc")
      START_TIMER("SHELL:read_junk_ex_cc")
      ENSURE(self%n_cc>0,"SHELL:read_junk_ex_cc ... n_cc must be entered first")
      ENSURE(NOT associated(self%ex),"SHELL:read_junk_ex_cc ... ex already entered")
      ENSURE(NOT associated(self%cc),"SHELL:read_junk_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      do i = 1,self%n_cc
         call skip_next_item_(stdin)
         call read_(stdin,self%ex(i))
         call read_(stdin,self%cc(i))
      end do
     STOP_TIMER("SHELL:read_junk_ex_cc")
      UNSTACK
   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    SHELL :: self
   ! Read the "keys".
   ! The following code is inherited from OBJECT
     STACK("SHELL:read_keys")
     START_TIMER("SHELL:read_keys")
     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("SHELL:read_keys")
      CHECK
   end subroutine

   subroutine process_keys(self)
    SHELL :: self
   ! Process each of the words in the "keys" list.
   ! The following code is inherited from OBJECT
      INT :: k,l,n_key
      STR(STR_SIZE) :: keyword
      STRVEC(STR_SIZE,:), PTR :: internal
      STACK("SHELL:process_keys")
      START_TIMER("SHELL:process_keys")
      ENSURE(associated(keys),"SHELL:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            ENSURE(l>0,"SHELL:process_keys ... no matching closing brace, }")
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
     STOP_TIMER("SHELL:process_keys")
      UNSTACK
   end subroutine

   function keys_created(self) result(res)
    SHELL :: self
   ! Return TRUE if the list-element keys are created.
      BIN :: res
   ! The following code is inherited from OBJECT
      STACK("SHELL:keys_created")
      START_TIMER("SHELL:keys_created")
      res = associated(keys)
     STOP_TIMER("SHELL:keys_created")
      UNSTACK
   end function

   subroutine set_keys(self,the_keys)
    SHELL :: self
   ! This is for setting the "keys" externally.
     STRVEC(len=*,:) :: the_keys
   ! The following code is inherited from OBJECT
     STACK("SHELL:set_keys")
     START_TIMER("SHELL:set_keys")
     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)
     STOP_TIMER("SHELL:set_keys")
      CHECK
   end subroutine

   subroutine clear_keys(self)
    SHELL :: self
   ! This is for destroying the "keys" externally.
   ! The following code is inherited from OBJECT
     STACK("SHELL:clear_keys")
     START_TIMER("SHELL:clear_keys")
     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if
     STOP_TIMER("SHELL:clear_keys")
      CHECK
   end subroutine

   subroutine put_table_footer(self)
    SHELL :: self
   ! Output a table footer from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
   ! The following code is inherited from OBJECT
     STACK("SHELL:put_table_footer")
     START_TIMER("SHELL:put_table_footer")
     call dash_(stdout,width=table_width_(self))
     STOP_TIMER("SHELL:put_table_footer")
      CHECK
   end subroutine

   subroutine put_table_header(self)
    SHELL :: self
   ! Output a table header from the list of "keys". NOTE: not all keywords need
   ! contribute to the banner - any unrecognised keyword is skipped.
     STR(STR_SIZE) :: word
     INT :: width,k

     STACK("SHELL:put_table_header")
     START_TIMER("SHELL:put_table_header")
     width = table_width_(self)

     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush       "); call flush_(stdout); exit
           case ("put_l       "); call put_(stdout,"l",int_width=TRUE)
           case ("put_l_chr   "); call put_(stdout,"l",int_width=TRUE)
           case ("put_l_int   "); call put_(stdout,"l",int_width=TRUE)
           case ("put_n_cc    "); call put_(stdout,"n_cc",int_width=TRUE)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=TRUE)
           case ("put_norm    "); call put_(stdout,"norm")
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout) ! In case they didn't write one.
           WARN("SHELL:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if
     STOP_TIMER("SHELL:put_table_header")
      CHECK
   end subroutine

   function table_width(self) result(res)
    SHELL :: self
   ! Return how wide a table is, based on "keys".  Note that not all keywords
   ! need to contribute to the banner - any unrecognised keyword is skipped.
     INT :: res
     INT :: int_dash,real_dash,k
     STR(STR_SIZE) :: word
     STACK("SHELL:table_width")
     START_TIMER("SHELL:table_width")
     int_dash = 0
     real_dash = 0
     ENSURE(associated(keys),"SHELL:table_width ... no keywords")
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           "); ! exit surrounding loop
         case ("put_l       "); int_dash = int_dash + 1
         case ("put_l_chr   "); int_dash = int_dash + 1
         case ("put_l_int   "); int_dash = int_dash + 1
         case ("put_n_cc    "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("put_norm    "); real_dash = real_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width
     STOP_TIMER("SHELL:table_width")
      CHECK
   end function

!  *************
!  Input methods
!  *************

   function same_as(self,sh) result(same)
    SHELL :: self
   ! Return TRUE if the shell "self" is the same as "sh".
      SHELL :: sh
      BIN :: same
      STACK("SHELL:same_as")
      START_TIMER("SHELL:same_as")
      same = self%l==sh%l AND self%n_comp==sh%n_comp &
         AND same_as_(self%ex,sh%ex) AND same_as_(self%cc,sh%cc)
     STOP_TIMER("SHELL:same_as")
      CHECK
   end function

   function l_chr(self) result(res)
    SHELL :: self
   ! Return a character representation for the angular mtm
      STR(1) :: res
      INT :: l
      STACK("SHELL:l_chr")
      START_TIMER("SHELL:l_chr")
      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            DIE_IF(l>23,"SHELL:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select
     STOP_TIMER("SHELL:l_chr")
      CHECK
   end function

   subroutine make_contraction_matrix(self,ccm)
    SHELL :: self
   ! Return the contraction coefficient matrix. Size of ccm is (.n_prim,.n_comp).
      REALMAT(:,:) :: ccm
      INT :: b,p,i
      REAL :: f
      REALVEC(:), PTR :: fac
      STACK("SHELL:make_contraction_matrix")
      START_TIMER("SHELL:make_contraction_matrix")
      call create_(fac, self%n_comp )
      call normalising_factors_(fac, self%l )
      do b = 1,self%n_comp       ! do over basis components
         p = b               ! primitive p
         f = fac(b)
         do i = 1,self%n_cc      ! do over contractions
            ccm(p,b) = f*self%cc(i)
            p = p + self%n_comp
         end do
      end do
      call destroy_(fac)
     STOP_TIMER("SHELL:make_contraction_matrix")
      CHECK
   end subroutine

   PURE function norm(self) result(res)
    SHELL :: self
   ! Return the norm of the shell, assuming that the existing contraction
   ! coefficients are with respect to NORMALISED gaussians
      IN :: self
      REAL :: res
      INT :: i,j
      REAL :: sum,a,b,ab
      sum = ZERO
      do i = 1,self%n_cc
         a = self%ex(i)
         do j = 1,i-1
            b = self%ex(j)
            ab = TWO*sqrt(a*b)/(a+b);
            sum = sum + TWO*self%cc(i)*self%cc(j)*ab**(self%l+ONE+HALF);
         end do
         sum = sum + self%cc(i)*self%cc(i)
      end do
      res = sqrt(sum) * ((TWO*PI)**(THREE/FOUR))
     STOP_TIMER("SHELL:norm")
   end function

   subroutine unnormalise(self)
    SHELL :: self
   ! Unnormalise self as if all components in the shell were x^l, and also
   ! assuming the existing contraction coefficients are initially with respect
   ! to raw UNNORMALISED gaussians. It is up to you to correct this factor with
   ! appropriate double factorial square roots for other components.

      ! Take out the normalisation of each primitive.
      ! The double factorial should be: fac = 1/sqrt(df(nx)*df(ny)*df(nz))
      ! where n are the cartesian powers of the basis component
       STACK("SHELL:unnormalise")
       START_TIMER("SHELL:unnormalise")
       self%cc(:) = self%cc(:) * (FOUR*self%ex(:))**(HALF*self%l+HALF+QUARTER) &
                * (ONE/(norm_(self)*sqrt(double_factorial_(self%l))))
     STOP_TIMER("SHELL:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    SHELL :: self
   ! Normalise self as if all components in the shell were x^l, and also
   ! assuming the existing contraction coefficients are with respect to raw
   ! unnormalised gaussians. This will undo routine "unnormalise".
      ! The ((TWO*PI)**(THREE/FOUR)) / .norm factor is to make the cc=1 for a
      ! shell with one primitive
      STACK("SHELL:renormalise")
      START_TIMER("SHELL:renormalise")
      self%cc(:) = self%cc(:) / ((FOUR*self%ex(:))**(HALF*self%l+HALF+QUARTER)) &
               * (sqrt(double_factorial_(self%l)) * ((TWO*PI)**(THREE/FOUR)) / norm_(self))
     STOP_TIMER("SHELL:renormalise")
      CHECK
   end subroutine

   PURE function n_prim(self) result(res)
    SHELL :: self
   ! Return the number of primitive gaussians in the shell
      IN :: self
      INT :: res
      res = self%n_comp*self%n_cc
     STOP_TIMER("SHELL:n_prim")
   end function

   subroutine put(self)
    SHELL :: self
   ! Put the shell information to "stdout"
       INT :: i
      STACK("SHELL:put")
      START_TIMER("SHELL:put")
      call flush_(stdout)
      call show_(stdout,"L quantum number = ",self%l)
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"N", int_width=TRUE)
      call put_(stdout,"Exponents")
      call put_(stdout,"Contraction")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      do i = 1,self%n_cc
         call put_(stdout,i)
         call put_(stdout, self%ex(i))
         call put_(stdout, self%cc(i))
         call flush_(stdout)
      end do
     STOP_TIMER("SHELL:put")
      CHECK
   end subroutine

end
