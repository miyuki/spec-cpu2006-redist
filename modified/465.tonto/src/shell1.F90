!-------------------------------------------------------------------------------
!
! SHELL1: For describing contracted cartesian gaussian shells with a position
! coordinate
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
! $Id: shell1.foo,v 1.27.2.1 2003/03/06 10:40:57 dylan Exp $
!
!-------------------------------------------------------------------------------

module SHELL1_MODULE

#  include "shell1.use"

   implicit none

#  include "macros"
#  include "shell1.int"


contains

   subroutine create(self)
    SHELL1 :: self
   ! Create a shell object
      PTR :: self
      STACK("SHELL1:create")
      START_TIMER("SHELL1:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(SHELL1_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
     STOP_TIMER("SHELL1:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,shell)
    SHELL1 :: self
   ! Create a shell object from another copy
      PTR :: self
      SHELL1, IN :: shell
      STACK("SHELL1:create_1")
      START_TIMER("SHELL1:create_1")
      call create_(self)
      call copy_(self,shell)
     STOP_TIMER("SHELL1:create_1")
      UNSTACK
   end subroutine

   subroutine create_copy(self,shell)
    SHELL1 :: self
   ! Create a copy of a shell1 object
      PTR :: self
      SHELL1, IN :: shell
      STACK("SHELL1:create_copy")
      START_TIMER("SHELL1:create_copy")
      call create_(self)
      call copy_(self,shell)
     STOP_TIMER("SHELL1:create_copy")
      UNSTACK
   end subroutine

   subroutine create_copy_1(self,shell,pos)
    SHELL1 :: self
   ! Create a shell object from another copy
      PTR :: self
      SHELL, IN :: shell
      REALVEC(:), IN :: pos
      STACK("SHELL1:create_copy_1")
      START_TIMER("SHELL1:create_copy_1")
      call create_(self)
      call copy_(self,shell,pos)
     STOP_TIMER("SHELL1:create_copy_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    SHELL1 :: self
   ! Destroy a shell object
      PTR :: self
      STACK("SHELL1:destroy")
      START_TIMER("SHELL1:destroy")
      if (NOT associated(self)) then; STOP_TIMER("SHELL1:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(SHELL1_SIZE)
      deallocate(self)
     STOP_TIMER("SHELL1:destroy")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    SHELL1 :: self
   ! Nullify the pointer parts of self
     STACK("SHELL1:nullify_ptr_part")
     START_TIMER("SHELL1:nullify_ptr_part")
     nullify(self%ex)
     nullify(self%cc)
     STOP_TIMER("SHELL1:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    SHELL1 :: self
   ! Destroy pointer part of a shell object
      STACK("SHELL1:destroy_ptr_part")
      START_TIMER("SHELL1:destroy_ptr_part")
      call destroy_(self%ex)
      call destroy_(self%cc)
     STOP_TIMER("SHELL1:destroy_ptr_part")
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

   subroutine copy(self,shell)
    SHELL1 :: self
   ! Make a shell1 object from a shell1 object.
   ! NOTE : ensure the ptr parts have been destroyed or nullified beforehand.
     SHELL1, IN :: shell
     STACK("SHELL1:copy")
     START_TIMER("SHELL1:copy")
     self = shell
     call create_copy_(self%ex,shell%ex)
     call create_copy_(self%cc,shell%cc)
     STOP_TIMER("SHELL1:copy")
      UNSTACK
   end subroutine

   subroutine copy_1(self,shell,pos)
    SHELL1 :: self
   ! Make a shell1 object from the shell and its position.
   ! NOTE : ensure the ptr parts have been destroyed beforehand.
     OUT :: self
     SHELL, IN :: shell
     REALVEC(:), IN :: pos
     STACK("SHELL1:copy_1")
     START_TIMER("SHELL1:copy_1")
     ENSURE(NOT associated(self%ex),"SHELL1:copy_1 ... ex not destroyed")
     ENSURE(NOT associated(self%cc),"SHELL1:copy_1 ... cc not destroyed")
     self%n_cc = shell%n_cc
     self%l = shell%l
     self%n_comp = shell%n_comp
     self%pos = pos
     call create_copy_(self%ex,shell%ex)
     call create_copy_(self%cc,shell%cc)
     STOP_TIMER("SHELL1:copy_1")
      UNSTACK
   end subroutine

   subroutine set(self,shell,pos)
    SHELL1 :: self
   ! Set a shell1 object.
      REALVEC(:), optional :: pos
      SHELL, optional :: shell
      STACK("SHELL1:set")
      START_TIMER("SHELL1:set")
      if (present(pos)) self%pos = pos
      if (present(shell)) then
         self%l = shell%l
         self%n_comp = shell%n_comp
         self%n_cc = shell%n_cc
         self%ex => shell%ex
         self%cc => shell%cc
      end if
     STOP_TIMER("SHELL1:set")
      CHECK
   end subroutine

   subroutine set_1(self,shell)
    SHELL1 :: self
   ! Set a shell1 object
      SHELL1 :: shell
      STACK("SHELL1:set_1")
      START_TIMER("SHELL1:set_1")
      self = shell
     STOP_TIMER("SHELL1:set_1")
      CHECK
   end subroutine

   subroutine set_defaults(self)
    SHELL1 :: self
   !
      STACK("SHELL1:set_defaults")
      START_TIMER("SHELL1:set_defaults")
      self%l = 0
      self%n_comp = 0
      self%n_cc = 0
      self%pos = ZERO
     STOP_TIMER("SHELL1:set_defaults")
      CHECK
   end subroutine

   function n_prim(self) result(res)
    SHELL1 :: self
   ! Return the number of primitive gaussians in the shell
      INT :: res
      STACK("SHELL1:n_prim")
      START_TIMER("SHELL1:n_prim")
      res = self%n_comp*self%n_cc
     STOP_TIMER("SHELL1:n_prim")
      CHECK
   end function

   subroutine update(self)
    SHELL1 :: self
   ! Update the shell data
      STACK("SHELL1:update")
      START_TIMER("SHELL1:update")
      self%n_comp = (self%l+1)*(self%l+2)/2
     STOP_TIMER("SHELL1:update")
      CHECK
   end subroutine

!   n_comp_sum result (res)
!   ! No. of cartesian components up to shell with momentum .l
!     res :: INT
!     res = (.l+1)*(.l+2)*(.l+3)/6
!   end

   subroutine put(self)
    SHELL1 :: self
   ! Put the shell information to "stdout"
       INT :: i
      STACK("SHELL1:put")
      START_TIMER("SHELL1:put")
      call flush_(stdout)
      call show_(stdout,"L quantum number =",self%l,real_width=TRUE)
      call show_(stdout,"Position         =",self%pos)
      call flush_(stdout)
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
      call dash_(stdout,int_fields=1,real_fields=2)
     STOP_TIMER("SHELL1:put")
      CHECK
   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    SHELL1 :: self
   ! Read data from "stdin" using keyword style input.
      STR(STR_SIZE) :: word
      STACK("SHELL1:read_keywords")
      START_TIMER("SHELL1:read_keywords")
      call read_(stdin,word)
      DIE_IF(word/="{","SHELL1:read_keywords ... expecting an open bracket symbol, {")
      read_loop: do             ! Loop over keywords
         call read_(stdin,word)
         if (word=="}")         exit read_loop
         if (reverted_(stdin))    exit read_loop
         call process_keyword_(self,word)
      end do read_loop
      call update_(self)
     STOP_TIMER("SHELL1:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    SHELL1 :: self
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("SHELL1:process_keyword")
      START_TIMER("SHELL1:process_keyword")
      word = keyword
      call to_lower_case_(word)
      if (includes_(word," ")) then
         call redirect_(stdin,(/word/))
         call read_keywords_(self)
         call revert_(stdin)
      else
         select case (word)
           case ("}          "); ! exit case
           case ("cc=        "); call read_cc_(self)
           case ("ex=        "); call read_ex_(self)
           case ("ex,cc=     "); call read_ex_cc_(self)
           case ("junk,ex,cc="); call read_junk_ex_cc_(self)
           case ("l=         "); call read_l_(self)
           case ("l_chr=     "); call read_l_chr_(self)
           case ("l_int=     "); call read_l_int_(self)
           case ("n_cc=      "); call read_n_cc_(self)
           case ("pos=       "); call read_pos_(self)
           case ("put        "); call put_(self)
           case ("units=     "); call read_units_(self)
           case  default ;       allocate(tonto%known_keywords(12))
           tonto%known_keywords(1) = "}          "
           tonto%known_keywords(2) = "cc=        "
           tonto%known_keywords(3) = "ex=        "
           tonto%known_keywords(4) = "ex,cc=     "
           tonto%known_keywords(5) = "junk,ex,cc="
           tonto%known_keywords(6) = "l=         "
           tonto%known_keywords(7) = "l_chr=     "
           tonto%known_keywords(8) = "l_int=     "
           tonto%known_keywords(9) = "n_cc=      "
           tonto%known_keywords(10) = "pos=       "
           tonto%known_keywords(11) = "put        "
           tonto%known_keywords(12) = "units=     "
           call unknown_(tonto,word,"SHELL1:process_keyword")
           deallocate(tonto%known_keywords)
         end select
      end if
     STOP_TIMER("SHELL1:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    SHELL1 :: self
   ! Read a string which describes the units to be used
      STACK("SHELL1:read_units")
      START_TIMER("SHELL1:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("SHELL1:read_units")
      CHECK
   end subroutine

   subroutine read_l(self)
    SHELL1 :: self
   ! Read in the l symbol
      STR(STR_SIZE) :: word
      STACK("SHELL1:read_l")
      START_TIMER("SHELL1:read_l")
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if
     STOP_TIMER("SHELL1:read_l")
      CHECK
   end subroutine

   subroutine read_l_int(self)
    SHELL1 :: self
   ! Read in the l integer
      STR(STR_SIZE) :: word
      STACK("SHELL1:read_l_int")
      START_TIMER("SHELL1:read_l_int")
      call read_(stdin,word)
      ENSURE(is_int_(word),"SHELL1:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)
     STOP_TIMER("SHELL1:read_l_int")
      CHECK
   end subroutine

   subroutine read_l_chr(self)
    SHELL1 :: self
   ! Read in the l symbol
      STR(STR_SIZE) :: word
      STR(1) :: l_c
      INT :: l
      STACK("SHELL1:read_l_chr")
      START_TIMER("SHELL1:read_l_chr")
      call read_(stdin,word)
      ENSURE(len_trim(word)==1,"SHELL1:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s"); l = 0
         case ("p"); l = 1
         case ("d"); l = 2
         case ("f"); l = 3
         case ("g"); l = 4
         case default;
            DIE_IF(l_c<"g","SHELL1:read_l_chr ... unknown angular momentum character: "//l_c)
            l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l
     STOP_TIMER("SHELL1:read_l_chr")
      CHECK
   end subroutine

   subroutine read_pos(self)
    SHELL1 :: self
   ! Read in the position
      STACK("SHELL1:read_pos")
      START_TIMER("SHELL1:read_pos")
      call read_(stdin,self%pos)
     STOP_TIMER("SHELL1:read_pos")
      CHECK
   end subroutine

   subroutine read_n_cc(self)
    SHELL1 :: self
   ! Read in the number of contraction coefficients
      STACK("SHELL1:read_n_cc")
      START_TIMER("SHELL1:read_n_cc")
      call read_(stdin,self%n_cc)
   ENSURE(self%n_cc>0,"SHELL1:read_n_cc ... n_cc must be positive")
     STOP_TIMER("SHELL1:read_n_cc")
      CHECK
   end subroutine

   subroutine read_ex(self)
    SHELL1 :: self
   ! Read in the exponents
      STACK("SHELL1:read_ex")
      START_TIMER("SHELL1:read_ex")
      ENSURE(self%n_cc>0,"SHELL1:read_ex ... n_cc must be entered first")
      ENSURE(NOT associated(self%ex),"SHELL1:read_ex ... ex already entered")
      call create_(self%ex,self%n_cc)
      call read_(stdin,self%ex)
     STOP_TIMER("SHELL1:read_ex")
      UNSTACK
   end subroutine

   subroutine read_cc(self)
    SHELL1 :: self
   ! Read in the contraction coefficients
      STACK("SHELL1:read_cc")
      START_TIMER("SHELL1:read_cc")
      ENSURE(self%n_cc>0,"SHELL1:read_cc ... n_cc must be entered first")
      ENSURE(NOT associated(self%cc),"SHELL1:read_cc ... cc already entered")
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%cc)
     STOP_TIMER("SHELL1:read_cc")
      UNSTACK
   end subroutine

   subroutine read_ex_cc(self)
    SHELL1 :: self
   ! Read in the exponents and contractions
      STACK("SHELL1:read_ex_cc")
      START_TIMER("SHELL1:read_ex_cc")
      ENSURE(self%n_cc>0,"SHELL1:read_ex_cc ... n_cc must be entered first")
      ENSURE(NOT associated(self%ex),"SHELL1:read_ex_cc ... ex already entered")
      ENSURE(NOT associated(self%cc),"SHELL1:read_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%ex,self%cc)
     STOP_TIMER("SHELL1:read_ex_cc")
      UNSTACK
   end subroutine

   subroutine read_junk_ex_cc(self)
    SHELL1 :: self
   ! Read in the exponents and contractions preceded by a junk string
      INT :: i
      STACK("SHELL1:read_junk_ex_cc")
      START_TIMER("SHELL1:read_junk_ex_cc")
      ENSURE(self%n_cc>0,"SHELL1:read_junk_ex_cc ... n_cc must be entered first")
      ENSURE(NOT associated(self%ex),"SHELL1:read_junk_ex_cc ... ex already entered")
      ENSURE(NOT associated(self%cc),"SHELL1:read_junk_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      do i = 1,self%n_cc
         call skip_next_item_(stdin)
         call read_(stdin,self%ex(i))
         call read_(stdin,self%cc(i))
      end do
     STOP_TIMER("SHELL1:read_junk_ex_cc")
      UNSTACK
   end subroutine

   function same_as(self,sh) result(same)
    SHELL1 :: self
   ! Return TRUE if the shell "self" is the same as "sh".
      SHELL1 :: sh
      BIN :: same
      STACK("SHELL1:same_as")
      START_TIMER("SHELL1:same_as")
      same = self%l==sh%l AND self%n_comp==sh%n_comp &
         AND same_as_(self%ex,sh%ex) AND same_as_(self%cc,sh%cc) &
         AND same_as_(self%pos,sh%pos)
     STOP_TIMER("SHELL1:same_as")
      CHECK
   end function

   function l_chr(self) result(res)
    SHELL1 :: self
   ! Return a character representation for the angular mtm
      STR(1) :: res
      INT :: l
      STACK("SHELL1:l_chr")
      START_TIMER("SHELL1:l_chr")
      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            DIE_IF(l>23,"SHELL1:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select
     STOP_TIMER("SHELL1:l_chr")
      CHECK
   end function

   subroutine make_contraction_matrix(self,ccm)
    SHELL1 :: self
   ! Return the contraction coefficient matrix. Size of ccm is (.n_prim,.n_comp).
      REALMAT(:,:) :: ccm
      INT :: b,p,i
      REAL :: f
      REALVEC(:), PTR :: fac
      STACK("SHELL1:make_contraction_matrix")
      START_TIMER("SHELL1:make_contraction_matrix")
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
     STOP_TIMER("SHELL1:make_contraction_matrix")
      CHECK
   end subroutine

   function norm(self) result(res)
    SHELL1 :: self
   ! Return the norm of the shell, assuming that the existing contraction
   ! coefficients are with respect to NORMALISED gaussians
      REAL :: res
      INT :: i,j
      REAL :: sum,a,b,ab
      STACK("SHELL1:norm")
      START_TIMER("SHELL1:norm")
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
     STOP_TIMER("SHELL1:norm")
      CHECK
   end function

   subroutine unnormalise(self)
    SHELL1 :: self
   ! Unnormalise self as if all components in the shell were x^l, and also
   ! assuming the existing contraction coefficients are initially with respect
   ! to NORMALISED gaussians. It is up to you to correct this factor with
   ! appropriate double factorial square roots for other components.

      ! Take out the normalisation of each primitive.
      ! The double factorial should be: fac = 1/sqrt(df(nx)*df(ny)*df(nz))
      ! where n are the cartesian powers of the basis component
       STACK("SHELL1:unnormalise")
       START_TIMER("SHELL1:unnormalise")
       self%cc(:) = self%cc(:) * (FOUR*self%ex(:))**(HALF*self%l+HALF+QUARTER) &
                * (ONE/(norm_(self)*sqrt(double_factorial_(self%l))))
     STOP_TIMER("SHELL1:unnormalise")
      CHECK
   end subroutine

   subroutine renormalise(self)
    SHELL1 :: self
   ! Normalise self as if all components in the shell were x^l, and also
   ! assuming the existing contraction coefficients are with respect to raw
   ! unnormalised gaussians. This will undo routine "unnormalise".
   ! The overall ((TWO*PI)**(THREE/FOUR)) / .norm factor is to make the
   ! cc=1 for a shell with one primitive
      STACK("SHELL1:renormalise")
      START_TIMER("SHELL1:renormalise")
      self%cc(:) = self%cc(:) / ((FOUR*self%ex(:))**(HALF*self%l+HALF+QUARTER)) &
               * (sqrt(double_factorial_(self%l)) * ((TWO*PI)**(THREE/FOUR)) / norm_(self))
     STOP_TIMER("SHELL1:renormalise")
      CHECK
   end subroutine

   subroutine make_grid(self,g,pt)
    SHELL1 :: self
   ! Return "g(i,n)", the value of the shell component "n" on grid point "i"
   ! given a set of grid points "pt(i,1:3)"
   ! Note: it is assumed that the shell is normalised, and the appropriate
   ! double factorial normalising factors are introduced for each component
      REALMAT(:,:), target :: pt
       REALMAT(:,:), target :: g
      REALVEC(:), PTR :: x,y,z
      STACK("SHELL1:make_grid")
      START_TIMER("SHELL1:make_grid")
      x => pt(:,1); y => pt(:,2); z => pt(:,3)
      call make_grid_(self,g,x,y,z)
     STOP_TIMER("SHELL1:make_grid")
      CHECK
   end subroutine

   subroutine make_grid_1(self,f,x,y,z)
    SHELL1 :: self
   ! Return "f(i,n)", the value of the shell component "n" on grid point "i"
   ! given a set of grid points "(x(i),y(i),z(i))"
   ! Note: it is assumed that the shell is normalised; the appropriate
   ! double factorial normalising factors are introduced for each component
      REALVEC(:) :: x,y,z
      REALMAT(:,:) :: f
      REALVEC(:), PTR :: fac,bx,by,bz
      INTVEC(:), PTR :: nx,ny,nz
      INT :: n_pt,n,b
      REAL :: rr,xx,yy,zz,posx,posy,posz,f0,f1,bxb,byb,bzb
      REAL :: xx2,yy2,zz2,xx3,yy3,zz3,zz_f1
      STACK("SHELL1:make_grid_1")
      START_TIMER("SHELL1:make_grid_1")
      n_pt = size(x)
      posx = self%pos(1); posy = self%pos(2); posz = self%pos(3)
      select case (self%l)
        case (0)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             f(n,1) = sum( self%cc * exp( -rr * self%ex ))
          end do
        case (1)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz

             ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             f0 = sum( self%cc * exp( -rr * self%ex ))

             f(n,1) = xx*f0
             f(n,2) = yy*f0
             f(n,3) = zz*f0
          end do
        case (2)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz

             ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             f0 = sum( self%cc * exp( -rr * self%ex ))

             f1 = sqrt(THREE)*f0
             zz_f1 = zz*f1
             f(n,1) = xx*xx*f0
             f(n,2) = yy*yy*f0
             f(n,3) = zz*zz*f0
             f(n,4) = xx*yy*f1
             f(n,5) = xx*zz_f1
             f(n,6) = yy*zz_f1
          end do
        case (3)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             xx2 = xx*xx
             yy2 = yy*yy
             zz2 = zz*zz
             xx3 = xx2*xx
             yy3 = yy2*yy
             zz3 = zz2*zz

             ! Radial part, summed over primitives
             rr = xx2 + yy2 + zz2
             f0 = sum( self%cc * exp( -rr * self%ex ))

             f(n,1)  = fac(1)*f0*xx3
             f(n,2)  = fac(2)*f0*yy3
             f(n,3)  = fac(3)*f0*zz3
             f(n,4)  = fac(4)*f0*xx2*yy
             f(n,5)  = fac(5)*f0*xx2*zz
             f(n,6)  = fac(6)*f0*xx*yy2
             f(n,7)  = fac(7)*f0*yy2*zz
             f(n,8)  = fac(8)*f0*xx*zz2
             f(n,9)  = fac(9)*f0*yy*zz2
             f(n,10) = fac(10)*f0*xx*yy*zz
          end do
          call destroy_(fac)
        case default
          call create_(nx,self%n_comp); call create_(ny,self%n_comp); call create_(nz,self%n_comp)
          call make_gaussian_xyz_powers_(self%l,nx,ny,nz)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          call create_(bx,0,self%l)
          call create_(by,0,self%l)
          call create_(bz,0,self%l)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz

             ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             f0 = sum( self%cc * exp( -rr * self%ex ))

             ! Cartesian orbital part
             ! x**b,y**b,z**b for b=0,.l
             bx(0) = ONE;   by(0) = ONE;   bz(0) = ONE
             bx(1) = xx;    by(1) = yy;    bz(1) = zz
             bxb = xx;      byb = yy;      bzb = zz
             do b=2,self%l
               bxb = bxb*xx;   byb = byb*yy;   bzb = bzb*zz
               bx(b) = bxb;    by(b) = byb;    bz(b) = bzb
             end do

             ! Combine cartesian orbital powers with the exponential part
             f(n,:) = fac(:)*f0*bx(nx(:))*by(ny(:))*bz(nz(:))
          end do
          call destroy_(bz)
          call destroy_(by)
          call destroy_(bx)
          call destroy_(fac)
          call destroy_(nz); call destroy_(ny); call destroy_(nx)
     end select
     STOP_TIMER("SHELL1:make_grid_1")
      CHECK
   end subroutine

   subroutine make_nabla_grid(self,g,pt)
    SHELL1 :: self
   ! Return "g(i,n,1:3)", the value of the gradient of the shell component "n"
   ! on grid point "i" given a set of grid points "pt(i,1:3)".
      REALMAT(:,:) :: pt
      REALMAT3(:,:,:) :: g
      REALVEC(:), PTR :: fac,cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      INTMAT(:,:), PTR :: nn
      INT :: n_pt,n,b,nxb,nyb,nzb
      REAL :: xx,yy,zz,rr,facb
      REAL :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb
      STACK("SHELL1:make_nabla_grid")
      START_TIMER("SHELL1:make_nabla_grid")
      ENSURE(size(pt,2)==3,"SHELL1:make_nabla_grid ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%pos(1);  y = self%pos(2);  z = self%pos(3)
      select case (self%l)
        case (0)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             rr = xx*xx + yy*yy + zz*zz
             g1 = -TWO*sum(self%ex*self%cc*exp(-self%ex*rr))
             g(n,1,1) = g1*xx
             g(n,1,2) = g1*yy
             g(n,1,3) = g1*zz
          end do
        case (1)
          call create_(cc_exp_rr,self%n_cc)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -TWO*sum(self%ex*cc_exp_rr)
             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             g(n,1,1) = g0+xx*g1x
             g(n,1,2) = xx*g1y
             g(n,1,3) = xx*g1z
             g(n,2,1) = yy*g1x
             g(n,2,2) = g0+yy*g1y
             g(n,2,3) = yy*g1z
             g(n,3,1) = zz*g1x
             g(n,3,2) = zz*g1y
             g(n,3,3) = g0+zz*g1z
          end do
          call destroy_(cc_exp_rr)
        case default
          call create_(cc_exp_rr,self%n_cc)
          call create_(nn,3, self%n_comp );   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          call create_(bx,0,self%l);   call create_(by,0,self%l);   call create_(bz,0,self%l)
          call create_(gxb,0,self%l);  call create_(gyb,0,self%l);  call create_(gzb,0,self%l)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -TWO*sum(self%ex*cc_exp_rr)
             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
             bx(0) = ONE;         by(0) = ONE;         bz(0) = ONE
             bx(1) = xx;          by(1) = yy;          bz(1) = zz
             bxb = xx;            byb = yy;            bzb = zz
             gxb(0) = g1x;        gyb(0) = g1y;        gzb(0) = g1z
             gxb(1) = g0+xx*g1x;  gyb(1) = g0+yy*g1y;  gzb(1) = g0+zz*g1z
             do b=2,self%l
               gxb(b) = (b*g0+xx*g1x) * bxb           ! (n-1)th power
               gyb(b) = (b*g0+yy*g1y) * byb           ! of the xyz part
               gzb(b) = (b*g0+zz*g1z) * bzb
               bxb = bxb*xx;   byb = byb*yy;   bzb = bzb*zz
               bx(b) = bxb;    by(b) = byb;    bz(b) = bzb
             end do
             ! Loop over basis functions
             do b = 1,self%n_comp
                nxb = nn(1,b)
                nyb = nn(2,b)
                nzb = nn(3,b)
                facb = fac(b)         ! Basis fn. normalisation factor
                g(n,b,1) = facb * gxb(nxb) *  by(nyb) *  bz(nzb)
                g(n,b,2) = facb *  bx(nxb) * gyb(nyb) *  bz(nzb)
                g(n,b,3) = facb *  bx(nxb) *  by(nyb) * gzb(nzb)
             end do
          end do
          call destroy_(gzb);  call destroy_(gyb);  call destroy_(gxb)
          call destroy_(bz);   call destroy_(by);   call destroy_(bx)
          call destroy_(fac)
          call destroy_(nn)
          call destroy_(cc_exp_rr)
      end select
     STOP_TIMER("SHELL1:make_nabla_grid")
      CHECK
   end subroutine

   subroutine make_nabla_grid_1(self,g,f,pt)
    SHELL1 :: self
   ! Return "g(i,n,1:3)", the value of the gradient of the shell component "n"
   ! on grid point "i" given a set of grid points "pt(i,1:3)".
      REALMAT(:,:) :: f,pt
      REALMAT3(:,:,:) :: g
      REALVEC(:), PTR :: fac,cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      INTMAT(:,:), PTR :: nn
      INT :: n_pt,n,b,nxb,nyb,nzb
      REAL :: xx,yy,zz,rr,facb,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb,by_bz,facb_bx
      STACK("SHELL1:make_nabla_grid_1")
      START_TIMER("SHELL1:make_nabla_grid_1")
      ENSURE(size(pt,2)==3,"SHELL1:make_nabla_grid_1 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%pos(1);  y = self%pos(2);  z = self%pos(3)
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)
        case (0)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z

             ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g1 = -TWO*sum(self%ex*cc_exp_rr)

             f(n,1)   = sum(cc_exp_rr)
             g(n,1,1) = g1*xx
             g(n,1,2) = g1*yy
             g(n,1,3) = g1*zz
          end do
        case (1)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z

             ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -TWO*sum(self%ex*cc_exp_rr)

             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             f(n,1) = xx*g0
             f(n,2) = yy*g0
             f(n,3) = zz*g0
             g(n,1,1) = g0+xx*g1x
             g(n,1,2) = xx*g1y
             g(n,1,3) = xx*g1z
             g(n,2,1) = yy*g1x
             g(n,2,2) = g0+yy*g1y
             g(n,2,3) = yy*g1z
             g(n,3,1) = zz*g1x
             g(n,3,2) = zz*g1y
             g(n,3,3) = g0+zz*g1z
          end do
        case default
          call create_(nn,3, self%n_comp );   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac, self%n_comp );    call normalising_factors_(fac, self%l )
          call create_(bx,0,self%l);   call create_(by,0,self%l);   call create_(bz,0,self%l)
          call create_(gxb,0,self%l);  call create_(gyb,0,self%l);  call create_(gzb,0,self%l)
          do n = 1,n_pt              ! Do the exponential part of the gaussian
             xx=pt(n,1) - x;   yy=pt(n,2) - y;   zz=pt(n,3) - z
             ! Radial part, summed over primitives
             rr = xx*xx + yy*yy + zz*zz
             cc_exp_rr = self%cc*exp(-self%ex*rr)
             g0 = sum(cc_exp_rr)
             g1 = -TWO*sum(self%ex*cc_exp_rr)
             ! Cartesian orbital part
             ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
             g1x = g1*xx
             g1y = g1*yy
             g1z = g1*zz
             xx_g1x = xx*g1x
             yy_g1y = yy*g1y
             zz_g1z = zz*g1z
             bxb = xx;            byb = yy;            bzb = zz
             bx(0) = ONE;         by(0) = ONE;         bz(0) = ONE
             bx(1) = xx;          by(1) = yy;          bz(1) = zz
             gxb(0) = g1x;        gyb(0) = g1y;        gzb(0) = g1z
             gxb(1) = g0+xx_g1x;  gyb(1) = g0+yy_g1y;  gzb(1) = g0+zz_g1z
             do b=2,self%l
               gxb(b) = (b*g0+xx_g1x) * bxb          ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y) * byb          ! of the xyz part
               gzb(b) = (b*g0+zz_g1z) * bzb
               bxb = bxb*xx;   byb = byb*yy;   bzb = bzb*zz
               bx(b) = bxb;    by(b) = byb;    bz(b) = bzb
             end do
             ! Combine cartesian orbital powers with the exponential part
             do b = 1,self%n_comp
               nxb = nn(1,b)
               nyb = nn(2,b)
               nzb = nn(3,b)
               facb = fac(b)         ! Basis fn. normalisation factor
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb *  bzb
               facb_bx = facb  *  bxb
               f(n,b)   = facb_bx * by_bz * g0
               g(n,b,1) = facb    * gxb(nxb) * by_bz
               g(n,b,2) = facb_bx * gyb(nyb) * bzb
               g(n,b,3) = facb_bx * byb      * gzb(nzb)
             end do

          end do
          call destroy_(gzb);  call destroy_(gyb);  call destroy_(gxb)
          call destroy_(bz);   call destroy_(by);   call destroy_(bx)
          call destroy_(fac)
          call destroy_(nn)
      end select
      call destroy_(cc_exp_rr)
     STOP_TIMER("SHELL1:make_nabla_grid_1")
      CHECK
   end subroutine

   subroutine make_nabla_grid_fdm(self,g,f,pt)
    SHELL1 :: self
   ! Return "g(i,n,1:3)", the value of the gradient of the shell component "n"
   ! on grid point "i" given a set of grid points "pt(i,1:3)".
   ! This nabla grid is produced using the finite difference method.  It is much
   ! slower, but useful for checking.
      REALMAT(:,:) :: f,pt
      REALMAT3(:,:,:) :: g
      REALMAT(:,:), PTR :: f1,f2,pt1
      REALVEC(:), PTR :: comp
      REAL :: alpha
      INT :: n_pt,i
      STACK("SHELL1:make_nabla_grid_fdm")
      START_TIMER("SHELL1:make_nabla_grid_fdm")
      ENSURE(size(pt,2)==3,"SHELL1:make_nabla_grid_fdm ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      call make_grid_(self,f,pt)
      alpha = TOL(6)
      call create_(pt1,n_pt,3)
      call create_(f1,n_pt,self%n_comp)
      call create_(f2,n_pt,self%n_comp)
      do i=1,3
        comp => pt1(:,i)
        pt1 = pt
        comp = comp + alpha
        call make_grid_(self,f1,pt1)
        pt1 = pt
        comp = comp - alpha
        call make_grid_(self,f2,pt1)
        g(:,:,i) = HALF/alpha * (f2(:,:) - f1(:,:))
      end do
      call destroy_(f2)
      call destroy_(f1)
      call destroy_(pt1)
     STOP_TIMER("SHELL1:make_nabla_grid_fdm")
      CHECK
   end subroutine

   subroutine make_laplacian_grid(self,g,pt)
    SHELL1 :: self
   ! Return "g(i,n,1:3)", the value of the second derivatives
   ! (d/dx2,d/dy2,d/dz2) of the shell component "n" on grid point "i" given a
   ! set of grid points "pt(i,1:3)".
      REALMAT(:,:) :: pt
      REALMAT3(:,:,:) :: g
      REALVEC(:), PTR :: fac
      INTMAT(:,:), PTR :: nn
      INT :: n_pt,n,b,p,j,nx,ny,nz
      REAL :: aa,a2,x,y,z,rr,val
      REAL :: g0,g1,g2,g2x,g2y,g2z
      REAL :: gnbx,gnby,gnbz,x2,y2,z2
      REAL :: gx0,gx1,gy0,gy1,gz0,gz1,facb
      REAL :: tn,tn2
      STACK("SHELL1:make_laplacian_grid")
      START_TIMER("SHELL1:make_laplacian_grid")
      ENSURE(size(pt,2)==3,"SHELL1:make_laplacian_grid ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      select case (self%l)
        case (0)
          do n = 1,n_pt             ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc          ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g(n,1,1) = g1 + g2*x2
            g(n,1,2) = g1 + g2*y2
            g(n,1,3) = g1 + g2*z2
          end do
        case (1)
          do n = 1,n_pt             ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc          ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2
            g(n,1,1) = x * (3 * g1 + g2x)
            g(n,1,2) = x * (g1 + g2y)
            g(n,1,3) = x * (g1 + g2z)
            g(n,2,1) = y * (g1 + g2x)
            g(n,2,2) = y * (3 * g1 + g2y)
            g(n,2,3) = y * (g1 + g2z)
            g(n,3,1) = z * (g1 + g2x)
            g(n,3,2) = z * (g1 + g2y)
            g(n,3,3) = z * (3 * g1 + g2z)
          end do
        case default
          call create_(nn,3,self%n_comp);   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac,self%n_comp);    call normalising_factors_(fac,self%l)
          do n = 1,n_pt             ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc          ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 +    val
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2

            ! Some that are used multiple times.
            ! Taken out of loop over n_comp.
            gx0 = g1 + g2x
            gy0 = g1 + g2y
            gz0 = g1 + g2z
            gx1 = (THREE * g1 + g2x)*x  ! *x^1
            gy1 = (THREE * g1 + g2y)*y  ! *y^1
            gz1 = (THREE * g1 + g2z)*z  ! *z^1

            do b = 1,self%n_comp        ! Loop over basis functions
                nx = nn(1,b)
                ny = nn(2,b)
                nz = nn(3,b)

                select case (nx)
                  case (0);     gnbx = gx0
                  case (1);     gnbx = gx1
                  case (2);     gnbx = g0+g0 + (5 * g1 + g2x) * x * x
                  case default
                    tn2 = x**(nx-2)
                    tn = tn2*x*x
                    gnbx = nx*(nx-1)*g0*tn2 + ((nx+nx+1) * g1 + g2x) * tn
                end select
                select case (ny)
                  case (0);     gnby = gy0
                  case (1);     gnby = gy1
                  case (2);     gnby = g0+g0 + (5 * g1 + g2y) * y * y
                  case default
                    tn2 = y**(ny-2)
                    tn = tn2*y*y
                    gnby = ny*(ny-1)*g0*tn2 + ((ny+ny+1) * g1 + g2y) * tn
                end select
                select case (nz)
                  case (0);     gnbz = gz0
                  case (1);     gnbz = gz1
                  case (2);     gnbz = g0+g0 + (5 * g1 + g2z) * z * z
                  case default
                    tn2 = z**(nz-2)
                    tn = tn2*z*z
                    gnbz = nz*(nz-1)*g0*tn2 + ((nz+nz+1) * g1 + g2z) * tn
                end select

                do j = 1,nx
                   gnby = x*gnby    ! Do the cartesian (x_i)^j, i/=k part of
                   gnbz = x*gnbz    ! gaussian power of the xyz part not equal
                end do! to deriv. component k
                do j = 1,ny
                   gnbx = y*gnbx
                   gnbz = y*gnbz
                end do
                do j = 1,nz
                   gnbx = z*gnbx
                   gnby = z*gnby
                end do

                facb = fac(b)
                g(n,b,1) = gnbx*facb ! Basis fn. normalisation factor
                g(n,b,2) = gnby*facb
                g(n,b,3) = gnbz*facb
             end do
          end do
          call destroy_(fac)
          call destroy_(nn)
      end select
     STOP_TIMER("SHELL1:make_laplacian_grid")
      CHECK
   end subroutine

   subroutine make_laplacian_grid_1(self,g,h,i,pt)
    SHELL1 :: self
   ! Return "g(j,n,1:3)", the value of the second derivatives
   ! (d/dx2,d/dy2,d/dz2) of the shell component n on grid point j given a
   ! set of grid points "pt(j,1:3)". Also return "h(j,n,1:3)", the value of the
   ! first derivative, and return "i(n)", the value of the gaussian
      REALMAT(:,:) :: pt
      REALMAT3(:,:,:) :: g,h
      REALMAT(:,:) :: i
      REALVEC(:), PTR :: fac
      INTMAT(:,:), PTR :: nn
      INT :: n_pt,n,b,p,j,nx,ny,nz
      REAL :: aa,a2,x,y,z,rr,val
      REAL :: g0,g1,g2,g2x,g2y,g2z
      REAL :: gnbx,gnby,gnbz,x2,y2,z2
      REAL :: gx0,gx1,gy0,gy1,gz0,gz1,facb,inb
      REAL :: hnbx,hnby,hnbz,hx1,hy1,hz1,h1x,h1y,h1z
      REAL :: tn,tn1,tn2,twog0
      STACK("SHELL1:make_laplacian_grid_1")
      START_TIMER("SHELL1:make_laplacian_grid_1")
      ENSURE(size(pt,2)==3,"SHELL1:make_laplacian_grid_1 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      select case (self%l)
        case (0)
          do n = 1,n_pt             ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc          ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 + val
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g(n,1,1) = g1 + g2*x2
            g(n,1,2) = g1 + g2*y2
            g(n,1,3) = g1 + g2*z2
            h(n,1,1) = g1*x
            h(n,1,2) = g1*y
            h(n,1,3) = g1*z
            i(n,1)   = g0
          end do
        case (1)
          do n = 1,n_pt             ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc          ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 + val
              g1 = g1 - a2*val
              g2 = g2 + a2*a2*val
            end do
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2
            g(n,1,1) = x * (3 * g1 + g2x)
            g(n,1,2) = x * (g1 + g2y)
            g(n,1,3) = x * (g1 + g2z)
            g(n,2,1) = y * (g1 + g2x)
            g(n,2,2) = y * (3 * g1 + g2y)
            g(n,2,3) = y * (g1 + g2z)
            g(n,3,1) = z * (g1 + g2x)
            g(n,3,2) = z * (g1 + g2y)
            g(n,3,3) = z * (3 * g1 + g2z)
            h1x = g1*x
            h1y = g1*y
            h1z = g1*z
            h(n,1,1) = g0+x2*g1
            h(n,1,2) = x*h1y
            h(n,1,3) = x*h1z
            h(n,2,1) = y*h1x
            h(n,2,2) = g0+y2*g1
            h(n,2,3) = y*h1z
            h(n,3,1) = z*h1x
            h(n,3,2) = z*h1y
            h(n,3,3) = g0+z2*g1
            i(n,1)   = g0*x
            i(n,2)   = g0*y
            i(n,3)   = g0*z
          end do
        case default
          call create_(nn,3,self%n_comp);   call make_gaussian_xyz_powers_(self%l,nn)
          call create_(fac,self%n_comp);    call normalising_factors_(fac,self%l)
          do n = 1,n_pt             ! Do the exponential part of the gaussian
            x=pt(n,1) - self%pos(1); y=pt(n,2) - self%pos(2); z=pt(n,3) - self%pos(3)
            x2 = x*x;   y2 = y*y;   z2 = z*z
            rr = x2 + y2 + z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc          ! Loop over primitives
              aa = self%ex(p)
              a2 = aa+aa
              val = self%cc(p)*exp(-aa*rr)
              g0 = g0 + val
              g1 = g1 - aa*val
              g2 = g2 + aa*aa*val
!              g1 = g1 - a2*val
!              g2 = g2 + a2*a2*val
            end do
            g1 = g1 * TWO
            g2 = g2 * FOUR
            g2x = g2 * x2
            g2y = g2 * y2
            g2z = g2 * z2

            ! Some that are used multiple times.
            ! Taken out of loop over n_comp.
            gx0 = g1 + g2x
            gy0 = g1 + g2y
            gz0 = g1 + g2z
            gx1 = (THREE * g1 + g2x)*x  ! *x^1
            gy1 = (THREE * g1 + g2y)*y  ! *y^1
            gz1 = (THREE * g1 + g2z)*z  ! *z^1
            h1x = g1*x
            h1y = g1*y
            h1z = g1*z
            hx1 = g0 + x2*g1
            hy1 = g0 + y2*g1
            hz1 = g0 + z2*g1

            do b = 1,self%n_comp        ! Loop over basis functions
                inb = g0
                nx = nn(1,b)
                ny = nn(2,b)
                nz = nn(3,b)

                select case (nx)
                  case (0);     gnbx = gx0
                                hnbx = h1x
                  case (1);     gnbx = gx1
                                hnbx = hx1
                  case (2)
                    tn = x*x
                    twog0 = g0+g0
                    gnbx = twog0 + (5 * g1 + g2x)*tn
                    hnbx = twog0*x + h1x*tn
                  case default
                    tn2 = x**(nx-2)
                    tn1 = tn2*x
                    tn = tn2*x*x
                    gnbx = nx*(nx-1)*g0*tn2 + ((2*nx+1) * g1 + g2x)*tn
                    hnbx = nx*g0*tn1 + h1x*tn
                end select
                select case (ny)
                  case (0);     gnby = gy0
                                hnby = h1y
                  case (1);     gnby = gy1
                                hnby = hy1
                  case (2)
                    tn = y*y
                    twog0 = g0+g0
                    gnby = twog0 + (5 * g1 + g2y)*tn
                    hnby = twog0*y + h1y*tn
                  case default
                    tn2 = y**(ny-2)
                    tn1 = tn2*y
                    tn = tn2*y*y
                    gnby = ny*(ny-1)*g0*tn2 + ((2*ny+1) * g1 + g2y)*tn
                    hnby = ny*g0*tn1 + h1y*tn
                end select
                select case (nz)
                  case (0);     gnbz = gz0
                                hnbz = h1z
                  case (1);     gnbz = gz1
                                hnbz = hz1
                  case (2)
                    tn = z*z
                    twog0 = g0+g0
                    gnbz = twog0 + (5 * g1 + g2z)*tn
                    hnbz = twog0*z + h1z*tn
                  case default
                    tn2 = z**(nz-2)
                    tn1 = tn2*z
                    tn = tn2*z*z
                    gnbz = nz*(nz-1)*g0*tn2 + ((2*nz+1) * g1 + g2z)*tn
                    hnbz = nz*g0*tn1 + h1z*tn
                end select

                do j = 1,nx
                   gnby = x*gnby    ! Do the cartesian (x_i)^j, i/=k part of
                   gnbz = x*gnbz    ! gaussian power of the xyz part not equal
                   hnby = x*hnby
                   hnbz = x*hnbz
                   inb  = x*inb
                end do! to deriv. component k
                do j = 1,ny
                   gnbx = y*gnbx
                   gnbz = y*gnbz
                   hnbx = y*hnbx
                   hnbz = y*hnbz
                   inb  = y*inb
                end do
                do j = 1,nz
                   gnbx = z*gnbx
                   gnby = z*gnby
                   hnbx = z*hnbx
                   hnby = z*hnby
                   inb  = z*inb
                end do

                facb = fac(b)
                g(n,b,1) = gnbx*facb ! Basis fn. normalisation factor
                g(n,b,2) = gnby*facb
                g(n,b,3) = gnbz*facb
                h(n,b,1) = hnbx*facb
                h(n,b,2) = hnby*facb
                h(n,b,3) = hnbz*facb
                i(n,b)   = inb *facb
             end do
          end do
          call destroy_(fac)
          call destroy_(nn)
      end select
     STOP_TIMER("SHELL1:make_laplacian_grid_1")
      CHECK
   end subroutine

end
