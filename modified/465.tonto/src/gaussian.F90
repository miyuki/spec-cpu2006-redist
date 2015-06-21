!-------------------------------------------------------------------------------
!
! For describing a single gaussian function :: GAUSSIAN.
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
! $Id: gaussian.foo,v 1.7.2.1 2003/03/06 10:40:56 dylan Exp $
!-------------------------------------------------------------------------------

module GAUSSIAN_MODULE

#  include "gaussian.use"

   implicit none

#  include "macros"
#  include "gaussian.int"


contains

   subroutine create(self)
    GAUSSIAN :: self
   ! Create a gaussian object
      PTR :: self
      STACK("GAUSSIAN:create")
      START_TIMER("GAUSSIAN:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(GAUSSIAN_SIZE)
     STOP_TIMER("GAUSSIAN:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,l,pos,ex)
    GAUSSIAN :: self
   ! Create a gaussian object
      PTR :: self
       INT :: l
      REALVEC(3) :: pos
      REAL :: ex
      STACK("GAUSSIAN:create_1")
      START_TIMER("GAUSSIAN:create_1")
      nullify(self)
      allocate(self)
      ADD_MEMORY(GAUSSIAN_SIZE)
      call set_(self,l,pos,ex)
     STOP_TIMER("GAUSSIAN:create_1")
      UNSTACK
   end subroutine

   subroutine create_2(self,l,Rx,Ry,Rz,ex)
    GAUSSIAN :: self
   ! Create a gaussian object
      PTR :: self
       INT :: l
      REAL :: Rx,Ry,Rz
      REAL :: ex
      STACK("GAUSSIAN:create_2")
      START_TIMER("GAUSSIAN:create_2")
      nullify(self)
      allocate(self)
      ADD_MEMORY(GAUSSIAN_SIZE)
      call set_(self,l,Rx,Ry,Rz,ex)
     STOP_TIMER("GAUSSIAN:create_2")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    GAUSSIAN :: self
   ! Destroy a shell object
      PTR :: self
      STACK("GAUSSIAN:destroy")
      START_TIMER("GAUSSIAN:destroy")
      if (NOT associated(self)) then; STOP_TIMER("GAUSSIAN:destroy") UNSTACK return; end if
      DELETE_MEMORY(GAUSSIAN_SIZE)
      deallocate(self)
     STOP_TIMER("GAUSSIAN:destroy")
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

   subroutine create_copy(self,G)
    GAUSSIAN :: self
   ! Create a copy of "G"
      PTR :: self
       GAUSSIAN :: G
      STACK("GAUSSIAN:create_copy")
      START_TIMER("GAUSSIAN:create_copy")
      call create_(self)
      call copy_(self,G)
     STOP_TIMER("GAUSSIAN:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,G)
    GAUSSIAN :: self
   ! Set the gaussian to "G"
      GAUSSIAN :: G
      STACK("GAUSSIAN:copy")
      START_TIMER("GAUSSIAN:copy")
      self = G
     STOP_TIMER("GAUSSIAN:copy")
      CHECK
   end subroutine

   subroutine set(self,l,pos,ex)
    GAUSSIAN :: self
   ! Set a gaussian object
       INT :: l
      REALVEC(3) :: pos
      REAL :: ex
      STACK("GAUSSIAN:set")
      START_TIMER("GAUSSIAN:set")
      self%l = l
      self%pos = pos
      self%ex = ex
     STOP_TIMER("GAUSSIAN:set")
      CHECK
   end subroutine

   subroutine set_1(self,l,Rx,Ry,Rz,ex)
    GAUSSIAN :: self
   ! Create a gaussian object
       INT :: l
      REAL :: Rx,Ry,Rz
      REAL :: ex
      STACK("GAUSSIAN:set_1")
      START_TIMER("GAUSSIAN:set_1")
      self%l = l
      self%pos = (/Rx,Ry,Rz/)
      self%ex = ex
     STOP_TIMER("GAUSSIAN:set_1")
      CHECK
   end subroutine

   subroutine set_2(self,ex)
    GAUSSIAN :: self
   ! Set a gaussian object
      REAL :: ex
      STACK("GAUSSIAN:set_2")
      START_TIMER("GAUSSIAN:set_2")
      self%ex = ex
     STOP_TIMER("GAUSSIAN:set_2")
      CHECK
   end subroutine

   function l_chr(self) result(res)
    GAUSSIAN :: self
   ! Return a character representation for the angular mtm
      STR(1) :: res
       INT :: l
      STACK("GAUSSIAN:l_chr")
      START_TIMER("GAUSSIAN:l_chr")
      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            ENSURE(l<=23,"GAUSSIAN:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select
     STOP_TIMER("GAUSSIAN:l_chr")
      CHECK
   end function

!  **************
!  Output methods
!  **************

   subroutine put(self)
    GAUSSIAN :: self
   ! Put the gaussian information to file "stdout"
      STACK("GAUSSIAN:put")
      START_TIMER("GAUSSIAN:put")
      call show_(stdout,"L quantum number = ",self%l,real_width=TRUE)
      call show_(stdout,"Position         = ",self%pos)
      call show_(stdout,"Exponent         = ",self%ex)
     STOP_TIMER("GAUSSIAN:put")
      CHECK
   end subroutine

!  *************
!  Input methods
!  *************

   recursive subroutine read_keywords(self)
    GAUSSIAN :: self
   ! Read data from "stdin" using keyword style input.
   ! The following code is inherited from OBJECT
     STR(STR_SIZE) :: word
     STACK("GAUSSIAN:read_keywords")
     START_TIMER("GAUSSIAN:read_keywords")
     ENSURE(next_item_(stdin)=="{","GAUSSIAN:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                 ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do
     STOP_TIMER("GAUSSIAN:read_keywords")
      UNSTACK
   end subroutine

   subroutine process_keyword(self,keyword)
    GAUSSIAN :: self
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      STACK("GAUSSIAN:process_keyword")
      START_TIMER("GAUSSIAN:process_keyword")
      word = keyword
      call to_lower_case_(word)
      select case (word)
         case ("}     ");   ! exit case
         case ("ex=   ");   call read_ex_(self)
         case ("l=    ");   call read_l_(self)
         case ("l_chr=");   call read_l_chr_(self)
         case ("l_int=");   call read_l_int_(self)
         case ("pos=  ");   call read_pos_(self)
         case ("put   ");   call put_(self)
         case ("units=");   call read_units_(self)
         case default;      allocate(tonto%known_keywords(8))
         tonto%known_keywords(1) = "}     "
         tonto%known_keywords(2) = "ex=   "
         tonto%known_keywords(3) = "l=    "
         tonto%known_keywords(4) = "l_chr="
         tonto%known_keywords(5) = "l_int="
         tonto%known_keywords(6) = "pos=  "
         tonto%known_keywords(7) = "put   "
         tonto%known_keywords(8) = "units="
         call unknown_(tonto,word,"GAUSSIAN:process_keyword")
         deallocate(tonto%known_keywords)
      end select
     STOP_TIMER("GAUSSIAN:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_units(self)
    GAUSSIAN :: self
   ! Read a string which describes the units to be used
   ! The following code is inherited from OBJECT
      STACK("GAUSSIAN:read_units")
      START_TIMER("GAUSSIAN:read_units")
      call set_default_units_(stdin,next_str_(stdin))
     STOP_TIMER("GAUSSIAN:read_units")
      CHECK
   end subroutine

   subroutine read_junk(self)
    GAUSSIAN :: self
   ! Read in a junk string, useful for ignoring a field
   ! The following code is inherited from OBJECT
      STACK("GAUSSIAN:read_junk")
      START_TIMER("GAUSSIAN:read_junk")
      call skip_next_item_(stdin)
     STOP_TIMER("GAUSSIAN:read_junk")
      CHECK
   end subroutine

   subroutine read_l(self)
    GAUSSIAN :: self
   ! Read in the l symbol
      STR(STR_SIZE) :: word
      STACK("GAUSSIAN:read_l")
      START_TIMER("GAUSSIAN:read_l")
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if
     STOP_TIMER("GAUSSIAN:read_l")
      CHECK
   end subroutine

   subroutine read_l_int(self)
    GAUSSIAN :: self
   ! Read in the l integer
      STR(STR_SIZE) :: word
      STACK("GAUSSIAN:read_l_int")
      START_TIMER("GAUSSIAN:read_l_int")
      call read_(stdin,word)
      ENSURE(is_int_(word),"GAUSSIAN:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)
     STOP_TIMER("GAUSSIAN:read_l_int")
      CHECK
   end subroutine

   subroutine read_l_chr(self)
    GAUSSIAN :: self
   ! Read in the l symbol
      STR(STR_SIZE) :: word
      STR(1) :: l_c
      INT :: l
      STACK("GAUSSIAN:read_l_chr")
      START_TIMER("GAUSSIAN:read_l_chr")
      call read_(stdin,word)
      ENSURE(len_trim(word)==1,"GAUSSIAN:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s"); l = 0
         case ("p"); l = 1
         case ("d"); l = 2
         case ("f"); l = 3
         case ("g"); l = 4
         case default;
            DIE_IF(l_c<"g","GAUSSIAN:read_l_chr ... unknown angular momentum character: "//l_c)
            l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l
     STOP_TIMER("GAUSSIAN:read_l_chr")
      CHECK
   end subroutine

   subroutine read_ex(self)
    GAUSSIAN :: self
   ! Read in the exponents
      STACK("GAUSSIAN:read_ex")
      START_TIMER("GAUSSIAN:read_ex")
      call read_(stdin,self%ex)
     STOP_TIMER("GAUSSIAN:read_ex")
      CHECK
   end subroutine

   subroutine read_pos(self)
    GAUSSIAN :: self
   ! Read in the position
      STACK("GAUSSIAN:read_pos")
      START_TIMER("GAUSSIAN:read_pos")
      call read_(stdin,self%pos)
     STOP_TIMER("GAUSSIAN:read_pos")
      CHECK
   end subroutine

!*****************
!  Inquiry methods
!*****************

   PURE function l(self) result(res)
    GAUSSIAN :: self
   ! Return the angular momentum l
      IN :: self
      INT :: res
      res = self%l
     STOP_TIMER("GAUSSIAN:l")
   end function

   PURE function n_comp(self) result(res)
    GAUSSIAN :: self
   ! Return the number of components in the gaussian shell.
      IN :: self
      INT :: res
      res = (self%l+1)*(self%l+2)/2
     STOP_TIMER("GAUSSIAN:n_comp")
   end function

   PURE function pos(self) result(res)
    GAUSSIAN :: self
   ! Return the position of the shell
      IN :: self
      REALVEC(3) :: res
      res = self%pos
     STOP_TIMER("GAUSSIAN:pos")
   end function

   PURE function pos_1(self,comp) result(res)
    GAUSSIAN :: self
   ! Return the position of the shell
      IN :: self
      INT, IN :: comp
      REAL :: res
      res = self%pos(comp)
     STOP_TIMER("GAUSSIAN:pos_1")
   end function

   PURE function ex(self) result(res)
    GAUSSIAN :: self
   ! Return the exponent vector pointer
      IN :: self
      REAL :: res
      res = self%ex
     STOP_TIMER("GAUSSIAN:ex")
   end function

   subroutine make_grid(self,g,pt)
    GAUSSIAN :: self
   ! Return "g(i,n)", the value of the gaussian component "n" on grid point "i"
   ! given a set of grid points "pt(i,1:3)"
      REALMAT(:,:), target :: pt
       REALMAT(:,:), target :: g
      REALVEC(:), PTR :: x,y,z
   STACK("GAUSSIAN:make_grid")
   START_TIMER("GAUSSIAN:make_grid")
   ENSURE(size(g,2)==n_comp_(self),"GAUSSIAN:make_grid ... incorrectly dimensioned")
      x => pt(:,1); y => pt(:,2); z => pt(:,3)
      call make_grid_(self,g,x,y,z)
     STOP_TIMER("GAUSSIAN:make_grid")
      CHECK
   end subroutine

   subroutine make_grid_1(self,g,x,y,z)
    GAUSSIAN :: self
   ! Return "g(i,n)", the value of the gaussian component "n" on grid point "i"
   ! given a set of grid points "(x(i),y(i),z(i))"
      REALVEC(:) :: x,y,z
      REALMAT(:,:) :: g
      INTVEC(:), PTR :: nx,ny,nz
      INT :: n_pt,n,b,j
      REAL :: rr,xx,yy,zz,posx,posy,posz,g0,gbn
      STACK("GAUSSIAN:make_grid_1")
      START_TIMER("GAUSSIAN:make_grid_1")
      ENSURE(size(g,2)==n_comp_(self),"GAUSSIAN:make_grid_1 ... incorrectly dimensioned")
      n_pt = size(x)
      posx = self%pos(1); posy = self%pos(2); posz = self%pos(3)
      select case (self%l)
        case (0)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             g(n,1) = exp( -rr * self%ex )
          end do
        case (1)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             g0 = exp( -rr * self%ex )
             g(n,1) = xx*g0
             g(n,2) = yy*g0
             g(n,3) = zz*g0
          end do
        case (2)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz
             g0 = exp( -rr * self%ex )
             g(n,1) = xx*xx*g0
             g(n,2) = yy*yy*g0
             g(n,3) = zz*zz*g0
             g(n,4) = xx*yy*g0
             g(n,5) = xx*zz*g0
             g(n,6) = yy*zz*g0
          end do
        case default
          call create_(nx,n_comp_(self)); call create_(ny,n_comp_(self)); call create_(nz,n_comp_(self))
          call make_gaussian_xyz_powers_(self%l,nx,ny,nz)
          do n = 1,n_pt
             xx=x(n) - posx; yy=y(n) - posy; zz=z(n) - posz
             rr = xx*xx + yy*yy + zz*zz    ! Do exponential part of the gaussian
             g0 = exp( -rr * self%ex )
             do b = 1,n_comp_(self)              ! Loop over all basis functions
                gbn = g0                   ! Combine exponential and cartesian
                do j = 1,nx(b)             ! power of the x^j part
                   gbn = xx*gbn
                end do
                do j = 1,ny(b)
                   gbn = yy*gbn
                end do
                do j = 1,nz(b)
                   gbn = zz*gbn
                end do
                g(n,b) = gbn
             end do
          end do
          call destroy_(nz); call destroy_(ny); call destroy_(nx)
     end select
     STOP_TIMER("GAUSSIAN:make_grid_1")
      CHECK
   end subroutine

end
