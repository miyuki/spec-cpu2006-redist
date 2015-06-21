!-------------------------------------------------------------------------------
!
! OPVECTOR: Diagonals of operator matrices, e.g. eigenvalues an occupation numbers
!
! Provide a basis set (matrix) representation of the diagonal of a one-electron
! quantum mechanical operator. Can cope with polymorphic types of basis
! representations, including restricted, unrestricted, and general basis
! orbitals. Complex types aren't needed sice operators are hermitian.
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
! $Id: opvector.foo,v 1.16.2.3 2003/09/18 09:27:36 dylan Exp $
!-------------------------------------------------------------------------------

module OPVECTOR_MODULE

#  include "opvector.use"

   implicit none

#  include "macros"
#  include "opvector.int"


contains

   subroutine create(self,n_bf,opveckind)
    OPVECTOR :: self
   ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      PTR :: self
      INT :: n_bf
      STR(*), optional :: opveckind
      STACK("OPVECTOR:create")
      START_TIMER("OPVECTOR:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(OPVECTOR_SIZE)
      call nullify_ptr_part_(self)
      call set_(self,n_bf)
      if (present(opveckind)) call create_(self,opveckind)
     STOP_TIMER("OPVECTOR:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    OPVECTOR :: self
   ! Destroy an opmatrix object
      PTR :: self
      STACK("OPVECTOR:destroy")
      START_TIMER("OPVECTOR:destroy")
      if (NOT associated(self)) then; STOP_TIMER("OPVECTOR:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(OPVECTOR_SIZE)
      deallocate(self)
     STOP_TIMER("OPVECTOR:destroy")
      UNSTACK
   end subroutine

   subroutine create_1(self,opveckind)
    OPVECTOR :: self
   ! Create the appropriate spinorbital kind of the opvector
      STR(*) :: opveckind
      INT :: n_bf
      STACK("OPVECTOR:create_1")
      START_TIMER("OPVECTOR:create_1")
      n_bf = self%n_bf
      ENSURE(self%n_bf>=0,"OPVECTOR:create_1 ... number of basis functions is undefined")
      select case (opveckind)
         case ("restricted","restricted_complex");     call create_(self%restricted,n_bf)
         case ("unrestricted","unrestricted_complex"); call create_(self%alpha,n_bf)
                                                       call create_(self%beta,n_bf)
         case ("alpha","alpha_complex");               call create_(self%alpha,n_bf)
         case ("beta","beta_complex");                 call create_(self%beta,n_bf)
         case ("general","general_complex");           call create_(self%general,2*n_bf)
         case default; DIE("OPVECTOR:create_1 ... unknown spinorbital kind, " // trim(opveckind))
      end select
     STOP_TIMER("OPVECTOR:create_1")
      UNSTACK
   end subroutine

   subroutine destroy_1(self,opveckind)
    OPVECTOR :: self
   ! Destroy the appropriate spinorbital kind of the opmatrix
      STR(*) :: opveckind
      STACK("OPVECTOR:destroy_1")
      START_TIMER("OPVECTOR:destroy_1")
      select case (opveckind)
         case ("restricted","restriced_complex");       call destroy_(self%restricted)
         case ("unrestricted","unrestricted_complex");  call destroy_(self%alpha); call destroy_(self%beta)
         case ("alpha","alpha_complex");                call destroy_(self%alpha)
         case ("beta","beta_complex");                  call destroy_(self%beta)
         case ("general","general_complex");            call destroy_(self%general)
         case default; DIE("OPVECTOR:destroy_1 ... unknown spinorbital kind, " // trim(opveckind))
      end select
     STOP_TIMER("OPVECTOR:destroy_1")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    OPVECTOR :: self
   ! Nullify the pointer parts of the opmatrix object
      STACK("OPVECTOR:nullify_ptr_part")
      START_TIMER("OPVECTOR:nullify_ptr_part")
      nullify(self%restricted)
      nullify(self%alpha)
      nullify(self%beta)
      nullify(self%general)
     STOP_TIMER("OPVECTOR:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    OPVECTOR :: self
   ! Destroy the pointer parts of the opmatrix object
      STACK("OPVECTOR:destroy_ptr_part")
      START_TIMER("OPVECTOR:destroy_ptr_part")
      call destroy_(self%restricted)
      call destroy_(self%alpha)
      call destroy_(self%beta)
      call destroy_(self%general)
     STOP_TIMER("OPVECTOR:destroy_ptr_part")
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

   function created(self,opveckind) result(res)
    OPVECTOR :: self
   ! Returns true if subkind "opveckind" of self has been created
      PTR :: self
      STR(*) :: opveckind
      BIN :: res
      STACK("OPVECTOR:created")
      START_TIMER("OPVECTOR:created")
      if (NOT associated(self)) then
                                    res = FALSE; STOP_TIMER("OPVECTOR:created") UNSTACK return
      else
         select case (opveckind)
            case ("restricted");    res = associated(self%restricted)
            case ("unrestricted");  res = associated(self%alpha)
            case ("general");       res = associated(self%general)
            case default; DIE("OPVECTOR:created ... unknown spinorbital kind, " // trim(opveckind))
         end select
      end if
     STOP_TIMER("OPVECTOR:created")
      UNSTACK
   end function

   function destroyed(self,opveckind) result(res)
    OPVECTOR :: self
   ! Returns true if subkind "opveckind" of self has *not* been created
     PTR :: self
     STR(*) :: opveckind
     BIN :: res
     STACK("OPVECTOR:destroyed")
     START_TIMER("OPVECTOR:destroyed")
     if (NOT associated(self)) then
                                 res = TRUE; STOP_TIMER("OPVECTOR:destroyed") UNSTACK return
     else
       select case (opveckind)
         case ("restricted");    res = NOT associated(self%restricted)
         case ("unrestricted");  res = NOT associated(self%alpha)
         case ("general");       res = NOT associated(self%general)
         case default; DIE("OPVECTOR:destroyed ... unknown spinorbital kind, " // trim(opveckind))
       end select
     end if
     STOP_TIMER("OPVECTOR:destroyed")
      UNSTACK
   end function

   function any_created(self) result(res)
    OPVECTOR :: self
   ! Returns true if any component of self has been created
      BIN :: res
       BIN, dimension(3) :: c
      STACK("OPVECTOR:any_created")
      START_TIMER("OPVECTOR:any_created")
      c(1) = associated(self%restricted)
      c(2) = associated(self%alpha)
      c(3) = associated(self%general)
      res = any(c)
     STOP_TIMER("OPVECTOR:any_created")
      UNSTACK
   end function

   function all_destroyed(self) result(res)
    OPVECTOR :: self
   ! Returns true if all components of self have been destroyed
      BIN :: res
       BIN, dimension(3) :: d
      STACK("OPVECTOR:all_destroyed")
      START_TIMER("OPVECTOR:all_destroyed")
      d(1) = NOT associated(self%restricted)
      d(2) = NOT associated(self%alpha)
      d(3) = NOT associated(self%general)
      res = all(d)
     STOP_TIMER("OPVECTOR:all_destroyed")
      UNSTACK
   end function

   subroutine create_copy(self,v)
    OPVECTOR :: self
   ! Create an opvector object
      PTR :: self
       OPVECTOR, IN :: v
      STACK("OPVECTOR:create_copy")
      START_TIMER("OPVECTOR:create_copy")
      call create_(self,v%n_bf)
      call set_to_(self,v)
     STOP_TIMER("OPVECTOR:create_copy")
      UNSTACK
   end subroutine

   subroutine set(self,n_bf)
    OPVECTOR :: self
   ! Set with "n_bf" spatial orbitals
      INT, IN :: n_bf
      STACK("OPVECTOR:set")
      START_TIMER("OPVECTOR:set")
      self%n_bf = n_bf
     STOP_TIMER("OPVECTOR:set")
      CHECK
   end subroutine

   subroutine set_to(self,v)
    OPVECTOR :: self
   ! Set self to "v".
       OPVECTOR :: v
      STACK("OPVECTOR:set_to")
      START_TIMER("OPVECTOR:set_to")
      call set_(self, v%n_bf)
      if ( all_destroyed_(v)) then; STOP_TIMER("OPVECTOR:set_to") UNSTACK return; end if
      if ( associated(v%restricted)) then
         call destroy_(self,"restricted"); call create_(self,"restricted")
         self%restricted = v%restricted
      end if
      if ( associated(v%alpha)) then
         call destroy_(self,"alpha"); call create_(self,"alpha")
          self%alpha = v%alpha
      end if
      if ( associated(v%beta)) then
         call destroy_(self,"beta"); call create_(self,"beta")
         self%beta = v%beta
      end if
      if ( associated(v%general)) then
         call destroy_(self,"general"); call create_(self,"general")
         self%general = v%general
      end if
     STOP_TIMER("OPVECTOR:set_to")
      UNSTACK
   end subroutine

   function spinorbital_kind(self) result(res)
    OPVECTOR :: self
   ! Return the kind of spinorbitals used in the representation.
   ! The simplest spinorbital kind in use is the one returned.
      STR(STR_SIZE) :: res
      STACK("OPVECTOR:spinorbital_kind")
      START_TIMER("OPVECTOR:spinorbital_kind")
      if      ( associated(self%restricted) ) then; res = "restricted"
      else if ( associated(self%alpha) )      then; res = "unrestricted"
      else if ( associated(self%general) )    then; res = "general"
      else; DIE("OPVECTOR:spinorbital_kind ... no object created")
      end if
     STOP_TIMER("OPVECTOR:spinorbital_kind")
      CHECK
   end function

   subroutine zero(self)
    OPVECTOR :: self
   ! Set self to zero
      STR(STR_SIZE) :: opveckind
      STACK("OPVECTOR:zero")
      START_TIMER("OPVECTOR:zero")
      opveckind = spinorbital_kind_(self)
      select case(opveckind)
         case("restricted");      self%restricted = ZERO
         case("unrestricted");    self%alpha = ZERO; self%beta = ZERO
         case("general");         self%general = ZERO
         case default; DIE("OPVECTOR:zero ... unknown spinorbital kind, " // trim(opveckind))
      end select
     STOP_TIMER("OPVECTOR:zero")
      CHECK
   end subroutine

   subroutine convert_to(self,newkind)
    OPVECTOR :: self
   ! Convert self to a new basis kind "newkind"
     STR(*) :: newkind
     STR(STR_SIZE) :: oldkind
     STACK("OPVECTOR:convert_to")
     START_TIMER("OPVECTOR:convert_to")
     oldkind = spinorbital_kind_(self)
     if (newkind==oldkind) then; STOP_TIMER("OPVECTOR:convert_to") UNSTACK return; end if
     call create_(self,newkind)
     select case (oldkind)
       case("restricted")
         select case (newkind)
           case("unrestricted")
             self%alpha = self%restricted
             self%beta  = self%restricted
           case("general")
             self%general = ZERO
             call set_alpha_(self%general,self%restricted)
             call set_beta_(self%general,self%restricted)
           case default
             DIE("OPVECTOR:convert_to ... cant convert kind " // trim(oldkind) // " to kind " // trim(newkind))
         end select
       case("unrestricted")
         select case (newkind)
           case("restricted")
             self%restricted = self%alpha
           case("general")
             call set_alpha_(self%general,self%alpha)
             call set_beta_(self%general,self%beta)
           case default
             DIE("OPVECTOR:convert_to ... cant convert kind " // trim(oldkind) // " to kind " // trim(newkind))
         end select
       case("general")
         select case (newkind)
           case("unrestricted")
             self%alpha = alpha_(self%general)
             self%beta  = beta_(self%general)
           case default
             DIE("OPVECTOR:convert_to ... cant convert kind " // trim(oldkind) // " to kind " // trim(newkind))
         end select
       case default; DIE("OPVECTOR:convert_to ... cant convert old kind " // trim(oldkind))
     end select
     call destroy_(self,oldkind)
     STOP_TIMER("OPVECTOR:convert_to")
      UNSTACK
   end subroutine

   function no_of_occupied(self,opveckind,tol) result(res)
    OPVECTOR :: self
   ! Returns the number of non-zero "occupied" elements, i.e. all those elements
   ! greater than TOL(7).  If "opveckind" is present, the number of occupied
   ! elements is returned for that kind (the default is determined by the
   ! .spinorbital_kind). if "tol" is present, it is used instead of TOL(7) to
   ! determine what is occupied.
      IN :: self
      STR(*), optional, IN :: opveckind
      REAL, optional, IN :: tol
      INT :: res
      STR(STR_SIZE) :: itemkind
      REAL :: eps
      STACK("OPVECTOR:no_of_occupied")
      START_TIMER("OPVECTOR:no_of_occupied")
      itemkind = spinorbital_kind_(self)
      if (present(opveckind)) itemkind = opveckind
      eps = TOL(7)
      if (present(tol)) eps = tol
      select case (itemkind)
         case ("restricted")
            ENSURE(associated(self%restricted),"OPVECTOR:no_of_occupied ... no restricted part")
            res = count(self%restricted>=eps)
         case ("unrestricted")
            ENSURE(associated(self%alpha),"OPVECTOR:no_of_occupied ... no alpha part")
            ENSURE(associated(self%beta),"OPVECTOR:no_of_occupied ... no beta part")
            res = count(self%alpha>=eps) + count(self%beta>=eps)
         case ("alpha")
            ENSURE(associated(self%alpha),"OPVECTOR:no_of_occupied ... no alpha part")
            res = count(self%alpha>=eps)
         case ("beta")
            ENSURE(associated(self%beta),"OPVECTOR:no_of_occupied ... no beta part")
            res = count(self%beta>=eps)
         case ("general")
            ENSURE(associated(self%general),"OPVECTOR:no_of_occupied ... no general part")
            res = count(self%general>=eps)
         case default
            DIE("OPVECTOR:no_of_occupied ... unknown kind, "//trim(itemkind))
      end select
     STOP_TIMER("OPVECTOR:no_of_occupied")
      CHECK
   end function

end
