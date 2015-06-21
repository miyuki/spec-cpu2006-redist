!-------------------------------------------------------------------------------
!
! OPMATRIX: Operator matrices.
!
! Provide a basis set (matrix) representation of a one-electron quantum
! mechanical operator. Can cope with polymorphic types of basis representations,
! including restricted, unrestricted, and general basis orbital matrices.
! Also, complex types for all these matrices.
!
! Basically, its a polymorphic matrix type.
!
! Copyright (C) Dylan Jayatilaka 1998
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
! $Id: opmatrix.foo,v 1.25.2.2 2003/09/11 07:51:31 reaper Exp $
!-------------------------------------------------------------------------------

module OPMATRIX_MODULE

#  include "opmatrix.use"

   implicit none

#  include "macros"
#  include "opmatrix.int"


contains

   subroutine create(self,n_bf)
    OPMATRIX :: self
   ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      PTR :: self
      INT, IN, optional :: n_bf
      STACK("OPMATRIX:create")
      START_TIMER("OPMATRIX:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(OPMATRIX_SIZE)
      call nullify_ptr_part_(self)
      call set_(self,n_bf)
     STOP_TIMER("OPMATRIX:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,n_bf,opmatkind)
    OPMATRIX :: self
   ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      PTR :: self
      INT, IN :: n_bf
      STR(*) :: opmatkind
      STACK("OPMATRIX:create_1")
      START_TIMER("OPMATRIX:create_1")
      call create_(self,n_bf)
      call create_(self,opmatkind)
     STOP_TIMER("OPMATRIX:create_1")
      UNSTACK
   end subroutine

   subroutine create_copy(self,m)
    OPMATRIX :: self
   ! Create an opmatrix object
      PTR :: self
       OPMATRIX, IN :: m
      STACK("OPMATRIX:create_copy")
      START_TIMER("OPMATRIX:create_copy")
      call create_(self,m%n_bf)
      call set_to_(self,m)
     STOP_TIMER("OPMATRIX:create_copy")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    OPMATRIX :: self
   ! Destroy an opmatrix object
      PTR :: self
      STACK("OPMATRIX:destroy")
      START_TIMER("OPMATRIX:destroy")
      if (NOT associated(self)) then; STOP_TIMER("OPMATRIX:destroy") UNSTACK return; end if
      call destroy_ptr_part_(self)
      DELETE_MEMORY(OPMATRIX_SIZE)
      deallocate(self)
     STOP_TIMER("OPMATRIX:destroy")
      UNSTACK
   end subroutine

   subroutine create_2(self,opmatkind)
    OPMATRIX :: self
   ! Create the appropriate spinorbital kind the opmatrix
      STR(*), IN :: opmatkind
      INT :: n_bf
      STACK("OPMATRIX:create_2")
      START_TIMER("OPMATRIX:create_2")
      n_bf = self%n_bf
   ENSURE(n_bf > 0,"OPMATRIX:create_2 ... bad number of basis functions.")
      select case (opmatkind)
         case ("restricted");           call create_(self%restricted,n_bf,n_bf)
         case ("unrestricted");         call create_(self%alpha,n_bf,n_bf)
                                        call create_(self%beta,n_bf,n_bf)
         case ("alpha");                call create_(self%alpha,n_bf,n_bf)
         case ("beta");                 call create_(self%beta,n_bf,n_bf)
         case ("general");              call create_(self%general,2*n_bf,2*n_bf)
         case ("restricted_complex");   call create_(self%restricted_complex,n_bf,n_bf)
         case ("unrestricted_complex"); call create_(self%alpha_complex,n_bf,n_bf)
                                        call create_(self%beta_complex,n_bf,n_bf)
         case ("alpha_complex");        call create_(self%alpha_complex,n_bf,n_bf)
         case ("beta_complex");         call create_(self%beta_complex,n_bf,n_bf)
         case ("general_complex");      call create_(self%general_complex,2*n_bf,2*n_bf)
         case default;   DIE("OPMATRIX:create_2 ... unknown kind, "//trim(opmatkind))
      end select
     STOP_TIMER("OPMATRIX:create_2")
      UNSTACK
   end subroutine

   subroutine destroy_1(self,opmatkind)
    OPMATRIX :: self
   ! Destroy the appropriate spinorbital kind of the opmatrix
      STR(*) :: opmatkind
      STACK("OPMATRIX:destroy_1")
      START_TIMER("OPMATRIX:destroy_1")
      select case (opmatkind)
         case ("restricted");           call destroy_(self%restricted)
         case ("unrestricted");         call destroy_(self%alpha); call destroy_(self%beta)
         case ("alpha");                call destroy_(self%alpha)
         case ("beta");                 call destroy_(self%beta)
         case ("general");              call destroy_(self%general)
         case ("restricted_complex");   call destroy_(self%restricted_complex)
         case ("unrestricted_complex"); call destroy_(self%alpha_complex)
                                        call destroy_(self%beta_complex)
         case ("alpha_complex");        call destroy_(self%alpha_complex)
         case ("beta_complex");         call destroy_(self%beta_complex)
         case ("general_complex");      call destroy_(self%general_complex)
         case ("all");                  call destroy_ptr_part_(self)
         case default;   DIE("OPMATRIX:destroy_1 ... unknown kind, "//trim(opmatkind))
      end select
     STOP_TIMER("OPMATRIX:destroy_1")
      UNSTACK
   end subroutine

   subroutine nullify_ptr_part(self)
    OPMATRIX :: self
   ! Nullify the pointer parts of the opmatrix object
      STACK("OPMATRIX:nullify_ptr_part")
      START_TIMER("OPMATRIX:nullify_ptr_part")
      nullify(self%restricted)
      nullify(self%alpha)
      nullify(self%beta)
      nullify(self%general)
      nullify(self%restricted_complex)
      nullify(self%alpha_complex)
      nullify(self%beta_complex)
      nullify(self%general_complex)
      nullify(self%triangle)
      nullify(self%square)
     STOP_TIMER("OPMATRIX:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    OPMATRIX :: self
   ! Destroy the pointer parts of the opmatrix object
      STACK("OPMATRIX:destroy_ptr_part")
      START_TIMER("OPMATRIX:destroy_ptr_part")
      call destroy_(self%restricted)
      call destroy_(self%alpha)
      call destroy_(self%beta)
      call destroy_(self%general)
      call destroy_(self%restricted_complex)
      call destroy_(self%alpha_complex)
      call destroy_(self%beta_complex)
      call destroy_(self%general_complex)
      call destroy_(self%triangle)
      call destroy_(self%square)
     STOP_TIMER("OPMATRIX:destroy_ptr_part")
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

   function any_created(self) result(res)
    OPMATRIX :: self
   ! Returns true if any component of self has been created
      BIN :: res
      BIN, dimension(6) :: c
      STACK("OPMATRIX:any_created")
      START_TIMER("OPMATRIX:any_created")
      c(1) = associated(self%restricted)
      c(2) = associated(self%alpha)
      c(3) = associated(self%general)
      c(4) = associated(self%restricted_complex)
      c(5) = associated(self%alpha_complex)
      c(6) = associated(self%general_complex)
      res = any(c)
     STOP_TIMER("OPMATRIX:any_created")
      UNSTACK
   end function

   function all_destroyed(self) result(res)
    OPMATRIX :: self
   ! Returns true if all components of self have been destroyed
      BIN :: res
       BIN, dimension(6) :: d
      STACK("OPMATRIX:all_destroyed")
      START_TIMER("OPMATRIX:all_destroyed")
      d(1) = NOT associated(self%restricted)
      d(2) = NOT associated(self%alpha)
      d(3) = NOT associated(self%general)
      d(4) = NOT associated(self%restricted_complex)
      d(5) = NOT associated(self%alpha_complex)
      d(6) = NOT associated(self%general_complex)
      res = all(d)
     STOP_TIMER("OPMATRIX:all_destroyed")
      UNSTACK
   end function

   function created(self,opmatkind) result(res)
    OPMATRIX :: self
   ! Returns true if subkind "opmatkind" of self has been created
      PTR :: self
      STR(*) :: opmatkind
      BIN :: res
      STACK("OPMATRIX:created")
      START_TIMER("OPMATRIX:created")
      if (NOT associated(self)) then
                                           res = FALSE; STOP_TIMER("OPMATRIX:created") UNSTACK return
      else
         select case (opmatkind)
            case ("restricted");           res = associated(self%restricted)
            case ("unrestricted");         res = associated(self%alpha)
            case ("general");              res = associated(self%general)
            case ("restricted_complex");   res = associated(self%restricted_complex)
            case ("unrestricted_complex"); res = associated(self%alpha_complex)
            case ("general_complex");      res = associated(self%general_complex)
            case default;   DIE("OPMATRIX:created ... unknown kind, "//trim(opmatkind))
         end select
      end if
     STOP_TIMER("OPMATRIX:created")
      UNSTACK
   end function

   function destroyed(self,opmatkind) result(res)
    OPMATRIX :: self
   ! Returns true if subkind "opmatkind" of self has *not* been created
     STR(*) :: opmatkind
     BIN :: res
       STACK("OPMATRIX:destroyed")
       START_TIMER("OPMATRIX:destroyed")
       select case (opmatkind)
         case ("restricted");           res = NOT associated(self%restricted)
         case ("unrestricted");         res = NOT associated(self%alpha)
         case ("general");              res = NOT associated(self%general)
         case ("restricted_complex");   res = NOT associated(self%restricted_complex)
         case ("unrestricted_complex"); res = NOT associated(self%alpha_complex)
         case ("general_complex");      res = NOT associated(self%general_complex)
         case default;   DIE("OPMATRIX:destroyed ... unknown kind, "//trim(opmatkind))
       end select
     STOP_TIMER("OPMATRIX:destroyed")
      UNSTACK
   end function

   subroutine set(self,n_bf)
    OPMATRIX :: self
   ! Set with "n_bf" spatial orbitals
      INT, IN, optional :: n_bf
      STACK("OPMATRIX:set")
      START_TIMER("OPMATRIX:set")
      call set_defaults_(self)
      if (present(n_bf)) self%n_bf = n_bf
     STOP_TIMER("OPMATRIX:set")
      CHECK
   end subroutine

   subroutine set_defaults(self)
    OPMATRIX :: self
   ! Set defaults
      STACK("OPMATRIX:set_defaults")
      START_TIMER("OPMATRIX:set_defaults")
      self%n_bf = 0
     STOP_TIMER("OPMATRIX:set_defaults")
      CHECK
   end subroutine

   subroutine set_to(self,m)
    OPMATRIX :: self
   ! Set self to "m".
      OPMATRIX, IN :: m
     INT :: old_n_bf
     STACK("OPMATRIX:set_to")
     START_TIMER("OPMATRIX:set_to")
     old_n_bf = self%n_bf
     call set_(self,m%n_bf)
     if (associated(m%restricted)) then
        if (old_n_bf /= self%n_bf)     call destroy_(self,"restricted")
        if (NOT associated(self%restricted)) call create_(self,"restricted")
        self%restricted = m%restricted
     end if
     if (associated(m%alpha)) then
        if (old_n_bf /= self%n_bf) call destroy_(self,"alpha")
        if (NOT associated(self%alpha))  call create_(self,"alpha")
        self%alpha = m%alpha
     end if
     if (associated(m%beta)) then
        if (old_n_bf /= self%n_bf) call destroy_(self,"beta")
        if (NOT associated(self%beta))   call create_(self,"beta")
        self%beta = m%beta
     end if
     if (associated(m%general)) then
        if (old_n_bf /= self%n_bf)  call destroy_(self,"general")
        if (NOT associated(self%general)) call create_(self,"general")
        self%general = m%general
     end if
     if (associated(m%restricted_complex)) then
        if (old_n_bf /= self%n_bf)                call destroy_(self,"restricted_complex")
        if (destroyed_(self,"restricted_complex")) call create_(self,"restricted_complex")
        self%restricted_complex = m%restricted_complex
     end if
     if (associated(m%alpha_complex)) then
        if (old_n_bf /= self%n_bf)           call destroy_(self,"alpha_complex")
        if (destroyed_(self,"alpha_complex")) call create_(self,"alpha_complex")
        self%alpha_complex = m%alpha_complex
     end if
     if (associated(m%beta_complex)) then
        if (old_n_bf /= self%n_bf)          call destroy_(self,"beta_complex")
        if (destroyed_(self,"beta_complex")) call create_(self,"beta_complex")
        self%beta_complex = m%beta_complex
     end if
     if (associated(m%general_complex)) then
        if (old_n_bf /= self%n_bf)             call destroy_(self,"general_complex")
        if (destroyed_(self,"general_complex")) call create_(self,"general_complex")
        self%general_complex = m%general_complex
     end if
     STOP_TIMER("OPMATRIX:set_to")
      UNSTACK
   end subroutine

   function spinorbital_kind(self) result(res)
    OPMATRIX :: self
   ! Return the kind of spinorbitals used in the representation
   ! The simplest spinorbital kind in use is the one returned.
      STR(STR_SIZE) :: res
      STACK("OPMATRIX:spinorbital_kind")
      START_TIMER("OPMATRIX:spinorbital_kind")
      if      (associated(self%restricted) )         then; res = "restricted"
      else if (associated(self%alpha) )              then; res = "unrestricted"
      else if (associated(self%general) )            then; res = "general"
      else if (associated(self%restricted_complex) ) then; res = "restricted_complex"
      else if (associated(self%alpha_complex) )      then; res = "unrestricted_complex"
      else if (associated(self%general_complex) )    then; res = "general_complex"
      else; DIE("OPMATRIX:spinorbital_kind ... no object created")
      end if
     STOP_TIMER("OPMATRIX:spinorbital_kind")
      CHECK
   end function

   function number_kind(self) result(res)
    OPMATRIX :: self
   ! Return the kind of numbers used in the representation
      STR(STR_SIZE) :: res
      STACK("OPMATRIX:number_kind")
      START_TIMER("OPMATRIX:number_kind")
      if      ( associated(self%restricted) )         then; res = "real"
      else if ( associated(self%alpha) )              then; res = "real"
      else if ( associated(self%general) )            then; res = "real"
      else if ( associated(self%restricted_complex) ) then; res = "complex"
      else if ( associated(self%alpha_complex) )      then; res = "complex"
      else if ( associated(self%general_complex) )    then; res = "complex"
      else; DIE("OPMATRIX:number_kind ... no object created")
      end if
     STOP_TIMER("OPMATRIX:number_kind")
      CHECK
   end function

   function guess_scf_kind(self) result(res)
    OPMATRIX :: self
   ! Guess the kind of SCF to be used from the spinorbitals used in the
   ! representation.
      STR(STR_SIZE) :: res
      STACK("OPMATRIX:guess_scf_kind")
      START_TIMER("OPMATRIX:guess_scf_kind")
      if      ( associated(self%restricted) )         then; res = "rhf"
      else if ( associated(self%alpha) )              then; res = "uhf"
      else if ( associated(self%general) )            then; res = "ghf"
      else if ( associated(self%restricted_complex) ) then; res = "rchf"
      else if ( associated(self%alpha_complex) )      then; res = "uchf"
      else if ( associated(self%general_complex) )    then; res = "gchf"
      else; DIE("OPMATRIX:guess_scf_kind ... no object created")
      end if
     STOP_TIMER("OPMATRIX:guess_scf_kind")
      CHECK
   end function

   subroutine zero(self)
    OPMATRIX :: self
   ! Set self to zero
      STR(STR_SIZE) :: opmatkind
      STACK("OPMATRIX:zero")
      START_TIMER("OPMATRIX:zero")
      opmatkind = spinorbital_kind_(self)
      select case(opmatkind)
         case("restricted");           self%restricted = ZERO
         case("unrestricted");         self%alpha = ZERO; self%beta = ZERO
         case("alpha");                self%alpha = ZERO
         case("beta");                 self%beta = ZERO
         case("general");              self%general = ZERO
         case("restricted_complex");   self%restricted = ZERO
         case("unrestricted_complex"); self%alpha_complex = ZERO
                                       self%beta_complex = ZERO
         case("alpha_complex");        self%alpha_complex = ZERO
         case("beta_complex");         self%beta_complex = ZERO
         case("general_complex");      self%general_complex = ZERO
         case default;   DIE("OPMATRIX:zero ... unknown kind, "//trim(opmatkind))
      end select
     STOP_TIMER("OPMATRIX:zero")
      CHECK
   end subroutine

   subroutine convert_to(self,newkind,factor)
    OPMATRIX :: self
   ! Convert self to a new basis kind "newkind".  To convert MO's please use
   ! the next routine.
      STR(*) :: newkind
      REAL, optional :: factor
      REAL :: fac
      STR(STR_SIZE) :: oldkind
      STACK("OPMATRIX:convert_to")
      START_TIMER("OPMATRIX:convert_to")
      fac = ONE
      if (present(factor)) fac = factor
      oldkind = spinorbital_kind_(self)
      if (newkind==oldkind) then; STOP_TIMER("OPMATRIX:convert_to") UNSTACK return; end if
      call create_(self,newkind)
      select case (oldkind)
         case("restricted")
            select case (newkind)
               case("unrestricted")
                  self%alpha = fac*self%restricted
                  self%beta  = fac*self%restricted
               case("general")
                  self%general = ZERO
                  call alpha_alpha_set_to_(self%general,self%restricted,fac)
                  call beta_beta_set_to_(self%general,self%restricted,fac)
               case("restricted_complex")
                  self%restricted_complex = self%restricted
               case("unrestricted_complex")
                  self%alpha_complex = fac*self%restricted
                  self%beta_complex  = fac*self%restricted
               case("general_complex")
                  self%general_complex = ZERO
                  call alpha_alpha_set_to_(self%general_complex,self%restricted,fac)
                  call beta_beta_set_to_(self%general_complex,self%restricted,fac)
               case default
                  DIE("OPMATRIX:convert_to ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
            end select
         case("unrestricted")
            select case (newkind)
               case("restricted")
                  self%restricted = fac*self%alpha
               case("general")
                  self%general = ZERO
                  call alpha_alpha_set_to_(self%general,self%alpha)
                  call beta_beta_set_to_(self%general,self%beta)
               case("unrestricted_complex")
                  self%alpha_complex = self%alpha
                  self%beta_complex  = self%beta
               case("general_complex")
                  self%general_complex = ZERO
                  call alpha_alpha_set_to_(self%general_complex,self%alpha)
                  call beta_beta_set_to_(self%general_complex,self%beta)
               case default
                  DIE("OPMATRIX:convert_to ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
            end select
         case("general")
            select case (newkind)
               case("unrestricted")
                  self%alpha = alpha_alpha_(self%general)
                  self%beta  = beta_beta_(self%general)
               case("general_complex")
                  self%general_complex = self%general
               case default
                  DIE("OPMATRIX:convert_to ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
            end select
         case default; DIE("OPMATRIX:convert_to ... cant convert old kind "//trim(oldkind))
      end select
      call destroy_(self,oldkind)
     STOP_TIMER("OPMATRIX:convert_to")
      UNSTACK
   end subroutine

   subroutine convert_to_1(self,newkind,na,nb,quantization_axis)
    OPMATRIX :: self
   ! Convert self (which is regarded to be a set of MO's) to a
   ! new basis kind "newkind"
     STR(*) :: newkind
     INT :: na,nb
     REALVEC(3), optional :: quantization_axis
     BIN :: quantize
     STR(STR_SIZE) :: oldkind
     INT :: d,ne
     REAL :: lam,tmp,ar,br,bi
     STACK("OPMATRIX:convert_to_1")
     START_TIMER("OPMATRIX:convert_to_1")
     d = self%n_bf
     ne = na + nb
     oldkind = spinorbital_kind_(self)
     if (newkind==oldkind) then; STOP_TIMER("OPMATRIX:convert_to_1") UNSTACK return; end if
     call create_(self,newkind)
     select case (oldkind)
       case("restricted")
         select case (newkind)
           case("unrestricted")
             self%alpha = self%restricted
             self%beta  = self%restricted
           case("general")
             self%general = ZERO
             self%general(  1:d,     1:2*nb-1:2) = self%restricted(:,   1:nb)
             self%general(d+1: ,     2:2*nb  :2) = self%restricted(:,   1:nb)
             self%general(  1:d,2*nb+1:ne      ) = self%restricted(:,nb+1:na)
             self%general(d+1: ,  ne+1:2*na    ) = self%restricted(:,nb+1:na)
             self%general(1:d  ,2*na+1:2*d -1:2) = self%restricted(:,na+1: d)
             self%general(d+1: ,2*na+2:2*d   :2) = self%restricted(:,na+1: d)
           case("restricted_complex")
             self%restricted_complex = self%restricted
           case("unrestricted_complex")
             self%alpha_complex = self%restricted
             self%beta_complex  = self%restricted
           case("general_complex")
             self%general_complex = ZERO
             self%general_complex(  1:d,     1:2*nb-1:2) = self%restricted(:,   1:nb)
             self%general_complex(d+1: ,     2:2*nb  :2) = self%restricted(:,   1:nb)
             self%general_complex(  1:d,2*nb+1:ne      ) = self%restricted(:,nb+1:na)
             self%general_complex(d+1: ,  ne+1:2*na    ) = self%restricted(:,nb+1:na)
             self%general_complex(  1:d,2*na+1:2*d -1:2) = self%restricted(:,na+1: d)
             self%general_complex(d+1: ,2*na+2:2*d   :2) = self%restricted(:,na+1: d)
           case default
             DIE("OPMATRIX:convert_to_1 ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
         end select
       case("unrestricted")
         select case (newkind)
           case("restricted","alpha")
             self%restricted = self%alpha ! Loss of information here
             WARN("OPMATRIX:convert_to_1 ... Conversion from unrestricted to restricted ignores beta orbitals")
           case("beta")
             self%restricted = self%beta  ! Loss of information here
             WARN("OPMATRIX:convert_to_1 ... Conversion from unrestricted to restricted ignores alpha orbitals")
           case("general")
             self%general = ZERO
             self%general(  1:d,     1:2*nb-1:2) = self%alpha(:,   1:nb)
             self%general(d+1: ,     2:2*nb  :2) = self%beta (:,   1:nb)
             self%general(  1:d,2*nb+1:ne      ) = self%alpha(:,nb+1:na)
             self%general(d+1: ,  ne+1:2*na    ) = self%beta (:,nb+1:na)
             self%general(  1:d,2*na+1:2*d -1:2) = self%alpha(:,na+1: d)
             self%general(d+1: ,2*na+2:2*d   :2) = self%beta (:,na+1: d)
           case("unrestricted_complex")
             self%alpha_complex = self%alpha
             self%beta_complex  = self%beta
           case("general_complex")
             self%general_complex = ZERO
             quantize = FALSE
             if (present(quantization_axis)) &
               quantize = NOT equals_(quantization_axis,(/ZERO,ZERO,ONE/))
             if (quantize) then
               lam = norm_(quantization_axis)
               tmp = abs(quantization_axis(3)+lam) ! always positive anyway
               if (equals_(tmp,ZERO)) then
                  ar = ZERO
                  br = ONE
                  bi = ZERO
               else
                  ar = sqrt(tmp/(TWO*lam))
                  br = ONE/sqrt(TWO*lam*tmp)
                  bi = br
                  br = br*quantization_axis(1)
                  bi = bi*quantization_axis(2)
               end if
               self%general_complex(d+1: ,     1:  na) = cmplx(br,bi,kind=CPX_KIND)*self%alpha(:,   1:na)
               self%general_complex(  1:d,     1:  na) =           ar*self%alpha(:,   1:na)
               self%general_complex(d+1: ,  ne+1:nb+d) = cmplx(br,bi,kind=CPX_KIND)*self%alpha(:,na+1: d)
               self%general_complex(  1:d,  ne+1:nb+d) =           ar*self%alpha(:,na+1: d)
               br = -br
               self%general_complex(  1:d,  na+1:ne  ) = cmplx(br,bi,kind=CPX_KIND)*self%beta (:,   1:nb)
               self%general_complex(d+1: ,  na+1:ne  ) =           ar*self%beta (:,   1:nb)
               self%general_complex(  1:d,nb+d+1:    ) = cmplx(br,bi,kind=CPX_KIND)*self%beta (:,nb+1: d)
               self%general_complex(d+1: ,nb+d+1:    ) =           ar*self%beta (:,nb+1: d)
             else
               self%general_complex(  1:d,     1:na  ) = self%alpha(:,   1:na)
               self%general_complex(d+1: ,  na+1:ne  ) = self%beta (:,   1:nb)
               self%general_complex(  1:d,  ne+1:nb+d) = self%alpha(:,na+1:  )
               self%general_complex(d+1: ,nb+d+1:    ) = self%beta (:,nb+1:  )
             end if
    call flush_(stdout)
    call text_(stdout,"alpha orbs:"); call put_(stdout,self%alpha)
    call flush_(stdout)
    call text_(stdout,"beta  orbs:"); call put_(stdout,self%beta)
    call flush_(stdout)
    call text_(stdout,"cmplx orbs:"); call put_(stdout,self%general_complex)

           case default
             DIE("OPMATRIX:convert_to_1 ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
         end select
       case("general")
         select case (newkind)
           case("unrestricted")
             call alpha_alpha_put_to_(self%general,self%alpha)
             call beta_beta_put_to_(self%general,self%beta)
             WARN("OPMATRIX:convert_to_1 ... Conversion from general to unrestricted not recommended!")
           case("general_complex")
             self%general_complex = self%general
           case default
             DIE("OPMATRIX:convert_to_1 ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
         end select
       case default; DIE("OPMATRIX:convert_to_1 ... cant convert old kind "//trim(oldkind))
     end select
     call destroy_(self,oldkind)
     STOP_TIMER("OPMATRIX:convert_to_1")
      UNSTACK
   end subroutine

   function l_compress(self,opmatkind) result(res)
    OPMATRIX :: self
   ! Return the length of the compressed object
      INT :: res
      STR(*), optional :: opmatkind
       STR(STR_SIZE) :: k
      STACK("OPMATRIX:l_compress")
      START_TIMER("OPMATRIX:l_compress")
      if (present(opmatkind)) then; k = opmatkind
      else;                    k = spinorbital_kind_(self)
      end if
      select case (k)
         case("restricted");           res = HALF*self%n_bf*(self%n_bf+1)
         case("unrestricted");         res = self%n_bf*(self%n_bf+1)
         case("general");              res = self%n_bf*(2*self%n_bf+1)
         case("restricted_complex");   res = self%n_bf*self%n_bf
         case("unrestricted_complex"); res = 2*self%n_bf*self%n_bf
         case("general_complex");      res = 4*self%n_bf*self%n_bf
         case default;   DIE("OPMATRIX:l_compress ... unknown kind, "//trim(k))
      end select
     STOP_TIMER("OPMATRIX:l_compress")
      CHECK
   end function

   subroutine compress(self,opmatkind)
    OPMATRIX :: self
   ! Compress the opmatrix into a triangle or square vector to save space
      STR(*), optional :: opmatkind
       STR(STR_SIZE) :: k
      STACK("OPMATRIX:compress")
      START_TIMER("OPMATRIX:compress")
      k = spinorbital_kind_(self)
      if (present(opmatkind)) k = opmatkind
      select case (k)
         case("restricted")
            call destroy_(self%triangle)
            call create_(self%triangle,l_compress_(self,k))
            call compress_to_triangle_(self%restricted,self%triangle)
            call destroy_(self%restricted)
         case("unrestricted")
            call destroy_(self%triangle)
            call create_(self%triangle,l_compress_(self,k))
            call compress_to_triangle_(self%alpha, alpha_(self%triangle))
            call compress_to_triangle_(self%beta, beta_(self%triangle))
            call destroy_(self%alpha)
            call destroy_(self%beta)
         case("general")
            call destroy_(self%triangle)
            call create_(self%triangle,l_compress_(self,k))
            call compress_to_triangle_(self%general,self%triangle)
         case("restricted_complex")
            call destroy_(self%square)
            call create_(self%square,l_compress_(self,k))
            call compress_to_square_(self%restricted_complex,self%square)
            call destroy_(self%restricted_complex)
         case("unrestricted_complex")
            call destroy_(self%square)
            call create_(self%square,l_compress_(self,k))
            call compress_to_square_(self%alpha_complex, alpha_(self%square))
            call compress_to_square_(self%beta_complex, beta_(self%square))
            call destroy_(self%alpha_complex)
            call destroy_(self%beta_complex)
         case("general_complex")
            call destroy_(self%square)
            call create_(self%square,l_compress_(self,k))
            call compress_to_square_(self%general_complex, self%square)
            call destroy_(self%general_complex)
      end select
     STOP_TIMER("OPMATRIX:compress")
      UNSTACK
   end subroutine

   subroutine uncompress(self)
    OPMATRIX :: self
   ! Uncompress the opmatrix
      INT :: l_compress
      STACK("OPMATRIX:uncompress")
      START_TIMER("OPMATRIX:uncompress")
      l_compress = -1
      if (associated(self%triangle)) l_compress = size(self%triangle)
      if (associated(self%square))   l_compress = size(self%square)
      if (l_compress==l_compress_(self,"restricted")) then
            call destroy_(self,"restricted")
            call create_(self,"restricted")
            call uncompress_from_triangle_(self%restricted,self%triangle)
            call destroy_(self%triangle)
      else if (l_compress==l_compress_(self,"unrestricted")) then
            call destroy_(self,"unrestricted")
            call create_(self,"unrestricted")
            call uncompress_from_triangle_(self%alpha, alpha_(self%triangle))
            call uncompress_from_triangle_(self%beta, beta_(self%triangle))
            call destroy_(self%triangle)
      else if (l_compress==l_compress_(self,"general")) then
            call destroy_(self,"general")
            call create_(self,"general")
            call uncompress_from_triangle_(self%general,self%triangle)
            call destroy_(self%triangle)
      else if (l_compress==l_compress_(self,"restricted_complex")) then
            call destroy_(self,"restricted_complex")
            call create_(self,"restricted_complex")
            call uncompress_from_square_(self%restricted_complex,self%square)
            call destroy_(self%square)
      else if (l_compress==l_compress_(self,"unrestricted_complex")) then
            call destroy_(self,"unrestricted_complex")
            call create_(self,"unrestricted_complex")
            call uncompress_from_square_(self%alpha_complex, alpha_(self%square))
            call uncompress_from_square_(self%beta_complex, beta_(self%square))
            call destroy_(self%square)
      else if (l_compress==l_compress_(self,"general_complex")) then
            call destroy_(self,"general_complex")
            call create_(self,"general_complex")
            call uncompress_from_square_(self%general_complex,self%square)
            call destroy_(self%square)
      end if
     STOP_TIMER("OPMATRIX:uncompress")
      UNSTACK
   end subroutine

   subroutine schmidt_orthonormalise(self,S,scale)
    OPMATRIX :: self
   ! Schmidt Orthonormalise self.  (For molecular orbitals).
      REALMAT(:,:), IN :: S
      REAL, optional :: scale
      STR(STR_SIZE) :: opmatkind
      INT :: n
      REALMAT(:,:), PTR :: SS
      STACK("OPMATRIX:schmidt_orthonormalise")
      START_TIMER("OPMATRIX:schmidt_orthonormalise")
      opmatkind = spinorbital_kind_(self)
      select case(opmatkind)
         case("restricted")
                  call schmidt_orthonormalise_(self%restricted,S,scale)
         case("unrestricted")
                  call schmidt_orthonormalise_(self%alpha,S)
                  call schmidt_orthonormalise_(self%beta,S)
         case("alpha")
                  call schmidt_orthonormalise_(self%alpha,S,scale)
         case("beta")
                  call schmidt_orthonormalise_(self%beta,S,scale)
         case("general")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = ZERO
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call schmidt_orthonormalise_(self%general,SS,scale)
                  call destroy_(SS)
         case("restricted_complex")
                  call schmidt_orthonormalise_(self%restricted,S,scale)
         case("unrestricted_complex")
                  call schmidt_orthonormalise_(self%alpha_complex,S)
                  call schmidt_orthonormalise_(self%beta_complex,S)
         case("alpha_complex")
                  call schmidt_orthonormalise_(self%alpha_complex,S,scale)
         case("beta_complex")
                  call schmidt_orthonormalise_(self%beta_complex,S,scale)
         case("general_complex")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = ZERO
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call schmidt_orthonormalise_(self%general_complex,SS,scale)
                  call destroy_(SS)
         case default;   DIE("OPMATRIX:schmidt_orthonormalise ... unknown kind, "//trim(opmatkind))
      end select
     STOP_TIMER("OPMATRIX:schmidt_orthonormalise")
      CHECK
   end subroutine

   subroutine symmetrically_orthonormalise(self,S)
    OPMATRIX :: self
   ! Symmetrically orthonormalise self.  (For molecular orbitals).
   ! "S" is the AO overlap matrix
       REALMAT(:,:), IN :: S
      STR(STR_SIZE) :: opmatkind
       INT :: n
      REALMAT(:,:), PTR :: SS
      STACK("OPMATRIX:symmetrically_orthonormalise")
      START_TIMER("OPMATRIX:symmetrically_orthonormalise")
      opmatkind = spinorbital_kind_(self)
      select case(opmatkind)
         case("restricted")
                  call symmetrically_orthonormalise_(self%restricted,S)
         case("unrestricted")
                  call symmetrically_orthonormalise_(self%alpha,S)
                  call symmetrically_orthonormalise_(self%beta,S)
         case("alpha")
                  call symmetrically_orthonormalise_(self%alpha,S)
         case("beta")
                  call symmetrically_orthonormalise_(self%beta,S)
         case("general")
                  ENSURE(size(SS,1)==2*size(S,1),"OPMATRIX:symmetrically_orthonormalise ... wrong size, S")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = ZERO
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call symmetrically_orthonormalise_(self%general,SS)
                  call destroy_(SS)
         case("restricted_complex")
                  call symmetrically_orthonormalise_(self%restricted,S)
         case("unrestricted_complex")
                  call symmetrically_orthonormalise_(self%alpha_complex,S)
                  call symmetrically_orthonormalise_(self%beta_complex,S)
         case("alpha_complex")
                  call symmetrically_orthonormalise_(self%alpha_complex,S)
         case("beta_complex")
                  call symmetrically_orthonormalise_(self%beta_complex,S)
         case("general_complex")
                  ENSURE(size(SS,1)==2*size(S,1),"OPMATRIX:symmetrically_orthonormalise ... wrong size, S")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = ZERO
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call symmetrically_orthonormalise_(self%general_complex,SS)
                  call destroy_(SS)
         case default;   DIE("OPMATRIX:symmetrically_orthonormalise ... unknown kind, "//trim(opmatkind))
      end select
     STOP_TIMER("OPMATRIX:symmetrically_orthonormalise")
      CHECK
   end subroutine

   subroutine plus(self,X)
    OPMATRIX :: self
   ! Add X to self.
      OPMATRIX, IN :: X
     STR(STR_SIZE) :: opmatkind
   STACK("OPMATRIX:plus")
   START_TIMER("OPMATRIX:plus")
   ENSURE(spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:plus ... incompatible kinds")
     opmatkind = spinorbital_kind_(X)
     select case(opmatkind)
       case("restricted")
         call plus_(self%restricted,X%restricted)
       case("unrestricted")
         call plus_(self%alpha,X%alpha)
         call plus_(self%beta,X%beta)
       case("alpha")
         call plus_(self%alpha,X%alpha)
       case("beta")
         call plus_(self%beta,X%beta)
       case("general")
         call plus_(self%general,X%general)
       case("restricted_complex")
         call plus_(self%restricted,X%restricted)
       case("unrestricted_complex")
         call plus_(self%alpha_complex,X%alpha_complex)
         call plus_(self%beta_complex,X%beta_complex)
       case("alpha_complex")
         call plus_(self%alpha_complex,X%alpha_complex)
       case("beta_complex")
         call plus_(self%beta_complex,X%beta_complex)
       case("general_complex")
         call plus_(self%general_complex,X%general_complex)
       case default;   DIE("OPMATRIX:plus ... unknown kind, "//trim(opmatkind))
     end select
     STOP_TIMER("OPMATRIX:plus")
      CHECK
   end subroutine

   subroutine minus(self,X)
    OPMATRIX :: self
   ! Subtract X from self.
      OPMATRIX, IN :: X
     STR(STR_SIZE) :: opmatkind
   STACK("OPMATRIX:minus")
   START_TIMER("OPMATRIX:minus")
   ENSURE(spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:minus ... incompatible kinds")
     opmatkind = spinorbital_kind_(self)
     select case(opmatkind)
       case("restricted")
         call minus_(self%restricted,X%restricted)
       case("unrestricted")
         call minus_(self%alpha,X%alpha)
         call minus_(self%beta,X%beta)
       case("alpha")
         call minus_(self%alpha,X%alpha)
       case("beta")
         call minus_(self%beta,X%beta)
       case("general")
         call minus_(self%general,X%general)
       case("restricted_complex")
         call minus_(self%restricted,X%restricted)
       case("unrestricted_complex")
         call minus_(self%alpha_complex,X%alpha_complex)
         call minus_(self%beta_complex,X%beta_complex)
       case("alpha_complex")
         call minus_(self%alpha_complex,X%alpha_complex)
       case("beta_complex")
         call minus_(self%beta_complex,X%beta_complex)
       case("general_complex")
         call minus_(self%general_complex,X%general_complex)
       case default; DIE("OPMATRIX:minus ... unknown spinorbital kind, " // trim(opmatkind))
     end select
     STOP_TIMER("OPMATRIX:minus")
      CHECK
   end subroutine

   subroutine to_scaled(self,fac,X)
    OPMATRIX :: self
   ! Set self to scaled X.
      OPMATRIX, IN :: X
     REAL, IN :: fac
     STR(STR_SIZE) :: opmatkind
   STACK("OPMATRIX:to_scaled")
   START_TIMER("OPMATRIX:to_scaled")
   ENSURE(spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:to_scaled ... incompatible kinds")
     opmatkind = spinorbital_kind_(self)
     select case(opmatkind)
       case("restricted")
         call to_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted")
         call to_scaled_mat_(self%alpha,fac,X%alpha)
         call to_scaled_mat_(self%beta,fac,X%beta)
       case("alpha")
         call to_scaled_mat_(self%alpha,fac,X%alpha)
       case("beta")
         call to_scaled_mat_(self%beta,fac,X%beta)
       case("general")
         call to_scaled_mat_(self%general,fac,X%general)
       case("restricted_complex")
         call to_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted_complex")
         call to_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
         call to_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("alpha_complex")
         call to_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
       case("beta_complex")
         call to_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("general_complex")
         call to_scaled_mat_(self%general_complex,fac,X%general_complex)
       case default;   DIE("OPMATRIX:to_scaled ... unknown kind, "//trim(opmatkind))
     end select
     STOP_TIMER("OPMATRIX:to_scaled")
      CHECK
   end subroutine

   subroutine plus_scaled(self,fac,X)
    OPMATRIX :: self
   ! Set self to scaled X.
      OPMATRIX, IN :: X
     REAL, IN :: fac
     STR(STR_SIZE) :: opmatkind
   STACK("OPMATRIX:plus_scaled")
   START_TIMER("OPMATRIX:plus_scaled")
   ENSURE(spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:plus_scaled ... incompatible kinds")
     opmatkind = spinorbital_kind_(self)
     select case(opmatkind)
       case("restricted")
         call plus_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted")
         call plus_scaled_mat_(self%alpha,fac,X%alpha)
         call plus_scaled_mat_(self%beta,fac,X%beta)
       case("alpha")
         call plus_scaled_mat_(self%alpha,fac,X%alpha)
       case("beta")
         call plus_scaled_mat_(self%beta,fac,X%beta)
       case("general")
         call plus_scaled_mat_(self%general,fac,X%general)
       case("restricted_complex")
         call plus_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted_complex")
         call plus_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
         call plus_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("alpha_complex")
         call plus_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
       case("beta_complex")
         call plus_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("general_complex")
         call plus_scaled_mat_(self%general_complex,fac,X%general_complex)
       case default;   DIE("OPMATRIX:plus_scaled ... unknown kind, "//trim(opmatkind))
     end select
     STOP_TIMER("OPMATRIX:plus_scaled")
      CHECK
   end subroutine

   subroutine damp(self,old,mix)
    OPMATRIX :: self
   ! Constructs the new self matrix by combining it with the "old" one using a
   ! "mix" factor, i.e.  self = (1-mix)*self + mix*old.
     INOUT :: self
     OPMATRIX, IN :: old
     REAL, IN :: mix
     STACK("OPMATRIX:damp")
     START_TIMER("OPMATRIX:damp")
     call to_scaled_(self,ONE-mix,self)
     call plus_scaled_(self,mix,old)
     STOP_TIMER("OPMATRIX:damp")
      CHECK
   end subroutine

   function expectation(self,X) result(res)
    OPMATRIX :: self
   ! Get the expectation value of the matrix "X", i.e:
   !    res = Trace ( X  .self )
      REALMAT(:,:) :: X
      REAL :: res
      STR(STR_SIZE) :: opmatkind
   STACK("OPMATRIX:expectation")
   START_TIMER("OPMATRIX:expectation")
   ENSURE(size(X,1)==self%n_bf,"OPMATRIX:expectation ... wrong X dimension")
   ENSURE(is_square_(X),"OPMATRIX:expectation ... X is not square")
      opmatkind = spinorbital_kind_(self)
      select case (opmatkind)
        case ("restricted")
           res = trace_of_product_(self%restricted,X)
        case ("unrestricted")
           res = trace_of_product_(self%alpha,X)
           res =  trace_of_product_(self%beta,X) + res
        case ("general_complex")
           res = trace_of_product_(self%general_complex,X)
        case default; DIE("OPMATRIX:expectation ... not implemented, "//trim(opmatkind))
      end select
     STOP_TIMER("OPMATRIX:expectation")
      CHECK
   end function

end
