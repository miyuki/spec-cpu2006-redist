!---------------------------------------------------------------------------
!
!  GAUSSIAN4 : Quartets of gaussian functions
!
! Copyright (C) Dylan Jayatilaka, 1999
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
! $Id: gaussian4.foo,v 1.19.2.1 2003/03/06 10:40:56 dylan Exp $
!
!---------------------------------------------------------------------------

module GAUSSIAN4_MODULE

#  include "gaussian4.use"

   implicit none

#  include "macros"
#  include "gaussian4.int"


contains

   subroutine create(self)
    GAUSSIAN4 :: self
   ! Create
      PTR :: self
      STACK("GAUSSIAN4:create")
      START_TIMER("GAUSSIAN4:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(GAUSSIAN4_SIZE)
     STOP_TIMER("GAUSSIAN4:create")
      UNSTACK
   end subroutine

   subroutine create_1(self,Ga,Gb,Gc,Gd)
    GAUSSIAN4 :: self
   ! Create and set to "Ga" ... "Gd"
      PTR :: self
      GAUSSIAN :: Ga,Gb,Gc,Gd
      STACK("GAUSSIAN4:create_1")
      START_TIMER("GAUSSIAN4:create_1")
      nullify(self)
      allocate(self)
      ADD_MEMORY(GAUSSIAN4_SIZE)
      call set_(self,Ga,Gb,Gc,Gd)
     STOP_TIMER("GAUSSIAN4:create_1")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    GAUSSIAN4 :: self
   ! Destroy
      PTR :: self
      STACK("GAUSSIAN4:destroy")
      START_TIMER("GAUSSIAN4:destroy")
      if (NOT associated(self)) then; STOP_TIMER("GAUSSIAN4:destroy") UNSTACK return; end if
      DELETE_MEMORY(GAUSSIAN4_SIZE)
      deallocate(self)
     STOP_TIMER("GAUSSIAN4:destroy")
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
    GAUSSIAN4 :: self
   ! Create a copy of "G"
      PTR :: self
       GAUSSIAN4 :: G
      STACK("GAUSSIAN4:create_copy")
      START_TIMER("GAUSSIAN4:create_copy")
      call create_(self)
      call copy_(self,G)
     STOP_TIMER("GAUSSIAN4:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,G)
    GAUSSIAN4 :: self
   ! Set the gaussian quartet object to "G"
      GAUSSIAN4 :: G
      STACK("GAUSSIAN4:copy")
      START_TIMER("GAUSSIAN4:copy")
      self = G
     STOP_TIMER("GAUSSIAN4:copy")
      CHECK
   end subroutine

   subroutine set(self,Ga,Gb,Gc,Gd)
    GAUSSIAN4 :: self
   ! Set the gaussian pair object to "Ga" and "Gb"
      GAUSSIAN :: Ga,Gb,Gc,Gd
      STACK("GAUSSIAN4:set")
      START_TIMER("GAUSSIAN4:set")
      self%a%l   = Ga%l
      self%a%pos = Ga%pos
      self%a%ex  = Ga%ex
      self%b%l   = Gb%l
      self%b%pos = Gb%pos
      self%b%ex  = Gb%ex
      self%c%l   = Gc%l
      self%c%pos = Gc%pos
      self%c%ex  = Gc%ex
      self%d%l   = Gd%l
      self%d%pos = Gd%pos
      self%d%ex  = Gd%ex
     STOP_TIMER("GAUSSIAN4:set")
      CHECK
   end subroutine

   subroutine set_1(self,l_a,pos_a,ex_a,l_b,pos_b,ex_b,l_c,pos_c,ex_c,l_d,pos_d,ex_d)
    GAUSSIAN4 :: self
   ! Set a gaussian object
      INT :: l_a,l_b,l_c,l_d
      REALVEC(3) :: pos_a,pos_b,pos_c,pos_d
      REAL :: ex_a,ex_b,ex_c,ex_d
      STACK("GAUSSIAN4:set_1")
      START_TIMER("GAUSSIAN4:set_1")
      self%a%l   = l_a
      self%a%pos = pos_a
      self%a%ex  = ex_a
      self%b%l   = l_b
      self%b%pos = pos_b
      self%b%ex  = ex_b
      self%c%l   = l_c
      self%c%pos = pos_c
      self%c%ex  = ex_c
      self%d%l   = l_d
      self%d%pos = pos_d
      self%d%ex  = ex_d
     STOP_TIMER("GAUSSIAN4:set_1")
      CHECK
   end subroutine

   subroutine set_2(self,ex_a,ex_b,ex_c,ex_d)
    GAUSSIAN4 :: self
   ! Set the exponents of the pair
      REAL, optional :: ex_a,ex_b,ex_c,ex_d
      STACK("GAUSSIAN4:set_2")
      START_TIMER("GAUSSIAN4:set_2")
      if (present(ex_a)) self%a%ex = ex_a
      if (present(ex_b)) self%b%ex = ex_b
      if (present(ex_c)) self%c%ex = ex_c
      if (present(ex_d)) self%d%ex = ex_d
     STOP_TIMER("GAUSSIAN4:set_2")
      CHECK
   end subroutine

   subroutine make_ERI_ints(self,I)
    GAUSSIAN4 :: self
   ! Make ERI matrix "I" using Rys decomposition of 1/r_{12}.
       REALMAT4(:,:,:,:) :: I
      REALMAT5(:,:,:,:,:), PTR :: Ix,Iy,Iz
      INTVEC(:), PTR :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      RYS, PTR :: rys
      REALVEC(3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      INT :: l_e,l_f,n_a,n_b,n_c,n_d,n_roots
      STACK("GAUSSIAN4:make_ERI_ints")
      START_TIMER("GAUSSIAN4:make_ERI_ints")
      l_e = self%a%l + self%b%l
      l_f = self%c%l + self%d%l
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,self%b%l+1,l_f+1,self%d%l+1)
      call create_(Iy,n_roots,l_e+1,self%b%l+1,l_f+1,self%d%l+1)
      call create_(Iz,n_roots,l_e+1,self%b%l+1,l_f+1,self%d%l+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
      !!!!!!!!!!!!!!!!!!!!
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1), rys%r, rys%w,rho,zinv,einv,PA,QC,QP)
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD)
      ! Form the integrals
      I = sum(Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)
      !!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      I = fac*I
     STOP_TIMER("GAUSSIAN4:make_ERI_ints")
      CHECK
   end subroutine

   PURE subroutine transfer_2d_ints(self,Ix,Iy,Iz,AB,CD,max_b,max_d)
    GAUSSIAN4 :: self
   ! Use the transfer relation to put momenta on centres B and D to get all
   ! the 2d integrals "Ix", "Iy" and "Iz". If present, "max_b" and "max_d"
   ! are the maximum l-values desired for centers b and d, respectively.
   ! You must ensure that the "Ii" arrays are big enough, in this case.
      IN :: self
      REALMAT5(:,:,:,:,:), INOUT :: Ix,Iy,Iz
      REALVEC(3), IN :: AB,CD
      INT, IN, optional :: max_b,max_d
      INT :: le1,lf1,l_a,l_b,l_c,l_d,a,b,c,d,a1,b1,c1,d1,la1
      l_a = self%a%l; l_b = self%b%l; l_c = self%c%l; l_d = self%d%l
      if (present(max_b)) l_b=max_b
      if (present(max_d)) l_d=max_d
      if (l_b==0 AND l_d==0) then; STOP_TIMER("GAUSSIAN4:transfer_2d_ints") return; end if
      if (l_b/=0) then
         le1 = l_a + l_b + 1
         do b = 1,l_b
         do a = 1,le1 - b
            b1 = b   + 1
            a1 = a   + 1
            Ix(:,a,b1,:,1) = Ix(:,a1,b,:,1) + AB(1)*Ix(:,a,b,:,1)
            Iy(:,a,b1,:,1) = Iy(:,a1,b,:,1) + AB(2)*Iy(:,a,b,:,1)
            Iz(:,a,b1,:,1) = Iz(:,a1,b,:,1) + AB(3)*Iz(:,a,b,:,1)
         end do
         end do
      end if
      if (l_d/=0) then
      !  la1 = l_a + 1
         la1 = l_a + l_b - self%b%l + 1 ! increase more along a as for b
         lf1 = l_c + l_d + 1
         do d = 1,l_d
         do c = 1,lf1 - d
            d1 = d   + 1
            c1 = c   + 1
            Ix(:,1:la1,:,c,d1) = Ix(:,1:la1,:,c1,d) + CD(1)*Ix(:,1:la1,:,c,d)
            Iy(:,1:la1,:,c,d1) = Iy(:,1:la1,:,c1,d) + CD(2)*Iy(:,1:la1,:,c,d)
            Iz(:,1:la1,:,c,d1) = Iz(:,1:la1,:,c1,d) + CD(3)*Iz(:,1:la1,:,c,d)
         end do
         end do
      end if
     STOP_TIMER("GAUSSIAN4:transfer_2d_ints")
   end subroutine

   PURE subroutine form_2d_ints(self,Ix,Iy,Iz,t2,wt,rho,zinv,einv,PA,QC,QP,max_e,max_f)
    GAUSSIAN4 :: self
   ! Forms the two dimensional integrals "Ix", "Iy" and "Iz" with momenta only
   ! on centres A and C, using Rys roots "t2" and weights "wt".
   ! Other variables are intermediates, to avoid calculations: see make_ERI_ints.
   ! If present, "max_e" and "max_f" are used for the maximum angular momenta
   ! of centers (a+b) and (c+d) respectively -- for derivative integrals.
     IN :: self
     REALMAT3(:,:,:), INOUT :: Ix,Iy,Iz
     REALVEC(:), IN :: t2,wt
     REAL, IN :: rho,zinv,einv
     REALVEC(3), IN :: PA,QC,QP
     INT, IN, optional :: max_e,max_f
     REALVEC(size(t2)) :: ret,rzt,ce,cf,bb,ce1,cf1
     INT :: l_e,l_f,e,f,e1,f1,fp1,ep1,k
     l_e = self%a%l + self%b%l
     l_f = self%c%l + self%d%l
     if (present(max_e)) l_e = max_e
     if (present(max_f)) l_f = max_f
     Ix(:,1,1) = ONE
     Iy(:,1,1) = ONE
     Iz(:,1,1) = ONE
     if (l_e/=0 OR l_f/=0) then
       if (l_f>0) then
         ret = rho*t2*einv
         Ix(:,1,2) = QC(1) - ret(:) * QP(1)
         Iy(:,1,2) = QC(2) - ret(:) * QP(2)
         Iz(:,1,2) = QC(3) - ret(:) * QP(3)
         if (l_f>1) then
           cf = (ONE - ret) * HALF * einv
           do f = 2,l_f
             f1  = f - 1
             fp1 = f + 1
             cf1 = f1 * cf
             Ix(:,1,fp1) = Ix(:,1,2) * Ix(:,1,f) + cf1 * Ix(:,1,f1)
             Iy(:,1,fp1) = Iy(:,1,2) * Iy(:,1,f) + cf1 * Iy(:,1,f1)
             Iz(:,1,fp1) = Iz(:,1,2) * Iz(:,1,f) + cf1 * Iz(:,1,f1)
           end do
         end if
       end if
       if (l_e>0) then
         rzt = rho * t2 * zinv
         Ix(:,2,1) = PA(1) + rzt(:) * QP(1)
         Iy(:,2,1) = PA(2) + rzt(:) * QP(2)
         Iz(:,2,1) = PA(3) + rzt(:) * QP(3)
         if (l_e>1) then
           ce = (ONE - rzt) * HALF * zinv
           do e = 2, l_e
             e1  = e - 1
             ep1 = e + 1
             ce1 = e1 * ce
             Ix(:,ep1,1) = Ix(:,2,1) * Ix(:,e,1) + ce1 * Ix(:,e1,1)
             Iy(:,ep1,1) = Iy(:,2,1) * Iy(:,e,1) + ce1 * Iy(:,e1,1)
             Iz(:,ep1,1) = Iz(:,2,1) * Iz(:,e,1) + ce1 * Iz(:,e1,1)
           end do
         end if
       end if
       if (l_f>0 AND l_e>0) then
         bb = HALF*einv*rzt
         Ix(:,2,2)=Ix(:,1,2)*Ix(:,2,1)+bb
         Iy(:,2,2)=Iy(:,1,2)*Iy(:,2,1)+bb
         Iz(:,2,2)=Iz(:,1,2)*Iz(:,2,1)+bb
         if (l_f>1) then
           do f=2,l_f
             f1  = f - 1
             fp1 = f + 1
             cf1 = f1 * cf
             Ix(:,2,fp1) = Ix(:,1,2) * Ix(:,2,f) + cf1 * Ix(:,2,f1) + bb *Ix(:,1,f)
             Iy(:,2,fp1) = Iy(:,1,2) * Iy(:,2,f) + cf1 * Iy(:,2,f1) + bb *Iy(:,1,f)
             Iz(:,2,fp1) = Iz(:,1,2) * Iz(:,2,f) + cf1 * Iz(:,2,f1) + bb *Iz(:,1,f)
           end do
         end if
         if (l_e>1) then
           do e = 2, l_e
             e1  =e - 1
             ep1 =e + 1
             ce1 =e1 * ce
             do f=2, l_f + 1
               f1 = f - 1
               Ix(:,ep1,f) = Ix(:,2,1)*Ix(:,e,f)+ce1*Ix(:,e1,f)+f1*bb(:)*Ix(:,e,f1)
               Iy(:,ep1,f) = Iy(:,2,1)*Iy(:,e,f)+ce1*Iy(:,e1,f)+f1*bb(:)*Iy(:,e,f1)
               Iz(:,ep1,f) = Iz(:,2,1)*Iz(:,e,f)+ce1*Iz(:,e1,f)+f1*bb(:)*Iz(:,e,f1)
             end do
           end do
         end if
       end if
     end if
     ! Multiply Iz by the weight
     do k = 1,size(Iz,1)
        Iz(k,:,:) = Iz(k,:,:)*wt(k)
     end do
     STOP_TIMER("GAUSSIAN4:form_2d_ints")
   end subroutine

   subroutine make_spin_orbit_ints(self,Sx,Sy,Sz,Ox,Oy,Oz)
    GAUSSIAN4 :: self
   ! Make the same-spin orbit integrals "Sx" "Sy" "Sz"  and the
   ! other spin orbit integrals  "Ox" "Oy" "Oz" using Rys method.
   ! Reference: Bearpark et al., Mol. Phys. 80, p. 479 (1993)
      REALMAT4(:,:,:,:) :: Sx,Sy,Sz,Ox,Oy,Oz
      REALMAT5(:,:,:,:,:), PTR :: Ix,Iy,Iz,LLx,LLy,LLz,RRx,RRy,RRz
      INTVEC(:), PTR :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      RYS, PTR :: rys
      REALVEC(3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      INT :: l_e,l_f,l_a,l_b,l_c,l_d,n_a,n_b,n_c,n_d,n_roots
      STACK("GAUSSIAN4:make_spin_orbit_ints")
      START_TIMER("GAUSSIAN4:make_spin_orbit_ints")
      l_a = self%a%l + 1; l_b = self%b%l + 1
      l_c = self%c%l + 1; l_d = self%d%l + 1
      l_e = self%a%l + self%b%l + 1; l_f = self%c%l + self%d%l + 1 ! One higherfor differentiating
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)   ! Basic intermediate integrals
      call create_(Iy,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      call create_(Iz,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Ix = ZERO; Iy = ZERO; Iz = ZERO
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1), rys%r, rys%w,rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                            ! one unit higher for differentiating
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD,max_b=l_b,max_d=l_d) ! a & c will automatically be one unit higher
      call create_(LLx,n_roots,l_a,l_b,l_c,l_d) ! Derivative integrals
      call create_(LLy,n_roots,l_a,l_b,l_c,l_d)
      call create_(LLz,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRx,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRy,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,Ix,"a",LLx); call differentiate_(self,Iy,"a",LLy); call differentiate_(self,Iz,"a",LLz)
      call differentiate_(self,Ix,"b",RRx); call differentiate_(self,Iy,"b",RRy); call differentiate_(self,Iz,"b",RRz)
      ! Form the same-spin orbit integrals

      Sx = sum(Ix(:,ax,bx,cx,dx)*(LLy(:,ay,by,cy,dy)*RRz(:,az,bz,cz,dz)-RRy(:,ay,by,cy,dy)*LLz(:,az,bz,cz,dz)),dim=1)
      Sy = sum(Iy(:,ay,by,cy,dy)*(LLz(:,az,bz,cz,dz)*RRx(:,ax,bx,cx,dx)-RRz(:,az,bz,cz,dz)*LLx(:,ax,bx,cx,dx)),dim=1)
      Sz = sum(Iz(:,az,bz,cz,dz)*(LLx(:,ax,bx,cx,dx)*RRy(:,ay,by,cy,dy)-RRx(:,ax,bx,cx,dx)*LLy(:,ay,by,cy,dy)),dim=1)

!     do d=1,n_d
!       dix=dx(d)
!       diy=dy(d)
!       diz=dz(d)
!       do c=1,n_c
!         cix=cx(c)
!         ciy=cy(c)
!         ciz=cz(c)
!         do b=1,n_b
!           bix=bx(b)
!           biy=by(b)
!           biz=bz(b)
!           do a=1,n_a
!             aix=ax(a)
!             aiy=ay(a)
!             aiz=az(a)
!             Sxn=ZERO
!             Syn=ZERO
!             Szn=ZERO
!             do n=1,n_roots
!               LLxn = LLx(n,aix,bix,cix,dix)
!               LLyn = LLy(n,aiy,biy,ciy,diy)
!               LLzn = LLz(n,aiz,biz,ciz,diz)
!               RRxn = RRx(n,aix,bix,cix,dix)
!               RRyn = RRy(n,aiy,biy,ciy,diy)
!               RRzn = RRz(n,aiz,biz,ciz,diz)
!               Sxn = Sxn + Ix(n,aix,bix,cix,dix)*(LLyn*RRzn - RRyn*LLzn)
!               Syn = Syn + Iy(n,aiy,biy,ciy,diy)*(LLzn*RRxn - RRzn*LLxn)
!               Szn = Szn + Iz(n,aiz,biz,ciz,diz)*(LLxn*RRyn - RRxn*LLyn)
!             end
!             Sx(a,b,c,d)=Sxn
!             Sy(a,b,c,d)=Syn
!             Sz(a,b,c,d)=Szn
!           end
!         end
!       end
!     end

      call differentiate_(self,Ix,"c",LLx); call differentiate_(self,Iy,"c",LLy); call differentiate_(self,Iz,"c",LLz)
      call differentiate_(self,Ix,"d",RRx); call differentiate_(self,Iy,"d",RRy); call differentiate_(self,Iz,"d",RRz)
      ! Form the other spin orbit integrals

      Ox = sum(Ix(:,ax,bx,cx,dx)*(LLy(:,ay,by,cy,dy)*RRz(:,az,bz,cz,dz)-RRy(:,ay,by,cy,dy)*LLz(:,az,bz,cz,dz)),dim=1)
      Oy = sum(Iy(:,ay,by,cy,dy)*(LLz(:,az,bz,cz,dz)*RRx(:,ax,bx,cx,dx)-RRz(:,az,bz,cz,dz)*LLx(:,ax,bx,cx,dx)),dim=1)
      Oz = sum(Iz(:,az,bz,cz,dz)*(LLx(:,ax,bx,cx,dx)*RRy(:,ay,by,cy,dy)-RRx(:,ax,bx,cx,dx)*LLy(:,ay,by,cy,dy)),dim=1)

!     do d=1,n_d
!       dix=dx(d)
!       diy=dy(d)
!       diz=dz(d)
!       do c=1,n_c
!         cix=cx(c)
!         ciy=cy(c)
!         ciz=cz(c)
!         do b=1,n_b
!           bix=bx(b)
!           biy=by(b)
!           biz=bz(b)
!           do a=1,n_a
!             aix=ax(a)
!             aiy=ay(a)
!             aiz=az(a)
!             Sxn=ZERO
!             Syn=ZERO
!             Szn=ZERO
!             do n=1,n_roots
!               LLxn = LLx(n,aix,bix,cix,dix)
!               LLyn = LLy(n,aiy,biy,ciy,diy)
!               LLzn = LLz(n,aiz,biz,ciz,diz)
!               RRxn = RRx(n,aix,bix,cix,dix)
!               RRyn = RRy(n,aiy,biy,ciy,diy)
!               RRzn = RRz(n,aiz,biz,ciz,diz)
!               Oxn = Oxn + Ix(n,aix,bix,cix,dix)*(LLyn*RRzn - RRyn*LLzn)
!               Oyn = Oyn + Iy(n,aiy,biy,ciy,diy)*(LLzn*RRxn - RRzn*LLxn)
!               Ozn = Ozn + Iz(n,aiz,biz,ciz,diz)*(LLxn*RRyn - RRxn*LLyn)
!             end
!             Ox(a,b,c,d)=Oxn
!             Oy(a,b,c,d)=Oyn
!             Oz(a,b,c,d)=Ozn
!           end
!         end
!       end
!     end

      call destroy_(RRz); call destroy_(RRy); call destroy_(RRx)
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      Sx = fac*Sx; Sy = fac*Sy; Sz = fac*Sz
      Ox = fac*Ox; Oy = fac*Oy; Oz = fac*Oz
     STOP_TIMER("GAUSSIAN4:make_spin_orbit_ints")
      CHECK
   end subroutine

   subroutine differentiate(self,I,index,ID)
    GAUSSIAN4 :: self
   ! Differentiate a gaussian-integral matrix "I" with respect to the
   ! *nuclear* coordinate on basis function "index", which can be
   ! "a" "b" "c" or "d" depending on which center is differntiated.
   ! Place the result in "ID".
      REALMAT5(:,:,:,:,:) :: I,ID
      STR(*) :: index
      INT :: i_a,i_b,i_c,i_d,d_a,d_b,d_c,d_d,a,b,c,d
      REAL :: a2,b2,c2,d2
      STACK("GAUSSIAN4:differentiate")
      START_TIMER("GAUSSIAN4:differentiate")
      i_a = ubound(I,2);  i_b = ubound(I,3);  i_c = ubound(I,4);  i_d = ubound(I,5)
      d_a = ubound(ID,2); d_b = ubound(ID,3); d_c = ubound(ID,4); d_d = ubound(ID,5)
      ID = ZERO
      select case (index)
         case("a")
            ENSURE(i_a>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            ENSURE(i_a>d_a,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            a2 = TWO*self%a%ex
            ID(:,1,:,:,:) = a2*I(:,2  ,1:d_b,1:d_c,1:d_d)
            do a = 2,d_a
            ID(:,a,:,:,:) = a2*I(:,a+1,1:d_b,1:d_c,1:d_d) - (a-1)*I(:,a-1,1:d_b,1:d_c,1:d_d)
            end do
         case("b")
            ENSURE(i_b>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            ENSURE(i_b>d_b,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            b2 = TWO*self%b%ex
            ID(:,:,1,:,:) = b2*I(:,1:d_a,2  ,1:d_c,1:d_d)
            do b = 2,d_b
            ID(:,:,b,:,:) = b2*I(:,1:d_a,b+1,1:d_c,1:d_d) - (b-1)*I(:,1:d_a,b-1,1:d_c,1:d_d)
            end do
         case("c")
            ENSURE(i_c>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            ENSURE(i_c>d_c,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            c2 = TWO*self%c%ex
            ID(:,:,:,1,:) = c2*I(:,1:d_a,1:d_b,2  ,1:d_d)
            do c = 2,d_c
            ID(:,:,:,c,:) = c2*I(:,1:d_a,1:d_b,c+1,1:d_d) - (c-1)*I(:,1:d_a,1:d_b,c-1,1:d_d)
            end do
         case("d")
            ENSURE(i_d>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            ENSURE(i_d>d_d,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            d2 = TWO*self%d%ex
            ID(:,:,:,:,1) = d2*I(:,1:d_a,1:d_b,1:d_c,2  )
            do d = 2,d_d
            ID(:,:,:,:,d) = d2*I(:,1:d_a,1:d_b,1:d_c,d+1) - (d-1)*I(:,1:d_a,1:d_b,1:d_c,d-1)
            end do
      end select
     STOP_TIMER("GAUSSIAN4:differentiate")
      CHECK
   end subroutine

   subroutine make_spin_spin_dipole_ints(self,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz)
    GAUSSIAN4 :: self
   ! Make the spin spin magnetic dipole-dipole integrals "Dij" using Rys method.
   ! Reference: None. But see Bearpark et al., Mol. Phys. 80, p. 479 (1993) for
   ! inspiration.
      REALMAT4(:,:,:,:) :: Dxx,Dyy,Dzz,Dxy,Dxz,Dyz
      REALMAT5(:,:,:,:,:), PTR :: Ix,Iy,Iz,Lx,Ly,Lz,Rx,Ry,Rz,LL,LR,RR
      INTVEC(:), PTR :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      RYS, PTR :: rys
      REALVEC(3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      REAL :: Dxx_abcd,Dyy_abcd,Dzz_abcd,Dxy_abcd,Dxz_abcd,Dyz_abcd,Ixyn,Ixzn,Iyzn
      REAL :: Lxn,Lyn,Lzn,Ixn,Iyn,Izn,Rxn,Ryn,Rzn
      INT :: l_e,l_f,l_a,l_b,l_c,l_d,n_a,n_b,n_c,n_d,n_roots
      INT :: a,b,c,d,n,aix,aiy,aiz,bix,biy,biz,cix,ciy,ciz,dix,diy,diz
      STACK("GAUSSIAN4:make_spin_spin_dipole_ints")
      START_TIMER("GAUSSIAN4:make_spin_spin_dipole_ints")
      l_a = self%a%l + 1; l_b = self%b%l + 1
      l_c = self%c%l + 1; l_d = self%d%l + 1
      l_e = self%a%l + self%b%l + 2; l_f = self%c%l + self%d%l    ! Two higher for differentiating A, B.
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)  ! Basic intermediate integrals
      call create_(Iy,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)
      call create_(Iz,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Ix = ZERO; Iy = ZERO; Iz = ZERO
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1), rys%r, rys%w,rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                    ! two units higher on B for differentiating
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD,max_b=l_b+1) ! A will automatically be 2 units higher
      call create_(Lx,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Ly,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Lz,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Rx,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Ry,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Rz,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(LL,n_roots,l_a  ,l_b  ,l_c,l_d)
      call create_(LR,n_roots,l_a  ,l_b  ,l_c,l_d)
      call create_(RR,n_roots,l_a  ,l_b  ,l_c,l_d)
      call differentiate_(self,Ix,"a",Lx); call differentiate_(self,Ix,"b",Rx)
      call differentiate_(self,Iy,"a",Ly); call differentiate_(self,Iy,"b",Ry)
      call differentiate_(self,Iz,"a",Lz); call differentiate_(self,Iz,"b",Rz)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call differentiate_(self,Lx,"a",LL); call differentiate_(self,Rx,"b",RR)
      call differentiate_(self,Lx,"b",LR); LR = TWO*LR

!     Dxx = sum(LL(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     LR(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     RR(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dxx_abcd = ZERO
              do n=1,n_roots
                Iyzn = Iy(n,aiy,biy,ciy,diy) * Iz(n,aiz,biz,ciz,diz)
                Dxx_abcd = Dxx_abcd + Iyzn * (LL(n,aix,bix,cix,dix) + &
                                              LR(n,aix,bix,cix,dix) + &
                                              RR(n,aix,bix,cix,dix))
              end do
              Dxx(a,b,c,d) = Dxx_abcd
            end do
          end do
        end do
      end do

      call differentiate_(self,Ly,"a",LL); call differentiate_(self,Ry,"b",RR)
      call differentiate_(self,Ly,"b",LR); LR = TWO*LR

!     Dyy = sum(Ix(:,ax,bx,cx,dx)*LL(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*LR(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*RR(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dyy_abcd = ZERO
              do n=1,n_roots
                Ixzn = Ix(n,aix,bix,cix,dix) * Iz(n,aiz,biz,ciz,diz)
                Dyy_abcd = Dyy_abcd + Ixzn * (LL(n,aiy,biy,ciy,diy) + &
                                              LR(n,aiy,biy,ciy,diy) + &
                                              RR(n,aiy,biy,ciy,diy))
              end do
              Dyy(a,b,c,d) = Dyy_abcd
            end do
          end do
        end do
      end do

      call differentiate_(self,Lz,"a",LL); call differentiate_(self,Rz,"b",RR)
      call differentiate_(self,Lz,"b",LR); LR = TWO*LR

!     Dzz = sum(Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*LL(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*LR(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Iz(:,ay,by,cy,dy)*RR(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dzz_abcd = ZERO
              do n=1,n_roots
                Ixyn = Ix(n,aix,bix,cix,dix) * Iy(n,aiy,biy,ciy,diy)
                Dzz_abcd = Dzz_abcd + Ixyn * (LL(n,aiz,biz,ciz,diz) + &
                                              LR(n,aiz,biz,ciz,diz) + &
                                              RR(n,aiz,biz,ciz,diz))
              end do
              Dzz(a,b,c,d) = Dzz_abcd
            end do
          end do
        end do
      end do

      call destroy_(RR); call destroy_(LR); call destroy_(LL)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     Dxy = sum(Lx(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Lx(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)
!     Dxz = sum(Lx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Lx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz),dim=1)
!     Dyz = sum(Ix(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dxy_abcd = ZERO
              Dxz_abcd = ZERO
              Dyz_abcd = ZERO
              do n=1,n_roots
                Lxn = Lx(n,aix,bix,cix,dix)
                Lyn = Ly(n,aiy,biy,ciy,diy)
                Lzn = Lz(n,aiz,biz,ciz,diz)
                Ixn = Ix(n,aix,bix,cix,dix)
                Iyn = Iy(n,aiy,biy,ciy,diy)
                Izn = Iz(n,aiz,biz,ciz,diz)
                Rxn = Rx(n,aix,bix,cix,dix)
                Ryn = Ry(n,aiy,biy,ciy,diy)
                Rzn = Rz(n,aiz,biz,ciz,diz)
                Dxy_abcd = Dxy_abcd + Izn * (Lxn*Lyn + Lxn*Ryn + Rxn*Lyn + Rxn*Ryn)
                Dxz_abcd = Dxz_abcd + Iyn * (Lxn*Lzn + Lxn*Rzn + Rxn*Lzn + Rxn*Rzn)
                Dyz_abcd = Dyz_abcd + Ixn * (Lyn*Lzn + Lyn*Rzn + Ryn*Lzn + Ryn*Rzn)
              end do
              Dxy(a,b,c,d) = Dxy_abcd
              Dxz(a,b,c,d) = Dxz_abcd
              Dyz(a,b,c,d) = Dyz_abcd
            end do
          end do
        end do
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      Dxx = fac*Dxx; Dyy = fac*Dyy; Dzz = fac*Dzz
      Dxy = fac*Dxy; Dxz = fac*Dxz; Dyz = fac*Dyz
     STOP_TIMER("GAUSSIAN4:make_spin_spin_dipole_ints")
      CHECK
   end subroutine

   subroutine make_ERI_derivatives(self,AA,BB,CC,DD)
    GAUSSIAN4 :: self
   ! Make the ERI gradient integrals "AA", "BB", "CC" and "DD". The last dimension
   ! determines whether the derivative is x, y, or z.
      REALMAT5(:,:,:,:,:), optional :: AA,BB,CC,DD
      REALMAT5(:,:,:,:,:), PTR :: IIx,IIy,IIz,AAx,AAy,AAz,BBx,BBy,BBz,CCx,CCy,CCz,DDx,DDy,DDz
      INTVEC(:), PTR :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      RYS, PTR :: rys
      REALVEC(3) :: AB,CD,P,Q,PA,QC,QP
      REAL :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      INT :: l_e,l_f,l_a,l_b,l_c,l_d,n_a,n_b,n_c,n_d,n_roots
      STACK("GAUSSIAN4:make_ERI_derivatives")
      START_TIMER("GAUSSIAN4:make_ERI_derivatives")
      l_a = self%a%l + 1; l_b = self%b%l + 1
      l_c = self%c%l + 1; l_d = self%d%l + 1
      l_e = self%a%l + self%b%l + 1; l_f = self%c%l + self%d%l + 1 ! One higher for differentiating
      n_roots = (l_e+l_f+2)/2
      call create_(IIx,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)   ! Basic intermediate integrals
      call create_(IIy,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      call create_(IIz,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = ONE/zeta
      einv = ONE/eta
      zeinv = ONE/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = TWOPI5ON2*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IIx = ZERO; IIy = ZERO; IIz = ZERO
      call form_2d_ints_(self,IIx(:,:,1,:,1),IIy(:,:,1,:,1),IIz(:,:,1,:,1),rys%r,rys%w,rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                               ! one unit higher for differentiating
      call transfer_2d_ints_(self,IIx,IIy,IIz,AB,CD,max_b=l_b,max_d=l_d) ! a & c will automatically be one unit higher
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(AA)) then
      call create_(AAx,n_roots,l_a,l_b,l_c,l_d) ! Derivative integrals
      call create_(AAy,n_roots,l_a,l_b,l_c,l_d)
      call create_(AAz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"a",AAx); call differentiate_(self,IIy,"a",AAy); call differentiate_(self,IIz,"a",AAz)
      AA(:,:,:,:,1) = sum(AAx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      AA(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*AAy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      AA(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*AAz(:,az,bz,cz,dz),dim=1)
      call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
      AA = fac*AA
      end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(BB)) then
      call create_(BBx,n_roots,l_a,l_b,l_c,l_d) ! Derivative integrals
      call create_(BBy,n_roots,l_a,l_b,l_c,l_d)
      call create_(BBz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"b",BBx); call differentiate_(self,IIy,"b",BBy); call differentiate_(self,IIz,"b",BBz)
      BB(:,:,:,:,1) = sum(BBx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      BB(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*BBy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      BB(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*BBz(:,az,bz,cz,dz),dim=1)
      call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
      BB = fac*BB
      end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(CC)) then
      call create_(CCx,n_roots,l_a,l_b,l_c,l_d) ! Derivative integrals
      call create_(CCy,n_roots,l_a,l_b,l_c,l_d)
      call create_(CCz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"c",CCx); call differentiate_(self,IIy,"c",CCy); call differentiate_(self,IIz,"c",CCz)
      CC(:,:,:,:,1) = sum(CCx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      CC(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*CCy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      CC(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*CCz(:,az,bz,cz,dz),dim=1)
      call destroy_(CCz); call destroy_(CCy); call destroy_(CCx)
      CC = fac*CC
      end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(DD)) then
      call create_(DDx,n_roots,l_a,l_b,l_c,l_d) ! Derivative integrals
      call create_(DDy,n_roots,l_a,l_b,l_c,l_d)
      call create_(DDz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"d",DDx); call differentiate_(self,IIy,"d",DDy); call differentiate_(self,IIz,"d",DDz)
      DD(:,:,:,:,1) = sum(DDx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      DD(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*DDy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      DD(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*DDz(:,az,bz,cz,dz),dim=1)
      call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
      DD = fac*DD
      end if
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(IIz); call destroy_(IIy); call destroy_(IIx)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
     STOP_TIMER("GAUSSIAN4:make_ERI_derivatives")
      CHECK
   end subroutine

   subroutine put(self,out)
    GAUSSIAN4 :: self
   ! Put the object to file "out"
      TEXTFILE :: out
      STACK("GAUSSIAN4:put")
      START_TIMER("GAUSSIAN4:put")
      call flush_(out)
      call text_(out,"GAUSSIAN4 output:")
      call flush_(out)
      call show_(out,"l_a   =",self%a%l,real_width=TRUE)
      call show_(out,"l_b   =",self%b%l,real_width=TRUE)
      call show_(out,"l_c   =",self%c%l,real_width=TRUE)
      call show_(out,"l_d   =",self%d%l,real_width=TRUE)
      call show_(out,"Ra    =",self%a%pos(1),self%a%pos(2),self%a%pos(3))
      call show_(out,"Rb    =",self%b%pos(1),self%b%pos(2),self%b%pos(3))
      call show_(out,"Rc    =",self%c%pos(1),self%c%pos(2),self%c%pos(3))
      call show_(out,"Rd    =",self%d%pos(1),self%d%pos(2),self%d%pos(3))
      call show_(out,"alpha =",self%a%ex)
      call show_(out,"beta  =",self%b%ex)
      call show_(out,"gamma =",self%c%ex)
      call show_(out,"delta =",self%d%ex)
     STOP_TIMER("GAUSSIAN4:put")
      CHECK
   end subroutine

end
