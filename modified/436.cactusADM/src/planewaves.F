      subroutine planewaves( cctk_dim, cctk_gsh, cctk_lsh, cctk_lbnd,   
     &                       cctk_ubnd, cctk_lssh, cctk_from, cctk_to, c
     &ctk_bbox,                          cctk_delta_time, cctk_time, cct
     &k_delta_space,                          cctk_origin_space, cctk_le
     &vfac, cctk_convlevel,                          cctk_nghostzones, c
     &ctk_iteration, cctkGH,Xconfac0,Xconfac1,Xconfac2,Xconfac_1derivs0,
     &Xconfac_1derivs1,Xconfac_1derivs2,Xconfac_2derivs0,Xconfac_2derivs
     &1,Xconfac_2derivs2,Xcoordinates0,Xcoordinates1,Xcoordinates2,Xcurv
     &0,Xcurv1,Xcurv2,Xlapse0,Xlapse1,Xlapse2,Xmask0,Xmask1,Xmask2,Xmetr
     &ic0,Xmetric1,Xmetric2,Xshift0,Xshift1,Xshift2,active_slicing_handl
     &e,alp,betax,betay,betaz,coarse_dx,coarse_dy,coarse_dz,conformal_st
     &ate,courant_min_time,courant_wave_speed,emask,gxx,gxy,gxz,gyy,gyz,
     &gzz,kxx,kxy,kxz,kyy,kyz,kzz,psi,psix,psixx,psixy,psixz,psiy,psiyy,
     &psiyz,psiz,psizz,r,shift_state,x,y,z)
      implicit none
      INTEGER cctk_dim
      INTEGER cctk_gsh(cctk_dim),cctk_lsh(cctk_dim)
      INTEGER cctk_lbnd(cctk_dim),cctk_ubnd(cctk_dim)
      INTEGER cctk_lssh(     3*cctk_dim)
      INTEGER cctk_from(cctk_dim),cctk_to(cctk_dim)
      INTEGER cctk_bbox(2*cctk_dim)
      REAL*8 cctk_delta_time, cctk_time
      REAL*8 cctk_delta_space(cctk_dim)
      REAL*8 cctk_origin_space(cctk_dim)
      INTEGER cctk_levfac(cctk_dim)
      INTEGER cctk_convlevel
      INTEGER cctk_nghostzones(cctk_dim)
      INTEGER cctk_iteration
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4 cctkGH
#else
      integer*8 cctkGH
#endif
      INTEGER Xconfac0
      INTEGER Xconfac1
      INTEGER Xconfac2
      INTEGER Xconfac_1derivs0
      INTEGER Xconfac_1derivs1
      INTEGER Xconfac_1derivs2
      INTEGER Xconfac_2derivs0
      INTEGER Xconfac_2derivs1
      INTEGER Xconfac_2derivs2
      INTEGER Xcoordinates0
      INTEGER Xcoordinates1
      INTEGER Xcoordinates2
      INTEGER Xcurv0
      INTEGER Xcurv1
      INTEGER Xcurv2
      INTEGER Xlapse0
      INTEGER Xlapse1
      INTEGER Xlapse2
      INTEGER Xmask0
      INTEGER Xmask1
      INTEGER Xmask2
      INTEGER Xmetric0
      INTEGER Xmetric1
      INTEGER Xmetric2
      INTEGER Xshift0
      INTEGER Xshift1
      INTEGER Xshift2
      INTEGER*4 active_slicing_handle
      REAL*8 alp(Xlapse0,Xlapse1,Xlapse2)
      REAL*8 betax(Xshift0,Xshift1,Xshift2)
      REAL*8 betay(Xshift0,Xshift1,Xshift2)
      REAL*8 betaz(Xshift0,Xshift1,Xshift2)
      REAL*8 coarse_dx
      REAL*8 coarse_dy
      REAL*8 coarse_dz
      INTEGER*4 conformal_state
      REAL*8 courant_min_time
      REAL*8 courant_wave_speed
      REAL*8 emask(Xmask0,Xmask1,Xmask2)
      REAL*8 gxx(Xmetric0,Xmetric1,Xmetric2)
      REAL*8 gxy(Xmetric0,Xmetric1,Xmetric2)
      REAL*8 gxz(Xmetric0,Xmetric1,Xmetric2)
      REAL*8 gyy(Xmetric0,Xmetric1,Xmetric2)
      REAL*8 gyz(Xmetric0,Xmetric1,Xmetric2)
      REAL*8 gzz(Xmetric0,Xmetric1,Xmetric2)
      REAL*8 kxx(Xcurv0,Xcurv1,Xcurv2)
      REAL*8 kxy(Xcurv0,Xcurv1,Xcurv2)
      REAL*8 kxz(Xcurv0,Xcurv1,Xcurv2)
      REAL*8 kyy(Xcurv0,Xcurv1,Xcurv2)
      REAL*8 kyz(Xcurv0,Xcurv1,Xcurv2)
      REAL*8 kzz(Xcurv0,Xcurv1,Xcurv2)
      REAL*8 psi(Xconfac0,Xconfac1,Xconfac2)
      REAL*8 psix(Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2)
      REAL*8 psixx(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)
      REAL*8 psixy(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)
      REAL*8 psixz(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)
      REAL*8 psiy(Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2)
      REAL*8 psiyy(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)
      REAL*8 psiyz(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)
      REAL*8 psiz(Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2)
      REAL*8 psizz(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)
      REAL*8 r(Xcoordinates0,Xcoordinates1,Xcoordinates2)
      INTEGER*4 shift_state
      REAL*8 x(Xcoordinates0,Xcoordinates1,Xcoordinates2)
      REAL*8 y(Xcoordinates0,Xcoordinates1,Xcoordinates2)
      REAL*8 z(Xcoordinates0,Xcoordinates1,Xcoordinates2)
      
      REAL*8  amplitude
      REAL*8  wavecenter
      REAL*8  wavelength
      REAL*8  wavephi
      REAL*8  wavepulse
      REAL*8  wavetheta
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4  packet
      integer*4  parity
      integer*4  teuk_no_vee
      integer*4  wavesgoing
#else
      integer*8  packet
      integer*8  parity
      integer*8  teuk_no_vee
      integer*8  wavesgoing
#endif
      INTEGER*4 mvalue
      COMMON /IDLinearWavespriv/amplitude,wavecenter,wavelength,wavephi,
     &wavepulse,wavetheta,packet,parity,teuk_no_vee,wavesgoing,mvalue
      REAL*8  CCTKH3
      REAL*8  CCTKH4
      REAL*8  CCTKH8
      REAL*8  CCTKH11
      REAL*8  CCTKH12
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4  CCTKH0
      integer*4  CCTKH2
      integer*4  initial_data
      integer*4  CCTKH5
      integer*4  CCTKH6
      integer*4  CCTKH7
      integer*4  CCTKH14
      integer*4  CCTKH15
      integer*4  CCTKH16
#else
      integer*8  CCTKH0
      integer*8  CCTKH2
      integer*8  initial_data
      integer*8  CCTKH5
      integer*8  CCTKH6
      integer*8  CCTKH7
      integer*8  CCTKH14
      integer*8  CCTKH15
      integer*8  CCTKH16
#endif
      INTEGER*4 CCTKH1
      INTEGER*4 CCTKH9
      INTEGER*4 CCTKH10
      INTEGER*4 CCTKH13
      INTEGER*4 use_conformal
      INTEGER*4 use_conformal_derivs
      INTEGER*4 CCTKH17
      COMMON /EINSTEINrest/CCTKH3,CCTKH4,CCTKH8,CCTKH11,CCTKH12,CCTKH0,C
     &CTKH2,initial_data,CCTKH5,CCTKH6,CCTKH7,CCTKH14,CCTKH15,CCTKH16,CC
     &TKH1,CCTKH9,CCTKH10,CCTKH13,use_conformal,use_conformal_derivs,CCT
     &KH17
      
      INTEGER iin,iout
       REAL*8 pi
       REAL*8 ra,the,phi
       REAL*8 wave,wavep
       REAL*8 kx,ky,kz,w
       REAL*8 kxp,kyp,kzp,wp
      CHARACTER*200 infoline
       REAL*8 plus,minus,plusp,minusp,ain,aout
      INTEGER i,j,k
      INTEGER CCTK_Equals
      pi = 3.14159265358979d0
      ra   = wavecenter
      wave = wavelength   
      wavep= wavepulse     
      iin  = 0
      iout = 0
      if(CCTK_Equals(wavesgoing,'in').ne.0) then
         iin = 1
      elseif(CCTK_Equals(wavesgoing,'out').ne.0) then
         iout = 1
      elseif(CCTK_Equals(wavesgoing,'both').ne.0) then
         iin = 1
         iout = 1
      endif
      the = pi*wavetheta/180.d0
      phi = pi*wavephi/180.d0
      call  CCTK_Info("IDLinearWaves",('Plane waves'))
      write(infoline,'(A13,G12.7)')
     &    ' amplitude = ',amplitude
      call  CCTK_Info("IDLinearWaves",(infoline))
      write(infoline,'(A14,G12.7)')
     &    ' wavecenter = ',ra
      call  CCTK_Info("IDLinearWaves",(infoline))
      write(infoline,'(A14,G12.7)')
     &    ' wavelength = ',wave
      call  CCTK_Info("IDLinearWaves",(infoline))
      write(infoline,'(A14,G12.7)')
     &    ' wavepulse = ',wavep
      call  CCTK_Info("IDLinearWaves",(infoline))
      kx = 2*pi*sin(the)*cos(phi)/wave
      ky = 2*pi*sin(the)*sin(phi)/wave
      kz = 2*pi*cos(the)/wave
      w = (kx*kx+ky*ky+kz*kz)**0.5
      kxp = 2*pi*sin(the)*cos(phi)/wavep
      kyp = 2*pi*sin(the)*sin(phi)/wavep
      kzp = 2*pi*cos(the)/wavep
      wp = (kxp*kxp+kyp*kyp+kzp*kzp)**0.5
      do k=1,cctk_lsh(3)
         do j=1,cctk_lsh(2)
            do i=1,cctk_lsh(1)                 
               plus = (kx*x(i,j,k)+ky*y(i,j,k)+kz*z(i,j,k)+w*cctk_time)
               minus = (kx*x(i,j,k)+ky*y(i,j,k)+kz*z(i,j,k)-w*cctk_time)
     &
               plusp =(kxp*x(i,j,k)+kyp*y(i,j,k)+kzp*z(i,j,k)+wp*(cctk_t
     &ime-ra))
               minusp =(kxp*x(i,j,k)+kyp*y(i,j,k)+kzp*z(i,j,k)-wp*(cctk_
     &time-ra))
               ain = iin*amplitude*cos(plus)*exp(-(plusp)**2)
               aout = iout*amplitude*cos(minus)*exp(-(minusp)**2)
        gxx(i,j,k) = cos(the)**2*cos(phi)**2*(1+ain+aout) +
     &     sin(phi)**2*(1-ain-aout) + sin(the)**2*cos(phi)**2
        gxy(i,j,k) = cos(the)**2*sin(phi)*cos(phi)*(1+ain+aout) +
     &    sin(phi)*cos(phi)*(1-ain-aout) - sin(the)**2*sin(phi)*cos(phi)
     &
        gxz(i,j,k) = sin(the)*cos(the)*cos(phi)*(1+ain+aout) -
     &    sin(the)*cos(the)*cos(phi)
        gyy(i,j,k) = cos(the)**2*sin(phi)**2*(1+ain+aout) +
     &    cos(phi)**2*(1-ain-aout) + sin(the)**2*sin(phi)**2
        gyz(i,j,k) = -sin(the)*cos(the)*sin(phi)*(1+ain+aout) +
     &    sin(the)*cos(the)*sin(phi)
        gzz(i,j,k) = sin(the)**2*(1+ain+aout) + cos(the)**2
        kxx(i,j,k) = amplitude*
     &  (cos(the)**2*cos(phi)**2*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))) - 
     &  sin(phi)**2*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))))/(-2.d0)
        kxy(i,j,k) = amplitude*
     &  (cos(the)**2*sin(phi)*cos(phi)*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))) -
     &  sin(phi)*cos(phi)*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))))/(-2.d0)
        kxz(i,j,k) = amplitude*
     &  (sin(the)*cos(the)*cos(phi)*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))))/(-2.d0)
        kyy(i,j,k) = amplitude*
     &  (cos(the)**2*sin(phi)**2*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))) -
     &  cos(phi)**2*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))))/(-2.d0)
        kyz(i,j,k) = amplitude*
     &  (-sin(the)*cos(the)*sin(phi)*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))))/(-2.d0)
        kzz(i,j,k) = amplitude*
     &  (sin(the)**2*
     &  (iin*(-w*sin(plus)*exp(-(plusp)**2) -
     &  2.d0*wp*plusp*cos(plus)*exp(-(plusp)**2)) + 
     &  iout*(w*sin(minus)*exp(-(minusp)**2) +
     &  2.d0*wp*minusp*cos(minus)*exp(-(minusp)**2))))/(-2.d0)
          enddo 
        enddo
      enddo
      if (use_conformal == 1) then
         conformal_state = 1
         do k=1,cctk_lsh(3)
            do j=1,cctk_lsh(2)
               do i=1,cctk_lsh(1)                 
                  psi(i,j,k)   = 1d0
                  psix(i,j,k)  = 0d0
                  psiy(i,j,k)  = 0d0
                  psiz(i,j,k)  = 0d0    
                  psixy(i,j,k) = 0d0
                  psixz(i,j,k) = 0d0
                  psiyz(i,j,k) = 0d0  
                  psixx(i,j,k) = 0d0
                  psiyy(i,j,k) = 0d0
                  psizz(i,j,k) = 0d0
               end do
            end do
         end do
      else
         conformal_state = 0
      end if
      return
      end
