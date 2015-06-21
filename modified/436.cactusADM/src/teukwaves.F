      subroutine teukwaves( cctk_dim, cctk_gsh, cctk_lsh, cctk_lbnd,    
     &                      cctk_ubnd, cctk_lssh, cctk_from, cctk_to, cc
     &tk_bbox,                          cctk_delta_time, cctk_time, cctk
     &_delta_space,                          cctk_origin_space, cctk_lev
     &fac, cctk_convlevel,                          cctk_nghostzones, cc
     &tk_iteration, cctkGH,Xconfac0,Xconfac1,Xconfac2,Xconfac_1derivs0,X
     &confac_1derivs1,Xconfac_1derivs2,Xconfac_2derivs0,Xconfac_2derivs1
     &,Xconfac_2derivs2,Xcoordinates0,Xcoordinates1,Xcoordinates2,Xcurv0
     &,Xcurv1,Xcurv2,Xlapse0,Xlapse1,Xlapse2,Xmask0,Xmask1,Xmask2,Xmetri
     &c0,Xmetric1,Xmetric2,Xshift0,Xshift1,Xshift2,active_slicing_handle
     &,alp,betax,betay,betaz,coarse_dx,coarse_dy,coarse_dz,conformal_sta
     &te,courant_min_time,courant_wave_speed,emask,gxx,gxy,gxz,gyy,gyz,g
     &zz,kxx,kxy,kxz,kyy,kyz,kzz,psi,psix,psixx,psixy,psixz,psiy,psiyy,p
     &siyz,psiz,psizz,r,shift_state,x,y,z)
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
      
       REAL*8 amp,m,ra,pi
       REAL*8 wave,wave2,wave3,wave4,wave5,wave6,wave7,wave8
      INTEGER ipacket,iparity
      INTEGER ingoing,outgoing
       REAL*8 kappa
       REAL*8 xp,yp,zp,rp
       REAL*8 teuk_grr,teuk_grt,teuk_grp,teuk_gtt,teuk_gtp,teuk_gpp
       REAL*8 teuk_hrr,teuk_hrt,teuk_hrp,teuk_htt,teuk_htp,teuk_hpp
       REAL*8 teuk_tp,teuk_tn
       REAL*8 tp2,tn2,fp,fn,fpa,fna,fpb,fnb,fpc,fnc,fpd,fnd,fpe,fne
       REAL*8 ca,cb,cc,ck,cl,frr,frt,frp,ftt1,ftt2,ftp,fpp1,fpp2
       REAL*8 cadot,cbdot,ccdot,ckdot,cldot
       REAL*8 drt,drp,dtt,dtp,dpp,sint,cost,sinp,cosp,tmp
       REAL*8 drx,dry,drz,dtx,dty,dtz,dpx,dpy,dpz
      CHARACTER*200 infoline
      INTEGER i,j,k
      INTEGER CCTK_Equals
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
      
      pi    = 3.14159265358979
      kappa = sqrt(143.0d0/pi)/12288.0d0
      if (wavecenter.ne.0.) then
         call  CCTK_Warn(0,75,"teukwaves.F77","IDLinearWaves","Teukwaves
     &: wavecenter must be zero for time symmetry")
      endif
      amp  = amplitude
      m    = mvalue
      wave = wavelength
      ra   = wavecenter
      if (CCTK_Equals(packet,'eppley') == 1) then
         ipacket = 1
         call  CCTK_Info("IDLinearWaves",('Teukolsky Packet = eppley')) 
     &
      elseif (CCTK_Equals(packet,'evans') == 1) then
         ipacket = 2
         call  CCTK_Info("IDLinearWaves",('Teukolsky Packet = evans'))
         amp = amp*(4.0d0/3.0d0)*kappa
      elseif (CCTK_Equals(packet,'square') == 1) then
         ipacket = 3
         call  CCTK_Info("IDLinearWaves",('Teukolsky Packet = square'))
      end if
      write(infoline,'(A13,G12.7)')
     &    ' amplitude = ',amp
      call  CCTK_Info("IDLinearWaves",(infoline))
      write(infoline,'(A11,G12.7)')
     &    ' m value = ',m
      call  CCTK_Info("IDLinearWaves",(infoline))
      write(infoline,'(A14,G12.7)')
     &    ' wavecenter = ',ra
      call  CCTK_Info("IDLinearWaves",(infoline))
      write(infoline,'(A14,G12.7)')
     &    ' wavelength = ',wave
      call  CCTK_Info("IDLinearWaves",(infoline))
      if (CCTK_Equals(parity,'even') == 1) then    
         iparity = 0
      elseif (CCTK_Equals(parity,'odd') == 1) then 
         iparity = 1
      endif
      if(CCTK_Equals(wavesgoing,'in') == 1) then
         ingoing = 1
      endif
      if(CCTK_Equals(wavesgoing,'out') == 1) then
         outgoing = 1
      endif
      if(CCTK_Equals(wavesgoing,'both') == 1) then
         ingoing  = 1
         outgoing = 1
      endif
      if (ipacket.eq.1) then
         if (ingoing.eq.0.or.outgoing.eq.0) then
            call  CCTK_Warn(4,140,"teukwaves.F77","IDLinearWaves",'Epply
     & packet is only non-singular at the origin for ingoing-outgoing co
     &mbination of waves')
         endif
      endif
      wave2 = wave*wave
      wave3 = wave2*wave
      wave4 = wave3*wave
      wave5 = wave4*wave
      wave6 = wave5*wave
      wave7 = wave6*wave
      wave8 = wave7*wave
      do k=1,cctk_lsh(3)
         do j=1,cctk_lsh(2)
            do i=1,cctk_lsh(1)
               xp = x(i,j,k)
               yp = y(i,j,k)
               zp = z(i,j,k)
               rp = r(i,j,k)
               teuk_tp = (cctk_time+rp-ra)
               teuk_tn = (cctk_time-rp+ra)
               tp2 = teuk_tp**2
               tn2 = teuk_tn**2
               fp = 0.0d0
               fpa = 0.0d0
               fpb = 0.0d0
               fpc = 0.0d0
               fpd = 0.0d0
               fpe = 0.0d0
               fn = 0.0d0
               fna = 0.0d0
               fnb = 0.0d0
               fnc = 0.0d0
               fnd = 0.0d0
               fne = 0.0d0
               if (ipacket.eq.1) then
                  if (ra.ne.0.and.ingoing.eq.1.and.outgoing.eq.1) then
                     teuk_tn = (cctk_time-rp-ra)
                     tn2 = teuk_tn**2
                  endif
                  if (ingoing.eq.1) then
                     tmp = exp(-tp2)
                     fp = amp * teuk_tp*tmp
                     fpa = amp * (1 - 2*tp2)*tmp
                     fpb = amp * teuk_tp*(4*tp2 - 6)*tmp
                     fpc = amp * (24*tp2 - 8*tp2*tp2 - 6)*tmp
                     fpd = amp * teuk_tp*(60 - 80*tp2 + 16*tp2*tp2)*tmp
                     fpe = amp * (
     &                    (60 - 360*tp2 + 240*tp2*tp2 - 32*tp2*tp2*tp2)*
     &tmp )
                     if (outgoing.eq.0.and.(teuk_tp+wavepulse).le.0) 
     &                    then
                        fp  = 0.0d0
                        fpa = 0.0d0
                        fpb = 0.0d0
                        fpc = 0.0d0
                        fpd = 0.0d0
                        fpe = 0.0d0
                     endif
                  endif
                  if (outgoing.eq.1) then
                     tmp = exp(-tn2)
                     fn = amp * teuk_tn*tmp
                     fna = amp * (1 - 2*tn2)*tmp
                     fnb = amp * teuk_tn*(4*tn2 - 6)*tmp
                     fnc = amp * (24*tn2 - 8*tn2*tn2 -6)*tmp
                     fnd = amp * teuk_tn*(60 - 80*tn2 + 16*tn2*tn2)*tmp
                     fne = amp * (
     &                    (60 - 360*tn2 + 240*tn2*tn2 - 32*tn2*tn2*tn2)*
     &tmp )
                  endif
               endif
               if(ipacket.eq.2) then
                  if (ingoing.eq.1) then
                     tmp = 1.0d0 - tp2/wave2
                     if (abs(teuk_tp).lt.wave) then
                        fp  =  amp*wave5*tmp**6
                        fpa =  -12.0d0*amp*wave3*teuk_tp*tmp**5
                        fpb =  -12.0d0*amp*wave3*(
     &                       tmp**5 - 10.0d0*tp2/wave2*tmp**4 )
                        fpc =  120.0d0*amp*wave3*(
     &                       3.0d0*teuk_tp/wave2*tmp**4 
     &                       -8.0d0*teuk_tp*tp2/wave4*tmp**3 )
                        fpd = 360.0d0*amp*wave3*(
     &                       1.0d0/wave2*tmp**4 
     &                       -16.0d0*tp2/wave4*tmp**3
     &                       +16.0d0*tp2*tp2/wave6*tmp**2 )
                        fpe = 2880.0d0*amp*wave3*(
     &                       -5.0d0*teuk_tp/wave4*tmp**3
     &                       +20.0d0*teuk_tp*tp2/wave6*tmp**2
     &                       -8.0d0*teuk_tp*tp2*tp2/wave8*tmp )
                     else
                        fp=0.
                        fpa=0.
                        fpb=0.
                        fpc=0.
                        fpd=0.
                        fpe=0.
                     endif
                  endif
                  if (outgoing.eq.1) then
                     tmp = 1.0d0 - tn2/wave2
                     if (abs(teuk_tn).lt.wave) then
                        fn  =  amp*wave5*tmp**6
                        fna =  -12.0d0*amp*wave3*teuk_tn*tmp**5
                        fnb =  -12.0d0*amp*wave3*(
     &                       tmp**5 - 10.0d0*tn2/wave2*tmp**4 )
                        fnc =  120.0d0*amp*wave3*(
     &                       3.0d0*teuk_tn/wave2*tmp**4 
     &                       -8.0d0*teuk_tn*tn2/wave4*tmp**3 )
                        fnd = 360.0d0*amp*wave3*(
     &                       1.0d0/wave2*tmp**4 
     &                       -16.0d0*tn2/wave4*tmp**3
     &                       +16.0d0*tn2*tn2/wave6*tmp**2 )
                        fne = 2880.0d0*amp*wave3*(
     &                       -5.0d0*teuk_tn/wave4*tmp**3
     &                       +20.0d0*teuk_tn*tn2/wave6*tmp**2
     &                       -8.0d0*teuk_tn*tn2*tn2/wave8*tmp )
                     else
                        fn=0.
                        fna=0.
                        fnb=0.
                        fnc=0.
                        fnd=0.
                        fne=0.
                     endif
                  endif
               endif
               if(ipacket.eq.3) then 
                  call  CCTK_Warn(4,308,"teukwaves.F77","IDLinearWaves",
     &'Need to calculate fpe,fne')
                  call  CCTK_Warn(4,309,"teukwaves.F77","IDLinearWaves",
     &'Need conditionals for in out waves')
                  if (abs(teuk_tp).lt.wave) then
                     fp = amp * (1-(tp2/wave2))**2
                     fpa = amp * (-4*teuk_tp)*(1-tp2/wave2)/wave2
                     fpb = amp * (8*tp2/wave4-4*(1-tp2/wave2)/wave2)
                     fpc = amp * 24*teuk_tp/wave4
                     fpd = amp * 24/wave4
                  else
                     fp=0.
                     fpa=0.
                     fpb=0.
                     fpc=0.
                     fpd=0.
                  endif
                  if (abs(teuk_tn).lt.wave) then
                     fn = amp * (1-(tn2/wave2))**2
                     fna = amp * (-4*teuk_tn)*(1-tn2/wave2)/wave2
                     fnb = amp * (8*tn2/wave4-4*(1-tn2/wave2)/wave2)
                     fnc = amp * 24*teuk_tn/wave4
                     fnd = amp * 24/wave4  
                  else
                     fn=0.
                     fna=0.
                     fnb=0.
                     fnc=0.
                     fnd=0.
                  endif
               endif
               ca = 3*( (fpb-fnb)/rp**3 -3*(fna+fpa)/rp**4 +3*(fp-fn)/rp
     &**5 )
               cb = -( -(fnc+fpc)/rp**2 +3*(fpb-fnb)/rp**3 -6*(fna+fpa)/
     &rp**4
     &              +6*(fp-fn)/rp**5 ) 
               cc = ( (fpd-fnd)/rp -2*(fnc+fpc)/rp**2 +9*(fpb-fnb)/rp**3
     &
     &              -21*(fna+fpa)/rp**4 +21*(fp-fn)/rp**5 )/4
               ck = (fpb-fnb)/rp**2-3*(fpa+fna)/rp**3+3*(fp-fn)/rp**4
               cl = -(fpc+fnc)/rp+2*(fpb-fnb)/rp**2-3*(fpa+fna)/rp**3+
     &              3*(fp-fn)/rp**4
               if(ingoing.eq.0.and.outgoing.eq.1) then
                  ca=-ca
                  cb=-cb
                  cc=-cc
                  ck=-ck
                  cl=-cl
               endif
               sint = (xp**2+yp**2)**0.5/rp
               cost = zp/rp
               sinp = yp/(xp**2+yp**2)**0.5
               cosp = xp/(xp**2+yp**2)**0.5
               if (m.eq.0) then
                  frr = 2-3*sint**2
                  frt = -3*sint*cost
                  frp = 0.
                  ftt1 = 3*sint**2
                  ftt2 = -1.
                  ftp = 0.
                  fpp1 = -ftt1
                  fpp2 = 3*sint**2-1
                  drt = 0.
                  drp = -4*cost*sint
                  dtt = 0.
                  dtp = -sint**2
                  dpp = 0.
               elseif (m.eq.-1) then
                  frr = 2*sint*cost*sinp
                  frt = (cost**2-sint**2)*sinp 
                  frp = cost*cosp 
                  ftt1 = -frr
                  ftt2 = 0.
                  ftp = sint*cosp 
                  fpp1 = -ftt1
                  fpp2 = ftt1
                  drt = -2*cost*sinp
                  drp = -2*(cost**2-sint**2)*cosp
                  dtt = -sint*sinp
                  dtp = -cost*sint*cosp
                  dpp = sint*sinp
               elseif (m.eq.-2) then
                  frr = sint**2*2*sinp*cosp
                  frt = sint*cost*2*sinp*cosp
                  frp = sint*(1-2*sinp**2)
                  ftt1 = (1+cost**2)*2*sinp*cosp
                  ftt2 = -2*sinp*cosp
                  fpp1 = -ftt1
                  fpp2 = cost**2*2*sinp*cosp
                  drt = 8*sint*sinp*cosp
                  drp = 4*sint*cost*(1-2*sinp**2)
                  dtt = -4*cost*sinp*cosp
                  dtp = (2-sint**2)*(2*sinp**2-1)
                  dpp = 2*cost*2*sinp*cosp
               elseif (m.eq.1) then
                  frr = 2*sint*cost*cosp
                  frt = (cost**2-sint**2)*cosp
                  frp = -cost*sinp
                  ftt1 = -2*sint*cost*cosp
                  ftt2 = 0.
                  ftp = -sint*sinp
                  fpp1 = -ftt1
                  fpp2 = ftt1
                  drt = -2*cost*cosp
                  drp = 2*(cost**2-sint**2)*sinp
                  dtt = -sint*cosp
                  dtp = cost*sint*sinp
                  dpp = sint*cosp
               elseif (m.eq.2) then
                  frr = sint**2*(1-2*sinp**2)
                  frt = sint*cost*(1-2*sinp**2)
                  frp = -sint*2*sinp*cosp
                  ftt1 = (1+cost**2)*(1-2*sinp**2)
                  ftt2 = (2*sinp**2-1)
                  ftp = cost*2*sinp*cosp
                  fpp1 = -ftt1
                  fpp2 = cost**2*(1-2*sinp**2)
                  drt = 4*sint*(1-2*sinp**2)
                  drp = -4*sint*cost*2*sinp*cosp
                  dtt = -2*cost*(1-2*sinp**2)
                  dtp = (2-sint**2)*2*sinp*cosp
                  dpp = 2*cost*(1-2*sinp**2)
               else
                  call  CCTK_Warn(0,442,"teukwaves.F77","IDLinearWaves",
     &'m should be one of -2,-1,0,1,2')
               endif
               if (iparity.eq.0) then    
                  teuk_grr = 1 + ca*frr
                  teuk_grt = cb*frt*rp
                  teuk_grp = cb*frp*(xp**2+yp**2)**0.5
                  teuk_gtt = rp**2*(1 + cc*ftt1 + ca*ftt2)
                  teuk_gtp = (ca - 2*cc)*ftp*rp*(xp**2+yp**2)**0.5
                  teuk_gpp = (xp**2+yp**2)*(1 + cc*fpp1 + ca*fpp2)
               else
                  teuk_grr = 1.
                  teuk_grt = ck*drt*rp
                  teuk_grp = ck*drp*rp*sint
                  teuk_gtt = (1+cl*dtt)*rp**2
                  teuk_gtp = cl*dtp*rp**2*sint
                  teuk_gpp = (1+cl*dpp)*rp**2*sint**2
               endif
               drx = xp/rp
               dry = yp/rp
               drz = zp/rp
               dtx = xp*zp/(rp**2*sqrt(xp**2+yp**2))
               dty = yp*zp/(rp**2*sqrt(xp**2+yp**2))
               dtz = (zp**2/rp**2-1)/sqrt(xp**2+yp**2)
               dpx = -yp/(xp**2+yp**2)
               dpy = xp/(xp**2+yp**2)
               dpz = 0.
               gxx(i,j,k) = drx**2*teuk_grr+2.*drx*dtx*teuk_grt
     &              +2.*drx*dpx*teuk_grp+dtx**2*teuk_gtt
     &              +2.*dtx*dpx*teuk_gtp+dpx**2*teuk_gpp
               gyy(i,j,k) = dry**2*teuk_grr+2.*dry*dty*teuk_grt
     &              +2.*dry*dpy*teuk_grp+dty**2*teuk_gtt
     &              +2.*dty*dpy*teuk_gtp+dpy**2*teuk_gpp
               gzz(i,j,k) = drz**2*teuk_grr+2.*drz*dtz*teuk_grt
     &              +2.*drz*dpz*teuk_grp+dtz**2*teuk_gtt
     &              +2.*dtz*dpz*teuk_gtp+dpz**2*teuk_gpp
               gxy(i,j,k) = drx*dry*teuk_grr+(drx*dty+dtx*dry)*teuk_grt
     &              +(drx*dpy+dpx*dry)*teuk_grp
     &              +dtx*dty*teuk_gtt+(dtx*dpy+dpx*dty)*teuk_gtp+
     &              dpx*dpy*teuk_gpp
               gxz(i,j,k) = drx*drz*teuk_grr+(drx*dtz+dtx*drz)*teuk_grt
     &              +(drx*dpz+dpx*drz)*teuk_grp
     &              +dtx*dtz*teuk_gtt+(dtx*dpz+dpx*dtz)*teuk_gtp+
     &              dpx*dpz*teuk_gpp
               gyz(i,j,k) = dry*drz*teuk_grr+(dry*dtz+dty*drz)*teuk_grt
     &              +(dry*dpz+dpy*drz)*teuk_grp
     &              +dty*dtz*teuk_gtt+(dty*dpz+dpy*dtz)*teuk_gtp+
     &              dpy*dpz*teuk_gpp
               cadot = 3*( (fpc-fnc)/rp**3 -3*(fnb+fpb)/rp**4 
     &              +3*(fpa-fna)/rp**5 )
               cbdot = -( -(fnd+fpd)/rp**2 +3*(fpc-fnc)/rp**3 
     &              -6*(fnb+fpb)/rp**4
     &              +6*(fpa-fna)/rp**5 ) 
               ccdot = ( (fpe-fne)/rp -2*(fnd+fpd)/rp**2 +9*(fpc-fnc)/rp
     &**3
     &              -21*(fnb+fpb)/rp**4 +21*(fpa-fna)/rp**5 )/4
               ckdot = (fpc-fnc)/rp**2-3*(fpb+fnb)/rp**3+3*(fpa-fna)/rp*
     &*4
               cldot = -(fpd+fnd)/rp+2*(fpc-fnc)/rp**2-3*(fpb+fnb)/rp**3
     &+
     &              3*(fpa-fna)/rp**4
               if (iparity.eq.0) then    
                  teuk_hrr = -0.5*(cadot*frr)
                  teuk_hrt = -0.5*(cbdot*frt*rp)
                  teuk_hrp = -0.5*(cbdot*frp*(xp**2+yp**2)**0.5)
                  teuk_htt = -0.5*(rp**2*(ccdot*ftt1 + cadot*ftt2))
                  teuk_htp = 
     &                 -0.5*((cadot - 2*ccdot)*ftp*rp*(xp**2+yp**2)**0.5
     &)
                  teuk_hpp = -0.5*((xp**2+yp**2)*(ccdot*fpp1 + cadot*fpp
     &2))
               else
                  teuk_hrr = 0.
                  teuk_hrt = -0.5*(ckdot*drt*rp)
                  teuk_hrp = -0.5*(ckdot*drp*rp*sint)
                  teuk_htt = -0.5*((cldot*dtt)*rp**2)
                  teuk_htp = -0.5*(cldot*dtp*rp**2*sint)
                  teuk_hpp = -0.5*((cldot*dpp)*rp**2*sint**2)
               endif
               kxx(i,j,k) = drx**2*teuk_hrr+2.*drx*dtx*teuk_hrt
     &              +2.*drx*dpx*teuk_hrp+dtx**2*teuk_htt
     &              +2.*dtx*dpx*teuk_htp+dpx**2*teuk_hpp
               kyy(i,j,k) = dry**2*teuk_hrr+2.*dry*dty*teuk_hrt
     &              +2.*dry*dpy*teuk_hrp+dty**2*teuk_htt
     &              +2.*dty*dpy*teuk_htp+dpy**2*teuk_hpp
               kzz(i,j,k) = drz**2*teuk_hrr+2.*drz*dtz*teuk_hrt
     &              +2.*drz*dpz*teuk_hrp+dtz**2*teuk_htt
     &              +2.*dtz*dpz*teuk_htp+dpz**2*teuk_hpp
               kxy(i,j,k) = drx*dry*teuk_hrr+(drx*dty+dtx*dry)*teuk_hrt
     &              +(drx*dpy+dpx*dry)*teuk_hrp
     &              +dtx*dty*teuk_htt+(dtx*dpy+dpx*dty)*teuk_htp+
     &              dpx*dpy*teuk_hpp
               kxz(i,j,k) = drx*drz*teuk_hrr+(drx*dtz+dtx*drz)*teuk_hrt
     &              +(drx*dpz+dpx*drz)*teuk_hrp
     &              +dtx*dtz*teuk_htt+(dtx*dpz+dpx*dtz)*teuk_htp+
     &              dpx*dpz*teuk_hpp
               kyz(i,j,k) = dry*drz*teuk_hrr+(dry*dtz+dty*drz)*teuk_hrt
     &              +(dry*dpz+dpy*drz)*teuk_hrp
     &              +dty*dtz*teuk_htt+(dty*dpz+dpy*dtz)*teuk_htp+
     &              dpy*dpz*teuk_hpp
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
