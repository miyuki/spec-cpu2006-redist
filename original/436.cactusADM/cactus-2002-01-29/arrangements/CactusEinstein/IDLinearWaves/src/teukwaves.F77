#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
c Using macro definitions from Einstein
#include "CactusEinstein/Einstein/src/Einstein.h"


/*@@
  @file     teukwaves.F
  @date     Jan 96
  @author   Joan Masso + Malcolm Tobias
  @desc
            Routines for the teukolsky waves initialization.
  @enddesc

@@*/
        
/*@@
  @routine  teukwaves
  @date     Jan 96
  @author   Malcolm Tobias + Joan Masso 
  @desc 
            Generates the analytic solutions of the
            metric components for linearized quadrupolar (teukolsky) waves.
            More changes for cactus port: all 3d arrays are converted
            to point calculations. What a pain!!!

            Three and a half years later....
            Converted to Cactus4.0

  @enddesc
@@*/

      subroutine teukwaves(CCTK_ARGUMENTS)
      implicit none
      
      DECLARE_CCTK_ARGUMENTS

      CCTK_REAL amp,m,ra,pi
      CCTK_REAL wave,wave2,wave3,wave4,wave5,wave6,wave7,wave8
      INTEGER ipacket,iparity
      INTEGER ingoing,outgoing
      CCTK_REAL kappa
c     point values of x.y z,r
      CCTK_REAL xp,yp,zp,rp
      
c     spherical metric
      CCTK_REAL teuk_grr,teuk_grt,teuk_grp,teuk_gtt,teuk_gtp,teuk_gpp
      CCTK_REAL teuk_hrr,teuk_hrt,teuk_hrp,teuk_htt,teuk_htp,teuk_hpp
      
c     special coefficeints needed for teukolsky waves
c     All these were 3d arrays in old versions of Newage,G,etc..
c     uffff....
      CCTK_REAL teuk_tp,teuk_tn
      CCTK_REAL tp2,tn2,fp,fn,fpa,fna,fpb,fnb,fpc,fnc,fpd,fnd,fpe,fne
      CCTK_REAL ca,cb,cc,ck,cl,frr,frt,frp,ftt1,ftt2,ftp,fpp1,fpp2
      CCTK_REAL cadot,cbdot,ccdot,ckdot,cldot
      CCTK_REAL drt,drp,dtt,dtp,dpp,sint,cost,sinp,cosp,tmp

c     from old spheretocart
      CCTK_REAL drx,dry,drz,dtx,dty,dtz,dpx,dpy,dpz
      
      CHARACTER*200 infoline
      INTEGER i,j,k
      INTEGER CCTK_Equals

      DECLARE_CCTK_PARAMETERS
            
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      pi    = 3.14159265358979
      kappa = sqrt(143.0d0/pi)/12288.0d0
      
      if (wavecenter.ne.0.) then
         call CCTK_WARN(0,"Teukwaves: wavecenter must be zero for time symmetry")
      endif
 
c     CACTUS 4.0: all REAL/INTEGER parameter are available as variables and 
c     need not be derived from the database. Nevertheless I will assign 
c     the parameter variables to the variables of CAC3.0, to allow for 
c     easy portation (wavepulse stays wavepulse btw). (GERD)
      
      amp  = amplitude
      m    = mvalue
      wave = wavelength
      ra   = wavecenter
      
c     to be consistent with Evans & Abrahams, rescale the amplitude
c     by kappa, because they use that in the packet and
c     4/3 because they use I not Teuk. F  
      if (CCTK_Equals(packet,'eppley') == 1) then
         ipacket = 1
         call CCTK_INFO('Teukolsky Packet = eppley') 
      elseif (CCTK_Equals(packet,'evans') == 1) then
         ipacket = 2
         call CCTK_INFO('Teukolsky Packet = evans')
c     to be consistent with Evans & Abrahams, rescale the amplitude
c     by kappa, because they use that in the packet and
c     4/3 because they use I not Teuk. F
         amp = amp*(4.0d0/3.0d0)*kappa
      elseif (CCTK_Equals(packet,'square') == 1) then
         ipacket = 3
         call CCTK_INFO('Teukolsky Packet = square')
      end if
      
      write(infoline,'(A13,G12.7)')
     &    ' amplitude = ',amp
      call CCTK_INFO(infoline)
      write(infoline,'(A11,G12.7)')
     &    ' m value = ',m
      call CCTK_INFO(infoline)
      write(infoline,'(A14,G12.7)')
     &    ' wavecenter = ',ra
      call CCTK_INFO(infoline)
      write(infoline,'(A14,G12.7)')
     &    ' wavelength = ',wave
      call CCTK_INFO(infoline)
      
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
            call CCTK_WARN(4,'Epply packet is only non-singular at the origin for ingoing-outgoing combination of waves')
         endif
      endif
      
      wave2 = wave*wave
      wave3 = wave2*wave
      wave4 = wave3*wave
      wave5 = wave4*wave
      wave6 = wave5*wave
      wave7 = wave6*wave
      wave8 = wave7*wave
         

c  ****************************************   
c     initial data for teukolsky waves
c  ****************************************
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

c initialize everything to zero
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
               
c     eppley package --> x exp(-x^2)  
               if (ipacket.eq.1) then
c     to keep the analytic solution valid for the eppley packet:   
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
     &                    (60 - 360*tp2 + 240*tp2*tp2 - 32*tp2*tp2*tp2)*tmp )
                     
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
     &                    (60 - 360*tn2 + 240*tn2*tn2 - 32*tn2*tn2*tn2)*tmp )
                     
                  endif
               endif
               
               
c     evans package  --> w^4(1-x^2/w^2)^6
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
      
c     square package --> (1-x^2/w^2)^2
               if(ipacket.eq.3) then 
                  
                  call CCTK_WARN(4,'Need to calculate fpe,fne')
                  call CCTK_WARN(4,'Need conditionals for in out waves')
                  
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
               
c     
c     coefficients
c     
               ca = 3*( (fpb-fnb)/rp**3 -3*(fna+fpa)/rp**4 +3*(fp-fn)/rp**5 )
               cb = -( -(fnc+fpc)/rp**2 +3*(fpb-fnb)/rp**3 -6*(fna+fpa)/rp**4
     &              +6*(fp-fn)/rp**5 ) 
               cc = ( (fpd-fnd)/rp -2*(fnc+fpc)/rp**2 +9*(fpb-fnb)/rp**3
     &              -21*(fna+fpa)/rp**4 +21*(fp-fn)/rp**5 )/4
               ck = (fpb-fnb)/rp**2-3*(fpa+fna)/rp**3+3*(fp-fn)/rp**4
               cl = -(fpc+fnc)/rp+2*(fpb-fnb)/rp**2-3*(fpa+fna)/rp**3+
     &              3*(fp-fn)/rp**4
               
c     If we only have an outgoing wave, we should flip the sign
c     of the above coefficients.  That is because the above coefficients
c     assume an ingoing and outgoing wave packet of the form:
c     wave = ingoing - outgoing.  For a single wave, we want ingoing
c     and outgoing to have the same sign.
               
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
               
c     mvalue
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
                  call CCTK_WARN(0,'m should be one of -2,-1,0,1,2')
               endif
               
c     parity
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
               
c     Finally, convert the spherical components to cartesian
               
c     define derivatives drx = (dr/dx)               
               drx = xp/rp
               dry = yp/rp
               drz = zp/rp
      
               dtx = xp*zp/(rp**2*sqrt(xp**2+yp**2))
               dty = yp*zp/(rp**2*sqrt(xp**2+yp**2))
               dtz = (zp**2/rp**2-1)/sqrt(xp**2+yp**2)
               
               dpx = -yp/(xp**2+yp**2)
               dpy = xp/(xp**2+yp**2)
               dpz = 0.
               
c     cartesian g
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
                


c     for the ext. curv. well need...
               cadot = 3*( (fpc-fnc)/rp**3 -3*(fnb+fpb)/rp**4 
     &              +3*(fpa-fna)/rp**5 )
               cbdot = -( -(fnd+fpd)/rp**2 +3*(fpc-fnc)/rp**3 
     &              -6*(fnb+fpb)/rp**4
     &              +6*(fpa-fna)/rp**5 ) 
               ccdot = ( (fpe-fne)/rp -2*(fnd+fpd)/rp**2 +9*(fpc-fnc)/rp**3
     &              -21*(fnb+fpb)/rp**4 +21*(fpa-fna)/rp**5 )/4
               ckdot = (fpc-fnc)/rp**2-3*(fpb+fnb)/rp**3+3*(fpa-fna)/rp**4
               cldot = -(fpd+fnd)/rp+2*(fpc-fnc)/rp**2-3*(fpb+fnb)/rp**3+
     &              3*(fpa-fna)/rp**4
               
c     parity
               if (iparity.eq.0) then    
                  teuk_hrr = -0.5*(cadot*frr)
                  teuk_hrt = -0.5*(cbdot*frt*rp)
                  teuk_hrp = -0.5*(cbdot*frp*(xp**2+yp**2)**0.5)
                  teuk_htt = -0.5*(rp**2*(ccdot*ftt1 + cadot*ftt2))
                  teuk_htp = 
     &                 -0.5*((cadot - 2*ccdot)*ftp*rp*(xp**2+yp**2)**0.5)
                  teuk_hpp = -0.5*((xp**2+yp**2)*(ccdot*fpp1 + cadot*fpp2))
               else
                  teuk_hrr = 0.
                  teuk_hrt = -0.5*(ckdot*drt*rp)
                  teuk_hrp = -0.5*(ckdot*drp*rp*sint)
                  teuk_htt = -0.5*((cldot*dtt)*rp**2)
                  teuk_htp = -0.5*(cldot*dtp*rp**2*sint)
                  teuk_hpp = -0.5*((cldot*dpp)*rp**2*sint**2)
               endif
               
c     time symmetry
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

c     initialize the conformal factor
      if (use_conformal == 1) then
         conformal_state = CONFORMAL_METRIC
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
         conformal_state = NOCONFORMAL_METRIC
      end if

      
      return
      end 
      

      
