/*@@
   @file      planewaves.f
   @date      Wed Jan 24 16:51:03 1996
   @author    Malcolm Tobias + Joan Masso
   @desc 
	Routine to initialize linear plane wave spacetime
   @enddesc 
   @hdate Tue Mar 16 12:38:04 1999 @hauthor Gerd Lanfermann
   @hdesc Converted to Cactus4.0. the code and its comments of the
   original authors are kept. There is NO BM suport at this point.
   Id like to know how Cactus handles this w/o #ifdef !
      
@@*/

 /*@@
   @routine    planewaves
   @date       Mon Jan 29 11:57:00 1996
   @author     Malcolm Tobias + Joan Masso
   @desc   
	Routine to initialize a linear plane wave travelling in an 
	arbitrary direction.  The form of the packet is:  <p>   
	$ A*exp\left[-(kp_ix^i-\omega_p (time-ra))^2\right]
        cos(k_ix^i-\omega \ time) $ 
	<p>
	where: <p>
	A = amplitude of the wave  <br>
	k  = the wave # of the sine wave <br>
	$\omega$ = the frequency of the sine wave  <br>
	kp = the wave # of the gaussian modulating the sine wave<br>
	$\omega_p$ = the frequency of the gaussian <br>
	ra = the initial position of the packet(s)
   @enddesc 
   @calledby linearWaves
   @calls 
@@*/

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
c Using macro definitions from Einstein
#include "CactusEinstein/Einstein/src/Einstein.h"


      subroutine planewaves(CCTK_ARGUMENTS)
      implicit none
      
      DECLARE_CCTK_ARGUMENTS
      DECLARE_CCTK_PARAMETERS
      
      INTEGER iin,iout
      CCTK_REAL pi
      CCTK_REAL ra,the,phi
      CCTK_REAL wave,wavep
      CCTK_REAL kx,ky,kz,w
      CCTK_REAL kxp,kyp,kzp,wp
      CHARACTER*200 infoline

c     local arrays (scalars! Man, what a sweat!)
      CCTK_REAL plus,minus,plusp,minusp,ain,aout
      
      INTEGER i,j,k

      INTEGER CCTK_Equals

      pi = 3.14159265358979d0

c     Wave characteristics

c     amplitude is declared by DECLARE_PARAMETER, for all other parameters
c     we stick to the naming in the cactus3.2.0 version of cactus

c     wavecentering
      ra   = wavecenter
     
c     wavelength
      wave = wavelength   

c     wavepulse
      wavep= wavepulse     

c     determine whether the wave is ingoing, outgoing, or both
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

c     determine the direction for the plane wave to travel
c     and convert it from degrees to radians
      the = pi*wavetheta/180.d0
      phi = pi*wavephi/180.d0
      
      call CCTK_INFO('Plane waves')
      write(infoline,'(A13,G12.7)')
     &    ' amplitude = ',amplitude
      call CCTK_INFO(infoline)
      write(infoline,'(A14,G12.7)')
     &    ' wavecenter = ',ra
      call CCTK_INFO(infoline)
      write(infoline,'(A14,G12.7)')
     &    ' wavelength = ',wave
      call CCTK_INFO(infoline)
      write(infoline,'(A14,G12.7)')
     &    ' wavepulse = ',wavep
      call CCTK_INFO(infoline)

c     precalc
      kx = 2*pi*sin(the)*cos(phi)/wave
      ky = 2*pi*sin(the)*sin(phi)/wave
      kz = 2*pi*cos(the)/wave
      w = (kx*kx+ky*ky+kz*kz)**0.5
      
      kxp = 2*pi*sin(the)*cos(phi)/wavep
      kyp = 2*pi*sin(the)*sin(phi)/wavep
      kzp = 2*pi*cos(the)/wavep
      wp = (kxp*kxp+kyp*kyp+kzp*kzp)**0.5

c  *************** plane waves ********************
      do k=1,cctk_lsh(3)
         do j=1,cctk_lsh(2)
            do i=1,cctk_lsh(1)		 
               plus = (kx*x(i,j,k)+ky*y(i,j,k)+kz*z(i,j,k)+w*cctk_time)
               minus = (kx*x(i,j,k)+ky*y(i,j,k)+kz*z(i,j,k)-w*cctk_time)
               
               plusp =(kxp*x(i,j,k)+kyp*y(i,j,k)+kzp*z(i,j,k)+wp*(cctk_time-ra))
               minusp =(kxp*x(i,j,k)+kyp*y(i,j,k)+kzp*z(i,j,k)-wp*(cctk_time-ra))
               
               ain = iin*amplitude*cos(plus)*exp(-(plusp)**2)
               aout = iout*amplitude*cos(minus)*exp(-(minusp)**2)
	
c     the metric functions
        gxx(i,j,k) = cos(the)**2*cos(phi)**2*(1+ain+aout) +
     &     sin(phi)**2*(1-ain-aout) + sin(the)**2*cos(phi)**2

	gxy(i,j,k) = cos(the)**2*sin(phi)*cos(phi)*(1+ain+aout) +
     &    sin(phi)*cos(phi)*(1-ain-aout) - sin(the)**2*sin(phi)*cos(phi)

	gxz(i,j,k) = sin(the)*cos(the)*cos(phi)*(1+ain+aout) -
     &    sin(the)*cos(the)*cos(phi)

	gyy(i,j,k) = cos(the)**2*sin(phi)**2*(1+ain+aout) +
     &    cos(phi)**2*(1-ain-aout) + sin(the)**2*sin(phi)**2

	gyz(i,j,k) = -sin(the)*cos(the)*sin(phi)*(1+ain+aout) +
     &    sin(the)*cos(the)*sin(phi)

	gzz(i,j,k) = sin(the)**2*(1+ain+aout) + cos(the)**2


c     At this pint, the CAC3.2.0 version has some code for the dxgxx,....
c     defined, how does CAC4.0 handles this without  #ifdef THORN_BONAMASSO ?
        
c  and finally the extrinsic curvatures
c       Joan: I removed the division by alp as the initialization
c       of the lapse may take place later. This data is consistent
c       for initial lapse equal one.
	
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
	

c     loop over sh ends here:
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
      




        
