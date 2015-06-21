
#define PROBLEM_SIZE 40
#define PRECISION REAL*4

c Change 001, by BRN:  eliminate ..._psi4 and ..._fac variables, and
c simplify expressions that used them.

c Change 002, by BRN:  elimination of simple A=B assignments.

c Change 003, by BRN:  Hoist loop invars.  Further expr simplification
c and simple assignment elimination, especially involving T.. variables.

c Change 004, by BRN:  Strength reduction of some divides, and
c                  Assignment of CSEs to tmp1212, tmp1313, tmp2323

c Change 005, by BRN:  More assignment of CSEs to tmps

c Change 006, by BRN:  Assignment of CSE to tmpgam123111

c Change 007, by BRN:  Assignment of CSE to tmpgam123112

c Change 008, by BRN:  Assignment of CSE to tmpgam123113

c Change 009, by BRN:  Mere formatting clean-up.

c Change 010, by BRN:  Eliminate unary sign operators.

c Change 011, by BRN:  Eliminate mod operators.

c Change 012, by BRN:  Set array dimensions to the constant 40.

c Change 013, by BRN:  Delete out dead code.

c Change 014, by BRN:  Assignment of CSE to tmpk...

c Change 015, by BRN:  Clean-up and reordering code

c Change 016, by BRN:  Combine seven intermediate arrays into one.

c Change 017, by BRN:  Write commented-out assembly equivalent.

c Change 018, by BRN:  Combine similar arg arrays and simplify linkage.

c Change 019, by BRN:  Reduce mem reqs or l array to more reasonable.

c Revision by FA:  Reviewed each statement of the optimized code against the original kernel for bugs in logic and overall consistency.

C Change 020, by FA: added printed of output array

c Change 021, by FA:  Commented all statements related to "output" for optimized run.





      subroutine Bench_StaggeredLeapfrog2(
     &   cctk_delta_time,   cctk_delta_space,
     &   ADM_metric_prev0,  ADM_metric_prev1,  ADM_metric_prev2,
     &   ADM_curv_stag0,    ADM_curv_stag1,    ADM_curv_stag2,
     &   lapse0,            lapse1,            lapse2,
     &   ADM_g_n,           ADM_g,
     &   ADM_k_stag_n,      ADM_k_stag,        ADM_k_stag_p,
     &   alp)
      implicit none

c     FIX PROBLEM SIZE
c     ================
      integer,parameter :: nx = PROBLEM_SIZE
      integer,parameter :: ny = PROBLEM_SIZE
      integer,parameter :: nz = PROBLEM_SIZE

      PRECISION cctk_delta_time
      PRECISION cctk_delta_space(3)
      INTEGER ADM_curv_stag0
      INTEGER ADM_curv_stag1
      INTEGER ADM_curv_stag2
      INTEGER ADM_metric_prev0
      INTEGER ADM_metric_prev1
      INTEGER ADM_metric_prev2
      PRECISION
     &    ADM_g_n(6,ADM_metric_prev0,ADM_metric_prev1,ADM_metric_prev2),
     &    ADM_g  (6,ADM_metric_prev0,ADM_metric_prev1,ADM_metric_prev2),
     &    ADM_k_stag_n(6,ADM_curv_stag0,ADM_curv_stag1,ADM_curv_stag2),
     &    ADM_k_stag  (6,ADM_curv_stag0,ADM_curv_stag1,ADM_curv_stag2),
     &    ADM_k_stag_p(6,ADM_curv_stag0,ADM_curv_stag1,ADM_curv_stag2)
      INTEGER lapse0
      INTEGER lapse1
      INTEGER lapse2
      PRECISION alp(lapse0,lapse1,lapse2)
      
      integer :: i,j,k
      integer :: im,i0,ip,jm,j0,jp,km,k0,kp
       PRECISION :: dx,dy,dz,dt,i2dx,i2dy,i2dz,idx2,idy2,idz2
       PRECISION :: i2dxdiv2,i2dydiv2,i2dzdiv2
       PRECISION :: i4dxdy,i4dxdz,i4dydz
       PRECISION   detg_tempxx ,detg_tempxy ,detg_tempxz 
       PRECISION   detg_tempyy ,detg_tempyz ,detg_tempzz 
       PRECISION   detg_detcg 
       PRECISION   uppermet_fdet 
       PRECISION   uppermet_uxx , uppermet_uxy , uppermet_uxz 
       PRECISION   uppermet_uyy , uppermet_uyz , uppermet_uzz 
       PRECISION   dxdgxydiv2,dxdgxzdiv2,dxdgyzdiv2
       PRECISION   dydgxydiv2,dydgxzdiv2,dydgyzdiv2
       PRECISION   dzdgxydiv2,dzdgxzdiv2,dzdgyzdiv2
       PRECISION   dxxdgxx,dxxdgxy,dxxdgxz,dxxdgyy,dxxdgyz,dxxdgzz 
       PRECISION   deldelg1212,deldelg1213,deldelg1223,deldelg1233 
       PRECISION   deldelg1312,deldelg1313,deldelg1322,deldelg1323
       PRECISION   dyydgxx,dyydgxy,dyydgxz,dyydgyy,dyydgyz,dyydgzz 
       PRECISION   deldelg2311,deldelg2312,deldelg2313,deldelg2323 
       PRECISION   dzzdgxx,dzzdgxy,dzzdgxz,dzzdgyy,dzzdgyz 
       PRECISION   gammado111,gammado112,gammado113,gammado122,gammado123 
       PRECISION   gammado133,gammado211,gammado212,gammado213,gammado222 
       PRECISION   gammado223,gammado233,gammado311,gammado312,gammado313 
       PRECISION   gammado322,gammado323,gammado333 
       PRECISION   gamma111,gamma112,gamma113,gamma122,gamma123,gamma133 
       PRECISION   gamma211,gamma212,gamma213,gamma222,gamma223,gamma233 
       PRECISION   gamma311,gamma312,gamma313,gamma322,gamma323,gamma333 
       PRECISION   ricci_R11,ricci_R12,ricci_R13,
     &          ricci_R22,ricci_R23,ricci_R33 
       PRECISION   KK11,KK12,KK13,KK22,KK23,KK33 
       PRECISION   trk_trK 
       PRECISION   da_dxda,da_dyda,da_dzda 
       PRECISION   dda_dxxda,dda_dxyda,dda_dxzda,
     &          dda_dyyda,dda_dyzda,dda_dzzda 
       PRECISION   cdcda_cdxxda,cdcda_cdxyda,cdcda_cdxzda 
       PRECISION   cdcda_cdyyda,cdcda_cdyzda,cdcda_cdzzda 
       PRECISION   dkdt_dkxxdt,dkdt_dkxydt,dkdt_dkxzdt 
       PRECISION   dkdt_dkyydt,dkdt_dkyzdt,dkdt_dkzzdt 
       PRECISION zero,one,pi8,half,fac
c      PRECISION,DIMENSION(40,40,3):: lgxx,lgxy,lgxz,lgyy,lgyz,lgzz,lalp
       PRECISION,DIMENSION(8,40,40,3):: l
       PRECISION gxxc,gxyc,gxzc,gyyc,gyzc,gzzc
       PRECISION kxxc,kxyc,kxzc,kyyc,kyzc,kzzc
       PRECISION tmp1212, tmp1313, tmp2323
       PRECISION tmpdxxdgyz, tmpdyydgxz, tmpdzzdgxy
       PRECISION tmpgam123111, tmpgam123112, tmpgam123113
       PRECISION tmpkxxc, tmpkyyc, tmpkzzc, tmpkxyc2, tmpkxzc2, tmpkyzc2
       PRECISION tmpricci1, tmpricci2, tmpricci3
       PRECISION tmpkxyckxzc, tmpkxyckyzc, tmpkxzckyzc
       integer,parameter :: xx=1
       integer,parameter :: xy=2
       integer,parameter :: xz=3
       integer,parameter :: yy=4
       integer,parameter :: yz=5
       integer,parameter :: zz=6
       integer,parameter :: lgxx=xx
       integer,parameter :: lgxy=xy
       integer,parameter :: lgxz=xz
       integer,parameter :: lgyy=yy
       integer,parameter :: lgyz=yz
       integer,parameter :: lgzz=zz
       integer,parameter :: lalp=7

c       PRECISION output(144)
c       common /dump/ output

      half = 0.5D0
      dt = cctk_delta_time
      dx = cctk_delta_space(1)
      dy = cctk_delta_space(2)
      dz = cctk_delta_space(3)
      i2dx = 1d0/(2d0*dx)
      i2dy = 1d0/(2d0*dy)
      i2dz = 1d0/(2d0*dz)

      i2dxdiv2 = i2dx*0.5d0
      i2dydiv2 = i2dy*0.5d0
      i2dzdiv2 = i2dz*0.5d0

      i4dxdy = i2dx * i2dy
      i4dxdz = i2dx * i2dz
      i4dydz = i2dy * i2dz

      idx2 = i2dx * i2dx * 4d0
      idy2 = i2dy * i2dy * 4d0
      idz2 = i2dz * i2dz * 4d0

      km = 1
      k0 = 2

      do k = 0,nz-1
       kp = km
       km = k0
       k0 = 6-kp-km

       do j=1,ny
        do i=1,nx
         fac = 2.0d0*dt*alp(i,j,k+1)
         ADM_g_n(xx,i,j,k+1) = ADM_g(xx,i,j,k+1)-
     &                         ADM_k_stag(xx,i,j,k+1)*fac
         ADM_g_n(xy,i,j,k+1) = ADM_g(xy,i,j,k+1)-
     &                         ADM_k_stag(xy,i,j,k+1)*fac
         ADM_g_n(xz,i,j,k+1) = ADM_g(xz,i,j,k+1)-
     &                         ADM_k_stag(xz,i,j,k+1)*fac
         ADM_g_n(yy,i,j,k+1) = ADM_g(yy,i,j,k+1)-
     &                         ADM_k_stag(yy,i,j,k+1)*fac
         ADM_g_n(yz,i,j,k+1) = ADM_g(yz,i,j,k+1)-
     &                         ADM_k_stag(yz,i,j,k+1)*fac
         ADM_g_n(zz,i,j,k+1) = ADM_g(zz,i,j,k+1)-
     &                         ADM_k_stag(zz,i,j,k+1)*fac
         l(lgxx,i,j,kp) = ADM_g_n(xx,i,j,k+1)
         l(lgxy,i,j,kp) = ADM_g_n(xy,i,j,k+1)
         l(lgxz,i,j,kp) = ADM_g_n(xz,i,j,k+1)
         l(lgyy,i,j,kp) = ADM_g_n(yy,i,j,k+1)
         l(lgyz,i,j,kp) = ADM_g_n(yz,i,j,k+1)
         l(lgzz,i,j,kp) = ADM_g_n(zz,i,j,k+1)
         l(lalp,i,j,kp) = alp(i,j,k+1)
        end do
       end do

       if (k < 2) cycle

       do j=2,ny-1
        jp=j+1
        j0=j
        jm=j-1
        do i=2,nx-1
         ip=i+1
         i0=i
         im=i-1 

C uncomment the following lines to get the compiler to set-up these
C array addresses and scalar values, which are needed by the
C hand-coded assembly code for the body of the loop.

C        output(i-1)=0.5
C        if (i<4) then
C          l(lgxx,i0,j0,k0) = 2.0
C          l(lgxx,i0,j0,km) = dt
C          l(lgxx,i0,j0,kp) = dx
C          l(lgxx,i0,jp,k0) = dy
C          ADM_k_stag_n(xx,i,j,k) = dz
C          ADM_k_stag  (xx,i,j,k) = i2dx
C          ADM_k_stag_p(xx,i,j,k) = i2dy
C        elseif (i<6)
C          l(lgxx,i0,j0,k0) = i2dz
C          l(lgxx,i0,j0,km) = i2dxdiv2
C          l(lgxx,i0,j0,kp) = i2dydiv2
C          l(lgxx,i0,jp,k0) = i2dzdiv2
C          ADM_k_stag_n(xx,i,j,k) = i4dxdy
C          ADM_k_stag  (xx,i,j,k) = i4dxdz
C          ADM_k_stag_p(xx,i,j,k) = i4dydz
C        else
C          l(lgxx,i0,j0,k0) = 0.0
C          l(lgxx,i0,j0,km) = 0.0
C          l(lgxx,i0,j0,kp) = 0.0
C          l(lgxx,i0,jp,k0) = 0.0
C          ADM_k_stag_n(xx,i,j,k) = idx2
C          ADM_k_stag  (xx,i,j,k) = idy2
C          ADM_k_stag_p(xx,i,j,k) = idz2
C        endif


ci      add       Rpl_jp_k0=Rpl_j0_k0,Rl_lin_inc_for_4th_idx
ci      sub       Rpl_jm_k0=Rpl_j0_k0,Rl_lin_inc_for_4th_idx
ci      add       Rpl_jp_kp=Rpl_j0_kp,Rl_lin_inc_for_4th_idx
ci      sub       Rpl_jm_kp=Rpl_j0_kp,Rl_lin_inc_for_4th_idx
ci      add       Rpl_jp_km=Rpl_j0_km,Rl_lin_inc_for_4th_idx
ci      sub       Rpl_jm_km=Rpl_j0_km,Rl_lin_inc_for_4th_idx
ci      add       Rpl_jp_kp=16,Rpl_jp_kp
ci      add       Rpl_jm_kp=16,Rpl_jm_kp
ci      add       Rpl_jp_km=16,Rpl_jp_km
ci      add       Rpl_jm_km=16,Rpl_jm_km


         gxxc = l(lgxx,i0,j0,k0)
         gxyc = l(lgxy,i0,j0,k0)
         gxzc = l(lgxz,i0,j0,k0)
         gyyc = l(lgyy,i0,j0,k0)
         gyzc = l(lgyz,i0,j0,k0)
         gzzc = l(lgzz,i0,j0,k0)

cm      ld8       Rlgxxxylo=[Rpl_j0_k0],8
cm      ld8       Rlgxzyylo=[Rpl_j0_k0],8
cm      ld8       Rlgyzzzlo=[Rpl_j0_k0],16
cm      ld8       Rlgxxxyhi=[Rpl_j0_k0],8
cm      ld8       Rlgxzyyhi=[Rpl_j0_k0],8
cm      ld8       Rlgyzzzhi=[Rpl_j0_k0],-48-32
ci      unpack4.l Rgxxc=Rlgxxxyhi,Rlgxxxylo
ci      unpack4.h Rgxyc=Rlgxxxyhi,Rlgxxxylo
ci      unpack4.l Rgxzc=Rlgxzyyhi,Rlgxzyylo
ci      unpack4.h Rgyyc=Rlgxzyyhi,Rlgxzyylo
ci      unpack4.l Rgyzc=Rlgyzzzhi,Rlgyzzzlo
ci      unpack4.h Rgzzc=Rlgyzzzhi,Rlgyzzzlo
cm      setf.sig  Fgxxc=Rgxxc
cm      setf.sig  Fgxyc=Rgxyc
cm      setf.sig  Fgxzc=Rgxzc
cm      setf.sig  Fgyyc=Rgyyc
cm      setf.sig  Fgyzc=Rgyzc
cm      setf.sig  Fgzzc=Rgzzc

c        output(  1)=gxxc
c        output(  2)=gxyc
c        output(  3)=gxzc
c        output(  4)=gyyc
c        output(  5)=gyzc
c        output(  6)=gzzc
cd      getf.sig  Rdmptmp=Fgxxc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgxyc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgxzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgyyc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgyzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgzzc
cd      st4       [Rpoutput]=Rdmptmp,4


         detg_tempxx = gyyc*gzzc-gyzc*gyzc
         detg_tempxy = gxzc*gyzc-gxyc*gzzc
         detg_tempxz = gxyc*gyzc-gxzc*gyyc
         detg_tempyy = gxxc*gzzc-gxzc*gxzc
         detg_tempyz = gxyc*gxzc-gxxc*gyzc
         detg_tempzz = gxxc*gyyc-gxyc*gxyc

cf      fpma      Fdetg_tempxx=Fgyyc,Fgzzc,f0
cf      fpma      Fdetg_tempxy=Fgxzc,Fgyzc,f0
cf      fpma      Fdetg_tempxz=Fgxyc,Fgyzc,f0
cf      fpma      Fdetg_tempyy=Fgxxc,Fgzzc,f0
cf      fpma      Fdetg_tempyz=Fgxyc,Fgxzc,f0
cf      fpma      Fdetg_tempzz=Fgxxc,Fgyyc,f0
cf      fpnma     Fdetg_tempxx=Fgyzc,Fgyzc,Fdetg_tempxx
cf      fpnma     Fdetg_tempxy=Fgxyc,Fgzzc,Fdetg_tempxy
cf      fpnma     Fdetg_tempxz=Fgxzc,Fgyyc,Fdetg_tempxz
cf      fpnma     Fdetg_tempyy=Fgxzc,Fgxzc,Fdetg_tempyy
cf      fpnma     Fdetg_tempyz=Fgxxc,Fgyzc,Fdetg_tempyz
cf      fpnma     Fdetg_tempzz=Fgxyc,Fgxyc,Fdetg_tempzz

c        output(  7)=detg_tempxx
c        output(  8)=detg_tempxy
c        output(  9)=detg_tempxz
c        output( 10)=detg_tempyy
c        output( 11)=detg_tempyz
c        output( 12)=detg_tempzz
cd      getf.sig  Rdmptmp=Fdetg_tempxx
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdetg_tempxy
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdetg_tempxz
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdetg_tempyy
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdetg_tempyz
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdetg_tempzz
cd      st4       [Rpoutput]=Rdmptmp,4


         detg_detcg  = detg_tempxx*gxxc+
     &                 detg_tempxy*gxyc+
     &                 detg_tempxz*gxzc

cf      fpma      Fdetg_detcg=Fdetg_tempxx,Fgxxc
cf      fpma      Fdetg_detcg=detg_tempxy,Fgxyc,Fdetg_detcg
cf      fpma      Fdetg_detcg=detg_tempxz,Fgxzc,Fdetg_detcg

c        output( 13)=detg_detcg
cd      getf.sig  Rdmptmp=Fdetg_detcg
cd      st4       [Rpoutput]=Rdmptmp,4


         uppermet_fdet  = 1d0/detg_detcg

cf      fprcpa    Frcp1,Piter=f1,Fdetg_detcg  //0
cf(Piter) fpnma.s1  Frcp2=Frcp1,Fdetg_detcg,f1  //5
cf(Piter) fpma.s1   Frcp3=f1,Frcp1,f0           //5
cf(Piter) fpma.s1   Frcp4=Frcp2,Frcp2,f0        //10
cf(Piter) fpma.s1   Frcp5=Frcp3,Frcp2,Frcp3     //10
cf(Piter) fpma.s1   Frcp6=Frcp1,Frcp2,Frcp1     //11
cf(Piter) fpma.s1   Frcp7=Frcp4,Frcp4,f0        //15
cf(Piter) fpma.s1   Frcp8=Frcp5,Frcp4,Frcp5     //15
cf(Piter) fpma.s1   Frcp9=Frcp6,Frcp4,Frcp6     //16
cf(Piter) fpma.d.s1 FrcpA=Frcp8,Frcp7,Frcp8     //20
cf(Piter) fpma.s1   FrcpB=Frcp9,Frcp7,Frcp9     //21
cf(Piter) fpnma.s1  FrcpC=Fdetg_detcg,FrcpA,f1  //25
cf(Piter) fpma.d    Frcp1=FrcpC,FrcpB,FrcpA     //30
cf      fpma.s    Fuppermet_fdet=Frcp1,f1,f0  //35

c        output( 14)=uppermet_fdet
cd      getf.sig  Rdmptmp=Fuppermet_fdet
cd      st4       [Rpoutput]=Rdmptmp,4

         uppermet_uxx  = detg_tempxx*uppermet_fdet 
         uppermet_uxy  = detg_tempxy*uppermet_fdet 
         uppermet_uxz  = detg_tempxz*uppermet_fdet 
         uppermet_uyy  = detg_tempyy*uppermet_fdet 
         uppermet_uyz  = detg_tempyz*uppermet_fdet 
         uppermet_uzz  = detg_tempzz*uppermet_fdet 

cf      fpma      Fuppermet_uxx=Fdetg_tempxx,Fuppermet_fdet 
cf      fpma      Fuppermet_uxy=Fdetg_tempxy,Fuppermet_fdet 
cf      fpma      Fuppermet_uxz=Fdetg_tempxz,Fuppermet_fdet 
cf      fpma      Fuppermet_uyy=Fdetg_tempyy,Fuppermet_fdet 
cf      fpma      Fuppermet_uyz=Fdetg_tempyz,Fuppermet_fdet 
cf      fpma      Fuppermet_uzz=Fdetg_tempzz,Fuppermet_fdet 

c        output( 15)=uppermet_uxx
c        output( 16)=uppermet_uxy
c        output( 17)=uppermet_uxz
c        output( 18)=uppermet_uyy
c        output( 19)=uppermet_uyz
c        output( 20)=uppermet_uzz
cd      getf.sig  Rdmptmp=Fuppermet_uxx
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fuppermet_uxy
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fuppermet_uxz
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fuppermet_uyy
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fuppermet_uyz
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fuppermet_uzz
cd      st4       [Rpoutput]=Rdmptmp,4


         gammado111 = i2dxdiv2*(l(lgxx,ip,j0,k0)-l(lgxx,im,j0,k0))
         dxdgxydiv2 = i2dxdiv2*(l(lgxy,ip,j0,k0)-l(lgxy,im,j0,k0))
         dxdgxzdiv2 = i2dxdiv2*(l(lgxz,ip,j0,k0)-l(lgxz,im,j0,k0))
         gammado212 = i2dxdiv2*(l(lgyy,ip,j0,k0)-l(lgyy,im,j0,k0))
         dxdgyzdiv2 = i2dxdiv2*(l(lgyz,ip,j0,k0)-l(lgyz,im,j0,k0))
         gammado313 = i2dxdiv2*(l(lgzz,ip,j0,k0)-l(lgzz,im,j0,k0))

cm      ld8       Rlgxxxy_im_j0_k0_lo=[Rpl_j0_k0],8
cm      ld8       Rlgxzyy_im_j0_k0_lo=[Rpl_j0_k0],8
cm      ld8       Rlgyzzz_im_j0_k0_lo=[Rpl_j0_k0],16
cm      ld8       Rlgxxxy_im_j0_k0_hi=[Rpl_j0_k0],8
cm      ld8       Rlgxzyy_im_j0_k0_hi=[Rpl_j0_k0],8
cm      ld8       Rlgyzzz_im_j0_k0_hi=[Rpl_j0_k0],16
ci      unpack4.l Rlgxx_im_j0_k0=Rlgxxxy_im_j0_k0_hi,Rlgxxxy_im_j0_k0_lo
ci      unpack4.h Rlgxy_im_j0_k0=Rlgxxxy_im_j0_k0_hi,Rlgxxxy_im_j0_k0_lo
ci      unpack4.l Rlgxz_im_j0_k0=Rlgxzyy_im_j0_k0_hi,Rlgxzyy_im_j0_k0_lo
ci      unpack4.h Rlgyy_im_j0_k0=Rlgxzyy_im_j0_k0_hi,Rlgxzyy_im_j0_k0_lo
ci      unpack4.l Rlgyz_im_j0_k0=Rlgyzzz_im_j0_k0_hi,Rlgyzzz_im_j0_k0_lo
ci      unpack4.h Rlgzz_im_j0_k0=Rlgyzzz_im_j0_k0_hi,Rlgyzzz_im_j0_k0_lo
cm      setf.sig  Fgxxc_im_j0_k0=Rgxxc_im_j0_k0
cm      setf.sig  Fgxyc_im_j0_k0=Rgxyc_im_j0_k0
cm      setf.sig  Fgxzc_im_j0_k0=Rgxzc_im_j0_k0
cm      setf.sig  Fgyyc_im_j0_k0=Rgyyc_im_j0_k0
cm      setf.sig  Fgyzc_im_j0_k0=Rgyzc_im_j0_k0
cm      setf.sig  Fgzzc_im_j0_k0=Rgzzc_im_j0_k0
cm      ld8       Rlgxxxy_ip_j0_k0_lo=[Rpl_j0_k0],8
cm      ld8       Rlgxzyy_ip_j0_k0_lo=[Rpl_j0_k0],8
cm      ld8       Rlgyzzz_ip_j0_k0_lo=[Rpl_j0_k0],16
cm      ld8       Rlgxxxy_ip_j0_k0_hi=[Rpl_j0_k0],8
cm      ld8       Rlgxzyy_ip_j0_k0_hi=[Rpl_j0_k0],8
cm      ld8       Rlgyzzz_ip_j0_k0_hi=[Rpl_j0_k0],-24
ci      unpack4.l Rlgxx_ip_j0_k0=Rlgxxxy_ip_j0_k0_hi,Rlgxxxy_ip_j0_k0_lo
ci      unpack4.h Rlgxy_ip_j0_k0=Rlgxxxy_ip_j0_k0_hi,Rlgxxxy_ip_j0_k0_lo
ci      unpack4.l Rlgxz_ip_j0_k0=Rlgxzyy_ip_j0_k0_hi,Rlgxzyy_ip_j0_k0_lo
ci      unpack4.h Rlgyy_ip_j0_k0=Rlgxzyy_ip_j0_k0_hi,Rlgxzyy_ip_j0_k0_lo
ci      unpack4.l Rlgyz_ip_j0_k0=Rlgyzzz_ip_j0_k0_hi,Rlgyzzz_ip_j0_k0_lo
ci      unpack4.h Rlgzz_ip_j0_k0=Rlgyzzz_ip_j0_k0_hi,Rlgyzzz_ip_j0_k0_lo
cm      setf.sig  Fgxxc_ip_j0_k0=Rgxxc_ip_j0_k0
cm      setf.sig  Fgxyc_ip_j0_k0=Rgxyc_ip_j0_k0
cm      setf.sig  Fgxzc_ip_j0_k0=Rgxzc_ip_j0_k0
cm      setf.sig  Fgyyc_ip_j0_k0=Rgyyc_ip_j0_k0
cm      setf.sig  Fgyzc_ip_j0_k0=Rgyzc_ip_j0_k0
cm      setf.sig  Fgzzc_ip_j0_k0=Rgzzc_ip_j0_k0
cf      fpms      Fgammado111=Flgxx_ip_j0_k0,f1,Flgxx_im_j0_k0
cf      fpms      Fdxdgxydiv2=Flgxy_ip_j0_k0,f1,Flgxy_im_j0_k0
cf      fpms      Fdxdgxzdiv2=Flgxz_ip_j0_k0,f1,Flgxz_im_j0_k0
cf      fpms      Fgammado212=Flgyy_ip_j0_k0,f1,Flgyy_im_j0_k0
cf      fpms      Fdxdgyzdiv2=Flgyz_ip_j0_k0,f1,Flgyz_im_j0_k0
cf      fpms      Fgammado313=Flgzz_ip_j0_k0,f1,Flgzz_im_j0_k0
cf      fpma      Fgammado111=Fgammado111,Fi2dxdiv2,f0
cf      fpma      Fdxdgxydiv2=Fdxdgxydiv2,Fi2dxdiv2,f0
cf      fpma      Fdxdgxzdiv2=Fdxdgxzdiv2,Fi2dxdiv2,f0
cf      fpma      Fgammado212=Fgammado212,Fi2dxdiv2,f0
cf      fpma      Fdxdgyzdiv2=Fdxdgyzdiv2,Fi2dxdiv2,f0
cf      fpma      Fgammado313=Fgammado313,Fi2dxdiv2,f0

c        output( 21)=gammado111
c        output( 22)=dxdgxydiv2
c        output( 23)=dxdgxzdiv2
c        output( 24)=gammado212
c        output( 25)=dxdgyzdiv2
c        output( 26)=gammado313
cd      getf.sig  Rdmptmp=Fgammado111
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdxdgxydiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdxdgxzdiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado212
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdxdgyzdiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado313
cd      st4       [Rpoutput]=Rdmptmp,4


         dxxdgyy = idx2*(l(lgyy,im,j0,k0)-2*gyyc+l(lgyy,ip,j0,k0))
         dxxdgzz = idx2*(l(lgzz,im,j0,k0)-2*gzzc+l(lgzz,ip,j0,k0))
         dxxdgyz = idx2*(l(lgyz,im,j0,k0)-2*gyzc+l(lgyz,ip,j0,k0))

cf      fpma      Fdxxdgyy=Flgyy_ip_j0_k0,f1,Flgyy_im_j0_k0
cf      fpma      Fdxxdgzz=Flgzz_ip_j0_k0,f1,Flgzz_im_j0_k0
cf      fpma      Fdxxdgyz=Flgyz_ip_j0_k0,f1,Flgyz_im_j0_k0
cf      fpnma     Fdxxdgyy=Fgyyc,Ftwo,Fdxxdgyy
cf      fpnma     Fdxxdgzz=Fgzzc,Ftwo,Fdxxdgzz
cf      fpnma     Fdxxdgyz=Fgyzc,Ftwo,Fdxxdgyz
cf      fpma      Fdxxdgyy=Fdxxdgyy,Fidx2,f0
cf      fpma      Fdxxdgzz=Fdxxdgzz,Fidx2,f0
cf      fpma      Fdxxdgyz=Fdxxdgyz,Fidx2,f0

c        output( 27)=dxxdgyy
c        output( 28)=dxxdgzz
c        output( 29)=dxxdgyz
cd      getf.sig  Rdmptmp=Fdxxdgyy
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdxxdgzz
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdxxdgyz
cd      st4       [Rpoutput]=Rdmptmp,4


         gammado112 = i2dydiv2*(l(lgxx,i0,jp,k0)-l(lgxx,i0,jm,k0))
         dydgxydiv2 = i2dydiv2*(l(lgxy,i0,jp,k0)-l(lgxy,i0,jm,k0))
         dydgxzdiv2 = i2dydiv2*(l(lgxz,i0,jp,k0)-l(lgxz,i0,jm,k0))
         gammado222 = i2dydiv2*(l(lgyy,i0,jp,k0)-l(lgyy,i0,jm,k0))
         dydgyzdiv2 = i2dydiv2*(l(lgyz,i0,jp,k0)-l(lgyz,i0,jm,k0))
         gammado323 = i2dydiv2*(l(lgzz,i0,jp,k0)-l(lgzz,i0,jm,k0))

cm      ld8       Rlgxxxy_i0_jm_k0_lo=[Rpl_jm_k0],8
cm      ld8       Rlgxzyy_i0_jm_k0_lo=[Rpl_jm_k0],8
cm      ld8       Rlgyzzz_i0_jm_k0_lo=[Rpl_jm_k0],16
cm      ld8       Rlgxxxy_i0_jm_k0_hi=[Rpl_jm_k0],8
cm      ld8       Rlgxzyy_i0_jm_k0_hi=[Rpl_jm_k0],8
cm      ld8       Rlgyzzz_i0_jm_k0_hi=[Rpl_jm_k0],-12
ci      unpack4.l Rlgxx_i0_jm_k0=Rlgxxxy_i0_jm_k0_hi,Rlgxxxy_i0_jm_k0_lo
ci      unpack4.h Rlgxy_i0_jm_k0=Rlgxxxy_i0_jm_k0_hi,Rlgxxxy_i0_jm_k0_lo
ci      unpack4.l Rlgxz_i0_jm_k0=Rlgxzyy_i0_jm_k0_hi,Rlgxzyy_i0_jm_k0_lo
ci      unpack4.h Rlgyy_i0_jm_k0=Rlgxzyy_i0_jm_k0_hi,Rlgxzyy_i0_jm_k0_lo
ci      unpack4.l Rlgyz_i0_jm_k0=Rlgyzzz_i0_jm_k0_hi,Rlgyzzz_i0_jm_k0_lo
ci      unpack4.h Rlgzz_i0_jm_k0=Rlgyzzz_i0_jm_k0_hi,Rlgyzzz_i0_jm_k0_lo
cm      setf.sig  Fgxxc_i0_jm_k0=Rgxxc_i0_jm_k0
cm      setf.sig  Fgxyc_i0_jm_k0=Rgxyc_i0_jm_k0
cm      setf.sig  Fgxzc_i0_jm_k0=Rgxzc_i0_jm_k0
cm      setf.sig  Fgyyc_i0_jm_k0=Rgyyc_i0_jm_k0
cm      setf.sig  Fgyzc_i0_jm_k0=Rgyzc_i0_jm_k0
cm      setf.sig  Fgzzc_i0_jm_k0=Rgzzc_i0_jm_k0
cm      ld8       Rlgxxxy_i0_jp_k0_lo=[Rpl_jp_k0],8
cm      ld8       Rlgxzyy_i0_jp_k0_lo=[Rpl_jp_k0],8
cm      ld8       Rlgyzzz_i0_jp_k0_lo=[Rpl_jp_k0],16
cm      ld8       Rlgxxxy_i0_jp_k0_hi=[Rpl_jp_k0],8
cm      ld8       Rlgxzyy_i0_jp_k0_hi=[Rpl_jp_k0],8
cm      ld8       Rlgyzzz_i0_jp_k0_hi=[Rpl_jp_k0],-12
ci      unpack4.l Rlgxx_i0_jp_k0=Rlgxxxy_i0_jp_k0_hi,Rlgxxxy_i0_jp_k0_lo
ci      unpack4.h Rlgxy_i0_jp_k0=Rlgxxxy_i0_jp_k0_hi,Rlgxxxy_i0_jp_k0_lo
ci      unpack4.l Rlgxz_i0_jp_k0=Rlgxzyy_i0_jp_k0_hi,Rlgxzyy_i0_jp_k0_lo
ci      unpack4.h Rlgyy_i0_jp_k0=Rlgxzyy_i0_jp_k0_hi,Rlgxzyy_i0_jp_k0_lo
ci      unpack4.l Rlgyz_i0_jp_k0=Rlgyzzz_i0_jp_k0_hi,Rlgyzzz_i0_jp_k0_lo
ci      unpack4.h Rlgzz_i0_jp_k0=Rlgyzzz_i0_jp_k0_hi,Rlgyzzz_i0_jp_k0_lo
cm      setf.sig  Fgxxc_i0_jp_k0=Rgxxc_i0_jp_k0
cm      setf.sig  Fgxyc_i0_jp_k0=Rgxyc_i0_jp_k0
cm      setf.sig  Fgxzc_i0_jp_k0=Rgxzc_i0_jp_k0
cm      setf.sig  Fgyyc_i0_jp_k0=Rgyyc_i0_jp_k0
cm      setf.sig  Fgyzc_i0_jp_k0=Rgyzc_i0_jp_k0
cm      setf.sig  Fgzzc_i0_jp_k0=Rgzzc_i0_jp_k0
cf      fpms      Fgammado112=Flgxx_i0_jp_k0,f1,Flgxx_i0_jm_k0
cf      fpms      Fdydgxydiv2=Flgxy_i0_jp_k0,f1,Flgxy_i0_jm_k0
cf      fpms      Fdydgxzdiv2=Flgxz_i0_jp_k0,f1,Flgxz_i0_jm_k0
cf      fpms      Fgammado222=Flgyy_i0_jp_k0,f1,Flgyy_i0_jm_k0
cf      fpms      Fdydgyzdiv2=Flgyz_i0_jp_k0,f1,Flgyz_i0_jm_k0
cf      fpms      Fgammado323=Flgzz_i0_jp_k0,f1,Flgzz_i0_jm_k0
cf      fpma      Fgammado112=Fgammado112,Fi2dydiv2,f0
cf      fpma      Fdydgxydiv2=Fdydgxydiv2,Fi2dydiv2,f0
cf      fpma      Fdydgxzdiv2=Fdydgxzdiv2,Fi2dydiv2,f0
cf      fpma      Fgammado222=Fgammado222,Fi2dydiv2,f0
cf      fpma      Fdydgyzdiv2=Fdydgyzdiv2,Fi2dydiv2,f0
cf      fpma      Fgammado323=Fgammado323,Fi2dydiv2,f0

c        output( 30)=gammado112
c        output( 31)=dydgxydiv2
c        output( 32)=dydgxzdiv2
c        output( 33)=gammado222
c        output( 34)=dydgyzdiv2
c        output( 35)=gammado323
cd      getf.sig  Rdmptmp=Fgammado112
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdydgxydiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdydgxzdiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado222
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdydgyzdiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado323
cd      st4       [Rpoutput]=Rdmptmp,4


         dyydgxx = idy2*(l(lgxx,i0,jp,k0)-2*gxxc+l(lgxx,i0,jm,k0))
         dyydgzz = idy2*(l(lgzz,i0,jp,k0)-2*gzzc+l(lgzz,i0,jm,k0))
         dyydgxz = idy2*(l(lgxz,i0,jp,k0)-2*gxzc+l(lgxz,i0,jm,k0))

cf      fpma      Fdyydgxx=Flgxx_i0_jp_k0,f1,Flgxx_i0_jm_k0
cf      fpma      Fdyydgzz=Flgzz_i0_jp_k0,f1,Flgzz_i0_jm_k0
cf      fpma      Fdyydgxz=Flgxz_i0_jp_k0,f1,Flgxz_i0_jm_k0
cf      fpnma     Fdyydgxx=Fgxxc,Ftwo,Fdyydgxx
cf      fpnma     Fdyydgzz=Fgzzc,Ftwo,Fdyydgzz
cf      fpnma     Fdyydgxz=Fgxzc,Ftwo,Fdyydgxz
cf      fpma      Fdyydgxx=Fdyydgxx,Fidy2,f0
cf      fpma      Fdyydgzz=Fdyydgzz,Fidy2,f0
cf      fpma      Fdyydgxz=Fdyydgxz,Fidy2,f0

c        output( 36)=dyydgxx
c        output( 37)=dyydgzz
c        output( 38)=dyydgxz
cd      getf.sig  Rdmptmp=Fdyydgxx
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdyydgzz
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdyydgxz
cd      st4       [Rpoutput]=Rdmptmp,4


         gammado113 = i2dzdiv2*(l(lgxx,i0,j0,kp)-l(lgxx,i0,j0,km))
         dzdgxydiv2 = i2dzdiv2*(l(lgxy,i0,j0,kp)-l(lgxy,i0,j0,km))
         dzdgxzdiv2 = i2dzdiv2*(l(lgxz,i0,j0,kp)-l(lgxz,i0,j0,km))
         gammado223 = i2dzdiv2*(l(lgyy,i0,j0,kp)-l(lgyy,i0,j0,km))
         dzdgyzdiv2 = i2dzdiv2*(l(lgyz,i0,j0,kp)-l(lgyz,i0,j0,km))
         gammado333 = i2dzdiv2*(l(lgzz,i0,j0,kp)-l(lgzz,i0,j0,km))

cm      ld8       Rlgxxxy_i0_j0_km_lo=[Rpl_j0_km],8
cm      ld8       Rlgxzyy_i0_j0_km_lo=[Rpl_j0_km],8
cm      ld8       Rlgyzzz_i0_j0_km_lo=[Rpl_j0_km],16
cm      ld8       Rlgxxxy_i0_j0_km_hi=[Rpl_j0_km],8
cm      ld8       Rlgxzyy_i0_j0_km_hi=[Rpl_j0_km],8
cm      ld8       Rlgyzzz_i0_j0_km_hi=[Rpl_j0_km],-16
ci      unpack4.l Rlgxx_i0_j0_km=Rlgxxxy_i0_j0_km_hi,Rlgxxxy_i0_j0_km_lo
ci      unpack4.h Rlgxy_i0_j0_km=Rlgxxxy_i0_j0_km_hi,Rlgxxxy_i0_j0_km_lo
ci      unpack4.l Rlgxz_i0_j0_km=Rlgxzyy_i0_j0_km_hi,Rlgxzyy_i0_j0_km_lo
ci      unpack4.h Rlgyy_i0_j0_km=Rlgxzyy_i0_j0_km_hi,Rlgxzyy_i0_j0_km_lo
ci      unpack4.l Rlgyz_i0_j0_km=Rlgyzzz_i0_j0_km_hi,Rlgyzzz_i0_j0_km_lo
ci      unpack4.h Rlgzz_i0_j0_km=Rlgyzzz_i0_j0_km_hi,Rlgyzzz_i0_j0_km_lo
cm      setf.sig  Fgxxc_i0_j0_km=Rgxxc_i0_j0_km
cm      setf.sig  Fgxyc_i0_j0_km=Rgxyc_i0_j0_km
cm      setf.sig  Fgxzc_i0_j0_km=Rgxzc_i0_j0_km
cm      setf.sig  Fgyyc_i0_j0_km=Rgyyc_i0_j0_km
cm      setf.sig  Fgyzc_i0_j0_km=Rgyzc_i0_j0_km
cm      setf.sig  Fgzzc_i0_j0_km=Rgzzc_i0_j0_km
cm      ld8       Rlgxxxy_i0_j0_kp_lo=[Rpl_j0_kp],8
cm      ld8       Rlgxzyy_i0_j0_kp_lo=[Rpl_j0_kp],8
cm      ld8       Rlgyzzz_i0_j0_kp_lo=[Rpl_j0_kp],16
cm      ld8       Rlgxxxy_i0_j0_kp_hi=[Rpl_j0_kp],8
cm      ld8       Rlgxzyy_i0_j0_kp_hi=[Rpl_j0_kp],8
cm      ld8       Rlgyzzz_i0_j0_kp_hi=[Rpl_j0_kp],-16
ci      unpack4.l Rlgxx_i0_j0_kp=Rlgxxxy_i0_j0_kp_hi,Rlgxxxy_i0_j0_kp_lo
ci      unpack4.h Rlgxy_i0_j0_kp=Rlgxxxy_i0_j0_kp_hi,Rlgxxxy_i0_j0_kp_lo
ci      unpack4.l Rlgxz_i0_j0_kp=Rlgxzyy_i0_j0_kp_hi,Rlgxzyy_i0_j0_kp_lo
ci      unpack4.h Rlgyy_i0_j0_kp=Rlgxzyy_i0_j0_kp_hi,Rlgxzyy_i0_j0_kp_lo
ci      unpack4.l Rlgyz_i0_j0_kp=Rlgyzzz_i0_j0_kp_hi,Rlgyzzz_i0_j0_kp_lo
ci      unpack4.h Rlgzz_i0_j0_kp=Rlgyzzz_i0_j0_kp_hi,Rlgyzzz_i0_j0_kp_lo
cm      setf.sig  Fgxxc_i0_j0_kp=Rgxxc_i0_j0_kp
cm      setf.sig  Fgxyc_i0_j0_kp=Rgxyc_i0_j0_kp
cm      setf.sig  Fgxzc_i0_j0_kp=Rgxzc_i0_j0_kp
cm      setf.sig  Fgyyc_i0_j0_kp=Rgyyc_i0_j0_kp
cm      setf.sig  Fgyzc_i0_j0_kp=Rgyzc_i0_j0_kp
cm      setf.sig  Fgzzc_i0_j0_kp=Rgzzc_i0_j0_kp
cf      fpms      Fgammado113=Flgxx_i0_j0_kp,f1,Flgxx_i0_j0_km
cf      fpms      Fdzdgxydiv2=Flgxy_i0_j0_kp,f1,Flgxy_i0_j0_km
cf      fpms      Fdzdgxzdiv2=Flgxz_i0_j0_kp,f1,Flgxz_i0_j0_km
cf      fpms      Fgammado223=Flgyy_i0_j0_kp,f1,Flgyy_i0_j0_km
cf      fpms      Fdzdgyzdiv2=Flgyz_i0_j0_kp,f1,Flgyz_i0_j0_km
cf      fpms      Fgammado333=Flgzz_i0_j0_kp,f1,Flgzz_i0_j0_km
cf      fpma      Fgammado113=Fgammado113,Fi2dzdiv2,f0
cf      fpma      Fdzdgxydiv2=Fdzdgxydiv2,Fi2dzdiv2,f0
cf      fpma      Fdzdgxzdiv2=Fdzdgxzdiv2,Fi2dzdiv2,f0
cf      fpma      Fgammado223=Fgammado223,Fi2dzdiv2,f0
cf      fpma      Fdzdgyzdiv2=Fdzdgyzdiv2,Fi2dzdiv2,f0
cf      fpma      Fgammado333=Fgammado333,Fi2dzdiv2,f0

c        output( 39)=gammado113
c        output( 40)=dzdgxydiv2
c        output( 41)=dzdgxzdiv2
c        output( 42)=gammado223
c        output( 43)=dzdgyzdiv2
c        output( 44)=gammado333
cd      getf.sig  Rdmptmp=Fgammado113
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdzdgxydiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdzdgxzdiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado223
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdzdgyzdiv2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado333
cd      st4       [Rpoutput]=Rdmptmp,4


         dzzdgxx = idz2*(l(lgxx,i0,j0,kp)-2*gxxc+l(lgxx,i0,j0,km))
         dzzdgyy = idz2*(l(lgyy,i0,j0,kp)-2*gyyc+l(lgyy,i0,j0,km))
         dzzdgxy = idz2*(l(lgxy,i0,j0,kp)-2*gxyc+l(lgxy,i0,j0,km))

cf      fpma      Fdzzdgxx=Flgxx_i0_j0_kp,f1,Flgxx_i0_j0_km
cf      fpma      Fdzzdgyy=Flgyy_i0_j0_kp,f1,Flgyy_i0_j0_km
cf      fpma      Fdzzdgxy=Flgxy_i0_j0_kp,f1,Flgxy_i0_j0_km
cf      fpnma     Fdzzdgxx=Fgxxc,Ftwo,Fdzzdgxx
cf      fpnma     Fdzzdgyy=Fgyyc,Ftwo,Fdzzdgyy
cf      fpnma     Fdzzdgxy=Fgxyc,Ftwo,Fdzzdgxy
cf      fpma      Fdzzdgxx=Fdzzdgxx,Fidz2,f0
cf      fpma      Fdzzdgyy=Fdzzdgyy,Fidz2,f0
cf      fpma      Fdzzdgxy=Fdzzdgxy,Fidz2,f0

c        output( 45)=dzzdgxx
c        output( 46)=dzzdgyy
c        output( 47)=dzzdgxy
cd      getf.sig  Rdmptmp=Fdzzdgxx
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdzzdgyy
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fdzzdgxy
cd      st4       [Rpoutput]=Rdmptmp,4


         gammado213  = dxdgyzdiv2-dydgxzdiv2+dzdgxydiv2
         gammado122  = 2.0d0*dydgxydiv2-gammado212
         gammado133  = 2.0d0*dzdgxzdiv2-gammado313
         gammado211  = 2.0d0*dxdgxydiv2-gammado112
         gammado233  = 2.0d0*dzdgyzdiv2-gammado323
         gammado311  = 2.0d0*dxdgxzdiv2-gammado113
         gammado322  = 2.0d0*dydgyzdiv2-gammado223
         gammado123  = 2.0d0*dzdgxydiv2-gammado213
         gammado312  = 2.0d0*dxdgyzdiv2-gammado213

cf      fpms       Fgammado213=Fdxdgyzdiv2,f1,Fdydgxzdiv2
cf      fpma       Fgammado213=Fgammado213,f1,Fdzdgxydiv2
cf      fpms       Fgammado122=Fdydgxydiv2,Ftwo,Fgammado212
cf      fpms       Fgammado133=Fdzdgxzdiv2,Ftwo,Fgammado313
cf      fpms       Fgammado211=Fdxdgxydiv2,Ftwo,Fgammado112
cf      fpms       Fgammado233=Fdzdgyzdiv2,Ftwo,Fgammado323
cf      fpms       Fgammado311=Fdxdgxzdiv2,Ftwo,Fgammado113
cf      fpms       Fgammado322=Fdydgyzdiv2,Ftwo,Fgammado223
cf      fpms       Fgammado123=Fdzdgxydiv2,Ftwo,Fgammado213
cf      fpms       Fgammado312=Fdxdgyzdiv2,Ftwo,Fgammado213

c        output( 48)=gammado213
c        output( 49)=gammado122
c        output( 50)=gammado133
c        output( 51)=gammado211
c        output( 52)=gammado233
c        output( 53)=gammado311
c        output( 54)=gammado322
c        output( 55)=gammado123
c        output( 56)=gammado312
cd      getf.sig  Rdmptmp=Fgammado213
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado122
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado133
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado211
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado233
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado311
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado322
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado123
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgammado312
cd      st4       [Rpoutput]=Rdmptmp,4


         gamma111  = gammado111*uppermet_uxx+
     &               gammado211*uppermet_uxy+
     &               gammado311*uppermet_uxz 
         gamma112  = gammado112*uppermet_uxx+
     &               gammado212*uppermet_uxy+
     &               gammado312*uppermet_uxz 
         gamma113  = gammado113*uppermet_uxx+
     &               gammado213*uppermet_uxy+
     &               gammado313*uppermet_uxz 
         gamma122  = gammado122*uppermet_uxx+
     &               gammado222*uppermet_uxy+
     &               gammado322*uppermet_uxz 
         gamma123  = gammado123*uppermet_uxx+
     &               gammado223*uppermet_uxy+
     &               gammado323*uppermet_uxz 
         gamma133  = gammado133*uppermet_uxx+
     &               gammado233*uppermet_uxy+
     &               gammado333*uppermet_uxz 
         gamma211  = gammado111*uppermet_uxy+
     &               gammado211*uppermet_uyy+
     &               gammado311*uppermet_uyz 
         gamma212  = gammado112*uppermet_uxy+
     &               gammado212*uppermet_uyy+
     &               gammado312*uppermet_uyz 
         gamma213  = gammado113*uppermet_uxy+
     &               gammado213*uppermet_uyy+
     &               gammado313*uppermet_uyz 
         gamma222  = gammado122*uppermet_uxy+
     &               gammado222*uppermet_uyy+
     &               gammado322*uppermet_uyz 
         gamma223  = gammado123*uppermet_uxy+
     &               gammado223*uppermet_uyy+
     &               gammado323*uppermet_uyz 
         gamma233  = gammado133*uppermet_uxy+
     &               gammado233*uppermet_uyy+
     &               gammado333*uppermet_uyz 
         gamma311  = gammado111*uppermet_uxz+
     &               gammado211*uppermet_uyz+
     &               gammado311*uppermet_uzz 
         gamma312  = gammado112*uppermet_uxz+
     &               gammado212*uppermet_uyz+
     &               gammado312*uppermet_uzz 
         gamma313  = gammado113*uppermet_uxz+
     &               gammado213*uppermet_uyz+
     &               gammado313*uppermet_uzz 
         gamma322  = gammado122*uppermet_uxz+
     &               gammado222*uppermet_uyz+
     &               gammado322*uppermet_uzz 
         gamma323  = gammado123*uppermet_uxz+
     &               gammado223*uppermet_uyz+
     &               gammado323*uppermet_uzz 
         gamma333  = gammado133*uppermet_uxz+
     &               gammado233*uppermet_uyz+
     &               gammado333*uppermet_uzz 

cf      fpma       Fgamma111=Fgammado111,Fuppermet_uxx,f0
cf      fpma       Fgamma112=Fgammado112,Fuppermet_uxx,f0
cf      fpma       Fgamma113=Fgammado113,Fuppermet_uxx,f0
cf      fpma       Fgamma122=Fgammado122,Fuppermet_uxx,f0
cf      fpma       Fgamma123=Fgammado123,Fuppermet_uxx,f0
cf      fpma       Fgamma133=Fgammado133,Fuppermet_uxx,f0
cf      fpma       Fgamma211=Fgammado111,Fuppermet_uxy,f0
cf      fpma       Fgamma212=Fgammado112,Fuppermet_uxy,f0
cf      fpma       Fgamma213=Fgammado113,Fuppermet_uxy,f0
cf      fpma       Fgamma222=Fgammado122,Fuppermet_uxy,f0
cf      fpma       Fgamma223=Fgammado123,Fuppermet_uxy,f0
cf      fpma       Fgamma233=Fgammado133,Fuppermet_uxy,f0
cf      fpma       Fgamma311=Fgammado111,Fuppermet_uxz,f0
cf      fpma       Fgamma312=Fgammado112,Fuppermet_uxz,f0
cf      fpma       Fgamma313=Fgammado113,Fuppermet_uxz,f0
cf      fpma       Fgamma322=Fgammado122,Fuppermet_uxz,f0
cf      fpma       Fgamma323=Fgammado123,Fuppermet_uxz,f0
cf      fpma       Fgamma333=Fgammado133,Fuppermet_uxz,f0
cf      fpma       Fgamma111=Fgammado211,Fuppermet_uxy,Fgamma111
cf      fpma       Fgamma112=Fgammado212,Fuppermet_uxy,Fgamma112
cf      fpma       Fgamma113=Fgammado213,Fuppermet_uxy,Fgamma113
cf      fpma       Fgamma122=Fgammado222,Fuppermet_uxy,Fgamma122
cf      fpma       Fgamma123=Fgammado223,Fuppermet_uxy,Fgamma123
cf      fpma       Fgamma133=Fgammado233,Fuppermet_uxy,Fgamma133
cf      fpma       Fgamma211=Fgammado211,Fuppermet_uyy,Fgamma211
cf      fpma       Fgamma212=Fgammado212,Fuppermet_uyy,Fgamma212
cf      fpma       Fgamma213=Fgammado213,Fuppermet_uyy,Fgamma213
cf      fpma       Fgamma222=Fgammado222,Fuppermet_uyy,Fgamma222
cf      fpma       Fgamma223=Fgammado223,Fuppermet_uyy,Fgamma223
cf      fpma       Fgamma233=Fgammado233,Fuppermet_uyy,Fgamma233
cf      fpma       Fgamma311=Fgammado211,Fuppermet_uyz,Fgamma311
cf      fpma       Fgamma312=Fgammado212,Fuppermet_uyz,Fgamma312
cf      fpma       Fgamma313=Fgammado213,Fuppermet_uyz,Fgamma313
cf      fpma       Fgamma322=Fgammado222,Fuppermet_uyz,Fgamma322
cf      fpma       Fgamma323=Fgammado223,Fuppermet_uyz,Fgamma323
cf      fpma       Fgamma333=Fgammado233,Fuppermet_uyz,Fgamma333
cf      fpma       Fgamma111=Fgammado311,Fuppermet_uxz,Fgamma111
cf      fpma       Fgamma112=Fgammado312,Fuppermet_uxz,Fgamma112
cf      fpma       Fgamma113=Fgammado313,Fuppermet_uxz,Fgamma113
cf      fpma       Fgamma122=Fgammado322,Fuppermet_uxz,Fgamma122
cf      fpma       Fgamma123=Fgammado323,Fuppermet_uxz,Fgamma123
cf      fpma       Fgamma133=Fgammado333,Fuppermet_uxz,Fgamma133
cf      fpma       Fgamma211=Fgammado311,Fuppermet_uyz,Fgamma211
cf      fpma       Fgamma212=Fgammado312,Fuppermet_uyz,Fgamma212
cf      fpma       Fgamma213=Fgammado313,Fuppermet_uyz,Fgamma213
cf      fpma       Fgamma222=Fgammado322,Fuppermet_uyz,Fgamma222
cf      fpma       Fgamma223=Fgammado323,Fuppermet_uyz,Fgamma223
cf      fpma       Fgamma233=Fgammado333,Fuppermet_uyz,Fgamma233
cf      fpma       Fgamma311=Fgammado311,Fuppermet_uzz,Fgamma311
cf      fpma       Fgamma312=Fgammado312,Fuppermet_uzz,Fgamma312
cf      fpma       Fgamma313=Fgammado313,Fuppermet_uzz,Fgamma313
cf      fpma       Fgamma322=Fgammado322,Fuppermet_uzz,Fgamma322
cf      fpma       Fgamma323=Fgammado323,Fuppermet_uzz,Fgamma323
cf      fpma       Fgamma333=Fgammado333,Fuppermet_uzz,Fgamma333

c        output( 57)=gamma111
c        output( 58)=gamma112
c        output( 59)=gamma113
c        output( 60)=gamma122
c        output( 61)=gamma123
c        output( 62)=gamma133
c        output( 63)=gamma211
c        output( 64)=gamma212
c        output( 65)=gamma213
c        output( 66)=gamma222
c        output( 67)=gamma223
c        output( 68)=gamma233
c        output( 69)=gamma311
c        output( 70)=gamma312
c        output( 71)=gamma313
c        output( 72)=gamma322
c        output( 73)=gamma323
c        output( 74)=gamma333
cd      getf.sig  Rdmptmp=Fgamma111
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma112
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma113
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma122
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma123
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma133
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma211
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma212
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma213
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma222
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma223
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma233
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma311
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma312
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma313
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma322
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma323
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fgamma333
cd      st4       [Rpoutput]=Rdmptmp,4


         tmpgam123111= gammado313*gamma312
     &                -gammado323*gamma311
     &                +gammado213*gamma212
     &                -gammado223*gamma211
     &                +gammado113*gamma112
     &                -gammado123*gamma111

         tmpgam123112= gammado322*gamma313
     &                -gammado323*gamma312
     &                +gammado222*gamma213
     &                -gammado223*gamma212
     &                +gammado122*gamma113
     &                -gammado123*gamma112

         tmpgam123113= gammado123*gamma113
     &                -gammado133*gamma112
     &                +gammado223*gamma213
     &                -gammado233*gamma212
     &                +gammado323*gamma313
     &                -gammado333*gamma312

cf      fpma       Ftmpgam123111=Fgammado313,Fgamma312,f0
cf      fpma       Ftmpgam123112=Fgammado322,Fgamma313,f0
cf      fpma       Ftmpgam123113=Fgammado123,Fgamma113,f0
cf      fpnma      Ftmpgam123111=Fgammado323,Fgamma311,Ftmpgam123111
cf      fpnma      Ftmpgam123112=Fgammado323,Fgamma312,Ftmpgam123112
cf      fpnma      Ftmpgam123113=Fgammado133,Fgamma112,Ftmpgam123113
cf      fpma       Ftmpgam123111=Fgammado213,Fgamma212,Ftmpgam123111
cf      fpma       Ftmpgam123112=Fgammado222,Fgamma213,Ftmpgam123112
cf      fpma       Ftmpgam123113=Fgammado223,Fgamma213,Ftmpgam123113
cf      fpnma      Ftmpgam123111=Fgammado223,Fgamma211,Ftmpgam123111
cf      fpnma      Ftmpgam123112=Fgammado223,Fgamma212,Ftmpgam123112
cf      fpnma      Ftmpgam123113=Fgammado233,Fgamma212,Ftmpgam123113
cf      fpma       Ftmpgam123111=Fgammado113,Fgamma112,Ftmpgam123111
cf      fpma       Ftmpgam123112=Fgammado122,Fgamma113,Ftmpgam123112
cf      fpma       Ftmpgam123113=Fgammado323,Fgamma313,Ftmpgam123113
cf      fpnma      Ftmpgam123111=Fgammado123,Fgamma111,Ftmpgam123111
cf      fpnma      Ftmpgam123112=Fgammado123,Fgamma112,Ftmpgam123112
cf      fpnma      Ftmpgam123113=Fgammado333,Fgamma312,Ftmpgam123113

c        output( 75)=tmpgam123111
c        output( 76)=tmpgam123112
c        output( 77)=tmpgam123113
cd      getf.sig  Rdmptmp=Ftmpgam123111
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpgam123112
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpgam123113
cd      st4       [Rpoutput]=Rdmptmp,4


         deldelg1212 = i4dxdy*(l(lgxy,ip,jp,k0)-l(lgxy,ip,jm,k0)-
     &                         l(lgxy,im,jp,k0)+l(lgxy,im,jm,k0))
         tmp1212 = deldelg1212-(dxxdgyy+dyydgxx)*half-
     &             gammado122*gamma111+gammado112*gamma112-
     &             gammado222*gamma211+gammado212*gamma212-
     &             gammado322*gamma311+gammado312*gamma312

cm      ld4       Rlgxy_ip_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgxy_ip_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgxy_ip_jp_k0_hi=[Rpl_jp_k0],-96
cm      ld4       Rlgxy_ip_jm_k0_hi=[Rpl_jm_k0],-96
cm      ld4       Rlgxy_im_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgxy_im_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgxy_im_jp_k0_hi=[Rpl_jp_k0],36
cm      ld4       Rlgxy_im_jm_k0_hi=[Rpl_jm_k0],36
ci      unpack4.l Rlgxy_ip_jp_k0=Rlgxy_ip_jp_k0_hi,Rlgxy_ip_jp_k0_lo
ci      unpack4.l Rlgxy_ip_jm_k0=Rlgxy_ip_jm_k0_hi,Rlgxy_ip_jm_k0_lo
ci      unpack4.l Rlgxy_im_jp_k0=Rlgxy_im_jp_k0_hi,Rlgxy_im_jp_k0_lo
ci      unpack4.l Rlgxy_im_jm_k0=Rlgxy_im_jm_k0_hi,Rlgxy_im_jm_k0_lo
cm      setf.sig  Flgxy_ip_jp_k0=Rlgxy_ip_jp_k0
cm      setf.sig  Flgxy_ip_jm_k0=Rlgxy_ip_jm_k0
cm      setf.sig  Flgxy_im_jp_k0=Rlgxy_im_jp_k0
cm      setf.sig  Flgxy_im_jm_k0=Rlgxy_im_jm_k0
cf      fpms      Fdeldelg1212=Flgxy_ip_jp_k0,f1,Flgxy_ip_jm_k0
cf      fpms      Fdeldelg1212=Fdeldelg1212,f1,Flgxy_im_jp_k0
cf      fpma      Fdeldelg1212=Fdeldelg1212,f1,Flgxy_im_jm_k0
cf      fpma      Fdeldelg1212=Fdeldelg1212,Fi4dxdy,f0
cf      fpma      Ftmp1212=Fdxxdgyy,f1,Fdyydgxx
cf      fpnma     Ftmp1212=Ftmp1212,Fhalf,Fdeldelg1212
cf      fpnma     Ftmp1212=Fgammado122,Fgamma111,Ftmp1212
cf      fpma      Ftmp1212=Fgammado112,Fgamma112,Ftmp1212
cf      fpnma     Ftmp1212=Fgammado222,Fgamma211,Ftmp1212
cf      fpma      Ftmp1212=Fgammado212,Fgamma212,Ftmp1212
cf      fpnma     Ftmp1212=Fgammado322,Fgamma311,Ftmp1212
cf      fpma      Ftmp1212=Fgammado312,Fgamma312,Ftmp1212

c        output( 78)=deldelg1212
c        output( 79)=tmp1212
cd      getf.sig  Rdmptmp=Fdeldelg1212
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmp1212
cd      st4       [Rpoutput]=Rdmptmp,4


         deldelg1313 = i4dxdz*(l(lgxz,ip,j0,kp)-l(lgxz,ip,j0,km)-
     &                         l(lgxz,im,j0,kp)+l(lgxz,im,j0,km))
         tmp1313 = deldelg1313-(dxxdgzz+dzzdgxx)*half-
     &             gammado133*gamma111+gammado113*gamma113-
     &             gammado233*gamma211+gammado213*gamma213-
     &             gammado333*gamma311+gammado313*gamma313

cm      ld4       Rlgxz_ip_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgxz_ip_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgxz_ip_j0_kp_hi=[Rpl_j0_kp],-96
cm      ld4       Rlgxz_ip_j0_km_hi=[Rpl_j0_km],-96
cm      ld4       Rlgxz_im_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgxz_im_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgxz_im_j0_kp_hi=[Rpl_j0_kp],28
cm      ld4       Rlgxz_im_j0_km_hi=[Rpl_j0_km],28
ci      unpack4.l Rlgxz_ip_j0_kp=Rlgxz_ip_j0_kp_hi,Rlgxz_ip_j0_kp_lo
ci      unpack4.l Rlgxz_ip_j0_km=Rlgxz_ip_j0_km_hi,Rlgxz_ip_j0_km_lo
ci      unpack4.l Rlgxz_im_j0_kp=Rlgxz_im_j0_kp_hi,Rlgxz_im_j0_kp_lo
ci      unpack4.l Rlgxz_im_j0_km=Rlgxz_im_j0_km_hi,Rlgxz_im_j0_km_lo
cm      setf.sig  Flgxz_ip_j0_kp=Rlgxz_ip_j0_kp
cm      setf.sig  Flgxz_ip_j0_km=Rlgxz_ip_j0_km
cm      setf.sig  Flgxz_im_j0_kp=Rlgxz_im_j0_kp
cm      setf.sig  Flgxz_im_j0_km=Rlgxz_im_j0_km
cf      fpms      Fdeldelg1313=Flgxy_ip_j0_kp,f1,Flgxy_ip_j0_km
cf      fpms      Fdeldelg1313=Fdeldelg1313,f1,Flgxy_im_j0_kp
cf      fpma      Fdeldelg1313=Fdeldelg1313,f1,Flgxy_im_j0_km
cf      fpma      Fdeldelg1313=Fdeldelg1313,Fi4dxdz,f0
cf      fpma      Ftmp1313=Fdxxdgzz,f1,Fdzzdgxx
cf      fpnma     Ftmp1313=Ftmp1313,Fhalf,Fdeldelg1313
cf      fpnma     Ftmp1313=Fgammado133,Fgamma111,Ftmp1313
cf      fpma      Ftmp1313=Fgammado113,Fgamma113,Ftmp1313
cf      fpnma     Ftmp1313=Fgammado233,Fgamma211,Ftmp1313
cf      fpma      Ftmp1313=Fgammado213,Fgamma213,Ftmp1313
cf      fpnma     Ftmp1313=Fgammado333,Fgamma311,Ftmp1313
cf      fpma      Ftmp1313=Fgammado313,Fgamma313,Ftmp1313

c        output( 80)=deldelg1313
c        output( 81)=tmp1313
cd      getf.sig  Rdmptmp=Fdeldelg1313
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmp1313
cd      st4       [Rpoutput]=Rdmptmp,4


         deldelg2323 = i4dydz*(l(lgyz,i0,jp,kp)-l(lgyz,i0,jp,km)-
     &                         l(lgyz,i0,jm,kp)+l(lgyz,i0,jm,km))
         tmp2323 = deldelg2323-(dyydgzz+dzzdgyy)*half-
     &             gammado133*gamma122+gammado123*gamma123-
     &             gammado233*gamma222+gammado223*gamma223-
     &             gammado333*gamma322+gammado323*gamma323

cm      ld4       Rlgyz_i0_jp_kp_lo=[Rpl_jp_kp],32
cm      ld4       Rlgyz_i0_jp_kp_hi=[Rpl_jp_kp],-48
cm      ld4       Rlgyz_i0_jp_km_lo=[Rpl_jp_km],32
cm      ld4       Rlgyz_i0_jp_km_hi=[Rpl_jp_km],-48
cm      ld4       Rlgyz_i0_jm_kp_lo=[Rpl_jm_kp],32
cm      ld4       Rlgyz_i0_jm_kp_hi=[Rpl_jm_kp],-48
cm      ld4       Rlgyz_i0_jm_km_lo=[Rpl_jm_km],32
cm      ld4       Rlgyz_i0_jm_km_hi=[Rpl_jm_km],-48
ci      unpack4.l Rlgyz_i0_jp_kp=Rlgyz_i0_jp_kp_hi,Rlgyz_i0_jp_kp_lo
ci      unpack4.l Rlgyz_i0_jp_km=Rlgyz_i0_jp_km_hi,Rlgyz_i0_jp_km_lo
ci      unpack4.l Rlgyz_i0_jm_kp=Rlgyz_i0_jm_kp_hi,Rlgyz_i0_jm_kp_lo
ci      unpack4.l Rlgyz_i0_jm_km=Rlgyz_i0_jm_km_hi,Rlgyz_i0_jm_km_lo
cm      setf.sig  Flgyz_i0_jp_km=Rlgyz_i0_jp_km
cm      setf.sig  Flgyz_i0_jp_kp=Rlgyz_i0_jp_kp
cm      setf.sig  Flgyz_i0_jm_km=Rlgyz_i0_jm_km
cm      setf.sig  Flgyz_i0_jm_kp=Rlgyz_i0_jm_kp
cf      fpms      Fdeldelg2323=Flgyz_i0_jp_kp,f1,Flgyz_i0_jp_km
cf      fpms      Fdeldelg2323=Fdeldelg2323,f1,Flgyz_i0_jm_kp
cf      fpma      Fdeldelg2323=Fdeldelg2323,f1,Flgyz_i0_jm_km
cf      fpma      Fdeldelg2323=Fdeldelg2323,Fi4dydz,f0
cf      fpma      Ftmp2323=Fdyydgzz,f1,Fdzzdgyy
cf      fpnma     Ftmp2323=Ftmp2323,Fhalf,Fdeldelg2323
cf      fpnma     Ftmp2323=Fgammado133,Fgamma122,Ftmp2323
cf      fpma      Ftmp2323=Fgammado123,Fgamma123,Ftmp2323
cf      fpnma     Ftmp2323=Fgammado233,Fgamma222,Ftmp2323
cf      fpma      Ftmp2323=Fgammado223,Fgamma223,Ftmp2323
cf      fpnma     Ftmp2323=Fgammado333,Fgamma322,Ftmp2323
cf      fpma      Ftmp2323=Fgammado323,Fgamma323,Ftmp2323

c        output( 82)=deldelg2323
c        output( 83)=tmp2323
cd      getf.sig  Rdmptmp=Fdeldelg2323
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmp2323
cd      st4       [Rpoutput]=Rdmptmp,4


         deldelg1213 = i4dxdy*(l(lgxz,ip,jp,k0)-l(lgxz,ip,jm,k0)-
     &                         l(lgxz,im,jp,k0)+l(lgxz,im,jm,k0))
         deldelg1312 = i4dxdz*(l(lgxy,ip,j0,kp)-l(lgxy,ip,j0,km)-
     &                         l(lgxy,im,j0,kp)+l(lgxy,im,j0,km))
         deldelg2311 = i4dydz*(l(lgxx,i0,jp,kp)-l(lgxx,i0,jp,km)-
     &                         l(lgxx,i0,jm,kp)+l(lgxx,i0,jm,km))
         tmpdxxdgyz=dxxdgyz-deldelg1213-deldelg1312+deldelg2311

cm      ld4       Rlgxz_ip_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgxz_ip_jp_k0_hi=[Rpl_jp_k0],-96
cm      ld4       Rlgxz_im_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgxz_im_jp_k0_hi=[Rpl_jp_k0],40
cm      ld4       Rlgxz_ip_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgxz_ip_jm_k0_hi=[Rpl_jm_k0],-96
cm      ld4       Rlgxz_im_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgxz_im_jm_k0_hi=[Rpl_jm_k0],40
ci      unpack4.l Rlgxz_ip_jp_k0=Rlgxz_ip_jp_k0_hi,Rlgxz_ip_jp_k0_lo
ci      unpack4.l Rlgxz_im_jp_k0=Rlgxz_im_jp_k0_hi,Rlgxz_im_jp_k0_lo
ci      unpack4.l Rlgxz_ip_jm_k0=Rlgxz_ip_jm_k0_hi,Rlgxz_ip_jm_k0_lo
ci      unpack4.l Rlgxz_im_jm_k0=Rlgxz_im_jm_k0_hi,Rlgxz_im_jm_k0_lo
cm      setf.sig  Flgxz_ip_jp_k0=Rlgxz_ip_jp_k0
cm      setf.sig  Flgxz_ip_jm_k0=Rlgxz_ip_jm_k0
cm      setf.sig  Flgxz_im_jp_k0=Rlgxz_im_jp_k0
cm      setf.sig  Flgxz_im_jm_k0=Rlgxz_im_jm_k0
cf      fpms      Fdeldelg1213=Flgxz_ip_jp_k0,f1,Flgxz_ip_jm_k0
cf      fpms      Fdeldelg1213=Fdeldelg1213,f1,Flgxz_im_jp_k0
cf      fpma      Fdeldelg1213=Fdeldelg1213,f1,Flgxz_im_jm_k0
cf      fpnma     Ftmpdxxdgyz=Fdeldelg1213,Fi4dxdy,Fdxxdgyz

cm      ld4       Rlgxy_ip_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgxy_ip_j0_kp_hi=[Rpl_j0_kp],-96
cm      ld4       Rlgxy_im_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgxy_im_j0_kp_hi=[Rpl_j0_kp],40
cm      ld4       Rlgxy_ip_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgxy_ip_j0_km_hi=[Rpl_j0_km],-96
cm      ld4       Rlgxy_im_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgxy_im_j0_km_hi=[Rpl_j0_km],40
ci      unpack4.l Rlgxy_ip_j0_kp=Rlgxy_ip_j0_kp_hi,Rlgxy_ip_j0_kp_lo
ci      unpack4.l Rlgxy_ip_j0_km=Rlgxy_ip_j0_km_hi,Rlgxy_ip_j0_km_lo
ci      unpack4.l Rlgxy_im_j0_kp=Rlgxy_im_j0_kp_hi,Rlgxy_im_j0_kp_lo
ci      unpack4.l Rlgxy_im_j0_km=Rlgxy_im_j0_km_hi,Rlgxy_im_j0_km_lo
cm      setf.sig  Flgxy_ip_j0_kp=Rlgxy_ip_j0_kp
cm      setf.sig  Flgxy_ip_j0_km=Rlgxy_ip_j0_km
cm      setf.sig  Flgxy_im_j0_km=Rlgxy_im_j0_km
cm      setf.sig  Flgxy_im_j0_kp=Rlgxy_im_j0_kp
cf      fpms      Fdeldelg1312=Flgxy_ip_j0_kp,f1,Flgxy_ip_j0_km
cf      fpms      Fdeldelg1312=Fdeldelg1312,f1,Flgxy_im_j0_kp
cf      fpma      Fdeldelg1312=Fdeldelg1312,f1,Flgxy_im_j0_km
cf      fpnma     Ftmpdxxdgyz=Fdeldelg1312,Fi4dxdz,Ftmpdxxdgyz

cm      ld4       Rlgxx_i0_jp_kp_lo=[Rpl_jp_kp],32
cm      ld4       Rlgxx_i0_jp_kp_hi=[Rpl_jp_kp],-28
cm      ld4       Rlgxx_i0_jm_kp_lo=[Rpl_jm_kp],32
cm      ld4       Rlgxx_i0_jm_kp_hi=[Rpl_jm_kp],-28
cm      ld4       Rlgxx_i0_jp_km_lo=[Rpl_jp_km],32
cm      ld4       Rlgxx_i0_jp_km_hi=[Rpl_jp_km],-28
cm      ld4       Rlgxx_i0_jm_km_lo=[Rpl_jm_km],32
cm      ld4       Rlgxx_i0_jm_km_hi=[Rpl_jm_km],-28
ci      unpack4.l Rlgxx_i0_jp_kp=Rlgxx_i0_jp_kp_hi,Rlgxx_i0_jp_kp_lo
ci      unpack4.l Rlgxx_i0_jp_km=Rlgxx_i0_jp_km_hi,Rlgxx_i0_jp_km_lo
ci      unpack4.l Rlgxx_i0_jm_kp=Rlgxx_i0_jm_kp_hi,Rlgxx_i0_jm_kp_lo
ci      unpack4.l Rlgxx_i0_jm_km=Rlgxx_i0_jm_km_hi,Rlgxx_i0_jm_km_lo
cm      setf.sig  Flgxx_i0_jp_km=Rlgxx_i0_jp_km
cm      setf.sig  Flgxx_i0_jp_kp=Rlgxx_i0_jp_kp
cm      setf.sig  Flgxx_i0_jm_km=Rlgxx_i0_jm_km
cm      setf.sig  Flgxx_i0_jm_kp=Rlgxx_i0_jm_kp
cf      fpms      Fdeldelg2311=Flgxx_i0_jp_kp,f1,Flgxx_i0_jp_km
cf      fpms      Fdeldelg2311=Fdeldelg2311,f1,Flgxx_i0_jm_kp
cf      fpma      Fdeldelg2311=Fdeldelg2311,f1,Flgxx_i0_jm_km
cf      fpma      Ftmpdxxdgyz=Fdeldelg2311,Fi4dydz,Ftmpdxxdgyz

c        output( 84)=tmpdxxdgyz
cd      getf.sig  Rdmptmp=Ftmpdxxdgyz
cd      st4       [Rpoutput]=Rdmptmp,4


         deldelg1223 = i4dxdy*(l(lgyz,ip,jp,k0)-l(lgyz,ip,jm,k0)-
     &                         l(lgyz,im,jp,k0)+l(lgyz,im,jm,k0))
         deldelg1322 = i4dxdz*(l(lgyy,ip,j0,kp)-l(lgyy,ip,j0,km)-
     &                         l(lgyy,im,j0,kp)+l(lgyy,im,j0,km))
         deldelg2312 = i4dydz*(l(lgxy,i0,jp,kp)-l(lgxy,i0,jp,km)-
     &                         l(lgxy,i0,jm,kp)+l(lgxy,i0,jm,km))
         tmpdyydgxz=dyydgxz-deldelg1223+deldelg1322-deldelg2312

cm      ld4       Rlgyz_ip_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgyz_ip_jp_k0_hi=[Rpl_jp_k0],-96
cm      ld4       Rlgyz_im_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgyz_im_jp_k0_hi=[Rpl_jp_k0],36
cm      ld4       Rlgyz_ip_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgyz_ip_jm_k0_hi=[Rpl_jm_k0],-96
cm      ld4       Rlgyz_im_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgyz_im_jm_k0_hi=[Rpl_jm_k0],36
ci      unpack4.l Rlgyz_ip_jp_k0=Rlgyz_ip_jp_k0_hi,Rlgyz_ip_jp_k0_lo
ci      unpack4.l Rlgyz_im_jp_k0=Rlgyz_im_jp_k0_hi,Rlgyz_im_jp_k0_lo
ci      unpack4.l Rlgyz_ip_jm_k0=Rlgyz_ip_jm_k0_hi,Rlgyz_ip_jm_k0_lo
ci      unpack4.l Rlgyz_im_jm_k0=Rlgyz_im_jm_k0_hi,Rlgyz_im_jm_k0_lo
cm      setf.sig  Flgyz_ip_jp_k0=Rlgyz_ip_jp_k0
cm      setf.sig  Flgyz_ip_jm_k0=Rlgyz_ip_jm_k0
cm      setf.sig  Flgyz_im_jp_k0=Rlgyz_im_jp_k0
cm      setf.sig  Flgyz_im_jm_k0=Rlgyz_im_jm_k0
cf      fpms      Fdeldelg1223=Flgyz_ip_jp_k0,f1,Flgyz_ip_jm_k0
cf      fpms      Fdeldelg1223=Fdeldelg1223,f1,Flgyz_im_jp_k0
cf      fpma      Fdeldelg1223=Fdeldelg1223,f1,Flgyz_im_jm_k0
cf      fpnma     Ftmpdyydgxz=Fdeldelg1223,Fi4dxdy,Fdyydgxz

cm      ld4       Rlgyy_ip_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgyy_ip_j0_kp_hi=[Rpl_j0_kp],-96
cm      ld4       Rlgyy_im_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgyy_im_j0_kp_hi=[Rpl_j0_kp],36
cm      ld4       Rlgyy_ip_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgyy_ip_j0_km_hi=[Rpl_j0_km],-96
cm      ld4       Rlgyy_im_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgyy_im_j0_km_hi=[Rpl_j0_km],36
ci      unpack4.l Rlgyy_ip_j0_kp=Rlgyy_ip_j0_kp_hi,Rlgyy_ip_j0_kp_lo
ci      unpack4.l Rlgyy_ip_j0_km=Rlgyy_ip_j0_km_hi,Rlgyy_ip_j0_km_lo
ci      unpack4.l Rlgyy_im_j0_kp=Rlgyy_im_j0_kp_hi,Rlgyy_im_j0_kp_lo
ci      unpack4.l Rlgyy_im_j0_km=Rlgyy_im_j0_km_hi,Rlgyy_im_j0_km_lo
cm      setf.sig  Flgyy_ip_j0_km=Rlgyy_ip_j0_km
cm      setf.sig  Flgyy_ip_j0_kp=Rlgyy_ip_j0_kp
cm      setf.sig  Flgyy_im_j0_km=Rlgyy_im_j0_km
cm      setf.sig  Flgyy_im_j0_kp=Rlgyy_im_j0_kp
cf      fpms      Fdeldelg1322=Flgyy_ip_j0_kp,f1,Flgyy_ip_j0_km
cf      fpms      Fdeldelg1322=Fdeldelg1322,f1,Flgyy_im_j0_kp
cf      fpma      Fdeldelg1322=Fdeldelg1322,f1,Flgyy_im_j0_km
cf      fpma      Ftmpdyydgxz=Fdeldelg1322,Fi4dxdz,Ftmpdyydgxz

cm      ld4       Rlgxy_i0_jp_kp_lo=[Rpl_jp_kp],32
cm      ld4       Rlgxy_i0_jp_kp_hi=[Rpl_jp_kp],-28
cm      ld4       Rlgxy_i0_jm_kp_lo=[Rpl_jm_kp],32
cm      ld4       Rlgxy_i0_jm_kp_hi=[Rpl_jm_kp],-28
cm      ld4       Rlgxy_i0_jp_km_lo=[Rpl_jp_km],32
cm      ld4       Rlgxy_i0_jp_km_hi=[Rpl_jp_km],-28
cm      ld4       Rlgxy_i0_jm_km_lo=[Rpl_jm_km],32
cm      ld4       Rlgxy_i0_jm_km_hi=[Rpl_jm_km],-28
ci      unpack4.l Rlgxy_i0_jp_kp=Rlgxy_i0_jp_kp_hi,Rlgxy_i0_jp_kp_lo
ci      unpack4.l Rlgxy_i0_jp_km=Rlgxy_i0_jp_km_hi,Rlgxy_i0_jp_km_lo
ci      unpack4.l Rlgxy_i0_jm_kp=Rlgxy_i0_jm_kp_hi,Rlgxy_i0_jm_kp_lo
ci      unpack4.l Rlgxy_i0_jm_km=Rlgxy_i0_jm_km_hi,Rlgxy_i0_jm_km_lo
cm      setf.sig  Flgxy_i0_jp_km=Rlgxy_i0_jp_km
cm      setf.sig  Flgxy_i0_jp_kp=Rlgxy_i0_jp_kp
cm      setf.sig  Flgxy_i0_jm_km=Rlgxy_i0_jm_km
cm      setf.sig  Flgxy_i0_jm_kp=Rlgxy_i0_jm_kp
cf      fpms      Fdeldelg2312=Flgxy_i0_jp_kp,f1,Flgxy_i0_jp_km
cf      fpms      Fdeldelg2312=Fdeldelg2312,f1,Flgxy_i0_jm_kp
cf      fpma      Fdeldelg2312=Fdeldelg2312,f1,Flgxy_i0_jm_km
cf      fpnma     Ftmpdyydgxz=Fdeldelg2312,Fi4dydz,Ftmpdyydgxz

c        output( 85)=tmpdyydgxz
cd      getf.sig  Rdmptmp=Ftmpdyydgxz
cd      st4       [Rpoutput]=Rdmptmp,4


         deldelg1233 = i4dxdy*(l(lgzz,ip,jp,k0)-l(lgzz,ip,jm,k0)-
     &                         l(lgzz,im,jp,k0)+l(lgzz,im,jm,k0))
         deldelg1323 = i4dxdz*(l(lgyz,ip,j0,kp)-l(lgyz,ip,j0,km)-
     &                         l(lgyz,im,j0,kp)+l(lgyz,im,j0,km))
         deldelg2313 = i4dydz*(l(lgxz,i0,jp,kp)-l(lgxz,i0,jp,km)-
     &                         l(lgxz,i0,jm,kp)+l(lgxz,i0,jm,km))
         tmpdzzdgxy=dzzdgxy+deldelg1233-deldelg1323-deldelg2313

cm      ld4       Rlgzz_ip_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgzz_ip_jp_k0_hi=[Rpl_jp_k0],-96
cm      ld4       Rlgzz_im_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlgzz_im_jp_k0_hi=[Rpl_jp_k0],4
cm      ld4       Rlgzz_ip_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgzz_ip_jm_k0_hi=[Rpl_jm_k0],-96
cm      ld4       Rlgzz_im_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlgzz_im_jm_k0_hi=[Rpl_jm_k0],4
ci      unpack4.l Rlgzz_ip_jp_k0=Rlgzz_ip_jp_k0_hi,Rlgzz_ip_jp_k0_lo
ci      unpack4.l Rlgzz_im_jp_k0=Rlgzz_im_jp_k0_hi,Rlgzz_im_jp_k0_lo
ci      unpack4.l Rlgzz_ip_jm_k0=Rlgzz_ip_jm_k0_hi,Rlgzz_ip_jm_k0_lo
ci      unpack4.l Rlgzz_im_jm_k0=Rlgzz_im_jm_k0_hi,Rlgzz_im_jm_k0_lo
cm      setf.sig  Flgzz_ip_jp_k0=Rlgzz_ip_jp_k0
cm      setf.sig  Flgzz_ip_jm_k0=Rlgzz_ip_jm_k0
cm      setf.sig  Flgzz_im_jp_k0=Rlgzz_im_jp_k0
cm      setf.sig  Flgzz_im_jm_k0=Rlgzz_im_jm_k0
cf      fpms      Fdeldelg1233=Flgzz_ip_jp_k0,f1,Flgzz_ip_jm_k0
cf      fpms      Fdeldelg1233=Fdeldelg1233,f1,Flgzz_im_jp_k0
cf      fpma      Fdeldelg1233=Fdeldelg1233,f1,Flgzz_im_jm_k0
cf      fpna      Ftmpdzzdgxy=Fdeldelg1233,Fi4dxdy,Fdzzdgxy

cm      ld4       Rlgyz_ip_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgyz_ip_j0_kp_hi=[Rpl_j0_kp],-96
cm      ld4       Rlgyz_im_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlgyz_im_j0_kp_hi=[Rpl_j0_kp],8
cm      ld4       Rlgyz_ip_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgyz_ip_j0_km_hi=[Rpl_j0_km],-96
cm      ld4       Rlgyz_im_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlgyz_im_j0_km_hi=[Rpl_j0_km],8
ci      unpack4.l Rlgyz_ip_j0_kp=Rlgyz_ip_j0_kp_hi,Rlgyz_ip_j0_kp_lo
ci      unpack4.l Rlgyz_ip_j0_km=Rlgyz_ip_j0_km_hi,Rlgyz_ip_j0_km_lo
ci      unpack4.l Rlgyz_im_j0_kp=Rlgyz_im_j0_kp_hi,Rlgyz_im_j0_kp_lo
ci      unpack4.l Rlgyz_im_j0_km=Rlgyz_im_j0_km_hi,Rlgyz_im_j0_km_lo
cm      setf.sig  Flgyz_ip_j0_km=Rlgyz_ip_j0_km
cm      setf.sig  Flgyz_ip_j0_kp=Rlgyz_ip_j0_kp
cm      setf.sig  Flgyz_im_j0_km=Rlgyz_im_j0_km
cm      setf.sig  Flgyz_im_j0_kp=Rlgyz_im_j0_kp
cf      fpms      Fdeldelg1323=Flgyz_ip_j0_kp,f1,Flgyz_ip_j0_km
cf      fpms      Fdeldelg1323=Fdeldelg1323,f1,Flgyz_im_j0_kp
cf      fpma      Fdeldelg1323=Fdeldelg1323,f1,Flgyz_im_j0_km
cf      fpnma     Ftmpdzzdgxy=Fdeldelg1323,Fi4dxdz,Ftmpdzzdgxy

cm      ld4       Rlgxz_i0_jp_kp_lo=[Rpl_jp_kp],32
cm      ld4       Rlgxz_i0_jp_kp_hi=[Rpl_jp_kp],-16
cm      ld4       Rlgxz_i0_jm_kp_lo=[Rpl_jm_kp],32
cm      ld4       Rlgxz_i0_jm_kp_hi=[Rpl_jm_kp],-16
cm      ld4       Rlgxz_i0_jp_km_lo=[Rpl_jp_km],32
cm      ld4       Rlgxz_i0_jp_km_hi=[Rpl_jp_km],-16
cm      ld4       Rlgxz_i0_jm_km_lo=[Rpl_jm_km],32
cm      ld4       Rlgxz_i0_jm_km_hi=[Rpl_jm_km],-16
ci      unpack4.l Rlgxz_i0_jp_kp=Rlgxz_i0_jp_kp_hi,Rlgxz_i0_jp_kp_lo
ci      unpack4.l Rlgxz_i0_jp_km=Rlgxz_i0_jp_km_hi,Rlgxz_i0_jp_km_lo
ci      unpack4.l Rlgxz_i0_jm_kp=Rlgxz_i0_jm_kp_hi,Rlgxz_i0_jm_kp_lo
ci      unpack4.l Rlgxz_i0_jm_km=Rlgxz_i0_jm_km_hi,Rlgxz_i0_jm_km_lo
cm      setf.sig  Flgxz_i0_jp_km=Rlgxz_i0_jp_km
cm      setf.sig  Flgxz_i0_jp_kp=Rlgxz_i0_jp_kp
cm      setf.sig  Flgxz_i0_jm_km=Rlgxz_i0_jm_km
cm      setf.sig  Flgxz_i0_jm_kp=Rlgxz_i0_jm_kp
cf      fpms      Fdeldelg2313=Flgxz_i0_jp_kp,f1,Flgxz_i0_jp_km
cf      fpms      Fdeldelg2313=Fdeldelg2313,f1,Flgxz_i0_jm_kp
cf      fpma      Fdeldelg2313=Fdeldelg2313,f1,Flgxz_i0_jm_km
cf      fpnma     Ftmpdzzdgxy=Fdeldelg2313,Fi4dydz,Ftmpdzzdgxy

c        output( 86)=tmpdzzdgxy
cd      getf.sig  Rdmptmp=Ftmpdzzdgyz
cd      st4       [Rpoutput]=Rdmptmp,4


         tmpricci1 = tmpdxxdgyz*half-tmpgam123111
         tmpricci2 = tmpdyydgxz*half+tmpgam123112
         tmpricci3 = tmpdzzdgxy*half-tmpgam123113

cf      fpnma     Ftmpricci1=Ftmpdxxdgyz,Fhalf,Ftmpgam123111
cf      fpma      Ftmpricci2=Ftmpdyydgxz,Fhalf,Ftmpgam123112
cf      fpnma     Ftmpricci3=Ftmpdzzdgxy,Fhalf,Ftmpgam123113

c        output( 87)=tmpricci1
c        output( 88)=tmpricci2
c        output( 89)=tmpricci3
cd      getf.sig  Rdmptmp=Ftmpricci1
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpricci2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpricci3
cd      st4       [Rpoutput]=Rdmptmp,4


         ricci_R11 = tmp1212*uppermet_uyy-
     &               (tmpdxxdgyz+2*tmpgam123111)*uppermet_uyz+
     &               tmp1313*uppermet_uzz 

cf      fpma      Fricci_R11=Ftmpgam123111,Ftwo,Ftmpdxxdgyz
cf      fpma      Fricci_R11=Fricci_R11,Fuppermet_uyz,f0
cf      fpms      Fricci_R11=Ftmp1212,Fuppermet_uyy,Fricci_R11
cf      fpma      Fricci_R11=Ftmp1313,Fuppermet_uzz,Fricci_R11

c        output( 90)=ricci_R11
cd      getf.sig  Rdmptmp=Fricci_R11
cd      st4       [Rpoutput]=Rdmptmp,4


         ricci_R22 = tmp1212*uppermet_uxx-
     &               (tmpdyydgxz+2*tmpgam123112)*uppermet_uxz+
     &               tmp2323*uppermet_uzz

cf      fpma      Fricci_R22=Ftmpgam123112,Ftwo,Ftmpdyydgxz
cf      fpma      Fricci_R22=Fricci_R22,Fuppermet_uxz,f0
cf      fpms      Fricci_R22=Ftmp1212,Fuppermet_uxx,Fricci_R22
cf      fpma      Fricci_R22=Ftmp2323,Fuppermet_uzz,Fricci_R22

c        output( 91)=ricci_R22
cd      getf.sig  Rdmptmp=Fricci_R22
cd      st4       [Rpoutput]=Rdmptmp,4


         ricci_R33 = tmp1313*uppermet_uxx-
     &               (tmpdzzdgxy-2*tmpgam123113)*uppermet_uxy+
     &               tmp2323*uppermet_uyy

cf      fpma      Fricci_R33=Ftmpgam123113,Ftwo,Ftmpdzzdgxy
cf      fpma      Fricci_R33=Fricci_R33,Fuppermet_uxy,f0
cf      fpms      Fricci_R33=Ftmp1313,Fuppermet_uxx,Fricci_R33
cf      fpma      Fricci_R33=Ftmp2323,Fuppermet_uyy,Fricci_R33

c        output( 92)=ricci_R33
cd      getf.sig  Rdmptmp=Fricci_R33
cd      st4       [Rpoutput]=Rdmptmp,4


         ricci_R12 = tmpricci1*uppermet_uxz-
     &               tmp1212*uppermet_uxy+
     &               tmpricci2*uppermet_uyz-
     &               tmpricci3*uppermet_uzz

cf      fpma      Fricci_R12=Ftmpricci1,Fuppermet_uxz,f0
cf      fpnma     Fricci_R12=Ftmp1212,Fuppermet_uxy,Fricci_R12
cf      fpma      Fricci_R12=Ftmpricci2,Fuppermet_uyz,Fricci_R12
cf      fpnma     Fricci_R12=Ftmpricci3,Fuppermet_uzz,Fricci_R12

c        output( 93)=ricci_R12
cd      getf.sig  Rdmptmp=Fricci_R12
cd      st4       [Rpoutput]=Rdmptmp,4


         ricci_R13 = tmpricci1*uppermet_uxy-
     &               tmp1313*uppermet_uxz-
     &               tmpricci2*uppermet_uyy+
     &               tmpricci3*uppermet_uyz

cf      fpma      Fricci_R13=Ftmpricci1,Fuppermet_uxy,f0
cf      fpnma     Fricci_R13=Ftmp1313,Fuppermet_uxz,Fricci_R13
cf      fpnma     Fricci_R13=Ftmpricci2,Fuppermet_uyy,Fricci_R13
cf      fpma      Fricci_R13=Ftmpricci3,Fuppermet_uyz,Fricci_R13

c        output( 94)=ricci_R13
cd      getf.sig  Rdmptmp=Fricci_R13
cd      st4       [Rpoutput]=Rdmptmp,4

         ricci_R23 = tmpricci2*uppermet_uxy-
     &               tmpricci1*uppermet_uxx+
     &               tmpricci3*uppermet_uxz-
     &               tmp2323*uppermet_uyz

cf      fpma      Fricci_R23=Ftmpricci2,Fuppermet_uxy,f0
cf      fpnma     Fricci_R23=Ftmpricci1,Fuppermet_uxx,Fricci_R23
cf      fpma      Fricci_R23=Ftmpricci3,Fuppermet_uxz,Fricci_R23
cf      fpnma     Fricci_R23=Ftmp1313,Fuppermet_uyz,Fricci_R23

c        output( 95)=ricci_R23
cd      getf.sig  Rdmptmp=Fricci_R23
cd      st4       [Rpoutput]=Rdmptmp,4


         kxxc = 1.5D0*ADM_k_stag(xx,i,j,k)-0.5D0*ADM_k_stag_p(xx,i,j,k)
         kxyc = 1.5D0*ADM_k_stag(xy,i,j,k)-0.5D0*ADM_k_stag_p(xy,i,j,k)
         kxzc = 1.5D0*ADM_k_stag(xz,i,j,k)-0.5D0*ADM_k_stag_p(xz,i,j,k)
         kyyc = 1.5D0*ADM_k_stag(yy,i,j,k)-0.5D0*ADM_k_stag_p(yy,i,j,k)
         kyzc = 1.5D0*ADM_k_stag(yz,i,j,k)-0.5D0*ADM_k_stag_p(yz,i,j,k)
         kzzc = 1.5D0*ADM_k_stag(zz,i,j,k)-0.5D0*ADM_k_stag_p(zz,i,j,k)

cm      ld4       R0kxx_stag=[Rpk_stag],4
cm      ld4       R0kxy_stag=[Rpk_stag],4
cm      ld4       R0kxz_stag=[Rpk_stag],4
cm      ld4       R0kyy_stag=[Rpk_stag],4
cm      ld4       R0kyz_stag=[Rpk_stag],4
cm      ld4       R0kzz_stag=[Rpk_stag],4
cm      ld4       R1kxx_stag=[Rpk_stag],4
cm      ld4       R1kxy_stag=[Rpk_stag],4
cm      ld4       R1kxz_stag=[Rpk_stag],4
cm      ld4       R1kyy_stag=[Rpk_stag],4
cm      ld4       R1kyz_stag=[Rpk_stag],4
cm      ld4       R1kzz_stag=[Rpk_stag],4
ci      unpack4.l Rkxx_stag=R1kxx_stag,R0kxx_stag
ci      unpack4.l Rkxy_stag=R1kxy_stag,R0kxy_stag
ci      unpack4.l Rkxz_stag=R1kxz_stag,R0kxz_stag
ci      unpack4.l Rkyy_stag=R1kyy_stag,R0kyy_stag
ci      unpack4.l Rkyz_stag=R1kyz_stag,R0kyz_stag
ci      unpack4.l Rkzz_stag=R1kzz_stag,R0kzz_stag
cm      setf.sig  Fkxx_stag=Rkxx_stag
cm      setf.sig  Fkxy_stag=Rkxy_stag
cm      setf.sig  Fkxz_stag=Rkxz_stag
cm      setf.sig  Fkyy_stag=Rkyy_stag
cm      setf.sig  Fkyz_stag=Rkyz_stag
cm      setf.sig  Fkzz_stag=Rkzz_stag
cf      fpma      Fkxxc=Fkxx_stag,Fhalf,Fkxx_stag
cf      fpma      Fkxyc=Fkxy_stag,Fhalf,Fkxy_stag
cf      fpma      Fkxzc=Fkxz_stag,Fhalf,Fkxz_stag
cf      fpma      Fkyyc=Fkyy_stag,Fhalf,Fkyy_stag
cf      fpma      Fkyzc=Fkyz_stag,Fhalf,Fkyz_stag
cf      fpma      Fkzzc=Fkzz_stag,Fhalf,Fkzz_stag
cm      ld4       R0kxx_stag_p=[Rpk_stag_p],4
cm      ld4       R0kxy_stag_p=[Rpk_stag_p],4
cm      ld4       R0kxz_stag_p=[Rpk_stag_p],4
cm      ld4       R0kyy_stag_p=[Rpk_stag_p],4
cm      ld4       R0kyz_stag_p=[Rpk_stag_p],4
cm      ld4       R0kzz_stag_p=[Rpk_stag_p],4
cm      ld4       R1kxx_stag_p=[Rpk_stag_p],4
cm      ld4       R1kxy_stag_p=[Rpk_stag_p],4
cm      ld4       R1kxz_stag_p=[Rpk_stag_p],4
cm      ld4       R1kyy_stag_p=[Rpk_stag_p],4
cm      ld4       R1kyz_stag_p=[Rpk_stag_p],4
cm      ld4       R1kzz_stag_p=[Rpk_stag_p],4
ci      unpack4.l Rkxx_stag_p=R1kxx_stag_p,R0kxx_stag_p
ci      unpack4.l Rkxy_stag_p=R1kxy_stag_p,R0kxy_stag_p
ci      unpack4.l Rkxz_stag_p=R1kxz_stag_p,R0kxz_stag_p
ci      unpack4.l Rkyy_stag_p=R1kyy_stag_p,R0kyy_stag_p
ci      unpack4.l Rkyz_stag_p=R1kyz_stag_p,R0kyz_stag_p
ci      unpack4.l Rkzz_stag_p=R1kzz_stag_p,R0kzz_stag_p
cm      setf.sig  Fkxx_stag_p=Rkxx_stag_p
cm      setf.sig  Fkxy_stag_p=Rkxy_stag_p
cm      setf.sig  Fkxz_stag_p=Rkxz_stag_p
cm      setf.sig  Fkyy_stag_p=Rkyy_stag_p
cm      setf.sig  Fkyz_stag_p=Rkyz_stag_p
cm      setf.sig  Fkzz_stag_p=Rkzz_stag_p
cf      fpnma     Fkxxc=Fkxx_stag_p,Fhalf,Fkxxc
cf      fpnma     Fkxyc=Fkxy_stag_p,Fhalf,Fkxyc
cf      fpnma     Fkxzc=Fkxz_stag_p,Fhalf,Fkxzc
cf      fpnma     Fkyyc=Fkyy_stag_p,Fhalf,Fkyyc
cf      fpnma     Fkyzc=Fkyz_stag_p,Fhalf,Fkyzc
cf      fpnma     Fkzzc=Fkzz_stag_p,Fhalf,Fkzzc


c        output( 96)=kxxc
c        output( 97)=kxyc
c        output( 98)=kxzc
c        output( 99)=kyyc
c        output(100)=kyzc
c        output(101)=kzzc
cd      getf.sig  Rdmptmp=Fkxxc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fkxyc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fkxzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fkyyc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fkyzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Fkzzc
cd      st4       [Rpoutput]=Rdmptmp,4


         tmpkxyckxzc = kxyc*kxzc
         tmpkxyckyzc = kxyc*kyzc
         tmpkxzckyzc = kxzc*kyzc
         tmpkxxc = kxxc*kyzc+tmpkxyckxzc
         tmpkyyc = kyyc*kxzc+tmpkxyckyzc
         tmpkzzc = kzzc*kxyc+tmpkxzckyzc
         tmpkxyc2 = kxyc*kxyc
         tmpkxzc2 = kxzc*kxzc
         tmpkyzc2 = kyzc*kyzc

cf      fpma      Ftmpkxyckxzc=Fkxyc,Fkxzc,f0
cf      fpma      Ftmpkxyckyzc=Fkxyc,Fkyzc,f0
cf      fpma      Ftmpkxzckyzc=Fkxzc,Fkyzc,f0
cf      fpma      Ftmpkxxc=Fkxxc,Fkyzc,Ftmpkxyckxzc
cf      fpma      Ftmpkyyc=Fkyyc,Fkxzc,Ftmpkxyckyzc
cf      fpma      Ftmpkzzc=Fkzzc,Fkxyc,Ftmpkxzckyzc
cf      fpma      Ftmpkxyc2=Fkxyc,Fkxyc,f0
cf      fpma      Ftmpkxzc2=Fkxzc,Fkxzc,f0
cf      fpma      Ftmpkyzc2=Fkyzc,Fkyzc,f0

c        output(102)=tmpkxyckxzc
c        output(103)=tmpkxyckyzc
c        output(104)=tmpkxzckyzc
c        output(105)=tmpkxxc
c        output(106)=tmpkyyc
c        output(107)=tmpkzzc
c        output(108)=tmpkxyc2
c        output(109)=tmpkxzc2
c        output(110)=tmpkyzc2
cd      getf.sig  Rdmptmp=Ftmpkxyckxzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkxyckyzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkxzckyzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkxxc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkyyc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkzzc
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkxyc2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkxzc2
cd      st4       [Rpoutput]=Rdmptmp,4
cd      getf.sig  Rdmptmp=Ftmpkyzc2
cd      st4       [Rpoutput]=Rdmptmp,4


         KK11=2*(uppermet_uyz*tmpkxyckxzc+
     &           kxxc*(uppermet_uxy*kxyc+uppermet_uxz*kxzc))+
     &        uppermet_uxx*kxxc*kxxc+
     &        uppermet_uyy*tmpkxyc2+
     &        uppermet_uzz*tmpkxzc2

cf      fpma      FKK11=Fuppermet_uxy,Fkxyc,f0
cf      fpma      FKK11=Fuppermet_uxz,Fkxzc,FKK11
cf      fpma      FKK11=Fkxxc,FKK11,f0
cf      fpma      FKK11=Fuppermet_uyz,Ftmpkxyckxzc,FKK11
cf      fpma      FKK11=FKK11,Ftwo,f0
cf      fpma      Fkxxc2=Fkxxc,Fkxxc,f0
cf      fpma      FKK11=Fuppermet_uxx,Fkxxc2,FKK11
cf      fpma      FKK11=Fuppermet_uyy,Ftmpkxyc2,FKK11
cf      fpma      FKK11=Fuppermet_uzz,Ftmpkxzc2,FKK11

c        output(111)=KK11
cd      getf.sig  Rdmptmp=FKK11
cd      st4       [Rpoutput]=Rdmptmp,4


         KK12=kxyc*(uppermet_uxx*kxxc+uppermet_uyy*kyyc)+
     &        uppermet_uzz*tmpkxzckyzc+
     &        uppermet_uxz*tmpkxxc+
     &        uppermet_uyz*tmpkyyc+
     &        uppermet_uxy*(kxxc*kyyc+tmpkxyc2)

cf      fpma      FKK12=Fuppermet_uxx,Fkxxc,f0
cf      fpma      FKK12=Fuppermet_uyy,Fkyyc,FK12
cf      fpma      FKK12=Fkxyc,FKK12,f0
cf      fpma      FKK12=Fuppermet_uzz,Ftmpkxzckyzc,FKK12
cf      fpma      FKK12=Fuppermet_uxz,Ftmpkxxc,FKK12
cf      fpma      FKK12=Fuppermet_uyz,Ftmpkyyc,FKK12
cf      fpma      FKK12tmp=Fkxxc,Fkyyc,Ftmpkxyc2
cf      fpma      FKK12=Fuppermet_uxy,FKK12tmp,FKK12

c        output(112)=KK12
cd      getf.sig  Rdmptmp=FKK12
cd      st4       [Rpoutput]=Rdmptmp,4


         KK13=kxzc*(uppermet_uxx*kxxc+uppermet_uzz*kzzc)+
     &        uppermet_uyy*tmpkxyckyzc+
     &        uppermet_uxy*tmpkxxc+
     &        uppermet_uyz*tmpkzzc+
     &        uppermet_uxz*(kxxc*kzzc+tmpkxzc2)

cf      fpma      FKK13=Fuppermet_uxx,Fkxxc,f0
cf      fpma      FKK13=Fuppermet_uzz,Fkzzc,FK13
cf      fpma      FKK13=Fkxzc,FKK13,f0
cf      fpma      FKK13=Fuppermet_uyy,Ftmpkxyckyzc,FKK13
cf      fpma      FKK13=Fuppermet_uxy,Ftmpkxxc,FKK13
cf      fpma      FKK13=Fuppermet_uyz,Ftmpkzzc,FKK13
cf      fpma      FKK13tmp=Fkxxc,Fkzzc,Ftmpkxzc2
cf      fpma      FKK13=Fuppermet_uxz,FKK13tmp,FKK13

c        output(113)=KK13
cd      getf.sig  Rdmptmp=FKK13
cd      st4       [Rpoutput]=Rdmptmp,4


         KK22=2*(uppermet_uxz*tmpkxyckyzc+
     &           kyyc*(uppermet_uyz*kyzc+uppermet_uxy*kxyc))+
     &        uppermet_uxx*tmpkxyc2+
     &        uppermet_uyy*kyyc*kyyc+
     &        uppermet_uzz*tmpkyzc2

cf      fpma      FKK22=Fuppermet_uyz,Fkyzc,f0
cf      fpma      FKK22=Fuppermet_uxy,Fkxyc,FKK22
cf      fpma      FKK22=Fkyyc,FKK22,f0
cf      fpma      FKK22=Fuppermet_uxz,Ftmpkxyckyzc,FKK22
cf      fpma      FKK22=FKK22,Ftwo,f0
cf      fpma      FKK22=Fuppermet_uxx,Ftmpkxyc2,FKK22
cf      fpma      Fkyyc2=Fkyyc,Fkyyc,f0
cf      fpma      FKK22=Fuppermet_uyy,Fkyyc2,FKK22
cf      fpma      FKK22=Fuppermet_uzz,Ftmpkyzc2,FKK22

c        output(114)=KK22
cd      getf.sig  Rdmptmp=FKK22
cd      st4       [Rpoutput]=Rdmptmp,4


         KK23=kyzc*(uppermet_uyy*kyyc+uppermet_uzz*kzzc)+
     &        uppermet_uxx*tmpkxyckxzc+
     &        uppermet_uxy*tmpkyyc+
     &        uppermet_uxz*tmpkzzc+
     &        uppermet_uyz*(kyyc*kzzc+tmpkyzc2) 

cf      fpma      FKK23=Fuppermet_uyy,Fkyyc,f0
cf      fpma      FKK23=Fuppermet_uzz,Fkzzc,FK23
cf      fpma      FKK23=Fkyzc,FKK23,f0
cf      fpma      FKK23=Fuppermet_uxx,Ftmpkxyckxzc,FKK23
cf      fpma      FKK23=Fuppermet_uxy,Ftmpkyyc,FKK23
cf      fpma      FKK23=Fuppermet_uxz,Ftmpkzzc,FKK23
cf      fpma      FKK23tmp=Fkyyc,Fkzzc,Ftmpkyzc2
cf      fpma      FKK23=Fuppermet_uyz,FKK23tmp,FKK23

c        output(115)=KK23
cd      getf.sig  Rdmptmp=FKK23
cd      st4       [Rpoutput]=Rdmptmp,4


         KK33=2*(uppermet_uxy*tmpkxzckyzc+
     &           kzzc*(uppermet_uyz*kyzc+uppermet_uxz*kxzc))+
     &        uppermet_uxx*tmpkxzc2+
     &        uppermet_uyy*tmpkyzc2+
     &        uppermet_uzz*kzzc*kzzc 

cf      fpma      FKK33=Fuppermet_uyz,Fkyzc,f0
cf      fpma      FKK33=Fuppermet_uxz,Fkxzc,FKK33
cf      fpma      FKK33=Fkzzc,FKK33,f0
cf      fpma      FKK33=Fuppermet_uxy,Ftmpkxzckyzc,FKK33
cf      fpma      FKK33=FKK33,Ftwo,f0
cf      fpma      FKK33=Fuppermet_uxx,Ftmpkxzc2,FKK33
cf      fpma      FKK33=Fuppermet_uyy,Ftmpkyzc2,FKK33
cf      fpma      Fkzzc2=Fkzzc,Fkzzc,f0
cf      fpma      FKK33=Fuppermet_uzz,Fkzzc2,FKK33

c        output(116)=KK33
cd      getf.sig  Rdmptmp=FKK33
cd      st4       [Rpoutput]=Rdmptmp,4


         trk_trK = uppermet_uxx*kxxc+
     &             uppermet_uyy*kyyc+
     &             uppermet_uzz*kzzc+
     &             2D0*(uppermet_uxy*kxyc+
     &                  uppermet_uxz*kxzc+
     &                  uppermet_uyz*kyzc)

cf      fpma      Ftrk_trK=Fuppermet_uxy,Fkxyc,f0
cf      fpma      Ftrk_trK=Fuppermet_uxz,Fkxzc,Ftrk_trK
cf      fpma      Ftrk_trK=Fuppermet_uyz,Fkyzc,Ftrk_trK
cf      fpma      Ftrk_trK=Ftrk_trK,f1,Ftrk_trK
cf      fpma      Ftrk_trK=Fuppermet_uxx,Fkxxc,Ftrk_trK
cf      fpma      Ftrk_trK=Fuppermet_uyy,Fkyyc,Ftrk_trK
cf      fpma      Ftrk_trK=Fuppermet_uzz,Fkzzc,Ftrk_trK

c        output(117)=trk_trK
cd      getf.sig  Rdmptmp=Ftrk_trK
cd      st4       [Rpoutput]=Rdmptmp,4


         da_dxda  = i2dx*(l(lalp,ip,j0,k0)-l(lalp,im,j0,k0))

cm      ld4       Rlalp_ip_j0_k0_lo=[Rpl_j0_k0],32
cm      ld4       Rlalp_ip_j0_k0_hi=[Rpl_j0_k0],-96
cm      ld4       Rlalp_im_j0_k0_lo=[Rpl_j0_k0],32
cm      ld4       Rlalp_im_j0_k0_hi=[Rpl_j0_k0],40
ci      unpack4.l Rlalp_ip_j0_k0=Rlalp_ip_j0_k0_hi,Rlalp_ip_j0_k0_lo
ci      unpack4.l Rlalp_im_j0_k0=Rlalp_im_j0_k0_hi,Rlalp_im_j0_k0_lo
cm      setf.sig  Flalp_ip_j0_k0=Rlalp_ip_j0_k0
cm      setf.sig  Flalp_im_j0_k0=Rlalp_im_j0_k0
cf      fpms      Fda_dxda=Flalp_ip_j0_k0,f1,Flalp_im_j0_k0
cf      fpma      Fda_dxda=Fi2dx,Fda_dxda,f0
ci      unpack4.l Rlalp_i0_j0_k0=Rlalp_ip_j0_k0_lo,Rlalp_im_j0_k0_hi

c        output(118)=da_dxda
cd      getf.sig  Rdmptmp=Fda_dxda
cd      st4       [Rpoutput]=Rdmptmp,4


         da_dyda  = i2dy*(l(lalp,i0,jp,k0)-l(lalp,i0,jm,k0))

cm      ld4       Rlalp_im_jp_k0_hi=[Rpl_jp_k0],32
cm      ld4       Rlalp_ip_jp_k0_lo=[Rpl_jp_k0],32
cm      ld4       Rlalp_ip_jp_k0_hi=[Rpl_jp_k0],-96
cm      ld4       Rlalp_im_jp_k0_lo=[Rpl_jp_k0]
ci      unpack4.l Rlalp_i0_jp_k0=Rlalp_ip_jp_k0_lo,Rlalp_im_jp_k0_hi
cm      ld4       Rlalp_im_jm_k0_hi=[Rpl_jm_k0],32
cm      ld4       Rlalp_ip_jm_k0_lo=[Rpl_jm_k0],32
cm      ld4       Rlalp_ip_jm_k0_hi=[Rpl_jm_k0],-96
cm      ld4       Rlalp_im_jm_k0_lo=[Rpl_jm_k0]
ci      unpack4.l Rlalp_i0_jm_k0=Rlalp_ip_jm_k0_lo,Rlalp_im_jm_k0_hi
cm      setf.sig  Flalp_i0_jp_k0=Rlalp_i0_jp_k0
cm      setf.sig  Flalp_i0_jm_k0=Rlalp_i0_jm_k0
cf      fpms      Fda_dyda=Flalp_i0_jp_k0,f1,Flalp_i0_jm_k0
cf      fpma      Fda_dyda=Fi2dy,Fda_dyda,f0
ci      unpack4.l Rlalp_ip_jp_k0=Rlalp_ip_jp_k0_hi,Rlalp_ip_jp_k0_lo
ci      unpack4.l Rlalp_im_jp_k0=Rlalp_im_jp_k0_hi,Rlalp_im_jp_k0_lo
ci      unpack4.l Rlalp_ip_jm_k0=Rlalp_ip_jm_k0_hi,Rlalp_ip_jm_k0_lo
ci      unpack4.l Rlalp_im_jm_k0=Rlalp_im_jm_k0_hi,Rlalp_im_jm_k0_lo

c        output(119)=da_dyda
cd      getf.sig  Rdmptmp=Fda_dyda
cd      st4       [Rpoutput]=Rdmptmp,4


         da_dzda  = i2dz*(l(lalp,i0,j0,kp)-l(lalp,i0,j0,km))

cm      ld4       Rlalp_im_j0_kp_hi=[Rpl_j0_kp],32
cm      ld4       Rlalp_ip_j0_kp_lo=[Rpl_j0_kp],32
cm      ld4       Rlalp_ip_j0_kp_hi=[Rpl_j0_kp],-96
cm      ld4       Rlalp_im_j0_kp_lo=[Rpl_j0_kp],72
ci      unpack4.l Rlalp_i0_j0_kp=Rlalp_ip_j0_kp_lo,Rlalp_im_j0_kp_hi
cm      ld4       Rlalp_im_j0_km_hi=[Rpl_j0_km],32
cm      ld4       Rlalp_ip_j0_km_lo=[Rpl_j0_km],32
cm      ld4       Rlalp_ip_j0_km_hi=[Rpl_j0_km],-96
cm      ld4       Rlalp_im_j0_km_lo=[Rpl_j0_km],72
ci      unpack4.l Rlalp_i0_j0_km=Rlalp_ip_j0_km_lo,Rlalp_im_j0_km_hi
cm      setf.sig  Flalp_i0_j0_kp=Rlalp_i0_j0_kp
cm      setf.sig  Flalp_i0_j0_km=Rlalp_i0_j0_km
cf      fpms      Fda_dzda=Flalp_i0_j0_kp,f1,Flalp_i0_j0_km
cf      fpma      Fda_dzda=Fi2dz,Fda_dyda,f0
ci      unpack4.l Rlalp_ip_j0_kp=Rlalp_ip_j0_kp_hi,Rlalp_ip_j0_kp_lo
ci      unpack4.l Rlalp_im_j0_kp=Rlalp_im_j0_kp_hi,Rlalp_im_j0_kp_lo
ci      unpack4.l Rlalp_ip_j0_km=Rlalp_ip_j0_km_hi,Rlalp_ip_j0_km_lo
ci      unpack4.l Rlalp_im_j0_km=Rlalp_im_j0_km_hi,Rlalp_im_j0_km_lo

c        output(120)=da_dzda
cd      getf.sig  Rdmptmp=Fda_dzda
cd      st4       [Rpoutput]=Rdmptmp,4


         dda_dxxda  = idx2*(l(lalp,ip,j0,k0)-
     &                      2D0*l(lalp,i0,j0,k0)+
     &                      l(lalp,im,j0,k0))

cm      setf.sig  Flalp_i0_j0_k0=Rlalp_i0_j0_k0
cf      fpnma     Fdda_dxxda=Flalp_i0_j0_k0,Ftwo,Flalp_ip_j0_k0
cf      fpma      Fdda_dxxda=Fdda_dxxda,f1,Flalp_im_j0_k0
cf      fpma      Fdda_dxxda=Fdda_dxxda,Fidx2,f0

c        output(121)=dda_dxxda
cd      getf.sig  Rdmptmp=Fdda_dxxda
cd      st4       [Rpoutput]=Rdmptmp,4


         cdcda_cdxxda=(dda_dxxda-
     &                 gamma111*da_dxda-
     &                 gamma211*da_dyda-
     &                 gamma311*da_dzda)

cf      fpnma     Fcdcda_cdxxda=Fgamma111,Fda_dxda,Fdda_dxxda
cf      fpnma     Fcdcda_cdxxda=Fgamma211,Fda_dyda,Fcdcda_cdxxda
cf      fpnma     Fcdcda_cdxxda=Fgamma311,Fda_dzda,Fcdcda_cdxxda

c        output(122)=cdcda_cdxxda
cd      getf.sig  Rdmptmp=Fcdcda_cdxxda
cd      st4       [Rpoutput]=Rdmptmp,4


         dkdt_dkxxdt=l(lalp,i0,j0,k0)*(ricci_R11-2*KK11+kxxc*trk_trK)-
     &               cdcda_cdxxda 

cf      fpnma     Fdkdt_dkxxdt=FKK11,Ftwo,Fricci_R11
cf      fpma      Fdkdt_dkxxdt=Fkxxc,Ftrk_trK,Fdkdt_dkxxdt
cf      fpms      Fdkdt_dkxxdt=Flalp_i0_j0_k0,Fdkdt_dkxxdt,Fcdcda_cdxxda

c        output(123)=dkdt_dkxxdt
cd      getf.sig  Rdmptmp=Fdkdt_dkxxdt
cd      st4       [Rpoutput]=Rdmptmp,4


         ADM_k_stag_n(xx,i,j,k) = ADM_k_stag(xx,i,j,k) + dkdt_dkxxdt*dt

cf      fpma      Fkxx_stag_n=Fdkdt_dkxxdt,Fdt,Fkxx_stag
cm      getf.sig  Rkxx_stag_n=Fkxx_stag_n
cm      st4       [Rpk_stag_n]=Rkxx_stag_n,24
ci      shr       Rkxx_stag_n=Rkxx_stag_n,32
cm      st4       [Rpk_stag_n]=Rkxx_stag_n,-20

c        output(124)=ADM_k_stag_n(xx,i,j,k)
cd      getf.sig  Rdmptmp=Fkxx_stag_n
cd      st4       [Rpoutput]=Rdmptmp,4


         dda_dxyda  = i4dxdy*(l(lalp,ip,jp,k0)-
     &                        l(lalp,ip,jm,k0)-
     &                        l(lalp,im,jp,k0)+
     &                        l(lalp,im,jm,k0))

cm      setf.sig  Flalp_ip_jp_k0=Rlalp_ip_jp_k0
cm      setf.sig  Flalp_ip_jm_k0=Rlalp_ip_jm_k0
cm      setf.sig  Flalp_im_jp_k0=Rlalp_im_jp_k0
cm      setf.sig  Flalp_im_jm_k0=Rlalp_im_jm_k0
cf      fpms      Fdda_dxyda=Flalp_ip_jp_k0,f1,Flalp_ip_jm_k0
cf      fpms      Fdda_dxyda=Fdda_dxyda,f1,Flalp_im_jp_k0
cf      fpma      Fdda_dxyda=Fdda_dxyda,f1,Flalp_im_jm_k0
cf      fpma      Fdda_dxyda=Fi4xdy,Fdda_dxyda,f0

c        output(125)=dda_dxyda
cd      getf.sig  Rdmptmp=Fdda_dxyda
cd      st4       [Rpoutput]=Rdmptmp,4


         cdcda_cdxyda=(dda_dxyda-
     &                 gamma112*da_dxda-
     &                 gamma212*da_dyda-
     &                 gamma312*da_dzda)

cf      fpnma     Fcdcda_cdxyda=Fgamma112,Fda_dxda,Fdda_dxyda
cf      fpnma     Fcdcda_cdxyda=Fgamma212,Fda_dyda,Fcdcda_cdxyda
cf      fpnma     Fcdcda_cdxyda=Fgamma312,Fda_dzda,Fcdcda_cdxyda

c        output(126)=cdcda_cdxyda
cd      getf.sig  Rdmptmp=Fcdcda_cdxyda
cd      st4       [Rpoutput]=Rdmptmp,4


         dkdt_dkxydt=l(lalp,i0,j0,k0)*(ricci_R12-2*KK12+kxyc*trk_trK)-
     &               cdcda_cdxyda 

cf      fpnma     Fdkdt_dkxydt=FKK12,Ftwo,Fricci_R12
cf      fpma      Fdkdt_dkxydt=Fkxyc,Ftrk_trK,Fdkdt_dkxydt
cf      fpms      Fdkdt_dkxydt=Flalp_i0_j0_k0,Fdkdt_dkxydt,Fcdcda_cdxyda

c        output(127)=dkdt_dkxydt
cd      getf.sig  Rdmptmp=Fdkdt_dkxydt
cd      st4       [Rpoutput]=Rdmptmp,4


         ADM_k_stag_n(xy,i,j,k) = ADM_k_stag(xy,i,j,k) + dkdt_dkxydt*dt

cf      fpma      Fkxy_stag_n=Fdkdt_dkxydt,Fdt,Fkxy_stag
cm      getf.sig  Rkxy_stag_n=Fkxy_stag_n
cm      st4       [Rpk_stag_n]=Rkxy_stag_n,24
ci      shr       Rkxy_stag_n=Rkxy_stag_n,32
cm      st4       [Rpk_stag_n]=Rkxy_stag_n,-20

c        output(128)=ADM_k_stag_n(xy,i,j,k)
cd      getf.sig  Rdmptmp=Fkxy_stag_n
cd      st4       [Rpoutput]=Rdmptmp,4


         dda_dxzda  = i4dxdz*(l(lalp,ip,j0,kp)-
     &                        l(lalp,ip,j0,km)-
     &                        l(lalp,im,j0,kp)+
     &                        l(lalp,im,j0,km))

cm      setf.sig  Flalp_ip_j0_km=Rlalp_ip_j0_km
cm      setf.sig  Flalp_ip_j0_kp=Rlalp_ip_j0_kp
cm      setf.sig  Flalp_im_j0_km=Rlalp_im_j0_km
cm      setf.sig  Flalp_im_j0_kp=Rlalp_im_j0_kp
cf      fpms      Fdda_dxzda=Flalp_ip_j0_kp,f1,Flalp_ip_j0_km
cf      fpms      Fdda_dxzda=Fdda_dxzda,f1,Flalp_im_j0_kp
cf      fpma      Fdda_dxzda=Fdda_dxzda,f1,Flalp_im_j0_km
cf      fpma      Fdda_dxzda=Fi4xdz,Fdda_dxzda,f0

c        output(129)=dda_dxzda
cd      getf.sig  Rdmptmp=Fdda_dxzda
cd      st4       [Rpoutput]=Rdmptmp,4


         cdcda_cdxzda=(dda_dxzda-
     &                 gamma113*da_dxda-
     &                 gamma213*da_dyda-
     &                 gamma313*da_dzda)

cf      fpnma     Fcdcda_cdxzda=Fgamma113,Fda_dxda,Fdda_dxzda
cf      fpnma     Fcdcda_cdxzda=Fgamma213,Fda_dyda,Fcdcda_cdxzda
cf      fpnma     Fcdcda_cdxzda=Fgamma313,Fda_dzda,Fcdcda_cdxzda

c        output(130)=cdcda_cdxzda
cd      getf.sig  Rdmptmp=Fcdcda_cdxzda
cd      st4       [Rpoutput]=Rdmptmp,4


         dkdt_dkxzdt=l(lalp,i0,j0,k0)*(ricci_R13-2*KK13+kxzc*trk_trK)-
     &               cdcda_cdxzda 

cf      fpnma     Fdkdt_dkxzdt=FKK13,Ftwo,Fricci_R13
cf      fpma      Fdkdt_dkxzdt=Fkxzc,Ftrk_trK,Fdkdt_dkxzdt
cf      fpms      Fdkdt_dkxzdt=Flalp_i0_j0_k0,Fdkdt_dkxzdt,Fcdcda_cdxzda

c        output(131)=dkdt_dkxzdt
cd      getf.sig  Rdmptmp=Fdkdt_dkxzdt
cd      st4       [Rpoutput]=Rdmptmp,4


         ADM_k_stag_n(xz,i,j,k) = ADM_k_stag(xz,i,j,k) + dkdt_dkxzdt*dt

cf      fpma      Fkxz_stag_n=Fdkdt_dkxzdt,Fdt,Fkxz_stag
cm      getf.sig  Rkxz_stag_n=Fkxz_stag_n
cm      st4       [Rpk_stag_n]=Rkxz_stag_n,24
ci      shr       Rkxz_stag_n=Rkxz_stag_n,32
cm      st4       [Rpk_stag_n]=Rkxz_stag_n,-20

c        output(132)=ADM_k_stag_n(xz,i,j,k)
cd      getf.sig  Rdmptmp=Fkxz_stag_n
cd      st4       [Rpoutput]=Rdmptmp,4


         dda_dyyda  = idy2*(l(lalp,i0,jp,k0)-
     &                      2D0*l(lalp,i0,j0,k0)+
     &                      l(lalp,i0,jm,k0))

cf      fpnma     Fdda_dyyda=Flalp_i0_j0_k0,Ftwo,Flalp_i0_jp_k0
cf      fpma      Fdda_dyyda=Fdda_dyyda,f1,Flalp_i0_jm_k0
cf      fpma      Fdda_dyyda=Fdda_dyyda,Fidy2,f0

c        output(133)=dda_dyyda
cd      getf.sig  Rdmptmp=Fdda_dyyda
cd      st4       [Rpoutput]=Rdmptmp,4


         cdcda_cdyyda=(dda_dyyda-
     &                 gamma122*da_dxda-
     &                 gamma222*da_dyda-
     &                 gamma322*da_dzda)

cf      fpnma     Fcdcda_cdyyda=Fgamma122,Fda_dxda,Fdda_dyyda
cf      fpnma     Fcdcda_cdyyda=Fgamma222,Fda_dyda,Fcdcda_cdyyda
cf      fpnma     Fcdcda_cdyyda=Fgamma322,Fda_dzda,Fcdcda_cdyyda

c        output(134)=cdcda_cdyyda
cd      getf.sig  Rdmptmp=Fcdcda_cdyyda
cd      st4       [Rpoutput]=Rdmptmp,4


         dkdt_dkyydt=l(lalp,i0,j0,k0)*(ricci_R22-2*KK22+kyyc*trk_trK)-
     &               cdcda_cdyyda 

cf      fpnma     Fdkdt_dkyydt=FKK22,Ftwo,Fricci_R22
cf      fpma      Fdkdt_dkyydt=Fkyyc,Ftrk_trK,Fdkdt_dkyydt
cf      fpms      Fdkdt_dkyydt=Flalp_i0_j0_k0,Fdkdt_dkyydt,Fcdcda_cdyyda

c        output(135)=dkdt_dkyydt
cd      getf.sig  Rdmptmp=Fdkdt_dkyydt
cd      st4       [Rpoutput]=Rdmptmp,4


         ADM_k_stag_n(yy,i,j,k) = ADM_k_stag(yy,i,j,k) + dkdt_dkyydt*dt

cf      fpma      Fkyy_stag_n=Fdkdt_dkyydt,Fdt,Fkyy_stag
cm      getf.sig  Rkyy_stag_n=Fkyy_stag_n
cm      st4       [Rpk_stag_n]=Rkyy_stag_n,24
ci      shr       Rkyy_stag_n=Rkyy_stag_n,32
cm      st4       [Rpk_stag_n]=Rkyy_stag_n,-20

c        output(136)=ADM_k_stag_n(yy,i,j,k)
cd      getf.sig  Rdmptmp=Fkyy_stag_n
cd      st4       [Rpoutput]=Rdmptmp,4


         dda_dyzda  = i4dydz*(l(lalp,i0,jp,kp)-
     &                        l(lalp,i0,jp,km)-
     &                        l(lalp,i0,jm,kp)+
     &                        l(lalp,i0,jm,km))

cm      ld4       Rlalp_i0_jp_kp_lo=[Rpl_jp_kp],4
cm      ld4       Rlalp_i0_jp_kp_hi=[Rpl_jp_kp]
cm      ld4       Rlalp_i0_jp_km_lo=[Rpl_jp_km],4
cm      ld4       Rlalp_i0_jp_km_hi=[Rpl_jp_km]
cm      ld4       Rlalp_i0_jm_kp_lo=[Rpl_jm_kp],4
cm      ld4       Rlalp_i0_jm_kp_hi=[Rpl_jm_kp]
cm      ld4       Rlalp_i0_jm_km_lo=[Rpl_jm_km],4
cm      ld4       Rlalp_i0_jm_km_hi=[Rpl_jm_km]
ci      unpack4.l Rlalp_i0_jp_kp=Rlalp_i0_jp_kp_hi,Rlalp_i0_jp_kp_lo
ci      unpack4.l Rlalp_i0_jp_km=Rlalp_i0_jp_km_hi,Rlalp_i0_jp_km_lo
ci      unpack4.l Rlalp_i0_jm_kp=Rlalp_i0_jm_kp_hi,Rlalp_i0_jm_kp_lo
ci      unpack4.l Rlalp_i0_jm_km=Rlalp_i0_jm_km_hi,Rlalp_i0_jm_km_lo
cm      setf.sig  Flalp_i0_jp_kp=Rlalp_i0_jp_kp
cm      setf.sig  Flalp_i0_jp_km=Rlalp_i0_jp_km
cm      setf.sig  Flalp_i0_jm_kp=Rlalp_i0_jm_kp
cm      setf.sig  Flalp_i0_jm_km=Rlalp_i0_jm_km
cf      fpms      Fdda_dyzda=Flalp_i0_jp_kp,f1,Flalp_i0_jp_km
cf      fpms      Fdda_dyzda=Fdda_dyzda,f1,Flalp_i0_jm_kp
cf      fpma      Fdda_dyzda=Fdda_dyzda,f1,Flalp_i0_jm_km
cf      fpma      Fdda_dyzda=Fi4ydz,Fdda_dyzda,f0

c        output(137)=dda_dyzda
cd      getf.sig  Rdmptmp=Fdda_dyzda
cd      st4       [Rpoutput]=Rdmptmp,4


         cdcda_cdyzda=(dda_dyzda-
     &                 gamma123*da_dxda-
     &                 gamma223*da_dyda-
     &                 gamma323*da_dzda)

cf      fpnma     Fcdcda_cdyzda=Fgamma123,Fda_dxda,Fdda_dyzda
cf      fpnma     Fcdcda_cdyzda=Fgamma223,Fda_dyda,Fcdcda_cdyzda
cf      fpnma     Fcdcda_cdyzda=Fgamma323,Fda_dzda,Fcdcda_cdyzda

c        output(138)=cdcda_cdyzda
cd      getf.sig  Rdmptmp=Fcdcda_cdyzda
cd      st4       [Rpoutput]=Rdmptmp,4


         dkdt_dkyzdt=l(lalp,i0,j0,k0)*(ricci_R23-2*KK23+kyzc*trk_trK)-
     &               cdcda_cdyzda

cf      fpnma     Fdkdt_dkyzdt=FKK23,Ftwo,Fricci_R23
cf      fpma      Fdkdt_dkyzdt=Fkyzc,Ftrk_trK,Fdkdt_dkyzdt
cf      fpms      Fdkdt_dkyzdt=Flalp_i0_j0_k0,Fdkdt_dkyzdt,Fcdcda_cdyzda

c        output(139)=dkdt_dkyzdt
cd      getf.sig  Rdmptmp=Fdkdt_dkyzdt
cd      st4       [Rpoutput]=Rdmptmp,4


         ADM_k_stag_n(yz,i,j,k) = ADM_k_stag(yz,i,j,k) + dkdt_dkyzdt*dt

cf      fpma      Fkyz_stag_n=Fdkdt_dkyzdt,Fdt,Fkyz_stag
cm      getf.sig  Rkyz_stag_n=Fkyz_stag_n
cm      st4       [Rpk_stag_n]=Rkyz_stag_n,24
ci      shr       Rkyz_stag_n=Rkyz_stag_n,32
cm      st4       [Rpk_stag_n]=Rkyz_stag_n,-20

c        output(140)=ADM_k_stag_n(yz,i,j,k)
cd      getf.sig  Rdmptmp=Fkyz_stag_n
cd      st4       [Rpoutput]=Rdmptmp,4


         dda_dzzda  = idz2*(l(lalp,i0,j0,kp)-
     &                      2D0*l(lalp,i0,j0,k0)+
     &                      l(lalp,i0,j0,km))

cf      fpnma     Fdda_dzzda=Flalp_i0_j0_k0,Ftwo,Flalp_i0_j0_kp
cf      fpma      Fdda_dzzda=Fdda_dzzda,f1,Flalp_i0_j0_km
cf      fpma      Fdda_dzzda=Fdda_dzzda,Fidz2,f0

c        output(141)=dda_dzzda
cd      getf.sig  Rdmptmp=Fdda_dzzda
cd      st4       [Rpoutput]=Rdmptmp,4


         cdcda_cdzzda=(dda_dzzda-
     &                 gamma133*da_dxda-
     &                 gamma233*da_dyda-
     &                 gamma333*da_dzda)

cf      fpnma     Fcdcda_cdzzda=Fgamma133,Fda_dxda,Fdda_dzzda
cf      fpnma     Fcdcda_cdzzda=Fgamma233,Fda_dyda,Fcdcda_cdzzda
cf      fpnma     Fcdcda_cdzzda=Fgamma333,Fda_dzda,Fcdcda_cdzzda

c        output(142)=cdcda_cdzzda
cd      getf.sig  Rdmptmp=Fcdcda_cdzzda
cd      st4       [Rpoutput]=Rdmptmp,4


         dkdt_dkzzdt=l(lalp,i0,j0,k0)*(ricci_R33-2*KK33+kzzc*trk_trK)-
     &               cdcda_cdzzda 

cf      fpnma     Fdkdt_dkzzdt=FKK33,Ftwo,Fricci_R33
cf      fpma      Fdkdt_dkzzdt=Fkzzc,Ftrk_trK,Fdkdt_dkzzdt
cf      fpms      Fdkdt_dkzzdt=Flalp_i0_j0_k0,Fdkdt_dkzzdt,Fcdcda_cdzzda

c        output(143)=dkdt_dkzzdt
cd      getf.sig  Rdmptmp=Fdkdt_dkzzdt
cd      st4       [Rpoutput]=Rdmptmp,4


         ADM_k_stag_n(zz,i,j,k) = ADM_k_stag(zz,i,j,k) + dkdt_dkzzdt*dt 

cf      fpma      Fkzz_stag_n=Fdkdt_dkzzdt,Fdt,Fkzz_stag
cm      getf.sig  Rkzz_stag_n=Fkzz_stag_n
cm      st4       [Rpk_stag_n]=Rkzz_stag_n,24
ci      shr       Rkzz_stag_n=Rkzz_stag_n,32
cm      st4       [Rpk_stag_n]=Rkzz_stag_n,4

c        output(144)=ADM_k_stag_n(zz,i,j,k)
cd      getf.sig  Rdmptmp=Fkzz_stag_n
cd      st4       [Rpoutput]=Rdmptmp,4


        end do
       end do
      end do

c	Check contents of output array
c	do j = 1, 144
c	   print*, j, output(j)
c	enddo 

      end subroutine Bench_StaggeredLeapfrog2
