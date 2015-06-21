
#define PROBLEM_SIZE 40
#define PRECISION REAL*4

c     ==================================================================
c
c     Wrapper for StaggeredLeapfrog2
c    
c     VERSION: 27th Septemper #4
c
c     ==================================================================
c     BRN & FA: Commented ALL print statements.
c     FA: Increased number of interations to 2000 to amortize cost of loading program
c	FA: Verified consistency with original kernel

      program stagleap2

      implicit none

c     ==================================================================
c     PARAMETERS YOU MAY WANT TO CHANGE
      
c     FIX PROBLEM SIZE
c     ================
      integer,parameter :: dimx = PROBLEM_SIZE
      integer,parameter :: dimy = PROBLEM_SIZE
      integer,parameter :: dimz = PROBLEM_SIZE

c     Number of iterations to perform
      INTEGER, PARAMETER :: niterations = 100

c     ==================================================================
c     PARAMETERS YOU DONT WANT TO CHANGE
      
c     Dimension of arrays without storage
      INTEGER, PARAMETER :: empty = 1

c     Dimensions of arrays
      INTEGER, PARAMETER :: cctk_dim = 3


c     ==================================================================
c     LOCAL VARIABLES
      
      INTEGER i,j,k


c     ==================================================================
c     DECLARE VARIABLES PASSED INTO CACTUS ROUTINE

c      INTEGER,cctk_dim=3
      INTEGER cctk_gsh(cctk_dim),cctk_lsh(cctk_dim)
      INTEGER cctk_lbnd(cctk_dim),cctk_ubnd(cctk_dim)
      INTEGER cctk_lssh(3 *cctk_dim)
      INTEGER cctk_from(cctk_dim),cctk_to(cctk_dim)
      INTEGER cctk_bbox(2*cctk_dim)
      PRECISION   cctk_delta_time, cctk_time
      PRECISION   cctk_delta_space(cctk_dim)
      PRECISION   cctk_origin_space(cctk_dim)
      INTEGER cctk_levfac(cctk_dim)
      INTEGER cctk_convlevel
      INTEGER cctk_nghostzones(cctk_dim)
      INTEGER cctk_iteration
      integer* 4   cctkGH
      INTEGER ADM_curv_prev0
      INTEGER ADM_curv_prev1
      INTEGER ADM_curv_prev2
      INTEGER ADM_curv_stag0
      INTEGER ADM_curv_stag1
      INTEGER ADM_curv_stag2
      INTEGER ADM_metric_prev0
      INTEGER ADM_metric_prev1
      INTEGER ADM_metric_prev2
      INTEGER ADM_sources0
      INTEGER ADM_sources1
      INTEGER ADM_sources2

      PRECISION,ALLOCATABLE::   ADM_g(:,:,:,:)
      PRECISION,ALLOCATABLE::   ADM_g_n(:,:,:,:)

      PRECISION,ALLOCATABLE::   ADM_k_stag_p(:,:,:,:)
      PRECISION,ALLOCATABLE::   ADM_k_stag(:,:,:,:)
      PRECISION,ALLOCATABLE::   ADM_k_stag_n(:,:,:,:)

      PRECISION,ALLOCATABLE::   adms_alp(:,:,:)
      PRECISION,ALLOCATABLE::   adms_gxx(:,:,:)
      PRECISION,ALLOCATABLE::   adms_gxy(:,:,:)
      PRECISION,ALLOCATABLE::   adms_gxz(:,:,:)
      PRECISION,ALLOCATABLE::   adms_gyy(:,:,:)
      PRECISION,ALLOCATABLE::   adms_gyz(:,:,:)
      PRECISION,ALLOCATABLE::   adms_gzz(:,:,:)
      PRECISION,ALLOCATABLE::   adms_kxx(:,:,:)
      PRECISION,ALLOCATABLE::   adms_kxy(:,:,:)
      PRECISION,ALLOCATABLE::   adms_kxz(:,:,:)
      PRECISION,ALLOCATABLE::   adms_kyy(:,:,:)
      PRECISION,ALLOCATABLE::   adms_kyz(:,:,:)
      PRECISION,ALLOCATABLE::   adms_kzz(:,:,:)

      INTEGER confac0
      INTEGER confac1
      INTEGER confac2
      INTEGER confac_1derivs0
      INTEGER confac_1derivs1
      INTEGER confac_1derivs2
      INTEGER confac_2derivs0
      INTEGER confac_2derivs1
      INTEGER confac_2derivs2
      INTEGER coordinates0
      INTEGER coordinates1
      INTEGER coordinates2
      INTEGER curv0
      INTEGER curv1
      INTEGER curv2
      INTEGER lapse0
      INTEGER lapse1
      INTEGER lapse2
      INTEGER metric0
      INTEGER metric1
      INTEGER metric2
      INTEGER shift0
      INTEGER shift1
      INTEGER shift2
      INTEGER*4   active_slicing_handle
      PRECISION,ALLOCATABLE::   alp(:,:,:)
      PRECISION,ALLOCATABLE::   betax(:,:,:)
      PRECISION,ALLOCATABLE::   betay(:,:,:)
      PRECISION,ALLOCATABLE::   betaz(:,:,:)
      PRECISION   coarse_dx
      PRECISION   coarse_dy
      PRECISION   coarse_dz
      INTEGER*4   conformal_state
      PRECISION,ALLOCATABLE::   gxx(:,:,:)
      PRECISION,ALLOCATABLE::   gxy(:,:,:)
      PRECISION,ALLOCATABLE::   gxz(:,:,:)
      PRECISION,ALLOCATABLE::   gyy(:,:,:)
      PRECISION,ALLOCATABLE::   gyz(:,:,:)
      PRECISION,ALLOCATABLE::   gzz(:,:,:)
      PRECISION,ALLOCATABLE::   kxx(:,:,:)
      PRECISION,ALLOCATABLE::   kxy(:,:,:)
      PRECISION,ALLOCATABLE::   kxz(:,:,:)
      PRECISION,ALLOCATABLE::   kyy(:,:,:)
      PRECISION,ALLOCATABLE::   kyz(:,:,:)
      PRECISION,ALLOCATABLE::   kzz(:,:,:)
      PRECISION,ALLOCATABLE::   psi(:,:,:)
      PRECISION,ALLOCATABLE::   psix(:,:,:)
      PRECISION,ALLOCATABLE::   psixx(:,:,:)
      PRECISION,ALLOCATABLE::   psixy(:,:,:)
      PRECISION,ALLOCATABLE::   psixz(:,:,:)
      PRECISION,ALLOCATABLE::   psiy(:,:,:)
      PRECISION,ALLOCATABLE::   psiyy(:,:,:)
      PRECISION,ALLOCATABLE::   psiyz(:,:,:)
      PRECISION,ALLOCATABLE::   psiz(:,:,:)
      PRECISION,ALLOCATABLE::   psizz(:,:,:)
      PRECISION,ALLOCATABLE::   r(:,:,:)
      INTEGER*4   shift_state
      PRECISION,ALLOCATABLE::   x(:,:,:)
      PRECISION,ALLOCATABLE::   y(:,:,:)
      PRECISION,ALLOCATABLE::   z(:,:,:)

      integer,parameter :: xx=1
      integer,parameter :: xy=2
      integer,parameter :: xz=3
      integer,parameter :: yy=4
      integer,parameter :: yz=5
      integer,parameter :: zz=6

c     ==================================================================
c     ALLOCATE ARRAYS PASSED INTO CACTUS ROUTINE

      ADM_metric_prev0=dimx
      ADM_metric_prev1=dimy
      ADM_metric_prev2=dimz
      ALLOCATE(ADM_g(6,ADM_metric_prev0,ADM_metric_prev1,ADM_metric_p
     &rev2))
      ALLOCATE(ADM_g_n(6,ADM_metric_prev0,ADM_metric_prev1,ADM_metric_p
     &rev2))

      ADM_curv_prev0=dimx
      ADM_curv_prev1=dimy
      ADM_curv_prev2=dimz
      ADM_curv_stag0=dimx
      ADM_curv_stag1=dimy
      ADM_curv_stag2=dimz
      ALLOCATE(ADM_k_stag_p(6,ADM_curv_prev0,ADM_curv_prev1,
     &ADM_curv_prev2))
      ALLOCATE(ADM_k_stag(6,ADM_curv_stag0,ADM_curv_stag1,ADM_curv_stag
     &2))
      ALLOCATE(ADM_k_stag_n(6,ADM_curv_prev0,ADM_curv_prev1,
     &ADM_curv_prev2))

      ADM_sources0=empty
      ADM_sources1=empty
      ADM_sources2=empty
      ALLOCATE(adms_alp(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_gxx(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_gxy(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_gxz(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_gyy(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_gyz(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_gzz(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_kxx(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_kxy(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_kxz(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_kyy(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_kyz(ADM_sources0,ADM_sources1,ADM_sources2))
      ALLOCATE(adms_kzz(ADM_sources0,ADM_sources1,ADM_sources2))

      lapse0=dimx
      lapse1=dimy
      lapse2=dimz
      ALLOCATE(alp(lapse0,lapse1,lapse2))

      shift0=empty
      shift1=empty
      shift2=empty
      ALLOCATE(betax(shift0,shift1,shift2))
      ALLOCATE(betay(shift0,shift1,shift2))
      ALLOCATE(betaz(shift0,shift1,shift2))

      metric0=empty
      metric1=empty
      metric2=empty
      ALLOCATE(gxx(metric0,metric1,metric2))
      ALLOCATE(gxy(metric0,metric1,metric2))
      ALLOCATE(gxz(metric0,metric1,metric2))
      ALLOCATE(gyy(metric0,metric1,metric2))
      ALLOCATE(gyz(metric0,metric1,metric2))
      ALLOCATE(gzz(metric0,metric1,metric2))

      curv0=empty
      curv1=empty
      curv2=empty
      ALLOCATE(kxx(curv0,curv1,curv2))
      ALLOCATE(kxy(curv0,curv1,curv2))
      ALLOCATE(kxz(curv0,curv1,curv2))
      ALLOCATE(kyy(curv0,curv1,curv2))
      ALLOCATE(kyz(curv0,curv1,curv2))
      ALLOCATE(kzz(curv0,curv1,curv2))

      confac0=empty
      confac1=empty
      confac2=empty
      ALLOCATE(psi(confac0,confac1,confac2))
      
      confac_1derivs0=empty
      confac_1derivs1=empty
      confac_1derivs2=empty
      confac_2derivs0=empty
      confac_2derivs1=empty
      confac_2derivs2=empty
      ALLOCATE(psix(confac_1derivs0,confac_1derivs1,confac_1derivs2))
      ALLOCATE(psixx(confac_2derivs0,confac_2derivs1,confac_2derivs2))
      ALLOCATE(psixy(confac_2derivs0,confac_2derivs1,confac_2derivs2))
      ALLOCATE(psixz(confac_2derivs0,confac_2derivs1,confac_2derivs2))
      ALLOCATE(psiy(confac_1derivs0,confac_1derivs1,confac_1derivs2))
      ALLOCATE(psiyy(confac_2derivs0,confac_2derivs1,confac_2derivs2))
      ALLOCATE(psiyz(confac_2derivs0,confac_2derivs1,confac_2derivs2))
      ALLOCATE(psiz(confac_1derivs0,confac_1derivs1,confac_1derivs2))
      ALLOCATE(psizz(confac_2derivs0,confac_2derivs1,confac_2derivs2))

      coordinates0=empty
      coordinates1=empty
      coordinates2=empty
      ALLOCATE(r(coordinates0,coordinates1,coordinates2))
      ALLOCATE(x(coordinates0,coordinates1,coordinates2))
      ALLOCATE(y(coordinates0,coordinates1,coordinates2))
      ALLOCATE(z(coordinates0,coordinates1,coordinates2))


c     ==================================================================
c     FAKE INITIAL VALUES OF PASSED IN VARIABLES

c      print *, "startup"
 
      cctk_delta_time=0.1d0

      cctk_lsh(1) = dimx
      cctk_lsh(2) = dimy
      cctk_lsh(3) = dimz
      
      do i=1,3 
       cctk_delta_space(i)=1.0
      enddo
 
      conformal_state=0
       
       do k=1,dimz
         do j=1,dimy
            do i=1,dimx
               ADM_g(xx,i,j,k)=1.0d0
               ADM_g(xy,i,j,k)=0.0d0
               ADM_g(xz,i,j,k)=0.0d0 
               ADM_g(yy,i,j,k)=1.0d0 
               ADM_g(yz,i,j,k)=0.0d0 
               ADM_g(zz,i,j,k)=1.0d0 
               alp(i,j,k)=1.0d0 
               ADM_k_stag(xx,i,j,k)=0.1d0
               ADM_k_stag(xy,i,j,k)=0.0d0 
               ADM_k_stag(xz,i,j,k)=0.0d0 
               ADM_k_stag(yy,i,j,k)=0.0d0 
               ADM_k_stag(yz,i,j,k)=0.0d0 
               ADM_k_stag(zz,i,j,k)=0.0d0
               ADM_k_stag_p(xx,i,j,k)=0.0d0
               ADM_k_stag_p(xy,i,j,k)=0.0d0 
               ADM_k_stag_p(xz,i,j,k)=0.0d0 
               ADM_k_stag_p(yy,i,j,k)=0.0d0 
               ADM_k_stag_p(yz,i,j,k)=0.0d0 
               ADM_k_stag_p(zz,i,j,k)=0.0d0
            enddo
         enddo
      enddo


c     ==================================================================
c     CALL CACTUS ROUTINE

      do i=1,niterations
c        write(*,*) 'iteration=',i 
         call Bench_StaggeredLeapfrog2(
     &   cctk_delta_time,   cctk_delta_space,
     &   ADM_metric_prev0,  ADM_metric_prev1,  ADM_metric_prev2,
     &   ADM_curv_stag0,    ADM_curv_stag1,    ADM_curv_stag2,
     &   lapse0,            lapse1,            lapse2,
     &   ADM_g_n,           ADM_g,
     &   ADM_k_stag_n,      ADM_k_stag,        ADM_k_stag_p,
     &   alp)

      enddo

c     ==================================================================
c     CHECK RESULTS

c      print *,"Value after 2000 iterations with 40^3 "
c      print *,"       should be: 8.376903698980404E-002"
c      print *,"Got: ",sum(ADM_k_stag_n(1,1:dimx,1:dimy,1:dimz))/
c     &     real(cctk_lsh(1)*cctk_lsh(2)*cctk_lsh(3))



      end


