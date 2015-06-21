      subroutine Bench_Preloop( cctk_dim, cctk_gsh, cctk_lsh, cctk_lbnd,
     &                          cctk_ubnd, cctk_lssh, cctk_from, cctk_to
     &, cctk_bbox,                          cctk_delta_time, cctk_time, 
     &cctk_delta_space,                          cctk_origin_space, cctk
     &_levfac, cctk_convlevel,                          cctk_nghostzones
     &, cctk_iteration, cctkGH,XADM_curv_stag0,XADM_curv_stag1,XADM_curv
     &_stag2,XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2,XADM_
     &sources0,XADM_sources1,XADM_sources2,ADM_gxx,ADM_gxx_p,ADM_gxy,ADM
     &_gxy_p,ADM_gxz,ADM_gxz_p,ADM_gyy,ADM_gyy_p,ADM_gyz,ADM_gyz_p,ADM_g
     &zz,ADM_gzz_p,ADM_kxx_stag,ADM_kxx_stag_p,ADM_kxx_stag_p_p,ADM_kxy_
     &stag,ADM_kxy_stag_p,ADM_kxy_stag_p_p,ADM_kxz_stag,ADM_kxz_stag_p,A
     &DM_kxz_stag_p_p,ADM_kyy_stag,ADM_kyy_stag_p,ADM_kyy_stag_p_p,ADM_k
     &yz_stag,ADM_kyz_stag_p,ADM_kyz_stag_p_p,ADM_kzz_stag,ADM_kzz_stag_
     &p,ADM_kzz_stag_p_p,adms_gxx,adms_gxy,adms_gxz,adms_gyy,adms_gyz,ad
     &ms_gzz,adms_kxx,adms_kxy,adms_kxz,adms_kyy,adms_kyz,adms_kzz,Xconf
     &ac0,Xconfac1,Xconfac2,Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1d
     &erivs2,Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2,Xcoordin
     &ates0,Xcoordinates1,Xcoordinates2,Xcurv0,Xcurv1,Xcurv2,Xlapse0,Xla
     &pse1,Xlapse2,Xmask0,Xmask1,Xmask2,Xmetric0,Xmetric1,Xmetric2,Xshif
     &t0,Xshift1,Xshift2,active_slicing_handle,alp,betax,betay,betaz,coa
     &rse_dx,coarse_dy,coarse_dz,conformal_state,courant_min_time,couran
     &t_wave_speed,emask,gxx,gxy,gxz,gyy,gyz,gzz,kxx,kxy,kxz,kyy,kyz,kzz
     &,psi,psix,psixx,psixy,psixz,psiy,psiyy,psiyz,psiz,psizz,r,shift_st
     &ate,x,y,z)
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
      INTEGER XADM_curv_stag0
      INTEGER XADM_curv_stag1
      INTEGER XADM_curv_stag2
      INTEGER XADM_metric_prev0
      INTEGER XADM_metric_prev1
      INTEGER XADM_metric_prev2
      INTEGER XADM_sources0
      INTEGER XADM_sources1
      INTEGER XADM_sources2
      REAL*8 ADM_gxx(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_pre
     &v2)
      REAL*8 ADM_gxx_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_p
     &rev2)
      REAL*8 ADM_gxy(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_pre
     &v2)
      REAL*8 ADM_gxy_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_p
     &rev2)
      REAL*8 ADM_gxz(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_pre
     &v2)
      REAL*8 ADM_gxz_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_p
     &rev2)
      REAL*8 ADM_gyy(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_pre
     &v2)
      REAL*8 ADM_gyy_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_p
     &rev2)
      REAL*8 ADM_gyz(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_pre
     &v2)
      REAL*8 ADM_gyz_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_p
     &rev2)
      REAL*8 ADM_gzz(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_pre
     &v2)
      REAL*8 ADM_gzz_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_p
     &rev2)
      REAL*8 ADM_kxx_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag
     &2)
      REAL*8 ADM_kxx_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_st
     &ag2)
      REAL*8 ADM_kxx_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_
     &stag2)
      REAL*8 ADM_kxy_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag
     &2)
      REAL*8 ADM_kxy_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_st
     &ag2)
      REAL*8 ADM_kxy_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_
     &stag2)
      REAL*8 ADM_kxz_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag
     &2)
      REAL*8 ADM_kxz_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_st
     &ag2)
      REAL*8 ADM_kxz_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_
     &stag2)
      REAL*8 ADM_kyy_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag
     &2)
      REAL*8 ADM_kyy_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_st
     &ag2)
      REAL*8 ADM_kyy_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_
     &stag2)
      REAL*8 ADM_kyz_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag
     &2)
      REAL*8 ADM_kyz_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_st
     &ag2)
      REAL*8 ADM_kyz_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_
     &stag2)
      REAL*8 ADM_kzz_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag
     &2)
      REAL*8 ADM_kzz_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_st
     &ag2)
      REAL*8 ADM_kzz_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_
     &stag2)
      REAL*8 adms_gxx(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_gxy(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_gxz(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_gyy(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_gyz(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_gzz(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_kxx(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_kxy(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_kxz(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_kyy(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_kyz(XADM_sources0,XADM_sources1,XADM_sources2)
      REAL*8 adms_kzz(XADM_sources0,XADM_sources1,XADM_sources2)
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
      
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4  method
#else
      integer*8  method
#endif
      COMMON /benchadmrest/method
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4  bound
#else
      integer*8  bound
#endif
      INTEGER*4 time_symmetric
      COMMON /BenchADMpriv/bound,time_symmetric
      REAL*8  CCTKH2
      REAL*8  CCTKH3
      REAL*8  CCTKH8
      REAL*8  CCTKH11
      REAL*8  CCTKH12
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4  CCTKH0
      integer*4  evolution_system
      integer*4  CCTKH4
      integer*4  CCTKH5
      integer*4  CCTKH6
      integer*4  CCTKH7
      integer*4  CCTKH14
      integer*4  slicing
      integer*4  CCTKH15
#else
      integer*8  CCTKH0
      integer*8  evolution_system
      integer*8  CCTKH4
      integer*8  CCTKH5
      integer*8  CCTKH6
      integer*8  CCTKH7
      integer*8  CCTKH14
      integer*8  slicing
      integer*8  CCTKH15
#endif
      INTEGER*4 CCTKH1
      INTEGER*4 CCTKH9
      INTEGER*4 CCTKH10
      INTEGER*4 CCTKH13
      INTEGER*4 CCTKH16
      INTEGER*4 CCTKH17
      INTEGER*4 CCTKH18
      COMMON /EINSTEINrest/CCTKH2,CCTKH3,CCTKH8,CCTKH11,CCTKH12,CCTKH0,e
     &volution_system,CCTKH4,CCTKH5,CCTKH6,CCTKH7,CCTKH14,slicing,CCTKH1
     &5,CCTKH1,CCTKH9,CCTKH10,CCTKH13,CCTKH16,CCTKH17,CCTKH18
#if !defined(SPEC_CPU_LP64) && !defined(SPEC_CPU_P64)
      integer*4  timestep_method
#else
      integer*8  timestep_method
#endif
      INTEGER*4 timestep_outonly
      COMMON /TIMErest/timestep_method,timestep_outonly
      
      INTEGER Einstein_GetSlicingHandle
      CHARACTER*200 :: infoline
      CHARACTER*200 :: fortslicing
      INTEGER*4      :: nslicing
      call CCTK_FortranString(nslicing,slicing,fortslicing)
#ifndef SPEC_CPU
      if (active_slicing_handle .eq. Einstein_GetSlicingHandle("geodesic
     &")) then
         call  CCTK_Info("BenchADM",("Geodesic slicing (lapse set to uni
     &ty)"))
         call  CCTK_Info("BenchADM",("  lapse will not be evolved in ADM
     & routines"))
      else
         call  CCTK_Warn(1,74,"PreLoop.F","BenchADM","WARNING slicing re
     &gistered but not coded!")
      end if
#endif
      return
      end
