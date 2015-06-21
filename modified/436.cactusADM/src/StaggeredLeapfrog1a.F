      subroutine Bench_StaggeredLeapfrog1a( cctk_dim, cctk_gsh, cctk_lsh
     &, cctk_lbnd,                          cctk_ubnd, cctk_lssh, cctk_f
     &rom, cctk_to, cctk_bbox,                          cctk_delta_time,
     & cctk_time, cctk_delta_space,                          cctk_origin
     &_space, cctk_levfac, cctk_convlevel,                          cctk
     &_nghostzones, cctk_iteration, cctkGH,XADM_curv_stag0,XADM_curv_sta
     &g1,XADM_curv_stag2,XADM_metric_prev0,XADM_metric_prev1,XADM_metric
     &_prev2,XADM_sources0,XADM_sources1,XADM_sources2,ADM_gxx,ADM_gxx_p
     &,ADM_gxy,ADM_gxy_p,ADM_gxz,ADM_gxz_p,ADM_gyy,ADM_gyy_p,ADM_gyz,ADM
     &_gyz_p,ADM_gzz,ADM_gzz_p,ADM_kxx_stag,ADM_kxx_stag_p,ADM_kxx_stag_
     &p_p,ADM_kxy_stag,ADM_kxy_stag_p,ADM_kxy_stag_p_p,ADM_kxz_stag,ADM_
     &kxz_stag_p,ADM_kxz_stag_p_p,ADM_kyy_stag,ADM_kyy_stag_p,ADM_kyy_st
     &ag_p_p,ADM_kyz_stag,ADM_kyz_stag_p,ADM_kyz_stag_p_p,ADM_kzz_stag,A
     &DM_kzz_stag_p,ADM_kzz_stag_p_p,adms_gxx,adms_gxy,adms_gxz,adms_gyy
     &,adms_gyz,adms_gzz,adms_kxx,adms_kxy,adms_kxz,adms_kyy,adms_kyz,ad
     &ms_kzz,Xconfac0,Xconfac1,Xconfac2,Xconfac_1derivs0,Xconfac_1derivs
     &1,Xconfac_1derivs2,Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2deri
     &vs2,Xcoordinates0,Xcoordinates1,Xcoordinates2,Xcurv0,Xcurv1,Xcurv2
     &,Xlapse0,Xlapse1,Xlapse2,Xmask0,Xmask1,Xmask2,Xmetric0,Xmetric1,Xm
     &etric2,Xshift0,Xshift1,Xshift2,active_slicing_handle,alp,betax,bet
     &ay,betaz,coarse_dx,coarse_dy,coarse_dz,conformal_state,courant_min
     &_time,courant_wave_speed,emask,gxx,gxy,gxz,gyy,gyz,gzz,kxx,kxy,kxz
     &,kyy,kyz,kzz,psi,psix,psixx,psixy,psixz,psiy,psiyy,psiyz,psiz,psiz
     &z,r,shift_state,x,y,z)
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
      
      REAL*8   dxdb_oo2dx 
      REAL*8   dxdb_dxdbx 
      REAL*8   dxdb_dxdby 
      REAL*8   dxdb_dxdbz 
      REAL*8   dydb_oo2dy 
      REAL*8   dydb_dydbx 
      REAL*8   dydb_dydby 
      REAL*8   dydb_dydbz 
      REAL*8   dzdb_oo2dz
      REAL*8   dzdb_dzdbx 
      REAL*8   dzdb_dzdby 
      REAL*8   dzdb_dzdbz 
      REAL*8   cdxdg_oo2dx 
      REAL*8   delgb111 
      REAL*8   delgb112 
      REAL*8   delgb113 
      REAL*8   delgb122 
      REAL*8   delgb123 
      REAL*8   delgb133 
      REAL*8   dxdg_psi4
      REAL*8   dxdg_fac
      REAL*8   dxdgxx
      REAL*8   dxdgxy
      REAL*8   dxdgxz
      REAL*8   dxdgyy
      REAL*8   dxdgyz
      REAL*8   dxdgzz
      REAL*8   cdydg_oo2dy 
      REAL*8   delgb211 
      REAL*8   delgb212 
      REAL*8   delgb213 
      REAL*8   delgb222 
      REAL*8   delgb223 
      REAL*8   delgb233 
      REAL*8   dydg_psi4
      REAL*8   dydg_fac
      REAL*8   dydgxx
      REAL*8   dydgxy
      REAL*8   dydgxz
      REAL*8   dydgyy
      REAL*8   dydgyz
      REAL*8   dydgzz
      REAL*8   cdzdg_oo2dz 
      REAL*8   delgb311 
      REAL*8   delgb312 
      REAL*8   delgb313 
      REAL*8   delgb322 
      REAL*8   delgb323 
      REAL*8   delgb333 
      REAL*8   dzdg_psi4
      REAL*8   dzdg_fac
      REAL*8   dzdgxx
      REAL*8   dzdgxy
      REAL*8   dzdgxz
      REAL*8   dzdgyy
      REAL*8   dzdgyz
      REAL*8   dzdgzz
      REAL*8   lieg_lgxx 
      REAL*8   lieg_lgxy 
      REAL*8   lieg_lgxz 
      REAL*8   lieg_lgyy 
      REAL*8   lieg_lgyz 
      REAL*8   lieg_lgzz 
      REAL*8   cdgdt_ipsi4 
      REAL*8   cdgdt_cdgxxdt 
      REAL*8   cdgdt_cdgxydt 
      REAL*8   cdgdt_cdgxzdt 
      REAL*8   cdgdt_cdgyydt 
      REAL*8   cdgdt_cdgyzdt 
      REAL*8   cdgdt_cdgzzdt 
      REAL*8   :: Ttt,Ttx,Tty,Ttz,Txx,Txy,Txz,Tyy,Tyz,Tzz
      REAL*8   :: det,uxx,uxy,uxz,uyy,uyz,uzz
      REAL*8   detg_psi4 
      REAL*8   detg_tempxx ,detg_tempxy ,detg_tempxz 
      REAL*8   detg_tempyy ,detg_tempyz ,detg_tempzz 
      REAL*8   detg_detg , detg_detcg 
      REAL*8   uppermet_fdet 
      REAL*8   uppermet_uxx , uppermet_uxy , uppermet_uxz 
      REAL*8   uppermet_uyy , uppermet_uyz , uppermet_uzz 
       REAL*8   trt_ialp2 
       REAL*8   trt_trt 
      REAL*8   dxxdg_fac 
      REAL*8   dxxdg_oodx2 
      REAL*8   dxxdgxx 
      REAL*8   dxxdgxy 
      REAL*8   dxxdgxz 
      REAL*8   dxxdgyy 
      REAL*8   dxxdgyz 
      REAL*8   dxxdgzz 
      REAL*8   dxydg_fac 
      REAL*8   dxydg_oo4dxdy  
      REAL*8   deldelg1211 
      REAL*8   deldelg1212 
      REAL*8   deldelg1213 
      REAL*8   deldelg1222 
      REAL*8   deldelg1223 
      REAL*8   deldelg1233 
      REAL*8   dxzdg_fac 
      REAL*8   dxzdg_oo4dxdz  
      REAL*8   deldelg1311 
      REAL*8   deldelg1312 
      REAL*8   deldelg1313 
      REAL*8   deldelg1322 
      REAL*8   deldelg1323 
      REAL*8   deldelg1333 
      REAL*8   dyydg_fac 
      REAL*8   dyydg_oody2 
      REAL*8   dyydgxx 
      REAL*8   dyydgxy 
      REAL*8   dyydgxz 
      REAL*8   dyydgyy 
      REAL*8   dyydgyz 
      REAL*8   dyydgzz 
      REAL*8   dyzdg_fac 
      REAL*8   dyzdg_oo4dydz 
      REAL*8   deldelg2311 
      REAL*8   deldelg2312 
      REAL*8   deldelg2313 
      REAL*8   deldelg2322 
      REAL*8   deldelg2323 
      REAL*8   deldelg2333 
      REAL*8   dzzdg_fac 
      REAL*8   dzzdg_oodz2 
      REAL*8   dzzdgxx 
      REAL*8   dzzdgxy 
      REAL*8   dzzdgxz 
      REAL*8   dzzdgyy 
      REAL*8   dzzdgyz 
      REAL*8   dzzdgzz 
      REAL*8   gammado111 
      REAL*8   gammado112 
      REAL*8   gammado113 
      REAL*8   gammado122 
      REAL*8   gammado123 
      REAL*8   gammado133 
      REAL*8   gammado211 
      REAL*8   gammado212 
      REAL*8   gammado213 
      REAL*8   gammado222 
      REAL*8   gammado223 
      REAL*8   gammado233 
      REAL*8   gammado311 
      REAL*8   gammado312 
      REAL*8   gammado313 
      REAL*8   gammado322 
      REAL*8   gammado323 
      REAL*8   gammado333 
      REAL*8   gamma111 
      REAL*8   gamma112 
      REAL*8   gamma113 
      REAL*8   gamma122 
      REAL*8   gamma123 
      REAL*8   gamma133 
      REAL*8   gamma211 
      REAL*8   gamma212 
      REAL*8   gamma213 
      REAL*8   gamma222 
      REAL*8   gamma223 
      REAL*8   gamma233 
      REAL*8   gamma311 
      REAL*8   gamma312 
      REAL*8   gamma313 
      REAL*8   gamma322 
      REAL*8   gamma323 
      REAL*8   gamma333 
      REAL*8   ricci_R11 
      REAL*8   ricci_R12 
      REAL*8   ricci_R13 
      REAL*8   ricci_R22 
      REAL*8   ricci_R23 
      REAL*8   ricci_R33 
      REAL*8   KK11 
      REAL*8   KK12 
      REAL*8   KK13 
      REAL*8   KK22 
      REAL*8   KK23 
      REAL*8   KK33 
       REAL*8   trk_trK 
      REAL*8   da_oo2dx 
      REAL*8   da_oo2dy 
      REAL*8   da_oo2dz 
      REAL*8   da_dxda 
      REAL*8   da_dyda 
      REAL*8   da_dzda 
      REAL*8   dda_oodx2 
      REAL*8   dda_oody2 
      REAL*8   dda_oodz2 
      REAL*8   dda_oo4dxdy 
      REAL*8   dda_oo4dxdz 
      REAL*8   dda_oo4dydz 
      REAL*8   dda_dxxda 
      REAL*8   dda_dxyda 
      REAL*8   dda_dxzda 
      REAL*8   dda_dyyda 
      REAL*8   dda_dyzda 
      REAL*8   dda_dzzda 
      REAL*8   cdcda_cdxxda 
      REAL*8   cdcda_cdxyda 
      REAL*8   cdcda_cdxzda 
      REAL*8   cdcda_cdyyda 
      REAL*8   cdcda_cdyzda 
      REAL*8   cdcda_cdzzda 
      REAL*8   dxdk_oo2dx 
      REAL*8   dxdk_dxdkxx 
      REAL*8   dxdk_dxdkxy 
      REAL*8   dxdk_dxdkxz 
      REAL*8   dxdk_dxdkyy 
      REAL*8   dxdk_dxdkyz 
      REAL*8   dxdk_dxdkzz 
      REAL*8   dydk_oo2dy 
      REAL*8   dydk_dydkxx 
      REAL*8   dydk_dydkxy 
      REAL*8   dydk_dydkxz 
      REAL*8   dydk_dydkyy 
      REAL*8   dydk_dydkyz 
      REAL*8   dydk_dydkzz 
      REAL*8   dzdk_oo2dz 
      REAL*8   dzdk_dzdkxx 
      REAL*8   dzdk_dzdkxy 
      REAL*8   dzdk_dzdkxz 
      REAL*8   dzdk_dzdkyy 
      REAL*8   dzdk_dzdkyz 
      REAL*8   dzdk_dzdkzz 
      REAL*8   liek_lkxx 
      REAL*8   liek_lkxy 
      REAL*8   liek_lkxz 
      REAL*8   liek_lkyy 
      REAL*8   liek_lkyz 
      REAL*8   liek_lkzz 
      REAL*8   dkdt_dkxxdt 
      REAL*8   dkdt_dkxydt 
      REAL*8   dkdt_dkxzdt 
      REAL*8   dkdt_dkyydt 
      REAL*8   dkdt_dkyzdt 
      REAL*8   dkdt_dkzzdt 
      integer :: i,j,k
      integer :: nx,ny,nz
      integer :: ierror
       REAL*8 :: dx,dy,dz,dt,i2dx,i2dy,i2dz,idx2,idy2,idz2
       REAL*8 :: 
     &     adm_rhsK_metric_xx,adm_rhsK_metric_xy,adm_rhsK_metric_xz,
     &     adm_rhsK_metric_yy,adm_rhsK_metric_yz,adm_rhsK_metric_zz
       REAL*8 dth,ipsi4,pi
      ADMs_gxx = 0.0d0
      ADMs_gxy = 0.0d0
      ADMs_gxz = 0.0d0
      ADMs_gyy = 0.0d0
      ADMs_gyz = 0.0d0
      ADMs_gzz = 0.0d0
      ADMs_kxx = 0.0d0
      ADMs_kxy = 0.0d0
      ADMs_kxz = 0.0d0
      ADMs_kyy = 0.0d0
      ADMs_kyz = 0.0d0
      ADMs_kzz = 0.0d0
      ADM_kxx_stag_p_p = 0.0d0
      ADM_kxy_stag_p_p = 0.0d0
      ADM_kxz_stag_p_p = 0.0d0
      ADM_kyy_stag_p_p = 0.0d0
      ADM_kyz_stag_p_p = 0.0d0
      ADM_kzz_stag_p_p = 0.0d0
      pi = acos(-1.0d0)
      dt = cctk_delta_time
      dx = cctk_delta_space(1)
      dy = cctk_delta_space(2)
      dz = cctk_delta_space(3)
      i2dx = 1d0/(2d0*dx)
      i2dy = 1d0/(2d0*dy)
      i2dz = 1d0/(2d0*dz)
      idx2 = 1d0/(dx*dx)
      idy2 = 1d0/(dy*dy)
      idz2 = 1d0/(dz*dz)
      nx = cctk_lsh(1)
      ny = cctk_lsh(2)
      nz = cctk_lsh(3)
      do k=1,nz
         do j=1,ny
            do i=1,nx
               cdgdt_cdgxxdt  =-2* alp(i,j,k)*kxx(i,j,k) 
               cdgdt_cdgxydt  =-2* alp(i,j,k)*kxy(i,j,k) 
               cdgdt_cdgxzdt  =-2* alp(i,j,k)*kxz(i,j,k) 
               cdgdt_cdgyydt  =-2* alp(i,j,k)*kyy(i,j,k) 
               cdgdt_cdgyzdt  =-2* alp(i,j,k)*kyz(i,j,k) 
               cdgdt_cdgzzdt  =-2* alp(i,j,k)*kzz(i,j,k) 
               adms_gxx(i,j,k) = cdgdt_cdgxxdt 
               adms_gxy(i,j,k) = cdgdt_cdgxydt 
               adms_gxz(i,j,k) = cdgdt_cdgxzdt 
               adms_gyy(i,j,k) = cdgdt_cdgyydt 
               adms_gyz(i,j,k) = cdgdt_cdgyzdt 
               adms_gzz(i,j,k) = cdgdt_cdgzzdt 
            end do
         end do
      end do
      do k=2,nz-1
         do j=2,ny-1
            do i=2,nx-1
               detg_psi4 = 1.0d0
               detg_tempxx =  gyy(i,j,k)*gzz(i,j,k)-gyz(i,j,k)*gyz(i,j,k
     &)
               detg_tempxy =  gxz(i,j,k)*gyz(i,j,k)-gxy(i,j,k)*gzz(i,j,k
     &)
               detg_tempxz  =-gxz(i,j,k)*gyy(i,j,k)+gxy(i,j,k)*gyz(i,j,k
     &) 
               detg_tempyy =  gxx(i,j,k)*gzz(i,j,k)-gxz(i,j,k)*gxz(i,j,k
     &)
               detg_tempyz =  gxy(i,j,k)*gxz(i,j,k)-gxx(i,j,k)*gyz(i,j,k
     &)
               detg_tempzz =  gxx(i,j,k)*gyy(i,j,k)-gxy(i,j,k)*gxy(i,j,k
     &)
               detg_detcg = (detg_tempxx*gxx(i,j,k)+
     &              detg_tempxy*gxy(i,j,k)+detg_tempxz*gxz(i,j,k))
               detg_detg = detg_psi4 **3* detg_detcg 
               det = detg_detcg 
               uppermet_fdet = detg_psi4*detg_detcg 
               uppermet_uxx = detg_tempxx / uppermet_fdet 
               uppermet_uxy = detg_tempxy / uppermet_fdet 
               uppermet_uxz = detg_tempxz / uppermet_fdet 
               uppermet_uyy = detg_tempyy / uppermet_fdet 
               uppermet_uyz = detg_tempyz / uppermet_fdet 
               uppermet_uzz = detg_tempzz / uppermet_fdet 
               uxx = uppermet_uxx ; uxy = uppermet_uxy ; uxz = uppermet_
     &uxz 
               uyy = uppermet_uyy ; uyz = uppermet_uyz ; uzz = uppermet_
     &uzz 
               cdxdg_oo2dx = 1D0/(2D0* dx)
               delgb111 = cdxdg_oo2dx *(gxx(i+1,j,k)-gxx(i-1,j,k))
               delgb112 = cdxdg_oo2dx *(gxy(i+1,j,k)-gxy(i-1,j,k))
               delgb113 = cdxdg_oo2dx *(gxz(i+1,j,k)-gxz(i-1,j,k))
               delgb122 = cdxdg_oo2dx *(gyy(i+1,j,k)-gyy(i-1,j,k))
               delgb123 = cdxdg_oo2dx *(gyz(i+1,j,k)-gyz(i-1,j,k))
               delgb133 = cdxdg_oo2dx *(gzz(i+1,j,k)-gzz(i-1,j,k))
               dxdg_psi4 = 1
               dxdg_fac  = 0
               dxdgxx = delgb111*dxdg_psi4+dxdg_fac*gxx(i,j,k) 
               dxdgxy = delgb112*dxdg_psi4+dxdg_fac*gxy(i,j,k) 
               dxdgxz = delgb113*dxdg_psi4+dxdg_fac*gxz(i,j,k) 
               dxdgyy = delgb122*dxdg_psi4+dxdg_fac*gyy(i,j,k) 
               dxdgyz = delgb123*dxdg_psi4+dxdg_fac*gyz(i,j,k) 
               dxdgzz = delgb133*dxdg_psi4+dxdg_fac*gzz(i,j,k) 
               dxxdg_oodx2 = 1D0/(cctk_delta_space(1)*cctk_delta_space(1
     &))
               dxxdg_fac  = 0
               dxxdgxx = 2* delgb111*dxdg_fac+dxxdg_fac*gxx(i,j,k)+dxdg_
     &
     &              psi4 
     &              *dxxdg_oodx2 *(gxx(i+1,j,k)-2* gxx(i,j,k)+gxx(i-1,j,
     &k))
               dxxdgxy = 2* delgb112*dxdg_fac+dxxdg_fac*gxy(i,j,k)+dxdg_
     &
     &              psi4 
     &              *dxxdg_oodx2 *(gxy(i+1,j,k)-2* gxy(i,j,k)+gxy(i-1,j,
     &k))
               dxxdgxz = 2* delgb113*dxdg_fac+dxxdg_fac*gxz(i,j,k)+dxdg_
     &
     &              psi4 
     &              *dxxdg_oodx2 *(gxz(i+1,j,k)-2* gxz(i,j,k)+gxz(i-1,j,
     &k))
               dxxdgyy = 2* delgb122*dxdg_fac+dxxdg_fac*gyy(i,j,k)+dxdg_
     &
     &              psi4 
     &              *dxxdg_oodx2 *(gyy(i+1,j,k)-2* gyy(i,j,k)+gyy(i-1,j,
     &k))
               dxxdgyz = 2* delgb123*dxdg_fac+dxxdg_fac*gyz(i,j,k)+dxdg_
     &
     &              psi4 
     &              *dxxdg_oodx2 *(gyz(i+1,j,k)-2* gyz(i,j,k)+gyz(i-1,j,
     &k))
               dxxdgzz = 2* delgb133*dxdg_fac+dxxdg_fac*gzz(i,j,k)+dxdg_
     &
     &              psi4 
     &              *dxxdg_oodx2 *(gzz(i+1,j,k)-2* gzz(i,j,k)+gzz(i-1,j,
     &k))
               cdydg_oo2dy = 1D0/(2D0* dy)
               delgb211 = cdydg_oo2dy *(gxx(i,j+1,k)-gxx(i,j-1,k))
               delgb212 = cdydg_oo2dy *(gxy(i,j+1,k)-gxy(i,j-1,k))
               delgb213 = cdydg_oo2dy *(gxz(i,j+1,k)-gxz(i,j-1,k))
               delgb222 = cdydg_oo2dy *(gyy(i,j+1,k)-gyy(i,j-1,k))
               delgb223 = cdydg_oo2dy *(gyz(i,j+1,k)-gyz(i,j-1,k))
               delgb233 = cdydg_oo2dy *(gzz(i,j+1,k)-gzz(i,j-1,k))
               dydg_psi4 = 1
               dydg_fac  = 0
               dydgxx = delgb211*dydg_psi4+dydg_fac*gxx(i,j,k) 
               dydgxy = delgb212*dydg_psi4+dydg_fac*gxy(i,j,k) 
               dydgxz = delgb213*dydg_psi4+dydg_fac*gxz(i,j,k) 
               dydgyy = delgb222*dydg_psi4+dydg_fac*gyy(i,j,k) 
               dydgyz = delgb223*dydg_psi4+dydg_fac*gyz(i,j,k) 
               dydgzz = delgb233*dydg_psi4+dydg_fac*gzz(i,j,k) 
               dxydg_oo4dxdy = 1D0/(4D0*dx*dy)
               dxydg_fac  = 0
               deldelg1211 = delgb211*dxdg_fac+delgb111*dydg_fac+dxydg_f
     &
     &                 ac*gxx(i,j,k) 
     &              + dxdg_psi4*dxydg_oo4dxdy *
     &              (gxx(i+1,j+1,k)-gxx(i+1,j-1,k)-gxx(i-1,j+1,k)+gxx(i-
     &1,j-1,k)
     &              )
               deldelg1212 = delgb212*dxdg_fac+delgb112*dydg_fac+dxydg_f
     &
     &                 ac*gxy(i,j,k) 
     &              + dxdg_psi4*dxydg_oo4dxdy *
     &              (gxy(i+1,j+1,k)-gxy(i+1,j-1,k)-gxy(i-1,j+1,k)+gxy(i-
     &1,j-1,k)
     &              )
               deldelg1213 = delgb213*dxdg_fac+delgb113*dydg_fac+dxydg_f
     &
     &              ac*gxz(i,j,k) 
     &              + dxdg_psi4*dxydg_oo4dxdy *
     &              (gxz(i+1,j+1,k)-gxz(i+1,j-1,k)-gxz(i-1,j+1,k)+gxz(i-
     &1,j-1,k)
     &              )
               deldelg1222 = delgb222*dxdg_fac+delgb122*dydg_fac+dxydg_f
     &
     &              ac*gyy(i,j,k) 
     &                 + dxdg_psi4*dxydg_oo4dxdy *
     &                 (gyy(i+1,j+1,k)-gyy(i+1,j-1,k)-gyy(i-1,j+1,k)+gyy
     &(i-1,j-1,k)
     &                 )
                  deldelg1223 = delgb223*dxdg_fac+delgb123*dydg_fac+dxyd
     &g_f
     &                 ac*gyz(i,j,k) 
     &                 + dxdg_psi4*dxydg_oo4dxdy *
     &                 (gyz(i+1,j+1,k)-gyz(i+1,j-1,k)-gyz(i-1,j+1,k)+gyz
     &(i-1,j-1,k)
     &                 )
                  deldelg1233 = delgb233*dxdg_fac+delgb133*dydg_fac+dxyd
     &g_f
     &                 ac*gzz(i,j,k) 
     &                 + dxdg_psi4*dxydg_oo4dxdy *
     &                 (gzz(i+1,j+1,k)-gzz(i+1,j-1,k)-gzz(i-1,j+1,k)+gzz
     &(i-1,j-1,k)
     &                 )
                  cdzdg_oo2dz = 1D0/(2D0* dz)
                  delgb311 = cdzdg_oo2dz *(gxx(i,j,k+1)-gxx(i,j,k-1))
                  delgb312 = cdzdg_oo2dz *(gxy(i,j,k+1)-gxy(i,j,k-1))
                  delgb313 = cdzdg_oo2dz *(gxz(i,j,k+1)-gxz(i,j,k-1))
                  delgb322 = cdzdg_oo2dz *(gyy(i,j,k+1)-gyy(i,j,k-1))
                  delgb323 = cdzdg_oo2dz *(gyz(i,j,k+1)-gyz(i,j,k-1))
                  delgb333 = cdzdg_oo2dz *(gzz(i,j,k+1)-gzz(i,j,k-1))
                  dzdg_psi4 = 1
                  dzdg_fac  = 0
                  dzdgxx = delgb311*dzdg_psi4+dzdg_fac*gxx(i,j,k) 
                  dzdgxy = delgb312*dzdg_psi4+dzdg_fac*gxy(i,j,k) 
                  dzdgxz = delgb313*dzdg_psi4+dzdg_fac*gxz(i,j,k) 
                  dzdgyy = delgb322*dzdg_psi4+dzdg_fac*gyy(i,j,k) 
                  dzdgyz = delgb323*dzdg_psi4+dzdg_fac*gyz(i,j,k) 
                  dzdgzz = delgb333*dzdg_psi4+dzdg_fac*gzz(i,j,k) 
                  dxzdg_oo4dxdz = 1D0/(4D0*dx*dz)
                  dxzdg_fac  = 0
                  deldelg1311 = delgb311*dxdg_fac+delgb111*dzdg_fac+dxzd
     &g_f
     &                 ac*gxx(i,j,k)+
     &                 dxdg_psi4*dxzdg_oo4dxdz *(gxx(i+1,j,k+1)-gxx(i+1,
     &j,k-1)-gxx
     &                 (i-1,j,k+1)+
     &                 gxx(i-1,j,k-1))
                  deldelg1312 = delgb312*dxdg_fac+delgb112*dzdg_fac+dxzd
     &g_f
     &                 ac*gxy(i,j,k)+
     &                 dxdg_psi4*dxzdg_oo4dxdz *(gxy(i+1,j,k+1)-gxy(i+1,
     &j,k-1)-gxy
     &                 (i-1,j,k+1)+
     &                 gxy(i-1,j,k-1))
                  deldelg1313 = delgb313*dxdg_fac+delgb113*dzdg_fac+dxzd
     &g_f
     &                 ac*gxz(i,j,k)+
     &                 dxdg_psi4*dxzdg_oo4dxdz *(gxz(i+1,j,k+1)-gxz(i+1,
     &j,k-1)-gxz
     &                 (i-1,j,k+1)+
     &                 gxz(i-1,j,k-1))
                  deldelg1322 = delgb322*dxdg_fac+delgb122*dzdg_fac+dxzd
     &g_f
     &                 ac*gyy(i,j,k)+
     &                 dxdg_psi4*dxzdg_oo4dxdz *(gyy(i+1,j,k+1)-gyy(i+1,
     &j,k-1)-gyy
     &                 (i-1,j,k+1)+
     &                 gyy(i-1,j,k-1))
                  deldelg1323 = delgb323*dxdg_fac+delgb123*dzdg_fac+dxzd
     &g_f
     &                 ac*gyz(i,j,k)+
     &                 dxdg_psi4*dxzdg_oo4dxdz *(gyz(i+1,j,k+1)-gyz(i+1,
     &j,k-1)-gyz
     &                 (i-1,j,k+1)+
     &                 gyz(i-1,j,k-1))
                  deldelg1333 = delgb333*dxdg_fac+delgb133*dzdg_fac+dxzd
     &g_f
     &                 ac*gzz(i,j,k)+
     &                 dxdg_psi4*dxzdg_oo4dxdz *(gzz(i+1,j,k+1)-gzz(i+1,
     &j,k-1)-gzz
     &                 (i-1,j,k+1)+
     &                 gzz(i-1,j,k-1))
                  dyydg_oody2 = 1D0/(dy*dy)
                  dyydg_fac  = 0
                  dyydgxx = 2* delgb211*dydg_fac+dyydg_fac*gxx(i,j,k)+dy
     &dg_
     &                 psi4 
     &                 * dyydg_oody2 *(gxx(i,j+1,k)-2* gxx(i,j,k)+gxx(i,
     &j-1,k))
                  dyydgxy = 2* delgb212*dydg_fac+dyydg_fac*gxy(i,j,k)+dy
     &dg_
     &                 psi4 
     &                 * dyydg_oody2 *(gxy(i,j+1,k)-2* gxy(i,j,k)+gxy(i,
     &j-1,k))
                  dyydgxz = 2* delgb213*dydg_fac+dyydg_fac*gxz(i,j,k)+dy
     &dg_
     &                 psi4 
     &                 * dyydg_oody2 *(gxz(i,j+1,k)-2* gxz(i,j,k)+gxz(i,
     &j-1,k))
                  dyydgyy = 2* delgb222*dydg_fac+dyydg_fac*gyy(i,j,k)+dy
     &dg_
     &                 psi4 
     &                 * dyydg_oody2 *(gyy(i,j+1,k)-2* gyy(i,j,k)+gyy(i,
     &j-1,k))
                  dyydgyz = 2* delgb223*dydg_fac+dyydg_fac*gyz(i,j,k)+dy
     &dg_
     &                 psi4 
     &                 * dyydg_oody2 *(gyz(i,j+1,k)-2* gyz(i,j,k)+gyz(i,
     &j-1,k))
                  dyydgzz = 2* delgb233*dydg_fac+dyydg_fac*gzz(i,j,k)+dy
     &dg_
     &                 psi4 
     &                 * dyydg_oody2 *(gzz(i,j+1,k)-2* gzz(i,j,k)+gzz(i,
     &j-1,k))
                  dyzdg_oo4dydz = 1D0/(4D0*dy*dz)
                  dyzdg_fac  = 0
                  deldelg2311 = delgb311*dydg_fac+delgb211*dzdg_fac+dyzd
     &g_f
     &                 ac*gxx(i,j,k) 
     &                 +dydg_psi4*dyzdg_oo4dydz *(gxx(i,j+1,k+1)-gxx(i,j
     &+1,k-1)-
     &                 gxx(i,j-1,k+1) 
     &                 +gxx(i,j-1,k-1))
                  deldelg2312 = delgb312*dydg_fac+delgb212*dzdg_fac+dyzd
     &g_f
     &                 ac*gxy(i,j,k) 
     &                 +dydg_psi4*dyzdg_oo4dydz *(gxy(i,j+1,k+1)-gxy(i,j
     &+1,k-1)-
     &                 gxy(i,j-1,k+1) 
     &                 +gxy(i,j-1,k-1))
                  deldelg2313 = delgb313*dydg_fac+delgb213*dzdg_fac+dyzd
     &g_f
     &                 ac*gxz(i,j,k) 
     &                 +dydg_psi4*dyzdg_oo4dydz *(gxz(i,j+1,k+1)-gxz(i,j
     &+1,k-1)-
     &                 gxz(i,j-1,k+1) 
     &                 +gxz(i,j-1,k-1))
                  deldelg2322 = delgb322*dydg_fac+delgb222*dzdg_fac+dyzd
     &g_f
     &                 ac*gyy(i,j,k) 
     &                 +dydg_psi4*dyzdg_oo4dydz *(gyy(i,j+1,k+1)-gyy(i,j
     &+1,k-1)-
     &                 gyy(i,j-1,k+1) 
     &                 +gyy(i,j-1,k-1))
                  deldelg2323 = delgb323*dydg_fac+delgb223*dzdg_fac+dyzd
     &g_f
     &                 ac*gyz(i,j,k) 
     &                 +dydg_psi4*dyzdg_oo4dydz *(gyz(i,j+1,k+1)-gyz(i,j
     &+1,k-1)-
     &                 gyz(i,j-1,k+1) 
     &                 +gyz(i,j-1,k-1))
                  deldelg2333 = delgb333*dydg_fac+delgb233*dzdg_fac+dyzd
     &g_f
     &                 ac*gzz(i,j,k) 
     &                 +dydg_psi4*dyzdg_oo4dydz *(gzz(i,j+1,k+1)-gzz(i,j
     &+1,k-1)-
     &                 gzz(i,j-1,k+1) 
     &                 +gzz(i,j-1,k-1))
                  dzzdg_oodz2 = 1D0/(dz*dz)
                  dzzdg_fac  = 0
                  dzzdgxx = 2* delgb311*dzdg_fac+dzzdg_fac*gxx(i,j,k)+dz
     &dg_
     &                 psi4 
     &                 * dzzdg_oodz2 *(gxx(i,j,k+1)-2* gxx(i,j,k)+gxx(i,
     &j,k-1))
                  dzzdgxy = 2* delgb312*dzdg_fac+dzzdg_fac*gxy(i,j,k)+dz
     &dg_
     &                 psi4 
     &                 * dzzdg_oodz2 *(gxy(i,j,k+1)-2* gxy(i,j,k)+gxy(i,
     &j,k-1))
                  dzzdgxz = 2* delgb313*dzdg_fac+dzzdg_fac*gxz(i,j,k)+dz
     &dg_
     &                 psi4 
     &                 * dzzdg_oodz2 *(gxz(i,j,k+1)-2* gxz(i,j,k)+gxz(i,
     &j,k-1))
                  dzzdgyy = 2* delgb322*dzdg_fac+dzzdg_fac*gyy(i,j,k)+dz
     &dg_
     &                 psi4 
     &                 * dzzdg_oodz2 *(gyy(i,j,k+1)-2* gyy(i,j,k)+gyy(i,
     &j,k-1))
                  dzzdgyz = 2* delgb323*dzdg_fac+dzzdg_fac*gyz(i,j,k)+dz
     &dg_
     &                 psi4 
     &                 * dzzdg_oodz2 *(gyz(i,j,k+1)-2* gyz(i,j,k)+gyz(i,
     &j,k-1))
                  dzzdgzz = 2* delgb333*dzdg_fac+dzzdg_fac*gzz(i,j,k)+dz
     &dg_
     &                 psi4 
     &                 * dzzdg_oodz2 *(gzz(i,j,k+1)-2* gzz(i,j,k)+gzz(i,
     &j,k-1))
                  gammado111 = dxdgxx /2D0
                  gammado112 = dydgxx /2D0
                  gammado113 = dzdgxx /2D0
                  gammado122  =-dxdgyy /2D0+dydgxy 
                  gammado123 = (- dxdgyz+dydgxz+dzdgxy)/2D0
                  gammado133  =-dxdgzz /2D0+dzdgxz 
                  gammado211 = dxdgxy-dydgxx /2D0
                  gammado212 = dxdgyy /2D0
                  gammado213 = ( dxdgyz-dydgxz+dzdgxy)/2D0
                  gammado222 = dydgyy /2D0
                  gammado223 = dzdgyy /2D0
                  gammado233  =-dydgzz /2D0+dzdgyz 
                  gammado311 =  dxdgxz-dzdgxx /2D0
                  gammado312 = ( dxdgyz+dydgxz-dzdgxy)/2D0
                  gammado313 = dxdgzz/2D0
                  gammado322 = dydgyz-dzdgyy/2D0
                  gammado323 = dydgzz/2D0
                  gammado333 = dzdgzz/2D0
                  gamma111 = gammado111*uppermet_uxx+gammado211*uppermet
     &_uxy
     &                 +gammado311*uppermet_uxz 
                  gamma112 = gammado112*uppermet_uxx+gammado212*uppermet
     &_uxy
     &                 +gammado312*uppermet_uxz 
                  gamma113 = gammado113*uppermet_uxx+gammado213*uppermet
     &_uxy
     &                 +gammado313*uppermet_uxz 
                  gamma122 = gammado122*uppermet_uxx+gammado222*uppermet
     &_uxy
     &                 +gammado322*uppermet_uxz 
                  gamma123 = gammado123*uppermet_uxx+gammado223*uppermet
     &_uxy
     &                 +gammado323*uppermet_uxz 
                  gamma133 = gammado133*uppermet_uxx+gammado233*uppermet
     &_uxy
     &                 +gammado333*uppermet_uxz 
                  gamma211 = gammado111*uppermet_uxy+gammado211*uppermet
     &_uyy
     &                 +gammado311*uppermet_uyz 
                  gamma212 = gammado112*uppermet_uxy+gammado212*uppermet
     &_uyy
     &                 +gammado312*uppermet_uyz 
                  gamma213 = gammado113*uppermet_uxy+gammado213*uppermet
     &_uyy
     &                 +gammado313*uppermet_uyz 
                  gamma222 = gammado122*uppermet_uxy+gammado222*uppermet
     &_uyy
     &                 +gammado322*uppermet_uyz 
                  gamma223 = gammado123*uppermet_uxy+gammado223*uppermet
     &_uyy
     &                 +gammado323*uppermet_uyz 
                  gamma233 = gammado133*uppermet_uxy+gammado233*uppermet
     &_uyy
     &                 +gammado333*uppermet_uyz 
                  gamma311 = gammado111*uppermet_uxz+gammado211*uppermet
     &_uyz
     &                 +gammado311*uppermet_uzz 
                  gamma312 = gammado112*uppermet_uxz+gammado212*uppermet
     &_uyz
     &                 +gammado312*uppermet_uzz 
                  gamma313 = gammado113*uppermet_uxz+gammado213*uppermet
     &_uyz
     &                 +gammado313*uppermet_uzz 
                  gamma322 = gammado122*uppermet_uxz+gammado222*uppermet
     &_uyz
     &                 +gammado322*uppermet_uzz 
                  gamma323 = gammado123*uppermet_uxz+gammado223*uppermet
     &_uyz
     &                 +gammado323*uppermet_uzz 
                  gamma333 = gammado133*uppermet_uxz+gammado233*uppermet
     &_uyz
     &                 +gammado333*uppermet_uzz 
                  ricci_R11 = (deldelg1212+(- dxxdgyy-dyydgxx)/2-
     &                 gammado122*gamma111+gammado112*gamma112-
     &                 gammado222*gamma211+gammado212*gamma212-
     &                 gammado322*gamma311+gammado312*gamma312)* 
     &                 uppermet_uyy+(- dxxdgyz+deldelg1213+
     &                 deldelg1312-deldelg2311-2*(gammado223*gamma211+
     &                 gammado323*gamma311)+2*(-(gammado123*gamma111)+
     &                 gammado113*gamma112+gammado213*gamma212+
     &                 gammado313*gamma312))*uppermet_uyz+(deldelg1313 
     &                 +(- dxxdgzz-dzzdgxx)/2-gammado133*gamma111+
     &                 gammado113*gamma113-gammado233*gamma211+
     &                 gammado213*gamma213-gammado333*gamma311+
     &                 gammado313*gamma313)* uppermet_uzz 
                  ricci_R12 = (- deldelg1212+(dxxdgyy+dyydgxx)/2+
     &                 gammado122*gamma111-gammado112*gamma112+gammado22
     &2*
     &                 gamma211-gammado212*gamma212+gammado322*gamma311-
     &
     &                 gammado312*gamma312)* uppermet_uxy+((dxxdgyz-
     &                 deldelg1213-deldelg1312+deldelg2311)/2+gammado123
     &*
     &                 gamma111-gammado113*gamma112+gammado223*gamma211-
     &
     &                 gammado213*gamma212+gammado323*gamma311-gammado31
     &3*
     &                 gamma312)*uppermet_uxz+((- deldelg1223+deldelg132
     &2+
     &                 dyydgxz-deldelg2312)/2-
     &                 gammado123*gamma112+gammado122*gamma113-gammado22
     &3*ga
     &                 mma212+
     &                 gammado222*gamma213-gammado323*gamma312+
     &                 gammado322*gamma313)* uppermet_uyz+((- deldelg123
     &3+delde
     &                 lg1323+
     &                 deldelg2313-dzzdgxy)/2-gammado133*gamma112+
     &                 gammado123*gamma113-gammado233*gamma212+gammado22
     &3*ga
     &                 mma213 -
     &                 gammado333*gamma312+gammado323*gamma313)* upperme
     &t_uzz
                  ricci_R13 = ((dxxdgyz-deldelg1213-deldelg1312+
     &                 deldelg2311)/2+gammado123*gamma111-gammado113*gam
     &ma112 
     &                 +
     &                 gammado223*gamma211-gammado213*gamma212+gammado32
     &3*ga
     &                 mma311 -
     &                 gammado313*gamma312)* uppermet_uxy+(- deldelg1313
     &+(dxxdg
     &                 zz+
     &                 dzzdgxx)/2+gammado133*gamma111-gammado113*gamma11
     &3+
     &                 gammado233*gamma211-gammado213*gamma213+gammado33
     &3*ga
     &                 mma311 -
     &                 gammado313*gamma313)* uppermet_uxz+((deldelg1223-
     &deldelg
     &                 1322 -
     &                 dyydgxz+deldelg2312)/2+gammado123*gamma112 -
     &                 gammado122*gamma113+gammado223*gamma212-gammado22
     &2*ga
     &                 mma213+
     &                 gammado323*gamma312-gammado322*gamma313)* upperme
     &t_uyy+
     &                 ((deldelg1233 
     &                 -deldelg1323-deldelg2313+dzzdgxy)/2+gammado133*ga
     &mma1
     &                 12 
     &                 -gammado123*gamma113+gammado233*gamma212-gammado2
     &23*
     &                 gamma213+
     &                 gammado333*gamma312-gammado323*gamma313)* upperme
     &t_uyz
                  ricci_R22 = (deldelg1212+(- dxxdgyy-dyydgxx)/2-
     &                 gammado122*gamma111+gammado112*gamma112-gammado22
     &2*ga
     &                 mma211+
     &                 gammado212*gamma212-gammado322*gamma311+
     &                 gammado312*gamma312)* uppermet_uxx+(deldelg1223-d
     &eldelg1
     &                 322 -
     &                 dyydgxz+deldelg2312+2*(gammado123*gamma112 -
     &                 gammado122*gamma113+gammado223*gamma212+gammado32
     &3*ga
     &                 mma312)-
     &                 2*(gammado222*gamma213+gammado322*gamma313))*uppe
     &rmet_u
     &                 xz+
     &                 (deldelg2323+(- dyydgzz-dzzdgyy)/2-
     &                 gammado133*gamma122+gammado123*gamma123-gammado23
     &3*ga
     &                 mma222+
     &                 gammado223*gamma223-gammado333*gamma322+
     &                 gammado323*gamma323)* uppermet_uzz
                  ricci_R23 = ((- dxxdgyz+deldelg1213+deldelg1312 -
     &                 deldelg2311)/2-gammado123*gamma111+gammado113*gam
     &ma112-
     &                 gammado223*gamma211+gammado213*gamma212-gammado32
     &3*ga
     &                 mma311+
     &                 gammado313*gamma312)* uppermet_uxx+((- deldelg122
     &3+delde
     &                 lg1322+
     &                 dyydgxz-deldelg2312)/2-gammado123*gamma112+
     &                 gammado122*gamma113-gammado223*gamma212+gammado22
     &2*ga
     &                 mma213 -
     &                 gammado323*gamma312+gammado322*gamma313)* upperme
     &t_uxy+
     &                 ((deldelg1233 
     &                 -deldelg1323-deldelg2313+dzzdgxy)/2+gammado133*ga
     &mma1
     &                 12 
     &                 -gammado123*gamma113+gammado233*gamma212-gammado2
     &23*
     &                 gamma213+
     &                 gammado333*gamma312-gammado323*gamma313)* upperme
     &t_uxz+
     &                 (- deldelg2323 
     &                 +(dyydgzz+dzzdgyy)/2+gammado133*gamma122 -
     &                 gammado123*gamma123+gammado233*gamma222-gammado22
     &3*ga
     &                 mma223+
     &                 gammado333*gamma322-gammado323*gamma323)* upperme
     &t_uyz
                  ricci_R33 = (deldelg1313+(- dxxdgzz-dzzdgxx)/2-
     &                 gammado133*gamma111+gammado113*gamma113-gammado23
     &3*ga
     &                 mma211+
     &                 gammado213*gamma213-gammado333*gamma311+
     &                 gammado313*gamma313)* uppermet_uxx+(- deldelg1233
     &+deldel
     &                 g1323+
     &                 deldelg2313-dzzdgxy-2*(gammado233*gamma212+
     &                 gammado333*gamma312)+2*(-(gammado133*gamma112)+ga
     &mmado1
     &                 23*gamma113+
     &                 gammado223*gamma213+gammado323*gamma313))* upperm
     &et_uxy  
     &                 + (deldelg2323 
     &                 +(- dyydgzz-dzzdgyy)/2-gammado133*gamma122+
     &                 gammado123*gamma123-gammado233*gamma222+gammado22
     &3*ga
     &                 mma223 -
     &                 gammado333*gamma322+gammado323*gamma323)* upperme
     &t_uyy
                  KK11 = 2*(uppermet_uyz*kxy(i,j,k)*kxz(i,j,k)+kxx(i,j,k
     &) *(
     &                 uppermet_uxy*kxy(i,j,k)+
     &                 uppermet_uxz*kxz(i,j,k)))+uppermet_uxx*kxx(i,j,k)
     &*kxx(i,
     &                 j,k)+
     &                 uppermet_uyy*kxy(i,j,k)*kxy(i,j,k)+uppermet_uzz*k
     &xz(i,j,
     &                 k)*kxz(i,j,k) 
                  KK12 = kxy(i,j,k) *(uppermet_uxx*kxx(i,j,k)+uppermet_u
     &yy*k
     &                 yy(i,j,k))+
     &                 uppermet_uzz*kxz(i,j,k)*kyz(i,j,k)+uppermet_uxz *
     &(kxy(i,j,
     &                 k)*kxz(i,j,k)+
     &                 kxx(i,j,k)*kyz(i,j,k))+uppermet_uyz *(kxz(i,j,k)*
     &kyy(i,j,k
     &                 )+kxy(i,j,k)*kyz(i,j,k))+
     &                 uppermet_uxy *(kxx(i,j,k)*kyy(i,j,k)+kxy(i,j,k)*k
     &xy(i,j,k)
     &                 ) 
                  KK13 = uppermet_uyy*kxy(i,j,k)*kyz(i,j,k)+uppermet_uxy
     & *(k
     &                 xy(i,j,k)*kxz(i,j,k)+
     &                 kxx(i,j,k)*kyz(i,j,k))+kxz(i,j,k) *(uppermet_uxx*
     &kxx(i,j,k
     &                 )+uppermet_uzz*kzz(i,j,k))+
     &                 uppermet_uyz *(kxz(i,j,k)*kyz(i,j,k)+kxy(i,j,k)*k
     &zz(i,j,k)
     &                 )+
     &                 uppermet_uxz *(kxx(i,j,k)*kzz(i,j,k)+kxz(i,j,k)*k
     &xz(i,j,k)
     &                 ) 
                  KK22 = 2*(uppermet_uyz*kyy(i,j,k)*kyz(i,j,k)+kxy(i,j,k
     &) *(
     &                 uppermet_uxy*kyy(i,j,k)+
     &                 uppermet_uxz*kyz(i,j,k)))+uppermet_uxx*kxy(i,j,k)
     &*kxy(i,
     &                 j,k) +
     &                 uppermet_uyy*kyy(i,j,k)*kyy(i,j,k)+uppermet_uzz*k
     &yz(i,j,
     &                 k)*kyz(i,j,k)
                  KK23 = uppermet_uxx*kxy(i,j,k)*kxz(i,j,k)+uppermet_uxy
     & *(k
     &                 xz(i,j,k)*kyy(i,j,k)+
     &                 kxy(i,j,k)*kyz(i,j,k))+kyz(i,j,k) *(uppermet_uyy*
     &kyy(i,j,k
     &                 )+uppermet_uzz*kzz(i,j,k))+
     &                 uppermet_uxz *(kxz(i,j,k)*kyz(i,j,k)+kxy(i,j,k)*k
     &zz(i,j,k)
     &                 )+
     &                 uppermet_uyz *(kyy(i,j,k)*kzz(i,j,k)+kyz(i,j,k)*k
     &yz(i,j,k)
     &                 ) 
                  KK33 = 2*(uppermet_uyz*kyz(i,j,k)*kzz(i,j,k)+kxz(i,j,k
     &) *(
     &                 
     &                 uppermet_uxy*kyz(i,j,k)+
     &                 uppermet_uxz*kzz(i,j,k)))+uppermet_uxx*kxz(i,j,k)
     &*kxz(i,
     &                 j,k)+
     &                 uppermet_uyy*kyz(i,j,k)*kyz(i,j,k)+uppermet_uzz*k
     &zz(i,j,
     &                 
     &                 k)*kzz(i,j,k)  
                  trk_trK = uppermet_uxx*kxx(i,j,k)+uppermet_uyy*kyy(i,j
     &,k) 
     &                 +uppermet_uzz*kzz(i,j,k)+2D0*(uppermet_uxy*kxy(i,
     &j,k
     &                 ) 
     &                 +uppermet_uxz*kxz(i,j,k)+uppermet_uyz*kyz(i,j,k))
     &
                  da_oo2dx = 1D0/(2D0* dx)
                  da_oo2dy = 1D0/(2D0* dy)
                  da_oo2dz = 1D0/(2D0* dz)
                  da_dxda = da_oo2dx *(alp(i+1,j,k)-alp(i-1,j,k))
                  da_dyda = da_oo2dy *(alp(i,j+1,k)-alp(i,j-1,k))
                  da_dzda = da_oo2dz *(alp(i,j,k+1)-alp(i,j,k-1))
                  dda_oodx2 = 1D0/(dx*dx)
                  dda_oody2 = 1D0/(dy*dy)
                  dda_oodz2 = 1D0/(dz*dz)
                  dda_oo4dxdy = 1D0/(4D0* dx*dy)
                  dda_oo4dxdz = 1D0/(4D0* dx*dz)
                  dda_oo4dydz = 1D0/(4D0* dy*dz)
                  dda_dxxda = dda_oodx2 *(alp(i+1,j,k) -2D0* alp(i,j,k)+
     &alp(i
     &                 -1,j,k))
                  dda_dyyda = dda_oody2 *(alp(i,j+1,k) -2D0* alp(i,j,k)+
     &alp(i
     &                 ,j-1,k))
                  dda_dzzda = dda_oodz2 *(alp(i,j,k+1) -2D0* alp(i,j,k)+
     &alp(i
     &                 ,j,k-1))
                  dda_dxyda = dda_oo4dxdy *(alp(i+1,j+1,k)-alp(i+1,j-1,k
     &)-alp(i
     &                 -1,j+1,k)+alp(i-1,j-1,k))
                  dda_dxzda = dda_oo4dxdz *(alp(i+1,j,k+1)-alp(i+1,j,k-1
     &)-alp(i
     &                 -1,j,k+1)+alp(i-1,j,k-1))
                  dda_dyzda = dda_oo4dydz *(alp(i,j+1,k+1)-alp(i,j+1,k-1
     &)-alp(i
     &                 ,j-1,k+1)+alp(i,j-1,k-1))
                  cdcda_cdxxda = (dda_dxxda-gamma111*da_dxda-gamma211*da
     &_dy
     &                 da-gamma311*da_dzda)
                  cdcda_cdxyda = (dda_dxyda-gamma112*da_dxda-gamma212*da
     &_dy
     &                 da-gamma312*da_dzda)
                  cdcda_cdxzda = (dda_dxzda-gamma113*da_dxda-gamma213*da
     &_dy
     &                 da-gamma313*da_dzda)
                  cdcda_cdyyda = (dda_dyyda-gamma122*da_dxda-gamma222*da
     &_dy
     &                 da-gamma322*da_dzda)
                  cdcda_cdyzda = (dda_dyzda-gamma123*da_dxda-gamma223*da
     &_dy
     &                 da-gamma323*da_dzda)
                  cdcda_cdzzda = (dda_dzzda-gamma133*da_dxda-gamma233*da
     &_dy
     &                 da-gamma333*da_dzda)
                  dkdt_dkxxdt = alp(i,j,k) *(ricci_R11-2* KK11+kxx(i,j,k
     &)*trk_
     &                 trK)- cdcda_cdxxda 
                  dkdt_dkxydt = alp(i,j,k) *(ricci_R12-2* KK12+kxy(i,j,k
     &)*trk_
     &                 trK)- cdcda_cdxyda 
                  dkdt_dkxzdt = alp(i,j,k) *(ricci_R13-2* KK13+kxz(i,j,k
     &)*trk_
     &                 trK)- cdcda_cdxzda 
                  dkdt_dkyydt = alp(i,j,k) *(ricci_R22-2* KK22+kyy(i,j,k
     &)*trk_
     &                 trK)- cdcda_cdyyda 
                  dkdt_dkyzdt = alp(i,j,k) *(ricci_R23-2* KK23+kyz(i,j,k
     &)*trk_
     &                 trK)- cdcda_cdyzda 
                  dkdt_dkzzdt = alp(i,j,k) *(ricci_R33-2* KK33+kzz(i,j,k
     &)*trk_
     &                 trK)- cdcda_cdzzda 
               adm_rhsK_metric_xx = dkdt_dkxxdt 
               adm_rhsK_metric_xy = dkdt_dkxydt 
               adm_rhsK_metric_xz = dkdt_dkxzdt  
               adm_rhsK_metric_yy = dkdt_dkyydt 
               adm_rhsK_metric_yz = dkdt_dkyzdt 
               adm_rhsK_metric_zz = dkdt_dkzzdt 
               adms_kxx(i,j,k) = adm_rhsK_metric_xx
               adms_kxy(i,j,k) = adm_rhsK_metric_xy
               adms_kxz(i,j,k) = adm_rhsK_metric_xz
               adms_kyy(i,j,k) = adm_rhsK_metric_yy
               adms_kyz(i,j,k) = adm_rhsK_metric_yz
               adms_kzz(i,j,k) = adm_rhsK_metric_zz
            end do
         end do
      end do
      call CCTK_SyncGroup(ierror,cctkGH,"benchadm::ADM_sources")
      ADM_gxx = gxx
      ADM_gxy = gxy
      ADM_gxz = gxz
      ADM_gyy = gyy
      ADM_gyz = gyz
      ADM_gzz = gzz
      dth = 0.5D0*dt
      ADM_kxx_stag_p = kxx-dth*adms_kxx
      ADM_kxy_stag_p = kxy-dth*adms_kxy
      ADM_kxz_stag_p = kxz-dth*adms_kxz
      ADM_kyy_stag_p = kyy-dth*adms_kyy
      ADM_kyz_stag_p = kyz-dth*adms_kyz
      ADM_kzz_stag_p = kzz-dth*adms_kzz
      ADM_kxx_stag   = kxx+dth*adms_kxx
      ADM_kxy_stag   = kxy+dth*adms_kxy
      ADM_kxz_stag   = kxz+dth*adms_kxz
      ADM_kyy_stag   = kyy+dth*adms_kyy
      ADM_kyz_stag   = kyz+dth*adms_kyz
      ADM_kzz_stag   = kzz+dth*adms_kzz
      end subroutine Bench_StaggeredLeapfrog1a
