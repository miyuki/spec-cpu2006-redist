#ifdef FCODE
#define DECLARE_EINSTEIN_PRIVATE_FARGUMENTS \
INTEGER Xdetofg0&&\
INTEGER Xdetofg1&&\
INTEGER Xdetofg2&&\
INTEGER Xspherical_curv0&&\
INTEGER Xspherical_curv1&&\
INTEGER Xspherical_curv2&&\
INTEGER Xspherical_metric0&&\
INTEGER Xspherical_metric1&&\
INTEGER Xspherical_metric2&&\
INTEGER Xtrace_of_K0&&\
INTEGER Xtrace_of_K1&&\
INTEGER Xtrace_of_K2&&\
CCTK_REAL detg(Xdetofg0,Xdetofg1,Xdetofg2)&&\
CCTK_REAL gpp(Xspherical_metric0,Xspherical_metric1,Xspherical_metric2)&&\
CCTK_REAL gqp(Xspherical_metric0,Xspherical_metric1,Xspherical_metric2)&&\
CCTK_REAL gqq(Xspherical_metric0,Xspherical_metric1,Xspherical_metric2)&&\
CCTK_REAL grp(Xspherical_metric0,Xspherical_metric1,Xspherical_metric2)&&\
CCTK_REAL grq(Xspherical_metric0,Xspherical_metric1,Xspherical_metric2)&&\
CCTK_REAL grr(Xspherical_metric0,Xspherical_metric1,Xspherical_metric2)&&\
CCTK_REAL kpp(Xspherical_curv0,Xspherical_curv1,Xspherical_curv2)&&\
CCTK_REAL kqp(Xspherical_curv0,Xspherical_curv1,Xspherical_curv2)&&\
CCTK_REAL kqq(Xspherical_curv0,Xspherical_curv1,Xspherical_curv2)&&\
CCTK_REAL krp(Xspherical_curv0,Xspherical_curv1,Xspherical_curv2)&&\
CCTK_REAL krq(Xspherical_curv0,Xspherical_curv1,Xspherical_curv2)&&\
CCTK_REAL krr(Xspherical_curv0,Xspherical_curv1,Xspherical_curv2)&&\
CCTK_REAL trK(Xtrace_of_K0,Xtrace_of_K1,Xtrace_of_K2)&&\


#define EINSTEIN_PRIVATE_FARGUMENTS \
Xdetofg0,Xdetofg1,Xdetofg2,Xspherical_curv0,Xspherical_curv1,Xspherical_curv2,Xspherical_metric0,Xspherical_metric1,Xspherical_metric2,Xtrace_of_K0,Xtrace_of_K1,Xtrace_of_K2,detg,gpp,gqp,gqq,grp,grq,grr,kpp,kqp,kqq,krp,krq,krr,trK


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_EINSTEIN_PRIVATE_CARGUMENTS \
CCTK_REAL *detg=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::detg")][0]); \
CCTK_REAL *gpp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::gpp")][0]); \
CCTK_REAL *gqp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::gqp")][0]); \
CCTK_REAL *gqq=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::gqq")][0]); \
CCTK_REAL *grp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::grp")][0]); \
CCTK_REAL *grq=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::grq")][0]); \
CCTK_REAL *grr=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::grr")][0]); \
CCTK_REAL *kpp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::kpp")][0]); \
CCTK_REAL *kqp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::kqp")][0]); \
CCTK_REAL *kqq=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::kqq")][0]); \
CCTK_REAL *krp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::krp")][0]); \
CCTK_REAL *krq=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::krq")][0]); \
CCTK_REAL *krr=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::krr")][0]); \
CCTK_REAL *trK=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("Einstein::trK")][0]); \


#define USE_EINSTEIN_PRIVATE_CARGUMENTS \
cctk_dummy_pointer = &detg; \
cctk_dummy_pointer = &gpp; \
cctk_dummy_pointer = &gqp; \
cctk_dummy_pointer = &gqq; \
cctk_dummy_pointer = &grp; \
cctk_dummy_pointer = &grq; \
cctk_dummy_pointer = &grr; \
cctk_dummy_pointer = &kpp; \
cctk_dummy_pointer = &kqp; \
cctk_dummy_pointer = &kqq; \
cctk_dummy_pointer = &krp; \
cctk_dummy_pointer = &krq; \
cctk_dummy_pointer = &krr; \
cctk_dummy_pointer = &trK; \


#define DECLARE_EINSTEIN_PRIVATE_C2F \
static int CCTKARGNUM_detg = -1; \
static int CCTKGROUPNUM_detofg = -1; \
static int CCTKARGNUM_gpp = -1; \
static int CCTKGROUPNUM_spherical_metric = -1; \
static int CCTKARGNUM_gqp = -1; \
static int CCTKARGNUM_gqq = -1; \
static int CCTKARGNUM_grp = -1; \
static int CCTKARGNUM_grq = -1; \
static int CCTKARGNUM_grr = -1; \
static int CCTKARGNUM_kpp = -1; \
static int CCTKGROUPNUM_spherical_curv = -1; \
static int CCTKARGNUM_kqp = -1; \
static int CCTKARGNUM_kqq = -1; \
static int CCTKARGNUM_krp = -1; \
static int CCTKARGNUM_krq = -1; \
static int CCTKARGNUM_krr = -1; \
static int CCTKARGNUM_trK = -1; \
static int CCTKGROUPNUM_trace_of_K = -1; \


#define INITIALISE_EINSTEIN_PRIVATE_C2F \
if(CCTKARGNUM_detg == -1) CCTKARGNUM_detg = CCTK_VarIndex("Einstein::detg"); \
if(CCTKGROUPNUM_detofg == -1) CCTKGROUPNUM_detofg = CCTK_GroupIndex("Einstein::detofg"); \
if(CCTKARGNUM_gpp == -1) CCTKARGNUM_gpp = CCTK_VarIndex("Einstein::gpp"); \
if(CCTKGROUPNUM_spherical_metric == -1) CCTKGROUPNUM_spherical_metric = CCTK_GroupIndex("Einstein::spherical_metric"); \
if(CCTKARGNUM_gqp == -1) CCTKARGNUM_gqp = CCTK_VarIndex("Einstein::gqp"); \
if(CCTKARGNUM_gqq == -1) CCTKARGNUM_gqq = CCTK_VarIndex("Einstein::gqq"); \
if(CCTKARGNUM_grp == -1) CCTKARGNUM_grp = CCTK_VarIndex("Einstein::grp"); \
if(CCTKARGNUM_grq == -1) CCTKARGNUM_grq = CCTK_VarIndex("Einstein::grq"); \
if(CCTKARGNUM_grr == -1) CCTKARGNUM_grr = CCTK_VarIndex("Einstein::grr"); \
if(CCTKARGNUM_kpp == -1) CCTKARGNUM_kpp = CCTK_VarIndex("Einstein::kpp"); \
if(CCTKGROUPNUM_spherical_curv == -1) CCTKGROUPNUM_spherical_curv = CCTK_GroupIndex("Einstein::spherical_curv"); \
if(CCTKARGNUM_kqp == -1) CCTKARGNUM_kqp = CCTK_VarIndex("Einstein::kqp"); \
if(CCTKARGNUM_kqq == -1) CCTKARGNUM_kqq = CCTK_VarIndex("Einstein::kqq"); \
if(CCTKARGNUM_krp == -1) CCTKARGNUM_krp = CCTK_VarIndex("Einstein::krp"); \
if(CCTKARGNUM_krq == -1) CCTKARGNUM_krq = CCTK_VarIndex("Einstein::krq"); \
if(CCTKARGNUM_krr == -1) CCTKARGNUM_krr = CCTK_VarIndex("Einstein::krr"); \
if(CCTKARGNUM_trK == -1) CCTKARGNUM_trK = CCTK_VarIndex("Einstein::trK"); \
if(CCTKGROUPNUM_trace_of_K == -1) CCTKGROUPNUM_trace_of_K = CCTK_GroupIndex("Einstein::trace_of_K"); \


#define EINSTEIN_PRIVATE_C2F_PROTO \
const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *


#define PASS_EINSTEIN_PRIVATE_C2F(xGH) \
(const int *)(CCTKGROUPNUM_detofg<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "Einstein::detofg"))),(const int *)(CCTKGROUPNUM_detofg<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "Einstein::detofg"))),(const int *)(CCTKGROUPNUM_detofg<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "Einstein::detofg"))),(const int *)(CCTKGROUPNUM_spherical_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "Einstein::spherical_curv"))),(const int *)(CCTKGROUPNUM_spherical_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "Einstein::spherical_curv"))),(const int *)(CCTKGROUPNUM_spherical_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "Einstein::spherical_curv"))),(const int *)(CCTKGROUPNUM_spherical_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "Einstein::spherical_metric"))),(const int *)(CCTKGROUPNUM_spherical_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "Einstein::spherical_metric"))),(const int *)(CCTKGROUPNUM_spherical_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "Einstein::spherical_metric"))),(const int *)(CCTKGROUPNUM_trace_of_K<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "Einstein::trace_of_K"))),(const int *)(CCTKGROUPNUM_trace_of_K<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "Einstein::trace_of_K"))),(const int *)(CCTKGROUPNUM_trace_of_K<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "Einstein::trace_of_K"))),(CCTK_REAL *)(CCTKARGNUM_detg<0 ? NULL : (xGH)->data[CCTKARGNUM_detg][1-1]),(CCTK_REAL *)(CCTKARGNUM_gpp<0 ? NULL : (xGH)->data[CCTKARGNUM_gpp][1-1]),(CCTK_REAL *)(CCTKARGNUM_gqp<0 ? NULL : (xGH)->data[CCTKARGNUM_gqp][1-1]),(CCTK_REAL *)(CCTKARGNUM_gqq<0 ? NULL : (xGH)->data[CCTKARGNUM_gqq][1-1]),(CCTK_REAL *)(CCTKARGNUM_grp<0 ? NULL : (xGH)->data[CCTKARGNUM_grp][1-1]),(CCTK_REAL *)(CCTKARGNUM_grq<0 ? NULL : (xGH)->data[CCTKARGNUM_grq][1-1]),(CCTK_REAL *)(CCTKARGNUM_grr<0 ? NULL : (xGH)->data[CCTKARGNUM_grr][1-1]),(CCTK_REAL *)(CCTKARGNUM_kpp<0 ? NULL : (xGH)->data[CCTKARGNUM_kpp][1-1]),(CCTK_REAL *)(CCTKARGNUM_kqp<0 ? NULL : (xGH)->data[CCTKARGNUM_kqp][1-1]),(CCTK_REAL *)(CCTKARGNUM_kqq<0 ? NULL : (xGH)->data[CCTKARGNUM_kqq][1-1]),(CCTK_REAL *)(CCTKARGNUM_krp<0 ? NULL : (xGH)->data[CCTKARGNUM_krp][1-1]),(CCTK_REAL *)(CCTKARGNUM_krq<0 ? NULL : (xGH)->data[CCTKARGNUM_krq][1-1]),(CCTK_REAL *)(CCTKARGNUM_krr<0 ? NULL : (xGH)->data[CCTKARGNUM_krr][1-1]),(CCTK_REAL *)(CCTKARGNUM_trK<0 ? NULL : (xGH)->data[CCTKARGNUM_trK][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_EINSTEIN_PROTECTED_FARGUMENTS \


#define EINSTEIN_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_EINSTEIN_PROTECTED_CARGUMENTS \


#define USE_EINSTEIN_PROTECTED_CARGUMENTS \


#define DECLARE_EINSTEIN_PROTECTED_C2F \


#define INITIALISE_EINSTEIN_PROTECTED_C2F \


#define EINSTEIN_PROTECTED_C2F_PROTO \



#define PASS_EINSTEIN_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_EINSTEIN_PUBLIC_FARGUMENTS \
INTEGER Xconfac0&&\
INTEGER Xconfac1&&\
INTEGER Xconfac2&&\
INTEGER Xconfac_1derivs0&&\
INTEGER Xconfac_1derivs1&&\
INTEGER Xconfac_1derivs2&&\
INTEGER Xconfac_2derivs0&&\
INTEGER Xconfac_2derivs1&&\
INTEGER Xconfac_2derivs2&&\
INTEGER Xcoordinates0&&\
INTEGER Xcoordinates1&&\
INTEGER Xcoordinates2&&\
INTEGER Xcurv0&&\
INTEGER Xcurv1&&\
INTEGER Xcurv2&&\
INTEGER Xlapse0&&\
INTEGER Xlapse1&&\
INTEGER Xlapse2&&\
INTEGER Xmask0&&\
INTEGER Xmask1&&\
INTEGER Xmask2&&\
INTEGER Xmetric0&&\
INTEGER Xmetric1&&\
INTEGER Xmetric2&&\
INTEGER Xshift0&&\
INTEGER Xshift1&&\
INTEGER Xshift2&&\
CCTK_INT active_slicing_handle&&\
CCTK_REAL alp(Xlapse0,Xlapse1,Xlapse2)&&\
CCTK_REAL betax(Xshift0,Xshift1,Xshift2)&&\
CCTK_REAL betay(Xshift0,Xshift1,Xshift2)&&\
CCTK_REAL betaz(Xshift0,Xshift1,Xshift2)&&\
CCTK_REAL coarse_dx&&\
CCTK_REAL coarse_dy&&\
CCTK_REAL coarse_dz&&\
CCTK_INT conformal_state&&\
CCTK_REAL courant_min_time&&\
CCTK_REAL courant_wave_speed&&\
CCTK_REAL emask(Xmask0,Xmask1,Xmask2)&&\
CCTK_REAL gxx(Xmetric0,Xmetric1,Xmetric2)&&\
CCTK_REAL gxy(Xmetric0,Xmetric1,Xmetric2)&&\
CCTK_REAL gxz(Xmetric0,Xmetric1,Xmetric2)&&\
CCTK_REAL gyy(Xmetric0,Xmetric1,Xmetric2)&&\
CCTK_REAL gyz(Xmetric0,Xmetric1,Xmetric2)&&\
CCTK_REAL gzz(Xmetric0,Xmetric1,Xmetric2)&&\
CCTK_REAL kxx(Xcurv0,Xcurv1,Xcurv2)&&\
CCTK_REAL kxy(Xcurv0,Xcurv1,Xcurv2)&&\
CCTK_REAL kxz(Xcurv0,Xcurv1,Xcurv2)&&\
CCTK_REAL kyy(Xcurv0,Xcurv1,Xcurv2)&&\
CCTK_REAL kyz(Xcurv0,Xcurv1,Xcurv2)&&\
CCTK_REAL kzz(Xcurv0,Xcurv1,Xcurv2)&&\
CCTK_REAL psi(Xconfac0,Xconfac1,Xconfac2)&&\
CCTK_REAL psix(Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2)&&\
CCTK_REAL psixx(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)&&\
CCTK_REAL psixy(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)&&\
CCTK_REAL psixz(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)&&\
CCTK_REAL psiy(Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2)&&\
CCTK_REAL psiyy(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)&&\
CCTK_REAL psiyz(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)&&\
CCTK_REAL psiz(Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2)&&\
CCTK_REAL psizz(Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2)&&\
CCTK_REAL r(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\
CCTK_INT shift_state&&\
CCTK_REAL x(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\
CCTK_REAL y(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\
CCTK_REAL z(Xcoordinates0,Xcoordinates1,Xcoordinates2)&&\


#define EINSTEIN_PUBLIC_FARGUMENTS \
Xconfac0,Xconfac1,Xconfac2,Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2,Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2,Xcoordinates0,Xcoordinates1,Xcoordinates2,Xcurv0,Xcurv1,Xcurv2,Xlapse0,Xlapse1,Xlapse2,Xmask0,Xmask1,Xmask2,Xmetric0,Xmetric1,Xmetric2,Xshift0,Xshift1,Xshift2,active_slicing_handle,alp,betax,betay,betaz,coarse_dx,coarse_dy,coarse_dz,conformal_state,courant_min_time,courant_wave_speed,emask,gxx,gxy,gxz,gyy,gyz,gzz,kxx,kxy,kxz,kyy,kyz,kzz,psi,psix,psixx,psixy,psixz,psiy,psiyy,psiyz,psiz,psizz,r,shift_state,x,y,z


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_EINSTEIN_PUBLIC_CARGUMENTS \
CCTK_INT *active_slicing_handle=(CCTK_INT *)(cctkGH->data[CCTK_VarIndex("einstein::active_slicing_handle")][0]); \
CCTK_REAL *alp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::alp")][0]); \
CCTK_REAL *betax=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::betax")][0]); \
CCTK_REAL *betay=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::betay")][0]); \
CCTK_REAL *betaz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::betaz")][0]); \
CCTK_REAL *coarse_dx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dx")][0]); \
CCTK_REAL *coarse_dy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dy")][0]); \
CCTK_REAL *coarse_dz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dz")][0]); \
CCTK_INT *conformal_state=(CCTK_INT *)(cctkGH->data[CCTK_VarIndex("einstein::conformal_state")][0]); \
CCTK_REAL *courant_min_time=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("TIME::courant_min_time")][0]); \
CCTK_REAL *courant_wave_speed=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("TIME::courant_wave_speed")][0]); \
CCTK_REAL *emask=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::emask")][0]); \
CCTK_REAL *gxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::gxx")][0]); \
CCTK_REAL *gxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::gxy")][0]); \
CCTK_REAL *gxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::gxz")][0]); \
CCTK_REAL *gyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::gyy")][0]); \
CCTK_REAL *gyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::gyz")][0]); \
CCTK_REAL *gzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::gzz")][0]); \
CCTK_REAL *kxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::kxx")][0]); \
CCTK_REAL *kxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::kxy")][0]); \
CCTK_REAL *kxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::kxz")][0]); \
CCTK_REAL *kyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::kyy")][0]); \
CCTK_REAL *kyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::kyz")][0]); \
CCTK_REAL *kzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::kzz")][0]); \
CCTK_REAL *psi=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psi")][0]); \
CCTK_REAL *psix=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psix")][0]); \
CCTK_REAL *psixx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psixx")][0]); \
CCTK_REAL *psixy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psixy")][0]); \
CCTK_REAL *psixz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psixz")][0]); \
CCTK_REAL *psiy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psiy")][0]); \
CCTK_REAL *psiyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psiyy")][0]); \
CCTK_REAL *psiyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psiyz")][0]); \
CCTK_REAL *psiz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psiz")][0]); \
CCTK_REAL *psizz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("einstein::psizz")][0]); \
CCTK_REAL *r=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::r")][0]); \
CCTK_INT *shift_state=(CCTK_INT *)(cctkGH->data[CCTK_VarIndex("einstein::shift_state")][0]); \
CCTK_REAL *x=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::x")][0]); \
CCTK_REAL *y=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::y")][0]); \
CCTK_REAL *z=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::z")][0]); \


#define USE_EINSTEIN_PUBLIC_CARGUMENTS \
cctk_dummy_pointer = &active_slicing_handle; \
cctk_dummy_pointer = &alp; \
cctk_dummy_pointer = &betax; \
cctk_dummy_pointer = &betay; \
cctk_dummy_pointer = &betaz; \
cctk_dummy_pointer = &coarse_dx; \
cctk_dummy_pointer = &coarse_dy; \
cctk_dummy_pointer = &coarse_dz; \
cctk_dummy_pointer = &conformal_state; \
cctk_dummy_pointer = &courant_min_time; \
cctk_dummy_pointer = &courant_wave_speed; \
cctk_dummy_pointer = &emask; \
cctk_dummy_pointer = &gxx; \
cctk_dummy_pointer = &gxy; \
cctk_dummy_pointer = &gxz; \
cctk_dummy_pointer = &gyy; \
cctk_dummy_pointer = &gyz; \
cctk_dummy_pointer = &gzz; \
cctk_dummy_pointer = &kxx; \
cctk_dummy_pointer = &kxy; \
cctk_dummy_pointer = &kxz; \
cctk_dummy_pointer = &kyy; \
cctk_dummy_pointer = &kyz; \
cctk_dummy_pointer = &kzz; \
cctk_dummy_pointer = &psi; \
cctk_dummy_pointer = &psix; \
cctk_dummy_pointer = &psixx; \
cctk_dummy_pointer = &psixy; \
cctk_dummy_pointer = &psixz; \
cctk_dummy_pointer = &psiy; \
cctk_dummy_pointer = &psiyy; \
cctk_dummy_pointer = &psiyz; \
cctk_dummy_pointer = &psiz; \
cctk_dummy_pointer = &psizz; \
cctk_dummy_pointer = &r; \
cctk_dummy_pointer = &shift_state; \
cctk_dummy_pointer = &x; \
cctk_dummy_pointer = &y; \
cctk_dummy_pointer = &z; \


#define DECLARE_EINSTEIN_PUBLIC_C2F \
static int CCTKARGNUM_active_slicing_handle = -1; \
static int CCTKGROUPNUM_slicing_flags = -1; \
static int CCTKARGNUM_alp = -1; \
static int CCTKGROUPNUM_lapse = -1; \
static int CCTKARGNUM_betax = -1; \
static int CCTKGROUPNUM_shift = -1; \
static int CCTKARGNUM_betay = -1; \
static int CCTKARGNUM_betaz = -1; \
static int CCTKARGNUM_coarse_dx = -1; \
static int CCTKGROUPNUM_gridspacings = -1; \
static int CCTKARGNUM_coarse_dy = -1; \
static int CCTKARGNUM_coarse_dz = -1; \
static int CCTKARGNUM_conformal_state = -1; \
static int CCTKGROUPNUM_flags = -1; \
static int CCTKARGNUM_courant_min_time = -1; \
static int CCTKGROUPNUM_speedvars = -1; \
static int CCTKARGNUM_courant_wave_speed = -1; \
static int CCTKARGNUM_emask = -1; \
static int CCTKGROUPNUM_mask = -1; \
static int CCTKARGNUM_gxx = -1; \
static int CCTKGROUPNUM_metric = -1; \
static int CCTKARGNUM_gxy = -1; \
static int CCTKARGNUM_gxz = -1; \
static int CCTKARGNUM_gyy = -1; \
static int CCTKARGNUM_gyz = -1; \
static int CCTKARGNUM_gzz = -1; \
static int CCTKARGNUM_kxx = -1; \
static int CCTKGROUPNUM_curv = -1; \
static int CCTKARGNUM_kxy = -1; \
static int CCTKARGNUM_kxz = -1; \
static int CCTKARGNUM_kyy = -1; \
static int CCTKARGNUM_kyz = -1; \
static int CCTKARGNUM_kzz = -1; \
static int CCTKARGNUM_psi = -1; \
static int CCTKGROUPNUM_confac = -1; \
static int CCTKARGNUM_psix = -1; \
static int CCTKGROUPNUM_confac_1derivs = -1; \
static int CCTKARGNUM_psixx = -1; \
static int CCTKGROUPNUM_confac_2derivs = -1; \
static int CCTKARGNUM_psixy = -1; \
static int CCTKARGNUM_psixz = -1; \
static int CCTKARGNUM_psiy = -1; \
static int CCTKARGNUM_psiyy = -1; \
static int CCTKARGNUM_psiyz = -1; \
static int CCTKARGNUM_psiz = -1; \
static int CCTKARGNUM_psizz = -1; \
static int CCTKARGNUM_r = -1; \
static int CCTKGROUPNUM_coordinates = -1; \
static int CCTKARGNUM_shift_state = -1; \
static int CCTKARGNUM_x = -1; \
static int CCTKARGNUM_y = -1; \
static int CCTKARGNUM_z = -1; \


#define INITIALISE_EINSTEIN_PUBLIC_C2F \
if(CCTKARGNUM_active_slicing_handle == -1) CCTKARGNUM_active_slicing_handle = CCTK_VarIndex("einstein::active_slicing_handle"); \
if(CCTKGROUPNUM_slicing_flags == -1) CCTKGROUPNUM_slicing_flags = CCTK_GroupIndex("einstein::slicing_flags"); \
if(CCTKARGNUM_alp == -1) CCTKARGNUM_alp = CCTK_VarIndex("einstein::alp"); \
if(CCTKGROUPNUM_lapse == -1) CCTKGROUPNUM_lapse = CCTK_GroupIndex("einstein::lapse"); \
if(CCTKARGNUM_betax == -1) CCTKARGNUM_betax = CCTK_VarIndex("einstein::betax"); \
if(CCTKGROUPNUM_shift == -1) CCTKGROUPNUM_shift = CCTK_GroupIndex("einstein::shift"); \
if(CCTKARGNUM_betay == -1) CCTKARGNUM_betay = CCTK_VarIndex("einstein::betay"); \
if(CCTKARGNUM_betaz == -1) CCTKARGNUM_betaz = CCTK_VarIndex("einstein::betaz"); \
if(CCTKARGNUM_coarse_dx == -1) CCTKARGNUM_coarse_dx = CCTK_VarIndex("GRID::coarse_dx"); \
if(CCTKGROUPNUM_gridspacings == -1) CCTKGROUPNUM_gridspacings = CCTK_GroupIndex("GRID::gridspacings"); \
if(CCTKARGNUM_coarse_dy == -1) CCTKARGNUM_coarse_dy = CCTK_VarIndex("GRID::coarse_dy"); \
if(CCTKARGNUM_coarse_dz == -1) CCTKARGNUM_coarse_dz = CCTK_VarIndex("GRID::coarse_dz"); \
if(CCTKARGNUM_conformal_state == -1) CCTKARGNUM_conformal_state = CCTK_VarIndex("einstein::conformal_state"); \
if(CCTKGROUPNUM_flags == -1) CCTKGROUPNUM_flags = CCTK_GroupIndex("einstein::flags"); \
if(CCTKARGNUM_courant_min_time == -1) CCTKARGNUM_courant_min_time = CCTK_VarIndex("TIME::courant_min_time"); \
if(CCTKGROUPNUM_speedvars == -1) CCTKGROUPNUM_speedvars = CCTK_GroupIndex("TIME::speedvars"); \
if(CCTKARGNUM_courant_wave_speed == -1) CCTKARGNUM_courant_wave_speed = CCTK_VarIndex("TIME::courant_wave_speed"); \
if(CCTKARGNUM_emask == -1) CCTKARGNUM_emask = CCTK_VarIndex("einstein::emask"); \
if(CCTKGROUPNUM_mask == -1) CCTKGROUPNUM_mask = CCTK_GroupIndex("einstein::mask"); \
if(CCTKARGNUM_gxx == -1) CCTKARGNUM_gxx = CCTK_VarIndex("einstein::gxx"); \
if(CCTKGROUPNUM_metric == -1) CCTKGROUPNUM_metric = CCTK_GroupIndex("einstein::metric"); \
if(CCTKARGNUM_gxy == -1) CCTKARGNUM_gxy = CCTK_VarIndex("einstein::gxy"); \
if(CCTKARGNUM_gxz == -1) CCTKARGNUM_gxz = CCTK_VarIndex("einstein::gxz"); \
if(CCTKARGNUM_gyy == -1) CCTKARGNUM_gyy = CCTK_VarIndex("einstein::gyy"); \
if(CCTKARGNUM_gyz == -1) CCTKARGNUM_gyz = CCTK_VarIndex("einstein::gyz"); \
if(CCTKARGNUM_gzz == -1) CCTKARGNUM_gzz = CCTK_VarIndex("einstein::gzz"); \
if(CCTKARGNUM_kxx == -1) CCTKARGNUM_kxx = CCTK_VarIndex("einstein::kxx"); \
if(CCTKGROUPNUM_curv == -1) CCTKGROUPNUM_curv = CCTK_GroupIndex("einstein::curv"); \
if(CCTKARGNUM_kxy == -1) CCTKARGNUM_kxy = CCTK_VarIndex("einstein::kxy"); \
if(CCTKARGNUM_kxz == -1) CCTKARGNUM_kxz = CCTK_VarIndex("einstein::kxz"); \
if(CCTKARGNUM_kyy == -1) CCTKARGNUM_kyy = CCTK_VarIndex("einstein::kyy"); \
if(CCTKARGNUM_kyz == -1) CCTKARGNUM_kyz = CCTK_VarIndex("einstein::kyz"); \
if(CCTKARGNUM_kzz == -1) CCTKARGNUM_kzz = CCTK_VarIndex("einstein::kzz"); \
if(CCTKARGNUM_psi == -1) CCTKARGNUM_psi = CCTK_VarIndex("einstein::psi"); \
if(CCTKGROUPNUM_confac == -1) CCTKGROUPNUM_confac = CCTK_GroupIndex("einstein::confac"); \
if(CCTKARGNUM_psix == -1) CCTKARGNUM_psix = CCTK_VarIndex("einstein::psix"); \
if(CCTKGROUPNUM_confac_1derivs == -1) CCTKGROUPNUM_confac_1derivs = CCTK_GroupIndex("einstein::confac_1derivs"); \
if(CCTKARGNUM_psixx == -1) CCTKARGNUM_psixx = CCTK_VarIndex("einstein::psixx"); \
if(CCTKGROUPNUM_confac_2derivs == -1) CCTKGROUPNUM_confac_2derivs = CCTK_GroupIndex("einstein::confac_2derivs"); \
if(CCTKARGNUM_psixy == -1) CCTKARGNUM_psixy = CCTK_VarIndex("einstein::psixy"); \
if(CCTKARGNUM_psixz == -1) CCTKARGNUM_psixz = CCTK_VarIndex("einstein::psixz"); \
if(CCTKARGNUM_psiy == -1) CCTKARGNUM_psiy = CCTK_VarIndex("einstein::psiy"); \
if(CCTKARGNUM_psiyy == -1) CCTKARGNUM_psiyy = CCTK_VarIndex("einstein::psiyy"); \
if(CCTKARGNUM_psiyz == -1) CCTKARGNUM_psiyz = CCTK_VarIndex("einstein::psiyz"); \
if(CCTKARGNUM_psiz == -1) CCTKARGNUM_psiz = CCTK_VarIndex("einstein::psiz"); \
if(CCTKARGNUM_psizz == -1) CCTKARGNUM_psizz = CCTK_VarIndex("einstein::psizz"); \
if(CCTKARGNUM_r == -1) CCTKARGNUM_r = CCTK_VarIndex("GRID::r"); \
if(CCTKGROUPNUM_coordinates == -1) CCTKGROUPNUM_coordinates = CCTK_GroupIndex("GRID::coordinates"); \
if(CCTKARGNUM_shift_state == -1) CCTKARGNUM_shift_state = CCTK_VarIndex("einstein::shift_state"); \
if(CCTKARGNUM_x == -1) CCTKARGNUM_x = CCTK_VarIndex("GRID::x"); \
if(CCTKARGNUM_y == -1) CCTKARGNUM_y = CCTK_VarIndex("GRID::y"); \
if(CCTKARGNUM_z == -1) CCTKARGNUM_z = CCTK_VarIndex("GRID::z"); \


#define EINSTEIN_PUBLIC_C2F_PROTO \
const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,CCTK_INT *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_INT *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_INT *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *


#define PASS_EINSTEIN_PUBLIC_C2F(xGH) \
(const int *)(CCTKGROUPNUM_confac<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::confac"))),(const int *)(CCTKGROUPNUM_confac<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::confac"))),(const int *)(CCTKGROUPNUM_confac<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::confac"))),(const int *)(CCTKGROUPNUM_confac_1derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::confac_1derivs"))),(const int *)(CCTKGROUPNUM_confac_1derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::confac_1derivs"))),(const int *)(CCTKGROUPNUM_confac_1derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::confac_1derivs"))),(const int *)(CCTKGROUPNUM_confac_2derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::confac_2derivs"))),(const int *)(CCTKGROUPNUM_confac_2derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::confac_2derivs"))),(const int *)(CCTKGROUPNUM_confac_2derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::confac_2derivs"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::curv"))),(const int *)(CCTKGROUPNUM_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::curv"))),(const int *)(CCTKGROUPNUM_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::curv"))),(const int *)(CCTKGROUPNUM_lapse<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::lapse"))),(const int *)(CCTKGROUPNUM_lapse<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::lapse"))),(const int *)(CCTKGROUPNUM_lapse<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::lapse"))),(const int *)(CCTKGROUPNUM_mask<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::mask"))),(const int *)(CCTKGROUPNUM_mask<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::mask"))),(const int *)(CCTKGROUPNUM_mask<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::mask"))),(const int *)(CCTKGROUPNUM_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::metric"))),(const int *)(CCTKGROUPNUM_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::metric"))),(const int *)(CCTKGROUPNUM_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::metric"))),(const int *)(CCTKGROUPNUM_shift<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "einstein::shift"))),(const int *)(CCTKGROUPNUM_shift<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "einstein::shift"))),(const int *)(CCTKGROUPNUM_shift<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "einstein::shift"))),(CCTK_INT *)(CCTKARGNUM_active_slicing_handle<0 ? NULL : (xGH)->data[CCTKARGNUM_active_slicing_handle][1-1]),(CCTK_REAL *)(CCTKARGNUM_alp<0 ? NULL : (xGH)->data[CCTKARGNUM_alp][1-1]),(CCTK_REAL *)(CCTKARGNUM_betax<0 ? NULL : (xGH)->data[CCTKARGNUM_betax][1-1]),(CCTK_REAL *)(CCTKARGNUM_betay<0 ? NULL : (xGH)->data[CCTKARGNUM_betay][1-1]),(CCTK_REAL *)(CCTKARGNUM_betaz<0 ? NULL : (xGH)->data[CCTKARGNUM_betaz][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dx<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dx][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dy<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dy][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dz<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dz][1-1]),(CCTK_INT *)(CCTKARGNUM_conformal_state<0 ? NULL : (xGH)->data[CCTKARGNUM_conformal_state][1-1]),(CCTK_REAL *)(CCTKARGNUM_courant_min_time<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_min_time][1-1]),(CCTK_REAL *)(CCTKARGNUM_courant_wave_speed<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_wave_speed][1-1]),(CCTK_REAL *)(CCTKARGNUM_emask<0 ? NULL : (xGH)->data[CCTKARGNUM_emask][1-1]),(CCTK_REAL *)(CCTKARGNUM_gxx<0 ? NULL : (xGH)->data[CCTKARGNUM_gxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_gxy<0 ? NULL : (xGH)->data[CCTKARGNUM_gxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_gxz<0 ? NULL : (xGH)->data[CCTKARGNUM_gxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_gyy<0 ? NULL : (xGH)->data[CCTKARGNUM_gyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_gyz<0 ? NULL : (xGH)->data[CCTKARGNUM_gyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_gzz<0 ? NULL : (xGH)->data[CCTKARGNUM_gzz][1-1]),(CCTK_REAL *)(CCTKARGNUM_kxx<0 ? NULL : (xGH)->data[CCTKARGNUM_kxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_kxy<0 ? NULL : (xGH)->data[CCTKARGNUM_kxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_kxz<0 ? NULL : (xGH)->data[CCTKARGNUM_kxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_kyy<0 ? NULL : (xGH)->data[CCTKARGNUM_kyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_kyz<0 ? NULL : (xGH)->data[CCTKARGNUM_kyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_kzz<0 ? NULL : (xGH)->data[CCTKARGNUM_kzz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psi<0 ? NULL : (xGH)->data[CCTKARGNUM_psi][1-1]),(CCTK_REAL *)(CCTKARGNUM_psix<0 ? NULL : (xGH)->data[CCTKARGNUM_psix][1-1]),(CCTK_REAL *)(CCTKARGNUM_psixx<0 ? NULL : (xGH)->data[CCTKARGNUM_psixx][1-1]),(CCTK_REAL *)(CCTKARGNUM_psixy<0 ? NULL : (xGH)->data[CCTKARGNUM_psixy][1-1]),(CCTK_REAL *)(CCTKARGNUM_psixz<0 ? NULL : (xGH)->data[CCTKARGNUM_psixz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiy<0 ? NULL : (xGH)->data[CCTKARGNUM_psiy][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiyy<0 ? NULL : (xGH)->data[CCTKARGNUM_psiyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiyz<0 ? NULL : (xGH)->data[CCTKARGNUM_psiyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiz<0 ? NULL : (xGH)->data[CCTKARGNUM_psiz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psizz<0 ? NULL : (xGH)->data[CCTKARGNUM_psizz][1-1]),(CCTK_REAL *)(CCTKARGNUM_r<0 ? NULL : (xGH)->data[CCTKARGNUM_r][1-1]),(CCTK_INT *)(CCTKARGNUM_shift_state<0 ? NULL : (xGH)->data[CCTKARGNUM_shift_state][1-1]),(CCTK_REAL *)(CCTKARGNUM_x<0 ? NULL : (xGH)->data[CCTKARGNUM_x][1-1]),(CCTK_REAL *)(CCTKARGNUM_y<0 ? NULL : (xGH)->data[CCTKARGNUM_y][1-1]),(CCTK_REAL *)(CCTKARGNUM_z<0 ? NULL : (xGH)->data[CCTKARGNUM_z][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define EINSTEIN_FARGUMENTS _CCTK_FARGUMENTS\
,EINSTEIN_PRIVATE_FARGUMENTS\
,EINSTEIN_PUBLIC_FARGUMENTS\


#define DECLARE_EINSTEIN_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \
DECLARE_EINSTEIN_PRIVATE_FARGUMENTS \
DECLARE_EINSTEIN_PUBLIC_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_EINSTEIN_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \
DECLARE_EINSTEIN_PRIVATE_CARGUMENTS \
DECLARE_EINSTEIN_PUBLIC_CARGUMENTS \


#define USE_EINSTEIN_CARGUMENTS _USE_CCTK_CARGUMENTS \
USE_EINSTEIN_PRIVATE_CARGUMENTS \
USE_EINSTEIN_PUBLIC_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define EINSTEIN_C2F_PROTO _CCTK_C2F_PROTO\
,EINSTEIN_PRIVATE_C2F_PROTO\
,EINSTEIN_PUBLIC_C2F_PROTO\


#define PASS_EINSTEIN_C2F(xGH) _PASS_CCTK_C2F(xGH)\
,PASS_EINSTEIN_PRIVATE_C2F(xGH)\
,PASS_EINSTEIN_PUBLIC_C2F(xGH)\


#define DECLARE_EINSTEIN_C2F _DECLARE_CCTK_C2F \
DECLARE_EINSTEIN_PRIVATE_C2F \
DECLARE_EINSTEIN_PUBLIC_C2F \


#define INITIALISE_EINSTEIN_C2F _INITIALISE_CCTK_C2F \
INITIALISE_EINSTEIN_PRIVATE_C2F \
INITIALISE_EINSTEIN_PUBLIC_C2F \


#define EINSTEIN_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/


