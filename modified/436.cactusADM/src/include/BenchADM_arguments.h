#ifdef FCODE
#define DECLARE_BENCHADM_PRIVATE_FARGUMENTS \
INTEGER XADM_curv_stag0&&\
INTEGER XADM_curv_stag1&&\
INTEGER XADM_curv_stag2&&\
INTEGER XADM_metric_prev0&&\
INTEGER XADM_metric_prev1&&\
INTEGER XADM_metric_prev2&&\
INTEGER XADM_sources0&&\
INTEGER XADM_sources1&&\
INTEGER XADM_sources2&&\
CCTK_REAL ADM_gxx(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gxx_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gxy(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gxy_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gxz(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gxz_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gyy(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gyy_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gyz(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gyz_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gzz(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_gzz_p(XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2)&&\
CCTK_REAL ADM_kxx_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxx_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxx_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxy_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxy_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxy_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxz_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxz_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kxz_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kyy_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kyy_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kyy_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kyz_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kyz_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kyz_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kzz_stag(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kzz_stag_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL ADM_kzz_stag_p_p(XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2)&&\
CCTK_REAL adms_gxx(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_gxy(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_gxz(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_gyy(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_gyz(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_gzz(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_kxx(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_kxy(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_kxz(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_kyy(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_kyz(XADM_sources0,XADM_sources1,XADM_sources2)&&\
CCTK_REAL adms_kzz(XADM_sources0,XADM_sources1,XADM_sources2)&&\


#define BENCHADM_PRIVATE_FARGUMENTS \
XADM_curv_stag0,XADM_curv_stag1,XADM_curv_stag2,XADM_metric_prev0,XADM_metric_prev1,XADM_metric_prev2,XADM_sources0,XADM_sources1,XADM_sources2,ADM_gxx,ADM_gxx_p,ADM_gxy,ADM_gxy_p,ADM_gxz,ADM_gxz_p,ADM_gyy,ADM_gyy_p,ADM_gyz,ADM_gyz_p,ADM_gzz,ADM_gzz_p,ADM_kxx_stag,ADM_kxx_stag_p,ADM_kxx_stag_p_p,ADM_kxy_stag,ADM_kxy_stag_p,ADM_kxy_stag_p_p,ADM_kxz_stag,ADM_kxz_stag_p,ADM_kxz_stag_p_p,ADM_kyy_stag,ADM_kyy_stag_p,ADM_kyy_stag_p_p,ADM_kyz_stag,ADM_kyz_stag_p,ADM_kyz_stag_p_p,ADM_kzz_stag,ADM_kzz_stag_p,ADM_kzz_stag_p_p,adms_gxx,adms_gxy,adms_gxz,adms_gyy,adms_gyz,adms_gzz,adms_kxx,adms_kxy,adms_kxz,adms_kyy,adms_kyz,adms_kzz


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BENCHADM_PRIVATE_CARGUMENTS \
CCTK_REAL *ADM_gxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gxx")][0]); \
CCTK_REAL *ADM_gxx_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gxx")][1]); \
CCTK_REAL *ADM_gxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gxy")][0]); \
CCTK_REAL *ADM_gxy_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gxy")][1]); \
CCTK_REAL *ADM_gxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gxz")][0]); \
CCTK_REAL *ADM_gxz_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gxz")][1]); \
CCTK_REAL *ADM_gyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gyy")][0]); \
CCTK_REAL *ADM_gyy_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gyy")][1]); \
CCTK_REAL *ADM_gyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gyz")][0]); \
CCTK_REAL *ADM_gyz_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gyz")][1]); \
CCTK_REAL *ADM_gzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gzz")][0]); \
CCTK_REAL *ADM_gzz_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_gzz")][1]); \
CCTK_REAL *ADM_kxx_stag=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxx_stag")][0]); \
CCTK_REAL *ADM_kxx_stag_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxx_stag")][1]); \
CCTK_REAL *ADM_kxx_stag_p_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxx_stag")][2]); \
CCTK_REAL *ADM_kxy_stag=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxy_stag")][0]); \
CCTK_REAL *ADM_kxy_stag_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxy_stag")][1]); \
CCTK_REAL *ADM_kxy_stag_p_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxy_stag")][2]); \
CCTK_REAL *ADM_kxz_stag=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxz_stag")][0]); \
CCTK_REAL *ADM_kxz_stag_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxz_stag")][1]); \
CCTK_REAL *ADM_kxz_stag_p_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kxz_stag")][2]); \
CCTK_REAL *ADM_kyy_stag=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kyy_stag")][0]); \
CCTK_REAL *ADM_kyy_stag_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kyy_stag")][1]); \
CCTK_REAL *ADM_kyy_stag_p_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kyy_stag")][2]); \
CCTK_REAL *ADM_kyz_stag=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kyz_stag")][0]); \
CCTK_REAL *ADM_kyz_stag_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kyz_stag")][1]); \
CCTK_REAL *ADM_kyz_stag_p_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kyz_stag")][2]); \
CCTK_REAL *ADM_kzz_stag=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kzz_stag")][0]); \
CCTK_REAL *ADM_kzz_stag_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kzz_stag")][1]); \
CCTK_REAL *ADM_kzz_stag_p_p=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::ADM_kzz_stag")][2]); \
CCTK_REAL *adms_gxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_gxx")][0]); \
CCTK_REAL *adms_gxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_gxy")][0]); \
CCTK_REAL *adms_gxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_gxz")][0]); \
CCTK_REAL *adms_gyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_gyy")][0]); \
CCTK_REAL *adms_gyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_gyz")][0]); \
CCTK_REAL *adms_gzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_gzz")][0]); \
CCTK_REAL *adms_kxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_kxx")][0]); \
CCTK_REAL *adms_kxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_kxy")][0]); \
CCTK_REAL *adms_kxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_kxz")][0]); \
CCTK_REAL *adms_kyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_kyy")][0]); \
CCTK_REAL *adms_kyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_kyz")][0]); \
CCTK_REAL *adms_kzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("BenchADM::adms_kzz")][0]); \


#define USE_BENCHADM_PRIVATE_CARGUMENTS \
cctk_dummy_pointer = &ADM_gxx; \
cctk_dummy_pointer = &ADM_gxx_p; \
cctk_dummy_pointer = &ADM_gxy; \
cctk_dummy_pointer = &ADM_gxy_p; \
cctk_dummy_pointer = &ADM_gxz; \
cctk_dummy_pointer = &ADM_gxz_p; \
cctk_dummy_pointer = &ADM_gyy; \
cctk_dummy_pointer = &ADM_gyy_p; \
cctk_dummy_pointer = &ADM_gyz; \
cctk_dummy_pointer = &ADM_gyz_p; \
cctk_dummy_pointer = &ADM_gzz; \
cctk_dummy_pointer = &ADM_gzz_p; \
cctk_dummy_pointer = &ADM_kxx_stag; \
cctk_dummy_pointer = &ADM_kxx_stag_p; \
cctk_dummy_pointer = &ADM_kxx_stag_p_p; \
cctk_dummy_pointer = &ADM_kxy_stag; \
cctk_dummy_pointer = &ADM_kxy_stag_p; \
cctk_dummy_pointer = &ADM_kxy_stag_p_p; \
cctk_dummy_pointer = &ADM_kxz_stag; \
cctk_dummy_pointer = &ADM_kxz_stag_p; \
cctk_dummy_pointer = &ADM_kxz_stag_p_p; \
cctk_dummy_pointer = &ADM_kyy_stag; \
cctk_dummy_pointer = &ADM_kyy_stag_p; \
cctk_dummy_pointer = &ADM_kyy_stag_p_p; \
cctk_dummy_pointer = &ADM_kyz_stag; \
cctk_dummy_pointer = &ADM_kyz_stag_p; \
cctk_dummy_pointer = &ADM_kyz_stag_p_p; \
cctk_dummy_pointer = &ADM_kzz_stag; \
cctk_dummy_pointer = &ADM_kzz_stag_p; \
cctk_dummy_pointer = &ADM_kzz_stag_p_p; \
cctk_dummy_pointer = &adms_gxx; \
cctk_dummy_pointer = &adms_gxy; \
cctk_dummy_pointer = &adms_gxz; \
cctk_dummy_pointer = &adms_gyy; \
cctk_dummy_pointer = &adms_gyz; \
cctk_dummy_pointer = &adms_gzz; \
cctk_dummy_pointer = &adms_kxx; \
cctk_dummy_pointer = &adms_kxy; \
cctk_dummy_pointer = &adms_kxz; \
cctk_dummy_pointer = &adms_kyy; \
cctk_dummy_pointer = &adms_kyz; \
cctk_dummy_pointer = &adms_kzz; \


#define DECLARE_BENCHADM_PRIVATE_C2F \
static int CCTKARGNUM_ADM_gxx = -1; \
static int CCTKGROUPNUM_ADM_metric_prev = -1; \
static int CCTKARGNUM_ADM_gxy = -1; \
static int CCTKARGNUM_ADM_gxz = -1; \
static int CCTKARGNUM_ADM_gyy = -1; \
static int CCTKARGNUM_ADM_gyz = -1; \
static int CCTKARGNUM_ADM_gzz = -1; \
static int CCTKARGNUM_ADM_kxx_stag = -1; \
static int CCTKGROUPNUM_ADM_curv_stag = -1; \
static int CCTKARGNUM_ADM_kxy_stag = -1; \
static int CCTKARGNUM_ADM_kxz_stag = -1; \
static int CCTKARGNUM_ADM_kyy_stag = -1; \
static int CCTKARGNUM_ADM_kyz_stag = -1; \
static int CCTKARGNUM_ADM_kzz_stag = -1; \
static int CCTKARGNUM_adms_gxx = -1; \
static int CCTKGROUPNUM_ADM_sources = -1; \
static int CCTKARGNUM_adms_gxy = -1; \
static int CCTKARGNUM_adms_gxz = -1; \
static int CCTKARGNUM_adms_gyy = -1; \
static int CCTKARGNUM_adms_gyz = -1; \
static int CCTKARGNUM_adms_gzz = -1; \
static int CCTKARGNUM_adms_kxx = -1; \
static int CCTKARGNUM_adms_kxy = -1; \
static int CCTKARGNUM_adms_kxz = -1; \
static int CCTKARGNUM_adms_kyy = -1; \
static int CCTKARGNUM_adms_kyz = -1; \
static int CCTKARGNUM_adms_kzz = -1; \


#define INITIALISE_BENCHADM_PRIVATE_C2F \
if(CCTKARGNUM_ADM_gxx == -1) CCTKARGNUM_ADM_gxx = CCTK_VarIndex("BenchADM::ADM_gxx"); \
if(CCTKGROUPNUM_ADM_metric_prev == -1) CCTKGROUPNUM_ADM_metric_prev = CCTK_GroupIndex("BenchADM::ADM_metric_prev"); \
if(CCTKARGNUM_ADM_gxy == -1) CCTKARGNUM_ADM_gxy = CCTK_VarIndex("BenchADM::ADM_gxy"); \
if(CCTKARGNUM_ADM_gxz == -1) CCTKARGNUM_ADM_gxz = CCTK_VarIndex("BenchADM::ADM_gxz"); \
if(CCTKARGNUM_ADM_gyy == -1) CCTKARGNUM_ADM_gyy = CCTK_VarIndex("BenchADM::ADM_gyy"); \
if(CCTKARGNUM_ADM_gyz == -1) CCTKARGNUM_ADM_gyz = CCTK_VarIndex("BenchADM::ADM_gyz"); \
if(CCTKARGNUM_ADM_gzz == -1) CCTKARGNUM_ADM_gzz = CCTK_VarIndex("BenchADM::ADM_gzz"); \
if(CCTKARGNUM_ADM_kxx_stag == -1) CCTKARGNUM_ADM_kxx_stag = CCTK_VarIndex("BenchADM::ADM_kxx_stag"); \
if(CCTKGROUPNUM_ADM_curv_stag == -1) CCTKGROUPNUM_ADM_curv_stag = CCTK_GroupIndex("BenchADM::ADM_curv_stag"); \
if(CCTKARGNUM_ADM_kxy_stag == -1) CCTKARGNUM_ADM_kxy_stag = CCTK_VarIndex("BenchADM::ADM_kxy_stag"); \
if(CCTKARGNUM_ADM_kxz_stag == -1) CCTKARGNUM_ADM_kxz_stag = CCTK_VarIndex("BenchADM::ADM_kxz_stag"); \
if(CCTKARGNUM_ADM_kyy_stag == -1) CCTKARGNUM_ADM_kyy_stag = CCTK_VarIndex("BenchADM::ADM_kyy_stag"); \
if(CCTKARGNUM_ADM_kyz_stag == -1) CCTKARGNUM_ADM_kyz_stag = CCTK_VarIndex("BenchADM::ADM_kyz_stag"); \
if(CCTKARGNUM_ADM_kzz_stag == -1) CCTKARGNUM_ADM_kzz_stag = CCTK_VarIndex("BenchADM::ADM_kzz_stag"); \
if(CCTKARGNUM_adms_gxx == -1) CCTKARGNUM_adms_gxx = CCTK_VarIndex("BenchADM::adms_gxx"); \
if(CCTKGROUPNUM_ADM_sources == -1) CCTKGROUPNUM_ADM_sources = CCTK_GroupIndex("BenchADM::ADM_sources"); \
if(CCTKARGNUM_adms_gxy == -1) CCTKARGNUM_adms_gxy = CCTK_VarIndex("BenchADM::adms_gxy"); \
if(CCTKARGNUM_adms_gxz == -1) CCTKARGNUM_adms_gxz = CCTK_VarIndex("BenchADM::adms_gxz"); \
if(CCTKARGNUM_adms_gyy == -1) CCTKARGNUM_adms_gyy = CCTK_VarIndex("BenchADM::adms_gyy"); \
if(CCTKARGNUM_adms_gyz == -1) CCTKARGNUM_adms_gyz = CCTK_VarIndex("BenchADM::adms_gyz"); \
if(CCTKARGNUM_adms_gzz == -1) CCTKARGNUM_adms_gzz = CCTK_VarIndex("BenchADM::adms_gzz"); \
if(CCTKARGNUM_adms_kxx == -1) CCTKARGNUM_adms_kxx = CCTK_VarIndex("BenchADM::adms_kxx"); \
if(CCTKARGNUM_adms_kxy == -1) CCTKARGNUM_adms_kxy = CCTK_VarIndex("BenchADM::adms_kxy"); \
if(CCTKARGNUM_adms_kxz == -1) CCTKARGNUM_adms_kxz = CCTK_VarIndex("BenchADM::adms_kxz"); \
if(CCTKARGNUM_adms_kyy == -1) CCTKARGNUM_adms_kyy = CCTK_VarIndex("BenchADM::adms_kyy"); \
if(CCTKARGNUM_adms_kyz == -1) CCTKARGNUM_adms_kyz = CCTK_VarIndex("BenchADM::adms_kyz"); \
if(CCTKARGNUM_adms_kzz == -1) CCTKARGNUM_adms_kzz = CCTK_VarIndex("BenchADM::adms_kzz"); \


#define BENCHADM_PRIVATE_C2F_PROTO \
const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *


#define PASS_BENCHADM_PRIVATE_C2F(xGH) \
(const int *)(CCTKGROUPNUM_ADM_curv_stag<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "BenchADM::ADM_curv_stag"))),(const int *)(CCTKGROUPNUM_ADM_curv_stag<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "BenchADM::ADM_curv_stag"))),(const int *)(CCTKGROUPNUM_ADM_curv_stag<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "BenchADM::ADM_curv_stag"))),(const int *)(CCTKGROUPNUM_ADM_metric_prev<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "BenchADM::ADM_metric_prev"))),(const int *)(CCTKGROUPNUM_ADM_metric_prev<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "BenchADM::ADM_metric_prev"))),(const int *)(CCTKGROUPNUM_ADM_metric_prev<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "BenchADM::ADM_metric_prev"))),(const int *)(CCTKGROUPNUM_ADM_sources<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "BenchADM::ADM_sources"))),(const int *)(CCTKGROUPNUM_ADM_sources<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "BenchADM::ADM_sources"))),(const int *)(CCTKGROUPNUM_ADM_sources<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "BenchADM::ADM_sources"))),(CCTK_REAL *)(CCTKARGNUM_ADM_gxx<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gxx<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gxx][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gxy<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gxy<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gxy][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gxz<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gxz<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gxz][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gyy<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gyy<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gyy][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gyz<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gyz<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gyz][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gzz<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gzz][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_gzz<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_gzz][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxx_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxx_stag][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxx_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxx_stag][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxx_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxx_stag][3-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxy_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxy_stag][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxy_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxy_stag][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxy_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxy_stag][3-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxz_stag][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxz_stag][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kxz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kxz_stag][3-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kyy_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kyy_stag][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kyy_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kyy_stag][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kyy_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kyy_stag][3-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kyz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kyz_stag][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kyz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kyz_stag][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kyz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kyz_stag][3-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kzz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kzz_stag][1-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kzz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kzz_stag][2-1]),(CCTK_REAL *)(CCTKARGNUM_ADM_kzz_stag<0 ? NULL : (xGH)->data[CCTKARGNUM_ADM_kzz_stag][3-1]),(CCTK_REAL *)(CCTKARGNUM_adms_gxx<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_gxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_gxy<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_gxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_gxz<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_gxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_gyy<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_gyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_gyz<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_gyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_gzz<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_gzz][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_kxx<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_kxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_kxy<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_kxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_kxz<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_kxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_kyy<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_kyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_kyz<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_kyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_adms_kzz<0 ? NULL : (xGH)->data[CCTKARGNUM_adms_kzz][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_BENCHADM_PROTECTED_FARGUMENTS \


#define BENCHADM_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BENCHADM_PROTECTED_CARGUMENTS \


#define USE_BENCHADM_PROTECTED_CARGUMENTS \


#define DECLARE_BENCHADM_PROTECTED_C2F \


#define INITIALISE_BENCHADM_PROTECTED_C2F \


#define BENCHADM_PROTECTED_C2F_PROTO \



#define PASS_BENCHADM_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_BENCHADM_PUBLIC_FARGUMENTS \
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


#define BENCHADM_PUBLIC_FARGUMENTS \
Xconfac0,Xconfac1,Xconfac2,Xconfac_1derivs0,Xconfac_1derivs1,Xconfac_1derivs2,Xconfac_2derivs0,Xconfac_2derivs1,Xconfac_2derivs2,Xcoordinates0,Xcoordinates1,Xcoordinates2,Xcurv0,Xcurv1,Xcurv2,Xlapse0,Xlapse1,Xlapse2,Xmask0,Xmask1,Xmask2,Xmetric0,Xmetric1,Xmetric2,Xshift0,Xshift1,Xshift2,active_slicing_handle,alp,betax,betay,betaz,coarse_dx,coarse_dy,coarse_dz,conformal_state,courant_min_time,courant_wave_speed,emask,gxx,gxy,gxz,gyy,gyz,gzz,kxx,kxy,kxz,kyy,kyz,kzz,psi,psix,psixx,psixy,psixz,psiy,psiyy,psiyz,psiz,psizz,r,shift_state,x,y,z


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BENCHADM_PUBLIC_CARGUMENTS \
CCTK_INT *active_slicing_handle=(CCTK_INT *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::active_slicing_handle")][0]); \
CCTK_REAL *alp=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::alp")][0]); \
CCTK_REAL *betax=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::betax")][0]); \
CCTK_REAL *betay=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::betay")][0]); \
CCTK_REAL *betaz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::betaz")][0]); \
CCTK_REAL *coarse_dx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dx")][0]); \
CCTK_REAL *coarse_dy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dy")][0]); \
CCTK_REAL *coarse_dz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::coarse_dz")][0]); \
CCTK_INT *conformal_state=(CCTK_INT *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::conformal_state")][0]); \
CCTK_REAL *courant_min_time=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("TIME::courant_min_time")][0]); \
CCTK_REAL *courant_wave_speed=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("TIME::courant_wave_speed")][0]); \
CCTK_REAL *emask=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::emask")][0]); \
CCTK_REAL *gxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::gxx")][0]); \
CCTK_REAL *gxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::gxy")][0]); \
CCTK_REAL *gxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::gxz")][0]); \
CCTK_REAL *gyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::gyy")][0]); \
CCTK_REAL *gyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::gyz")][0]); \
CCTK_REAL *gzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::gzz")][0]); \
CCTK_REAL *kxx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::kxx")][0]); \
CCTK_REAL *kxy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::kxy")][0]); \
CCTK_REAL *kxz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::kxz")][0]); \
CCTK_REAL *kyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::kyy")][0]); \
CCTK_REAL *kyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::kyz")][0]); \
CCTK_REAL *kzz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::kzz")][0]); \
CCTK_REAL *psi=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psi")][0]); \
CCTK_REAL *psix=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psix")][0]); \
CCTK_REAL *psixx=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psixx")][0]); \
CCTK_REAL *psixy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psixy")][0]); \
CCTK_REAL *psixz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psixz")][0]); \
CCTK_REAL *psiy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psiy")][0]); \
CCTK_REAL *psiyy=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psiyy")][0]); \
CCTK_REAL *psiyz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psiyz")][0]); \
CCTK_REAL *psiz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psiz")][0]); \
CCTK_REAL *psizz=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::psizz")][0]); \
CCTK_REAL *r=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::r")][0]); \
CCTK_INT *shift_state=(CCTK_INT *)(cctkGH->data[CCTK_VarIndex("EINSTEIN::shift_state")][0]); \
CCTK_REAL *x=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::x")][0]); \
CCTK_REAL *y=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::y")][0]); \
CCTK_REAL *z=(CCTK_REAL *)(cctkGH->data[CCTK_VarIndex("GRID::z")][0]); \


#define USE_BENCHADM_PUBLIC_CARGUMENTS \
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


#define DECLARE_BENCHADM_PUBLIC_C2F \
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


#define INITIALISE_BENCHADM_PUBLIC_C2F \
if(CCTKARGNUM_active_slicing_handle == -1) CCTKARGNUM_active_slicing_handle = CCTK_VarIndex("EINSTEIN::active_slicing_handle"); \
if(CCTKGROUPNUM_slicing_flags == -1) CCTKGROUPNUM_slicing_flags = CCTK_GroupIndex("EINSTEIN::slicing_flags"); \
if(CCTKARGNUM_alp == -1) CCTKARGNUM_alp = CCTK_VarIndex("EINSTEIN::alp"); \
if(CCTKGROUPNUM_lapse == -1) CCTKGROUPNUM_lapse = CCTK_GroupIndex("EINSTEIN::lapse"); \
if(CCTKARGNUM_betax == -1) CCTKARGNUM_betax = CCTK_VarIndex("EINSTEIN::betax"); \
if(CCTKGROUPNUM_shift == -1) CCTKGROUPNUM_shift = CCTK_GroupIndex("EINSTEIN::shift"); \
if(CCTKARGNUM_betay == -1) CCTKARGNUM_betay = CCTK_VarIndex("EINSTEIN::betay"); \
if(CCTKARGNUM_betaz == -1) CCTKARGNUM_betaz = CCTK_VarIndex("EINSTEIN::betaz"); \
if(CCTKARGNUM_coarse_dx == -1) CCTKARGNUM_coarse_dx = CCTK_VarIndex("GRID::coarse_dx"); \
if(CCTKGROUPNUM_gridspacings == -1) CCTKGROUPNUM_gridspacings = CCTK_GroupIndex("GRID::gridspacings"); \
if(CCTKARGNUM_coarse_dy == -1) CCTKARGNUM_coarse_dy = CCTK_VarIndex("GRID::coarse_dy"); \
if(CCTKARGNUM_coarse_dz == -1) CCTKARGNUM_coarse_dz = CCTK_VarIndex("GRID::coarse_dz"); \
if(CCTKARGNUM_conformal_state == -1) CCTKARGNUM_conformal_state = CCTK_VarIndex("EINSTEIN::conformal_state"); \
if(CCTKGROUPNUM_flags == -1) CCTKGROUPNUM_flags = CCTK_GroupIndex("EINSTEIN::flags"); \
if(CCTKARGNUM_courant_min_time == -1) CCTKARGNUM_courant_min_time = CCTK_VarIndex("TIME::courant_min_time"); \
if(CCTKGROUPNUM_speedvars == -1) CCTKGROUPNUM_speedvars = CCTK_GroupIndex("TIME::speedvars"); \
if(CCTKARGNUM_courant_wave_speed == -1) CCTKARGNUM_courant_wave_speed = CCTK_VarIndex("TIME::courant_wave_speed"); \
if(CCTKARGNUM_emask == -1) CCTKARGNUM_emask = CCTK_VarIndex("EINSTEIN::emask"); \
if(CCTKGROUPNUM_mask == -1) CCTKGROUPNUM_mask = CCTK_GroupIndex("EINSTEIN::mask"); \
if(CCTKARGNUM_gxx == -1) CCTKARGNUM_gxx = CCTK_VarIndex("EINSTEIN::gxx"); \
if(CCTKGROUPNUM_metric == -1) CCTKGROUPNUM_metric = CCTK_GroupIndex("EINSTEIN::metric"); \
if(CCTKARGNUM_gxy == -1) CCTKARGNUM_gxy = CCTK_VarIndex("EINSTEIN::gxy"); \
if(CCTKARGNUM_gxz == -1) CCTKARGNUM_gxz = CCTK_VarIndex("EINSTEIN::gxz"); \
if(CCTKARGNUM_gyy == -1) CCTKARGNUM_gyy = CCTK_VarIndex("EINSTEIN::gyy"); \
if(CCTKARGNUM_gyz == -1) CCTKARGNUM_gyz = CCTK_VarIndex("EINSTEIN::gyz"); \
if(CCTKARGNUM_gzz == -1) CCTKARGNUM_gzz = CCTK_VarIndex("EINSTEIN::gzz"); \
if(CCTKARGNUM_kxx == -1) CCTKARGNUM_kxx = CCTK_VarIndex("EINSTEIN::kxx"); \
if(CCTKGROUPNUM_curv == -1) CCTKGROUPNUM_curv = CCTK_GroupIndex("EINSTEIN::curv"); \
if(CCTKARGNUM_kxy == -1) CCTKARGNUM_kxy = CCTK_VarIndex("EINSTEIN::kxy"); \
if(CCTKARGNUM_kxz == -1) CCTKARGNUM_kxz = CCTK_VarIndex("EINSTEIN::kxz"); \
if(CCTKARGNUM_kyy == -1) CCTKARGNUM_kyy = CCTK_VarIndex("EINSTEIN::kyy"); \
if(CCTKARGNUM_kyz == -1) CCTKARGNUM_kyz = CCTK_VarIndex("EINSTEIN::kyz"); \
if(CCTKARGNUM_kzz == -1) CCTKARGNUM_kzz = CCTK_VarIndex("EINSTEIN::kzz"); \
if(CCTKARGNUM_psi == -1) CCTKARGNUM_psi = CCTK_VarIndex("EINSTEIN::psi"); \
if(CCTKGROUPNUM_confac == -1) CCTKGROUPNUM_confac = CCTK_GroupIndex("EINSTEIN::confac"); \
if(CCTKARGNUM_psix == -1) CCTKARGNUM_psix = CCTK_VarIndex("EINSTEIN::psix"); \
if(CCTKGROUPNUM_confac_1derivs == -1) CCTKGROUPNUM_confac_1derivs = CCTK_GroupIndex("EINSTEIN::confac_1derivs"); \
if(CCTKARGNUM_psixx == -1) CCTKARGNUM_psixx = CCTK_VarIndex("EINSTEIN::psixx"); \
if(CCTKGROUPNUM_confac_2derivs == -1) CCTKGROUPNUM_confac_2derivs = CCTK_GroupIndex("EINSTEIN::confac_2derivs"); \
if(CCTKARGNUM_psixy == -1) CCTKARGNUM_psixy = CCTK_VarIndex("EINSTEIN::psixy"); \
if(CCTKARGNUM_psixz == -1) CCTKARGNUM_psixz = CCTK_VarIndex("EINSTEIN::psixz"); \
if(CCTKARGNUM_psiy == -1) CCTKARGNUM_psiy = CCTK_VarIndex("EINSTEIN::psiy"); \
if(CCTKARGNUM_psiyy == -1) CCTKARGNUM_psiyy = CCTK_VarIndex("EINSTEIN::psiyy"); \
if(CCTKARGNUM_psiyz == -1) CCTKARGNUM_psiyz = CCTK_VarIndex("EINSTEIN::psiyz"); \
if(CCTKARGNUM_psiz == -1) CCTKARGNUM_psiz = CCTK_VarIndex("EINSTEIN::psiz"); \
if(CCTKARGNUM_psizz == -1) CCTKARGNUM_psizz = CCTK_VarIndex("EINSTEIN::psizz"); \
if(CCTKARGNUM_r == -1) CCTKARGNUM_r = CCTK_VarIndex("GRID::r"); \
if(CCTKGROUPNUM_coordinates == -1) CCTKGROUPNUM_coordinates = CCTK_GroupIndex("GRID::coordinates"); \
if(CCTKARGNUM_shift_state == -1) CCTKARGNUM_shift_state = CCTK_VarIndex("EINSTEIN::shift_state"); \
if(CCTKARGNUM_x == -1) CCTKARGNUM_x = CCTK_VarIndex("GRID::x"); \
if(CCTKARGNUM_y == -1) CCTKARGNUM_y = CCTK_VarIndex("GRID::y"); \
if(CCTKARGNUM_z == -1) CCTKARGNUM_z = CCTK_VarIndex("GRID::z"); \


#define BENCHADM_PUBLIC_C2F_PROTO \
const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,const int *,CCTK_INT *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_INT *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *,CCTK_INT *,CCTK_REAL *,CCTK_REAL *,CCTK_REAL *


#define PASS_BENCHADM_PUBLIC_C2F(xGH) \
(const int *)(CCTKGROUPNUM_confac<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::confac"))),(const int *)(CCTKGROUPNUM_confac<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::confac"))),(const int *)(CCTKGROUPNUM_confac<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::confac"))),(const int *)(CCTKGROUPNUM_confac_1derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::confac_1derivs"))),(const int *)(CCTKGROUPNUM_confac_1derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::confac_1derivs"))),(const int *)(CCTKGROUPNUM_confac_1derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::confac_1derivs"))),(const int *)(CCTKGROUPNUM_confac_2derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::confac_2derivs"))),(const int *)(CCTKGROUPNUM_confac_2derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::confac_2derivs"))),(const int *)(CCTKGROUPNUM_confac_2derivs<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::confac_2derivs"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_coordinates<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "GRID::coordinates"))),(const int *)(CCTKGROUPNUM_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::curv"))),(const int *)(CCTKGROUPNUM_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::curv"))),(const int *)(CCTKGROUPNUM_curv<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::curv"))),(const int *)(CCTKGROUPNUM_lapse<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::lapse"))),(const int *)(CCTKGROUPNUM_lapse<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::lapse"))),(const int *)(CCTKGROUPNUM_lapse<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::lapse"))),(const int *)(CCTKGROUPNUM_mask<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::mask"))),(const int *)(CCTKGROUPNUM_mask<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::mask"))),(const int *)(CCTKGROUPNUM_mask<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::mask"))),(const int *)(CCTKGROUPNUM_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::metric"))),(const int *)(CCTKGROUPNUM_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::metric"))),(const int *)(CCTKGROUPNUM_metric<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::metric"))),(const int *)(CCTKGROUPNUM_shift<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 0, "EINSTEIN::shift"))),(const int *)(CCTKGROUPNUM_shift<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 1, "EINSTEIN::shift"))),(const int *)(CCTKGROUPNUM_shift<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, 2, "EINSTEIN::shift"))),(CCTK_INT *)(CCTKARGNUM_active_slicing_handle<0 ? NULL : (xGH)->data[CCTKARGNUM_active_slicing_handle][1-1]),(CCTK_REAL *)(CCTKARGNUM_alp<0 ? NULL : (xGH)->data[CCTKARGNUM_alp][1-1]),(CCTK_REAL *)(CCTKARGNUM_betax<0 ? NULL : (xGH)->data[CCTKARGNUM_betax][1-1]),(CCTK_REAL *)(CCTKARGNUM_betay<0 ? NULL : (xGH)->data[CCTKARGNUM_betay][1-1]),(CCTK_REAL *)(CCTKARGNUM_betaz<0 ? NULL : (xGH)->data[CCTKARGNUM_betaz][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dx<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dx][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dy<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dy][1-1]),(CCTK_REAL *)(CCTKARGNUM_coarse_dz<0 ? NULL : (xGH)->data[CCTKARGNUM_coarse_dz][1-1]),(CCTK_INT *)(CCTKARGNUM_conformal_state<0 ? NULL : (xGH)->data[CCTKARGNUM_conformal_state][1-1]),(CCTK_REAL *)(CCTKARGNUM_courant_min_time<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_min_time][1-1]),(CCTK_REAL *)(CCTKARGNUM_courant_wave_speed<0 ? NULL : (xGH)->data[CCTKARGNUM_courant_wave_speed][1-1]),(CCTK_REAL *)(CCTKARGNUM_emask<0 ? NULL : (xGH)->data[CCTKARGNUM_emask][1-1]),(CCTK_REAL *)(CCTKARGNUM_gxx<0 ? NULL : (xGH)->data[CCTKARGNUM_gxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_gxy<0 ? NULL : (xGH)->data[CCTKARGNUM_gxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_gxz<0 ? NULL : (xGH)->data[CCTKARGNUM_gxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_gyy<0 ? NULL : (xGH)->data[CCTKARGNUM_gyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_gyz<0 ? NULL : (xGH)->data[CCTKARGNUM_gyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_gzz<0 ? NULL : (xGH)->data[CCTKARGNUM_gzz][1-1]),(CCTK_REAL *)(CCTKARGNUM_kxx<0 ? NULL : (xGH)->data[CCTKARGNUM_kxx][1-1]),(CCTK_REAL *)(CCTKARGNUM_kxy<0 ? NULL : (xGH)->data[CCTKARGNUM_kxy][1-1]),(CCTK_REAL *)(CCTKARGNUM_kxz<0 ? NULL : (xGH)->data[CCTKARGNUM_kxz][1-1]),(CCTK_REAL *)(CCTKARGNUM_kyy<0 ? NULL : (xGH)->data[CCTKARGNUM_kyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_kyz<0 ? NULL : (xGH)->data[CCTKARGNUM_kyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_kzz<0 ? NULL : (xGH)->data[CCTKARGNUM_kzz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psi<0 ? NULL : (xGH)->data[CCTKARGNUM_psi][1-1]),(CCTK_REAL *)(CCTKARGNUM_psix<0 ? NULL : (xGH)->data[CCTKARGNUM_psix][1-1]),(CCTK_REAL *)(CCTKARGNUM_psixx<0 ? NULL : (xGH)->data[CCTKARGNUM_psixx][1-1]),(CCTK_REAL *)(CCTKARGNUM_psixy<0 ? NULL : (xGH)->data[CCTKARGNUM_psixy][1-1]),(CCTK_REAL *)(CCTKARGNUM_psixz<0 ? NULL : (xGH)->data[CCTKARGNUM_psixz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiy<0 ? NULL : (xGH)->data[CCTKARGNUM_psiy][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiyy<0 ? NULL : (xGH)->data[CCTKARGNUM_psiyy][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiyz<0 ? NULL : (xGH)->data[CCTKARGNUM_psiyz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psiz<0 ? NULL : (xGH)->data[CCTKARGNUM_psiz][1-1]),(CCTK_REAL *)(CCTKARGNUM_psizz<0 ? NULL : (xGH)->data[CCTKARGNUM_psizz][1-1]),(CCTK_REAL *)(CCTKARGNUM_r<0 ? NULL : (xGH)->data[CCTKARGNUM_r][1-1]),(CCTK_INT *)(CCTKARGNUM_shift_state<0 ? NULL : (xGH)->data[CCTKARGNUM_shift_state][1-1]),(CCTK_REAL *)(CCTKARGNUM_x<0 ? NULL : (xGH)->data[CCTKARGNUM_x][1-1]),(CCTK_REAL *)(CCTKARGNUM_y<0 ? NULL : (xGH)->data[CCTKARGNUM_y][1-1]),(CCTK_REAL *)(CCTKARGNUM_z<0 ? NULL : (xGH)->data[CCTKARGNUM_z][1-1])


#endif /*CCODE*/


#ifdef FCODE
#define BENCHADM_FARGUMENTS _CCTK_FARGUMENTS\
,BENCHADM_PRIVATE_FARGUMENTS\
,BENCHADM_PUBLIC_FARGUMENTS\


#define DECLARE_BENCHADM_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \
DECLARE_BENCHADM_PRIVATE_FARGUMENTS \
DECLARE_BENCHADM_PUBLIC_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_BENCHADM_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \
DECLARE_BENCHADM_PRIVATE_CARGUMENTS \
DECLARE_BENCHADM_PUBLIC_CARGUMENTS \


#define USE_BENCHADM_CARGUMENTS _USE_CCTK_CARGUMENTS \
USE_BENCHADM_PRIVATE_CARGUMENTS \
USE_BENCHADM_PUBLIC_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define BENCHADM_C2F_PROTO _CCTK_C2F_PROTO\
,BENCHADM_PRIVATE_C2F_PROTO\
,BENCHADM_PUBLIC_C2F_PROTO\


#define PASS_BENCHADM_C2F(xGH) _PASS_CCTK_C2F(xGH)\
,PASS_BENCHADM_PRIVATE_C2F(xGH)\
,PASS_BENCHADM_PUBLIC_C2F(xGH)\


#define DECLARE_BENCHADM_C2F _DECLARE_CCTK_C2F \
DECLARE_BENCHADM_PRIVATE_C2F \
DECLARE_BENCHADM_PUBLIC_C2F \


#define INITIALISE_BENCHADM_C2F _INITIALISE_CCTK_C2F \
INITIALISE_BENCHADM_PRIVATE_C2F \
INITIALISE_BENCHADM_PUBLIC_C2F \


#define BENCHADM_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/


