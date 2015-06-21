#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperBenchADM(void *GH, void *fpointer);
int CactusBindingsVariables_BenchADM_Initialise(void);
int CactusBindingsVariables_BenchADM_Initialise(void)
{
  if (CCTKi_CreateGroup("ADM_METRIC_PREV","BENCHADM","BENCHADM",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    2,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "ADM_gxx",
                   "ADM_gxy",
                   "ADM_gxz",
                   "ADM_gyy",
                   "ADM_gyz",
                   "ADM_gzz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group ADM_metric_prev with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group ADM_metric_prev with dimension 3");
  }
}


  if (CCTKi_CreateGroup("ADM_CURV_STAG","BENCHADM","BENCHADM",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    3,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "ADM_kxx_stag",
                   "ADM_kxy_stag",
                   "ADM_kxz_stag",
                   "ADM_kyy_stag",
                   "ADM_kyz_stag",
                   "ADM_kzz_stag")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group ADM_curv_stag with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group ADM_curv_stag with dimension 3");
  }
}


  if (CCTKi_CreateGroup("ADM_SOURCES","BENCHADM","BENCHADM",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    12,
                   "adms_kxx",
                   "adms_kxy",
                   "adms_kxz",
                   "adms_kyy",
                   "adms_kyz",
                   "adms_kzz",
                   "adms_gxx",
                   "adms_gxy",
                   "adms_gxz",
                   "adms_gyy",
                   "adms_gyz",
                   "adms_gzz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group ADM_sources with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group ADM_sources with dimension 3");
  }
}


  CCTKi_RegisterFortranWrapper("BenchADM", CCTKi_BindingsFortranWrapperBenchADM);

  return 0;
}
