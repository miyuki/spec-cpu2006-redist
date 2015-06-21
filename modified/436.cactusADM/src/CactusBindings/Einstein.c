#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperEinstein(void *GH, void *fpointer);
int CactusBindingsVariables_Einstein_Initialise(void);
int CactusBindingsVariables_Einstein_Initialise(void)
{
  if (CCTKi_CreateGroup("FLAGS","EINSTEIN","EINSTEIN",
                    "SCALAR",
                    "INT",
                    "PUBLIC",
                    1,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    2,
                   "shift_state",
                   "conformal_state")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group flags with different dimension 1");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group flags with dimension 1");
  }
}


  if (CCTKi_CreateGroup("SLICING_FLAGS","EINSTEIN","EINSTEIN",
                    "SCALAR",
                    "INT",
                    "PUBLIC",
                    1,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "active_slicing_handle")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group slicing_flags with different dimension 1");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group slicing_flags with dimension 1");
  }
}


  if (CCTKi_CreateGroup("METRIC","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "gxx",
                   "gxy",
                   "gxz",
                   "gyy",
                   "gyz",
                   "gzz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group metric with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group metric with dimension 3");
  }
}


  if (CCTKi_CreateGroup("CURV","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "kxx",
                   "kxy",
                   "kxz",
                   "kyy",
                   "kyz",
                   "kzz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group curv with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group curv with dimension 3");
  }
}


  if (CCTKi_CreateGroup("LAPSE","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "alp")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group lapse with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group lapse with dimension 3");
  }
}


  if (CCTKi_CreateGroup("SHIFT","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    3,
                   "betax",
                   "betay",
                   "betaz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group shift with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group shift with dimension 3");
  }
}


  if (CCTKi_CreateGroup("CONFAC","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "psi")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group confac with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group confac with dimension 3");
  }
}


  if (CCTKi_CreateGroup("CONFAC_1DERIVS","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    3,
                   "psix",
                   "psiy",
                   "psiz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group confac_1derivs with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group confac_1derivs with dimension 3");
  }
}


  if (CCTKi_CreateGroup("CONFAC_2DERIVS","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "psixx",
                   "psixy",
                   "psixz",
                   "psiyy",
                   "psiyz",
                   "psizz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group confac_2derivs with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group confac_2derivs with dimension 3");
  }
}


  if (CCTKi_CreateGroup("MASK","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "emask")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group mask with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group mask with dimension 3");
  }
}


  if (CCTKi_CreateGroup("TRACE_OF_K","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "trK")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group trace_of_K with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group trace_of_K with dimension 3");
  }
}


  if (CCTKi_CreateGroup("DETOFG","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "detg")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group detofg with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group detofg with dimension 3");
  }
}


  if (CCTKi_CreateGroup("SPHERICAL_METRIC","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "grr",
                   "gqq",
                   "gpp",
                   "grq",
                   "grp",
                   "gqp")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group spherical_metric with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group spherical_metric with dimension 3");
  }
}


  if (CCTKi_CreateGroup("SPHERICAL_CURV","EINSTEIN","EINSTEIN",
                    "GF",
                    "REAL",
                    "PRIVATE",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    6,
                   "krr",
                   "kqq",
                   "kpp",
                   "krq",
                   "krp",
                   "kqp")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group spherical_curv with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group spherical_curv with dimension 3");
  }
}


  CCTKi_RegisterFortranWrapper("Einstein", CCTKi_BindingsFortranWrapperEinstein);

  return 0;
}
