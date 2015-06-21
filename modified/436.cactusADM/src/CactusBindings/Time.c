#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperTime(void *GH, void *fpointer);
int CactusBindingsVariables_Time_Initialise(void);
int CactusBindingsVariables_Time_Initialise(void)
{
  if (CCTKi_CreateGroup("SPEEDVARS","TIME","TIME",
                    "SCALAR",
                    "REAL",
                    "PUBLIC",
                    1,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    2,
                   "courant_wave_speed",
                   "courant_min_time")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group speedvars with different dimension 1");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group speedvars with dimension 1");
  }
}


  if (CCTKi_CreateGroup("COURANTTEMPS","TIME","TIME",
                    "SCALAR",
                    "REAL",
                    "PRIVATE",
                    1,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    1,
                   "courant_dt")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group couranttemps with different dimension 1");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group couranttemps with dimension 1");
  }
}


  CCTKi_RegisterFortranWrapper("Time", CCTKi_BindingsFortranWrapperTime);

  return 0;
}
