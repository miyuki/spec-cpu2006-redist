#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperCartGrid3D(void *GH, void *fpointer);
int CactusBindingsVariables_CartGrid3D_Initialise(void);
int CactusBindingsVariables_CartGrid3D_Initialise(void)
{
  if (CCTKi_CreateGroup("GRIDSPACINGS","CARTGRID3D","GRID",
                    "SCALAR",
                    "REAL",
                    "PUBLIC",
                    1,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    3,
                   "coarse_dx",
                   "coarse_dy",
                   "coarse_dz")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group gridspacings with different dimension 1");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group gridspacings with dimension 1");
  }
}


  if (CCTKi_CreateGroup("COORDINATES","CARTGRID3D","GRID",
                    "GF",
                    "REAL",
                    "PUBLIC",
                    3,
                    1,
                    "NONE",
                    "DEFAULT",
                    "",
                    "",
                    4,
                   "x",
                   "y",
                   "z",
                   "r")==1)
{
  int param_type;
  const CCTK_INT *allow_mixeddim_gfs;
  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet("allow_mixeddim_gfs","Cactus",&param_type);
  if (*allow_mixeddim_gfs)
  {
    CCTK_VWarn(2,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " creating GF group coordinates with different dimension 3");
  }
  else
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus"
,    "CCTKi_CreateGroup: Working dimension already set,"
    " cannot create GF group coordinates with dimension 3");
  }
}


  CCTKi_RegisterFortranWrapper("CartGrid3D", CCTKi_BindingsFortranWrapperCartGrid3D);

  return 0;
}
