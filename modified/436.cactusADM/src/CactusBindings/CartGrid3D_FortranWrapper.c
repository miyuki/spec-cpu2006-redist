#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_CartGrid3D
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperCartGrid3D(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperCartGrid3D(cGH *GH, void *fpointer)
{
  void (*function)(CARTGRID3D_C2F_PROTO);

  DECLARE_CARTGRID3D_C2F
  INITIALISE_CARTGRID3D_C2F

  function = (void (*)(CARTGRID3D_C2F_PROTO))fpointer;

  function(PASS_CARTGRID3D_C2F(GH));

  return 0;

}
