#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Boundary
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperBoundary(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperBoundary(cGH *GH, void *fpointer)
{
  void (*function)(BOUNDARY_C2F_PROTO);

  DECLARE_BOUNDARY_C2F
  INITIALISE_BOUNDARY_C2F

  function = (void (*)(BOUNDARY_C2F_PROTO))fpointer;

  function(PASS_BOUNDARY_C2F(GH));

  return 0;

}
