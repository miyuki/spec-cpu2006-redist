#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGHSlab
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperPUGHSlab(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperPUGHSlab(cGH *GH, void *fpointer)
{
  void (*function)(PUGHSLAB_C2F_PROTO);

  DECLARE_PUGHSLAB_C2F
  INITIALISE_PUGHSLAB_C2F

  function = (void (*)(PUGHSLAB_C2F_PROTO))fpointer;

  function(PASS_PUGHSLAB_C2F(GH));

  return 0;

}
