#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_PUGH
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperPUGH(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperPUGH(cGH *GH, void *fpointer)
{
  void (*function)(PUGH_C2F_PROTO);

  DECLARE_PUGH_C2F
  INITIALISE_PUGH_C2F

  function = (void (*)(PUGH_C2F_PROTO))fpointer;

  function(PASS_PUGH_C2F(GH));

  return 0;

}
