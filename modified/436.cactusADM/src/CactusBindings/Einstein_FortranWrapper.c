#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Einstein
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperEinstein(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperEinstein(cGH *GH, void *fpointer)
{
  void (*function)(EINSTEIN_C2F_PROTO);

  DECLARE_EINSTEIN_C2F
  INITIALISE_EINSTEIN_C2F

  function = (void (*)(EINSTEIN_C2F_PROTO))fpointer;

  function(PASS_EINSTEIN_C2F(GH));

  return 0;

}
