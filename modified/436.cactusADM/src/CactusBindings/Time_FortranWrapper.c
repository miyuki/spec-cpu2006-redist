#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Time
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperTime(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperTime(cGH *GH, void *fpointer)
{
  void (*function)(TIME_C2F_PROTO);

  DECLARE_TIME_C2F
  INITIALISE_TIME_C2F

  function = (void (*)(TIME_C2F_PROTO))fpointer;

  function(PASS_TIME_C2F(GH));

  return 0;

}
