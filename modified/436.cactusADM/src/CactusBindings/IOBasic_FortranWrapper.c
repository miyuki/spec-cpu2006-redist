#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IOBasic
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperIOBasic(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperIOBasic(cGH *GH, void *fpointer)
{
  void (*function)(IOBASIC_C2F_PROTO);

  DECLARE_IOBASIC_C2F
  INITIALISE_IOBASIC_C2F

  function = (void (*)(IOBASIC_C2F_PROTO))fpointer;

  function(PASS_IOBASIC_C2F(GH));

  return 0;

}
