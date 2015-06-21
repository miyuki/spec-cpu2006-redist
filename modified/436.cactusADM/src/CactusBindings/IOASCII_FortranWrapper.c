#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IOASCII
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperIOASCII(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperIOASCII(cGH *GH, void *fpointer)
{
  void (*function)(IOASCII_C2F_PROTO);

  DECLARE_IOASCII_C2F
  INITIALISE_IOASCII_C2F

  function = (void (*)(IOASCII_C2F_PROTO))fpointer;

  function(PASS_IOASCII_C2F(GH));

  return 0;

}
