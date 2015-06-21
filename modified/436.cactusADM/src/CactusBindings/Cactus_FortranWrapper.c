#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Cactus
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperCactus(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperCactus(cGH *GH, void *fpointer)
{
  void (*function)(CACTUS_C2F_PROTO);

  DECLARE_CACTUS_C2F
  INITIALISE_CACTUS_C2F

  function = (void (*)(CACTUS_C2F_PROTO))fpointer;

  function(PASS_CACTUS_C2F(GH));

  return 0;

}
