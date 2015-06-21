#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IDLinearWaves
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperIDLinearWaves(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperIDLinearWaves(cGH *GH, void *fpointer)
{
  void (*function)(IDLINEARWAVES_C2F_PROTO);

  DECLARE_IDLINEARWAVES_C2F
  INITIALISE_IDLINEARWAVES_C2F

  function = (void (*)(IDLINEARWAVES_C2F_PROTO))fpointer;

  function(PASS_IDLINEARWAVES_C2F(GH));

  return 0;

}
