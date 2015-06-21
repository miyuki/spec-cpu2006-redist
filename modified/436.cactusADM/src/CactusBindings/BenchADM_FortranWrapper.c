#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_BenchADM
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperBenchADM(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperBenchADM(cGH *GH, void *fpointer)
{
  void (*function)(BENCHADM_C2F_PROTO);

  DECLARE_BENCHADM_C2F
  INITIALISE_BENCHADM_C2F

  function = (void (*)(BENCHADM_C2F_PROTO))fpointer;

  function(PASS_BENCHADM_C2F(GH));

  return 0;

}
