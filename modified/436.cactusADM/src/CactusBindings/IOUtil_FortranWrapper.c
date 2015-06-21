#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_IOUtil
#include "cctk.h"
#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Comm.h"
#include "cctk_Arguments.h"

int CCTKi_BindingsFortranWrapperIOUtil(cGH *GH, void *fpointer);

int CCTKi_BindingsFortranWrapperIOUtil(cGH *GH, void *fpointer)
{
  void (*function)(IOUTIL_C2F_PROTO);

  DECLARE_IOUTIL_C2F
  INITIALISE_IOUTIL_C2F

  function = (void (*)(IOUTIL_C2F_PROTO))fpointer;

  function(PASS_IOUTIL_C2F(GH));

  return 0;

}
