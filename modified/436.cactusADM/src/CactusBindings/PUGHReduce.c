#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperPUGHReduce(void *GH, void *fpointer);
int CactusBindingsVariables_PUGHReduce_Initialise(void);
int CactusBindingsVariables_PUGHReduce_Initialise(void)
{
  CCTKi_RegisterFortranWrapper("PUGHReduce", CCTKi_BindingsFortranWrapperPUGHReduce);

  return 0;
}
