#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperIOBasic(void *GH, void *fpointer);
int CactusBindingsVariables_IOBasic_Initialise(void);
int CactusBindingsVariables_IOBasic_Initialise(void)
{
  CCTKi_RegisterFortranWrapper("IOBasic", CCTKi_BindingsFortranWrapperIOBasic);

  return 0;
}
