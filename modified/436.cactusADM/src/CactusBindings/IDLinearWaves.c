#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperIDLinearWaves(void *GH, void *fpointer);
int CactusBindingsVariables_IDLinearWaves_Initialise(void);
int CactusBindingsVariables_IDLinearWaves_Initialise(void)
{
  CCTKi_RegisterFortranWrapper("IDLinearWaves", CCTKi_BindingsFortranWrapperIDLinearWaves);

  return 0;
}
