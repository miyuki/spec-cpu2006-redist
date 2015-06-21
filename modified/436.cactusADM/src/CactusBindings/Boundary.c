#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctki_Groups.h"
#include "cctki_FortranWrappers.h"
int CCTKi_BindingsFortranWrapperBoundary(void *GH, void *fpointer);
int CactusBindingsVariables_Boundary_Initialise(void);
int CactusBindingsVariables_Boundary_Initialise(void)
{
  CCTKi_RegisterFortranWrapper("Boundary", CCTKi_BindingsFortranWrapperBoundary);

  return 0;
}
