#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedBOUNDARY.h"

int CCTKi_BindingsCreateBoundaryParameters(void);

int CCTKi_BindingsCreateBoundaryParameters(void)
{
  CCTKi_ParameterCreate("radpower", /* The parameter name */
                        "Boundary",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Power of decay rate in extrapolation used in radiative boundaries", /* The description */
                  "-1",  /* The default value */
                  &(RESTRICTED_BOUNDARY_STRUCT.radpower),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "A negative value switches off this feature");

  return 0;
}

int CCTKi_BindingsBoundaryParameterExtensions(void);

int CCTKi_BindingsBoundaryParameterExtensions(void)
{
  return 0;
}
