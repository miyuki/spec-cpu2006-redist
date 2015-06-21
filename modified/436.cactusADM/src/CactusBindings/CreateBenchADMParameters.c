#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedBENCHADM.h"
#include "ParameterCPrivateBENCHADM.h"
#include "ParameterCRestrictedEINSTEIN.h"
#include "ParameterCRestrictedTIME.h"

int CCTKi_BindingsCreateBenchADMParameters(void);

int CCTKi_BindingsCreateBenchADMParameters(void)
{
  CCTKi_ParameterCreate("method", /* The parameter name */
                        "BenchADM",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Numerical method to use for ADM", /* The description */
                  "stagleap",  /* The default value */
                  &(RESTRICTED_BENCHADM_STRUCT.method),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "stagleap",  "Evolve using Staggered Leapfrog");

  CCTKi_ParameterCreate("bound", /* The parameter name */
                        "BenchADM",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Boundary condition to implement", /* The description */
                  "none",  /* The default value */
                  &(PRIVATE_BENCHADM_STRUCT.bound),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "none",  "No boundary condition");

  CCTKi_ParameterCreate("time_symmetric", /* The parameter name */
                        "BenchADM",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How to get the initial data for staggered leapfrog", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_BENCHADM_STRUCT.time_symmetric),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  return 0;
}

int CCTKi_BindingsBenchADMParameterExtensions(void);

int CCTKi_BindingsBenchADMParameterExtensions(void)
{
  CCTKi_ParameterAddRange("EINSTEIN",
                          "evolution_system",
                          "BenchADM",
                          "ADM",
                           "Evolve using ADM evolution scheme");

  CCTKi_ParameterAddRange("EINSTEIN",
                          "slicing",
                          "BenchADM",
                          "geodesic",
                           "Lapse is set to unity");

  return 0;
}
