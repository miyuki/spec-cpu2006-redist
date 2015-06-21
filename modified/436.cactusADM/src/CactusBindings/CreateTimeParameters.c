#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedTIME.h"
#include "ParameterCPrivateTIME.h"

int CCTKi_BindingsCreateTimeParameters(void);

int CCTKi_BindingsCreateTimeParameters(void)
{
  CCTKi_ParameterCreate("timestep_method", /* The parameter name */
                        "Time",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Method for calculating timestep", /* The description */
                  "courant_static",  /* The default value */
                  &(RESTRICTED_TIME_STRUCT.timestep_method),   /* The actual data pointer */
                  4       /* How many allowed ranges it has */,
                  "given",  "Use given timestep",
                  "courant_static",  "Courant condition at BASEGRID",
                  "courant_speed",  "Courant condition at PRESTEP (using wavespeed)",
                  "courant_time",  "Courant condition at PRESTEP (using min time)");

  CCTKi_ParameterCreate("timestep_outonly", /* The parameter name */
                        "Time",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Don't set a dynamic timestep, just output what it would be", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_TIME_STRUCT.timestep_outonly),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("courant_fac", /* The parameter name */
                        "Time",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The courant timestep condition dt = courant_fac*max(delta_space)/speed/sqrt(dim)", /* The description */
                  "0.9",  /* The default value */
                  &(PRIVATE_TIME_STRUCT.courant_fac),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "0:*",  "For positive timestep",
                  "*:0",  "For negative timestep");

  CCTKi_ParameterCreate("dtfac", /* The parameter name */
                        "Time",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The standard timestep condition dt = dtfac*max(delta_space)", /* The description */
                  "0.5 ",  /* The default value */
                  &(PRIVATE_TIME_STRUCT.dtfac),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "0:*",  "For positive timestep",
                  "*:0",  "For negative timestep");

  CCTKi_ParameterCreate("outtimestep_every", /* The parameter name */
                        "Time",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How often to output courant timestep", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_TIME_STRUCT.outtimestep_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Zero means no output");

  CCTKi_ParameterCreate("timestep", /* The parameter name */
                        "Time",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Absolute value for timestep", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_TIME_STRUCT.timestep),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "Could be anything");

  return 0;
}

int CCTKi_BindingsTimeParameterExtensions(void);

int CCTKi_BindingsTimeParameterExtensions(void)
{
  return 0;
}
