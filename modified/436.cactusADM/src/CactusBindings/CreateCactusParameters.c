#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedCACTUS.h"
#include "ParameterCPrivateCACTUS.h"

int CCTKi_BindingsCreateCactusParameters(void);

int CCTKi_BindingsCreateCactusParameters(void)
{
  CCTKi_ParameterCreate("cctk_final_time", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Final time for evolution", /* The description */
                  "-1.0",  /* The default value */
                  &(RESTRICTED_CACTUS_STRUCT.cctk_final_time),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("cctk_initial_time", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Initial time for evolution", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_CACTUS_STRUCT.cctk_initial_time),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("cctk_itlast", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Final iteration number", /* The description */
                  "10",  /* The default value */
                  &(RESTRICTED_CACTUS_STRUCT.cctk_itlast),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Any integer");

  CCTKi_ParameterCreate("terminate", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Condition on which to terminate evolution loop", /* The description */
                  "iteration",  /* The default value */
                  &(RESTRICTED_CACTUS_STRUCT.terminate),   /* The actual data pointer */
                  5       /* How many allowed ranges it has */,
                  "never",  "Never terminate",
                  "iteration",  "Take termination condition from iteration number",
                  "time",  "Take termination condition from coordinate time",
                  "either",  "Take termination condition from either iteration number or coordinate time",
                  "both",  "Take termination condition from both iteration number and coordinate time");

  CCTKi_ParameterCreate("terminate_next", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Terminate on next iteration?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_CACTUS_STRUCT.terminate_next),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("allow_mixeddim_gfs", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Allow use of GFs from different dimensions", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.allow_mixeddim_gfs),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cctk_brief_output", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Give only brief output", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_brief_output),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cctk_full_warnings", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Give detailed information for each warning statement", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_full_warnings),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cctk_run_title", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Description of this simulation", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_run_title),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "Any string");

  CCTKi_ParameterCreate("cctk_show_banners", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Show any registered banners for the different thorns", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_show_banners),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cctk_show_schedule", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Print the scheduling tree to standard output", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_show_schedule),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cctk_strong_param_check", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Die on parameter errors in CCTK_PARAMCHECK", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_strong_param_check),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cctk_timer_output", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Provide output from timers", /* The description */
                  "off",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.cctk_timer_output),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "off",  "No timer output",
                  "full",  "Detailed timer output");

  CCTKi_ParameterCreate("manual_cache_setup", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Set the cache size manually", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.manual_cache_setup),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("manual_cache_size", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size to set the cache to if not done automatically (bytes)", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.manual_cache_size),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:",  "Any whole number");

  CCTKi_ParameterCreate("manual_cacheline_bytes", /* The parameter name */
                        "Cactus",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of a cacheline if not set automatically (bytes)", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_CACTUS_STRUCT.manual_cacheline_bytes),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:",  "Any whole number");

  return 0;
}

int CCTKi_BindingsCactusParameterExtensions(void);

int CCTKi_BindingsCactusParameterExtensions(void)
{
  return 0;
}
