#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCPrivateIOBASIC.h"
#include "ParameterCRestrictedIO.h"

int CCTKi_BindingsCreateIOBasicParameters(void);

int CCTKi_BindingsCreateIOBasicParameters(void)
{
  CCTKi_ParameterCreate("outInfo_every", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to do Info output", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outInfo_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "");

  CCTKi_ParameterCreate("outInfo_reductions", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "List of reductions to output as Info to screen", /* The description */
                  "minimum maximum",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outInfo_reductions),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("outInfo_vars", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Variables to output as Info to screen", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outInfo_vars),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("outScalar_every", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to do Info output", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outScalar_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "");

  CCTKi_ParameterCreate("outScalar_reductions", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "List of reductions to output into files", /* The description */
                  "minimum maximum norm1 norm2",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outScalar_reductions),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("outScalar_style", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which style for Scalar output", /* The description */
                  "xgraph",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outScalar_style),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "gnuplot",  "1D output readable by gnuplot",
                  "xgraph",  "1D output readable by xgraph");

  CCTKi_ParameterCreate("outScalar_vars", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Variables to output into files", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outScalar_vars),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("out_format", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Which format for Scalar floating-point number output", /* The description */
                  ".13f",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.out_format),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "^(\\.[0-9]{1,2})?[EGefg]$",  "output with given precision in exponential / floating point notation");

  CCTKi_ParameterCreate("outdirScalar", /* The parameter name */
                        "IOBasic",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Name of IO Scalar output directory, overrides outdir", /* The description */
                  ".",  /* The default value */
                  &(PRIVATE_IOBASIC_STRUCT.outdirScalar),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  return 0;
}

int CCTKi_BindingsIOBasicParameterExtensions(void);

int CCTKi_BindingsIOBasicParameterExtensions(void)
{
  CCTKi_ParameterAddRange("IO",
                          "out_fileinfo",
                          "IOBasic",
                          "axis labels",
                           "add axis labels information to output files");

  return 0;
}
