#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCPrivateIOASCII.h"
#include "ParameterCRestrictedIO.h"

int CCTKi_BindingsCreateIOASCIIParameters(void);

int CCTKi_BindingsCreateIOASCIIParameters(void)
{
  CCTKi_ParameterCreate("out1D_d", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Do 1D IOASCII output in the diagonal-direction", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_d),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out1D_every", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to do 1D ASCII output, overrides out_every", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Values <= 0 disable 1D output");

  CCTKi_ParameterCreate("out1D_style", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which style for 1D lines ASCII output", /* The description */
                  "xgraph",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_style),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "xgraph",  "f over x plots suitable for xgraph",
                  "gnuplot f(x)",  "f over x plots suitable for gnuplot",
                  "gnuplot f(t,x)",  "f over t,x plots suitable for gnuplot");

  CCTKi_ParameterCreate("out1D_vars", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Variables to output in 1D ASCII file format", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_vars),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matces everything");

  CCTKi_ParameterCreate("out1D_x", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Do 1D IOASCII output in the x-direction", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_x),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out1D_xline_y", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-coord for 1D lines in x-direction", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_xline_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out1D_xline_yi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-index (from 0) for 1D lines in x-direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_xline_yi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out1D_xline_z", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-coord for 1D lines in x-direction", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_xline_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out1D_xline_zi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-index (from 0) for 1D lines in x-direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_xline_zi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out1D_y", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Do 1D IOASCII output in the y-direction", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_y),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out1D_yline_x", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-coord for 1D lines in y-direction", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_yline_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out1D_yline_xi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-index (from 0) for 1D lines in y-direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_yline_xi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out1D_yline_z", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-coord for 1D lines in y-direction", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_yline_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out1D_yline_zi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-index (from 0) for 1D lines in y-direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_yline_zi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out1D_z", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Do 1D IOASCII output in the z-direction", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_z),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out1D_zline_x", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-coord for 1D lines in z-direction", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_zline_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out1D_zline_xi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-index (from 0) for 1D lines in z-direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_zline_xi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out1D_zline_y", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-coord for 1D lines in z-direction", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_zline_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out1D_zline_yi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-index (from 0) for 1D lines in z-direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out1D_zline_yi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out2D_every", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to do 2D ASCII output, overrides out_every", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Values <= 0 disable 2D output");

  CCTKi_ParameterCreate("out2D_style", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which style for 2D slices ASCII output", /* The description */
                  "gnuplot f(x,y)",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_style),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "gnuplot f(x,y)",  "f over x,y plots suitable for gnuplot",
                  "gnuplot f(t,x,y)",  "f over t,x,y plots suitable for gnuplot");

  CCTKi_ParameterCreate("out2D_vars", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Variables to output in 2D ASCII file format", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_vars),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("out2D_xyplane_z", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-coord for 2D planes in xy", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_xyplane_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out2D_xyplane_zi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-index (from 0) for 2D planes in xy", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_xyplane_zi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out2D_xzplane_y", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-coord for 2D planes in xz", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_xzplane_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out2D_xzplane_yi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-index (from 0) for 2D planes in xz", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_xzplane_yi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out2D_yzplane_x", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-coord for 2D planes in yz", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_yzplane_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out2D_yzplane_xi", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-index (from 0) for 2D planes in yz", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out2D_yzplane_xi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out3D_every", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to do 3D ASCII output, overrides out_every", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out3D_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Values <= 0 disable 3D output");

  CCTKi_ParameterCreate("out3D_style", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which style for 3D volume ASCII output", /* The description */
                  "gnuplot f(x,y,z)",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out3D_style),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "gnuplot f(x,y,z)",  "f over x,y,z plots suitable for gnuplot",
                  "gnuplot f(t,x,y,z)",  "f over t,x,y,z plots suitable for gnuplot");

  CCTKi_ParameterCreate("out3D_vars", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Variables to output in 3D ASCII file format", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out3D_vars),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("out_format", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Which format for ASCII floating-point number output", /* The description */
                  ".13f",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out_format),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "^(\\.[0-9]{1,2})?[EGefg]$",  "output with given precision in exponential / floating point notation");

  CCTKi_ParameterCreate("out_style", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which style for 1D ASCII output (DEPRICATED IN BETA12)", /* The description */
                  "xgraph",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.out_style),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "gnuplot",  "output readable by gnuplot",
                  "xgraph",  "output readable by xgraph");

  CCTKi_ParameterCreate("outdir1D", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Name of 1D ASCII output directory, overrides outdir", /* The description */
                  ".",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.outdir1D),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matces everything");

  CCTKi_ParameterCreate("outdir2D", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Name of 2D ASCII output directory, overrides outdir", /* The description */
                  ".",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.outdir2D),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matces everything");

  CCTKi_ParameterCreate("outdir3D", /* The parameter name */
                        "IOASCII",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Name of 3D ASCII output directory, overrides outdir", /* The description */
                  ".",  /* The default value */
                  &(PRIVATE_IOASCII_STRUCT.outdir3D),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matces everything");

  return 0;
}

int CCTKi_BindingsIOASCIIParameterExtensions(void);

int CCTKi_BindingsIOASCIIParameterExtensions(void)
{
  CCTKi_ParameterAddRange("IO",
                          "out_fileinfo",
                          "IOASCII",
                          "axis labels",
                           "add axis labels information to output files");

  return 0;
}
