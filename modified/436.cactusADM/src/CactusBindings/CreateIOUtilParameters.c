#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedIO.h"

int CCTKi_BindingsCreateIOUtilParameters(void);

int CCTKi_BindingsCreateIOUtilParameters(void)
{
  CCTKi_ParameterCreate("checkpoint_ID", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Checkpoint initial data ?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_ID),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("checkpoint_ID_file", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "File name for initial data checkpoint", /* The description */
                  "checkpointID",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_ID_file),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("checkpoint_dir", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Output directory for checkpoint files", /* The description */
                  ".",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_dir),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("checkpoint_every", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Checkpoint every x iterations", /* The description */
                  "-1",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "negative values disable checkpointing");

  CCTKi_ParameterCreate("checkpoint_file", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "File name for regular checkpoint", /* The description */
                  "checkpoint.chkpt",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_file),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("checkpoint_keep", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "How many checkpoint files to keep", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_keep),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:*",  "1 overwrites the latest checkpoint file");

  CCTKi_ParameterCreate("checkpoint_keep_all", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Keep all checkpoint files ?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_keep_all),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("checkpoint_on_terminate", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Checkpoint after last iteration", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.checkpoint_on_terminate),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("new_filename_scheme", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Use the new filename scheme for output files ?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.new_filename_scheme),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("newverbose", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Level of screen output for IO?", /* The description */
                  "standard",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.newverbose),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "none",  "Give no output",
                  "standard",  "Initial description for each method",
                  "full",  "Maximal output");

  CCTKi_ParameterCreate("out3D_downsample_x", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Factor by which to downsample output in x direction. Point (0,0,0) is always included.", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_downsample_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("out3D_downsample_y", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Factor by which to downsample output in y direction. Point (0,0,0) is always included.", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_downsample_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("out3D_downsample_z", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Factor by which to downsample output in z direction. Point (0,0,0) is always included.", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_downsample_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("out3D_mode", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which mode for 3D IO", /* The description */
                  "proc",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_mode),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "proc",  "every processor writes its share of data into a separate file",
                  "np",  "data is collected and written by every N'th processor into a separate file, where N is specified by the parameter out3D_procs",
                  "onefile",  "all output is written into a single file by processor 0");

  CCTKi_ParameterCreate("out3D_parameters", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Write parameters to 3D output files ?", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_parameters),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out3D_procs", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Do IO on every N processors.", /* The description */
                  "8",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_procs),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("out3D_septimefiles", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Write one file per time slice, as opposed to all data in one file", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_septimefiles),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out3D_single", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Output 3D data in single precision ? This parameter is ignored for Cactus compiled with single precision", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_single),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out3D_unchunked", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Don't write data in chunks. This parameter is ignored for single-processor runs where output is always done in unchunked mode.", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out3D_unchunked),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("out_every", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to do IO output", /* The description */
                  "-1",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_every),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "-1",  "Never",
                  "1:*",  "Every so many iterations"	);

  CCTKi_ParameterCreate("out_fileinfo", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Add some useful file information to output files ?", /* The description */
                  "all",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_fileinfo),   /* The actual data pointer */
                  4       /* How many allowed ranges it has */,
                  "none",  "no file information",
                  "creation date",  "add creation date",
                  "parameter filename",  "add parameter filename",
                  "all",  "add all available file information");

  CCTKi_ParameterCreate("out_xline_y", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-coord for 1D lines in x-direction", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xline_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_xline_yi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-index (from 0) for 1D lines in x-direction", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xline_yi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_xline_z", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-coord for 1D lines in x-direction", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xline_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_xline_zi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-index (from 0) for 1D lines in x-direction", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xline_zi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_xyplane_z", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-coord for 2D planes in xy", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xyplane_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "" );

  CCTKi_ParameterCreate("out_xyplane_zi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-index (from 0) for 2D planes in xy", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xyplane_zi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_xzplane_y", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-coord for 2D planes in xz", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xzplane_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_xzplane_yi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-index (from 0) for 2D planes in xz", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_xzplane_yi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_yline_x", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-coord for 1D lines in y-direction", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_yline_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_yline_xi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-index (from 0) for 1D lines in y-direction", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_yline_xi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_yline_z", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-coord for 1D lines in y-direction", /* The description */
                  "0.0 ",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_yline_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_yline_zi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "z-index (from 0) for 1D lines in y-direction", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_yline_zi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_yzplane_x", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-coord for 2D planes in yz", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_yzplane_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_yzplane_xi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-index (from 0) for 2D planes in yz", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_yzplane_xi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_zline_x", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-coord for 1D lines in z-direction", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_zline_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_zline_xi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "x-index (from 0) for 1D lines in z-direction", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_zline_xi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("out_zline_y", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-coord for 1D lines in z-direction", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_zline_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("out_zline_yi", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "y-index (from 0) for 1D lines in z-direction", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.out_zline_yi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "");

  CCTKi_ParameterCreate("outdir", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Name of IO output directory", /* The description */
                  ".",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.outdir),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("parfile_name", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Filename for the parameter file to be written", /* The description */
                  "",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.parfile_name),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "a valid filename, or an empty string if the original parameter "             "filename should be used");

  CCTKi_ParameterCreate("parfile_update_every", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to update the parameter file for steered parameters", /* The description */
                  "0",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.parfile_update_every),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Update every so many iterations (zero disables updating)");

  CCTKi_ParameterCreate("parfile_write", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Write a parameter file to 'IO::outdir'", /* The description */
                  "copy",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.parfile_write),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "no",  "Do not write a parameter file",
                  "copy",  "Copy the original parameter file",
                  "generate",  "Generate a parameter file from the current settings");

  CCTKi_ParameterCreate("print_timing_info", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Print timing information on I/O operations.", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.print_timing_info),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("recover", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Recover from a checkpoint file ?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.recover),   /* The actual data pointer */
                  4       /* How many allowed ranges it has */,
                  "no",  "Don't recover",
                  "manual",  "Recover from the checkpoint file given as <recovery_dir>/<recover_file>",
                  "auto",  "Automatically recover from the latest checkpoint file found in <recovery_dir>",
                  "autoprobe",  "Probe for checkpoint files and automatically recover, continue as usual if nothing was found");

  CCTKi_ParameterCreate("recover_ID_files", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "List of files to read in as initial data", /* The description */
                  "",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.recover_ID_files),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("recover_ID_vars", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "List of variables to read in from the given initial data files", /* The description */
                  "all",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.recover_ID_vars),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("recover_and_remove", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Remove checkpoint file after successful recovery ?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.recover_and_remove),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("recover_file", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "File name of recovery file", /* The description */
                  "checkpoint.chkpt",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.recover_file),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("recovery_dir", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_RECOVER,              /* Is it steerable ?  */
                  "Directory to look for the recovery file", /* The description */
                  ".",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.recovery_dir),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches everything");

  CCTKi_ParameterCreate("verbose", /* The parameter name */
                        "IOUtil",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Give extended screen output in IO?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_IO_STRUCT.verbose),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  return 0;
}

int CCTKi_BindingsIOUtilParameterExtensions(void);

int CCTKi_BindingsIOUtilParameterExtensions(void)
{
  return 0;
}
