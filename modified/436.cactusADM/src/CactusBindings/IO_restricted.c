#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cctk_Config.h"
#include "CParameterStructNames.h"
#include "cctk_Misc.h"
#include "ParameterBindings.h"

struct 
{
  CCTK_REAL  out_xline_y;
  CCTK_REAL  out_xline_z;
  CCTK_REAL  out_xyplane_z;
  CCTK_REAL  out_xzplane_y;
  CCTK_REAL  out_yline_x;
  CCTK_REAL  out_yline_z;
  CCTK_REAL  out_yzplane_x;
  CCTK_REAL  out_zline_x;
  CCTK_REAL  out_zline_y;
  char * checkpoint_ID_file;
  char * checkpoint_dir;
  char * checkpoint_file;
  char * newverbose;
  char * out3D_mode;
  char * out_fileinfo;
  char * outdir;
  char * parfile_name;
  char * parfile_write;
  char * recover;
  char * recover_ID_files;
  char * recover_ID_vars;
  char * recover_file;
  char * recovery_dir;
  CCTK_INT  checkpoint_ID;
  CCTK_INT  checkpoint_every;
  CCTK_INT  checkpoint_keep;
  CCTK_INT  checkpoint_keep_all;
  CCTK_INT  checkpoint_on_terminate;
  CCTK_INT  new_filename_scheme;
  CCTK_INT  out3D_downsample_x;
  CCTK_INT  out3D_downsample_y;
  CCTK_INT  out3D_downsample_z;
  CCTK_INT  out3D_parameters;
  CCTK_INT  out3D_procs;
  CCTK_INT  out3D_septimefiles;
  CCTK_INT  out3D_single;
  CCTK_INT  out3D_unchunked;
  CCTK_INT  out_every;
  CCTK_INT  out_xline_yi;
  CCTK_INT  out_xline_zi;
  CCTK_INT  out_xyplane_zi;
  CCTK_INT  out_xzplane_yi;
  CCTK_INT  out_yline_xi;
  CCTK_INT  out_yline_zi;
  CCTK_INT  out_yzplane_xi;
  CCTK_INT  out_zline_xi;
  CCTK_INT  out_zline_yi;
  CCTK_INT  parfile_update_every;
  CCTK_INT  print_timing_info;
  CCTK_INT  recover_and_remove;
  CCTK_INT  verbose;
} RESTRICTED_IO_STRUCT;
