#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
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

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_IO_STRUCT_PARAMS \
  const CCTK_REAL  out_xline_y = RESTRICTED_IO_STRUCT.out_xline_y; \
  const CCTK_REAL  out_xline_z = RESTRICTED_IO_STRUCT.out_xline_z; \
  const CCTK_REAL  out_xyplane_z = RESTRICTED_IO_STRUCT.out_xyplane_z; \
  const CCTK_REAL  out_xzplane_y = RESTRICTED_IO_STRUCT.out_xzplane_y; \
  const CCTK_REAL  out_yline_x = RESTRICTED_IO_STRUCT.out_yline_x; \
  const CCTK_REAL  out_yline_z = RESTRICTED_IO_STRUCT.out_yline_z; \
  const CCTK_REAL  out_yzplane_x = RESTRICTED_IO_STRUCT.out_yzplane_x; \
  const CCTK_REAL  out_zline_x = RESTRICTED_IO_STRUCT.out_zline_x; \
  const CCTK_REAL  out_zline_y = RESTRICTED_IO_STRUCT.out_zline_y; \
  const char * checkpoint_ID_file = RESTRICTED_IO_STRUCT.checkpoint_ID_file; \
  const char * checkpoint_dir = RESTRICTED_IO_STRUCT.checkpoint_dir; \
  const char * checkpoint_file = RESTRICTED_IO_STRUCT.checkpoint_file; \
  const char * newverbose = RESTRICTED_IO_STRUCT.newverbose; \
  const char * out3D_mode = RESTRICTED_IO_STRUCT.out3D_mode; \
  const char * out_fileinfo = RESTRICTED_IO_STRUCT.out_fileinfo; \
  const char * outdir = RESTRICTED_IO_STRUCT.outdir; \
  const char * parfile_name = RESTRICTED_IO_STRUCT.parfile_name; \
  const char * parfile_write = RESTRICTED_IO_STRUCT.parfile_write; \
  const char * recover = RESTRICTED_IO_STRUCT.recover; \
  const char * recover_ID_files = RESTRICTED_IO_STRUCT.recover_ID_files; \
  const char * recover_ID_vars = RESTRICTED_IO_STRUCT.recover_ID_vars; \
  const char * recover_file = RESTRICTED_IO_STRUCT.recover_file; \
  const char * recovery_dir = RESTRICTED_IO_STRUCT.recovery_dir; \
  const CCTK_INT  checkpoint_ID = RESTRICTED_IO_STRUCT.checkpoint_ID; \
  const CCTK_INT  checkpoint_every = RESTRICTED_IO_STRUCT.checkpoint_every; \
  const CCTK_INT  checkpoint_keep = RESTRICTED_IO_STRUCT.checkpoint_keep; \
  const CCTK_INT  checkpoint_keep_all = RESTRICTED_IO_STRUCT.checkpoint_keep_all; \
  const CCTK_INT  checkpoint_on_terminate = RESTRICTED_IO_STRUCT.checkpoint_on_terminate; \
  const CCTK_INT  new_filename_scheme = RESTRICTED_IO_STRUCT.new_filename_scheme; \
  const CCTK_INT  out3D_downsample_x = RESTRICTED_IO_STRUCT.out3D_downsample_x; \
  const CCTK_INT  out3D_downsample_y = RESTRICTED_IO_STRUCT.out3D_downsample_y; \
  const CCTK_INT  out3D_downsample_z = RESTRICTED_IO_STRUCT.out3D_downsample_z; \
  const CCTK_INT  out3D_parameters = RESTRICTED_IO_STRUCT.out3D_parameters; \
  const CCTK_INT  out3D_procs = RESTRICTED_IO_STRUCT.out3D_procs; \
  const CCTK_INT  out3D_septimefiles = RESTRICTED_IO_STRUCT.out3D_septimefiles; \
  const CCTK_INT  out3D_single = RESTRICTED_IO_STRUCT.out3D_single; \
  const CCTK_INT  out3D_unchunked = RESTRICTED_IO_STRUCT.out3D_unchunked; \
  const CCTK_INT  out_every = RESTRICTED_IO_STRUCT.out_every; \
  const CCTK_INT  out_xline_yi = RESTRICTED_IO_STRUCT.out_xline_yi; \
  const CCTK_INT  out_xline_zi = RESTRICTED_IO_STRUCT.out_xline_zi; \
  const CCTK_INT  out_xyplane_zi = RESTRICTED_IO_STRUCT.out_xyplane_zi; \
  const CCTK_INT  out_xzplane_yi = RESTRICTED_IO_STRUCT.out_xzplane_yi; \
  const CCTK_INT  out_yline_xi = RESTRICTED_IO_STRUCT.out_yline_xi; \
  const CCTK_INT  out_yline_zi = RESTRICTED_IO_STRUCT.out_yline_zi; \
  const CCTK_INT  out_yzplane_xi = RESTRICTED_IO_STRUCT.out_yzplane_xi; \
  const CCTK_INT  out_zline_xi = RESTRICTED_IO_STRUCT.out_zline_xi; \
  const CCTK_INT  out_zline_yi = RESTRICTED_IO_STRUCT.out_zline_yi; \
  const CCTK_INT  parfile_update_every = RESTRICTED_IO_STRUCT.parfile_update_every; \
  const CCTK_INT  print_timing_info = RESTRICTED_IO_STRUCT.print_timing_info; \
  const CCTK_INT  recover_and_remove = RESTRICTED_IO_STRUCT.recover_and_remove; \
  const CCTK_INT  verbose = RESTRICTED_IO_STRUCT.verbose; \

#define USE_RESTRICTED_IO_STRUCT_PARAMS \
  cctk_pdummy_pointer = &out_xline_y; \
  cctk_pdummy_pointer = &out_xline_z; \
  cctk_pdummy_pointer = &out_xyplane_z; \
  cctk_pdummy_pointer = &out_xzplane_y; \
  cctk_pdummy_pointer = &out_yline_x; \
  cctk_pdummy_pointer = &out_yline_z; \
  cctk_pdummy_pointer = &out_yzplane_x; \
  cctk_pdummy_pointer = &out_zline_x; \
  cctk_pdummy_pointer = &out_zline_y; \
  cctk_pdummy_pointer = &checkpoint_ID_file; \
  cctk_pdummy_pointer = &checkpoint_dir; \
  cctk_pdummy_pointer = &checkpoint_file; \
  cctk_pdummy_pointer = &newverbose; \
  cctk_pdummy_pointer = &out3D_mode; \
  cctk_pdummy_pointer = &out_fileinfo; \
  cctk_pdummy_pointer = &outdir; \
  cctk_pdummy_pointer = &parfile_name; \
  cctk_pdummy_pointer = &parfile_write; \
  cctk_pdummy_pointer = &recover; \
  cctk_pdummy_pointer = &recover_ID_files; \
  cctk_pdummy_pointer = &recover_ID_vars; \
  cctk_pdummy_pointer = &recover_file; \
  cctk_pdummy_pointer = &recovery_dir; \
  cctk_pdummy_pointer = &checkpoint_ID; \
  cctk_pdummy_pointer = &checkpoint_every; \
  cctk_pdummy_pointer = &checkpoint_keep; \
  cctk_pdummy_pointer = &checkpoint_keep_all; \
  cctk_pdummy_pointer = &checkpoint_on_terminate; \
  cctk_pdummy_pointer = &new_filename_scheme; \
  cctk_pdummy_pointer = &out3D_downsample_x; \
  cctk_pdummy_pointer = &out3D_downsample_y; \
  cctk_pdummy_pointer = &out3D_downsample_z; \
  cctk_pdummy_pointer = &out3D_parameters; \
  cctk_pdummy_pointer = &out3D_procs; \
  cctk_pdummy_pointer = &out3D_septimefiles; \
  cctk_pdummy_pointer = &out3D_single; \
  cctk_pdummy_pointer = &out3D_unchunked; \
  cctk_pdummy_pointer = &out_every; \
  cctk_pdummy_pointer = &out_xline_yi; \
  cctk_pdummy_pointer = &out_xline_zi; \
  cctk_pdummy_pointer = &out_xyplane_zi; \
  cctk_pdummy_pointer = &out_xzplane_yi; \
  cctk_pdummy_pointer = &out_yline_xi; \
  cctk_pdummy_pointer = &out_yline_zi; \
  cctk_pdummy_pointer = &out_yzplane_xi; \
  cctk_pdummy_pointer = &out_zline_xi; \
  cctk_pdummy_pointer = &out_zline_yi; \
  cctk_pdummy_pointer = &parfile_update_every; \
  cctk_pdummy_pointer = &print_timing_info; \
  cctk_pdummy_pointer = &recover_and_remove; \
  cctk_pdummy_pointer = &verbose; \


