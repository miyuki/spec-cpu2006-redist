#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  out1D_xline_y;
  CCTK_REAL  out1D_xline_z;
  CCTK_REAL  out1D_yline_x;
  CCTK_REAL  out1D_yline_z;
  CCTK_REAL  out1D_zline_x;
  CCTK_REAL  out1D_zline_y;
  CCTK_REAL  out2D_xyplane_z;
  CCTK_REAL  out2D_xzplane_y;
  CCTK_REAL  out2D_yzplane_x;
  char * out1D_style;
  char * out1D_vars;
  char * out2D_style;
  char * out2D_vars;
  char * out3D_style;
  char * out3D_vars;
  char * out_format;
  char * out_style;
  char * outdir1D;
  char * outdir2D;
  char * outdir3D;
  CCTK_INT  out1D_d;
  CCTK_INT  out1D_every;
  CCTK_INT  out1D_x;
  CCTK_INT  out1D_xline_yi;
  CCTK_INT  out1D_xline_zi;
  CCTK_INT  out1D_y;
  CCTK_INT  out1D_yline_xi;
  CCTK_INT  out1D_yline_zi;
  CCTK_INT  out1D_z;
  CCTK_INT  out1D_zline_xi;
  CCTK_INT  out1D_zline_yi;
  CCTK_INT  out2D_every;
  CCTK_INT  out2D_xyplane_zi;
  CCTK_INT  out2D_xzplane_yi;
  CCTK_INT  out2D_yzplane_xi;
  CCTK_INT  out3D_every;
} PRIVATE_IOASCII_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_IOASCII_STRUCT_PARAMS \
  const CCTK_REAL  out1D_xline_y = PRIVATE_IOASCII_STRUCT.out1D_xline_y; \
  const CCTK_REAL  out1D_xline_z = PRIVATE_IOASCII_STRUCT.out1D_xline_z; \
  const CCTK_REAL  out1D_yline_x = PRIVATE_IOASCII_STRUCT.out1D_yline_x; \
  const CCTK_REAL  out1D_yline_z = PRIVATE_IOASCII_STRUCT.out1D_yline_z; \
  const CCTK_REAL  out1D_zline_x = PRIVATE_IOASCII_STRUCT.out1D_zline_x; \
  const CCTK_REAL  out1D_zline_y = PRIVATE_IOASCII_STRUCT.out1D_zline_y; \
  const CCTK_REAL  out2D_xyplane_z = PRIVATE_IOASCII_STRUCT.out2D_xyplane_z; \
  const CCTK_REAL  out2D_xzplane_y = PRIVATE_IOASCII_STRUCT.out2D_xzplane_y; \
  const CCTK_REAL  out2D_yzplane_x = PRIVATE_IOASCII_STRUCT.out2D_yzplane_x; \
  const char * out1D_style = PRIVATE_IOASCII_STRUCT.out1D_style; \
  const char * out1D_vars = PRIVATE_IOASCII_STRUCT.out1D_vars; \
  const char * out2D_style = PRIVATE_IOASCII_STRUCT.out2D_style; \
  const char * out2D_vars = PRIVATE_IOASCII_STRUCT.out2D_vars; \
  const char * out3D_style = PRIVATE_IOASCII_STRUCT.out3D_style; \
  const char * out3D_vars = PRIVATE_IOASCII_STRUCT.out3D_vars; \
  const char * out_format = PRIVATE_IOASCII_STRUCT.out_format; \
  const char * out_style = PRIVATE_IOASCII_STRUCT.out_style; \
  const char * outdir1D = PRIVATE_IOASCII_STRUCT.outdir1D; \
  const char * outdir2D = PRIVATE_IOASCII_STRUCT.outdir2D; \
  const char * outdir3D = PRIVATE_IOASCII_STRUCT.outdir3D; \
  const CCTK_INT  out1D_d = PRIVATE_IOASCII_STRUCT.out1D_d; \
  const CCTK_INT  out1D_every = PRIVATE_IOASCII_STRUCT.out1D_every; \
  const CCTK_INT  out1D_x = PRIVATE_IOASCII_STRUCT.out1D_x; \
  const CCTK_INT  out1D_xline_yi = PRIVATE_IOASCII_STRUCT.out1D_xline_yi; \
  const CCTK_INT  out1D_xline_zi = PRIVATE_IOASCII_STRUCT.out1D_xline_zi; \
  const CCTK_INT  out1D_y = PRIVATE_IOASCII_STRUCT.out1D_y; \
  const CCTK_INT  out1D_yline_xi = PRIVATE_IOASCII_STRUCT.out1D_yline_xi; \
  const CCTK_INT  out1D_yline_zi = PRIVATE_IOASCII_STRUCT.out1D_yline_zi; \
  const CCTK_INT  out1D_z = PRIVATE_IOASCII_STRUCT.out1D_z; \
  const CCTK_INT  out1D_zline_xi = PRIVATE_IOASCII_STRUCT.out1D_zline_xi; \
  const CCTK_INT  out1D_zline_yi = PRIVATE_IOASCII_STRUCT.out1D_zline_yi; \
  const CCTK_INT  out2D_every = PRIVATE_IOASCII_STRUCT.out2D_every; \
  const CCTK_INT  out2D_xyplane_zi = PRIVATE_IOASCII_STRUCT.out2D_xyplane_zi; \
  const CCTK_INT  out2D_xzplane_yi = PRIVATE_IOASCII_STRUCT.out2D_xzplane_yi; \
  const CCTK_INT  out2D_yzplane_xi = PRIVATE_IOASCII_STRUCT.out2D_yzplane_xi; \
  const CCTK_INT  out3D_every = PRIVATE_IOASCII_STRUCT.out3D_every; \

#define USE_PRIVATE_IOASCII_STRUCT_PARAMS \
  cctk_pdummy_pointer = &out1D_xline_y; \
  cctk_pdummy_pointer = &out1D_xline_z; \
  cctk_pdummy_pointer = &out1D_yline_x; \
  cctk_pdummy_pointer = &out1D_yline_z; \
  cctk_pdummy_pointer = &out1D_zline_x; \
  cctk_pdummy_pointer = &out1D_zline_y; \
  cctk_pdummy_pointer = &out2D_xyplane_z; \
  cctk_pdummy_pointer = &out2D_xzplane_y; \
  cctk_pdummy_pointer = &out2D_yzplane_x; \
  cctk_pdummy_pointer = &out1D_style; \
  cctk_pdummy_pointer = &out1D_vars; \
  cctk_pdummy_pointer = &out2D_style; \
  cctk_pdummy_pointer = &out2D_vars; \
  cctk_pdummy_pointer = &out3D_style; \
  cctk_pdummy_pointer = &out3D_vars; \
  cctk_pdummy_pointer = &out_format; \
  cctk_pdummy_pointer = &out_style; \
  cctk_pdummy_pointer = &outdir1D; \
  cctk_pdummy_pointer = &outdir2D; \
  cctk_pdummy_pointer = &outdir3D; \
  cctk_pdummy_pointer = &out1D_d; \
  cctk_pdummy_pointer = &out1D_every; \
  cctk_pdummy_pointer = &out1D_x; \
  cctk_pdummy_pointer = &out1D_xline_yi; \
  cctk_pdummy_pointer = &out1D_xline_zi; \
  cctk_pdummy_pointer = &out1D_y; \
  cctk_pdummy_pointer = &out1D_yline_xi; \
  cctk_pdummy_pointer = &out1D_yline_zi; \
  cctk_pdummy_pointer = &out1D_z; \
  cctk_pdummy_pointer = &out1D_zline_xi; \
  cctk_pdummy_pointer = &out1D_zline_yi; \
  cctk_pdummy_pointer = &out2D_every; \
  cctk_pdummy_pointer = &out2D_xyplane_zi; \
  cctk_pdummy_pointer = &out2D_xzplane_yi; \
  cctk_pdummy_pointer = &out2D_yzplane_xi; \
  cctk_pdummy_pointer = &out3D_every; \


