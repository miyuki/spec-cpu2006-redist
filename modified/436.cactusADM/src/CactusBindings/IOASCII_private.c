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
