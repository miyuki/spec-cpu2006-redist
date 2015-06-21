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
  CCTK_INT  ghost_size;
  CCTK_INT  ghost_size_x;
  CCTK_INT  ghost_size_y;
  CCTK_INT  ghost_size_z;
  CCTK_INT  global_nsize;
  CCTK_INT  global_nx;
  CCTK_INT  global_ny;
  CCTK_INT  global_nz;
  CCTK_INT  periodic;
  CCTK_INT  periodic_x;
  CCTK_INT  periodic_y;
  CCTK_INT  periodic_z;
} RESTRICTED_DRIVER_STRUCT;
