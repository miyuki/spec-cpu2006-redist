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
  CCTK_REAL  dx;
  CCTK_REAL  dxyz;
  CCTK_REAL  dy;
  CCTK_REAL  dz;
  CCTK_REAL  xmax;
  CCTK_REAL  xmin;
  CCTK_REAL  xyzmax;
  CCTK_REAL  xyzmin;
  CCTK_REAL  ymax;
  CCTK_REAL  ymin;
  CCTK_REAL  zmax;
  CCTK_REAL  zmin;
  char * bitant_plane;
  char * domain;
  char * quadrant_direction;
  char * type;
  CCTK_INT  symmetry_xmax;
  CCTK_INT  symmetry_xmin;
  CCTK_INT  symmetry_ymax;
  CCTK_INT  symmetry_ymin;
  CCTK_INT  symmetry_zmax;
  CCTK_INT  symmetry_zmin;
} RESTRICTED_GRID_STRUCT;
