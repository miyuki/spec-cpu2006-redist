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
  CCTK_INT  avoid_origin;
  CCTK_INT  avoid_originx;
  CCTK_INT  avoid_originy;
  CCTK_INT  avoid_originz;
  CCTK_INT  no_origin;
  CCTK_INT  no_originx;
  CCTK_INT  no_originy;
  CCTK_INT  no_originz;
} PRIVATE_CARTGRID3D_STRUCT;
