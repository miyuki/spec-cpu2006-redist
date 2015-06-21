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
  CCTK_REAL  courant_fac;
  CCTK_REAL  dtfac;
  CCTK_REAL  timestep;
  CCTK_INT  outtimestep_every;
} PRIVATE_TIME_STRUCT;
