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
  CCTK_REAL  cctk_final_time;
  CCTK_REAL  cctk_initial_time;
  char * terminate;
  CCTK_INT  cctk_itlast;
  CCTK_INT  terminate_next;
} RESTRICTED_CACTUS_STRUCT;
