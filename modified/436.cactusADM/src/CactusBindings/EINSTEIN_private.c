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
  CCTK_REAL  dtfac;
  CCTK_REAL  gauge_speed;
  char * wavecalc;
  CCTK_INT  conformal_storage_all;
  CCTK_INT  rsquared_in_sphm;
} PRIVATE_EINSTEIN_STRUCT;
