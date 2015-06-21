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
  char * outInfo_reductions;
  char * outInfo_vars;
  char * outScalar_reductions;
  char * outScalar_style;
  char * outScalar_vars;
  char * out_format;
  char * outdirScalar;
  CCTK_INT  outInfo_every;
  CCTK_INT  outScalar_every;
} PRIVATE_IOBASIC_STRUCT;
