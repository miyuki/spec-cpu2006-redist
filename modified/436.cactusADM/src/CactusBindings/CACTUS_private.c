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
  char * cctk_run_title;
  char * cctk_timer_output;
  CCTK_INT  allow_mixeddim_gfs;
  CCTK_INT  cctk_brief_output;
  CCTK_INT  cctk_full_warnings;
  CCTK_INT  cctk_show_banners;
  CCTK_INT  cctk_show_schedule;
  CCTK_INT  cctk_strong_param_check;
  CCTK_INT  manual_cache_setup;
  CCTK_INT  manual_cache_size;
  CCTK_INT  manual_cacheline_bytes;
} PRIVATE_CACTUS_STRUCT;
