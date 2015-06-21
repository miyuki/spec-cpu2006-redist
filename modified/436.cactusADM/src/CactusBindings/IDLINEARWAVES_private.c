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
  CCTK_REAL  amplitude;
  CCTK_REAL  wavecenter;
  CCTK_REAL  wavelength;
  CCTK_REAL  wavephi;
  CCTK_REAL  wavepulse;
  CCTK_REAL  wavetheta;
  char * packet;
  char * parity;
  char * teuk_no_vee;
  char * wavesgoing;
  CCTK_INT  mvalue;
} PRIVATE_IDLINEARWAVES_STRUCT;
