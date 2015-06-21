#ifndef _TIME_PARAMETERS_H_

#define _TIME_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedTIME.h"
#include "ParameterCPrivateTIME.h"


#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_TIME_STRUCT_PARAMS \
  DECLARE_PRIVATE_TIME_STRUCT_PARAMS \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_TIME_STRUCT_PARAMS \
  USE_PRIVATE_TIME_STRUCT_PARAMS \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
