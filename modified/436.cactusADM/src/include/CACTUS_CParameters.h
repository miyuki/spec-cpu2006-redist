#ifndef _CACTUS_PARAMETERS_H_

#define _CACTUS_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedCACTUS.h"
#include "ParameterCPrivateCACTUS.h"


#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_CACTUS_STRUCT_PARAMS \
  DECLARE_PRIVATE_CACTUS_STRUCT_PARAMS \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_CACTUS_STRUCT_PARAMS \
  USE_PRIVATE_CACTUS_STRUCT_PARAMS \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
