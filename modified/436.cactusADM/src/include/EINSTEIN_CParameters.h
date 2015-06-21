#ifndef _EINSTEIN_PARAMETERS_H_

#define _EINSTEIN_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedEINSTEIN.h"
#include "ParameterCPrivateEINSTEIN.h"

#include "ParameterCRestrictedGRID.h"
#include "ParameterCRestrictedTIME.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_EINSTEIN_STRUCT_PARAMS \
  DECLARE_PRIVATE_EINSTEIN_STRUCT_PARAMS \
  const char * domain = RESTRICTED_GRID_STRUCT.domain; \
  const char * timestep_method = RESTRICTED_TIME_STRUCT.timestep_method; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_EINSTEIN_STRUCT_PARAMS \
  USE_PRIVATE_EINSTEIN_STRUCT_PARAMS \
  cctk_pdummy_pointer = &domain; \
  cctk_pdummy_pointer = &timestep_method; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
