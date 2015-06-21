#ifndef _BENCHADM_PARAMETERS_H_

#define _BENCHADM_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedBENCHADM.h"
#include "ParameterCPrivateBENCHADM.h"

#include "ParameterCRestrictedEINSTEIN.h"
#include "ParameterCRestrictedTIME.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_BENCHADM_STRUCT_PARAMS \
  DECLARE_PRIVATE_BENCHADM_STRUCT_PARAMS \
  const char * evolution_system = RESTRICTED_EINSTEIN_STRUCT.evolution_system; \
  const char * slicing = RESTRICTED_EINSTEIN_STRUCT.slicing; \
  const char * timestep_method = RESTRICTED_TIME_STRUCT.timestep_method; \
  const CCTK_INT  timestep_outonly = RESTRICTED_TIME_STRUCT.timestep_outonly; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_BENCHADM_STRUCT_PARAMS \
  USE_PRIVATE_BENCHADM_STRUCT_PARAMS \
  cctk_pdummy_pointer = &evolution_system; \
  cctk_pdummy_pointer = &slicing; \
  cctk_pdummy_pointer = &timestep_method; \
  cctk_pdummy_pointer = &timestep_outonly; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
