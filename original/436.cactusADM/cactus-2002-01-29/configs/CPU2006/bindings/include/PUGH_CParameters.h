#ifndef _PUGH_PARAMETERS_H_

#define _PUGH_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedDRIVER.h"
#include "ParameterCPrivatePUGH.h"

#include "ParameterCRestrictedCACTUS.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_DRIVER_STRUCT_PARAMS \
  DECLARE_PRIVATE_PUGH_STRUCT_PARAMS \
  const char * terminate = RESTRICTED_CACTUS_STRUCT.terminate; \
  const CCTK_REAL  cctk_initial_time = RESTRICTED_CACTUS_STRUCT.cctk_initial_time; \
  const CCTK_REAL  cctk_final_time = RESTRICTED_CACTUS_STRUCT.cctk_final_time; \
  const CCTK_INT  cctk_itlast = RESTRICTED_CACTUS_STRUCT.cctk_itlast; \
  const CCTK_INT  terminate_next = RESTRICTED_CACTUS_STRUCT.terminate_next; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_DRIVER_STRUCT_PARAMS \
  USE_PRIVATE_PUGH_STRUCT_PARAMS \
  cctk_pdummy_pointer = &terminate; \
  cctk_pdummy_pointer = &cctk_initial_time; \
  cctk_pdummy_pointer = &cctk_final_time; \
  cctk_pdummy_pointer = &cctk_itlast; \
  cctk_pdummy_pointer = &terminate_next; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
