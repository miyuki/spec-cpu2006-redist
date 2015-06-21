#ifndef _IDLINEARWAVES_PARAMETERS_H_

#define _IDLINEARWAVES_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCPrivateIDLINEARWAVES.h"

#include "ParameterCRestrictedEINSTEIN.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_PRIVATE_IDLINEARWAVES_STRUCT_PARAMS \
  const char * initial_data = RESTRICTED_EINSTEIN_STRUCT.initial_data; \
  const CCTK_INT  use_conformal = RESTRICTED_EINSTEIN_STRUCT.use_conformal; \
  const CCTK_INT  use_conformal_derivs = RESTRICTED_EINSTEIN_STRUCT.use_conformal_derivs; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_PRIVATE_IDLINEARWAVES_STRUCT_PARAMS \
  cctk_pdummy_pointer = &initial_data; \
  cctk_pdummy_pointer = &use_conformal; \
  cctk_pdummy_pointer = &use_conformal_derivs; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
