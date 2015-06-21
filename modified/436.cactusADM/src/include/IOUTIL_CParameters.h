#ifndef _IOUTIL_PARAMETERS_H_

#define _IOUTIL_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedIO.h"


#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_IO_STRUCT_PARAMS \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_IO_STRUCT_PARAMS \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
