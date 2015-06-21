#ifndef _CARTGRID3D_PARAMETERS_H_

#define _CARTGRID3D_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCRestrictedGRID.h"
#include "ParameterCPrivateCARTGRID3D.h"

#include "ParameterCRestrictedDRIVER.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_RESTRICTED_GRID_STRUCT_PARAMS \
  DECLARE_PRIVATE_CARTGRID3D_STRUCT_PARAMS \
  const CCTK_INT  periodic = RESTRICTED_DRIVER_STRUCT.periodic; \
  const CCTK_INT  periodic_x = RESTRICTED_DRIVER_STRUCT.periodic_x; \
  const CCTK_INT  periodic_y = RESTRICTED_DRIVER_STRUCT.periodic_y; \
  const CCTK_INT  periodic_z = RESTRICTED_DRIVER_STRUCT.periodic_z; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_RESTRICTED_GRID_STRUCT_PARAMS \
  USE_PRIVATE_CARTGRID3D_STRUCT_PARAMS \
  cctk_pdummy_pointer = &periodic; \
  cctk_pdummy_pointer = &periodic_x; \
  cctk_pdummy_pointer = &periodic_y; \
  cctk_pdummy_pointer = &periodic_z; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
