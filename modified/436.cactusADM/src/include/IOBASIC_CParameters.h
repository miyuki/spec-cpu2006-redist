#ifndef _IOBASIC_PARAMETERS_H_

#define _IOBASIC_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCPrivateIOBASIC.h"

#include "ParameterCRestrictedIO.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_PRIVATE_IOBASIC_STRUCT_PARAMS \
  const char * outdir = RESTRICTED_IO_STRUCT.outdir; \
  const CCTK_INT  out_every = RESTRICTED_IO_STRUCT.out_every; \
  const char * newverbose = RESTRICTED_IO_STRUCT.newverbose; \
  const CCTK_INT  new_filename_scheme = RESTRICTED_IO_STRUCT.new_filename_scheme; \
  const char * out_fileinfo = RESTRICTED_IO_STRUCT.out_fileinfo; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_PRIVATE_IOBASIC_STRUCT_PARAMS \
  cctk_pdummy_pointer = &outdir; \
  cctk_pdummy_pointer = &out_every; \
  cctk_pdummy_pointer = &newverbose; \
  cctk_pdummy_pointer = &new_filename_scheme; \
  cctk_pdummy_pointer = &out_fileinfo; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
