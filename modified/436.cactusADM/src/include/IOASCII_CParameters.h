#ifndef _IOASCII_PARAMETERS_H_

#define _IOASCII_PARAMETERS_H_

#include "ParameterCGlobal.h"
#include "ParameterCPrivateIOASCII.h"

#include "ParameterCRestrictedIO.h"

#define DECLARE_CCTK_PARAMETERS \
  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  DECLARE_PRIVATE_IOASCII_STRUCT_PARAMS \
  const char * outdir = RESTRICTED_IO_STRUCT.outdir; \
  const CCTK_INT  out_every = RESTRICTED_IO_STRUCT.out_every; \
  const CCTK_REAL  out_xline_y = RESTRICTED_IO_STRUCT.out_xline_y; \
  const CCTK_REAL  out_xline_z = RESTRICTED_IO_STRUCT.out_xline_z; \
  const CCTK_REAL  out_yline_x = RESTRICTED_IO_STRUCT.out_yline_x; \
  const CCTK_REAL  out_yline_z = RESTRICTED_IO_STRUCT.out_yline_z; \
  const CCTK_REAL  out_zline_x = RESTRICTED_IO_STRUCT.out_zline_x; \
  const CCTK_REAL  out_zline_y = RESTRICTED_IO_STRUCT.out_zline_y; \
  const CCTK_INT  out_xline_yi = RESTRICTED_IO_STRUCT.out_xline_yi; \
  const CCTK_INT  out_xline_zi = RESTRICTED_IO_STRUCT.out_xline_zi; \
  const CCTK_INT  out_yline_xi = RESTRICTED_IO_STRUCT.out_yline_xi; \
  const CCTK_INT  out_yline_zi = RESTRICTED_IO_STRUCT.out_yline_zi; \
  const CCTK_INT  out_zline_xi = RESTRICTED_IO_STRUCT.out_zline_xi; \
  const CCTK_INT  out_zline_yi = RESTRICTED_IO_STRUCT.out_zline_yi; \
  const CCTK_REAL  out_yzplane_x = RESTRICTED_IO_STRUCT.out_yzplane_x; \
  const CCTK_REAL  out_xzplane_y = RESTRICTED_IO_STRUCT.out_xzplane_y; \
  const CCTK_REAL  out_xyplane_z = RESTRICTED_IO_STRUCT.out_xyplane_z; \
  const CCTK_INT  out_xyplane_zi = RESTRICTED_IO_STRUCT.out_xyplane_zi; \
  const CCTK_INT  out_xzplane_yi = RESTRICTED_IO_STRUCT.out_xzplane_yi; \
  const CCTK_INT  out_yzplane_xi = RESTRICTED_IO_STRUCT.out_yzplane_xi; \
  const char * newverbose = RESTRICTED_IO_STRUCT.newverbose; \
  const CCTK_INT  new_filename_scheme = RESTRICTED_IO_STRUCT.new_filename_scheme; \
  const char * out_fileinfo = RESTRICTED_IO_STRUCT.out_fileinfo; \
  const void *cctk_pdummy_pointer;

#define USE_CCTK_PARAMETERS \
  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \
  USE_PRIVATE_IOASCII_STRUCT_PARAMS \
  cctk_pdummy_pointer = &outdir; \
  cctk_pdummy_pointer = &out_every; \
  cctk_pdummy_pointer = &out_xline_y; \
  cctk_pdummy_pointer = &out_xline_z; \
  cctk_pdummy_pointer = &out_yline_x; \
  cctk_pdummy_pointer = &out_yline_z; \
  cctk_pdummy_pointer = &out_zline_x; \
  cctk_pdummy_pointer = &out_zline_y; \
  cctk_pdummy_pointer = &out_xline_yi; \
  cctk_pdummy_pointer = &out_xline_zi; \
  cctk_pdummy_pointer = &out_yline_xi; \
  cctk_pdummy_pointer = &out_yline_zi; \
  cctk_pdummy_pointer = &out_zline_xi; \
  cctk_pdummy_pointer = &out_zline_yi; \
  cctk_pdummy_pointer = &out_yzplane_x; \
  cctk_pdummy_pointer = &out_xzplane_y; \
  cctk_pdummy_pointer = &out_xyplane_z; \
  cctk_pdummy_pointer = &out_xyplane_zi; \
  cctk_pdummy_pointer = &out_xzplane_yi; \
  cctk_pdummy_pointer = &out_yzplane_xi; \
  cctk_pdummy_pointer = &newverbose; \
  cctk_pdummy_pointer = &new_filename_scheme; \
  cctk_pdummy_pointer = &out_fileinfo; \
  cctk_pdummy_pointer = cctk_pdummy_pointer;

#endif
