#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_INT  ghost_size;
  CCTK_INT  ghost_size_x;
  CCTK_INT  ghost_size_y;
  CCTK_INT  ghost_size_z;
  CCTK_INT  global_nsize;
  CCTK_INT  global_nx;
  CCTK_INT  global_ny;
  CCTK_INT  global_nz;
  CCTK_INT  periodic;
  CCTK_INT  periodic_x;
  CCTK_INT  periodic_y;
  CCTK_INT  periodic_z;
} RESTRICTED_DRIVER_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_DRIVER_STRUCT_PARAMS \
  const CCTK_INT  ghost_size = RESTRICTED_DRIVER_STRUCT.ghost_size; \
  const CCTK_INT  ghost_size_x = RESTRICTED_DRIVER_STRUCT.ghost_size_x; \
  const CCTK_INT  ghost_size_y = RESTRICTED_DRIVER_STRUCT.ghost_size_y; \
  const CCTK_INT  ghost_size_z = RESTRICTED_DRIVER_STRUCT.ghost_size_z; \
  const CCTK_INT  global_nsize = RESTRICTED_DRIVER_STRUCT.global_nsize; \
  const CCTK_INT  global_nx = RESTRICTED_DRIVER_STRUCT.global_nx; \
  const CCTK_INT  global_ny = RESTRICTED_DRIVER_STRUCT.global_ny; \
  const CCTK_INT  global_nz = RESTRICTED_DRIVER_STRUCT.global_nz; \
  const CCTK_INT  periodic = RESTRICTED_DRIVER_STRUCT.periodic; \
  const CCTK_INT  periodic_x = RESTRICTED_DRIVER_STRUCT.periodic_x; \
  const CCTK_INT  periodic_y = RESTRICTED_DRIVER_STRUCT.periodic_y; \
  const CCTK_INT  periodic_z = RESTRICTED_DRIVER_STRUCT.periodic_z; \

#define USE_RESTRICTED_DRIVER_STRUCT_PARAMS \
  cctk_pdummy_pointer = &ghost_size; \
  cctk_pdummy_pointer = &ghost_size_x; \
  cctk_pdummy_pointer = &ghost_size_y; \
  cctk_pdummy_pointer = &ghost_size_z; \
  cctk_pdummy_pointer = &global_nsize; \
  cctk_pdummy_pointer = &global_nx; \
  cctk_pdummy_pointer = &global_ny; \
  cctk_pdummy_pointer = &global_nz; \
  cctk_pdummy_pointer = &periodic; \
  cctk_pdummy_pointer = &periodic_x; \
  cctk_pdummy_pointer = &periodic_y; \
  cctk_pdummy_pointer = &periodic_z; \


