#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  char * info;
  char * initialize_memory;
  char * partition;
  char * partition_1d_x;
  char * partition_2d_x;
  char * partition_2d_y;
  char * partition_3d_x;
  char * partition_3d_y;
  char * partition_3d_z;
  char * processor_topology;
  char * storage_verbose;
  CCTK_INT  cacheline_mult;
  CCTK_INT  enable_all_storage;
  CCTK_INT  local_nsize;
  CCTK_INT  local_nx;
  CCTK_INT  local_ny;
  CCTK_INT  local_nz;
  CCTK_INT  overloadabort;
  CCTK_INT  overloadarraygroupsizeb;
  CCTK_INT  overloadbarrier;
  CCTK_INT  overloaddisablegroupcomm;
  CCTK_INT  overloaddisablegroupstorage;
  CCTK_INT  overloadenablegroupcomm;
  CCTK_INT  overloadenablegroupstorage;
  CCTK_INT  overloadevolve;
  CCTK_INT  overloadexit;
  CCTK_INT  overloadgroupdynamicdata;
  CCTK_INT  overloadmyproc;
  CCTK_INT  overloadnprocs;
  CCTK_INT  overloadparallelinit;
  CCTK_INT  overloadquerygroupstorageb;
  CCTK_INT  overloadsyncgroup;
  CCTK_INT  padding_active;
  CCTK_INT  padding_address_spacing;
  CCTK_INT  padding_cacheline_bits;
  CCTK_INT  padding_size;
  CCTK_INT  processor_topology_1d_x;
  CCTK_INT  processor_topology_2d_x;
  CCTK_INT  processor_topology_2d_y;
  CCTK_INT  processor_topology_3d_x;
  CCTK_INT  processor_topology_3d_y;
  CCTK_INT  processor_topology_3d_z;
  CCTK_INT  storage_report_every;
  CCTK_INT  timer_output;
} PRIVATE_PUGH_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_PUGH_STRUCT_PARAMS \
  const char * info = PRIVATE_PUGH_STRUCT.info; \
  const char * initialize_memory = PRIVATE_PUGH_STRUCT.initialize_memory; \
  const char * partition = PRIVATE_PUGH_STRUCT.partition; \
  const char * partition_1d_x = PRIVATE_PUGH_STRUCT.partition_1d_x; \
  const char * partition_2d_x = PRIVATE_PUGH_STRUCT.partition_2d_x; \
  const char * partition_2d_y = PRIVATE_PUGH_STRUCT.partition_2d_y; \
  const char * partition_3d_x = PRIVATE_PUGH_STRUCT.partition_3d_x; \
  const char * partition_3d_y = PRIVATE_PUGH_STRUCT.partition_3d_y; \
  const char * partition_3d_z = PRIVATE_PUGH_STRUCT.partition_3d_z; \
  const char * processor_topology = PRIVATE_PUGH_STRUCT.processor_topology; \
  const char * storage_verbose = PRIVATE_PUGH_STRUCT.storage_verbose; \
  const CCTK_INT  cacheline_mult = PRIVATE_PUGH_STRUCT.cacheline_mult; \
  const CCTK_INT  enable_all_storage = PRIVATE_PUGH_STRUCT.enable_all_storage; \
  const CCTK_INT  local_nsize = PRIVATE_PUGH_STRUCT.local_nsize; \
  const CCTK_INT  local_nx = PRIVATE_PUGH_STRUCT.local_nx; \
  const CCTK_INT  local_ny = PRIVATE_PUGH_STRUCT.local_ny; \
  const CCTK_INT  local_nz = PRIVATE_PUGH_STRUCT.local_nz; \
  const CCTK_INT  overloadabort = PRIVATE_PUGH_STRUCT.overloadabort; \
  const CCTK_INT  overloadarraygroupsizeb = PRIVATE_PUGH_STRUCT.overloadarraygroupsizeb; \
  const CCTK_INT  overloadbarrier = PRIVATE_PUGH_STRUCT.overloadbarrier; \
  const CCTK_INT  overloaddisablegroupcomm = PRIVATE_PUGH_STRUCT.overloaddisablegroupcomm; \
  const CCTK_INT  overloaddisablegroupstorage = PRIVATE_PUGH_STRUCT.overloaddisablegroupstorage; \
  const CCTK_INT  overloadenablegroupcomm = PRIVATE_PUGH_STRUCT.overloadenablegroupcomm; \
  const CCTK_INT  overloadenablegroupstorage = PRIVATE_PUGH_STRUCT.overloadenablegroupstorage; \
  const CCTK_INT  overloadevolve = PRIVATE_PUGH_STRUCT.overloadevolve; \
  const CCTK_INT  overloadexit = PRIVATE_PUGH_STRUCT.overloadexit; \
  const CCTK_INT  overloadgroupdynamicdata = PRIVATE_PUGH_STRUCT.overloadgroupdynamicdata; \
  const CCTK_INT  overloadmyproc = PRIVATE_PUGH_STRUCT.overloadmyproc; \
  const CCTK_INT  overloadnprocs = PRIVATE_PUGH_STRUCT.overloadnprocs; \
  const CCTK_INT  overloadparallelinit = PRIVATE_PUGH_STRUCT.overloadparallelinit; \
  const CCTK_INT  overloadquerygroupstorageb = PRIVATE_PUGH_STRUCT.overloadquerygroupstorageb; \
  const CCTK_INT  overloadsyncgroup = PRIVATE_PUGH_STRUCT.overloadsyncgroup; \
  const CCTK_INT  padding_active = PRIVATE_PUGH_STRUCT.padding_active; \
  const CCTK_INT  padding_address_spacing = PRIVATE_PUGH_STRUCT.padding_address_spacing; \
  const CCTK_INT  padding_cacheline_bits = PRIVATE_PUGH_STRUCT.padding_cacheline_bits; \
  const CCTK_INT  padding_size = PRIVATE_PUGH_STRUCT.padding_size; \
  const CCTK_INT  processor_topology_1d_x = PRIVATE_PUGH_STRUCT.processor_topology_1d_x; \
  const CCTK_INT  processor_topology_2d_x = PRIVATE_PUGH_STRUCT.processor_topology_2d_x; \
  const CCTK_INT  processor_topology_2d_y = PRIVATE_PUGH_STRUCT.processor_topology_2d_y; \
  const CCTK_INT  processor_topology_3d_x = PRIVATE_PUGH_STRUCT.processor_topology_3d_x; \
  const CCTK_INT  processor_topology_3d_y = PRIVATE_PUGH_STRUCT.processor_topology_3d_y; \
  const CCTK_INT  processor_topology_3d_z = PRIVATE_PUGH_STRUCT.processor_topology_3d_z; \
  const CCTK_INT  storage_report_every = PRIVATE_PUGH_STRUCT.storage_report_every; \
  const CCTK_INT  timer_output = PRIVATE_PUGH_STRUCT.timer_output; \

#define USE_PRIVATE_PUGH_STRUCT_PARAMS \
  cctk_pdummy_pointer = &info; \
  cctk_pdummy_pointer = &initialize_memory; \
  cctk_pdummy_pointer = &partition; \
  cctk_pdummy_pointer = &partition_1d_x; \
  cctk_pdummy_pointer = &partition_2d_x; \
  cctk_pdummy_pointer = &partition_2d_y; \
  cctk_pdummy_pointer = &partition_3d_x; \
  cctk_pdummy_pointer = &partition_3d_y; \
  cctk_pdummy_pointer = &partition_3d_z; \
  cctk_pdummy_pointer = &processor_topology; \
  cctk_pdummy_pointer = &storage_verbose; \
  cctk_pdummy_pointer = &cacheline_mult; \
  cctk_pdummy_pointer = &enable_all_storage; \
  cctk_pdummy_pointer = &local_nsize; \
  cctk_pdummy_pointer = &local_nx; \
  cctk_pdummy_pointer = &local_ny; \
  cctk_pdummy_pointer = &local_nz; \
  cctk_pdummy_pointer = &overloadabort; \
  cctk_pdummy_pointer = &overloadarraygroupsizeb; \
  cctk_pdummy_pointer = &overloadbarrier; \
  cctk_pdummy_pointer = &overloaddisablegroupcomm; \
  cctk_pdummy_pointer = &overloaddisablegroupstorage; \
  cctk_pdummy_pointer = &overloadenablegroupcomm; \
  cctk_pdummy_pointer = &overloadenablegroupstorage; \
  cctk_pdummy_pointer = &overloadevolve; \
  cctk_pdummy_pointer = &overloadexit; \
  cctk_pdummy_pointer = &overloadgroupdynamicdata; \
  cctk_pdummy_pointer = &overloadmyproc; \
  cctk_pdummy_pointer = &overloadnprocs; \
  cctk_pdummy_pointer = &overloadparallelinit; \
  cctk_pdummy_pointer = &overloadquerygroupstorageb; \
  cctk_pdummy_pointer = &overloadsyncgroup; \
  cctk_pdummy_pointer = &padding_active; \
  cctk_pdummy_pointer = &padding_address_spacing; \
  cctk_pdummy_pointer = &padding_cacheline_bits; \
  cctk_pdummy_pointer = &padding_size; \
  cctk_pdummy_pointer = &processor_topology_1d_x; \
  cctk_pdummy_pointer = &processor_topology_2d_x; \
  cctk_pdummy_pointer = &processor_topology_2d_y; \
  cctk_pdummy_pointer = &processor_topology_3d_x; \
  cctk_pdummy_pointer = &processor_topology_3d_y; \
  cctk_pdummy_pointer = &processor_topology_3d_z; \
  cctk_pdummy_pointer = &storage_report_every; \
  cctk_pdummy_pointer = &timer_output; \


