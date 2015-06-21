#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cctk_Config.h"
#include "CParameterStructNames.h"
#include "cctk_Misc.h"
#include "ParameterBindings.h"

struct 
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
