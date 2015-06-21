#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedDRIVER.h"
#include "ParameterCPrivatePUGH.h"
#include "ParameterCRestrictedCACTUS.h"

int CCTKi_BindingsCreatePUGHParameters(void);

int CCTKi_BindingsCreatePUGHParameters(void)
{
  CCTKi_ParameterCreate("ghost_size", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The width of the ghost zone in each direction", /* The description */
                  "-1",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.ghost_size),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Any positive number to override the ghost_size_[xyz] parameters");

  CCTKi_ParameterCreate("ghost_size_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The width of the ghost zone in the x direction", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.ghost_size_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("ghost_size_y", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The width of the ghost zone in the y direction", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.ghost_size_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("ghost_size_z", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The width of the ghost zone in the z direction", /* The description */
                  "1",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.ghost_size_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Must be a positive integer");

  CCTKi_ParameterCreate("global_nsize", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in each spatial direction", /* The description */
                  "-1",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.global_nsize),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Grid of this size in each dir distributed across all processors");

  CCTKi_ParameterCreate("global_nx", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in the x direction", /* The description */
                  "10",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.global_nx),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Grid of this size distributed across all processors");

  CCTKi_ParameterCreate("global_ny", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in the y direction", /* The description */
                  "10",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.global_ny),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Grid of this size distributed across all processors");

  CCTKi_ParameterCreate("global_nz", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in the z direction", /* The description */
                  "10",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.global_nz),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Grid of this size distributed across all processors");

  CCTKi_ParameterCreate("periodic", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Periodic boundary conditions", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.periodic),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("periodic_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Periodic boundary conditions in x-direction", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.periodic_x),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("periodic_y", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Periodic boundary conditions in y-direction", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.periodic_y),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("periodic_z", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Periodic boundary conditions in z-direction", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_DRIVER_STRUCT.periodic_z),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("cacheline_mult", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Multiplier for cacheline number", /* The description */
                  "4001",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.cacheline_mult),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",   "Any positive number");

  CCTKi_ParameterCreate("enable_all_storage", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Enable storage for all GFs?", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.enable_all_storage),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("info", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Provide additional information about what PUGH is doing", /* The description */
                  "none",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.info),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "none",  "No extra information",
                  "load",  "Load on each processor");

  CCTKi_ParameterCreate("initialize_memory", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How to initialize memory for grid variables at allocation time", /* The description */
                  "none",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.initialize_memory),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "none",  "Do not initialize storage for allocated grid variables (default)",
                  "zero",  "Zero out all elements of all allocated grid variables",
                  "NaN",  "Set all elements of allocated floating point grid variables to Not-a-Number values");

  CCTKi_ParameterCreate("local_nsize", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in each spatial direction", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.local_nsize),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Grid of this size in each dir on each processor");

  CCTKi_ParameterCreate("local_nx", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in the x direction", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.local_nx),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Grid of this size on each processor");

  CCTKi_ParameterCreate("local_ny", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in the y direction", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.local_ny),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Grid of this size on each processor");

  CCTKi_ParameterCreate("local_nz", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "The size of the grid in the z direction", /* The description */
                  "-1",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.local_nz),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-1:*",  "Grid of this size on each processor");

  CCTKi_ParameterCreate("overloadabort", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload Abort driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadabort),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadarraygroupsizeb", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload ArrayGroupSizeB driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadarraygroupsizeb),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadbarrier", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload Barrier driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadbarrier),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloaddisablegroupcomm", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload DisableGroupComm driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloaddisablegroupcomm),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloaddisablegroupstorage", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload DisableGroupStorage driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloaddisablegroupstorage),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadenablegroupcomm", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload EnableGroupComm driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadenablegroupcomm),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadenablegroupstorage", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload EnableGroupStorage driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadenablegroupstorage),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadevolve", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload Evolve driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadevolve),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadexit", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload Exit driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadexit),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadgroupdynamicdata", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload GroupDynamicData driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadgroupdynamicdata),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadmyproc", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload MyProc driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadmyproc),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadnprocs", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload nProcs driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadnprocs),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadparallelinit", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload ParallelInit driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadparallelinit),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadquerygroupstorageb", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload QueryGroupStorageB driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadquerygroupstorageb),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("overloadsyncgroup", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Overload SyncGroup driver function", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.overloadsyncgroup),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("padding_active", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Padd 3D arrays so they line up on cache lines?", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.padding_active),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("padding_address_spacing", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Number of CCTK_REALs to space starting addresses in lowest padding_cacheline_bits bits", /* The description */
                  "24",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.padding_address_spacing),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",   "Any positive number");

  CCTKi_ParameterCreate("padding_cacheline_bits", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Number of bits which have to be unique to padd properly for cache lines", /* The description */
                  "12",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.padding_cacheline_bits),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:*",  "Any positive number");

  CCTKi_ParameterCreate("padding_size", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How many CCTK_REALs to pad by; we have to noodle around with the starting address, so if there isn't enough space we may not be able to pad", /* The description */
                  "4112",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.padding_size),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",   "Any positive number");

  CCTKi_ParameterCreate("partition", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Is the partition manual", /* The description */
                  "automatic",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "automatic",  "even",
                  "manual",  "specified by partition_XYZ ..");

  CCTKi_ParameterCreate("partition_1d_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Tells how to partition on direction X", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition_1d_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches anything");

  CCTKi_ParameterCreate("partition_2d_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Tells how to partition on direction X", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition_2d_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches anything");

  CCTKi_ParameterCreate("partition_2d_y", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Tells how to partition on direction y", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition_2d_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches anything");

  CCTKi_ParameterCreate("partition_3d_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Tells how to partition on direction X", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition_3d_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches anything");

  CCTKi_ParameterCreate("partition_3d_y", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Tells how to partition on direction y", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition_3d_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches anything");

  CCTKi_ParameterCreate("partition_3d_z", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Tells how to partition on direction z", /* The description */
                  "",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.partition_3d_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "A regex which matches anything");

  CCTKi_ParameterCreate("processor_topology", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How to determine the processor topology", /* The description */
                  "automatic",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "manual",  "Specified by proc_top_nx etc",
                  "automatic",  "Automatically generated");

  CCTKi_ParameterCreate("processor_topology_1d_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "No of Procs in X direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology_1d_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "See proc_topology");

  CCTKi_ParameterCreate("processor_topology_2d_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "No of Procs in X direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology_2d_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "See proc_topology");

  CCTKi_ParameterCreate("processor_topology_2d_y", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "No of Procs in X direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology_2d_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "See proc_topology");

  CCTKi_ParameterCreate("processor_topology_3d_x", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "No of Procs in X direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology_3d_x),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "See proc_topology");

  CCTKi_ParameterCreate("processor_topology_3d_y", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "No of Procs in X direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology_3d_y),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "See proc_topology");

  CCTKi_ParameterCreate("processor_topology_3d_z", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "No of Procs in X direction", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.processor_topology_3d_z),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "See proc_topology");

  CCTKi_ParameterCreate("storage_report_every", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "How often to provide a report on storage information", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.storage_report_every),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "0:0",  "Never report",
                  "1:*",  "Report at intervals");

  CCTKi_ParameterCreate("storage_verbose", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Report on memory assignment", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.storage_verbose),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "yes",  "Standard storage information",
                  "report",  "Provide a report of storage every storage_report_every iterations and at termination",
                  "no",  "Provide no information");

  CCTKi_ParameterCreate("timer_output", /* The parameter name */
                        "PUGH",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Print time spent in communication", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_PUGH_STRUCT.timer_output),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  return 0;
}

int CCTKi_BindingsPUGHParameterExtensions(void);

int CCTKi_BindingsPUGHParameterExtensions(void)
{
  return 0;
}
