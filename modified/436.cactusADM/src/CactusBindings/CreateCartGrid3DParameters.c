#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedGRID.h"
#include "ParameterCPrivateCARTGRID3D.h"
#include "ParameterCRestrictedDRIVER.h"

int CCTKi_BindingsCreateCartGrid3DParameters(void);

int CCTKi_BindingsCreateCartGrid3DParameters(void)
{
  CCTKi_ParameterCreate("bitant_plane", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Plane defining bitant domain", /* The description */
                  "xy",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.bitant_plane),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "xy",  "xy-plane",
                  "xz",  "xz-plane",
                  "yz",  "yz-plane");

  CCTKi_ParameterCreate("domain", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Domain type", /* The description */
                  "full",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.domain),   /* The actual data pointer */
                  4       /* How many allowed ranges it has */,
                  "octant",  "Use an octant about the origin",
                  "quadrant",  "Use a quadrant in x-y plane",
                  "bitant",  "Use a bitant about the x-y plane",
                  "full",  "Use the full domain");

  CCTKi_ParameterCreate("dx", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coarse grid spacing in x-direction", /* The description */
                  "0.3",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.dx),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Positive");

  CCTKi_ParameterCreate("dxyz", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coarse grid spacing in x,y,z-directions", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.dxyz),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Positive");

  CCTKi_ParameterCreate("dy", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coarse grid spacing in y-direction", /* The description */
                  "0.3",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.dy),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Positive");

  CCTKi_ParameterCreate("dz", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coarse grid spacing in z-direction", /* The description */
                  "0.3",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.dz),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:*",  "Positive");

  CCTKi_ParameterCreate("quadrant_direction", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Direction defining quadrant domain", /* The description */
                  "z",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.quadrant_direction),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "x",  "x-direction",
                  "y",  "y-direction",
                  "z",  "z-direction");

  CCTKi_ParameterCreate("symmetry_xmax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Symmetry boundary condition on upper x boundary", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.symmetry_xmax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Logical");

  CCTKi_ParameterCreate("symmetry_xmin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Symmetry boundary condition on lower x boundary", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.symmetry_xmin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Logical");

  CCTKi_ParameterCreate("symmetry_ymax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Symmetry boundary condition on upper y boundary", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.symmetry_ymax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Logical");

  CCTKi_ParameterCreate("symmetry_ymin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Symmetry boundary condition on lower y boundary", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.symmetry_ymin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Logical");

  CCTKi_ParameterCreate("symmetry_zmax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Symmetry boundary condition on upper z boundary", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.symmetry_zmax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Logical");

  CCTKi_ParameterCreate("symmetry_zmin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Symmetry boundary condition on lower z boundary", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.symmetry_zmin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Logical");

  CCTKi_ParameterCreate("type", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Grid type", /* The description */
                  "box",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.type),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "box",  "Box grid from -0.5 to 0.5",
                  "byrange",  "Specify min and max values",
                  "byspacing",  "Specify grid spacings");

  CCTKi_ParameterCreate("xmax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate maximum in x-direction", /* The description */
                  "1.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.xmax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("xmin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate minimum in x-direction", /* The description */
                  "-1.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.xmin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("xyzmax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate maximum in xyz-directions", /* The description */
                  "-424242",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.xyzmax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("xyzmin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate minimum in x,y,z-directions", /* The description */
                  "-424242",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.xyzmin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("ymax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate maximum in y-direction", /* The description */
                  "1.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.ymax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("ymin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate minimum in y-direction", /* The description */
                  "-1.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.ymin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("zmax", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate maximum in z-direction", /* The description */
                  "1.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.zmax),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("zmin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Coordinate minimum in z-direction", /* The description */
                  "-1.0",  /* The default value */
                  &(RESTRICTED_GRID_STRUCT.zmin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything");

  CCTKi_ParameterCreate("avoid_origin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Don't place grid points on the coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.avoid_origin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("avoid_originx", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Don't place grid points on the x-coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.avoid_originx),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("avoid_originy", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Don't place grid points on the y-coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.avoid_originy),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("avoid_originz", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Don't place grid points on the z-coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.avoid_originz),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("no_origin", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "DEPRECATED: Don't place grid points on the coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.no_origin),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("no_originx", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "DEPRECATED: Don't place grid points on the x-coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.no_originx),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("no_originy", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "DEPRECATED: Don't place grid points on the y-coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.no_originy),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("no_originz", /* The parameter name */
                        "CartGrid3D",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "DEPRECATED: Don't place grid points on the z-coordinate origin/axes", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_CARTGRID3D_STRUCT.no_originz),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  return 0;
}

int CCTKi_BindingsCartGrid3DParameterExtensions(void);

int CCTKi_BindingsCartGrid3DParameterExtensions(void)
{
  return 0;
}
