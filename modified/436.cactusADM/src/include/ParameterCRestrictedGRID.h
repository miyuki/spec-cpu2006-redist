#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  dx;
  CCTK_REAL  dxyz;
  CCTK_REAL  dy;
  CCTK_REAL  dz;
  CCTK_REAL  xmax;
  CCTK_REAL  xmin;
  CCTK_REAL  xyzmax;
  CCTK_REAL  xyzmin;
  CCTK_REAL  ymax;
  CCTK_REAL  ymin;
  CCTK_REAL  zmax;
  CCTK_REAL  zmin;
  char * bitant_plane;
  char * domain;
  char * quadrant_direction;
  char * type;
  CCTK_INT  symmetry_xmax;
  CCTK_INT  symmetry_xmin;
  CCTK_INT  symmetry_ymax;
  CCTK_INT  symmetry_ymin;
  CCTK_INT  symmetry_zmax;
  CCTK_INT  symmetry_zmin;
} RESTRICTED_GRID_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_GRID_STRUCT_PARAMS \
  const CCTK_REAL  dx = RESTRICTED_GRID_STRUCT.dx; \
  const CCTK_REAL  dxyz = RESTRICTED_GRID_STRUCT.dxyz; \
  const CCTK_REAL  dy = RESTRICTED_GRID_STRUCT.dy; \
  const CCTK_REAL  dz = RESTRICTED_GRID_STRUCT.dz; \
  const CCTK_REAL  xmax = RESTRICTED_GRID_STRUCT.xmax; \
  const CCTK_REAL  xmin = RESTRICTED_GRID_STRUCT.xmin; \
  const CCTK_REAL  xyzmax = RESTRICTED_GRID_STRUCT.xyzmax; \
  const CCTK_REAL  xyzmin = RESTRICTED_GRID_STRUCT.xyzmin; \
  const CCTK_REAL  ymax = RESTRICTED_GRID_STRUCT.ymax; \
  const CCTK_REAL  ymin = RESTRICTED_GRID_STRUCT.ymin; \
  const CCTK_REAL  zmax = RESTRICTED_GRID_STRUCT.zmax; \
  const CCTK_REAL  zmin = RESTRICTED_GRID_STRUCT.zmin; \
  const char * bitant_plane = RESTRICTED_GRID_STRUCT.bitant_plane; \
  const char * domain = RESTRICTED_GRID_STRUCT.domain; \
  const char * quadrant_direction = RESTRICTED_GRID_STRUCT.quadrant_direction; \
  const char * type = RESTRICTED_GRID_STRUCT.type; \
  const CCTK_INT  symmetry_xmax = RESTRICTED_GRID_STRUCT.symmetry_xmax; \
  const CCTK_INT  symmetry_xmin = RESTRICTED_GRID_STRUCT.symmetry_xmin; \
  const CCTK_INT  symmetry_ymax = RESTRICTED_GRID_STRUCT.symmetry_ymax; \
  const CCTK_INT  symmetry_ymin = RESTRICTED_GRID_STRUCT.symmetry_ymin; \
  const CCTK_INT  symmetry_zmax = RESTRICTED_GRID_STRUCT.symmetry_zmax; \
  const CCTK_INT  symmetry_zmin = RESTRICTED_GRID_STRUCT.symmetry_zmin; \

#define USE_RESTRICTED_GRID_STRUCT_PARAMS \
  cctk_pdummy_pointer = &dx; \
  cctk_pdummy_pointer = &dxyz; \
  cctk_pdummy_pointer = &dy; \
  cctk_pdummy_pointer = &dz; \
  cctk_pdummy_pointer = &xmax; \
  cctk_pdummy_pointer = &xmin; \
  cctk_pdummy_pointer = &xyzmax; \
  cctk_pdummy_pointer = &xyzmin; \
  cctk_pdummy_pointer = &ymax; \
  cctk_pdummy_pointer = &ymin; \
  cctk_pdummy_pointer = &zmax; \
  cctk_pdummy_pointer = &zmin; \
  cctk_pdummy_pointer = &bitant_plane; \
  cctk_pdummy_pointer = &domain; \
  cctk_pdummy_pointer = &quadrant_direction; \
  cctk_pdummy_pointer = &type; \
  cctk_pdummy_pointer = &symmetry_xmax; \
  cctk_pdummy_pointer = &symmetry_xmin; \
  cctk_pdummy_pointer = &symmetry_ymax; \
  cctk_pdummy_pointer = &symmetry_ymin; \
  cctk_pdummy_pointer = &symmetry_zmax; \
  cctk_pdummy_pointer = &symmetry_zmin; \


