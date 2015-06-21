#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_INT  avoid_origin;
  CCTK_INT  avoid_originx;
  CCTK_INT  avoid_originy;
  CCTK_INT  avoid_originz;
  CCTK_INT  no_origin;
  CCTK_INT  no_originx;
  CCTK_INT  no_originy;
  CCTK_INT  no_originz;
} PRIVATE_CARTGRID3D_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_CARTGRID3D_STRUCT_PARAMS \
  const CCTK_INT  avoid_origin = PRIVATE_CARTGRID3D_STRUCT.avoid_origin; \
  const CCTK_INT  avoid_originx = PRIVATE_CARTGRID3D_STRUCT.avoid_originx; \
  const CCTK_INT  avoid_originy = PRIVATE_CARTGRID3D_STRUCT.avoid_originy; \
  const CCTK_INT  avoid_originz = PRIVATE_CARTGRID3D_STRUCT.avoid_originz; \
  const CCTK_INT  no_origin = PRIVATE_CARTGRID3D_STRUCT.no_origin; \
  const CCTK_INT  no_originx = PRIVATE_CARTGRID3D_STRUCT.no_originx; \
  const CCTK_INT  no_originy = PRIVATE_CARTGRID3D_STRUCT.no_originy; \
  const CCTK_INT  no_originz = PRIVATE_CARTGRID3D_STRUCT.no_originz; \

#define USE_PRIVATE_CARTGRID3D_STRUCT_PARAMS \
  cctk_pdummy_pointer = &avoid_origin; \
  cctk_pdummy_pointer = &avoid_originx; \
  cctk_pdummy_pointer = &avoid_originy; \
  cctk_pdummy_pointer = &avoid_originz; \
  cctk_pdummy_pointer = &no_origin; \
  cctk_pdummy_pointer = &no_originx; \
  cctk_pdummy_pointer = &no_originy; \
  cctk_pdummy_pointer = &no_originz; \


