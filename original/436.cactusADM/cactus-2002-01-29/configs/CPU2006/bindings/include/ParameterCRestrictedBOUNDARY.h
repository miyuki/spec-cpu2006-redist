#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_INT  radpower;
} RESTRICTED_BOUNDARY_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_BOUNDARY_STRUCT_PARAMS \
  const CCTK_INT  radpower = RESTRICTED_BOUNDARY_STRUCT.radpower; \

#define USE_RESTRICTED_BOUNDARY_STRUCT_PARAMS \
  cctk_pdummy_pointer = &radpower; \


