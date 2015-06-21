#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  char * timestep_method;
  CCTK_INT  timestep_outonly;
} RESTRICTED_TIME_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_TIME_STRUCT_PARAMS \
  const char * timestep_method = RESTRICTED_TIME_STRUCT.timestep_method; \
  const CCTK_INT  timestep_outonly = RESTRICTED_TIME_STRUCT.timestep_outonly; \

#define USE_RESTRICTED_TIME_STRUCT_PARAMS \
  cctk_pdummy_pointer = &timestep_method; \
  cctk_pdummy_pointer = &timestep_outonly; \


