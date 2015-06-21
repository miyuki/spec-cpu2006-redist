#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  char * bound;
  CCTK_INT  time_symmetric;
} PRIVATE_BENCHADM_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_BENCHADM_STRUCT_PARAMS \
  const char * bound = PRIVATE_BENCHADM_STRUCT.bound; \
  const CCTK_INT  time_symmetric = PRIVATE_BENCHADM_STRUCT.time_symmetric; \

#define USE_PRIVATE_BENCHADM_STRUCT_PARAMS \
  cctk_pdummy_pointer = &bound; \
  cctk_pdummy_pointer = &time_symmetric; \


