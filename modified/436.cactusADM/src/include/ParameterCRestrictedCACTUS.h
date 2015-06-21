#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  cctk_final_time;
  CCTK_REAL  cctk_initial_time;
  char * terminate;
  CCTK_INT  cctk_itlast;
  CCTK_INT  terminate_next;
} RESTRICTED_CACTUS_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_CACTUS_STRUCT_PARAMS \
  const CCTK_REAL  cctk_final_time = RESTRICTED_CACTUS_STRUCT.cctk_final_time; \
  const CCTK_REAL  cctk_initial_time = RESTRICTED_CACTUS_STRUCT.cctk_initial_time; \
  const char * terminate = RESTRICTED_CACTUS_STRUCT.terminate; \
  const CCTK_INT  cctk_itlast = RESTRICTED_CACTUS_STRUCT.cctk_itlast; \
  const CCTK_INT  terminate_next = RESTRICTED_CACTUS_STRUCT.terminate_next; \

#define USE_RESTRICTED_CACTUS_STRUCT_PARAMS \
  cctk_pdummy_pointer = &cctk_final_time; \
  cctk_pdummy_pointer = &cctk_initial_time; \
  cctk_pdummy_pointer = &terminate; \
  cctk_pdummy_pointer = &cctk_itlast; \
  cctk_pdummy_pointer = &terminate_next; \


