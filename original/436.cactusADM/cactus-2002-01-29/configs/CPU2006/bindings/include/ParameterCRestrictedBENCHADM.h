#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  char * method;
} RESTRICTED_BENCHADM_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_BENCHADM_STRUCT_PARAMS \
  const char * method = RESTRICTED_BENCHADM_STRUCT.method; \

#define USE_RESTRICTED_BENCHADM_STRUCT_PARAMS \
  cctk_pdummy_pointer = &method; \


