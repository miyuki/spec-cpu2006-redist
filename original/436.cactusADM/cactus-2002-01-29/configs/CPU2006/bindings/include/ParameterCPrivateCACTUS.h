#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  char * cctk_run_title;
  char * cctk_timer_output;
  CCTK_INT  allow_mixeddim_gfs;
  CCTK_INT  cctk_brief_output;
  CCTK_INT  cctk_full_warnings;
  CCTK_INT  cctk_show_banners;
  CCTK_INT  cctk_show_schedule;
  CCTK_INT  cctk_strong_param_check;
  CCTK_INT  manual_cache_setup;
  CCTK_INT  manual_cache_size;
  CCTK_INT  manual_cacheline_bytes;
} PRIVATE_CACTUS_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_CACTUS_STRUCT_PARAMS \
  const char * cctk_run_title = PRIVATE_CACTUS_STRUCT.cctk_run_title; \
  const char * cctk_timer_output = PRIVATE_CACTUS_STRUCT.cctk_timer_output; \
  const CCTK_INT  allow_mixeddim_gfs = PRIVATE_CACTUS_STRUCT.allow_mixeddim_gfs; \
  const CCTK_INT  cctk_brief_output = PRIVATE_CACTUS_STRUCT.cctk_brief_output; \
  const CCTK_INT  cctk_full_warnings = PRIVATE_CACTUS_STRUCT.cctk_full_warnings; \
  const CCTK_INT  cctk_show_banners = PRIVATE_CACTUS_STRUCT.cctk_show_banners; \
  const CCTK_INT  cctk_show_schedule = PRIVATE_CACTUS_STRUCT.cctk_show_schedule; \
  const CCTK_INT  cctk_strong_param_check = PRIVATE_CACTUS_STRUCT.cctk_strong_param_check; \
  const CCTK_INT  manual_cache_setup = PRIVATE_CACTUS_STRUCT.manual_cache_setup; \
  const CCTK_INT  manual_cache_size = PRIVATE_CACTUS_STRUCT.manual_cache_size; \
  const CCTK_INT  manual_cacheline_bytes = PRIVATE_CACTUS_STRUCT.manual_cacheline_bytes; \

#define USE_PRIVATE_CACTUS_STRUCT_PARAMS \
  cctk_pdummy_pointer = &cctk_run_title; \
  cctk_pdummy_pointer = &cctk_timer_output; \
  cctk_pdummy_pointer = &allow_mixeddim_gfs; \
  cctk_pdummy_pointer = &cctk_brief_output; \
  cctk_pdummy_pointer = &cctk_full_warnings; \
  cctk_pdummy_pointer = &cctk_show_banners; \
  cctk_pdummy_pointer = &cctk_show_schedule; \
  cctk_pdummy_pointer = &cctk_strong_param_check; \
  cctk_pdummy_pointer = &manual_cache_setup; \
  cctk_pdummy_pointer = &manual_cache_size; \
  cctk_pdummy_pointer = &manual_cacheline_bytes; \


