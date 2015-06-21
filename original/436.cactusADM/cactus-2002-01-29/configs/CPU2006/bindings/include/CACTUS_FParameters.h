#define DECLARE_CCTK_PARAMETERS \
CCTK_REAL  cctk_final_time&&\
CCTK_REAL  cctk_initial_time&&\
CCTK_STRING  terminate&&\
CCTK_INT cctk_itlast&&\
CCTK_INT terminate_next&&\
COMMON /Cactusrest/cctk_final_time,cctk_initial_time,terminate,cctk_itlast,terminate_next&&\
CCTK_STRING  cctk_run_title&&\
CCTK_STRING  cctk_timer_output&&\
CCTK_INT allow_mixeddim_gfs&&\
CCTK_INT cctk_brief_output&&\
CCTK_INT cctk_full_warnings&&\
CCTK_INT cctk_show_banners&&\
CCTK_INT cctk_show_schedule&&\
CCTK_INT cctk_strong_param_check&&\
CCTK_INT manual_cache_setup&&\
CCTK_INT manual_cache_size&&\
CCTK_INT manual_cacheline_bytes&&\
COMMON /Cactuspriv/cctk_run_title,cctk_timer_output,allow_mixeddim_gfs,cctk_brief_output,cctk_full_warnings,cctk_show_banners,cctk_show_schedule,cctk_strong_param_check,manual_cache_setup,manual_cache_size,manual_cacheline_bytes&&\


