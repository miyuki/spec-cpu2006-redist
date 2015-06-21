#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  dtfac;
  CCTK_REAL  gauge_speed;
  char * wavecalc;
  CCTK_INT  conformal_storage_all;
  CCTK_INT  rsquared_in_sphm;
} PRIVATE_EINSTEIN_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_EINSTEIN_STRUCT_PARAMS \
  const CCTK_REAL  dtfac = PRIVATE_EINSTEIN_STRUCT.dtfac; \
  const CCTK_REAL  gauge_speed = PRIVATE_EINSTEIN_STRUCT.gauge_speed; \
  const char * wavecalc = PRIVATE_EINSTEIN_STRUCT.wavecalc; \
  const CCTK_INT  conformal_storage_all = PRIVATE_EINSTEIN_STRUCT.conformal_storage_all; \
  const CCTK_INT  rsquared_in_sphm = PRIVATE_EINSTEIN_STRUCT.rsquared_in_sphm; \

#define USE_PRIVATE_EINSTEIN_STRUCT_PARAMS \
  cctk_pdummy_pointer = &dtfac; \
  cctk_pdummy_pointer = &gauge_speed; \
  cctk_pdummy_pointer = &wavecalc; \
  cctk_pdummy_pointer = &conformal_storage_all; \
  cctk_pdummy_pointer = &rsquared_in_sphm; \


