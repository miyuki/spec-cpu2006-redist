#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  amplitude;
  CCTK_REAL  wavecenter;
  CCTK_REAL  wavelength;
  CCTK_REAL  wavephi;
  CCTK_REAL  wavepulse;
  CCTK_REAL  wavetheta;
  char * packet;
  char * parity;
  char * teuk_no_vee;
  char * wavesgoing;
  CCTK_INT  mvalue;
} PRIVATE_IDLINEARWAVES_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_IDLINEARWAVES_STRUCT_PARAMS \
  const CCTK_REAL  amplitude = PRIVATE_IDLINEARWAVES_STRUCT.amplitude; \
  const CCTK_REAL  wavecenter = PRIVATE_IDLINEARWAVES_STRUCT.wavecenter; \
  const CCTK_REAL  wavelength = PRIVATE_IDLINEARWAVES_STRUCT.wavelength; \
  const CCTK_REAL  wavephi = PRIVATE_IDLINEARWAVES_STRUCT.wavephi; \
  const CCTK_REAL  wavepulse = PRIVATE_IDLINEARWAVES_STRUCT.wavepulse; \
  const CCTK_REAL  wavetheta = PRIVATE_IDLINEARWAVES_STRUCT.wavetheta; \
  const char * packet = PRIVATE_IDLINEARWAVES_STRUCT.packet; \
  const char * parity = PRIVATE_IDLINEARWAVES_STRUCT.parity; \
  const char * teuk_no_vee = PRIVATE_IDLINEARWAVES_STRUCT.teuk_no_vee; \
  const char * wavesgoing = PRIVATE_IDLINEARWAVES_STRUCT.wavesgoing; \
  const CCTK_INT  mvalue = PRIVATE_IDLINEARWAVES_STRUCT.mvalue; \

#define USE_PRIVATE_IDLINEARWAVES_STRUCT_PARAMS \
  cctk_pdummy_pointer = &amplitude; \
  cctk_pdummy_pointer = &wavecenter; \
  cctk_pdummy_pointer = &wavelength; \
  cctk_pdummy_pointer = &wavephi; \
  cctk_pdummy_pointer = &wavepulse; \
  cctk_pdummy_pointer = &wavetheta; \
  cctk_pdummy_pointer = &packet; \
  cctk_pdummy_pointer = &parity; \
  cctk_pdummy_pointer = &teuk_no_vee; \
  cctk_pdummy_pointer = &wavesgoing; \
  cctk_pdummy_pointer = &mvalue; \


