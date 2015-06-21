#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  char * outInfo_reductions;
  char * outInfo_vars;
  char * outScalar_reductions;
  char * outScalar_style;
  char * outScalar_vars;
  char * out_format;
  char * outdirScalar;
  CCTK_INT  outInfo_every;
  CCTK_INT  outScalar_every;
} PRIVATE_IOBASIC_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_IOBASIC_STRUCT_PARAMS \
  const char * outInfo_reductions = PRIVATE_IOBASIC_STRUCT.outInfo_reductions; \
  const char * outInfo_vars = PRIVATE_IOBASIC_STRUCT.outInfo_vars; \
  const char * outScalar_reductions = PRIVATE_IOBASIC_STRUCT.outScalar_reductions; \
  const char * outScalar_style = PRIVATE_IOBASIC_STRUCT.outScalar_style; \
  const char * outScalar_vars = PRIVATE_IOBASIC_STRUCT.outScalar_vars; \
  const char * out_format = PRIVATE_IOBASIC_STRUCT.out_format; \
  const char * outdirScalar = PRIVATE_IOBASIC_STRUCT.outdirScalar; \
  const CCTK_INT  outInfo_every = PRIVATE_IOBASIC_STRUCT.outInfo_every; \
  const CCTK_INT  outScalar_every = PRIVATE_IOBASIC_STRUCT.outScalar_every; \

#define USE_PRIVATE_IOBASIC_STRUCT_PARAMS \
  cctk_pdummy_pointer = &outInfo_reductions; \
  cctk_pdummy_pointer = &outInfo_vars; \
  cctk_pdummy_pointer = &outScalar_reductions; \
  cctk_pdummy_pointer = &outScalar_style; \
  cctk_pdummy_pointer = &outScalar_vars; \
  cctk_pdummy_pointer = &out_format; \
  cctk_pdummy_pointer = &outdirScalar; \
  cctk_pdummy_pointer = &outInfo_every; \
  cctk_pdummy_pointer = &outScalar_every; \


