#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  courant_fac;
  CCTK_REAL  dtfac;
  CCTK_REAL  timestep;
  CCTK_INT  outtimestep_every;
} PRIVATE_TIME_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_PRIVATE_TIME_STRUCT_PARAMS \
  const CCTK_REAL  courant_fac = PRIVATE_TIME_STRUCT.courant_fac; \
  const CCTK_REAL  dtfac = PRIVATE_TIME_STRUCT.dtfac; \
  const CCTK_REAL  timestep = PRIVATE_TIME_STRUCT.timestep; \
  const CCTK_INT  outtimestep_every = PRIVATE_TIME_STRUCT.outtimestep_every; \

#define USE_PRIVATE_TIME_STRUCT_PARAMS \
  cctk_pdummy_pointer = &courant_fac; \
  cctk_pdummy_pointer = &dtfac; \
  cctk_pdummy_pointer = &timestep; \
  cctk_pdummy_pointer = &outtimestep_every; \


