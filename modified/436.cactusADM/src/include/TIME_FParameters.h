#define DECLARE_CCTK_PARAMETERS \
CCTK_STRING  timestep_method&&\
CCTK_INT timestep_outonly&&\
COMMON /timerest/timestep_method,timestep_outonly&&\
CCTK_REAL  courant_fac&&\
CCTK_REAL  dtfac&&\
CCTK_REAL  timestep&&\
CCTK_INT outtimestep_every&&\
COMMON /Timepriv/courant_fac,dtfac,timestep,outtimestep_every&&\


