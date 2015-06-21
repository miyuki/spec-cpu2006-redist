#ifdef __cplusplus
extern "C"
{
#endif

extern struct 
{
  CCTK_REAL  gaussian_amplitude;
  CCTK_REAL  gaussian_sigma2;
  CCTK_REAL  psiminustwo_cut;
  CCTK_REAL  rot_shift_att_sigma;
  CCTK_REAL  rotation_omega;
  char * advection;
  char * evolution_system;
  char * initial_data;
  char * initial_lapse;
  char * initial_shift;
  char * mixed_slicing;
  char * shift;
  char * slicing;
  char * slicing_verbose;
  CCTK_INT  einstein_register_slicing;
  CCTK_INT  rot_shift_att;
  CCTK_INT  rot_shift_att_pow;
  CCTK_INT  rotation_psipower;
  CCTK_INT  use_conformal;
  CCTK_INT  use_conformal_derivs;
  CCTK_INT  use_mask;
} RESTRICTED_EINSTEIN_STRUCT;

#ifdef __cplusplus
}
#endif

#define DECLARE_RESTRICTED_EINSTEIN_STRUCT_PARAMS \
  const CCTK_REAL  gaussian_amplitude = RESTRICTED_EINSTEIN_STRUCT.gaussian_amplitude; \
  const CCTK_REAL  gaussian_sigma2 = RESTRICTED_EINSTEIN_STRUCT.gaussian_sigma2; \
  const CCTK_REAL  psiminustwo_cut = RESTRICTED_EINSTEIN_STRUCT.psiminustwo_cut; \
  const CCTK_REAL  rot_shift_att_sigma = RESTRICTED_EINSTEIN_STRUCT.rot_shift_att_sigma; \
  const CCTK_REAL  rotation_omega = RESTRICTED_EINSTEIN_STRUCT.rotation_omega; \
  const char * advection = RESTRICTED_EINSTEIN_STRUCT.advection; \
  const char * evolution_system = RESTRICTED_EINSTEIN_STRUCT.evolution_system; \
  const char * initial_data = RESTRICTED_EINSTEIN_STRUCT.initial_data; \
  const char * initial_lapse = RESTRICTED_EINSTEIN_STRUCT.initial_lapse; \
  const char * initial_shift = RESTRICTED_EINSTEIN_STRUCT.initial_shift; \
  const char * mixed_slicing = RESTRICTED_EINSTEIN_STRUCT.mixed_slicing; \
  const char * shift = RESTRICTED_EINSTEIN_STRUCT.shift; \
  const char * slicing = RESTRICTED_EINSTEIN_STRUCT.slicing; \
  const char * slicing_verbose = RESTRICTED_EINSTEIN_STRUCT.slicing_verbose; \
  const CCTK_INT  einstein_register_slicing = RESTRICTED_EINSTEIN_STRUCT.einstein_register_slicing; \
  const CCTK_INT  rot_shift_att = RESTRICTED_EINSTEIN_STRUCT.rot_shift_att; \
  const CCTK_INT  rot_shift_att_pow = RESTRICTED_EINSTEIN_STRUCT.rot_shift_att_pow; \
  const CCTK_INT  rotation_psipower = RESTRICTED_EINSTEIN_STRUCT.rotation_psipower; \
  const CCTK_INT  use_conformal = RESTRICTED_EINSTEIN_STRUCT.use_conformal; \
  const CCTK_INT  use_conformal_derivs = RESTRICTED_EINSTEIN_STRUCT.use_conformal_derivs; \
  const CCTK_INT  use_mask = RESTRICTED_EINSTEIN_STRUCT.use_mask; \

#define USE_RESTRICTED_EINSTEIN_STRUCT_PARAMS \
  cctk_pdummy_pointer = &gaussian_amplitude; \
  cctk_pdummy_pointer = &gaussian_sigma2; \
  cctk_pdummy_pointer = &psiminustwo_cut; \
  cctk_pdummy_pointer = &rot_shift_att_sigma; \
  cctk_pdummy_pointer = &rotation_omega; \
  cctk_pdummy_pointer = &advection; \
  cctk_pdummy_pointer = &evolution_system; \
  cctk_pdummy_pointer = &initial_data; \
  cctk_pdummy_pointer = &initial_lapse; \
  cctk_pdummy_pointer = &initial_shift; \
  cctk_pdummy_pointer = &mixed_slicing; \
  cctk_pdummy_pointer = &shift; \
  cctk_pdummy_pointer = &slicing; \
  cctk_pdummy_pointer = &slicing_verbose; \
  cctk_pdummy_pointer = &einstein_register_slicing; \
  cctk_pdummy_pointer = &rot_shift_att; \
  cctk_pdummy_pointer = &rot_shift_att_pow; \
  cctk_pdummy_pointer = &rotation_psipower; \
  cctk_pdummy_pointer = &use_conformal; \
  cctk_pdummy_pointer = &use_conformal_derivs; \
  cctk_pdummy_pointer = &use_mask; \


