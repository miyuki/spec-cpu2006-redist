#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cctk_Config.h"
#include "CParameterStructNames.h"
#include "cctk_Misc.h"
#include "ParameterBindings.h"

struct 
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
