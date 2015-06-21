#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCRestrictedEINSTEIN.h"
#include "ParameterCPrivateEINSTEIN.h"
#include "ParameterCRestrictedGRID.h"
#include "ParameterCRestrictedTIME.h"

int CCTKi_BindingsCreateEinsteinParameters(void);

int CCTKi_BindingsCreateEinsteinParameters(void)
{
  CCTKi_ParameterCreate("advection", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How to finite difference advection terms on the shift", /* The description */
                  "center",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.advection),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "upwind1",  "First order upwind",
                  "upwind2",  "Second order upwind",
                  "center",  "Second order centered");

  CCTKi_ParameterCreate("einstein_register_slicing", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Register slicing management routines", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.einstein_register_slicing),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("evolution_system", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Spacetime system to evolve", /* The description */
                  "none",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.evolution_system),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "none",  "Do not use a spacetime" );

  CCTKi_ParameterCreate("gaussian_amplitude", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Amplitude of gaussian bump in initial lapse", /* The description */
                  "0.05",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.gaussian_amplitude),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("gaussian_sigma2", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Sigma**2 of gaussian bump in initial lapse", /* The description */
                  "0.05",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.gaussian_sigma2),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("initial_data", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Physical spacetime model to evolve", /* The description */
                  "flat",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.initial_data),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "flat",  "Flat Minkowski space");

  CCTKi_ParameterCreate("initial_lapse", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How to initialise the lapse", /* The description */
                  "one",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.initial_lapse),   /* The actual data pointer */
                  4       /* How many allowed ranges it has */,
                  "one",  "Set to unity",
                  "gaussian",  "Set to a gaussian bump (flatty)",
                  "isotropic",  "Set to isotropic lapse",
                  "psiminustwo",  "Set to psi to the minus two");

  CCTKi_ParameterCreate("initial_shift", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How to initialise the shift", /* The description */
                  "zero",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.initial_shift),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "zero",  "Set to zero",
                  "rotation",  "Set initial shift to rigid rotation");

  CCTKi_ParameterCreate("mixed_slicing", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "STRING",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Mixed slicing conditions, space separated, with *decreasing* importance", /* The description */
                  "none",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.mixed_slicing),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ".*",  "No default slicing");

  CCTKi_ParameterCreate("psiminustwo_cut", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Cut off for psiminustwo initial lapse", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.psiminustwo_cut),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0.0:1.0",  "0 means no cut off, 1 means a full cut off");

  CCTKi_ParameterCreate("rot_shift_att", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Use attenuation to zero the rotation shift at punctures", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.rot_shift_att),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("rot_shift_att_pow", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Half of the power to be used in the attenuation function", /* The description */
                  "1 ",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.rot_shift_att_pow),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "1:", " Larger than zero");

  CCTKi_ParameterCreate("rot_shift_att_sigma", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Width of the attenuation function", /* The description */
                  "0.2",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.rot_shift_att_sigma),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:", " Larger than zero please");

  CCTKi_ParameterCreate("rotation_omega", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_ALWAYS,              /* Is it steerable ?  */
                  "Angular velocity for initial rotation shift", /* The description */
                  "0.0",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.rotation_omega),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Anything goes");

  CCTKi_ParameterCreate("rotation_psipower", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Power of psi for initial rotation shift", /* The description */
                  "2",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.rotation_psipower),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:",  "Positive please");

  CCTKi_ParameterCreate("shift", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Which shift condition to use", /* The description */
                  "none",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.shift),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "none",  "No shift is used",
                  "zero",  "Zero shift is used",
                  "static",  "Shift remains static");

  CCTKi_ParameterCreate("slicing", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Slicing condition to implement", /* The description */
                  "geodesic",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.slicing),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "geodesic",  "Lapse is set to unity",
                  "static",  "Lapse is not evolved",
                  "mixed",  "Allow mixed slicing, specified in parameter mixed_slicing");

  CCTKi_ParameterCreate("slicing_verbose", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Print information on current slicing", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.slicing_verbose),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "yes",  "print slicing info",
                  "no",  "do not print slicing info");

  CCTKi_ParameterCreate("use_conformal", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Use conformal metric?", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.use_conformal),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("use_conformal_derivs", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Use derivatives of conformal metric?", /* The description */
                  "yes",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.use_conformal_derivs),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("use_mask", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "RESTRICTED",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Turn on storage for mask?", /* The description */
                  "no",  /* The default value */
                  &(RESTRICTED_EINSTEIN_STRUCT.use_mask),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("conformal_storage_all", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Assign storage for psi and derivatives", /* The description */
                  "yes",  /* The default value */
                  &(PRIVATE_EINSTEIN_STRUCT.conformal_storage_all),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("dtfac", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Courant factor for evolution", /* The description */
                  "0.5",  /* The default value */
                  &(PRIVATE_EINSTEIN_STRUCT.dtfac),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "Can  in principle be anything, although negative would be strange");

  CCTKi_ParameterCreate("gauge_speed", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Gauge speed for Courant calculation", /* The description */
                  "1.0",  /* The default value */
                  &(PRIVATE_EINSTEIN_STRUCT.gauge_speed),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "*:*",  "");

  CCTKi_ParameterCreate("rsquared_in_sphm", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "BOOLEAN",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Parameter in carttosphere, what does it ?", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_EINSTEIN_STRUCT.rsquared_in_sphm),   /* The actual data pointer */
                  0       /* How many allowed ranges it has */);

  CCTKi_ParameterCreate("wavecalc", /* The parameter name */
                        "Einstein",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "How to calculate the wavespeed for Courant", /* The description */
                  "three_point_inverse",  /* The default value */
                  &(PRIVATE_EINSTEIN_STRUCT.wavecalc),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "three_point_inverse",  "",
                  "seven_point",  "");

  return 0;
}

int CCTKi_BindingsEinsteinParameterExtensions(void);

int CCTKi_BindingsEinsteinParameterExtensions(void)
{
  return 0;
}
