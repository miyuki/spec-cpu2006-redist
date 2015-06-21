#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdarg.h>

#include "cctk_Config.h"
#include "cctk_Constants.h"
#include "ParameterBindings.h"
#include "CParameterStructNames.h"
#include "ParameterCPrivateIDLINEARWAVES.h"
#include "ParameterCRestrictedEINSTEIN.h"

int CCTKi_BindingsCreateIDLinearWavesParameters(void);

int CCTKi_BindingsCreateIDLinearWavesParameters(void)
{
  CCTKi_ParameterCreate("amplitude", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Amplitude of the wave: both for teuk and plane", /* The description */
                  "0.001",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.amplitude),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:",  "positive amplitude");

  CCTKi_ParameterCreate("mvalue", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "INT",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "m value for teukwaves waves: integer from -2 to 2", /* The description */
                  "0",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.mvalue),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "-2:2",  "implemented : m = -2..2");

  CCTKi_ParameterCreate("packet", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Packet for teukwaves: eppley,evans,square", /* The description */
                  "eppley",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.packet),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "eppley",  "Eppley type",
                  "evans",  "Evans type",
                  "square",  "Square type");

  CCTKi_ParameterCreate("parity", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Parity for teukwaves: even or odd", /* The description */
                  "even",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.parity),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "even",  "even parity",
                  "odd",  "odd parity");

  CCTKi_ParameterCreate("teuk_no_vee", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Initialize Teuk. waves with V=0?", /* The description */
                  "no",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.teuk_no_vee),   /* The actual data pointer */
                  2       /* How many allowed ranges it has */,
                  "no",  "Bona Masso setting",
                  "yes",  "Bona Masso setting");

  CCTKi_ParameterCreate("wavecenter", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "linears waves thingie", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.wavecenter),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("wavelength", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "linearwaves wave length", /* The description */
                  "2.0",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.wavelength),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:",  "positive wavelength");

  CCTKi_ParameterCreate("wavephi", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Phi angle for planewaves", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.wavephi),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  CCTKi_ParameterCreate("wavepulse", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "planewaves thingy for the gaussian pulse", /* The description */
                  "1.0",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.wavepulse),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  "0:",  "positive pulse");

  CCTKi_ParameterCreate("wavesgoing", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "KEYWORD",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "in and outgoing waves...", /* The description */
                  "both",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.wavesgoing),   /* The actual data pointer */
                  3       /* How many allowed ranges it has */,
                  "in",  "Ingoing wave",
                  "out",  "Outgoing wave"	,
                  "both",  "In and outgoing wave");

  CCTKi_ParameterCreate("wavetheta", /* The parameter name */
                        "IDLinearWaves",     /* The thorn          */
                        "REAL",       /* The parameter type*/
                        "PRIVATE",     /* The scoping block  */
                  CCTK_STEERABLE_NEVER,              /* Is it steerable ?  */
                  "Theta angle for planewaves", /* The description */
                  "0.0",  /* The default value */
                  &(PRIVATE_IDLINEARWAVES_STRUCT.wavetheta),   /* The actual data pointer */
                  1       /* How many allowed ranges it has */,
                  ":",  "");

  return 0;
}

int CCTKi_BindingsIDLinearWavesParameterExtensions(void);

int CCTKi_BindingsIDLinearWavesParameterExtensions(void)
{
  CCTKi_ParameterAddRange("EINSTEIN",
                          "initial_data",
                          "IDLinearWaves",
                          "teukwaves",
                           "linear waves initial data- Teukolsky waves");

  CCTKi_ParameterAddRange("EINSTEIN",
                          "initial_data",
                          "IDLinearWaves",
                          "planewaves",
                           "linear waves initial data- plane waves");

  return 0;
}
