#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cctk_Config.h"
#include "cctk_Misc.h"
#include "cctk_WarnLevel.h"
extern int CCTKi_BindingsCreateIOASCIIParameters(void);

extern int CCTKi_BindingsCreateBenchADMParameters(void);

extern int CCTKi_BindingsCreateCactusParameters(void);

extern int CCTKi_BindingsCreateBoundaryParameters(void);

extern int CCTKi_BindingsCreatePUGHParameters(void);

extern int CCTKi_BindingsCreateIOUtilParameters(void);

extern int CCTKi_BindingsCreatePUGHSlabParameters(void);

extern int CCTKi_BindingsCreateCartGrid3DParameters(void);

extern int CCTKi_BindingsCreateIOBasicParameters(void);

extern int CCTKi_BindingsCreatePUGHReduceParameters(void);

extern int CCTKi_BindingsCreateIDLinearWavesParameters(void);

extern int CCTKi_BindingsCreateTimeParameters(void);

extern int CCTKi_BindingsCreateEinsteinParameters(void);

extern int CCTKi_BindingsIOASCIIParameterExtensions(void);

extern int CCTKi_BindingsBenchADMParameterExtensions(void);

extern int CCTKi_BindingsCactusParameterExtensions(void);

extern int CCTKi_BindingsBoundaryParameterExtensions(void);

extern int CCTKi_BindingsPUGHParameterExtensions(void);

extern int CCTKi_BindingsIOUtilParameterExtensions(void);

extern int CCTKi_BindingsPUGHSlabParameterExtensions(void);

extern int CCTKi_BindingsCartGrid3DParameterExtensions(void);

extern int CCTKi_BindingsIOBasicParameterExtensions(void);

extern int CCTKi_BindingsPUGHReduceParameterExtensions(void);

extern int CCTKi_BindingsIDLinearWavesParameterExtensions(void);

extern int CCTKi_BindingsTimeParameterExtensions(void);

extern int CCTKi_BindingsEinsteinParameterExtensions(void);

int CCTKi_BindingsParametersInitialise(void);

int CCTKi_BindingsParametersInitialise(void)
{

  CCTKi_BindingsCreateIOASCIIParameters();

  CCTKi_BindingsCreateBenchADMParameters();

  CCTKi_BindingsCreateCactusParameters();

  CCTKi_BindingsCreateBoundaryParameters();

  CCTKi_BindingsCreatePUGHParameters();

  CCTKi_BindingsCreateIOUtilParameters();

  CCTKi_BindingsCreatePUGHSlabParameters();

  CCTKi_BindingsCreateCartGrid3DParameters();

  CCTKi_BindingsCreateIOBasicParameters();

  CCTKi_BindingsCreatePUGHReduceParameters();

  CCTKi_BindingsCreateIDLinearWavesParameters();

  CCTKi_BindingsCreateTimeParameters();

  CCTKi_BindingsCreateEinsteinParameters();

  CCTKi_BindingsIOASCIIParameterExtensions();

  CCTKi_BindingsBenchADMParameterExtensions();

  CCTKi_BindingsCactusParameterExtensions();

  CCTKi_BindingsBoundaryParameterExtensions();

  CCTKi_BindingsPUGHParameterExtensions();

  CCTKi_BindingsIOUtilParameterExtensions();

  CCTKi_BindingsPUGHSlabParameterExtensions();

  CCTKi_BindingsCartGrid3DParameterExtensions();

  CCTKi_BindingsIOBasicParameterExtensions();

  CCTKi_BindingsPUGHReduceParameterExtensions();

  CCTKi_BindingsIDLinearWavesParameterExtensions();

  CCTKi_BindingsTimeParameterExtensions();

  CCTKi_BindingsEinsteinParameterExtensions();

return 0;
}
