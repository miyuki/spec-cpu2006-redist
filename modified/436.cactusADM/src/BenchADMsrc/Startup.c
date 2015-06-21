#ifdef SPEC_CPU
# define THORN_IS_BenchADM
#endif /* SPEC_CPU */

#include "cctk.h"
#include "cctk_Parameters.h"
#include "Slicing.h"

static char *rcsid = "$Header: /cactus/CactusBench/BenchADM/src/Startup.c,v 1.5 2001/05/10 17:20:51 allen Exp $";

CCTK_FILEVERSION(CactusBench_BenchADM_Startup_c)

void BenchRegisterSlicing(void);

void BenchRegisterSlicing(void) 
{

  DECLARE_CCTK_PARAMETERS

  if (CCTK_Equals(evolution_system,"ADM")) 
  {
    Einstein_RegisterSlicing("geodesic");
  }

  USE_CCTK_PARAMETERS;   return;

}  
