#ifdef SPEC_CPU
# define THORN_IS_BenchADM
#endif /* SPEC_CPU */
 /*@@
   @file      ParamCheck.c
   @date      November 1999
   @author    Gabrielle Allen
   @desc 
      Check parameters for ADM
   @enddesc 
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

#include "Einstein.h"

static char *rcsid = "$Header: /cactus/CactusBench/BenchADM/src/ParamCheck.c,v 1.9 2001/05/10 17:20:51 allen Exp $";

CCTK_FILEVERSION(CactusBench_BenchADM_ParamCheck_c)

void Bench_ParamCheck(CCTK_ARGUMENTS);

 /*@@
   @routine    ParamChecker
   @date       November 1999
   @author     Gabrielle Allen
   @desc 
      Check parameters for ADM
   @enddesc 
   @calls     
   @history 
 
   @endhistory 

@@*/

void Bench_ParamCheck(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  CCTK_INFO("Evolve using the ADM system");

  if (CCTK_Equals(method,"stagleap")) 
  {
    CCTK_INFO("  with staggered leapfrog");
  }

  /* Check for problems with courant */

  if (CCTK_Equals(timestep_method,"courant_speed") 
      || CCTK_Equals(timestep_method,"courant_time")) 
  {
    if (!timestep_outonly) 
    {
      if (CCTK_Equals(method,"stagleap")) 
      {
	CCTK_WARN(0,"Staggered Leapfrog and Courant are Incompatible");
      }
      else if (CCTK_Equals(method,"leapfrog")) 
      {
	CCTK_WARN(2, "Leapfrog and Courant are untested");
      }
    }
    
  }

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;  

}
