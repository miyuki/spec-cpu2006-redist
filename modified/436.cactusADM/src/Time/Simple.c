#ifdef SPEC_CPU
# define THORN_IS_Time
#endif /* SPEC_CPU */
 /*@@
   @file      Simple.c
   @date      September 4 1999
   @author    Gabrielle Allen
   @desc 
   Standard specification of timestep
   @enddesc 
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusBase/Time/src/Simple.c,v 1.11 2001/05/10 12:35:44 goodale Exp $";

CCTK_FILEVERSION(CactusBase_Time_Simple_c)

void Time_Simple(CCTK_ARGUMENTS);

void Time_Simple(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_PARAMETERS
  DECLARE_CCTK_ARGUMENTS

  CCTK_REAL min_spacing=0;

  /* Calculate the minimum grid spacing */
  if (cctk_dim>=1)
  {
    min_spacing = cctk_delta_space[0];
  }

  if (cctk_dim>=2)
  {
    min_spacing = (min_spacing<cctk_delta_space[1] ?
	min_spacing : cctk_delta_space[1]);
  }
  
  if (cctk_dim>=3)
  {
    min_spacing = (min_spacing<cctk_delta_space[2] ?
	min_spacing : cctk_delta_space[2]);
  }

  if (cctk_dim>=4)
  {
    CCTK_WARN(0,"Time Step not defined for greater than 4 dimensions");
  }

  /* Calculate the timestep */
  cctkGH->cctk_delta_time = dtfac*min_spacing;

  
  

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
