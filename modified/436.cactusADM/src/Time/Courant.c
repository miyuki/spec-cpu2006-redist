#ifdef SPEC_CPU
# define THORN_IS_Time
#endif /* SPEC_CPU */
 /*@@
   @file      Courant.c
   @date      September 4 1999
   @author    Gabrielle Allen
   @desc
              Specification of timestep using Courant condition
   @enddesc
   @version   $Id: Courant.c,v 1.15 2002/01/02 17:20:16 tradke Exp $
 @@*/

#include <stdlib.h>
#include <math.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusBase/Time/src/Courant.c,v 1.15 2002/01/02 17:20:16 tradke Exp $";

CCTK_FILEVERSION(CactusBase_Time_Courant_c)

void Time_Courant(CCTK_ARGUMENTS);

void Time_Courant(CCTK_ARGUMENTS)
{
  CCTK_REAL min_spacing;
  DECLARE_CCTK_PARAMETERS
  DECLARE_CCTK_ARGUMENTS


  /* Calculate the minimum grid spacing */
  min_spacing = cctk_delta_space[0];

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

  /* Calculate the courant timestep */
  if (CCTK_Equals(timestep_method,"courant_time"))
  {
    *courant_dt = courant_fac*(*courant_min_time)/sqrt((double)cctk_dim);
  }
  else if (CCTK_Equals(timestep_method,"courant_speed"))
  {
    *courant_dt = courant_fac/(*courant_wave_speed)/sqrt((double) cctk_dim);
  }

  /* Set the Cactus timestep */

  if (!timestep_outonly)
  {
    cctkGH->cctk_delta_time = *courant_dt;
    CCTK_VInfo(CCTK_THORNSTRING,"Time step set to %f",cctkGH->cctk_delta_time);
  }
  else
  {
    cctkGH->cctk_delta_time = dtfac*min_spacing;
    CCTK_VInfo(CCTK_THORNSTRING,"Courant timestep would be %f",*courant_dt);
  }
  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
