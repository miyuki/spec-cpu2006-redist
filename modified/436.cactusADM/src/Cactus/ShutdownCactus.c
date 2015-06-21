#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ShutdownCactus.c
   @date      Mon Sep 28 14:25:48 1998
   @author    Tom Goodale
   @desc
              Contains routines to shutdown cactus.
   @enddesc
   @version   $Id: ShutdownCactus.c,v 1.13 2001/11/05 14:58:54 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_Misc.h"
#include "cctk_Parameters.h"
#include "cctk_Schedule.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ShutdownCactus.c,v 1.13 2001/11/05 14:58:54 tradke Exp $";

CCTK_FILEVERSION(main_ShutdownCactus_c)

 /*@@
   @routine    CCTKi_ShutdownCactus
   @date       Mon Sep 28 14:50:50 1998
   @author     Tom Goodale
   @desc
               Cactus specific shutdown stuff.
   @enddesc
   @calls      CCTK_SchedulePrintTimes

   @var        ConfigData
   @vdesc      Flesh configuration data
   @vtype      tFleshConfig *
   @vio        unused
   @endvar

   @returntype int
   @returndesc
               0  - success
   @endreturndesc
@@*/
int CCTKi_ShutdownCactus(tFleshConfig *ConfigData)
{
  DECLARE_CCTK_PARAMETERS


  /* avoid compiler warning about unused argument */
  ConfigData = ConfigData;

  if (CCTK_Equals (cctk_timer_output, "full"))
  {
    CCTK_SchedulePrintTimes (NULL);
  }

  USE_CCTK_PARAMETERS;   return 0;
}
