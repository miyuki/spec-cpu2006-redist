#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CCTKi_CallStartupFunctions.c
   @date      Mon Sep 28 14:16:19 1998
   @author    Tom Goodale
   @desc
              Contains routines to deal with thorn startup functions.
   @enddesc
   @version   $Id: CallStartupFunctions.c,v 1.18 2001/11/05 14:58:53 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_Schedule.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/CallStartupFunctions.c,v 1.18 2001/11/05 14:58:53 tradke Exp $";

CCTK_FILEVERSION(main_CallStartupFunctions_c)

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_CallStartupFunctions
   @date       Mon Sep 28 14:24:39 1998
   @author     Tom Goodale
   @desc
               Calls CCTK_ScheduleTraverse() on "CCTK_STARTUP"
   @enddesc
   @calls      CCTK_ScheduleTraverse

   @var        ConfigData
   @vdesc      Flesh configuration data
   @vtype      tFleshConfig *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0  - success
   @endreturndesc
@@*/

int CCTKi_CallStartupFunctions (tFleshConfig *ConfigData)
{
  ConfigData = ConfigData;

  CCTK_ScheduleTraverse ("CCTK_STARTUP", NULL, NULL);

  return (0);
}
