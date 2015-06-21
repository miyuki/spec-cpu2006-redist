#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      InitialiseCactus.c
   @date      Fri Sep 18 14:04:02 1998
   @author    Tom Goodale
   @desc 
              Responsible for doing any cactus specific initialisations
   @enddesc 
   @version   $Id: InitialiseCactus.c,v 1.28 2001/11/05 14:58:53 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "cctk_Flesh.h"
#include "cctk_Parameter.h"
#include "cctk_Schedule.h"
#include "cctk_WarnLevel.h"
#include "cctk_Misc.h"

#include "cctki_Banner.h"
#include "cctki_Bindings.h"
#include "cctki_Schedule.h"


static const char *rcsid = "$Header: /cactus/Cactus/src/main/InitialiseCactus.c,v 1.28 2001/11/05 14:58:53 tradke Exp $";

CCTK_FILEVERSION(main_InitialiseCactus_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static int CCTKi_InitialiseScheduler (tFleshConfig *ConfigData);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

int CCTKBindings_RegisterThornFunctions (void);
int CCTKi_InitialiseSubsystemDefaults (void);
int CCTKi_ProcessEnvironment (int *argc, char ***argv,tFleshConfig *ConfigData);
int CCTKi_BindingsParameterRecoveryInitialise (void);

int CCTKi_ProcessCommandLine (int *inargc, char ***inargv, tFleshConfig *ConfigData);
int CCTKi_ProcessEnvironment (int *argc, char ***argv,tFleshConfig *ConfigData);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

#define CCTK_PRINTSEPARATOR \
  printf("--------------------------------------------------------------------------------\n");

static time_t startuptime;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_InitialiseCactus
   @date       Fri Sep 18 14:05:21 1998
   @author     Tom Goodale
   @desc 
   
   @enddesc 
   @calls      CCTKi_InitialiseSubsystemDefaults
               CCTKi_ProcessEnvironment
               CCTKi_ProcessCommandLine
               CCTKi_CactusBanner
               CCTKi_InitialiseDataStructures
               CCTKi_ProcessParameterDatabase
               CCTKi_BindingsVariablesInitialise
               CCTKi_InitialiseScheduler
               CCTKi_CallStartupFunctions
               CCTKi_PrintBanners
 
   @var        argc
   @vdesc      The number of command line arguments
   @vtype      int *
   @vio        inout
   @endvar 
   @var        argv
   @vdesc      The command line arguments
   @vtype      char ***
   @vio        inout
   @endvar 
   @var        ConfigData
   @vdesc      Flesh configuration data
   @vtype      tFleshConfig *
   @vio        inout
   @endvar 

   @returntype int
   @returndesc
               0  - success
   @endreturndesc
@@*/
int CCTKi_InitialiseCactus (int *argc, char ***argv, tFleshConfig *ConfigData)
{
  startuptime = time (NULL);

  CCTKi_InitialiseSubsystemDefaults ();

  CCTKi_ProcessEnvironment (argc, argv, ConfigData);

  CCTKi_ProcessCommandLine (argc, argv, ConfigData);

  CCTKi_CactusBanner ();

  CCTKi_InitialiseDataStructures (ConfigData);

  CCTKi_ProcessParameterDatabase (ConfigData);

  CCTKi_BindingsVariablesInitialise ();

  CCTKBindings_RegisterThornFunctions ();

  CCTKi_InitialiseScheduler (ConfigData);

  CCTKi_CallStartupFunctions (ConfigData);

  CCTKi_PrintBanners ();

  return (0);
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    CCTKi_InitialiseScheduler
   @date       Fri Sep 17 19:34:55 1999
   @author     Tom Goodale
   @desc 
               Initialise all scheduled items
   @enddesc 
   @calls      CCTKi_SetParameterSetMask
               CCTKi_BindingsParameterRecoveryInitialise
               CCTKi_BindingsScheduleInitialise
               CCTKi_DoScheduleSortAllGroups
               CCTK_SchedulePrint 

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
static int CCTKi_InitialiseScheduler (tFleshConfig *ConfigData)
{
  int i, retcode;
  const CCTK_INT *cctk_show_schedule;
  extern void CCTKi_SetParameterSetMask (int mask);


  /* avoid compiler warning about unused arguments */
  ConfigData = ConfigData;

  CCTKi_SetParameterSetMask (PARAMETER_RECOVERY_IN);

  if (CCTKi_BindingsParameterRecoveryInitialise () < 0)
  {
    CCTK_Warn (0, __LINE__, __FILE__, "Cactus", "Failed to recover parameters");
  }

  CCTKi_SetParameterSetMask (PARAMETER_RECOVERY_POST);

  CCTKi_BindingsScheduleInitialise ();

  retcode = CCTKi_DoScheduleSortAllGroups ();

  cctk_show_schedule = (const CCTK_INT *)
                       CCTK_ParameterGet ("cctk_show_schedule", "Cactus", &i);

  if (*cctk_show_schedule)
  {
    CCTK_PRINTSEPARATOR
    CCTK_SchedulePrint (NULL);
    CCTK_PRINTSEPARATOR
  }

  return (retcode);
}


 /*@@
   @routine    CCTK_RunTime
   @date       Tue Oct 3 2000
   @author     Gabrielle Allen
   @desc 
               Seconds since startup
   @enddesc 

   @returntype int
   @returndesc
               The number of seconds since the run started.
   @endreturndesc
@@*/
int CCTK_RunTime (void)
{
  return ((int) (time (NULL) - startuptime));
}
