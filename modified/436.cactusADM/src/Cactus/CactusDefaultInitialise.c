#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusDefaultInitialise.c
   @date      Tue Sep 29 12:45:04 1998
   @author    Tom Goodale
   @desc 
   Default cactus initialisation routine.
   @enddesc 
 @@*/

/*#define DEBUG_CCTK*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_GHExtensions.h"
#include "cctk_Parameter.h"

#include "cctki_Bindings.h"
#include "cctki_GHExtensions.h"
#include "cctki_ScheduleBindings.h"
#include "cctki_WarnLevel.h"

#include "CactusMainDefaults.h"
#include "CactusCommFunctions.h"

static const char *rcsid = "$Id: CactusDefaultInitialise.c,v 1.51 2001/12/17 22:31:27 tradke Exp $";

CCTK_FILEVERSION(main_CactusDefaultInitialise_c)

/* Local function prototypes */
int CactusInitialiseGH(cGH *GH);

 /*@@
   @routine    CactusDefaultInitialise
   @date       Tue Sep 29 12:45:04 1998
   @author     Tom Goodale
   @desc 
   Default initialisation routine.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int CactusDefaultInitialise(tFleshConfig *config)
{
  cGH *GH;
  int convergence_level;

#if 0
  CactusResetTimer(config->timer[INITIALISATION]);
  CactusResetTimer(config->timer[EVOLUTION]);
  CactusResetTimer(config->timer[ELLIPTIC]);

  CactusStartTimer(config->timer[INITIALISATION]);
#endif

  convergence_level = 0;
  while((GH = CCTK_SetupGH(config, convergence_level)))
  {
    CCTKi_AddGH(config, convergence_level, GH);

    CactusInitialiseGH(GH);

    convergence_level++;
  };

#if 0
  CactusStopTimer(config->timer[INITIALISATION]);
#endif

  return 0;
}


 /*@@
   @routine    CactusInitialiseGH
   @date       Mon Feb  1 12:13:09 1999
   @author     Tom Goodale
   @desc 
   Responsible for initialising a GH.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int CactusInitialiseGH(cGH *GH)
{
  int param_type;
  const CCTK_REAL *cctk_initial_time;

  cctk_initial_time = (const CCTK_REAL *) CCTK_ParameterGet("cctk_initial_time",
                                                       "Cactus", &param_type);

  /* Initialise time */
  GH->cctk_time = *cctk_initial_time;

  /* Initialise iteration number */
  GH->cctk_iteration = 0;
 
#ifdef DEBUG_CCTK
  CCTK_PRINTSEPARATOR
  printf("In Cactus_Initialise\n--------------------\n");
  printf("  Initializing GH->cctk_time = %f\n",GH->cctk_time);
  printf("  Initializing GH->cctk_iteration = %u\n",GH->cctk_iteration);
  CCTK_PRINTSEPARATOR
#endif
  
  /* Do the schedule initialisation on this GH */
  CCTKi_ScheduleGHInit((void *)GH);

  /* Initialise all the extensions. */
  CCTKi_InitGHExtensions(GH);

  /* FIXME : PARAM_CHECK SHOULD BE BEFORE HERE */
  CCTK_Traverse(GH, "CCTK_PARAMCHECK");
  CCTKi_FinaliseParamWarn();

  CCTK_Traverse(GH, "CCTK_BASEGRID"); 

  /* Traverse routines setting up initial data */
  CCTK_Traverse(GH, "CCTK_INITIAL");

  /* Traverse poststep initial routines which should only be done once */
  CCTK_Traverse(GH, "CCTK_POSTINITIAL");

  CCTK_Traverse(GH, "CCTK_POSTSTEP");

  /* Traverse recovery and ID checkpoint routines */
  CCTK_Traverse(GH, "CCTK_RECOVER_VARIABLES");
  CCTK_Traverse(GH, "CCTK_CPINITIAL");

  return 1;

}
