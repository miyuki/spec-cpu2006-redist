#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusDefaultEvolve.c
   @date      Thu Oct  8 17:28:46 1998
   @author    Tom Goodale
   @desc
              Default cactus evolution stuff.
   @enddesc
   @version   $Id: CactusDefaultEvolve.c,v 1.55 2001/09/20 21:46:18 tradke Exp $
 @@*/

/* #define DEBUG_CCTK 1 */

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_Parameters.h"
#include "cctk_Groups.h"
#include "cctk_WarnLevel.h"
#include "cctk_Termination.h"
#include "cctk_Main.h"
#include "cctk_Misc.h"
#include "cctk_IO.h"

static const char *rcsid="$Header: /cactus/Cactus/src/main/CactusDefaultEvolve.c,v 1.55 2001/09/20 21:46:18 tradke Exp $";

CCTK_FILEVERSION(main_CactusDefaultEvolve_c)

/* Define some macros for convenience. */

#define TERMINATION_RAISED_BRDCAST 4

#define ForallConvLevels(iteration, conv_level)                               \
        {                                                                     \
          int factor = 1;                                                     \
          for (conv_level = 0; conv_level < config->nGHs; conv_level++)       \
          {                                                                   \
            if (iteration % factor == 0)                                      \
            {

#define EndForallConvLevels                                                   \
            }                                                                 \
            factor *= 2;                                                      \
          }                                                                   \
        }

int cactus_terminate;


/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

#if 0
static int cactus_terminate_global = 0;
#endif

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static int DoneMainLoop (cGH *GH, CCTK_REAL cctk_time, int iteration);
static void StepGH (cGH *GH);

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

int CactusDefaultEvolve (tFleshConfig *config);

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    CactusDefaultEvolve
   @date       Thu Oct  8 17:30:15 1998
   @author     Tom Goodale
   @desc
               The default cactus evolution routine.
   @enddesc
   @calls      CCTK_Traverse
               CCTK_OutputGH
               DoneMainLoop
               StepGH
   @history
   @hdate Fri May 12 2000 @hauthor Thomas Radke
   @hdesc  Moved evolution loop termination check into DoneMainLoop()
   @endhistory

   @var        config
   @vdesc      pointer to flesh configuration structure
   @vtype      tFleshConfig *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               always returns 0
   @endreturndesc
@@*/
int CactusDefaultEvolve (tFleshConfig *config)
{
  int var;
  unsigned int convergence_level, iteration;


#if 0
  CactusStartTimer (config->timer[OUTPUT]);
#endif

  /*** Call OUTPUT for this GH (this routine    ***/
  /*** checks if output is necessary) and makes ***/
  /*** a Traverse with CCTK_ANALYSIS      ***/
  iteration = CCTK_MainLoopIndex ();
  ForallConvLevels (iteration, convergence_level)
  {
    CCTK_Traverse (config->GH[convergence_level], "CCTK_ANALYSIS");
    CCTK_OutputGH (config->GH[convergence_level]);
  }
  EndForallConvLevels;

#if 0
  CactusStopTimer (config->timer[OUTPUT]);
  CactusStartTimer (config->timer[EVOLUTION]);
#endif

  while (! DoneMainLoop (config->GH[0], config->GH[0]->cctk_time, iteration))
  {
    if (iteration == 0)
    {
      /* Can only use CactusDefaultEvolve with one timelevel */
      for (var = CCTK_NumVars () - 1; var >= 0; var--)
      {
        if (CCTK_NumTimeLevelsFromVarI (var) > 1)
        {
          CCTK_VWarn (0,__LINE__,__FILE__,"Cactus",
                      "Variable '%s' has multiple timelevels, default Cactus "
                      "evolve routine cannot rotate",
                      CCTK_VarName (var));
        }
      }
    }
    /* HERE ROTATE TIMELEVELS FOR ALL CONVERGENCE LEVELS */

    iteration = CCTK_SetMainLoopIndex (++iteration);

    /* Step each convergence level */
    ForallConvLevels (CCTK_MainLoopIndex (), convergence_level)
    {
      StepGH (config->GH[convergence_level]);
      /*
      CCTK_InfoOutput (config->GH[convergence_level], convergence_level);
      */
    }
    EndForallConvLevels;

    /* Dump out checkpoint data on all levels */
    ForallConvLevels (iteration, convergence_level)
    {
      CCTK_Traverse (config->GH[convergence_level], "CCTK_CHECKPOINT");
    }
    EndForallConvLevels;

    /* Output perhaps */
#if 0
    CactusStartTimer (config->timer[OUTPUT]);
#endif
    /*** Call OUTPUT for this GH (this routine    ***/
    /*** checks if output is necessary) and makes ***/
    /*** an Traverse with CCTK_ANALYSIS      ***/
    ForallConvLevels (iteration, convergence_level)
    {
        CCTK_Traverse (config->GH[convergence_level], "CCTK_ANALYSIS");
        CCTK_OutputGH (config->GH[convergence_level]);
    }
    EndForallConvLevels;

#if 0
    CactusStopTimer (config->timer[OUTPUT]);
#endif

#if 0
    ConvergenceReport (config->GH, iteration);
    TerminationStepper (config->GH[0]);
#endif

    /* Termination has been raised and broadcasted, exit loop*/
    if (cactus_terminate == TERMINATION_RAISED_BRDCAST)
    {
      break;
    }

  } /*** END OF MAIN ITERATION LOOP ***/

#if 0
  CactusStopTimer (config->timer[EVOLUTION]);
#endif

  return (0);
}


 /*@@
   @routine    DoneMainLoop
   @date       Fri May 12 2000
   @author     Thomas Radke
   @desc
               Check the termination conditions for the evolution loop
   @enddesc
   @calls      CCTK_TerminationReached

   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        time
   @vdesc      current physical time
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        iteration
   @vdesc      current iteration
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               true (1) or false (0) for done/not done with main loop
   @endreturndesc
@@*/
static int DoneMainLoop (cGH *GH, CCTK_REAL time, int iteration)
{
  int retval;
  DECLARE_CCTK_PARAMETERS


  retval = terminate_next || CCTK_TerminationReached (GH);
  if (! retval)
  {
    if (CCTK_Equals (terminate, "never"))
    {
      retval = 0;
    }
    else if (CCTK_Equals (terminate, "iteration"))
    {
      retval = iteration >= cctk_itlast;
    }
    else
    {
      if (cctk_initial_time < cctk_final_time)
      {
        retval = time >= cctk_final_time;
      }
      else
      {
        retval = time <= cctk_final_time;
      }
      if (CCTK_Equals (terminate, "either"))
      {
        retval |= iteration >= cctk_itlast;
      }
      else if (CCTK_Equals (terminate, "both"))
      {
        retval &= iteration >= cctk_itlast;
      }
    }
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    StepGH
   @date       Fri Aug 14 12:39:49 1998
   @author     Gerd Lanfermann
   @desc
               The full set of routines used to execute all schedule points
               in the main iteration loop. Makes calls to the individual
               routines for each schedule point.
   @enddesc
   @calls      CCTK_Traverse

   @var        GH
   @vdesc      pointer to CCTK grid hierachy
   @vtype      cGH *
   @vio        in
   @endvar
 @@*/
static void StepGH (cGH *GH)
{
  GH->cctk_time = GH->cctk_time + GH->cctk_delta_time;
  GH->cctk_iteration++;

  CCTK_Traverse (GH, "CCTK_PRESTEP");
  CCTK_Traverse (GH, "CCTK_EVOL");
  CCTK_Traverse (GH, "CCTK_POSTSTEP");
}


#if 0
 /*@@
   @routine    TerminationStepper
   @date       Fri Aug 14 13:07:11 1998
   @author     Gerd Lanfermann
   @desc
               catctus_terminate is a global variable with these values:
               TERMINATION_NOT_RAISED    : not signaled yet (cactus_initial.c)
               TERMINATION_RAISED_LOCAL  : signaled on one PE, not reduced
                                           (MPI_LOR) to all PEs yet (main.c)
               TERMINATION_RAISED_BRDCAST: reduced -> can now be used to
                                           terminate (chkpnt_terminate.c)
                                           by the scheduler
               the raised termiantion signal is caught on 1 PE only and has to
               be recduced on all PEs before a termination sequenced can be
               launched (I like that)
   @enddesc
   @calls      CCTK_Traverse
@@*/
static void TerminationStepper (cGH *GH)
{
  cactus_terminate_global = cactus_terminate;

#ifdef CCTK_MPI
  MPI_Allreduce (&cactus_terminate, &cactus_terminate_global, 1, MPI_INT,
                 MPI_LOR, MPI_COMM_WORLD);
#endif
  if (cactus_terminate_global)
  {
    cactus_terminate = TERMINATION_RAISED_BRDCAST;
    printf ("RECEIVED GLOBAL TERMINATION SIGNAL\n");
  }

  CCTK_Traverse (GH, "CCTK_TERMINATE");
}
#endif
