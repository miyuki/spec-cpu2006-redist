#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      PUGH_Evolve.c
   @date      Thu Oct  8 17:28:46 1998
   @author    Tom Goodale
   @desc
   PUGH evolution stuff.
   @enddesc
   @version $Header: /cactus/CactusPUGH/PUGH/src/Evolve.c,v 1.16 2001/10/31 12:04:59 tradke Exp $
   @hdate Tue Mar 28 22:41:15 2000 @hauthor Ed Evans
   @hdesc Moved into PUGH and added time level rotation
   @hdate Tue Mar 28 22:41:45 2000 @hauthor Tom Goodale
   @hdesc Tidied up
 @@*/

#include <stdio.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Termination.h"

#include "pugh.h"

static const char *rcsid="$Header: /cactus/CactusPUGH/PUGH/src/Evolve.c,v 1.16 2001/10/31 12:04:59 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_Evolve_c)

/* Define some macros for convenience. */

#define ForallConvLevels(iteration, conv_level)  {                     \
                                        int factor = 1;                \
                                        for(conv_level = 0 ;           \
                                            conv_level < config->nGHs; \
                                            conv_level++)              \
                                        {                              \
                                          if(iteration%factor == 0)    \
                                          {

#define EndForallConvLevels                                            \
                                          };                           \
                                          factor *=2;                  \
                                        };                             \
                                     }


/* Local function prototypes. */

static int DoneMainLoop (const cGH *GH, CCTK_REAL cctk_time, int iteration);
static int StepGH(cGH *GH);
static void RotateTimeLevelsGH(cGH *cgh);


 /*@@
   @routine    PUGH_Evolve
   @date       Thu Oct  8 17:30:15 1998
   @author     Tom Goodale
   @desc
   PUGH's evolution routine.
   @enddesc
   @calls
   @calledby
   @history
   @hdate Tue Mar 28 22:42:47 2000 @hauthor Ed Evans
   @hdesc  Copied from CactusDefaultEvolve and added rotation
   @hdate Tue Mar 28 22:43:18 2000 @hauthor Tom Goodale
   @hdesc Tidied
   @hdate Fri May 12 2000 @hauthor Thomas Radke
   @hdesc  Moved evolution loop termination check into DoneMainLoop()
   @endhistory

@@*/
int PUGH_Evolve(tFleshConfig *config)
{
  unsigned int convergence_level;
  unsigned int iteration=CCTK_MainLoopIndex();

  /*** Call OUTPUT for this GH (this routine    ***/
  /*** checks if output is necessary) and makes ***/
  /*** a Traverse with CCTK_ANALYSIS      ***/
  ForallConvLevels(iteration, convergence_level)
  {
      CCTK_Traverse(config->GH[convergence_level], "CCTK_ANALYSIS");
      CCTK_OutputGH(config->GH[convergence_level]);
  }
  EndForallConvLevels;

  while (! DoneMainLoop (config->GH[0],config->GH[0]->cctk_time, iteration))
  {

    ForallConvLevels(iteration, convergence_level)
    {

      RotateTimeLevelsGH(config->GH[convergence_level]);
      /*
      CCTK_InfoOutput(config->GH[convergence_level], convergence_level);
      */
    }
    EndForallConvLevels;

    iteration = CCTK_SetMainLoopIndex(++iteration);

    /* Step each convergence level */

    ForallConvLevels(iteration, convergence_level)
    {

      StepGH(config->GH[convergence_level]);
      /*
      CCTK_InfoOutput(config->GH[convergence_level], convergence_level);
      */
    }
    EndForallConvLevels;

    /* Dump out checkpoint data on all levels */
    ForallConvLevels(iteration, convergence_level)
    {
      CCTK_Traverse(config->GH[convergence_level], "CCTK_CHECKPOINT");
    }
    EndForallConvLevels;

    /* Output perhaps */

    /*** Call OUTPUT for this GH (this routine    ***/
    /*** checks if output is necessary) and makes ***/
    /*** an Traverse with CCTK_ANALYSIS      ***/
    ForallConvLevels(iteration, convergence_level)
    {
        CCTK_Traverse(config->GH[convergence_level], "CCTK_ANALYSIS");
        CCTK_OutputGH(config->GH[convergence_level]);
    }
    EndForallConvLevels;

#if 0
    /* Termination has been raised and broadcasted, exit loop*/
    if (cactus_terminate==TERMINATION_RAISED_BRDCAST) break;
#endif

  } /*** END OF MAIN ITERATION LOOP ***/

  return 0;
}

/************************************************************************/

 /*@@
   @routine    DoneMainLoop
   @date       Fri May 12 2000
   @author     Thomas Radke
   @desc
               Check the termination conditions for the evolution loop
   @enddesc
   @calls

   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        time
   @vdesc      coordinate time
   @vtype      CCTK_REAL
   @vio        in
   @endvar
   @var        iteration
   @vdesc      iteration number
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               1 - condition for evolution loop termination is true
               0 - condition for evolution loop termination is false
   @endreturndesc
@@*/
static int DoneMainLoop (const cGH *GH, CCTK_REAL time, int iteration)
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
     The full set of routines used to execute all schedule point
     int the main iteration loop. Makes calls to the individual
     routines for each schedule point.
   @enddesc
   @calls  CCTK_Traverse
   @calledby main
 @@*/

static int StepGH(cGH *GH)
{
  /* Advance GH->iteration BEFORE evolving */

  GH->cctk_time = GH->cctk_time + GH->cctk_delta_time;
  GH->cctk_iteration++;

  CCTK_Traverse(GH, "CCTK_PRESTEP");
  CCTK_Traverse(GH, "CCTK_EVOL");
  CCTK_Traverse(GH, "CCTK_POSTSTEP");

  return 0;
}


 /*@@
   @routine    RotateTimeLevelsGH
   @date       Tue Mar 28 22:43:49 2000
   @author     Ed Evans
   @desc
   Rotates the time levels
   @enddesc
   @calls
   @calledby
   @history

   @endhistory

@@*/
static void RotateTimeLevelsGH(cGH *cgh)
{
  int var;
  int numtimelevels;
  int i;
  void *temp;
  pGH *pughGH;

  pughGH = PUGH_pGH(cgh);

  for(var = 0; var < pughGH->nvariables; var++)
  {
    numtimelevels = CCTK_NumTimeLevelsFromVarI(var);
    if(numtimelevels>1)
    {
      temp=pughGH->variables[var][numtimelevels-1];

      for(i = numtimelevels-1; i>0; i--)
      {
        pughGH->variables[var][i]=pughGH->variables[var][i-1];
      }

      pughGH->variables[var][0]=temp;
    }
  }

  return;
}
