#ifdef SPEC_CPU
# define THORN_IS_IOBasic
#endif /* SPEC_CPU */
 /*@@
   @file      Startup.c
   @date      Friday 18th September 1999
   @author    Gabrielle Allen
   @desc
              Startup routines for IOBasic.
   @enddesc
   @version   $Id: Startup.c,v 1.13 2001/08/26 17:22:14 allen Exp $
 @@*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "ioutil_Utils.h"
#include "iobasicGH.h"

static const char *rcsid = "$Header: /cactus/CactusBase/IOBasic/src/Startup.c,v 1.13 2001/08/26 17:22:14 allen Exp $";

CCTK_FILEVERSION(CactusBase_IOBasic_Startup_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
void IOBasic_Startup (void);

/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static void *IOBasic_SetupGH (tFleshConfig *config,
                              int convergence_level,
                              cGH *GH);


 /*@@
   @routine   IOBasic_Startup
   @date      Friday 18th September 1999
   @author    Gabrielle Allen
   @desc
              The startup registration routine for IOBasic.
              Registers the GH extensions needed for IOBasic
              along with its setup routine.
   @enddesc
   @calls     CCTK_RegisterGHExtensionSetupGH
@@*/
void IOBasic_Startup (void)
{
  if (CCTK_GHExtensionHandle ("IO") < 0)
  {
    CCTK_WARN (1, "Thorn IOUtil was not activated. "
                  "No IOBasic I/O methods will be enabled.");
    return;
  }

  CCTK_RegisterGHExtensionSetupGH (CCTK_RegisterGHExtension ("IOBasic"),
                                   IOBasic_SetupGH);
}


/****************************************************************************/
/*                           local routines                                 */
/****************************************************************************/
 /*@@
   @routine   IOBasic_SetupGH
   @date      Sat Feb 6 1999
   @author    Gabrielle Allen
   @desc
              Allocates and sets up IOBasic's GH extension structure
   @enddesc

   @calls     CCTK_RegisterIOMethod
              CCTK_RegisterIOMethodOutputGH
              CCTK_RegisterIOMethodOutputVarAs
              CCTK_RegisterIOMethodTimeToOutput
              CCTK_RegisterIOMethodTriggerOutput

   @var       config
   @vdesc     the CCTK configuration as provided by the flesh
   @vtype     tFleshConfig *
   @vio       unused
   @endvar
   @var       convergence_level
   @vdesc     the convergence level
   @vtype     int
   @vio       unused
   @endvar
   @var       GH
   @vdesc     Pointer to CCTK grid hierarchy
   @vtype     cGH *
   @vio       in
   @endvar

   @returntype  void *
   @returndesc
                pointer to the allocated GH extension structure
   @endreturndesc
@@*/
static void *IOBasic_SetupGH (tFleshConfig *config,
                              int convergence_level,
                              cGH *GH)
{
  DECLARE_CCTK_PARAMETERS
  int i;
  iobasicGH *newGH;


  /* suppress compiler warnings about unused variables */
  config = config;
  convergence_level = convergence_level;
  GH = GH;

  /* allocate the GH extension and its components */
  newGH = (iobasicGH *) malloc (sizeof (iobasicGH));
  if (newGH)
  {
    /* Register the IOBasic routines as output methods  */
    i = CCTK_RegisterIOMethod ("Scalar");
    CCTK_RegisterIOMethodOutputGH (i, IOBasic_ScalarOutputGH);
    CCTK_RegisterIOMethodOutputVarAs (i, IOBasic_ScalarOutputVarAs);
    CCTK_RegisterIOMethodTimeToOutput (i, IOBasic_TimeForScalarOutput);
    CCTK_RegisterIOMethodTriggerOutput (i, IOBasic_TriggerScalarOutput);

    i = CCTK_RegisterIOMethod ("Info");
    CCTK_RegisterIOMethodOutputGH (i, IOBasic_InfoOutputGH);
    CCTK_RegisterIOMethodTimeToOutput (i, IOBasic_TimeForInfoOutput);
    CCTK_RegisterIOMethodTriggerOutput (i, IOBasic_TriggerInfoOutput);

    if (CCTK_Equals (newverbose, "standard") ||
        CCTK_Equals( newverbose, "full"))
    {
      CCTK_INFO ("I/O Method 'Scalar' registered");
      CCTK_INFO ("Scalar: Output of scalar quantities (grid scalars, "
                 "reductions) to ASCII files");
      CCTK_INFO ("I/O Method 'Info' registered");
      CCTK_INFO ("Info: Output of scalar quantities (grid scalars, "
                 "reductions) to screen");
    }

    i = CCTK_NumVars ();

#ifdef SPEC_CPU
    newGH->outScalar_every = -1;
    newGH->outInfo_every = -1;
#endif

    newGH->info_reductions = (iobasic_reductionlist_t *)
                             calloc (i, sizeof (iobasic_reductionlist_t));
    newGH->do_outScalar   = (char *) malloc (i * sizeof (char));
    newGH->outInfo_last   = (int *)  malloc (i * sizeof (int));
    newGH->outScalar_last = (int *)  malloc (i * sizeof (int));

    memset (newGH->outInfo_last,   -1, i * sizeof (int));
    memset (newGH->outScalar_last, -1, i * sizeof (int));

    newGH->filenameListScalar = NULL;

    /* Check whether "IOBasic::outdirScalar" was set.
       If so take this dir otherwise default to "IO::outdir" */
    if (CCTK_ParameterQueryTimesSet ("outdirScalar", CCTK_THORNSTRING) <= 0)
    {
      outdirScalar = outdir;
    }
    /* skip the directory pathname if output goes into current directory */
    if (strcmp (outdirScalar, "."))
    {
      i = strlen (outdirScalar);
      newGH->outdirScalar = (char *) malloc (i + 2);
      strcpy (newGH->outdirScalar, outdirScalar);
      newGH->outdirScalar[i] = '/';
      newGH->outdirScalar[i+1] = 0;
    }
    else
    {
      newGH->outdirScalar = "";
    }

    /* create the output dir */
    if (*newGH->outdirScalar && CCTK_MyProc (GH) == 0)
    {
      i = IOUtil_CreateDirectory (GH, newGH->outdirScalar, 0, 0);
      if (i < 0)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "IOBasic_SetupGH: Couldn't create Scalar output directory "
                    "'%s'", newGH->outdirScalar);
      }
      else if (i >= 0 && CCTK_Equals (newverbose, "full"))
      {
	CCTK_VInfo (CCTK_THORNSTRING,
		    "Scalar: Output to directory '%s'",
		    newGH->outdirScalar);
      }

    }
  }

  USE_CCTK_PARAMETERS;   return (newGH);
}
