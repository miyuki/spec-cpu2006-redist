#ifdef SPEC_CPU
# define THORN_IS_IOASCII
#endif /* SPEC_CPU */
 /*@@
   @file      Startup.c
   @date      Sat Feb 6 1999
   @author    Gabrielle Allen
   @desc 
              Startup routines for IOASCII.
   @enddesc 
   @version   $Id: Startup.c,v 1.12 2001/12/13 12:23:15 tradke Exp $
 @@*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_IOMethods.h"
#include "cctk_Parameters.h"
#include "ioutil_Utils.h"
#include "ioASCIIGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Id: Startup.c,v 1.12 2001/12/13 12:23:15 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOASCII_Startup_c)


/* prototypes of routines defined in this source file */
void IOASCII_Startup (void);
static void *IOASCII_SetupGH (tFleshConfig *config,
                              int convergence_level,
                              cGH *GH);

 /*@@
   @routine   IOASCII_Startup
   @date      Sat Feb 6 1999
   @author    Gabrielle Allen
   @desc 
              The startup registration routine for IOASCII.
              Registers the GH extensions needed for IOASCII
              along with its setup routine.
   @enddesc 
   @calls     CCTK_RegisterGHExtensionSetupGH
@@*/
void IOASCII_Startup (void)
{
  /* check that thorn IOUtil was activated */
  if (CCTK_GHExtensionHandle ("IO") < 0)
  {
    CCTK_WARN (1, "IOASCII_Startup: Thorn IOUtil was not activated. "
                  "No IOASCII IO methods will be enabled.");
    return;
  }

  CCTK_RegisterGHExtensionSetupGH (CCTK_RegisterGHExtension ("IOASCII"),
                                   IOASCII_SetupGH);

}


/****************************************************************************/
/*                           local routines                                 */
/****************************************************************************/
 /*@@
   @routine   IOASCII_SetupGH
   @date      Sat Feb 6 1999
   @author    Gabrielle Allen
   @desc 
              Allocates and sets up IOASCII's GH extension structure
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
static void *IOASCII_SetupGH (tFleshConfig *config,
                              int convergence_level,
                              cGH *GH)
{
  DECLARE_CCTK_PARAMETERS
  int i, j, numvars, maxdim;
  asciiioGH *newGH;


  /* suppress compiler warnings about unused variables */
  config = config;
  convergence_level = convergence_level;
  GH = GH;

  /* Register the IOASCII routines as output methods  */
  i = CCTK_RegisterIOMethod ("IOASCII_1D");
  CCTK_RegisterIOMethodOutputGH (i, IOASCII_Output1DGH);
  CCTK_RegisterIOMethodOutputVarAs (i, IOASCII_Output1DVarAs);
  CCTK_RegisterIOMethodTimeToOutput (i, IOASCII_TimeFor1D);
  CCTK_RegisterIOMethodTriggerOutput (i, IOASCII_TriggerOutput1D);

  i = CCTK_RegisterIOMethod ("IOASCII_2D");
  CCTK_RegisterIOMethodOutputGH (i, IOASCII_Output2DGH);
  CCTK_RegisterIOMethodOutputVarAs (i, IOASCII_Output2DVarAs);
  CCTK_RegisterIOMethodTimeToOutput (i, IOASCII_TimeFor2D);
  CCTK_RegisterIOMethodTriggerOutput (i, IOASCII_TriggerOutput2D);

  i = CCTK_RegisterIOMethod ("IOASCII_3D");
  CCTK_RegisterIOMethodOutputGH (i, IOASCII_Output3DGH);
  CCTK_RegisterIOMethodOutputVarAs (i, IOASCII_Output3DVarAs);
  CCTK_RegisterIOMethodTimeToOutput (i, IOASCII_TimeFor3D);
  CCTK_RegisterIOMethodTriggerOutput (i, IOASCII_TriggerOutput3D);

  if (CCTK_Equals (newverbose, "standard") ||
      CCTK_Equals( newverbose, "full"))
  {
    CCTK_INFO ("I/O Method 'IOASCII_1D' registered");
    CCTK_INFO ("IOASCII_1D: Output of 1D lines of grid functions/arrays "
               "to ASCII files");
    CCTK_INFO ("I/O Method 'IOASCII_2D' registered");
    CCTK_INFO ("IOASCII_2D: Output of 2D planes of grid functions/arrays "
               "to ASCII files");
    CCTK_INFO ("I/O Method 'IOASCII_3D' registered");
    CCTK_INFO ("IOASCII_3D: Output of 3D grid functions/arrays "
               "to ASCII files");
  }

  /* allocate the GH extension and its components */
  newGH = (asciiioGH *) malloc (sizeof (asciiioGH));

  if(newGH)
  {
    numvars = CCTK_NumVars ();
    newGH->do_out1D = (char *) malloc (numvars * sizeof (char));
        newGH->do_out2D = (char *) malloc (numvars * sizeof (char));
    newGH->do_out3D = (char *) malloc (numvars * sizeof (char));
    newGH->out1D_last = (int *) malloc (numvars * sizeof (int));
    newGH->out2D_last = (int *) malloc (numvars * sizeof (int));
    newGH->out3D_last = (int *) malloc (numvars * sizeof (int));

    for (i = 0; i < numvars; i++)
    {
      newGH->out1D_last[i] = -1;
      newGH->out2D_last[i] = -1;
      newGH->out3D_last[i] = -1;
    }

    newGH->filenameList1D = NULL;
    newGH->fileList_2D = NULL;
    newGH->fileList_3D = NULL;

    maxdim = CCTK_MaxDim ();
    newGH->spxyz = (int ***) malloc (maxdim * sizeof (int **));
    newGH->sp2xyz = (int **) malloc (3 * sizeof (int *));

    for (i = maxdim - 1; i >= 0; i--)
    {
      newGH->spxyz[i]    = (int **) malloc ((i+1) * sizeof (int *));

      for (j = 0; j <= i; j++)
      {
        newGH->spxyz[i][j] = (int  *) calloc ((i+1), sizeof (int ));
      }

      newGH->sp2xyz[i]   = (int  *) calloc ((i+1), sizeof (int ));
    }

    /* How often to output */
    newGH->out1D_every = out_every > 0 ? out_every : -1;
    if (out1D_every > 0)
    {
      newGH->out1D_every = out1D_every;
    }
    newGH->out2D_every = out_every > 0 ? out_every : -1;
    if (out2D_every > 0)
    {
      newGH->out2D_every = out2D_every;
    }
    newGH->out3D_every = out_every > 0 ? out_every : -1;
    if (out3D_every > 0)
    {
      newGH->out3D_every = out3D_every;
    }

    if (CCTK_Equals (newverbose, "standard") ||
        CCTK_Equals (newverbose, "full"))
    {
      if (out1D_every > 0)
      {
        CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_1D: Output every %d iterations",
                    newGH->out1D_every);
      }
      else
      {
        CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_1D: Output not activated");
      }
      if (out2D_every > 0)
      {
        CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_2D: Output every %d iterations",
                    newGH->out2D_every);
      }
      else
      {
        CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_2D: Output not activated");
      }
      if (out3D_every > 0)
      {
        CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_3D: Output every %d iterations",
                    newGH->out3D_every);
      }
      else
      {
        CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_3D: Output not activated");
      }
    }

    
    
    /* Deal with the output directories */
    /* Check whether "outdirXD" was set.
       If so take this dir otherwise default to "IO::outdir" */
    newGH->outdir1D =
      CCTK_ParameterQueryTimesSet ("outdir1D", CCTK_THORNSTRING) > 0 ?
      strdup (outdir1D) : strdup (outdir);
    newGH->outdir2D =
      CCTK_ParameterQueryTimesSet ("outdir2D", CCTK_THORNSTRING) > 0 ?
      strdup (outdir2D) : strdup (outdir);
    newGH->outdir3D =
      CCTK_ParameterQueryTimesSet ("outdir3D", CCTK_THORNSTRING) > 0 ?
      strdup (outdir3D) : strdup (outdir);

    /* create the 1D output dir */
    i = IOUtil_CreateDirectory (GH, newGH->outdir1D, 0, 0);
    if (i < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_InitGH: Problem creating IOASCII 1D output"
                  " directory '%s'", newGH->outdir1D);
    }
    else if (i >= 0 && CCTK_Equals (newverbose, "full"))
    {
      CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_1D: Output to directory '%s'",
                  newGH->outdir1D);
    }

    /* create the 2D output dir */
    i = IOUtil_CreateDirectory (GH, newGH->outdir2D, 0, 0);
    if (i < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_InitGH: problem creating IOASCII 2D output"
                  " directory '%s'", newGH->outdir2D);
    }
    else if (i >= 0 && CCTK_Equals (newverbose, "full"))
    {
      CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_2D: Output to directory '%s'",
                  newGH->outdir2D);
    }

    /* create the 3D output dir */
    i = IOUtil_CreateDirectory (GH, newGH->outdir3D, 0, 0);
    if (i < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_InitGH: problem creating IOASCII 3D output"
                  " directory '%s'", newGH->outdir3D);
    }
    else if (i >= 0 && CCTK_Equals (newverbose, "full"))
    {
      CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_3D: Output to directory '%s'",
                  newGH->outdir3D);
    }

  }
  else
  {
    CCTK_WARN (0, "IOASCII_InitGH: Unable to allocate memory for GH");
  }

  USE_CCTK_PARAMETERS;   return (newGH);
}
