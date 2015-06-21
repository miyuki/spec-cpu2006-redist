#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusDefaultComm.c
   @date      Tue Sep 29 15:06:22 1998
   @author    Tom Goodale
   @desc
              Default communication routines.
   @enddesc
   @version   $Id: CactusDefaultComm.c,v 1.48 2001/10/31 11:45:31 tradke Exp $
 @@*/


#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_Groups.h"
#include "cctk_Constants.h"
#include "CactusMainDefaults.h"
#include "cctki_GHExtensions.h"

#include "cctk_ParamCheck.h"

#ifdef CCTK_MPI
#include "mpi.h"
#endif

static const char *rcsid = "$Header: /cactus/Cactus/src/comm/CactusDefaultComm.c,v 1.48 2001/10/31 11:45:31 tradke Exp $";

CCTK_FILEVERSION(comm_CactusDefaultComm_c)

/********************************************************************
 *********************     Global Data   *****************************
 ********************************************************************/

/* FIXME:  This should be in a header somewhere */
#ifdef CCTK_MPI
extern char MPI_Active;
#endif

/********************************************************************
 *********************     Local Definitions   **********************
 ********************************************************************/

#ifdef CCTK_MPI
#define CACTUS_MPI_ERROR(xf)                                                  \
          do                                                                  \
          {                                                                   \
            int errcode;                                                      \
                                                                              \
                                                                              \
            if((errcode = xf) != MPI_SUCCESS)                                 \
            {                                                                 \
              char mpi_error_string[MPI_MAX_ERROR_STRING+1];                  \
              int resultlen;                                                  \
                                                                              \
                                                                              \
              MPI_Error_string(errcode, mpi_error_string, &resultlen);        \
              fprintf(stderr, "MPI Call %s returned error code %d (%s)\n",    \
                              #xf, errcode, mpi_error_string);                \
              fprintf(stderr, "At line %d of file %s\n",                      \
                              __LINE__, __FILE__);                            \
            }                                                                 \
          } while (0)
#endif

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/
cGH *CactusDefaultSetupGH(tFleshConfig *config, int convergence_level);
int CactusDefaultMyProc(const cGH *GH);
int CactusDefaultnProcs(const cGH *GH);
int CactusDefaultExit(cGH *GH, int retval);
int CactusDefaultAbort(cGH *GH, int retval);
int CactusDefaultBarrier(const cGH *GH);


 /*@@
   @routine    CactusDefaultSetupGH
   @date       Tue Sep 29 15:06:22 1998
   @author     Tom Goodale
   @desc
               Default cactus SetupGH routine.
   @enddesc
   @calls      CCTK_MaxDim
               CCTK_NumVars
               CCTK_NumTimeLevelsFromVarI
               CCTK_NumGroups
               CCTKi_SetupGHExtensions

   @var        config
   @vdesc      Pointer to flesh configuration environment
   @vtype      tFleshConfig *
   @vio        in
   @endvar
   @var        convergence_level
   @vdesc      convergence leve of new cGH
   @vtype      int
   @vio        in
   @endvar

   @returntype cGH *
   @returndesc
               the pointer to the new cGH structure,
               or NULL if memory allocation failed
   @endreturndesc
@@*/
cGH *CactusDefaultSetupGH(tFleshConfig *config, int convergence_level)
{
  cGH *thisGH;
  int n_groups;
  int n_variables;
  int variable;
  int ntimelevels;
  int cctk_dim;


  /* Put this in for the moment until parameter stuff is done. */
  if(convergence_level > 0)
  {
    return (NULL);
  }

  /* Initialise this since it is used later and in exceptional
   * circumstances might not be initialsed beforehand.
   */

  variable = -1;

  /* Create a new Grid Hierarchy */
  thisGH = (cGH *)malloc(sizeof(cGH));

  if(thisGH)
  {
    thisGH->cctk_dim = CCTK_MaxDim();

    /* Need this to be at least one otherwise the memory allocation will fail. */
    cctk_dim = thisGH->cctk_dim;
    if(thisGH->cctk_dim == 0) cctk_dim = 1;
    thisGH->cctk_iteration    = 0;
    thisGH->cctk_gsh          = (int *)malloc(cctk_dim*sizeof(int));
    thisGH->cctk_lsh          = (int *)malloc(cctk_dim*sizeof(int));
    thisGH->cctk_lbnd         = (int *)malloc(cctk_dim*sizeof(int));
    thisGH->cctk_ubnd         = (int *)malloc(cctk_dim*sizeof(int));

    thisGH->cctk_lssh         = (int *)malloc(CCTK_NSTAGGER*cctk_dim*sizeof(int));
    thisGH->cctk_to           = (int *)malloc(cctk_dim*sizeof(int));
    thisGH->cctk_from         = (int *)malloc(cctk_dim*sizeof(int));
    thisGH->cctk_bbox         = (int *)malloc(2*cctk_dim*sizeof(int));
    thisGH->cctk_nghostzones  = (int *)malloc(2*cctk_dim*sizeof(int));
    thisGH->cctk_levfac       = (int *)malloc(cctk_dim*sizeof(int));
    thisGH->cctk_delta_space  = (CCTK_REAL *)malloc(cctk_dim*sizeof(CCTK_REAL));
    /* FIXME : Next line goes when coords are done properly */
    thisGH->cctk_origin_space = (CCTK_REAL *)malloc(cctk_dim*sizeof(CCTK_REAL));

    thisGH->cctk_delta_time = 1;
    thisGH->cctk_convlevel = 0;

    n_variables = CCTK_NumVars();

    /* Allocate memory for the variable data pointers.
     * Note we want at least one to prevent memory allocattion from failing !
     */
    thisGH->data = (void ***)malloc((n_variables ? n_variables:1)*sizeof(void **));

    if(thisGH->data)
    {
      for(variable = 0; variable < n_variables; variable++)
      {
        ntimelevels = CCTK_NumTimeLevelsFromVarI(variable);

        thisGH->data[variable] = (void **)calloc(ntimelevels, sizeof(void *));
        if(thisGH->data[variable] == NULL)
        {
          break;
        }
      }
    }

    thisGH->extensions = NULL;

    /* Allocate memory for the group data pointers.
     * Note we want at least one to prevent memory allocattion from failing !
     */
    n_groups = CCTK_NumGroups();
    thisGH->GroupData = (cGHGroupData *)malloc((n_groups ? n_groups:1)*sizeof(cGHGroupData));

  }

  if(thisGH &&
     thisGH->cctk_gsh &&
     thisGH->cctk_lsh &&
     thisGH->cctk_lbnd &&
     thisGH->cctk_ubnd &&
     thisGH->cctk_lssh &&
     thisGH->cctk_from &&
     thisGH->cctk_to &&
     thisGH->cctk_bbox &&
     thisGH->cctk_nghostzones &&
     thisGH->cctk_levfac &&
     thisGH->cctk_delta_space &&
     thisGH->cctk_origin_space &&
     thisGH->data &&
     variable == n_variables &&
     thisGH->GroupData)
  {
    /* Traverse list of GH setup routines. */
    CCTKi_SetupGHExtensions(config, convergence_level, thisGH);
  }
  else
  {
    /* FIXME: should free potentially allocated memory for this GH */
    thisGH = NULL;
  }

  return (thisGH);
}


 /*@@
   @routine    CactusDefaultMyProc
   @date       Tue Jan 23 1999
   @author     Gabrielle Allen
   @desc
               Default Cactus MyProc routine.
   @enddesc
   @calls      CCTK_ParamChecking
               MPI_Comm_rank

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        unused
   @endvar

   @returntype int
   @returndesc
               the processor number of the caller
   @endreturndesc
@@*/
int CactusDefaultMyProc (const cGH *GH)
{
  int myproc;


  /* avoid compiler warning about unused parameter */
  GH = GH;

  myproc = 0;
#ifdef CCTK_MPI
  if(! CCTK_ParamChecking() && MPI_Active)
  {
    CACTUS_MPI_ERROR (MPI_Comm_rank (MPI_COMM_WORLD, &myproc));
  }
#endif

  return (myproc);
}


 /*@@
   @routine    CactusDefaultnProcs
   @date       Tue Jan 23 1999
   @author     Gabrielle Allen
   @desc
               Default Cactus nProcs routine.
   @enddesc
   @calls      CCTK_ParamCheckNProcs
               MPI_Comm_size

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        unused
   @endvar

   @returntype int
   @returndesc
               the total number of processors
   @endreturndesc
@@*/
int CactusDefaultnProcs (const cGH *GH)
{
  int nprocs;


  /* avoid compiler warning about unused parameter */
  GH = GH;

  if (CCTK_ParamChecking ())
  {
    nprocs = CCTK_ParamCheckNProcs ();
  }
  else
  {
    nprocs = 1;
#ifdef CCTK_MPI
    if(MPI_Active)
    {
      CACTUS_MPI_ERROR (MPI_Comm_size (MPI_COMM_WORLD, &nprocs));
    }
#endif
  }

  return (nprocs);
}


 /*@@
   @routine    CactusDefaultExit
   @date       Tue Apr 18 15:21:15 2000
   @author     Gerd Lanfermann
   @desc
               The default for when people call CCTK_Exit.
   @enddesc
   @calls      MPI_Finalize
               exit

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        unused
   @endvar
   @var        retval
   @vdesc      return code to exit with
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               This function should never return.
               But if it does it will return 0.
   @endreturndesc
@@*/
int CactusDefaultExit (cGH *GH, int retval)
{
  /* avoid compiler warning about unused parameter */
  GH = GH;

#ifdef CCTK_MPI
  if(MPI_Active)
  {
    CACTUS_MPI_ERROR (MPI_Finalize ());
  }
#endif
  exit (retval);
  return (0);
}


 /*@@
   @routine    CactusDefaultAbort
   @date       Saturday July 15 2000
   @author     Gabrielle Allen
   @desc
               The default for when people call CCTK_Abort.
   @enddesc
   @calls      MPI_Abort
               exit

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        unused
   @endvar
   @var        retval
   @vdesc      return code to abort with
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               This function should never return.
               But if it does it will return 0.
   @endreturndesc
@@*/
int CactusDefaultAbort (cGH *GH, int retval)
{
  /* avoid compiler warning about unused parameter */
  GH = GH;

#ifdef CCTK_MPI
  if (MPI_Active)
  {
    /* flush stdout and stderr before calling MPI_Abort() because
       some MPI implementations simply kill other MPI processes */
    fflush (stdout);
    fflush (stderr);
    CACTUS_MPI_ERROR (MPI_Abort (MPI_COMM_WORLD, retval));
  }
#else
  /* FIXME */
  /*abort();*/
  retval = retval;
#endif
  exit (0);
  return (0);
}


 /*@@
   @routine    CactusDefaultBarrier
   @date       Tue Apr 18 15:21:42 2000
   @author     Tom Goodale
   @desc
               The default for when people call CCTK_Barrier
   @enddesc

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        unused
   @endvar

   @returntype int
   @returndesc
               0 for success
   @endreturndesc
@@*/
int CactusDefaultBarrier (const cGH *GH)
{
  /* avoid compiler warning about unused parameter */
  GH = GH;

  return (0);
}
