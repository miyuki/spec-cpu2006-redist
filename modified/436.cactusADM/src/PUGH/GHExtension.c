#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      GHExtension.c
   @date      Thu Feb  4 10:47:14 1999
   @author    Tom Goodale
   @desc 
              Pugh GH extension stuff.
   @enddesc 
   @version   $Id: GHExtension.c,v 1.59 2001/11/05 15:01:46 tradke Exp $
 @@*/

/* #define DEBUG_PUGH 1 */

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Schedule.h"
#include "cctk_Parameters.h"

#include "pugh.h"
#include "pugh_extension.h"


static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/GHExtension.c,v 1.59 2001/11/05 15:01:46 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_pugh_GHExtension_c)


/* prototypes of routines defined in this source file */
int PUGH_PrintTimingInfo (cGH *GH);
void PUGH_RegisterCallFunc (int (* func) (void *function,
                                          cFunctionData *fdata,
                                          void *data));

static int (* PUGHCallFunc)(void *function, cFunctionData *fdata, void *data) = NULL;

 /*@@
   @routine    PUGH_SetupGH
   @date       Wed Feb  3 23:10:19 1999
   @author     Tom Goodale
   @desc 
               Sets up a new PUGH GH extension structure.
   @enddesc 
   @calls      PUGH_GFSize
               PUGH_GFGhostsize
               PUGH_GFPeriodic
               PUGH_SetupPGH
               CCTK_GroupData
               CCTK_GroupSizesI
               CCTK_GroupGhostsizesI
               PUGH_SetupGroup
   @var        config
   @vdesc      pointer to flesh configuration structure
   @vtype      tFleshConfig *
   @vio        not used
   @endvar
   @var        convergence_level
   @vdesc      convergence level of this GH
   @vtype      int
   @vio        not used
   @endvar
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        out
   @endvar

   @returntype void *
   @returndesc
               pointer to new PUGH GH extension, or NULL in case of errors.
   @endreturndesc
@@*/
void *PUGH_SetupGH (tFleshConfig *config, 
                    int           convergence_level, 
                    cGH          *GH)
{
  int i, group, maxdim;
  pGH *newGH;
  cGroup pgroup;
  CCTK_INT **size;
  int *nsize, *ghostsize, *perme, *groupghostsize, *groupsize;
  DECLARE_CCTK_PARAMETERS


  /* avoid compiler warnings about unused parameters */
  config = config;
  convergence_level = convergence_level;

  /* Set up the GH */
  maxdim = CCTK_MaxDim ();

  nsize          = (int *) malloc (5 * maxdim * sizeof (int));
  ghostsize      = nsize + 1*maxdim;
  perme          = nsize + 2*maxdim;

  PUGH_GFSize (maxdim, nsize);
  PUGH_GFGhostsize (maxdim, ghostsize);
  PUGH_GFPeriodic (maxdim, perme);

  newGH = PUGH_SetupPGH (GH, maxdim, nsize, ghostsize, 
                         CCTK_StaggerVars() ? PUGH_STAGGER : PUGH_NO_STAGGER,
                         perme);
  if (!newGH)
  {
    CCTK_WARN (0, "Failed to allocate memory for a pGH");
  }

  /* Set the identity for this pGH. For now this is an empty string. */
  newGH->identity_string = (char *) calloc (1, sizeof (char));

  /* Set up groups on the GH */
  for (group = 0; group < CCTK_NumGroups (); group++)
  {
    CCTK_GroupData (group, &pgroup);
    groupsize      = nsize + 3*maxdim;
    groupghostsize = nsize + 4*maxdim;

    /* Set the global size of the variables in the group */
    size = CCTK_GroupSizesI (group);
    if (size) 
    {
      for (i = 0; i < pgroup.dim; i++)
      {
        groupsize[i] = *size[i];
        /* Check distribution type */
        if (pgroup.disttype == CCTK_DISTRIB_CONSTANT)
        {
          groupsize[i] = -groupsize[i];
        }
      }
    }
    else
    {
      groupsize = NULL;
    }

    /* Set the ghostzone size of the variables in the group */
    size = CCTK_GroupGhostsizesI (group);
    if (size) 
    {
      for (i = 0; i < pgroup.dim; i++)
      {
        groupghostsize[i] = *size[i];
      }
    }
    else if (pgroup.grouptype == CCTK_ARRAY)
    {
      memset (groupghostsize, 0, pgroup.dim * sizeof (int));
    }
    else
    {
      groupghostsize = NULL;
    }

    PUGH_SetupGroup (newGH, groupsize, groupghostsize, pgroup.grouptype, 
                     pgroup.vartype, pgroup.dim, pgroup.numvars, 
                     pgroup.stagtype, pgroup.numtimelevels,pgroup.vectorgroup);
  }

  /* clean up */
  free (nsize);

  USE_CCTK_PARAMETERS;   return (newGH);
}


 /*@@
   @routine    PUGH_InitGHBasics
   @date       Thu May 11 2000
   @author     Thomas Radke
   @desc 
               Initializes the basic fields of a cGH structure.
   @enddesc 
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        out
   @endvar
@@*/
static void PUGH_InitGHBasics (cGH *GH)
{
  int  dim, stagger;
  pGH *mypGH;
  pGExtras *GFExtras;


  mypGH = PUGH_pGH (GH);

  if (GH->cctk_dim > 0)
  {
    GFExtras = mypGH->GFExtras[GH->cctk_dim - 1];
  }
  else
  {
    GFExtras = NULL;
  }

  GH->cctk_convlevel = 0;

  for (dim = 0; dim < GH->cctk_dim; dim++)
  {
    GH->cctk_levfac[dim]      = 1;
    GH->cctk_nghostzones[dim] = GFExtras->nghostzones[dim];
    GH->cctk_lsh[dim]         = GFExtras->lnsize[dim];
    GH->cctk_gsh[dim]         = GFExtras->nsize[dim];
    GH->cctk_bbox[2*dim]      = GFExtras->bbox[2*dim];
    GH->cctk_bbox[2*dim+1]    = GFExtras->bbox[2*dim+1];
    GH->cctk_lbnd[dim]        = GFExtras->lb[mypGH->myproc][dim];
    GH->cctk_ubnd[dim]        = GFExtras->ub[mypGH->myproc][dim];
    
    for (stagger = 0; stagger < CCTK_NSTAGGER; stagger++) 
    {
      GH->cctk_lssh[CCTK_LSSH_IDX (stagger, dim)] = GH->cctk_lsh[dim];
      if (GH->cctk_bbox[2*dim + 1] == 1 && stagger > 0)
      {
        GH->cctk_lssh[CCTK_LSSH_IDX (stagger, dim)]--;
      }
    }
  }
}


 /*@@
   @routine    PUGH_InitGHVariables
   @date       Thu May 11 2000
   @author     Thomas Radke
   @desc 
               Initializes the variables of a cGH structure.
   @enddesc 
   @calls      CCTK_GroupTypeFromVarI
               CCTK_NumTimeLevelsFromVarI
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        out
   @endvar
@@*/
static void PUGH_InitGHVariables (cGH *GH)
{
  int  var, gtype, level, ntimelevels;
  pGH *mypGH;


  mypGH = PUGH_pGH (GH);

  for (var = 0; var < mypGH->nvariables; var++)
  {
    gtype = CCTK_GroupTypeFromVarI (var);
    ntimelevels = CCTK_NumTimeLevelsFromVarI (var);

    for (level = 0; level < ntimelevels; level++)
    {
      switch (gtype)
      { 
        case CCTK_SCALAR:
          GH->data[var][level] = mypGH->variables[var][level]; 
          break;
        case CCTK_GF:
        case CCTK_ARRAY:
          GH->data[var][level] = ((pGA *) mypGH->variables[var][level])->data;
          break;
        default:
          CCTK_WARN (1, "PUGH_InitGHVariables: Unknown group type");
      }
    }
  }
}


 /*@@
   @routine    PUGH_InitGH
   @date       Wed Feb  3 23:10:19 1999
   @author     Tom Goodale
   @desc 
               Initialization routine for a given GH structure.
   @enddesc 
   @calls      CCTK_NumGroups
               CCTK_GroupName
               CCTK_EnableGroupStorage
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        out
   @endvar

   @returntype void *
   @returndesc
               always returns 0
   @endreturndesc
@@*/
int PUGH_InitGH (cGH *GH)
{
  int i;
  DECLARE_CCTK_PARAMETERS


  PUGH_InitGHBasics (GH);

  /* Enable storage for all variables if required */
  if (enable_all_storage)
  {
    for (i = 0; i < CCTK_NumGroups (); i++)
    { 
      CCTK_EnableGroupStorage (GH, CCTK_GroupName (i));
    }
  }

  USE_CCTK_PARAMETERS;   return (0);
}


 /*@@
   @routine    PUGH_ScheduleTraverseGH
   @date       Thu Jan 27 15:14:09 2000
   @author     Tom Goodale
   @desc 
               Fills out the cGH and then calls schedule traverse.
   @enddesc 
   @calls     
   @history 
   @hdate      Thu Jan 27 15:15:35 2000
   @hauthor    Tom Goodale
   @hdesc      Was originally PUGH_rfrTraverse 
   @hdate      Thu May 11 2000 
   @hauthor    Thomas Radke
   @hdesc      Splitted pGH setup into PUGH_InitGHBasics() and
               PUGH_InitGHVariables()
   @endhistory 

   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        where
   @vdesc      name of this scheduling bin
   @vtype      const char *
   @vio        in
   @endvar

   @returntype void *
   @returndesc
               return code of @seeroutine CCTK_ScheduleTraverse
   @endreturndesc
@@*/
int PUGH_ScheduleTraverseGH (cGH *GH, 
                             const char *where)
{
  int retval;


  if (strcmp ("CCTK_SHUTDOWN", where))
  {
    PUGH_InitGHBasics (GH);
    PUGH_InitGHVariables (GH);
  }

  retval = CCTK_ScheduleTraverse (where, GH, PUGHCallFunc);

  return (retval);
}

void PUGH_RegisterCallFunc (int (* func) (void *function,
                                          cFunctionData *fdata,
                                          void *data))
{
  PUGHCallFunc = func;
}

 /*@@
   @routine    PUGH_GFSize
   @date       Wed Feb  3 23:10:19 1999
   @author     Tom Goodale
   @desc 
               Gets the size of a grid function of a given dimension.
               FIXME: This cannot be made static' because it is used by
                      thorn BAM_Elliptic.
   @enddesc 
   @var        dim
   @vdesc      dimension of the grid function
   @vtype      int
   @vio        in
   @endvar
   @var        nsize
   @vdesc      size in every direction
   @vtype      int *
   @vio        out
   @endvar
@@*/
/* static */ void PUGH_GFSize (int dim, int *nsize)
{
  int dir, size;
  DECLARE_CCTK_PARAMETERS


  if (global_nsize > 0 || local_nsize > 0) 
  {
    size = global_nsize > 0 ? global_nsize : -local_nsize;
    for (dir = 0; dir < dim; dir++)
    {
      nsize[dir] = size;
    }
  }
  else
  {
    switch (dim)
    {
      case 3: nsize[2] = local_nz > 0 ? -local_nz : global_nz;
      case 2: nsize[1] = local_ny > 0 ? -local_ny : global_ny;
      case 1: nsize[0] = local_nx > 0 ? -local_nx : global_nx;
    }
  }
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    PUGH_GFGhostsize
   @date       Wed Feb  3 23:10:19 1999
   @author     Tom Goodale
   @desc 
               Gets the ghostsize of a grid function of a given dimension.
               FIXME: This cannot be made static' because it is used by
                      thorn BAM_Elliptic.
   @enddesc 
   @var        dim
   @vdesc      dimension of the grid function
   @vtype      int
   @vio        in
   @endvar
   @var        ghostsize
   @vdesc      ghostsize in every direction
   @vtype      int *
   @vio        out
   @endvar
@@*/
/* static */ void PUGH_GFGhostsize (int dim, int *ghostsize)
{
  int dir;
  DECLARE_CCTK_PARAMETERS


  if (ghost_size >= 0)
  {
    for (dir = 0; dir < dim; dir++)
    {
      ghostsize[dir] = ghost_size;
    }
  }
  else
  {
    memset (ghostsize, 0, dim);
    switch (dim)
    {
      case 3: ghostsize[2] = ghost_size_z;
      case 2: ghostsize[1] = ghost_size_y;
      case 1: ghostsize[0] = ghost_size_x;
    }
  }
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    PUGH_GFPeriodic
   @date       Wed Feb  3 23:10:19 1999
   @author     Tom Goodale
   @desc 
               Gets the periodic BC of a grid function of a given dimension.
               FIXME: This cannot be made static' because it is used by
                      thorn BAM_Elliptic.
   @enddesc 
   @var        dim
   @vdesc      dimension of the grid function
   @vtype      int
   @vio        in
   @endvar
   @var        perme
   @vdesc      periodicity in every direction
   @vtype      int *
   @vio        out
   @endvar
@@*/
/* static */ void PUGH_GFPeriodic (int dim, int *perme)
{
  DECLARE_CCTK_PARAMETERS


  if (periodic)
  {
    switch (dim)
    {
      case 3: perme[2] = periodic_z;
      case 2: perme[1] = periodic_y;
      case 1: perme[0] = periodic_x;
    }
  }
  else
  {
    memset (perme, 0, dim * sizeof (int));
  }    
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    PUGH_PrintTimingInfo
   @date       Wed Feb  3 23:10:19 1999
   @author     Tom Goodale
   @desc 
               Prints the info from PUGH communication timers to stdout.
   @enddesc 
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        out
   @endvar

   @returntype int
   @returndesc
               always returns 0
   @endreturndesc
@@*/
int PUGH_PrintTimingInfo (cGH *GH)
{
  int i;
  cTimerData *info;
  pGH *pughGH = PUGH_pGH (GH);


  if (pughGH->comm_time < 0)
  {
    return 0;
  }

  info = CCTK_TimerCreateData ();
  if (info)
  {
    CCTK_INFO ("Elapsed time spent in communication:");
    CCTK_TimerI (pughGH->comm_time, info);
    for (i = 0; i < info->n_vals; i++)
    {
      switch (info->vals[i].type)
      {
        case val_int:
          CCTK_VInfo (CCTK_THORNSTRING, "  %s: %d %s", info->vals[i].heading,
                      info->vals[i].val.i, info->vals[i].units);
          break;

        case val_long:
          CCTK_VInfo (CCTK_THORNSTRING, "  %s: %d %s", info->vals[i].heading,
                      (int) info->vals[i].val.l, info->vals[i].units);
          break;

        case val_double:
          CCTK_VInfo (CCTK_THORNSTRING, "  %s: %.3f %s", info->vals[i].heading,
                      info->vals[i].val.d, info->vals[i].units);
          break;

        default:
          CCTK_WARN (1, "Unknown data type for timer info");
          break;
      }
    }

    CCTK_TimerDestroyData (info);
  }
  else
  {
    CCTK_WARN (1, "Couldn't create timer info structure ! "
                  "No timing output available.");
  }

  return (0);
}
