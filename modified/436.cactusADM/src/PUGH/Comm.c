#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      Comm.c
   @date      Thu Feb  4 11:34:29 1999
   @author    Tom Goodale
   @desc 
   Pugh communication functions
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGH/src/Comm.c,v 1.59 2001/10/31 12:04:59 tradke Exp $
 @@*/

/*#define DEBUG_PUGH 1*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h> 
#include <string.h>

#include "cctk.h"

#include "pugh.h"
#include "pughi.h"
#include "pugh_Comm.h"
#include "cctk_Parameters.h"

static const char *rcsid="$Header: /cactus/CactusPUGH/PUGH/src/Comm.c,v 1.59 2001/10/31 12:04:59 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_Comm_c)


/* local function prototypes */
static int PUGH_EnableGArrayGroupComm(pGH *pughGH,
                                      int first_var,
                                      int commflag);
static int PUGH_EnableComm(pGH *pughGH,
                           pComm *comm,
                           int commflag);
static int PUGH_DisableComm(pGH *pughGH,
                            pComm *comm);
static int PUGH_SyncGArrayGroup(pGH *pughGH,
                                int first_var);
static int PUGH_Sync(pGH *pughGH,
                     pComm *comm);
static int PUGH_SyncSingleProc(pGH *pughGH,
                               pComm *comm);


/*@@
  @routine PUGH_SyncGroup
  @author  Thomas Radke
  @date    30 Mar 1999
  @desc
  Synchronizes all variables in the group indicated by groupname.
  Only groups of type GROUP_ARRAY and GROUP_GF can be synchronized.
  @enddesc
  @calls   CCTK_DecomposeName CCTK_GroupIndex CCTK_GroupData CCTK_WARN
           PUGH_SyncGArrayGroup
  @history
 
  @endhistory 
  @var     GH
  @vdesc   Pointer to CCTK grid hierarchy
  @vtype   cGH
  @vio     in
  @endvar 
  @var     groupname
  @vdesc   name of the group to be synchronized
  @vtype   const char *
  @vio     in
  @endvar 
  @@*/

int PUGH_SyncGroup(cGH *GH, const char *groupname)
{
  cGroup pgroup;           /* group information */
  int group;               /* group index */
  int rc;                  /* return code */

#ifdef DEBUG_PUGH
  printf (" PUGH_SyncGroup: request for group '%s'\n", groupname);
  fflush (stdout);
#endif

  /* get the group info from its index */
  group = CCTK_GroupIndex(groupname);
  if (group < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "PUGH_SyncGroup: Unknown group: %s", groupname);
    rc = -1;
  }
  else
  {
    CCTK_GroupData(group, &pgroup);

    if (pgroup.grouptype == CCTK_SCALAR)
    {
      rc = 0;
      CCTK_VWarn(4, __LINE__, __FILE__, CCTK_THORNSTRING,
		 "PUGH_SyncGroup: Synchronising scalar group: %s",groupname);
    }
    else if (pgroup.grouptype == CCTK_GF || pgroup.grouptype == CCTK_ARRAY)
    {
      rc = PUGH_SyncGArrayGroup(PUGH_pGH(GH), CCTK_FirstVarIndexI(group));
    }
    else
    {
      CCTK_WARN(1, "PUGH_SyncGroup: Unknown group type");
      rc = 0;
    }
  }

  return (rc);
}


/*@@
  @routine PUGH_EnableGroupComm
  @author  Thomas Radke
  @date    30 Mar 1999
  @desc
  Enables communication for all variables in the group indicated by groupname.
  @enddesc
  @calls   CCTK_DecomposeName CCTK_GroupIndex CCTK_GroupData CCTK_WARN
  @history
 
  @endhistory 
  @var     GH
  @vdesc   Pointer to CCTK grid hierarchy
  @vtype   cGH
  @vio     in
  @endvar 
  @var     groupname
  @vdesc   name of the group to be synchronized
  @vtype   const char *
  @vio     in
  @endvar 
  @@*/

int PUGH_EnableGroupComm(cGH *GH, const char *groupname)
{
  int group;               /* group index */
  cGroup pgroup;           /* group information */
  int rc;                  /* return code */

#ifdef DEBUG_PUGH
  printf(" PUGH_EnableGroupComm: request for group '%s'\n", groupname);
  fflush(stdout);
#endif

  /* get the group info from its index */
  group = CCTK_GroupIndex(groupname);
  CCTK_GroupData(group, &pgroup);

  if (pgroup.grouptype == CCTK_SCALAR)
  {
    rc = 1;
  }
  else if (pgroup.grouptype == CCTK_GF || pgroup.grouptype == CCTK_ARRAY)
  {
    rc = PUGH_EnableGArrayGroupComm(PUGH_pGH(GH),
                                    CCTK_FirstVarIndexI(group),
                                    PUGH_ALLCOMM);
  }
  else
  {
    CCTK_WARN(1, "Unknown group type in PUGH_EnableGroupComm");
    rc = 0;
  }

  return (rc);
}


/*@@
  @routine PUGH_DisableGroupComm
  @author  Thomas Radke
  @date    30 Mar 1999
  @desc
  Disables communication for all variables in the group indicated by groupname.
  @enddesc
  @calls   CCTK_DecomposeName CCTK_GroupIndex CCTK_GroupData CCTK_WARN
  @history
 
  @endhistory 
  @var     GH
  @vdesc   Pointer to CCTK grid hierarchy
  @vtype   cGH
  @vio     in
  @endvar 
  @var     groupname
  @vdesc   name of the group to be synchronized
  @vtype   const char *
  @vio     in
  @endvar 
  @@*/

int PUGH_DisableGroupComm(cGH *GH, const char *groupname)
{
  int group;               /* group index */
  cGroup pgroup;           /* pointer to group information */
  int rc;                  /* return code */

  pGH *pughGH;
  int var;

#ifdef DEBUG_PUGH
  printf(" PUGH_DisableGroupComm: request for group '%s'\n", groupname);
  fflush(stdout);
#endif

  /* get the group info from its index */
  group = CCTK_GroupIndex(groupname);
  CCTK_GroupData(group, &pgroup);

  if (pgroup.grouptype == CCTK_SCALAR)
  {
    rc = 1;
  }
  else if (pgroup.grouptype == CCTK_GF || pgroup.grouptype == CCTK_ARRAY)
  {
    pughGH=PUGH_pGH(GH);
    var = CCTK_FirstVarIndexI(group);

    /* FIXME:  workaround.  This one is really bad ! */
    rc = PUGH_DisableGArrayGroupComm(pughGH, var,(((pGA ***)pughGH->variables)[var][0])->groupcomm);
  }
  else
  {
    CCTK_WARN(1, "Unknown group type in PUGH_DisableGroupComm");
    rc = 0;
  }

  return (rc);
}


 /*@@
   @routine    PUGH_EnableGArrayComm
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Enables communication for a single array.
   @enddesc
   @history
   @endhistory
@@*/
int PUGH_EnableGArrayComm(pGA *GA,
                          int commflag)
{

#ifdef DEBUG_PUGH
  printf(" PUGH_EnableGArrayComm: request for var '%s' commflag %d\n",
          GA->name, commflag);
  fflush(stdout);
#endif

  return (PUGH_EnableComm((pGH *) GA->parent, GA->comm, commflag));
}


 /*@@
   @routine    PUGH_DisableGArrayComm
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Disables communication for a single array.
   @enddesc
   @history
   @endhistory
@@*/
int PUGH_DisableGArrayComm(pGA *GA)
{

#ifdef DEBUG_PUGH
  printf(" PUGH_DisableGArrayComm: request for var '%s'\n", GA->name);
  fflush(stdout);
#endif

  return (PUGH_DisableComm((pGH *) GA->parent, GA->comm));
}


 /*@@
   @routine    PUGH_SyncGArrayGroup
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Synchronizes a group of arrays
               given the first variable within this group.
   @enddesc
   @history
   @endhistory
@@*/
int PUGH_SyncGArrayGroup(pGH *pughGH,
                         int first_var)
{
  pGA *firstGA;

  firstGA = (pGA *) pughGH->variables [first_var][0];

#ifdef DEBUG_PUGH
  printf(" PUGH_SyncGArrayGroup: request for group with first var '%s'\n",
         firstGA->name);
  fflush(stdout);
#endif

  return (PUGH_Sync(pughGH, firstGA->groupcomm));
}


 /*@@
   @routine    PUGH_SyncGArray
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Synchronizes a single array variable given the GA structure
               of this variable.
   @enddesc
   @history
   @endhistory
@@*/
int PUGH_SyncGArray(pGA *GA)
{

#ifdef DEBUG_PUGH
  printf(" PUGH_SyncGArray: request for var '%s'\n", GA->name);
  fflush(stdout);
#endif

  return (PUGH_Sync((pGH *) GA->parent, GA->comm));
}


int PUGH_Barrier(const cGH *GH)
{
#ifdef CCTK_MPI
  CACTUS_MPI_ERROR (MPI_Barrier (PUGH_pGH (GH)->PUGH_COMM_WORLD));
#else
  GH = GH;
#endif

  return (0);
}


/*****************************************************************************/
/* local functions                                                           */
/*****************************************************************************/

 /*@@
   @routine    PUGH_EnableGArrayGroupComm
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Enables communication for a group of arrays
               given the first variable within this group.
   @enddesc
   @history
   @endhistory
@@*/
static int PUGH_EnableGArrayGroupComm(pGH *pughGH,
                                      int first_var,
                                      int commflag)
{
  pGA *GA;                 /* first variable in group */


  GA = pughGH->variables [first_var][0];

#ifdef DEBUG_PUGH
  printf(" PUGH_EnableGArrayGroupComm: request for group "
         "with first var '%s', commflag %d\n", GA->name, commflag);
  fflush(stdout);
#endif

  return (PUGH_EnableComm(pughGH, GA->groupcomm, commflag));
}


 /*@@
   @routine    PUGH_DisableGArrayGroupComm
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Disables communication for a group of arrays
               given the first variable within this group.
   @enddesc
   @history
   @endhistory
@@*/
int PUGH_DisableGArrayGroupComm(pGH *pughGH,
                                int first_var,
                                pComm *groupcomm)
{
#ifdef DEBUG_PUGH
  pGA *GA;                 /* first variable in group */

  GA = (pGA *) pughGH->variables[first_var][0];
  printf(" PUGH_DisableGArrayGroupComm: request for group "
         "with first var '%s'\n", GA->name);
  fflush(stdout);
#endif

  /* get rid of compiler warning about unused parameters */
  first_var = first_var;

  return (PUGH_DisableComm(pughGH, groupcomm));
}


 /*@@
   @routine    PUGH_EnableComm
   @date       Sun Jan 23 12:46:23 2000
   @author     Gabrielle Allen
   @desc
   This sets the docomm[2*dim] array of the GA based
   on the setting of the comm flag and allocates the comm buffers.
   @enddesc
   @history
               @date Mon Jun 05 2000 @author Thomas Radke
               Moved buffer allocation from PUGH_EnableGArrayDataStorage
   @endhistory
@@*/
static int PUGH_EnableComm(pGH *pughGH,
                           pComm *comm,
                           int commflag)
{
  int retval;              /* return value */
#ifdef CCTK_MPI
  pGA *GA;                 /* GA structure the pComm belongs to */
  int i, idir;             /* loopers */
  int dir;                 /* direction */
  int sz;                  /* buffer size */
#endif


  retval = 1;

#ifdef CCTK_MPI
  /* check whether this comm buffer is already set up */
  if (comm->commflag == commflag)
  {
    return (retval);
  }

  /* get the GA structure the comm structure belongs to
     For a group comm structure this GA is the first one within the group. */
  GA = (pGA *) pughGH->variables [comm->first_var][comm->sync_timelevel];

  if (comm->commflag == PUGH_NOCOMM)
  {

#ifdef DEBUG_PUGH
    printf (" PUGH_EnableComm: allocating comm buffer for %d vars starting "
            "with %d '%s'\n", comm->n_vars, comm->first_var, GA->name);
    fflush (stdout);
#endif

    /* allocate memory for communication buffers: 2 faces per direction */
    for (i = 0; i < 2 * GA->extras->dim; i++)
    {
      if (GA->connectivity->neighbours[pughGH->myproc][i] >= 0)
      {
        dir = i/2;

        /* no ghostzones -> no comm buffers */
        sz = comm->n_vars * GA->extras->nghostzones[dir];
        if (sz > 0)
        {
          for (idir = 0; idir < GA->extras->dim; idir++)
          {
            if (idir != dir)
            {
              sz *= GA->extras->lnsize[idir];
            }
          }
          comm->buffer_sz[i]   = sz;
          comm->send_buffer[i] = malloc(sz * GA->varsize);
          comm->recv_buffer[i] = malloc(sz * GA->varsize);

          if (! (comm->send_buffer[i] && comm->recv_buffer[i]))
          {
            for (; i >=0 ; i--)
            {
              if (comm->send_buffer[i])
              {
                free(comm->send_buffer[i]);
              }
              if (comm->recv_buffer[i])
              {
                free(comm->recv_buffer[i]);
              }
              comm->buffer_sz[i] = 0;
            }

            CCTK_WARN(1, "Out of memory !");
            retval = -1;
            break;
          }
        }
        else
        {
          comm->buffer_sz[i]   = 0;
          comm->send_buffer[i] = NULL;
          comm->recv_buffer[i] = NULL;
        }
      }
      else               /* no neighbor -> no comm buffers */
      {
        comm->buffer_sz[i]   = 0;
        comm->send_buffer[i] = NULL;
        comm->recv_buffer[i] = NULL;
      }
    }
  }

  /* set up comm flags for each face */
  if (retval >= 0)
  {
    /* Copy commflag */
    comm->commflag = commflag;

    /* First set all communcation off */
    for (idir = 0; idir < 2 * GA->extras->dim; idir++)
      comm->docomm[idir] = 0;

    if (commflag == PUGH_ALLCOMM)
    {
      for (idir = 0; idir < 2 * GA->extras->dim; idir++)
      {
        comm->docomm[idir] = comm->buffer_sz[idir] > 0;
      }
    }
    else if (commflag == PUGH_PLUSFACESCOMM)
    {
      for (idir = 0; idir < GA->extras->dim; idir++)
      {
        comm->docomm[2*idir+1] = comm->buffer_sz[2*idir+1] > 0;
      }
    }
    else if (commflag == PUGH_MINUSFACESCOMM)
    {
      for (idir = 0; idir < GA->extras->dim; idir++)
      {
        comm->docomm[2*idir] = comm->buffer_sz[2*idir] > 0;
      }
    }
    else
    {
      for (idir = 0; idir < GA->extras->dim; idir++)
      {
        if (commflag == PUGH_COMM(idir))
        {
          comm->docomm[2*idir] = comm->buffer_sz[2*idir] > 0;
          comm->docomm[2*idir+1] = comm->buffer_sz[2*idir+1] > 0;
        }
      }
    }

    /* FIXME Add back the check that you have a valid COMM model: Gab */

    /* Handle nsize = 1 type cases. This is only important for one
       processor MPI periodic boundaries */

    for (idir = 0; idir < GA->extras->dim; idir++)
    {
      if (GA->extras->nsize[idir] == 1)
      {
        comm->docomm[2*idir] = 0;
        comm->docomm[2*idir+1] = 0;
      }
    }
  }

#else

  /* get rid of compiler warning about unused parameters */
  pughGH = pughGH;
  comm = comm;
  commflag = commflag;

#endif   /* CCTK_MPI */

  return retval;
}


 /*@@
   @routine    PUGH_DisableComm
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               This frees the communication buffers
               of a given comm structure.
   @enddesc
   @history
               Separated from routine PUGH_DisableGArrayDataStorage()
   @endhistory
@@*/
static int PUGH_DisableComm(pGH *pughGH,
                            pComm *comm)
{
#ifdef CCTK_MPI
  int i;                  /* looper */
  pGA *GA;                /* GA structure the comm structure belongs to */


  /* get the GA to which the comm structure belongs to
     For a comm structure this is the first variable within this group. */
  GA = (pGA *) pughGH->variables [comm->first_var][comm->sync_timelevel];

#ifdef DEBUG_PUGH
  printf (" PUGH_DisableComm: freeing comm buffer for group of %d vars and "
          "first var '%s'\n", comm->n_vars, GA->name);
  fflush (stdout);
#endif

  if (comm->commflag != PUGH_NOCOMM)
  {
    /* free memory for communication buffers: 2 faces per direction */
    for (i = 0; i < 2 * GA->extras->dim; i++)
    {
      if (comm->send_buffer[i])
      {
        free(comm->send_buffer[i]);
        comm->send_buffer[i] = NULL;
      }

      if (comm->recv_buffer[i])
      {
        free(comm->recv_buffer[i]);
        comm->recv_buffer[i] = NULL;
      }

      comm->buffer_sz[i] = 0;
    }

    comm->commflag = PUGH_NOCOMM;
  }
  else
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "PUGH_DisableComm: Communication already disabled for "
		"group of %d vars "
                "and first var '%s'", comm->n_vars, GA->name);
  }

#else

  /* get rid of compiler warning about unused parameters */
  pughGH = pughGH;
  comm = comm;

#endif   /* CCTK_MPI */

  return (1);
}


 /*@@
   @routine    PUGH_Sync
   @date       Mon Jun 05 2000
   @author     Thomas Radke
   @desc
               Finally synchronizes a variable or group of variables
               according to a given comm structure.
   @enddesc
   @calls      PUGH_SyncSingleProc
   @history
   @endhistory
@@*/
static int PUGH_Sync(pGH *pughGH,
                     pComm *comm)
{
#ifdef CCTK_MPI
  int dir;
  pGA *GA;
  MPI_Status mss;
#ifdef PUGH_WITH_DERIVED_DATATYPES
  int i;
  MPI_Request *sr;
#endif
#ifdef COMM_TIMING
  double t1, t2;
#endif
#endif


  /* single-processor case in handled in separate routine */
  if (pughGH->nprocs == 1)
  {
    return (PUGH_SyncSingleProc (pughGH, comm));
  }

#ifdef CCTK_MPI

  /* start the timer for communication time */
  if (pughGH->comm_time >= 0)
  {
    CCTK_TimerStartI (pughGH->comm_time);
  }

  GA = (pGA *) pughGH->variables [comm->first_var][comm->sync_timelevel];

#ifdef PUGH_WITH_DERIVED_DATATYPES
  if (pughGH->commmodel == PUGH_DERIVEDTYPES) 
  {
    /* 2 faces, send and receive is the 2 * 2 */
    sr = (MPI_Request *) malloc(comm->n_vars * 2 * 2 * sizeof(MPI_Request));
  }
#endif

#ifdef DEBUG_PUGH
  printf (" PUGH_Sync: syncing group of %d vars with first var '%s'\n",
          comm->n_vars, GA->name);
  fflush (stdout);
#endif

  for (dir = 0; dir < GA->extras->dim; dir ++) 
  {

#ifdef COMM_TIMING
    t1 = MPI_Wtime();
#endif

    PostReceiveGA(pughGH, 2*dir, comm);
    PostReceiveGA(pughGH, 2*dir+1, comm);
    
#ifdef COMM_TIMING
    t2 = MPI_Wtime();
    printf("PR : %f\n",t2-t1);
#endif

    PostSendGA(pughGH, 2*dir, comm);
    PostSendGA(pughGH, 2*dir+1, comm);

#ifdef COMM_TIMING
    t1 = MPI_Wtime();
    printf("PS : %f\n",t1-t2);
#endif

    /* Now comes the big difference between derived types and
       allocated buffers. With derived types, we now have to
       wait on all our recieve AND SEND buffers so we can
       keep on using the send buffers ( as communications are
       in-place). With the allocated we have to wait on each
       recieve, but not on the send, since we don't need the
       send buffer until we pack a send again (above)
    */
    
    if (pughGH->commmodel == PUGH_ALLOCATEDBUFFERS) 
    {
      /* Do a wait any on the receives */
      MPI_Wait(&comm->rreq[2*dir], &mss);
      FinishReceiveGA(pughGH, 2*dir, comm);
      MPI_Wait(&comm->rreq[2*dir+1], &mss);
      FinishReceiveGA(pughGH, 2*dir+1, comm);
    }
#ifdef PUGH_WITH_DERIVED_DATATYPES
    else if (pughGH->commmodel == PUGH_DERIVEDTYPES) 
    {
      /* Load up the thing for the waitall */
      for (i = 0; i < comm->n_vars; i++)
      {
        int id = i * 2 * 2;
        pGA *GA = (pGA *) pughGH->variables [i][comm->sync_timelevel];

        if (GA->comm->docomm[2*dir] &&
            GA->storage)
        {
          sr[id] = GA->comm->sreq[2*dir];
          sr[id+1] = GA->comm->rreq[2*dir];
        }
        else
        {
          sr[id] = MPI_REQUEST_NULL;
          sr[id+1] = MPI_REQUEST_NULL;
        }

        if (GA->comm->docomm[2*dir+1] &&
            GA->storage)
        {
          sr[id+2] = GA->comm->sreq[2*dir+1];
          sr[id+3] = GA->comm->rreq[2*dir+1];
        }
        else
        {
          sr[id+2] = MPI_REQUEST_NULL;
          sr[id+3] = MPI_REQUEST_NULL;
        }
      }
      /* Now do a waitall */
      MPI_Waitall(4*comm->n_vars, sr, &mss);
    }
#endif

#ifdef COMM_TIMING
    t2 = MPI_Wtime();
    printf("FR : %f\n",t2-t1);
#endif

  }

#ifdef PUGH_WITH_DERIVED_DATATYPES
  if (pughGH->commmodel == PUGH_DERIVEDTYPES) 
  {
    free(sr);
  }
  else
#endif
  {
    /* wait for MPI to finish all outstanding send requests */
    CACTUS_MPI_ERROR (MPI_Waitall (2 * GA->extras->dim, comm->sreq,
                                   comm->sstatus));
  }

  /* get the time spent in communication */
  if (pughGH->comm_time >= 0)
  {
    CCTK_TimerStopI(pughGH->comm_time);
  }

#endif /* CCTK_MPI */

  return (0);
}


 /*@@
   @routine    PUGH_SyncSingleProc
   @date       Sun Jan 14 2001
   @author     Thomas Radke
   @desc
               Finally synchronizes a variable or group of variables
               in the single-processor case.
   @enddesc
   @history
   @endhistory
@@*/
static int PUGH_SyncSingleProc(pGH *pughGH,
                               pComm *comm)
{
  pGA *GA;
  int i, face, dim, copy_bytes, offset_from, offset_to;
  char *data;
  int *istart_from, *iend_from, *iterator_from;
  int *istart_to, *iterator_to;


  GA = (pGA *) pughGH->variables [comm->first_var][comm->sync_timelevel];

  /* since we need to iterators here we need to allocate one */
  iterator_from = GA->extras->iterator;
  iterator_to = (int *) malloc (GA->extras->dim * sizeof (int));

  /* loop over all faces */
  for (face = 0; face < 2 * GA->extras->dim; face++)
  {

    /* there's only something to do for periodic boundary conditions */
    if (GA->connectivity->perme[face / 2])
    {
      /* get the index ranges for the nested loops */
      istart_from   = GA->extras->overlap[GA->stagger][0][face];
      iend_from     = GA->extras->overlap[GA->stagger][1][face];
      if (face & 1)
      {
        istart_to   = GA->extras->ghosts[GA->stagger][0][face - 1];
      }
      else
      {
        istart_to   = GA->extras->ghosts[GA->stagger][0][face + 1];
      }

      /* set iterators to the start vector */
      for(dim = 0; dim < GA->extras->dim; dim++)
      {
        iterator_from[dim] = istart_from[dim];
        iterator_to[dim] = istart_to[dim];
      }

      /* get the number of bytes to copy in the lowest dimension */
      copy_bytes = (iend_from[0] - istart_from[0]) * GA->varsize;

      /* now do the nested loops starting with the innermost */
      dim = 1;
      while (1)
      {
        /* check for end of current loop */
        if (GA->extras->dim > 1 && iterator_from[dim] >= iend_from[dim])
        {
          /* increment outermost loopers */
          for (dim++; dim < GA->extras->dim; dim++)
          {
            iterator_from[dim]++;
            iterator_to[dim]++;
            if (iterator_from[dim] < iend_from[dim])
            {
              break;
            }
          }

          /* done if beyond outermost loop */
          if (dim >= GA->extras->dim)
          {
            break;
          }

          /* reset innermost loopers */
          for (dim--; dim > 0; dim--)
          {
            iterator_from[dim] = istart_from[dim];
            iterator_to[dim] = istart_to[dim];
          }
          dim = 1;
        }

        /* get the linear offsets */
        offset_from = istart_from[0];
        offset_to   = istart_to[0];
        for (i = 1; i < GA->extras->dim; i++)
        {
          offset_from += iterator_from[i] * GA->extras->hyper_volume[i];
          offset_to   += iterator_to[i]   * GA->extras->hyper_volume[i];
        }
        offset_from *= GA->varsize;
        offset_to   *= GA->varsize;

        /* copy the data for all the variables in the comm structure */
        for (i = comm->first_var; i < comm->first_var + comm->n_vars; i++)
        {
          data = ((pGA *) pughGH->variables[i][comm->sync_timelevel])->data;
          memcpy (data + offset_to, data + offset_from, copy_bytes);
        }

        /* increment current loopers */
        if (GA->extras->dim > 1)
        {
          /* increment current looper */
          iterator_from[dim]++;
          iterator_to[dim]++;
        }
        else
        {
          /* exit loop if array dim is 1D */
          break;
        }

      } /* end of nested loops over all dimensions */
    }
  }

  /* free the allocated iterator */
  free (iterator_to);

  return (0);
}
