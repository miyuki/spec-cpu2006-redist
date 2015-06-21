#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      PostReceiveGA.c
   @date      Wed Feb 2 11:28:06 1997
   @author    Gabrielle Allen
   @desc 
   Routines which post all the IRecv commands for a single sync.
   These allow the asyncronous model proposed in, for example,
   @seeroutine PUGH_Sync to go!
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGH/src/PostReceiveGA.c,v 1.10 2001/05/10 17:10:17 allen Exp $
 @@*/

#include <stdio.h>

#include "pugh.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/PostReceiveGA.c,v 1.10 2001/05/10 17:10:17 allen Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_PostReceiveGA_c)

/*#define DEBUG_PUGH*/


#ifdef CCTK_MPI

void PostReceiveGA(pGH *pughGH, int dir, pComm *comm);

 /*@@
   @routine   PostReceiveGA
   @date      Thu Apr  3 12:10:46 1997
   @author    Paul Walker
   @desc 
   This routine posts a recieve (MPI_Irecv) of the buffer
   we want to get the send from another processor, which
   will be sent by @seeroutine PostSendGA and then
   finalized by @seeroutoine FinishRecvGA. 
   <p>
   Aside from a silly calculation to get a unique tag
   based on neigbors and processors, this is a very
   straightforward routine.
   @enddesc 
   @history
   @hdate Nov 4 1998 @hauthor Gabrielle Allen
   @hdesc Allow for forced synchronization of all GAs with storage
   @endhistory
 @@*/

void PostReceiveGA(pGH *pughGH, int dir, pComm *comm)
{
  pGA *GA;
  int rtag;
  int neighbour;

  GA = (pGA *) pughGH->variables[comm->first_var][0];

  /* return if no storage assigned */
  if (! GA->storage)
  {
    CCTK_VWarn(2, __LINE__, __FILE__, CCTK_THORNSTRING,
               "Trying to synchronize variable '%s' with no storage", GA->name);
    return;
  }

  /* return if communication not required and no forced synchronisation */
  if (! (pughGH->forceSync || comm->docomm[dir]))
  {
    return;
  }

  /* return if there is no neighbour in the given direction */
  neighbour = GA->connectivity->neighbours[pughGH->myproc][dir];
  if (neighbour < 0)
  {
    return;
  }

  /* note this is the complement of the stag set in PostSendGA */
  rtag = 1000 + dir + 2 * (pughGH->myproc + pughGH->nprocs * GA->id);
  /* mod the tag to force MPI compliance */
  rtag = rtag % 32768;

#ifdef DEBUG_PUGH
  printf ("PostReceiveGA: into direction %d from proc %d with rtag %d size %d "
          "for %d vars starting from '%s'\n",
          dir, neighbour, rtag, comm->buffer_sz[dir], comm->n_vars, GA->name);
#endif

  if (pughGH->commmodel == PUGH_ALLOCATEDBUFFERS)
  {
    CACTUS_MPI_ERROR (MPI_Irecv (comm->recv_buffer[dir],
                                 comm->buffer_sz[dir], comm->mpi_type,
                                 neighbour, rtag,
                                 pughGH->PUGH_COMM_WORLD, &comm->rreq[dir]));
  }
#ifdef PUGH_WITH_DERIVED_DATATYPES
  else if (pughGH->commmodel == PUGH_DERIVEDTYPES)
  {
    int var;
    MPI_Datatype *recv_dt;


    switch (GA->vtype) 
    {
      case CCTK_VARIABLE_CHAR:
        recv_dt = pughGH->recv_char_dt[GA->stagger];
        break;

      case CCTK_VARIABLE_INT:
        recv_dt = pughGH->recv_int_dt[GA->stagger];
        break;

      case CCTK_VARIABLE_REAL:
        recv_dt = pughGH->recv_real_dt[GA->stagger];
        break;

      case CCTK_VARIABLE_COMPLEX:
        recv_dt = pughGH->recv_complex_dt[GA->stagger];
        break;

      default:
        CCTK_WARN (1, "Unknown variable type in PostReceiveGA");
        return;
    }

    for (var = comm->first_var; var < comm->first_var + comm->n_vars; var++)
    {
      pGA *GA = (pGA *) pughGH->variables[var][comm->sync_timelevel];


      CACTUS_MPI_ERROR (MPI_Irecv (GA->data,
                                   1, recv_dt[dir],
                                   neighbour, (rtag + var) % 32768,
                                   pughGH->PUGH_COMM_WORLD,
                                   &GA->comm->rreq[dir]));
    }
  }
#endif
}
#endif   /* CCTK_MPI */
