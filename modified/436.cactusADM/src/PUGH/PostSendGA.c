#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      PostSendGA.c
   @date      Wed  Feb  2 11:58:14 2000
   @author    Gabrielle Allen
   @desc 
   The send of the three-way pugh communcations branch
   which is @seefile PostReceiveGA.c, @seefile PostSendGA.c
   and @seefile FinishReceiveGA.c. See the documentation for
   @seeroutine PostSendGA for details on this routine.
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGH/src/PostSendGA.c,v 1.14 2001/06/18 14:38:57 tradke Exp $
 @@*/

#include <string.h>

#include "cctk.h"
#include "pugh.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/PostSendGA.c,v 1.14 2001/06/18 14:38:57 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_PostSendGA_c)

/*#define DEBUG_PUGH*/

#ifdef CCTK_MPI

void PostSendGA(pGH *pughGH, int dir, pComm *comm);

 /*@@
   @routine    PostSendGA
   @date       Thu Apr  3 11:58:59 1997
   @author     Paul Walker
   @desc 
   This routine posts an asyncronous MPI send of a buffer
   for a face (MPI_Isend). It does this by first packing up
   a send buffer (allocated in @seeroutine PUGH_SetupGArrayGroupComm) then
   sending it out on the pipe.
   <p>
   Since this is an asynchronous communications model we
   assume that a recieve has been posted for the send.
   thus the correct calling order for this is as in
   @seeroutine PUGH_Sync, eg, after a Recv.
   <p>
   Note this does <b>not</b> wait on the send buffer from previous
   communications. It is the users responsibility to wait on
   that buffer.
   @enddesc 
   @calledby   PUGH_Sync
   @history
   @hdate Nov 4 1998 @hauthor Gabrielle Allen
   @hdesc Allow for forced synchronization of all GFs with storage
   @endhistory
@@*/

void PostSendGA(pGH *pughGH, int dir, pComm *comm)
{
  pGA *GA;
  int stag, dircomp;
  int neighbour;
  int i, dim, copy_bytes, offset;
  char *copy_from, *copy_to;
  int *istart, *iend, *iterator;


  GA = (pGA *) pughGH->variables[comm->first_var][0];

  /* return if no storage assigned */
  if (! GA->storage)
  {
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

  dircomp = dir+1;                /* Complementary direction */
  if (dircomp % 2 == 0) 
  {
    dircomp = dir-1;
  }

  /* note this is the complement of the rtag set in PostReceiveGA */
  stag = 1000 + dircomp + 2 * (neighbour + pughGH->nprocs * GA->id);
  /* mod the tag to force MPI compliance */
  stag = stag % 32768;

#ifdef DEBUG_PUGH
  printf ("PostSendGA: into direction %d to proc %d with stag %d size %d for "
          "%d vars starting with '%s'\n",
          dir, neighbour, stag, comm->buffer_sz[dir], comm->n_vars, GA->name);
#endif

  if (pughGH->commmodel == PUGH_ALLOCATEDBUFFERS) 
  {

    istart   = GA->extras->overlap[GA->stagger][0][dir];
    iend     = GA->extras->overlap[GA->stagger][1][dir];
    iterator = GA->extras->iterator;

    /* set iterator to the start vector */
    for(dim = 0; dim < GA->extras->dim; dim++)
    {
      iterator[dim] = istart[dim];
    }

    /* get the source and the number of bytes to copy in the lowest dimension */
    copy_to    = comm->send_buffer[dir];
    copy_bytes = (iend[0] - istart[0]) * GA->varsize;

    /* now do the nested loops starting with the innermost */
    dim = 1;
    while (1)
    {
      /* check for end of current loop */
      if (GA->extras->dim > 1 && iterator[dim] >= iend[dim])
      {
        /* increment outermost loopers */
        for (dim++; dim < GA->extras->dim; dim++)
        {
          iterator[dim]++;
          if (iterator[dim] < iend[dim])
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
          iterator[dim] = istart[dim];
        }
        dim = 1;
      }

      /* get the linear offset */
      offset = istart[0];
      for (i = 1; i < GA->extras->dim; i++)
      {
        offset += iterator[i] * GA->extras->hyper_volume[i];
      }
      offset *= GA->varsize;

      /* copy the data in dimension 0 */
      for (i = comm->first_var; i < comm->first_var + comm->n_vars; i++)
      {
        copy_from = ((pGA *) pughGH->variables[i][comm->sync_timelevel])->data;
        memcpy (copy_to, copy_from + offset, copy_bytes);
        copy_to += copy_bytes;
      }

      if (GA->extras->dim > 1)
      {
        /* increment current looper */ 
        iterator[dim]++;
      }
      else
      {
        /* exit loop if array dim is 1D */
        break;
      }

    } /* end of nested loops over all dimensions */

    /* now post send */
    CACTUS_MPI_ERROR (MPI_Isend (comm->send_buffer[dir],
                                 comm->buffer_sz[dir], comm->mpi_type,
                                 neighbour, stag,
                                 pughGH->PUGH_COMM_WORLD, &comm->sreq[dir]));
  }
#ifdef PUGH_WITH_DERIVED_DATATYPES
  else if (pughGH->commmodel == PUGH_DERIVEDTYPES)
  {
    int var;
    MPI_Datatype *send_dt;


    switch (GA->vtype) 
    {

      case CCTK_VARIABLE_CHAR:
        send_dt = pughGH->send_char_dt[GA->stagger];
        break;

      case CCTK_VARIABLE_INT:
        send_dt = pughGH->send_int_dt[GA->stagger];
        break;

      case CCTK_VARIABLE_REAL:
        send_dt = pughGH->send_real_dt[GA->stagger];
        break;

      case CCTK_VARIABLE_COMPLEX:
        send_dt = pughGH->send_complex_dt[GA->stagger];
        break;

      default:
        CCTK_WARN (1, "Unknown variable type in PostSendGA");
        return;

    }

    for (var = comm->first_var; var < comm->first_var + comm->n_vars; var++)
    {
      pGA *GA = (pGA *) pughGH->variables[var][comm->sync_timelevel];


      CACTUS_MPI_ERROR (MPI_Isend (GA->data,
                                   1, send_dt[dir],
                                   neighbour, (stag + var) % 32768,
                                   pughGH->PUGH_COMM_WORLD,
                                   &GA->comm->sreq[dir]));
      CACTUS_MPI_ERROR (MPI_Request_free (&comm->sreq[dir]));
    }
  }
#endif
}
#endif /* CCTK_MPI */
