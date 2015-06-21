#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      FinishReceiveGA.c
   @date      Wed Feb 2 11:37:28 1997
   @author    Gabrielle Allen
   @desc
              The routine which finalize the MPI recieves for a communication.
              Critically linked with @seefile PostRecvGA.c and
              @seefile PostSendGA.c
   @enddesc
   @version   $Id: FinishReceiveGA.c,v 1.11 2001/06/12 22:14:24 tradke Exp $
 @@*/

/*#define DEBUG_PUGH*/

#include <string.h>

#include "pugh.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/FinishReceiveGA.c,v 1.11 2001/06/12 22:14:24 tradke Exp $";
CCTK_FILEVERSION(CactusPUGH_PUGH_FinishReceiveGA_c)


#ifdef CCTK_MPI

void FinishReceiveGA(pGH *pughGH, int dir, pComm *comm);

 /*@@
   @routine    FinishReceiveGA
   @date       Thu Apr  3 11:38:07 1997
   @author     Paul Walker
   @desc
   This routine finalizes the MPI communication through a face.
   It is crucially linked with @seeroutine PostRecvGA and
   @seeroutine PostSendGA.
   <p>
   <b>Important!</b>
   This routine does <b>not</b> wait on the recieves.
   Before it is called, you must do the MPI_Wait or
   else you will not get the right answer. for an
   example of this, see @seeroutine PUGH_DisableArrayGroupComm or
   @seeroutine PUGH_Sync
   @enddesc
   @calledby   PUGH_Sync
   @history
   @hdate Nov 4 1998 @hauthor Gabrielle Allen
   @hdesc Allow for forced synchronization of all GAs with storage
   @endhistory
@@*/

void FinishReceiveGA(pGH *pughGH, int dir, pComm *comm)
{
  pGA *GA;
  int i, dim, copy_bytes, offset;
  char *copy_from, *copy_to;
  int *istart, *iend, *iterator;


  GA = (pGA *) pughGH->variables[comm->first_var][0];

  /* Return if GA has no storage */
  if (! GA->storage)
  {
    return;
  }

  /* Return if communication not required and no forced synchronisation */
  if (! (pughGH->forceSync || comm->docomm[dir]))
  {
    return;
  }

  if (GA->connectivity->neighbours[pughGH->myproc][dir] < 0)
  {
    return;
  }

  /* Here wait one at a time, so the others can arrive while
     we copy the data back in... */

#ifdef DEBUG_PUGH
  printf ("FinishReceiveGA: into direction %d from proc %d for %d vars "
          "starting with '%s'\n",
          dir, GA->connectivity->neighbours[pughGH->myproc][dir],
          comm->n_vars, GA->name);
#endif

  istart   = GA->extras->ghosts[GA->stagger][0][dir];
  iend     = GA->extras->ghosts[GA->stagger][1][dir];
  iterator = GA->extras->iterator;

  /* set iterator to the start vector */
  for(dim = 0; dim < GA->extras->dim; dim++)
  {
    iterator[dim] = istart[dim];
  }

  /* get the source and the number of bytes to copy in the lowest dimension */
  copy_from  = comm->recv_buffer[dir];
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
      copy_to = ((pGA *) pughGH->variables[i][comm->sync_timelevel])->data;
      memcpy (copy_to + offset, copy_from, copy_bytes);
      copy_from += copy_bytes;
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

}
#endif /* CCTK_MPI */
