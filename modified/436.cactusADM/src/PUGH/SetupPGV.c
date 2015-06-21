#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      SetupPGV.c
   @date      Wed Oct 27 11:09:05 1999
   @author    Tom Goodale
   @desc 
   Sets up a PUGH grid variable.
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGH/src/SetupPGV.c,v 1.41 2001/11/02 16:18:03 goodale Exp $
 @@*/

/*#define DEBUG_PUGH*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "cctk.h"
#include "pugh.h"
#include "pughi.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGH/src/SetupPGV.c,v 1.41 2001/11/02 16:18:03 goodale Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_SetupPGV_c)


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static int PUGH_IntegerRoot(int number, int invpower);

static int IntSort(const void *a, const void *b);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    PUGH_SetupPGExtras
   @date       Tue Nov  9 09:23:27 1999
   @author     Tom Goodale
   @desc 
   Sets up a PGExtras structure.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
pGExtras *PUGH_SetupPGExtras(int dim, 
                             int *perme,
                             int stagger,
                             int *sh,
                             int *nghosts,
                             int total_procs,
                             int *nprocs,
                             int this_proc)
{
  int error;
  pGExtras *this;

  this = (pGExtras *)malloc(sizeof(pGExtras));

  /* Setup memory */
  if(this)
  {
    error = PUGH_SetupPGExtrasMemory(dim, 
                                     total_procs,
                                     nprocs,
                                     this);

    if(!error)
    {

      this->dim = dim;

      PUGH_SetupPGExtrasSizes(dim, perme, stagger, sh, nghosts, 
                              total_procs, nprocs, this_proc,this);
      PUGH_SetupPGExtrasOwnership(dim, perme, stagger, sh, nghosts, 
                                  total_procs, nprocs, this_proc, this);
      PUGH_SetupPGExtrasStaggering(dim, perme, stagger, sh, nghosts, 
                                   total_procs, nprocs, this_proc, this);
    }      

  }

  return this;
}

 /*@@
   @routine    PUGH_DestroyPGExtras
   @date       Mar 12 2000
   @author     Thomas Radke
   @desc 
   Destroys a PGExtras structure.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
void PUGH_DestroyPGExtras(pGExtras **PGExtras)
{
  if(PGExtras && *PGExtras)
  {
    int i, j;

    for(i = 0 ; i < PUGH_NSTAGGER; i++)
    {
      for(j = 0; j < 2; j++)
      {
        free((*PGExtras)->ghosts[i][j][0]);
        free((*PGExtras)->overlap[i][j][0]);
        free((*PGExtras)->ownership[i][j]);
        free((*PGExtras)->ghosts[i][j]);
        free((*PGExtras)->overlap[i][j]);
      }
    }
    free((*PGExtras)->lb[0]);
    free((*PGExtras)->ub[0]);
    free((*PGExtras)->rnsize[0]);

    free((*PGExtras)->lb);
    free((*PGExtras)->ub);
    free((*PGExtras)->rnsize);
    free((*PGExtras)->rnpoints);
    free((*PGExtras)->nghostzones);
    free((*PGExtras)->nsize);
    free((*PGExtras)->lnsize);
    free((*PGExtras)->bbox);
    free((*PGExtras)->iterator);
    free((*PGExtras)->hyper_volume);

    free(*PGExtras);
    *PGExtras = NULL;
  }
}

 /*@@
   @routine    PUGH_SetupConnectivity
   @date       Fri Nov  5 11:32:12 1999
   @author     Tom Goodale
   @desc 
   Create a connectivity structure containing 
   all the details of processor connectivities
   for this processor layout.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
pConnectivity *PUGH_SetupConnectivity(int dim, 
                                      int total_procs, 
                                      int *nprocs,
                                      int *perme)
{
  pConnectivity *this;

  int i;

  /* Allocate memory */
  this = (pConnectivity *)malloc(sizeof(pConnectivity));

  if(this)
  {
    this->nprocs     = (int *)malloc(dim*sizeof(int));
    this->perme      = (int *)malloc(dim*sizeof(int));
    this->neighbours = (int **)malloc(total_procs*sizeof(int *));

    if(this->neighbours)
    {
      this->neighbours[0] = (int *)malloc(total_procs*2*dim*sizeof(int));
    }
    else
    {
      free(this->neighbours);
      this->neighbours = NULL;
    }


    if(!this->nprocs     || 
       !this->perme      ||
       !this->neighbours)
    {
      if(this->neighbours)
      {
        free(this->neighbours[0]);
      }

      free(this->neighbours);
      free(this->perme);
      free(this->nprocs);

      free(this);
      this = NULL;
    }
  }

  /* Fill in data */
  if(this)
  {
    this->dim = dim;

    for(i = 0 ; i < dim; i++)
    {
      this->nprocs[i] = nprocs[i];
      this->perme[i] = perme[i];
    }

    for(i = 1; i < total_procs; i++)
    {
      this->neighbours[i] = this->neighbours[0]+(2*dim*i);
    }

    PUGH_GenerateTopology(dim, total_procs, this->nprocs);

    PUGH_GenerateNeighbours(dim, total_procs, this->nprocs, this->neighbours, this->perme);

  }

  return this;
}

 /*@@
   @routine    PUGH_DestroyConnectivity
   @date       Mar 12 2000
   @author     Thomas Radke
   @desc 
   Destroys a connectivity structure containing 
   all the details of processor connectivities
   for this processor layout.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
void PUGH_DestroyConnectivity(pConnectivity **conn)
{
  if (conn && *conn)
  {
    free((*conn)->perme);
    free((*conn)->neighbours[0]);
    free((*conn)->neighbours);
    free((*conn)->nprocs);
    free(*conn);
    *conn = NULL;
  }
}

 /*@@
   @routine    PUGH_GenerateTopology
   @date       Fri Nov  5 11:31:21 1999
   @author     Tom Goodale
   @desc 
   Generate the appropriate processor topology for this processor
   decomposition.
   @enddesc 
   @calls     
   @calledby   
   @history 
   @hdate Tue Jan 30 17:04:50 2001 @hauthor Tom Goodale
   @hdesc Added call to integer root function and qsort to
          avoid problems with real to integer conversions and
          demonstrable failure of the algorithm when dealing
          with large prime factors. 
   @endhistory 

@@*/
int PUGH_GenerateTopology(int dim, int total_procs, int *nprocs)
{
  int i;
  int used_procs;
  int free_procs;
  int retval;
  int free_dims;

  used_procs = 0;
  free_procs = total_procs;

  retval = 0;

  free_dims = dim;

  for(i=0; i < dim; i++)
  {
    if((nprocs[i])>0)
    {
      free_dims--;
      if(used_procs) 
      {
        used_procs *= nprocs[i];
      }
      else
      {
        used_procs = nprocs[i];
      }
      if (total_procs%used_procs)
      {
        CCTK_WARN(0, "Inconsistent PUGH topology");
        retval = 1;
      }
      else
      {
        free_procs = total_procs/used_procs;
      }
    }
  }

  /* Ok calculate topology if necessary */
  if(free_dims && ! retval)
  {
    /* This algorithm gives the most number of processors
     * in the highest dimension.
     */

    int *working;
    int root;
    int place;

    root = free_dims;
    working = (int *)calloc(free_dims,sizeof(int));
#ifdef DEBUG_PUGH
    printf("Processor topology for dimension %d\n",dim);
#endif


    for(i = 0; i < free_dims  ; i++)
    {
      working[i] = PUGH_IntegerRoot(free_procs, root);
      
      while(free_procs % working[i]) working[i]--;

#ifdef DEBUG_PUGH
      printf(" working[%d] = %d\n",i,working[i]);
#endif
      free_procs /= working[i];
      root--;
    }

    
    /* The above doesn't necessarily sort them properly 
     * e.g. if one of the factors is a prime then the
     * above will sort the 1 before the prime.
     */
    qsort(working,free_dims,sizeof(int),IntSort);

    for(i = 0,place=0; i < dim ; i++)
    {
      if(nprocs[i] <= 0)
      {
        nprocs[i] = working[place];
        place++;
      }

#ifdef DEBUG_PUGH
      printf(" nprocs[%d] = %d\n",i,nprocs[i]);
#endif
    }

    free(working);
  }

  return retval;
}

 /*@@
   @routine    PUGH_GenerateNeighbours
   @date       Mon Nov  8 08:15:08 1999
   @author     Tom Goodale
   @desc 
   Works out the array of neighbouring processors for
   every processor.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_GenerateNeighbours(int dim, 
                            int total_procs, 
                            int *nprocs, 
                            int **neighbours, 
                            int *perme)
{
  int retval;
  int i;

  int idim;
  int *pos;
  int temp;
  
  pos = (int *)malloc(dim*sizeof(int));

  if(pos)
  {
    for(i = 0; i < total_procs; i++)
    {
      PUGH_DecomposeIJK(dim,i,nprocs, pos);
      
      for(idim = 0; idim < dim ; idim++)
      {
        /* Deal with minus neighbour in this direction */
        pos[idim]--;
        
        if(pos[idim] > -1)
        {
          neighbours[i][idim*2] = PUGH_ComposeIJK(dim, nprocs, pos);
        }
        else if(perme[idim])
        {
          temp = pos[idim];
          pos[idim] = nprocs[idim]-1;
          neighbours[i][idim*2] = PUGH_ComposeIJK(dim, nprocs, pos);
          pos[idim] = temp;
        }
        else
        {
          neighbours[i][idim*2] = -1;
        }

        pos[idim]++;
        
        /* Deal with plus neighbour in this direction */
        pos[idim]++;

        if(pos[idim] < nprocs[idim])
        {
          neighbours[i][idim*2+1] = PUGH_ComposeIJK(dim, nprocs, pos);
        }
        else if(perme[idim])
        {
          temp = pos[idim];
          pos[idim] = 0;
          neighbours[i][idim*2+1] = PUGH_ComposeIJK(dim, nprocs, pos);
          pos[idim] = temp;
        }
        else
        {
          neighbours[i][idim*2+1] = -1;
        }

        pos[idim]--;
      }
    }

    retval = 0;
  }
  else
  {
    retval = 1;
  }

  free(pos);

#ifdef DEBUG_PUGH
  /* Print neighbours */
  printf("Neighbours (dim: %d)\n",dim);
  for (i=0;i<total_procs;i++)
  {
    printf("Proc %d: ",i);
    for (idim=0;idim<2*dim;idim++)
    {
      printf(" %d",neighbours[i][idim]);
    }
    printf("\n");
  }
#endif

  return retval;
}
  


 /*@@
   @routine    PUGH_DecomposeIJK
   @date       Fri Nov  5 11:29:43 1999
   @author     Tom Goodale
   @desc 
   Decompose an ijk index into seperate components.
   Taken from libHLL.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_DecomposeIJK(int dim, int ijk, int *nprocs, int *pos)
{
  int idim;

  /* Locate position in coordinate space.
   *
   * e.g. ijk = i+nx*(j+ny*k)
   * => i = ijk % nx
   *    ijk/nx = j + ny*k
   */

  for(idim = 0; idim < dim; idim++)
  {
    pos[idim] = ijk % nprocs[idim];
    ijk /= nprocs[idim];
  }

  return 0;
}


 /*@@
   @routine    PUGH_ComposeIJK
   @date       Fri Nov  5 11:29:43 1999
   @author     Tom Goodale
   @desc 
   Compose an ijk index from seperate components.
   Taken from libHLL.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_ComposeIJK(int dim, 
                    int *nprocs, 
                    int *pos)
{
  int ijk;
  int idim;

  /* Construct the position in the linear space. 
   *
   * e.g. i+nx*(j+ny*k)
   */

  ijk = pos[dim-1];
  for(idim = dim-2; idim >=0; idim--)
  {
    ijk = pos[idim] + nprocs[idim]*ijk;
  };

  return ijk;
}


 /*@@
   @routine    PUGH_SetupPGExtrasMemory
   @date       Mon Nov  8 08:16:02 1999
   @author     Tom Goodale
   @desc 
   Allocate memory for the members of the pGExtras structure.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_SetupPGExtrasMemory(int dim, 
                             int total_procs,
                             int *nprocs,
                             pGExtras *this)
{
  int retcode;
  int i,j,k;

  retcode = 0;

  if(this)
  {
    /* Do it in stages.
     * First: things depending on the number of processors 
     */
    this->lb       = (int **)malloc(total_procs*sizeof(int *));
    this->ub       = (int **)malloc(total_procs*sizeof(int *));
    this->rnsize   = (int **)malloc(total_procs*sizeof(int *));
    this->rnpoints = (int *) malloc(total_procs*sizeof(int));

    /* Things just depending on dimension */
    this->nghostzones  = (int *)malloc(dim*sizeof(int));
    this->nsize        = (int *)malloc(dim*sizeof(int));
    this->lnsize       = (int *)malloc(dim*sizeof(int));
    this->bbox         = (int *)malloc(2*dim*sizeof(int));
    this->iterator     = (int *)malloc(dim*sizeof(int));
    this->hyper_volume = (int *)malloc(dim*sizeof(int));

    /* Check all the above succeeded and then get memory for 
     * arrays hanging off the above. 
     */
    if(this->lb          &&
       this->ub          &&
       this->rnsize      &&
       this->rnpoints    &&
       this->nghostzones &&
       this->nsize       &&
       this->lnsize      &&
       this->bbox        &&
       this->iterator    &&
       this->hyper_volume)
    {
      this->lb[0]     = (int *)malloc(total_procs  *dim*sizeof(int));
      this->ub[0]     = (int *)malloc(total_procs  *dim*sizeof(int));
      this->rnsize[0] = (int *)malloc(total_procs*2*dim*sizeof(int));

      if(this->lb[0]     &&
         this->ub[0]     &&
         this->rnsize[0])
      {
        for (i = 1; i < total_procs; i++)
        {
          this->lb[i]     = this->lb[0]     + i*dim;
          this->ub[i]     = this->ub[0]     + i*dim;
          this->rnsize[i] = this->rnsize[0] + i*2*dim;
        }
      }
      else
      {
        /* Free inner arrays */
        free(this->lb[0]);
        this->lb[0] = NULL;
        free(this->ub[0]);
        this->ub[0] = NULL;
        free(this->rnsize[0]);
        this->rnsize[0] = NULL;

        /* Free toplevel arrays */
        free(this->lb);
        this->lb = NULL;
        free(this->ub);
        this->ub = NULL;
        free(this->rnsize);
        this->rnsize = NULL;
        free(this->rnpoints);
        this->rnpoints = NULL;
        free(this->nghostzones);
        this->nghostzones = NULL;               
        free(this->nsize);
        this->nsize = NULL;               
        free(this->lnsize);
        this->lnsize = NULL;               
        free(this->bbox);
        this->bbox = NULL;               
        free(this->iterator);
        this->iterator = NULL;               
        free(this->hyper_volume);
        this->hyper_volume = NULL;               
      }
    }
    else
    {
      /* Free toplevel arrays */
      free(this->lb);
      this->lb = NULL;
      free(this->ub);
      this->ub = NULL;
      free(this->rnsize);
      this->rnsize = NULL;
      free(this->rnpoints);
      this->rnpoints = NULL;
      free(this->nghostzones);
      this->nghostzones = NULL;               
      free(this->nsize);
      this->nsize = NULL;               
      free(this->lnsize);
      this->lnsize = NULL;               
      free(this->bbox);
      this->bbox = NULL;               
      free(this->iterator);
      this->iterator = NULL;               
      free(this->hyper_volume);
      this->hyper_volume = NULL;               
    }
      

    if(this->lb          &&
       this->ub          &&
       this->rnsize      &&
       this->rnpoints    &&
       this->nghostzones &&
       this->nsize       &&
       this->lnsize      &&
       this->bbox        &&
       this->iterator    &&
       this->hyper_volume)
    {
      retcode = 0;
      for (i = 0 ; i < PUGH_NSTAGGER; i++)
      {
        for (j = 0; j < 2; j++)
        {
          this->ownership[i][j]    = (int *) malloc(dim*sizeof(int));
          this->ghosts[i][j]       = (int **)malloc(2*dim*sizeof(int *));
          this->overlap[i][j]      = (int **)malloc(2*dim*sizeof(int *));
          if(this->ghosts[i][j] &&
             this->overlap[i][j])
          {
            this->ghosts[i][j][0]  = (int *)malloc(2*dim*dim*sizeof(int));
            this->overlap[i][j][0] = (int *)malloc(2*dim*dim*sizeof(int));
            for (k=1; k < 2*dim; k++)
            {
              this->ghosts[i][j][k]  = this->ghosts[i][j][0] + k*dim;
              this->overlap[i][j][k] = this->overlap[i][j][0] + k*dim;
            }
          }
          else
          {
            free(this->ownership[i][j]);
            this->ownership[i][j] = NULL;

            free(this->ghosts[i][j]);
            this->ghosts[i][j] = NULL;

            free(this->overlap[i][j]);
            this->overlap[i][j] = NULL;
            retcode = 1;
            break;
          }          
        }
        if(retcode)
        {
          for(j=1; j >=0 ; j--)
          {
              free(this->ownership[i][j]);
              this->ownership[i][j] = NULL;
              
              free(this->ghosts[i][j]);
              this->ghosts[i][j] = NULL;
              
              free(this->overlap[i][j]);
              this->overlap[i][j] = NULL;
              
          }
          break;
        }
      }
      if(retcode)
      {
        /* Loop back through the arrays freeing things */
        for(i--; i >=0; i--)
        {
          for(j=1; j >=0 ; j--)
          {
            free(this->ghosts[i][j][0]);
            free(this->overlap[i][j][0]);

            free(this->ownership[i][j]);
            this->ownership[i][j] = NULL;
            
            free(this->ghosts[i][j]);
            this->ghosts[i][j] = NULL;
          
            free(this->overlap[i][j]);
            this->overlap[i][j] = NULL;
          }

          free(this->ownership[i]);
          free(this->ghosts[i]);
          free(this->overlap[i]);
        }

        /* Free the stuff originally allocated */

        free(this->lb[0]);
        this->lb[0] = NULL;
        free(this->ub[0]);
        this->ub[0] = NULL;
        free(this->rnsize[0]);
        this->rnsize[0] = NULL;

        free(this->lb);
        this->lb = NULL;
        free(this->ub);
        this->ub = NULL;
        free(this->rnsize);
        this->rnsize = NULL;
        free(this->rnpoints);
        this->rnpoints = NULL;
        free(this->nghostzones);
        this->nghostzones = NULL;               
        free(this->nsize);
        this->nsize = NULL;               
        free(this->lnsize);
        this->lnsize = NULL;               
        free(this->bbox);
        this->bbox = NULL;               
        free(this->iterator);
        this->iterator = NULL;               
        free(this->hyper_volume);
        this->hyper_volume = NULL;               
      }
    }          
  }
  else
  {
    retcode = -1;
  }

  return retcode;
}

 /*@@
   @routine    PUGH_SetupPGExtrasSizes
   @date       Mon Nov  8 08:59:33 1999
   @author     Tom Goodale
   @desc 
   Sets up the size information in the pGExtras 
   structure.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_SetupPGExtrasSizes(int dim, 
                            int *perme,
                            int stagger,
                            int *sh,
                            int *nghosts,
                            int total_procs,
                            int *nprocs,
                            int this_proc,
                            pGExtras *this)
{
  int dir;
  int maxpoints, minpoints, avgpoints,proc;

  /* Setup the global shape */
  for (dir=0 ; dir < dim ; dir++)
  {
    /* A -ve size means constant load per proc */
    if (sh[dir] < 0 && nprocs[dir] > 1) 
    {
      this->nsize[dir] = (nprocs[dir]-2) * 
        (-sh[dir] - 2*nghosts[dir]) +
        2 * (-sh[dir] - nghosts[dir]);
      
      if (stagger == PUGH_STAGGER) 
      {
        this->nsize[dir] -= nprocs[dir]-1;
      }
    } 
    else 
    {
      this->nsize[dir] = abs(sh[dir]);
    }
  }

  /* Set the number of ghost zones. */
  for (dir = 0; dir < dim; dir++)
  {
    this->nghostzones[dir] = nghosts[dir];
  }

  /* Setup the bounding box stuff */

  PUGH_SetupBoundingBox(dim, 
                        perme, 
                        stagger, 
                        sh, 
                        nghosts, 
                        total_procs, 
                        nprocs, 
                        this); 

  /* Set the remote sizes */

  PUGH_SetupRemoteSizes(dim, 
                        perme, 
                        stagger, 
                        sh, 
                        nghosts, 
                        total_procs, 
                        nprocs, 
                        this); 
  
  /* Set the local sizes */

  for (dir = 0; dir < dim; dir++)
  {
    this->lnsize[dir] = this->rnsize[this_proc][dir];
  }

  this->npoints = this->rnpoints[this_proc];

  /* Set up the maxskew */
  
  maxpoints=this->npoints;
  minpoints=this->npoints;
  avgpoints=0;
  for (proc = 0; proc < total_procs; proc++)
  {
    maxpoints = (maxpoints<this->rnpoints[proc]) ? this->rnpoints[proc] : maxpoints;
    minpoints = (minpoints>this->rnpoints[proc]) ? this->rnpoints[proc] : minpoints;
    avgpoints += this->rnpoints[proc];
  }
  avgpoints = avgpoints/total_procs;

  this->maxskew = avgpoints > 0 ? 100*(maxpoints-minpoints)/avgpoints : 0;

  return 0;
}
     

 /*@@
   @routine    PUGH_SetupPGExtrasOwnership
   @date       Mon Nov  8 09:00:10 1999
   @author     Tom Goodale
   @desc 
   Sets up ownership, overlap, ghostzones, etc 
   in a pGExtras structure.

   Mostly taken from original SetupOwnership by Paul.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_SetupPGExtrasOwnership(int dim, 
                                int *perme,
                                int stagger,
                                int *sh,
                                int *nghosts,
                                int total_procs,
                                int *nprocs,
                                int this_proc,
                                pGExtras *this)
{
  int tmp;
  int dir, idir;
  int istart, iend;

  /* Ownership is pretty easy. Remember ownership is indexed as
     [stagger][ijk][min/max]. See pGF_Reduction for a use of this,
     among others.
     Note: Ownership is same for staggered and non-staggered grids
   */
  for (dir = 0 ; dir < dim; dir++)
  {
    this->ownership[PUGH_VERTEXCTR][0][dir] = 
      (this->lb[this_proc][dir] == 0 ? 0 : this->nghostzones[dir]);
    this->ownership[PUGH_VERTEXCTR][1][dir] =
      (this->ub[this_proc][dir] == this->nsize[dir]-1 ? 
       this->lnsize[dir] : this->lnsize[dir] - this->nghostzones[dir]);
    this->bbox[2*dir] = this->lb[this_proc][dir] == 0;
    this->bbox[2*dir+1] = this->ub[this_proc][dir] == 
                          this->nsize[dir]-1;


  }

  /* correct for periodic identification : Tue Jun 17 08:40:15 CDT 1997 */
  for (dir = 0; dir < dim; dir++)
  {
    if(perme[dir])
    {
        this->ownership[PUGH_VERTEXCTR][0][dir] = 
          this->nghostzones[dir];
        this->ownership[PUGH_VERTEXCTR][1][dir] = 
          this->lnsize[dir] - this->nghostzones[dir];
    }
  } 

  for (dir = 0; dir < dim; dir++)
  {
    if (this->nsize[dir] == 1) 
    {
      this->ownership[PUGH_VERTEXCTR][0][dir] = 0;
      this->ownership[PUGH_VERTEXCTR][1][dir] = 1;
    }
  }


  /* Great now set up the overlap region. This is the region we own,
     but which is covered by our neighbors ghost. So this is what we
     want to SEND
   */
  for (dir = 0; dir < 2*dim; dir++) 
  {
    for (idir = 0; idir < dim; idir++)
    {
      istart = 0; 
      iend   = this->lnsize[idir];

      /* We want to send at the dir - 1 */

      /* Correct to direction specific sweeps ... */
      if (dir == 2*idir) 
      {
        /* Want to go from sw to sw + sw (eg, 1 to < 2, 2 to < 4) */
        istart = this->nghostzones[idir];
        iend   = istart + this->nghostzones[idir];
      }
      if (dir == 2*idir+1) {
        /* Want to go from nx - 2 sw to nx - sw
           (eg nx-2 to < nx-1, nx-4 to < nx-2)
           Luckily iend is aready set to nx ... */
        tmp = iend;
        istart = tmp - 2 * this->nghostzones[idir];
        iend   = tmp -     this->nghostzones[idir];
      }

      this->overlap[PUGH_VERTEXCTR][0][dir][idir] = istart;
      this->overlap[PUGH_VERTEXCTR][1][dir][idir] = iend;
    }
  }

  /* And finally the ghost indices. This used to be in
     pGF_FinishRecv.c, but now it is here.
   */
  for (dir = 0; dir < 2*dim; dir++) 
  {
    /* We want to send at the dir - 1 */
    for (idir = 0; idir < dim; idir++)
    {
      istart = 0; 
      iend = this->lnsize[idir];

      /* Correct to direction specific sweeps ... */
      /* Remember this is SW less than the other way... */
      if (dir == 2*idir) 
      {
      /* Want to go from sw to sw + sw (eg, 1 to < 2, 2 to < 4) */
        istart = 0;
        iend   = this->nghostzones[idir];
      }
      if (dir == 2*idir+1) 
      {
      /* Want to go from nx - 2 sw to nx - sw
         (eg nx-2 to < nx-1, nx-4 to < nx-2)
         Luckily iend is aready set to nx ... */
        tmp = iend;
        istart = tmp - this->nghostzones[idir];
        iend   = tmp;
      }

      this->ghosts[PUGH_VERTEXCTR][0][dir][idir] = istart;
      this->ghosts[PUGH_VERTEXCTR][1][dir][idir] = iend;

    }

  }

  return 0;
}

int PUGH_SetupPGExtrasStaggering(int dim, 
                                 int *perme, 
                                 int stagger, 
                                 int *sh, 
                                 int *nghosts, 
                                 int total_procs, 
                                 int *nprocs, 
                                 int this_proc, 
                                 pGExtras *this)
{
  int s,d,k,dir;
  int *upperbnd;

  upperbnd = (int*) malloc(dim*sizeof(int));  

  for (d=0;d<dim;d++)
  {
    upperbnd[d] = (this->ub[this_proc][d] == this->nsize[d]-1) ? 1 : 0;
  }
 
  /* copy ownership/ghost/overlap from the default PUGH_VERTEXCTR */
  for(s=1;s<PUGH_NSTAGGER;s++)
  {
    for(d=0;d<dim;d++)
    {
      for(k=0;k<2;k++)
      {
        this->ownership[s][k][d]    = this->ownership[0][k][d];
        for (dir=0;dir<2*dim;dir++)
        {
          this->overlap[s][k][dir][d] = this->overlap[0][k][dir][d];
          this->ghosts [s][k][dir][d] = this->ghosts [0][k][dir][d]; 
        }
      }
      
      /* decrease ownership at upper bound */
      if(upperbnd[d]) 
      {
        this->ownership[s][1][d] --;
      }
    }

    /* correct overlaps */
    for(dir=0;dir<2*dim;dir++)
    {
      for (d=0;d<dim;d++)
      {
        /* if we are at a upper physical bnd && at a upperface (1,3,5) 
           && we are in the dir direction decrease by one */
        if((upperbnd[d])&&((dir+1)%2==0)&&(dir==2*d+1))
        {
          this->overlap[s][0][dir][d]--;
          this->overlap[s][1][dir][d]--;
        }
      }
    }
  }

  free(upperbnd);

  return(0);
}

 /*@@
   @routine    PUGH_SetupBoundingBox
   @date       Mon Nov  8 09:03:40 1999
   @author     Tom Goodale
   @desc 
   Sets up the bounding box info for a pgExtras structure.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_SetupBoundingBox(int dim, 
                          int *perme,
                          int stagger,
                          int *sh,
                          int *nghosts,
                          int total_procs,
                          int *nprocs,
                          pGExtras *this)
{
  int pnum,dir;

  int **bounds;
  int *pos;

  bounds = (int **)malloc(dim*sizeof(int*));
  pos  = (int *)malloc(dim*sizeof(int));

  if(bounds && pos)
  {
    /* Work out the bounds in each direction - either from parameters 
       file or default*/
    PUGH_GetBounds(dim, bounds, nprocs, this->nsize);

    /*    for (dir = 0 ; dir < dim; dir++)
    {
      step[dir] = (this->nsize[dir]-1) / nprocs[dir];
    }
    */

    for(pnum = 0; pnum < total_procs; pnum++)
    {

      PUGH_DecomposeIJK(dim, pnum, nprocs, pos);
      
      for(dir = 0 ; dir < dim; dir++)
      {
        if (pos[dir] == 0)
        {
          this->lb[pnum][dir] = 0;
        }
        else
        {
          this->lb[pnum][dir] = bounds[dir][pos[dir]] +1 - nghosts[dir];
          if(stagger == PUGH_STAGGER)
          {
            this->lb[pnum][dir] --;
          }
        }

        if (pos[dir] == nprocs[dir]-1) 
        {
          this->ub[pnum][dir] = this->nsize[dir]-1;
        }
        else
        {
          this->ub[pnum][dir] = bounds[dir][pos[dir]+1] + this->nghostzones[dir];
        }
      }
    }

  }

  for (dir=0; dir<dim;dir++)
  {
    free(bounds[dir]);
  }
  free(bounds);
  free(pos);

#ifdef DEBUG_PUGH
  {
    int i;
    for(i=0; i<total_procs; i++) {
      printf(" setup_Bounding_Box (%d):", i);
      for (dir = 0 ; dir < dim; dir++)
        printf(" (%d,%d)",
               this->lb[i][dir], this->ub[i][dir]);               
      printf(" \n");
    }
  }
#endif

  return 0;
}


 /*@@
   @routine    PUGH_SetupRemoteSizes
   @date       Mon Nov  8 09:07:27 1999
   @author     Tom Goodale
   @desc 
   Determines info about the sizes on each processor.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int PUGH_SetupRemoteSizes(int dim, 
                          int *perme,
                          int stagger,
                          int *sh,
                          int *nghosts,
                          int total_procs,
                          int *nprocs,
                          pGExtras *this)
{
  int pnum;
  int dir;

  /* Save the number of points on each processor */

  for(pnum = 0; pnum < total_procs; pnum++)
  {

    this->rnpoints[pnum] = 1;
    for (dir=0;dir<dim;dir++)
    {
      this->rnsize[pnum][dir] = (this->ub[pnum][dir]-this->lb[pnum][dir]+1);
      this->rnpoints[pnum]  *=  this->rnsize[pnum][dir];
    }
  }

  return 0;
}


 /*@@
   @routine    PUGH_SetupGArrayGroupComm
   @date       Tue 06 Jun 2000
   @author     Thomas Radke
   @desc 
   Sets up a communication buffer for a group of GAs.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
@@*/
pComm *PUGH_SetupGArrayGroupComm(pGH *pughGH,
                                 int dim,
                                 int first_var,
                                 int n_vars,
                                 int sync_timelevel,
                                 int vartype,
                                 pGExtras *extras)
{
  int i;
  pComm *this;


  this = (pComm *) malloc (sizeof (pComm));

  if (this)
  {
    this->buffer_sz   = (int *)   malloc (2 * dim * sizeof (int));
    this->send_buffer = (void **) malloc (2 * dim * sizeof (void *));
    this->recv_buffer = (void **) malloc (2 * dim * sizeof (void *));
#ifdef CCTK_MPI
    this->sreq    = (MPI_Request *) malloc (2 * dim * sizeof (MPI_Request));
    this->rreq    = (MPI_Request *) malloc (2 * dim * sizeof (MPI_Request));
    this->sstatus = (MPI_Status *)  malloc (2 * dim * sizeof (MPI_Status));
#endif
    this->docomm      = (int *)   malloc (2 * dim * sizeof (int));

    if(this->buffer_sz   &&
       this->send_buffer &&
       this->recv_buffer &&
#ifdef CCTK_MPI
       this->sreq        &&
       this->rreq        &&
       this->sstatus     &&
#endif
       this->docomm)
    {
      this->commflag       = PUGH_NOCOMM;
      this->first_var      = first_var;
      this->n_vars         = n_vars;
      this->sync_timelevel = sync_timelevel;

#ifdef CCTK_MPI
      this->mpi_type       = PUGH_MPIDataType (pughGH, vartype);
#endif

      for (i = 0; i < 2 * dim; i++)
      {
        this->buffer_sz[i]   = 0;
        this->send_buffer[i] = NULL;
        this->recv_buffer[i] = NULL;
#ifdef CCTK_MPI
        /* Null my send and receive requests */
        this->sreq[i]        = MPI_REQUEST_NULL;
        this->rreq[i]        = MPI_REQUEST_NULL;
#endif
        this->docomm[i]      = PUGH_NOCOMM;
      }
    }
    else
    {
      if (this->buffer_sz)
      { 
        free (this->buffer_sz);
      }
      if (this->send_buffer)
      {
        free (this->send_buffer);
      }
      if (this->recv_buffer)
      {
        free (this->recv_buffer);
      }
#ifdef CCTK_MPI
      if (this->sreq)
      {
        free (this->sreq);
      }
      if (this->rreq)
      {
        free (this->rreq);
      }
      if (this->sstatus)
      {
        free (this->sstatus);
      }
#endif
      if (this->docomm)
      {
        free (this->docomm);
      }

      free (this);
      this = NULL;
    }
  }

  /* Now set the hyper_volume vector in the extras structure
     which is used to copy the ghostzones from/to the comm buffers.
     NOTE: Sizes are counted in 'number of elements' here.
           hyper_volume[0] is unused.
   */
  if(this)
  {
    extras->hyper_volume[0] = 1;
    for (i = 1; i < extras->dim; i++)
    {
      extras->hyper_volume[i] = extras->hyper_volume[i-1] * extras->lnsize[i-1];
    }
  }

  return this;
}


 /*@@
   @routine    PUGH_SetupGArrayComm
   @date       Tue 06 Jun 2000
   @author     Thomas Radke
   @desc 
   Sets up a communication buffer for a GA.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
@@*/
pComm *PUGH_SetupGArrayComm(pGH *pughGH,
                            int dim,
                            int var,
                            int sync_timelevel,
                            int vartype,
                            pGExtras *extras)
{
  return PUGH_SetupGArrayGroupComm(pughGH,
                                   dim,
                                   var,
                                   1,
                                   sync_timelevel,
                                   vartype,
                                   extras);
}


 /*@@
   @routine    PUGH_DestroyComm
   @date       Tue 06 Jun 2000
   @author     Thomas Radke
   @desc 
   Destroys a communication buffer.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
@@*/
void PUGH_DestroyComm(pComm **comm)
{
  free ((*comm)->buffer_sz);
  free ((*comm)->send_buffer);
  free ((*comm)->recv_buffer);
#ifdef CCTK_MPI
  free ((*comm)->sreq);
  free ((*comm)->rreq);
  free ((*comm)->sstatus);
#endif
  free ((*comm)->docomm);
  free (*comm);

  *comm = NULL;
}


 /*@@
   @routine    PUGH_SetupGArray
   @date       Mon Nov  8 16:29:34 1999
   @author     Tom Goodale
   @desc 
   Sets up a new pGA.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
pGA *PUGH_SetupGArray(void *parent,
                      pGExtras *extras,
                      pConnectivity *connectivity,
                      pComm *groupcomm,
                      const char *name,
                      int id,
                      int arrayid,
                      int varsize, 
                      int vtype, 
                      int stagger,
                      int vector_size,
                      int vector_entry,
                      pGA *vector_base) 
{
  pGA *this;

  this = (pGA *)malloc(sizeof(pGA));

  if(this)
  {
    this->extras       = extras;
    this->connectivity = connectivity;
    this->comm         = NULL;
    this->groupcomm    = groupcomm;
    this->parent       = parent;
    this->varsize      = varsize;
    this->vtype        = vtype;
    this->stagger      = stagger;
    this->id           = id;
    this->arrayid      = arrayid;
    this->storage      = PUGH_NOSTORAGE;

    this->name        = (char *) malloc((strlen(name)+1)*sizeof(char));
    this->padddata    = (void *) calloc(1, varsize);
    this->data        = this->padddata;

    this->vector_size  = vector_size;
    this->vector_entry = vector_entry;
    this->vector_base  = vector_base;

    if(this->name && this->padddata)
    {
      strcpy(this->name, name);
    }
    else
    {
      if(this->name)
      {
        free(this->name);
      }
      if(this->padddata)
      {
        free(this->padddata);
      }
      free(this);
      this = NULL;
    }
  }

  return this;
}

 /*@@
   @routine    PUGH_DestroyGArray
   @date       Mar 12 2000
   @author     Thomas Radke
   @desc 
   Destroys a pGA object.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
void PUGH_DestroyGArray(pGA **GA)
{
  if(GA && *GA)
  {
    /* free storage if still allocated */
    if((*GA)->storage != PUGH_NOSTORAGE)
    {
      PUGH_DisableGArrayDataStorage(*GA);
    }

    /* free comm buffer if created before */
    if((*GA)->comm)
    {
      if ((*GA)->comm->commflag != PUGH_NOCOMM)
      {
        PUGH_DisableGArrayComm(*GA);
      }
      PUGH_DestroyComm(&(*GA)->comm);
    }

    /* free allocated resources */
    free((*GA)->name);
    free((*GA)->padddata);
    free(*GA);
    *GA = NULL;
  }
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    PUGH_IntegerRoot
   @date       Tue Jan 30 17:06:21 2001
   @author     Tom Goodale
   @desc 
   Generate the highest integer below a given integer root of an integer.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     number
   @vdesc   The number to take the root of
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     invpower
   @vdesc   The root to take
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   The highest integer below the desired root.
   @endreturndesc
@@*/
static int PUGH_IntegerRoot(int number, int invpower)
{
  int i;
  int tmp;
  int root;

  for(root = 1; root <= number; root++)
  {
    for(i=1, tmp=root; i < invpower; i++, tmp*=root);

    if(tmp > number)
    {
      root--;
      break;
    }
  }
  
  return root;
}


 /*@@
   @routine    IntSort
   @date       Tue Jan 30 17:08:47 2001
   @author     Tom Goodale
   @desc
               Sorts two integers for the qsort routine.
   @enddesc

   @var        a
   @vdesc      Pointer to first integer to compare
   @vtype      const void *
   @vio        in
   @endvar
   @var        b
   @vdesc      Pointer to second integer to compare
   @vtype      const void *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               -ve if b is greater than a.<BR>
               +ve if a is greater than b.<BR>
               0   if a is equal to b.
   @endreturndesc
@@*/
static int IntSort (const void *a, const void *b)
{
  return (*(const int *) a - *(const int *) b);
}
