#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file      LoadAware.c
   @date      Fri Feb 21 10:21:36 2000                 
   @author    Matei Ripeanu
   @desc 
   
   @enddesc
   @version $Header: /cactus/CactusPUGH/PUGH/src/LoadAware.c,v 1.10 2002/01/07 13:51:27 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "pugh.h"
#include "cctk.h"
#include "cctk_Parameters.h"

static const char *rcsid="$Header: /cactus/CactusPUGH/PUGH/src/LoadAware.c,v 1.10 2002/01/07 13:51:27 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGH_LoadAware_c)


/* #define DEBUG_LA 1 */


/* prototypes of routines defined in this source file */
int PUGH_SetPartitionInfo (int dim, const char *partition_info[]);
static int PUGH_GetSliceSizes (int np,
                               int grid_points,
                               const char *slicesS,
                               int **slices,
                               int manual);

/* static pointers to strings which contain partition information if set */
static char *partition_1D[1] = {NULL};
static char *partition_2D[2] = {NULL, NULL};
static char *partition_3D[3] = {NULL, NULL, NULL}; 


 /*@@
   @routine    PUGH_SetPartitionInfo
   @date       Tue 13 Feb 2001
   @author     Thomas Radke
   @desc
               Set/reset the static partition info string variables.

               Note that using such static variables here instead of the
               partition_[123]d_[xyz] parameters is just a hack because
               we don't want to make those steerable (this could make
               people think they can change the processor topology at
               runtime).
               But eg. BAM wants to set up a new topology when creating
               new grids.
   @enddesc

   @var        dim
   @vdesc      number of dimensions of processor topology
   @vtype      int
   @vio        in
   @endvar
   @var        partition_info
   @vdesc      list of strings with partition information
   @vtype      const char *[dim]
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0: OK                             
               -1: invalid dimension
   @endreturndesc
@@*/
int PUGH_SetPartitionInfo (int dim, const char *partition_info[])
{
  int retval;


  retval = 0;
  if (dim == 1)
  {
    if (partition_1D[0])
    {
      free (partition_1D[0]);
    }
    partition_1D[0] = strdup (partition_info[0]);
  }
  else if (dim == 2)
  {
    if (partition_2D[0])
    {
      free (partition_2D[0]);
    }
    if (partition_2D[1])
    {
      free (partition_2D[1]);
    }
    partition_2D[0] = strdup (partition_info[0]);
    partition_2D[1] = strdup (partition_info[1]);
  }
  else if (dim == 3)
  {
    if (partition_3D[0])
    {
      free (partition_3D[0]);
    }
    if (partition_3D[1])
    {
      free (partition_3D[1]);
    }
    if (partition_3D[2])
    {
      free (partition_3D[2]);
    }
    partition_3D[0] = strdup (partition_info[0]);
    partition_3D[1] = strdup (partition_info[1]);
    partition_3D[2] = strdup (partition_info[2]);
  }
  else
  {
    CCTK_WARN (1, "Only 1D, 2D, and 3D supported");
    retval = -1;
  }

  return (retval);
}


 /*@@
   @routine    PUGH_GetBounds
   @date       Fri Feb 21 10:21:36 2000
   @author     Matei Ripeanu
   @desc
   
   @enddesc
   @calls      PUGH_GetSliceSizes

   @var        dim
   @vdesc      dimension to set up bounds for
   @vtype      int
   @vio        in
   @endvar
   @var        bounds
   @vdesc      bounds to be allocated and set up
   @vtype      *int [dim-1]
   @vio        out
   @endvar
   @var        dim
   @vdesc      processors in every direction
   @vtype      int [dim-1]
   @vio        in
   @endvar
   @var        nsize
   @vdesc      points in every direction
   @vtype      int [dim-1]
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0: OK                             
               -1: invalid dimension
   @endreturndesc
@@*/
int PUGH_GetBounds (int dim, int *bounds[], int nprocs[], int nsize[])
{
  DECLARE_CCTK_PARAMETERS
  int i, retval, manual, external_manual;
  const char *partition_info;


  retval = 0;
  manual = CCTK_Equals (partition, "manual");

  if (dim == 1)
  {
    external_manual = partition_1D[0] != NULL;
    partition_info = external_manual ? (const char *) partition_1D[0] :
                                       partition_1d_x;
    PUGH_GetSliceSizes (nprocs[0], nsize[0], partition_info, &bounds[0],
                        manual || external_manual);
  }
  else if (dim == 2)
  {
    external_manual = partition_2D[0] != NULL;
    partition_info = external_manual ? (const char *) partition_2D[0] :
                                       partition_2d_x;
    PUGH_GetSliceSizes (nprocs[0], nsize[0], partition_info, &bounds[0],
                        manual || external_manual);
    external_manual = partition_2D[1] != NULL;
    partition_info = external_manual ? (const char *) partition_2D[1] :
                                       partition_2d_y;
    PUGH_GetSliceSizes (nprocs[1], nsize[1], partition_info, &bounds[1],
                        manual || external_manual);
  }
  else if (dim == 3)
  {
    external_manual = partition_3D[0] != NULL;
    partition_info = external_manual ? (const char *) partition_3D[0] :
                                       partition_3d_x;
    PUGH_GetSliceSizes (nprocs[0], nsize[0], partition_info, &bounds[0],
                        manual || external_manual);
    external_manual = partition_3D[1] != NULL;
    partition_info = external_manual ? (const char *) partition_3D[1] :
                                       partition_3d_y;
    PUGH_GetSliceSizes (nprocs[1], nsize[1], partition_info, &bounds[1],
                        manual || external_manual);
    external_manual = partition_3D[2] != NULL;
    partition_info = external_manual ? (const char *) partition_3D[2] :
                                       partition_3d_z;
    PUGH_GetSliceSizes (nprocs[2], nsize[2], partition_info, &bounds[2],
                        manual || external_manual);
  }
  else
  {
    if (manual)
    {
      CCTK_WARN (1, "Only 1D, 2D, and 3D supported in manual topology setup");
      retval = -1;
    }
    else
    {
      for (i = 0; i < dim; i++)
      {
        PUGH_GetSliceSizes (nprocs[i], nsize[i], NULL, &bounds[i], 0);
      }
    }
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@                                                    
   @routine    PUGH_GetSliceSizes
   @date       Fri Feb 21 10:21:36 2000
   @author     Matei Ripeanu
   @desc
               Computes partition based on parameter information
   @enddesc

   @var        np
   @vdesc      number of processors on this direction
   @vtype      int
   @vio        in
   @endvar
   @var        total
   @vdesc      number of grid points
   @vtype      int
   @vio        in
   @endvar
   @var        slicesS
   @vdesc      line in parameter file, format "10:10:10"
   @vtype      char *
   @vio        in
   @endvar
   @var        slices
   @vdesc      grid points foe each processor, int array
   @vtype      int **
   @vio        inout
   @endvar
   @var        manual
   @vdesc      manual or automatic partition
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0: OK
               -1: can not allocate memory
               -2: expect data for np processors but got for more
               -3: expect data for np processors but got for fewer
               -4: total doesn't match
   @endreturndesc
@@*/
static int PUGH_GetSliceSizes (int np,
                               int grid_points,
                               const char *slicesS,
                               int **slices,
                               int manual)
{
  int tmp, i=0, rt=0;


  *slices = (int *) malloc (np * sizeof (int));
  if (*slices == NULL)
  {
    CCTK_WARN (1, "Not enough memory");
    return (-1);
  }

  if (manual && (strlen (slicesS) > 0))
  {
    while (*slicesS)
    {
      if (i >= np)
      {
        CCTK_WARN (1, "Wrong partition parameters: "
                      "expect data for fewer processors");
        return (-2);
      }
      sscanf (slicesS, "%d", &tmp);
      if (i == 0)
      {
        (*slices)[i] = 0;
      }
      else
      {
        (*slices)[i] = rt - 1;
      }
      rt = rt + tmp;
      i++;
      while (isdigit ((int) *slicesS))
      {
        slicesS++;
      }
      while (*slicesS && ! isdigit ((int) *slicesS))
      {
        slicesS++;
      }
    }

    if (i != np)
    {
      CCTK_WARN (1, "Wrong partition parameters: "
                    "expect data for more processors");
      return (-3);
    }

    if (rt != grid_points)
    {
      CCTK_WARN (1, "Wrong partition parameters: "
                    "total number of grid points doesnt match");
      return (-4);
    }

  }
  else
  {
    for (i = 0; i < np; i++)
    {
      (*slices)[i] = rt;
      tmp = (grid_points - rt -1) / (np - i);
      rt = rt + tmp;
    }
  }

#ifdef DEBUG_LA
  printf("\n");
  for (i = 0; i<np; i++) printf( "%d :: ", (*slices)[i]);
  printf("\n");
#endif
  
  return (0);
}


/*
int main(void) {
  int ret, i, *rez;
  ret = PUGH_GetSliceSizes (5, 72, "12:12:24:12:12", &rez, 1);
  
  if (ret != 0) 
    printf ("Error: %d\n", ret);
  else
    for (i=0; i<5; i++) {   
      printf("%d\n", rez[i]);
    }
}
*/
