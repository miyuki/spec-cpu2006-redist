#ifdef SPEC_CPU
# define THORN_IS_IOUtil
#endif /* SPEC_CPU */
/*@@
   @file      Utils.c
   @date      Tue 4th July 2000
   @author    Gabrielle Allen
   @desc
              Utility routines which may be called by other IO thorns
   @enddesc 
   @version   $Id: Utils.c,v 1.15 2001/12/28 21:26:13 tradke Exp $
 @@*/


#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "ioutil_Utils.h"
#include "ioutil_CheckpointRecovery.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/IOUtil/src/Utils.c,v 1.15 2001/12/28 21:26:13 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOUtil_Utils_c)


/* uncomment this if you want some debugging output */
/* #define DEBUG_IOUTIL 1 */


/*@@
   @routine    IOUtil_1DLines
   @date       July 4 2000
   @author     Gabrielle Allen, Gerd Lanfermann, Thomas Radke
   @desc
               Fills out an array determining where to position 1D lines
               for output on a multidimensional regular Cartesian unigrid.
               The first slot of the array specifies the 1D line direction,
               the second slot fixes the indices of the starting point
               of that line on the grid.
   @enddesc

   @calls      CCTK_CoordSystemHandle
               CCTK_CoordRange

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        num_dims
   @vdesc      number of dimensions of underlying grid
   @vtype      int
   @vio        in
   @endvar
   @var        origin_index
   @vdesc      origin for 1D lines given by grid point indices
   @vtype      const int [num_dims][num_dims]
   @vio        in
   @endvar
   @var        origin_phys
   @vdesc      origin for 1D lines given by physical coordinates
   @vtype      const CCTK_REAL [num_dims][num_dims]
   @vio        in
   @endvar
   @var        slice_center
   @vdesc      resulting 1D line slice center
   @vtype      int [num_dims][num_dims]
   @vio        out
   @endvar

   @returntype int
   @returndesc
               0  for success
               -1 no coordinate system of given dimensions found
   @endreturndesc
 @@*/
int IOUtil_1DLines (const cGH *GH,
                    int num_dims,
                    int *const *const origin_index,
                    CCTK_REAL *const *const origin_phys,
                    int *const *slice_center)
{
  int dim, dir;
  char coord_system_name[20];
  CCTK_REAL *lower_range, *upper_range;
  int retval;

  retval = 0;

  /* allocate arrays for ranges */
  lower_range = (CCTK_REAL *) calloc (2 * num_dims, sizeof (CCTK_REAL));
  upper_range = lower_range + num_dims;

  /* get the appropriate coordinate system name */
  sprintf (coord_system_name, "cart%dd", num_dims);
  if (CCTK_CoordSystemHandle (coord_system_name) >= 0)
  {
    /* get the ranges in every direction */
    for (dir = 0; dir < num_dims; dir++)
    {
      if (CCTK_CoordRange (GH, &lower_range[dir], &upper_range[dir],
                           dir + 1, NULL, coord_system_name) < 0)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "IOUtil_1DLines: Could not get ranges for %c-direction "
                    "of coordinate system '%s'",
                    'x' + dir, coord_system_name);
      }
    }
  }
  else
  {
    CCTK_VWarn (4, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOUtil_1DLines: Cartesian coordinate system '%s' not found"
                " - no slice center set up for output of 1D lines from %dD "
                "variables", coord_system_name, num_dims);
    for (dir = 0; dir < num_dims; dir++)
    {
      for (dim = 0; dim < num_dims; dim++)
      {
        slice_center[dir][dim] = 0;
      }
    }

    retval = -1;
  }

  /* now set the slice center for each line
     according to origin_index[] or origin_phys[] */

  if (! retval)
  {
    for (dir = 0; dir < num_dims; dir++)
    {
      for (dim = 0; dim < num_dims; dim++)
      {
        if (dim == dir)
        {
          /* line always starts at the first point */
          slice_center[dir][dim] = 0;
        }
        else if (origin_index && origin_index[dir][dim] >= 0)
        {
          /* FIXME: check upper index bounds also ?? */
          slice_center[dir][dim] = origin_index[dir][dim];
        }
        else if (lower_range[dim] > origin_phys[dir][dim] || 
                 upper_range[dim] < origin_phys[dir][dim])
        {
          CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                      "IOUtil_1DLines: %c-coordinate for slice center of 1D "
                      "lines in %c-direction (%f) is out of grid coordinates "
                      "range (%f, %f)",
                      'x' + dim, 'x' + dir, (double) origin_phys[dir][dim],
                      (double) lower_range[dim], (double) upper_range[dim]);
          CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                      "IOUtil_1DLines: no 1D %c-line output will be written for "
                      "%dD variables with this slice center default",
                      'x' + dir, num_dims);
          
          slice_center[dir][dim] = -1;
        }
        else 
        {
          /* Find index for first point above the chosen coordinate */
          slice_center[dir][dim] =
            ceil ((origin_phys[dir][dim] - lower_range[dim]) /
                  GH->cctk_delta_space[dim] - 1e-6);

#ifdef DEBUG_IOUTIL
          printf("spxyz for %c-coord of lines in %c-direction is %d\n",
                 'x' + dim,'x' + dir, slice_center[dir][dim]);
#endif
        }
      }
    }
  }
  
  /* free allocated resources */
  free (lower_range);

  return (retval);
}


/*@@
   @routine    IOUtil_2DPlanes
   @date       July 4 2000
   @author     Gabrielle Allen, Gerd Lanfermann, Thomas Radke
   @desc
               Fills out an array determining where to position 2D planes
               for output on a multidimensional regular Cartesian unigrid.
   @enddesc

   @calls      CCTK_CoordRange

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        num_dims
   @vdesc      number of dimensions of underlying grid
   @vtype      int
   @vio        in
   @endvar
   @var        origin_index
   @vdesc      origin for 2D planes given by grid point indices
   @vtype      const int [num_dims]
   @vio        in
   @endvar
   @var        origin_phys
   @vdesc      origin for 2D planes given by physical coordinates
   @vtype      const CCTK_REAL [num_dims]
   @vio        in
   @endvar
   @var        slice_center
   @vdesc      resulting 2D plane slice center
   @vtype      int [num_dims]
   @vio        out
   @endvar

   @returntype int
   @returndesc
               0  for success
               -1 no coordinate system of given dimensions found
   @endreturndesc
 @@*/
int IOUtil_2DPlanes (const cGH *GH,
                     int num_dims,
                     const int *origin_index,
                     const CCTK_REAL *origin_phys,
                     int *slice_center)
{
  int dir;
  char coord_system_name[20];
  CCTK_REAL *lower_range, *upper_range;


  /* get the appropriate coordinate system name */
  sprintf (coord_system_name, "cart%dd", num_dims);

  if (CCTK_CoordSystemHandle (coord_system_name) < 0)
  {
    CCTK_VWarn (4, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOUtil_2DPlanes: Cartesian coordinate system '%s' not found"
                " - no slice center set up for output of 2D planes from %dD "
                "variables", coord_system_name, num_dims);
    return (-1);
  }

  /* allocate arrays for ranges */
  lower_range = (CCTK_REAL *) calloc (2 * num_dims, sizeof (CCTK_REAL));
  upper_range = lower_range + num_dims;

  /* get the ranges in every direction */
  for (dir = 0; dir < num_dims; dir++)
  {
    if (CCTK_CoordRange (GH, &lower_range[dir], &upper_range[dir],
                         num_dims - dir, NULL, coord_system_name) < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOUtil_2DPlanes: Could not get ranges for %c-direction "
                  "of coordinate system '%s'",
                  'x' + (num_dims - dir - 1), coord_system_name);
    }
  }

  /* now set the slice center for each line
     according to origin_index[] or origin_phys[] */
  for (dir = 0; dir < num_dims; dir++)
  {
    if (origin_index && origin_index[dir] >= 0)
    {
      slice_center[dir] = origin_index[dir];
    }
    else if (lower_range[dir] > origin_phys[dir] || 
             upper_range[dir] < origin_phys[dir])
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOUtil_2DPlanes: %c-coordinate for slice center of 2D "
                  "planes (%f) is out of grid coordinates range (%f, %f)",
                  'x' + (num_dims - dir - 1), (double) origin_phys[dir],
                  (double) lower_range[dir], (double) upper_range[dir]);
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOUtil_2DPlanes: no 2D planes in %c-direction will be "
                  "written for %dD variables with this slice center default",
                  'x' + (num_dims - dir - 1), num_dims);
      slice_center[dir] = 0;
    }
    else
    {
      /* Find index for first point above the chosen coordinate */
      slice_center[dir] = ceil ((origin_phys[dir] - lower_range[dir]) /
                                GH->cctk_delta_space[(num_dims - dir - 1)] -
                                1e-6);

#ifdef DEBUG_IOUTIL
      printf("sp2xyz for planes perpendicular to %d-direction is %d\n",
             (num_dims - dir - 1), (CCTK_INT) slice_center[dir]);
#endif
    }
  }

  /* free allocated resources */
  free (lower_range);

  return (0);
}


 /*@@
   @routine    IOUtil_PrintTimings
   @date       Wed Jun 28 2000
   @author     Thomas Radke
   @desc
               Gets the timing information for the given timers and prints it
               as INFO messages to screen.
   @enddesc
   @history
   @endhistory
   @var        description
   @vdesc      description of the timers
   @vtype      const char *
   @vio        in
   @endvar
   @var        ntimers
   @vdesc      number of timers passed in
   @vtype      int
   @vio        in
   @endvar
   @var        timers
   @vdesc      array of timers
   @vtype      const int [ntimers]
   @vio        in
   @endvar
   @var        timer_descriptions
   @vdesc      array of timer descriptions
   @vtype      const char *const [ntimers]
   @vio        in
   @endvar
@@*/

void IOUtil_PrintTimings (const char *description,
                          int ntimers,
                          const int *timers,
                          const char *const *const timer_descriptions)
{
  int i, j;
  cTimerData *info;


  info = CCTK_TimerCreateData ();
  if (info)
  {
    CCTK_INFO (description);

    for (i = 0; i < info->n_vals; i++)
    {
      for (j = 0; j < ntimers; j++)
      {
        CCTK_TimerI (timers[j], info);
        if (j == 0)
        {
          CCTK_VInfo (CCTK_THORNSTRING, "  %s:", info->vals[i].heading);
        }
        switch (info->vals[i].type)
        {
          case val_int:
            CCTK_VInfo (CCTK_THORNSTRING, "    %s %5d %s",
                        timer_descriptions [j],
                        info->vals[i].val.i, info->vals[i].units);
            break;

          case val_long:
            CCTK_VInfo (CCTK_THORNSTRING, "    %s %5d %s",
                        timer_descriptions [j],
                        (int) info->vals[i].val.l, info->vals[i].units);
            break;

          case val_double:
            CCTK_VInfo (CCTK_THORNSTRING, "    %s %5.1f %s",
                        timer_descriptions [j],
                        info->vals[i].val.d, info->vals[i].units);
            break;

          default:
            CCTK_WARN(1, "Unknown data type for timer info");
            break;
        }
      }
    }
    CCTK_INFO ("-----------------------------------------\n");
    CCTK_TimerDestroyData (info);
  }
  else
  {
    CCTK_WARN (1, "Couldn't create timer info structure ! "
                  "No timing output available.");
  }
}


 /*@@
   @routine    IOUtil_CreateDirectory
   @date       Fri 10 Aug 2001
   @author     Thomas Radke
   @desc
               Creates an output directory path and makes sure it is visible
               on all I/O processors.
               It is assumed that processor 0 is always an I/O processor.
               If there are other I/O processors, they will also try and
               create the directory themselfs. This guarantees the directory
               be created on all nodes in case the I/O processors don't share
               a common filesystem.
   @enddesc
   @calls      CCTK_MyProc
               CCTK_CreateDirectory
               CCTK_Barrier

   @var        GH
   @vdesc      pointer to the GH extensions
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        dirname
   @vdesc      the directory to create
   @vtype      const char *
   @vio        in
   @endvar
   @var        multiple_io_procs
   @vdesc      flag indicating that there will be I/O from multiple I/O procs
   @vtype      int
   @vio        in
   @endvar
   @var        ioproc
   @vdesc      I/O processor associated with this processor
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for non-I/O processors, or
               return code of @seeroutine CCTK_CreateDirectory
   @endreturndesc
@@*/
int IOUtil_CreateDirectory (const cGH *GH, const char *dirname,
                            int multiple_io_procs, int ioproc)
{
  int myproc, retval;


  /* default return value for non-I/O processors */
  retval = 0;

  /* first, processor 0 creates the directory */
  myproc = CCTK_MyProc (GH);
  if (myproc == 0)
  {
    retval = CCTK_CreateDirectory (0755, dirname);
  }

  if (multiple_io_procs)
  {
    /* now the other I/O processors create the directory
       after syncing with processor 0 */
    CCTK_Barrier (GH);
    if (myproc == ioproc || ioproc != 0)
    {
      retval = CCTK_CreateDirectory (0755, dirname);
    }
  }

  return (retval);
}
