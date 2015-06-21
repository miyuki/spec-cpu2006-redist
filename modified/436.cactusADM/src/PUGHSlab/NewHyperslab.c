#ifdef SPEC_CPU
# define THORN_IS_PUGHSlab
#endif /* SPEC_CPU */
 /*@@
   @file      Hyperslab.c
   @date      Thu 23 Nov 2000
   @author    Thomas Radke
   @desc 
              Routines to extract hyperslabs from CCTK array variables
   @enddesc 
   @version   $Id: NewHyperslab.c,v 1.11 2001/12/03 22:10:04 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"

#include "pugh.h"
#include "NewPUGHSlab.h"
#include "PUGHSlabi.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGHSlab/src/NewHyperslab.c,v 1.11 2001/12/03 22:10:04 tradke Exp $";
CCTK_FILEVERSION(CactusPUGH_PUGHSlab_NewHyperslab_c)

/********************************************************************
 ********************    Macro Definitions   ************************
 ********************************************************************/
/* define this if you want debugging output */
/*#define DEBUG 1*/

/* macros for getting the minimum/maximum value */
#ifdef MIN
#undef MIN
#endif
#define MIN(x, y)             ((x) < (y) ? (x) : (y))
#ifdef MAX
#undef MAX
#endif
#define MAX(x, y)             ((x) > (y) ? (x) : (y))

/* shortcuts for the local/global start/endpoints on this processor */
#define MY_LOCAL_SP(extras, istag, dim)    (extras->ownership[istag][0][dim])
#define MY_LOCAL_EP(extras, istag, dim)    (extras->ownership[istag][1][dim])
#define MY_GLOBAL_SP(extras, myproc, istag, dim)                              \
        (extras->lb[myproc][dim] + MY_LOCAL_SP (extras, istag, dim))
#define MY_GLOBAL_EP(extras, myproc, istag, dim)                              \
        (extras->lb[myproc][dim] + MY_LOCAL_EP (extras, istag, dim))


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* Gerd's routine to get 1D lines from a 3D array */
int Hyperslab_CollectData1D (const cGH *GH, int vindex, int vtimelvl,
                             const int *origin,
                             const int *directions,
                             int downsampling,
                             int length,
                             void **hdata,
                             int *hsize,
                             int proc);


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int checkFullHyperslab (pGA *GA,
                               int hdim,
                               const int global_startpoint[/* vdim */],
                               const int extents[/* hdim */],
                               const int downsample_[/* hdim */]);
static const char *checkParameters (const cGH *GH, int vindex, int vtimelvl,
                                    int hdim,
                                    const int global_startpoint[/* vdim */],
                                    const int directions[/* hdim*vdim */],
                                    const int extents[/* hdim */],
                                    const int downsample_[/* hdim */],
                                    void **hdata,
                                    int hsize[/* hdim */]);


/********************************************************************
 ********************    Macro Definitions   ************************
 ********************************************************************/
/* define this if you want debugging output */
/*#define DEBUG 1*/

/* macros for getting the minimum/maximum value */
#ifdef MIN
#undef MIN
#endif
#define MIN(x, y)             ((x) < (y) ? (x) : (y))
#ifdef MAX
#undef MAX
#endif
#define MAX(x, y)             ((x) > (y) ? (x) : (y))

/* shortcuts for the local/global start/endpoints on this processor */
#define MY_LOCAL_SP(extras, istag, dim)    (extras->ownership[istag][0][dim])
#define MY_LOCAL_EP(extras, istag, dim)    (extras->ownership[istag][1][dim])
#define MY_GLOBAL_SP(extras, myproc, istag, dim)                              \
        (extras->lb[myproc][dim] + MY_LOCAL_SP (extras, istag, dim))
#define MY_GLOBAL_EP(extras, myproc, istag, dim)                              \
        (extras->lb[myproc][dim] + MY_LOCAL_EP (extras, istag, dim))


/*@@
  @routine    Hyperslab_GetLocalHyperslab
  @date       Fri May 12 2000
  @author     Thomas Radke
  @desc
              Extract a hyperslab from the processor-local chunk
              of a domain-decomposed Cactus array variable.

              This routine delivers the local hyperslab data
              to be collected into a global hyperslab
              by Hyperslab_GetHyperslab().
              IO methods can call this routine as well to collect the
              local hyperslab data and output it in parallel.
  @enddesc 

  @calls      PUGHSlabi_GetDatatypeConversionFn

  @var        GH
  @vdesc      Pointer to CCTK grid hierarchy
  @vtype      const cGH *
  @vio        in
  @endvar
  @var        vindex
  @vdesc      index of variable to get a hyperslab from
  @vtype      int
  @vio        in
  @endvar
  @var        vtimelvl
  @vdesc      timelvl of variable to get a hyperslab from
  @vtype      int
  @vio        in
  @endvar
  @var        hdim
  @vdesc      dimensionality of the requested hyperslab
  @vtype      int
  @vio        in
  @endvar
  @var        htype
  @vdesc      CCTK datatype of the requested hyperslab
  @vtype      int
  @vio        in
  @endvar
  @var        conversion_fn
  @vdesc      pointer to a user-supplied data conversion function
  @vtype      t_hslabConversionFn
  @vio        in
  @endvar
  @var        global_startpoint
  @vdesc      global coordinates of the hyperslab origin
  @vtype      const int[dimensions of vindex]
  @vio        in
  @endvar
  @var        directions
  @vdesc      directions which span the hyperslab
  @vtype      const int[hdim times dimensions of vindex]
  @vio        in
  @endvar
  @var        extents
  @vdesc      number of grid points to follow in each hyperslab direction
              starting from origin
              Negative values are taken as extents up to the grid boundaries.
  @vtype      const int[hdim]
  @vio        in
  @endvar
  @var        downsample_
  @vdesc      downsampling values for each hyperslab dimension
  @vtype      const int[hdim]
  @vio        in
  @endvar
  @var        hdata
  @vdesc      pointer to store the address of the hyperslab data buffer
  @vtype      void **
  @vio        out
  @endvar
  @var        free_data
  @vdesc      address of flag which decides whether the returned data needs
              to be freed or not
  @vtype      int *
  @vio        out
  @endvar
  @var        hsize
  @vdesc      sizes of the (local) hyperslab data buffer in each dimension
  @vtype      int[hdim]
  @vio        out
  @endvar
  @var        hsize_global
  @vdesc      sizes of the global hyperslab data buffer in each dimension
  @vtype      int[hdim]
  @vio        out
  @endvar
  @var        hoffset_global
  @vdesc      if not NULL, array to save the offsets of the local hyperslab
              into the global one for each dimension
  @vtype      int[hdim]
  @vio        out
  @endvar
@@*/
int NewHyperslab_GetLocalHyperslab (const cGH *GH,
                                    int vindex,
                                    int vtimelvl,
                                    int hdim,
                                    int htype,
                                    t_hslabConversionFn conversion_fn,
                                    const int global_startpoint[/* vdim */],
                                    const int directions[/* hdim*vdim */],
                                    const int extents[/* hdim */],
                                    const int downsample_[/* hdim */],
                                    void **hdata,
                                    int *free_hdata,
                                    int hsize[/* hdim */],
                                    int hsize_global[/* hdim */],
                                    int hoffset_global[/* hdim */])
{
  int *point;                    /* looper over hyperslab dimensions */
  int *startpoint,               /* hyperslab's local start and endpoint */
      *endpoint;                 /* within the variable's grid dimensions */
  int *downsample;               /* the downsample_[] vector extended to vdim */
  int *my_global_startpoint,     /* hyperslab's global start and endpoint */
      *global_endpoint;
  int *points_per_dim;           /* points per subvolume */
  int *do_dir;                   /* directions in which to span the hyperslab */
  int stagger_index;             /* stagger index in direction i */
  int myproc;                    /* local processor ID */
  int i;                         /* general looper */
  int full_hyperslab;            /* flag indicating a full hyperslab request */
  int vdim;                      /* looper over all source dimensions */
  int vdata_size,                /* size of one data point in bytes for */
      hdata_size;                /* source and hyperslab data */
  int totals;                    /* total number of hyperslab data points */
  int dim0_points;               /* number of hyperslab points in dim 0 */
  int dim0_hsize;                /* byte size of hyperslab points in dim 0 */
  char *typed_vdata,             /* byte pointers into source and */
       *typed_hdata;             /* hyperslab data arrays */
  int retval;                    /* the return value (0 for success) */
  cGroup vinfo;                  /* variable's group info */
  pGH *pughGH;                   /* pointer to the current pGH */
  pGA *GA;                       /* the variable's GA structure from PUGH */
  const char *errormsg;          /* error message string */


  /* initialize the return parameters to some known value
     even before checking them */
  if (hdata)
  {
    *hdata = NULL;
  }
  if (free_hdata)
  {
    *free_hdata = 0;
  }

  /* do some plausibility checks */
  errormsg = checkParameters (GH, vindex, vtimelvl, hdim,
                              global_startpoint, directions, extents,
                              downsample_, hdata, hsize);

  /* immediately return in case of errors */
  if (errormsg)
  {
    CCTK_WARN (1, errormsg);
    return (-1);
  }

  /* get the info on the variable to extract a hyperslab from */
  CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo);

  /* if datatype conversion was requested
     get the appropriate predefined datatype conversion routine
     in case the user didn't supply one by her own */
  if (vinfo.vartype != htype)
  {
    if (conversion_fn == NULL)
    {
      conversion_fn = PUGHSlabi_GetDatatypeConversionFn (vinfo.vartype, htype);
      if (! conversion_fn)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "No predefined PUGHSlab routine available to convert "
                    "'%s' into '%s'",
                    CCTK_VarTypeName (vinfo.vartype), CCTK_VarTypeName (htype));
        return (-1);
      }
    }
  }
  else if (conversion_fn)
  {
    CCTK_WARN (8, "Datatype conversion routine supplied but no datatype "
                  "conversion requested. Ignoring conversion routine...");
    conversion_fn = NULL;
  }

  /* set the default return code */
  retval = 0;

  /* allocate the temporary arrays */
  point = (int *) malloc (8 * vinfo.dim * sizeof (int));
  startpoint           = point + 1*vinfo.dim;
  endpoint             = point + 2*vinfo.dim;
  my_global_startpoint = point + 3*vinfo.dim;
  global_endpoint      = point + 4*vinfo.dim;
  downsample           = point + 5*vinfo.dim;
  do_dir               = point + 6*vinfo.dim;
  points_per_dim       = point + 7*vinfo.dim;

  /* get the actual directions in which to spawn the hyperslab */
  memcpy (do_dir, directions, vinfo.dim * sizeof (int));
  for (i = 1; i < hdim; i++)
  {
    for (vdim = 0; vdim < vinfo.dim; vdim++)
    {
      do_dir[vdim] |= directions[i * vinfo.dim + vdim];
    }
  }

  /* get the pGH pointer and the variable's GA structure */
  pughGH = PUGH_pGH (GH);
  GA     = (pGA *) pughGH->variables[vindex][vtimelvl];

  /* get the local processor ID */
  myproc = CCTK_MyProc (GH);

  /* check whether the full local data patch was requested as hyperslab */
  full_hyperslab = checkFullHyperslab (GA, hdim, global_startpoint, extents,
                                       downsample_);
  if (full_hyperslab)
  {
    memset (startpoint,   0,                  vinfo.dim * sizeof (int));
    memcpy (endpoint,     GA->extras->lnsize, vinfo.dim * sizeof (int));
    memcpy (downsample,   downsample_,        vinfo.dim * sizeof (int));

    memcpy (hsize,        GA->extras->lnsize, vinfo.dim * sizeof (int));
    memcpy (hsize_global, GA->extras->nsize,  vinfo.dim * sizeof (int));
    totals = GA->extras->npoints;
  }
  else
  {
    /* compute the global endpoint */
    for (vdim = hdim = 0; vdim < vinfo.dim; vdim++)
    {
      if (do_dir[vdim])
      {
        global_endpoint[vdim] = extents[hdim] > 0 ?
                                 MIN (global_startpoint[vdim] + extents[hdim],
                                      GA->extras->nsize[vdim]) :
                                 GA->extras->nsize[vdim];
        downsample[vdim] = downsample_[hdim];
        hdim++;
      }
      else
      {
        global_endpoint[vdim] = global_startpoint[vdim] + 1;
        downsample[vdim] = 1;
      }
    }

    /* compute my global startpoint from the global ranges */
    for (vdim = 0; vdim < vinfo.dim; vdim++)
    {
      stagger_index = CCTK_StaggerDirIndex (vdim, vinfo.stagtype);

      if (global_startpoint[vdim] < MY_GLOBAL_EP (GA->extras, myproc,
                                                  stagger_index, vdim))
      {
        if (global_startpoint[vdim] < MY_GLOBAL_SP (GA->extras, myproc,
                                                    stagger_index, vdim))
        {
          int npoints;

          npoints = (MY_GLOBAL_SP (GA->extras, myproc, stagger_index, vdim)
                     - global_startpoint[vdim]) / downsample[vdim];
          if ((MY_GLOBAL_SP (GA->extras, myproc, stagger_index, vdim)
               - global_startpoint[vdim]) % downsample[vdim])
          {
            npoints++;
          }
          my_global_startpoint[vdim] = global_startpoint[vdim] +
                                       npoints*downsample[vdim];
        }
        else
        {
          my_global_startpoint[vdim] = global_startpoint[vdim];     
        }
      }
      else
      {
        my_global_startpoint[vdim] = -1;
      }
    }

    /* compute the local start- and endpoint from the global ranges */
    totals = 1;
    for (vdim = hdim = 0; vdim < vinfo.dim; vdim++)
    {
      stagger_index = CCTK_StaggerDirIndex (vdim, vinfo.stagtype);

      if (my_global_startpoint[vdim] >= 0 &&
          my_global_startpoint[vdim] <  MY_GLOBAL_EP (GA->extras, myproc,
                                                      stagger_index, vdim))
      {
        startpoint[vdim] = my_global_startpoint[vdim] -
                           GA->extras->lb[myproc][vdim];
      }
      else
      {
        startpoint[vdim] = -1;
      }

      if (global_endpoint[vdim] > MY_GLOBAL_SP (GA->extras, myproc,
                                                stagger_index, vdim))
      {
        endpoint[vdim] = MIN (MY_LOCAL_EP (GA->extras, stagger_index, vdim),
                              global_endpoint[vdim] - GA->extras->lb[myproc][vdim]);
      }
      else
      {
        endpoint[vdim] = -1;
      }

#ifdef DEBUG
      printf ("direction %d: local ranges[%d, %d)\n",
              vdim, startpoint[vdim], endpoint[vdim]);
#endif

      if (endpoint[vdim] < 0 || startpoint[vdim] < 0)
      {
        totals = 0;
        endpoint[vdim] = startpoint[vdim];
      }

      if (do_dir[vdim])
      {
        /* compute the global and local size in each hyperslab dimension */
        hsize_global[hdim] = (global_endpoint[vdim] - global_startpoint[vdim]) /
                              downsample[vdim];
        if ((global_endpoint[vdim] - global_startpoint[vdim]) %
            downsample[vdim])
        {
          hsize_global[hdim]++;
        }
        if (GA->connectivity->perme[vdim])
        {
          hsize_global[hdim] -= 2 * GA->extras->nghostzones[vdim];
        }
        hsize[hdim] = (endpoint[vdim] - startpoint[vdim]) / downsample[vdim];
        if ((endpoint[vdim] - startpoint[vdim]) % downsample[vdim])
        {
          hsize[hdim]++;
        }
        totals *= hsize[hdim];
        hdim++;
      }
    }
  } /* end of else branch for 'if (full_hyperslab)' */

#ifdef DEBUG
  printf ("total number of hyperslab data points: %d\n", totals);
#endif

  /* nested loop over vinfo.dim dimensions */
  /* NOTE: the following code assumes startpoint[vdim] < endpoint[vdim] */
  if (totals > 0)
  {
    void *vdata = CCTK_VarDataPtrI (GH, vtimelvl, vindex);


    /* if requested, compute the offsets into the global hyperslab */
    if (hoffset_global)
    {
      for (vdim = hdim = 0; vdim < vinfo.dim; vdim++)
      {
        if (do_dir[vdim])
        {
          if (full_hyperslab)
          {
            hoffset_global[hdim] = GA->extras->lb[myproc][hdim];
          }
          else
          {
            hoffset_global[hdim] = (my_global_startpoint[vdim] -
                                    global_startpoint[vdim]) / downsample[vdim];
            if (GA->connectivity->perme[vdim])
            {
              hoffset_global[hdim] -= GA->extras->nghostzones[vdim];
            }
          }
#ifdef DEBUG
          printf ("hoffset_global, hsize in direction %d: %d, %d\n",
                  hdim, hoffset_global[hdim], hsize[hdim]);
#endif
          hdim++;
        }
      }
    }

    if (full_hyperslab && conversion_fn == NULL)
    {
      /* free_hdata was set to false already at function entry */
      *hdata = vdata;
    }
    else
    {
      /* get the byte size of a single data point
         in the variable and hyperslab data array */
      vdata_size = CCTK_VarTypeSize (vinfo.vartype);
      hdata_size = CCTK_VarTypeSize (htype);

      /* allocate the buffer for the hyperslab data */
      *hdata = malloc (totals * hdata_size);
      typed_hdata = (char *) (*hdata);
      *free_hdata = 1;

      /* compute the points_per_dim[] vector */
      /* NOTE: this could be computed at startup and kept in a GH extension
               once we have one for thorn Hyperslab */
      points_per_dim[0] = 1;
      for (vdim = 1; vdim < vinfo.dim; vdim++)
      {
        points_per_dim[vdim] = points_per_dim[vdim-1] *
                               GA->extras->lnsize[vdim-1];
      }

      /* get the number of hyperslab points in lowest dimension
         and their size in bytes */
      dim0_points = (endpoint[0] - startpoint[0]) / downsample[0];
      if ((endpoint[0] - startpoint[0]) % downsample[0])
      {
        dim0_points++;
      }
      dim0_hsize = dim0_points * hdata_size;

      /* transform the ranges into byte ranges */
      for (i = 0; i < vinfo.dim; i++)
      {
        startpoint[i] *= vdata_size;
        endpoint[i]   *= vdata_size;
        downsample[i] *= vdata_size;
      }

      /* initialize the index vector to the local startpoint */
      memcpy (point, startpoint, vinfo.dim * sizeof (point[0]));

      /* do the nested loops starting with the innermost */
      vdim = 1;
      while (1)
      {
        /* check for end of current loop */
        if (vinfo.dim > 1 && point[vdim] >= endpoint[vdim])
        {
          /* increment outermost loopers */
          for (vdim++; vdim < vinfo.dim; vdim++)
          {
            point[vdim] += downsample[vdim];
            if (point[vdim] < endpoint[vdim])
            {
              break;
            }
          }

          /* done if beyond outermost loop */
          if (vdim >= vinfo.dim)
          {
            break;
          }

          /* reset innermost loopers */
          for (vdim--; vdim > 0; vdim--)
          {
            point[vdim] = startpoint[vdim];
          }
          vdim = 1;
        }

        /* get the byte pointer into the source array */
        typed_vdata = (char *) vdata + point[0];
        for (i = 1; i < vinfo.dim; i++)
        {
          typed_vdata += point[i] * points_per_dim[i];
        }

        /* copy the data in lowest dimension: if possible copy all data points
           in a row otherwise do it one by one */
        if (downsample[0] == vdata_size)
        {
          if (conversion_fn)
          {
            conversion_fn (typed_vdata, typed_hdata, dim0_points, 1, 1);
          }
          else
          {
            memcpy (typed_hdata, typed_vdata, dim0_hsize);
          }
        }
        else
        {
          if (conversion_fn)
          {
            conversion_fn (typed_vdata, typed_hdata, dim0_points,
                           downsample[0], 1);
            typed_vdata += downsample[0] * dim0_points;
          }
          else
          {
            for (i = 0; i < dim0_hsize; i += hdata_size)
            {
              memcpy (typed_hdata + i, typed_vdata, vdata_size);
              typed_vdata += downsample[0];
            }
          }
        }
        typed_hdata += dim0_hsize;

        if (vinfo.dim > 1)
        {
          /* increment current looper */
          point[vdim] += downsample[vdim];
        }
        else
        {
          /* exit loop if hyperslab dim is only 1D */
          break;
        }

      } /* end of nested loops over all dimensions */
    } /* end of branch extracting the hyperslab data */

  }
  else
  {
    /* clear out the return values if there's no hyperslab data */
    /* hdata and free_hdata were set to NULL/false already at function entry */
    memset (hsize, 0, hdim * sizeof (int));
  }

  /* free allocated temporary memory */
  free (point);

  return (retval);
}


#if 0
/*@@
  @routine    Hyperslab_GetHyperslab
  @date       Fri May 12 2000
  @author     Thomas Radke
  @desc
              Extract a hyperslab from a Cactus array variable.
              All processor-local hyperslab data are collected
              into a single global buffer.
  @enddesc 

  @calls      Hyperslab_CollectData1D
              Hyperslab_GetLocalHyperslab

  @var        GH
  @vdesc      Pointer to CCTK grid hierarchy
  @vtype      const cGH *
  @vio        in
  @endvar
  @var        vindex
  @vdesc      index of variable to get a hyperslab from
  @vtype      int
  @vio        in
  @endvar
  @var        vtimelvl
  @vdesc      timelvl of variable to get a hyperslab from
  @vtype      int
  @vio        in
  @endvar
  @var        hdim
  @vdesc      dimensionality of the requested hyperslab
  @vtype      int
  @vio        in
  @endvar
  @var        htype
  @vdesc      CCTK datatype of the requested hyperslab
  @vtype      int
  @vio        in
  @endvar
  @var        conversion_fn
  @vdesc      pointer to a user-supplied data conversion function
  @vtype      PUGHSlab_conversion_fn
  @vio        in
  @endvar
  @var        global_startpoint
  @vdesc      global coordinates of the hyperslab origin
  @vtype      const int[dimensions of vindex]
  @vio        in
  @endvar
  @var        directions
  @vdesc      directions which span the hyperslab
  @vtype      const int[hdim times dimensions of vindex]
  @vio        in
  @endvar
  @var        extents
  @vdesc      number of grid points to follow in each hyperslab direction
              starting from origin
              Negative values are taken as extents up to the grid boundaries.
  @vtype      const int[hdim]
  @vio        in
  @endvar
  @var        downsample_
  @vdesc      downsampling values for each hyperslab dimension
  @vtype      const int[hdim]
  @vio        in
  @endvar
  @var        hdata
  @vdesc      pointer to store the address of the hyperslab data buffer
  @vtype      void **
  @vio        out
  @endvar
  @var        free_data
  @vdesc      address of flag which decides whether the returned data needs
              to be freed or not
  @vtype      int *
  @vio        out
  @endvar
  @var        hsize
  @vdesc      sizes of the hyperslab data buffer in each dimension
  @vtype      int[hdim]
  @vio        out
  @endvar
@@*/
int NewHyperslab_GetHyperslab (const cGH *GH,
                            int target_proc,
                            int vindex,
                            int vtimelvl,
                            int hdim,
                            int htype,
                            PUGHSlab_conversion_fn conversion_fn,
                            const int global_startpoint[/* vdim */],
                            const int directions[/* hdim*vdim */],
                            const int extents[/* hdim */],
                            const int downsample_[/* hdim */],
                            void **hdata,
                            int *free_hdata,
                            int hsize[/* hdim */])
{
  int myproc, nprocs;         /* processor identification */
  int retval;                 /* the return value */
  cGroup vinfo;               /* variable's group info structure */
  void *hdata_local;          /* buffer holding local hyperslab data */
  void **hdata_ptr;           /* address of local hyperslab buffer pointer */
  int *hsize_local,           /* sizes of the local and global hyperslab */
      *hsize_global;          /* for each direction */
  int *hoffset_local;         /* offsets of local into the global hyperslab */
  const char *errormsg;       /* explanation string in case of errors */
#ifdef CCTK_MPI
  int i, proc;                /* loopers */
  int totals_local,           /* number of local and global */
      totals_global;          /* hyperslab points */
  MPI_Comm comm;              /* MPI communicator to use */
  MPI_Datatype mpi_vtype;     /* MPI datatype to use */
  int htypesize;              /* byte-size of a single hyperslab data point */
  void *chunked_hdata;        /* receive buffer for all hyperslab chunks */
  int *displs, *recvcnts;     /* displacements/receive counts for gathering */
  CCTK_INT *sizes_local,      /* MPI buffers for communicating the */
           *sizes_global;     /* hyperslab sizes and offsets */
#endif


  /* initialize the return parameters to some known value
     even before checking them */
  if (hdata)
  {
    *hdata = NULL;
  }
  if (free_hdata)
  {
    *free_hdata = 0;
  }

  /* do some plausibility checks */
  errormsg = checkParameters (GH, vindex, vtimelvl, hdim,
                              global_startpoint, directions, extents,
                              downsample_, hdata, hsize);

  /* FIXME: hack for getting diagonals from 3D variables
            This is calling Gerd's CollectData1D() routine which can
            extract non-orthogonal lines too but is fixed to 3D data. */
  if (errormsg && ! strcmp (errormsg, "Given normal vector isn't orthogonal"))
  {
    int length;


    /* get the info on the variable to extract a hyperslab from */
    CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo);

    if (hdim != 1 || vinfo.dim != 3)
    {
      CCTK_WARN (1, "Non-orthogonal hyperslabs are supported as 1D lines "
                    "from 3D variables only");
      return (-1);
    }

    length = extents[0];
    if (length > 0)
    {
      length--;
    }
    return (Hyperslab_CollectData1D (GH, vindex, vtimelvl, global_startpoint,
                                     directions, downsample_[0], length,
                                     hdata, hsize, target_proc));
  }

  /* immediately return in case of errors */
  if (errormsg)
  {
    CCTK_WARN (1, errormsg);
    return (-1);
  }

  /* get processor information */
  myproc = CCTK_MyProc (GH);
  nprocs = CCTK_nProcs (GH);
  if (target_proc >= nprocs)
  {
    CCTK_WARN (1, "Invalid target processor ID");
    return (-1);
  }

  /* target processors must pass pointers to where to store the results */
  if (target_proc < 0 || target_proc == myproc)
  {
    if (! hdata || ! hsize)
    {
      CCTK_WARN (1, "Must pass valid hyperslab data and sizes buffer pointers");
      return (-1);
    }

    /* default return values: no hyperslab data */
    *hdata = NULL;
    memset (hsize, 0, hdim * sizeof (int));
  }

  /* get the info on the variable to extract a hyperslab from */
  CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo);

  /* set the pointers to pass to Hyperslab_GetLocalHyperslab() */
  if (nprocs == 1)
  {
    hdata_ptr = hdata;
    hsize_local = hsize_global = hsize;
    hoffset_local = NULL;
  }
  else
  {
    hdata_ptr = &hdata_local;
    hsize_local = (int *) malloc (3 * hdim * sizeof (int));
    hoffset_local = hsize_local + hdim;
    hsize_global = hoffset_local + hdim;
  }

  /* get the processor-local hyperslab */
  retval = NewHyperslab_GetLocalHyperslab (GH, vindex, vtimelvl, hdim, htype,
                                        conversion_fn, global_startpoint,
                                        directions, extents, downsample_, 0,
                                        hdata_ptr, free_hdata, hsize_local,
                                        hsize_global, hoffset_local);

  /* that's it for the single processor case */
  if (nprocs == 1)
  {
    return (retval);
  }

#ifdef CCTK_MPI
  if (retval)
  {
    CCTK_WARN (1, "Hyperslab_GetLocalHyperslab() failed\n");
    free (hsize_local);
    return (retval);
  }

  /* now build the CCTK_INT buffer to communicate the sizes and offsets */
  sizes_local = (CCTK_INT *) malloc ((1 + nprocs) * (2*hdim + 1)
                                                  * sizeof (CCTK_INT));
  sizes_global = sizes_local + 2*hdim + 1;
  totals_local = 1;
  for (i = 0; i < hdim; i++)
  {
    totals_local *= hsize_local[i];
    sizes_local[0*hdim + i] = hsize_local[i];
    sizes_local[1*hdim + i] = hoffset_local[i];
  }
  sizes_local[2*hdim + 0] = totals_local;

  comm = PUGH_pGH (GH)->PUGH_COMM_WORLD;
  /* FIXME: do an Allgather here so that all procs know how many data points
            we have. This is useful to decide whether we have to gather 
            something at all or not and thus can return immediately */
  CACTUS_MPI_ERROR (MPI_Allgather (sizes_local, 2*hdim + 1, PUGH_MPI_INT,
                    sizes_global, 2*hdim + 1, PUGH_MPI_INT, comm));

  /* sum up the total number of hyperslab data points */
  totals_global = sizes_global[2*hdim];
  for (proc = 1; proc < nprocs; proc++) 
  {
    totals_global += sizes_global[proc*(2*hdim+1) + 2*hdim];
  }

  /* immediately return if there is no data at all */
  if (totals_global <= 0)
  {
    free (sizes_local);
    free (hsize_local);
    return (retval);
  }

  htypesize = CCTK_VarTypeSize (htype);
  if (target_proc < 0 || target_proc == myproc)
  {
    /* copy dimensions for the returned hyperslab */
    memcpy (hsize, hsize_global, hdim * sizeof (int));

    /* set the displacements/receive counts for the gather operation */
    displs = (int *) malloc (2 * nprocs * sizeof (int));
    recvcnts = displs + nprocs;

    /* for hdim == 1 we let MPI_Gatherv() do the sorting directly into the
       hyperslab buffer using the given hyperslab offsets and sizes */
    if (hdim == 1)
    {
      for (proc = 0; proc < nprocs; proc++)
      {
        displs[proc]   = sizes_global[proc*(2*hdim+1) + 2*hdim - 1];
        recvcnts[proc] = sizes_global[proc*(2*hdim+1) + 2*hdim];
      }
    }
    else
    {
      displs[0] = 0;
      recvcnts[0] = sizes_global[2*hdim];
      for (proc = 1; proc < nprocs; proc++)
      {
        displs[proc]   = displs[proc-1] + sizes_global[(proc-1)*(2*hdim+1) +
                                                       2*hdim];
        recvcnts[proc] = sizes_global[proc*(2*hdim+1) + 2*hdim];
      }
    }

    chunked_hdata = malloc (totals_global * htypesize);
  }
  else
  {
    displs = recvcnts = NULL;
    chunked_hdata = NULL;
  }

  /* detect the MPI datatype to use */
  mpi_vtype = PUGH_MPIDataType (PUGH_pGH (GH), htype);

  /* collect the hyperslab chunks from all processors */
  if (target_proc < 0)
  {
    CACTUS_MPI_ERROR (MPI_Allgatherv (hdata_local, totals_local, mpi_vtype,
                      chunked_hdata, recvcnts, displs, mpi_vtype, comm));
  }
  else
  {
    CACTUS_MPI_ERROR (MPI_Gatherv (hdata_local, totals_local, mpi_vtype,
                      chunked_hdata, recvcnts, displs, mpi_vtype,
                      target_proc, comm));
  }

  /* free the processor-local chunk */
  if (hdata_local)
  {
    free (hdata_local);
  }

  /* Now we got the global hyperslab data in a chunked array.
     The user wants it unchunked, so let's sort it... */
  if (target_proc < 0 || target_proc == myproc)
  {
    /* for hdim == 1 the hyperslab was already sorted by MPI_Gatherv()
       we just need to return that buffer */
    if (hdim == 1)
    {
      *hdata = chunked_hdata;
    }
    else
    {
      int j;
      int *point;
      int *points_per_hdim;
      char *copy_from, *copy_to;
      CCTK_INT *hsize_chunk, *hoffset_chunk;
      int linear_hoffset;


      /* allocate temporary vectors */
      point = (int *) malloc (2 * hdim * sizeof (int));
      points_per_hdim = point + hdim;

      points_per_hdim[0] = 1;
      for (i = 1; i < hdim; i++)
      {
        points_per_hdim[i] = points_per_hdim[i-1] * hsize[i-1];
      }

      /* allocate buffer for the returned hyperslab */
      *hdata = malloc (totals_global * htypesize);

      /* use char pointers for easy incrementing when copying */
      copy_from = (char *) chunked_hdata;
      copy_to   = (char *) *hdata;

      /* now copy the chunks from each processor into the global hyperslab */
      for (proc = 0; proc < nprocs; proc++)
      {
        /* skip processors which didn't contribute any data */
        if (sizes_global[proc * (2*hdim + 1) + 2*hdim] <= 0)
        {
          continue;
        }

        hsize_chunk = sizes_global + proc * (2*hdim+1);
        hoffset_chunk = hsize_chunk + hdim;

        /* set startpoint to zero */
        memset (point, 0, hdim * sizeof (int));

        i = 1;
        while (1)
        {
          /* check for end of current loop */
          if (point[i] >= hsize_chunk[i])
          {
            /* increment outermost loopers */
            for (i++; i < hdim; i++)
            {
              if (++point[i] < hsize_chunk[i])
              {
                break;
              }
            }

            /* done if beyond outermost loop */
            if (i >= hdim)
            {
              break;
            }

            /* reset innermost loopers */
            for (i--; i > 0; i--)
            {
              point[i] = 0;
            }
            i = 1;
          }

          /* get the linear offset */
          linear_hoffset = hoffset_chunk[0];
          for (j = 1; j < hdim; j++)
          {
            linear_hoffset += (hoffset_chunk[j] + point[j]) *
                               points_per_hdim[j];
          }
          /* copy the line */
          memcpy (copy_to + linear_hoffset * htypesize,
                  copy_from, hsize_chunk[0] * htypesize);
          copy_from += hsize_chunk[0] * htypesize;

          /* increment current looper */
          point[i]++;

        } /* end of nested loops over all dimensions */
      } /* end of loop over all processors */

      /* free allocated temporary vectors and the chunk buffer */
      free (chunked_hdata);
      free (point);
    }

    /* free allocated vectors for MPI communication */
    free (displs);
  }

  /* free allocated arrays for communicating the hyperslab sizes of offsets */
  free (sizes_local);
  free (hsize_local);

#endif /* CCTK_MPI */

  return (retval);
}
#endif


/********************** local routines ************************************/
static int checkFullHyperslab (pGA *GA,
                               int hdim,
                               const int global_startpoint[/* vdim */],
                               const int extents[/* hdim */],
                               const int downsample_[/* hdim */])
{
  int i;
  int is_full_hyperslab;


  is_full_hyperslab = hdim == GA->extras->dim;

  if (is_full_hyperslab)
  {
    for (i = 0; i < GA->extras->dim; i++)
    {
      is_full_hyperslab &= (global_startpoint[i] == 0);
      is_full_hyperslab &= (extents[i] <= 0);
      is_full_hyperslab &= (downsample_[i] <= 1);
      is_full_hyperslab &= (GA->connectivity->perme[i] == 0);
    }
  }

  return (is_full_hyperslab);
}


static const char *checkParameters (const cGH *GH, int vindex, int vtimelvl,
                                    int hdim,
                                    const int global_startpoint[/* vdim */],
                                    const int directions[/* hdim*vdim */],
                                    const int extents[/* hdim */],
                                    const int downsample_[/* hdim */],
                                    void **hdata,
                                    int hsize[/* hdim */])
{
  int i, vdim;                   /* looper */
  int num_directions;            /* number of non-zero directions */
  cGroup vinfo;                  /* variable's group info */


  /* check the variable index and timelevel */
  if (vindex < 0 || vindex >= CCTK_NumVars ())
  {
    return ("Invalid variable index");
  }
  if (vtimelvl < 0 || vtimelvl >= CCTK_NumTimeLevelsFromVarI (vindex))
  {
    return ("Invalid timelevel");
  }

  /* check the passed pointers */
  if (! global_startpoint || ! directions || ! extents || ! downsample_ ||
      ! hdata || ! hsize)
  {
    return ("NULL pointer(s) passed as parameters");
  }

  /* check the extent and downsample parameters */
  for (vdim = 0; vdim < hdim; vdim++)
  {
    if (extents[vdim] == 0)
    {
      return ("Invalid hyperslab extent parameters");
    }
    if (downsample_[vdim] <= 0)
    {
      return ( "Invalid hyperslab downsample parameters");
    }
  }

  /* get the info on the variable to extract a hyperslab from */
  if (CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo) < 0)
  {
    return ("Couldn't get group info");
  }

  /* check the variable's grouptype */
  if (vinfo.grouptype != CCTK_GF && vinfo.grouptype != CCTK_ARRAY)
  {
    return ("Invalid variable group type");
  }

  /* check the hyperslab dimension */
  if (hdim <= 0 || hdim > vinfo.dim)
  {
    return ("Invalid hyperslab dimension");
  }

  /* check the direction(s) of the hyperslab */
  for (i = 0; i < hdim; i++)
  {
    for (vdim = 0, num_directions = 0; vdim < vinfo.dim; vdim++)
    {
      if (directions[i * vinfo.dim + vdim])
      {
        num_directions++;
      }
    }
    if (num_directions == 0)
    {
      return ("Given direction vector is a null vector");
    }
    if (num_directions != 1)
    {
      return ("Given direction vector isn't orthogonal");
    }
  }

  /* check if PUGH is active */
  if (! PUGH_pGH (GH))
  {
    return ("No GH extension for PUGH found. Did you activate thorn PUGH ?");
  }

  return (NULL);
}
