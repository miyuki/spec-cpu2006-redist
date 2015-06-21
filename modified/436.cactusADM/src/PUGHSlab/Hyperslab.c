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
   @version   $Id: Hyperslab.c,v 1.22 2001/12/19 00:00:37 tradke Exp $
 @@*/


#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"

#include "ioGH.h"
#include "pugh.h"
#include "PUGHSlab.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGHSlab/src/Hyperslab.c,v 1.22 2001/12/19 00:00:37 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGHSlab_Hyperslab_c)

/* enable debugging output */
/* #define DEBUG 1 */

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


/* macro for copying all relevant data points
   (ranging from origin[] to endpoint[])
   into the typed hyperslab data buffer */
#define PICKUP_HYPERSLAB_DATA(cctk_type, vdim, vdata, hdata, point,           \
                              origin, endpoint, downsample,points_per_dim)\
    {                                                                         \
      int i, dim, idx, dim0_elements;                                         \
      cctk_type *typed_vdata = (cctk_type *) vdata;                           \
      cctk_type *typed_hdata = (cctk_type *) hdata;                           \
                                                                              \
                                                                              \
      /* set point to local origin */                                     \
      memcpy (point, origin, vdim * sizeof (point[0]));                   \
                                                                              \
      dim0_elements = endpoint[0] - origin[0];                            \
                                                                              \
      /* do the nested loops starting with the innermost */                   \
      dim = 1;                                                                \
      while (1)                                                               \
      {                                                                       \
        /* check for end of current loop */                                   \
        if (vdim > 1 && point[dim] >= endpoint[dim])                          \
        {                                                                     \
          /* increment outermost loopers */                                   \
          for (dim++; dim < vdim; dim++)                                      \
          {                                                                   \
            point[dim] += downsample[dim];                                    \
            if (point[dim] < endpoint[dim])                                   \
            {                                                                 \
              break;                                                          \
            }                                                                 \
          }                                                                   \
                                                                              \
          /* done if beyond outermost loop */                                 \
          if (dim >= vdim)                                                    \
          {                                                                   \
            break;                                                            \
          }                                                                   \
                                                                              \
          /* reset innermost loopers */                                       \
          for (dim--; dim > 0; dim--)                                         \
          {                                                                   \
            point[dim] = origin[dim];                                     \
          }                                                                   \
          dim = 1;                                                            \
        }                                                                     \
                                                                              \
        /* get the linear offset */                                           \
        idx = point[0];                                                       \
        for (i = 1; i < vdim; i++)                                            \
        {                                                                     \
          idx += point[i] * points_per_dim[i];                                \
        }                                                                     \
                                                                              \
        /* copy the data in lowest dimension */                               \
        /* if possible (no downsampling was requested) use plain memcpy()     \
           otherwise copy element-wise in a loop */                           \
        if (downsample[0] == 1)                                               \
        {                                                                     \
          memcpy (typed_hdata, typed_vdata + idx,                             \
                  dim0_elements * sizeof (*typed_hdata));                     \
          typed_hdata += dim0_elements;                                       \
        }                                                                     \
        else                                                                  \
        {                                                                     \
          for (i = 0; i < dim0_elements; i += downsample[0])                  \
          {                                                                   \
            *typed_hdata++ = typed_vdata[idx + i];                            \
          }                                                                   \
        }                                                                     \
                                                                              \
        if (vdim > 1)                                                         \
        {                                                                     \
          /* increment current looper */                                      \
          point[dim] += downsample[dim];                                      \
        }                                                                     \
        else                                                                  \
        {                                                                     \
          /* exit loop if hyperslab dim is only 1D */                         \
          break;                                                              \
        }                                                                     \
                                                                              \
      } /* end of nested loops over all dimensions */                         \
    }


/* routine to check parameters passed to the Hyperslab routines */
static const char *checkParameters (const cGH *GH, int vindex, int vtimelvl,
                                    int hdim,
                                    const int global_origin[/*vdim*/],
                                    const int directions[/*vdim*/],
                                    const int extents[/*hdim*/],
                                    const int downsample_[/*hdim*/],
                                    void **hdata,
                                    int hsize[/*hdim*/]);
#ifdef CCTK_MPI
static void SortIntoUnchunked (int hdim,
                               const int hsize[],
                               int totals_global,
                               const CCTK_INT sizes_global[],
                               void **hdata,
                               void *chunked_hdata,
                               int vtypesize,
                               int nprocs);
#endif  /* CCTK_MPI */


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
  @calls
  @calledby   Hyperslab_GetHyperslab
  @history 
  @endhistory 

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
  @var        global_origin
  @vdesc      global coordinates of the hyperslab origin
  @vtype      const int[dimensions of vindex]
  @vio        in
  @endvar
  @var        directions
  @vdesc      directions which span the hyperslab
              This is the direction vector for 1D hyperslabs,
              or the normal vector of the 2 lowest dimensions
              for hyperslabs of higher dimensions.
              If the hyperslab is of same dimensionality as the variable
              the directions vector is ignored.
  @vtype      const int[dimensions of vindex]
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
int Hyperslab_GetLocalHyperslab (const cGH *GH,
                                 int vindex,
                                 int vtimelvl,
                                 int hdim,
                                 const int global_origin[/*vdim*/],
                                 const int directions[/*vdim*/],
                                 const int extents[/*hdim*/],
                                 const int downsample_[/*hdim*/],
                                 void **hdata,
                                 int hsize[/*hdim*/],
                                 int hsize_global[/*hdim*/],
                                 int hoffset_global[/*hdim*/])
{
  int *point;                    /* looper over hyperslab dimensions */
  int *origin,               /* hyperslab's local start and endpoint */
      *endpoint;                 /* within the variable's grid dimensions */
  int *downsample;               /* the downsample_[] vector extended to vdim */
  int *my_global_origin,     /* hyperslab's global start and endpoint */
      *global_endpoint;
  int *points_per_dim;           /* points per subvolume */
  int *do_dir;                   /* directions in which to span the hyperslab */
  int stagger_index;             /* stagger index in direction i */
  int myproc;                    /* local processor ID */
  cGroup vinfo;                  /* variable's group info */
  int vdim;                      /* looper over all dimensions */
  int totals;                    /* total number of hyperslab data points */
  int retval;                    /* the return value (0 for success) */
  pGH *pughGH;                   /* pointer to the current pGH */
  pGA *GA;                       /* the variable's GA structure from PUGH */
  const char *errormsg;          /* explanation string in case of errors */


  /* do some plausibility checks */
  errormsg = checkParameters (GH, vindex, vtimelvl, hdim,
                              global_origin, directions, extents,
                              downsample_, hdata, hsize);

  /* immediately return in case of errors */
  if (errormsg)
  {
    CCTK_WARN (1, errormsg);
    return (-1);
  }

  /* set the default return code */
  retval = 0;

  /* check for zero-size hyperslab */
  totals = 1;
  for (vdim = 0; vdim < hdim; vdim++)
  {
    totals *= extents[vdim];
  }
  if (totals == 0)
  {
    /* initialize output parameters to all zeros */
    for (vdim = 0; vdim < hdim; vdim++)
    {
      hsize[vdim] = hsize_global[vdim] = 0;
      if (hoffset_global)
      {
        hoffset_global[vdim] = 0;
      }
    }
    *hdata = NULL;

    return (retval);
  }

  /* get the info on the variable to extract a hyperslab from */
  CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo);

  /* allocate the temporary arrays */
  point = (int *) malloc (8 * vinfo.dim * sizeof (int));
  origin           = point + 1*vinfo.dim;
  endpoint             = point + 2*vinfo.dim;
  my_global_origin = point + 3*vinfo.dim;
  global_endpoint      = point + 4*vinfo.dim;
  downsample           = point + 5*vinfo.dim;
  do_dir               = point + 6*vinfo.dim;
  points_per_dim       = point + 7*vinfo.dim;

  /* get the actual directions in which to spawn the hyperslab */
  /* Note: hdim == 1    the normal vector is the directions vector itself
           hdim <  vdim the normal vector is the negated directions vector
           hdim == vdim the normal vector is completely ignored,
                        and all directions are selected */
  if (hdim == 1)
  {
    memcpy (do_dir, directions, vinfo.dim * sizeof (int));
  }
  else if (hdim < vinfo.dim)
  {
    for (vdim = 0; vdim < vinfo.dim; vdim++)
    {
      do_dir[vdim] = ! directions[vdim];
    }
  }
  else
  {
    for (vdim = 0; vdim < vinfo.dim; vdim++)
    {
      do_dir[vdim] = 1;
    }
  }

  /* get the pGH pointer and the variable's GA structure */
  pughGH = PUGH_pGH (GH);
  GA     = (pGA *) pughGH->variables[vindex][vtimelvl];

  /* compute the global endpoint */
  for (vdim = hdim = 0; vdim < vinfo.dim; vdim++)
  {
    if (do_dir[vdim])
    {
      global_endpoint[vdim] = extents[hdim] > 0 ?
                               MIN (global_origin[vdim] + extents[hdim],
                                    GA->extras->nsize[vdim]) :
                               GA->extras->nsize[vdim];
      downsample[vdim] = downsample_[hdim];
      hdim++;
    }
    else
    {
      global_endpoint[vdim] = global_origin[vdim] + 1;
      downsample[vdim] = 1;
    }
  }

  /* get the local processor ID */
  myproc = CCTK_MyProc (GH);

  /* compute my global origin from the global ranges */
  for (vdim = 0; vdim < vinfo.dim; vdim++)
  {
    stagger_index = CCTK_StaggerDirIndex (vdim, vinfo.stagtype);

    if (global_origin[vdim] < MY_GLOBAL_EP (GA->extras, myproc,
                                                 stagger_index, vdim))
    {
      if (global_origin[vdim] < MY_GLOBAL_SP (GA->extras, myproc,
                                                   stagger_index, vdim))
      {
        int npoints;

        npoints = (MY_GLOBAL_SP (GA->extras, myproc, stagger_index, vdim)
                   - global_origin[vdim]) / downsample[vdim];
        if ((MY_GLOBAL_SP (GA->extras, myproc, stagger_index, vdim)
             - global_origin[vdim]) % downsample[vdim])
        {
          npoints++;
        }
        my_global_origin[vdim] = global_origin[vdim] +
                                      npoints*downsample[vdim];
      }
      else
      {
        my_global_origin[vdim] = global_origin[vdim];     
      }
    }
    else
    {
      my_global_origin[vdim] = -1;
    }
  }

  /* compute the local start- and endpoint from the global ranges */
  totals = 1;
  for (vdim = hdim = 0; vdim < vinfo.dim; vdim++)
  {
    stagger_index = CCTK_StaggerDirIndex (vdim, vinfo.stagtype);

    if (my_global_origin[vdim] >= 0 &&
        my_global_origin[vdim] <  MY_GLOBAL_EP (GA->extras, myproc,
                                                     stagger_index, vdim))
    {
      origin[vdim] = my_global_origin[vdim] -
                          GA->extras->lb[myproc][vdim];
    }
    else
    {
      origin[vdim] = -1;
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
            vdim, origin[vdim], endpoint[vdim]);
#endif

    if (endpoint[vdim] < 0 || origin[vdim] < 0)
    {
      totals = 0;
      endpoint[vdim] = origin[vdim];
    }

    if (do_dir[vdim])
    {
      /* compute the global and local size in each hyperslab dimension */
      hsize_global[hdim] = (global_endpoint[vdim] - global_origin[vdim]) /
                            downsample[vdim];
      if ((global_endpoint[vdim] - global_origin[vdim]) %
          downsample[vdim])
      {
        hsize_global[hdim]++;
      }
      if (GA->connectivity->perme[vdim])
      {
        hsize_global[hdim] -= 2 * GA->extras->nghostzones[vdim];
      }
      hsize[hdim] = (endpoint[vdim] - origin[vdim]) / downsample[vdim];
      if ((endpoint[vdim] - origin[vdim]) % downsample[vdim])
      {
        hsize[hdim]++;
      }
      totals *= hsize[hdim];
      hdim++;
    }
  }

#ifdef DEBUG
  printf ("total number of hyperslab data points: %d\n", totals);
#endif

  /* nested loop over vinfo.dim dimensions */
  /* NOTE: the following code assumes origin[vdim] < endpoint[vdim] */
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
          hoffset_global[hdim] = (my_global_origin[vdim] -
                            global_origin[vdim]) / downsample[vdim];
          if (GA->connectivity->perme[vdim])
          {
            hoffset_global[hdim] -= GA->extras->nghostzones[vdim];
          }
#ifdef DEBUG
          printf ("hoffset_global, hsize in direction %d: %d, %d\n",
                  hdim, hoffset_global[hdim], hsize[hdim]);
#endif
          hdim++;
        }
      }
    }

    /* allocate the buffer for the hyperslab data */
    *hdata = malloc (totals * CCTK_VarTypeSize (vinfo.vartype));

    /* compute the points_per_dim[] vector */
    /* NOTE: this could be computed at startup and kept in a GH extension
             once we have one for thorn Hyperslab */
    points_per_dim[0] = 1;
    for (vdim = 1; vdim < vinfo.dim; vdim++)
    {
      points_per_dim[vdim] = points_per_dim[vdim-1] *
                             GA->extras->lnsize[vdim-1];
    }

    /* now get the hyperslab data points using that wonderful macro... */
    switch (vinfo.vartype)
    {
      case CCTK_VARIABLE_CHAR:
        PICKUP_HYPERSLAB_DATA (CCTK_BYTE, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

      case CCTK_VARIABLE_INT:
        PICKUP_HYPERSLAB_DATA (CCTK_INT, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

      case CCTK_VARIABLE_REAL:
        PICKUP_HYPERSLAB_DATA (CCTK_REAL, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

      case CCTK_VARIABLE_COMPLEX:
        PICKUP_HYPERSLAB_DATA (CCTK_COMPLEX, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

#ifdef CCTK_INT2
      case CCTK_VARIABLE_INT2:
        PICKUP_HYPERSLAB_DATA (CCTK_INT2, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;
#endif

#ifdef CCTK_INT4
      case CCTK_VARIABLE_INT4:
        PICKUP_HYPERSLAB_DATA (CCTK_INT4, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;
#endif

#ifdef CCTK_INT8
      case CCTK_VARIABLE_INT8:
        PICKUP_HYPERSLAB_DATA (CCTK_INT8, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;
#endif

#ifdef CCTK_REAL4
      case CCTK_VARIABLE_REAL4:
        PICKUP_HYPERSLAB_DATA (CCTK_REAL4, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

      case CCTK_VARIABLE_COMPLEX8:
        PICKUP_HYPERSLAB_DATA (CCTK_COMPLEX8, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;
#endif

#ifdef CCTK_REAL8
      case CCTK_VARIABLE_REAL8:
        PICKUP_HYPERSLAB_DATA (CCTK_REAL8, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

      case CCTK_VARIABLE_COMPLEX16:
        PICKUP_HYPERSLAB_DATA (CCTK_COMPLEX16, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;
#endif

#ifdef CCTK_REAL16
      case CCTK_VARIABLE_REAL16:
        PICKUP_HYPERSLAB_DATA (CCTK_REAL16, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;

      case CCTK_VARIABLE_COMPLEX32:
        PICKUP_HYPERSLAB_DATA (CCTK_COMPLEX32, vinfo.dim, vdata, *hdata,
                               point, origin, endpoint, downsample,
                               points_per_dim);
        break;
#endif

      default:
        CCTK_WARN (1, "Unsupported variable type");
        retval = -1;
        break;
    }
  }
  else
  {
    /* clear out the return values if there's no hyperslab data */
    *hdata = NULL;
    memset (hsize, 0, hdim * sizeof (int));
  }

  /* free allocated temporary memory */
  free (point);

  return (retval);
}


/*@@
  @routine    Hyperslab_GetHyperslab
  @date       Fri May 12 2000
  @author     Thomas Radke
  @desc
              Extract a hyperslab from a Cactus array variable.
              All processor-local hyperslab data are collected
              into a single global buffer.
  @enddesc 
  @calls
  @calledby
  @history 
  @endhistory 

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
  @var        global_origin
  @vdesc      global coordinates of the hyperslab origin
  @vtype      const int[dimensions of vindex]
  @vio        in
  @endvar
  @var        directions
  @vdesc      directions which span the hyperslab
              This is the direction vector for 1D hyperslabs,
              or the normal vector of the 2 lowest dimensions
              for hyperslabs of higher dimensions.
              If the hyperslab is of same dimensionality as the variable
              the directions vector is ignored.
  @vtype      const int[dimensions of vindex]
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
  @var        hsize
  @vdesc      sizes of the hyperslab data buffer in each dimension
  @vtype      int[hdim]
  @vio        out
  @endvar
@@*/
int Hyperslab_GetHyperslab (const cGH *GH,
                            int target_proc,
                            int vindex,
                            int vtimelvl,
                            int hdim,
                            const int global_origin[/*vdim*/],
                            const int directions[/*vdim*/],
                            const int extents[/*hdim*/],
                            const int downsample_[/*hdim*/],
                            void **hdata,
                            int hsize[/*hdim*/])
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
  int vtypesize;              /* byte-size of a single data point */
  void *chunked_hdata;        /* receive buffer for all hyperslab chunks */
  int *displs, *recvcnts;     /* displacements/receive counts for gathering */
  CCTK_INT *sizes_local,      /* MPI buffers for communicating the */
           *sizes_global;     /* hyperslab sizes and offsets */
#endif


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

  /* do some plausibility checks */
  errormsg = checkParameters (GH, vindex, vtimelvl, hdim,
                              global_origin, directions, extents,
                              downsample_, hdata, hsize);

  /* FIXME: hack for getting diagonals from 3D variables */
  if (errormsg && ! strcmp (errormsg, "Given normal vector isn't axis-parallel"))
  {
    int mapping;
    CCTK_INT my_directions[3], my_global_origin[3];
    CCTK_INT my_extents[1], my_downsample_[1], my_hsize[1];


    if (hdim != 1 || vinfo.dim != 3)
    {
      CCTK_WARN (1, "Non-axis-parallel hyperslabs are supported as diagonals "
                    "from non-staggered 3D variables only");
      return (-1);
    }

    /* need to convert this into CCTK_INT vectors */
    my_directions[0] = directions[0];
    my_directions[1] = directions[1];
    my_directions[2] = directions[2];
    my_global_origin[0] = global_origin[0];
    my_global_origin[1] = global_origin[1];
    my_global_origin[2] = global_origin[2];
    my_extents[0] = extents[0];
    my_downsample_[0] = downsample_[0];

    mapping = Hyperslab_DefineGlobalMappingByIndex (GH, vindex, 1,my_directions,
                                                    my_global_origin,my_extents,
                                                    my_downsample_, -1,
                                                    target_proc, NULL,my_hsize);
    /* convert result back into int */
    hsize[0] = my_hsize[0];

    if (mapping < 0)
    {
      CCTK_WARN (1, "Failed to define hyperslab mapping for 3D diagonal");
      return (-1);
    }
    if (hsize[0] > 0)
    {
      *hdata = malloc (hsize[0] * CCTK_VarTypeSize (vinfo.vartype));
      retval = Hyperslab_Get (GH, mapping, vindex, 0, vinfo.vartype, *hdata);
    }
    else
    {
      *hdata = NULL;
      retval = 0;
    }
    Hyperslab_FreeMapping (mapping);

    return (retval);
  }

  /* immediately return in case of errors */
  if (errormsg)
  {
    CCTK_WARN (1, errormsg);
    return (-1);
  }

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
  retval = Hyperslab_GetLocalHyperslab (GH, vindex, vtimelvl, hdim,
                                        global_origin, directions, extents,
                                        downsample_, hdata_ptr, hsize_local,
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

  vtypesize = CCTK_VarTypeSize (vinfo.vartype);
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

    chunked_hdata = malloc (totals_global * vtypesize);
  }
  else
  {
    displs = recvcnts = NULL;
    chunked_hdata = NULL;
  }

  /* detect the MPI datatype to use */
  mpi_vtype = PUGH_MPIDataType (PUGH_pGH (GH), vinfo.vartype);

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
    SortIntoUnchunked (hdim, hsize, totals_global, sizes_global, hdata, chunked_hdata, vtypesize, nprocs);

    /* free allocated vectors for MPI communication */
    free (displs);
  }

  /* free allocated arrays for communicating the hyperslab sizes of offsets */
  free (sizes_local);
  free (hsize_local);

#endif /* CCTK_MPI */

  return (retval);
}


/********************** local routines ************************************/
#ifdef CCTK_MPI
static void SortIntoUnchunked (int hdim,
                               const int hsize[],
                               int totals_global,
                               const CCTK_INT sizes_global[],
                               void **hdata,
                               void *chunked_hdata,
                               int vtypesize,
                               int nprocs)
{
  int i, j, proc;
  int *point;
  int *points_per_hdim;
  char *copy_from, *copy_to;
  const CCTK_INT *hsize_chunk, *hoffset_chunk;
  int linear_hoffset;


  /* for hdim == 1 the hyperslab was already sorted by MPI_Gatherv()
     we just need to return that buffer */
  if (hdim == 1)
  {
    *hdata = chunked_hdata;
  }
  else
  {
    /* allocate temporary vectors */
    point = (int *) malloc (2 * hdim * sizeof (int));
    points_per_hdim = point + hdim;

    points_per_hdim[0] = 1;
    for (i = 1; i < hdim; i++)
    {
      points_per_hdim[i] = points_per_hdim[i-1] * hsize[i-1];
    }

    /* allocate buffer for the returned hyperslab */
    *hdata = malloc (totals_global * vtypesize);

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

      /* set origin to zero */
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
        memcpy (copy_to + linear_hoffset * vtypesize,
                copy_from, hsize_chunk[0] * vtypesize);
        copy_from += hsize_chunk[0] * vtypesize;

        /* increment current looper */
        point[i]++;

      } /* end of nested loops over all dimensions */
    } /* end of loop over all processors */

    /* free allocated temporary vectors and the chunk buffer */
    free (chunked_hdata);
    free (point);
  }
}
#endif  /* CCTK_MPI */


/********************** local routines ************************************/
static const char *checkParameters (const cGH *GH, int vindex, int vtimelvl,
                                    int hdim,
                                    const int global_origin[/*vdim*/],
                                    const int directions[/*vdim*/],
                                    const int extents[/*hdim*/],
                                    const int downsample_[/*hdim*/],
                                    void **hdata,
                                    int hsize[/*hdim*/])
{
  int dim;                       /* looper */
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
  if (! global_origin || ! directions || ! extents || ! downsample_ ||
      ! hdata || ! hsize)
  {
    return ("NULL pointer(s) passed as parameters");
  }

  /* check the downsample parameter */
  for (dim = 0; dim < hdim; dim++)
  {
    if (downsample_[dim] <= 0)
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
  if (hdim < 0 || hdim > vinfo.dim)
  {
    return ("Invalid hyperslab dimension");
  }

  /* check the direction(s) of the hyperslab
     if it is less-dimensional than the variable */
  if (hdim != vinfo.dim)
  {
    for (dim = 0, num_directions = 0; dim < vinfo.dim; dim++)
    {
      if (directions[dim])
      {
        num_directions++;
      }
    }
    if (num_directions == 0)
    {
      return ("Given normal vector is a null vector");
    }
    if (num_directions > 1)
    {
      return ("Given normal vector isn't axis-parallel");
    }
  }

  /* check if PUGH is active */
  if (! PUGH_pGH (GH))
  {
    return ("No GH extension for PUGH found. Did you activate thorn PUGH ?");
  }

  return (NULL);
}
