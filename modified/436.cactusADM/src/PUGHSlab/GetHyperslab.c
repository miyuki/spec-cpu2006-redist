#ifdef SPEC_CPU
# define THORN_IS_PUGHSlab
#endif /* SPEC_CPU */
 /*@@
   @file      GetHyperslab.c
   @date      Sun 2 Dec 2001
   @author    Thomas Radke
   @desc
              Routines to extract hyperslabs from CCTK array variables
   @enddesc
   @version   $Id: GetHyperslab.c,v 1.3 2001/12/16 21:43:05 tradke Exp $
 @@*/


#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"

#include "pugh.h"
#include "PUGHSlab.h"
#include "PUGHSlabi.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGHSlab/src/GetHyperslab.c,v 1.3 2001/12/16 21:43:05 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGHSlab_GetHyperslab_c)

/********************************************************************
 ********************    Macro Definitions   ************************
 ********************************************************************/
/* define this if you want debugging output */
/* #define DEBUG 1 */


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int GetLocalHyperslab (const cGH *GH,
                              const hslab_mapping_t *mapping,
                              int vindex,
                              int timelevel,
                              int hdatatype,
                              void *hdata);
static int GetDiagonalFromFrom3D (const cGH *GH,
                                  const hslab_mapping_t *mapping,
                                  int vindex,
                                  int timelevel,
                                  int hdatatype,
                                  void *hdata);


CCTK_INT Hyperslab_Get (const cGH *GH,
                        CCTK_INT   mapping_handle,
                        CCTK_INT   vindex,
                        CCTK_INT   timelevel,
                        CCTK_INT   hdatatype,
                        void      *hdata)
{
  int retval;
  hslab_mapping_t *mapping;


  mapping = PUGHSlabi_GetMapping (mapping_handle);
  if (mapping == NULL)
  {
    return (-1);
  }

  /* check mapping consistency */
  /*** FIXME ***/

  /* get the processor-local hyperslab */
  retval = GetLocalHyperslab (GH, mapping, vindex, timelevel, hdatatype, hdata);

  return (retval);
}


CCTK_INT Hyperslab_GetList (const cGH *GH,
                            CCTK_INT mapping_handle,
                            CCTK_INT num_arrays,
                            const CCTK_INT *vindices   /* num_arrays */,
                            const CCTK_INT *timelevels /* num_arrays */,
                            const CCTK_INT *hdatatypes /* num_arrays */,
                            void *const *hdata         /* num_arrays */)
{
  int i, retval;


  retval = 0;
  for (i = 0; i < num_arrays; i++)
  {
    if (Hyperslab_Get (GH, mapping_handle, vindices[i],
                       timelevels ? timelevels[i] : 0,
                       hdatatypes ? hdatatypes[i] : -1,
                       hdata[i]) == 0)
    {
      retval++;
    }
  }

  return (retval);
}

/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
/*@@
  @routine    GetLocalHyperslab
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

  @calls      PUGHSlab_GetDatatypeConversionFn

  @var        GH
  @vdesc      Pointer to CCTK grid hierarchy
  @vtype      cGH *
  @vio        in
  @endvar
  @var        vindex
  @vdesc      index of variable to get a hyperslab from
  @vtype      int
  @vio        in
  @endvar
  @var        timelevel
  @vdesc      timelvl of variable to get a hyperslab from
  @vtype      int
  @vio        in
  @endvar
  @var        hdim
  @vdesc      dimensionality of the requested hyperslab
  @vtype      int
  @vio        in
  @endvar
  @var        hdatatype
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
  @var        downsample
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
static int GetLocalHyperslab (const cGH *GH,
                              const hslab_mapping_t *mapping,
                              int vindex,
                              int timelevel,
                              int hdatatype,
                              void *hdata)
{
  int *point;                    /* looper over hyperslab dimensions */
  int *startpoint,               /* hyperslab's local start and endpoint */
      *endpoint;                 /* within the variable's grid dimensions */
  int *downsample;               /* the downsample[] vector extended to vdim */
  int *points_per_dim;           /* points per subvolume */
  int myproc;                    /* local processor ID */
  int i;                         /* general looper */
  int vdim;                      /* looper over all source dimensions */
  int vdata_size,                /* size of one data point in bytes for */
      hdata_size;                /* source and hyperslab data */
  int dim0_points;               /* number of hyperslab points in dim 0 */
  int dim0_hsize;                /* byte size of hyperslab points in dim 0 */
  const char *typed_vdata;             /* byte pointers into source and */
  char *typed_hdata;             /* hyperslab data arrays */
  const void *vdata;
  int retval;                    /* the return value (0 for success) */
  cGroup vinfo;                  /* variable's group info */
  pGH *pughGH;                   /* pointer to the current pGH */
  pGA *GA;                       /* the variable's GA structure from PUGH */
  const char *errormsg;          /* error message string */
  t_hslabConversionFn conversion_fn;


  /* do some plausibility checks */
  errormsg = NULL;
  if (! GH || ! mapping || (mapping->totals > 0 && ! hdata))
  {
    errormsg = "NULL pointer(s) passed for GH/mapping/hdata arguments";
  }
  else if (CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo) < 0)
  {
    errormsg = "Invalid variable index given";
  }
  else if (timelevel < 0 || timelevel >= vinfo.numtimelevels)
  {
    errormsg = "Invalid timelevel given";
  }
  else if (vinfo.grouptype != mapping->vinfo.grouptype ||
           vinfo.disttype  != mapping->vinfo.disttype ||
           vinfo.dim       != mapping->vinfo.dim ||
           vinfo.stagtype  != mapping->vinfo.stagtype)
  {
    errormsg = "Group data for template variable in mapping and given variable "
               "don't match";
  }

  /* immediately return in case of errors */
  if (errormsg)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "GetLocalHyperslab: %s", errormsg);
    return (-1);
  }

  /* check if there's any data to extract */
  if (mapping->totals == 0)
  {
    return (0);
  }

  /* diagonals from 3D variables are treated special */
  if (mapping->is_diagonal_in_3D)
  {
    retval = GetDiagonalFromFrom3D (GH, mapping, vindex, timelevel, hdatatype,
                                    hdata);
    return (retval);
  }

  /* if datatype conversion was requested
     get the appropriate predefined datatype conversion routine
     in case the user didn't supply one by her own */
  if (hdatatype < 0)
  {
    hdatatype = vinfo.vartype;
  }
  conversion_fn = mapping->conversion_fn;
  if (vinfo.vartype != hdatatype)
  {
    if (conversion_fn == NULL)
    {
      conversion_fn = PUGHSlabi_GetDatatypeConversionFn (vinfo.vartype,
                                                         hdatatype);
      if (! conversion_fn)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "No predefined PUGHSlab routine available to convert "
                    "'%s' into '%s'", CCTK_VarTypeName (vinfo.vartype),
                    CCTK_VarTypeName (hdatatype));
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

  /* allocate the temporary arrays */
  point = (int *) malloc (5 * vinfo.dim * sizeof (int));
  startpoint           = point + 1*vinfo.dim;
  endpoint             = point + 2*vinfo.dim;
  downsample           = point + 3*vinfo.dim;
  points_per_dim       = point + 4*vinfo.dim;

  memcpy (startpoint, mapping->local_startpoint, vinfo.dim * sizeof (int));
  memcpy (endpoint, mapping->local_endpoint, vinfo.dim * sizeof (int));
  memcpy (downsample, mapping->downsample, vinfo.dim * sizeof (int));

  /* get the pGH pointer and the variable's GA structure */
  pughGH = PUGH_pGH (GH);
  GA     = (pGA *) pughGH->variables[vindex][timelevel];

  /* get the local processor ID */
  myproc = CCTK_MyProc (GH);

  /* nested loop over vinfo.dim dimensions */
  /* NOTE: the following code assumes startpoint[vdim] < endpoint[vdim] */
  vdata = CCTK_VarDataPtrI (GH, timelevel, vindex);

  if (mapping->is_full_hyperslab && conversion_fn == NULL)
  {
    memcpy (hdata, vdata, mapping->totals * CCTK_VarTypeSize (vinfo.vartype));
  }
  else
  {
    /* get the byte size of a single data point
       in the variable and hyperslab data array */
    vdata_size = CCTK_VarTypeSize (vinfo.vartype);
    hdata_size = CCTK_VarTypeSize (hdatatype);

    typed_hdata = (char *) hdata;

    /* compute the points_per_dim[] vector */
    /* NOTE: this could be computed at startup and kept in a GH extension
             once we have one for thorn PUGHSlab */
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
      typed_vdata = (const char *) vdata + point[0];
#if 0
fprintf (stderr, "***** base vdata %p offset %d '%s'\n", vdata, point[0], CCTK_FullName (vindex));
#endif
      for (i = 1; i < vinfo.dim; i++)
      {
        typed_vdata += point[i] * points_per_dim[i];
      }

      /* copy the data in lowest dimension: if possible copy all data points
         in a row otherwise do it one by one */
      if (downsample[0] == vdata_size)
      {
        if (mapping->conversion_fn)
        {
          mapping->conversion_fn (typed_vdata, typed_hdata, dim0_points, 1,1);
        }
        else
        {
#if 0
fprintf (stderr, "***** copying %d bytes from %p tp %p\n", dim0_hsize, typed_vdata, typed_hdata);
#endif
          memcpy (typed_hdata, typed_vdata, dim0_hsize);
        }
      }
      else
      {
        if (mapping->conversion_fn)
        {
          mapping->conversion_fn (typed_vdata, typed_hdata, dim0_points,
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

  /* free allocated temporary memory */
  free (point);

  return (0);
}


static int GetDiagonalFromFrom3D (const cGH *GH,
                                  const hslab_mapping_t *mapping,
                                  int vindex,
                                  int timelevel,
                                  int hdatatype,
                                  void *hdata)
{
  int i, j, k, myproc, nprocs, linear_idx;
  CCTK_INT local_npoints, npoints;
  int vdatatype, htypesize, vtypesize;
  const char *vdata;
  char *local_hdata;
  const pGH *pughGH;
  const pGExtras *extras;
#ifdef CCTK_MPI
  CCTK_INT *global_npoints;
  int *recvcnts, *displs;
  MPI_Datatype mpidatatype;
#endif



  /* get the pGH pointer and the variable's GA structure */
  pughGH = PUGH_pGH (GH);
  extras = ((const pGA *) pughGH->variables[vindex][timelevel])->extras;

  vdatatype = CCTK_VarTypeI (vindex);
  if (hdatatype < 0)
  {
    hdatatype = vdatatype;
  }
  htypesize = CCTK_VarTypeSize (hdatatype);
  vtypesize = CCTK_VarTypeSize (vdatatype);
  vdata = (const char *) CCTK_VarDataPtrI (GH, timelevel, vindex);

  myproc = CCTK_MyProc (GH);
  nprocs = CCTK_nProcs (GH);
  if (nprocs == 1)
  {
    local_hdata = (char *) hdata;
  }
  else
  {
    local_hdata = (char *) malloc (mapping->global_hsize[0] * htypesize);
  }

  i = mapping->global_startpoint[0] - extras->lb[myproc][0];
  j = mapping->global_startpoint[1] - extras->lb[myproc][1];
  k = mapping->global_startpoint[2] - extras->lb[myproc][2];
  local_npoints = 0;
  for (npoints = 0; npoints < mapping->global_hsize[0]; npoints++)
  {
    if (i >= extras->ownership[0][0][0] && i < extras->ownership[0][1][0] &&
        j >= extras->ownership[0][0][1] && j < extras->ownership[0][1][1] &&
        k >= extras->ownership[0][0][2] && k < extras->ownership[0][1][2])
    {
      linear_idx = i + j*extras->hyper_volume[1] + k*extras->hyper_volume[2];
      if (vdatatype != hdatatype)
      {
        mapping->conversion_fn (vdata + linear_idx*vtypesize, local_hdata,
                                1, 1, 1);
      }
      else
      {
        memcpy (local_hdata, vdata + linear_idx*vtypesize, htypesize);
      }
      local_hdata += htypesize;
      local_npoints++;
    }
    i += mapping->downsample[0];
    j += mapping->downsample[1];
    k += mapping->downsample[2];
  }
  local_hdata -= local_npoints * htypesize;

#ifdef CCTK_MPI
  if (nprocs > 1)
  {
    /* allocate communication buffers */
    myproc = CCTK_MyProc (GH);
    if (mapping->target_proc < 0 || mapping->target_proc == myproc)
    {
      global_npoints = (CCTK_INT *) malloc (nprocs * sizeof (CCTK_INT));
      recvcnts = (int *) malloc (2 * nprocs * sizeof (int));
      displs   = recvcnts + nprocs;
    }
    else
    {
      global_npoints = NULL; recvcnts = displs = NULL; hdata = NULL;
    }

    /* gather the number of local points on each processor */
    if (mapping->target_proc < 0)
    {
      CACTUS_MPI_ERROR (MPI_Allgather (&local_npoints, 1, PUGH_MPI_INT,
                                       global_npoints, 1, PUGH_MPI_INT,
                                       pughGH->PUGH_COMM_WORLD));
    }
    else
    {
      CACTUS_MPI_ERROR (MPI_Gather (&local_npoints, 1, PUGH_MPI_INT,
                                    global_npoints, 1, PUGH_MPI_INT,
                                    mapping->target_proc,
                                    pughGH->PUGH_COMM_WORLD));
    }

    /* compute the receive count and displacement vectors */
    if (mapping->target_proc < 0 || mapping->target_proc == myproc)
    {
      for (i = 0; i < nprocs; i++)
      {
        recvcnts[i] = (int) global_npoints[i];
        displs[i] = i == 0 ? 0 : displs[i-1] + recvcnts[i-1];
      }
    }

    /* get the MPI datatype for the hyperslab data */
    mpidatatype = PUGH_MPIDataType (pughGH, hdatatype);

    /* gather the local hyperslab data from each processor */
    if (mapping->target_proc < 0)
    {
      CACTUS_MPI_ERROR (MPI_Allgatherv (local_hdata, local_npoints,
                                        mpidatatype, hdata, recvcnts, displs,
                                        mpidatatype, pughGH->PUGH_COMM_WORLD));
    }
    else
    {
      CACTUS_MPI_ERROR (MPI_Gatherv (local_hdata, local_npoints,
                                     mpidatatype, hdata, recvcnts, displs,
                                     mpidatatype, mapping->target_proc,
                                     pughGH->PUGH_COMM_WORLD));
    }

    /* free communication buffers */
    if (mapping->target_proc < 0 || mapping->target_proc == myproc)
    {
      free (global_npoints);
      free (recvcnts);
    }
  }
#endif  /* CCTK_MPI */

  /* free allocated resources */
  if (nprocs > 1)
  {
    free (local_hdata);
  }

  return (0);
}
