#ifdef SPEC_CPU
# define THORN_IS_PUGHSlab
#endif /* SPEC_CPU */
 /*@@
   @file      Mapping.c
   @date      Sun 2 Dec 2001
   @author    Thomas Radke
   @desc
              Routines to define hyperslab mappings
   @enddesc
   @version   $Id: Mapping.c,v 1.4 2001/12/18 23:46:22 tradke Exp $
 @@*/


#include <stdlib.h>
#include <string.h>

#include "cctk.h"

#include "pugh.h"

#include "PUGHSlab.h"
#include "PUGHSlabi.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGHSlab/src/Mapping.c,v 1.4 2001/12/18 23:46:22 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGHSlab_Mapping_c)

/********************************************************************
 ********************    Macro Definitions   ************************
 ********************************************************************/
/* define this if you want debugging output */
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


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int nmapping_list = 0;
static hslab_mapping_t *mapping_list = NULL;

static int IsFullHyperslab (const pGA *GA,
                            const CCTK_INT *origin,
                            const CCTK_INT *extent,
                            const hslab_mapping_t *mapping);


CCTK_INT Hyperslab_DefineGlobalMappingByIndex (
           const cGH *GH,
           CCTK_INT vindex,
           CCTK_INT dim,
           const CCTK_INT *direction  /* vdim*dim */,
           const CCTK_INT *origin     /* vdim */,
           const CCTK_INT *extent     /* dim */,
           const CCTK_INT *downsample /* dim */,
           CCTK_INT table_handle,
           CCTK_INT target_proc,
           t_hslabConversionFn conversion_fn,
           CCTK_INT *hsize            /* dim */)
{
  unsigned int vdim, hdim, num_dirs;
  int retval;
  int stagger_index;
  int myproc;
  int npoints;
  hslab_mapping_t *mapping;
  const char *error_msg;
  const pGH *pughGH;              /* pointer to the current pGH */
  const pGA *GA;                  /* the variable's GA structure from PUGH */
  cGroup vinfo;


  /* PUGHSlab doesn't use table information */
  if (table_handle >= 0)
  {
    CCTK_WARN (1, "Hyperslab_DefineGlobalMappingByIndex: table information is "
                  "ignored");
  }

  /* check parameter consistency */
  retval = 0;
  error_msg = NULL;
  if (CCTK_GroupData (CCTK_GroupIndexFromVarI (vindex), &vinfo) < 0)
  {
    error_msg = "invalid variable index given";
    retval = -1;
  }
  else if (vinfo.grouptype != CCTK_GF && vinfo.grouptype != CCTK_ARRAY)
  {
    error_msg = "invalid variable group type given "
                "(not a CCTK_GF or CCTK_ARRAY type)";
    retval = -2;
  }
  else if (dim < 0 || dim > vinfo.dim)
  {
    error_msg = "invalid hyperslab dimension given";
    retval = -2;
  }
  else if (! direction || ! origin || ! extent || ! hsize)
  {
    error_msg = "NULL pointer(s) passed for direction/origin/extent/hsize "
                "parameters";
    retval = -3;
  }
  else if (target_proc >= CCTK_nProcs (GH))
  {
    error_msg = "invalid target procesor ID given";
    retval = -4;
  }
  else if ((pughGH = (const pGH *) PUGH_pGH (GH)) == NULL)
  {
    error_msg = "no PUGH GH extension registered (PUGH not activated ?)";
    retval = -4;
  }
  else
  {
    for (vdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
    {
      retval |= origin[vdim] < 0;
      if (vdim < (unsigned int) dim)
      {
        retval |= extent[vdim] <= 0;
        if (downsample)
        {
          retval |= downsample[vdim] <= 0;
        }
      }
    }
    if (retval)
    {
      error_msg = "invalid hyperslab origin/extent/downsample vectors given";
      retval = -5;
    }
  }
  if (! retval)
  {
    mapping = (hslab_mapping_t *) malloc (sizeof (hslab_mapping_t));
    if (mapping)
    {
      mapping->vectors = (int *) malloc ((6*vinfo.dim + 2*dim) * sizeof (int));
    }
    if (mapping == NULL || mapping->vectors == NULL)
    {
      if (mapping)
      {
        free (mapping);
      }
      error_msg = "couldn't allocate hyperslab mapping structure";
      retval = -6;
    }
  }

  /* return in case of errors */
  if (retval)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Hyperslab_DefineGlobalMappingByIndex: %s", error_msg);
    return (retval);
  }

  mapping->hdim = (unsigned int) dim;
  mapping->vinfo = vinfo;
  mapping->target_proc = target_proc;
  mapping->conversion_fn = conversion_fn;

  /* assign memory for the other vectors */
  mapping->local_startpoint  = mapping->vectors + 0*vinfo.dim;
  mapping->local_endpoint    = mapping->vectors + 1*vinfo.dim;
  mapping->global_startpoint = mapping->vectors + 2*vinfo.dim;
  mapping->global_endpoint   = mapping->vectors + 3*vinfo.dim;
  mapping->do_dir            = mapping->vectors + 4*vinfo.dim;
  mapping->downsample        = mapping->vectors + 5*vinfo.dim;
  mapping->local_hsize       = mapping->vectors + 6*vinfo.dim + 0*dim;
  mapping->global_hsize      = mapping->vectors + 6*vinfo.dim + 1*dim;

  /* check direction vectors */
  for (hdim = 0; hdim < mapping->hdim; hdim++)
  {
    num_dirs = 0;
    for (vdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
    {
      if (direction[hdim*vinfo.dim + vdim])
      {
        num_dirs++;
      }
    }
    if (num_dirs == 0)
    {
      free (mapping->vectors); free (mapping);
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Hyperslab_DefineGlobalMappingByIndex: %d-direction vector "
                  "is a null vector", hdim);
      return (-7);
    }

    mapping->is_diagonal_in_3D = num_dirs == 3 && mapping->hdim == 1;
    if (num_dirs != 1 && ! mapping->is_diagonal_in_3D)
    {
      free (mapping->vectors); free (mapping);
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Hyperslab_DefineGlobalMappingByIndex: %d-direction vector "
                  "isn't axis-orthogonal", hdim);
      return (-7);
    }
  }

  /* diagonals can be extracted from non-staggered 3D variables only */ 
  if (mapping->is_diagonal_in_3D && vinfo.stagtype != 0)
  {
    free (mapping->vectors); free (mapping);
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Hyperslab_DefineGlobalMappingByIndex: diagonals can be "
                "extracted from non-staggered 3D variables only");
    return (-7);
  }

  for (vdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
  {
    mapping->do_dir[vdim] = 0;
    for (hdim = 0; hdim < mapping->hdim; hdim++)
    {
      if (direction[hdim*vinfo.dim + vdim])
      {
        mapping->do_dir[vdim]++;
      }
    }
    if (mapping->do_dir[vdim] > 1)
    {
      free (mapping->vectors); free (mapping);
      CCTK_WARN (1, "Hyperslab_DefineGlobalMappingByIndex: duplicate direction "
                    "vectors given");
      return (-8);
    }
  }

  /* get the pGH pointer and the variable's GA structure */
  GA     = (const pGA *) pughGH->variables[vindex][0];
  myproc = CCTK_MyProc (GH);

  /* check extent */
  for (vdim = hdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
  {
    if (mapping->do_dir[vdim] && hdim < mapping->hdim)
    {
      if (origin[vdim] + extent[hdim] > GA->extras->nsize[vdim])
      {
        free (mapping->vectors); free (mapping);
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Hyperslab_DefineGlobalMappingByIndex: extent in "
                    "%d-direction exceeds grid size", hdim);
        return (-8);
      }
      hdim++;
    }
    else if (mapping->is_diagonal_in_3D &&
             origin[vdim] + extent[0] > GA->extras->nsize[vdim])
    {
      free (mapping->vectors); free (mapping);
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Hyperslab_DefineGlobalMappingByIndex: extent in "
                  "%d-direction exceeds grid size", vdim);
      return (-8);
    }
  }

  /* now fill out the hyperslab mapping structure */
  for (vdim = hdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
  {
    mapping->downsample[vdim] = 1;

    if (mapping->do_dir[vdim] && hdim < mapping->hdim)
    {
      if (downsample)
      {
        mapping->downsample[vdim] = downsample[hdim];
      }
      mapping->global_hsize[hdim] = extent[hdim] / mapping->downsample[vdim];
      if (extent[hdim] % mapping->downsample[vdim])
      {
        mapping->global_hsize[hdim]++;
      }
      /* subtract ghostzones for periodic BC */
      if (GA->connectivity->perme[vdim])
      {
        mapping->global_hsize[hdim] -= 2 * GA->extras->nghostzones[vdim];
      }
      hdim++;
    }
    else if (mapping->is_diagonal_in_3D)
    {
      mapping->totals = extent[0] / mapping->downsample[0];
      if (extent[0] % mapping->downsample[0])
      {
        mapping->totals++;
      }
      /* subtract ghostzones for periodic BC */
      if (GA->connectivity->perme[vdim])
      {
        mapping->totals -= 2 * GA->extras->nghostzones[vdim];
      }
      if ((unsigned int) mapping->global_hsize[0] > mapping->totals)
      {
        mapping->global_hsize[0] = mapping->totals;
      }
    }
  }

  /* check whether the full local data patch was requested as hyperslab */
  mapping->is_full_hyperslab = IsFullHyperslab (GA, origin, extent, mapping);
  if (mapping->is_full_hyperslab)
  {
    memset (mapping->local_startpoint, 0, vinfo.dim * sizeof (int));
    memcpy (mapping->local_endpoint, GA->extras->lnsize, vinfo.dim*sizeof(int));
    mapping->totals = GA->extras->npoints;
  }
  else if (mapping->is_diagonal_in_3D)
  {
    /* just initialize the downsample and global_startpoint vectors */
    for (vdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
    {
      mapping->downsample[vdim] = mapping->downsample[0];
      mapping->global_startpoint[vdim] = origin[vdim];
    }
  }
  else
  {
    /* compute the global endpoint */
    for (vdim = hdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
    {
      mapping->global_endpoint[vdim] = origin[vdim] +
                                   (mapping->do_dir[vdim] ? extent[hdim++] : 1);
    }

    /* compute this processor's global startpoint from the global ranges */
    for (vdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
    {
      stagger_index = CCTK_StaggerDirIndex (vdim, vinfo.stagtype);

      if (origin[vdim] < MY_GLOBAL_EP (GA->extras, myproc, stagger_index, vdim))
      {
        mapping->global_startpoint[vdim] = origin[vdim];
        if (origin[vdim] < MY_GLOBAL_SP (GA->extras, myproc,stagger_index,vdim))
        {
          npoints = (MY_GLOBAL_SP (GA->extras, myproc, stagger_index, vdim)
                     - origin[vdim]) / mapping->downsample[vdim];
          if ((MY_GLOBAL_SP (GA->extras, myproc, stagger_index, vdim)
               - origin[vdim]) % mapping->downsample[vdim])
          {
            npoints++;
          }
          mapping->global_startpoint[vdim] += npoints*mapping->downsample[vdim];
        }
      }
      else
      {
        mapping->global_startpoint[vdim] = -1;
      }
    }

    /* compute the local start- and endpoint from the global ranges */
    mapping->totals = 1;
    for (vdim = hdim = 0; vdim < (unsigned int) vinfo.dim; vdim++)
    {
      stagger_index = CCTK_StaggerDirIndex (vdim, vinfo.stagtype);

      if (mapping->global_startpoint[vdim] >= 0 &&
          mapping->global_startpoint[vdim] <  MY_GLOBAL_EP (GA->extras, myproc,
                                                            stagger_index,vdim))
      {
        mapping->local_startpoint[vdim] = mapping->global_startpoint[vdim] -
                                          GA->extras->lb[myproc][vdim];
      }
      else
      {
        mapping->local_startpoint[vdim] = -1;
      }

      if (mapping->global_endpoint[vdim] > MY_GLOBAL_SP (GA->extras, myproc,
                                                         stagger_index, vdim))
      {
        mapping->local_endpoint[vdim] =
          MIN (MY_LOCAL_EP (GA->extras, stagger_index, vdim),
                            mapping->global_endpoint[vdim] -
                            GA->extras->lb[myproc][vdim]);
      }
      else
      {
        mapping->local_endpoint[vdim] = -1;
      }

#ifdef DEBUG
      printf ("direction %d: global ranges [%d, %d), local ranges[%d, %d)\n",
              vdim,
              mapping->global_startpoint[vdim], mapping->global_endpoint[vdim],
              mapping->local_startpoint[vdim], mapping->local_endpoint[vdim]);
#endif

      if (mapping->local_endpoint[vdim] < 0 ||
          mapping->local_startpoint[vdim] < 0)
      {
        mapping->totals = 0;
        mapping->local_endpoint[vdim] = mapping->local_startpoint[vdim];
      }

      if (mapping->do_dir[vdim])
      {
        /* compute the local size in each hyperslab dimension */
        mapping->local_hsize[hdim] = (mapping->local_endpoint[vdim] -
                                   mapping->local_startpoint[vdim]) /
                                  mapping->downsample[vdim];
        if ((mapping->local_endpoint[vdim] - mapping->local_startpoint[vdim]) %
            mapping->downsample[vdim])
        {
          mapping->local_hsize[hdim]++;
        }
        mapping->totals *= mapping->local_hsize[hdim];
        hdim++;
      }
    }
  } /* end of else branch for 'if (mapping->is_full_hyperslab)' */

#ifdef DEBUG
  printf ("total number of hyperslab data points: %d\n", mapping->totals);
#endif

#if 0
  if (mapping->totals > 0)
  {
    /* if requested, compute the offsets into the global hyperslab */
    if (hoffset_global)
    {
      for (i = hdim = 0; i < vinfo.dim; i++)
      {
        if (mapping->do_dir[i])
        {
          if (mapping->is_full_hyperslab)
          {
            hoffset_global[hdim] = GA->extras->lb[myproc][i];
          }
          else
          {
            hoffset_global[hdim] = (mapping->global_startpoint[i] -
                                    origin[i]) / mapping->downsample[i];
            if (GA->connectivity->perme[i])
            {
              hoffset_global[hdim] -= GA->extras->nghostzones[i];
            }
          }
#ifdef DEBUG
          printf ("hoffset_global, hsize in direction %d: %d, %d\n",
                  hdim, hoffset_global[hdim], mapping->local_hsize[hdim]);
#endif
          hdim++;
        }
      }
    }
  }
#endif

  /* add this mapping to the mapping list */
  if (mapping_list)
  {
    mapping_list->prev = mapping;
  }
  mapping->prev = NULL;
  mapping->next = mapping_list;
  mapping_list = mapping;

  mapping->handle = nmapping_list++;

  /* set the global hsize in the return arguments */
  if (hsize)
  {
    for (hdim = 0; hdim < mapping->hdim; hdim++)
    {
      hsize[hdim] = mapping->global_hsize[hdim];
    }
  }

  return (mapping->handle);
}


CCTK_INT Hyperslab_FreeMapping (CCTK_INT mapping_handle)
{
  hslab_mapping_t *mapping;


  mapping = mapping_list;
  while (mapping)
  {
    if (mapping->handle == mapping_handle)
    {
      if (mapping == mapping_list)
      {
        mapping_list = mapping_list->next;
      }
      else
      {
        if (mapping->next)
        {
          mapping->next->prev = mapping->prev;
        }
        if (mapping->prev)
        {
          mapping->prev->next = mapping->next;
        }
      }
      free (mapping->vectors);
      free (mapping);
      return (0);
    }
    mapping = mapping->next;
  }

  return (-1);
}


hslab_mapping_t *PUGHSlabi_GetMapping (int mapping_handle)
{
  hslab_mapping_t *mapping;


  mapping = mapping_list;
  while (mapping && mapping->handle != mapping_handle)
  {
    mapping = mapping->next;
  }

  return (mapping);
}


static int IsFullHyperslab (const pGA *GA,
                            const CCTK_INT *origin,
                            const CCTK_INT *extent,
                            const hslab_mapping_t *mapping)
{
  int i, retval;


  retval = mapping->hdim == (unsigned int) GA->extras->dim;
  if (retval)
  {
    for (i = 0; i < GA->extras->dim; i++)
    {
      retval &= (origin[i] == 0);
      retval &= (extent[i] <= 0);
      retval &= (mapping->downsample[i] <= 1);
      retval &= (GA->connectivity->perme[i] == 0);
    }
  }

  return (retval);
}
