#ifdef SPEC_CPU
# define THORN_IS_PUGHReduce
#endif /* SPEC_CPU */
 /*@@
   @file      ReductionNormInf.c
   @date      Sat Aug 25, 2001
   @author    Thomas Radke, Paul Walker, Erik Schnetter
   @desc 
              Defines the PUGH reduction operator to get the norm
              of an arbitrary array which is defined as:
                  norm_inf = $\max |a_i| $
   @enddesc 
   @version   $Id: ReductionNormInf.c,v 1.2 2001/11/05 15:04:13 tradke Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "pugh_reductions.h"

static const char *rcsid = "$Header: /cactus/CactusPUGH/PUGHReduce/src/ReductionNormInf.c,v 1.2 2001/11/05 15:04:13 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGHReduce_ReductionNormInf_c)

/* local function prototypes */
static int PUGH_ReductionNormInf (cGH *GH,
                                  int proc,
                                  int num_dims,
                                  int from[/* dim */],
                                  int to[/* dim */],
                                  int iterator[/* dim */],
                                  int points_per_dim[/* dim */],
                                  int num_points,
                                  int num_inarrays,
                                  int intypes[/* num_inarrays */],
                                  void *inarrays[/* num_inarrays */],
                                  int num_outvals,
                                  CCTK_REAL outvals[/*num_inarrays*num_outvals*/]);


/*@@
  @routine PUGH_ReductionNormInfArrays
  @author  Thomas Radke
  @date    19 Aug 1999
  @desc
           Registered PUGH reduction routine for computing the "norm_inf"
           of a set of arrays.
           The arrays are described by their dimensions and variable type.
           If the number of requested output values equals the number of
           elements in an input array, global reduction is done element-wise.
           Otherwise num_outvals must be 1, and global reduction is done on the
           results of the local reductions.
           Type casting of the result is provided by specifying the
           requested output datatype. The intermediate reduction value
           is always computed as a CCTK_REAL value internally.
  @enddesc
  @history
  @endhistory 
  @var     GH
  @vdesc   Pointer to CCTK grid hierarchy
  @vtype   cGH
  @vio     in
  @endvar 
  @var     proc
  @vdesc   processor that should receive the result of operation
           (negative value means all processors receive the result)
  @vtype   int
  @vio     in
  @endvar 
  @var     num_dims
  @vdesc   number of dimensions of input arrays
  @vtype   int
  @vio     in
  @endvar 
  @var     dims
  @vdesc   dimensions of input arrays
  @vtype   int *
  @vio     in
  @endvar 
  @var     num_arrays
  @vdesc   number of input arrays
  @vtype   int
  @vio     in
  @endvar 
  @var     inarrays
  @vdesc   field of input arrays
  @vtype   void **
  @vio     in
  @endvar 
  @var     intype
  @vdesc   (common) variable type of input arrays
  @vtype   int
  @vio     in
  @endvar 
  @var     num_outvals
  @vdesc   number of values per output array
  @vtype   int
  @vio     in
  @endvar 
  @var     outvals
  @vdesc   pointer to buffer holding the output values
  @vtype   void *
  @vio     in
  @endvar 
  @var     outtype
  @vdesc   (common) variable type of output arrays
  @vtype   int
  @vio     in
  @endvar 
@@*/
int PUGH_ReductionNormInfArrays (cGH *GH,
                                 int proc,
                                 int num_dims,
                                 int dims[/* num_dims */],
                                 int num_inarrays,
                                 void *inarrays[/* num_inarrays */],
                                 int intype,
                                 int num_outvals,
                                 void *outvals /* [num_outvals] */,
                                 int outtype)
{
  return (PUGH_ReductionArrays (GH, proc, num_dims, dims,
                                intype, num_inarrays, inarrays,
                                outtype, num_outvals, outvals,
                                PUGH_ReductionNormInf));
}


/*@@
  @routine PUGH_ReductionNormInfGVs
  @author  Thomas Radke
  @date    23 Apr 1999
  @desc
           PUGH reduction routine to be registered for calculating the norm1.
           It just goes through the list of variables and calls the appropriate
           grouptype reduction routine.
           Global reduction is always done on the results of the local
           reductions, so num_outvals must be 1.
  @enddesc
  @history
  @endhistory 
  @var     GH
  @vdesc   Pointer to CCTK grid hierarchy
  @vtype   cGH
  @vio     in
  @endvar 
  @var     proc
  @vdesc   processor that should receive the result of operation
           (negative value means all processors receive the result)
  @vtype   int
  @vio     in
  @endvar 
  @var     nOutVars
  @vdesc   number of requested output elements
  @vtype   int
  @vio     in
  @endvar 
  @var     outtype
  @vdesc   type of return value
  @vtype   int
  @vio     in
  @endvar 
  @var     outvals
  @vdesc   array for the result values to be stored
  @vtype   void *
  @vio     in
  @endvar 
  @var     num_invars
  @vdesc   number of variables in the list
  @vtype   int
  @vio     in
  @endvar 
  @var     invars
  @vdesc   list of variables (given as their global indices)
  @vtype   int *
  @vio     in
  @endvar 
@@*/
int PUGH_ReductionNormInfGVs (cGH *GH,
                              int proc,  
                              int num_outvals,
                              int outtype, 
                              void *outvals /* [num_outvals] */,
                              int num_invars,
                              int invars[/* num_invars */])
{
  return (PUGH_ReductionGVs (GH, proc, num_invars, invars,
                             outtype, num_outvals, outvals,
                             PUGH_ReductionNormInf));
}


/*****************************************************************************/
/*                             local functions                               */
/*****************************************************************************/
/*@@
   @routine    PUGH_ReductionNormInf
   @date       Aug 19 1999
   @author     Thomas Radke
   @desc       Returns the "norm_inf" of a distributed array with
               'num_points' elements. Global reduction is done element-wise
               (num_outvals == 1) or on the results of the local reductions.

               "norm_inf" is defined as $\max |a_i|$.
   @enddesc 
@@*/
static int PUGH_ReductionNormInf (cGH *GH,
                                  int proc,
                                  int num_dims,
                                  int from[/* dim */],
                                  int to[/* dim */],
                                  int iterator[/* dim */],
                                  int points_per_dim[/* dim */],
                                  int num_points,
                                  int num_inarrays,
                                  int intypes[/* num_inarrays */],
                                  void *inarrays[/* num_inarrays */],
                                  int num_outvals,
                                  CCTK_REAL outvals[/*num_inarrays*num_outvals*/])
{
  int i, total_outvals;
  pGH *pughGH;
#ifdef CCTK_MPI
  CCTK_REAL *local_outvals;
#endif


/* macros to complete the ITERATE_ARRAY macro */
#define INITIAL_REDUCTION_VALUE(data)       0
#ifdef ABS
#undef ABS
#endif
#define ABS(x)      ((x) < 0 ? -(x) : (x))
#ifdef MAX
#undef MAX
#endif
#define MAX(x,y)    ((x) >= (y) ? (x) : (y))


  /* avoid compiler warnings about unused parameters */
  proc = proc;
  num_points = num_points;

  for (i = total_outvals = 0; i < num_inarrays; i++)
  {
    switch (intypes[i])
    {
      case CCTK_VARIABLE_CHAR:
#define REDUCTION_OPERATION(norm_inf, scalar)   norm_inf = MAX(norm_inf,scalar)
        ITERATE_ARRAY (CCTK_BYTE, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_BYTE, outvals, num_outvals, total_outvals);
        break;

      case CCTK_VARIABLE_INT:
#undef  REDUCTION_OPERATION
#define REDUCTION_OPERATION(norm_inf, scalar)   norm_inf = MAX(norm_inf, ABS(scalar))
        ITERATE_ARRAY (CCTK_INT, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_INT, outvals, num_outvals, total_outvals);
        break;

#ifdef CCTK_INT2
      case CCTK_VARIABLE_INT2:
        ITERATE_ARRAY (CCTK_INT2, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_INT2, outvals, num_outvals, total_outvals);
        break;
#endif

#ifdef CCTK_INT4
      case CCTK_VARIABLE_INT4:
        ITERATE_ARRAY (CCTK_INT4, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_INT4, outvals, num_outvals, total_outvals);
        break;
#endif

#ifdef CCTK_INT8
      case CCTK_VARIABLE_INT8:
        ITERATE_ARRAY (CCTK_INT8, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_INT8, outvals, num_outvals, total_outvals);
        break;
#endif

      case CCTK_VARIABLE_REAL:
        ITERATE_ARRAY (CCTK_REAL, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_REAL, outvals, num_outvals, total_outvals);
        break;

#ifdef CCTK_REAL4
      case CCTK_VARIABLE_REAL4:
        ITERATE_ARRAY (CCTK_REAL4, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_REAL4, outvals, num_outvals, total_outvals);
        break;
#endif

#ifdef CCTK_REAL8
      case CCTK_VARIABLE_REAL8:
        ITERATE_ARRAY (CCTK_REAL8, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_REAL8, outvals, num_outvals, total_outvals);
        break;
#endif

#ifdef CCTK_REAL16
      case CCTK_VARIABLE_REAL16:
        ITERATE_ARRAY (CCTK_REAL16, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_REAL16, outvals, num_outvals, total_outvals);
        break;
#endif

      case CCTK_VARIABLE_COMPLEX:
#undef  REDUCTION_OPERATION
#define REDUCTION_OPERATION(norm_inf, scalar)   norm_inf = MAX(norm_inf, CCTK_CmplxAbs(scalar))
        ITERATE_ARRAY (CCTK_COMPLEX, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_REAL, outvals, num_outvals, total_outvals);
        break;

      default:
        CCTK_WARN (1, "PUGH_ReductionNormInf: Unknown variable type");
        return (-1);
    }
  }

  pughGH = PUGH_pGH (GH);

#ifdef CCTK_MPI
  /* do the global reduction from the processors' local reduction results */
  if (pughGH->nprocs > 1) 
  {
    local_outvals = (CCTK_REAL *) malloc (total_outvals * sizeof (CCTK_REAL));

    /* outvals[] contains now the local sum */
    memcpy (local_outvals, outvals, total_outvals * sizeof (CCTK_REAL));
    if (proc < 0)
    {
      CACTUS_MPI_ERROR (MPI_Allreduce (local_outvals, outvals, total_outvals,
                        PUGH_MPI_REAL, MPI_SUM, pughGH->PUGH_COMM_WORLD));
    }
    else
    {
      CACTUS_MPI_ERROR (MPI_Reduce (local_outvals, outvals, total_outvals,
                        PUGH_MPI_REAL, MPI_SUM, proc, pughGH->PUGH_COMM_WORLD));
    }
    free (local_outvals);
  }

#endif

  return (0);
}
