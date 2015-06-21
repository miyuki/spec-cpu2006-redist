#ifdef SPEC_CPU
# define THORN_IS_PUGHReduce
#endif /* SPEC_CPU */
 /*@@
   @file      ReductionSum.c
   @date      Thu Apr  3 11:54:53 1997
   @author    Thomas Radke, Paul Walker
   @desc 
              Defines the PUGH reduction operator to get the sum
              of an arbitrary array.
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGHReduce/src/ReductionSum.c,v 1.6 2001/11/05 15:04:13 tradke Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "pugh_reductions.h"

static const char *rcsid = "$Id: ReductionSum.c,v 1.6 2001/11/05 15:04:13 tradke Exp $";

CCTK_FILEVERSION(CactusPUGH_PUGHReduce_ReductionSum_c)

/* local function prototypes */
static int PUGH_ReductionSum (cGH *GH,
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
  @routine PUGH_ReductionSumArrays
  @author  Thomas Radke
  @date    19 Aug 1999
  @desc
           Registered PUGH reduction routine for computing the sums
           of a set of arrays.
           The arrays are described by their dimensions and variable type.
           For the number of output values only 1 is accepted.
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
  @var     nDims
  @vdesc   number of dimensions of input arrays
  @vtype   int
  @vio     in
  @endvar 
  @var     dims
  @vdesc   dimensions of input arrays
  @vtype   int *
  @vio     in
  @endvar 
  @var     nArrays
  @vdesc   number of input arrays
  @vtype   int
  @vio     in
  @endvar 
  @var     inArrays
  @vdesc   field of input arrays
  @vtype   void **
  @vio     in
  @endvar 
  @var     inType
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
int PUGH_ReductionSumArrays (cGH *GH, 
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
                                PUGH_ReductionSum));
}


/*@@
  @routine PUGH_ReductionSumGVs
  @author  Thomas Radke
  @date    23 Apr 1999
  @desc
           PUGH reduction routine to be registered for calculating the sum.
           It just goes through the list of variables and calls the appropriate
           grouptype reduction routine.
           If the number of requested output values equals the size of the
           variable, global reduction is done element-wise.
           Otherwise num_outvals must be 1, and global reduction is done on the
           results of the local reductions.
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
  @var     num_outvals
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
int PUGH_ReductionSumGVs (cGH *GH, 
                             int proc, 
                             int num_outvals,
                             int outtype, 
                             void *outvals /* [num_outvals] */,
                             int num_invars, 
                             int invars[/* num_invars */])
{
  return (PUGH_ReductionGVs (GH, proc, num_invars, invars, 
                             outtype, num_outvals, outvals,
                             PUGH_ReductionSum));
}


/*****************************************************************************/
/*                             local functions                               */
/*****************************************************************************/
/*@@
   @routine    PUGH_ReductionSum
   @date       Aug 19 1999
   @author     Thomas Radke
   @desc       Returns the sum of a distributed array with
               'num_points' elements. Global reduction is done element-wise
               (num_outvals == 1) or on the results of the local reductions.
   @enddesc
@@*/
static int PUGH_ReductionSum (cGH *GH,
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
#ifdef CCTK_MPI
  pGH *pughGH;
  CCTK_REAL *local_outvals;
#endif


  /* avoid compiler warnings about unused parameters */
  GH = GH;
  proc = proc;
  num_points = num_points;

/* macros to complete the ITERATE_ARRAY macro */
#define INITIAL_REDUCTION_VALUE(array)     0
#define REDUCTION_OPERATION(sum, scalar)   sum += scalar

  for (i = total_outvals = 0; i < num_inarrays; i++)
  {
    switch (intypes[i])
    {
      case CCTK_VARIABLE_CHAR:
        ITERATE_ARRAY (CCTK_BYTE, num_dims, inarrays[i],
                       from, to, iterator, points_per_dim,
                       CCTK_BYTE, outvals, num_outvals, total_outvals);
        break;

      case CCTK_VARIABLE_INT:
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
        CCTK_WARN (1, "PUGH_ReductionSum: Don't know how to compute "
                      "the sum of complex variables !!!");
        return (-1);

      default:
        CCTK_WARN (1, "PUGH_ReductionSum: Unknown variable type");
        return (-1);
    }
  }

#ifdef CCTK_MPI
  pughGH = PUGH_pGH (GH);

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
