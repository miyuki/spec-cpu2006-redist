 /*@@
   @header    pugh_reductions.h
   @date      April 29 1999
   @author    Gabrielle
   @desc 
   Prototypes for pugh reduction operators
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGHReduce/src/pugh_reductions.h,v 1.2 2001/08/27 16:30:51 tradke Exp $
 @@*/

#ifndef _PUGH_REDUCTIONS_H_
#define _PUGH_REDUCTIONS_H_


#include "cctk.h"
#include "cctk_Reduction.h"
#include "pugh.h"

/***
   Macro to iterate over every element of an arbitrary sized array

   The array is given by
     - its CCTK type, cctk_type,
     - the number of dimensions, vdim,
     - an (untyped) pointer to its data, vdata,
     - one-dimensional vectors, from[] and to[], of length vdim,
       specifying the start- and endpoint to iterate over for each dimension

   The result of the reduction is specified by
     - the array where to store the results, outvals[]
     - the type of the intermediate result scalar, outvals_type
     - the number of scalars to reduce from this input array, num_outvals
     - the total number of scalars for all input arrays, total_outvals
       (incremented after each iteration)

   An iteration vector, iterator[], is passed in to hold the current indices
   for each dimension of the array.
   Another vector of size vdim, points_per_dim[], is used to compute the
   linear index out of the iterator[] elements.

   Before the iteration loop, a local reduction variable, typed_outval,
   is defined and initialized by the macro INITIAL_REDUCTION_VALUE(typed_vdata).

   For each iteration the macro REDUCTION_OPERATION(typed_outval, typed_inval)
   is called, with the local reduction variable and the pointer to the
   current data element.

   Finally, the local reduction variable, typed_outval, is type-casted
   to CCTK_REAL and assigned to the current output array value outvals[].
   The total output scalar counter, total_outvals, is incremented.


   NOTE: The macro assumes from[i] > to[i] for all 1 <= i < vdim such that
         at least one iteration will be performed.
         Make sure this is true when you set up the from[] and to[] vectors.

         For efficiency reasons, the innermost loop (the one for dimension 0)
         is coded as a for() loop. Thus the overhead to iterate over a
         one-dimensional array should be kept to a minimum.
***/
#define ITERATE_ARRAY(cctk_type, vdim, vdata, from, to, iterator,             \
                      points_per_dim,                                         \
                      outvals_type, outvals, num_outvals, total_outvals)      \
    {                                                                         \
      int _i, _j, _dim, _vindex;                                              \
      cctk_type *typed_vdata = (cctk_type *) (vdata);                         \
      outvals_type typed_outval;                                              \
                                                                              \
                                                                              \
      for (_j = 0; _j < num_outvals; _j++, typed_vdata++)                     \
      {                                                                       \
        /* get the linear index of the element to start with */               \
        _vindex = from[0];                                                    \
        for (_i = 1; _i < vdim; _i++)                                         \
          _vindex += from [_i] * points_per_dim [_i];                         \
        typed_outval = INITIAL_REDUCTION_VALUE(typed_vdata + _vindex);        \
                                                                              \
        /* set iterator to local startpoint */                                \
        memcpy (iterator, from, vdim * sizeof (int));                         \
                                                                              \
        /* do the nested loops starting with the second-innermost */          \
        _dim = 1;                                                             \
        while (1)                                                             \
        {                                                                     \
          /* get the linear index */                                          \
          _vindex = 0;                                                        \
          for (_i = 1; _i < vdim; _i++)                                       \
            _vindex += iterator [_i] * points_per_dim [_i];                   \
                                                                              \
          /* do the reduction for the innermost loop (lowest dimension) */    \
          for (_i = from [0]; _i < to [0]; _i++)                              \
          {                                                                   \
            REDUCTION_OPERATION (typed_outval, typed_vdata [_i + _vindex]);   \
          }                                                                   \
                                                                              \
          if (vdim > 1)                                                       \
          {                                                                   \
            /* increment current looper and check for end */                  \
            if (++iterator [_dim] >= to [_dim])                               \
            {                                                                 \
              /* increment outermost loopers */                               \
              for (_dim++; _dim < vdim; _dim++)                               \
              {                                                               \
                if (++iterator [_dim] < to [_dim])                            \
                  break;                                                      \
              }                                                               \
                                                                              \
              /* done if beyond outermost loop */                             \
              if (_dim >= vdim)                                               \
                break;                                                        \
                                                                              \
              /* reset innermost loopers */                                   \
              for (_dim--; _dim >= 0; _dim--)                                 \
                iterator [_dim] = from [_dim];                                \
              _dim = 1;                                                       \
            }                                                                 \
          }                                                                   \
          else                                                                \
          {                                                                   \
            /* exit loop if array is one-dimensional */                       \
            break;                                                            \
          }                                                                   \
                                                                              \
        } /* end of nested loops over all dimensions */                       \
                                                                              \
        outvals [total_outvals++] = (CCTK_REAL) typed_outval;                 \
                                                                              \
      } /* end of loop over num_outvals */                                    \
    }


#ifdef __cplusplus
extern "C" {
#endif

int PUGH_ReductionMinValGVs  (REDUCTION_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionMaxValGVs  (REDUCTION_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionSumGVs     (REDUCTION_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionNorm1GVs   (REDUCTION_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionNorm2GVs   (REDUCTION_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionNormInfGVs (REDUCTION_OPERATOR_REGISTER_ARGLIST);

int PUGH_ReductionMinValArrays  (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionMaxValArrays  (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionSumArrays     (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionNorm1Arrays   (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionNorm2Arrays   (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST);
int PUGH_ReductionNormInfArrays (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST);

typedef int (*reduction_fn_t) (cGH *GH,
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

int PUGH_ReductionGVs (cGH *GH,
                       int proc,
                       int num_invars,
                       int invars[/* num_invars */],
                       int outtype,
                       int num_outvals,
                       void *outvals /* [num_outvals] */,
                       reduction_fn_t reduction_fn);
int PUGH_ReductionArrays (cGH *GH,
                          int proc,
                          int num_dims,
                          int dims[/* num_dims */],
                          int intype,
                          int num_inarrays,
                          void *inarrays[/* num_inarrays */],
                          int outtype,
                          int num_outvals,
                          void *outvals /* [num_outvals] */,
                          reduction_fn_t reduction_fn);

#ifdef __cplusplus
}
#endif

#endif
