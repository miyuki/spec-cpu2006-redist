 /*@@
   @header    cctk_Reduction.h
   @date      
   @author    Gabrielle Allen
   @desc 
   Header file for using reduction operators
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Reduction.h,v 1.13 2001/12/27 19:13:18 allen Exp $
 @@*/

#ifndef _CCTK_REDUCTION_H_
#define _CCTK_REDUCTION_H_

#define REDUCTION_OPERATOR_REGISTER_ARGLIST  \
          cGH *arg_GH, \
          int arg_proc, \
          int arg_num_outvals, \
          int arg_outtype, \
          void *arg_outvals, \
          int arg_num_invars, \
          int arg_varlist []


#define REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST \
  cGH *arg_GH, \
  int arg_proc, \
  int arg_nDims, \
  int arg_dims [], \
  int arg_nArrays, \
  void *arg_inArrays [], \
  int arg_inType, \
  int arg_nOutVals, \
  void *arg_outVals, \
  int arg_outType

#ifdef __cplusplus
extern "C" 
{
#endif

/* prototype for reduction operator routine */
typedef int (*cReduceOperator) (cGH *GH,
				int arg_proc, 
				int arg_num_outvals, 
				int arg_outtype, 
				void *arg_outvals, 
				int arg_num_invars, 
				int arg_varlist[]);

int CCTK_Reduce(cGH *GH,
                int proc,
                int operation_handle,
                int num_out_vals,
                int type_out_vals,
                void *out_vals,
                int num_in_fields, ...);

int CCTK_ReductionHandle(const char *reduction);

#define CCTK_RegisterReductionOperator(a,b) \
        CCTKi_RegisterReductionOperator(CCTK_THORNSTRING,a,b) 

int CCTKi_RegisterReductionOperator(const char *thorn,
				    cReduceOperator operatorGV,
				    const char *name);

int CCTK_ReductionArrayHandle(const char *reduction);

int CCTK_RegisterReductionArrayOperator(
         int (*function)(REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST),
         const char *name);

const char *CCTK_ReduceOperatorImplementation(int handle);

const char *CCTK_ReduceOperator (int handle);

int CCTK_NumReduceOperators(void);

/* FIXME: old interface - should go */
int CCTK_ReduceLocalScalar (cGH *GH, int proc, int operation_handle,
                            void *inScalar, void *outScalar, int dataType);


/* FIXME: old interface - should go */
int CCTK_ReduceLocalArray1D (cGH *GH, int proc, int operation_handle,
                            void *in_array1d, void *out_array1d, 
                            int num_in_array1d, int data_type);

int CCTK_ReduceLocScalar(cGH *GH, int proc, int operation_handle,
                         void *in_scalar, void *out_scalar, int data_type);

int CCTK_ReduceLocArrayToArray1D(cGH *GH, int proc, int operation_handle,
                                 void *in_array1d, void *out_array1d, 
                                 int num_in_array1d,
                                 int data_type);

int CCTK_ReduceLocArrayToArray2D(cGH *GH, int proc, int operation_handle,
                                 void *in_array2d, void *out_array2d, 
                                 int xsize, int ysize,
                                 int data_type);

int CCTK_ReduceLocArrayToArray3D(cGH *GH, int proc, int operation_handle,
                                 void *in_array3d, void *out_array3d, 
                                 int xsize, int  ysize, int zsize,
                                 int data_type);


int CCTK_ReduceArray(cGH *GH,
                     int proc,
                     int operation_handle,
                     int num_out_vals,
                     int type_out_vals,
                     void *out_vals,
                     int num_dims,
                     int num_in_arrays,
                     int type_in_arrays,
                     ... );

#ifdef __cplusplus
}
#endif

#endif /* _CCTK_REDUCTION_H_ */
