 /*@@
   @header    cctk_Interp.h
   @date      July 07 1999
   @author    Thomas Radke
   @desc
              Header file for using interpolation operators
   @enddesc
   @history
   @date      July 07 1999
   @author    Thomas Radke
   @hdesc     Just copied from cctk_Reduction.h
   @endhistory
 @@*/


#ifndef _CCTK_INTERP_H_
#define _CCTK_INTERP_H_

#ifdef __cplusplus
extern "C"
{
#endif

/* prototype for interpolation operator routine
   working on a list of grid variables */
typedef int (*cInterpOperatorGV) (cGH *GH,
                                  const char *coord_system,
                                  int num_points,
                                  int num_in_array_indices,
                                  int num_out_arrays,
                                  const void *const interp_coord_arrays[],
                                  const int interp_coord_array_types[],
                                  const int in_array_indices[],
                                  void *const out_arrays[],
                                  const int out_array_types[]);

/* prototype for interpolation operator routine
   working on local arrays */
typedef int (*cInterpOperatorLocal) (cGH *GH,
				     int num_points,
                                     int num_dims,
                                     int num_in_arrays,
                                     int num_out_arrays,
                                     const int coord_dims[],
                                     const void *const coord_arrays[],
                                     const int coord_array_types[],
                                     const void *const interp_coord_arrays[],
                                     const int interp_coord_array_types[],
                                     const void *const in_arrays[],
                                     const int in_array_types[],
                                     void *const out_arrays[],
                                     const int out_array_types[]);

int CCTK_InterpHandle (const char *name);

#define CCTK_InterpRegisterOperatorGV(a,b) CCTKi_InterpRegisterOperatorGV(CCTK_THORNSTRING,a,b)
int CCTKi_InterpRegisterOperatorGV (const char *thorn,
				    cInterpOperatorGV operator_GV,
				    const char *name);

#define CCTK_InterpRegisterOperatorLocal(a,b) CCTKi_InterpRegisterOperatorLocal(CCTK_THORNSTRING,a,b)
int CCTKi_InterpRegisterOperatorLocal (const char *thorn,
				       cInterpOperatorLocal operator_local,
				       const char *name);

int CCTK_InterpGV (cGH *GH,
                   int operator_handle,
                   int coord_system_handle,
                   int num_points,
                   int num_in_array_indices,
                   int num_out_arrays,
                   ...);

int CCTK_InterpLocal (cGH *GH,
                      int operator_handle,
                      int num_points,
                      int num_dims,
                      int num_in_arrays,
                      int num_out_arrays,
                      ...);

const char *CCTK_InterpOperatorImplementation(int handle);

const char *CCTK_InterpOperator(int handle);

int CCTK_NumInterpOperators(void);

#ifdef __cplusplus
}
#endif

#endif  /* _INTERP_H_ */
