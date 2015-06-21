#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Reduction.c
   @date      1999/05/13
   @author    Gabrielle Allen
   @desc
              This file contains routines to deal with registering and 
              using functions providing reduction operations.
   @enddesc 
   @version   $Id: Reduction.c,v 1.33 2001/12/27 19:19:04 allen Exp $
 @@*/

/* #define DEBUG_REDUCTION 1 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_Groups.h"
#include "cctk_Reduction.h"
#include "cctk_WarnLevel.h"
#include "cctk_ActiveThorns.h"

#include "StoreHandledData.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/comm/Reduction.c,v 1.33 2001/12/27 19:19:04 allen Exp $";

CCTK_FILEVERSION(comm_Reduction_c)

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header cctk_Reduce.h
   here only follow the fortran wrapper prototypes */

void CCTK_FCALL cctk_reductionhandle_
     (int *handle, ONE_FORTSTRING_ARG);

void CCTK_FCALL cctk_reductionarrayhandle_
     (int *operation_handle, ONE_FORTSTRING_ARG);

void CCTK_FCALL cctk_reduce_
     (int *fortranreturn,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      int *num_out_vals,
      int *type_out_vals,
      void *out_vals,
      int *num_in_fields,
      ... );

void CCTK_FCALL cctk_reducearray_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      int *num_out_vals,
      int *type_out_vals,
      void *out_vals,
      int *num_dims,
      int *num_in_arrays,
      int *type_in_arrays,
      ... );

/* FIXME: OLD INTERFACE */
void CCTK_FCALL cctk_reducelocalscalar_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      void *in_scalar,
      void *out_scalar,
      int *data_type);

void CCTK_FCALL cctk_reducelocscalar_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      void *in_scalar,
      void *out_scalar,
      int *data_type);

void CCTK_FCALL cctk_reducelocalarray1d_
     (int *fortran_return,
      cGH *GH,
      int *proc,
      int *operation_handle,
      void *in_array1d,
      void *out_array1d,
      int *num_in_array1d,
      int *data_type);

void CCTK_FCALL cctk_reducelocarraytoarray1d_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      void *in_array1d,
      void *out_array1d,
      int *num_in_array1d,
      int *data_type);
void CCTK_FCALL  cctk_reducelocarraytoarray2d_
     (int  *fortran_return, cGH *GH, 
      int  *proc,
      int  *operation_handle,
      void *in_array2d,
      void *out_array2d,
      int  *xsize, int *ysize,
      int  *data_type);
void CCTK_FCALL  cctk_reducelocarraytoarray3d_
     (int  *fortran_return, cGH *GH,
      int  *proc,
      int  *operation_handle,
      void *in_array3d,
      void *out_array3d,
      int  *xsize, int *ysize, int *zsize,
      int  *data_type);
/********************************************************************
 ********************    Internal Typedefs   ************************
 ********************************************************************/
/* structure holding the routines for a registered reduction operator */
typedef struct
{
  const char *implementation;
  const char *name;
  cReduceOperator reduce_operator;
} t_reduce_operator;


/********************************************************************
 ********************    Static Variables   *************************
 ********************************************************************/

static cHandledData *ReductionOperators = NULL;
static int num_reductions = 0;
static cHandledData *ReductionArrayOperators = NULL;
static int num_reductions_array = 0;


 /*@@
   @routine    CCTKi_RegisterReductionOperator
   @date       April 28 1999
   @author     Gabrielle Allen
   @desc 
   Registers "function" as a reduction operator called "name"
   @enddesc 
   @var     function
   @vdesc   Routine containing reduction operator
   @vtype   (void (*))
   @vio     
   @vcomment 
   @endvar 
   @var     name
   @vdesc   String containing name of reduction operator
   @vtype   const char *
   @vio     in
   @vcomment 
   @endvar 
@@*/
 

int CCTKi_RegisterReductionOperator(const char *thorn,
				    cReduceOperator operator,
				    const char *name)
{
  int handle;
  t_reduce_operator *reduce_operator;

  /* Check arguments */

  /* Check that the method hasn't already been registered */
  handle = Util_GetHandle(ReductionOperators, name, 
			  (void **) &reduce_operator);

  if(handle < 0)
  {

    reduce_operator = (t_reduce_operator *) 
      malloc (sizeof (t_reduce_operator));
    
    if (reduce_operator)
    {
      reduce_operator->implementation = 
	      CCTK_ThornImplementation(thorn);
      reduce_operator->name = name;
      reduce_operator->reduce_operator = operator;
      handle = Util_NewHandle(&ReductionOperators, name, reduce_operator);
    
      /* Remember how many reduction operators there are */
      num_reductions++;
    }
  }
  else
  {
    /* Reduction operator with this name already exists. */
    CCTK_Warn(1,__LINE__,__FILE__,"Cactus",
              "CCTK_RegisterReductionOperator: Reduction operator "
	      "with this name already exists");
    handle = -1;
  }

#ifdef DEBUG_REDUCTION
  CCTK_PRINTSEPARATOR
  printf("In CCTK_RegisterReductionOperator\n");
  printf("---------------------------------\n");
  printf("  Registering %s with handle %d\n",name,handle);
  CCTK_PRINTSEPARATOR
#endif
    
  return handle;

}


 /*@@
   @routine    CCTK_ReductionHandle
   @date       April 28 1999
   @author     Gabrielle Allen
   @desc 
   Returns the handle of a given reduction operator
   @enddesc 
   @var     reduction
   @vdesc   String containing name of reduction operator
   @vtype   const char *
   @vio     in
   @vcomment 
   @endvar 
@@*/

int CCTK_ReductionHandle(const char *reduction)
{

  int handle;
  void **data=NULL; /* isn't used here */

  handle = Util_GetHandle(ReductionOperators, reduction, data);

#ifdef DEBUG_REDUCTION
  CCTK_PRINTSEPARATOR
  printf("In CCTK_ReductionHandle\n");
  printf("-----------------------\n");
  printf("  Got handle %d for %s\n",handle,reduction);
  CCTK_PRINTSEPARATOR
#endif

  if (handle < 0)
    CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
              "CCTK_ReductionHandle: No handle found for reduction operator '%s'",
              reduction);

  return handle;

}  

void CCTK_FCALL cctk_reductionhandle_(int *handle, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(reduction)
  *handle = CCTK_ReductionHandle(reduction);
  free(reduction);
}


 /*@@
   @routine    CCTK_Reduce
   @date       April 28 1999
   @author     Gabrielle Allen
   @desc 
               Generic routine for doing a reduction operation on a set of
               Cactus variables.
   @enddesc 
   @var     GH
   @vdesc   pointer to the grid hierarchy
   @vtype   cGH *
   @vio     in
   @endvar 
   @var     proc
   @vdesc   processor that receives the result of the reduction operation
            (a negative value means that all processors get the result)
   @vtype   int
   @vio     in
   @endvar 
   @var     operation_handle
   @vdesc   the handle specifying the reduction operator
   @vtype   int
   @vio     in
   @endvar 
   @var     num_out_vals
   @vdesc   number of elements in the reduction output
   @vtype   int
   @vio     in
   @endvar 
   @var     type_out_vals
   @vdesc   datatype of the output values
   @vtype   int
   @vio     in
   @endvar 
   @var     out_vals
   @vdesc   pointer to buffer holding the output values
   @vtype   void *
   @vio     in
   @endvar 
   @var     num_in_fields
   @vdesc   number of input fields passed in the variable argument list
   @vtype   int
   @vio     in
   @endvar 
   @var     <...>
   @vdesc   list of variables indices of input fields
   @vtype   int
   @vio     in
   @endvar 
@@*/

int CCTK_Reduce(cGH *GH, 
                int proc,
                int operation_handle,
                int num_out_vals,
                int type_out_vals,
                void *out_vals,
                int num_in_fields,
                ... )
{
  va_list indices;
  int i;
  int retval;
  int *in_fields;
  t_reduce_operator *operator;

  /* Get the pointer to the reduction operator */
  if (operation_handle < 0)
  {
    CCTK_Warn(3,__LINE__,__FILE__,"Cactus",
              "CCTK_Reduce: Invalid handle passed to CCTK_Reduce");
    retval = -1;
  }
  else
  {
    operator = (t_reduce_operator *) 
      Util_GetHandledData(ReductionOperators,operation_handle);
    
    if (!operator)
    {
      CCTK_Warn(3,__LINE__,__FILE__,"Cactus",
		"CCTK_Reduce: Reduction operation is not registered"
		"and cannot be called");
      retval = -1;
    }
    else
    {
      /* Fill in the array of variable indices from the variable 
	 argument list */
      in_fields = malloc(num_in_fields*sizeof(int));
      va_start(indices, num_in_fields);
      for (i=0; i<num_in_fields; i++)
      {
	in_fields[i] = va_arg(indices,int);
      }
      va_end(indices);

      retval = operator->reduce_operator (GH, proc, num_out_vals, 
					type_out_vals, out_vals,
					num_in_fields, in_fields);
  
      free(in_fields);
    }
  }
  return retval;
}

void CCTK_FCALL cctk_reduce_
     (int *fortranreturn,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      int *num_out_vals,
      int *type_out_vals,
      void *out_vals,
      int *num_in_fields,
      ... )
{ 
  va_list indices;
  int retval;
  int i;
  int *in_fields;
  t_reduce_operator *operator;

  /* initialize return code to indicate an error */
  *fortranreturn = -1;

  if (*operation_handle < 0)
  {
    CCTK_Warn(3,__LINE__,__FILE__,"Cactus",
              "CCTK_Reduce: Invalid handle passed to CCTK_Reduce");
    retval = -1;
  }
  else
  {
    /* Get the pointer to the reduction operator */
    operator = (t_reduce_operator *) 
      Util_GetHandledData(ReductionOperators,*operation_handle);
    
    if (!operator)
    {
      CCTK_Warn(3,__LINE__,__FILE__,"Cactus",
		"CCTK_Reduce: Reduction operation is not registered"
		" and cannot be called");
      retval = -1;
    }
    else
    {
      
      /* Fill in the array of variable indices from the variable 
	 argument list */
      in_fields = malloc (*num_in_fields * sizeof (int));
      va_start(indices, num_in_fields);
      for (i=0; i<*num_in_fields; i++)
      {
	in_fields[i] = *va_arg(indices,int *);
      }
      va_end(indices);
  
      retval = operator->reduce_operator (GH, *proc, *num_out_vals, 
					  *type_out_vals, out_vals,
					  *num_in_fields,in_fields);
      
      free(in_fields);
    }
  }
  *fortranreturn = retval;
}


 /*@@
   @routine CCTK_RegisterReductionArrayOperator
   @date    Aug 19 1999
   @author  Thomas Radke
   @desc 
            Registers "function" as a array reduction operator called "name"
   @enddesc 
   @var     function
   @vdesc   Routine containing reduction operator
   @vtype   (int (*))
   @vio     
   @vcomment 
   @endvar 
   @var     name
   @vdesc   String containing name of reduction operator
   @vtype   const char *
   @vio     in
   @vcomment 
   @endvar 
@@*/
 
int CCTK_RegisterReductionArrayOperator
         (int (*function)(REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST),
         const char *name)
{
  int handle;

  /* Check that the method hasn't already been registered */
  handle = Util_GetHandle(ReductionArrayOperators, name, NULL);

  if(handle < 0)
  {
    /* Get a handle for it. */
    handle = Util_NewHandle(&ReductionArrayOperators, name, (int *)function);
    
    /* Remember how many reduction operators there are */
    num_reductions_array++;
   }
  else
  {
    /* Reduction operator with this name already exists. */
    CCTK_Warn(1,__LINE__,__FILE__,"Cactus",
              "CCTK_RegisterReductionArrayOperator: "
	      "Array reduction operator with this name already exists");
    handle = -1;
  }

#ifdef DEBUG_REDUCTION
  CCTK_PRINTSEPARATOR
  printf("In CCTK_RegisterReductionArrayOperator\n");
  printf("---------------------------------\n");
  printf("  Registering %s with handle %d\n",name,handle);
  CCTK_PRINTSEPARATOR
#endif
    
  return handle;

}


 /*@@
   @routine CCTK_ReductionArrayHandle
   @date    Aug 19 1999
   @author  Thomas Radke
   @desc 
            Returns the handle of a given array reduction operator
   @enddesc 
   @var     reduction
   @vdesc   String containing name of array reduction operator
   @vtype   const char *
   @vio     in
   @vcomment 
   @endvar 
@@*/

int CCTK_ReductionArrayHandle(const char *reduction)
{

  int handle;
  void **data=NULL; /* isn't used here */

  handle = Util_GetHandle(ReductionArrayOperators, reduction, data);

#ifdef DEBUG_REDUCTION
  CCTK_PRINTSEPARATOR
  printf("In CCTK_ReductionArrayHandle\n");
  printf("-----------------------\n");
  printf("  Got handle %d for %s\n",handle,reduction);
  CCTK_PRINTSEPARATOR
#endif

  if (handle < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                "CCTK_ReductionArrayHandle: "
		"No handle found for array reduction operator '%s'", 
		reduction);
  }

  return handle;
}  

void CCTK_FCALL cctk_reductionarrayhandle_
     (int *operation_handle, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(reduction)
  *operation_handle = CCTK_ReductionArrayHandle(reduction);
  free(reduction);
}


 /*@@
   @routine    CCTK_ReduceArray
   @date       Aug 19 1999
   @author     Thomas Radke
   @desc 
               Generic routine for doing a reduction operation on a set of
               arrays.
   @enddesc 
   @var     GH
   @vdesc   pointer to the grid hierarchy
   @vtype   cGH *
   @vio     in
   @endvar 
   @var     proc
   @vdesc   processor that receives the result of the reduction operation
            (a negative value means that all processors get the result)
   @vtype   int
   @vio     in
   @endvar 
   @var     operation_handle
   @vdesc   the handle specifying the reduction operator
   @vtype   int
   @vio     in
   @endvar 
   @var     num_out_vals
   @vdesc   number of elements in the reduction output
   @vtype   int
   @vio     in
   @endvar 
   @var     type_out_vals
   @vdesc   datatype of the reduction output
   @vtype   int
   @vio     in
   @endvar 
   @var     out_vals
   @vdesc   pointer to buffer holding the output values
   @vtype   void *
   @vio     in
   @endvar 
   @var     num_dims
   @vdesc   number of dimensions of input arrays
   @vtype   int
   @vio     in
   @endvar 
   @var     num_in_fields
   @vdesc   number of input fields passed in the variable argument list
   @vtype   int
   @vio     in
   @endvar 
   @var     type_in_arrays
   @vdesc   datatype of the input arrays
   @vtype   int
   @vio     in
   @endvar 
   @var     <...>
   @vdesc   list of dimensions of input arrays
   @vtype   int
   @vio     in
   @endvar 
   @var     <...>
   @vdesc   list of pointers to input arrays
   @vtype   void *
   @vio     in
   @endvar 
@@*/

int CCTK_ReduceArray(cGH *GH, 
                     int proc,
                     int operation_handle,
                     int num_out_vals,
                     int type_out_vals,
                     void *out_vals,
                     int num_dims,
                     int num_in_arrays,
                     int type_in_arrays,
                     ... )
{
 
  va_list indices;
  int i;
  int *dims;
  void **in_arrays;
  int (*function)(REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST)=NULL; 


  /* Get the pointer to the reduction operator */
  if (operation_handle < 0)
  {
    CCTK_Warn(3,__LINE__,__FILE__,"Cactus",
              "CCTK_ReduceArray: Invalid handle passed to CCTK_ReduceArray");
    return (-1);
  }

  function = (int (*)(REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST))
             Util_GetHandledData(ReductionArrayOperators,operation_handle);
    
  if (! function)
  {
    CCTK_Warn(3,__LINE__,__FILE__,"Cactus",
              "CCTK_ReduceArray: Array reduction operation is not registered "
                 "and cannot be called");
    return (-1);
  }

  /* allocate memory for dims and input array pointers */
  dims = (int *) malloc (num_dims * sizeof (int));
  in_arrays = (void **) malloc (num_in_arrays * sizeof (void *));

  /* Fill in the arrays of dims and input array pointers
     from the variable argument list */

  va_start(indices, type_in_arrays);

  for (i = 0; i < num_dims; i++) 
    dims [i] = va_arg (indices, int);
  for (i = 0; i < num_in_arrays; i++)
    in_arrays [i] = va_arg (indices, void *);

  va_end(indices);
        
  function (GH, proc, num_dims, dims,
            num_in_arrays, in_arrays, type_in_arrays,
            num_out_vals, out_vals, type_out_vals);
        
  free (in_arrays);
  free (dims);
        
  return (0);
}

void CCTK_FCALL cctk_reducearray_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      int *num_out_vals,
      int *type_out_vals,
      void *out_vals,
      int *num_dims,
      int *num_in_arrays,
      int *type_in_arrays,
      ... )
{
 
  va_list varargs;
  int i;
  int *dims;
  void **in_arrays;
  int (*function)(REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST); 


  /* initialize return code to indicate an error */
  *fortran_return = -1;

  /* Get the pointer to the reduction operator */
  if (*operation_handle < 0)
  {
    CCTK_Warn (3,__LINE__,__FILE__,"Cactus",
               "CCTK_ReduceArray: Invalid handle passed to CCTK_ReduceArray");
    return;
  }

  function = (int (*) (REDUCTION_ARRAY_OPERATOR_REGISTER_ARGLIST))
              Util_GetHandledData (ReductionArrayOperators, *operation_handle);
    
  if (! function)
  {
    CCTK_Warn (3,__LINE__,__FILE__,"Cactus", 
               "CCTK_ReduceArray: Array reduction operation is not registered "
	       "and cannot be called");
    return;
  }

  /* allocate memory for dims and input array pointers */
  dims = (int *) malloc (*num_dims * sizeof (int));
  in_arrays = (void **) malloc (*num_in_arrays * sizeof (void *));

  /* Fill in the arrays of dims and input array pointers
     from the variable argument list */

  va_start (varargs, type_in_arrays);

  for (i = 0; i < *num_dims; i++)
    dims [i] = *va_arg (varargs, int *);
  for (i = 0; i < *num_in_arrays; i++)
    in_arrays [i] = va_arg (varargs, void *);

  va_end (varargs);
        
  function (GH, *proc, *num_dims, dims,
            *num_in_arrays, in_arrays, *type_in_arrays,
            *num_out_vals, out_vals, *type_out_vals);
        
  free (in_arrays);
  free (dims);

  *fortran_return = 0;
}


 /*@@
   @routine    CCTK_ReduceLocalScalar
   @date       Aug 19 1999
   @author     Thomas Radke
   @desc    Wrapper function for reduction of a single scalar
   @enddesc 
   @var     GH
   @vdesc   pointer to the grid hierarchy
   @vtype   cGH *
   @vio     in
   @endvar 
   @var     proc
   @vdesc   processor that receives the result of the reduction operation
            (a negative value means that all processors get the result)
   @vtype   int
   @vio     in
   @endvar 
   @var     operation_handle
   @vdesc   the handle specifying the reduction operator
   @vtype   int
   @vio     in
   @endvar 
                            void *in_scalar, void *out_scalar, int data_type)
   @var     in_scalar
   @vdesc   pointer to input scalar
   @vtype   void *
   @vio     in
   @endvar 
   @var     out_scalar
   @vdesc   pointer to output scalar
   @vtype   void *
   @vio     in
   @endvar 
   @var     data_type
   @vdesc   datatype for both input and output scalar
   @vtype   int
   @vio     in
   @endvar 
@@*/

/*** FIXME: OLD INTERFACE gerd ***/
int CCTK_ReduceLocalScalar (cGH *GH, int proc, int operation_handle,
                            void *in_scalar, void *out_scalar, int data_type)
{
  return (CCTK_ReduceArray (GH, proc, operation_handle,
                            1, data_type, out_scalar,
                            1, 1, data_type, 1, in_scalar));
}

/*** FIXME: OLD INTERFACE gerd ***/
void CCTK_FCALL cctk_reducelocalscalar_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      void *in_scalar,
      void *out_scalar,
      int *data_type)
{
  *fortran_return = CCTK_ReduceArray (GH, *proc, *operation_handle,
                                      1, *data_type, out_scalar,
                                      1, 1, *data_type, 1, in_scalar);
}


int CCTK_ReduceLocScalar (cGH *GH, int proc, int operation_handle,
                            void *in_scalar, void *out_scalar, int data_type)
{
  return (CCTK_ReduceArray (GH, proc, operation_handle,
                            1, data_type, out_scalar,
                            1, 1, data_type, 1, in_scalar));
}

void CCTK_FCALL cctk_reducelocscalar_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      void *in_scalar,
      void *out_scalar,
      int *data_type)
{
  *fortran_return = CCTK_ReduceArray (GH, *proc, *operation_handle,
                                      1, *data_type, out_scalar,
                                      1, 1, *data_type, 1, in_scalar);
}


 /*@@
   @routine    CCTK_ReduceLocArrayToArray1D
   @date       Thu Oct 14 12:10:01 1999
   @author     Gerd Lanfermann
   @desc 
        Interface to the migthy CCTK_Reduce for
        reduction of local 1D arrays to local arrays 
        (element by element).
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

/*** FIXME: OLD INTERFACE gerd ***/
int CCTK_ReduceLocalArray1D (cGH *GH, int proc, int operation_handle, 
                             void *in_array1d, void *out_array1d, 
                             int num_in_array1d, 
                             int data_type)
{
  return (CCTK_ReduceArray (GH, proc, operation_handle,
                            num_in_array1d, data_type, out_array1d,
                            1, 1, data_type, num_in_array1d, in_array1d));
}

int CCTK_ReduceLocArrayToArray1D(cGH *GH, int proc, int operation_handle,
                                 void *in_array1d, void *out_array1d, 
                                 int num_in_array1d,
                                 int data_type)
{
  return (CCTK_ReduceArray (GH, proc, operation_handle,
                            num_in_array1d, data_type, out_array1d,
                            1, 1, data_type, num_in_array1d, in_array1d));
}

/*** FIXME: OLD INTERFACE gerd ***/
void CCTK_FCALL cctk_reducelocalarray1d_
     (int *fortran_return,
      cGH *GH,
      int *proc,
      int *operation_handle,
      void *in_array1d,
      void *out_array1d,
      int *num_in_array1d,
      int *data_type)
{
  *fortran_return = CCTK_ReduceArray (GH, *proc, *operation_handle,
                                      *num_in_array1d, *data_type, out_array1d,
                                      1, 1, *data_type, *num_in_array1d,
                                      in_array1d);
}


/*@@
   @routine    CCTK_ReduceLocArrayToArray1D
   @date       Sat Nov 27 22:52:10 1999
   @author     Gerd Lanfermann
   @desc 
       Interface for the reduction of local 1d arrays
       to the mighty CCCTK_reduce interface. 
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/


void CCTK_FCALL cctk_reducelocarraytoarray1d_
     (int *fortran_return,
      cGH *GH, 
      int *proc,
      int *operation_handle,
      void *in_array1d,
      void *out_array1d,
      int *num_in_array1d,
      int *data_type)
{
  *fortran_return = CCTK_ReduceArray (GH, *proc, *operation_handle,
                                      *num_in_array1d, *data_type, out_array1d,
                                      1, 1, *data_type, *num_in_array1d,
                                      in_array1d);
}

/*@@
   @routine    CCTK_ReduceLocArrayToArray2D
   @date       Sat Nov 27 22:52:10 1999
   @author     Gerd Lanfermann
   @desc 
       Interface for the reduction of local 2d arrays
       to the mighty CCCTK_reduce interface. 
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/



int CCTK_ReduceLocArrayToArray2D(cGH *GH, int proc, int operation_handle,
                                 void *in_array2d, void *out_array2d, 
                                 int xsize, int ysize,
                                 int data_type)
{
  int  lin_size= xsize*ysize;
  return (CCTK_ReduceArray (GH, proc, operation_handle,
                            lin_size,
                            data_type, out_array2d,
                            2, 1, data_type, 
                            xsize,ysize, in_array2d));
}

void CCTK_FCALL  cctk_reducelocarraytoarray2d_
     (int  *fortran_return, cGH *GH, 
      int  *proc,
      int  *operation_handle,
      void *in_array2d,
      void *out_array2d,
      int  *xsize, int *ysize,
      int  *data_type)
{
  int lin_size = (*xsize)*(*ysize);
  *fortran_return =  CCTK_ReduceArray (GH, *proc, *operation_handle,
                                      lin_size,
                                      *data_type, out_array2d,
                                      2, 1, *data_type, 
                                      *xsize, *ysize,
                                      in_array2d);
}

/*@@
   @routine    CCTK_ReduceLocArrayToArray1D
   @date       Sat Nov 27 22:52:10 1999
   @author     Gerd Lanfermann
   @desc 
       Interface for the reduction of local 3d arrays
       to 3d arrays.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CCTK_ReduceLocArrayToArray3D(cGH *GH, int proc, int operation_handle,
                                 void *in_array3d, void *out_array3d, 
                                 int xsize, int  ysize, int zsize,
                                 int data_type)
{

  int lin_size =  xsize*ysize*zsize;
  return (CCTK_ReduceArray (GH, proc, operation_handle,
                            lin_size,
                            data_type, out_array3d,
                            3, 1, data_type, 
                            xsize,ysize,zsize,
                            in_array3d));
}

void CCTK_FCALL  cctk_reducelocarraytoarray3d_
     (int  *fortran_return, cGH *GH, 
      int  *proc,
      int  *operation_handle,
      void *in_array3d,
      void *out_array3d,
      int  *xsize, int *ysize, int *zsize,
      int  *data_type)
{
  int lin_size =  (*xsize)*(*ysize)*(*zsize);
  *fortran_return =  CCTK_ReduceArray (GH, *proc, *operation_handle,
                                       lin_size,
                                       *data_type, out_array3d,
                                       3, 1, *data_type, 
                                       *xsize,*ysize,*zsize, 
                                       in_array3d);
}

 /*@@
   @routine    CCTK_NumReduceOperators
   @date       Mon Oct 22 2001
   @author     Gabrielle Allen
   @desc
               The number of reduction operators registered
   @enddesc
   @returntype int
   @returndesc
               number of reduction operators
   @endreturndesc
@@*/

int CCTK_NumReduceOperators()
{
  return num_reductions;
}

 /*@@
   @routine    CCTK_ReduceOperatorImplementation
   @date       Mon Oct 22 2001
   @author     Gabrielle Allen
   @desc
   Provide the implementation which provides an reduction operator
   @enddesc
   @returntype int
   @returndesc
               Implementation which supplied the interpolation operator
   @endreturndesc
@@*/

const char *CCTK_ReduceOperatorImplementation(int handle)
{
  t_reduce_operator *operator;
  const char *imp=NULL;

  operator = (t_reduce_operator *) 
    Util_GetHandledData (ReductionOperators, handle);
  
  if (operator)
  {
    imp = operator->implementation;
  }

  return imp;
}



 /*@@
   @routine    CCTK_ReduceOperator
   @date       December 27 2001
   @author     Gabrielle Allen
   @desc
               Returns the name of a reduction operator
   @enddesc
   @var        handle
   @vdesc      Handle for reduction operator
   @vtype      int
   @vio        in
   @vcomment
   @endvar

   @returntype const char *
   @returndesc
   The name of the reduction operator, or NULL if the handle 
   is invalid
   @endreturndesc
@@*/
const char *CCTK_ReduceOperator (int handle)
{
  const char *name=NULL;
  t_reduce_operator *operator;

  if (handle < 0)
  {
    CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
                "CCTK_ReduceOperator: Handle %d invalid", handle);
  }
  else
  {
    operator = (t_reduce_operator *) Util_GetHandledData (ReductionOperators,
							  handle);
    if (operator)
    {
      name = operator->name;
    }
    else
    {
      CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
		  "CCTK_ReduceOperator: Handle %d invalid", handle);
    }
  }

  return name;
}
