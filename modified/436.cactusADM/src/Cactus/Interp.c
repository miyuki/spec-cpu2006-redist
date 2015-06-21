#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Interp.c
   @date      July 07 1999
   @author    Thomas Radke
   @desc
              Registration and invocation routines for interpolation operators.
   @enddesc
   @history
   @date      July 07 1999
   @author    Thomas Radke
   @hdesc     Just copied from Reduction.c
   @endhistory
   @version   $Id: Interp.c,v 1.24 2001/12/27 19:19:03 allen Exp $
 @@*/

/* #define DEBUG_INTERP 1 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "cctk_Constants.h"
#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_Groups.h"
#include "cctk_GroupsOnGH.h"
#include "StoreHandledData.h"
#include "cctk_Interp.h"
#include "cctk_WarnLevel.h"
#include "cctk_Coord.h"
#include "cctk_ActiveThorns.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/comm/Interp.c,v 1.24 2001/12/27 19:19:03 allen Exp $";

CCTK_FILEVERSION(comm_Interp_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header cctk_Groups.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL cctk_interphandle_
                           (int *handle,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_interpgv_
                           (int *fortranreturn,
                            cGH *GH,
                            const int *operator_handle,
                            const int *coord_system_handle,
                            const int *num_points,
                            const int *num_in_array_indices,
                            const int *num_out_arrays,
                            ...);
void CCTK_FCALL cctk_interplocal_
                           (int *fortranreturn,
                            cGH *GH,
                            const int *operator_handle,
                            const int *num_points,
                            const int *num_dims,
                            const int *num_in_arrays,
                            const int *num_out_arrays,
                            ...);


/********************************************************************
 ********************    Macro Definitions   ************************
 ********************************************************************/
/* macro to read a list of items from a variable argument list into an array */
#define VARARGS_TO_ARRAY(array, type, modifier, count, varargs_list)          \
          {                                                                   \
            int i;                                                            \
                                                                              \
                                                                              \
            for (i = 0; i < (count); i++)                                     \
            {                                                                 \
              (array)[i] = modifier va_arg (varargs_list, type);              \
            }                                                                 \
          }

/* empty define to pass into the VARARGS_TO_ARRAY macro
   (some preprocessors need that) */
#define NOTHING


/********************************************************************
 ********************    Internal Typedefs   ************************
 ********************************************************************/
/* structure holding the routines for a registered interpolation operator */
typedef struct
{
  const char *implementation;
  const char *name;
  cInterpOperatorGV interp_operator_GV;
  cInterpOperatorLocal interp_operator_local;
} t_interp_operator;


/********************************************************************
 ********************    Static Variables   *************************
 ********************************************************************/
/* static data: interpolation operator database and counter for registered
                operators */
static cHandledData *interp_operators = NULL;
static int num_interp_operators = 0;


 /*@@
   @routine    CCTKi_InterpRegisterOperatorGV
   @date       Mon 12 Feb 2001
   @author     Thomas Radke
   @desc
               Registers a routine as an interpolation operator for
               CCTK grid variables
   @enddesc

   @var        operator_GV
   @vdesc      interpolation operator
   @vtype      cctk_interp_gv_operator
   @vio        in
   @endvar
   @var        name
   @vdesc      name identifying the interpolation operator
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the handle for the newly registered operator, or<p>
               -1 NULL pointer was passed as interpolation operator routine<p>
               -2 failed to allocate memory<p>
               -3 interpolation operator by given name already exists
   @endreturndesc
@@*/
int CCTKi_InterpRegisterOperatorGV (const char *thorn,
				    cInterpOperatorGV operator_GV,
				    const char *name)
{
  int handle;
  t_interp_operator *operator;


  /* Check arguments */
  if (operator_GV == NULL)
  {
    CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
               "CCTKi_InterpRegisterOperatorGV: "
	       "NULL pointer passed as interpolation operator routine");
    handle = -1;
  }
  else
  {
    /* Check that the method hasn't already been registered */
    handle = Util_GetHandle (interp_operators, name, (void **) &operator);

    if (handle < 0)
    {
      /* Get a handle for it. */
      operator = (t_interp_operator *) malloc (sizeof (t_interp_operator));
      if (operator)
      {
	operator->implementation = CCTK_ThornImplementation(thorn);
	operator->name = name;
        operator->interp_operator_GV = operator_GV;
        operator->interp_operator_local = NULL;
        handle = Util_NewHandle (&interp_operators, name, operator);

        /* Remember how many interpolation operators there are */
        num_interp_operators++;
      }
      else
      {
        CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
                   "CCTKi_InterpRegisterOperatorGV: "
		   "Couldn't allocate interpolation operator handle");
        handle = -2;
      }
    }
    else if (operator->interp_operator_GV == NULL)
    {
      operator->interp_operator_GV = operator_GV;
    }
    else
    {
      /* Interpolation operator with this name already exists. */
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTKi_InterpRegisterOperatorGV: "
		  "Operator '%s' already exists",
                  name);
      handle = -3;
    }
  }

  return (handle);
}


 /*@@
   @routine    CCTKi_InterpRegisterOperatorLocal
   @date       Mon 12 Feb 2001
   @author     Thomas Radke
   @desc
               Registers a routine as an interpolation operator for
               processor-local arrays
   @enddesc

   @var        operator_local
   @vdesc      interpolation operator
   @vtype      cctk_interp_local_operator
   @vio        in
   @endvar
   @var        name
   @vdesc      name identifying the interpolation operator
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the handle for the newly registered operator, or<p>
               -1 NULL pointer was passed as interpolation operator routine<p>
               -2 failed to allocate memory<p>
               -3 interpolation operator by given name already exists
   @endreturndesc
@@*/
int CCTKi_InterpRegisterOperatorLocal (const char *thorn, 
				       cInterpOperatorLocal operator_local,
				       const char *name)
{
  int handle;
  t_interp_operator *operator;


  /* Check arguments */
  if (operator_local == NULL)
  {
    CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
               "CCTKi_InterpRegisterOperatorLocal: "
	       "NULL pointer passed as interpolation operator routine");
    handle = -1;
  }
  else
  {
    /* Check that the method hasn't already been registered */
    handle = Util_GetHandle (interp_operators, name, (void **) &operator);

    if (handle < 0)
    {
      /* Get a handle for it. */
      operator = (t_interp_operator *) malloc (sizeof (t_interp_operator));
      if (operator)
      {
	operator->implementation = CCTK_ThornImplementation(thorn);
	operator->name = name;
        operator->interp_operator_local = operator_local;
        operator->interp_operator_GV = NULL;
        handle = Util_NewHandle (&interp_operators, name, operator);

        /* Remember how many interpolation operators there are */
        num_interp_operators++;
      }
      else
      {
        CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
                   "CCTKi_InterpRegisterOperatorLocal: "
		   "Couldn't allocate interpolation operator handle");
        handle = -2;
      }
    }
    else if (operator->interp_operator_local == NULL)
    {
      operator->interp_operator_local = operator_local;
    }
    else
    {
      /* Interpolation operator with this name already exists. */
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTKi_InterpRegisterOperatorLocal: "
		  "Operator '%s' already exists",
                  name);
      handle = -3;
    }
  }

  return (handle);
}


 /*@@
   @routine    CCTK_InterpHandle
   @date       July 07 1999
   @author     Thomas Radke
   @desc
               Returns the handle of a given interpolation operator
   @enddesc
   @var        name
   @vdesc      String containing name of interpolation operator
   @vtype      const char *
   @vio        in
   @vcomment
   @endvar

   @returntype int
   @returndesc
               the handle for the operator registered by this name
               or negative otherwise
   @endreturndesc
@@*/
int CCTK_InterpHandle (const char *name)
{
  int handle;


  handle = Util_GetHandle (interp_operators, name, NULL);

#ifdef DEBUG_INTERP
  printf("In CCTK_InterpHandle\n");
  printf("--------------------------\n");
  printf("  Got handle %d for %s\n",handle,name);
#endif

  if (handle < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                "No handle found for interpolation operator '%s'", name);
  }

  return (handle);
}


void CCTK_FCALL cctk_interphandle_
                           (int *handle, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (name)
  *handle = CCTK_InterpHandle (name);
  free (name);
}


 /*@@
   @routine    CCTK_InterpOperator
   @date       December 27 2001
   @author     Gabrielle Allen
   @desc
               Returns the name of a interpolation operator
   @enddesc
   @var        handle
   @vdesc      Handle for interpolation operator
   @vtype      int
   @vio        in
   @vcomment
   @endvar

   @returntype const char *
   @returndesc
   The name of the interpolation operator, or NULL if the handle 
   is invalid
   @endreturndesc
@@*/
const char *CCTK_InterpOperator (int handle)
{
  const char *name=NULL;
  t_interp_operator *operator;

  if (handle < 0)
  {
    CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
                "CCTK_InterpHandle: Handle %d invalid", handle);
  }
  else
  {
    operator = (t_interp_operator *) Util_GetHandledData (interp_operators,
							  handle);
    if (operator)
    {
      name = operator->name;
    }
    else
    {
      CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
		  "CCTK_InterpHandle: Handle %d invalid", handle);
    }
  }

  return name;
}


 /*@@
   @routine    CCTK_InterpGV
   @date       Sun 28 Jan 2001
   @author     Thomas Radke
   @desc
               The general CCTK interpolation routine for grid variables
               Just puts the arguments from the variable argument list
               into arrays and calls the appropriate interpolation operator.
   @enddesc
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        operator_handle
   @vdesc      handle for the interpolation operator
   @vtype      int
   @vio        in
   @endvar
   @var        coord_system_handle
   @vdesc      handle for the coordinate system
   @vtype      int
   @vio        in
   @endvar
   @var        num_points
   @vdesc      number of points to interpolate at
   @vtype      int
   @vio        in
   @endvar
   @var        num_in_array_indices
   @vdesc      number of passed input array indices
   @vtype      int
   @vio        in
   @endvar
   @var        num_out_arrays
   @vdesc      number of passed output arrays
   @vtype      int
   @vio        in
   @vcomment   end of fixed argument list
   @endvar

   @var        interp_coord_arrays
   @vdesc      list of coordinate arrays for points to interpolate at
   @vtype      void [dimensions of coordinate system][num_points]
   @vio        in
   @endvar
   @var        interp_coord_array_types
   @vdesc      types of passed coordinate arrays
   @vtype      int [dimensions of coordinate system]
   @vio        in
   @endvar
   @var        in_array_indices
   @vdesc      list of grid variables (given by their indices) to interpolate
   @vtype      int [num_in_array_indices]
   @vio        in
   @endvar
   @var        out_arrays
   @vdesc      list of output arrays which receive the interpolation results
   @vtype      void * [num_out_arrays]
   @vio        out
   @endvar
   @var        out_array_types
   @vdesc      types of output arrays which receive the interpolation results
   @vtype      int [num_out_arrays]
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return value of the interpolation operator,
               -1 if invalid interpolation operator handle was passed in
               -2 if invalid coordinate system handle was passed in
   @endreturndesc
@@*/
int CCTK_InterpGV (cGH *GH,
                   int operator_handle,
                   int coord_system_handle,
                   int num_points,
                   int num_in_array_indices,
                   int num_out_arrays,
                   ...)
{
  va_list indices;
  int num_dims, retcode;
  const char *coord_system;
  int *in_array_indices, *interp_coord_array_types, *out_array_types;
  const void **interp_coord_arrays;
  void **out_arrays;
  t_interp_operator *operator;


  /* Get the interpolation operator routine and the coordinate system name */
  operator = (t_interp_operator *) Util_GetHandledData (interp_operators,
                                                        operator_handle);
  coord_system = CCTK_CoordSystemName (coord_system_handle);

  if (operator == NULL)
  {
    CCTK_Warn (3, __LINE__, __FILE__, "Cactus",
               "CCTK_InterpGV: Invalid interpolation operator handle passed to CCTK_InterpGV");
    retcode = -1;
  }
  else if (coord_system == NULL)
  {
    CCTK_Warn (3, __LINE__, __FILE__, "Cactus",
               "CCTK_InterpGV: Invalid coordinate system handle passed to CCTK_InterpGV");
    retcode = -2;
  }
  else
  {
    num_dims = CCTK_CoordSystemDim (coord_system);

    interp_coord_arrays = (const void **) malloc (num_dims * sizeof (void *));
    interp_coord_array_types = (int *) malloc (num_dims * sizeof (int));
    in_array_indices    = (int *) malloc (num_in_array_indices * sizeof (int));
    out_arrays          = (void **) malloc (num_out_arrays * sizeof (void *));
    out_array_types     = (int *) malloc (num_out_arrays * sizeof (int));

    /* Fill in the arrays from the variable argument list */
    va_start (indices, num_out_arrays);
    VARARGS_TO_ARRAY (interp_coord_arrays, void *, NOTHING, num_dims, indices);
    VARARGS_TO_ARRAY (interp_coord_array_types, int, NOTHING, num_dims,indices);
    VARARGS_TO_ARRAY (in_array_indices, int, NOTHING, num_in_array_indices,
                      indices);
    VARARGS_TO_ARRAY (out_arrays, void *, NOTHING, num_out_arrays, indices);
    VARARGS_TO_ARRAY (out_array_types, int, NOTHING, num_out_arrays, indices);
    va_end (indices);

    retcode = operator->interp_operator_GV (GH, coord_system, num_points,
                                            num_in_array_indices,num_out_arrays,
                                            interp_coord_arrays,
                                            interp_coord_array_types,
                                            in_array_indices,
                                            out_arrays, out_array_types);

    free (out_array_types);
    free (out_arrays);
    free (in_array_indices);
    free (interp_coord_array_types);
    free (interp_coord_arrays);
  }

  return (retcode);
}

void CCTK_FCALL cctk_interpgv_
                           (int *fortranreturn,
                            cGH *GH,
                            const int *operator_handle,
                            const int *coord_system_handle,
                            const int *num_points,
                            const int *num_in_array_indices,
                            const int *num_out_arrays,
                            ...)
{
  va_list indices;
  int num_dims, retcode;
  const char *coord_system;
  int *in_array_indices, *interp_coord_array_types, *out_array_types;
  const void **interp_coord_arrays;
  void **out_arrays;
  t_interp_operator *operator;


  /* Get the interpolation operator and the coordinate system name */
  operator = (t_interp_operator *) Util_GetHandledData (interp_operators,
                                                        *operator_handle);
  coord_system = CCTK_CoordSystemName (*coord_system_handle);

  if (operator == NULL)
  {
    CCTK_Warn (3, __LINE__, __FILE__, "Cactus",
               "CCTK_InterpGV: "
	       "Invalid interpolation operator handle passed to CCTK_InterpGV");
    retcode = -1;
  }
  else if (coord_system == NULL)
  {
    CCTK_Warn (3, __LINE__, __FILE__, "Cactus",
               "CCTK_InterpGV: Invalid coordinate system handle passed to CCTK_InterpGV");
    retcode = -1;
  }
  else
  {
    num_dims = CCTK_CoordSystemDim (coord_system);
    interp_coord_arrays = (const void **) malloc (num_dims * sizeof (void *));
    interp_coord_array_types = (int *) malloc (num_dims * sizeof (int));
    in_array_indices    = (int *) malloc (*num_in_array_indices * sizeof (int));
    out_arrays          = (void **) malloc (*num_out_arrays * sizeof (void *));
    out_array_types     = (int *) malloc (*num_out_arrays * sizeof (int));

    /* Fill in the arrays from the variable argument list */
    va_start (indices, num_out_arrays);
    VARARGS_TO_ARRAY (interp_coord_arrays, void *, NOTHING, num_dims, indices);
    VARARGS_TO_ARRAY (interp_coord_array_types, int *, *, num_dims, indices);
    VARARGS_TO_ARRAY (in_array_indices, int *, *,*num_in_array_indices,indices);
    VARARGS_TO_ARRAY (out_arrays, void *, NOTHING, *num_out_arrays, indices);
    VARARGS_TO_ARRAY (out_array_types, int *, *, *num_out_arrays, indices);
    va_end (indices);

    retcode = operator->interp_operator_GV (GH, coord_system, *num_points,
                                            *num_in_array_indices,
                                            *num_out_arrays,
                                            interp_coord_arrays,
                                            interp_coord_array_types,
                                            in_array_indices,
                                            out_arrays, out_array_types);

    free (out_array_types);
    free (out_arrays);
    free (in_array_indices);
    free (interp_coord_array_types);
    free (interp_coord_arrays);
  }

  *fortranreturn = retcode;
}


 /*@@
   @routine    CCTK_InterpLocal
   @date       Thu 01 Feb 2001
   @author     Thomas Radke
   @desc
               The general CCTK interpolation routine for a list of local
               arrays.
               Just puts the arguments from the variable argument list
               into arrays and calls the appropriate interpolation operator.
   @enddesc
   @var        GH
   @vdesc      pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        operator_handle
   @vdesc      handle for the interpolation operator
   @vtype      int
   @vio        in
   @endvar
   @var        num_points
   @vdesc      number of points to interpolate at
   @vtype      int
   @vio        in
   @endvar
   @var        num_dims
   @vdesc      number of dimensions of the underlying grid
   @vtype      int
   @vio        in
   @endvar
   @var        num_in_arrays
   @vdesc      number of passed input arrays
   @vtype      int
   @vio        in
   @endvar
   @var        num_out_arrays
   @vdesc      number of passed output arrays
   @vtype      int
   @vio        in
   @vcomment   end of fixed argument list
   @endvar

   @var        dims
   @vdesc      coordinate dimensions of the underlying grid
   @vtype      int [num_dims]
   @vio        in
   @endvar
   @var        coord_arrays
   @vdesc      coordinates of the underlying grid
   @vtype      void * [num_dims]
   @vio        in
   @endvar
   @var        coord_array_types
   @vdesc      CCTK data types of passed coordinate arrays
   @vtype      int [num_dims]
   @vio        in
   @endvar
   @var        interp_coord_arrays
   @vdesc      coordinates of points to interpolate at
   @vtype      void * [num_dims]
   @vio        in
   @endvar
   @var        interp_coord_array_types
   @vdesc      CCTK data types of passed interpolation coordinate arrays
   @vtype      int [num_dims]
   @vio        in
   @endvar
   @var        in_arrays
   @vdesc      list of input arrays to interpolate from
   @vtype      void * [num_in_arrays]
   @vio        in
   @endvar
   @var        in_array_types
   @vdesc      CCTK data types of input arrays to interpolate from
   @vtype      int [num_in_arrays]
   @vio        in
   @endvar
   @var        out_arrays
   @vdesc      list of output arrays to hold the interpolation results
   @vtype      void * [num_out_arrays]
   @vio        out
   @endvar
   @var        out_array_types
   @vdesc      CCTK data types of output arrays
   @vtype      int [num_out_arrays]
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return value of the interpolation operator,
               -1 if invalid interpolation operator handle was passed in
   @endreturndesc
@@*/
int CCTK_InterpLocal (cGH *GH,
                      int operator_handle,
                      int num_points,
                      int num_dims,
                      int num_in_arrays,
                      int num_out_arrays,
                      ...)
{
  va_list indices;
  int retcode;
  int *coord_dims;
  int *coord_array_types, *interp_coord_array_types;
  int *in_array_types, *out_array_types;
  const void **coord_arrays, **interp_coord_arrays, **in_arrays;
  void **out_arrays;
  t_interp_operator *operator;


  /* Get the interpolation operator */
  operator = (t_interp_operator *) Util_GetHandledData (interp_operators,
                                                        operator_handle);
  if (operator == NULL)
  {
    CCTK_Warn (3, __LINE__, __FILE__, "Cactus",
               "CCTK_InterpLocal: Invalid interpolation operator handle");
    retcode = -1;
  }
  else
  {
    coord_dims = (int *) malloc (num_dims * sizeof (int));
    coord_arrays = (const void **) malloc (num_dims * sizeof (void *));
    coord_array_types = (int *) malloc (num_dims * sizeof (int));
    interp_coord_arrays = (const void **) malloc (num_dims * sizeof (void *));
    interp_coord_array_types = (int *) malloc (num_dims * sizeof (int));
    in_arrays = (const void **) malloc (num_in_arrays * sizeof (void *));
    in_array_types = (int *) malloc (num_in_arrays * sizeof (int));
    out_arrays = (void **) malloc (num_out_arrays * sizeof (void *));
    out_array_types = (int *) malloc (num_out_arrays * sizeof (int));

    /* Fill in the arrays from the variable argument list */
    va_start (indices, num_out_arrays);
    VARARGS_TO_ARRAY (coord_dims, int, NOTHING, num_dims, indices);
    VARARGS_TO_ARRAY (coord_arrays, void *, NOTHING, num_dims, indices);
    VARARGS_TO_ARRAY (coord_array_types, int, NOTHING, num_dims, indices);
    VARARGS_TO_ARRAY (interp_coord_arrays, void *, NOTHING, num_dims, indices);
    VARARGS_TO_ARRAY (interp_coord_array_types, int, NOTHING, num_dims,indices);
    VARARGS_TO_ARRAY (in_arrays, void *, NOTHING, num_in_arrays, indices);
    VARARGS_TO_ARRAY (in_array_types, int, NOTHING, num_in_arrays, indices);
    VARARGS_TO_ARRAY (out_arrays, void *, NOTHING, num_out_arrays, indices);
    VARARGS_TO_ARRAY (out_array_types, int, NOTHING, num_out_arrays, indices);
    va_end (indices);

    retcode = operator->interp_operator_local (GH, num_points, num_dims,
                                               num_in_arrays, num_out_arrays,
                                               coord_dims,
                                               coord_arrays, coord_array_types,
                                               interp_coord_arrays,
                                               interp_coord_array_types,
                                               in_arrays, in_array_types,
                                               out_arrays, out_array_types);

    free (out_array_types);
    free (out_arrays);
    free (in_array_types);
    free (in_arrays);
    free (interp_coord_array_types);
    free (interp_coord_arrays);
    free (coord_array_types);
    free (coord_arrays);
    free (coord_dims);
  }

  return (retcode);
}


void CCTK_FCALL cctk_interplocal_
                           (int *fortranreturn,
                            cGH *GH,
                            const int *operator_handle,
                            const int *num_points,
                            const int *num_dims,
                            const int *num_in_arrays,
                            const int *num_out_arrays,
                            ...)
{
  va_list indices;
  int retcode;
  int *coord_dims;
  int *coord_array_types, *interp_coord_array_types;
  int *in_array_types, *out_array_types;
  const void **coord_arrays, **interp_coord_arrays, **in_arrays;
  void **out_arrays;
  t_interp_operator *operator;


  /* Get the interpolation operator */
  operator = (t_interp_operator *) Util_GetHandledData (interp_operators,
                                                        *operator_handle);
  if (operator == NULL)
  {
    CCTK_Warn (3, __LINE__, __FILE__, "Cactus",
               "CCTK_InterpLocal: Invalid interpolation operator handle");
    retcode = -1;
  }
  else
  {
    coord_dims = (int *) malloc (*num_dims * sizeof (int));
    coord_arrays = (const void **) malloc (*num_dims * sizeof (void *));
    coord_array_types = (int *) malloc (*num_dims * sizeof (int));
    interp_coord_arrays = (const void **) malloc (*num_dims * sizeof (void *));
    interp_coord_array_types = (int *) malloc (*num_dims * sizeof (int));
    in_arrays = (const void **) malloc (*num_in_arrays * sizeof (void *));
    in_array_types = (int *) malloc (*num_in_arrays * sizeof (int));
    out_arrays = (void **) malloc (*num_out_arrays * sizeof (void *));
    out_array_types = (int *) malloc (*num_out_arrays * sizeof (int));

    /* Fill in the arrays from the variable argument list */
    va_start (indices, num_out_arrays);
    VARARGS_TO_ARRAY (coord_dims, int *, *, *num_dims, indices);
    VARARGS_TO_ARRAY (coord_arrays, void *, NOTHING, *num_dims, indices);
    VARARGS_TO_ARRAY (coord_array_types, int *, *, *num_dims, indices);
    VARARGS_TO_ARRAY (interp_coord_arrays, void *, NOTHING, *num_dims, indices);
    VARARGS_TO_ARRAY (interp_coord_array_types, int *, *, *num_dims, indices);
    VARARGS_TO_ARRAY (in_arrays, void *, NOTHING, *num_in_arrays, indices);
    VARARGS_TO_ARRAY (in_array_types, int *, *, *num_in_arrays, indices);
    VARARGS_TO_ARRAY (out_arrays, void *, NOTHING, *num_out_arrays, indices);
    VARARGS_TO_ARRAY (out_array_types, int *, *, *num_out_arrays, indices);
    va_end (indices);

    retcode = operator->interp_operator_local (GH, *num_points, *num_dims,
                                               *num_in_arrays, *num_out_arrays,
                                               coord_dims,
                                               coord_arrays, coord_array_types,
                                               interp_coord_arrays,
                                               interp_coord_array_types,
                                               in_arrays, in_array_types,
                                               out_arrays, out_array_types);

    free (out_array_types);
    free (out_arrays);
    free (in_array_types);
    free (in_arrays);
    free (interp_coord_array_types);
    free (interp_coord_arrays);
    free (coord_array_types);
    free (coord_arrays);
    free (coord_dims);
  }

  *fortranreturn = retcode;
}

 /*@@
   @routine    CCTK_NumInterpOperators
   @date       Mon Oct 22 2001
   @author     Gabrielle Allen
   @desc
               The number of interp operators registered
   @enddesc
   @returntype int
   @returndesc
               number of interpolation operators
   @endreturndesc
@@*/

int CCTK_NumInterpOperators()
{
  return num_interp_operators;
}

 /*@@
   @routine    CCTK_InterpOperatorImplementation
   @date       Mon Oct 22 2001
   @author     Gabrielle Allen
   @desc
   Provide the implementation which provides an interpolation operator
   @enddesc
   @returntype int
   @returndesc
               Implementation which supplied the interpolation operator
   @endreturndesc
@@*/

const char *CCTK_InterpOperatorImplementation(int handle)
{
  t_interp_operator *operator;
  const char *imp=NULL;

  operator = (t_interp_operator *) 
    Util_GetHandledData (interp_operators, handle);
  
  if (operator)
  {
    imp = operator->implementation;
  }

  return imp;
}
