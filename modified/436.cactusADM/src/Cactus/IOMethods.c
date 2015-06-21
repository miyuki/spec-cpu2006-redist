#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      IOMethods.c
   @date      Mon Jan 8 1999
   @author    Gabrielle Allen
   @desc
              Functions to deal with I/O registration
   @enddesc
   @version   $Id: IOMethods.c,v 1.36 2001/12/30 13:12:06 tradke Exp $
 @@*/

/* #define DEBUG_IO 1 */

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_ActiveThorns.h"
#include "cctk_IOMethods.h"
#include "cctk_Groups.h"
#include "StoreHandledData.h"
#include "cctk_FortranString.h"
#include "cctk_WarnLevel.h"
#include "cctk_IO.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/IO/IOMethods.c,v 1.36 2001/12/30 13:12:06 tradke Exp $";

CCTK_FILEVERSION (IO_IOMethods_c)

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/* Dummy registerable function prototypes. */
static int DummyOutputGH (const cGH *GH);
static int DummyOutputVarAs (const cGH *GH,
                             const char *var,
                             const char *alias);
static int DummyTriggerOutput (const cGH *GH, int var);
static int DummyTimeToOutput (const cGH *GH, int var);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

int CactusDefaultOutputGH (const cGH *GH);
int CactusDefaultOutputVarAsByMethod (const cGH *GH,
                                      const char *var,
                                      const char *methodname,
                                      const char *alias);

void CCTK_FCALL cctk_outputvarasbymethod_
                           (int *ierr, const cGH *GH, THREE_FORTSTRING_ARG);
void CCTK_FCALL cctk_outputvarbymethod_
                           (int *ierr, const cGH *GH, TWO_FORTSTRING_ARG);
int CCTKi_TriggerSaysGo (const cGH *GH, int variable);
int CCTKi_TriggerAction (void *GH, int variable);


/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/* Local data holding info on I/O methods.*/
static cHandledData *IOMethods = NULL;
static int num_methods = 0;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

/************************************************************************
 *
 *  Registration routines functions provided by I/O methods
 *
 ************************************************************************/

 /*@@
   @routine    CCTK_RegisterIOMethod
   @date       Wed Feb  3 13:33:09 1999
   @author     Tom Goodale
   @desc
               Registers a new I/O method.
   @enddesc
   @calls      Util_GetHandle
               Util_NewHandle

   @var        name
   @vdesc      The name of the method for I/O
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               positive handle for I/O method, or<BR>
               -1 if method with this name already registered,<BR>
               -2 if memory allocation failed
   @endreturndesc
@@*/
int CCTKi_RegisterIOMethod (const char *thorn, const char *name)
{
  int handle;
  struct IOMethod *new_method;


  /* Check that the method hasn't already been registered */
  handle = Util_GetHandle (IOMethods, name, NULL);

  if (handle < 0)
  {
    /* New extension. */
    new_method = (struct IOMethod *) malloc (sizeof (struct IOMethod));

    if (new_method)
    {
      /* Get a handle for it. */
      handle = Util_NewHandle (&IOMethods, name, new_method);

      /* Initialise the I/O method structure with dummy routines */
      new_method->implementation = CCTK_ThornImplementation (thorn);
      new_method->name           = name;
      new_method->OutputGH       = DummyOutputGH;
      new_method->OutputVarAs    = DummyOutputVarAs;
      new_method->TriggerOutput  = DummyTriggerOutput;
      new_method->TimeToOutput   = DummyTimeToOutput;

      /* Remember how many methods there are */
      num_methods++;
    }
    else
    {
      /* Memory failure. */
      handle = -2;
    }
  }
  else
  {
    /* Method already exists. */
    handle = -1;
  }

  return handle;
}


 /*@@
   @routine    CCTK_RegisterIOMethodOutputGH
   @date       Wed Feb  3 13:34:12 1999
   @author     Tom Goodale
   @desc
               Registers a function to register a routine for OutputGH.
   @enddesc
   @calls      Util_GetHandledData

   @var        handle
   @vdesc      identifies the method in the stored data
   @vtype      int
   @vio        in
   @endvar
   @var        OutputGH
   @vdesc      reference to the routine implementing OutputGH
   @vtype      int (*) (const cGH *)
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0  if function was successfully registered, or<BR>
               -1 if function did not register
   @endreturndesc
@@*/
int CCTK_RegisterIOMethodOutputGH (int handle, int (*OutputGH) (const cGH *GH))
{
  struct IOMethod *method;


  /* Get the method. */
  method = Util_GetHandledData (IOMethods, handle);
  if (method)
  {
    method->OutputGH = OutputGH;
  }

  return (method ? 0 : -1);
}


 /*@@
   @routine    CCTK_RegisterIOMethodOutputVarAs
   @date       Wed Feb  3 13:34:12 1999
   @author     Tom Goodale
   @desc
               Registers a function to register a routine for OutputVarAs
   @enddesc
   @calls      Util_GetHandledData

   @var        handle
   @vdesc      identifies the method in the stored data
   @vtype      int
   @vio        in
   @endvar
   @var        OutputVarAs
   @vdesc      reference to the routine implementing OutputVarAs
   @vtype      int (*) (const cGH *, const char *, const char *)
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 if function was successfully registered, or<BR>
               -1 if function did not register
   @endreturndesc
@@*/
int CCTK_RegisterIOMethodOutputVarAs (int handle,
                                      int (*OutputVarAs) (const cGH *GH,
                                                          const char *vname,
                                                          const char *alias))
{
  struct IOMethod *method;


  /* Get the extension. */
  method = Util_GetHandledData (IOMethods, handle);
  if (method)
  {
    method->OutputVarAs = OutputVarAs;
  }

  return (method ? 0 : -1);
}


 /*@@
   @routine    CCTK_RegisterIOMethodTriggerOutput
   @date       Wed Feb  3 13:34:12 1999
   @author     Tom Goodale
   @desc
               Registers a function to register a routine for TriggerOutput
   @enddesc
   @calls      Util_GetHandledData

   @var        handle
   @vdesc      identifies the method in the stored data
   @vtype      int
   @vio        in
   @endvar
   @var        TriggerOutput
   @vdesc      reference to the routine implementing TriggerOutput
   @vtype      int (*) (const cGH *, int)
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 if function was successfully registered, or<BR>
               -1 if function did not register
   @endreturndesc
@@*/
int CCTK_RegisterIOMethodTriggerOutput (int handle,
                                        int (*TriggerOutput) (const cGH *GH,
                                                              int vindex))
{
  struct IOMethod *method;


  /* Get the extension. */
  method = Util_GetHandledData (IOMethods, handle);
  if (method)
  {
    method->TriggerOutput = TriggerOutput;
  }

  return (method != NULL);
}


 /*@@
   @routine    CCTK_RegisterIOMethodTimeToOutput
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Registers a function to register a routine for TimeToOutput
   @enddesc
   @calls      Util_GetHandledData

   @var        handle
   @vdesc      identifies the method in the stored data
   @vtype      int
   @vio        in
   @endvar
   @var        TimeToOutput
   @vdesc      reference to the routine implementing TimeToOutput
   @vtype      int (*) (const cGH *, int)
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 if function was successfully registered, or<BR>
               -1 if function did not register
   @endreturndesc
@@*/
int CCTK_RegisterIOMethodTimeToOutput (int handle,
                                       int (*TimeToOutput) (const cGH *GH,
                                                            int vindex))
{
  struct IOMethod *method;


  /* Get the extension. */
  method = Util_GetHandledData (IOMethods, handle);
  if (method)
  {
    method->TimeToOutput = TimeToOutput;
  }

  return (method ? 0 : -1);
}

/************************************************************************
 *
 *  More I/O functions which perhaps should be overloadable
 *
 ************************************************************************/


 /*@@
   @routine    CCTK_OutputVarAs
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Loops over all methods for a given variable,
               calling each methods OutputVarAs routine
   @enddesc
   @calls      Util_GetHandledData
               IOMethod->OutputVarAs

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        var
   @vdesc      Name of variable to output
   @vtype      const char *
   @vio        in
   @endvar
   @var        alias
   @vdesc      Name variable should be output as
   @vtype      const char *alias
   @vio        in
   @vcomment   Note that the I/O method may not use this feature
   @endvar

   @returntype int
   @returndesc
               positive for the number of I/O methods which successfully
               did output for var, or<BR>
               -1 if no I/O methods were found
   @endreturndesc
@@*/
int CCTK_OutputVarAs (const cGH *GH, const char *var, const char *alias)
{
  int handle, retval;
  struct IOMethod *method;


  if (num_methods > 0)
  {
    for (handle = retval = 0; handle < num_methods; handle++)
    {
      method = (struct IOMethod *) Util_GetHandledData (IOMethods, handle);
      if (method && method->OutputVarAs (GH, var, alias) == 0)
      {
        retval++;
      }
    }
  }
  else
  {
    retval = -1;
  }

  return (retval);
}


 /*@@
   @routine    CCTK_OutputVar
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Outputs a variable using all methods and no alias
   @enddesc
   @calls      CCTK_OutputVarAs

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        var
   @vdesc      Name of variable to output
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_OutputVarAs
   @endreturndesc
@@*/
int CCTK_OutputVar (const cGH *GH, const char *var)
{
  int retval;


  retval = CCTK_OutputVarAs (GH, var, var);

  return (retval);
}


 /*@@
   @routine    CCTK_OutputVarByMethod
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Outputs a variable using one given method and no alias
   @enddesc
   @calls      CCTK_OutputVarAsByMethod

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        var
   @vdesc      Name of variable to output
   @vtype      const char *
   @vio        in
   @vcomment   This is also the name the variable will be output as
   @endvar
   @var        method
   @vdesc      Name of method to use for output
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_OutputVarAsByMethod
   @endreturndesc
@@*/
int CCTK_OutputVarByMethod (const cGH *GH, const char *var, const char *method)
{
  int retval;


  retval = CCTK_OutputVarAsByMethod (GH, var, method, var);

  return (retval);
}

void CCTK_FCALL cctk_outputvarbymethod_
                           (int *ierr, const cGH *GH, TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (var, method);
  *ierr = CCTK_OutputVarByMethod (GH, var, method);
  free (var);
  free (method);
}



 /*@@
   @routine    CCTK_NumIOMethods
   @date       Sat Oct 20 2001
   @author     Gabrielle Allen
   @desc
               The number of IO methods registered
   @enddesc
   @returntype int
   @returndesc
               number of IO methods
   @endreturndesc
@@*/

int CCTK_NumIOMethods (void)
{
  return (num_methods);
}


 /*@@
   @routine    CCTK_IOMethodImplementation
   @date       Sat Oct 20 2001
   @author     Gabrielle Allen
   @desc
               Provide the implementation which registered a method
   @enddesc
   @var        handle
   @vdesc      handle of I/O method
   @vtype      int
   @vio        in
   @endvar

   @returntype const char *
   @returndesc
               Implementation which registered this method
   @endreturndesc
@@*/
const char *CCTK_IOMethodImplementation (int handle)
{
  struct IOMethod *method;


  method = (struct IOMethod *) Util_GetHandledData (IOMethods, handle);

  return (method ? method->implementation : NULL);
}


 /*@@
   @routine    CCTK_IOMethod
   @date       Thu Dec 27 2001
   @author     Gabrielle Allen
   @desc
               Provide the IO Method name
   @enddesc
   @var        handle
   @vdesc      handle of I/O method
   @vtype      int
   @vio        in
   @endvar

   @returntype const char *
   @returndesc
               IO Method
   @endreturndesc
@@*/
const char *CCTK_IOMethod (int handle)
{
  struct IOMethod *method;

  method = (struct IOMethod *) Util_GetHandledData (IOMethods, handle);

  return (method ? method->name : NULL);
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/************************************************************************
 *
 *  Dummy functions.  Registered if no real function registered.
 *
 ************************************************************************/

 /*@@
   @routine    DummyOutputGH
   @date       Wed Feb  3 13:36:52 1999
   @author     Tom Goodale
   @desc
               Dummy for OutputGH functions.
   @enddesc
@@*/
static int DummyOutputGH (const cGH *GH)
{
  GH = GH;
  return 0;
}


 /*@@
   @routine    DummyTimeToOutput
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Dummy for TimeToOutput function.
   @enddesc
@@*/
static int DummyTimeToOutput (const cGH *GH, int var)
{
  GH = GH;
  var = var;
  return 0;
}


 /*@@
   @routine    DummyOutputVarAs
   @date       Wed Feb  3 13:37:31 1999
   @author     Tom Goodale
   @desc
               Dummy for OutputVarAs functions.
   @enddesc
@@*/
static int DummyOutputVarAs (const cGH *GH,
                             const char *var,
                             const char *alias)
{
  GH = GH;
  var = var;
  alias = alias;
  return 0;
}


static int DummyTriggerOutput (const cGH *GH, int var)
{
  GH = GH;
  var = var;
  return 0;
}


/************************************************************************
 *
 *  Default functions for overloadable functions provided by I/O methods
 *
 ************************************************************************/

 /*@@
   @routine    CactusDefaultOutputGH
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Loops over all methods calling each methods OutputGH
               routine
   @enddesc
   @calls      IOMethod->OutputGH

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               >= 0 for the total number of variables for which output was
                    was done by all I/O methods, or<BR>
               -1   if no I/O methods were found
   @endreturndesc
@@*/
int CactusDefaultOutputGH (const cGH *GH)
{
  int handle, retval;
  struct IOMethod *method;


  if (num_methods > 0)
  {
    for (handle = retval = 0; handle < num_methods; handle++)
    {
      method = (struct IOMethod *) Util_GetHandledData (IOMethods, handle);
      if (method)
      {
        retval += method->OutputGH (GH);
      }
    }
  }
  else
  {
    retval = -1;
  }

  return (retval);
}


 /*@@
   @routine    CactusDefaultOutputVarAsByMethod
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Outputs one chosen variable by one chosen method
   @enddesc

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        var
   @vdesc      Name of variable to output
   @vtype      const char *
   @vio        in
   @endvar
   @var        method
   @vdesc      Name of method to use for output
   @vtype      const char *
   @vio        in
   @endvar
   @var        alias
   @vdesc      Name variable should be output as
   @vtype      const char *alias
   @vio        in
   @vcomment   Note that the I/O method may not use this feature
   @endvar

   @returntype int
   @returndesc
               return code of I/O method's OutputVarAs() routine, or<BR>
               -1 if no such I/O method was found
   @endreturndesc
@@*/
int CactusDefaultOutputVarAsByMethod (const cGH *GH,
                                      const char *var,
                                      const char *methodname,
                                      const char *alias)
{
  int retval;
  struct IOMethod *method;


  Util_GetHandle (IOMethods, methodname, (void **) &method);
  if (method)
  {
    retval = method->OutputVarAs (GH, var, alias);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CactusDefaultOutputVarAsByMethod: I/O method '%s' not found "
                "for output of variable '%s'", methodname, var);
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL cctk_outputvarasbymethod_
                           (int *ierr, const cGH *GH, THREE_FORTSTRING_ARG)
{
  THREE_FORTSTRING_CREATE (var, methodname, alias);

  *ierr = CCTK_OutputVarAsByMethod (GH, var, methodname, alias);

  free (var);
  free (methodname);
  free (alias);
}



/************************************************************************
 *
 *  These routines are here because they need to get at IOMethods
 *
 ************************************************************************/

 /*@@
   @routine    CCTKi_TriggerSaysGo
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Checks if a trigger registered for a routine is
               due for output by any I/O method.
   @enddesc
   @calls      Util_GetHandledData
               IOMethod->TimeToOutput

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        variable
   @vdesc      GH variable index
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 if trigger says don't output me this iteration, or<BR>
               1 if trigger says output me this iteration
   @endreturndesc
@@*/
int CCTKi_TriggerSaysGo (const cGH *GH, int variable)
{
  int handle;
  struct IOMethod *method;


  /* Loop over all registered I/O methods */
  for (handle = 0; handle < num_methods; handle++)
  {
    /* Check if it is time to output this variable for this method */
    method = (struct IOMethod *) Util_GetHandledData (IOMethods, handle);
    if (method && method->TimeToOutput (GH, variable))
    {
      return (1);
    }
  }
  return (0);
}


 /*@@
   @routine    CCTKi_TriggerAction
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Essentially the same as CCTKi_TriggerSaysGo,
               but now calls each I/O method for which it is
               time to output the trigger
   @enddesc
   @calls      Util_GetHandledData
               CCTK_FullName
               CCTK_VarName
               IOMethod->TimeToOutput
               IOMethod->OutputVarAs

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        variable
   @vdesc      GH variable index
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0    = This should never happen, since at least
                      one I/O method should have been found by
                      CCTKi_TriggerSaysGo<BR>
               >0   = Number of I/O methods called for output for
                      this variable
   @endreturndesc
@@*/
int CCTKi_TriggerAction (void *GH, int variable)
{
  int handle;
  int nmethods;
  struct IOMethod *method;


  /* Count the number of methods used */
  nmethods = 0;

  /* Loop over all registered I/O methods */
  for (handle = 0; handle < num_methods; handle++)
  {
    /* Check if it is time to output this variable for this method */
    method = (struct IOMethod *) Util_GetHandledData (IOMethods, handle);
    if (method && method->TimeToOutput (GH, variable))
    {
      /* And if so do call the output routine for the method */
      method->TriggerOutput (GH, variable);
      nmethods++;
    }
  }

  return (nmethods);
}
