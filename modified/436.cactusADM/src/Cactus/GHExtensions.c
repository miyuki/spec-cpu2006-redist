#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      GHExtensions.c
   @date      Fri Jan 15 13:22:47 1999
   @author    Tom Goodale
   @desc
   Functions to deal with GH extensions
   @enddesc
   @version $Header: /cactus/Cactus/src/main/GHExtensions.c,v 1.27 2001/07/04 13:20:29 tradke Exp $
 @@*/

/*#define DEBUG*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_GHExtensions.h"
#include "StoreHandledData.h"
#include "cctk_WarnLevel.h"
#include "cctk_Schedule.h"
#include "cctki_GHExtensions.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/GHExtensions.c,v 1.27 2001/07/04 13:20:29 tradke Exp $";

CCTK_FILEVERSION(main_GHExtensions_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/* The GH extension structure.
 * This contains pointers to functions which the extension requires.
 *
 * To add a new function, you must:
 *
 *  a) Add it to this structure
 *  b) Initialise it to NULL in CCTK_RegisterGHExtension
 *  c) Check its value in CheckAllExtensionsSetup
 *  d) Provide a dummy function for CheckAllExtensionsSetup to use
 *  e) Provide a registration function.
 *  f) Add a prototype for the registration function to cctk_GHExtensions.h
 */
struct GHExtension
{
  void *(*SetupGH)(tFleshConfig *, int, cGH *);
  int    (*InitGH)(cGH *);
  int    (*ScheduleTraverseGH)(cGH *, const char *);
};


/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/* Function which checks that all the functions on all extensions have been
 * filled in.
 */
static int CheckAllExtensionsSetup(void);

/* Dummy function prototypes. */

static void *DummySetupGH(tFleshConfig *config,
                          int convergence_level,
                          cGH *GH);
static int DummyInitGH(cGH *GH);
static int DummyScheduleTraverseGH(cGH *GH,
                                   const char *where);


/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/
void CCTK_FCALL cctk_ghextensionhandle_
                           (int *handle, ONE_FORTSTRING_ARG);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/* Local data holding info on extensions..*/

static cHandledData *GHExtensions = NULL;
static int num_extensions = 0;


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/


/**************************************************************************
 **************************************************************************
 **************************************************************************/


 /*@@
   @routine    CCTK_RegisterGHExtension
   @date       Wed Feb  3 13:33:09 1999
   @author     Tom Goodale
   @desc
   Registers a new GH extension.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     name
   @vdesc   Name of the extension
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0+ - the handle
   -1 - extension already exists
   -2 - out of memory
   @endreturndesc
@@*/
int CCTK_RegisterGHExtension(const char *name)
{

  int handle;

  struct GHExtension *new_extension;

  /* Check that the extension hasn't already been registered */
  handle = Util_GetHandle(GHExtensions, name, NULL);

  if(handle < 0)
  {
    /* New extension. */
    new_extension = (struct GHExtension *)malloc(sizeof(struct GHExtension));

    if(new_extension)
    {
      /* Get a handle for it. */
      handle = Util_NewHandle(&GHExtensions, name, new_extension);

#ifdef DEBUG
      printf("CCTK_RegisterGHExtension: Extension %s gets handle %d\n",name,handle);
#endif

      /* Initialise the extension structure. */
      new_extension->InitGH = NULL;
      new_extension->SetupGH = NULL;
      new_extension->ScheduleTraverseGH = NULL;

      /* Remember how many extensions there are */
      num_extensions++;
    }
    else
    {
      /* Memory failure. */
      handle = -2;
    }
  }
  else
  {
    /* Extension already exists. */
    handle = -1;
  }

  return handle;
}

 /*@@
   @routine    CCTK_UnregisterGHExtension
   @date       Tue May 09 2000
   @author     Thomas Radke
   @desc
   Unregisters a GH extension.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var        name
   @vdesc      The name of the GH extension to unregister
   @vtype      const char *
   @vio        in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0  - success
   -1 - failure
   @endreturndesc
@@*/
int CCTK_UnregisterGHExtension(const char *name)
{
  int handle, ret_val = -1;
  void *extension;

  /* Check that the extension exists */
  handle = Util_GetHandle(GHExtensions, name, &extension);

  if(handle >= 0)
  {
    /* Delete extension handle and free its associated structure */
    if (Util_DeleteHandle(GHExtensions, handle) == 0)
    {
      free (extension);
      /* Remember how many extensions there are left */
      num_extensions--;
      ret_val = 0;

#ifdef DEBUG
      printf("CCTK_UnregisterGHExtension: unregistered extension %s with "
             "handle %d\n", name, handle);
#endif

    }
  }

  return ret_val;
}


/***************************************************************************
 *
 *          Function Registration Routines.
 *
 ***************************************************************************/

 /*@@
   @routine    CCTK_RegisterGHExtensionSetupGH
   @date       Wed Feb  3 13:34:12 1999
   @author     Tom Goodale
   @desc
   Registers a function to setup a GH extension.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     handle
   @vdesc   The extension handle
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     func
   @vdesc   the function used to setup the GH extension
   @vtype   void *(*)(tFleshConfig *, int, cGH *)
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - success
   0 - extension doesn't exist
   @endreturndesc
@@*/
int CCTK_RegisterGHExtensionSetupGH(int handle,
                                    void *(*func)(tFleshConfig *, int, cGH *))
{
  int return_code;
  struct GHExtension *extension;

  /* Get the extension. */
  extension = Util_GetHandledData(GHExtensions, handle);

  if(extension)
  {
    extension->SetupGH = func;
    return_code = 1;
  }
  else
  {
    return_code = 0;
  }

  return return_code;
}

 /*@@
   @routine    CCTK_RegisterGHExtensionInitGH
   @date       Wed Feb  3 13:33:36 1999
   @author     Tom Goodale
   @desc
   Registers a GH extension initialisation routine.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     handle
   @vdesc   The extension handle
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     func
   @vdesc   the function used to initialise the GH extension
   @vtype   void *(*)(cGH *)
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - success
   0 - extension doesn't exist
   @endreturndesc
@@*/
int CCTK_RegisterGHExtensionInitGH(int handle,
                                   int (*func)(cGH *))
{
  int return_code;
  struct GHExtension *extension;

  /* Get the extension. */
  extension = Util_GetHandledData(GHExtensions, handle);

  if(extension)
  {
    extension->InitGH = func;
    return_code = 1;
  }
  else
  {
    return_code = 0;
  }

  return return_code;
}

 /*@@
   @routine    CCTK_RegisterGHExtensionScheduleTraverseGH
   @date       Thu Jan 27 14:37:09 2000
   @author     Tom Goodale
   @desc
   Registers a GH extension Schedule traversal routine routine.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     handle
   @vdesc   The extension handle
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     func
   @vdesc   the function used to perform a schedule traverse
   @vtype   void *(*)(cGH *, const char *)
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - success
   0 - extension doesn't exist
   @endreturndesc
@@*/
int CCTK_RegisterGHExtensionScheduleTraverseGH(int handle,
                                               int (*func)(cGH *, const char *))
{
  int return_code;
  struct GHExtension *extension;

  /* Get the extension. */
  extension = Util_GetHandledData(GHExtensions, handle);

  if(extension)
  {
    extension->ScheduleTraverseGH = func;
    return_code = 1;
  }
  else
  {
    return_code = 0;
  }

  return return_code;
}


/***************************************************************************
 *
 *          Function Calling Routines.
 *
 ***************************************************************************/

 /*@@
   @routine    CCTKi_SetupGHExtensions
   @date       Wed Feb  3 13:32:26 1999
   @author     Tom Goodale
   @desc
   Sets up all registered GH extensions on a GH.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     config
   @vdesc   Flesh config data
   @vtype   tFleshConfig
   @vio     in
   @vcomment

   @endvar
   @var     convergence_level
   @vdesc   The convergence level
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     GH
   @vdesc   the cctk GH
   @vtype   cGH *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0 - success
   1 - memory failure
   2 - not all extensions properly setup
   @endreturndesc
@@*/
int CCTKi_SetupGHExtensions(tFleshConfig *config,
                           int convergence_level,
                           cGH *GH)
{
  int return_code;
  int handle;
  struct GHExtension *extension;

  if(CheckAllExtensionsSetup())
  {
    /* Create GHExtension array on the GH. */

    if(num_extensions)
    {
      GH->extensions = (void **)malloc(num_extensions*sizeof(void *));
    }
    else
    {
      GH->extensions = NULL;
    }

    if(GH->extensions || ! num_extensions)
    {
      for(handle = 0; handle < num_extensions; handle++)
      {
        /* Call the SetupGH routines for each extension. */
        extension =  (struct GHExtension *)Util_GetHandledData(GHExtensions, handle);
        GH->extensions[handle] = extension->SetupGH(config,
                                                    convergence_level,
                                                    GH);
#ifdef DEBUG
        printf("CCTKi_SetupGHExtensions: Set up extension for handle %d\n",handle);
#endif
      }
      return_code = 0;
    }
    else
    {
      return_code = 1;
    }
  }
  else
  {
    return_code = 2;
  }

  return return_code;
}

 /*@@
   @routine    CCTKi_InitGHExtensions
   @date       Wed Feb  3 14:12:18 1999
   @author     Tom Goodale
   @desc
   Calls the initialisation routine for a GH extension.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     GH
   @vdesc   The cctk GH
   @vtype   cGH *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
int CCTKi_InitGHExtensions(cGH *GH)
{
  int handle;
  struct GHExtension *extension;

  for(handle = 0; handle < num_extensions; handle++)
  {
    extension =  (struct GHExtension *)Util_GetHandledData(GHExtensions, handle);
    if(extension)
    {
      extension->InitGH(GH);
    }
  }

  return 0;
}

 /*@@
   @routine    CCTKi_ScheduleTraverseGHExtensions
   @date       Thu Jan 27 14:47:06 2000
   @author     Tom Goodale
   @desc
   Calls the routines which an extension needs called at a schedule traversal.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     GH
   @vdesc   The cctk GH
   @vtype   cGH *
   @vio     in
   @vcomment

   @endvar
   @var     where
   @vdesc   Schedule point to traverse
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   return code of @seeroutine CCTK_ScheduleTraverse, or
   0 - success
   @endreturndesc
@@*/
int CCTKi_ScheduleTraverseGHExtensions(cGH *GH,
                                       const char *where)
{
  int handle, retval;
  struct GHExtension *extension;

  if (num_extensions <= 0)
  {
    retval = CCTK_ScheduleTraverse(where, GH, NULL);
  }
  else
  {
    for(handle = 0; handle < num_extensions; handle++)
    {
      extension = (struct GHExtension *)Util_GetHandledData(GHExtensions, handle);
      if(extension)
      {
        extension->ScheduleTraverseGH(GH, where);
      }
    }
    retval = 0;
  }

  return (retval);
}

/************************************************************************
 *
 *                      Query functions.
 *
 ************************************************************************/

 /*@@
   @routine    CCTK_GHExtensionHandle
   @date       Tue Feb  9 18:23:41 1999
   @author     Tom Goodale
   @desc
   Gets the handle to the GH extension.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     name
   @vdesc   Name of the GH extension
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   The GH extension handle
   @endreturndesc
@@*/
int CCTK_GHExtensionHandle(const char *name)
{
  return Util_GetHandle(GHExtensions, name, NULL);
}

void CCTK_FCALL cctk_ghextensionhandle_
                           (int *handle, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (name)
  *handle = CCTK_GHExtensionHandle (name);
  free (name);
}


 /*@@
   @routine    CCTK_GHExtension
   @date       Sun Oct 8 2000
   @author     Thomas Radke
   @desc
               Gets the pointer to the GH extension.
   @enddesc
   @calls      Util_GetHandle

   @var        GH
   @vdesc      The cctk GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        name
   @vdesc      Name of the GH extension
   @vtype      const char *
   @vio        in
   @endvar

   @returntype void *
   @returndesc
               The GH extension,
               or NULL if no extension was registered under "name"
   @endreturndesc
@@*/
void *CCTK_GHExtension(const cGH *GH, const char *name)
{
  int handle;

  handle = Util_GetHandle(GHExtensions, name, NULL);

  return ((handle >= 0 && GH->extensions) ? GH->extensions[handle] : NULL);
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/***************************************************************************
 *
 *                 Checking routines
 *
 ***************************************************************************/


 /*@@
   @routine    CheckAllExtensionsSetup
   @date       Wed Feb  3 13:34:58 1999
   @author     Tom Goodale
   @desc
   Checks the state of all extensions.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @returntype int
   @returndesc
   1 - all ok
   @endreturndesc
@@*/
static int CheckAllExtensionsSetup(void)
{
  int return_code;
  int handle;
  struct GHExtension *extension;

  return_code = 1;

  /* Check all registered extensions. */
  for(handle = 0; handle < num_extensions; handle++)
  {
    extension =  (struct GHExtension *)Util_GetHandledData(GHExtensions, handle);
    if(extension)
    {
      /* Check that each function has been registered.
       * Print a warning if not, and then register a dummy function.
       */

      /* SetupGH */
      if(!extension->SetupGH)
      {
        CCTK_VWarn(4,__LINE__,__FILE__,"Cactus",
                   "GH Extension '%s' has not registered a SetupGH routine",
                   Util_GetHandleName(GHExtensions, handle));
        extension->SetupGH=DummySetupGH;
      }

      /*  InitGH */
      if(!extension->InitGH)
      {
        CCTK_VWarn(4,__LINE__,__FILE__,"Cactus",
                   "GH Extension '%s' has not registered a InitGH routine",
                   Util_GetHandleName(GHExtensions, handle));
        extension->InitGH=DummyInitGH;
      }

      /* ScheduleTraverse */
      if(!extension->ScheduleTraverseGH)
      {
        CCTK_VWarn(4,__LINE__,__FILE__,"Cactus",
                   "GH Extension '%s' has not registered a ScheduleTraverse routine",
                   Util_GetHandleName(GHExtensions, handle));
        extension->ScheduleTraverseGH=DummyScheduleTraverseGH;
      }
    }
  }

  return return_code;
}




/************************************************************************
 *
 *  Dummy functions.  Registered if no real function registered.
 *
 ************************************************************************/


 /*@@
   @routine    DummySetupGH
   @date       Wed Feb  3 13:36:52 1999
   @author     Tom Goodale
   @desc
   Dummy for SetupGH functions.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     config
   @vdesc   Flesh config data
   @vtype   tFleshConfig
   @vio     in
   @vcomment

   @endvar
   @var     convergence_level
   @vdesc   The convergence level
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     GH
   @vdesc   the cctk GH
   @vtype   cGH *
   @vio     in
   @vcomment

   @endvar

   @returntype void *
   @returndesc
   NULL
   @endreturndesc
@@*/
static void *DummySetupGH(tFleshConfig *config,
                          int convergence_level,
                          cGH *GH)
{
  config = config;
  convergence_level = convergence_level;
  GH = GH;
  return NULL;
}


 /*@@
   @routine    DummyInitGH
   @date       Wed Feb  3 13:37:31 1999
   @author     Tom Goodale
   @desc
   Dummy for InitGH functions.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     GH
   @vdesc   the cctk GH
   @vtype   cGH *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0
   @endreturndesc
@@*/
static int DummyInitGH(cGH *GH)
{
  GH = GH;
  return 0;
}


 /*@@
   @routine    DummyScheduleTraverseGH
   @date       Thu Jan 27 14:34:41 2000
   @author     Tom Goodale
   @desc

   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     GH
   @vdesc   the cctk GH
   @vtype   cGH *
   @vio     in
   @vcomment

   @endvar
   @var     where
   @vdesc   Schedule point to traverse
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0
   @endreturndesc
@@*/
static int DummyScheduleTraverseGH(cGH *GH,
                                   const char *where)
{
  GH = GH;
  where = where;
  return 0;
}
