#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Overload.c
   @date      Wed Feb  3 23:27:18 1999
   @author    Tom Goodale
   @desc
              Contains routines to overload the communication functions.
              Uses the overload macros to make sure of consistency and
              to save typing !
   @enddesc
   @version   $Id: OverloadComm.c,v 1.25 2001/12/09 23:34:55 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_WarnLevel.h"
#include "CactusRegister.h"
#include "OverloadMacros.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/comm/OverloadComm.c,v 1.25 2001/12/09 23:34:55 tradke Exp $";
CCTK_FILEVERSION(comm_OverloadComm_c)


/* Define the prototypes for the dummy functions. */
#define OVERLOADABLE(name) OVERLOADABLE_DUMMYPROTOTYPE(name)

  /* Deal seperately with the SetupGH routine */
#define CCTKi_DummyAbort    CactusDefaultAbort
#define CCTKi_DummyBarrier  CactusDefaultBarrier
#define CCTKi_DummyExit     CactusDefaultExit
#define CCTKi_DummyMyProc   CactusDefaultMyProc
#define CCTKi_DummynProcs   CactusDefaultnProcs
#define CCTKi_DummySetupGH  CactusDefaultSetupGH

#include "CommOverloadables.h"

  /* Reset the #define to prevent complications. */
#undef CCTKi_DummySetupGH  
#undef CCTKi_DummyMyProc  
#undef CCTKi_DummynProcs
#undef CCTKi_DummyBarrier
#undef CCTKi_DummyExit
#undef CCTKi_DummyAbort

#undef OVERLOADABLE

/* Create the overloadable function variables and the 
 * functions allowing the variables to be set.
 */
#define OVERLOADABLE(name) OVERLOADABLE_FUNCTION(name)

#include "CommOverloadables.h"

#undef OVERLOADABLE


 /*@@
   @routine    CCTKi_SetupCommFunctions
   @date       Thu Feb  4 08:21:26 1999
   @author     Tom Goodale
   @desc 
   Set any comm function which hasn't been overloaded to the default.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int CCTKi_SetupCommFunctions(void)
{

#define OVERLOADABLE(name) OVERLOADABLE_INITIALISE(name)

  /* Deal seperately with the SetupGH routine */
#define CCTKi_DummyAbort   CactusDefaultAbort
#define CCTKi_DummyBarrier CactusDefaultBarrier
#define CCTKi_DummyExit    CactusDefaultExit
#define CCTKi_DummyMyProc  CactusDefaultMyProc
#define CCTKi_DummynProcs  CactusDefaultnProcs
#define CCTKi_DummySetupGH CactusDefaultSetupGH

#include "CommOverloadables.h"

  /* Reset the #define to prevent complications. */
#undef CCTKi_DummyAbort
#undef CCTKi_DummyBarrier
#undef CCTKi_DummyExit
#undef CCTKi_DummyMyProc  
#undef CCTKi_DummynProcs
#undef CCTKi_DummySetupGH  

#undef OVERLOADABLE

  return 0;
}

/* Create the dummy function prototypes. */
#define OVERLOADABLE(name) OVERLOADABLE_DUMMYPROTOTYPE(name)

#include "CommOverloadables.h"

#undef OVERLOADABLE


/* Create the dummy functions. */
#define OVERLOADABLE(name) OVERLOADABLE_DUMMY(name)

#include "CommOverloadables.h"

#undef OVERLOADABLE



/* Fortran bindings prototypes for the comm functions */
int CCTK_FCALL cctk_nprocs_ (const cGH *GH);
int CCTK_FCALL cctk_myproc_ (const cGH *GH);
void CCTK_FCALL cctk_barrier_ (int *ierror, const cGH *GH);
void CCTK_FCALL cctk_exit_ (int *ierror, cGH *GH, const int *retval);
void CCTK_FCALL cctk_abort_ (int *ierror, cGH *GH, const int *retval);
void CCTK_FCALL cctk_syncgroup_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_enablegroupcomm_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_disablegroupcomm_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_enablegroupstorage_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_disablegroupstorage_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_querygroupstorage_ (int *ierror, const cGH *GH, ONE_FORTSTRING_ARG);


/* Fortran bindings definitions for the comm functions */
int CCTK_FCALL cctk_nprocs_ (const cGH *GH)
{
  return (CCTK_nProcs (GH));
}
 
int CCTK_FCALL cctk_myproc_ (const cGH *GH)
{
  return (CCTK_MyProc (GH));
}
 
void CCTK_FCALL cctk_barrier_ (int *ierror, const cGH *GH)
{
  *ierror = CCTK_Barrier (GH);
}

void CCTK_FCALL cctk_exit_ (int *ierror, cGH *GH, const int *retval)
{
  *ierror = CCTK_Exit (GH, *retval);
}

void CCTK_FCALL cctk_abort_ (int *ierror, cGH *GH, const int *retval)
{
  *ierror = CCTK_Abort (GH, *retval);
}

void CCTK_FCALL cctk_syncgroup_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (group_name)
  *ierror = CCTK_SyncGroup (GH, group_name);
  free (group_name); 
}

void CCTK_FCALL cctk_enablegroupcomm_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (group_name)
  *ierror = CCTK_EnableGroupComm (GH, group_name); 
  free (group_name);
}

void CCTK_FCALL cctk_disablegroupcomm_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (group_name)
  *ierror = CCTK_DisableGroupComm (GH, group_name); 
  free (group_name);
}

void CCTK_FCALL cctk_enablegroupstorage_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (group_name)
  *ierror = CCTK_EnableGroupStorage (GH, group_name);
  free (group_name);
}

void CCTK_FCALL cctk_disablegroupstorage_ (int *ierror, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (group_name)
  *ierror = CCTK_DisableGroupStorage (GH, group_name);
  free (group_name);
}

void CCTK_FCALL cctk_querygroupstorage_ (int *ierror, const cGH *GH, ONE_FORTSTRING_ARG)
{
  extern int CCTK_QueryGroupStorage (const cGH *, const char *);
  ONE_FORTSTRING_CREATE (group_name)
  *ierror = CCTK_QueryGroupStorage (GH, group_name);
  free (group_name);
}
