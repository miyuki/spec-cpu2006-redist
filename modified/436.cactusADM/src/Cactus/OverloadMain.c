#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Overload.c
   @date      Thu Feb  4 09:01:18 1999
   @author    Tom Goodale
   @desc 
   Contains routines to overload the main functions.
   Uses the overload macros to make sure of consistency and
   to save typing !
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/OverloadMain.c,v 1.14 2001/11/05 14:58:54 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_WarnLevel.h"
#include "CactusRegister.h"
#include "OverloadMacros.h"

static const char *rcsid="$Header: /cactus/Cactus/src/main/OverloadMain.c,v 1.14 2001/11/05 14:58:54 tradke Exp $";

CCTK_FILEVERSION(main_OverloadMain_c)

/* Define the prototypes for the dummy functions. */
#define OVERLOADABLE(name) OVERLOADABLE_DUMMYPROTOTYPE(name)

  /* These ones actually have defaults. */
#define CCTKi_DummyInitialise       CactusDefaultInitialise
#define CCTKi_DummyEvolve           CactusDefaultEvolve
#define CCTKi_DummyShutdown         CactusDefaultShutdown
#define CCTKi_DummyMainLoopIndex    CactusDefaultMainLoopIndex
#define CCTKi_DummySetMainLoopIndex CactusDefaultSetMainLoopIndex

#include "MainOverloadables.h"

  /* Reset the #define to prevent complications. */
#undef CCTKi_DummyInitialise 
#undef CCTKi_DummyEvolve     
#undef CCTKi_DummyShutdown   
#undef CCTKi_DummyMainLoopIndex
#undef CCTKi_DummySetMainLoopIndex

#undef OVERLOADABLE


/* Create the overloadable function variables and the 
 * functions allowing the variables to be set.
 */
#define OVERLOADABLE(name) OVERLOADABLE_FUNCTION(name)

#include "MainOverloadables.h"

#undef OVERLOADABLE

 /*@@
   @routine    CCTKi_SetupMainFunctions
   @date       Thu Feb  4 09:02:49 1999
   @author     Tom Goodale
   @desc 
   Set any main function which hasn't been overloaded to the default.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int CCTKi_SetupMainFunctions(void)
{

#define OVERLOADABLE(name) OVERLOADABLE_INITIALISE(name)

  /* These ones actually have defaults. */
#define CCTKi_DummyInitialise       CactusDefaultInitialise
#define CCTKi_DummyEvolve           CactusDefaultEvolve
#define CCTKi_DummyShutdown         CactusDefaultShutdown
#define CCTKi_DummyMainLoopIndex    CactusDefaultMainLoopIndex
#define CCTKi_DummySetMainLoopIndex CactusDefaultSetMainLoopIndex

#include "MainOverloadables.h"

  /* Reset the #define to prevent complications. */
#undef CCTKi_DummyInitialise 
#undef CCTKi_DummyEvolve     
#undef CCTKi_DummyShutdown   
#undef CCTKi_DummyMainLoopIndex
#undef CCTKi_DummySetMainLoopIndex

#undef OVERLOADABLE

  return 0;
}
