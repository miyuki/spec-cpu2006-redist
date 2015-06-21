#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Overload.c
   @date      Thu Feb  4 09:47:23 1999
   @author    Tom Goodale
   @desc 
   Contains routines to overload the IO functions.
   Uses the overload macros to make sure of consistency and
   to save typing !
   @enddesc 
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "OverloadMacros.h"
#include "CactusRegister.h"
#include "cctk_WarnLevel.h"

static const char *rcsid="$Header: /cactus/Cactus/src/IO/OverloadIO.c,v 1.11 2001/11/05 14:58:49 tradke Exp $";

CCTK_FILEVERSION (IO_OverloadIO_c)

/* Define the prototypes for the dummy functions. */
#define OVERLOADABLE(name) OVERLOADABLE_DUMMYPROTOTYPE(name)

/* There are default versions for all overloadable IO functions */
#define CCTKi_DummyOutputGH    CactusDefaultOutputGH
#define CCTKi_DummyOutputVarAsByMethod CactusDefaultOutputVarAsByMethod

#include "IOOverloadables.h"

#undef CCTKi_DummyOutputGH
#undef CCTKi_DummyOutputVarAsByMethod
#undef OVERLOADABLE

/* Create the overloadable function variables and the 
 * functions allowing the variables to be set.
 */
#define OVERLOADABLE(name) OVERLOADABLE_FUNCTION(name)

#include "IOOverloadables.h"

#undef OVERLOADABLE

 /*@@
   @routine    CCTKi_SetupIOFunctions
   @date       Thu Feb  4 09:58:29 1999
   @author     Tom Goodale
   @desc 
   Set any IO function which hasn't been overloaded to the default.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int CCTKi_SetupIOFunctions(void)
{

#define OVERLOADABLE(name) OVERLOADABLE_INITIALISE(name)

  /* There are default versions for all overloadable IO functions */
#define CCTKi_DummyOutputGH     CactusDefaultOutputGH
#define CCTKi_DummyOutputVarAsByMethod  CactusDefaultOutputVarAsByMethod

#include "IOOverloadables.h"

  /* Reset the #define to prevent complications. */
#undef CCTKi_DummyOutputGH
#undef CCTKi_DummyOutputVarAsByMethod

#undef OVERLOADABLE

  return 0;
}

/* Don't need dummy functions since we have defaults.
#define OVERLOADABLE(name) OVERLOADABLE_DUMMY(name)
#include "IOOverloadables.h"
#undef OVERLOADABLE(name)
*/
