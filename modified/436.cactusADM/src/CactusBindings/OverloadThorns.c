#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
/*@@
@file      OverloadThornFunctions.c
@desc 
Contains routines to overload thorn functions
Uses the overload macros to make sure of consistency and
to save typing !
@enddesc
@@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_WarnLevel.h"
#include "OverloadMacros.h"

/* Define the prototypes for the dummy functions. */
#define OVERLOADABLE(name) OVERLOADABLE_DUMMYPROTOTYPE(name)

#include "ThornOverloadables.h"

#undef OVERLOADABLE

#define OVERLOADABLE(name) OVERLOADABLE_FUNCTION(name)

#include "ThornOverloadables.h"

#undef OVERLOADABLE

#undef OVERLOADABLE_CALL
#undef OVERLOADABLE_PREFIX
#undef OVERLOADABLE_DUMMY_PREFIX

/* Initialising Stuff */

void CCTKBindings_SetupThornFunctions(void);
void CCTKBindings_SetupThornFunctions(void)
{
#undef OVERLOADABLE
#define OVERLOADABLE(name) OVERLOADABLE_INITIALISE(name)
#include "ThornOverloadables.h"
#undef OVERLOADABLE
}
