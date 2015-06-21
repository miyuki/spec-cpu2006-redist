 /*@@
   @header    cctk_IO.h
   @date      1999/07/22
   @author    Tom Goodale
   @desc
              Header defining the variables holding the overloaded
              communication functions
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/cctk_IO.h,v 1.3 2001/11/05 14:58:52 tradke Exp $
 @@*/

#ifndef _CCTK_IO_H_
#define _CCTK_IO_H_ 1

#include "OverloadMacros.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Define the prototypes for the overloading functions. */
#define OVERLOADABLE(name) OVERLOADABLE_OVERLOADPROTO(name)

#include "IOOverloadables.h"

#undef OVERLOADABLE


/* Define the prototypes for the functions. */
#define OVERLOADABLE(name) OVERLOADABLE_PROTOTYPE(name)

#include "IOOverloadables.h"

#undef OVERLOADABLE

int CCTK_OutputVarAs (const cGH *GH, const char *var, const char *alias);
int CCTK_OutputVar (const cGH *GH, const char *var);
int CCTK_OutputVarByMethod (const cGH *GH, const char *var, const char *method);

#ifdef __cplusplus
}
#endif

#endif
