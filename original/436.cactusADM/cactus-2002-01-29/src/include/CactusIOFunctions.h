 /*@@
   @header    CactusIOFunctions.h
   @date      Mon Jan 8 1999
   @author    Gabrielle Allen
   @desc 
   Overloadable IO functions
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/CactusIOFunctions.h,v 1.4 1999/11/24 20:50:49 goodale Exp $
 @@*/

#ifndef _CACTUSIOFUNCTIONS_H_
#define _CACTUSIOFUNCTIONS_H_

#include <stdarg.h>

#include "OverloadMacros.h"

#ifdef __cplusplus
extern "C" {
#endif

/* The functions. */

#define OVERLOADABLE(name) OVERLOADABLE_PROTOTYPE(name)

#include "IOOverloadables.h"

#undef OVERLOADABLE

#ifdef __cplusplus
}
#endif

#endif
