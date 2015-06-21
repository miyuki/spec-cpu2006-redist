 /*@@
   @header    CactusMainFunctions.h
   @date      Thu Jan 14 18:34:52 1999
   @author    Tom Goodale
   @desc 
   Main registerable functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/CactusMainFunctions.h,v 1.4 1999/11/24 20:50:50 goodale Exp $
 @@*/

#ifndef _CACTUSMAINFUNCTIONS_H_
#define _CACTUSMAINFUNCTIONS_H_

#include "OverloadMacros.h"

/* Function prototypes */

#ifdef __cplusplus
extern "C" {
#endif

#define OVERLOADABLE(name) OVERLOADABLE_PROTOTYPE(name)

#include "MainOverloadables.h"

#undef OVERLOADABLE

#ifdef __cplusplus
}
#endif

#endif
