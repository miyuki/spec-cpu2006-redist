 /*@@
   @header    Main.h
   @date      Tue Jun  1 13:24:32 1999
   @author    Tom Goodale
   @desc 
   Header defining the variables holding the overloaded main functions
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/Main.h,v 1.1 1999/06/01 15:10:05 goodale Exp $ 
   @@*/


#ifndef _MAIN_H_
#define _MAIN_H_

#include "OverloadMacros.h"

#ifdef __cplusplus
extern "C" {
#endif
/* Define the prototypes for the functions. */
#define OVERLOADABLE(name) OVERLOADABLE_PROTOTYPE(name)

#include "MainOverloadables.h"

#undef OVERLOADABLE

/* Define the prototypes for the overloading functions. */
#define OVERLOADABLE(name) OVERLOADABLE_OVERLOADPROTO(name)

#include "MainOverloadables.h"

#undef OVERLOADABLE

#ifdef __cplusplus
}
#endif

#endif




