 /*@@
   @header    cctk_Main.h
   @date      Tue Mar  21 19:42:29 2000
   @author    Tom Goodale
   @desc 
   Header defining the variables holding the overloaded main functions
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Main.h,v 1.1 2000/03/23 15:02:35 allen Exp $
 @@*/

#ifndef _CCTK_MAIN_H_
#define _CCTK_MAIN_H_

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




