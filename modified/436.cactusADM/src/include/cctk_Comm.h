 /*@@
   @header    cctk_Comm.h
   @date      Sat Feb  13 19:42:29 1999
   @author    Tom Goodale
   @desc
              Header defining the variables holding the overloaded
              communication functions
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/cctk_Comm.h,v 1.4 2001/11/05 14:58:51 tradke Exp $
 @@*/

#ifndef _CCTK_COMM_H_
#define _CCTK_COMM_H_ 1

#include "OverloadMacros.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Define the prototypes for the functions. */
#define OVERLOADABLE(name) OVERLOADABLE_PROTOTYPE(name)

#include "CommOverloadables.h"

#undef OVERLOADABLE

/* Define the prototypes for the overloading functions. */
#define OVERLOADABLE(name) OVERLOADABLE_OVERLOADPROTO(name)

#include "CommOverloadables.h"

#undef OVERLOADABLE

int CCTK_QueryGroupStorage (const cGH *GH, const char *groupname);
int CCTK_QueryGroupStorageI (const cGH *GH, int groupindex);
const int *CCTK_ArrayGroupSize (const cGH *GH, int dir, const char *groupname);
const int *CCTK_ArrayGroupSizeI (const cGH *GH, int dir, int groupindex);

#ifdef __cplusplus
}
#endif

#endif  /* _CCTK_COMM_H_ */
