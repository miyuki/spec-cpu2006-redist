 /*@@
   @header    CommOverloadables.h
   @date      Thu Feb  4 08:11:41 1999
   @author    Tom Goodale
   @desc
              The overloadable functions for the comm layer.
              See OverloadMacros.h to see how to use these.
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/CommOverloadables.h,v 1.18 2001/12/09 22:04:12 tradke Exp $
 @@*/

#include "cctk_Flesh.h"
#include "cctk_GroupsOnGH.h"

#ifdef OVERLOADABLE_CALL
#undef OVERLOADABLE_CALL
#endif

#ifdef OVERLOABLE_PREFIX
#undef OVERLOADABLE_PREFIX
#endif

#ifdef OVERLOABLE_DUMMY_PREFIX
#undef OVERLOADABLE_DUMMY_PREFIX
#endif

#define OVERLOADABLE_CALL CCTK_
#define OVERLOADABLE_PREFIX CCTK_
#define OVERLOADABLE_DUMMY_PREFIX CCTKi_Dummy

#ifdef ARGUMENTS
#undef ARGUMENTS
#endif

#ifdef USE_ARGUMENTS
#undef USE_ARGUMENTS
#endif

#ifdef RETURN_TYPE
#undef RETURN_TYPE
#endif

#define RETURN_TYPE int
#define ARGUMENTS cGH *GH, const char *group
#define USE_ARGUMENTS GH = GH; group = group;
OVERLOADABLE(SyncGroup)

OVERLOADABLE(EnableGroupStorage)
OVERLOADABLE(DisableGroupStorage)

OVERLOADABLE(EnableGroupComm)
OVERLOADABLE(DisableGroupComm)

#undef ARGUMENTS
#define ARGUMENTS const cGH *GH
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH;
OVERLOADABLE(Barrier)
OVERLOADABLE(MyProc)
OVERLOADABLE(nProcs)

#undef ARGUMENTS
#define ARGUMENTS cGH *GH
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH;
OVERLOADABLE(ParallelInit)

#undef ARGUMENTS
#define ARGUMENTS cGH *GH, int retval
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH; retval = retval;
#undef RETURN_TYPE
#define RETURN_TYPE int
OVERLOADABLE(Exit)
OVERLOADABLE(Abort)

#undef ARGUMENTS
#define ARGUMENTS tFleshConfig *config, int convergence_level
#undef USE_ARGUMENTS
#define USE_ARGUMENTS config = config; convergence_level = convergence_level;
#undef RETURN_TYPE
#define RETURN_TYPE cGH *
OVERLOADABLE(SetupGH)

#undef ARGUMENTS
#define ARGUMENTS const cGH *GH, int dir, int group, const char *groupname
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH; dir = dir; group = group; groupname = groupname;
#undef RETURN_TYPE
#define RETURN_TYPE const int *
OVERLOADABLE(ArrayGroupSizeB)

#undef ARGUMENTS
#define ARGUMENTS const cGH *GH, int group, const char *groupname
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH; group = group; groupname = groupname;
#undef RETURN_TYPE
#define RETURN_TYPE int
OVERLOADABLE(QueryGroupStorageB)

#undef ARGUMENTS
#define ARGUMENTS const cGH *GH, int group, cGroupDynamicData *data
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH; group = group; data = data;
#undef RETURN_TYPE
#define RETURN_TYPE int
OVERLOADABLE(GroupDynamicData)

#undef ARGUMENTS
#undef USE_ARGUMENTS
#undef RETURN_TYPE

#undef OVERLOADABLE_CALL
#undef OVERLOADABLE_PREFIX
#undef OVERLOADABLE_DUMMY_PREFIX
