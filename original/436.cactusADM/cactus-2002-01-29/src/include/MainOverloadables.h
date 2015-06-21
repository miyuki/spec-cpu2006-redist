 /*@@
   @header    MainOverloadables.h
   @date      Thu Feb  4 08:58:52 1999
   @author    Tom Goodale
   @desc
              The overloadable functions for the main layer.
              See OverloadMacros.h to see how to use these.
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/MainOverloadables.h,v 1.5 2001/12/09 22:04:12 tradke Exp $
 @@*/


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

#define ARGUMENTS tFleshConfig *config
#define USE_ARGUMENTS config = config
#define RETURN_TYPE int

OVERLOADABLE(Initialise)
OVERLOADABLE(Evolve)
OVERLOADABLE(Shutdown)

#undef ARGUMENTS
#define ARGUMENTS void
#undef USE_ARGUMENTS
#define USE_ARGUMENTS
#undef RETURN_TYPE
#define RETURN_TYPE int

OVERLOADABLE(MainLoopIndex)

#undef ARGUMENTS
#define ARGUMENTS int main_loop_index
#undef USE_ARGUMENTS
#define USE_ARGUMENTS main_loop_index = main_loop_index;
#undef RETURN_TYPE
#define RETURN_TYPE int

OVERLOADABLE(SetMainLoopIndex)

#undef ARGUMENTS
#undef USE_ARGUMENTS
#undef RETURN_TYPE

#undef OVERLOADABLE_CALL
#undef OVERLOADABLE_PREFIX
#undef OVERLOADABLE_DUMMY_PREFIX
