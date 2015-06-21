 /*@@
   @header    IOOverloadables.h
   @date      Thu Feb  4 09:59:34 1999
   @author    Tom Goodale
   @desc
              The overloadable functions for the IO layer.
              See OverloadMacros.h to see how to use these.
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/IOOverloadables.h,v 1.6 2001/12/09 22:04:12 tradke Exp $
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

#define ARGUMENTS const cGH *GH
#define USE_ARGUMENTS GH = GH;
#define RETURN_TYPE int
OVERLOADABLE(OutputGH)

#undef ARGUMENTS
#define ARGUMENTS const cGH *GH,      \
                  const char *var,    \
                  const char *method, \
                  const char *alias
#undef USE_ARGUMENTS
#define USE_ARGUMENTS GH = GH; var = var; method = method; alias = alias;
OVERLOADABLE(OutputVarAsByMethod)

#undef ARGUMENTS
#undef USE_ARGUMENTS
#undef RETURN_TYPE

#undef OVERLOADABLE_CALL
#undef OVERLOADABLE_PREFIX
#undef OVERLOADABLE_DUMMY_PREFIX
