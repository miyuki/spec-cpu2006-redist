/*@@
  @header    ThornOverloadables.h
  @desc
  The overloadable functions from thorns
  See OverloadMacros.h to see how to use these.
  @enddesc 
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

#define OVERLOADABLE_CALL CCTKBindings_
#define OVERLOADABLE_PREFIX
#define OVERLOADABLE_DUMMY_PREFIX CCTKBindings_Dummy

#ifdef ARGUMENTS
#undef ARGUMENTS
#endif

#ifdef RETURN_TYPE
#undef RETURN_TYPE
#endif

#ifdef ARGUMENTS
#undef ARGUMENTS
#endif

#ifdef RETURN_TYPE
#undef RETURN_TYPE
#endif

#undef OVERLOADABLE_CALL
#undef OVERLOADABLE_PREFIX
#undef OVERLOADABLE_DUMMY_PREFIX
