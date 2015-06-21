 /*@@
   @header    OverloadMacros.h
   @date      Thu Feb  4 08:02:29 1999
   @author    Tom Goodale
   @desc 
   Macros used for the overload functions
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/OverloadMacros.h,v 1.13 2001/12/09 22:04:13 tradke Exp $
 @@*/

#ifndef _OVERLOADMACROS_H_
#define _OVERLOADMACROS_H_ 1

/* These are a load of macros used to make overloadable functions. 
 *
 * Basically define ARGUMENTS with the arguments of the function,
 * and RETURN_TYPE as the return type
 * then put lines of the form OVERLOADABLE(function)
 * in a header file.
 * Defining OVERLOADABLE(name) as OVERLOADABLE_<macro>(name)
 * and then including the header will create functions, prototypes
 * dummy functions or some checking code as required.
 * 
 * One must also define
 *
 *  #define OVERLOADABLE_CALL prefix for calling overload functions 
 *  #define OVERLOADABLE_PREFIX prefix for real functions 
 *  #define OVERLOADABLE_DUMMY_PREFIX prefix for dummy functions
 *
 * to apply prefices to the functions.
 */

/* This macro defines a global variable with the name of the function
 * and a function which allows people to set its value.
 *
 * The function can only be called twice - to set the default, and to overload it.
 */
#define OVERLOADABLE_FUNCTION(name)   \
       _OVERLOADABLE_FUNCTION(OVERLOADABLE_CALL,OVERLOADABLE_PREFIX, OVERLOADABLE_DUMMY_PREFIX, name)
#define _OVERLOADABLE_FUNCTION(call, prefix, dummy_prefix, name)   \
       __OVERLOADABLE_FUNCTION(call, prefix, dummy_prefix, name)

#define __OVERLOADABLE_FUNCTION(call, prefix, dummy_prefix, name)   \
RETURN_TYPE (*prefix##name)(ARGUMENTS) = NULL;                      \
int call##Overload##name(RETURN_TYPE (*func)(ARGUMENTS));           \
int call##Overload##name(RETURN_TYPE (*func)(ARGUMENTS))            \
{                                                                   \
  int return_code;                                                  \
  static int overloaded = 0;                                        \
  if (func)                                                         \
  {                                                                 \
    if(overloaded < 2)                                              \
    {                                                               \
       prefix##name = func;                                         \
       overloaded++;                                                \
       return_code = overloaded;                                    \
    }                                                               \
    else                                                            \
    {                                                               \
       CCTK_VWarn(1,__LINE__,__FILE__,"Cactus","Overload Macros: "  \
                  "Attempted to overload function %s%s twice",      \
                  #prefix, #name);                                  \
       return_code = 0;                                             \
    }                                                               \
  }                                                                 \
  else                                                              \
  {                                                                 \
    return_code = overloaded;                                       \
  }                                                                 \
                                                                    \
  return return_code;                                               \
}

/* This macro creates an extern declaration for an overloadable function */

#define OVERLOADABLE_PROTOTYPE(name)   _OVERLOADABLE_PROTOTYPE(OVERLOADABLE_PREFIX, OVERLOADABLE_DUMMY_PREFIX, name)
#define _OVERLOADABLE_PROTOTYPE(prefix, dummy_prefix, name)   __OVERLOADABLE_PROTOTYPE(prefix, dummy_prefix, name)

#define __OVERLOADABLE_PROTOTYPE(prefix, dummy_prefix, name)       \
extern RETURN_TYPE (*prefix##name)(ARGUMENTS);

/* This macro defines a dummy function */
#define OVERLOADABLE_DUMMY(name)   _OVERLOADABLE_DUMMY(OVERLOADABLE_PREFIX, OVERLOADABLE_DUMMY_PREFIX, name)
#define _OVERLOADABLE_DUMMY(prefix, dummy_prefix, name)   __OVERLOADABLE_DUMMY(prefix, dummy_prefix, name)

#define __OVERLOADABLE_DUMMY(prefix, dummy_prefix, name)        \
RETURN_TYPE dummy_prefix##name(ARGUMENTS)                       \
{                                                               \
  /* prevent compiler warning about unused parameters */        \
  USE_ARGUMENTS                                                 \
  fprintf(stderr, "Dummy %s%s called.\n", #dummy_prefix,#name); \
  return 0;                                                     \
}

/* This macro defines the prototype for a dummy function. */
#define OVERLOADABLE_DUMMYPROTOTYPE(name)   _OVERLOADABLE_DUMMYPROTOTYPE(OVERLOADABLE_PREFIX, OVERLOADABLE_DUMMY_PREFIX, name)
#define _OVERLOADABLE_DUMMYPROTOTYPE(prefix, dummy_prefix, name)   __OVERLOADABLE_DUMMYPROTOTYPE(prefix, dummy_prefix, name)

#define __OVERLOADABLE_DUMMYPROTOTYPE(prefix, dummy_prefix, name)        \
RETURN_TYPE dummy_prefix##name(ARGUMENTS);


/* This macro initialises the function to the dummy if it
   hasn't already been set.
 */

#define OVERLOADABLE_INITIALISE(name)   _OVERLOADABLE_INITIALISE(OVERLOADABLE_CALL, OVERLOADABLE_DUMMY_PREFIX, name)
#define _OVERLOADABLE_INITIALISE(call, dummy_prefix, name)   __OVERLOADABLE_INITIALISE(call, dummy_prefix, name)

#define __OVERLOADABLE_INITIALISE(call, dummy_prefix, name)        \
  /*printf("Initialising overloadable function %s with %s%s\n",#name,#dummy_prefix,#name);*/ \
  call##Overload##name(dummy_prefix##name);



/* This macro defines a test for how many times a function has been 
   overloaded
 */

#define OVERLOADABLE_TEST(name)   _OVERLOADABLE_TEST(OVERLOADABLE_PREFIX, name)
#define _OVERLOADABLE_TEST(prefix, name)   __OVERLOADABLE_TEST(prefix, name)

#define __OVERLOADABLE_TEST(prefix, name)                           \
int call##Overload##name(NULL)                                      \
{                                                                   \
  int return_code;                                                  \
}


/* This macro defines the prototype for the overloading function itself */

#define OVERLOADABLE_OVERLOADPROTO(name)   \
       _OVERLOADABLE_OVERLOADPROTO(OVERLOADABLE_CALL,OVERLOADABLE_PREFIX, OVERLOADABLE_DUMMY_PREFIX, name)
#define _OVERLOADABLE_OVERLOADPROTO(call,prefix, dummy_prefix, name) \
       __OVERLOADABLE_OVERLOADPROTO(call,prefix, dummy_prefix, name)

#define __OVERLOADABLE_OVERLOADPROTO(call,prefix, dummy_prefix, name)        \
int call##Overload##name(RETURN_TYPE (*func)(ARGUMENTS));


#endif /* _OVERLOADMACROS_H_ */
