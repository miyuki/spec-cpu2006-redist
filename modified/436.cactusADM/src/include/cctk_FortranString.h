 /*@@
   @header    cctk_FortranString.h
   @date      Wed Feb  17 12:55 1999
   @author    Gabrielle Allen, Paul Walker
   @desc 
   Macros for dealing with strings passed from fortran to C.
   The memory for the new arguments created should be freed after use   

   To ensure that passing strings from fortran to C has a chance of 
   working with all compilers, we assume that

   ** Strings are always at the end of the argument list **

   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_FortranString.h,v 1.10 2001/06/20 08:09:12 allen Exp $
 @@*/

#include "cctk_Misc.h"

#ifndef _CCTK_FORTRANSTRING_H_
#define _CCTK_FORTRANSTRING_H_

#if defined T3E

#include <fortran.h>
#define ONE_FORTSTRING_ARG\
    _fcd cctk_str1
#define TWO_FORTSTRING_ARG\
    _fcd cctk_str1, _fcd cctk_str2
#define THREE_FORTSTRING_ARG\
    _fcd cctk_str1, _fcd cctk_str2, _fcd cctk_str3

#define ONE_FORTSTRING_CREATE(arg1)\
       int cctk_strlen1 = _fcdlen(cctk_str1); \
       char *arg1 = Util_NullTerminateString(_fcdtocp(cctk_str1),cctk_strlen1);
#define TWO_FORTSTRING_CREATE(arg1,arg2)\
       int  cctk_strlen1    = _fcdlen(cctk_str1);\
       int  cctk_strlen2    = _fcdlen(cctk_str2);\
       char *arg1 = Util_NullTerminateString(_fcdtocp(cctk_str1),cctk_strlen1);\
       char *arg2 = Util_NullTerminateString(_fcdtocp(cctk_str2),cctk_strlen2);
#define THREE_FORTSTRING_CREATE(arg1,arg2,arg3)\
       int  cctk_strlen1    = _fcdlen(cctk_str1);\
       int  cctk_strlen2    = _fcdlen(cctk_str2);\
       int  cctk_strlen3    = _fcdlen(cctk_str3);\
       char *arg1 = Util_NullTerminateString(_fcdtocp(cctk_str1),cctk_strlen1);\
       char *arg2 = Util_NullTerminateString(_fcdtocp(cctk_str2),cctk_strlen2);\
       char *arg3 = Util_NullTerminateString(_fcdtocp(cctk_str3),cctk_strlen3);

#define ONE_FORTSTRING_PTR(arg1)\
       char *arg1 = _fcdtocp(cctk_str1);
#define TWO_FORTSTRING_PTR(arg1, arg2)\
       char *arg1 = _fcdtocp(cctk_str1);\
       char *arg2 = _fcdtocp(cctk_str2);
#define THREE_FORTSTRING_PTR(arg1, arg2, arg3)\
       char *arg1 = _fcdtocp(cctk_str1);\
       char *arg2 = _fcdtocp(cctk_str2);\
       char *arg3 = _fcdtocp(cctk_str3);

/* DEPRECATED BETA 10 */
#define TWO_FORTSTRINGS_ARGS\
    _fcd cctk_str1, _fcd cctk_str2
#define THREE_FORTSTRINGS_ARGS\
    _fcd cctk_str1, _fcd cctk_str2, _fcd cctk_str3
#define TWO_FORTSTRINGS_CREATE(arg1,arg2)\
       int  cctk_strlen1    = _fcdlen(cctk_str1);\
       int  cctk_strlen2    = _fcdlen(cctk_str2);\
       char *arg1 = Util_NullTerminateString(_fcdtocp(cctk_str1),cctk_strlen1);\
       char *arg2 = Util_NullTerminateString(_fcdtocp(cctk_str2),cctk_strlen2);
#define THREE_FORTSTRINGS_CREATE(arg1,arg2,arg3)\
       int  cctk_strlen1    = _fcdlen(cctk_str1);\
       int  cctk_strlen2    = _fcdlen(cctk_str2);\
       int  cctk_strlen3    = _fcdlen(cctk_str3);\
       char *arg1 = Util_NullTerminateString(_fcdtocp(cctk_str1),cctk_strlen1);\
       char *arg2 = Util_NullTerminateString(_fcdtocp(cctk_str2),cctk_strlen2);\
       char *arg3 = Util_NullTerminateString(_fcdtocp(cctk_str3),cctk_strlen3);
/* END DEPRECATED */

#elif defined WIN32_DIGITAL_FORTRAN

#define ONE_FORTSTRING_ARG\
   char *cctk_str1, unsigned int cctk_strlen1
#define TWO_FORTSTRING_ARG\
   char *cctk_str1,\
   int cctk_strlen1,\
   char *cctk_str2,\
   int cctk_strlen2
#define THREE_FORTSTRING_ARG\
   char *cctk_str1,\
   int cctk_strlen1,\
   char *cctk_str2,\
   int cctk_strlen2,\
   char *cctk_str3,\
   int cctk_strlen3

#define ONE_FORTSTRING_CREATE(arg1)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);
#define TWO_FORTSTRING_CREATE(arg1,arg2)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);
#define THREE_FORTSTRING_CREATE(arg1,arg2,arg3)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);\
   char *arg3 = Util_NullTerminateString(cctk_str3,cctk_strlen3);

#define ONE_FORTSTRING_PTR(arg1)\
       char *arg1 = cctk_str1;
#define TWO_FORTSTRING_PTR(arg1, arg2)\
       char *arg1 = cctk_str1;\
       char *arg2 = cctk_str2;
#define THREE_FORTSTRING_PTR(arg1, arg2, arg3)\
       char *arg1 = cctk_str1;\
       char *arg2 = cctk_str2;\
       char *arg3 = cctk_str3;

/* DEPRECATED BETA 10 */
#define TWO_FORTSTRINGS_ARGS\
   char *cctk_str1, int cctk_strlen1, char *cctk_str2, int cctk_strlen2
#define THREE_FORTSTRINGS_ARGS\
   char *cctk_str1,\
   char *cctk_str2,\
   char *cctk_str3,\
   unsigned int cctk_strlen1,\
   unsigned int cctk_strlen2,\
   unsigned int cctk_strlen3
#define TWO_FORTSTRINGS_CREATE(arg1,arg2)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);
#define THREE_FORTSTRINGS_CREATE(arg1,arg2,arg3)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);\
   char *arg3 = Util_NullTerminateString(cctk_str3,cctk_strlen3);
/* END DEPRECATED */

#else

#define ONE_FORTSTRING_ARG\
   char *cctk_str1,\
   unsigned int cctk_strlen1
#define TWO_FORTSTRING_ARG\
   char *cctk_str1,\
   char *cctk_str2,\
   unsigned int cctk_strlen1,\
   unsigned int cctk_strlen2
#define THREE_FORTSTRING_ARG\
   char *cctk_str1,\
   char *cctk_str2,\
   char *cctk_str3,\
   unsigned int cctk_strlen1,\
   unsigned int cctk_strlen2,\
   unsigned int cctk_strlen3

#define ONE_FORTSTRING_CREATE(arg1)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);
#define TWO_FORTSTRING_CREATE(arg1,arg2)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);
#define THREE_FORTSTRING_CREATE(arg1,arg2,arg3)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);\
   char *arg3 = Util_NullTerminateString(cctk_str3,cctk_strlen3);

#define ONE_FORTSTRING_PTR(arg1)\
       char *arg1 = cctk_str1;
#define TWO_FORTSTRING_PTR(arg1, arg2)\
       char *arg1 = cctk_str1;\
       char *arg2 = cctk_str2;
#define THREE_FORTSTRING_PTR(arg1, arg2, arg3)\
       char *arg1 = cctk_str1;\
       char *arg2 = cctk_str2;\
       char *arg3 = cctk_str3;

/* DEPRECATED BETA 10 */
#define TWO_FORTSTRINGS_ARGS\
   char *cctk_str1,\
   char *cctk_str2,\
   unsigned int cctk_strlen1,\
   unsigned int cctk_strlen2
#define THREE_FORTSTRINGS_ARGS\
   char *cctk_str1,\
   char *cctk_str2,\
   char *cctk_str3,\
   unsigned int cctk_strlen1,\
   unsigned int cctk_strlen2,\
   unsigned int cctk_strlen3
#define TWO_FORTSTRINGS_CREATE(arg1,arg2)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);
#define THREE_FORTSTRINGS_CREATE(arg1,arg2,arg3)\
   char *arg1 = Util_NullTerminateString(cctk_str1,cctk_strlen1);\
   char *arg2 = Util_NullTerminateString(cctk_str2,cctk_strlen2);\
   char *arg3 = Util_NullTerminateString(cctk_str3,cctk_strlen3);
/* END DEPRECATED */

#endif

#endif /* _CCTK_FORTRANSTRING_H_*/
