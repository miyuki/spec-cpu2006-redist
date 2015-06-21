/* cctk_Config.h.  Generated automatically by configure.  */
#ifndef _CCTK_CONFIG_H_
#define _CCTK_CONFIG_H_

/* These are definitely there in C++ */
#ifndef __cplusplus

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define to empty if the keyword does not work.  */
/* #undef const */

#endif /* ! defined __cplusplus */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* Define if standard C headers are available. */
#define STDC_HEADERS 1

/* Size info for various types */

/* The number of bytes in a long int.  */
#define SIZEOF_LONG_INT 4

/* The number of bytes in a int.  */
#define SIZEOF_INT 4

/* The number of bytes in a short int.  */
#define SIZEOF_SHORT_INT 2


/* The number of bytes in a long double.  */
#define SIZEOF_LONG_DOUBLE 12

/* The number of bytes in a double.  */
#define SIZEOF_DOUBLE 8

/* The number of bytes in a float.  */
#define SIZEOF_FLOAT 4

/* The number of bytes in a char *.  */
#define SIZEOF_CHAR_P 4

/* The chosen CCTK precision */

/* Floating point precision */
/* #undef CCTK_REAL_PRECISION_16 */
#define CCTK_REAL_PRECISION_8 1
/* #undef CCTK_REAL_PRECISION_4 */

/* Integer precision */
/* #undef CCTK_INTEGER_PRECISION_8 */
#define CCTK_INTEGER_PRECISION_4 1
/* #undef CCTK_INTEGER_PRECISION_2 */

/* Modifier for Fortran function definitions. */

#define CCTK_FCALL 

/* What debugging options to use */

/* #undef CCTK_DEBUG */
/* #undef CCTK_TRACEMEMORY */

/* Various library functions */

#define HAVE_GETHOSTBYNAME 1
#define HAVE_GETOPT_LONG_ONLY 1
#define HAVE_CRYPT 1
#define HAVE_FINITE 1
#define HAVE_ISNAN 1

/* Do we have mode_t ? */

#define HAVE_MODE_T 1

/* Do we have socklen_t ? */

#define HAVE_SOCKLEN_T 1

/* Do we have SOCKET ? */

/* #undef HAVE_SOCKET_TYPE */

/* Some include things */
#define HAVE_TIME_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
#define HAVE_STRING_H 1
#define HAVE_ASSERT_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_GETOPT_H 1
#define HAVE_REGEX_H 1
#define HAVE_SYS_SOCKET_H 1
#define HAVE_NETINET_IN_H 1
#define HAVE_NETDB_H 1
#define HAVE_ARPA_INET_H 1
/* #undef HAVE_WINSOCK2_H */
#define HAVE_CRYPT_H 1
#define HAVE_DIRENT_H 1

#define TIME_WITH_SYS_TIME 1

/* Which format does the C++ STL on this machine provide. */

#define HAVE_VECTOR 1
#define HAVE_VECTOR_H 1

/* Timing stuff */

#define HAVE_TIME_GETTIMEOFDAY 1
#define GETTIMEOFDAY_NEEDS_TIMEZONE 1
#define HAVE_TIME_GETRUSAGE 1
/* #undef HAVE_TIME__FTIME */

/* Cache stuff */

#define CCTK_L2_CACHELINE_BYTES 32
#define CCTK_L2_CACHE_SIZE 512*1024

/* The name of the NULL device for redirecting things to */

#define NULL_DEVICE "/dev/null"

/* The TYPE of socklen_t if it does not exist */ 

#ifdef HAVE_SOCKLEN_T
#define CCTK_SOCKLEN_T socklen_t
#else
#define CCTK_SOCKLEN_T int
#endif

/******************************************************************************/

#ifdef CCODE

/* Integer sizes */

/* Define the type of an 8 byte integer */
#define CCTK_INT8 long long

/* Define the type of a 4 byte integer */
#define CCTK_INT4 int

/* Define the type of a 2 byte integer */
#define CCTK_INT2 short int

/* Float sizes */

/* Define the type of a 16 byte float */
/* #undef CCTK_REAL16 */

/* Define the type of an 8 byte float */
#define CCTK_REAL8 double

/* Define the type of a 4 byte float */
#define CCTK_REAL4 float

#ifdef __cplusplus

/* Some C++ compilers don't have bool ! */
#define HAVE_BOOL 1

#ifndef HAVE_BOOL
typedef enum {false, true} bool;
#endif /* HAVE_BOOL */

#endif /* __cplusplus */


#endif /*CCODE */

#ifdef FCODE

#endif /*FCODE */

/* Now include the code to pick an appropriate precison for reals and ints */
#include "cctk_Types.h"

/* Include any other stuff which is specific to this architecture */
#include "cctk_Archdefs.h"

/* Include any extra stuff from optional extra packages. */
#include "cctk_Extradefs.h"

#endif /* _CCTK_CONFIG_H_ */
