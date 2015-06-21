/* util_Table.h -- header for key-value tables */
/* $header$ */

/*@@
  @file         util_Table.h
  @header       util_Table.h
  @version      $Header: /cactus/Cactus/src/include/util_Table.h,v 1.4 2002/01/21 14:40:28 rideout Exp $
  @date         Wed Oct 31 14:20:43 MET 2001
  @author       Jonathan Thornburg <jthorn@aei.mpg.de>
  @desc
        This header defines the programmer API for Cactus key-value tables.
        This is documented in
           http://www.cactuscode.org/Development/Specs/KeyValueLookup.txt
        FIXME: this is a bit out of date :(
        FIXME: should write some latex docs
  @enddesc
  @seefile      seefile Table.c
  @@*/

/*
 * Having this file (a "util_*" low-level file) include a "cctk_*" high-level
 * file is an ugly hack, but we need the CCTK_{INT,REAL,COMPLEX}_* types
 * to define the table API.  So...
 */
#include "cctk_Types.h"

#ifndef _UTIL_TABLE_H_
#define _UTIL_TABLE_H_  1       /* define to 1 is Cactus standard */
                                /* n.b. usual standard is empty defn! */

#ifdef __cplusplus
extern "C"
{
#endif

/******************************************************************************/
/***** Macros for Flags Word **************************************************/
/******************************************************************************/

/*@@
  @defines      UTIL_TABLE_FLAGS_DEFAULT
  @desc         flags-word macro: no flags set (default)
  @@*/
#define UTIL_TABLE_FLAGS_DEFAULT                0

/*@@
  @defines      UTIL_TABLE_FLAGS_CASE_INSENSITIVE
  @desc         flags-word macro: key comparisons are case-insensitive
  @@*/
#define UTIL_TABLE_FLAGS_CASE_INSENSITIVE       0x1

/*@@
  @defines      UTIL_TABLE_FLAGS_USER_DEFINED_BASE
  @desc         flags-word macro: user-defined flags word bit masks
                should use only this and higher bit positions (i.e.
                all bit positions below this one are reserved for
                current or future Cactus use)
  @@*/
#define UTIL_TABLE_FLAGS_USER_DEFINED_BASE      0x10000

/******************************************************************************/
/***** Error Codes ************************************************************/
/******************************************************************************/

/*
 * error codes specific to the table routines (between -100 and -199)
 */

/*@@
  @defines      UTIL_ERROR_TABLE_BAD_FLAGS
  @desc         error return code: flags word is invalid
  @@*/
#define UTIL_ERROR_TABLE_BAD_FLAGS              (-100)

/*@@
  @defines      UTIL_ERROR_TABLE_BAD_KEY
  @desc         error return code: key contains '/' character
                                   or is otherwise invalid
  @@*/
#define UTIL_ERROR_TABLE_BAD_KEY                (-101)

/*@@
  @defines      UTIL_ERROR_TABLE_STRING_TRUNCATED
  @desc         error return code: string was truncated to fit in buffer
  @@*/
#define UTIL_ERROR_TABLE_STRING_TRUNCATED       (-102)

/*@@
  @defines      UTIL_ERROR_TABLE_NO_SUCH_KEY
  @desc         error return code: no such key in table
  @@*/
#define UTIL_ERROR_TABLE_NO_SUCH_KEY            (-103)

/*@@
  @defines      UTIL_ERROR_TABLE_WRONG_DATA_TYPE
  @desc         error return code: value associated with this key
                has the wrong data type for this function
  @@*/
#define UTIL_ERROR_TABLE_WRONG_DATA_TYPE        (-104)

/*@@
  @defines      UTIL_ERROR_TABLE_VALUE_IS_EMPTY
  @desc         error return code: value associated with this key
                is an empty (0-element) array
  @@*/
#define UTIL_ERROR_TABLE_VALUE_IS_EMPTY         (-105)

/*@@
  @defines      UTIL_ERROR_TABLE_ITERATOR_IS_NULL
  @desc         error return code: table iterator is in "null-pointer" state
  @@*/
#define UTIL_ERROR_TABLE_ITERATOR_IS_NULL       (-106)

/******************************************************************************/
/***** Main Table API *********************************************************/
/******************************************************************************/

/* create/destroy */
int Util_TableCreate(int flags);
int Util_TableDestroy(int handle);

/* query */
int Util_TableQueryFlags(int handle);
int Util_TableQueryNKeys(int handle);
int Util_TableQueryMaxKeyLength(int handle);
int Util_TableQueryValueInfo(int handle,
                             CCTK_INT *type_code, CCTK_INT *N_elements,
                             const char *key);

/* misc stuff */
int Util_TableDeleteKey(int handle, const char *key);

/* convenience routines to create and/or set from a "parameter-file" string */
int Util_TableCreateFromString(const char string[]);
int Util_TableSetFromString(int handle, const char string[]);

/* set/get a C-style null-terminated character string */
int Util_TableSetString(int handle,
                        const char *string,
                        const char *key);
int Util_TableGetString(int handle,
                        int buffer_length, char buffer[],
                        const char *key);

/**************************************/

/*
 * set routines
 */

/* pointers */
int Util_TableSetPointer(int handle, CCTK_POINTER value, const char *key);
int Util_TableSetFnPointer(int handle, CCTK_FN_POINTER value, const char *key);

/* a single character */
int Util_TableSetChar(int handle, CCTK_CHAR value, const char *key);

/* integers */
int Util_TableSetInt(int handle, CCTK_INT value, const char *key);
#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableSetInt2(int handle, CCTK_INT2 value, const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableSetInt4(int handle, CCTK_INT4 value, const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableSetInt8(int handle, CCTK_INT8 value, const char *key);
#endif

/* real numbers */
int Util_TableSetReal(int handle, CCTK_REAL value, const char *key);
#ifdef CCTK_REAL_PRECISION_4
int Util_TableSetReal4(int handle, CCTK_REAL4 value, const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_8
int Util_TableSetReal8(int handle, CCTK_REAL8 value, const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_16
int Util_TableSetReal16(int handle, CCTK_REAL16 value, const char *key);
#endif

/* complex numbers */
int Util_TableSetComplex(int handle, CCTK_COMPLEX value, const char *key);
#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableSetComplex8(int handle, CCTK_COMPLEX8 value, const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableSetComplex16(int handle, CCTK_COMPLEX16 value, const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableSetComplex32(int handle, CCTK_COMPLEX32 value, const char *key);
#endif

/**************************************/

/* arrays of pointers */
int Util_TableSetPointerArray(int handle,
                              int N_elements, const CCTK_POINTER array[],
                              const char *key);
int Util_TableSetFnPointerArray(int handle,
                                int N_elements, const CCTK_FN_POINTER array[],
                                const char *key);

/* arrays of characters (i.e. character strings with known length) */
/* note null termination is *not* required or enforced */
int Util_TableSetCharArray(int handle,
                           int N_elements, const CCTK_CHAR array[],
                           const char *key);

/* arrays of integers */
int Util_TableSetIntArray(int handle,
                          int N_elements, const CCTK_INT array[],
                          const char *key);
#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableSetInt2Array(int handle,
                           int N_elements, const CCTK_INT2 array[],
                           const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableSetInt4Array(int handle,
                           int N_elements, const CCTK_INT4 array[],
                           const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableSetInt8Array(int handle,
                           int N_elements, const CCTK_INT8 array[],
                           const char *key);
#endif

/* arrays of real numbers */
int Util_TableSetRealArray(int handle,
                           int N_elements, const CCTK_REAL array[],
                           const char *key);
#ifdef CCTK_REAL_PRECISION_4
int Util_TableSetReal4Array(int handle,
                            int N_elements, const CCTK_REAL4 array[],
                            const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_8
int Util_TableSetReal8Array(int handle,
                            int N_elements, const CCTK_REAL8 array[],
                            const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_16
int Util_TableSetReal16Array(int handle,
                             int N_elements, const CCTK_REAL16 array[],
                             const char *key);
#endif

/* arrays of complex numbers */
int Util_TableSetComplexArray(int handle,
                              int N_elements, const CCTK_COMPLEX array[],
                              const char *key);
#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableSetComplex8Array(int handle,
                               int N_elements, const CCTK_COMPLEX8 array[],
                               const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableSetComplex16Array(int handle,
                                int N_elements, const CCTK_COMPLEX16 array[],
                                const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableSetComplex32Array(int handle,
                                int N_elements, const CCTK_COMPLEX32 array[],
                                const char *key);
#endif

/**************************************/

/*
 * get routines
 */

/* pointers */
int Util_TableGetPointer(int handle, CCTK_POINTER *value, const char *key);
int Util_TableGetFnPointer(int handle, CCTK_FN_POINTER *value, const char *key);

/* a single character */
int Util_TableGetChar(int handle, CCTK_CHAR *value, const char *key);

/* integers */
int Util_TableGetInt(int handle, CCTK_INT *value, const char *key);
#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableGetInt2(int handle, CCTK_INT2 *value, const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableGetInt4(int handle, CCTK_INT4 *value, const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableGetInt8(int handle, CCTK_INT8 *value, const char *key);
#endif

/* real numbers */
int Util_TableGetReal(int handle, CCTK_REAL *value, const char *key);
#ifdef CCTK_REAL_PRECISION_4
int Util_TableGetReal4(int handle, CCTK_REAL4 *value, const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_8
int Util_TableGetReal8(int handle, CCTK_REAL8 *value, const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_16
int Util_TableGetReal16(int handle, CCTK_REAL16 *value, const char *key);
#endif

/* complex numbers */
int Util_TableGetComplex(int handle, CCTK_COMPLEX *value, const char *key);
#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableGetComplex8(int handle, CCTK_COMPLEX8 *value, const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableGetComplex16(int handle, CCTK_COMPLEX16 *value, const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableGetComplex32(int handle, CCTK_COMPLEX32 *value, const char *key);
#endif

/**************************************/

/* arrays of pointers */
int Util_TableGetPointerArray(int handle,
                              int N_elements, CCTK_POINTER array[],
                              const char *key);
int Util_TableGetFnPointerArray(int handle,
                                int N_elements, CCTK_FN_POINTER array[],
                                const char *key);

/* arrays of characters (i.e. character strings of known length) */
/* note null termination is *not* required or enforced */
int Util_TableGetCharArray(int handle,
                           int N_elements, CCTK_CHAR array[],
                           const char *key);

/* integers */
int Util_TableGetIntArray(int handle,
                          int N_elements, CCTK_INT array[],
                          const char *key);
#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableGetInt2Array(int handle,
                           int N_elements, CCTK_INT2 array[],
                           const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableGetInt4Array(int handle,
                           int N_elements, CCTK_INT4 array[],
                           const char *key);
#endif
#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableGetInt8Array(int handle,
                           int N_elements, CCTK_INT8 array[],
                           const char *key);
#endif

/* real numbers */
int Util_TableGetRealArray(int handle,
                           int N_elements, CCTK_REAL array[],
                           const char *key);
#ifdef CCTK_REAL_PRECISION_4
int Util_TableGetReal4Array(int handle,
                            int N_elements, CCTK_REAL4 array[],
                            const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_8
int Util_TableGetReal8Array(int handle,
                            int N_elements, CCTK_REAL8 array[],
                            const char *key);
#endif
#ifdef CCTK_REAL_PRECISION_16
int Util_TableGetReal16Array(int handle,
                             int N_elements, CCTK_REAL16 array[],
                             const char *key);
#endif

/* complex numbers */
int Util_TableGetComplexArray(int handle,
                              int N_elements, CCTK_COMPLEX array[],
                              const char *key);
#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableGetComplex8Array(int handle,
                               int N_elements, CCTK_COMPLEX8 array[],
                               const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableGetComplex16Array(int handle,
                                int N_elements, CCTK_COMPLEX16 array[],
                                const char *key);
#endif
#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableGetComplex32Array(int handle,
                                int N_elements, CCTK_COMPLEX32 array[],
                                const char *key);
#endif

/******************************************************************************/
/***** Table Iterator API *****************************************************/
/******************************************************************************/

/* create/destroy */
int Util_TableItCreate(int handle);
int Util_TableItDestroy(int ihandle);

/* test for "null-pointer" state */
int Util_TableItQueryIsNull(int ihandle);
int Util_TableItQueryIsNonNull(int ihandle);

/* query what the iterator points to */
int Util_TableItQueryTableHandle(int ihandle);
int Util_TableItQueryKeyValueInfo(int ihandle,
                                  int key_buffer_length, char key_buffer[],
                                  CCTK_INT *type_code, CCTK_INT *N_elements);

/* change value of iterator */
int Util_TableItAdvance(int ihandle);
int Util_TableItResetToStart(int ihandle);
int Util_TableItSetToNull(int ihandle);
int Util_TableItSetToKey(int ihandle, const char *key);

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

#ifdef __cplusplus
}
#endif

#endif  /* _UTIL_TABLE_H_ */
