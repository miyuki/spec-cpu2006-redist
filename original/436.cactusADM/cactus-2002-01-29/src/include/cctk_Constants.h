 /*@@
   @header    cctk_Constants.h
   @date      Fri Oct 15 21:29:23 CEST 1999
   @author    Gabrielle Allen
   @desc 
   Constants used by Cactus
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctk_Constants.h,v 1.7 2001/11/13 16:21:57 goodale Exp $
 @@*/

#ifndef _CCTK_CONSTANTS_H_
#define _CCTK_CONSTANTS_H_

#define CCTK_VARIABLE_VOID        0
#define CCTK_VARIABLE_BYTE        1
#define CCTK_VARIABLE_INT         2
#define CCTK_VARIABLE_INT2        3
#define CCTK_VARIABLE_INT4        4
#define CCTK_VARIABLE_INT8        5
#define CCTK_VARIABLE_REAL        6
#define CCTK_VARIABLE_REAL4       7
#define CCTK_VARIABLE_REAL8       8
#define CCTK_VARIABLE_REAL16      9
#define CCTK_VARIABLE_COMPLEX    10
#define CCTK_VARIABLE_COMPLEX8   11
#define CCTK_VARIABLE_COMPLEX16  12
#define CCTK_VARIABLE_COMPLEX32  13
#define CCTK_VARIABLE_STRING     14
#define CCTK_VARIABLE_POINTER	 15
#define CCTK_VARIABLE_FN_POINTER 16

/* DEPRECATED IN BETA 10 */
#define CCTK_VARIABLE_CHAR       1


/* steerable status of parameters */
#define CCTK_STEERABLE_NEVER   0
#define CCTK_STEERABLE_ALWAYS  1
#define CCTK_STEERABLE_RECOVER 2

/* number of staggerings */
#define CCTK_NSTAGGER      3


#endif /* _CCTK_CONSTANTS_ */

