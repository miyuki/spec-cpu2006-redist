 /*@@
   @header    cctk_Misc.h
   @date      Wed Jan 20 10:39:01 1999
   @author    Tom Goodale
   @desc 
   header file for miscellaneous routines.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Misc.h,v 1.9 2001/12/11 20:54:22 rideout Exp $
 @@*/

#include "cctk_Types.h"

#ifndef _CCTK_MISC_H_
#define _CCTK_MISC_H_

#ifdef __cplusplus
extern "C" {
#endif

int Util_SplitString(char **before, char **after, const char *string, const char *sep);

int CCTK_Equals(const char *string1, const char *string2);

char *Util_NullTerminateString(const char *, unsigned int);

int Util_InList(const char *string1, int n_elements, ...);

int Util_IntInRange(int inval, const char *range);
int Util_DoubleInRange(double inval, const char *range);
int Util_IntInRangeList(int inval, int n_elements, ...);
int Util_DoubleInRangeList(double inval, int n_elements, ...);

int CCTK_SetDoubleInRangeList(CCTK_REAL *data, const char *value, 
                              int n_elements, ...);
int CCTK_SetIntInRangeList(CCTK_INT *data, const char *value, 
                           int n_elements, ...);
int CCTK_SetKeywordInRangeList(char **data, const char *value, 
                               int n_elements, ...);
int CCTK_SetString(char **data, const char *value);
int CCTK_SetBoolean(CCTK_INT *data, const char *value);

int CCTK_RunTime(void);
int Util_CurrentTime(int len, char *now);
int Util_CurrentDate(int len, char *now);

int CCTK_RunTitle(int len, char *title);

#ifdef __cplusplus
}   
#endif

#endif
