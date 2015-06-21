 /*@@
   @header    util_String.h
   @date      Tue May  2 11:00:39 2000
   @author    Tom Goodale
   @desc 
   String routines
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/util_String.h,v 1.3 2001/05/24 15:22:36 goodale Exp $
 @@*/

#include <stdarg.h>

#ifndef _UTIL_STRING_H_
#define _UTIL_STRING_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

const char *Util_StrSep(const char **stringp, 
                        const char *delim);

int Util_SplitString(char **before, 
                     char **after, 
                     const char *string, 
                     const char *sep);

int Util_SplitFilename(char **dir, 
                       char **file, 
                       const char *string);

char *Util_Strdup(const char *s);

int Util_StrCmpi(const char *string1, 
                 const char *string2);

int Util_vsnprintf (char *str, size_t count, const char *fmt, va_list args);
int Util_snprintf (char *str,size_t count,const char *fmt,...);

int Util_asprintf(char **buffer, const char *fmt, ...);
int Util_asnprintf(char **buffer, size_t size, const char *fmt, ...);

#ifdef __cplusplus
}
#endif

#endif /* _UTIL_STRING_H_ */
