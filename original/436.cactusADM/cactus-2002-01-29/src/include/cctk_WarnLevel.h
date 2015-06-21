 /*@@
   @header    cctk_WarnLevel.h
   @date      Wed Feb 17 00:53:55 1999
   @author    Tom Goodale
   @desc 
   Header for the warning functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_WarnLevel.h,v 1.8 2000/04/10 14:47:11 tradke Exp $
 @@*/

#ifndef _CCTK_WARNLEVEL_H_
#define _CCTK_WARNLEVEL_H_

#ifdef __cplusplus 
extern "C" 
{
#endif

int CCTK_Warn(int level, 
               int line, 
               const char *file, 
               const char *thorn, 
               const char *message);
int CCTK_VWarn(int level, 
                int line, 
                const char *file, 
                const char *thorn, 
                const char *format, ...);
int CCTK_ParamWarn(const char *thorn, const char *message);
int CCTK_Info(const char *thorn, const char *message);
int CCTK_VInfo(const char *thorn, const char *format, ...);

#ifdef __cplusplus 
}
#endif

#endif
