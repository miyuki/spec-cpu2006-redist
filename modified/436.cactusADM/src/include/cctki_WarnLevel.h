 /*@@
   @header    cctki_WarnLevel.h
   @date      Wed Feb 17 00:53:55 1999
   @author    Tom Goodale
   @desc
              Header for the internal warning functions.
   @enddesc
   @version   $Id: cctki_WarnLevel.h,v 1.3 2001/04/20 21:38:29 tradke Exp $
 @@*/

#ifndef _CCTKI_WARNLEVEL_H_
#define _CCTKI_WARNLEVEL_H_

#ifdef __cplusplus
extern "C"
{
#endif

int  CCTKi_SetWarnLevel (int level);
int  CCTKi_SetParameterLevel (int level);
int  CCTKi_SetErrorLevel (int level);
void CCTKi_FinaliseParamWarn (void);
void CCTKi_NotYetImplemented (const char *message);
void CCTKi_ExpectError (int in,
                        int err,
                        int warnonerr,
                        int line,
                        const char *file,
                        const char *thorn,
                        const char *message);
void CCTKi_ExpectOK (int in,
                     int ok,
                     int warnonerr,
                     int line,
                     const char *file,
                     const char *thorn,
                     const char *message);

#ifdef __cplusplus
}
#endif

#endif
