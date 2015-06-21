 /*@@
   @header    cctk_GHExtensions.h
   @date      Fri Jan 15 14:15:20 1999
   @author    Tom Goodale
   @desc 
   Prototypes for functions dealing with GH extensions.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctk_GHExtensions.h,v 1.11 2001/05/19 18:05:19 tradke Exp $
 @@*/

#ifndef _CCTK_GHEXTENSIONS_H_
#define _CCTK_GHEXTENSIONS_H_

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTK_RegisterGHExtension(const char *name);
int CCTK_UnregisterGHExtension(const char *name);

int CCTK_RegisterGHExtensionSetupGH(int handle, 
                                    void *(*func)(tFleshConfig *, int, cGH *));

int CCTK_RegisterGHExtensionInitGH(int handle, 
                                   int (*func)(cGH *));

int CCTK_RegisterGHExtensionScheduleTraverseGH(int handle, 
                                               int (*func)(cGH *, const char *));

int CCTK_GHExtensionHandle(const char *name);

void *CCTK_GHExtension(const cGH *GH, const char *name);

#ifdef __cplusplus
}
#endif

#endif /* _CCTK_GHEXTENSIONS_H_ */

