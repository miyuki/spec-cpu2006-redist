 /*@@
   @header    cctki_GHExtensions.h
   @date      Fri Jan 15 14:15:20 1999
   @author    Tom Goodale
   @desc 
   Prototypes for functions dealing with GH extensions.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctki_GHExtensions.h,v 1.3 2000/04/16 15:14:10 allen Exp $
 @@*/

#ifndef _CCTKI_GHEXTENSIONS_H_
#define _CCTKI_GHEXTENSIONS_H_

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTKi_SetupGHExtensions(tFleshConfig *config, 
                           int convergence_level, 
                           cGH *GH);

int CCTKi_InitGHExtensions(cGH *GH);

int CCTKi_ScheduleTraverseGHExtensions(cGH *GH, const char *where);

#ifdef __cplusplus
}
#endif

#endif /* _CCTKI_GHEXTENSIONS_H_ */

