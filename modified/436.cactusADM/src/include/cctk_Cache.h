 /*@@
   @header    cctk_Cache.h
   @date      Tue Nov 30 09:31:45 1999
   @author    Tom Goodale
   @desc 
   Prototypes for cache routines.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Cache.h,v 1.3 2000/11/23 21:41:53 tradke Exp $
 @@*/

#ifndef _CCTK_CACHE_H_
#define _CCTK_CACHE_H_

#ifdef __cplusplus
extern "C"
{
#endif

void *Util_CacheMalloc(unsigned _index, 
                       unsigned long size, 
                       void **realstart);

#ifdef __cplusplus
}
#endif

#endif /* _CCTK_CACHE_H_ */
