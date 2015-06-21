 /*@@
   @header    cctki_Cache.h
   @date      Tue Nov 30 09:44:46 1999
   @author    Tom Goodale
   @desc 
   CCTK internal cache stuff.   
   @enddesc 
 @@*/

#ifndef _CCTKI_CACHE_H_
#define _CCTKI_CACHE_H_

#ifdef __cplusplus
extern "C"
{
#endif

int Utili_CacheDataSet(unsigned long cacheline_bytes,
                       unsigned long cache_size);

int Utili_CacheDataGet(unsigned long *cacheline_bytes,
                       unsigned long *cache_size);

int CCTKi_SetupCache(void);

#ifdef __cplusplus
}
#endif

#endif /* _CCTKI_CACHE_H_ */
