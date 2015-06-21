 /*@@
   @header    cctk_MemAlloc.h
   @date      Thu Jan 20 2000
   @author    Gerd Lanfermann
   @desc
   Prototypes for Cactus MemAlloc functions.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctk_MemAlloc.h,v 1.2 2000/03/07 11:18:54 goodale Exp $
 @@*/

#ifndef _CCTK_MEMALLOC_H_
#define _CCTK_MEMALLOC_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

void *CCTK_Malloc(size_t size, int line, const char *file);
void  CCTK_Free(void *pointer);
unsigned long int CCTK_TotalMemory(void);

#ifdef __cplusplus
}
#endif

#endif /* _CCTK_MEMALLOC_H_ */


