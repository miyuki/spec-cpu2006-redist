 /*@@
   @header    cctk_Malloc.h
   @date      Thu Jan 20 2000
   @author    Gerd Lanfermann
   @desc
   Prototypes for Cactus MemAlloc functions.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctk_Malloc.h,v 1.3 2000/04/19 22:25:26 allen Exp $
 @@*/

#ifndef _CCTK_MALLOC_H_
#define _CCTK_MALLOC_H_

#ifdef __cplusplus
extern "C" {
#endif

/* void *CCTKi_Malloc(t_size size, int line, const char *file);
   void CCTKi_Free(void *pointer); */

void CCTK_MemStat(void);
unsigned long int  CCTK_TotalMemory(void);

long int CCTK_MemTicketCash(int this_ticket);
int CCTK_MemTicketRequest(void);
int CCTK_MemTicketDelete(int this_ticket);


#ifdef __cplusplus
}
#endif

#endif


