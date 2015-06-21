 /*@@
   @header    cctk_Banner.h
   @date      Wed Oct 13 16:21:40 CEST 1999
   @author    Gabrielle Allen
   @desc 
   Prototypes and constants for banner functions
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Banner.h,v 1.2 2000/02/04 15:10:14 allen Exp $
 @@*/

#ifndef _CCTK_BANNER_H_
#define _CCTK_BANNER_H_

/* Prototypes */

#ifdef __cplusplus 
extern "C" {
#endif

int CCTK_RegisterBanner(const char *banner);

#ifdef __cplusplus 
}
#endif

#endif



