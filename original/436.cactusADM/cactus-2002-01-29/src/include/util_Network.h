 /*@@
   @header    util_Network.c
   @date      Fri 19 Jan 2001
   @author    Thomas Radke
   @desc
              Network related routines
   @enddesc
   @version $Header: /cactus/Cactus/src/include/util_Network.h,v 1.1 2001/01/19 12:39:07 tradke Exp $
 @@*/

#ifndef _UTIL_NETWORK_H_
#define _UTIL_NETWORK_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

void Util_GetHostName (char *name, int length);

#ifdef __cplusplus
}
#endif

#endif /* _UTIL_NETWORK_H_ */
