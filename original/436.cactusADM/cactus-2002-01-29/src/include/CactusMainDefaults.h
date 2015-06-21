 /*@@
   @header    CactusMainDefaults.h
   @date      Tue Sep 29 12:40:39 1998
   @author    Tom Goodale
   @desc 
   Prototypes for default functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/CactusMainDefaults.h,v 1.5 2000/07/14 23:35:43 allen Exp $
 @@*/

#ifndef _CACTUSMAINDEFAULTS_H_
#define _CACTUSMAINDEFAULTS_H_

#ifdef __cplusplus
extern "C" {
#endif

int CactusDefaultInitialise(tFleshConfig *);
int CactusDefaultEvolve(tFleshConfig *);
int CactusDefaultShutdown(tFleshConfig *);
int CactusDefaultExit(cGH *GH, int retval);
#ifdef __cplusplus
}
#endif

#endif
