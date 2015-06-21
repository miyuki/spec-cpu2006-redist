 /*@@
   @header    CactusRegister.h
   @date      Tue Sep 29 11:37:03 1998
   @author    Tom Goodale
   @desc 
   Functions used to register things in cactus.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/CactusRegister.h,v 1.4 2000/07/19 13:08:47 allen Exp $
 @@*/

#ifndef _CACTUS_REGISTRY_H_
#define _CACTUS_REGISTRY_H_

#ifdef __cplusplus
extern "C" {
#endif

int RegisterMainFunction(int key, int (*func)(tFleshConfig *));

int CCTKi_SetupMainFunctions(void);
int CCTKi_SetupCommFunctions(void);
int CCTKi_SetupIOFunctions(void);
int CCTKi_BindingsImplementationsInitialise(void);
int CCTKi_BindingsParametersInitialise(void);

#ifdef __cplusplus
}
#endif

#endif
