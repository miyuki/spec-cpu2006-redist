 /*@@
   @header    cctk_Bindings.h
   @date      Tue Jun  1 16:42:28 1999
   @author    Tom Goodale
   @desc 
   Accessable functions from the bindings.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctki_Bindings.h,v 1.1 2000/07/19 13:08:48 allen Exp $
 @@*/

#ifndef _CCTKI_BINDINGS_H_
#define _CCTKI_BINDINGS_H_

#ifdef __cplusplus
extern "C" {
#endif

int CCTKi_BindingsParametersInitialise(void);
int CCTKi_BindingsVariablesInitialise(void);
int CCTKi_BindingsScheduleInitialise(void);

int CCTKi_BindingsScheduleRegister(const char *type, void *data);

#ifdef __cplusplus
}
#endif


#endif
