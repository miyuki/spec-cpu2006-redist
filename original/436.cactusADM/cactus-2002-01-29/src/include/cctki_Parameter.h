 /*@@
   @header    cctki_Parameter.h
   @date      Wed Sep 15 22:49:24 1999
   @author    Tom Goodale
   @desc 
              Internal parameter prototypes
   @enddesc 
   @version   $Header: /cactus/Cactus/src/include/cctki_Parameter.h,v 1.3 2002/01/02 12:24:40 tradke Exp $
 @@*/

#ifndef _CCTKI_PARAMETER_H_
#define _CCTKI_PARAMETER_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTKi_SetParameter (const char *parameter, const char *value, int lineno);
int CCTKi_NumParameterFileErrors (int level);

#ifdef __cplusplus
}
#endif

#endif /* _CCTKI_PARAMETER_H_ */
