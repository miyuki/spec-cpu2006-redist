 /*@@
   @header    RegisterKeyedFunction.h
   @date      Tue Sep 29 09:41:49 1998
   @author    Tom Goodale
   @desc 
   Header file for keyed function registration.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/RegisterKeyedFunction.h,v 1.5 2000/04/19 15:29:37 goodale Exp $
 @@*/

#ifndef _REGISTERKEYEDFUNCTION_H_
#define _REGISTERKEYEDFUNCTION_H_ 1

/*****************************************************/
/* Function prototypes. */

#ifdef __cplusplus
extern "C" 
{
#endif

int RegisterKeyedFunction(void (*array[])(void), 
                          int min, int max, 
                          int key, void (*func)(void));

void  (**(CreateKeyedFunctionArray(int size)))(void);

#ifdef __cplusplus
}
#endif

/*****************************************************/

/* Possible return codes. */

enum RegisterKeyedFunctionErrors {REG_KEYED_FUNCTION_SUCCESS, 
                                  REG_KEYED_FUNCTION_ALREADY_ASSIGNED,
                                  REG_KEYED_FUNCTION_RANGE_ERROR};


/*****************************************************/


#endif
