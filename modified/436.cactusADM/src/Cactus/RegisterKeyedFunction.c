#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      RegisterKeyedFunction.c
   @date      Tue Sep 29 09:39:55 1998
   @author    Tom Goodale
   @desc 
   Routines to register keyed functions.
   @enddesc 
 @@*/
#include <stdio.h>
#include <stdlib.h>
#include "cctk_Flesh.h"
#include "RegisterKeyedFunction.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/RegisterKeyedFunction.c,v 1.6 2001/05/10 12:35:21 goodale Exp $";

CCTK_FILEVERSION(util_RegisterKeyedFunction_c)

 /*@@
   @routine    RegisterKeyedFunction
   @date       Tue Sep 29 09:41:08 1998
   @author     Tom Goodale
   @desc 
   Registers a function with a key between the minimum and maximum (inclusive).
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int RegisterKeyedFunction(void (*array[])(void), 
                          int min, int max, 
                          int key, void (*func)(void))
{
  int return_code;

  if(key >= min && key <= max)
  {
    if(array[key] == NULL)
    {
      array[key] = func;
      return_code = REG_KEYED_FUNCTION_SUCCESS;
    }
    else
    {
      return_code = REG_KEYED_FUNCTION_ALREADY_ASSIGNED;
    };
  }
  else
  { 
    return_code = REG_KEYED_FUNCTION_RANGE_ERROR;
  };

  return return_code;
}

 /*@@
   @routine    CreateKeyedFunctionArray
   @date       Tue Sep 29 11:16:51 1998
   @author     Tom Goodale
   @desc 

   This creates a keyed function array and initialises it to NULL.

   Function which returns a pointer to a pointer to a function which returns void.
   (An array of pointers to functions which return void.
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
void  (**(CreateKeyedFunctionArray(int size)))(void)
{
  void (**array)(void);
  int i;

  /* Allocate the memory. */
  array = (void (**)(void))malloc(size*sizeof(void (*)(void)));

  if(array)
  {
    /* Initialise the data. */
    for(i = 0; i < size; i++)
    {
      array[i] = NULL;
    };
  };

  return array;
}
    


#ifdef TEST_KEYED_FUNCTIONS

static void (**functions)(void);

void RegisterTestFunction(int key, void (*func)(void))
{
  int retcode;
  if((retcode = RegisterKeyedFunction(functions, 0, 2, key, func)) == 1)
  {
    fprintf(stderr, "Test function %d already registered.\n", key);
  }
  else if(retcode == 2)
  {
    fprintf(stderr, "Unknown test function %d\n", key);
  }
}

#define CREATE_FUNC(x)  void function ## _ ## x (void) { printf("I'm function %d\n", x); }

CREATE_FUNC(0)
CREATE_FUNC(1)
CREATE_FUNC(2)
CREATE_FUNC(3)
CREATE_FUNC(4)
 
#define FUNC(x) function ## _ ## x

#define REGTEST(x) RegisterTestFunction(x, FUNC(x))

int main(int argc, char *argv[])
{
  int i;
  void (*test)();

  functions = CreateKeyedFunctionArray(3);

  if(!functions)
  {
    fprintf(stderr, "Function array is still null !\n");

    exit(1);
  };


  REGTEST(0);
  REGTEST(1);
  REGTEST(2);
  REGTEST(3);
  REGTEST(4);

  REGTEST(0);
  REGTEST(1);
  REGTEST(2);
  REGTEST(3);
  REGTEST(4);

  for(i = 0; i < 3; i++)
  {
    test = functions[i];

    if(test)
    {
      test();
    }
    else
    {
      printf("Error test function %d is null !!!\n", i);
    };

  };

}

#endif
