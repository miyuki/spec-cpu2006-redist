#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      StoreHandledData.c
   @date      Mon Feb  1 17:54:26 1999
   @author    Tom Goodale
   @desc 
   Stores data referenced by a handle
   @enddesc 
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "StoreHandledData.h"

static const char *rcsid="$Header: /cactus/Cactus/src/util/StoreHandledData.c,v 1.13 2001/07/03 21:49:41 tradke Exp $";

CCTK_FILEVERSION(util_StoreHandledData_c)

/* Purely internal definitions. */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif


static int FindNextUnused(cHandledData *storage, int first);


 /*@@
   @routine    Util_NewHandle
   @date       Fri May  8 12:56:43 1998
   @author     Tom Goodale
   @desc 
   Adds an data object to the array.
   Resizes the array if necessary.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int Util_NewHandle(cHandledData **storage, const char *name, void *data)
{
  int return_code;

  void *temp;
  
  if(*storage == NULL)
  {
    *storage = (cHandledData *)malloc(sizeof(cHandledData));

    if(*storage)
    {
      (*storage)->array = NULL;
      (*storage)->array_size = 0;
      (*storage)->first_unused = 0;
    }
    else
    {
      return_code = -1;
    }
  }
  
  if(*storage)
  {
    /* Check if the array needs to be resized. */
    if((*storage)->first_unused <= (*storage)->array_size)
    {
      if(!(temp = (void *)realloc((*storage)->array, ((*storage)->array_size+1)*sizeof(cHandleStorage))))
      {
        /* Failed to allocate memory for new array element. */
        
        return_code = -2;
      }
      else
      {
        (*storage)->array = temp;
        
        /* Fill in data in array. */
        (*storage)->array[(*storage)->array_size].in_use = TRUE;
        (*storage)->array[(*storage)->array_size].data = data;
        (*storage)->array[(*storage)->array_size].name = (char *)malloc((strlen(name)+1)*sizeof(char));
        
        if((*storage)->array[(*storage)->array_size].name)
        {
          strcpy((*storage)->array[(*storage)->array_size].name, name);
        }
        
        return_code = (*storage)->array_size;
        
        /* Increase array size counter. */
        (*storage)->array_size++;
        
        /* Record position of first unused array element. */
        (*storage)->first_unused = (*storage)->array_size;
      }
    }
    else
    {
      /* There's an available element in the array. */

      if((*storage)->array[(*storage)->first_unused].in_use == TRUE)
      {
        /* The pointers have become corrupted in some fashion.
         *
         * Could write a repair function, but probably safer to just
         * produce an error.
         */
        return_code = -2;
      }
      else
      {      
        /* Fill in data in array. */
        (*storage)->array[(*storage)->first_unused].in_use = TRUE;
        (*storage)->array[(*storage)->first_unused].data = data;
      
        return_code = (*storage)->first_unused;
      
        /* Change pointer to first unused array element. */
        (*storage)->first_unused = FindNextUnused(*storage, (*storage)->first_unused);
      }
    }
  }
  else
  {
    return_code = -1;
  }

  return return_code;

}

 /*@@
   @routine    Util_DeleteHandle
   @date       Fri May  8 14:13:16 1998
   @author     Tom Goodale
   @desc 
   Removes a data object from the array.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int Util_DeleteHandle(cHandledData *storage, int handle)
{
  int return_code;

  if((handle >= 0)&&((unsigned int)handle < storage->array_size))
  {
    /* It's a valid handle. */
    storage->array[handle].in_use = FALSE;
    storage->array[handle].data = NULL;
    free(storage->array[handle].name);
    storage->array[handle].name = NULL;

    if((unsigned int)handle < storage->first_unused)
    {
      storage->first_unused = handle;
    };

    return_code = 0;
  }
  else
  {
    /* The handle does not exist. */
    return_code = 1;
  };

  return return_code;

}


 /*@@
   @routine    FindNextUnused
   @date       Fri May  8 14:09:13 1998
   @author     Tom Goodale
   @desc 
   Finds the next unused element in the handle array.
   Returns the size of the array if all are in use.
   Assumes there are no unused ones before the value of the `first' parameter.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

static int FindNextUnused(cHandledData *storage, int first)
{
  unsigned int current;

  current = first+1;

  /* Loop until the end of the array. */

  while(current < storage->array_size)
  {
    if(storage->array[current].in_use==FALSE) break;

    current++;
  }
    
  return current;

}
    
 /*@@
   @routine    Util_GetHandledData
   @date       Fri May  8 17:36:33 1998
   @author     Tom Goodale
   @desc 
   Gets a pointer to the data corresponding to the give handle.
   @enddesc 
   @calls     
   @calledby
   @history 
 
   @endhistory 

@@*/
void *Util_GetHandledData(cHandledData *storage, int handle)
{
  void *data;

  if(storage)
  {
    if((handle >= 0)&&
       ((unsigned int)handle < storage->array_size)&&
       (storage->array[handle].in_use == TRUE))
    {
      /* The data exists */
      data = storage->array[handle].data;
    }
    else
    {
      /* The data is non-existant. */
      data = NULL;
    }
  }
  else
  {
    /* There is no data registered. */
    data = NULL;
  }

  return data;
}

 /*@@
   @routine    Util_GetHandle
   @date       Tue Feb  2 10:55:34 1999
   @author     Tom Goodale
   @desc 
   Gets the handle associated with a piece of data.  Also returns the data
   associated with the handle for speed.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
int Util_GetHandle(cHandledData *storage, const char *name, void **data)
{
  int handle;
  unsigned int current;

  handle = -1;

  if(data)
  {
    *data = NULL;
  }

  if(storage)
  {
    for(current = 0; current < storage->array_size; current++)
    {
      if(storage->array[current].in_use == TRUE)
      {
        if(!strcmp(name, storage->array[current].name))
        {
          handle = current;
          /* Return the associated data if required. */
          if(data)
          {
            *data = storage->array[current].data;
          };
          break;
        }
      }
    }
  }
  else
  {
    handle = -2;
  }

  return handle;
}

 /*@@
   @routine    Util_GetHandleName
   @date       Wed Feb  3 12:52:53 1999
   @author     Tom Goodale
   @desc 
   Gets the name associated with a handle.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
const char *Util_GetHandleName(cHandledData *storage, int handle)
{
  const char *name;

  if(storage)
  {
    if((handle >= 0)&&
       ((unsigned int)handle < storage->array_size)&&       
       (storage->array[handle].in_use == TRUE))
    {
      /* The data exists */
      name = storage->array[handle].name;
    }
    else
    {
      /* The data is non-existant. */
      name = NULL;
    }
  }
  else
  {
    /* There is no data registered. */
    name = NULL;
  }

  return name;
}
  

/*#define TEST_STOREHANDLEDDATA */
#ifdef TEST_STOREHANDLEDDATA

/* Test routine to allow the above code to be tested independently of
 * other code. 
 */

static char first_name[]  = "First Item";
static char first_data[]  = "First Data";
static char second_name[] = "Second Item";
static char second_data[] = "Second Data";
static char third_name[]  = "Third Item";
static char third_data[]  = "Third Data";

int main(void)
{
  cHandledData *handledata;

  char *data;

  int handle, handle1, handle2, handle3;

  handledata = NULL; 


  /* Test creation of the data. */
  handle1 = Util_NewHandle(&handledata, first_name, first_data);
  handle2 = Util_NewHandle(&handledata, second_name, second_data);
  handle3 = Util_NewHandle(&handledata, third_name, third_data);

  /* Test accessing the data. */
  if((data = Util_GetHandledData(handledata, handle1)))
  {
    printf("Name %s (%d) has data %s\n", first_name, handle1, data);
  };

  if((data = Util_GetHandledData(handledata, handle2)))
  {
    printf("Name %s (%d) has data %s\n", second_name, handle2, data);
  };

  if((data = Util_GetHandledData(handledata, handle3)))
  {
    printf("Name %s (%d) has data %s\n", third_name, handle3, data);
  };

  /* Test getting by name */

  if((handle = Util_GetHandle(handledata, first_name, (void **)&data)) > -1)
  {
    printf("Name %s (%d, was %d) has data %s\n", first_name, handle, handle1, data);
  };

  if((handle = Util_GetHandle(handledata, second_name, (void **)&data)) > -1)
  {
    printf("Name %s (%d, was %d) has data %s\n", second_name, handle, handle2, data);
  };

  if((handle = Util_GetHandle(handledata, third_name, (void **)&data)) > -1)
  {
    printf("Name %s (%d, was %d) has data %s\n", third_name, handle, handle3, data);
  };


  return 0;

}

#endif
  
