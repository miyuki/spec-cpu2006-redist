#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      StoreKeyedData.c
   @date      Tue Sep  1 09:57:57 1998
   @author    Tom Goodale
   @desc 
   Contains routines to store pointers to miscellaneous keyed data 
   and to search for a particular piece of data.
   @enddesc 

   @version $ID$
 @@*/
#include <stdio.h>
#include <stdlib.h>
#include "cctk_Flesh.h"
#include "StoreKeyedData.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/StoreKeyedData.c,v 1.4 2001/05/10 12:35:22 goodale Exp $";
 
CCTK_FILEVERSION(util_StoreKeyedData_c)

 /*@@
   @routine    StoreKeyedData
   @date       Tue Sep  1 09:59:09 1998
   @author     Tom Goodale
   @desc 
   Stores a piece of data.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     list
   @vdesc   A pointer to the list to store the data in
   @vtype   pKeyedData **
   @vio     inout
   @vcomment 
   This should be a pointer to NULL to create a new list.
   @endvar 
   @var     key
   @vdesc   An integer containing the key of the data item
   @vtype   int
   @vio     in
   @vcomment 

   @endvar 
   @var     data
   @vdesc   The data to store
   @vtype   void *
   @vio     in
   @vcomment 
   This is a void pointer to any sort of data.
   @endvar 

   @returntype int
   @returndesc
   This routine returns
      0 on success
      1 if memory allocation failed
   @endreturndesc
@@*/
int StoreKeyedData(pKeyedData **storage, int key, void *data)
{
  int return_code;

  pKeyedData *new;

  /* Allocate memory for the new element on the list. */
  new = (pKeyedData *)malloc(sizeof(pKeyedData));

  if(new)
  {
    /* Store the key */
    new->key = key;

    /* Store the data. */
    new->data = data;

    /* Link it into the list. */
    new->next = (*storage);
    new->last = NULL;
    
    if(new->next) new->next->last = new;
      
    (*storage) = new;
    
    return_code = 0;
  }
  else
  {
    return_code = 1;
  };


  return return_code;

}

 /*@@
   @routine    GetKeyedData
   @date       Tue Sep  1 10:11:10 1998
   @author     Tom Goodale
   @desc 
   Searches for a keyed data item and returns the appropriate data.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     list
   @vdesc   The keyed data storage the data in
   @vtype   pKeyedData *
   @vio     in
   @vcomment 
   If this is a pointer to NULL, the routine returns immediately.
   @endvar 
   @var     key
   @vdesc   An integer containing the key of the data item
   @vtype   int
   @vio     in
   @vcomment 

   @endvar 

   @returntype void *
   @returndesc
   This routine returns
      a pointer to the data on success
      NULL if the list is NULL or the key was not found.
   @endreturndesc
@@*/
void *GetKeyedData(pKeyedData *storage, int key)
{
  void *return_val;

  pKeyedData *current;

  return_val = NULL;

  if(storage)
  {
    /* Traverse the list */
    for(current = storage; current ; current = current->next)
    {
      /* Compare the key */
      if(current->key == key)
      {
        return_val = current->data;
        break;
      };
    };
  };

  return return_val;
}


 /*@@
   @routine    DestroyKeyedData
   @date       Tue Sep  1 10:40:49 1998
   @author     Tom Goodale
   @desc 
   Frees the memory allocated for keyed data storage. 
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     storage
   @vdesc   The storage to destroy.
   @vtype   pKeyedData *
   @vio     in
   @vcomment 
   If this is a pointer to NULL, the routine returns immediately.
   @endvar 

@@*/
void DestroyKeyedData(pKeyedData *storage)
{
  pKeyedData *current;
  pKeyedData *next;
  
  /* Traverse the list freeing memory. */
  for(current = storage; current; current = next)
  {
    next = current->next;

    free(current);
  };

}


/*#define TEST_StoreKeyedData*/
#ifdef TEST_StoreKeyedData

/* Test routine to allow the above code to be tested independently of
 * other code. 
 */

static int first_key  = 1;
static char first_data[]  = "First Data";
static int second_key = 2;
static char second_data[] = "Second Data";
static int third_key  = 3;
static char third_data[]  = "Third Data";

#define DETECT_ERROR(error) if(error) {printf("Error on line %d\n", __LINE__); exit(1);}

int main(void)
{
  pKeyedData *list;

  char *data;

  list = NULL; 

  /* Test creation of the list. */
  DETECT_ERROR(StoreKeyedData(&list, first_key, first_data));
  DETECT_ERROR(StoreKeyedData(&list, second_key, second_data));
  DETECT_ERROR(StoreKeyedData(&list, third_key, third_data));

  /* Test accessing the data. */
  if((data = GetKeyedData(list, first_key)))
  {
    printf("Key %d has data %s\n", first_key, data);
  };

  if((data = GetKeyedData(list, second_key)))
  {
    printf("Key %d has data %s\n", second_key, data);
  };

  if((data = GetKeyedData(list, third_key)))
  {
    printf("Key %d has data %s\n", third_key, data);
  };

  /* test destroying the list. */
  DestroyKeyedData(list);

  list = NULL;


  return 0;

}

#endif
