#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      StoreNamedData.c
   @date      Tue Sep  1 09:57:57 1998
   @author    Tom Goodale
   @desc 
   Contains routines to store pointers to miscellaneous named data in
   a linked list, and to search the list for a particular piece of data.
   @enddesc 

   @version $ID$
 @@*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "StoreNamedData.h"
#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/StoreNamedData.c,v 1.5 2001/05/10 12:35:22 goodale Exp $";

CCTK_FILEVERSION(util_StoreNamedData_c)
 

 /*@@
   @routine    StoreNamedData
   @date       Tue Sep  1 09:59:09 1998
   @author     Tom Goodale
   @desc 
   Stores a piece of data in a linked list.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     list
   @vdesc   A pointer to the list to store the data in
   @vtype   pNamedData **
   @vio     inout
   @vcomment 
   This should be a pointer to NULL to create a new list.
   @endvar 
   @var     name
   @vdesc   A string containing the name of the data item
   @vtype   const char *
   @vio     in
   @vcomment 
   A new string is created and the data from this string is copied into it.
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
int StoreNamedData(pNamedData **list, const char *name, void *data)
{
  int return_code;

  pNamedData *new;

  /* Allocate memory for the new element on the list. */
  new = (pNamedData *)malloc(sizeof(pNamedData));

  if(new)
  {
    /* Allocate memory for name field in the structure. */
    new->name = malloc((strlen(name)+1)*sizeof(char));
    if(new->name)
    {
      /* Copy the name. */
      strcpy(new->name, name);

      /* Store the data. */
      new->data = data;

      /* Link it into the list. */
      new->next = (*list);
      new->last = NULL;
      
      if(new->next) new->next->last = new;

      (*list) = new;

      return_code = 0;
    }
    else
    {
      free(new);
      return_code = 1;
    };
  }
  else
  {
    return_code = 1;
  };


  return return_code;

}

 /*@@
   @routine    GetNamedData
   @date       Tue Sep  1 10:11:10 1998
   @author     Tom Goodale
   @desc 
   Searches a list of named data items and returns the appropriate data.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     list
   @vdesc   The list to store the data in
   @vtype   pNamedData *
   @vio     in
   @vcomment 
   If this is a pointer to NULL, the routine returns immediately.
   @endvar 
   @var     name
   @vdesc   A string containing the name of the data item
   @vtype   const char *
   @vio     in
   @vcomment 
   The routine searches for an exact match, case dependent.
   @endvar 

   @returntype void *
   @returndesc
   This routine returns
      a pointer to the data on success
      NULL if the list is NULL or the name was not found.
   @endreturndesc
@@*/
void *GetNamedData(pNamedData *list, const char *name)
{
  void *return_val;

  pNamedData *current;

  return_val = NULL;

  if(list)
  {
    /* Traverse the list */
    for(current = list; current ; current = current->next)
    {
      /* Compare the name */
      if(!strcmp(current->name, name))
      {
        return_val = current->data;
        break;
      };
    };
  };

  return return_val;
}


 /*@@
   @routine    DestroyNamedDataList
   @date       Tue Sep  1 10:40:49 1998
   @author     Tom Goodale
   @desc 
   Frees the memory allocated to a a list of named data items. 
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     list
   @vdesc   The list to destroy.
   @vtype   pNamedData *
   @vio     in
   @vcomment 
   If this is a pointer to NULL, the routine returns immediately.
   @endvar 

@@*/
void DestroyNamedDataList(pNamedData *list)
{
  pNamedData *current;
  pNamedData *next;
  
  /* Traverse the list freeing memory. */
  for(current = list; current; current = next)
  {
    next = current->next;

    free(current);
  };

}


/*#define TEST_StoreNamedData*/
#ifdef TEST_StoreNamedData

/* Test routine to allow the above code to be tested independently of
 * other code. 
 */

static char first_name[]  = "First Item";
static char first_data[]  = "First Data";
static char second_name[] = "Second Item";
static char second_data[] = "Second Data";
static char third_name[]  = "Third Item";
static char third_data[]  = "Third Data";

#define DETECT_ERROR(error) if(error) {printf("Error on line %d\n", __LINE__); exit(1);}

int main(void)
{
  pNamedData *list;

  char *data;

  list = NULL; 

  /* Test creation of the list. */
  DETECT_ERROR(StoreNamedData(&list, first_name, first_data));
  DETECT_ERROR(StoreNamedData(&list, second_name, second_data));
  DETECT_ERROR(StoreNamedData(&list, third_name, third_data));

  /* Test accessing the data. */
  if((data = GetNamedData(list, first_name)))
  {
    printf("Name %s has data %s\n", first_name, data);
  };

  if((data = GetNamedData(list, second_name)))
  {
    printf("Name %s has data %s\n", second_name, data);
  };

  if((data = GetNamedData(list, third_name)))
  {
    printf("Name %s has data %s\n", third_name, data);
  };

  /* test destroying the list. */
  DestroyNamedDataList(list);

  list = NULL;


  return 0;

}

#endif
