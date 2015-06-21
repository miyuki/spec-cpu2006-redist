#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      StringList.c
   @date      Mon May 21 16:55:14 2001
   @author    Tom Goodale
   @desc 
   Stuff for fixed-size lists of sorted unique strings.
   @enddesc
   @version $Header: /cactus/Cactus/src/util/StringList.c,v 1.3 2001/09/01 10:48:39 tradke Exp $
 @@*/
#include <stdio.h>
#include <stdlib.h>

#ifndef TEST_STRINGLIST
#include "cctk.h"
#endif

#include "util_String.h"
#include "util_StringList.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/StringList.c,v 1.3 2001/09/01 10:48:39 tradke Exp $";

#ifndef TEST_STRINGLIST
CCTK_FILEVERSION(util_StringList_c)
#endif

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    Util_StringListCreate
   @date       Mon May 21 17:00:45 2001
   @author     Tom Goodale
   @desc 
   Creates a string list object
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     size
   @vdesc   Size of the list
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype uStringList
   @returndesc
   This is the (opaque) stringlist object.
   @endreturndesc
@@*/
uStringList *Util_StringListCreate(int size)
{
  uStringList *this;

  this=(uStringList *)malloc(sizeof(uStringList));

  if(this)
  {
    this->list=calloc(size+1,sizeof(struct iInternalStringList));

    this->current = NULL;
    this->fill=0;
  }

  return this;
}

 /*@@
   @routine    Util_StringListAdd
   @date       Mon May 21 17:03:35 2001
   @author     Tom Goodale
   @desc 
   Adds a string to the stringlist.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     list
   @vdesc   The list object
   @vtype   uStringList
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     item
   @vdesc   The string to add
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   1  - added
   0  - duplicate
   -1 - internal error
   @endreturndesc
@@*/
int Util_StringListAdd(uStringList *list, const char *item)
{
  int retval;
  int position;
  struct iInternalStringList *this, *prev;

  if(list->fill == 0)
  {
    list->head = list->list;
    list->head->string = Util_Strdup(item);
    list->head->next   = NULL;
    list->fill++;
    retval = 1;
  }
  else
  {
    retval = -1;
    prev=NULL;
    for(this=list->head; this; this=this->next)
    {
      if((position = Util_StrCmpi(item,this->string)) < 0)
      {
        list->list[list->fill].string = Util_Strdup(item);
        list->list[list->fill].next   = this;
        if(prev)
        {
          prev->next= &(list->list[list->fill]);
        }
        else
        {
          list->head=&(list->list[list->fill]);
        }

        list->fill++;
        retval = 1;
        break;
      }
      else if (position == 0)
      {
        retval = 0;
        break;
      }

      prev = this;
    }
    
    if(!this)
    {
      list->list[list->fill].string = Util_Strdup(item);
      list->list[list->fill].next   = NULL;
      prev->next=&(list->list[list->fill]);
      list->fill++;
      retval = 1;
    }
  }

  return retval;
}


 /*@@
   @routine    Util_StringListNext
   @date       Mon May 21 17:06:10 2001
   @author     Tom Goodale
   @desc 
   Gets the next string from a stringlist.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     this
   @vdesc   The stringlist object
   @vtype   uStringList
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     flag
   @vdesc   Start from the beginning or not
   @vtype   int
   @vio     in
   @vcomment 
   1 - get first item and reset marker
   0 - get next item
   @endvar 

   @returntype const char *
   @returndesc
   The next string, or NULL if there are no more.
   @endreturndesc

@@*/
const char *Util_StringListNext(uStringList *this, int flag)
{
  const char *retval;

  if(flag)
  {
    this->current=this->head;
  }

  if(this->current)
  {
    retval = this->current->string;
    this->current=this->current->next;
  }
  else
  {
    retval = NULL;
  }

  return retval;
}

 /*@@
   @routine    Util_StringListDestroy
   @date       Mon May 21 17:08:31 2001
   @author     Tom Goodale
   @desc 
   Destroys a stringlist.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     this
   @vdesc   The stringlist object
   @vtype   uStringList
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
void Util_StringListDestroy(uStringList *this)
{
  int i;
  for(i=0; i < this->fill; i++)
  {
    free(this->list[i].string);
  }
  free(this->list);
  free(this);
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

#ifdef TEST_STRINGLIST

int main(int argc, const char *argv[])
{
  int i;
  const char *item;

  uStringList *list = Util_StringListCreate(argc);

  for(i=0; i < argc; i++)
  {
    printf("Adding %s\n", argv[i]);

    if(!Util_StringListAdd(list, argv[i]))
    {
      printf("It was a duplicate !\n");
    }
  }

  for(item=Util_StringListNext(list,1); item; item=Util_StringListNext(list,0))
  {
    printf("Item is %s\n", item);
  }

  Util_StringListDestroy(list);

  return 0;

}

#endif /* TEST_STRINGLIST */
