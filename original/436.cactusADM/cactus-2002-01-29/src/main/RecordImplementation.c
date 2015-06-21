 /*@@
   @file      RecordImplementation.c
   @date      Wed Jan 13 21:03:55 1999
   @author    Tom Goodale
   @desc 
   Keeps a register of implementations and thorns associated 
   with implementations.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/RecordImplementation.c,v 1.8 2001/05/10 12:35:15 goodale Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "StoreNamedData.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/RecordImplementation.c,v 1.8 2001/05/10 12:35:15 goodale Exp $";

CCTK_FILEVERSION(main_RecordImplementation_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

typedef struct 
{
  int n_thorns;
  char **thornlist;
} t_ImplementationData;


/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static pNamedData *implementation_data = NULL;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_RegisterImplementation
   @date       Wed Jan 13 23:23:00 1999
   @author     Tom Goodale
   @desc 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     implementation
   @vdesc   The implementation to register
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     thorn
   @vdesc   A thorn providing this implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - success
   1 - memory failure realloc of thorn list
   2 - memory failure creating implementation
   3 - memory failure creating thornlist
   4 - memory failure malloc of name of thorn
   @endreturndesc
@@*/
int CCTKi_RegisterImplementation(const char *implementation,
                                 const char *thorn)
{
  int retval;
  char **temp;

  t_ImplementationData *data;

  if((data = (t_ImplementationData *)GetNamedData(implementation_data, implementation)))
  {
    data->n_thorns++;
    temp = (char **)realloc(data->thornlist,data->n_thorns*sizeof(char *));
    if(temp)
    {
      data->thornlist = temp;
      data->thornlist[data->n_thorns-1] = (char *)malloc((strlen(thorn)+1)*sizeof(char));
      if(data->thornlist[data->n_thorns-1])
      {
        strcpy(data->thornlist[data->n_thorns-1], thorn);
        retval = 0;
      }
      else
      {
        retval = 4;
      }
    }
    else
    {
      fprintf(stderr, "Unable to allocate memory for new thorn %s\n", thorn);
      retval = 1;
    }
  }
  else
  {
    data = (t_ImplementationData *)malloc(sizeof(t_ImplementationData));

    if(data)
    {
      data->thornlist = (char **)malloc(sizeof(char *));
      if(data->thornlist)
      {
        data->thornlist[0] = (char *)malloc((strlen(thorn)+1)*sizeof(char));
        if(data->thornlist[0])
        {
          strcpy(data->thornlist[0], thorn);
          data->n_thorns = 1;
          StoreNamedData(&implementation_data,implementation, data);
          retval = 0;
        }
        else
        {
          retval = 4;
        }
      }
      else
      {
        fprintf(stderr, "Unable to allocate memory for new thorn %s\n", thorn);
        retval = 3;
      }
    }
    else
    {
      fprintf(stderr, "Unable to allocate memory for new implementation %s\n", implementation);
      retval = 2;
    }

  }

  return retval;
}
  
 /*@@
   @routine    GetImplementationThorns
   @date       Wed Jan 13 23:22:43 1999
   @author     Tom Goodale
   @desc 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     implementation
   @vdesc   name of the implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     thornlist
   @vdesc   Pointer to array holding list of thorns
   @vtype   char ***
   @vio     out
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   Number of thorns providing implementation
   @endreturndesc
@@*/
int GetImplementationThorns(const char *implementation, char ***thornlist)
{
  int retval;
  t_ImplementationData *data;

  if((data = (t_ImplementationData *)GetNamedData(implementation_data, implementation)))
  {
    *thornlist = data->thornlist;
    retval = data->n_thorns;
  }
  else
  {
    *thornlist = NULL;
    retval = 0;
  }

  return retval;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/
