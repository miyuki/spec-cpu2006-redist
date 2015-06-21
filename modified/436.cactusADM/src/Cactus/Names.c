#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Names.c
   @date      Thu Mar  9 10:38:29 2000
   @author    Tom Goodale
   @desc 
   Routines to store and return data associated with various Cactus names.
   @enddesc
   @version $Header: /cactus/Cactus/src/main/Names.c,v 1.4 2001/05/10 12:35:13 goodale Exp $
 @@*/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cctki_Names.h"
#include "cctk_Flesh.h"
#include "util_Hash.h"

static const char *rcsid="$Header: /cactus/Cactus/src/main/Names.c,v 1.4 2001/05/10 12:35:13 goodale Exp $";

CCTK_FILEVERSION(main_Names_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

typedef struct IGroupData
{
  int gnum;
} iGroupData;

typedef struct IVariableData
{
  int vnum;
  int gnum;
} iVariableData;


/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static unsigned int CaseIndependentHash(unsigned int klen, 
                                        const char *key);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

#define INITIAL_GROUP_HASH_SIZE    64
#define INITIAL_VARIABLE_HASH_SIZE 64

static uHash *GroupData    = NULL;
static uHash *VariableData = NULL;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_NamesStoreGroup
   @date       Thu Mar  9 11:01:05 2000
   @author     Tom Goodale
   @desc 
   Stores the number of a group indexed by its name.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     gname
   @vdesc   Name of the group
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     gnum
   @vdesc   Group number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0  - success
   -1 - out of memory
   @endreturndesc
@@*/
int CCTKi_NamesStoreGroup(const char *gname, int gnum)
{
  int retval;

  unsigned int hash;

  iGroupData *data;

  /* Create the hash table if it's not already been done. */
  if(!GroupData)
  {
    GroupData = Util_HashCreate(INITIAL_GROUP_HASH_SIZE);
  }

  /* Allocate the memory */
  data = (iGroupData *)malloc(sizeof(iGroupData));

  /* Store the data */
  if(data && GroupData)
  {
    data->gnum = gnum;

    hash = CaseIndependentHash(strlen(gname), gname);

    retval = Util_HashStore(GroupData, strlen(gname), gname, hash, data);
  }
  else
  {
    retval = -1;
  }
    
  return retval;
}


 /*@@
   @routine    CCTKi_NamesStoreVariable
   @date       Thu Mar  9 11:58:53 2000
   @author     Tom Goodale
   @desc 
   Stores the variable number and group number associated with a variable
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of the variable
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     vnum
   @vdesc   Variable number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     gnum
   @vdesc   Number of group containing variable.
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0  - success
   -1 - out of memory
   @endreturndesc
@@*/
int CCTKi_NamesStoreVariable(const char *name, int vnum, int gnum)
{
  int retval;

  unsigned int hash;

  iVariableData *data;

  /* Create the hash table if it's not already been done. */
  if(!VariableData)
  {
    VariableData = Util_HashCreate(INITIAL_VARIABLE_HASH_SIZE);
  }

  /* Allocate the memory */
  data = (iVariableData *)malloc(sizeof(iVariableData));

  /* Store the data */
  if(data && VariableData)
  {
    data->vnum = vnum;
    data->gnum = gnum;

    hash = CaseIndependentHash(strlen(name), name);

    retval = Util_HashStore(VariableData, strlen(name), name, hash, data);
  }
  else
  {
    retval = -1;
  }
    
  return retval;
}

 /*@@
   @routine    CCTKi_NamesRetrieveGroupNum
   @date       Thu Mar  9 11:26:07 2000
   @author     Tom Goodale
   @desc 
   Gets the number associated with a group.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     gname
   @vdesc   Name of the group
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     gnum
   @vdesc   Group number
   @vtype   int *
   @vio     out
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0  - success
   -1 - group doesn't exist
   -2 - no groups exist
   @endreturndesc

@@*/
int CCTKi_NamesRetrieveGroupNum(const char *gname, int *gnum)
{
  int retval;
  unsigned int hash;

  iGroupData *data;

  if(GroupData)
  {
    hash = CaseIndependentHash(strlen(gname), gname);

    data = (iGroupData *)Util_HashData(GroupData, strlen(gname), gname, hash);

    if(data)
    {
      *gnum = data->gnum;
      retval = 0;
    }
    else
    {
      retval = -1;
    }
  }
  else
  {
    retval = -2;
  }

  return retval;
}

 /*@@
   @routine    CCTKi_NamesRetrieveVariableNum
   @date       Thu Mar  9 12:00:38 2000
   @author     Tom Goodale
   @desc 
   Gets the numbers associated with a variable.   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of the variable
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     vnum
   @vdesc   Variable number
   @vtype   int *
   @vio     out
   @vcomment 
 
   @endvar 
   @var     gnum
   @vdesc   Number of group containing variable.
   @vtype   int *
   @vio     out
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0  - success
   -1 - variable doesn't exist
   -2 - no variable exists
   @endreturndesc
@@*/
int CCTKi_NamesRetrieveVariableNum(const char *name, int *vnum, int *gnum)
{
  int retval;
  unsigned int hash;

  iVariableData *data;

  if(VariableData)
  {
    hash = CaseIndependentHash(strlen(name), name);

    data = (iVariableData *)Util_HashData(VariableData, strlen(name), name, hash);

    if(data)
    {
      *vnum = data->vnum;
      *gnum = data->gnum;
      retval = 0;
    }
    else
    {
      retval = -1;
    }
  }
  else
  {
    retval = -2;
  }

  return retval;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    CaseIndependenthHash
   @date       Wed Oct 27 22:15:17 1999
   @author     Tom Goodale
   @desc 
   Util_Hashing function.  I took this from the book 
      'Advanced Perl Programming' 
   published by O'Reilly, and it is apparently the
   algorithm used in Perl, and that is GPL.
   @enddesc 
   @calls     
   @calledby   
   @history 
   @hdate Thu Mar  9 11:04:48 2000 @hauthor Tom Goodale
   @hdesc Copied from Util_HashHash and modified to be case independent. 
   @endhistory 
   @var     klen
   @vdesc   Key length
   @vtype   unsigned int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     key
   @vdesc   The key
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype unsigned int
   @returndesc
   The hash value
   @endreturndesc
@@*/
static unsigned int CaseIndependentHash(unsigned int klen, 
                                        const char *key)
{
  unsigned int hash;
  int i;
  const char *pos;

  i   = klen;  
  pos = key;

  hash = 0;

  while(i--)
  {
    hash = hash*33 + toupper(*pos++);
  }

#ifdef DEBUG_HASH
  printf("key '%s', length %d, has hash %u\n", key, klen, hash);
#endif

  return hash;
}

/********************************************************************
 *********************     Test Routines    *************************
 ********************************************************************/

#ifdef TEST_NAMES

#include <stdio.h>

int main(int argc, const char *argv[])
{
  int n_strings;
  int i;
  char name[200];
  int number;
  int gnum;

  if(argc > 1)
  {
    n_strings = atoi(argv[1]);
  }
  else
  {
    n_strings = 5;
  }

  for(i = 0; i < n_strings; i++)
  {
    sprintf(name, "gname %d", i);

    CCTKi_NamesStoreGroup(name, i);

    sprintf(name, "vname %d", i);

    CCTKi_NamesStoreVariable(name, i, 2*i);
  }

  for(i = 0; i < n_strings; i++)
  {
    sprintf(name, "gname %d", i);

    CCTKi_NamesRetrieveGroupNum(name, &number);

    printf("Group %d has number %d\n", i, number);

    sprintf(name, "vname %d", i);

    CCTKi_NamesRetrieveVariableNum(name, &number, &gnum);

    printf("Variable %d has numbers %d %d\n", i, number, gnum);

  }


  return 0;
}

#endif /* TEST_NAMES */
