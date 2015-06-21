#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ActiveThorns.c
   @date      Sun Jul  4 16:15:36 1999
   @author    Tom Goodale
   @desc 
   Stuff to deal with activethorns.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/ActiveThorns.c,v 1.38 2001/12/04 21:57:22 tradke Exp $
 @@*/

#define DEBUG_ACTIVATE

#include "cctk_Config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SKBinTree.h"

#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_WarnLevel.h"

#include "util_String.h"
#include "util_StringList.h"

#include "cctk_ActiveThorns.h"
#include "cctki_ActiveThorns.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ActiveThorns.c,v 1.38 2001/12/04 21:57:22 tradke Exp $";

CCTK_FILEVERSION(main_ActiveThorns_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

struct THORN
{
  int active;
  char *implementation;
};

struct IMPLEMENTATION
{
  int active;
  t_sktree *thornlist;
  char *activating_thorn;
  
  int n_ancestors;
  char **ancestors;

  int n_friends;
  char **friends;
};

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static int RegisterImp(const char *name, 
                       const char *thorn,
                       const char **ancestors,
                       const char **friends);

static int ActivateThorn(const char *name);
static int ActivateImp(const char *implementation, const char *thorn);

static int CompareStrings(const void *string1, const void *string2);
static int JustPrintThornName(const char *key,void *input, void *dummy);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

int CCTK_FCALL cctk_isthornactive_
                          (ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_isthorncompiled_
     (int *retval, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_isimplementationcompiled_
                           (int *retval, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_isimplementationactive_
                           (int *retval, ONE_FORTSTRING_ARG);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static t_sktree *thornlist = NULL;
static t_sktree *implist   = NULL;

static int n_thorns = 0;
static int n_imps   = 0;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_RegisterThorn
   @date       Sun Jul  4 17:44:14 1999
   @author     Tom Goodale
   @desc 
   Registers a thorn with the flesh.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     attributes
   @vdesc   Thorn attributes
   @vtype   const struct iAttrributeList
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
    0 - success
   -1 - Duplicate thorn
   -2 - memory failure storing thorn
   -3 - memory failure storing implementation name
   -4 - failed to store thorn in tree
   @endreturndesc
@@*/
int CCTKi_RegisterThorn(const struct iAttributeList *attributes)
{
  int retval;
  int i;

  t_sktree *node;
  t_sktree *temp;

  struct THORN *thorn;

  const char *name;
  const char *imp;

  const char **ancestors;
  const char **friends;


  name = imp = NULL;
  ancestors = friends = NULL;

#if 0
  for(i=0; attributes[i].attribute; i++)
  {
    if(!strcmp(attributes[i].attribute, "functions"))
    {
      printf("Functions: \n");
      for(j=0; attributes[i].AttributeData.FuncList[j].keyword; j++)
      {
        printf("keyword   is %s\n", attributes[i].AttributeData.FuncList[j].keyword);
        printf("signature is %s\n", attributes[i].AttributeData.FuncList[j].signature);
        printf("pointer   is %p\n", attributes[i].AttributeData.FuncList[j].func);
      }
    }
    else
    {
      printf("attribute is %s\n", attributes[i].attribute);
      if(attributes[i].AttributeData.StringList)
      {
        for(j=0; attributes[i].AttributeData.StringList[j]; j++)
        {
          printf("   datapoint %s\n", attributes[i].AttributeData.StringList[j]);
        }
      }
    }
  }
#endif /* 0 */

  for(i=0; attributes[i].attribute; i++)
  {
    if(!strcmp(attributes[i].attribute, "name"))
    {
      if(attributes[i].AttributeData.StringList)
      {
        name = attributes[i].AttributeData.StringList[0];
      }
    }
    else if(!strcmp(attributes[i].attribute, "implementation"))
    {
      if(attributes[i].AttributeData.StringList)
      {
        imp = attributes[i].AttributeData.StringList[0];
      }
    }
    else if(!strcmp(attributes[i].attribute, "ancestors"))
    {
      ancestors = attributes[i].AttributeData.StringList;
    }
    else if(!strcmp(attributes[i].attribute, "friends"))
    {
      friends = attributes[i].AttributeData.StringList;
    }
    else
    {
      fprintf(stderr, "Unknown/unimplemented thorn attribute %s\n", attributes[i].attribute);
    }  
  }

  /*  printf("Registering thorn %s, which provides %s\n", name, imp);*/

  /* Does the thorn already exist ? */
  node = SKTreeFindNode(thornlist, name);

  if(!node)
  {
    n_thorns++;
    /* Create the structure to hold thorn info. */
    thorn = (struct THORN *)malloc(sizeof(struct THORN));

    if(thorn)
    {
      thorn->implementation = Util_Strdup(imp);

      if(thorn->implementation)
      {
        /* Fill out data for the thorn. */
        thorn->active = 0;

        /* Store the data in the tree */
        temp = SKTreeStoreData(thornlist, thornlist, name, thorn);

        if(!thornlist) 
        {
          thornlist = temp;
        }

        if(temp)
        {

          /* Register the implementation */
          RegisterImp(imp, name, ancestors, friends);

          retval = 0;
        }
        else
        {
          retval = -4;
        }
      }
      else
      {
        retval = -3;
      }
    }
    else
    {
      retval = -2;
    }
  }
  else
  {
    retval = -1;
  }

  return retval;
}

 /*@@
   @routine    CCTKi_ActivateThorn
   @date       Sun Jul  4 17:46:15 1999
   @author     Tom Goodale
   @desc 
   Activates a thorn and the associated implementation assuming
   the implementation isn't already active.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of thorn to activate
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
    0 - success
   -1 - non-existent thorn
   -2 - internal error
   -3 - thorn already active
   -4 - implementation already active
   @endreturndesc
@@*/
int CCTKi_ActivateThorn(const char *name)
{
  int retval;
  t_sktree *thornnode;
  t_sktree *impnode;
  
  struct THORN *thorn;
  struct IMPLEMENTATION *imp;

  printf("Activating thorn %s...", name);

  /* Find the thorn */
  thornnode = SKTreeFindNode(thornlist, name);

  if(thornnode)
  {
    thorn = (struct THORN *)(thornnode->data);

    /* Find the implementation */
    impnode = SKTreeFindNode(implist, thorn->implementation);

    if(impnode)
    {
      imp = (struct IMPLEMENTATION *)(impnode->data);
          
      if(!thorn->active)
      {
        if(!imp->active)
        {
          /* Activate the thorn. */
          printf("Success -> active implementation %s\n", thorn->implementation);
          thorn->active = 1;
          imp->active = 1;
          /* Remember which thorn activated this imp. */
          imp->activating_thorn = Util_Strdup(name);
          retval = 0;
        }
        else
        {
          printf("Failure -> Implementation %s already activated by %s\n", thorn->implementation, imp->activating_thorn);
          retval = -4;
        }
      }
      else
      {
        printf("Failure -> Thorn %s already active\n", name);
        retval = -3;
      }
    }
    else
    {
      printf("Internal error - can't find imp %s from thorn %s\n", thorn->implementation, name);
      retval = -2;
    }
  }
  else
  {
    printf("Failure -> non-existent thorn.\n");
    retval = -1;
  }

  return retval;

}


/*@@
   @routine    CCTK_IsThornActive
   @date       Sun Jul  4 17:46:56 1999
   @author     Tom Goodale
   @desc 
   Checks if a thorn is active.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of thorn
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - inactive
   1 - active
   @endreturndesc
@@*/
int CCTK_IsThornActive(const char *name)
{
  int retval;
  t_sktree *node;
  
  struct THORN *thorn;

  /* Find the thorn */
  node = SKTreeFindNode(thornlist, name);

  retval = 0;

  if(node)
  {
    thorn = (struct THORN *)(node->data);

    if(thorn->active)
    {
      retval = 1;
    }
  }

  return retval;
}

int CCTK_FCALL cctk_isthornactive_
                          (ONE_FORTSTRING_ARG)
{
  int retval;
  ONE_FORTSTRING_CREATE(name) 
  retval = CCTK_IsThornActive(name);
  free(name);
  return retval;
}

 /*@@
   @routine    CCTK_ThornImplementation
   @date       Sun Oct 17 21:10:19 1999
   @author     Tom Goodale
   @desc 
   Returns the implementation provided by the thorn.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of the thorn
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype const char *
   @returndesc 
   Name of the implementation or NULL
   @endreturndesc
@@*/
const char *CCTK_ThornImplementation(const char *name)
{
  const char *retval;
  t_sktree *node;
  
  struct THORN *thorn;

  /* Find the thorn */
  node = SKTreeFindNode(thornlist, name);

  retval = NULL;

  if(node)
  {
    thorn = (struct THORN *)(node->data);

    retval = thorn->implementation;
  }

  return retval;
}

 /*@@
   @routine    CCTK_ImplementationThorn
   @date       Sun Oct 17 22:04:13 1999
   @author     Tom Goodale
   @desc 
   Returns the name of one thorn providing an implementation.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of the implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype const char *
   @returndesc 
   Name of the thorn or NULL
   @endreturndesc
@@*/
const char *CCTK_ImplementationThorn(const char *name)
{
  const char *retval;

  t_sktree *node;
  
  struct IMPLEMENTATION *imp;

  /* Find the implementation */
  node = SKTreeFindNode(implist, name);

  retval = NULL;

  if(node)
  {
    imp = (struct IMPLEMENTATION *)(node->data);

    retval = imp->thornlist->key;
  }

  return retval;
}


/*@@
   @routine    CCTK_IsThornCompiled
   @date       Sun Jul  4 17:46:56 1999
   @author     Tom Goodale
   @desc 
   Checks if a thorn is compiled in.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of thorn
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - not compiled
   1 - compiled
   @endreturndesc
@@*/
int CCTK_IsThornCompiled(const char *name)
{
  int retval;
  t_sktree *node;
  
  /* Find the thorn */
  node = SKTreeFindNode(thornlist, name);

  retval = 0;

  if(node)
  {
    retval = 1;
  }

  return retval;
}

void CCTK_FCALL cctk_isthorncompiled_
     (int *retval, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(name) 
  *retval = CCTK_IsThornCompiled(name);
  free(name);
}


/*@@
   @routine    CCTK_IsImplementationCompiled
   @date       Sun June 3 2001
   @author     Gabrielle Allen
   @desc 
   Checks if a implementation is compiled in.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - not compiled
   1 - compiled
   @endreturndesc
@@*/
int CCTK_IsImplementationCompiled(const char *name)
{
  int retval;
  t_sktree *node;
  
  /* Find the thorn */
  node = SKTreeFindNode(implist, name);

  retval = 0;

  if(node)
  {
    retval = 1;
  }

  return retval;
}

void CCTK_FCALL cctk_isimplementationcompiled_
                           (int *retval, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(name) 
  *retval = CCTK_IsImplementationCompiled(name);
  free(name);
}


/*@@
   @routine    CCTK_IsImplementationActive
   @date       Sun Jul  4 17:46:56 1999
   @author     Tom Goodale
   @desc 
   Checks if an implementation is active.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - inactive
   1 - active
   @endreturndesc
@@*/
int CCTK_IsImplementationActive(const char *name)
{
  int retval;

  t_sktree *node;
  
  struct IMPLEMENTATION *imp;

  /* Find the implementation */
  node = SKTreeFindNode(implist, name);

  retval = 0;

  if(node)
  {
    imp = (struct IMPLEMENTATION *)(node->data);

    if(imp->active)
    {
      retval = 1;
    }
  }

  return retval;
}

void CCTK_FCALL cctk_isimplementationactive_
                           (int *retval, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(name) 
  *retval = CCTK_IsImplementationActive(name);
  free(name);
}

 /*@@
   @routine    CCTKi_PrintThorns
   @date       Mon Jul  5 10:02:15 1999
   @author     Tom Goodale
   @desc 
   Prints a list of thorns.
   Only lists active ones if the 'active' parameter is true.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     file
   @vdesc   File stream to print to
   @vtype   FILE *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     format
   @vdesc   format string for file
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     active
   @vdesc   Just print active thorns ?
   @vtype   int
   @vio     in
   @vcomment 
   Set to 0 to print all thorns. 
   @endvar 

   @returntype int
   @returndesc 
   0 - success
   @endreturndesc
@@*/
int CCTKi_PrintThorns(FILE *file, const char *format, int active)
{
  int retval;
  t_sktree *node;

  struct THORN *thorn;

  retval = 0;

  for(node= SKTreeFindFirst(thornlist);
      node; 
      node = node->next, retval++)
  {
    thorn = (struct THORN *)(node->data);

    if(thorn->active || !active)
    {
      fprintf(file, format, node->key);
    }
  }

  return retval;
}

 /*@@
   @routine    CCTKi_PrintImps
   @date       Mon Jul  5 10:08:19 1999
   @author     Tom Goodale
   @desc 
   Prints a list of implementations.
   Only lists active ones if the 'active' parameter is true.   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     file
   @vdesc   File stream to print to
   @vtype   FILE *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     format
   @vdesc   format string for file
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     active
   @vdesc   Just print active implementations ?
   @vtype   int
   @vio     in
   @vcomment 
   Set to 0 to print all implementations. 
   @endvar 

   @returntype int
   @returndesc 
   0 - success
   @endreturndesc
@@*/
int CCTKi_PrintImps(FILE *file, const char *format, int active)
{
  int retval;
  t_sktree *node;

  struct IMPLEMENTATION *imp;

  retval = 0;

  for(node= SKTreeFindFirst(implist);
      node; 
      node = node->next, retval++)
  {
    imp = (struct IMPLEMENTATION *)(node->data);

    if(imp->active || !active)
    {
      fprintf(file, format, node->key);
    }
  }

  return retval;
}


 /*@@
   @routine    CCTK_ActivatingThorn
   @date       Thu Oct 14 16:08:42 1999
   @author     Tom Goodale
   @desc 
   Finds the thorn which activated a particular implementation 
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   implementation name
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype const char *
   @returndesc 
   Name of activating thorn or NULL if inactive
   @endreturndesc
@@*/
const char *CCTK_ActivatingThorn(const char *name)
{
  const char *retval;

  t_sktree *node;
  
  struct IMPLEMENTATION *imp;

  /* Find the implementation */
  node = SKTreeFindNode(implist, name);

  retval = NULL;

  if(node)
  {
    imp = (struct IMPLEMENTATION *)(node->data);

    if(imp->active)
    {
      retval = imp->activating_thorn;
    }
  }

  return retval;
}



 /*@@
   @routine    CCTK_ImpThornList
   @date       Tue Jul 27 09:15:58 1999
   @author     Tom Goodale
   @desc 
   Return the thorns for an implementation.
   For now return an sktree - FIXME
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype t_sktree *
   @returndesc 
   Thornlist
   @endreturndesc
@@*/
t_sktree *CCTK_ImpThornList(const char *name)
{
  t_sktree *retval;

  t_sktree *node;
  
  struct IMPLEMENTATION *imp;
  

  /* Find the implementation */
  node = SKTreeFindNode(implist, name);

  if(node)
  {
    imp = (struct IMPLEMENTATION *)(node->data);

    retval = imp->thornlist;
  }
  else
  {
    retval = NULL;
  }

  return retval;
}


 /*@@
   @routine    CCTK_NumCompiledThorns
   @date       Tue Feb 02 2000
   @author     Thomas Radke
   @desc 
   Return the number of thorns compiled in.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

   @returntype int
   @returndesc 
   Number of thorns compiled in
   @endreturndesc
@@*/
int CCTK_NumCompiledThorns(void)
{
  return n_thorns;
}

  
 /*@@
   @routine    CCTK_CompiledThorn
   @date       Tue Feb 02 2000
   @author     Thomas Radke
   @desc 
   Return the name of the compiled thorn with given index.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     tindex
   @vdesc   thorn index
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype const char *
   @returndesc 
   Name of thorn
   @endreturndesc
@@*/
const char *CCTK_CompiledThorn(int tindex)
{
  int i;
  t_sktree *node;
  const char *ret_val;

  ret_val = NULL;

  for(node = SKTreeFindFirst(thornlist), i = 0;
      node;
      node = node->next, i++)
  {
    if (i == tindex)
    {
      ret_val = node->key;
      break;
    }
  }

  return ret_val;
}

  
 /*@@
   @routine    CCTK_NumCompiledImplementations
   @date       Tue Feb 02 2000
   @author     Thomas Radke
   @desc 
   Return the number of implementations compiled in.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

   @returntype int
   @returndesc 
   Number of implementations compiled in
   @endreturndesc
@@*/
int CCTK_NumCompiledImplementations(void)
{
  return n_imps;
}

  
 /*@@
   @routine    CCTK_CompiledImplementation
   @date       Tue Feb 02 2000
   @author     Thomas Radke
   @desc 
   Return the name of the compiled implementation with given index.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     tindex
   @vdesc   implementation index
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype const char *
   @returndesc 
   Name of implementation
   @endreturndesc
@@*/
const char *CCTK_CompiledImplementation(int tindex)
{
  int i;
  t_sktree *node;
  const char *ret_val;

  ret_val = NULL;

  for(node = SKTreeFindFirst(implist), i = 0;
      node;
      node = node->next, i++)
  {
    if (i == tindex)
    {
      ret_val = node->key;
      break;
    }
  }

  return ret_val;
}


 /*@@
   @routine    CCTK_ImplementationRequires
   @date       Sat Oct 20 2001
   @author     Gabrielle Allen
   @desc 
   Return the ancestors for an implementation
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

   @returntype int
   @returndesc 
   @endreturndesc
@@*/

uStringList *CCTK_ImplementationRequires(const char *imp)
{
  int i;
  t_sktree *impnode;
  struct IMPLEMENTATION *impdata;
  uStringList *ancestors;

  impnode = SKTreeFindNode(implist, imp);
  impdata = (struct IMPLEMENTATION *)(impnode->data);

  ancestors = Util_StringListCreate(n_thorns);

  /* Get ancestors */
  for(i=0; impdata->ancestors[i]; i++)
  {
    CCTK_ImplementationThorn(impdata->ancestors[i]);
    Util_StringListAdd(ancestors,impdata->ancestors[i]);
  }
      
  /* Get friends */
  for(i=0; impdata->friends[i]; i++)
  {
    CCTK_ImplementationThorn(impdata->friends[i]);
    Util_StringListAdd(ancestors,impdata->ancestors[i]);
  }

  return ancestors;
}

 /*@@
   @routine    CCTKi_ActivateThorns
   @date       Mon May 21 22:06:37 2001
   @author     Tom Goodale
   @desc 
   Activates a list of thorns if they are self consistent.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     activethornlist
   @vdesc   The list of thorns to activate.
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   -ve Number of errors encountered.
   @endreturndesc
@@*/
int CCTKi_ActivateThorns(const char *activethornlist)
{
  int retval;
  char *local_list;
  uStringList *required_thorns;
  uStringList *requested_imps;
  uStringList *required_imps;
  char *token;
  const char *this_imp;
  int n_warnings;
  int n_errors;
  t_sktree *impnode;
  t_sktree *impthornlist;

  struct IMPLEMENTATION *imp;
  int i;

  const char *imp1, *imp2;
  const char *thorn;

  local_list = Util_Strdup(activethornlist);

  required_thorns  = Util_StringListCreate(n_thorns);
  required_imps    = Util_StringListCreate(n_imps);
  requested_imps   = Util_StringListCreate(n_imps);

  printf("Activation requested for \n--->%s<---\n", activethornlist);

  n_errors = 0;
  n_warnings = 0;

  token = strtok(local_list, " \t\n");
  while(token)
  {
    if(CCTK_IsThornActive(token))
    {
      printf("Warning: thorn %s already active\n", token);
      n_warnings++;
    }
    else if(! (this_imp = CCTK_ThornImplementation(token)))
    {
      printf("Error: Thorn %s not found\n", token);
      n_errors++;
      /*  Give some more help */
      if (CCTK_IsImplementationCompiled(token))
      {
        impthornlist = CCTK_ImpThornList(token);

        printf("       However, implementation %s was found and is\n",token);
        printf("       provided by thorn(s):");
        SKTreeTraverseInorder(impthornlist, 
                              JustPrintThornName, NULL);
        printf("\n");
      }
    }
    else if(CCTK_IsImplementationActive(this_imp))
    {
      printf("Error: thorn %s provides implementation %s - already active\n", token, this_imp);
      n_errors++;
    }
    else if(! Util_StringListAdd(required_thorns,token))
    {
      printf("Warning: thorn %s already scheduled for activation\n", token);
      n_warnings++;
    }
    else if(! Util_StringListAdd(requested_imps,this_imp))
    {
      printf("Error: thorn %s provides implementation %s which is already scheduled for activation\n", token, this_imp);
      n_errors++;
    }
    else if((impnode = SKTreeFindNode(implist, this_imp)))
    {
      /* Ok, this thorn exists, and isn't active, a duplicate, or provide the same imp as another thorn 
       * which is active or has just been schedule for activation, so can get on with cataloging 
       * dependencies.
       */
      
      Util_StringListAdd(required_imps,this_imp);

      imp = (struct IMPLEMENTATION *)(impnode->data);

      /* Look at ancestors */
      for(i=0; imp->ancestors[i]; i++)
      {
        if(!CCTK_IsImplementationActive(imp->ancestors[i]))
        {
          /* We need this imp */
          Util_StringListAdd(required_imps, imp->ancestors[i]);
        }
      }
      
      /* Look at friends */
      for(i=0; imp->friends[i]; i++)
      {
        if(!CCTK_IsImplementationActive(imp->friends[i]))
        {
          /* We need this imp */
          Util_StringListAdd(required_imps, imp->friends[i]);
        }
      }
    }
    else
    {
      CCTK_Warn(0, __LINE__, __FILE__, "Cactus", 
                "Internal error :- please report this to cactusmaint@cactuscode.org");
    }
    token = strtok(NULL," \t\n");
  }

  /* No longer need the local list */
  free(local_list);

  if(! n_errors)
  {
    /* So, let's see if we are requesting all the imps we need */

    for(imp1=Util_StringListNext(requested_imps,1),
          imp2=Util_StringListNext(required_imps,1); 
        imp1&&imp2;
        imp1=Util_StringListNext(requested_imps,0),
          imp2=Util_StringListNext(required_imps,0))
    {
      do
      {
        if(Util_StrCmpi(imp1,imp2))
        {
          printf("Error: Required implementation %s not activated.\n", imp2);
          printf("       Add a thorn providing this implementation to ActiveThorns parameter.\n");
          n_errors++;
          /*  Give some more help */
          if (CCTK_IsImplementationCompiled(imp2))
          {
            impthornlist = CCTK_ImpThornList(imp2);

            printf("       This implementation is provided by compiled thorns:\n");
            printf("          ");
            SKTreeTraverseInorder(impthornlist, 
                              JustPrintThornName, NULL);
            printf("\n");
          }
          else
          {
            printf("       This implementation is not provided by any "
                   "compiled thorn\n");
          }
        }
        else
        {
          break;
        }
      } while((imp2=Util_StringListNext(required_imps,0)));
    }
    /* Since the requested imps is a subset of the required imps, 
     * we may still have some required imps to go through.
     */
    while((imp2))
    {
      printf("Error: required implementation %s not requested\n", imp2);
      printf("       Add a thorn providing this implementation to ActiveThorns parameter.\n");
      n_errors++;
      /*  Give some more help */
      if (CCTK_IsImplementationCompiled(imp2))
      {
        impthornlist = CCTK_ImpThornList(imp2);
        
        printf("       This implementation is provided by compiled thorns:\n");
        printf("          ");
        SKTreeTraverseInorder(impthornlist, 
                              JustPrintThornName, NULL);
        printf("\n");
      }
      else
      {
        printf("       This implementation is not provided by any "
               "compiled thorn\n");
      }
      imp2=Util_StringListNext(required_imps,0);
    }    
  }


  if(! n_errors)
  {
    /* Ok, so we have all required imps, so can activate the thorns, finally */

    for(thorn = Util_StringListNext(required_thorns, 1);
        thorn;
        thorn = Util_StringListNext(required_thorns, 0))
    {
      ActivateThorn(thorn);
    }

    retval = 0;
  }
  else
  {
    printf("Activation failed - %d errors in activation sequence\n", n_errors);
    retval = -n_errors;
  }

  Util_StringListDestroy(required_thorns);
  Util_StringListDestroy(required_imps);
  Util_StringListDestroy(requested_imps);

  return retval;
}
  
    

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    RegisterImp
   @date       Sun Jul  4 17:44:42 1999
   @author     Tom Goodale
   @desc 
   Registers an implementation.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   name of the implementation
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     thorn
   @vdesc   name of the thorn
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     ancestors
   @vdesc   ancestors of the implementation
   @vtype   const char **
   @vio     in
   @vcomment 
 
   @endvar 
   @var     friends
   @vdesc   friends of the implementation
   @vtype   const char **
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
    0 - success
   -1 - failed to store thorn in implementation
   -2 - memory failure creating implementation
   -3 - failed to store implementtion in tree
   @endreturndesc
@@*/
static int RegisterImp(const char *name, 
                       const char *thorn,
                       const char **ancestors,
                       const char **friends)
{
  int retval;
  int count;
  t_sktree *node;
  t_sktree *temp;

  struct IMPLEMENTATION *imp;

  /* Does the implementation already exist ? */
  node = SKTreeFindNode(implist, name);

  if(!node)
  {
    n_imps++;

    /* Create the structure to hold info about it. */
    imp = (struct IMPLEMENTATION *)malloc(sizeof(struct IMPLEMENTATION));

    if(imp)
    {
      imp->active = 0;

      /* Store the name of this thorn in a tree */      
      imp->thornlist = SKTreeStoreData(NULL,NULL, thorn, NULL);

      /* Store the info in the tree. */
      temp = SKTreeStoreData(implist, implist, name, imp);

      if(!implist) implist = temp;

      if(temp)
      {
        retval = 0;
      }
      else
      {
        retval = -3;
      }

      if(!retval)
      {
        /* Count the ancestors */
        for(count=0; ancestors[count];count++);

        imp->n_ancestors = count;
        imp->ancestors = (char **)malloc((count+1)*sizeof(char *));
        
        if(imp->ancestors)
        {
          for(count=0; ancestors[count];count++)
          {
            imp->ancestors[count] = Util_Strdup(ancestors[count]);
          }
          imp->ancestors[count] = NULL;

          qsort(imp->ancestors, count, sizeof(char *), CompareStrings);
 
        }

        /* Count the friends */
        for(count=0; friends[count];count++);

        imp->n_friends = count;
        imp->friends = (char **)malloc((count+1)*sizeof(char *));
        
        if(imp->friends)
        {
          for(count=0; friends[count];count++)
          {
            imp->friends[count] = Util_Strdup(friends[count]);
          }
          imp->friends[count] = NULL;

          qsort(imp->friends, count, sizeof(char *), CompareStrings);
        }
      }            
    }
    else
    {
      retval = -2;
    }
  }
  else
  {
    imp = (struct IMPLEMENTATION *)(node->data);
    SKTreeStoreData(imp->thornlist,imp->thornlist, thorn, NULL);

    retval = -1;
  }

  return retval;
}

 /*@@
   @routine    ActivateThorn
   @date       Mon May 21 22:09:47 2001
   @author     Tom Goodale
   @desc 
   Activate one thorn - assumes all error checking done by calling routine.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of thorn to activate
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0  - success
   -1 - can't find thorn
   @endreturndesc
@@*/
static int ActivateThorn(const char *name)
{
  int retval;
  t_sktree *thornnode;
  
  struct THORN *thorn;

  printf("Activating thorn %s...", name);

  /* Find the thorn */
  thornnode = SKTreeFindNode(thornlist, name);

  if(thornnode)
  {
    thorn = (struct THORN *)(thornnode->data);

    thorn->active = 1;

    printf("Success -> active implementation %s\n", thorn->implementation);

    retval = ActivateImp(thorn->implementation, name);
  }
  else
  {
    retval = -1;
  }

  return retval;
}

 /*@@
   @routine    ActivateImp
   @date       Mon May 21 22:09:47 2001
   @author     Tom Goodale
   @desc 
   Activate one implementation - assumes all error checking done by calling routine.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     implementation
   @vdesc   Name of implementation to activate
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     thorn
   @vdesc   Name of thorn activating this imp
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0  - success
   -1 - can't find implementation
   @endreturndesc
@@*/
static int ActivateImp(const char *implementation, const char *thorn)
{
  int retval;
  t_sktree *impnode;
  
  struct IMPLEMENTATION *imp;

  /* Find the implementation */
  impnode = SKTreeFindNode(implist, implementation);

  if(impnode)
  {
    imp = (struct IMPLEMENTATION *)(impnode->data);
          
    imp->active = 1;
    /* Remember which thorn activated this imp. */
    imp->activating_thorn = Util_Strdup(thorn);
    retval = 0;
  }
  else
  {
    retval = -1;
  }

  return retval;
}

/*@@
   @routine    CompareStrings
   @date       Thu Sep 14 18:57:52 2000
   @author     Tom Goodale
   @desc
   Case independent string comparison to pass to qsort.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory

   @@*/
static int CompareStrings(const void *string1, const void *string2)
{
  return Util_StrCmpi(*(const char **)string1, *(const char **)string2);
}

 /*@@
   @routine    JustPrintThornName
   @date       Mon Jun  4 19:05:45 2001
   @author     Tom Goodale
   @desc 
   Print the name of a thorn if it is passed from an sktree.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
static int JustPrintThornName(const char *key, void *input, void *dummy)
{
  input = input;
  dummy = dummy;

  printf(" %s", key);

  return 0;
}
