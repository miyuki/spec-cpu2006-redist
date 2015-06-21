#ifdef SPEC_CPU
# define THORN_IS_PUGH
#endif /* SPEC_CPU */
 /*@@
   @file    Storage.c
   @date    Tue Jun 13 2000
   @author  Thomas Radke
   @desc
            Pugh storage functions
   @enddesc
   @version $Id: Storage.c,v 1.23 2001/11/06 11:19:35 tradke Exp $
 @@*/

#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"

#include "pugh.h"
#include "pughi.h"
#include "pugh_Comm.h"

static const char *rcsid="$Header: /cactus/CactusPUGH/PUGH/src/Storage.c,v 1.23 2001/11/06 11:19:35 tradke Exp $";
CCTK_FILEVERSION(CactusPUGH_PUGH_Storage_c)

/* #define DEBUG_PUGH 1 */


/********************************************************************
 ********************    Static Variables   *************************
 ********************************************************************/
static float totalstorage = 0; /* Storage for GAs in Bytes */
static float maxstorage = 0;   /* Maximum storage for GAs in MBytes */
static int totalnumberGA = 0;  /* Number of stored GAs */
static int totalnumberGF = 0;  /* Number of stored GFs */
static int numberGA = 0;       /* Number of GAs at max */
static int numberGF = 0;       /* Number of GFs at max */

/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int PUGH_EnableScalarGroupStorage (pGH *pughGH,
                                          int first_var,
                                          int n_variables,
                                          int n_timelevels);
static int PUGH_EnableGArrayGroupStorage (pGH *pughGH,
                                          int first_var,
                                          int n_variables,
                                          int n_timelevels);
static void PUGH_InitializeMemory (const char *initialize_memory,
                                   int vtype,
                                   int bytes,
                                   void *data);

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
void PUGHi_PrintStorageReport (void);


 /*@@
   @routine    PUGH_ArrayGroupSize
   @author     Gabrielle Allen
   @date       11 Apr 1999
   @desc
               Returns size of arrays in a group, in a given direction.
               Either group index or its name must be given,
               the name takes precedence.
   @enddesc
   @calls      CCTK_GroupIndex
               CCTK_FirstVarIndexI


   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        dir
   @vdesc      direction to return size of
   @vtype      int
   @vio        in
   @endvar
   @var        group
   @vdesc      index of group
   @vtype      int
   @vio        in
   @endvar
   @var        groupname
   @vdesc      name of group
   @vtype      const char *
   @vio        in
   @endvar

   @returntype const int *
   @returndesc
               pointer to the size variable for the given direction, or
               NULL if given direction or group index/name are invalid
   @endreturndesc
@@*/
const int *PUGH_ArrayGroupSize (const cGH *GH,
                                int dir,
                                int group,
                                const char *groupname)
{
  int first_var;
  pGA *GA;
  const int *retval;


  if (groupname)
  {
    group = CCTK_GroupIndex (groupname);
  }

  first_var = CCTK_FirstVarIndexI (group);
  if (first_var >= 0)
  {
    /* get pointer to pGA for a timelevel of first variable in this group */
    GA = (pGA *) PUGH_pGH (GH)->variables[first_var][0];

    if (GA->storage == PUGH_STORAGE)
    {
      if (dir >= 0 && dir < GA->extras->dim)
      {
        retval = &GA->extras->lnsize[dir];
      }
      else
      {
        CCTK_WARN (1, "Wrong value for dir");
        retval = NULL;
      }
    }
    else
    {
      retval = &_cctk_one;
    }
  }
  else
  {
    if (groupname)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_ArrayGroupSize: Invalid group name '%s'",
                  groupname);
    }
    else
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_ArrayGroupSize: Invalid group ID %d",
                  group);
    }

    retval = NULL;
  }

  return (retval);
}


 /*@@
   @routine    PUGH_QueryGroupStorage
   @author     Gabrielle Allen
   @date       13 Apr 1999
   @desc
               Checks if group has storage assigned.
               Either group index or its name must be given,
               the name takes precedence.
   @enddesc
   @calls      CCTK_GroupIndex
               CCTK_FirstVarIndexI
               CCTK_GroupTypeI

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        group
   @vdesc      index of group
   @vtype      int
   @vio        in
   @endvar
   @var        groupname
   @vdesc      name of the group to be queried
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 if storage for this group is assigned
                0 if storage for this group is not assigned
               -1 if given group index/name is invalid
   @endreturndesc
@@*/
int PUGH_QueryGroupStorage (const cGH *GH, int group, const char *groupname)
{
  int first_var;
  int storage;
  int grouptype;
  int vtypesize;
  int retval;
  pGH *pughGH;


  retval = -1;

  if (groupname)
  {
    group = CCTK_GroupIndex (groupname);
  }

  /* get first variable in group */
  first_var = CCTK_FirstVarIndexI (group);
  if (first_var >= 0)
  {
    pughGH = PUGH_pGH (GH);
    grouptype = CCTK_GroupTypeI (group);
    if (grouptype == CCTK_SCALAR)
    {
      vtypesize = CCTK_VarTypeSize (CCTK_VarTypeI (first_var));
      storage = ((char *) pughGH->variables[first_var][0])[vtypesize];
    }
    else if (grouptype == CCTK_GF || grouptype == CCTK_ARRAY)
    {
      storage = ((pGA *) pughGH->variables[first_var][0])->storage;
    }
    else
    {
      storage = -1;
      CCTK_WARN (1, "Unknown group type in PUGH_QueryGroupStorage");
    }

    if (storage == PUGH_STORAGE)
    {
      retval = 1;
    }
    else if (storage == PUGH_NOSTORAGE)
    {
      retval = 0;
    }
    else
    {
      CCTK_WARN (1, "Inconsistency in PUGH_QueryGroupStorage");
    }
  }
  else
  {
    if (groupname)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_ArrayGroupSize: Invalid group name '%s'",
                  groupname);
    }
    else
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_ArrayGroupSize: Invalid group ID %d",
                  group);
    }
  }

  return (retval);
}


 /*@@
   @routine    PUGH_EnableGroupStorage
   @author     Tom Goodale
   @date       30 Mar 1999
   @desc
               Enables storage for all variables in the group
               indicated by groupname.
   @enddesc
   @calls      CCTK_GroupIndex
               CCTK_GroupData
               PUGH_EnableScalarGroupStorage
               PUGH_EnableGArrayGroupStorage

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        groupname
   @vdesc      name of the group to enable storage for
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                return code of @seeroutine PUGH_EnableScalarGroupStorage or
                @seeroutine PUGH_EnableGArrayGroupStorage: <BR>
                1 if storage was already enabled, or <BR>
                0 if storage was successfully enabled <BR>
               -1 if group type is not one of the above <BR>
               -2 if an invalid GH pointer was given <BR>
               -3 if invalid groupname was given
   @endreturndesc
@@*/
int PUGH_EnableGroupStorage (cGH *GH, const char *groupname)
{
  DECLARE_CCTK_PARAMETERS
  int group;               /* group index */
  int first_var;           /* first variable's index */
  cGroup pgroup;           /* group information */
  int retval;
  pGA *GA;
  pGH *pughGH;


#ifdef DEBUG_PUGH
  printf (" PUGH_EnableGroupStorage: request for group '%s'\n", groupname);
  fflush (stdout);
#endif

  pughGH = PUGH_pGH (GH);
  group = CCTK_GroupIndex (groupname);

  if (pughGH && group >= 0)
  {
    first_var = CCTK_FirstVarIndexI (group);

    /* get the group info from its index */
    CCTK_GroupData (group, &pgroup);

    if (pgroup.grouptype == CCTK_SCALAR)
    {
      retval = PUGH_EnableScalarGroupStorage (pughGH,
                                              first_var,
                                              pgroup.numvars,
                                              pgroup.numtimelevels);
    }
    else if (pgroup.grouptype == CCTK_GF || pgroup.grouptype == CCTK_ARRAY)
    {
      retval = PUGH_EnableGArrayGroupStorage (pughGH,
                                              first_var,
                                              pgroup.numvars,
                                              pgroup.numtimelevels);
      if (!CCTK_Equals(storage_verbose,"no") && retval == 0)
      {
        /* get GA pointer of first var in group */
        GA = (pGA *) pughGH->variables[first_var][0];
        if (pgroup.grouptype == CCTK_GF)
        {
          totalnumberGF  += pgroup.numvars * pgroup.numtimelevels;
        }
        else
        {
          totalnumberGA  += pgroup.numvars * pgroup.numtimelevels;
        }
        totalstorage += (GA->extras->npoints * GA->varsize *
                         pgroup.numtimelevels * pgroup.numvars) /
                        (float) (1024*1024);
        if (totalstorage > maxstorage)
        {
          numberGF = totalnumberGF;
          numberGA = totalnumberGA;
          maxstorage = totalstorage;
        }

        /* Report on memory usage */
        if (CCTK_Equals(storage_verbose,"yes"))
        {
          CCTK_VInfo (CCTK_THORNSTRING, "Switched memory on for group '%s'"
                      "  [GFs: %d GAs: %d Total Size: %6.2fMB]",
                      groupname, totalnumberGF, totalnumberGA, totalstorage);
        }
      }

    }
    else
    {
      CCTK_WARN (1, "PUGH_EnableGroupStorage: Unknown group type");
      retval = -1;
    }

  }
  else
  {
    if (! pughGH)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_EnableGroupStorage: Error with cctkGH pointer "
                  "for group %s", groupname);
      retval = -2;
    }
    else
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_EnableGroupStorage: Invalid group %s", groupname);
      retval = -3;
    }
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    PUGH_DisableGroupStorage
   @author     Tom Goodale
   @date       30 Mar 1999
   @desc
               Disables storage for all variables in the group
               indicated by groupname.
   @enddesc
   @calls      CCTK_GroupIndex
               CCTK_GroupData
               CCTK_FirstVarIndexI
               PUGH_DisableGArrayGroupStorage

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        groupname
   @vdesc      name of the group to enable storage for
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 if storage for given group was disabled
               -1 if group type is invalid
   @endreturndesc
@@*/
int PUGH_DisableGroupStorage (cGH *GH, const char *groupname)
 {
  DECLARE_CCTK_PARAMETERS
  int group;               /* group index */
  cGroup pgroup;           /* group information */
  int vtypesize, retval;
  pGA ***variables;
  int first_var, var, level;
  int unchanged;           /* count how many aren't toggled */
  char *temp;


#ifdef DEBUG_PUGH
  printf (" PUGH_DisableGroupStorage: request for group '%s'\n", groupname);
  fflush (stdout);
#endif

  group = CCTK_GroupIndex (groupname);
  CCTK_GroupData (group, &pgroup);

  /* get global index of first variable in group */
  first_var = CCTK_FirstVarIndexI (group);

  variables = (pGA ***) PUGH_pGH (GH)->variables;

  /* get the group info from its index */
  unchanged = 0;

  retval = 1;
  if (pgroup.grouptype == CCTK_GF || pgroup.grouptype == CCTK_ARRAY)
  {
    for (var = first_var; var < first_var+pgroup.numvars; var++)
    {
      for (level = 0; level < pgroup.numtimelevels; level++)
      {
        unchanged += PUGH_DisableGArrayDataStorage (variables[var][level]);
      }
    }
  }
  else if (pgroup.grouptype == CCTK_SCALAR)
  {
    vtypesize = CCTK_VarTypeSize (pgroup.vartype);
    for (var = first_var; var < first_var+pgroup.numvars; var++)
    {
      for (level = 0; level < pgroup.numtimelevels; level++)
      {
        temp = (char *) variables[var][level];
        if (temp[vtypesize] == PUGH_STORAGE)
        {
          temp[vtypesize] = PUGH_NOSTORAGE;
        }
        else
        {
          unchanged++;
        }
      }
    }
  }
  else
  {
    CCTK_WARN (1, "Unknown group type in PUGH_DisableGroupStorage");
    retval = -1;
  }

  /* Report on memory usage */
  if (!CCTK_Equals(storage_verbose,"no") && retval >= 0)
  {
    if (unchanged == 0)
    {

      /* Memory toggled */
      if (pgroup.grouptype == CCTK_GF)
      {
        totalnumberGF  -= pgroup.numvars;
      }
      else if (pgroup.grouptype == CCTK_ARRAY)
      {
        totalnumberGA  -= pgroup.numvars;
      }
      totalstorage -= (variables[first_var][0]->extras->npoints *
                       variables[first_var][0]->varsize *
                       pgroup.numtimelevels * pgroup.numvars) /
                      (float) (1024 * 1024);
      if (CCTK_Equals(storage_verbose,"yes"))
      {
        CCTK_VInfo (CCTK_THORNSTRING, "Switched memory off for group '%s'"
                    "  [GFs: %d GAs: %d Total Size: %6.2fMB]",
                    groupname, totalnumberGF, totalnumberGA, totalstorage);
      }
    }
    else if (unchanged == pgroup.numvars)
    {
      /* Memory already off */
      if (CCTK_Equals(storage_verbose,"yes"))
      {
        CCTK_VInfo (CCTK_THORNSTRING, "Memory already off for group '%s'", groupname);
      }
    }
    else
    {
      CCTK_WARN (1, "PUGH_DisableGroupStorage: Inconsistency in group memory assignment");
    }
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/

 /*@@
   @routine    PUGH_EnableScalarGroupStorage
   @author     Thomas Radke
   @date       Thu 30 Aug 2001
   @desc
               Enables storage for a group of CCTK_SCALAR variables
               For efficiency reasons, PUGH allocates storage for scalars
               only once when the group is created.
               The current state of storage allocation (which is toggled by
               Enable/DisableGroupStorage) is stored in a byte-sized flag
               immediately after the scalar data.
   @enddesc

   @var        pughGH
   @vdesc      Pointer to PUGH GH extensions
   @vtype      pGH *
   @vio        in
   @endvar
   @var        first_var
   @vdesc      index of the first variable to enable storage for
   @vtype      int
   @vio        in
   @endvar
   @var        n_variables
   @vdesc      total number of variables to enable storage for
   @vtype      int
   @vio        in
   @endvar
   @var        n_timelevels
   @vdesc      total number of timelevels to enable storage for
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 if storage was already enabled before
                0 if storage was successfully enabled
   @endreturndesc
@@*/
static int PUGH_EnableScalarGroupStorage (pGH *pughGH,
                                          int first_var,
                                          int n_variables,
                                          int n_timelevels)
{
  DECLARE_CCTK_PARAMETERS
  int vtype, vtypesize, variable, level, retval;
  void *temp;


  vtype = CCTK_VarTypeI (first_var);
  vtypesize = CCTK_VarTypeSize (vtype);
  temp = pughGH->variables[first_var][0];
  retval = ((char *) temp)[vtypesize] == PUGH_STORAGE;

  /* don't assign storage if was already switched on */
  if (! retval)
  {
    for (variable = 0; variable < n_variables; variable++)
    {
      for (level = 0; level < n_timelevels; level++)
      {
        temp = pughGH->variables[variable+first_var][level];

        /* raise the query_storage flag */
        ((char *) temp)[vtypesize] = PUGH_STORAGE;

        /* initialize memory if desired */
        if (! CCTK_Equals (initialize_memory, "none"))
        {
          PUGH_InitializeMemory (initialize_memory, vtype, vtypesize, temp);
        }
      }
    }
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    PUGH_EnableGArrayGroupStorage
   @author     Tom Goodale
   @date       30 Mar 1999
   @desc
               Enables storage for a set of variables
   @enddesc
   @calls      PUGH_EnableGArrayDataStorage

   @var        pughGH
   @vdesc      Pointer to PUGH GH extensions
   @vtype      pGH *
   @vio        in
   @endvar
   @var        first_var
   @vdesc      index of the first variable to enable storage for
   @vtype      int
   @vio        in
   @endvar
   @var        n_variables
   @vdesc      total number of variables to enable storage for
   @vtype      int
   @vio        in
   @endvar
   @var        n_timelevels
   @vdesc      total number of timelevels to enable storage for
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 if storage was already enabled
                0 if storage was enabled (for at least one variable)
               -1 if there was an inconsistency in storage allocation
   @endreturndesc
@@*/
static int PUGH_EnableGArrayGroupStorage (pGH *pughGH,
                                          int first_var,
                                          int n_variables,
                                          int n_timelevels)
{
  DECLARE_CCTK_PARAMETERS
  int nstorage;   /* Number of Arrays for which storage was set */
  int nnostorage; /* Number of Arrays for which no storage was set */
  int retval;
  int var;
  pGA *GA;
  int level;


  nstorage = nnostorage = 0;

  for (var = first_var; var < first_var + n_variables; var++)
  {
    for (level = 0; level < n_timelevels; level++)
    {
      GA = (pGA *) pughGH->variables[var][level];

      if (! GA->storage)
      {
#ifdef DEBUG_PUGH
        printf (" PUGH_EnableGArrayGroupStorage: request for var '%s' "
                "timelevel %d\n", GA->name, level);
        fflush (stdout);
#endif
        PUGH_EnableGArrayDataStorage (GA,
                                      pughGH->myproc,
                                      initialize_memory,
                                      padding_active,
                                      padding_cacheline_bits,
                                      padding_size,
                                      padding_address_spacing);

        ((cGH *) pughGH->callerid)->data[var][level] = GA->data;

        nnostorage++;
      }
      else
      {
        ((cGH *) pughGH->callerid)->data[var][level] = GA->data;
        nstorage++;
      }
    }
  }

  if (nstorage > 0 && nnostorage > 0)
  {
    CCTK_WARN (0, "Group storage violation in PUGH_EnableGArrayGroupStorage");
    retval = -1;
  }
  else
  {
    retval = nstorage > 0 ? 1 : 0;
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    PUGH_EnableGArrayDataStorage
   @author     Tom Goodale
   @date       30 Mar 1999
   @desc
               Allocates storage for a single variable.
               For now this routine cannot be made static because it's used
               in BAM :-(
   @enddesc
   @calls      Util_CacheMalloc

   @var        GA
   @vdesc      Pointer to the variable's info structure
   @vtype      pGA *
   @vio        in
   @endvar
   @var        this_proc
   @vdesc      the processor ID
   @vtype      int
   @vio        unused
   @endvar
   @var        initialize_memory
   @vdesc      how to initialize allocated memory
   @vtype      const char *
   @vio        in
   @endvar
   @var        padding_active, padding_cacheline_bits, padding_size,
               padding_address_spacing
   @vdesc      padding information
   @vtype      int
   @vio        unused
   @endvar

   @returntype int
   @returndesc
                0 if storage was enabled
               -1 if memory allocation failed
   @endreturndesc
@@*/
/* static */ int PUGH_EnableGArrayDataStorage (pGA *GA,
                                         int this_proc,
                                         const char *initialize_memory,
                                         int padding_active,
                                         int padding_cacheline_bits,
                                         int padding_size,
                                         int padding_address_spacing)
{
  int retval;


  /* avoid compiler warnings about unused parameters */
  this_proc = this_proc;
  padding_active = padding_active;
  padding_cacheline_bits = padding_cacheline_bits;
  padding_size = padding_size;
  padding_address_spacing = padding_address_spacing;

  retval = 0;
  if (GA->storage == PUGH_NOSTORAGE)
  {

#ifdef DEBUG_PUGH
    printf (" PUGH_EnableGArrayDataStorage: allocating storage "
            "for var '%s'\n", GA->name);
    fflush (stdout);
#endif

    if(GA->vector_size > 1 && GA->vector_entry > 0)
    {
      GA->data = (char *)(GA->vector_base->data) + GA->extras->npoints * GA->varsize * GA->vector_entry;
      retval = 0;
    }
    else
    {

      /* Now assign memory for the variable itself */
      if (GA->padddata)
      {
        free (GA->padddata);
        GA->padddata = NULL;
      }

      if (GA->extras->npoints * GA->varsize <= 0)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "PUGH_EnableGArrayDataStorage: Tried to allocate storage "
                    "for zero-sized variable '%s'", GA->name);
        GA->data = GA->padddata = NULL;
      }
      else if (! padding_active)
      {
        /* Easy case. */
        GA->data = malloc (GA->extras->npoints * GA->varsize);
        GA->padddata = GA->data;
      }
      else
      {
        /* Use the Cactus Cache alignment function */
        GA->data = Util_CacheMalloc (GA->arrayid,
                                     GA->extras->npoints * GA->varsize * GA->vector_size,
                                     &GA->padddata);
      }

      /* Initialize the memory if desired. */
      if (GA->data && ! CCTK_Equals (initialize_memory, "none"))
      {
        PUGH_InitializeMemory (initialize_memory, GA->vtype,
                               GA->extras->npoints * GA->varsize, GA->data);
      }
    }

    if (GA->extras->npoints * GA->varsize > 0 && GA->padddata == NULL)
    {
      CCTK_VWarn (0, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "PUGH_EnableGArrayDataStorage: Cannot allocate data for "
                  "'%s' [%d]", GA->name, GA->id);
      retval = -1;
    }
  }

  GA->storage = PUGH_STORAGE;

  return (retval);
}


 /*@@
   @routine    PUGH_DisableGArrayDataStorage
   @author     Tom Goodale
   @date       30 Mar 1999
   @desc
               Frees allocated storage for a variable
               For now this routine cannot be made static because it's used
               in BAM :-(
   @enddesc

   @var        GA
   @vdesc      Pointer to the variable's info structure
   @vtype      pGA *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 if storage was already disabled
                0 if storage was freed
   @endreturndesc
@@*/
/* static */ int PUGH_DisableGArrayDataStorage (pGA *GA)
{
  int retval;

  retval = GA->storage != PUGH_STORAGE;

  if (GA->storage == PUGH_STORAGE)
  {

#ifdef DEBUG_PUGH
    printf ("PUGH_DisableGArrayDataStorage: freeing storage for var '%s'\n",
                GA->name);
    fflush (stdout);
#endif

    if(GA->vector_size > 1 && GA->vector_entry > 0)
    {
      GA->data = GA->padddata;
    }
    else
    {
      if (GA->padddata)
      {
        free (GA->padddata);
      }
      GA->padddata = calloc (1, GA->varsize);
      GA->data = GA->padddata;
    }

    GA->storage = PUGH_NOSTORAGE;
  }

  return retval;
}


 /*@@
   @routine    PUGH_InitializeMemory
   @author     Thomas Radke
   @date       Thu 30 Aug 2001
   @desc
               Initializes allocated memory to all zeros (all variable types)
               or NaNs (floating point types only)
   @enddesc

   @var        initialize_memory
   @vdesc      keyword describing how to initialize memory
   @vtype      const char *
   @vio        in
   @endvar
   @var        vtype
   @vdesc      CCTK variable type of the variable to initialize
   @vtype      int
   @vio        in
   @endvar
   @var        bytes
   @vdesc      total number of bytes to initialize
   @vtype      int
   @vio        in
   @endvar
   @var        data
   @vdesc      pointer to data to initialize
   @vtype      void *
   @vio        in
   @endvar
@@*/
static void PUGH_InitializeMemory (const char *initialize_memory,
                                   int vtype,
                                   int bytes,
                                   void *data)
{
  const char *vtypename;


  /* zero out variable */
  if (CCTK_Equals (initialize_memory, "zero"))
  {
    memset (data, 0, bytes);
  }
  /* set elements to Not-a-Number values (floating point variables only) */
  else if (CCTK_Equals (initialize_memory, "NaN"))
  {
    vtypename = CCTK_VarTypeName (vtype);
    if (strncmp (vtypename, "CCTK_VARIABLE_REAL",    18) == 0 ||
        strncmp (vtypename, "CCTK_VARIABLE_COMPLEX", 22) == 0)
    {
      memset (data, -1, bytes);
    }
  }
  else if (! CCTK_Equals (initialize_memory, "none"))
  {
    CCTK_VWarn (0, __LINE__, __FILE__, CCTK_THORNSTRING,
                "InitializeMemory: Unknown keyword '%s' for "
                "parameter 'initialize_memory'", initialize_memory);
  }
}



 /*@@
   @routine    PUGHi_PrintStorageReport
   @author     Gabrielle Allen
   @date       16th Sept 2001
   @desc
               Print a report about the use of storage
   @enddesc
@@*/
void PUGHi_PrintStorageReport ()
{
  CCTK_INFO("Storage statistics");
  CCTK_VInfo(CCTK_THORNSTRING, "  Maximum storage: %6.2fMB",maxstorage);
  CCTK_VInfo(CCTK_THORNSTRING, "  [%d Grid Functions, %d Grid Arrays]",numberGF,numberGA);
}
