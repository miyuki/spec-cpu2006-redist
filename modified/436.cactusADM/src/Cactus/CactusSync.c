#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusSync.c
   @date      Thu Sep  18 14:27:18 1999
   @author    Gerd Lanfermann
   @desc
              A collection of SyncGroup routines:
              Sync a group by the GROUP INDEX, by the group's VARIABLE NAME,
              by the group's VARIABLE INDEX.

              It ends up calling CCTK_SyncGroup(GH,groupname),
              which is overloaded (currently by PUGH).
   @enddesc
   @version   $Id: CactusSync.c,v 1.10 2001/12/09 23:34:54 tradke Exp $
 @@*/

#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_Comm.h"
#include "cctk_Groups.h"
#include "cctk_Sync.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/comm/CactusSync.c,v 1.10 2001/12/09 23:34:54 tradke Exp $";
CCTK_FILEVERSION(comm_CactusSync_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header cctk_Groups.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL cctk_syncgroupi_
                           (int *ierror, cGH *GH, const int *group);
void CCTK_FCALL cctk_syncgroupwithvar_
                           (int *ierror, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_syncgroupwithvari_
                           (int *ierror, cGH *GH, const int *var);


 /*@@
   @routine    CCTK_SyncGroupI
   @date       Thu Sep  18 14:27:18 1999
   @author     Gerd Lanfermann
   @desc
               Synchronizes a group given by its index.
   @enddesc
   @calls      CCTK_GroupName
               CCTK_SyncGroup

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        group
   @vdesc      group index
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if an invalid group was given,<BR>
               -2 if driver returned an error on syncing the group
   @endreturndesc
@@*/
int CCTK_SyncGroupI (cGH *GH, int group)
{
  int retval;
  char *groupname;


  retval = -1;
  groupname = CCTK_GroupName (group);
  if (groupname)
  {
    retval = CCTK_SyncGroup (GH, groupname);
    if (retval)
    {
      retval = -2;
    }
    free (groupname);
  }

  return (retval);
}

void CCTK_FCALL cctk_syncgroupi_
                           (int *ierror, cGH *GH, const int *group)
{
  CCTK_SyncGroupI (GH, *group);
  *ierror = 0;
}


 /*@@
   @routine    CCTK_SyncGroupWithVar
   @date       Thu Sep  18 14:27:18 1999
   @author     Gerd Lanfermann
   @desc
               Synchronizes a group which contains the given variable
               (given by its name).
   @enddesc
   @calls      CCTK_GroupIndexFromVarI
               CCTK_VarIndex
               CCTK_SyncGroupI

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        varname
   @vdesc      full variable name
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_SyncGroupI
   @endreturndesc
@@*/
int CCTK_SyncGroupWithVar (cGH *GH, const char *varname)
{
  int idx;


  idx = CCTK_VarIndex (varname);
  idx = CCTK_GroupIndexFromVarI (idx);
  return (CCTK_SyncGroupI (GH, idx));
}

void CCTK_FCALL cctk_syncgroupwithvar_
                           (int *ierror, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (varname);
  *ierror = CCTK_SyncGroupWithVar (GH, varname);
  free (varname);
}


 /*@@
   @routine    CCTK_SyncGroupWithVarI
   @date       Thu Sep  18 14:27:18 1999
   @author     Gerd Lanfermann
   @desc
               Synchronizes a group which contains the given variable
               (given by its index).
   @enddesc
   @calls      CCTK_GroupIndexFromVarI
               CCTK_SyncGroupI

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        var
   @vdesc      variable index
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_SyncGroupI
   @endreturndesc
@@*/
int CCTK_SyncGroupWithVarI (cGH *GH, int var)
{
  int idx;


  idx = CCTK_GroupIndexFromVarI (var);
  return (CCTK_SyncGroupI (GH, idx));
}

void CCTK_FCALL cctk_syncgroupwithvari_
                           (int *ierror, cGH *GH, const int *var)
{
  *ierror = CCTK_SyncGroupWithVarI (GH, *var);
}


 /*@@
   @routine    CCTK_SyncGroupsI
   @date       Thu Jan 27 18:00:15 2000
   @author     Tom Goodale
   @desc
               Synchronises a list of groups given by their group indices.
   @enddesc
   @calls      CCTK_SyncGroupI

   @var        GH
   @vdesc      Pointer to Grid Hierachy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        n_groups
   @vdesc      number of groups to synchronize
   @vtype      int
   @vio        in
   @endvar
   @var        groups
   @vdesc      list of group indices
   @vtype      const int *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the total number of groups synchronized
   @endreturndesc
@@*/
int CCTK_SyncGroupsI (cGH *GH,
                      int n_groups,
                      const int *groups)
{
  int i, retval;


  retval = 0;
  for (i = 0; i < n_groups; i++)
  {
    if (CCTK_SyncGroupI (GH, groups[i]) == 0)
    {
      retval++;
    }
  }

  return (retval);
}
