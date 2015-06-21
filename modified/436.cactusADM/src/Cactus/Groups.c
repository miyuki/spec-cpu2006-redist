#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/*@@
   @file      Groups.c
   @date      Mon Feb  1 12:16:28 1999
   @author    Tom Goodale
   @desc
              Routines to deal with groups.
   @enddesc
   @version   $Id: Groups.c,v 1.111 2001/11/27 23:46:28 tradke Exp $
 @@*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "cctk_Constants.h"
#include "cctk_WarnLevel.h"
#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_Groups.h"
#include "cctk_Parameter.h"
#include "cctk_Types.h"

#include "cctki_Stagger.h"
#include "cctki_Groups.h"

#include "util_Expression.h"

#include "util_String.h"

/*#define DEBUG_GROUPS*/

static const char *rcsid = "$Header: /cactus/Cactus/src/main/Groups.c,v 1.111 2001/11/27 23:46:28 tradke Exp $";

CCTK_FILEVERSION(main_Groups_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
/* prototypes for external C routines are declared in header cctk_Groups.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL cctk_groupindex_
                           (int *vindex,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_varindex_
                           (int *vindex,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_maxdim_
                           (int *dim);
void CCTK_FCALL cctk_numvars_
                           (int *num_vars);
void CCTK_FCALL cctk_numgroups_
                           (int *num_groups);
void CCTK_FCALL cctk_groupindexfromvari_
                           (int *gindex,
                            const int *var);
void CCTK_FCALL cctk_groupindexfromvar_
                           (int *vindex,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_grouptypenumber_
                           (int *number,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_vartypenumber_
                           (int *number,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_groupscopenumber_
                           (int *number,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_groupdistribnumber_
                           (int *number,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_firstvarindexi_
                           (int *first,
                            const int *group);
void CCTK_FCALL cctk_numvarsingroup_
                           (int *num,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_vartypei_
                           (int *type,
                            const int *var);
void CCTK_FCALL cctk_numtimelevelsfromvari_
                           (int *num,
                            const int *var);
void CCTK_FCALL cctk_numtimelevelsfromvar_
                           (int *num,
                            ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_printgroup_
                           (const int *group);
void CCTK_FCALL cctk_printvar_
                           (const int *var);
void CCTK_FCALL cctk_groupdimi_
                           (int *dim,
                            const int *group);
void CCTK_FCALL cctk_groupdimfromvari_
                           (int *dim,
                            const int *vi);


/********************************************************************
 ********************    Internal Typedefs   ************************
 ********************************************************************/
typedef struct
{
  char *name;
  int number;

  /* dimensional_comm_array[dim] */
  char *dimensional_comm_array;
} cVariableDefinition;

typedef struct
{
  /* The various names of the thing. */
  char *thorn,
       *implementation,
       *name;

  /* The group number. */
  int number;

  /* The types. */
  int gtype,
      vtype,
      dtype,
      staggertype;

  int gscope;

  int dim;

  int n_timelevels;

  int n_variables;

  /* *size[dim]  - pointers to parameter data*/
  CCTK_INT **size;

  /* *ghostsize[dim]  - pointers to parameter data*/
  CCTK_INT **ghostsize;

  /* variables[n_variables] */
  cVariableDefinition *variables;

  /* Variable array size parameter */

  const char *vararraysize;

} cGroupDefinition;


/********************************************************************
 ********************    Static Variables   *************************
 ********************************************************************/
/* Static variables needed to hold group and variable data. */

static int n_groups = 0;
static cGroupDefinition *groups = NULL;

static int total_variables = 0;

static int *group_of_variable = NULL;

static int maxdim = 0;
static int gfdim = 0;

static int staggered = 0;

/* When passing to fortran, must pass by reference
 * so need to define the odd global variable to pass 8-(
 */

int _cctk_one = 1;


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static cGroupDefinition *CCTKi_SetupGroup (const char *implementation,
                                           const char *name,
                                           int staggercode,
                                           int n_variables);
static CCTK_INT **CCTKi_ExtractSize (int dimension,
                                     const char *thorn,
                                     const char *sizestring);

static int CCTKi_ParamExpressionToInt(const char *expression, const char *thorn);

 /*@@
   @routine    CCTK_StaggerVars
   @date
   @author     Gerd Lanfermann
   @desc
               Checks if staggered group(s) exist
   @enddesc

   @returntype int
   @returndesc
               0 if no staggered group exists, non-zero otherwise
   @endreturndesc
@@*/
int CCTK_StaggerVars (void)
{
  return (staggered);
}


 /*@@
   @routine    CCTK_GroupIndex
   @date       Fri Jan 29 08:43:48 1999
   @author     Tom Goodale
   @desc
               Gets the global index number for the specified group.
   @enddesc

   @returntype int
   @returndesc
               the index of the given group, or negative for failure:
               -1 if no group of such name exists, or
                  error return code of @seeroutine CCTK_DecomposeName
   @endreturndesc
@@*/
int CCTK_GroupIndex (const char *fullgroupname)
{
  int group;
  int retval;
  char *impname,
       *groupname;


  impname = groupname = NULL;
  retval = CCTK_DecomposeName (fullgroupname, &impname, &groupname);
  if (! retval)
  {
    retval = -1;
    for (group = 0; group < n_groups; group++)
    {
      if (CCTK_Equals (impname, groups[group].implementation) &&
          CCTK_Equals (groupname, groups[group].name))
      {
        retval = group;
        break;
      }
    }

    if (retval < 0)
    {
      CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
                  "CCTK_GroupIndex: No group named '%s' found",
                  fullgroupname);
    }
  }

  /* Free memory from CCTK_DecomposeName */
  if (impname)
  {
    free (impname);
  }
  if (groupname)
  {
    free (groupname);
  }

  return (retval);
}

void CCTK_FCALL cctk_groupindex_
                           (int *vindex,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (name)
  *vindex = CCTK_GroupIndex (name);
  free (name);
}


 /*@@
   @routine    CCTK_VarIndex
   @date       Mon Feb  8 12:03:22 1999
   @author     Tom Goodale
   @desc
               For a given variable name, return its associated global index.
   @enddesc

   @returntype int
   @returndesc
               the index of the given variable, or negative for failure:
               -1 if no variable of such name exists, or
                  error return code of @seeroutine CCTK_DecomposeName
   @endreturndesc
@@*/
int CCTK_VarIndex (const char *fullvarname)
{
  int retval,
      group,
      variable;
  char *impname,
       *varname;


  impname = varname = NULL;
  retval = CCTK_DecomposeName (fullvarname, &impname, &varname);
  if (! retval)
  {
    retval = -1;
    for (group = 0; group < n_groups && retval < 0; group++)
    {
      if (CCTK_Equals (impname, groups[group].implementation))
      {
        for (variable = 0; variable < groups[group].n_variables; variable++)
        {
          if (CCTK_Equals (varname, groups[group].variables[variable].name))
          {
            retval = groups[group].variables[variable].number;
            break;
          }
        }
      }
    }

    if (retval < 0)
    {
      CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
                  "CCTK_VarIndex: No variable named '%s' found",
                  fullvarname);
    }
  }

#ifdef DEBUG_GROUPS
  printf (" In VarIndex\n", " ------------\n");
  printf ("   impname -%s-\n", impname);
  printf ("   varname -%s-\n", varname);
#endif

  /* free strings allocated in CCTK_DecomposeName() */
  if (impname)
  {
    free (impname);
  }
  if (varname)
  {
    free (varname);
  }

  return (retval);
}

void CCTK_FCALL cctk_varindex_
                           (int *vindex,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (name)
  *vindex = CCTK_VarIndex (name);
  free (name);
}


 /*@@
   @routine    CCTK_MaxDim
   @date       Mon Feb  8 12:04:01 1999
   @author     Tom Goodale
   @desc
               Gets the maximum dimension of all groups.
   @enddesc

   @returntype int
   @returndesc
               the maximum dimension of all groups
   @endreturndesc
@@*/
int CCTK_MaxDim (void)
{
  return (maxdim);
}

void CCTK_FCALL cctk_maxdim_
                           (int *dim)
{
  *dim = CCTK_MaxDim ();
}


 /*@@
   @routine    CCTK_NumVars
   @date       Mon Feb  8 12:04:50 1999
   @author     Tom Goodale
   @desc
               Gets the total number of variables.
   @enddesc

   @returntype int
   @returndesc
               total number of variables created so far
   @endreturndesc
@@*/
int CCTK_NumVars (void)
{
  return (total_variables);
}

void CCTK_FCALL cctk_numvars_
                           (int *num_vars)
{
  *num_vars = CCTK_NumVars ();
}


 /*@@
   @routine    CCTK_NumGroups
   @date       Mon Feb  8 12:04:50 1999
   @author     Tom Goodale
   @desc
               Gets the total number of groups.
   @enddesc

   @returntype int
   @returndesc
               total number of groups created so far
   @endreturndesc
@@*/
int CCTK_NumGroups (void)
{
  return (n_groups);
}

void CCTK_FCALL cctk_numgroups_
                           (int *num_groups)
{
  *num_groups = CCTK_NumGroups ();
}


 /*@@
   @routine    CCTK_GroupNameFromVarI
   @date       Mon Feb 22
   @author     Gabrielle Allen
   @desc
               Given a variable index return a group name.
   @enddesc

   @returntype char *
   @returndesc
               the full group name of the given variable (which should be freed
               if not needed anymore), or
               NULL if the given variable index is invalid, or out of memory
   @endreturndesc
@@*/
char *CCTK_GroupNameFromVarI (int var)
{
  int group;
  char *fullname;


  if (0 <= var && var < total_variables)
  {
    group = group_of_variable[var];
    fullname = (char *) malloc (strlen (groups[group].name) +
                                strlen (groups[group].implementation) + 3);
    if (fullname)
    {
      sprintf (fullname, "%s::%s",
               groups[group].implementation, groups[group].name);
    }
  }
  else
  {
    fullname = NULL;
  }

  return (fullname);
}


 /*@@
   @routine    CCTK_GroupIndexFromVarI
   @date       Mon Feb 22
   @author     Gabrielle Allen
   @desc
               Given a variable index return a group index.
   @enddesc

   @returntype int
   @returndesc
               the group index of the given variable, or
               -1 if the given variable index is invalid
   @endreturndesc
@@*/
int CCTK_GroupIndexFromVarI (int var)
{
  return ((0 <= var && var < total_variables) ? group_of_variable[var] : -1);
}

void CCTK_FCALL cctk_groupindexfromvari_
                           (int *gindex,
                            const int *var)
{
  *gindex = CCTK_GroupIndexFromVarI (*var);
}



 /*@@
   @routine    CCTK_GroupIndexFromVar
   @date       Mon Feb 22
   @author     Gabrielle Allen
   @desc
               Given a variable name returns a group index.
   @enddesc

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_GroupIndexFromVarI
               -1 if the given variable index is invalid
   @endreturndesc
@@*/
int CCTK_GroupIndexFromVar (const char *var)
{
  return CCTK_GroupIndexFromVarI (CCTK_VarIndex (var));
}

void CCTK_FCALL cctk_groupindexfromvar_
                           (int *vindex,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (var)
  *vindex = CCTK_GroupIndexFromVar (var);
  free (var);
}


 /*@@
   @routine    CCTK_ImpFromVarI
   @date       Mon Feb 22
   @author     Gabrielle Allen
   @desc
               Given a variable index return the implementation name.
   @enddesc

   @returntype const char *
   @returndesc
               the implementation name of the given variable, or
               NULL if given variable index is invalid
   @endreturndesc
@@*/
const char *CCTK_ImpFromVarI (int var)
{
  return ((0 <= var && var < total_variables) ?
          groups[group_of_variable[var]].implementation : NULL);
}


 /*@@
   @routine    CCTK_FullName
   @date       Mon Feb 22
   @author     Gabrielle Allen
   @desc
               Given a variable index return the variable's full name,
               ie. <implementation name>::<variable name>.
   @enddesc

   @returntype char *
   @returndesc
               the full name of the given variable (which should be freed
               if not needed anymore), or
               NULL if given variable index is invalid, or out out memory
   @endreturndesc
@@*/
char *CCTK_FullName (int var)
{
  const char *impname,
             *varname;
  char *fullname;


  varname = CCTK_VarName (var);
  if (varname)
  {
    impname = groups[group_of_variable[var]].implementation;
    fullname = (char *) malloc (strlen (varname) + strlen (impname) + 3);
    if (fullname)
    {
      sprintf (fullname, "%s::%s", impname, varname);
    }
  }
  else
  {
    fullname = NULL;
  }

  return (fullname);
}


 /*@@
   @routine    CCTK_GroupTypeNumber
   @date       Mon Feb  8 14:44:45 1999
   @author     Tom Goodale
   @desc
               Gets the type number associated with a group.
   @enddesc

   @returntype int
   @returndesc
                the type number of the given group type, or
               -1 if given group type is invalid
   @endreturndesc
@@*/
int CCTK_GroupTypeNumber (const char *type)
{
  int retval;


  if (! strcmp (type, "SCALAR"))
  {
    retval = CCTK_SCALAR;
  }
  else if (! strcmp (type, "GF"))
  {
    retval = CCTK_GF;
  }
  else if (! strcmp (type, "ARRAY"))
  {
    retval = CCTK_ARRAY;
  }
  else
  {
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL cctk_grouptypenumber_
                           (int *number,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (type)
  *number = CCTK_GroupTypeNumber (type);
  free (type);
}


 /*@@
   @routine    CCTK_VarTypeNumber
   @date       Mon Feb  8 14:44:45 1999
   @author     Tom Goodale
   @desc
               Gets the type number associated with a variable.
   @enddesc

   @returntype int
   @returndesc
                the type number of the given variable type, or
               -1 if given variable type is invalid
   @endreturndesc
@@*/
int CCTK_VarTypeNumber (const char *type)
{
  int retval;


  if (! strcmp (type, "INT"))
  {
    retval = CCTK_VARIABLE_INT;
  }
  else if (! strcmp (type, "INT2"))
  {
    retval = CCTK_VARIABLE_INT2;
  }
  else if (! strcmp (type, "INT4"))
  {
    retval = CCTK_VARIABLE_INT4;
  }
  else if (! strcmp (type, "INT8"))
  {
    retval = CCTK_VARIABLE_INT8;
  }
  else if (! strcmp (type, "REAL"))
  {
    retval = CCTK_VARIABLE_REAL;
  }
  else if (! strcmp (type, "REAL4"))
  {
    retval = CCTK_VARIABLE_REAL4;
  }
  else if (! strcmp (type, "REAL8"))
  {
    retval = CCTK_VARIABLE_REAL8;
  }
  else if (! strcmp (type, "REAL16"))
  {
    retval = CCTK_VARIABLE_REAL16;
  }
  else if (! strcmp (type, "COMPLEX"))
  {
    retval = CCTK_VARIABLE_COMPLEX;
  }
  else if (! strcmp (type, "COMPLEX8"))
  {
    retval = CCTK_VARIABLE_COMPLEX8;
  }
  else if (! strcmp (type, "COMPLEX16"))
  {
    retval = CCTK_VARIABLE_COMPLEX16;
  }
  else if (! strcmp (type, "COMPLEX32"))
  {
    retval = CCTK_VARIABLE_COMPLEX32;
  }
  else if (! strcmp (type, "BYTE"))
  {
    retval = CCTK_VARIABLE_BYTE;
  }
  /* DEPRECATED IN BETA 10 */
  else if (! strcmp (type, "CHAR"))
  {
    retval = CCTK_VARIABLE_CHAR;
  }
  else
  {
    retval = -1;
  }

  return retval;
}

void CCTK_FCALL cctk_vartypenumber_
                           (int *number,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (type)
  *number = CCTK_VarTypeNumber (type);
  free (type);
}


 /*@@
   @routine    CCTK_VarTypeName
   @date       Mon Jan  3 13:50:56 CET 2000
   @author     Gabrielle Allen
   @desc
               Gets the variable type name associated with a variable type.
   @enddesc

   @returntype const char *
   @returndesc
                the type name of the given variable type, or
               -1 if given variable type is invalid
   @endreturndesc
@@*/
const char *CCTK_VarTypeName (int vtype)
{
  const char *retval;


  switch (vtype)
  {
    case CCTK_VARIABLE_INT:
      retval = "CCTK_VARIABLE_INT";
      break;

    case CCTK_VARIABLE_INT2:
      retval = "CCTK_VARIABLE_INT2";
      break;

    case CCTK_VARIABLE_INT4:
      retval = "CCTK_VARIABLE_INT4";
      break;

    case CCTK_VARIABLE_INT8:
      retval = "CCTK_VARIABLE_INT8";
      break;

    case CCTK_VARIABLE_REAL:
      retval = "CCTK_VARIABLE_REAL";
      break;

    case CCTK_VARIABLE_REAL4:
      retval = "CCTK_VARIABLE_REAL4";
      break;

    case CCTK_VARIABLE_REAL8:
      retval = "CCTK_VARIABLE_REAL8";
      break;

    case CCTK_VARIABLE_COMPLEX:
      retval = "CCTK_VARIABLE_COMPLEX";
      break;

    case CCTK_VARIABLE_COMPLEX8:
      retval = "CCTK_VARIABLE_COMPLEX8";
      break;

    case CCTK_VARIABLE_COMPLEX16:
      retval = "CCTK_VARIABLE_COMPLEX16";
      break;

    case CCTK_VARIABLE_COMPLEX32:
      retval = "CCTK_VARIABLE_COMPLEX32";
      break;

    case CCTK_VARIABLE_CHAR:
      retval = "CCTK_VARIABLE_CHAR";
      break;

    case CCTK_VARIABLE_STRING:
      retval = "CCTK_VARIABLE_STRING";
      break;

    default:
      retval = NULL;
      break;
  }

  return retval;
}


 /*@@
   @routine    CCTK_GroupScopeNumber
   @date       Tuesday June 22 1999
   @author     Gabrielle Allen
   @desc
               Gets the scope number associated with a group.
   @enddesc

   @returntype int
   @returndesc
                the scope number of the given scope type, or
               -1 if given scope type is invalid
   @endreturndesc
@@*/
int CCTK_GroupScopeNumber (const char *type)
{
  int retval;


  if (! strcmp (type, "PRIVATE"))
  {
    retval = CCTK_PRIVATE;
  }
  else if (! strcmp (type, "PROTECTED"))
  {
    retval = CCTK_PROTECTED;
  }
  else if (! strcmp (type, "PUBLIC"))
  {
    retval = CCTK_PUBLIC;
  }
  else
  {
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL cctk_groupscopenumber_
                           (int *number,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (type)
  *number = CCTK_GroupScopeNumber (type);
  free (type);
}


 /*@@
   @routine    CCTK_GroupDistribNumber
   @date       Tuesday June 22 1999
   @author     Gabrielle Allen
   @desc
               Gets the distribution number associated with a group.
   @enddesc

   @returntype int
   @returndesc
                the distribution number of the given distribution type, or
               -1 if given distribution type is invalid
   @endreturndesc
@@*/
int CCTK_GroupDistribNumber (const char *dtype)
{
  int retval;


  if (! strcmp (dtype, "CONSTANT"))
  {
    retval = CCTK_DISTRIB_CONSTANT;
  }
  else if (! strcmp (dtype, "DEFAULT"))
  {
    retval = CCTK_DISTRIB_DEFAULT;
  }
  else
  {
    retval = -1;
  }

  return (retval);
}

void CCTK_FCALL cctk_groupdistribnumber_
                           (int *number,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (type)
  *number = CCTK_GroupDistribNumber (type);
  free (type);
}


 /*@@
   @routine    CCTK_GroupData
   @date       Mon Feb  8 15:56:01 1999
   @author     Tom Goodale
   @desc
               For a given group index, gets the group type, variable type,
               and the number of variables
   @enddesc

   @returntype int
   @returndesc
                0 for success
               -1 if given group index is invalid
               -2 if given pointer to store group data is NULL
   @endreturndesc
@@*/
int CCTK_GroupData (int group, cGroup *gp)
{
  int retval;


  retval = (0 <= group && group < n_groups) ? 0 : -1;
  if (! retval)
  {
    if (gp)
    {
      gp->grouptype     = groups[group].gtype;
      gp->vartype       = groups[group].vtype;
      gp->disttype      = groups[group].dtype;
      gp->dim           = groups[group].dim;
      gp->numvars       = groups[group].n_variables;
      gp->numtimelevels = groups[group].n_timelevels;
      gp->stagtype      = groups[group].staggertype;

      if(groups[group].vararraysize)
      {
        gp->vectorgroup = 1;
      }
      else
      {
        gp->vectorgroup = 0;
      }
    }
    else
    {
      retval = -2;
    }
  }

  return (retval);
}


 /*@@
   @routine    CCTK_VarName
   @date       Tue Feb  9 15:34:56 1999
   @author     Tom Goodale
   @desc
               Gets the name of a variable.
   @enddesc

   @returntype const char *
   @returndesc
               the name of the given variable, or
               -1 if given variable index is invalid
   @endreturndesc
@@*/
const char *CCTK_VarName (int var)
{
  return ((0 <= var && var < total_variables) ?
          groups[group_of_variable[var]]
            .variables[var-groups[group_of_variable[var]].variables[0].number]
            .name
          : NULL);
}


 /*@@
   @routine    CCTK_DecomposeName
   @date       Tue Feb  9 15:39:14 1999
   @author     Tom Goodale
   @desc
               Decomposes a full group or variable name of the form imp::name
   @enddesc

   @returntype int
   @returndesc
                0 for success (implementation and name are set to the
                  full name's implementation and name), or
                  negative otherwise (a non-zero error return code of
                  @seeroutine Util_SplitString is translated into one of the
                  following error codes:
               -2 if failed to catch error code from Util_SplitString
               -3 if given full name is in wrong format
               -4 if memory allocation failed
   @endreturndesc
@@*/
int CCTK_DecomposeName (const char *fullname,
                        char **implementation,
                        char **name)
{
  int retval;


  retval = Util_SplitString (implementation, name, fullname, "::");
  if (retval)
  {
    if (retval == 1)
    {
      CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                  "CCTK_DecomposeName: Full name '%s' in wrong format",
                  fullname);
      retval = -3;
    }
    else if (retval == 2)
    {
      CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
                 "CCTK_DecomposeName: Memory allocation failed");
      retval = -4;
    }
    else
    {
      CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
                 "CCTK_DecomposeName: Error failed to be caught");
      retval = -2;
    }
  }

  return (retval);
}


 /*@@
   @routine    CCTK_GroupName
   @date       Tue Apr  9 15:39:14 1999
   @author     Gabrielle Allen
   @desc
               Given a group index returns the group name
   @enddesc

   @returntype char *
   @returndesc
               the full name of the given group (which should be freed
               if not needed anymore), or
               -1 if given group index is invalid
   @endreturndesc
@@*/
char *CCTK_GroupName (int group)
{
  char *name;


  name = NULL;
  if (0 <= group && group < n_groups)
  {
    name = (char *) malloc (strlen (groups[group].implementation) +
                            strlen (groups[group].name) + 3);
    if (name)
    {
      sprintf (name, "%s::%s",
               groups[group].implementation, groups[group].name);
    }
  }

  return (name);
}


 /*@@
   @routine    CCTK_FirstVarIndexI
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group index returns the first variable index in the group
   @enddesc

   @returntype int
   @returndesc
               the index of the first variable in the given group, or
               -1 if given group index is invalid
   @endreturndesc
@@*/
int CCTK_FirstVarIndexI (int group)
{
  return ((0 <= group && group < n_groups) ?
          groups[group].variables[0].number : -1);
}

void CCTK_FCALL cctk_firstvarindexi_
                           (int *first,
                            const int *group)
{
  *first = CCTK_FirstVarIndexI (*group);
}


 /*@@
   @routine    CCTK_FirstVarIndex
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group name returns the first variable index in the group
   @enddesc

   @returntype int
   @returndesc
               the index of the first variable in the given group, or
               -1 if given group name is invalid
   @endreturndesc
@@*/
int CCTK_FirstVarIndex (const char *groupname)
{
  return CCTK_FirstVarIndexI (CCTK_GroupIndex (groupname));
}


 /*@@
   @routine    CCTK_NumVarsInGroupI
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group index returns the number of variables in the group
   @enddesc

   @returntype int
   @returndesc
               the number of variables in the given group, or
               -1 if given group index is invalid
   @endreturndesc
@@*/
int CCTK_NumVarsInGroupI (int group)
{
  return ((0 <= group && group < n_groups) ? groups[group].n_variables : -1);
}


 /*@@
   @routine    CCTK_NumVarsInGroup
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group name returns the number of variables in the group
   @enddesc

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_NumVarsInGroupI
               -1 if given group name is invalid
   @endreturndesc
@@*/
int CCTK_NumVarsInGroup (const char *groupname)
{
  return CCTK_NumVarsInGroupI (CCTK_GroupIndex (groupname));
}

void CCTK_FCALL cctk_numvarsingroup_
                           (int *num,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (groupname)
  *num = CCTK_NumVarsInGroup (groupname);
  free (groupname);
}


 /*@@
   @routine    CCTK_GroupTypeFromVarI
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a variable index return the type of group
   @enddesc

   @returntype int
   @returndesc
               the type of the given group, or
               -1 if given variable index is invalid
   @endreturndesc
@@*/
int CCTK_GroupTypeFromVarI (int var)
{
  return ((0 <= var && var < total_variables) ?
          groups[group_of_variable[var]].gtype : -1);
}


 /*@@
   @routine    CCTK_GroupTypeI
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group index return the type of group
   @enddesc

   @returntype int
   @returndesc
               the type of the given group, or
               -1 if given group index is invalid
   @endreturndesc
@@*/
int CCTK_GroupTypeI (int group)
{
  return ((0 <= group && group < n_groups) ? groups[group].gtype : -1);
}


 /*@@
   @routine    CCTK_VarTypeI
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a variable index return the variable type
   @enddesc

   @returntype int
   @returndesc
               the type of the given variable, or
               -1 if given variable index is invalid
   @endreturndesc
@@*/
int CCTK_VarTypeI (int var)
{
  return ((0 <= var && var < total_variables) ?
          groups[group_of_variable[var]].vtype : -1);
}

void CCTK_FCALL cctk_vartypei_
                           (int *type,
                            const int *var)
{
  *type = CCTK_VarTypeI (*var);
}


 /*@@
   @routine    CCTK_NumTimeLevelsI
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a variable index return the number of timelevels
   @enddesc

   @returntype int
   @returndesc
               the number of timelevels of the given variable, or
               -1 if given variable index is invalid
   @endreturndesc
@@*/
int CCTK_NumTimeLevelsFromVarI (int var)
{
  return ((0 <= var && var < total_variables) ?
          groups[group_of_variable[var]].n_timelevels : -1);
}

void CCTK_FCALL cctk_numtimelevelsfromvari_
                           (int *num,
                            const int *var)
{
  *num = CCTK_NumTimeLevelsFromVarI (*var);
}


 /*@@
   @routine    CCTK_NumTimeLevelsFromVar
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a variable name return the number of timelevels
   @enddesc

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_NumTimeLevelsFromVarI
   @endreturndesc
@@*/
int CCTK_NumTimeLevelsFromVar (const char *var)
{
  return CCTK_NumTimeLevelsFromVarI (CCTK_VarIndex (var));
}

void CCTK_FCALL cctk_numtimelevelsfromvar_
                           (int *num,
                            ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (var)
  *num = CCTK_NumTimeLevelsFromVar (var);
  free (var);
}


 /*@@
   @routine    CCTK_PrintGroup
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group index print the group name.
               This is for debugging purposes for Fortran routines.
   @enddesc
@@*/
void CCTK_FCALL cctk_printgroup_
                           (const int *group)
{
  fprintf (stdout, "Group %d is %s\n", *group, CCTK_GroupName (*group));
}


 /*@@
   @routine    CCTK_PrintVar
   @date       3 July 1999
   @author     Gabrielle Allen
   @desc
               Given a group index print the variable name.
               This is for debugging purposes for Fortran.
   @enddesc
@@*/
void CCTK_FCALL cctk_printvar_
                           (const int *var)
{
  fprintf (stdout, "Variable %d is %s\n", *var, CCTK_VarName (*var));
}


 /*@@
   @routine    CCTK_GroupSizesI
   @date       Sun Nov 28 12:56:44 1999
   @author     Tom Goodale
   @desc
               Returns the size array for a group.
   @enddesc

   @returntype CCTK_INT **
   @returndesc
               the pointer to the size array of the given group, or
               -1 if invalid group index was given
   @endreturndesc
@@*/
CCTK_INT **CCTK_GroupSizesI (int group)
{
  return ((0 <= group && group < n_groups) ? groups[group].size : NULL);
}


 /*@@
   @routine    CCTK_GroupGhostsizesI
   @date       Sun Jan 23 12:56:44 2000
   @author     Gabrielle Allen
   @desc
               Returns the ghostsize array for a group.
   @enddesc

   @returntype CCTK_INT **
   @returndesc
               the pointer to the ghostsize array of the given group, or
               NULL if invalid group index was given or the group exists
               but has no ghostsize information
   @endreturndesc
@@*/
CCTK_INT **CCTK_GroupGhostsizesI (int group)
{
  return ((0 <= group && group < n_groups) ? groups[group].ghostsize : NULL);
}


 /*@@
   @routine    CCTK_VarTypeSize
   @date       Sun Dec  5 10:08:05 1999
   @author     Gabrielle Allen
   @desc
               Returns the size of a given variable type
   @enddesc

   @returntype int
   @returndesc
               positive for the variable type's size (in bytes), or
               -1 if invalid variable type was given
   @endreturndesc
@@*/
int CCTK_VarTypeSize (int vtype)
{
  int var_size;


  switch (vtype)
  {
    case CCTK_VARIABLE_BYTE:
      var_size = sizeof (CCTK_BYTE);
      break;

    case CCTK_VARIABLE_POINTER:
      var_size = sizeof (CCTK_POINTER);
      break;

    case CCTK_VARIABLE_FN_POINTER:
      var_size = sizeof (CCTK_FN_POINTER);
      break;

    case CCTK_VARIABLE_INT:
      var_size = sizeof (CCTK_INT);
      break;

    case CCTK_VARIABLE_REAL:
      var_size = sizeof (CCTK_REAL);
      break;

    case CCTK_VARIABLE_COMPLEX:
      var_size = sizeof (CCTK_COMPLEX);
      break;

#ifdef CCTK_INT2
    case CCTK_VARIABLE_INT2:
      var_size = sizeof (CCTK_INT2);
      break;
#endif

#ifdef CCTK_INT4
    case CCTK_VARIABLE_INT4:
      var_size = sizeof (CCTK_INT4);
      break;
#endif

#ifdef CCTK_INT8
    case CCTK_VARIABLE_INT8:
      var_size = sizeof (CCTK_INT8);
      break;
#endif

#ifdef CCTK_REAL4
    case CCTK_VARIABLE_REAL4:
      var_size = sizeof (CCTK_REAL4);
      break;

    case CCTK_VARIABLE_COMPLEX8:
      var_size = sizeof (CCTK_COMPLEX8);
      break;
#endif

#ifdef CCTK_REAL8
    case CCTK_VARIABLE_REAL8:
      var_size = sizeof (CCTK_REAL8);
      break;

    case CCTK_VARIABLE_COMPLEX16:
      var_size = sizeof (CCTK_COMPLEX16);
      break;
#endif

#ifdef CCTK_REAL16
    case CCTK_VARIABLE_REAL16:
      var_size = sizeof (CCTK_REAL16);
      break;

    case CCTK_VARIABLE_COMPLEX32:
      var_size = sizeof (CCTK_COMPLEX32);
      break;
#endif

    default:
      CCTK_VWarn (4, __LINE__, __FILE__, "Cactus",
                  "CCTK_VarTypeSize: Unknown variable type (%d)", vtype);
      var_size = -1;
  }

  return (var_size);
}


 /*@@
   @routine    CCTK_GroupDimI
   @date       Wed Feb 2 2000
   @author     Gabrielle Allen
   @desc
               Given a group index returns the group dimension
   @enddesc

   @returntype int
   @returndesc
               the dimension of the given group, or
               -1 if given group index is invalid
   @endreturndesc
@@*/
int CCTK_GroupDimI (int group)
{
  return ((0 <= group && group < n_groups) ? groups[group].dim : -1);
}

void CCTK_FCALL cctk_groupdimi_
                           (int *dim,
                            const int *group)
{
  *dim = CCTK_GroupDimI (*group);
}


 /*@@
   @routine    CCTK_GroupDimFromVarI
   @date       Wed Feb 2 2000
   @author     Gabrielle Allen
   @desc
               Given a variable index returns the group dimension
   @enddesc

   @returntype int
   @returndesc
               the dimension of the variable's group, or
               -1 if given variable index is invalid
   @endreturndesc
@@*/
int CCTK_GroupDimFromVarI (int var)
{
  return ((0 <= var && var < total_variables) ?
          groups[group_of_variable[var]].dim : -1);
}

void CCTK_FCALL cctk_groupdimfromvari_
                           (int *dim,
                            const int *var)
{
  *dim = CCTK_GroupDimFromVarI (*var);
}


 /*@@
   @routine    CCTK_TraverseString
   @date       Wed 20 Sep 2000
   @author     Thomas Radke
   @desc
               Traverse through all variables and/or groups whose names
               appear in the given string, and call the callback routine
               with those indices and an optional option string appended
               to the variable/group name enclosed in square braces.
               The special keyword "all" in the string can be used to
               indicate that the callback should be called for all
               variables/groups.
   @enddesc
   @var        traverse_string
   @vdesc      list of variable and/or group names
   @vtype      const char *
   @vio        in
   @endvar
   @var        callback
   @vdesc      routine to call for every variable and/or group found
   @vtype      int (*) (int idx, const char *optstring, void *callback_arg)
   @vio        int
   @endvar
   @var        callback_arg
   @vdesc      an arbitrary argument which gets passed to the callback routine
   @vtype      void *
   @vio        in
   @endvar
   @var        selection
   @vdesc      decides whether group and/or variable names are accepted
               in the string
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               positive for the number of traversed variables, or
               -1 no callback routine was given<BR>
               -2 option string is not associated with a group or variable<BR>
               -3 unterminated option string<BR>
               -4 garbage found at end of option string
   @endreturndesc
@@*/
int CCTK_TraverseString (const char *traverse_string,
                         void (*callback) (int idx,
                                           const char *optstring,
                                           void *callback_arg),
                         void *callback_arg,
                         int selection)
{
  int retval, nesting, vindex, gindex, first, last, selected_all;
  char delimiter, *string, *parse_string, *group_var_string, *option_string;


  if (callback == NULL)
  {
    CCTK_VWarn (5, __LINE__, __FILE__, "Cactus", 
                "CCTK_TraverseString: No callback given");
    return (-1);
  }

  retval = 0;

  /* create a work copy of the string to traverse
     which we can edit in-place */
  parse_string = strdup (traverse_string);

  /* parse it token by token */
  string = parse_string;
  while (string && *string)
  {

    /* skip leading spaces */
    while (*string && isspace (*string))
    {
      string++;
    }
    if (! *string)
    {
      break;
    }

    /* find end of group/varname string (can be either EOS,
       space before next token, or following option string) */
    group_var_string = string;
    while (*string)
    {
      if (! *string || isspace (*string) || *string == '[')
      {
        break;
      }
      string++;
    }

    /* mark end of group/varname string */
    delimiter = *string;
    *string = 0;

    /* parse the option string if there is one */
    option_string = delimiter == '[' ? string + 1: NULL;
    if (option_string)
    {
      /* find end of option string (matching bracket) */
      nesting = 1;
      while (*(++string))
      {
        if (*string == '[')
        {
          nesting++;
        }
        else if (*string == ']')
        {
          if (--nesting == 0)
          {
            break;
          }
        }
      }
      delimiter = *string;
      *string = 0;
      if (option_string == group_var_string + 1)
      {
        CCTK_VWarn (5, __LINE__, __FILE__, "Cactus", 
                    "CCTK_TraverseString: option string '%s' not associated "
                    "with a group or variable name", option_string);
        retval = -2;
        break;
      }
      else if (! (delimiter == ']' && nesting == 0))
      {
        CCTK_VWarn (5, __LINE__, __FILE__, "Cactus", 
                    "CCTK_TraverseString: unterminated option string '%s'",
                    option_string);
        retval = -3;
        break;
      }
      else if (! (string[1] == 0 || isspace (string[1])))
      {
        CCTK_VWarn (5, __LINE__, __FILE__, "Cactus", 
                    "CCTK_TraverseString: garbage at end of option string '%s'",
                    option_string);
        retval = -4;
        break;
      }
    }

#ifdef DEBUG_GROUPS
    printf ("group/varname is '%s', option string is '%s'\n",
            group_var_string, option_string ? option_string : "(null)");
#endif

    /* Look for the token 'all' */
    selected_all = CCTK_Equals (group_var_string, "all");

    /* See if this name is "<implementation>::<variable>" */
    if (! selected_all &&
        (selection == CCTK_VAR || selection == CCTK_GROUP_OR_VAR))
    {
      first = last = CCTK_VarIndex (group_var_string);
    }
    else
    {
      first = last = -1;
    }
    if (first < 0)
    {

      /* See if this name is "<implementation>::<group>" */
      if (! selected_all &&
          (selection == CCTK_GROUP || selection == CCTK_GROUP_OR_VAR))
      {
        gindex = CCTK_GroupIndex (group_var_string);
      }
      else
      {
        gindex = -1;
      }
      if (gindex >= 0)
      {
        /* We have a group so now need all the variables in the group */
        first = CCTK_FirstVarIndexI (gindex);
        last = first + CCTK_NumVarsInGroupI (gindex) - 1;
      }
      else if (selected_all)
      {
        first = 0;
        if (selection == CCTK_GROUP)
        {
          last = CCTK_NumGroups () - 1;
        }
        else
        {
          last = CCTK_NumVars () - 1;
        }
      }
      else
      {
        first = last = -1;
      }
    }

    /* invoke the callback */
    if (first >= 0)
    {
      for (vindex = first; vindex <= last; vindex++)
      {
        (*callback) (vindex, option_string, callback_arg);
      }
      retval += last - first + 1;
    }
    else
    {
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTK_TraverseString: Ignoring '%s' in string "
                  "(invalid token)", group_var_string);
    }

    /* advance the parse string pointer */
    if (delimiter)
    {
      string++;
    }
  } /* end of while loop over all tokens in parse string */

  /* clean up */
  free (parse_string);

  return (retval);
}


#if 0
/*@@
   @routine    CCTKi_PrintGroupInfo
   @date       Thu Jan 14 15:25:54 1999
   @author     Gerd Lanfermann
   @desc
     Debugging info on the Groups.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory

@@*/

void CCTKi_PrintGroupInfo (void)
{
  int group_num;

  for (group_num = 0; group_num < n_groups; group_num++)
  {
    printf ("GROUP INFO: GrpNo./imp_name/name/stag %d   >%s<   >%s<  %d\n",
           group_num,
           groups[group_num].implementation,
           groups[group_num].name,
           groups[group_num].staggertype);
  }
}
#endif


 /*@@
   @routine    CCTKi_CreateGroup
   @date       Thu Jan 14 15:25:54 1999
   @author     Tom Goodale
   @desc
               Creates a new CCTK group
   @enddesc

   @returntype int
   @returndesc
               0 for success, non-zero otherwise
   @endreturndesc
@@*/
int CCTKi_CreateGroup (const char *gname,
                       const char *thorn,
                       const char *imp,
                       const char *gtype,
                       const char *vtype,
                       const char *gscope,
                       int         dimension,
                       int         ntimelevels,
                       const char *stype,
                       const char *dtype,
                       const char *size,
                       const char *ghostsize,
                       int         n_variables,
                       ...
                       )
{
  int retval;
  int groupscope;
  int staggercode;
  int variable;

  va_list ap;

  char *variable_name;
  char *vararraysize;

  cGroupDefinition *group;

  group = NULL;
  variable_name = NULL;
  vararraysize = NULL;

  retval = 0;

  va_start (ap, n_variables);

  /* get the staggercode */
  staggercode = CCTKi_ParseStaggerString (dimension, imp, gname, stype);

  if(n_variables == -1)
  {
    variable_name = va_arg (ap, char *);
      
    vararraysize = Util_Strdup(va_arg (ap, char *));

    n_variables = CCTKi_ParamExpressionToInt(vararraysize,thorn);

    if(n_variables < 1)
    {
      CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                  "CCTKi_CreateGroup: length of group %s less than 1 !",
                  gname);
    }
  }

  /* Allocate storage for the group */
  groupscope = CCTK_GroupScopeNumber (gscope);
  if (groupscope == CCTK_PUBLIC || groupscope == CCTK_PROTECTED)
  {
    group = CCTKi_SetupGroup (imp, gname, staggercode, n_variables);
  }
  else if (groupscope == CCTK_PRIVATE)
  {
    group = CCTKi_SetupGroup (thorn, gname, staggercode, n_variables);
  }
  else
  {
    CCTK_Warn (1, __LINE__, __FILE__, "Cactus",
              "CCTKi_CreateGroup: Unrecognised group scope");
  }

  /* Allocate storage for the group and setup some stuff. */
  if (group)
  {
    group->dim          = dimension;
    group->gtype        = CCTK_GroupTypeNumber (gtype);
    group->vtype        = CCTK_VarTypeNumber (vtype);
    group->gscope       = groupscope;
    group->staggertype  = staggercode;
    group->dtype        = CCTK_GroupDistribNumber (dtype);
    group->n_timelevels = ntimelevels;

    /* Extract the variable names from the argument list. */

    if(! vararraysize)
    {
      group->vararraysize = NULL;
      for (variable = 0; variable < n_variables; variable++)
      {
        variable_name = va_arg (ap, char *);

        group->variables[variable].name =
          (char *)malloc ((strlen (variable_name)+1)*sizeof (char));
        
        if (group->variables[variable].name)
        {
          strcpy (group->variables[variable].name, variable_name);
        }
        else
        {
          break;
        }
      }
    }
    else
    {
      group->vararraysize = Util_Strdup(va_arg (ap, char *));

      for (variable = 0; variable < n_variables; variable++)
      {
        char *name = NULL;
        Util_asprintf(&name, "%s[%d]", variable_name, variable);

        group->variables[variable].name = name;
      }
    }

    va_end (ap);

    if (variable < n_variables)
    {
      retval = 3;
    }
    else
    {
      if (dimension > maxdim)
      {
        maxdim    = dimension;
      }
      if (staggercode > 0)
      {
        staggered = 1;
      }
      group->size      = CCTKi_ExtractSize (dimension, thorn, size);
      group->ghostsize = CCTKi_ExtractSize (dimension, thorn, ghostsize);
    }

    /* Only typically have GFs in a single dimension */
    if (group->gtype == CCTK_GF)
    {
      if (gfdim > 0)
      {
        if (group->dim != gfdim)
        {
          retval = 1;
        }
      }
      else
      {
        gfdim = group->dim;
      }
    }
   
  }
  else
  {
    retval = 2;
  }

  if (retval)
  {
    CCTK_Warn (4, __LINE__, __FILE__, "Cactus", "CCTKi_CreateGroup: Error");
  }

  return (retval);
}


 /*@@
   @routine    CCTK_GroupImpI
   @date       20 Oct 2001
   @author     Gabrielle Allen
   @desc
   Return the implementation which created a group
   @enddesc

   @returntype const char *
   @returndesc
   Thorn name
   @endreturndesc
@@*/

const char *CCTK_GroupImplementationI(int group)
{
  const char *imp;
  
  imp = groups[group].implementation;

  return imp;
}

/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/

 /*@@
   @routine    CCTKi_SetupGroup
   @date       Thu Jan 14 16:38:40 1999
   @author     Tom Goodale
   @desc
               Stores the data associated with a group.
   @enddesc

   @returntype cGroupDefinition *
   @returndesc
               pointer to the associated group data structure, or
               NULL if out of memory
   @endreturndesc
@@*/
static cGroupDefinition *CCTKi_SetupGroup (const char *implementation,
                                           const char *name,
                                           int staggercode,
                                           int n_variables)
{
  int *temp_int;
  void *temp;
  cGroupDefinition *returndata;
  int variable;
  int group;


  for (group = 0; group < n_groups; group++)
  {
    if (CCTK_Equals (implementation, groups[group].implementation) &&
        CCTK_Equals (name, groups[group].name))
    {
      break;
    }
  }

  if (group >= n_groups)
  {
    /* Resize the array of groups */
    temp = realloc (groups, (n_groups + 1) * sizeof (cGroupDefinition));
    if (temp)
    {
      groups = (cGroupDefinition *) temp;

      /* Allocate memory to various fields */
      groups[n_groups].implementation = (char *) malloc (strlen (implementation)+1);

      groups[n_groups].name = (char *) malloc (strlen (name) + 1);

      groups[n_groups].variables = (cVariableDefinition *)
                                   malloc (n_variables *
                                           sizeof (cVariableDefinition));

      /* Resize the array holding correspondence between vars and groups. */
      temp_int = (int *) realloc (group_of_variable,
                                  (total_variables+n_variables) * sizeof (int));

      if (groups[n_groups].implementation &&
          groups[n_groups].name &&
          groups[n_groups].variables &&
          temp_int)
      {
        /* Fill in the data structures. */
        group_of_variable = temp_int;

        strcpy (groups[n_groups].implementation, implementation);
        strcpy (groups[n_groups].name, name);

        groups[n_groups].number     = n_groups;
        groups[n_groups].staggertype= staggercode;
        groups[n_groups].n_variables= n_variables;

        /* Fill in global variable numbers. */
        for (variable = 0; variable < n_variables; variable++)
        {
          groups[n_groups].variables[variable].number = total_variables;

          group_of_variable[total_variables] = n_groups;

          total_variables++;
        }

        n_groups++;
      }
      else
      {
        /* Memory allocation failed, so free any which may have been allocated. */
        free (groups[n_groups].implementation);
        groups[n_groups].implementation = NULL;

        free (groups[n_groups].name);
        groups[n_groups].name = NULL;

        free (groups[n_groups].variables);
        groups[n_groups].variables = NULL;

      }
    }

    /* Return the new group definition structure if successful, otherwise NULL.*/
    if (temp && groups[n_groups-1].name)
    {
      returndata =  &groups[n_groups-1];
    }
    else
    {
      returndata = NULL;
    }
  }
  else
  {
    returndata = &groups[group];
  }

#ifdef DEBUG_GROUPS
  printf ("Setting up group %s::%s\n", implementation, name);
#endif

  return (returndata);
}


 /*@@
   @routine    CCTKi_ExtractSize
   @date       Sun Nov 28 12:38:38 1999
   @author     Tom Goodale
   @desc
               Extracts the size array from a comma-separated list of
               positive integer constants or arithmetical combinations of
               integer parameters.

               FIXME:  This routine originally returned a pointer to the
                       parameter values, which is why it returns **.
                       Now it doesn't, so we probably have a memory leak.
   @enddesc

   @returntype CCTK_INT **
   @returndesc
               pointer to an allocated array of sizes for the given list, or
               NULL if no parameter list is given or out of memory
   @endreturndesc
@@*/
static CCTK_INT **CCTKi_ExtractSize (int dimension,
                                     const char *this_thorn,
                                     const char *sizestring)
{
  int         dim;
  char       *tmp;
  const char *last_comma, *next_comma;
  CCTK_INT   **size_array;


  if (strlen (sizestring))
  {
    next_comma = sizestring;

    size_array = (CCTK_INT **) malloc (dimension * sizeof (CCTK_INT *));

    if (size_array)
    {
      size_array[0] = (CCTK_INT *) malloc (dimension * sizeof (CCTK_INT));

      for (dim = 1; dim < dimension; dim++)
      {
        size_array[dim] = size_array[0] + dim;
      }

      for (dim = 0; dim < dimension; dim++)
      {
        /* find the comma as a delimiter for different dimension sizes */
        last_comma = next_comma[0] == ',' ? next_comma+1 : next_comma;
        next_comma = strstr (last_comma, ",");

        /* copy dimension size token into a work string buffer */
        tmp = strdup (last_comma);
        if (next_comma)
        {
          tmp[next_comma-last_comma] = '\0';
        }

        *size_array[dim] = CCTKi_ParamExpressionToInt (tmp, this_thorn);
   
        free (tmp);
      }
    }
  }
  else
  {
    /* No size specified */
    size_array = NULL;
  }

  return size_array;
}

 /*@@
   @routine    CCTKi_GroupLengthAsPointer
   @date       Sun Oct  7 03:58:44 2001
   @author     Tom Goodale
   @desc 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     fullgroupname
   @vdesc   The full name of a GV group
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype const int *
   @returndesc
               pointer to an integer containing the number of variables in the group
               NULL if group doesn't exist
   @endreturndesc
@@*/
const int *CCTKi_GroupLengthAsPointer(const char *fullgroupname)
{
  int group;
  const int *retval;
  char *impname, *groupname;


  retval = NULL;
  impname = NULL;
  groupname = NULL;

  
  if (! CCTK_DecomposeName (fullgroupname, &impname, &groupname))
  {
    for (group = 0; group < n_groups; group++)
    {
      if (CCTK_Equals (impname, groups[group].implementation) &&
          CCTK_Equals (groupname, groups[group].name))
      {
        retval = &groups[group].n_variables;
        break;
      }
    }

    if (retval == NULL)
    {
      CCTK_VWarn (6, __LINE__, __FILE__, "Cactus",
                  "CCTK_GroupIndex: No group named '%s' found",
                  fullgroupname);
    }
  }

  /* Free memory from CCTK_DecomposeName */
  if (impname)
  {
    free (impname);
  }
  if (groupname)
  {
    free (groupname);
  }

  return retval;
}

 /*@@
   @routine    IntParameterEvaluator
   @date       Fri Oct 12 10:01:32 2001
   @author     Tom Goodale
   @desc 
   Evaluates integer parameters for the expression parser.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     nvars
   @vdesc   Number of variables to evaluate
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     vars
   @vdesc   an array of parameter names or integers
   @vtype   const char * const *
   @vio     in
   @vcomment 
 
   @endvar
   @var     vals
   @vdesc   Output array to hold values
   @vtype   uExpressionValue *
   @vio     out
   @vcomment 
 
   @endvar 
   @var     data
   @vdesc   Data passed from expression evaluator
   @vtype   void *
   @vio     in
   @vcomment 
     Should be a char * with the thorn name.
   @endvar 

   @returntype int
   @returndesc
   0
   @endreturndesc
@@*/
static int IntParameterEvaluator(int nvars, 
                                 const char * const *vars, 
                                 uExpressionValue *vals, 
                                 void *data)
{
  int i;

  char *tmp;
  char *endptr;
  const CCTK_INT *paramval;
  char *thorn;
  char *param;
  const char *use_thorn;
  const char *use_param;
  int type;
  
  for(i=0; i < nvars; i++)
  {
    vals[i].type = ival;
    tmp = Util_Strdup(vars[i]);

    vals[i].value.ival = strtol(tmp,&endptr,0);

    if(endptr == tmp)
    {
      if(CCTK_DecomposeName (tmp,&thorn,&param))
      {
        thorn = NULL;
        param = NULL;
        use_thorn = (const char *)data;
        use_param = tmp;
      }
      else
      {
        use_thorn = thorn;
        use_param = param;
      }

      paramval = (const CCTK_INT *) CCTK_ParameterGet (use_param, use_thorn, &type);

      if(paramval && type==PARAMETER_INTEGER)
      {
        vals[i].value.ival = *paramval;
      }
      else if(!paramval)
      {
        CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                    "IntParameterEvaluator: '%s::%s' is not a parameter",
                    use_thorn, use_param);
      }
      else
      {
        CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                    "IntParameterEvaluator: '%s::%s' is not an integer parameter",
                    use_thorn, use_param);
      }
      free(thorn);
      free(param);
    }

    free(tmp);
  }

  return 0;
}
  
 /*@@
   @routine    CCTKi_ParamExpressionToInt
   @date       Fri Oct 12 10:04:18 2001
   @author     Tom Goodale
   @desc 
   Parses an arithmetic expression involving integer parameter names and
   returns the final integer
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     expression
   @vdesc   The expression to be parsed
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     thorn
   @vdesc   The thorn name
   @vtype   const char *
   @vio     in
   @vcomment 
     Used for parameters which aren't fully qualified.
   @endvar 

   @returntype int
   @returndesc
     The final value of the exppression
   @endreturndesc
@@*/
static int CCTKi_ParamExpressionToInt(const char *expression, const char *thorn)
{
  int retval;
  uExpression parsed_expression;
  uExpressionValue value;
  char *this_thorn;

  this_thorn = Util_Strdup(thorn);

  parsed_expression = Util_ExpressionParse(expression);

  if(parsed_expression)
  {
    /* Evaluate the expression */
    retval = Util_ExpressionEvaluate(parsed_expression,
                                     &value,
                                     IntParameterEvaluator, 
                                     this_thorn);

    Util_ExpressionFree(parsed_expression);
  }
  else
  {
    CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                "Unable to parse parameter expression '%s'",
                expression);
    retval = -1;
  }

  free(this_thorn);

  if(retval == 0)
  {
    retval = value.value.ival;
  }
  else
  {
    CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                "Unable to evaluate parameter expression '%s'",
                expression);
  }    

  return retval;
}
