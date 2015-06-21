#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/*@@
   @file      Parameters.c
   @date      Mon Jun 28 21:44:17 1999
   @author    Tom Goodale
   @desc
              Routines to deal with the parameters.
   @enddesc
   @version   $Id: Parameters.c,v 1.44 2002/01/02 12:15:57 tradke Exp $
 @@*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "cctk_Flesh.h"
#include "cctk_ActiveThorns.h"
#include "cctk_Constants.h"
#include "cctk_WarnLevel.h"
#include "cctk_Parameter.h"
#include "cctk_GNU.h"
#include "cctk_FortranString.h"
#include "ParameterBindings.h"

static const char *rcsid="$Header: /cactus/Cactus/src/main/Parameters.c,v 1.44 2002/01/02 12:15:57 tradke Exp $";

CCTK_FILEVERSION(main_Parameters_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/* what is a parameter:
 * - properties
 * - data
 * nothing else!
 * Props are supposed to be public to thorn programmers,
 * Data are supposed to be private to flesh!
 */
typedef struct PARAM
{
  cParamData *props;
  void       *data;
} t_param;

/* what is a list of parameters:
 * just a normal linked list...
 */
typedef struct PARAMLIST
{
  t_param          *param;
  struct PARAMLIST *last;
  struct PARAMLIST *next;
} t_paramlist;

/* what is a node of parameter tree
 * look at it: a list of linked lists... ;-)
 */
typedef struct PARAMTREENODE
{
  t_paramlist *paramlist;
} t_paramtreenode;

/********************************************************************
 ************************* Static Variables *************************
 ********************************************************************/

/* mask for CCTK_ParameterSet() */
static int cctk_parameter_set_mask;

/********************************************************************
 ********************* Fortran Wrapper Prototypes *******************
 ********************************************************************/

void CCTK_FCALL cctk_parametervalstring_
                           (CCTK_INT *nchars, THREE_FORTSTRING_ARG);

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static t_param *ParameterFind (const char *name,
                               const char *thorn,
                               int scope);

static t_param *ParameterNew (const char *thorn,
                              const char *name,
                              const char *type,
                              const char *scope,
                              int        steerable,
                              const char *description,
                              const char *defval,
                              void       *data);

static const void *ParameterGetSimple (const t_param *param, int *type);

static int ParameterSetSimple (t_param *param, const char *value);

static t_paramtreenode *ParameterPTreeNodeFind (t_sktree *tree,
                                                const char *name);

static int ParameterGetScope (const char *scope);
static int ParameterGetType (const char *type);

static int ParameterInsert (t_sktree **tree, t_param *newparam);

#ifdef TEST_PARAMETERS
static int ParameterPrintSimple (t_param *param,
                                 const char *format,
                                 FILE *file);
#endif

static int ParameterExtend (t_param *param,
                            const char *range_origin,
                            const char *range,
                            const char *range_description);

static int ParameterListAddParam (t_paramlist **paramlist,
                                  t_param *newparam);


static int ParameterSetKeyword  (t_param *param, const char *value);
static int ParameterSetString   (t_param *param, const char *value);
static int ParameterSetSentence (t_param *param, const char *value);
static int ParameterSetInteger  (t_param *param, const char *value);
static int ParameterSetReal     (t_param *param, const char *value);
static int ParameterSetBoolean  (t_param *param, const char *value);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

int CCTK_RegexMatch (const char *string,
                     const char *pattern,
                     const int nmatch,
                     regmatch_t *pmatch);

int STR_cmpi (const char *string1, const char *string2);
#define STR_CMP(a,b) STR_cmpi (a, b)

extern void CCTKi_SetParameterSetMask (int mask);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static t_sktree *paramtree = NULL;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/


/*@@
   @routine    CCTKi_ParameterCreate
   @date       Tue Jun 29 10:08:56 1999
   @author     Tom Goodale
   @desc
               Creates a parameter originating from a thorn/implementation.
   @enddesc
   @calls      ParameterFind
               ParameterNew
               ParameterExtend
               ParameterSetSimple

   @var        name
   @vdesc      The name of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      The originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        scope
   @vdesc      The scope of the parameter
   @vtype      const char *
   @vio        in
   @vcomment
               Private, Restricted, or Global
   @endvar
   @var        steerable
   @vdesc      Is the parameter steerable ?
   @vtype      int
   @vio        in
   @endvar
   @var        description
   @vdesc      A description of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        defval
   @vdesc      The default value
   @vtype      const char *
   @vio        in
   @endvar
   @var        data
   @vdesc      A pointer to the memory holding the parameter
   @vtype      void *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 on success, or<br>
               -1 on failure to create the parameter<br>
               -2 if parameter inconsistent
   @endreturndesc
@@*/
int CCTKi_ParameterCreate (const char *name,
                           const char *thorn,
                           const char *type,
                           const char *scope,
                           int        steerable,
                           const char *description,
                           const char *defval,
                           void       *data,
                           int n_ranges,
                           ...)
{
  int i, retval;
  t_param *param;
  va_list ranges;
  const char *rangeval, *rangedesc;


  param = ParameterFind (name, thorn, ParameterGetScope (scope));
  if (! param)
  {
    param = ParameterNew (thorn, name, type, scope, steerable, description,
                          defval, data);
    if (n_ranges)
    {
      va_start (ranges, n_ranges);

      for (i = 0; i < n_ranges; i++)
      {
        rangeval = (const char *) va_arg (ranges, const char *);
        rangedesc = (const char *) va_arg (ranges, const char *);

        ParameterExtend (param, thorn, rangeval, rangedesc);
      }
      va_end (ranges);
    }

    retval = ParameterSetSimple (param, defval);
  }
  else
  {
    retval = -2;
  }

  return (retval);
}


/*@@
   @routine    CCTKi_ParameterAddRange
   @date       Tue Jun 29 10:15:53 1999
   @author     Tom Goodale
   @desc
               Adds a range.
               Only allowed to add a range if in appropriate scope.
   @enddesc
   @calls      CCTK_ImpThornList
               SKTreeFindFirst
               ParameterFind
               ParameterExtend

   @var        origin
   @vdesc      The originating implementation or thorn of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        name
   @vdesc      The name of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        range_origin
   @vdesc      The originating implementation or thorn of the range
   @vtype      const char *
   @vio        in
   @endvar
   @var        range
   @vdesc      The new range
   @vtype      const char *
   @vio        in
   @endvar
   @var        range_description
   @vdesc      A description of the new range
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 on success, or<br>
               -1 if no such parameter exists
   @endreturndesc
@@*/
int CCTKi_ParameterAddRange (const char *implementation,
                             const char *name,
                             const char *range_origin,
                             const char *range,
                             const char *range_description)
{
  int retval;
  t_param *param;
  /* For the moment do this in the quick and dirty way 8-(  FIXME */
  t_sktree *thornlist, *node;


  retval = -1;

  thornlist = CCTK_ImpThornList (implementation);
  if (thornlist)
  {
    for (node = SKTreeFindFirst (thornlist); node; node = node->next)
    {
      param = ParameterFind (name, node->key, SCOPE_RESTRICTED);
      if (param)
      {
        retval = ParameterExtend (param, range_origin, range,range_description);
      }
      else
      {
        retval = -1;
      }
    }
  }

  return (retval);
}


/*@@
   @routine    CCTK_ParameterSet
   @date       Tue Jun 29 10:22:22 1999
   @author     Tom Goodale
   @desc
               Sets the value (checks for steerable if not initialisation).
   @enddesc
   @calls      ParameterFind
               ParameterSetSimple

   @var        name
   @vdesc      The name of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      The originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        value
   @vdesc      The value of the parameter
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<br>
               return code of @seeroutine ParameterSetSimple, or<br>
               -1 if parameter is out of range<br>
               -2 if parameter was not found<br>
               -3 if trying to steer a non-steerable parameter
   @endreturndesc
@@*/
int CCTK_ParameterSet (const char *name, const char *thorn, const char *value)
{
  int retval;
  t_param *param;


  param = ParameterFind (name, thorn, SCOPE_ANY);
  if (param)
  {
    /* before parameter recovery (which is while parsing the parameter file)
       all parameters can be set */
    if (cctk_parameter_set_mask == PARAMETER_RECOVERY_POST &&
        param->props->steerable != CCTK_STEERABLE_ALWAYS)
    {
      /* after parameter recovery only steerable parameters can be set */
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTK_ParameterSet: Cannot set parameter '%s::%s' "
                  "(non-steerable)", thorn, name);
      retval = -3;
    }
    else if (cctk_parameter_set_mask == PARAMETER_RECOVERY_IN &&
             param->props->n_set > 0)
    {
      /* during parameter recovery STEERABLE_NEVER parameters which were set
       from the parameter file are overwritten by the checkpoint file */
      if (param->props->steerable == CCTK_STEERABLE_NEVER)
      {
        CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                    "CCTK_ParameterSet: Non-steerable parameter '%s::%s' is "
                    "not set from the parameter file but recovered from the "
                    "checkpoint file",
                    thorn, name);
      }
      else
      {
        /* do not restore the original value */
      }
      retval = 0;
    }
    else
    {
      retval = ParameterSetSimple (param, value);

      /* register another set operation */
      param->props->n_set++;
    }
  }
  else
  {
    retval = -2;
  }

  return (retval);
}


/*@@
   @routine    CCTK_ParameterGet
   @date       Tue Jun 29 10:28:20 1999
   @author     Tom Goodale
   @desc
               Gets the pointer to the parameter.
               Should be used for checkpointing and recovery.
   @enddesc
   @calls      ParameterFind
               ParameterGetSimple

   @var        name
   @vdesc      The name of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      The originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        type
   @vdesc      The type of the parameter
   @vtype      int *
   @vio        out
   @vcomment   An integer representing the type of the parameter
   @endvar

   @returntype const void *
   @returndesc
               The address of the parameter, or<br>
               NULL if no parameter by that name was found
   @endreturndesc
@@*/
const void *CCTK_ParameterGet (const char *name, const char *thorn, int *type)
{
  const void *retval;
  t_param *param;


  param = ParameterFind (name, thorn, SCOPE_ANY);
  if (param)
  {
    retval = ParameterGetSimple (param, type);
  }
  else
  {
    retval = NULL;
  }

  return (retval);
}



/*@@
   @routine    CCTK_ParameterQueryTimesSet
   @date       Fri July 7 2000
   @author     Gabrielle Allen
   @desc
               Returns the number of times that a parameter has been set,
               that is if it returns 0 the parameter was not set in a
               parameter file.
   @enddesc
   @calls      CCTK_ParameterData

   @var        name
   @vdesc      The name of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      The originating thorn
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               number of times set, or<br>
               -1 if no parameter by that name exists
   @endreturndesc
@@*/
int CCTK_ParameterQueryTimesSet (const char *name, const char *thorn)
{
  const cParamData *param;


  param = CCTK_ParameterData (name, thorn);

  return (param ? param->n_set : -1);
}


/*@@
   @routine    CCTK_ParameterValString
   @date       Thu Jan 21 2000
   @author     Thomas Radke
   @desc
               Gets the string representation of a parameter's value
               - can be used for checkpointing and recovery.
   @enddesc
   @calls      CCTK_ParameterGet

   @var        param_name
   @vdesc      The name of the parameter
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      The originating thorn
   @vtype      const char *
   @vio        in
   @endvar

   @returntype char *
   @returndesc
               the allocated string (should be freed after usage), or<br>
               NULL if no parameter by that name was found
   @endreturndesc
@@*/
char *CCTK_ParameterValString (const char *param_name, const char *thorn)
{
  int param_type;
  const void *param_data;
  char *retval;
  char buffer[80];


  retval = NULL;

  param_data = CCTK_ParameterGet (param_name, thorn, &param_type);
  if (param_data)
  {
    switch (param_type)
    {
      case PARAMETER_KEYWORD:
      case PARAMETER_STRING:
      case PARAMETER_SENTENCE:
        retval = strdup (*(const char **) param_data);
        break;

      case PARAMETER_BOOLEAN:
        retval = strdup ((int) (*(const CCTK_INT *) param_data) ? "yes" : "no");
        break;

      case PARAMETER_INT:
        sprintf (buffer, "%d", (int) (*(const CCTK_INT *) param_data));
        retval = strdup (buffer);
        break;

      case PARAMETER_REAL:
        sprintf (buffer, "%g", (double) (*(const CCTK_REAL *) param_data));
        retval = strdup (buffer);
        break;

      default:
        CCTK_VWarn (3, __LINE__, __FILE__, "Cactus",
                    "CCTK_ParameterValString: Unknown type %d for parameter "
                    "'%s::%s'", param_type, thorn, param_name);
        break;
    }
  }

  return (retval);
}

/*@@
   @routine    CCTK_PARAMETERVARSTRING
   @date       Thu Jan 21 2000
   @author     Thomas Radke
   @desc
               Copies the stringified parameter value into a fortran string.
   @enddesc
   @calls      CCTK_ParameterValString

   @var        nchars
   @vdesc      Number of characters in the stringified parameter value, or<BR>
               -1 if the parameter doesn't exist
   @vtype      CCTK_INT *
   @vio        out
   @vcomment
               It will copy only as many characters as fit into the fortran
               string. You should check for truncation by comparing 'nchars'
               against the length of your fortran string.
   @endvar
   @var        THREE_FORTSTRING_ARG
   @vdesc      three fortran string arguments denoting parameter name,
               thorn name, and the string buffer to copy the parameter value to
   @vtype      FORTRAN string macro
   @vio        inout
   @endvar
@@*/
void CCTK_FCALL cctk_parametervalstring_
                           (CCTK_INT *nchars, THREE_FORTSTRING_ARG)
{
  size_t c_strlen;
  char *c_string;
  THREE_FORTSTRING_PTR (unused1, unused2, fortran_string)
  THREE_FORTSTRING_CREATE (param, thorn, value)


  /* get rid of compiler warnings about unused variables */
  unused1 = unused1;
  unused2 = unused2;

  c_string = CCTK_ParameterValString (param, thorn);
  if (c_string)
  {
    *nchars = c_strlen = strlen (c_string);
    if (c_strlen > (size_t) cctk_strlen3)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTK_ParameterValString: fortran string buffer is too short "
                  "to hold value '%s' of parameter '%s::%s', string will be "
                  "truncated", c_string, thorn, param);
      c_strlen = (size_t) cctk_strlen3;
    }
    /* copy up to the size of the fortran string
       and pad remaining chars in the fortran string with spaces */
    memcpy (fortran_string, c_string, c_strlen);
    memset (fortran_string + c_strlen, ' ', (size_t) cctk_strlen3 - c_strlen);
  }
  else
  {
    /* no such parameter exists */
    *nchars = -1;
  }

  free (param);
  free (thorn);
  free (value);
}

/*@@
   @routine    CCTK_ParameterWalk
   @date       Tue Jun 29 10:30:21 1999
   @author     Tom Goodale
   @desc
               Gets parameters in order, restricted to ones from 'origin',
               or all if 'origin' is NULL.  Starts with the first parameter
               if 'first' is true, otherwise gets the next one.
               Can be used for generating full help file, or for walking
               the list and checkpointing.
   @enddesc
   @calls      CCTK_ThornImplementation
               SKTreeFindFirst

   @var        first
   @vdesc      Flag to indicate get first parameter or not
   @vtype      int
   @vio        in
   @endvar
   @var        origin
   @vdesc      The thorn to walk, or NULL if to walk all params
   @vtype      const char *
   @vio        in
   @endvar
   @var        pfullname
   @vdesc      address of pointer to string where the fullname of the parameter
               will be stored
   @vtype      char *
   @vio        out
   @vcomment
               If not NULL, the full name is written into an allocated string
               which can be refered to via *pfullname. Should be freed after
               usage.  Full name is "implementation::name" for global and
               restricted parameters, and "thorn::name" for private parameters.
   @endvar
   @var        pdata
   @vdesc      address of pointer to parameter data structure
   @vtype      cParamData **
   @vio        out
   @vcomment
               If not NULL, the pointer to the parameter data structure
               is stored in *pdata.
   @endvar

   @returntype int
   @returndesc
            - zero for success
            - positive if parameter was not found
            - negative if initial startpoint was not set
   @endreturndesc

@@*/
int CCTK_ParameterWalk (int first,
                        const char *origin,
                        char **pfullname,
                        const cParamData **pdata)
{
  int             return_found;
  t_sktree        *tnode;
  t_paramtreenode *node;
  t_paramlist     *paramlist;
  t_param         *startpoint;
  static t_param  *prev_startpoint_all = NULL;
  static t_param  *prev_startpoint_thorn = NULL;

  /* FIXME : This routine has become extremely ugly:
   *         It should only have one return in it.
   *         The malloc failure should be flagged.
   */

  /* determine the startpoint for search */
  if (! first)
  {
    startpoint = origin ? prev_startpoint_thorn : prev_startpoint_all;

    if (startpoint == NULL)
    {
      CCTK_Warn (2, __LINE__, __FILE__, "Cactus",
                 "CCTK_ParameterWalk: Cannot walk through parameter list "
                 "without setting a startpoint at first");
      return (-1);
    }
    /* return next match AFTER startpoint */
    return_found = 0;
  }
  else
  {
    /* return next match which also becomes startpoint for following searches */
    return_found = 1;
    startpoint = NULL;
  }

  /* iterate over nodes */
  for (tnode = SKTreeFindFirst (paramtree) ; tnode ; tnode = tnode->next)
  {
    /* get node data */
    node  = (t_paramtreenode *) tnode->data;

    /* iterate over parameters in list */
    for (paramlist = node->paramlist; paramlist; paramlist = paramlist->next)
    {
      /* if startpoint is still unassigned set it to next match in list */
      if (startpoint == NULL)
      {
        if (! origin || CCTK_Equals (origin, paramlist->param->props->thorn))
        {
          startpoint = paramlist->param;
        }
      }

      /* Hey, we've found the startpoint ! */
      if (startpoint == paramlist->param)
      {
        /* Do we have to return this one ?
           If not prepare finding the next matching param. */
        if (return_found)
        {
          const char *prefix;

          if (pfullname)
          {
            prefix = startpoint->props->thorn;
            if (startpoint->props->scope != SCOPE_PRIVATE)
            {
              prefix = CCTK_ThornImplementation (prefix);
            }

            *pfullname = (char *) malloc (strlen (prefix) +
                                      strlen (startpoint->props->name) + 3);
            if(*pfullname)
            {
              sprintf (*pfullname, "%s::%s", prefix, startpoint->props->name);
            }
          }

          if (pdata)
          {
            *pdata = startpoint->props;
          }

          /* save the last startpoint */
          prev_startpoint_all = prev_startpoint_thorn = startpoint;

          return (0);
        }
        else
        {
          startpoint = NULL;
          return_found = 1;
        }
      }
    } /* end looping over parameter list */
  } /* end looping over all nodes */

  return (1);
}


/**********************************************************************/
/*@@
  @routine    CCTK_ParameterData
  @date       Tue Aug 31 18:10:46 MSZ 1999
  @author     Andre Merzky
  @desc
              For a given parameter, return description, type and range.
  @enddesc
  @calls      ParameterFind

  @var        name
  @vdesc      parameter name
  @vtype      const char *
  @vio        in
  @endvar
  @var        thorn
  @vdesc      thorn parameter belongs to
  @vtype      const char *
  @vio        in
  @endvar

  @returntype const cParamData *
  @returndesc
              pointer to parameter data structure on success,<br>
              NULL on failure.
  @endreturndesc
@@*/
const cParamData *CCTK_ParameterData (const char *name,
                                      const char *thorn)
{
  const t_param *param;


  param = ParameterFind (name, thorn, SCOPE_ANY);

  return (param ? param->props : NULL);
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    ParameterFind
   @date       Sun Oct 17 16:20:48 1999
   @author     Tom Goodale
   @desc
               Finds a parameter given by its name, thorn, and scope.
   @enddesc
   @calls      ParameterPTreeNodeFind
@@*/
static t_param *ParameterFind (const char *name,
                               const char *thorn,
                               int scope)
{
  t_paramtreenode *node;
  t_paramlist *list;


  list = NULL;

  node = ParameterPTreeNodeFind (paramtree, name);
  if (node)
  {
    for (list = node->paramlist; list; list = list->next)
    {
      if (! thorn)
      {
        if (list->param->props->scope == SCOPE_GLOBAL)
        {
          break;
        }
      }
      else if (scope == SCOPE_ANY)
      {
        if (thorn && ! STR_CMP (thorn, list->param->props->thorn))
        {
          break;
        }
      }
      else if (! STR_CMP (thorn, list->param->props->thorn) &&
               list->param->props->scope == scope)
      {
        break;
      }
    }
  }

  return (list ? list->param : NULL);
}


/*@@
   @routine    ParameterNew
   @date       Mon Jul 26 10:59:42 1999
   @author     Tom Goodale
   @desc
               Creates a new parameter.
   @enddesc
   @calls      ParameterGetScope
               ParameterGetType
               ParameterInsert
@@*/
static t_param *ParameterNew (const char *thorn,
                              const char *name,
                              const char *type,
                              const char *scope,
                              int        steerable,
                              const char *description,
                              const char *defval,
                              void       *data)
{
  t_param *newparam;


  newparam = (t_param *) malloc (sizeof (t_param));
  if (newparam)
  {
    newparam->props = (cParamData *) malloc (sizeof (cParamData));
    if (newparam->props)
    {
      newparam->props->thorn       = strdup (thorn);
      newparam->props->name        = strdup (name);
      newparam->props->description = strdup (description);
      newparam->props->defval      = strdup (defval);
      newparam->props->scope       = ParameterGetScope(scope);
      newparam->props->type        = ParameterGetType(type);
      newparam->props->steerable   = steerable;
      newparam->props->range       = NULL;
      newparam->props->n_set       = 0;

      newparam->data = data;

      if (newparam->props->type == PARAMETER_STRING ||
          newparam->props->type == PARAMETER_SENTENCE ||
          newparam->props->type == PARAMETER_KEYWORD)
      {
        *(char **) data = NULL;
      }

      ParameterInsert (&paramtree, newparam);
    }
  }

  return (newparam);
}


static t_paramtreenode *ParameterPTreeNodeFind (t_sktree *tree,
                                                const char *name)
{
  const t_sktree *node;


  node = SKTreeFindNode (tree, name);

  return (node ? (t_paramtreenode *) node->data : NULL);
}


static int ParameterGetScope (const char *scope)
{
  int retval;


  if (! STR_CMP (scope, "GLOBAL"))
  {
    retval = SCOPE_GLOBAL;
  }
  else if (! STR_CMP(scope, "RESTRICTED"))
  {
    retval = SCOPE_RESTRICTED;
  }
  else if (! STR_CMP(scope, "PRIVATE"))
  {
    retval = SCOPE_PRIVATE;
  }
  else
  {
    retval = -1;
  }

  return (retval);
}


static int ParameterGetType (const char *type)
{
  int retval;


  if (! STR_CMP (type, "KEYWORD"))
  {
    retval = PARAMETER_KEYWORD;
  }
  else if (! STR_CMP (type, "STRING"))
  {
    retval = PARAMETER_STRING;
  }
  else if (! STR_CMP (type, "SENTENCE"))
  {
    retval = PARAMETER_SENTENCE;
  }
  else if (! STR_CMP (type, "INT"))
  {
    retval = PARAMETER_INT;
  }
  else if (! STR_CMP (type, "REAL"))
  {
    retval = PARAMETER_REAL;
  }
  else if (! STR_CMP (type, "BOOLEAN"))
  {
    retval = PARAMETER_BOOLEAN;
  }
  else
  {
    retval = -1;
  }

  return (retval);
}


/*@@
   @routine    ParameterInsert
   @date       Fri Jul 16 10:08:25 1999
   @author     Tom Goodale
   @desc

   @enddesc
   @calls
@@*/
static int ParameterInsert (t_sktree **tree, t_param *newparam)
{
  int retval;
  t_sktree *treenode;
  t_paramtreenode *node;
  t_paramlist *list;


  treenode = SKTreeFindNode (*tree, newparam->props->name);
  if (treenode)
  {
    retval = ParameterListAddParam (&((t_paramtreenode *) treenode->data)->paramlist, newparam);
  }
  else
  {
    node = (t_paramtreenode *) malloc (sizeof (t_paramtreenode));
    list = (t_paramlist *) malloc (sizeof (t_paramlist));

    if (node && list)
    {
      node->paramlist = list;
      list->param = newparam;
      list->last = list->next = NULL;
      treenode = SKTreeStoreData (*tree, *tree, newparam->props->name, node);
      if (! *tree)
      {
        *tree = treenode;
      }
      retval = 0;
    }
    else
    {
      retval = -1;
      free (list);
      free (node);
    }
  }

  return (retval);
}


/*@@
   @routine    ParameterGetSimple
   @date       Fri Jul 16 10:07:46 1999
   @author     Tom Goodale
   @desc
               Gets the value of a parameter.
   @enddesc
@@*/
static const void *ParameterGetSimple (const t_param *param, int *type)
{
  *type = param->props->type;

  return (param->data);
}


/*@@
   @routine    ParameterExtend
   @date       Thu Jul 15 12:55:06 1999
   @author     Tom Goodale
   @desc
               Adds a range to a parameter.
   @enddesc
   @calls
@@*/
static int ParameterExtend (t_param *param,
                            const char *range_origin,
                            const char *range,
                            const char *range_description)
{
  int order, retcode;
  t_range *newrange, *rangenode, *lastnode;


  newrange = (t_range *) malloc (sizeof (t_range));
  if (newrange)
  {
    /* Fill out the data */
    newrange->range = strdup (range);
    newrange->origin = strdup (range_origin);
    newrange->description = strdup (range_description);
    newrange->last = newrange->next = NULL;
    newrange->active = 0;

    lastnode = NULL;

    /* Search the list for the right place to insert it. */
    for (rangenode = param->props->range; rangenode;rangenode = rangenode->next)
    {
      lastnode = rangenode;

      order = STR_CMP (range_origin, rangenode->origin);

      if (order <= 0)
      {
        /* Insert before this node */
        newrange->next = rangenode;
        newrange->last = rangenode->last;
        rangenode->last = newrange;
        if (param->props->range == rangenode)
        {
          param->props->range=newrange;
        }
        if (newrange->last)
        {
          newrange->last->next = newrange;
        }
        break;
      }
    }

    if (! rangenode)
    {
      /* Insert at the end of the list */
      newrange->next = NULL;
      newrange->last = lastnode;
      if (param->props->range == NULL)
      {
        param->props->range = newrange;
      }
      if (newrange->last)
      {
        newrange->last->next = newrange;
      }
    }

    retcode = 0;
  }
  else
  {
    retcode = -1;
  }

  return (retcode);
}


static int ParameterSetSimple (t_param *param, const char *value)
{
  int retval;


  switch (param->props->type)
  {
    case PARAMETER_KEYWORD:
      retval = ParameterSetKeyword (param, value); break;
    case PARAMETER_STRING:
      retval = ParameterSetString (param, value); break;
    case PARAMETER_SENTENCE:
      retval = ParameterSetSentence (param, value); break;
    case PARAMETER_INT:
      retval = ParameterSetInteger (param, value); break;
    case PARAMETER_REAL:
      retval = ParameterSetReal (param, value); break;
    case PARAMETER_BOOLEAN:
      retval = ParameterSetBoolean (param, value); break;
    default:
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "Unknown parameter type %d", param->props->type);
      retval = -2;
  }

  return (retval);
}


static int ParameterListAddParam (t_paramlist **paramlist, t_param *newparam)
{
  t_paramlist *node;


  node = (t_paramlist *) malloc (sizeof (t_paramlist));
  if (node)
  {
    node->param = newparam;

    /* Place at beginning of list for now. */
    node->next = *paramlist;
    node->last = NULL;
    (*paramlist)->last = node;

    *paramlist = node;
  }

  return (node != NULL);
}


static int ParameterSetKeyword (t_param *param, const char *value)
{
  int retval;
  const t_range *range;


  retval = -1;
  for (range = param->props->range; range; range = range->next)
  {
    if (CCTK_IsThornActive (range->origin) ||
        CCTK_Equals (param->props->thorn, range->origin))
    {
      if (!STR_CMP(value, range->range))
      {
        retval = CCTK_SetString (param->data, value);
        break;
      }
    }
  }

  if (retval == -1)
  {
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "ParameterSetKeyword: Unable to set keyword %s::%s - '%s' not "
                "in any active range",
                param->props->thorn, param->props->name, value);

    if (*(char **) param->data == NULL)
    {
      fprintf (stderr, "Since this was the default value, "
                       "setting anyway - please fix!\n");
      CCTK_SetString (param->data, value);
    }
  }

  return (retval);
}


static int ParameterSetString (t_param *param, const char *value)
{
  int retval;
  const t_range *range;


  retval = -1;
  for (range = param->props->range; range; range = range->next)
  {
    if (CCTK_IsThornActive (range->origin) ||
        CCTK_Equals (param->props->thorn, range->origin))
    {
#ifndef CCTK_PARAMUNCHECKED
      if (CCTK_RegexMatch (value, range->range, 0, NULL))
      {
#endif
        retval = CCTK_SetString (param->data, value);
        break;
#ifndef CCTK_PARAMUNCHECKED
      }
#endif
    }
  }

  if (retval == -1)
  {
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "ParameterSetString: Unable to set string '%s::%s' - '%s' not "
                "in any active range",
                param->props->thorn, param->props->name, value);

    if (*(char **) param->data == NULL)
    {
      fprintf (stderr, "Since this was the default value, "
                       "setting anyway - please fix!\n");
      CCTK_SetString (param->data, value);
    }
  }

  return (retval);
}


static int ParameterSetSentence (t_param *param, const char *value)
{
  int retval;
  const t_range *range;


  retval = -1;
  for (range = param->props->range; range; range = range->next)
  {
    if (CCTK_IsThornActive (range->origin) ||
        CCTK_Equals (param->props->thorn, range->origin))
    {
#ifndef CCTK_PARAMUNCHECKED
      if (CCTK_RegexMatch (value, range->range, 0, NULL))
      {
#endif
        retval = CCTK_SetString (param->data, value);
        break;
#ifndef CCTK_PARAMUNCHECKED
      }
#endif
    }
  }

  if (retval == -1)
  {
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "ParameterSetSentance: Unable to set sentance '%s::%s' - '%s' "
                "not in any active range",
                param->props->thorn, param->props->name, value);

    if (*(char **) param->data == NULL)
    {
      fprintf (stderr, "Since this was the default value, "
                       "setting anyway - please fix!\n");
      CCTK_SetString (param->data, value);
    }
  }

  return (retval);
}


static int ParameterSetInteger (t_param *param, const char *value)
{
  int inval, retval;
  const t_range *range;


  retval = -1;
  inval = strtol (value, NULL, 0);

  for (range = param->props->range; range; range = range->next)
  {
    if (CCTK_IsThornActive (range->origin) ||
        CCTK_Equals (param->props->thorn, range->origin))
    {
#ifndef CCTK_PARAMUNCHECKED
      if (Util_IntInRange (inval, range->range))
      {
#endif
        *(CCTK_INT *) param->data = inval;
        retval = 0;
        break;
#ifndef CCTK_PARAMUNCHECKED
      }
#endif
    }
  }

  if (retval == -1)
  {
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "ParameterSetInteger: Unable to set integer '%s::%s' - '%s' "
                "not in any active range",
                param->props->thorn, param->props->name, value);
  }

  return (retval);
}


static int ParameterSetReal (t_param *param, const char *value)
{
  int retval;
  unsigned int p;
  const t_range *range;
  double inval;
  char *temp;


  /*  Convert the value string to a double. Allow various formats.*/
  temp = strdup (value);
  for (p = 0; p < strlen (temp); p++)
  {
    if (temp[p] == 'E' || temp[p] == 'd' || temp[p] == 'D')
    {
      temp[p] = 'e';
      break;
    }
  }
  inval = atof (temp);
  free (temp);

  retval = -1;
  for (range = param->props->range; range; range = range->next)
  {
    if (CCTK_IsThornActive (range->origin) ||
        CCTK_Equals (param->props->thorn, range->origin))
    {
#ifndef CCTK_PARAMUNCHECKED
      if(Util_DoubleInRange (inval, range->range))
      {
#endif
        *(CCTK_REAL *) param->data = inval;
        retval = 0;
        break;
#ifndef CCTK_PARAMUNCHECKED
      }
#endif
    }
  }

  if (retval == -1)
  {
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "ParameterSetReal: Unable to set real '%s:%s' - '%s' not in "
                "any active range",
                param->props->thorn, param->props->name, value);
  }

  return (retval);
}


static int ParameterSetBoolean (t_param *param, const char *value)
{
  int retval;


  retval = CCTK_SetBoolean (param->data, value);
  if (retval == -1)
  {
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "ParameterSetBoolean: Unable to set boolean '%s::%s' - '%s' "
                "not recognised",
                param->props->thorn, param->props->name, value);
  }

  return (retval);
}


void CCTKi_SetParameterSetMask (int mask)
{
  cctk_parameter_set_mask = mask;
}

/*****************************************************************************/

/*#define TEST_PARAMETERS*/
#ifdef TEST_PARAMETERS

/*@@
   @routine    ParameterPrintDescription
   @date       Tue Jun 29 10:24:49 1999
   @author     Tom Goodale
   @desc
   Prints out a description on the given file descriptor with the given
   format.  Should include all data - i.e. ranges, range descriptions,
   range origins, and default value.

   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     name
   @vdesc   The name of the parameter
   @vtype   const char *
   @vio     in
   @vcomment
   name
   @endvar
   @var     thorn
   @vdesc   The originating thorn
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     implementation
   @vdesc   The originating implementation
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     format
   @vdesc   The printf format string
   @vtype   const char *
   @vio     in
   @vcomment
   This is for each line of the test, and should have one %s in it.
   @endvar
   @var     file
   @vdesc   File descriptor
   @vtype   FILE *
   @vio     in
   @vcomment
   The file to pront out on.
   @endvar

   @returntype int
   @returndesc
   1 on success, 0 on failure.
   @endreturndesc

@@*/
int ParameterPrintDescription(const char *name,
                              const char *thorn,
                              const char *format,
                              FILE *file)
{
  int retval;
  t_param *param;

  param = NULL;

  param = ParameterFind(name, thorn, SCOPE_ANY);

  if(param)
  {
    retval = ParameterPrintSimple(param, format, file);
  }
  else
  {
    retval = -1;
  }

  return retval;
}


static int ParameterPrintSimple(t_param *param,
                                const char *format,
                                FILE *file)
{
  t_range *range;

  fprintf(file, format, "Parameter", param->props->name);
  fprintf(file, format, "Thorn", param->props->thorn);
  fprintf(file, format, "Desc", param->props->description);
  fprintf(file, format, "Def", param->props->defval);

  for(range=param->props->range; range; range=range->next)
  {
    fprintf(file, format, "Range:", range->range);
  }

  return 0;
}
struct
{
  int a;
  char *foo;
  double beta;
} params;

int main(void)
{
  CCTKi_ParameterCreate("a", "thorn1", "imp1", "int", "global", 0,
                        "The a param", "2", &(params.a));

  CCTKi_ParameterCreate("foo", "thorn2", "imp2", "keyword", "private", 0,
                        "The foo param", "bingo", &(params.foo));

  printf("Testing thorn,null\n");

  ParameterPrintDescription("a",
                            "thorn1", /*const char *thorn,*/
                            NULL, /* const char *implementation,*/
                            "..%s..%s\n",/*  const char *format,*/
                            stdout);

  printf("Testing null,imp\n");

  ParameterPrintDescription("a",
                            NULL, /*const char *thorn,*/
                            "imp1", /* const char *implementation,*/
                            "..%s..%s\n",/*  const char *format,*/
                            stdout);

  printf("Testing thorn,thorn\n");

  ParameterPrintDescription("a",
                            "thorn1", /*const char *thorn,*/
                            "thorn1", /* const char *implementation,*/
                            "..%s..%s\n",/*  const char *format,*/
                            stdout);

  printf("Testing imp,imp\n");

  ParameterPrintDescription("a",
                            "imp1", /*const char *thorn,*/
                            "imp1", /* const char *implementation,*/
                            "..%s..%s\n",/*  const char *format,*/
                            stdout);

  printf("Testing imp,null\n");
  ParameterPrintDescription("a",
                            "imp1", /*const char *thorn,*/
                            NULL, /* const char *implementation,*/
                            "..%s..%s\n",/*  const char *format,*/
                            stdout);

  printf("Adding a range to a\n");
  ParameterAddRange("imp1",
                    "a",
                    "imp1",
                    "1:7:0",
                    "A nice range for a");

  printf("Adding another range to a\n");
  ParameterAddRange("imp1",
                    "a",
                    "imp1",
                    "1:7:1",
                    "Another nice range for a");

  printf("a is now\n");

  ParameterPrintDescription("a",
                            "thorn1", /*const char *thorn,*/
                            NULL, /* const char *implementation,*/
                            "..%s..%s\n",/*  const char *format,*/
                            stdout);

  return 0;
}

#endif
