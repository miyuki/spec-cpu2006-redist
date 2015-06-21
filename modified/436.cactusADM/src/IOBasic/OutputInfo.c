#ifdef SPEC_CPU
# define THORN_IS_IOBasic
#endif /* SPEC_CPU */
 /*@@
   @file      OutputInfo.c
   @date      June 31 1999
   @author    Gabrielle Allen
   @desc
              Functions to deal with info output of variables
   @enddesc
   @version   $Id: OutputInfo.c,v 1.34 2002/01/18 16:06:47 tradke Exp $
 @@*/

#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "util_String.h"
#include "iobasicGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/IOBasic/src/OutputInfo.c,v 1.34 2002/01/18 16:06:47 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOBasic_OutputInfo_c)

/********************************************************************
 ********************    Macro Definitions    ***********************
 ********************************************************************/
/* uncomment the following to get some debugging output */
/* #define IOBASIC_DEBUG 1 */

/* the number at which to switch from decimal to exponential notation */
#define DECIMAL_PRECISION       1.0e-8
#define DECIMAL_TOOBIG          1.0e+8
#define USE_DECIMAL_NOTATION(x) ((fabs (x) > DECIMAL_PRECISION) ||            \
                                 ((x) == 0.0) ||                              \
                                 (fabs(x) < DECIMAL_TOOBIG))
#define PRINT_FORMATTED_REDUCTION_VALUE(reduction)                            \
          if (reduction->is_valid)                                            \
          {                                                                   \
            if (USE_DECIMAL_NOTATION (reduction->value))                      \
            {                                                                 \
              printf ("%13.8f |", reduction->value);                          \
            }                                                                 \
            else                                                              \
            {                                                                 \
              printf ("%11.6e |", reduction->value);                          \
            }                                                                 \
          }                                                                   \
          else                                                                \
          {                                                                   \
            printf (" ------------ |");                                        \
          }


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static void CheckSteerableParameters (iobasicGH *myGH);
static void AssignReductionList (int vindex, const char *optstring, void *arg);
static void PrintHeader (iobasicGH *myGH, int num_vars);


 /*@@
   @routine    IOBasic_InfoOutputGH
   @date       June 31 1999
   @author     Gabrielle Allen
   @desc
               Loops over all variables and prints output if requested
   @enddesc
   @calls      CheckSteerableParameters
               PrintHeader
               IOBasic_WriteInfo

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the number of variables which were output at this iteration
               (or 0 if it wasn't time to output yet)
   @endreturndesc
@@*/
int IOBasic_InfoOutputGH (const cGH *GH)
{
  int vindex, num_vars, retval;
  iobasic_reduction_t *reduction;
  iobasicGH  *myGH;
  DECLARE_CCTK_PARAMETERS


  /* get the GH extensions for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  /* check if steerable parameters changed */
  CheckSteerableParameters (myGH);

  /* return if no output is required */
  if (myGH->outInfo_every <= 0 || GH->cctk_iteration % myGH->outInfo_every)
  {
    return (0);
  }

  /* print header if neccessary */
  num_vars = CCTK_NumVars ();
  if (myGH->info_reductions_changed)
  {
    PrintHeader (myGH, num_vars);
  }

  /* print the iteration/timestep information for all variables */
  if (USE_DECIMAL_NOTATION (GH->cctk_time))
  {
    printf ("%5d |%9.3f |", GH->cctk_iteration, GH->cctk_time);
  }
  else
  {
    printf ("%5d |%7.3e |", GH->cctk_iteration, GH->cctk_time);
  }

  /* loop over all variables */
  for (vindex = retval = 0; vindex < num_vars; vindex++)
  {
    /* check this variable should be output */
    if (myGH->info_reductions[vindex].num_reductions == 0)
    {
      continue;
    }

    /* check variable not already output this iteration */
    if (myGH->outInfo_last[vindex] != GH->cctk_iteration)
    {
#ifdef IOBASIC_DEBUG
      printf("\nIn IO OutputInfoGH\n----------------\n");
      printf("  Index = %d\n", vindex);
      printf("  Variable = -%s-\n", CCTK_VarName (vindex);
      printf("  Last output iteration was = %d\n", myGH->outInfo_last[vindex]);
#endif

      /* get the data to output */
      if (IOBasic_WriteInfo (GH, vindex) == 0)
      {
        /* register variable as having info output this iteration */
        myGH->outInfo_last[vindex] = GH->cctk_iteration;
        retval++;
      }
    }

    /* finally print the stuff to screen */
    reduction = myGH->info_reductions[vindex].reductions;
    while (reduction)
    {
      PRINT_FORMATTED_REDUCTION_VALUE (reduction);
      reduction = reduction->next;
    }
  } /* end of loop over all variables */

  /* add the new line */
  putchar ('\n');
  fflush (stdout);

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    IOBasic_TimeForInfoOutput
   @date       June 31 1999
   @author     Gabrielle Allen
   @desc
               Decides if it is time to output a variable using info output
   @enddesc
   @calls      CheckSteerableParameters

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        vindex
   @vdesc      index of variable to check for output
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               true/false (1 or 0) if this variable should be output or not
   @endreturndesc
@@*/
int IOBasic_TimeForInfoOutput (const cGH *GH, int vindex)
{
  int retval;
  char *fullname;
  iobasicGH *myGH;
  DECLARE_CCTK_PARAMETERS


  /* get the GH extensions for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  /* check if steerable parameters changed */
  CheckSteerableParameters (myGH);

  /* return if no output requested */
  if (myGH->outInfo_every <= 0 || GH->cctk_iteration % myGH->outInfo_every ||
      myGH->info_reductions[vindex].num_reductions == 0)
  {
    retval = 0;
  }
  else
  {
    /* check if not already output this iteration */
    retval = myGH->outInfo_last[vindex] != GH->cctk_iteration;
    if (! retval)
    {
      fullname = CCTK_FullName (vindex);
      CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Already done Info output for '%s' in current "
                  "iteration (probably via triggers)", fullname);
      free (fullname);
    }
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    IOBasic_TriggerInfoOutput
   @date       June 31 1999
   @author     Gabrielle Allen
   @desc
               Triggers the output of a variable using IOBasic's info output
   @enddesc
   @calls      IOBasic_WriteInfo

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        vindex
   @vdesc      index of variable to output
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine IOBasic_WriteInfo
   @endreturndesc
@@*/
int IOBasic_TriggerInfoOutput (const cGH *GH, int vindex)
{
  int retval;
  iobasicGH *myGH;
  DECLARE_CCTK_PARAMETERS


  /* get the GH extension for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

#ifdef IOBASIC_DEBUG
  printf ("\nIn IO TriggerOutputInfo\n---------------------\n");
  printf ("  Index = %d\n", vindex);
  printf ("  Variable = -%s-\n", CCTK_VarName (vindex);
#endif

  /* get the data values to output */
  retval = IOBasic_WriteInfo (GH, vindex);

  if (retval == 0)
  {
    /* gegister variable as having Info output at this iteration */
    myGH->outInfo_last[vindex] = GH->cctk_iteration;
  }

  USE_CCTK_PARAMETERS;   return (retval);
}


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
 /*@@
   @routine    CheckSteerableParameters
   @date       Tue 31 Jul 2001
   @author     Thomas Radke
   @desc
               Re-evaluates 'IOBasic::outInfo_every' and/or 'IO::out_every'
               resp. to set myGH->outInfo_every to the frequency of info output.
               Re-evaluates 'IOBasic::outInfo_vars' and
               'IOBasic::outInfo_reductions' and sets
               myGH->info_reductions_changed to true if one of them was changed.
   @enddesc
   @calls      CCTK_ParameterQueryTimesSet

   @var        myGH
   @vdesc      Pointer to IOBasic's GH extension
   @vtype      iobasicGH *
   @vio        in
   @endvar
@@*/
static void CheckSteerableParameters (iobasicGH *myGH)
{
  DECLARE_CCTK_PARAMETERS
  int vindex, out_old, times_set, update_info_reductions_list;
  iobasic_reduction_t *reduction, *next;
  static int outInfo_vars_lastset       = -1;
  static int outInfo_reductions_lastset = -1;


  /* how often to output */
  out_old = myGH->outInfo_every;
  myGH->outInfo_every  = out_every > 0 ? out_every : -1;
  if (outInfo_every > 0)
  {
    myGH->outInfo_every = outInfo_every;
  }
  if (myGH->outInfo_every != out_old)
  {
    if (CCTK_Equals (newverbose, "standard") ||
        CCTK_Equals (newverbose, "full"))
    {
      CCTK_VInfo (CCTK_THORNSTRING, "Info: Output every %d iterations",
                  myGH->outInfo_every);
    }
  }

  /* return if there's nothing to do */
  if (myGH->outInfo_every <= 0)
  {
    return;
  }

  /* check if the 'outInfo_reductions' parameter if it was changed */
  times_set = CCTK_ParameterQueryTimesSet ("outInfo_reductions",
                                           CCTK_THORNSTRING);
  update_info_reductions_list = times_set != outInfo_reductions_lastset;
  outInfo_reductions_lastset = times_set;

  /* check if the 'outInfo_vars' parameter if it was changed */
  times_set = CCTK_ParameterQueryTimesSet ("outInfo_vars", CCTK_THORNSTRING);
  update_info_reductions_list |= times_set != outInfo_vars_lastset;
  outInfo_vars_lastset = times_set;

  if (update_info_reductions_list)
  {
    /* free old info output lists ... */
    for (vindex = CCTK_NumVars ()-1; vindex >= 0; vindex--)
    {
      if (myGH->info_reductions[vindex].num_reductions > 0)
      {
        myGH->info_reductions[vindex].num_reductions = 0;
        reduction = myGH->info_reductions[vindex].reductions;
        while (reduction)
        {
          next = reduction->next;
          free (reduction->name);
          free (reduction);
          reduction = next;
        }
      }
    }

    /* ... and create new ones */
    if (CCTK_TraverseString (outInfo_vars, AssignReductionList, myGH,
                             CCTK_GROUP_OR_VAR) < 0)
    {
      CCTK_WARN (1, "Failed to parse 'IOBasic::outInfo_vars' parameter");
    }

    myGH->info_reductions_changed = 1;
  }
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    AssignReductionList
   @date       Tue 31 Jul 2001
   @author     Thomas Radke
   @desc
               Callback routine called by CCTK_TraverseString() to set the
               info output for a given variable.
               For CCTK_GF and CCTK_ARRAY variables, it builds a chained list
               of reduction operators according to the settings of 'optstring'
               or 'IOBasic::outInfo_reductions'.
   @enddesc
   @calls      CCTK_GroupTypeFromVarI
               CCTK_ReductionHandle

   @var        vindex
   @vdesc      index of the variable to set info output
   @vtype      int
   @vio        in
   @endvar
   @var        optstring
   @vdesc      option string for this variable
   @vtype      const char *
   @vio        in
   @endvar
   @var        arg
   @vdesc      user-supplied argument to callback routine (IOBasic GH extension)
   @vtype      void *
   @vio        in
   @endvar
@@*/
static void AssignReductionList (int vindex, const char *optstring, void *arg)
{
  DECLARE_CCTK_PARAMETERS
  iobasicGH *myGH = (iobasicGH *) arg;
  const char *string_start, *string_end;
  char *reduction_op, *reduction_op_list;
  int reduction_handle;
  iobasic_reductionlist_t *list;
  iobasic_reduction_t **new_reduction;


  list = &myGH->info_reductions[vindex];

  if (CCTK_GroupTypeFromVarI (vindex) == CCTK_SCALAR)
  {
    if (optstring)
    {
      CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "option list '%s' for variable '%s' ignored",
                  optstring, CCTK_VarName (vindex));
    }

    list->reductions = (iobasic_reduction_t *)
                       malloc (sizeof (iobasic_reduction_t));
    if (strncmp (CCTK_VarTypeName (CCTK_VarTypeI (vindex)),
                 "CCTK_VARIABLE_COMPLEX", 21))
    {
      list->num_reductions = 1;
      list->reductions->name = strdup ("scalar value");
      list->reductions->next = NULL;
    }
    else
    {
      list->num_reductions = 2;
      list->reductions->name = strdup ("real part");
      list->reductions->next = (iobasic_reduction_t *)
                               malloc (sizeof (iobasic_reduction_t));
      list->reductions->next->name = strdup ("imag part");
      list->reductions->next->next = NULL;
    }

#ifdef IOBASIC_DEBUG
    printf ("Set info scalar output for variable '%s'\n", CCTK_VarName(vindex));
#endif

    return;
  }

  /* initialize to empty list */
  list->num_reductions = 0;
  list->reductions = NULL;

  if (optstring)
  {
    if (strncmp (optstring, "reductions=<", 12) == 0 &&
        optstring[strlen (optstring) - 1] == '>')
    {
      reduction_op_list = strdup (optstring + 12);
      reduction_op_list[strlen (reduction_op_list) - 1] = 0;
    }
    else
    {
      CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "AssignReductionList: invalid syntax for option list '%s'",
                  optstring);
      return;
    }
  }
  else
  {
    reduction_op_list = strdup (outInfo_reductions);
  }

  /* now loop over all reduction operators */
  string_start = reduction_op_list;
  reduction_op = (char *) malloc (strlen (string_start) + 1);
  while (string_start && *string_start)
  {
    /* skip leading spaces */
    while (isspace ((int) *string_start))
    {
      string_start++;
    }
    if (! *string_start)
    {
      break;
    }

    /* advance to end of the operator string */
    string_end = string_start + 1;
    while (*string_end && ! isspace ((int) *string_end))
    {
      string_end++;
    }

    /* copy the operator string */
    strncpy (reduction_op, string_start, string_end - string_start);
    reduction_op[string_end - string_start] = 0;
    string_start = string_end;

    /* get the reduction handle from the reduction operator */
    reduction_handle = CCTK_ReductionHandle (reduction_op);
    if (reduction_handle < 0)
    {
      CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "AssignReductionList: Invalid reduction operator '%s'",
                  reduction_op);
      continue;
    }

    /* add new reduction to end of list */
    new_reduction = &list->reductions;
    while (*new_reduction)
    {
      if (strcmp ((*new_reduction)->name, reduction_op) == 0)
      {
        new_reduction = NULL;
        break;
      }
      new_reduction = &((*new_reduction)->next);
    }
    if (new_reduction == NULL)
    {
      CCTK_VWarn (3, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "AssignReductionList: Duplicate reduction operator '%s' will "
                  "be ignored", reduction_op);
      continue;
    }

    *new_reduction = (iobasic_reduction_t *) malloc (sizeof (iobasic_reduction_t));
    (*new_reduction)->handle = reduction_handle;
    (*new_reduction)->name   = strdup (reduction_op);
    (*new_reduction)->next   = NULL;
    list->num_reductions++;

#ifdef IOBASIC_DEBUG
    printf ("Set info reduction output '%s' for variable '%s'\n",
            (*new_reduction)->name, CCTK_VarName (vindex));
#endif
  }

  free (reduction_op_list);
  free (reduction_op);
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    PrintHeader
   @date       Tue 31 Jul 2001
   @author     Thomas Radke
   @desc
               It re-evaluates the info reduction lists of all CCTK variables
               and prints the header for the info output table.
   @enddesc
   @calls      CCTK_TraverseString

   @var        myGH
   @vdesc      Pointer to IOBasic's GH extension
   @vtype      iobasicGH *
   @vio        in
   @endvar
   @var        num_vars
   @vdesc      total number of CCTK variables
   @vtype      int
   @vio        in
   @endvar
@@*/
static void PrintHeader (iobasicGH *myGH, int num_vars)
{
  DECLARE_CCTK_PARAMETERS
  int i, num_columns, vindex;
  char *fullname, *msg, *oldmsg;
  iobasic_reduction_t *reduction;


  /* count number of info values to output */
  num_columns = 0;
  msg = NULL;
  for (vindex = 0; vindex < num_vars; vindex++)
  {
    num_columns += myGH->info_reductions[vindex].num_reductions;

    if (myGH->info_reductions[vindex].num_reductions > 0 &&
        (CCTK_Equals (newverbose, "standard") ||
         CCTK_Equals (newverbose, "full")))
    {
      fullname = CCTK_FullName (vindex);
      if (! msg)
      {
        Util_asprintf (&msg, "Info: Output requested for %s", fullname);
      }
      else
      {
        oldmsg = msg;
        Util_asprintf (&msg, "%s %s", oldmsg, fullname);
        free (oldmsg);
      }
      free (fullname);
    }
  }
  if (msg)
  {
    CCTK_INFO (msg);
    free (msg);
  }

  /* draw a horizontal delimiter line */
  printf ("------------------");
  for (i = 0; i < num_columns; i++)
  {
    printf ("---------------");
  }
  putchar ('\n');

  /* the first header line displays the iteration number (first column)
     and the full names of the variables to output (third column ff.) */
  printf ("  it  |          |");
  if (num_columns > 0)
  {
    for (vindex = 0; vindex < num_vars; vindex++)
    {
      i = myGH->info_reductions[vindex].num_reductions;
      if (i > 0)
      {
        fullname = CCTK_FullName (vindex);
        if (strlen (fullname) > (unsigned int) 15*i - 3)
        {
          printf (" *%s |", fullname + strlen (fullname) - (15*i - 4));
        }
        else
        {
          printf (" %-*s |", 15*i - 3, fullname);
        }
        free (fullname);
      }
    }
  }
  putchar ('\n');

  /* the second header line displays the physical time (second column)
     and the names of the reduction operators to apply (third column ff.) */
  printf ("      |    t     ");
  for (vindex = 0; vindex < num_vars; vindex++)
  {
    if (myGH->info_reductions[vindex].num_reductions > 0)
    {
      reduction = myGH->info_reductions[vindex].reductions;
      while (reduction)
      {
        printf ("| %-12s ", reduction->name);
        reduction = reduction->next;
      }
    }
  }
  printf ("|\n");

  /* finally draw another horizontal delimiter line */
  printf ("------------------");
  for (i = 0; i < num_columns; i++)
  {
    printf ("---------------");
  }
  putchar ('\n');
  fflush (stdout);

  myGH->info_reductions_changed = 0;
  USE_CCTK_PARAMETERS; }
