#ifdef SPEC_CPU
# define THORN_IS_IOASCII
#endif /* SPEC_CPU */
 /*@@
   @file      Output2D.c
   @date      Thu May 11 2000
   @author    Thomas Radke
   @desc
              Functions to deal with 2D ASCII output of variables
   @enddesc
   @version   $Id: Output2D.c,v 1.12 2001/12/28 21:22:44 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "util_String.h"
#include "ioASCIIGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Id: Output2D.c,v 1.12 2001/12/28 21:22:44 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOASCII_Output2D_c)

/* define this if you want debug output */
/*#define IOASCII_DEBUG 1*/


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static int CheckOutputVar (int vindex);
static void CheckSteerableParameters (asciiioGH *myGH);
static void SetOutputFlag (int vindex, const char *optstring, void *arg);


/*@@
   @routine    IOASCII_Output2DGH
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Loops over all variables and outputs them if necessary
   @enddesc
   @calls      IOASCII_TimeFor2D
               IOASCII_Write2D

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
int IOASCII_Output2DGH (const cGH *GH)
{
  int vindex, retval;
  asciiioGH *myGH;
  const char *name;


  /* Get the GH extensions for IOASCII */
  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  CheckSteerableParameters (myGH);

  /* Return if no output is required */
  if (myGH->out2D_every <= 0)
  {
    return (0);
  }

  /* Loop over all variables */
  for (vindex = retval = 0; vindex < CCTK_NumVars (); vindex++)
  {

    /* Check if this variable should be output */
    if (! IOASCII_TimeFor2D (GH, vindex))
    {
       continue;
    }

    /* Get the variable name for this index (for filename) */
    name = CCTK_VarName (vindex);

#ifdef IO_DEBUG
    printf ("\nIn IOASCII Output2DGH\n---------------------\n");
    printf ("  Index = %d\n", vindex);
    printf ("  Variable = -%s-\n", name);
    printf ("  On iteration %d\n", GH->cctk_iteration);
    printf ("  Last output iteration was = %d\n", myGH->out2D_last[vindex]);
#endif

    /* Make the IO call */
    if (IOASCII_Write2D (GH, vindex, name) == 0)
    {
      /* Register GF as having 2D output this iteration */
      myGH->out2D_last[vindex] = GH->cctk_iteration;
      retval++;
    }
  } /* end of loop over all variables */

  return (retval);
}


/*@@
   @routine    IOASCII_Output2DVarAs
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Unconditional output of a variable
               using the IOASCII 2D output method
   @enddesc
   @calls      IOASCII_Write2D

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        fullname
   @vdesc      complete name of variable to output
   @vtype      const char *
   @vio        in
   @endvar
   @var        alias
   @vdesc      alias name of variable to output
               (used to generate output filename)
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine IOASCII_Write2D, or<BR>
               -1 if variable cannot be output by IOASCII_2D
   @endreturndesc
@@*/
int IOASCII_Output2DVarAs (const cGH *GH, const char *fullname, const char *alias)
{
  int vindex, retval;


  vindex = CCTK_VarIndex (fullname);

#ifdef IO_DEBUG
  printf ("\nIn IOASCII Output2DVarAs\n-------------------\n");
  printf ("  Fullname = -%s-\n", fullname);
  printf ("  Alias = -%s-\n", alias);
  printf ("  Index = %d\n", vindex);
#endif

  retval = -1;
  if (CheckOutputVar (vindex) == 0)
  {
    retval = IOASCII_Write2D (GH, vindex, alias);
  }

  return (retval);
}


 /*@@
   @routine    IOASCII_TimeFor2D
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Decides if it is time to output a variable
               using the IOASCII 2D output method
   @enddesc

   @var        GH
   @vdesc      Pointer to CCTK GH
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        vindex
   @vdesc      index of variable
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               1 if output should take place at this iteration, or<BR>
               0 if not
   @endreturndesc
@@*/
int IOASCII_TimeFor2D (const cGH *GH, int vindex)
{
  int return_type;
  asciiioGH *myGH;
  char *fullname;


  /* Default is do not do output */
  return_type = 0;

  /* Get the GH extensions for IOASCII */
  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  CheckSteerableParameters (myGH);

  /* return if no output requested */
  if (myGH->out2D_every <= 0)
  {
    return (0);
  }

  /* Check if this variable should be output */
  if (myGH->do_out2D[vindex] && (GH->cctk_iteration % myGH->out2D_every == 0))
  {
    /* Check if variable wasn't already output this iteration */
    if (myGH->out2D_last[vindex] == GH->cctk_iteration)
    {
      fullname = CCTK_FullName (vindex);
      CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Already done IOASCII 2D output for '%s' in current "
                  "iteration (probably via triggers)", fullname);
      free (fullname);
    }
    else
    {
      return_type = 1;
    }
  }

  return (return_type);
}


/*@@
   @routine    IOASCII_TriggerOutput2D
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Triggers the output of a variable
               using the IOASCII 2D output method
   @enddesc
   @calls      IOASCII_Write2D

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
               return code of @seeroutine IOASCII_Write2D
   @endreturndesc
@@*/
int IOASCII_TriggerOutput2D (const cGH *GH, int vindex)
{
  int retval;
  const char *var;
  asciiioGH *myGH;


  /* get GH extensions for IOASCII */
  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  var = CCTK_VarName (vindex);

  /* Do the 2D output */
#ifdef IO_DEBUG
  printf ("\nIn IOASCII TriggerOutput2D\n---------------------\n");
  printf ("  Index = %d\n", vindex);
  printf ("  Variable = -%s-\n", var);
#endif

  retval = IOASCII_Write2D (GH, vindex, var);

  if (retval == 0)
  {
    /* Register variables as having 2D output this iteration */
    myGH->out2D_last[vindex] = GH->cctk_iteration;
  }

  return (retval);
}


/**************************** local functions ******************************/
/* check if steerable parameters have changed */
static void CheckSteerableParameters (asciiioGH *myGH)
{
  int i, out_old, times_set, num_vars;
  char *fullname, *msg;
  static int out2D_vars_lastset = -1;
  DECLARE_CCTK_PARAMETERS


  out_old = myGH->out2D_every;

  /* How often to output */
  myGH->out2D_every = out_every > 0 ? out_every : -1;
  if (out2D_every > 0)
  {
    myGH->out2D_every = out2D_every;
  }

  /* Report if frequency changed */

  if (myGH->out2D_every != out_old)
  {
    if (CCTK_Equals (newverbose, "standard") ||
        CCTK_Equals (newverbose, "full"))
    {
      CCTK_VInfo (CCTK_THORNSTRING, "IOASCII_2D: Output every %d iterations",
                  myGH->out2D_every);
    }
  }

  /* re-parse the 'out2D_vars' parameter if it was changed */
  times_set = CCTK_ParameterQueryTimesSet ("out2D_vars", CCTK_THORNSTRING);
  if (times_set != out2D_vars_lastset)
  {
    num_vars = CCTK_NumVars ();
    memset (myGH->do_out2D, 0, num_vars);
    CCTK_TraverseString (out2D_vars, SetOutputFlag, myGH->do_out2D,
                         CCTK_GROUP_OR_VAR);

    if (CCTK_Equals (newverbose, "standard") ||
        CCTK_Equals (newverbose, "full"))
    {
      msg = NULL;
      for (i = 0; i < num_vars; i++)
      {
        if (myGH->do_out2D[i])
        {
          fullname = CCTK_FullName (i);
          if (! msg)
          {
            Util_asprintf (&msg, "IOASCII_2D: Output requested for %s",
                           fullname);
          }
          else
          {
            Util_asprintf (&msg, "%s %s", msg, fullname);
          }
          free (fullname);
        }
      }
      if (msg)
      {
        CCTK_INFO (msg);
        free (msg);
      }
    }

    /* Save the last setting of 'out2D_vars' parameter */
    out2D_vars_lastset = times_set;
  }

  USE_CCTK_PARAMETERS; }


/* check if this variable can be output (static conditions) */
static int CheckOutputVar (int vindex)
{
  int groupindex;
  cGroup groupinfo;
  char *fullname;
  const char *errormsg;


  /* get the variable group information */
  groupindex = CCTK_GroupIndexFromVarI (vindex);
  CCTK_GroupData (groupindex, &groupinfo);

  /* check if variable is grid array type with either 2 or 3 dimensions */
  errormsg = NULL;
  if (groupinfo.dim < 2 || groupinfo.dim > 3)
  {
    errormsg = "No IOASCII 2D output for '%s' (dim != [2,3])";
  }
  else if (groupinfo.grouptype != CCTK_GF && groupinfo.grouptype != CCTK_ARRAY)
  {
    errormsg = "No IOASCII 2D output for '%s' (not a grid function or array)";
  }

  if (errormsg)
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING, errormsg, fullname);
    free (fullname);
  }

  return (errormsg != NULL);
}


/* callback for CCTK_TraverseString() to set the output flag
   for the given variable */
static void SetOutputFlag (int vindex, const char *optstring, void *arg)
{
  char *flags = (char *) arg;


  /* Check the variable type */
  if (CheckOutputVar (vindex) == 0)
  {
    flags[vindex] = 1;
  }

  if (optstring)
  {
    CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Optional string '%s' in variable name ignored", optstring);
  }
}
