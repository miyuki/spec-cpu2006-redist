#ifdef SPEC_CPU
# define THORN_IS_IOBasic
#endif /* SPEC_CPU */
 /*@@
   @file      Output.c
   @date      Mon 21 September
   @author    Gabrielle Allen
   @desc
              Functions to deal with scalar output of grid variables
   @enddesc
 @@*/

/* #define IOBASIC_DEBUG 1 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "util_String.h"
#include "iobasicGH.h"

static const char *rcsid = "$Header: /cactus/CactusBase/IOBasic/src/OutputScalar.c,v 1.7 2002/01/18 16:06:47 tradke Exp $";

CCTK_FILEVERSION(CactusBase_IOBasic_OutputScalar_c)


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static void CheckSteerableParameters (iobasicGH *myGH);
static void SetOutputFlag (int vindex, const char *optstring, void *arg);


/*@@
   @routine    IOBasic_ScalarOutputGH
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Loops over all variables and outputs them if necessary
   @enddesc
   @calls      IOBasic_TimeForScalarOutput
               IOBasic_WriteScalarGS
               IOBasic_WriteScalarGA

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
int IOBasic_ScalarOutputGH (const cGH *GH)
{
  int i, vindex, retval;
  const char *name;
  iobasicGH *myGH;


  /* Get the GH extensions for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  CheckSteerableParameters (myGH);

  /* Return if no output is required */
  if (myGH->outScalar_every <= 0)
  {
    return (0);
  }

  /* Loop over all variables */
  for (vindex = retval = 0; vindex < CCTK_NumVars (); vindex++)
  {
    /* Is it time for output ? */
    if (! IOBasic_TimeForScalarOutput (GH, vindex))
    {
      continue;
    }

    /* Get the variable name for this index (for filename) */
    name = CCTK_VarName (vindex);

#ifdef IOBASIC_DEBUG
    printf("\nIn IOBasic_ScalarOutputGH\n----------------\n");
    printf("  Index = %d\n",vindex);
    printf("  Variable = -%s-\n",name);
    printf("  Last output iteration was = %d\n",myGH->outScalar_last[vindex]);
#endif

    /* Make the IO call */
    if (CCTK_GroupTypeFromVarI (vindex) == CCTK_SCALAR)
    {
      i = IOBasic_WriteScalarGS (GH, vindex, name);
    }
    else
    {
      i = IOBasic_WriteScalarGA (GH, vindex, name);
    }
    if (i == 0)
    {
      /* Register GF as having 0D output this iteration */
      myGH->outScalar_last [vindex] = GH->cctk_iteration;
      retval++;
    }
  } /* end of loop over all variables */

  return (retval);
}


/*@@
   @routine    IOBasic_ScalarOutputVarAs
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Unconditional output of a variable using IOBasic's SCALAR
               output method
   @enddesc
   @calls      IOBasic_WriteScalarGS
               IOBasic_WriteScalarGA

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
   @vdesc      alias name of variable to output (used to generate output filename)
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine IOBasic_WriteScalarGS or
               @seeroutine IOBasic_WriteScalarGA
   @endreturndesc
@@*/
int IOBasic_ScalarOutputVarAs (const cGH *GH, const char *fullname, const char *alias)
{
  int vindex, retval;


  vindex = CCTK_VarIndex (fullname);

#ifdef IOBASIC_DEBUG
  printf("\nIn IOBasic_ScalarOutputVarAs\n-------------------\n");
  printf("  Fullname = -%s-\n",fullname);
  printf("  Alias = -%s-\n",alias);
  printf("  Index = %d\n",vindex);
#endif

  if (CCTK_GroupTypeFromVarI (vindex) == CCTK_SCALAR)
  {
    retval = IOBasic_WriteScalarGS (GH, vindex, alias);
  }
  else
  {
    retval = IOBasic_WriteScalarGA (GH, vindex, alias);
  }

  return (retval);
}


 /*@@
   @routine    IOBasic_TimeForScalarOutput
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Decides if it is time to do Scalar output.
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
int IOBasic_TimeForScalarOutput (const cGH *GH, int vindex)
{
  int return_type;
  iobasicGH *myGH;
  char *fullname;


  /* Default is do not do output */
  return_type = 0;

  /* Get the GH extension for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  CheckSteerableParameters (myGH);

  /* check if any output was requested */
  if (myGH->outScalar_every <= 0)
  {
    return (0);
  }

  /* Check if this variable should be output */
  if (myGH->do_outScalar [vindex] &&
      (GH->cctk_iteration % myGH->outScalar_every) == 0)
  {
    /* Check if variable wasn't already output this iteration */
    if (myGH->outScalar_last [vindex] == GH->cctk_iteration)
    {
      fullname = CCTK_FullName (vindex);
      CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Already done IOBasic scalar output for '%s' in "
                  "current iteration (probably via triggers)", fullname);
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
   @routine    IOBasic_TriggerScalarOutput
   @date       Sat March 6 1999
   @author     Gabrielle Allen
   @desc
               Triggers the output of a variable using IOBasic's Scalar
               output method
   @enddesc

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
               return code of @seeroutine IOBasic_WriteScalarGS or
               @seeroutine IOBasic_WriteScalarGA
   @endreturndesc
@@*/
int IOBasic_TriggerScalarOutput (const cGH *GH, int vindex)
{
  int retval;
  const char *name;
  iobasicGH *myGH;


  /* Get the GH extension for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  name = CCTK_VarName (vindex);

#ifdef IOBASIC_DEBUG
  printf ("\nIn IOBasic_TriggerScalarOutput\n---------------------\n");
  printf ("  Index = %d\n", vindex);
  printf ("  Variable = -%s-\n", name);
#endif

  /* Do the Scalar output */
  if (CCTK_GroupTypeFromVarI (vindex) == CCTK_SCALAR)
  {
    retval = IOBasic_WriteScalarGS (GH, vindex, name);
  }
  else
  {
    retval = IOBasic_WriteScalarGA (GH, vindex, name);
  }

  if (retval == 0)
  {
    /* Register variable as having Scalar output this iteration */
    myGH->outScalar_last [vindex] = GH->cctk_iteration;
  }

  return (retval);
}


/**************************** local functions ******************************/
/* check if steerable parameters have changed */
static void CheckSteerableParameters (iobasicGH *myGH)
{
  int i, num_vars, out_old;
  int times_set;
  char *fullname, *msg, *oldmsg;
  static int outScalar_vars_lastset = -1;
  DECLARE_CCTK_PARAMETERS


  /* How often to output */
  out_old = myGH->outScalar_every;
  myGH->outScalar_every = out_every > 0 ? out_every : -1;
  if (outScalar_every > 0)
  {
    myGH->outScalar_every = outScalar_every;
  }

  if (myGH->outScalar_every != out_old)
  {
    if (CCTK_Equals (newverbose, "standard") ||
        CCTK_Equals (newverbose, "full"))
    {
      CCTK_VInfo (CCTK_THORNSTRING, "Scalar: Output every %d iterations",
                  myGH->outScalar_every);
    }
  }

  /* re-parse the 'outScalar_vars' parameter if it was changed */
  times_set = CCTK_ParameterQueryTimesSet ("outScalar_vars", CCTK_THORNSTRING);
  if (times_set != outScalar_vars_lastset)
  {
    num_vars = CCTK_NumVars ();
    memset (myGH->do_outScalar, 0, num_vars);
    CCTK_TraverseString (outScalar_vars, SetOutputFlag, myGH->do_outScalar,
                         CCTK_GROUP_OR_VAR);

    if (myGH->outScalar_every &&
        (CCTK_Equals (newverbose, "standard") ||
         CCTK_Equals (newverbose, "full")))
    {
      msg = NULL;
      for (i = 0; i < num_vars; i++)
      {
        if (myGH->do_outScalar[i])
        {
          fullname = CCTK_FullName (i);
          if (! msg)
          {
            Util_asprintf (&msg, "Scalar: Output requested for %s", fullname);
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
    }

    /* Save the last setting of 'outScalar_vars' parameter */
    outScalar_vars_lastset = times_set;
  }

  USE_CCTK_PARAMETERS; }


/* callback for CCTK_TraverseString() to set the output flag
   for the given variable */
static void SetOutputFlag (int vindex, const char *optstring, void *arg)
{
  char *flags = (char *) arg;


  flags[vindex] = 1;

  if (optstring)
  {
    CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Optional string '%s' in variable name ignored", optstring);
  }
}
