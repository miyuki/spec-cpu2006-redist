#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CommandLine.c
   @date      Wed Feb 17 00:11:26 1999
   @author    Tom Goodale
   @desc
              Routines to deal with command line arguments.
   @enddesc
   @version   $Id: CommandLine.c,v 1.47 2002/01/06 12:24:41 allen Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "CommandLine.h"
#include "cctk_Flesh.h"
#include "cGH.h"

#include "cctk_Version.h"
#include "cctk_ActiveThorns.h"
#include "cctk_Comm.h"
#include "cctk_Misc.h"
#include "cctk_ParamCheck.h"
#include "cctk_WarnLevel.h"

#include "cctki_ActiveThorns.h"
#include "cctki_WarnLevel.h"


#define NEED_PARAMETER_SCOPE_STRINGS
#define NEED_PARAMETER_TYPE_STRINGS

#include "cctk_Parameter.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/CommandLine.c,v 1.47 2002/01/06 12:24:41 allen Exp $";

CCTK_FILEVERSION(main_CommandLine_c)

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static void CommandLinePrintParameter (const cParamData *properties);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/* FIXME: these should be put in a header somewhere */

int CCTK_CommandLine (char ***outargv);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static int redirectsubs = 0;
static int paramchecking = 0;

/********************************************************************
 *********************     Global Data   *****************************
 ********************************************************************/

int cctki_paramchecking;
int cctki_paramcheck_nprocs;

/********************************************************************
 *********************        Defines          **********************
 ********************************************************************/

#define CACTUS_COMMANDLINE_OPTIONS                                            \
        "[-h] [-O] [-o paramname] [-x [nprocs]] [-W n] [-E n] [-r] [-T] "     \
        "[-t name] [-parameter-level <level>] [-v] <parameter_file_name>"

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_CommandLineTestThorncompiled
   @date       Wed Feb 17 10:25:30 1999
   @author     Gabrielle Allen
   @desc
               Tests if a given thorn has been compiled.
               At the moment the given thorn must be in the format
               <arrangement name>/<thorn name>
   @enddesc
   @calls      CCTK_IsThornCompiled

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineTestThornCompiled (const char *argument)
{
  int retval;


  retval = CCTK_IsThornCompiled (argument);
  if (CCTK_MyProc (NULL) == 0)
  {
    printf ("Thorn '%s' %savailable.\n", argument, retval ? "" : "un");
  }

  CCTK_Exit (NULL, retval);
}


 /*@@
   @routine    CCTKi_CommandLineDescribeAllParameters
   @date       Tue Apr 18 15:00:12 2000
   @author     Tom Goodale
   @desc
               Describe all the parameters
   @enddesc
   @calls      CCTK_NumCompiledThorns
               CCTK_CompiledThorn
               CCTK_ParameterWalk
               CommandLinePrintParameter

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineDescribeAllParameters (const char *argument)
{
  int first, n_thorns, thorn;
  char *param;
  const char *thornname;
  const cParamData *properties;


  if (CCTK_MyProc (NULL) == 0)
  {
    n_thorns = CCTK_NumCompiledThorns ();

    for (thorn = 0; thorn < n_thorns; thorn++)
    {
      thornname = CCTK_CompiledThorn (thorn);
      printf ("\nParameters of thorn '%s' providing implementation '%s':\n",
              thornname, CCTK_ThornImplementation (thornname));

      first = 1;
      while (CCTK_ParameterWalk (first, thornname, &param, &properties) == 0)
      {
        if (argument)
        {
          switch (*argument)
          {
            case 'v':
              CommandLinePrintParameter (properties);
              break;
            default :
              fprintf (stderr, "Unknown verbosity option %s\n", argument);
              CCTK_Exit (NULL, 2);
          }
        }
        else
        {
          printf ("%s\n", param);
        }

        free (param);
        first = 0;
      }
    }
  }

  CCTK_Exit (NULL, 0);
}


 /*@@
   @routine    CCTKi_CommandLineDescribeParameter
   @date       Tue Apr 18 15:00:33 2000
   @author     Tom Goodale
   @desc
               Describe a particular parameter.
   @enddesc
   @calls      Util_SplitString
               CCTK_ParameterData
               CCTK_ImplementationThorn
               CommandLinePrintParameter

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineDescribeParameter (const char *argument)
{
  char *thorn, *param;
  const char *cthorn;
  const cParamData *properties;


  if (CCTK_MyProc (NULL) == 0)
  {
    Util_SplitString (&thorn, &param, argument, "::");

    if (! param)
    {
      properties = CCTK_ParameterData (argument, NULL);
    }
    else
    {
      properties = CCTK_ParameterData (param, thorn);
      if (! properties)
      {
        cthorn = CCTK_ImplementationThorn (thorn);
        properties = CCTK_ParameterData (param, cthorn);
      }

      free (thorn);
      free (param);
    }

    CommandLinePrintParameter (properties);
  }

  CCTK_Exit (NULL, 0);
}


 /*@@
   @routine    CCTKi_CommandLineTestParameters
   @date       Tue Apr 18 15:00:45 2000
   @author     Tom Goodale
   @desc

   @enddesc
   @calls      CCTKi_CommandLineUsage

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineTestParameters (const char *argument)
{
  int nprocs;
  char *endptr;


  if (argument == NULL)
  {
    nprocs = 1;
  }
  else
  {
    nprocs = strtol (argument, &endptr, 10);
    if (! (endptr && *endptr == 0))
    {
      CCTKi_CommandLineUsage ();
    }
  }

  paramchecking = 1;
  cctki_paramchecking = 1;
  cctki_paramcheck_nprocs = nprocs;
}


/*@@
   @routine    CCTKi_CommandLineWarningLevel
   @date       Wed Feb 17 00:58:56 1999
   @author     Tom Goodale
   @desc
               Sets the CCTK warning level from a command line argument.
   @enddesc
   @calls      CCTKi_SetWarningLevel
               CCTKi_CommandLineUsage

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineWarningLevel (const char *argument)
{
  char *endptr;
  long int warninglevel;


  warninglevel = strtol (argument, &endptr, 10);
  if (endptr && *endptr == 0)
  {
    CCTKi_SetWarnLevel (warninglevel);
  }
  else
  {
    CCTKi_CommandLineUsage ();
  }
}

 /*@@
   @routine    CCTKi_CommandLineErrorLevel
   @date       Wed Feb 17 00:58:56 1999
   @author     Tom Goodale
   @desc
               Sets the CCTK error level from a command line argument.
   @enddesc
   @calls      CCTKi_SetErrorLevel
               CCTKi_CommandLineUsage

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineErrorLevel (const char *argument)
{
  char *endptr;
  long int errorlevel;


  errorlevel = strtol (argument, &endptr, 10);
  if (endptr && *endptr == 0)
  {
    CCTKi_SetErrorLevel (errorlevel);
  }
  else
  {
    CCTKi_CommandLineUsage ();
  }
}


 /*@@
   @routine    CCTKi_CommandLineParameterLevel
   @date       Wed Feb 21 2001
   @author     Gabrielle Allen
   @desc
               Sets the parameter checking level from a command line argument.
   @enddesc
   @calls      CCTKi_SetParameterLevel

   @var        argument
   @vdesc      option argument
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_CommandLineParameterLevel (const char *argument)
{
  int parameterlevel;


  if (CCTK_Equals (argument, "strict"))
  {
    parameterlevel = CCTK_PARAMETER_STRICT;
  }
  else if (CCTK_Equals (argument, "normal"))
  {
    parameterlevel = CCTK_PARAMETER_NORMAL;
  }
  else if (CCTK_Equals (argument, "relaxed"))
  {
    parameterlevel = CCTK_PARAMETER_RELAXED;
  }
  else
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                "CCTKi_CommandLineParameterLevel: Parameter checking level "
                "'%s' not recognized, defaulting to normal", argument);
    parameterlevel = CCTK_PARAMETER_NORMAL;
  }

  CCTKi_SetParameterLevel (parameterlevel);
}


 /*@@
   @routine    CCTKi_CommandLineRedirectStdout
   @date       Fri Jul 23 11:32:46 1999
   @author     Tom Goodale
   @desc
               Sets the redirection flag for stdout.
   @enddesc
@@*/
void CCTKi_CommandLineRedirectStdout (void)
{
  /* Set the flag to say we need to redirect the stdout. */
  redirectsubs = 1;
}


 /*@@
   @routine    CCTKi_CommandLineListThorns
   @date       Tue Apr 18 15:05:00 2000
   @author     Tom Goodale
   @desc
               List the thorns which are compiled in.
   @enddesc
   @calls      CCTKi_PrintThorns
@@*/
void CCTKi_CommandLineListThorns (void)
{
  if (CCTK_MyProc (NULL) == 0)
  {
    printf ("\n---------------Compiled Thorns-------------\n");
    CCTKi_PrintThorns (stdout, "  %s\n", 0);
    printf ("-------------------------------------------\n\n");
  }

  CCTK_Exit (NULL, 1);
}


 /*@@
   @routine    CCTKi_CommandLineVersion
   @date       Fri Jul 23 12:57:45 1999
   @author     Tom Goodale
   @desc
               Prints version info.
   @enddesc
   @calls      CCTK_FullVersion
               CCTK_CompileDate
               CCTK_CompileTime
@@*/
void CCTKi_CommandLineVersion(void)
{
  char **argv;
  const char *version;


  if (CCTK_MyProc (NULL) == 0)
  {
    CCTK_CommandLine (&argv);
    version = CCTK_FullVersion ();

    printf ("%s: Version %s.  Compiled on %s at %s\n", argv[0], version,
            CCTK_CompileDate (), CCTK_CompileTime ());
  }

  CCTK_Exit (NULL, 1);
}


 /*@@
   @routine    CCTKi_CommandLineHelp
   @date       Fri Jul 23 12:57:23 1999
   @author     Tom Goodale
   @desc
               Prints a help message.
   @enddesc
   @calls      CCTK_CommandLine
               CCTK_CompileDate
               CCTK_CompileTime
@@*/
void CCTKi_CommandLineHelp (void)
{
  char **argv;
  const char *commandline_options_description =
    "-h, -help                           : gets this help.\n"
    "-O, -describe-all-parameters        : describes all the parameters.\n"
    "-o, -describe-parameter <paramname> : describe the given parameter.\n"
    "-x, -test-parameters [nprocs]       : does a quick test of the parameter file\n"
    "                                      pretending to be on nprocs processors, \n"
    "                                      or 1 if not given.\n"
    "-W, -warning-level <n>              : Sets the warning level to n.\n"
    "-E, -error-level <n>                : Sets the error level to n.\n"
    "-r, -redirect-stdout                : Redirects standard output to files.\n"
    "-T, -list-thorns                    : Lists the compiled-in thorns.\n"
    "-t, -test-thorn-compiled <name>     : Tests for the presence of thorn <name>.\n"
    "    -parameter-level <level>        : Sets the amount of parameter checking, \n"
    "                                      level can be script, normal, relaxed.\n"
    "-v, -version                        : Prints the version.\n"
    "-i, -ignore-next                    : Ignores the next argument.\n";


  if (CCTK_MyProc (NULL) == 0)
  {
    CCTK_CommandLine (&argv);

    printf ("%s, compiled on %s at %s\n",
            argv[0], CCTK_CompileDate(), CCTK_CompileTime());
    printf ("Usage: %s %s\n", argv[0], CACTUS_COMMANDLINE_OPTIONS);
    printf ("\nValid options:\n%s", commandline_options_description);
  }

  CCTK_Exit (NULL, 1);
}


 /*@@
   @routine    CCTKi_CommandLineUsage
   @date       Fri Jul 23 12:57:04 1999
   @author     Tom Goodale
   @desc
               Prints a usage message.
   @enddesc
   @calls      CCTK_CommandLine
@@*/
void CCTKi_CommandLineUsage (void)
{
  char **argv;


  if (CCTK_MyProc (NULL) == 0)
  {
    CCTK_CommandLine (&argv);

    printf ("Usage: %s %s\n", argv[0], CACTUS_COMMANDLINE_OPTIONS);
  }

  CCTK_Exit (NULL, 1);
}


 /*@@
   @routine    CCTKi_CommandLineFinished
   @date       Fri Jul 23 12:55:39 1999
   @author     Tom Goodale
   @desc
               Subroutine to do anything which has to be done based upon the
               commandline, but needs to be have a default.
   @enddesc
@@*/
void CCTKi_CommandLineFinished (void)
{
  int myproc;
  char fname[256];


  /* Are we in a paramcheck run ? */
  if (! paramchecking)
  {
    cctki_paramchecking = 0;
  }

  /* Redirect output from sub-processors ... */
  myproc = CCTK_MyProc (NULL);
  if (myproc)
  {
    if (redirectsubs)
    {
      sprintf (fname, "CCTK_Proc%d.out", myproc);
    }
    else
    {
      sprintf (fname, NULL_DEVICE);
    }
    freopen (fname, "w", stdout);
  }
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    CommandLinePrintParameter
   @date       Sun Oct 17 22:11:31 1999
   @author     Tom Goodale
   @desc
               Prints info for a parameter.
   @enddesc
   @calls      CCTK_ThornImplementation

   @var        properties
   @vdesc      Parameter properties
   @vtype      const cParamData *
   @vio        in
   @endvar
@@*/
static void CommandLinePrintParameter (const cParamData *properties)
{
  t_range *range;


  if (properties)
  {
    printf ("Parameter:   %s::%s", properties->thorn, properties->name);
    if (properties->scope != SCOPE_PRIVATE)
    {
      printf (", %s::%s", CCTK_ThornImplementation (properties->thorn),
                          properties->name);
    }
    printf ("\n");
    printf ("Description: \"%s\"\n", properties->description);
    printf ("Type:        %s\n", cctk_parameter_type_names[properties->type-1]);
    printf ("Default:     %s\n", properties->defval);
    printf ("Scope:       %s\n", cctk_parameter_scopes[properties->scope-1]);

    for (range = properties->range; range; range = range->next)
    {
      printf ("  Range:     %s\n", range->range);
      printf ("    Origin:      %s\n", range->origin);
      printf ("    Description: %s\n", range->description);
    }
  }
}
