#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ProcessCommandLine.c
   @date      Thu Sep 24 10:32:28 1998
   @author    Tom Goodale
   @desc 
   Routines to deal with the command line arguments.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/ProcessCommandLine.c,v 1.31 2001/05/10 12:35:14 goodale Exp $
 @@*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_GNU.h"
#include "cctk_Misc.h"
#include "cctk_CommandLine.h"

#include "CommandLine.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ProcessCommandLine.c,v 1.31 2001/05/10 12:35:14 goodale Exp $";

CCTK_FILEVERSION(main_ProcessCommandLine_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

void CCTK_FCALL cctk_parameterfilename_
     (int *retval, int *len, char *name);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static char *parameter_file_name=NULL;

static int argc = 0;

static char **argv = NULL;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_ProcessCommandLine
   @date       Thu Sep 24 10:33:31 1998
   @author     Tom Goodale
   @desc 
   Processes the command line arguments.
   @enddesc 
   @calls    CCTKi_CommandLineTestThornCompiled CCTKi_CommandLineDescribeAllParameters CCTKi_CommandLineDescribeParameter CCTKi_CommandLineTestParameters CCTKi_CommandLineWarningLevel CCTKi_CommandLineErrorLevel CCTKi_CommandLineParameterLevel CCTKi_CommandLineRedirectStdout CCTKi_CommandLineListThorns() CCTKi_CommandLineVersion() CCTKi_CommandLineHelp
   @calledby   
   @history 
 
   @endhistory 
   @var     inargc
   @vdesc   Number of runtime arguments
   @vtype   int *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     inargv
   @vdesc   Command line arguments
   @vtype   char ***
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     ConfigData
   @vdesc   Flesh configuration data
   @vtype   tFleshConfig
   @vio     inout
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0 -- success
   @endreturndesc

@@*/
int CCTKi_ProcessCommandLine(int *inargc, char ***inargv, tFleshConfig *ConfigData)
{

  int option_index = 0;
  int c;
  int ignore;

  /* Store the command line */
  argc = *inargc;

  argv = *inargv;

  ignore = 0;

  /* Process the command line - needs some work !*/

  if(argc>1)
  {
    while (1)
    {
      struct option long_options[] =
      {
        {"help", no_argument, NULL, 'h'},
        {"describe-all-parameters", optional_argument, NULL, 'O'},
        {"describe-parameter",      required_argument, NULL, 'o'},
        {"test-parameters",         optional_argument, NULL, 'x'},
        {"warning-level",           required_argument, NULL, 'W'},
        {"error-level",             required_argument, NULL, 'E'},
        {"parameter-level",         required_argument, NULL, 256},
        {"redirect-stdout",         no_argument,       NULL, 'r'},
        {"list-thorns",             no_argument,       NULL, 'T'},
        {"test-thorn-compiled",     required_argument, NULL, 't'},
        {"version",                 no_argument,       NULL, 'v'},
        {"ignore-next",             no_argument,       NULL, 'i'},
        {0, 0, 0, 0}
      };
      
      c = getopt_long_only (argc, argv, "hO::o:x::W:E:rTt:vi",
                            long_options, &option_index);
      if (c == -1)
        break;
  
      if(!ignore)
      {
        switch (c)
        {
          case 't': CCTKi_CommandLineTestThornCompiled(optarg); break;
          case 'O': CCTKi_CommandLineDescribeAllParameters(optarg); break;
          case 'o': CCTKi_CommandLineDescribeParameter(optarg); break;
          case 'x': CCTKi_CommandLineTestParameters(optarg); break;
          case 'W': CCTKi_CommandLineWarningLevel(optarg); break;
          case 'E': CCTKi_CommandLineErrorLevel(optarg); break;
          case 256: CCTKi_CommandLineParameterLevel(optarg); break;
          case 'r': CCTKi_CommandLineRedirectStdout(); break;
          case 'T': CCTKi_CommandLineListThorns(); break;
          case 'v': CCTKi_CommandLineVersion(); break;
          case 'i': ignore = 1; break;
          case 'h': 
          case '?':
            CCTKi_CommandLineHelp(); break;
          default:
            printf ("?? getopt returned character code 0%o ??\n", c);
        }
      }
      else
      {
        printf("Ignoring option\n");
        ignore = 0;
      }
    }

    if(argc > optind)
    {
      ConfigData->parameter_file_name = argv[optind];
      parameter_file_name = ConfigData->parameter_file_name;
    }
    else
    {
      CCTKi_CommandLineUsage();
    }
  }
  else
  {
    CCTKi_CommandLineUsage();
  }

  CCTKi_CommandLineFinished();

  return 0;
}


 /*@@
   @routine    CCTK_CommandLine
   @date       Wed Feb 17 00:19:30 1999
   @author     Tom Goodale
   @desc 
   Gets the command line arguments.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     outargv
   @vdesc   Place to dump the command line arguments
   @vtype   char ***
   @vio     out
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   The number of command line arguments.
   @endreturndesc
@@*/
int CCTK_CommandLine(char ***outargv)
{
  *outargv = argv;

  return argc;
}


 /*@@
   @routine    CCTK_ParameterFilename
   @date       Tue Oct 3 2000
   @author     Gabrielle Allen
   @desc 
   Returns the parameter filename
   @enddesc 
   @calls    CCTK_Equals 
   @calledby   
   @history 
 
   @endhistory 
   @var     len
   @vdesc   The length of the incoming string
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     filename
   @vdesc   String to contain the filename
   @vtype   char *
   @vio     out
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   The length of the returned string.
   @endreturndesc

@@*/
int CCTK_ParameterFilename(int len, char *filename)
{
  int retval;
  const char *copy_string;


  if (CCTK_Equals(parameter_file_name,"-"))
  {
    copy_string = "STDIN";
  }
  else
  {
    copy_string = parameter_file_name;
  }
  retval = strlen (copy_string);
  if (retval > len - 1)
  {
    retval = len - 1;
  }
  strncpy (filename, copy_string, retval);
  filename[retval] = 0;
  return retval;
}

void CCTK_FCALL cctk_parameterfilename_
     (int *retval, int *len, char *name)
{
  *retval = CCTK_ParameterFilename(*len,name);
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/
