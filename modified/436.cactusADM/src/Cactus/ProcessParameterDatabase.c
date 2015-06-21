#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ProcessParameterDatabase.c
   @date      Thu Sep 24 10:34:46 1998
   @author    Tom Goodale
   @desc 
              Routines to determine the parameters and store them.
   @enddesc 
   @version   $Id: ProcessParameterDatabase.c,v 1.20 2002/01/02 12:24:41 tradke Exp $
 @@*/

#include <stdio.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_Parameter.h"
#include "cctk_WarnLevel.h"

#include "cctki_Parameter.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ProcessParameterDatabase.c,v 1.20 2002/01/02 12:24:41 tradke Exp $";

CCTK_FILEVERSION(main_ProcessParameterDatabase_c)

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/
int ParseFile (FILE *ifp, 
               int (*set_function) (const char *, const char *, int),
               tFleshConfig *ConfigData);
void CCTKi_SetParameterSetMask (int mask);


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_ProcessParameterDatabase
   @date       Thu Sep 24 10:37:07 1998
   @author     Tom Goodale
   @desc 
   
   @enddesc 
   @calls      CCTKi_SetParameterSetMask
               CCTKi_NumParameterFileErrors
               ParseFile
 
   @var        ConfigData
   @vdesc      Flesh configuration data
   @vtype      tFleshConfig *
   @vio        inout
   @endvar 

   @returntype int
   @returndesc
                0 - success, or<BR>
               -1 - unable to open parameter file
   @endreturndesc
@@*/
int CCTKi_ProcessParameterDatabase (tFleshConfig *ConfigData)
{
  int parse_errors;
  FILE *parameter_file;


  CCTKi_SetParameterSetMask (PARAMETER_RECOVERY_PRE);

  if (! strcmp (ConfigData->parameter_file_name, "-"))
  {
    parameter_file = stdin;
  }
  else
  {
    parameter_file = fopen (ConfigData->parameter_file_name, "r");
  }

  if (parameter_file)
  {
    parse_errors = ParseFile (parameter_file, CCTKi_SetParameter, ConfigData);

    if (strcmp (ConfigData->parameter_file_name, "-"))
    {
      fclose (parameter_file);
    }

    if (parse_errors)
    {
      CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                  "CCTKi_SetParameterSetMask: %d parsing errors in "
                  "parameter file", parse_errors);
    }      

    if (CCTKi_NumParameterFileErrors (1))
    {
      CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                  "CCTKi_SetParameterSetMask: %d level 1 errors in "
                  "parameter file", CCTKi_NumParameterFileErrors (1));
    }

    if (CCTKi_NumParameterFileErrors (0))
    {
      CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
                  "CCTKi_SetParameterSetMask: %d level 0 errors in "
                  "parameter file", CCTKi_NumParameterFileErrors (0));
    }
  }
  else
  {
    fprintf (stderr, "Unable to open parameter file '%s'\n", 
             ConfigData->parameter_file_name);
  }
      
  return (parameter_file ? 0 : -1);
}
