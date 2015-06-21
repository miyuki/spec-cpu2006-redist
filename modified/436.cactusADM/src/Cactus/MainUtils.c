#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      MainUtils.c
   @date      Sep 22 1999
   @author    Thomas Radke, Gabrielle Allen
   @desc 
   Utility Flesh routines
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/MainUtils.c,v 1.6 2001/05/10 12:35:13 goodale Exp $
 @@*/

#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_Misc.h"
#include "cctk_Schedule.h"
#include "cctk_Parameter.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/MainUtils.c,v 1.6 2001/05/10 12:35:13 goodale Exp $";

CCTK_FILEVERSION(main_MainUtils_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/
int CCTK_RunTitle(int len, char *title);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTK_RunTitle
   @date       Sun Sep 17 2000
   @author     Gabrielle Allen
   @desc 
   Returns the simulation description
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     len
   @vdesc   The length of the title buffer
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     title
   @vdesc   The title buffer
   @vtype   char *
   @vio     out
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   The length of the title
   - if title is NULL
   @endreturndesc
@@*/
int CCTK_RunTitle(int len, char *title)
{
  int retval; 
  int param_type;
  const char *cctk_title;

  cctk_title = (*(const char **)
		CCTK_ParameterGet("cctk_run_title",
				  "Cactus",
				  &param_type));
  
  if (cctk_title)
  {
    if (CCTK_Equals(cctk_title,""))
    {
      strncpy(title,"Cactus Simulation",len-1);
    }
    else
    {
      strncpy(title,cctk_title,len-1);
    }
    retval = strlen(title);
    retval=retval > len ? 0 : retval;
  }
  else
  {
    retval = -1;
  }
  return retval;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/
