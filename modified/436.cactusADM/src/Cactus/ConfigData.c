#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ConfigData.c
   @date      Fri Jan 15 13:27:50 1999
   @author    Tom Goodale
   @desc
              Miscellaneous routines to deal with configuration data
   @enddesc 
   @version   $Id: ConfigData.c,v 1.9 2001/11/05 14:58:53 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ConfigData.c,v 1.9 2001/11/05 14:58:53 tradke Exp $";

CCTK_FILEVERSION(main_ConfigData_c)


 /*@@
   @routine    CCTKi_AddGH
   @date       Fri Jan 15 13:43:11 1999
   @author     Tom Goodale
   @desc 
               Adds a GH to the config_data structure at a particular
               convergence level.
   @enddesc 
 
   @var        config
   @vdesc      Flesh config data
   @vtype      tFleshConfig *
   @vio        in
   @endvar 
   @var        convergence_level
   @vdesc      The convergence level
   @vtype      unsigned int
   @vio        in
   @endvar 
   @var        GH
   @vdesc      the cctk GH
   @vtype      cGH *
   @vio        in
   @endvar 

   @returntype int
   @returndesc 
               0 - success
               1 - memory failure
               2 - duplicate convergence level
   @endreturndesc
@@*/
int CCTKi_AddGH (tFleshConfig *config, unsigned int convergence_level, cGH *GH)
{
  int retval;
  unsigned int i;
  cGH **temp;


  retval = 0;

  if(config->nGHs == 0 || convergence_level > config->nGHs-1)
  {
    temp = (cGH **)realloc(config->GH, (convergence_level+1)*sizeof(cGH *));

    if(temp)
    {
      config->GH = temp;
      for(i=config->nGHs; i<convergence_level+1;i++)
      {
        config->GH[i] = NULL;
      }
      config->nGHs=convergence_level+1;
    }
    else
    {
      retval = 1;
    }
  }

  if(!retval && !config->GH[convergence_level])
  {
    config->GH[convergence_level] = GH;
  }
  else
  {
    fprintf(stderr, "Tried to store two GHs at the same convergence level !\n");
    retval = 2;
  }

  return (retval);
}
