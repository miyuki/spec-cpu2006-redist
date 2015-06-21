#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      SetupCache.c
   @date      Tue Nov 30 10:30:09 1999
   @author    Tom Goodale
   @desc 
              Sets up cache stuff for the CCTK
   @enddesc 
   @version   $Id: SetupCache.c,v 1.10 2001/11/05 14:58:54 tradke Exp $
 @@*/

#include "cctk_Config.h"
#include "cctk_Flesh.h"
#include "cctk_Parameters.h"

#include "cctki_Cache.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/SetupCache.c,v 1.10 2001/11/05 14:58:54 tradke Exp $";

CCTK_FILEVERSION(main_SetupCache_c)

/********************************************************************
 *********************   CCTK Local Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_SetupCache
   @date       Tue Nov 30 10:50:02 1999
   @author     Tom Goodale
   @desc 
               Sets the cache information.
   @enddesc 
   @calls      Utili_CacheDataSet

   @returntype int
   @returndesc
               0 - success
   @endreturndesc
@@*/
int CCTKi_SetupCache (void)
{
  unsigned long cache_size;
  unsigned long cacheline_bytes;
  DECLARE_CCTK_PARAMETERS


  if (manual_cache_setup)
  {
    cache_size      = manual_cache_size;
    cacheline_bytes = manual_cacheline_bytes;
  } 
  else
  {
    cache_size      = CCTK_L2_CACHE_SIZE;
    cacheline_bytes = CCTK_L2_CACHELINE_BYTES;
  }

  Utili_CacheDataSet (cacheline_bytes, cache_size);

  USE_CCTK_PARAMETERS;   return (0);
}
