#ifdef SPEC_CPU
# define THORN_IS_Time
#endif /* SPEC_CPU */
 /*@@
   @file      Initialise.c
   @date      May 12 2001
   @author    Gabrielle Allen
   @desc 
   Initialise grid variables
   @enddesc 
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "cctk_Arguments.h"

static const char *rcsid = "$Header: /cactus/CactusBase/Time/src/Initialise.c,v 1.1 2001/06/04 18:20:15 allen Exp $";

CCTK_FILEVERSION(CactusBase_Time_Initialise_c)

void Time_Initialise(CCTK_ARGUMENTS);

void Time_Initialise(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS

  *courant_wave_speed = 0;  
  *courant_min_time = 0;
  *courant_dt = 0;
  USE_CCTK_CARGUMENTS }
