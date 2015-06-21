#ifdef SPEC_CPU
# define THORN_IS_Time
#endif /* SPEC_CPU */
 /*@@
   @file      Given.c
   @date      September 4 1999
   @author    Gabrielle Allen
   @desc 
   Standard specification of timestep
   @enddesc 
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusBase/Time/src/Given.c,v 1.4 2001/06/04 18:20:14 allen Exp $";

CCTK_FILEVERSION(CactusBase_Time_Given_c)

void Time_Given(CCTK_ARGUMENTS);

void Time_Given(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_PARAMETERS
  DECLARE_CCTK_ARGUMENTS

  cctkGH->cctk_delta_time = timestep;  
  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
