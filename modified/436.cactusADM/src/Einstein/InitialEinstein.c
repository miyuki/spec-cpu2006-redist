#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/InitialEinstein.c,v 1.8 2001/05/10 12:35:52 goodale Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_InitialEinstein_c)

void InitialEinstein(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int i,j,k,index;

  /* Decide whether to use a conformal factor */

  if (use_conformal) 
  {
    *conformal_state = CONFORMAL_METRIC;
  }
  else 
  {
    *conformal_state = NOCONFORMAL_METRIC;
  }

  /* Decide whether to use a shift */

  if (!CCTK_Equals(shift,"none"))
  {
    *shift_state = SHIFT_ACTIVE;

    if (CCTK_Equals(shift,"zero"))
    {
      for(k=0; k < cctk_lsh[2]; k++)
      {
	for(j=0; j < cctk_lsh[1]; j++)
	{
	  for(i=0; i < cctk_lsh[0]; i++)
	  {

	    index = CCTK_GFINDEX3D(cctkGH, i,j,k);
 
	    betax[index] = 0.0;
	    betay[index] = 0.0;
	    betaz[index] = 0.0;

	  }
	}
      }                                            

    }
  }
  else
  {
    *shift_state = SHIFT_INACTIVE;
  }

  
  

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
