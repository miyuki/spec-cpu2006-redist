#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
 /*@@
   @file      MaskInit.c
   @date      
   @author    Miguel Alcubierre
   @desc 
   Initialise the mask (I just copied LapseInits.c)
   @enddesc 
 @@*/

#include <math.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/MaskInit.c,v 1.4 2002/01/04 10:18:18 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_MaskInit_c)

void MaskOne(CCTK_ARGUMENTS);

void MaskOne(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS

  int index,i,j,k,nx,ny,nz;

  nx = cctk_lsh[0];
  ny = cctk_lsh[1];
  nz = cctk_lsh[2];
  
  for(k=0; k < nz; k++)
  {
    for(j=0; j < ny; j++)
    {
      for(i=0; i < nx; i++)
      {

        index = CCTK_GFINDEX3D(cctkGH, i,j,k);
        emask[index] = 1.0;

      }
    }
  }

  USE_CCTK_CARGUMENTS   return;
}


      
