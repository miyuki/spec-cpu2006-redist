#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/InitialFlat.c,v 1.8 2002/01/04 10:18:16 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_InitialFlat_c)

void InitialFlat(CCTK_ARGUMENTS);

void InitialFlat(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int index,i,j,k,nx,ny,nz;

  CCTK_INFO("Setting flat Minkowski space in Einstein");

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

        gxx[index] = 1;
        gyy[index] = 1;
        gzz[index] = 1;
        gxy[index] = 0; 
        gxz[index] = 0;  
        gyz[index] = 0;   
      
        kxx[index] = 0; 
        kxy[index] = 0;
        kxz[index] = 0;
        kyy[index] = 0;
        kyz[index] = 0;
        kzz[index] = 0;
      
        if (CCTK_Equals(shift,"none")==0) 
	{
	  *shift_state = SHIFT_ACTIVE;
	  betax[index] = 0;
	  betay[index] = 0;
	  betaz[index] = 0;
	}
        else
        {
          *shift_state = SHIFT_INACTIVE;
        }
      
	if (use_conformal) 
	{
	  *conformal_state = CONFORMAL_METRIC;
	  psi[index]   = 1;
	  psix[index]  = 0;
	  psiy[index]  = 0;
	  psiz[index]  = 0;    
	  psixy[index] = 0;
	  psixz[index] = 0;
	  psiyz[index] = 0;  
	  psixx[index] = 0;
	  psiyy[index] = 0;
	  psizz[index] = 0;
	}
	else
	{
	  *conformal_state = NOCONFORMAL_METRIC;
	}
      }
    }
  }

  
  

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
