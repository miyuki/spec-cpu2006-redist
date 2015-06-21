#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"
#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/evaltrK.c,v 1.12 2002/01/04 10:02:43 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_evaltrK_c)

#define SQR(a) ((a)*(a))

void evaltrK(CCTK_ARGUMENTS);

void evaltrK(CCTK_ARGUMENTS) 
{
 
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int  i;
  CCTK_REAL det, uxx, uxy, uyy, uxz, uyz, uzz, two;
  CCTK_REAL gxxp, gxyp, gxzp, gyyp, gyzp, gzzp;
  
  two = 2.0;

  /* loop over all the gridpoints */
  for(i=0;i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2];i++)  
  {
       
    /* get the metric */
    gxxp = gxx[i];
    gxyp = gxy[i];
    gxzp = gxz[i];
    gyyp = gyy[i];
    gyzp = gyz[i];
    gzzp = gzz[i];
      
    /* compute determinant */
    det=-(gxzp*gxzp*gyyp)+2.*gxyp*gxzp*gyzp-gxxp*gyzp*
      gyzp-gxyp*gxyp*gzzp+gxxp*gyyp*gzzp;
    
    /* invert metric. This is the conformal upper metric */
    uxx=(-SQR(gyzp) + gyyp*gzzp)/det;
    uxy=(gxzp*gyzp - gxyp*gzzp)/det;
    uyy=(-SQR(gxzp) + gxxp*gzzp)/det;
    uxz=(-gxzp*gyyp + gxyp*gyzp)/det;
    uyz=(gxyp*gxzp - gxxp*gyzp)/det;
    uzz=(-SQR(gxyp) + gxxp*gyyp)/det;
    
    /* Calculate trK */
    trK[i] = (uxx*kxx[i] + uyy*kyy[i] +
	      uzz*kzz[i]+ two*uxy*kxy[i] +
	      two*uxz*kxz[i] + two*uyz*kyz[i]);
    detg[i]= det;
    
  }

  if (*conformal_state==CONFORMAL_METRIC)
  {
    for(i=0;i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2];i++)  
    {
      trK[i] = trK[i] /(SQR(psi[i])*SQR(psi[i]));
    }
  }

  
  

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
