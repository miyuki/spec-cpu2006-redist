#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
 /*@@
   @file    Courant.c
   @date    July 1997 
   @author  grg-UIB (Toni Arbona) + Joan Masso
   @desc
            It determines the maximum allowed dt in 3d
   @enddesc
 @@*/

#include <math.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

#include "Einstein.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/Courant.c,v 1.18 2002/01/05 19:05:47 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_Courant_c)

void CalcCourant(CCTK_ARGUMENTS);

void CalcCourant(CCTK_ARGUMENTS) 
{
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  CCTK_REAL gxxp,gxyp,gxzp,gyyp,gyzp,gzzp,psip,alpp;
  /*  CCTK_REAL betaxp,betayp,betazp;*/
  CCTK_REAL uxx,uyy,uzz,det;
  CCTK_REAL psi2;
  CCTK_REAL s1,s2,s3,s4,s5,s6,s7,mins,t1,t2,t3;
  CCTK_REAL maxfactor,localfactor,fgauge;
  CCTK_REAL minfactor,tmp;
  CCTK_REAL dx,dy,dz;
  int i,min_handle;
   
   /* start maximal factor to zero */
   maxfactor = 0.;
   minfactor = 1000000000.0;

   dx = cctk_delta_space[0];
   dy = cctk_delta_space[1];
   dz = cctk_delta_space[2];

   /* Make separate loops for different methods to avoid ifs */
   if (CCTK_Equals(timestep_method,"courant_time"))
   {
     /* loop over all the grid */
     for(i=0;i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2];i++)  
     {

       /* get the metric */
       gxxp = gxx[i];
       gxyp = gxy[i];
       gxzp = gxz[i];
       gyyp = gyy[i];
       gyzp = gyz[i];
       gzzp = gzz[i];
       alpp = alp[i];
       
       /*if(*shift_state == SHIFT_ACTIVE)
       {
	 betaxp = betax[i];
	 betayp = betay[i];
	 betazp = betaz[i];
       }
       else
       {
	 betaxp = 0.0;
	 betayp = 0.0;
	 betazp = 0.0;
	 }*/
	 
       if(*conformal_state == CONFORMAL_METRIC)
       {
         psip = psi[i];
	 psi2 = psip*psip;
       }
       else
       {
         psip = 1.0;
	 psi2 = 1.0;
       }

       /* compute determinant */
       det=-(gxzp*gxzp*gyyp)+2.*gxyp*gxzp*gyzp-gxxp*gyzp*gyzp-
	 gxyp*gxyp*gzzp+gxxp*gyyp*gzzp;

       /* invert metric */
       uxx =(-gyzp*gyzp+gyyp*gzzp)/det;
       uyy =(-gxzp*gxzp+gxxp*gzzp)/det;
       uzz =(-gxyp*gxyp+gxxp*gyyp)/det; 
      
       /* get the gauge */
       fgauge = 1.;   /* default value valid for geodesic,maximal,etc... */
       if (CCTK_Equals(slicing,"harmonic")) fgauge = gauge_speed;
       if (CCTK_Equals(slicing,"1+log")) fgauge = gauge_speed/alpp;
       
       /* Calculate 1/time to cross zone in each direction */
       localfactor = sqrt(fgauge)*alpp*sqrt(uxx)/psi2/dx;
       maxfactor = (localfactor > maxfactor ? localfactor : maxfactor);
       localfactor = sqrt(fgauge)*alpp*sqrt(uyy)/psi2/dy;
       maxfactor = (localfactor > maxfactor ? localfactor : maxfactor);
       localfactor = sqrt(fgauge)*alpp*sqrt(uzz)/psi2/dz;
       maxfactor = (localfactor > maxfactor ? localfactor : maxfactor);

     }

     minfactor = 1/maxfactor;

     min_handle = CCTK_ReductionArrayHandle("minimum");
     CCTK_ReduceLocalScalar(cctkGH,-1,min_handle,&minfactor, 
				   &tmp, CCTK_VARIABLE_REAL);
     *courant_min_time = tmp;

   }
   else if (CCTK_Equals(timestep_method,"courant_speed"))
   {
     /* loop over all the grid */
     for(i=0;i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2];i++)  
     {

       /* get the metric */
       gxxp = gxx[i];
       gxyp = gxy[i];
       gxzp = gxz[i];
       gyyp = gyy[i];
       gyzp = gyz[i];
       gzzp = gzz[i];
       alpp = alp[i];
       
       /* if(*shift_state == SHIFT_ACTIVE)
       {
	 betaxp = betax[i];
	 betayp = betay[i];
	 betazp = betaz[i];
       }
       else
       {
	 betaxp = 0.0;
	 betayp = 0.0;
	 betazp = 0.0;
	 }*/
	 
       if(*conformal_state == CONFORMAL_METRIC)
       {
         psip = psi[i];
	 psi2 = psip*psip;
       }
       else
       {
         psip = 1.0;
	 psi2 = 1.0;
       }

       s1 = gxxp * dx * dx;
       s2 = gyyp * dy * dy;
       s3 = gzzp * dz * dz;
       t1 = 2.0 * gxyp * dx * dy;
       t2 = 2.0 * gxzp * dx * dz;
       t3 = 2.0 * gyzp * dy * dz;
       s4 = s1 + s2 + t1;
       s5 = s1 + s3 + t2;
       s6 = s2 + s3 + t3;
       s7 = s1 + s2 + s3 + t1 + t2 + t3;
       mins = (s2 < s1 ? s2 : s1);
       mins = (s3 < mins ? s3 : mins);
       mins = (s4 < mins ? s4 : mins);
       mins = (s5 < mins ? s5 : mins);
       mins = (s6 < mins ? s6 : mins);
       mins = (s7 < mins ? s7 : mins);
       
       mins = sqrt(mins);
       localfactor = mins * psi2 / alpp;
       minfactor = (localfactor < minfactor ? localfactor : minfactor);
       
     }

     min_handle = CCTK_ReductionArrayHandle("minimum");
     CCTK_ReduceLocalScalar(cctkGH,-1,min_handle,&minfactor, 
				   &tmp, CCTK_VARIABLE_REAL);
     *courant_wave_speed = 1/tmp;


   }

   
   USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
