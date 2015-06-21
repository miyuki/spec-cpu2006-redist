#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
/*@@
   @file      carttosphere.c
   @date      
   @author    
   @desc
   Transform metric or extrinsic curvature variables in Cartesian 
   coordinate system to Spherical polar coordinates
   @enddesc
   @version   $Id: carttosphere.c,v 1.12 2002/01/04 09:56:14 allen Exp $
 @@*/

#include <math.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/carttosphere.c,v 1.12 2002/01/04 09:56:14 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_carttosphere_c)

#define SQR(a) ((a)*(a))

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/

void metric_carttosphere(CCTK_ARGUMENTS);
void curv_carttosphere(CCTK_ARGUMENTS);

 /*@@
   @routine    metric_carttosphere
   @date       
   @author     
   @desc
   Transform 3-metric from Cartesian to spherical polar coordinates
   @enddesc

   @returntype int
   @returndesc
   @endreturndesc
@@*/

void metric_carttosphere(CCTK_ARGUMENTS) 
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  CCTK_REAL cost, sint, sinp, cosp;
  CCTK_REAL rr, sxy;
  int i;
  int r2norm;
      
  CCTK_REAL    txx,txy,txz,tyy,tyz,tzz;
    
  r2norm = (rsquared_in_sphm == 1);

  /* loop over all the grid */
  for(i=0;i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2];i++)  
  {
    
     txx = gxx[i];
     txy = gxy[i];
     txz = gxz[i];
     tyy = gyy[i];
     tyz = gyz[i];
     tzz = gzz[i];
     rr  = r[i];
     sxy = sqrt( SQR(x[i]) + SQR(y[i]));
       
  /* be careful with r=0 and xy plane */
     if (rr==0.0) 
     {
       cost = 1.0;
       sint = 0.0; 
       sinp = 0.0;
       cosp = 1.0;
     } 
     else if (sxy==0) 
     {
       cost = 1.0; 
       sint = 0.0;
       sinp = 0.0;
       cosp = 1.0;
     } 
     else 
     {
       cost = z[i]/rr;
       sint = sxy/rr;
       sinp = y[i]/sxy;
       cosp = x[i]/sxy;
     }
     
     grr[i]= 
       tyy*SQR(sinp)*SQR(sint)+
       2*cosp*txy*sinp*SQR(sint)+
       SQR(cosp)*txx*SQR(sint)+
       2*cost*tyz*sinp*sint+
       2*cosp*cost*txz*sint+
       SQR(cost)*tzz;
               
     gqq[i] = 
       (tzz*SQR(sint)+
	(-2*cost*tyz*sinp-
	 2*cosp*cost*txz)*sint+
	SQR(cost)*tyy*SQR(sinp)+
	2*cosp*SQR(cost)*txy*sinp
	+SQR(cosp)*SQR(cost)*txx);
     
     if (r2norm) 
     {
       gqq[i] = gqq[i] * SQR(r[i]);
     }
     
     gpp[i] = 
       (txx*SQR(sinp)-
	2*cosp*txy*sinp+
	SQR(cosp)*tyy);
     
     if (r2norm)  
     {
       gpp[i] = gpp[i] * SQR(r[i]) * SQR(sint);
     }
               
     grq[i] = 
       (cost*tyy*SQR(sinp)*sint+
	2*cosp*cost*txy*sinp*sint-
	cost*tzz*sint+
	SQR(cosp)*cost*txx*sint+
	2*SQR(cost)*tyz*sinp-
	tyz*sinp+
	2*cosp*SQR(cost)*txz-
	cosp*txz)*r[i];
     
     if (r2norm) 
     {
       grq[i] = grq[i] * r[i];
     }

     grp[i] = 
       ((-txy*SQR(sinp)+
	 (cosp*tyy-cosp*txx)*sinp+
	 SQR(cosp)*txy)*sint-
	cost*txz*sinp+cosp*cost*tyz);
     
     if (r2norm)  
     {
       grp[i] = grp[i] * r[i] * sint;
     }
     
     gqp[i] =
       ((txz*sinp-cosp*tyz)*sint+cost*(-txy*SQR(sinp)+
         cosp*(tyy-txx)*sinp+SQR(cosp)*txy));

     if (r2norm) 
     {
       gqp[i] = gqp[i] * SQR(r[i]) * sint;
     }
               
  }
  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }


               
 /*@@
   @routine    curv_carttosphere
   @date       
   @author     
   @desc
   Transform extrinsic curvature from Cartesian to spherical polar coordinates
   @enddesc

   @returntype int
   @returndesc
   @endreturndesc
@@*/

void curv_carttosphere(CCTK_ARGUMENTS) 
{
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  CCTK_REAL    cost, sint, sinp, cosp;
  CCTK_REAL    rr,sxy;
  int i;
  int r2norm;
      
  CCTK_REAL    txx,txy,txz,tyy,tyz,tzz;
      
  r2norm = (rsquared_in_sphm == 1) ;

  /* loop over all the gridpoints */
  for(i=0;i<cctk_lsh[0]*cctk_lsh[1]*cctk_lsh[2];i++)  
  {
    
     txx = kxx[i];
     txy = kxy[i];
     txz = kxz[i];
     tyy = kyy[i];
     tyz = kyz[i];
     tzz = kzz[i];
     rr  = r[i];
     sxy = sqrt( SQR(x[i]) + SQR(y[i]) );

     /* be careful with r=0 and xy plane */
     if (rr==0.0) 
     { 
       cost = 1.0;
       sint = 0.0; 
       sinp = 0.0;
       cosp = 1.0;
     } 
     else if (sxy==0.0) 
     {
       cost = 1.0;
       sint = 0.0;
       sinp = 0.0;
       cosp = 1.0;
     } 
     else 
     {
       cost = z[i]/rr;
       sint = sxy/rr; 
       sinp = y[i]/sxy;
       cosp = x[i]/sxy;
     }

     krr[i]= 
       tyy*SQR(sinp)*SQR(sint)+
       2*cosp*txy*sinp*SQR(sint)+
       SQR(cosp)*txx*SQR(sint)+
       2*cost*tyz*sinp*sint+
       2*cosp*cost*txz*sint+
       SQR(cost)*tzz;
               
     kqq[i] = 
       (tzz*SQR(sint)+
	(-2*cost*tyz*sinp-
	 2*cosp*cost*txz)*sint+
	SQR(cost)*tyy*SQR(sinp)+
	2*cosp*SQR(cost)*txy*sinp
	+SQR(cosp)*SQR(cost)*txx);

     if (r2norm)
     {
       kqq[i] = kqq[i] * SQR(r[i]);
     }
               
     kpp[i] = 
       (txx*SQR(sinp)-
	2*cosp*txy*sinp+
	SQR(cosp)*tyy);

     if (r2norm) 
     {
       kpp[i] = kpp[i] * SQR(r[i]) * SQR(sint);
     }

     krq[i] = 
       (cost*tyy*SQR(sinp)*sint+
	2*cosp*cost*txy*sinp*sint-
	cost*tzz*sint+
	SQR(cosp)*cost*txx*sint+
	2*SQR(cost)*tyz*sinp-
	tyz*sinp+
	2*cosp*SQR(cost)*txz-
	cosp*txz)*r[i];

     if (r2norm) 
     {
       krq[i] = krq[i] * r[i];
     }
     
     krp[i] = 
       ((-txy*SQR(sinp)+
	 (cosp*tyy-cosp*txx)*sinp+
	 SQR(cosp)*txy)*sint-
	cost*txz*sinp+cosp*cost*tyz);

     if (r2norm) 
     {
       krp[i] = krp[i] * r[i] * sint;
     }
     
     kqp[i] =
       ((txz*sinp-cosp*tyz)*sint+
	cost*(-txy*SQR(sinp)
	      +cosp*(tyy-txx)*sinp+
	      SQR(cosp)*txy));
     
     if (r2norm) 
     {
       kqp[i] = kqp[i] * SQR(r[i]) * sint;
     }
     
  }
  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS }
