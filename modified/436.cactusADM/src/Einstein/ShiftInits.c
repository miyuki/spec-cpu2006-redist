#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
 /*@@
   @file      ShiftInits.c
   @date      June 2001
   @author    Miguel Alcubierre
   @desc 
     Initialise shift vector.
   @enddesc 
 @@*/

#include <math.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"

#include "Einstein.h"

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
void ShiftZero(CCTK_ARGUMENTS);
void ShiftRotation(CCTK_ARGUMENTS);

void ShiftZero(CCTK_ARGUMENTS)
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

        index = CCTK_GFINDEX3D(cctkGH,i,j,k);
        betax[index] = 0.0;
        betay[index] = 0.0;
        betaz[index] = 0.0;

      }
    }
  }

  USE_CCTK_CARGUMENTS   return;

}




void ShiftRotation(CCTK_ARGUMENTS)
{

  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

  int index,i,j,k,nx,ny,nz;
  
  const CCTK_REAL *m1p, *m2p, *x1p, *x2p, *y1p, *y2p, *z1p, *z2p;
  CCTK_REAL fac, r12, r22, sigma;
  int use_att, m1t, m2t, x1t, x2t, y1t, y2t, z1t, z2t, apower;

  nx = cctk_lsh[0];
  ny = cctk_lsh[1];
  nz = cctk_lsh[2];
  
  use_att = 0;
  if ( rot_shift_att ) 
  {
    if ( CCTK_Equals(initial_data,"BrBr") ) 
    {
      CCTK_INFO("Attenuated corotation shift applied: puncture data is used");
    
      m1p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhm1", "BAM_Elliptic", &m1t );
      x1p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhx1", "BAM_Elliptic", &x1t );
      y1p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhy1", "BAM_Elliptic", &y1t );
      z1p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhz1", "BAM_Elliptic", &z1t );
      m2p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhm2", "BAM_Elliptic", &m2t );
      x2p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhx2", "BAM_Elliptic", &x2t );
      y2p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhy2", "BAM_Elliptic", &y2t );
      z2p = (const CCTK_REAL*) 
	CCTK_ParameterGet ( "bhz2", "BAM_Elliptic", &z2t );
      if ( *m1p != 0.0 ) 
      {
	use_att += 1;
      }
      if ( *m2p != 0.0 ) 
      {
	use_att += 1;
      }
    }
  }

  for(k=0; k < nz; k++)
  {
    for(j=0; j < ny; j++) 
    {
      for(i=0; i < nx; i++)
      {
        index = CCTK_GFINDEX3D(cctkGH,i,j,k);
        if ( use_att == 0 ) 
	{
          fac = 1.0 / pow(psi[index],rotation_psipower);
        }
        else 
	{
          sigma = rot_shift_att_sigma;
          apower = rot_shift_att_pow;
          fac = 1.0;
          if ( *m1p != 0.0 ) 
	  {
            r12 = ( pow(x[index]-*x1p,2) +
                    pow(y[index]-*y1p,2) +
                    pow(z[index]-*z1p,2) ) / pow(sigma,2);
            fac -= exp(-pow(r12,apower));
          }
          if ( *m2p != 0.0 ) 
	  {
            r22 = ( pow(x[index]-*x2p,2) +
                    pow(y[index]-*y2p,2) +
                    pow(z[index]-*z2p,2) ) / pow(sigma,2);
            fac -= exp(-pow(r22,apower));
          }
           
        }

        betax[index] = -rotation_omega*y[index]*fac;
        betay[index] = +rotation_omega*x[index]*fac;
        betaz[index] = 0.0;

      }
    }
  }

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;

}
