 /*@@
   @header    Einstein.h
   @date      Thu Oct 21 02:20:04 CEST 1999
   @author    Gabrielle Allen
   @desc 
   Prototypes and constants for Einstein
   @enddesc 
   @version $Header: /cactus/CactusEinstein/Einstein/src/Einstein.h,v 1.2 1999/10/21 08:42:53 allen Exp $
 @@*/

#ifndef _EINSTEIN_H_
#define _EINSTEIN_H_

#define CONFORMAL_METRIC 1
#define NOCONFORMAL_METRIC 0

#define SHIFT_ACTIVE 1
#define SHIFT_INACTIVE 0

#ifdef CCODE

#ifdef __cplusplus 
extern "C" {
#endif

void ConfToPhys(int nx,int ny,int nz,
		CCTK_REAL *psi,
		CCTK_REAL *gxx,
		CCTK_REAL *gxy,
		CCTK_REAL *gxz,
		CCTK_REAL *gyy,
		CCTK_REAL *gyz,
		CCTK_REAL *gzz);

void PhysToConf(int nx,int ny,int nz,
		CCTK_REAL *psi,
		CCTK_REAL *gxx,
		CCTK_REAL *gxy,
		CCTK_REAL *gxz,
		CCTK_REAL *gyy,
		CCTK_REAL *gyz,
		CCTK_REAL *gzz);

#ifdef __cplusplus 
}
#endif

#endif

#endif

