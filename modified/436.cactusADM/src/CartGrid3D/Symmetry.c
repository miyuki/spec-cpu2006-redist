#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */

 /*@@
   @file      Symmetry.c
   @date      Tue Apr 18 14:14:16 2000
   @author    Gerd Lanfermann
   @desc 
   Routines to apply the 1/2/3D Symmetries for
   all symmetry domains (octant/bitant/quadrant).
   @enddesc 
 @@*/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/Symmetry.c,v 1.28 2001/11/05 14:59:33 tradke Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_Symmetry_c)

/*#define SYM_DEBUG*/

int CartApplySym3Di(cGH *GH, int *doSym, int *cntstag,
		   int *lssh, int *ghostz, int *sym, CCTK_REAL *var);
int CartApplySym2Di(cGH *GH, int *doSym, int *cntstag,
		   int *lssh, int *ghostz, int *sym, CCTK_REAL *var);
int CartApplySym1Di(cGH *GH, int *doSym, int *cntstag,
		   int *lssh, int *ghostz, int *sym, CCTK_REAL *var); 


/*@@
   @routine    CartApplySym3Di
   @date       Tue Apr 18 14:17:23 2000
   @author     Gerd Lanfermann
   @desc Apply Symmetry BC to 3D variables
   
   Variables passed through:
   cGH *GH        pointer to cGH
   int *doSym     flags whether to apply a symmetries on a given face
                  size 2*dim, here we only check for lower faces:0,2,4
   int *cntstag   value used when the gridpoints are staggered
                  around the origin
   int *lssh      size of the domain, 
   int *ghostz    size of the ghostzone
   int *sym       symmetry values
   CCTK_REAL *var pointer to variable
   
   index convention:
   i ~ x ~ 0
   j ~ y ~ 1
   k ~ z ~ 2 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/


int CartApplySym3Di(cGH *GH, int *doSym, int *cntstag,
		   int *lssh, int *ghostz, int *sym, CCTK_REAL *var) 
{

  int i,j,k;

#ifdef SYM_DEBUG
  printf(" doSym: %d %d /  %d %d /  %d %d \n",
	 doSym[0],doSym[1],
	 doSym[2],doSym[3],
	 doSym[4],doSym[5]);
  printf(" lssh: %d %d %d sym: %d %d %d \n", 
	 lssh[0],lssh[1],lssh[2], sym[0], sym[2], sym[4] );
  printf(" ghostz %d %d %d \n",ghostz[0],ghostz[1],ghostz[2]);
  printf(" cntstag: %d %d %d\n",cntstag[0],cntstag[1],cntstag[2]);
#endif

  if (doSym[0] == 1)
  {
    for(k=0; k < lssh[2]; k++)
    {
      for(j=0; j < lssh[1]; j++)
      {
        for(i=0; i < ghostz[0]; i++)
        {
          var[CCTK_GFINDEX3D(GH,i,j,k)] = 
	    sym[0]*var[CCTK_GFINDEX3D(GH,2*ghostz[0]-cntstag[0]-i,j,k)];
        }
      }
    }
  }
 if (doSym[2] == 1)
  {
    for(i=0; i < lssh[0]; i++)
      {
	for(k=0; k < lssh[2]; k++)
	  {
	    for(j=0; j < ghostz[1]; j++)
	      {
		var[CCTK_GFINDEX3D(GH,i,j,k)] = 
		  sym[2]*var[CCTK_GFINDEX3D(GH,i,2*ghostz[1]-cntstag[1]-j,k)];
	      }
	  }
      }
  }
 if (doSym[4] == 1)
  {
    for(i=0; i < lssh[0]; i++)
      {
	for(j=0; j < lssh[1]; j++)
	  {
	    for(k=0; k < ghostz[2]; k++)
	      {
		var[CCTK_GFINDEX3D(GH,i,j,k)] = 
		  sym[4]*var[CCTK_GFINDEX3D(GH,i,j,2*ghostz[2]-cntstag[2]-k)];
	      }
	  }
      }
  }
 return(0);
}


/*@@
   @routine    CartApplySym2Di
   @date       Tue Apr 18 14:17:23 2000
   @author     Gerd Lanfermann
   @desc Apply Symmetry BC to 2D variables


   index convention:
   i ~ x ~ 0
   j ~ y ~ 1
   k ~ z ~ 2 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
  
int CartApplySym2Di(cGH *GH, int *doSym, int *cntstag,
		   int *lssh, int *ghostz, int *sym, CCTK_REAL *var) 
{
  int i,j;

  if (doSym[0] == 1)
  {
    for(j=0; j < lssh[1]; j++)
    {
      for(i=0; i < ghostz[0]; i++)
      {
	var[CCTK_GFINDEX2D(GH,i,j)] = 
	  sym[0]*var[CCTK_GFINDEX2D(GH,2*ghostz[0]-cntstag[0]-i,j)];
      }
    }
  }
  
  if (doSym[2] == 1)
  {
    for(i=0; i < lssh[0]; i++)
    {
      for(j=0; j < ghostz[1]; j++)
      {
	var[CCTK_GFINDEX2D(GH,i,j)] = 
	  sym[2]*var[CCTK_GFINDEX2D(GH,i,2*ghostz[1]-cntstag[1]-j)];
      }
    }
  }
  
  return(0);
}


/*@@
   @routine    CartApplySym1Di
   @date       Tue Apr 18 14:17:23 2000
   @author     Gerd Lanfermann
   @desc Apply Symmetry BC to 1D variables


   index convention:
   i ~ x ~ 0
   j ~ y ~ 1
   k ~ z ~ 2 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CartApplySym1Di(cGH *GH,  int *doSym, int *cntstag,
		   int *lssh, int *ghostz, int *sym, CCTK_REAL *var) 
{
  int i;


  /* avoid compiler warnings about unused parameters */
  GH = GH;
  lssh = lssh;

  if (doSym[0] == 1)
  {
    for(i=0; i < ghostz[0]; i++)
    {
      var[CCTK_GFINDEX1D(GH,i)] = 
	sym[0]*var[CCTK_GFINDEX1D(GH,2*ghostz[0]-cntstag[0]-i)];
    }
  }

  return(0);
}
