 /*@@
   @file      SymmetryCondition.c
   @date      Thu Oct  7 16:45:19 1999
   @author    Tom Goodale
   @desc 
   C version of Gerd's symmetry stuff.
   @enddesc 
 @@*/

#include "cctk.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/Attic/SymmetryCondition.c,v 1.6 2001/05/10 12:35:38 goodale Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_SymmetryCondition_c)


/*@@
   @routine    SymmetryCondition
   @date       Mon Mar 15 15:51:57 1999
   @author     Gerd Lanfermann
   @desc 
               Routine performs the symmetry boundary operations.
   @enddesc 
   @calls     
   @calledby   
   @history 
   @hdate Thu Oct  7 16:47:35 1999 @hauthor Tom Goodale
   @hdesc Converted to C 
   @endhistory 
@@*/


#define GFINDEX3D(sh,i,j,k) ((i) + sh[0]*((j)+sh[1]*(k)))

void SymmetryCondition(int nxyz[],CCTK_REAL var[], int nghostzones,int sym[], int doSym[])
{
  int i,j,k;

  int sw;

  /*      
   *      Apply symmetry if 
   *       * the grid chunk has a physical boundary (bbox)
   *       * its size in a direction is bigger than one (sh)
   *       * we actually want a symmetry (sx.ne.ESYM_UNSET)
   */
  if (doSym[1] == 1 || doSym[3] == 1 || doSym[5] == 1)
  {
    CCTKi_NotYetImplemented("Right hand side boundary conditions");
  }

  if (doSym[0] == 1)
  {
    for(k=0; k < nxyz[2]; k++)
    {
      for(j=0; j < nxyz[1]; j++)
      {
        for(sw=0; sw < nghostzones; sw++)
        {
          var[GFINDEX3D(nxyz,sw,j,k)] = 
	    sym[0]*var[GFINDEX3D(nxyz,2*nghostzones-1-sw,j,k)];
        }
      }
    }
  }

  if (doSym[2] == 1)
  {
    for(k=0; k < nxyz[2]; k++)
    {
      for(sw=0; sw < nghostzones; sw++)
      {
        for(i=0; i < nxyz[0]; i++)
        {
          var[GFINDEX3D(nxyz,i,sw,k)] = 
	    sym[2]*var[GFINDEX3D(nxyz,i,2*nghostzones-1-sw,k)];
        }
      }
    }
  }

  if (doSym[4] == 1)
  {
    for(sw=0; sw < nghostzones; sw++)
    {
      for(j=0; j < nxyz[1]; j++)
      {
        for(i=0; i < nxyz[0]; i++)
        {
          var[GFINDEX3D(nxyz,i,j,sw)] = 
	    sym[4]*var[GFINDEX3D(nxyz,i,j,2*nghostzones-1-sw)];
        }
      }
    }
  }

  return;
}

void CCTK_FNAME(SymmetryCondition)(int nxyz[],CCTK_REAL var[], int *nghostzones,int sym[], int doSym[])
{
  SymmetryCondition(nxyz, var, *nghostzones, sym, doSym);
}
