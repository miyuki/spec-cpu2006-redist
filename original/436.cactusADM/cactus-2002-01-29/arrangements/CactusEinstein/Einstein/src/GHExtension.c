#include "cctk.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "EinsteinBoundariesGH.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/Attic/GHExtension.c,v 1.16 2001/05/10 12:35:52 goodale Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_GHExtension_c)

void *EinsteinSym_AllocGHex(tFleshConfig *config, int convlevel, cGH *GH) 
{
  int gf,grid_dim,NumVars;     /* Number of  dimensions, grid functions */
  EinsteinBoundGHex *newGHex;  /* Type of GHextension is EinsteinBoundGHex */

  NumVars  = CCTK_GetNumVars(); /* Get number of grid functions */
  grid_dim = CCTK_GetMaxDim();  /* Get maximal dimension of the grid */

  /* allocate the GHextension */
  newGHex = (EinsteinBoundGHex*)malloc(sizeof(EinsteinBoundGHex));
  
  /* allocation for the number of grid functions*/
  newGHex->EinsteinSym = (int **)malloc(NumVars*sizeof(int *));
  
  /* allocation for the number of dimensions*/
  for (gf=0;gf<NumVars;gf++) 
  {
    newGHex->EinsteinSym[gf] = (int *)malloc(grid_dim*sizeof(int));
  }

  /* Now we have something, that looks like [0..NumVars-1][0..grid_dim-1] 
     and we return that. For the progammer, this will be merged into
     the GH and can be referenced  in the following manner:
     int handle  = CCTK_GHExtensionHandle("EinsteinBound");
                   ..... which returns a pointer the GHextension
     BoundGHEx   = ((pGH *)GH->extensions[handle]);
                   ..... BoundGHex can now be used as:
     BoundGHex->EinsteinBound[3][2] = 1 ;
  */ 

  return newGHex;  
}
  
void EinsteinSym_InitGHex(cGH *GH) {
  EinsteinBoundGHex *newGHex;      /* type of GHexten. we init here: EinsteinBoundGHex */
  int handle;                      /* to reference the GHextension*/
  int gf,d,f;
  int NumVars =CCTK_GetNumVars(); /* number of gf's */
  int grid_dim;                    

  /*We need to get the EinsteinBound piece of the grid hierarchy 
    (btw, called GH/gh, for the uninitialized) */
  grid_dim = CCTK_GetMaxDim();                          /*get the dimension of the grid */
  handle   = CCTK_GHExtensionHandle("EinsteinBound");      /* WHERE HAVE I DEFINED THIS NAME ??*/
  newGHex  = (EinsteinBoundGHex*) GH->extensions[handle]; /* now we have the GH extension ..*/

  /* ... and initialize them: */
  for (gf=0;gf<NumVars;gf++) 
  {
    for(d=0;d<grid_dim;d++) 
    {
      newGHex->EinsteinSym[gf][d] = ESYM_UNSET;  
      /* ESYM_UNSET: not set (not even default) */
    }
  }
}
      
      
      


