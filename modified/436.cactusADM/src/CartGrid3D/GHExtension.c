#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */
 /*@@
   @file      GHExtension.c
   @date      Mon Mar 15 15:48:42 1999
   @author    Gerd Lanfermann
   @desc 
              Set up the symmetry GH extension. This should really be done
              with StoredData
   @enddesc 
   @version   $Id: GHExtension.c,v 1.11 2001/11/05 14:59:33 tradke Exp $
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "Symmetry.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/GHExtension.c,v 1.11 2001/11/05 14:59:33 tradke Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_GHExtension_c)

int Symmetry_InitGHex(cGH *GH);
void *Symmetry_AllocGHex(tFleshConfig *config, int convlevel, cGH *GH);
int Symmetry_InitFGHex(cGH *GH);

void *Symmetry_AllocGHex(tFleshConfig *config, int convlevel, cGH *GH) 
{
  
  int gf,grid_dim,NumVars;     /* Number of  dimensions, grid functions */
  SymmetryGHex *newGHex;       /* Type of GHextension is EinsteinBoundGHex */


  /* avoid compiler warnings about unused arguments */
  config = config;
  convlevel = convlevel;
  GH = GH;

  NumVars  = CCTK_NumVars(); /* Get number of grid functions */
  grid_dim = CCTK_MaxDim();  /* Get maximal dimension of the grid */

  /* allocate the GHextension */
  newGHex = (SymmetryGHex*)malloc(sizeof(SymmetryGHex));
  
  /* allocation for the number of grid functions*/
  newGHex->GFSym = (int **)malloc(NumVars*sizeof(int *));

  /* allocation for the number of dimensions*/
  for (gf=0;gf<NumVars;gf++) 
  {
    newGHex->GFSym[gf] = (int *)malloc(2*grid_dim*sizeof(int));
  }

  /* Now we have something, that looks like [0..NumVars-1][0..grid_dim-1] 
     and we return that: This will be merged into
     the GH and can be referenced  in the following manner:
     int handle  = CCTK_GHExtensionHandle("Symmetry");
                   ..... which returns a pointer the GHextension
     BoundGHEx   = ((pGH *)GH->extensions[handle]);
                   ..... BoundGHex can now be used as:
     BoundGHex->GFSym[3][2] = 1 ;
  */ 

  return newGHex;  

}
  
int Symmetry_InitGHex(cGH *GH) 
{
  int retval = 0;
  SymmetryGHex *newGHex;   
  int handle;              
  int gf,d;
  int NumVars =CCTK_NumVars(); 
  int grid_dim;                    

  grid_dim = CCTK_MaxDim();    
  handle   = CCTK_GHExtensionHandle("Symmetry"); 
  newGHex  = (SymmetryGHex*) GH->extensions[handle]; 

  /* ... and initialize them: */
  for (gf=0;gf<NumVars;gf++) 
  {
    for(d=0;d<2*grid_dim;d++) 
    {
      newGHex->GFSym[gf][d] = GFSYM_UNSET;  /* not set */
    }
  }

  return retval;
}
      
      
      
