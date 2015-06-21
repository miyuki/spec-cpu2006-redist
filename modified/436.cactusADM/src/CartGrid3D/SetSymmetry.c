#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */

/*@@
   @file      Symmetry.c
   @date      Mon Mar 15 15:09:00 1999
   @author    Gerd Lanfermann
   @desc 
     This file contains the routines for registering and applying symmetry 
     boundary conditions
   @enddesc 
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_FortranString.h"
#include "Symmetry.h" 

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/SetSymmetry.c,v 1.7 2001/05/10 12:35:37 goodale Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_SetSymmetry_c)

#define MAX_DIM  3
#define MAX_FACE 6


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

void DecodeSymParameters3D(int sym[6]);

void CCTK_FCALL setcartsymvi_
     (int *ierr, cGH *GH, int *sym, int *vi);
void CCTK_FCALL setcartsymvn_
     (int *ierr, cGH *GH, int *sym, ONE_FORTSTRING_ARG);
void CCTK_FCALL setcartsymgi_
     (int *ierr, cGH *GH, int *sym, int *gi);
void CCTK_FCALL setcartsymgn_
     (int *ierr, cGH *GH, int *sym, ONE_FORTSTRING_ARG);



/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    SetCartSymmetry
   @date       Mon Mar 15 15:10:58 1999
   @author     Gerd Lanfermann
   @desc 
   This routine sets the GH extension (EinsteinBoundGHex *bGHex), 
   which describes the symmetry  boundary type of each GF. Takes 
   the name of the GF ("implementation::gfname") and the symmetry operators 
   sx,sy,sz and inserts them in the array bGHex. 
   These values will looked up by the application routines SymmetryWrappers
   @enddesc 
   @calls     
   @calledby   
   @history enhanced by E.Schnetter
 
   @endhistory 

@@*/

int SetCartSymVI(cGH *GH, int *sym, int vi) 
{
  
  DECLARE_CCTK_PARAMETERS

  int domainsym[MAX_FACE];
  SymmetryGHex *sGHex;
  int dir;
  
  /* Pointer to the SymmetryGHextension */
  sGHex  = (SymmetryGHex *)GH->extensions[CCTK_GHExtensionHandle("Symmetry")];

  /* Reference the hash table in the GHex and tell it what kind of
     symmetry is being applied 
     (depending on sym and the grid layout) 
     If there is no symmetry necessary,set ESYM_NOSYM
     When we apply a symmetry and find ESYM_UNSET, something went wrong! 
   */

#ifdef SYM_DEBUG
  printf("SetSymmetry: %s   [%d,%d,%d]\n",CCTK_VarName(vi), sym[0],sym[1],sym[2]);
#endif
  
  DecodeSymParameters3D(domainsym);
  for (dir=0; dir<MAX_FACE; ++dir) 
  {
    if (domainsym[dir]) 
    {
      sGHex->GFSym[vi][dir] = sym[dir/2];
    } 
    else 
    {
      sGHex->GFSym[vi][dir] = GFSYM_NOSYM;
    }
  }

#ifdef SYM_DEBUG
  printf("SetSymmetry: %s   [%d,%d,%d]\n\n",imp_gf, 
	 sGHex->GFSym[vi][0],
	 sGHex->GFSym[vi][2],
	 sGHex->GFSym[vi][4]);
#endif
  USE_CCTK_PARAMETERS;   return 0;
}

void CCTK_FCALL setcartsymvi_
     (int *ierr, cGH *GH, int *sym, int *vi)
{
  *ierr = SetCartSymVI(GH, sym, *vi);
}

 /*@@
   @routine    SetCartSymVN
   @date       Thu May 11 13:32:55 2000
   @author     Gerd Lanfermann
   @desc 
      Applies symmetry boundary conditions from
      variable index
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int SetCartSymVN(cGH *GH, int *sym, const char *vn) {
  int vi;
  vi = CCTK_VarIndex(vn);
  
  if (vi>-1)
  {
    return(SetCartSymVI(GH, sym, vi));
  }
  else
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "Cannot find variable %s in SetCartSymVN",vn);
    return(-1);
  }
}

void CCTK_FCALL setcartsymvn_
     (int *ierr, cGH *GH, int *sym, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(vn)
  *ierr = SetCartSymVN(GH, sym, vn);
  free(vn);
}



 /*@@
   @routine SetCartSymGI
   @date       
   @author  Gerd Lanfermann   
   @desc 
      Applies symmetry boundary conditions from
      Group index
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int SetCartSymGI(cGH *GH, int *sym, int gi) 
{
  
  DECLARE_CCTK_PARAMETERS

  int domainsym[MAX_FACE];
  SymmetryGHex *sGHex; 
  int first_vari,numvars,vi;
  int dir;
  
  sGHex  = (SymmetryGHex *)GH->extensions[CCTK_GHExtensionHandle("Symmetry")];

  first_vari = CCTK_FirstVarIndexI(gi);
  numvars    = CCTK_NumVarsInGroupI(gi);

  if (first_vari<0)
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "Cannot find group %s (grp.index: %d) in SetCartSymGI",
	       CCTK_GroupName(gi),first_vari);
    return(-1);
  }

  /* Reference the hash table in the GHex and tell it what kind of
     symmetry is being applied 
     (depending on sym and the grid layout) 
     If there is no symmetry necessary,set ESYM_NOSYM
     When we apply a symmetry and find ESYM_UNSET, something went wrong! 
   */
  for (vi=first_vari; vi<first_vari+numvars; vi++) 
  {

#ifdef SYM_DEBUG
    printf("SetSymmetry: %s   [%d,%d,%d]\n",CCTK_VarName(vi), 
	   sym[0],sym[1],sym[2]);
#endif
    
    DecodeSymParameters3D (domainsym);
    for (dir=0; dir<MAX_FACE; dir++) 
    {
      if (domainsym[dir]) 
      {
	sGHex->GFSym[vi][dir] = sym[dir/2];
      }
      else 
      {
	sGHex->GFSym[vi][dir] = GFSYM_NOSYM;
      }
    }

#ifdef SYM_DEBUG
    printf("SetSymmetry: %s   [%d,%d,%d]\n\n",imp_gf, 
	   sGHex->GFSym[vi][0],
	   sGHex->GFSym[vi][2],
	   sGHex->GFSym[vi][4]);
#endif
  }
  USE_CCTK_PARAMETERS;   return(0);
}

void CCTK_FCALL setcartsymgi_
     (int *ierr, cGH *GH, int *sym, int *gi)
{
  *ierr = SetCartSymGI(GH, sym, *gi);
}



 /*@@
   @routine    
   @date       
   @author     
   @desc 
      Applies symmetry boundary conditions from
      "Implementation::Groupname"
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int SetCartSymGN(cGH *GH, int *sym, const char *gn) 
{
  int gi = CCTK_GroupIndex(gn);
  
  if (gi>-1)
  {
    return(SetCartSymGI(GH, sym, gi));
  }
  else
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "Cannot find group %s in SetCartSymGN",gn);
    return(-1);
  }
}

void CCTK_FCALL setcartsymgn_
     (int *ierr, cGH *GH, int *sym, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(gn)
  *ierr = SetCartSymGN(GH, sym, gn);
  free(gn);
}
