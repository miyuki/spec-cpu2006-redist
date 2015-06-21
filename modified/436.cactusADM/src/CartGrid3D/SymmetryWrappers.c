#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */
 /*@@
   @file      SymmetryWrappers.c
   @date      April 2000
   @author    Gerd Lanfermann
   @desc 
   Apply symmetry boundary conditions
   @enddesc 
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_FortranString.h"
#include "Symmetry.h"

void CCTK_FCALL cartsymgi_(int *ierr, cGH *GH, int *gi);
void CCTK_FCALL cartsymgn_
     (int *ierr, cGH *GH, ONE_FORTSTRING_ARG);
void CCTK_FCALL cartsymvi_(int *ierr, cGH *GH, int *vi);
void CCTK_FCALL cartsymvn_
     (int *ierr, cGH *GH, ONE_FORTSTRING_ARG);

int CartApplySym3Di(cGH *GH, 
		    int *doSym, 
		    int *cntstag,
		    int *lssh, 
		    int *ghostz, 
		    int *sym,
		    CCTK_REAL *var);

int CartApplySym2Di(cGH *GH, 
		    int *doSym, 
		    int *cntstag,
		    int *lssh, 
		    int *ghostz, 
		    int *sym,
		    CCTK_REAL *var);

int CartApplySym1Di(cGH *GH, 
		    int *doSym, 
		    int *cntstag,
		    int *lssh, 
		    int *ghostz, 
		    int *sym,
		    CCTK_REAL *var);


static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/SymmetryWrappers.c,v 1.15 2001/08/18 21:40:41 allen Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_SymmetryWrappers_c)

/*$#define SYM_DEBUG$*/


 /*@@
   @routine    
   @date       
   @author     
   @desc 

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CartSymGI(cGH *GH, int gi) 
{ 
  DECLARE_CCTK_PARAMETERS

  int numvars, vi, first_vi;
  int idim, gdim; 
  int berr=-1,ierr=-1;
  int time;
  int *doSym, *dstag, *lssh, *cntstag;
  SymmetryGHex *sGHex;

  /* Get out if we are sure no symmetries should be applied */
  /* FIXME: There has to be a better way early bailout! */
  /*$if (CCTK_Equals(domain,"full")) return 0;$*/

  first_vi = CCTK_FirstVarIndexI(gi);
  if (first_vi<0)
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "CartSymGI: Cannot find group %d (%s)",gi,CCTK_GroupName(gi));
    return(-1);
  }

  numvars  = CCTK_NumVarsInGroupI(gi);
  gdim     = CCTK_GroupDimI(gi);


  sGHex  = (SymmetryGHex*)GH->extensions[CCTK_GHExtensionHandle("Symmetry")];
  doSym  = (int *)malloc((2*gdim)*sizeof(int)); 
  dstag  = (int *)malloc(gdim*sizeof(int));
  lssh   = (int *)malloc(gdim*sizeof(int));
  cntstag= (int *)malloc(gdim*sizeof(int));
 
  /* get the directional staggering of the group */
  ierr   = CCTK_GroupStaggerDirArrayGI(dstag, gdim, gi);
 
  /* Set value to one if  grid is staggered around the center */  

  /* Avoid origin?  Default is yes */
  cntstag[0] = no_origin && no_originx && avoid_origin && avoid_originx;
  cntstag[1] = no_origin && no_originy && avoid_origin && avoid_originy;
  cntstag[2] = no_origin && no_originz && avoid_origin && avoid_originz;

  /* Use next time level, if present */
  time   = 0;
  /*if (time < 0)
  {
    time = 0;
    }*/

  for (vi=first_vi; vi<first_vi+numvars; vi++) 
  {
    /* Apply Symmetries to lower sides [0,2,4,...] if:
       + if the Symmetry is activated (== NOT NOSYM)
       + if the Symmetry is set (== NOT UNSET)
       + if the length in the direction is more than 1 grid point
       + if the processor has a lower physical boundary.
       Whether a grid allows a symmetry along a direction (e.g. octant=all)
       is part if the Symmetry Setup process.

       No Symmetries for "upper" sides : [1,3,5,...]
    */
    for (idim=0; idim<gdim; idim++) 
    {
      if ((sGHex->GFSym[vi][idim*2] == GFSYM_UNSET))
      {
	CCTK_VWarn(0,__LINE__,__FILE__,CCTK_THORNSTRING,
		  "Symmetries unspecified for %s",CCTK_FullName(vi));
      }

      lssh[idim]      = GH->cctk_lssh[CCTK_LSSH_IDX(dstag[idim],idim)];
      
      doSym[idim*2]   = (((sGHex->GFSym[vi][idim*2]!=GFSYM_NOSYM) &&
			  (sGHex->GFSym[vi][idim*2]!=GFSYM_UNSET)) &&
			 lssh[idim]>1 && GH->cctk_bbox[idim*2]==1);
      doSym[idim*2+1] = 0;     

    }

#ifdef SYM_DEBUG
    printf(" DOSYM: %s [%d,%d] [%d,%d] [%d,%d]   --- %d %d %d \n",
	   CCTK_VarName(vi),
	   doSym[0],doSym[1],
	   doSym[2],doSym[3],
	   doSym[4],doSym[5],
	   sGHex->GFSym[vi][0],
	   sGHex->GFSym[vi][2],
	   sGHex->GFSym[vi][4]);
#endif
      
    switch (gdim) 
    {
    case 1: berr = CartApplySym1Di(GH, 
				   doSym,
				   cntstag,
				   lssh,
				   GH->cctk_nghostzones,
				   sGHex->GFSym[vi],
				   GH->data[vi][time]); break;
    case 2: berr = CartApplySym2Di(GH, 
				   doSym,
				   cntstag,
				   lssh,
				   GH->cctk_nghostzones,
				   sGHex->GFSym[vi],
				   GH->data[vi][time]); break;
    case 3: berr = CartApplySym3Di(GH, 
				   doSym,
				   cntstag, 
				   lssh, 
				   GH->cctk_nghostzones,
				   sGHex->GFSym[vi],
				   GH->data[vi][time]); break;     
    default: berr = -1; CCTK_WARN(1, "No Symmetries for GF of dim>3");
    }
    berr=(berr>-1)?0:-1;
  }
  
  free(dstag);
  free(doSym);
  free(lssh);
  free(cntstag);

  USE_CCTK_PARAMETERS;   return(ierr);
}

void CCTK_FCALL cartsymgi_(int *ierr, cGH *GH, int *gi)
{
  *ierr = CartSymGI(GH,*gi);
  return;
}



 /*@@
   @routine    
   @date       
   @author     
   @desc 

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CartSymGN(cGH *GH, const char *gn)
{
  int gi=CCTK_GroupIndex(gn);
  int retval=-1;
  if (gi>-1)
  {
    retval = CartSymGI(GH,gi);
  }
  else
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "CartSymGN: Cannot find group %s",gn);
  }
  return retval;
}

void CCTK_FCALL cartsymgn_
     (int *ierr, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(gn)
  *ierr = CartSymGN(GH,gn);
  free(gn);  
  return;
}


 /*@@
   @routine    
   @date       
   @author     
   @desc 

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CartSymVI(cGH *GH, int vi) 
{
  DECLARE_CCTK_PARAMETERS
  
  int gi;
  int idim, gdim;
  int berr=-1;
  int time;
  int *doSym, *dstag, *lssh, *cntstag;
  SymmetryGHex *sGHex;

  /* Get out if we are sure no symmetries should be applied */
  /* FIXME: There has to be a better way early bailout! */
  if (CCTK_Equals(domain,"full")) return 0;
    
  /* get the dimension/directional staggering of the 
     group that vi belongs to */
  gi     = CCTK_GroupIndexFromVarI(vi);
  if (gi<0)
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "Cannot find variable index %d (%s) in CartSymVI",
	       vi,CCTK_FullName(vi));
    return(-1);
  }

  gdim   = CCTK_GroupDimI(gi);

  sGHex  = (SymmetryGHex*)GH->extensions[CCTK_GHExtensionHandle("Symmetry")];
  doSym  = (int *)malloc((2*gdim)*sizeof(int)); 
  dstag  = (int *)malloc(gdim*sizeof(int));
  lssh   = (int *)malloc(gdim*sizeof(int));
  cntstag= (int *)malloc(gdim*sizeof(int));

  /* get the directional staggering of the group */
  berr   = CCTK_GroupStaggerDirArrayGI(dstag, gdim, gi);
  
  /* Avoid origin?  Default is yes */
  cntstag[0] = no_origin && no_originx && avoid_origin && avoid_originx;
  cntstag[1] = no_origin && no_originy && avoid_origin && avoid_originy;
  cntstag[2] = no_origin && no_originz && avoid_origin && avoid_originz;

  /* Use next time level, if present */
  time   = 0;
  /*  if (time < 0)
  {
    time = 0;
    }*/


  /* Apply Symmetries to lower sides [0,2,4] if:
     + if the Symmetry is activated (== NOT NOSYM)
     + if the Symmetry is set (== NOT UNSET)
     + if the length in the direction is more than 1 grid point
     + if the processor has a lower physical boundary.
     Whether a grid allows a symmetry along a direction (e.g. octant=all)
     is part if the Symmetry Setup process.

     No Symmetries for "upper" sides : [1,3,5]
  */
  for (idim=0; idim<gdim; idim++) 
  { 
    if (sGHex->GFSym[vi][idim*2]==GFSYM_UNSET) 
      {
	CCTK_VWarn(0,__LINE__,__FILE__,CCTK_THORNSTRING,
		   "Symmetries unspecified for %s", CCTK_FullName(vi));
      }

    lssh[idim]      = GH->cctk_lssh[CCTK_LSSH_IDX(dstag[idim],idim)];
      
    doSym[idim*2]   = (((sGHex->GFSym[vi][idim*2]!=GFSYM_NOSYM)  &&
			(sGHex->GFSym[vi][idim*2]!=GFSYM_UNSET)) &&
		       lssh[idim]>1 && GH->cctk_bbox[idim*2]);
    doSym[idim*2+1] = 0;
  }

#ifdef SYM_DEBUG
    printf(" DOSYM: %s [%d,%d] [%d,%d] [%d,%d]   --- %d %d %d \n",
           CCTK_VarName(vi),
           doSym[0],doSym[1],
           doSym[2],doSym[3],
           doSym[4],doSym[5],
           sGHex->GFSym[vi][0],
           sGHex->GFSym[vi][2],
           sGHex->GFSym[vi][4]);
#endif


  
  switch (gdim) 
  {
  case 1: berr = CartApplySym1Di(GH, 
				 doSym, 
				 cntstag,
				 lssh,
				 GH->cctk_nghostzones,
				 sGHex->GFSym[vi],
				 GH->data[vi][time]); break;
  case 2: berr = CartApplySym2Di(GH, 
				 doSym,
				 cntstag,
				 lssh,
				 GH->cctk_nghostzones,
				 sGHex->GFSym[vi],
				 GH->data[vi][time]); break;
  case 3: berr = CartApplySym3Di(GH, 
				 doSym, 
				 cntstag,
				 lssh,
				 GH->cctk_nghostzones,
				 sGHex->GFSym[vi],
				 GH->data[vi][time]); break;     
  default: berr = -1; CCTK_WARN(1, "No Symmetries for GF dim>3");
  }

  free(lssh);
  free(dstag);
  free(doSym);
  free(cntstag);
 
  USE_CCTK_PARAMETERS;   return(berr);
}

void CCTK_FCALL cartsymvi_(int *ierr, cGH *GH, int *vi)
{
  *ierr = CartSymVI(GH, *vi);
  return;
}



 /*@@
   @routine    
   @date       
   @author     
   @desc 

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CartSymVN(cGH *GH, const char *vn)
{
  int retval=-1;
  int vi=CCTK_VarIndex(vn);

  if (vi>-1)
  {
    retval = CartSymVI(GH, vi);
  }
  else
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "Cannot find variable %s in CartSymVN",vn);
  } 
  return retval;
}

void CCTK_FCALL cartsymvn_
     (int *ierr, cGH *GH, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(vn)
  *ierr = CartSymVN(GH,vn);
  free(vn);
  return;
}
  
