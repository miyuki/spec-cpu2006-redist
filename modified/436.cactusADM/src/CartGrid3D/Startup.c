#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */
 /*@@
   @file      Startup.c
   @date      Mon Mar 15 15:48:42 1999
   @author    Gerd Lanfermann
   @desc 
     Startup file to register the GHextension and coordinates
   @enddesc 
 @@*/

#include "cctk.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/Startup.c,v 1.16 2001/08/18 21:40:41 allen Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_Startup_c)

void *Symmetry_AllocGHex(tFleshConfig *config, int convlevel, cGH *GH);
int   Symmetry_InitGHex(cGH *GH);
int   SymmetryStartup(void);
int RegisterCartGrid3DCoords(void);

 /*@@
   @routine    SymmetryStartup
   @date       Mon Mar 15 15:49:16 1999
   @author     Gerd Lanfermann
   @desc 
     Routine registers the Setup and Initialation routines for the
     GHExtension, which holds the symmetry BCs.  We name the 
     GHextension "Symmetry" and get an  integer ("handle") 
     identifying the GHex.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

/* Store the handle in a global variable for the moment. */
int Symmetry_handle;


int SymmetryStartup(void) 
{
  Symmetry_handle = CCTK_RegisterGHExtension("Symmetry");
  
  /* Register the allocation and init routine */
  CCTK_RegisterGHExtensionSetupGH(Symmetry_handle,Symmetry_AllocGHex);
  CCTK_RegisterGHExtensionInitGH(Symmetry_handle,Symmetry_InitGHex);

  return 0;
}


 /*@@
   @routine    RegisterCartGrid3DCoords
   @date       
   @author     Gabrielle Allen
   @desc 
     Routine registers the coordinates provided by CartGrid3D
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int RegisterCartGrid3DCoords(void)
{

  int ierr;
  int nerrors=0;

  ierr=CCTK_CoordRegisterSystem(3,"cart3d");
  ierr=CCTK_CoordRegisterSystem(3,"spher3d");

  ierr=CCTK_CoordRegisterData(1,"grid::x","x","cart3d");
  if (ierr<0)
  {
    CCTK_WARN(1,"Problem with registering coordinate x");
    nerrors--;
  }
  ierr=CCTK_CoordRegisterData(2,"grid::y","y","cart3d");
  if (ierr<0)
  {
    CCTK_WARN(1,"Problem with registering coordinate y");
    nerrors--;
  }
  ierr=CCTK_CoordRegisterData(3,"grid::z","z","cart3d");
  if (ierr<0)
  {
    CCTK_WARN(1,"Problem with registering coordinate z");
    nerrors--;
  }
  ierr=CCTK_CoordRegisterData(1,"grid::r","r","spher3d");
  if (ierr<0)
  {
    CCTK_WARN(1,"Problem with registering coordinate r");
    nerrors--;
  }

  return nerrors;
}
