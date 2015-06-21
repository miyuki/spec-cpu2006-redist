#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */
 /*@@
   @file      DecodeSymParameters.c
   @date      Wed May 10 18:58:00 EST 2000
   @author    Erik Schnetter
   @desc
   Decode the symmetry parameters.
   @enddesc
   @version $Header: /cactus/CactusBase/CartGrid3D/src/DecodeSymParameters.c,v 1.4 2001/02/24 13:04:43 allen Exp $
 @@*/

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/DecodeSymParameters.c,v 1.4 2001/02/24 13:04:43 allen Exp $"; 

CCTK_FILEVERSION(CactusBase_CartGrid3D_DecodeSymParameters_c)

void DecodeSymParameters3D(int sym[6]);

/*@@
   @routine    DecodeSymParameters3D
   @date       Thu May 11 11:49:08 2000
   @author     Erik Schnetter
   @desc 
      Decode the Symmetry parameters. 
      returns the symmetry flags (yes/no=1/0) 
      in the array sym
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

void DecodeSymParameters3D(int sym[6])
{
  DECLARE_CCTK_PARAMETERS

  /* The default is as set by the explicit symmetry parameters */
  /* lower faces */
  sym[0] = symmetry_xmin;
  sym[2] = symmetry_ymin;
  sym[4] = symmetry_zmin;

  /* upper faces */
  sym[1] = symmetry_xmax;
  sym[3] = symmetry_ymax;
  sym[5] = symmetry_zmax;

  /* The default can be overridden by bitant, quadrant, and octant mode */
  if (CCTK_Equals(domain, "bitant")) 
  {
    if (CCTK_Equals(bitant_plane, "xy")) 
    {
      sym[4] = 1;
    } 
    else if (CCTK_Equals(bitant_plane, "xz")) 
    {
      sym[2] = 1;
    } 
    else if (CCTK_Equals(bitant_plane, "yz")) 
    {
      sym[0] = 1;
    }
  } 
  else if (CCTK_Equals(domain, "quadrant")) 
  {
    if (CCTK_Equals(quadrant_direction, "x")) 
    {
      sym[2] = 1;
      sym[4] = 1;
    } 
    else if (CCTK_Equals(quadrant_direction, "y")) 
    {
      sym[0] = 1;
      sym[4] = 1;
    } 
    else if (CCTK_Equals(quadrant_direction, "z")) 
    {
      sym[0] = 1;
      sym[2] = 1;
    }
  } 
  else if (CCTK_Equals(domain, "octant")) 
  {
    sym[0] = 1;
    sym[2] = 1;
    sym[4] = 1;
  }
  USE_CCTK_PARAMETERS; }
