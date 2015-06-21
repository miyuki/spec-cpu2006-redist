#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/*@@
   @file      DebugDefines.c
   @date      Tue 2 Jul 2001
   @author    Thomas Radke
   @desc
              Routines to provide some debugging support for the Cactus code.
   @enddesc
   @version   $Id: DebugDefines.c,v 1.1 2001/07/03 12:42:51 tradke Exp $
 @@*/

#include "cctk_Config.h"
#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/DebugDefines.c,v 1.1 2001/07/03 12:42:51 tradke Exp $";

CCTK_FILEVERSION(main_DebugDefines_c)


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/
#ifdef CCTK_DEBUG
int CCTK_GFINDEX1D (const cGH *GH, int i);
int CCTK_GFINDEX2D (const cGH *GH, int i, int j);
int CCTK_GFINDEX3D (const cGH *GH, int i, int j, int k);
int CCTK_GFINDEX4D (const cGH *GH, int i, int j, int k, int l);


 /*@@
   @routine    CCTK_GFINDEX?D
   @date       Tue 2 Jul 2001
   @author     Thomas Radke
   @desc
               Compute the linear index of a grid function element
               from its spatial indices
   @enddesc

   @var        GH
   @vdesc      pointer to CCTK grid hierarchy extension
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        i, j, k, l
   @vdesc      spatial indices
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the linear index for the given spatial indices
   @endreturndesc
@@*/
int CCTK_GFINDEX1D (const cGH *GH, int i)
{
  GH = GH;
  return (i);
}

int CCTK_GFINDEX2D (const cGH *GH, int i, int j)
{
  return (i + GH->cctk_lsh[0]*j);
}

int CCTK_GFINDEX3D (const cGH *GH, int i, int j, int k)
{
  return (i + GH->cctk_lsh[0]*(j + GH->cctk_lsh[1]*k));
}

int CCTK_GFINDEX4D (const cGH *GH, int i, int j, int k, int l)
{
  return (i + GH->cctk_lsh[0]*(j + GH->cctk_lsh[1]*(k + GH->cctk_lsh[2] * l)));
}

#endif /* CCTK_DEBUG */
