#ifdef SPEC_CPU
# define THORN_IS_CartGrid3D
#endif /* SPEC_CPU */
 /*@@
   @file      ParamCheck.c
   @date      Thu Oct  7 17:11:44 1999
   @author    Tom Goodale
   @desc 
   C version of Gab's paramcheck stuff
   @enddesc 
 @@*/

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

static const char *rcsid = "$Header: /cactus/CactusBase/CartGrid3D/src/ParamCheck.c,v 1.10 2001/05/10 12:35:37 goodale Exp $";

CCTK_FILEVERSION(CactusBase_CartGrid3D_ParamCheck_c)

void ParamCheck_CartGrid3D(CCTK_ARGUMENTS);

 /*@@
   @routine    ParamCheckCartGrid3D
   @date      Tue Feb 23 1999
   @author    Gabrielle Allen
   @desc 
   Check parameters for CartGrid3D
   @enddesc 
   @calls     
   @calledby   
   @history 
   @hdate Thu Oct  7 17:23:15 1999 @hauthor Tom Goodale
   @hdesc Converted to C 
   @endhistory 

@@*/
void ParamCheck_CartGrid3D(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS
    
  int  iflag;

  iflag = 0;

  if (CCTK_Equals(type,"byrange")) 
  {
    if (CCTK_Equals(domain,"octant")) 
    {
      iflag++;
    } 
    else if (CCTK_Equals(domain,"quadrant")) 
    {
      iflag++;
    } 
    else if (CCTK_Equals(domain,"bitant")) 
    {
      iflag++;
    } 
    else if (CCTK_Equals(domain,"full")) 
    {
      iflag++;
    }

  } 
  else if (CCTK_Equals(type,"byspacing")) 
  {
    if (CCTK_Equals(domain,"bitant")) 
    { 
      iflag++;
    } 
    else if (CCTK_Equals(domain,"quadrant")) 
    { 
      iflag++;
    } else if (CCTK_Equals(domain,"octant")) 
    { 
      iflag++;
    } 
    else if (CCTK_Equals(domain,"full")) 
    { 
      iflag++;
    }
  } 
  else if (CCTK_Equals(type,"box")) 
  { 
    iflag++;

    if (!CCTK_Equals(domain,"full")) 
      CCTK_PARAMWARN("No symmetries can be used with box grid");
  }

  /* No grid was set up */

  if (iflag != 1) 
  {
    CCTK_PARAMWARN("No grid set up in CartGrid3D");
  }

  USE_CCTK_PARAMETERS;   USE_CCTK_CARGUMENTS    return;

}
