#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
 /*@@
   @file    InitSymBound.F
   @date    March 1999
   @author  Gerd Lanfermann
   @desc
            Sets the symmetries for the Einstein grid functions
   @enddesc
 @@*/

#include "cctk.h"
#include "cctk_Arguments.h"

#include "Symmetry.h"

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/InitSymBound.c,v 1.9 2002/01/04 10:01:40 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_InitSymBound_c)

void Einstein_InitSymBound(CCTK_ARGUMENTS);

 /*@@
   @routine    Einstein_InitSymBound
   @date       March 1999
   @author     Gerd Lanfermann
   @desc 
               Sets the symmetries for the Einstein grid functions
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

void Einstein_InitSymBound(CCTK_ARGUMENTS)
{
      
  DECLARE_CCTK_ARGUMENTS 
      
  int one;
  int sym[3];

  one = 1;
  
  sym[0] =  one;
  sym[1] =  one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::gxx");
  SetCartSymVN(cctkGH, sym,"einstein::gyy");
  SetCartSymVN(cctkGH, sym,"einstein::gzz");
  sym[0] = -one;
  sym[1] = -one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::gxy");
  sym[0] = -one;
  sym[1] =  one;
  sym[2] = -one;
  SetCartSymVN(cctkGH, sym,"einstein::gxz");
  sym[0] =  one;
  sym[1] = -one;
  sym[2] = -one;
  SetCartSymVN(cctkGH, sym,"einstein::gyz");
  
  /* GROUP: curv */
  sym[0] =  one;
  sym[1] =  one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::kxx");
  SetCartSymVN(cctkGH, sym,"einstein::kyy");
  SetCartSymVN(cctkGH, sym,"einstein::kzz");
  sym[0] = -one;
  sym[1] = -one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::kxy");
  sym[0] = -one;
  sym[1] =  one;
  sym[2] = -one;
  SetCartSymVN(cctkGH, sym,"einstein::kxz");
  sym[0] =  one;
  sym[1] = -one;
  sym[2] = -one;
  SetCartSymVN(cctkGH, sym,"einstein::kyz");
  
  sym[0] =  one;
  sym[1] =  one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::alp");

  sym[0] = -one;
  sym[1] =  one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::betax");
  sym[0] =  one;
  sym[1] = -one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::betay");
  sym[0] =  one;
  sym[1] =  one;
  sym[2] = -one;
  SetCartSymVN(cctkGH, sym,"einstein::betaz");

  sym[0] =  one;
  sym[1] =  one;
  sym[2] =  one;
  SetCartSymVN(cctkGH, sym,"einstein::emask");

  USE_CCTK_CARGUMENTS   return;
}
