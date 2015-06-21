#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      FortranBindingsIO.c
   @date      Thu Feb  18 14:27:18 1999
   @author    Gabrielle Allen
   @desc 
              Fortran bindings for the IO functions
   @enddesc 
   @version   $Id: FortranBindingsIO.c,v 1.9 2001/11/05 14:58:49 tradke Exp $
 @@*/ 

#include <stdlib.h>
#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_IO.h"
#include "cctk_IOMethods.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/IO/FortranBindingsIO.c,v 1.9 2001/11/05 14:58:49 tradke Exp $";

CCTK_FILEVERSION(IO_FortranBindingsIO_c)


void CCTK_FCALL cctk_outputgh_
                           (int *istat, const cGH *GH);
void CCTK_FCALL cctki_registeriomethod_
                           (int *handle, TWO_FORTSTRING_ARG);


void CCTK_FCALL cctk_outputgh_
                           (int *istat, const cGH *GH)
{
  *istat = CCTK_OutputGH (GH);
}

void CCTK_FCALL cctki_registeriomethod_
                           (int *handle, TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (thorn, name);
  *handle = CCTKi_RegisterIOMethod (thorn, name);
  free (thorn);
  free (name);
}
