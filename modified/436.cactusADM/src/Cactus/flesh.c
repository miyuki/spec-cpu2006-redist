#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      flesh.cc
   @date      Fri Sep 18 14:17:08 1998
   @author    Tom Goodale
   @desc 
   Main program file for cactus.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/flesh.cc,v 1.10 2000/10/05 00:06:58 goodale Exp $
 @@*/
#include <stdio.h>

#include "cctk_Flesh.h"
#include "CactusMainFunctions.h"

static char *rcsid = "$Header: /cactus/Cactus/src/main/flesh.cc,v 1.10 2000/10/05 00:06:58 goodale Exp $";

CCTK_FILEVERSION(main_flesh_cc)

 /*@@
   @routine    main
   @date       Fri Sep 18 14:17:37 1998
   @author     Tom Goodale
   @desc 
   Main program for cactus.  This has to be c++ as on some
   architectures you need the main program in c++ if there's
   going to be any c++ at all in your program.
   
   @enddesc 
   @calls     CCTKi_InitialiseCactus CCTKi_ShutdownCactus
   @calledby   
   @history 
 
   @endhistory 
   @var     argc
   @vdesc   The number of command line arguments
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     argv
   @vdesc   The command line arguments
   @vtype   char *[]
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0 -- success
   @endreturndesc

@@*/
int main(int argc, char **argv)
{
  tFleshConfig ConfigData;

  /* Initialise any cactus specific stuff.
   */
  CCTKi_InitialiseCactus(&argc, &argv, &ConfigData);

  /* This is a (c-linkage) routine which has been registered by a thorn.
   */
  CCTK_Initialise(&ConfigData);

  /* This is a (c-linkage) routine which has been registered by a thorn.
   */
  CCTK_Evolve(&ConfigData);

  /* This is a (c-linkage) routine which has been registered by a thorn.
   */
  CCTK_Shutdown(&ConfigData);

  /* Shut down any cactus specific stuff.
   */
  CCTKi_ShutdownCactus(&ConfigData);

  return 0;
}
