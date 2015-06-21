#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusDefaultMainLoopIndex.c
   @date      Wed Oct 04 16:53:58 2000
   @author    Gabrielle Allen
   @desc 
              The default main loop index routines
   @enddesc 
   @version   $Id: CactusDefaultMainLoopIndex.c,v 1.3 2001/11/05 14:58:53 tradke Exp $
 @@*/

#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/CactusDefaultMainLoopIndex.c,v 1.3 2001/11/05 14:58:53 tradke Exp $";

CCTK_FILEVERSION(main_CactusDefaultMainLoopIndex_c)

static int iteration = 0;

int CactusDefaultMainLoopIndex (void);
int CactusDefaultSetMainLoopIndex (int main_loop_index);


 /*@@
   @routine    CactusDefaultMainLoopIndex
   @date       Wed Oct 04 16:53:58 2000
   @author     Gabrielle Allen
   @desc 
               Default main loop index routine
   @enddesc 

   @returntype int
   @returndesc
               current value of the iteration counter
   @endreturndesc
@@*/
int CactusDefaultMainLoopIndex (void)
{
  return (iteration);
}


 /*@@
   @routine    CactusDefaultSetMainLoopIndex
   @date       Sep 22 1999
   @author     Thomas Radke
   @desc 
               Sets the iteration counter variable of the evolution loop.
               This is used for recovery.
   @enddesc

   @returntype int
   @returndesc
               value of the new iteration counter
   @endreturndesc
@@*/
int CactusDefaultSetMainLoopIndex (int main_loop_index)
{
  iteration = main_loop_index;
  return (iteration);
}
