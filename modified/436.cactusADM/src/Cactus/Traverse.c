#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Traverse.c
   @date      Mon Apr 17 19:16:40 2000
   @author    Tom Goodale
   @desc 
   Routines for traversal.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/Traverse.c,v 1.3 2001/05/10 12:35:17 goodale Exp $
 @@*/

#include "cctk_Config.h"
#include "cGH.h"
#include "cctk_Flesh.h"
#include "cctki_GHExtensions.h"

static const char *rcsid="$Header: /cactus/Cactus/src/main/Traverse.c,v 1.3 2001/05/10 12:35:17 goodale Exp $";

CCTK_FILEVERSION(main_Traverse_c)

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTK_Traverse
   @date       Thu Jan 27 14:42:16 2000
   @author     Tom Goodale
   @desc 
   Routine called to traverse functions 
   @enddesc 
   @calls     CCTKi_ScheduleTraverseGHExtensions
   @calledby   
   @history 
 
   @endhistory 
   @var     GH
   @vdesc   The cGH the functions operate on.
   @vtype   cGH
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     where
   @vdesc   The schedule group to traverse
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   Returns the return value of CCTKi_ScheduleTraverse
   @endreturndesc
@@*/
int CCTK_Traverse(cGH *GH, const char *where)
{
  int retcode;

  retcode = CCTKi_ScheduleTraverseGHExtensions(GH, where);

  return retcode;
}
