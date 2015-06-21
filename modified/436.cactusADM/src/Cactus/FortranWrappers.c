#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      FortranWrappers.c
   @date      Sat Sep 18 00:42:12 1999
   @author    Tom Goodale
   @desc 
   File for dealing with fortran wrapper functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/FortranWrappers.c,v 1.5 2001/05/10 12:35:12 goodale Exp $
 @@*/

#include <stdlib.h>

#include "cctki_FortranWrappers.h"
#include "cctk_Flesh.h"

#include "StoreNamedData.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/FortranWrappers.c,v 1.5 2001/05/10 12:35:12 goodale Exp $";

CCTK_FILEVERSION(main_FortranWrappers_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static pNamedData *registry = NULL;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_RegisterFortranWrapper
   @date       Sat Sep 18 00:51:21 1999
   @author     Tom Goodale
   @desc 
   Registers a Fortran wrapper function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   name of the wrapper function
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     function
   @vdesc   Wrapper function
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   The return code of StoreNamedData
   @endreturndesc
@@*/
int CCTKi_RegisterFortranWrapper(const char *name, 
                                 int (*function)(void *, void *))
{
  int retcode;
  retcode = StoreNamedData(&registry, name, (void *)function);

  return retcode;
}

 /*@@
   @routine    CCTKi_FortranWrapper
   @date       Sat Sep 18 00:51:57 1999
   @author     Tom Goodale
   @desc 
   Gets a Fortran wrapper function. 
   (A function taking (void *,void *), and returning an int. )
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     name
   @vdesc   Name of the wrapper
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype int (*)(void *,void *)
   @returndesc 
   The wrapper function
   @endreturndesc
@@*/
int (*CCTKi_FortranWrapper(const char *name))(void *, void *)
{
  return (int (*)(void *,void *))GetNamedData(registry, name);
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/
