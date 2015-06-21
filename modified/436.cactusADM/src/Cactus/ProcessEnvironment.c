#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ProcessEnvironment.c
   @date      Fri Feb 26 11:20:15 1999
   @author    Tom Goodale
   @desc 
   Checks the environment for various settings, and acts on them.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/ProcessEnvironment.c,v 1.10 2001/12/13 15:43:53 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"

#ifdef CCTK_MPI
#include "mpi.h"
#endif

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ProcessEnvironment.c,v 1.10 2001/12/13 15:43:53 tradke Exp $";

CCTK_FILEVERSION(main_ProcessEnvironment_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/
int CCTKi_ProcessEnvironment (int *argc, char ***argv,tFleshConfig *ConfigData);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

#ifdef CCTK_MPI
#define CACTUS_MPI_ERROR(xf)                                                  \
          do                                                                  \
          {                                                                   \
            int errcode;                                                      \
                                                                              \
                                                                              \
            if((errcode = xf) != MPI_SUCCESS)                                 \
            {                                                                 \
              char mpi_error_string[MPI_MAX_ERROR_STRING+1];                  \
              int resultlen;                                                  \
                                                                              \
                                                                              \
              MPI_Error_string(errcode, mpi_error_string, &resultlen);        \
              fprintf(stderr, "MPI Call %s returned error code %d (%s)\n",    \
                              #xf, errcode, mpi_error_string);                \
              fprintf(stderr, "At line %d of file %s\n",                      \
                              __LINE__, __FILE__);                            \
            }                                                                 \
          } while (0)
#endif

#ifdef CCTK_MPI
char MPI_Active = 0;
#endif


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTKi_ProcessEnvironment
   @date       Fri Feb 26 11:20:15 1999
   @author     Tom Goodale
   @desc 
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     argc
   @vdesc   Number of arguments
   @vtype   int *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     argv
   @vdesc   Argument list
   @vtype   char ***
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     ConfigData
   @vdesc   Flesh configuration data
   @vtype   tFleshConfig
   @vio     inout
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc
   0  - success
   @endreturndesc

@@*/
int CCTKi_ProcessEnvironment(int *argc, char ***argv,tFleshConfig *ConfigData)
{
  /* avoid compiler warning about unused argument */
  ConfigData = ConfigData;
  
  /* Check if MPI compiled in but choosing not to use MPI. */  

#ifdef CCTK_MPI
  if(!getenv("CACTUS_NOMPI"))
  {
    MPI_Active = 1;

    CACTUS_MPI_ERROR(MPI_Init(argc, argv));
      
  }
#else
  argc = argc;
  argv = argv;
#endif

  return 0;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/
