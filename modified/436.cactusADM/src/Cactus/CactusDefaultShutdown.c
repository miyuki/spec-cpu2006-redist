#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusDefaultShutdown.c
   @date      Fri Feb 26 16:53:58 1999
   @author    Tom Goodale
   @desc 
   The default shutdown routines.
   @enddesc 
 @@*/

#include <stdio.h>

#include "cctk_Flesh.h"
#include "cctk_Comm.h"
#include "CactusMainDefaults.h"

#ifdef CCTK_MPI
#include "mpi.h"
#endif

static const char *rcsid = "$Header: /cactus/Cactus/src/main/CactusDefaultShutdown.c,v 1.17 2001/11/05 14:58:53 tradke Exp $";

CCTK_FILEVERSION(main_CactusDefaultShutdown_c)

#ifdef CCTK_MPI
extern char MPI_Active;
#endif

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

 /*@@
   @routine    CactusDefaultShutdown
   @date       Tue Sep 29 12:45:04 1998
   @author     Tom Goodale 
   @desc 
   Default shutdown routine.
   @enddesc 
   @calls     
   @calledby   
   @history introducing CCTK_SHUTDOWN scheduling [03/00  Gerd Lanfermann]
 
   @endhistory 

@@*/
int CactusDefaultShutdown(tFleshConfig *config)
{
  int myproc;
  unsigned int conv_level;

  myproc = CCTK_MyProc(config->GH[0]);

  /* Execute termination for all convergence levels */
  for(conv_level = 0 ; conv_level < config->nGHs;  conv_level++) 
  {    
    CCTK_Traverse(config->GH[conv_level], "CCTK_TERMINATE"); 
  }
 
  /* Execute shutdown for all convergence levels */
  for(conv_level = 0 ; conv_level < config->nGHs;  conv_level++) 
  {    
    CCTK_Traverse(config->GH[conv_level], "CCTK_SHUTDOWN"); 
  }
 
#ifdef CCTK_MPI
  if(MPI_Active)
  {
    CACTUS_MPI_ERROR(MPI_Finalize());
  }
#endif

  if(myproc == 0)
  {
    printf("--------------------------------------------------------------------------------\n"); 
    printf("Done.\n");
  }

  return 0;
}
