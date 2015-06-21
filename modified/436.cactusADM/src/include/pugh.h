/*@@
  @header pugh.h
  @author Paul Walker
  @date March 1997
  @desc
  This is just a bunch of includes and some simple definitions.
  You should really see @seefile pGF.h @seefile pGH.h and
  @seefile pughProtos.h for the pugh bit, and @seefile
  pughProblem.h for the cactus-specific bits.
  @enddesc
  @version $Header: /cactus/CactusPUGH/PUGH/src/include/pugh.h,v 1.27 2001/10/31 12:05:01 tradke Exp $
@@*/

#ifndef _PUGH_H_
#define _PUGH_H_ 1

#include "cctk.h"

#ifdef CCTK_MPI
#include "mpi.h"
#endif

/***
 Define the different datatypes used for MPI communication
 NOTE: the complex datatype is defined dynamically at runtime in SetupPGH.c
 ***/
/* char type is easy */
#define PUGH_MPI_CHAR      MPI_CHAR

/* floating point types are architecture-independent,
   ie. a float has always 4 bytes, and a double has 8 bytes

   PUGH_MPI_REAL  is used for communicating reals of the generic CCTK_REAL type
   PUGH_MPI_REALn is used to explicitely communicate n-byte reals */
#ifdef  CCTK_REAL4
#define PUGH_MPI_REAL4  MPI_FLOAT
#endif
#ifdef  CCTK_REAL8
#define PUGH_MPI_REAL8  MPI_DOUBLE
#endif
#ifdef  CCTK_REAL16
#define PUGH_MPI_REAL16  (sizeof (CCTK_REAL16) == sizeof (long double) ?      \
                          MPI_LONG_DOUBLE : MPI_DATATYPE_NULL)
#endif


#ifdef  CCTK_REAL_PRECISION_16
#define PUGH_MPI_REAL   PUGH_MPI_REAL16
#elif   CCTK_REAL_PRECISION_8
#define PUGH_MPI_REAL   PUGH_MPI_REAL8
#elif   CCTK_REAL_PRECISION_4
#define PUGH_MPI_REAL   PUGH_MPI_REAL4
#endif


/* integer types are architecture-dependent:
   PUGH_MPI_INT  is used for communicating integers of the generic CCTK_INT type
   PUGH_MPI_INTn is used to explicitely communicate n-byte integers */
#ifdef  CCTK_INT8
# if !defined(SPEC_CPU_WINDOWS)
#  define PUGH_MPI_INT8  (sizeof (CCTK_INT8) == sizeof (int) ? MPI_INT :       \
                          sizeof (CCTK_INT8) == sizeof (long) ? MPI_LONG :     \
                          sizeof (CCTK_INT8) == sizeof (long long) ?           \
                          MPI_LONG_LONG_INT : MPI_DATATYPE_NULL)
# else
#  define PUGH_MPI_INT8  (sizeof (CCTK_INT8) == sizeof (int) ? MPI_INT :       \
                          sizeof (CCTK_INT8) == sizeof (long) ? MPI_LONG :     \
                          sizeof (CCTK_INT8) == sizeof (__int64) ?             \
                          MPI_LONG_LONG_INT : MPI_DATATYPE_NULL)
# endif
#endif

#ifdef  CCTK_INT4
#define PUGH_MPI_INT4  (sizeof (CCTK_INT4) == sizeof (int) ? MPI_INT :        \
                        sizeof (CCTK_INT4) == sizeof (short) ? MPI_SHORT :    \
                        MPI_DATATYPE_NULL)
#endif

#ifdef  CCTK_INT2
#define PUGH_MPI_INT2  (sizeof (CCTK_INT2) == sizeof (short) ? MPI_SHORT :    \
                        MPI_DATATYPE_NULL)
#endif

#ifdef  CCTK_INTEGER_PRECISION_8
#define PUGH_MPI_INT    PUGH_MPI_INT8
#elif   CCTK_INTEGER_PRECISION_4
#define PUGH_MPI_INT    PUGH_MPI_INT4
#elif   CCTK_INTEGER_PRECISION_2
#define PUGH_MPI_INT    PUGH_MPI_INT2
#endif


#define HEREINPUGH printf("I'm in %s at line %d\n",__FILE__,__LINE__);

#include "pugh_constants.h"

#include "pGV.h"
#include "pGH.h"

#ifdef CCTK_MPI

#define CACTUS_MPI_ERROR(fn_call)                                             \
          do {                                                                \
            int errcode;                                                      \
                                                                              \
            if ((errcode = fn_call) != MPI_SUCCESS)                           \
            {                                                                 \
              char mpi_error_string[MPI_MAX_ERROR_STRING+1];                  \
              int resultlen;                                                  \
                                                                              \
              MPI_Error_string (errcode, mpi_error_string, &resultlen);       \
              fprintf (stderr, "MPI call '%s' returned error code %d (%s)\n", \
                               #fn_call, errcode, mpi_error_string);          \
              fprintf(stderr, "At line %d of file %s\n", __LINE__, __FILE__); \
            }                                                                 \
          } while (0)
#endif

#ifdef __cplusplus
extern "C"
{
#endif


int PUGH_SetupGroup (pGH *newGH,
                     int *nsize,
                     int *nghostsize,
                     int  gtype,
                     int  vtype,
                     int  dim,
                     int  n_variables,
                     int  staggercode,
                     int  n_timelevels,
                     int  vectorgroup);

pGH *PUGH_SetupPGH(void *callerid,
                   int dim,
                   int *nsize,
                   int *nghostzones,
                   int staggertype,
                   int *perme);

void PUGH_GFSize(int dim, int *nsize);

void PUGH_GFGhostsize(int dim, int *ghostsize);
void PUGH_GFPeriodic(int dim, int *perme);

pGH *PUGH_pGH(const cGH *GH);

int PUGH_Evolve(tFleshConfig *config);

int PUGH_GetBounds(int dim,
                   int **bounds,
                   int *nprocs,
                   int *nsize);

int PUGH_Terminate (cGH *GH);

int PUGH_ParallelInit(cGH *GH);

int PUGH_Abort(cGH *GH, int retval);

int PUGH_MyProc(const cGH *GH);

int PUGH_nProcs(const cGH *GH);

int PUGH_Exit(cGH *GH, int retval);

const int *PUGH_Topology(const cGH *GH, int dim);

#ifdef CCTK_MPI
MPI_Datatype PUGH_MPIDataType (const pGH *pughGH, int cctk_type);
#endif

#ifdef __cplusplus
}
#endif

#endif /* defined _PUGH_H_ */
