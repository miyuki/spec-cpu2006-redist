 /*@@
   @header    pGH.h
   @date      Fri Feb 21 10:12:58 1997
   @author    Tom Goodale, Paul Walker
   @desc
              The Pugh Grid Hierarchy structure. 
   @enddesc
   @version   $Header: /cactus/CactusPUGH/PUGH/src/include/pGH.h,v 1.33 2001/06/14 14:50:59 tradke Exp $
 @@*/

#include "pugh_constants.h"

typedef struct PGH 
{

  /* pGH identifier */
  void     *callerid;
  int       dim;                /* The dimension of the GH */

  /* Size of the processor group */
  int       nprocs;             /* Number of processors */
  int       myproc;             /* My processor */
  int       commmodel;          /* Comm model is PUGH_ALLOCATEDBUFFERS */ 
                                /* or PUGH_DERIVEDTYPES. Currently unused */
  
  /* Size of the problems */
  int       nvariables;         /* Number of Grid variables */
  int       narrays;            /* Number of Grid arrays (inc. timelevels) */
  void   ***variables;          /* Pointers to them */

  /* What time level we're on */
  int       timelevel;

  int      *perme;              /* Periodic in each direction? */
  int       periodic;           /* Is the system periodic? */

  int       forceSync;          /* force synchronisation of GFs with storage */

  /* Coordinate information */
  /* FIXME: this is only needed for BAM */
  CCTK_REAL dx0, dy0, dz0, dt0; /* Delta of our system */

  /* Identification for a pGH */
  int       level;              /* level in Berger-Oliger stack */
  int       mglevel;            /* level in multigrid stack */
  int       convlevel;          /* level in convergence stack */

  /* pGH flags */
  int       active;             /* 1 -> GH active */
                                /* 0 -> GH is skipped for conv.test */
                                /* is used in NanCheckGH.c */

  int       comm_time;          /* time spent in communication */

#ifdef CCTK_MPI
  MPI_Datatype PUGH_mpi_complex;/* MPI datatype for generic CCTK_COMPLEX type */
  MPI_Datatype PUGH_mpi_complex8;/* same for fixed-precision CCTK_COMPLEX */
  MPI_Datatype PUGH_mpi_complex16;
  MPI_Datatype PUGH_mpi_complex32;
  MPI_Comm PUGH_COMM_WORLD;     /* the MPI communicator */

#ifdef PUGH_WITH_DERIVED_DATATYPES
  /* Derived data types for communication */
  MPI_Datatype send_char_dt [PUGH_NSTAGGER][6];
  MPI_Datatype recv_char_dt [PUGH_NSTAGGER][6];
  MPI_Datatype send_int_dt  [PUGH_NSTAGGER][6];
  MPI_Datatype recv_int_dt  [PUGH_NSTAGGER][6];
  MPI_Datatype send_real_dt [PUGH_NSTAGGER][6];
  MPI_Datatype recv_real_dt [PUGH_NSTAGGER][6];
  MPI_Datatype send_complex_dt [PUGH_NSTAGGER][6];
  MPI_Datatype recv_complex_dt [PUGH_NSTAGGER][6];
#endif
#endif

  /* Used for all grid functions */
  pGExtras **GFExtras;          /* [dim] stagger ? */
  pConnectivity **Connectivity; /* [dim] */

  char *identity_string;        /* identifier for this pGH */

} pGH;
