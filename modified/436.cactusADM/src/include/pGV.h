 /*@@
   @header    pGV.h
   @date      Wed Oct 13 01:14:02 CEST 1999  
   @author    Gabrielle Allen
   @desc 
   The Pugh Grid Variable structure. Based heavily on the former pGH.h and pGF.h
   @enddesc 
   @version $Header: /cactus/CactusPUGH/PUGH/src/include/pGV.h,v 1.13 2001/10/07 10:06:31 goodale Exp $
 @@*/

#ifndef _PGV_H_
#define _PGV_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

#define DATINDEX(GH,i,j,k) ((i) + GH->lnsize[0]*((j)+GH->lnsize[1]*(k)))

typedef enum {pgv_none, pgv_scalar, pgv_array, pgv_gf} pgv_type;

typedef struct PConnectivity
{
  int dim;
  int *nprocs;
  int **neighbours;
  int *perme;         /* Is the system periodic? */
} pConnectivity;

typedef struct PGS
{
  int vtype;
  void *data;
} pGS;

typedef struct PGExtras
{
  int   dim;                    /* dimension of GA */

  int *nsize;                  /* The global size of the array */

  /* Processor group layouts */
  double   maxskew;             /* Maximum point skew */
  int     **lb;                 /* Lower bound (nprocs X dim) for each proc */
  int     **ub;                 /* Upper bound (same sizes) */
  int      *bbox;               /* Flags physical/processor boundaries */
                                /* [2*dir] */
  int      *lnsize;             /* Size on this processor */
  int       npoints;            /* LOCAL number of points on this proc. */
  int      *rnpoints;           /* Number of points on each proc */
  int     **rnsize;             /* [#points on a proc][in each dir] */

  /* Copying ghostzones to/from comm buffers */
  int      *iterator;           /* Iterator for copying ghostzones [dim] */
  int      *hyper_volume;       /* Points per subcube dimension [dim] */

  /* Ghosts and overlaps. */
  int      *nghostzones;        /* Width of ghost zone */
  int      *ownership[PUGH_NSTAGGER][2];        
                                /* The box owned in each direction. */
                                /* [stagger][min/max][dir] */
                                
  int     **ghosts[PUGH_NSTAGGER][2];   
                                /* The ghost zones on each face. */
                                /* [stagger][min/max][face][dir] */
                                
  int     **overlap[PUGH_NSTAGGER][2];  
                                /* The overlap region owned on each face. */
                                /* [stagger][min/max][face][ijk] */

} pGExtras;

typedef struct PComm
{
  /*     buffer_sz[2*dim] */
  int   *buffer_sz;             /* Size of the face ghost zones */
  void  **send_buffer;          /* Storage for buffered Comm if */
  void  **recv_buffer;          /* we don't use derived types */
  int   commflag;               /* What is the comm flag set to? */
  /*     do_comm[2*dim] */
  int   *docomm;                /* Do we do comm or not? */

  int first_var;                /* first variable to be synced */
  int n_vars;                   /* number of variables to be synced */
  int sync_timelevel;           /* the timelevel to be synced */

#ifdef CCTK_MPI
  MPI_Request *sreq, *rreq;     /* Comm requests and statuses. */
  MPI_Status  *sstatus;
  MPI_Datatype mpi_type;        /* MPI datatype to use for communication */
#endif

} pComm;

typedef struct PGA
{
  char  *name;               /* The name of the grid function */
  int   id;                  /* My ID number in my GH parent. */
  int   arrayid;                /* My ID including timelevels */
  void  *padddata;           /* Storage for the data. */
  void  *data;               /* See the note above. */
  int   storage;             /* Do we have storage or not? */
  int   stagger;             /* Only Vertex Centered now... */

  void  *parent;             /* The GH to which I belong */
                             /* Note this is struct PGH whic is
                                typedeffed to pGH in pGH.h, but
                                that is included AFTER this so we 
                                need the full name for the lookahead
                                structure reference thingy.
                             */

  int   varsize;             /* The size of the data */
  int   vtype;               /* The type of the data */

  pGExtras *extras;
  pConnectivity *connectivity;

  pComm *comm;               /* comm buffer for single variable */
  pComm *groupcomm;          /* comm buffer for a variable group */

  int vector_size;
  int vector_entry;
  struct PGA *vector_base;
} pGA;

typedef struct PGV
{
  pgv_type type;

  pGS *scalar;
  pGA *array;
} pGV;

#ifdef __cplusplus
}
#endif

#endif
