 /*@@
   @file      pugh_constants.h
   @date      
   @author    
   @desc 
   @enddesc 
   @version $Id: pugh_constants.h,v 1.5 2000/03/05 16:02:31 allen Exp $
 @@*/

/* Pre-processor constants start with PUGH_ */

/* Constants for the comm directions */
#define PUGH_NOCOMM 0
#define PUGH_ALLCOMM 1
#define PUGH_PLUSFACESCOMM 2
#define PUGH_MINUSFACESCOMM 3
#define PUGH_COMM(i) i+4 

/* Constants for the storage */
#define PUGH_UNDEFINEDSTORAGE -1
#define PUGH_NOSTORAGE        0
#define PUGH_STORAGE          1

/* Constants for the GF Type. Only one now ... */
/* Number of staggerings available in the code */
#define PUGH_NSTAGGER 4

/* The index for each staggering type */
/* This classifies each PGF member */
#define PUGH_VERTEXCTR 0
#define PUGH_FACECTR(i) i+1

/* The index for each staggering type */
/* This determines the PGH should be set up */
#define PUGH_NO_STAGGER 0
#define PUGH_STAGGER 1

/* Comm types available */
#define PUGH_NOCOMMBUFFERS    0
#define PUGH_ALLOCATEDBUFFERS 1
#define PUGH_DERIVEDTYPES     2

/* Termination flags */
#define TERMINATION_NOT_RAISED     0    /* no termination flag, inital setting      */
#define TERMINATION_RAISED_LOCAL   1    /* raised on one pe, has to be broadcasted  */
#define TERMINATION_RAISED_BRDCAST 2    /* flag now available on all pes: termiante */

/* PUGH error flags */
#define PUGH_SUCCESS      0
#define PUGH_ERROR       -1
#define PUGH_ERRORMEMORY -2
