 /*@@
   @header    ioASCIIGH.h
   @date      Tue 9th Jan 1999
   @author    Gabrielle Allen
   @desc 
              The extensions to the GH structure from IO.
   @version   $Header: /cactus/CactusBase/IOASCII/src/ioASCIIGH.h,v 1.6 2001/12/28 21:22:44 tradke Exp $
 @@*/

#ifndef _IOASCII_IOASCII_H_
#define _IOASCII_IOASCII_H_

#include "StoreNamedData.h"

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct IOASCIIGH
{
  /* the number of times output */
  int  out1D_every;
  int  out2D_every;
  int  out3D_every;

  /* flags indicating output for var [i] */
  char   *do_out1D;
  char   *do_out2D;
  char   *do_out3D;

  /* directories in which to output */
  char  *outdir1D;
  char  *outdir2D;
  char  *outdir3D;

  /* the last iteration output for var [i] */
  int   *out1D_last;
  int   *out2D_last;
  int   *out3D_last;

  /* database for names of output files that were already created */
  pNamedData *filenameList1D;
  pNamedData *fileList_2D;
  pNamedData *fileList_3D;

  /* for 1D lines, we define the index where to start the line:
     spxyz[maxdim][XYZdirection][xyzstart_index] */
  int ***spxyz;

  /* for 2D planes, we define the index where to locate the plane
     sp2xyz[maxdim][perpendicular direction] */
  int **sp2xyz;

} asciiioGH;


/* prototypes of functions to be registered as I/O methods */
int IOASCII_Output1DGH (const cGH *GH);
int IOASCII_TriggerOutput1D (const cGH *GH, int);
int IOASCII_TimeFor1D (const cGH *GH, int);
int IOASCII_Output1DVarAs (const cGH *GH, const char *var, const char *alias);
int IOASCII_Output2DGH (const cGH *GH);
int IOASCII_TriggerOutput2D (const cGH *GH, int);
int IOASCII_TimeFor2D (const cGH *GH, int);
int IOASCII_Output2DVarAs (const cGH *GH, const char *var, const char *alias);
int IOASCII_Output3DGH (const cGH *GH);
int IOASCII_TriggerOutput3D (const cGH *GH, int);
int IOASCII_TimeFor3D (const cGH *GH, int);
int IOASCII_Output3DVarAs (const cGH *GH, const char *var, const char *alias);

/* other function prototypes */
int IOASCII_Write1D (const cGH *GH, int vindex, const char *alias);
int IOASCII_Write2D (const cGH *GH, int vindex, const char *alias);
int IOASCII_Write3D (const cGH *GH, int vindex, const char *alias);

#ifdef __cplusplus
} // extern "C"
#endif

#endif  /* _IOASCII_IOASCII_H_ */
