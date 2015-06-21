 /*@@
   @header    ioGH.h
   @date      Tue 9th Jan 1999
   @author    Gabrielle Allen
   @desc 
   The extensions to the GH structure from IOUtil.
   @history
   @hauthor Thomas Radke @hdate 16 Mar 1999
   @hdesc Added parameters for 2D and 3D output
   @hauthor Thomas Radke @hdate 17 Mar 1999
   @hdesc Changed naming: IEEEIO -> FlexIO
   @hauthor Thomas Radke @hdate 30 Mar 1999
   @hdesc Undefined DI macro
   @endhistory
   @version $Header: /cactus/CactusBase/IOUtil/src/ioGH.h,v 1.20 2000/09/21 13:21:16 tradke Exp $
 @@*/

#ifndef _IOGH_H_
#define _IOGH_H_ 1


#ifdef __cplusplus
extern "C"
{
#endif


/* IOUtil's GH extension structure */
typedef struct IOGH
{

#if 0
  /* TR: commented these things out because they were moved into the other
         IO thorns' GH extensions
         But maybe someday we want them back here, eg. if the user 
         wants to say how often to do 3D output in general for all IO thorns
         that can produce 3D output; of course they should still be able
         override this with their own parameters.
   */
  /* How often to output */
  int  infoevery;
  int  IO_0Devery;
  int  IO_1Devery;
  int  IO_2Devery;
  int  IO_3Devery;

  /* Directory in which to output */
  char  *outpfx_0D;
  char  *outpfx_1D;
  char  *outpfx_2D;
  char  *outpfx_3D;
#endif

  /* for 3D output */
  int ioproc;       /* the IO processor each proc belongs to */
  int nioprocs;     /* total number of IO processors */
  int ioproc_every; /* output by every N'th processor */
  int unchunked;    /* if true generate unchunked output file */
  int *downsample;  /* downsampling parameters array of size cctk_maxdim */
  int out_single;   /* if true output 3D data in single precision */

  /* for recovery */
  int recovered;    /* flag indicating restart after successful recovery */

  /* for data file reader */
  char *do_inVars;  /* flags indicating to read in variable [i] */

} ioGH;


#ifdef __cplusplus
}
#endif

#endif /* _IOGH_H_ */

