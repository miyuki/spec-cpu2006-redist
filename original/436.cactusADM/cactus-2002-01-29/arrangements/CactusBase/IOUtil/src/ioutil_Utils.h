 /*@@
   @header    ioutil_Utils.h
   @date      Tue 19 Sep 2000
   @author    Thomas Radke
   @desc 
              Function prototypes for setting up slice centers.
   @history
   @endhistory
   @version $Header: /cactus/CactusBase/IOUtil/src/ioutil_Utils.h,v 1.5 2001/11/01 13:24:37 tradke Exp $
 @@*/

#ifndef _IOUTIL_UTILS_H_
#define _IOUTIL_UTILS_H_

#ifdef __cplusplus
extern "C"
{
#endif


/* set the slice center for 1D lines */
int IOUtil_1DLines (const cGH *GH,
                    int num_dims,
                    int *const *const origin_index,
                    CCTK_REAL *const *const origin_phys,
                    int *const *const slice_center);

/* set the slice center for 2D planes */
int IOUtil_2DPlanes (const cGH *GH,
                     int num_dims,
                     const int *origin_index,
                     const CCTK_REAL *origin_phys,
                     int *slice_center);

int IOUtil_CreateDirectory (const cGH *GH,
                            const char *dirname,
                            int multiple_io_procs,
                            int ioproc);

#ifdef __cplusplus
}
#endif

#endif  /* _IOUTIL_UTILS_H_ */
