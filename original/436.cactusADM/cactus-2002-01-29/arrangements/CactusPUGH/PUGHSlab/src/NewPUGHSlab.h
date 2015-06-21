 /*@@
   @header    NewPUGHSlab.h
   @date      Sun 28 May 2000
   @author    Thomas Radke
   @desc 
              Function declarations of thorn Hyperslab
   @enddesc
   @history
   @endhistory
 @@*/


#ifndef _PUGHSLAB_NEWPUGHSLAB_H_
#define _PUGHSLAB_NEWPUGHSLAB_H_

#include "PUGHSlab.h"

#ifdef __cplusplus
extern "C"
{
#endif


/* function prototypes */
int NewHyperslab_GetLocalHyperslab (const cGH *GH,
                                    int vindex,
                                    int vtimelvl,
                                    int hdim,
                                    int htype,
                                    t_hslabConversionFn copy_fn,
                                    const int global_startpoint[/* vdim */],
                                    const int directions[/* hdim * vdim */],
                                    const int lengths[/* hdim */],
                                    const int downsample_[/* hdim */],
                                    void **hdata,
                                    int *free_hdata,
                                    int hsize[/* hdim */],
                                    int hsize_global[/* hdim */],
                                    int hoffset_global[/* hdim */]);
#if 0
int NewHyperslab_GetHyperslab (cGH *GH,
                            int target_proc,
                            int vindex,
                            int vtimelvl,
                            int hdim,
                            int htype,
                            PUGHSlab_conversion_fn copy_fn,
                            const int global_startpoint [/* vdim */],
                            const int directions[/* hdim*vdim */],
                            const int lengths[/* hdim */],
                            const int downsample_[/* hdim */],
                            void **hdata,
                            int *free_hdata,
                            int hsize[/* hdim */]);
#endif


#ifdef __cplusplus
}
#endif

#endif  /* _PUGHSLAB_NEWPUGHSLAB_H_ */
