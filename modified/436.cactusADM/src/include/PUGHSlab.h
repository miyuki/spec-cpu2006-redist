 /*@@
   @header    PUGHSlab.h
   @date      Sun 28 May 2000
   @author    Thomas Radke
   @desc 
              Global function declarations of thorn PUGHSlab
   @enddesc
   @version   $Header: /cactus/CactusPUGH/PUGHSlab/src/PUGHSlab.h,v 1.4 2001/12/04 21:47:26 tradke Exp $
 @@*/


#ifndef _PUGHSLAB_PUGHSLAB_H_
#define _PUGHSLAB_PUGHSLAB_H_

#ifdef __cplusplus
extern "C"
{
#endif

/* prototype of a datatype conversion routine */
typedef void (*t_hslabConversionFn) (const void *src,
                                     void *dst,
                                     CCTK_INT nelems,
                                     CCTK_INT src_stride,
                                     CCTK_INT dst_stride);

/* function prototypes */
CCTK_INT Hyperslab_Get (const cGH *GH,
                        CCTK_INT   mapping_handle,
                        CCTK_INT   vindex,
                        CCTK_INT   timelevel,
                        CCTK_INT   hdatatype,
                        void      *hdata);
CCTK_INT Hyperslab_GetList (const cGH *GH,
                            CCTK_INT mapping_handle,
                            CCTK_INT num_arrays,
                            const CCTK_INT *vindices   /* num_arrays */,
                            const CCTK_INT *timelevels /* num_arrays */,
                            const CCTK_INT *hdatatypes /* num_arrays */,
                            void *const *hdata         /* num_arrays */);
CCTK_INT Hyperslab_DefineGlobalMappingByIndex (
           const cGH *GH,
           CCTK_INT vindex,
           CCTK_INT hdim,
           const CCTK_INT *direction  /* vdim*hdim */,
           const CCTK_INT *origin     /* vdim */,
           const CCTK_INT *extent     /* hdim */,
           const CCTK_INT *downsample /* hdim */,
           CCTK_INT table_handle,
           CCTK_INT target_proc,
           t_hslabConversionFn conversion_fn,
           CCTK_INT *hsize            /* hdim */);
CCTK_INT Hyperslab_FreeMapping (CCTK_INT mapping_handle);


int Hyperslab_GetLocalHyperslab (const cGH *GH,
                                 int vindex,
                                 int vtimelvl,
                                 int hdim,
                                 const int global_startpoint [/*vdim*/],
                                 const int directions [/*vdim*/],
                                 const int lengths [/*hdim*/],
                                 const int downsample_ [/*hdim*/],
                                 void **hdata,
                                 int hsize [/*hdim*/], int ghsize [/*hdim*/],
                                 int hoffset [/*hdim*/]);
int Hyperslab_GetHyperslab (const cGH *GH,
                            int target_proc,
                            int vindex,
                            int vtimelvl,
                            int hdim,
                            const int global_startpoint [/*vdim*/],
                            const int directions [/*vdim*/],
                            const int lengths [/*hdim*/],
                            const int downsample_ [/*hdim*/],
                            void **hdata, int hsize [/*hdim*/]);

#ifdef __cplusplus
}
#endif

#endif  /* _PUGHSLAB_PUGHSLAB_H_ */
