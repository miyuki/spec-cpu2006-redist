 /*@@
   @header    PUGHSlabi.h
   @date      Sun 28 May 2000
   @author    Thomas Radke
   @desc 
              Internal function declarations of thorn PUGHSlab
   @enddesc
   @version   $Header: /cactus/CactusPUGH/PUGHSlab/src/PUGHSlabi.h,v 1.3 2001/12/05 18:21:05 tradke Exp $
 @@*/


#ifndef _PUGHSLAB_PUGHSLABI_H_
#define _PUGHSLAB_PUGHSLABI_H_

#include "cctk_Groups.h"
#include "PUGHSlab.h"

#ifdef __cplusplus
extern "C"
{
#endif

/* structure describing a hyperslab mapping */
typedef struct hslab_mapping_t
{
  int handle;
  int target_proc;
  unsigned int hdim;
  unsigned int vdim;
  int *vectors;
  int *local_startpoint;                 /* vdim */
  int *local_endpoint;                   /* vdim */
  int *global_startpoint;                /* vdim */
  int *global_endpoint;                  /* vdim */
  int *do_dir;                           /* vdim */
  int *downsample;                       /* vdim */
  int *global_hsize;                     /* hdim */
  int *local_hsize;                      /* hdim */
  unsigned int totals;
  int is_full_hyperslab;
  int is_diagonal_in_3D;
  t_hslabConversionFn conversion_fn;
  struct hslab_mapping_t *prev, *next;
  cGroup vinfo;
} hslab_mapping_t;


/* utility routines */
hslab_mapping_t *PUGHSlabi_GetMapping (int mapping_handle);
t_hslabConversionFn PUGHSlabi_GetDatatypeConversionFn (int src_type,
                                                       int dst_type);


#ifdef __cplusplus
}
#endif

#endif  /* _PUGHSLAB_PUGHSLABI_H_ */
