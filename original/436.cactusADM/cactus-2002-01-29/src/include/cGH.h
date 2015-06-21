 /*@@
   @header    cGH.h
   @date      Wed Feb 17 03:17:47 1999
   @author    Tom Goodale
   @desc 
   The cGH structure.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cGH.h,v 1.13 2000/07/14 23:20:04 allen Exp $
 @@*/

#ifndef _CGH_H_
#define _CGH_H_ 1

#ifndef _CCTK_TYPES_H
#include "cctk_Types.h"
#endif

typedef struct
{
  char storage;
  char comm;
} cGHGroupData;

typedef struct
{
  int cctk_dim;
  int cctk_iteration;

  /* ...[dim]*/
  int *cctk_gsh;
  int *cctk_lsh;
  int *cctk_lbnd;
  int *cctk_ubnd;

  /* local stagger shape lssh[3*dim] (3 staggerings) */
  int *cctk_lssh;

  /* unused */
  int *cctk_to;
  int *cctk_from;
  
  /* The grid spacings */
  CCTK_REAL cctk_delta_time;
  CCTK_REAL *cctk_delta_space;

  /* FIXME we want coordinate registration instead of this */
  CCTK_REAL *cctk_origin_space;

  /* The bounding box - 1 => a real boundary, 0 => a local grid boundary. */
  /* bbox[2*dim] */
  int *cctk_bbox;

  /* The refinement factor over the top level (coarsest) grid. */
  int *cctk_levfac;

  /* The convergence level (numbered from zero upwards) */
  int cctk_convlevel;

  /* The number of ghostzones in each direction */
  int *cctk_nghostzones;

  /* The coordinate time */
  CCTK_REAL cctk_time;

  /* data[var_num][TIMELEVEL][xyz]*/
  /* TIMELEVEL  I believe, xyz is linear */
  void ***data;

  /* The extension array */
  void **extensions;

  /* All the group data for this GH (storage, comm, etc. */
  cGHGroupData *GroupData;

} cGH;

#endif /* _CGH_H_ */
