/*@@
   @header    cctk.h
   @date      Tue Jan 26 17:29:34 1999
   @author    Tom Goodale
   @desc
              Main include file for the CCTK.
              All thorns should include this.
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/cctk.h,v 1.68 2001/12/29 11:19:10 allen Exp $
 @@*/

#ifndef _CCTK_H_
#define _CCTK_H_

/* Grab the main configuration info. */
#include "cctk_Config.h"

/* Define which thorn the file is in */
#include "definethisthorn.h"

/* Include the constants */
#include "cctk_Constants.h"


/* Define some stuff */

#ifdef FCODE

#define CCTK_PRINTSEPARATOR\
  print *,"-----------------------------------------------------------"

#define _CCTK_FARGUMENTS  cctk_dim, cctk_gsh, cctk_lsh, cctk_lbnd,\
                          cctk_ubnd, cctk_lssh, cctk_from, cctk_to, cctk_bbox,\
                          cctk_delta_time, cctk_time, cctk_delta_space,\
                          cctk_origin_space, cctk_levfac, cctk_convlevel,\
                          cctk_nghostzones, cctk_iteration, cctkGH

#define _DECLARE_CCTK_ARGUMENTS _DECLARE_CCTK_FARGUMENTS
#define _DECLARE_CCTK_FARGUMENTS INTEGER cctk_dim&&\
                           INTEGER cctk_gsh(cctk_dim),cctk_lsh(cctk_dim)&&\
                           INTEGER cctk_lbnd(cctk_dim),cctk_ubnd(cctk_dim)&&\
                           INTEGER cctk_lssh(CCTK_NSTAGGER*cctk_dim)&&\
                           INTEGER cctk_from(cctk_dim),cctk_to(cctk_dim)&&\
                           INTEGER cctk_bbox(2*cctk_dim)&&\
                           CCTK_REAL cctk_delta_time, cctk_time&&\
                           CCTK_REAL cctk_delta_space(cctk_dim)&&\
                           CCTK_REAL cctk_origin_space(cctk_dim)&&\
                           INTEGER cctk_levfac(cctk_dim)&&\
                           INTEGER cctk_convlevel&&\
                           INTEGER cctk_nghostzones(cctk_dim)&&\
                           INTEGER cctk_iteration&&\
                           CCTK_POINTER cctkGH&&\

#define CCTK_WARN(a,b) CCTK_Warn(a,__LINE__,__FORTRANFILE__,CCTK_THORNSTRING,b)

#define CCTK_CoordRegisterSystem(a,b,c) CCTKi_CoordRegisterSystem(a,b,CCTK_THORNSTRING,c)

#define CCTKi_EXPECTERR(in,err,warnonerr,message) CCTKi_ExpectError(in,err,warnonerr,__LINE__,__FORTRANFILE__,CCTK_THORNSTRING,message)

#define CCTKi_EXPECTOK(in,ok,warnonerr,message)  CCTKi_ExpectOK(in,ok,warnonerr,__LINE__,__FORTRANFILE__,CCTK_THORNSTRING,message)

#define CCTK_EQUALS(a,b) (CCTK_Equals(a,b).eq.1)

#define CCTK_PASS_FTOF CCTK_FARGUMENTS

#define CCTK_DELTA_SPACE(x) (cctk_delta_space(x)/cctk_levfac(x))
#define CCTK_DELTA_TIME cctk_delta_time
#define CCTK_LSSH(stag,dim) cctk_lssh((stag)+CCTK_NSTAGGER+(dim))
#define CCTK_LSSH_IDX(stag,dim) ((stag)+CCTK_NSTAGGER*(dim))

#define DECLARE_CCTK_FUNCTIONS &&\
        integer  CCTK_Equals, CCTK_MyProc, CCTK_nProcs, CCTK_IsThornActive&&\
        external CCTK_Equals, CCTK_MyProc, CCTK_nProcs, CCTK_IsThornActive


#endif /*FCODE*/

#ifdef CCODE

#include "cGH.h"

#include "cctk_ActiveThorns.h"
#include "cctk_Banner.h"
#include "cctk_Cache.h"
#include "cctk_Coord.h"
#include "cctk_Comm.h"
#include "cctk_CommandLine.h"
#include "cctk_Complex.h"
#include "cctk_File.h"
#include "cctk_Flesh.h"
#include "cctk_FunctionAliases.h"
#include "cctk_GHExtensions.h"
#include "cctk_Groups.h"
#include "cctk_GroupsOnGH.h"
#include "cctk_Interp.h"
#include "cctk_IO.h"
#include "cctk_IOMethods.h"
#include "cctk_Main.h"
#include "cctk_Malloc.h"
#include "cctk_Misc.h"
#include "cctk_Parameter.h"
#include "cctk_Reduction.h"
#include "cctk_Stagger.h"
#include "cctk_Sync.h"
#include "cctk_Timers.h"
#include "cctk_WarnLevel.h"


/*
 * routines/macros to compute the linear index
 * of a grid funtion element from its i/j/k dimensions
 *
 * For C++ these are defined as inline functions, for C as macros.
 * For CCTK_DEBUG these are external C routines defined in Debug.c.
 */

#ifdef CCTK_DEBUG

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

int CCTK_GFINDEX1D (const cGH *GH, int i);
int CCTK_GFINDEX2D (const cGH *GH, int i, int j);
int CCTK_GFINDEX3D (const cGH *GH, int i, int j, int k);
int CCTK_GFINDEX4D (const cGH *GH, int i, int j, int k, int l);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#else  /* CCTK_DEBUG */

#ifdef __cplusplus

inline int CCTK_GFINDEX1D (const cGH *GH, int i)
{
  GH = GH;
  return (i);
}

inline int CCTK_GFINDEX2D (const cGH *GH, int i, int j)
{
  return (i + GH->cctk_lsh[0]*j);
}

inline int CCTK_GFINDEX3D (const cGH *GH, int i, int j, int k)
{
  return (i + GH->cctk_lsh[0]*(j + GH->cctk_lsh[1]*k));
}

inline int CCTK_GFINDEX4D (const cGH *GH, int i, int j, int k, int l)
{
  return (i + GH->cctk_lsh[0]*(j + GH->cctk_lsh[1]*(k + GH->cctk_lsh[2] * l)));
}

#else /* __cplusplus */

#define CCTK_GFINDEX1D(GH, i)          (i)
#define CCTK_GFINDEX2D(GH, i, j)       ((i) + (GH)->cctk_lsh[0] * ((j)))
#define CCTK_GFINDEX3D(GH, i, j, k)    ((i) + (GH)->cctk_lsh[0] *             \
                                        ((j) + (GH)->cctk_lsh[1] * (k)))
#define CCTK_GFINDEX4D(GH, i, j, k, l) ((i) + (GH)->cctk_lsh[0] *             \
                                        ((j) + (GH)->cctk_lsh[1] *            \
                                         ((k) + (GH)->cctk_lsh[2] * (l))))

#endif /* __cplusplus */

#endif /* CCTK_DEBUG */


#define CCTK_PRINTSEPARATOR \
  printf("--------------------------------------------------------------------------------\n");

#define _DECLARE_CCTK_ARGUMENTS _DECLARE_CCTK_CARGUMENTS
#define _DECLARE_CCTK_CARGUMENTS \
            int         cctk_dim = cctkGH->cctk_dim;\
            int        *cctk_gsh = cctkGH->cctk_gsh;\
            int        *cctk_lsh = cctkGH->cctk_lsh;\
            int        *cctk_lbnd = cctkGH->cctk_lbnd;\
            int        *cctk_ubnd = cctkGH->cctk_ubnd;\
            int        *cctk_lssh = cctkGH->cctk_lssh;\
            int        *cctk_from = cctkGH->cctk_from;\
            int        *cctk_to = cctkGH->cctk_to;\
            int        *cctk_bbox = cctkGH->cctk_bbox;\
            CCTK_REAL   cctk_delta_time = cctkGH->cctk_delta_time;\
            CCTK_REAL   cctk_time = cctkGH->cctk_time;\
            CCTK_REAL  *cctk_delta_space = cctkGH->cctk_delta_space;\
            CCTK_REAL  *cctk_origin_space = cctkGH->cctk_origin_space;\
            int        *cctk_levfac = cctkGH->cctk_levfac;\
            int         cctk_convlevel = cctkGH->cctk_convlevel;\
            int        *cctk_nghostzones = cctkGH->cctk_nghostzones;\
            int         cctk_iteration = cctkGH->cctk_iteration;\
            const void *cctk_dummy_pointer;


#define _USE_CCTK_CARGUMENTS \
            cctk_dummy_pointer = &cctk_dim;\
            cctk_dummy_pointer = &cctk_gsh;\
            cctk_dummy_pointer = &cctk_lsh;\
            cctk_dummy_pointer = &cctk_lbnd;\
            cctk_dummy_pointer = &cctk_ubnd;\
            cctk_dummy_pointer = &cctk_lssh;\
            cctk_dummy_pointer = &cctk_from;\
            cctk_dummy_pointer = &cctk_to;\
            cctk_dummy_pointer = &cctk_bbox;\
            cctk_dummy_pointer = &cctk_delta_time;\
            cctk_dummy_pointer = &cctk_time;\
            cctk_dummy_pointer = &cctk_delta_space;\
            cctk_dummy_pointer = &cctk_origin_space;\
            cctk_dummy_pointer = &cctk_levfac;\
            cctk_dummy_pointer = &cctk_convlevel;\
            cctk_dummy_pointer = &cctk_nghostzones;\
            cctk_dummy_pointer = &cctk_iteration;\
            cctk_dummy_pointer = cctk_dummy_pointer;

#define _INITIALISE_CCTK_C2F
#define _DECLARE_CCTK_C2F
#define _PASS_CCTK_C2F(xGH) &((xGH)->cctk_dim),\
                            (xGH)->cctk_gsh,(xGH)->cctk_lsh,\
                            (xGH)->cctk_lbnd,(xGH)->cctk_ubnd,\
                            (xGH)->cctk_lssh,\
                            (xGH)->cctk_from,(xGH)->cctk_to,\
                            (xGH)->cctk_bbox,\
                            &((xGH)->cctk_delta_time),\
                            &((xGH)->cctk_time), (xGH)->cctk_delta_space,\
                            (xGH)->cctk_origin_space,\
                            (xGH)->cctk_levfac,\
                            &((xGH)->cctk_convlevel),\
                            (xGH)->cctk_nghostzones,\
                            &((xGH)->cctk_iteration),\
                            (xGH)
#define _CCTK_C2F_PROTO     int *,\
                            int *,\
                            int *,int *, int *, int *, int *,int *,int *,\
                            CCTK_REAL *, CCTK_REAL *, CCTK_REAL *,\
                            CCTK_REAL *,\
                            int *,\
                            int *,\
                            int *,\
                            int *,\
                            cGH *

extern int _cctk_one;

#define CCTK_STORAGESIZE(xGH, cctk_dim, group) \
                  (CCTK_QueryGroupStorage(xGH, group) ?\
                  (CCTK_ArrayGroupSize(xGH, cctk_dim, group)) : &_cctk_one)


#define CCTK_GROUPLENGTH(xGH, group) \
                  (CCTK_QueryGroupStorage(xGH, group) ?\
                  (CCTKi_GroupLengthAsPointer(group)) : &_cctk_one)

#define CCTK_EQUALS(a,b) (CCTK_Equals((a),(b))==1)

#define CCTK_PASS_CTOC cctkGH

#define CCTK_DELTA_SPACE(x) (cctk_delta_space[x]/cctk_levfac[x])
#define CCTK_DELTA_TIME cctk_delta_time
#define CCTK_LSSH(stag,dim) cctk_lssh[(stag)+CCTK_NSTAGGER*(dim)]
#define CCTK_LSSH_IDX(stag,dim) ((stag)+CCTK_NSTAGGER*(dim))

#define CCTK_WARN(a,b) CCTK_Warn(a,__LINE__,__FILE__,CCTK_THORNSTRING,b)

#define CCTK_MALLOC(s) CCTKi_Malloc(s,__LINE__,__FILE__)
#define CCTK_FREE(p) CCTKi_Free(p)

#define CCTKi_EXPCTERR(in,err,warnonerr,message) CCTKi_ExpectError(in,err,warnonerr,__LINE__,__FORTRANFILE__,CCTK_THORNSTRING,message)

#define CCTKi_EXPCTOK(in,ok,onerr,message) CCTKi_ExpectOK(in,ok,warnonerr,__LINE__,__FORTRANFILE__,CCTK_THORNSTRING,message)

#endif /*CCODE*/

#define CCTK_INFO(a) CCTK_Info(CCTK_THORNSTRING,(a))
#define CCTK_PARAMWARN(a) CCTK_ParamWarn(CCTK_THORNSTRING,(a))

/*
#define CCTK_MAKESTRING(x) CCTK_REALSTRING(x)
#define CCTK_REALSTRING(x) #x
#define CCTK_WARN(a,b) CCTK_Warn(a,CCTK_MAKESTRING(CCTK_THORN),b,__LINE__,__FILE__)
*/


#endif
