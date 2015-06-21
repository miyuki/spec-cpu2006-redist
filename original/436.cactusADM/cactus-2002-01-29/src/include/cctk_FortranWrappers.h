 /*@@
   @header    cctk_FortranWrappers.h
   @date      Sat Sep 18 00:52:35 1999
   @author    Tom Goodale
   @desc 
   Routines for dealing with Fortran wrappers
   @enddesc 
 @@*/

#ifndef __CCTK_FORTRANWRAPPERS_H_
#define __CCTK_FORTRANWRAPPERS_H_

#ifdef __cplusplus
extern "C" {
#endif

int CCTK_RegisterFortranWrapper(const char *name, int (*function)(void *, void *));

int (*CCTK_FortranWrapper(const char *name))(void *, void *);

#ifdef __cplusplus
}
#endif

#endif

