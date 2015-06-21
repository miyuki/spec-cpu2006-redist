 /*@@
   @header    cctki_FortranWrappers.h
   @date      Sat Sep 18 00:52:35 1999
   @author    Tom Goodale
   @desc 
   Routines for dealing with Fortran wrappers
   @enddesc 
 @@*/

#ifndef __CCTKI_FORTRANWRAPPERS_H_
#define __CCTKI_FORTRANWRAPPERS_H_

#ifdef __cplusplus
extern "C" {
#endif

int CCTKi_RegisterFortranWrapper(const char *name, int (*function)(void *, void *));

int (*CCTKi_FortranWrapper(const char *name))(void *, void *);

#ifdef __cplusplus
}
#endif

#endif

