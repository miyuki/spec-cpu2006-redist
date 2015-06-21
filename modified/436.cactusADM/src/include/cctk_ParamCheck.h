 /*@@
   @header    cctk_ParamCheck.h
   @date      Tue Apr 18 14:39:15 2000
   @author    Tom Goodale
   @desc 
   Stuff for when in Parameter checking mode.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_ParamCheck.h,v 1.1 2000/04/18 13:28:08 goodale Exp $
 @@*/

#ifndef _CCTK_PARAMCHECK_H_
#define _CCTK_PARAMCHECK_H_ 1

/* These are just here for speed at the mo.  Don't modify
 * directly outside the flesh.
 */

extern int cctki_paramchecking;
extern int cctki_paramcheck_nprocs;

/* Define these as macros for the moment, but give them an
 * interface like a function so they can can be chenged 
 * to functions later if necessary.
 */

#define CCTK_ParamCheckNProcs() cctki_paramcheck_nprocs
#define CCTK_ParamChecking() cctki_paramchecking

#ifdef __cplusplus
extern "C" 
{
#endif



#ifdef __cplusplus
}
#endif

#endif /* _CCTK_PARAMCHECK_H_ */

