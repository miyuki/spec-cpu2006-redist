 /*@@
   @header    cctk_Termination.h
   @date      Tue 24 Apr 2001
   @author    Thomas Radke
   @desc
              Prototypes of CCTK termination functions.
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/cctk_Termination.h,v 1.3 2001/09/20 21:44:04 tradke Exp $
 @@*/

#ifndef _CCTK_TERMINATION_H_
#define _CCTK_TERMINATION_H_ 1

#ifdef __cplusplus
extern "C"
{
#endif

int CCTK_TerminationReached (const cGH *GH);
void CCTK_TerminateNext (const cGH *GH);

#ifdef __cplusplus
}
#endif

#endif /* _CCTK_TERMINATION_H_ */
