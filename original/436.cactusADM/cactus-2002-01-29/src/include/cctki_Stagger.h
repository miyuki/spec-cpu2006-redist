 /*@@
   @header    cctki_Stagger.h
   @date      Thu Jan 20 2000
   @author    Gerd Lanfermann
   @desc 
   Prototypes and constants for stagger functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctki_Stagger.h,v 1.4 2000/02/24 09:52:17 lanfer Exp $
 @@*/

#ifndef _CCTKI_STAGGER_H_
#define _CCTKI_STAGGER_H_

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTKi_ParseStaggerString(int dim,  
                             const char *imp, 
                             const char *gname,  
                             const char *stype); 

#ifdef __cplusplus
}
#endif

/* number of implemented staggerings */
#define CCTK_NUM_STAGGER   3

/* number of staggerings (3); stagger flags: yes/no, */
#define CCTK_NSTAG         3
#define CCTK_NO_STAGGER    0
#define CCTK_STAGGER       1

/* stagger code in one direction */
#define CCTK_STAG_M        0
#define CCTK_STAG_C        1
#define CCTK_STAG_P        2

/* stagger code of three directions */
#define CCTK_STAG_MMM 0
#define CCTK_STAG_CMM 1
#define CCTK_STAG_PMM 2

#define CCTK_STAG_MCM 3
#define CCTK_STAG_CCM 4
#define CCTK_STAG_PCM 5

#define CCTK_STAG_MPM 6
#define CCTK_STAG_CPM 7
#define CCTK_STAG_PPM 8

#define CCTK_STAG_MMC 9
#define CCTK_STAG_CMC 11
#define CCTK_STAG_PMC 12

#define CCTK_STAG_MCC 13
#define CCTK_STAG_CCC 14
#define CCTK_STAG_PCC 15

#define CCTK_STAG_MPC 16
#define CCTK_STAG_CPC 17
#define CCTK_STAG_PPC 18

#define CCTK_STAG_MMP 19
#define CCTK_STAG_CMP 20
#define CCTK_STAG_PMP 21

#define CCTK_STAG_MCP 22
#define CCTK_STAG_CCP 23
#define CCTK_STAG_PCP 24

#define CCTK_STAG_MPP 25
#define CCTK_STAG_CPP 26
#define CCTK_STAG_PPP 27


#endif /* _CCTKI_STAGGER_H_ */
