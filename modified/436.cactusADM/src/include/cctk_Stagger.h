 /*@@
   @header    cctk_Stagger.h
   @date      Thu Jan 20 2000
   @author    Gerd Lanfermann
   @desc 
   Prototypes and constants for stagger functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Stagger.h,v 1.4 2000/04/19 18:10:31 lanfer Exp $
 @@*/

#ifndef _CCTK_STAGGER_H_
#define _CCTK_STAGGER_H_

#ifdef __cplusplus
extern "C" {
#endif

int CCTK_GroupStaggerIndexGI(int gindex);
int CCTK_GroupStaggerIndexGN(const char *gname);
int CCTK_StaggerIndex(const char *stype);
int CCTK_StaggerDirIndex(int dir, int sc);
int CCTK_StaggerDirArray(int *dindex , int dim, int sindex) ;
int CCTK_GroupStaggerDirArrayGI(int *dindex, int dim, int gi);
int CCTK_StaggerDirName(int dir, const char *stype);

int CCTKi_ParseStaggerString(int dim,
                             const char *imp, 
                             const char *gname,
                             const char *stype);

#ifdef __cplusplus
}
#endif

#endif
