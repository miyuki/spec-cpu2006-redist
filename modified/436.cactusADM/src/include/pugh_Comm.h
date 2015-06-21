 /*@@
   @header    pugh_Comm.h
   @date      Thu Feb  4 11:42:50 1999
   @author    Tom Goodale
   @desc
              Prototypes of functions to be overloaded by PUGH
   @enddesc
   @version   $Header: /cactus/CactusPUGH/PUGH/src/pugh_Comm.h,v 1.16 2001/11/05 15:01:47 tradke Exp $
 @@*/

#ifndef _PUGH_COMM_H_
#define _PUGH_COMM_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

int PUGH_Barrier (const cGH *GH);
int PUGH_SyncGroup (cGH *GH, const char *group);

int PUGH_EnableGroupStorage (cGH *GH, const char *group);
int PUGH_DisableGroupStorage (cGH *GH, const char *group);

int PUGH_EnableGroupComm (cGH *GH, const char *group);
int PUGH_DisableGroupComm (cGH *GH, const char *group);

const int *PUGH_ArrayGroupSize (const cGH *GH, int dir, int vindex, const char *groupname);
int PUGH_QueryGroupStorage (const cGH *GH, int group, const char *groupname);

int PUGH_GroupDynamicData (const cGH *GH, int group, cGroupDynamicData *data);

#ifdef __cplusplus
}
#endif

#endif  /* _PUGH_COMM_H_ */
