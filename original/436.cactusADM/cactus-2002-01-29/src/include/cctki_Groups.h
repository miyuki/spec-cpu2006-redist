 /*@@
   @header    cctki_Groups.h
   @date      Mon Feb  8 14:47:10 1999
   @author    Tom Goodale
   @desc 
   Prototypes and constants for internal group functions.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctki_Groups.h,v 1.6 2000/04/04 10:17:57 allen Exp $
 @@*/

#ifndef _CCTKI_GROUPS_H_
#define _CCTKI_GROUPS_H_

/* Prototypes */

#ifdef __cplusplus 
extern "C" {
#endif

void CCTKi_DumpGroupInfo(void); 

int  CCTKi_CreateGroup
  (const char* gname, 
   const char* thorn,  
   const char* imp,
   const char* gtype,
   const char* vtype,
   const char* gscope,
   int         dimension,
   int         ntimelevels,
   const char* stype,
   const char* dtype,
   const char* size,
   const char* ghostsize,
   int         n_variables,
   ...);

#ifdef __cplusplus 
}
#endif

#endif
