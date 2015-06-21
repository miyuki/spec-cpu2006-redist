 /*@@
   @header    cctki_Names.h
   @date      Thu Mar  9 12:11:20 2000
   @author    Tom Goodale
   @desc 
   
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctki_Names.h,v 1.1 2000/03/09 11:23:21 goodale Exp $
 @@*/

#ifndef _CCTKI_NAMES_H_
#define _CCTKI_NAMES_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTKi_NamesStoreGroup(const char *gname, int gnum);
int CCTKi_NamesStoreVariable(const char *name, int vnum, int gnum);
int CCTKi_NamesRetrieveGroupNum(const char *gname, int *gnum);
int CCTKi_NamesRetrieveVariableNum(const char *name, int *vnum, int *gnum);

#ifdef __cplusplus
}
#endif


#endif /* _CCTKI_NAMES_H_ */
