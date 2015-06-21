 /*@@
   @header    cctki_ActiveThorns.h
   @date      Sun Jul  4 17:39:50 1999
   @author    Tom Goodale
   @desc 
   
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctki_ActiveThorns.h,v 1.4 2001/05/22 11:26:59 goodale Exp $
 @@*/

#ifndef __CCTKI_ACTIVETHORNS_H_
#define __CCTKI_ACTIVETHORNS_H_

#ifdef __cplusplus 
extern "C" {
#endif

struct iFuncList
{
  const char *keyword;
  const char *signature;
  void (*func)(void);
};

union iAttributeData
{
  const char **StringList;
  const struct iFuncList *FuncList;
};

struct iAttributeList
{
  const char *attribute;
  union iAttributeData AttributeData;
};

int CCTKi_RegisterThorn(const struct iAttributeList *attributes);
int CCTKi_ActivateThorn(const char *name);
int CCTKi_PrintThorns(FILE *file, const char *format, int active);
int CCTKi_PrintImps(FILE *file, const char *format, int active);

int CCTKi_ActivateThorns(const char *thornlist);

#ifdef __cplusplus 
}
#endif

#endif /* _CCTKI_ACTIVETHORNS_H_ */
