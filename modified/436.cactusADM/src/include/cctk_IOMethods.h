 /*@@
   @header    cctk_IOMethods.h
   @date      1999/07/30
   @author    Gabrielle Allen
   @desc
              header file for handling IO methods
   @enddesc
   @version   $Version: cctk_IOMethods.h,v 1.3 2001/10/23 15:40:08 allen Exp $
 @@*/

#ifndef _CCTK_IOMETHODS_H_
#define _CCTK_IOMETHODS_H_

#ifdef __cplusplus
extern "C" {
#endif

struct IOMethod
{
  const char *implementation;
  const char *name;
  int (*OutputGH)      (const cGH *GH);
  int (*OutputVarAs)   (const cGH *GH, const char *vname, const char *alias);
  int (*TriggerOutput) (const cGH *GH, int vindex);
  int (*TimeToOutput)  (const cGH *GH, int vindex);
};

#define CCTK_RegisterIOMethod(a) CCTKi_RegisterIOMethod (CCTK_THORNSTRING, a)
int CCTKi_RegisterIOMethod (const char *thorn, const char *name);
int CCTK_RegisterIOMethodOutputGH (int handle, int (*OutputGH) (const cGH *GH));
int CCTK_RegisterIOMethodTimeToOutput (int handle,
                                       int (*TimeToOutput) (const cGH *GH,
                                                            int vindex));
int CCTK_RegisterIOMethodTriggerOutput (int handle,
                                        int (*TriggerOutput) (const cGH *GH,
                                                              int vindex));
int CCTK_RegisterIOMethodOutputVarAs (int handle,
                                      int (*OutputVarAs) (const cGH *GH,
                                                          const char *vname,
                                                          const char *alias));

const char *CCTK_IOMethodImplementation (int handle);

const char *CCTK_IOMethod (int handle);

int CCTK_NumIOMethods (void);

#ifdef __cplusplus
}
#endif

#endif  /* _CCTK_IOMETHODS_H_ */
