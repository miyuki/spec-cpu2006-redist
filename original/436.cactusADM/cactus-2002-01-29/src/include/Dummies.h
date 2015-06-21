 /*@@
   @header    Dummies.h
   @date      Tue Feb  2 19:01:45 1999
   @author    Tom Goodale
   @desc 
   A file to contain prototypes for some dummy functions until the real ones 
   are written. 
   @enddesc 
 @@*/

#ifndef _DUMMIES_H_
#define _DUMMIES_H_

#ifdef __cplusplus
extern "C" {
#endif

int CCTKi_DummyStorageOn(void *GH, int group);

int CCTKi_DummyStorageOff(void *GH, int group);

int CCTKi_DummyCommunicationOn(void *GH, int group);

int CCTKi_DummyCommunicationOff(void *GH, int group);

int CCTKi_DummyTriggerable(int variable);

int CCTKi_DummyTriggerSaysGo(void *GH, int variable);

int CCTKi_DummyTriggerAction(void *GH, int group);

int CCTKi_DummyCallFunc(void *GH, int language, void *function);


#ifdef __cplusplus
}
#endif


#endif



