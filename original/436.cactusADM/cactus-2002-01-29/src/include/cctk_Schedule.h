 /*@@
   @header    cctk_schedule.h
   @date      Thu Sep 16 19:05:27 1999
   @author    Tom Goodale
   @desc 
   Routines for creating schedule stuff.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Schedule.h,v 1.9 2001/11/03 19:56:54 allen Exp $
 @@*/

#ifndef _CCTK_SCHEDULE_H_
#define _CCTK_SCHEDULE_H_

typedef enum {LangNone, LangC, LangFortran} cLanguage;

typedef enum {FunctionNoArgs, FunctionOneArg, FunctionStandard} cFunctionType;

typedef struct
{
  cLanguage language;
  
  int (*FortranCaller)(cGH *, void *);

  cFunctionType type;

  int n_SyncGroups;

  int *SyncGroups;

  /* Option Flags */

  int global;

  /* The last items should be considered volatile and may
     not stay here */

  int n_TriggerGroups;
  int *TriggerGroups;
  char *where;
  char *routine;
  char *thorn;

} cFunctionData;

#ifdef __cplusplus
extern "C" 
{
#endif

int CCTK_CallFunction(void *function, 
                      cFunctionData *fdata, 
                      void *data);

int CCTK_ScheduleTraverse(const char *where, 
                           void *GH,   
                           int (*CallFunction)(void *, cFunctionData *, void *));

int CCTK_SchedulePrint(const char *where);
int CCTK_SchedulePrintTimes(const char *where);

cLanguage CCTK_TranslateLanguage(const char *sval);

  /*int CCTK_ScheduleFunction(void *function,
                          const char *name,
                          const char *thorn,
                          const char *implementation,
                          const char *description,
                          const char *where,
                          const char *language,
                          int n_mem_groups,
                          int n_comm_groups,
                          int n_trigger_groups,
                          int n_before,
                          int n_after,
                          int n_while,
                          ...);

int CCTK_ScheduleGroup(const char *name,
                       const char *thorn,
                       const char *implementation,
                       const char *description,
                       const char *where,
                       int n_mem_groups,
                       int n_comm_groups,
                       int n_trigger_groups,
                       int n_before,
                       int n_after,
                       int n_while,
                       ...);

int CCTK_ScheduleGroupStorage(const char *group);

int CCTK_ScheduleGroupComm(const char *group);

int CCTK_ScheduleTraverse(const char *where, 
                          void *GH,   
                          int (*calling_function)(void *, void *, void *));

int CCTK_ScheduleGHInit(void *GH);
  */


#ifdef __cplusplus
}
#endif

#endif /*_CCTK_SCHEDULE_H_*/
