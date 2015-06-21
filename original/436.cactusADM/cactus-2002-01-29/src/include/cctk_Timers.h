 /*@@
   @header    cctk_Timers.h
   @date      Thu Oct  8 18:31:45 1998
   @author    Tom Goodale
   @desc 
   Timer stuff
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Timers.h,v 1.5 2001/12/29 19:45:57 allen Exp $
 @@*/

#ifndef _CCTK_TIMERS_H_
#define _CCTK_TIMERS_H_

/*  Typedefs */

typedef enum {val_none, val_int, val_long, val_double} cTimerValType;

typedef struct
{
  cTimerValType type;
  const char *heading;
  const char *units;
  union
  {
    int        i;
    long int   l;
    double     d;
  } val;
} cTimerVal;

typedef struct
{
  int n_vals;
  cTimerVal *vals;
} cTimerData;


typedef struct
{
  const char *name;
  int  n_vals;
  void *(*create)(int);
  void (*destroy)(int, void *);
  void (*start)(int, void *);
  void (*stop)(int, void *);
  void (*reset)(int, void *);
  void (*get)(int, void *, cTimerVal *);
  void (*set)(int, void *, cTimerVal *);
} cClockFuncs;



/* Function prototypes */

#ifdef __cplusplus
extern "C" {
#endif

int CCTK_ClockRegister(const char *name, const cClockFuncs *functions);
int CCTK_NumTimers (void);
int CCTK_NumClocks (void);
const char *CCTK_TimerName (int timer_handle);
const char *CCTK_ClockName (int clock_handle);
int CCTK_ClockHandle(const char *nclock);
int CCTK_TimerCreate(const char *name);
int CCTK_TimerCreateI(void);
int CCTK_TimerDestroy(const char *name);
int CCTK_TimerDestroyI(int this_timer);
int CCTK_TimerStart(const char *name);
int CCTK_TimerStartI(int this_timer);
int CCTK_TimerStop(const char *name);
int CCTK_TimerStopI(int this_timer);
int CCTK_TimerReset(const char *name);
int CCTK_TimerResetI(int this_timer);
int CCTK_Timer(const char *name, cTimerData *info);
int CCTK_TimerI(int this_timer, cTimerData *info);

cTimerData *CCTK_TimerCreateData(void);
int CCTK_TimerDestroyData(cTimerData *info);

int CCTK_TimerPrintData (const char *ntimer, const char *nclock);
int CCTK_TimerPrintDataI(int this_timer, int this_clock);

#ifdef __cplusplus
}
#endif

#endif
