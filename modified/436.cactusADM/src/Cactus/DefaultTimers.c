#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      DefaultTimers.c
   @date      Wed Oct 20 16:17:42 1999
   @author    Tom Goodale
   @desc 
   Default Cactus timers
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/DefaultTimers.c,v 1.17 2001/12/11 22:15:10 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Config.h"
#include "cctk_Flesh.h"

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#   if HAVE_TIME_H 
#    include <time.h>
#   endif
# endif
#endif

#include "cctk_Timers.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/DefaultTimers.c,v 1.17 2001/12/11 22:15:10 tradke Exp $";

CCTK_FILEVERSION(main_DefaultTimers_c)


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/* Prototypes for registration functions */

#ifdef HAVE_TIME_GETTIMEOFDAY
static void CCTKi_RegisterTimersGetTimeOfDay(void);
#endif

#ifdef HAVE_TIME_GETRUSAGE
static void CCTKi_RegisterTimersGetrUsage(void);
#endif

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/
int CCTKi_RegisterDefaultTimerFunctions(void);


 /*@@
   @routine    CCTKi_RegisterDefaultTimerFunctions
   @date       Wed Oct 20 18:27:20 1999
   @author     Tom Goodale
   @desc 

   Master flesh timer registration function./
   
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

   @returntype int
   @returndesc 
   0
   @endreturndesc
@@*/
int CCTKi_RegisterDefaultTimerFunctions(void)
{

#ifdef HAVE_TIME_GETTIMEOFDAY
  CCTKi_RegisterTimersGetTimeOfDay();
#endif

#ifdef HAVE_TIME_GETRUSAGE
  CCTKi_RegisterTimersGetrUsage();
#endif

  return 0;
} 


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/



/*********************************************************************
 ****************             Actual timers      *********************
 *********************************************************************/



/*********************************************************************
 ****************   gettimeofday based timer     *********************
 *********************************************************************/

#ifdef HAVE_TIME_GETTIMEOFDAY

#include <unistd.h>

/* A structure to hold the relevent data */
typedef struct 
{
  struct timeval total;
  struct timeval last;
} t_GetTimeOfDayTimer;

static char *GetTimeOfDayHeading = "gettimeofday";
static char *GetTimeOfDayUnits   = "secs";

 /*@@
   @routine    CCTKi_TimerGetTimeOfDayCreate
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Create the timer structure for use with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype void *
   @returndesc 
   timer structure
   @endreturndesc
@@*/
static void *CCTKi_TimerGetTimeOfDayCreate(int timernum)
{
  t_GetTimeOfDayTimer *this;

  timernum = timernum;

  this = malloc(sizeof(t_GetTimeOfDayTimer));

  if(this)
  {
    this->total.tv_sec  = 0;
    this->total.tv_usec = 0;
  }

  return this;
}

 /*@@
   @routine    CCTKi_TimerGetTimeOfDayDestroy
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Destroy the timer structure for use with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     data
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetTimeOfDayDestroy(int timernum, void *data)
{
  timernum = timernum;
  if(data)
  {
    free(data);
  }
}


 /*@@
   @routine    CCTKi_TimerGetTimeOfDayStart
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Start the timer with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetTimeOfDayStart(int timernum, void *idata)
{
  t_GetTimeOfDayTimer *data;

  struct timeval tp;
  struct timezone tzp;

  timernum = timernum;
  data = (t_GetTimeOfDayTimer *)idata;

#ifdef SPEC_CPU
  data->last.tv_sec = 0;
  data->last.tv_usec = 0;
#else
  gettimeofday(&tp, &tzp);

  data->last = tp;
#endif

#ifdef DEBUG_TIMERS
  printf("Starting gettimeofday timer %d\n", timernum);
#endif
}

 /*@@
   @routine    CCTKi_TimerGetTimeOfDayStop
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Stop the timer with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetTimeOfDayStop(int timernum, void *idata)
{
  t_GetTimeOfDayTimer *data;

  struct timeval tp;
  struct timezone tzp;

  timernum = timernum;
  data = (t_GetTimeOfDayTimer *)idata;

#ifdef SPEC_CPU
  data->total.tv_sec = 0;
  data->total.tv_usec= 0;
#else
  gettimeofday(&tp, &tzp);

  data->total.tv_sec  += (tp.tv_sec  - data->last.tv_sec);
  data->total.tv_usec += (tp.tv_usec - data->last.tv_usec);
#endif

#ifdef DEBUG_TIMERS
  printf("Stopping gettimeofday timer %d\n", timernum);
#endif
}

 /*@@
   @routine    CCTKi_TimerGetTimeOfDayReset
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Reset the timer with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetTimeOfDayReset(int timernum, void *idata)
{
  t_GetTimeOfDayTimer *data;

  timernum = timernum;
  data = (t_GetTimeOfDayTimer *)idata;

  data->last.tv_sec  = 0;
  data->last.tv_usec  = 0;
  data->total.tv_sec  = 0;
  data->total.tv_usec  = 0;

}

 /*@@
   @routine    CCTKi_TimerGetTimeOfDayGet
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Get the time recorded with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     vals
   @vdesc   Timer value structure
   @vtype   cTimerVal
   @vio     out
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetTimeOfDayGet(int timernum, void *idata, cTimerVal *vals)
{
  t_GetTimeOfDayTimer *data;

  timernum = timernum;
  data = (t_GetTimeOfDayTimer *)idata;

  vals[0].type    = val_double;
  vals[0].heading = GetTimeOfDayHeading;
  vals[0].units   = GetTimeOfDayUnits;
  vals[0].val.d   = data->total.tv_sec + (double)data->total.tv_usec/1000000.0;
}

 /*@@
   @routine    CCTKi_TimerGetTimeOfDaySet
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Set the time for a gettimeofday function based timer.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     vals
   @vdesc   Timer value structure
   @vtype   cTimerVal
   @vio     in
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetTimeOfDaySet(int timernum, void *idata, cTimerVal *vals)
{
  t_GetTimeOfDayTimer *data;

  timernum = timernum;
  data = (t_GetTimeOfDayTimer *)idata;

  data->total.tv_sec  = (long)vals[0].val.d;
  data->total.tv_usec = (long)(1000000*vals[0].val.d-data->total.tv_sec);
}

 /*@@
   @routine    CCTKi_RegisterTimersGetTimeOfDay
   @date       Wed Oct 20 18:32:17 1999
   @author     Tom Goodale
   @desc 
   Register all the timer functions associated with the gettimeofday function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
static void CCTKi_RegisterTimersGetTimeOfDay(void)
{
  cClockFuncs functions;

  functions.n_vals  = 1;
  functions.create  = CCTKi_TimerGetTimeOfDayCreate;
  functions.destroy = CCTKi_TimerGetTimeOfDayDestroy;
  functions.start   = CCTKi_TimerGetTimeOfDayStart;
  functions.stop    = CCTKi_TimerGetTimeOfDayStop;
  functions.reset   = CCTKi_TimerGetTimeOfDayReset;
  functions.get     = CCTKi_TimerGetTimeOfDayGet;
  functions.set     = CCTKi_TimerGetTimeOfDaySet;

  CCTK_ClockRegister("GetTimeOfDay", &functions);
}
    
#endif /* HAVE_TIME_GETTIMEOFDAY */


/*********************************************************************
 ****************       getrusage based timers     *******************
 *********************************************************************/

#ifdef HAVE_TIME_GETRUSAGE

#include <sys/resource.h>

/* A structure to hold the relevent data */
typedef struct 
{
  struct timeval total;
  struct timeval last;
} t_GetrUsageTimer;

static char *GetrUsageHeading = "getrusage";
static char *GetrUsageUnits   = "secs";

 /*@@
   @routine    CCTKi_TimerGetrUsageCreate
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Create the timer structure for use with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 

   @returntype void *
   @returndesc 
   timer structure
   @endreturndesc

@@*/
static void *CCTKi_TimerGetrUsageCreate(int timernum)
{
  t_GetrUsageTimer *this;

  timernum = timernum;
  this = malloc(sizeof(t_GetrUsageTimer));

  if(this)
  {
    this->total.tv_sec  = 0;
    this->total.tv_usec = 0;
  }

  return this;
}

 /*@@
   @routine    CCTKi_TimerGetrUsageDestroy
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Destroy the timer structure for use with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     data
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetrUsageDestroy(int timernum, void *data)
{
  timernum = timernum;
  if(data)
  {
    free(data);
  }
}


 /*@@
   @routine    CCTKi_TimerGetrUsageStart
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Start the timer with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetrUsageStart(int timernum, void *idata)
{
  t_GetrUsageTimer *data;

  struct rusage ru;

  timernum = timernum;
  data = (t_GetrUsageTimer *)idata;

#ifdef SPEC_CPU
  data->last.tv_sec =0;
  data->last.tv_usec=0;
#else
  getrusage(RUSAGE_SELF, &ru);

  data->last = ru.ru_utime;
#endif

#ifdef DEBUG_TIMERS
  printf("Starting getrusage timer %d\n", timernum);
#endif
}

 /*@@
   @routine    CCTKi_TimerGetrUsageStop
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Stop the timer with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetrUsageStop(int timernum, void *idata)
{
  t_GetrUsageTimer *data;

  struct rusage ru;

  timernum = timernum;
  data = (t_GetrUsageTimer *)idata;

#ifdef SPEC_CPU
  data->total.tv_sec = 0;
  data->total.tv_usec= 0;
#else 
  getrusage(RUSAGE_SELF, &ru);

  data->total.tv_sec += ru.ru_utime.tv_sec - data->last.tv_sec;
  data->total.tv_usec += ru.ru_utime.tv_usec - data->last.tv_usec;
#endif

#ifdef DEBUG_TIMERS
  printf("Starting getrusage timer %d\n", timernum);
#endif
}

 /*@@
   @routine    CCTKi_TimerGetrUsageReset
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Reset the timer with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetrUsageReset(int timernum, void *idata)
{
  t_GetrUsageTimer *data;

  timernum = timernum;
  data = (t_GetrUsageTimer *)idata;

  data->total.tv_sec  = 0;
  data->total.tv_usec = 0;

}

 /*@@
   @routine    CCTKi_TimerGetrUsageGet
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Get the time recorded with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     vals
   @vdesc   Timer value structure
   @vtype   cTimerVal
   @vio     out
   @vcomment 
 
   @endvar 


@@*/
static void CCTKi_TimerGetrUsageGet(int timernum, void *idata, cTimerVal *vals)
{
  t_GetrUsageTimer *data;

  data = (t_GetrUsageTimer *)idata;

  timernum = timernum;

  vals[0].type    = val_double;
  vals[0].heading = GetrUsageHeading;
  vals[0].units   = GetrUsageUnits;
  vals[0].val.d   = data->total.tv_sec + (double)data->total.tv_usec/1000000.0;

}

 /*@@
   @routine    CCTKi_TimerGetrUsageSet
   @date       Wed Oct 20 18:28:19 1999
   @author     Tom Goodale
   @desc 
   Set the time for a getrusage function based timer.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     timernum
   @vdesc   timer number
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     idata
   @vdesc   timer data
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 
   @var     vals
   @vdesc   Timer value structure
   @vtype   cTimerVal
   @vio     in
   @vcomment 
 
   @endvar 

@@*/
static void CCTKi_TimerGetrUsageSet(int timernum, void *idata, cTimerVal *vals)
{
  t_GetrUsageTimer *data;

  timernum = timernum;

  data = (t_GetrUsageTimer *)idata;

  data->total.tv_sec  = (long)vals[0].val.d;
  data->total.tv_usec = (long)(1000000*vals[0].val.d-data->total.tv_sec);
}

 /*@@
   @routine    CCTKi_RegisterTimersGetrUsage
   @date       Wed Oct 20 18:32:17 1999
   @author     Tom Goodale
   @desc 
   Register all the timer functions associated with the getrusage function.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
static void CCTKi_RegisterTimersGetrUsage(void)
{
  cClockFuncs functions;

  functions.n_vals  = 1;
  functions.create  = CCTKi_TimerGetrUsageCreate;
  functions.destroy = CCTKi_TimerGetrUsageDestroy;
  functions.start   = CCTKi_TimerGetrUsageStart;
  functions.stop    = CCTKi_TimerGetrUsageStop;
  functions.reset   = CCTKi_TimerGetrUsageReset;
  functions.get     = CCTKi_TimerGetrUsageGet;
  functions.set     = CCTKi_TimerGetrUsageSet;

  CCTK_ClockRegister("GetrUsage", &functions);

}

#endif /* HAVE_TIME_GETRUSAGE */
