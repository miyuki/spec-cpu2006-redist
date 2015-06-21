#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      CactusTimers.c
   @date      Thu Oct  8 18:30:28 1998
   @author    Tom Goodale
   @desc
              Cactus Timer stuff.
   @enddesc
   @version   $Id: CactusTimers.c,v 1.24 2002/01/02 19:36:20 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_Timers.h"
#include "cctk_WarnLevel.h"
#include "StoreHandledData.h"
#include "cctk_FortranString.h"


static const char *rcsid = "$Header: /cactus/Cactus/src/util/CactusTimers.c,v 1.24 2002/01/02 19:36:20 tradke Exp $";

CCTK_FILEVERSION(util_CactusTimers_c)

/********************************************************************
 ***************     Fortran Wrapper Prototypes   *******************
 ********************************************************************/
void CCTK_FCALL cctk_numtimers_
                           (int *ierr);
void CCTK_FCALL cctk_numclocks_
                           (int *ierr);
void CCTK_FCALL cctk_timercreate_
                           (int *timer_index, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_timercreatei_
                           (int *ierr);
void CCTK_FCALL cctk_timerdestroy_
                           (int *ierr, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_timerdestroyi_
                           (int *ierr, int *this_timer);
void CCTK_FCALL cctk_timerstart_
                           (int *ierr, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_timerstarti_
                           (int *ierr, int *this_timer);
void CCTK_FCALL cctk_timerstop_
                           (int *ierr, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_timerstopi_
                           (int *ierr, int *this_timer);
void CCTK_FCALL cctk_timerreset_
                           (int *ierr, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_timerreseti_
                           (int *ierr, int *this_timer);
void CCTK_FCALL cctk_timerprintdatai_
                           (int *ierr, int *this_timer, int *this_clock);
void CCTK_FCALL cctk_timerprintdata_
                           (int *ierr, TWO_FORTSTRING_ARG);


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/
typedef struct
{
  void **data;
} t_Timer;


/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/
static int  CCTKi_TimerCreate (const char *timername);
static void CCTKi_TimerDestroy (int this_timer, t_Timer *timer);
static void CCTKi_TimerStart (int this_timer, t_Timer *timer);
static void CCTKi_TimerStop (int this_timer, t_Timer *timer);
static void CCTKi_TimerReset (int this_timer, t_Timer *timer);
static void CCTKi_Timer (int this_timer, t_Timer *timer, cTimerData *info);


/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/
static int n_clocks = 0;
static cHandledData *clocks = NULL;

/* The total number of clock values. */
static int n_clock_vals = 0;

static int n_timers = 0;
static cHandledData *timers = NULL;


 /*@@
   @routine    CCTK_ClockRegister
   @date       Wed Sep  1 10:09:27 1999
   @author     Tom Goodale
   @desc
               Registers a new timer function (clock).
   @enddesc
   @calls      Util_GetHandledData
               Util_NewHandle

   @var        clockname
   @vdesc      name of the clock to register
   @vtype      const char *
   @vio        in
   @endvar
   @var        functions
   @vdesc      callback routines of the clock to register
   @vtype      const cClockFuncs *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the handle for the new clock
   @endreturndesc
@@*/
int CCTK_ClockRegister (const char *clockname, const cClockFuncs *functions)
{
  void *tmp;
  int handle;
  t_Timer *timer;
  cClockFuncs *newfuncs;


  newfuncs = (cClockFuncs *) malloc (sizeof (cClockFuncs));
  if (newfuncs)
  {
    newfuncs->name = clockname;
    newfuncs->n_vals = functions->n_vals;
    newfuncs->create = functions->create;
    newfuncs->destroy = functions->destroy;
    newfuncs->start = functions->start;
    newfuncs->stop = functions->stop;
    newfuncs->reset = functions->reset;
    newfuncs->get = functions->get;
    newfuncs->set = functions->set;
  }

  /* Add this clock to all existing timers */
  for (handle = 0; handle < n_timers; handle++)
  {
    timer = (t_Timer *) Util_GetHandledData (timers, handle);

    tmp = realloc (timer->data, (n_clocks + 1) * sizeof (void *));
    if (tmp)
    {
      timer->data = (void **) tmp;
      timer->data[n_clocks] = functions->create (handle);
    }
  }

  /* Add this clock to the clock database */
  handle = Util_NewHandle (&clocks, clockname, newfuncs);
  n_clocks++;
  n_clock_vals += functions->n_vals;

  return (handle);
}


 /*@@
   @routine    CCTK_ClockName
   @date       Sat 29 Dec 2001
   @author     Gabrielle Allen
   @desc
               Return the name of a clock from a handle
   @enddesc

   @var        handle
   @vdesc      handle for the clock
   @vtype      int
   @vio        in
   @endvar

   @returntype const char *
   @returndesc
               Name of clock, or NULL if handle is invalid
   @endreturndesc
@@*/
const char *CCTK_ClockName (int handle)
{
  const cClockFuncs *funcs;


  funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);

  return (funcs ? funcs->name : NULL);
}


 /*@@
   @routine    CCTK_ClockHandle
   @date       Sat 29 Dec 2001
   @author     Gabrielle Allen
   @desc
               Return the name of a clock from its handle
   @enddesc

   @var        clockname
   @vdesc      name for the clock
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               positive for the handle of the clock, or -1 if name isn't known
   @endreturndesc
@@*/
int CCTK_ClockHandle (const char *clockname)
{
  int handle;
  const cClockFuncs *funcs;


  for (handle = CCTK_NumClocks () - 1; handle >= 0; handle--)
  {
    funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);
    if (funcs && strcmp (funcs->name, clockname) == 0)
    {
      break;
    }
  }

  return (handle);
}


 /*@@
   @routine    CCTK_NumClocks
   @date       Sat 29 Dec 2001
   @author     Gabrielle Allen
   @desc
               Return the total number of clocks
   @enddesc

   @returntype int
   @returndesc
               the total number of Cactus clocks
   @endreturndesc
@@*/
int CCTK_NumClocks (void)
{
  return (n_clocks);
}

void CCTK_FCALL cctk_numclocks_
                           (int *nclocks)
{
  *nclocks = CCTK_NumClocks ();
}


 /*@@
   @routine    CCTK_NumTimers
   @date       Tue 3 Jul 2001
   @author     Thomas Radke
   @desc
               Return the total number of timers.
   @enddesc

   @returntype int
   @returndesc
               the total number of Cactus timers
   @endreturndesc
@@*/
int CCTK_NumTimers (void)
{
  return (n_timers);
}

void CCTK_FCALL cctk_numtimers_
                           (int *ntimers)
{
  *ntimers = CCTK_NumTimers ();
}


 /*@@
   @routine    CCTK_TimerName
   @date       Tue 3 Jul 2001
   @author     Thomas Radke
   @desc
               Return the name of a Cactus timer given by its handle.
   @enddesc

   @var        timer_handle
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               the name of the Cactus timer, or NULL if no timer with that
               handle exists
   @endreturndesc
@@*/
const char *CCTK_TimerName (int timer_handle)
{
  const char *retval;


  retval = Util_GetHandleName (timers, timer_handle);

  return (retval);
}


 /*@@
   @routine    CCTK_TimerCreate
   @date       Wed Sep  1 10:09:57 1999
   @author     Tom Goodale
   @desc
               Creates a new timer with a given name
   @enddesc
   @calls      CCTKi_TimerCreate

   @var        timer name
   @vdesc      name for the timer 
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTKi_TimerCreate
   @endreturndesc
@@*/
int CCTK_TimerCreate (const char *timername)
{
  int retval;


  retval = CCTKi_TimerCreate (timername);

  return (retval);
}

void CCTK_FCALL cctk_timercreate_
                           (int *timer_index, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (timername)
  *timer_index = CCTK_TimerCreate (timername);
  free (timername);
}


 /*@@
   @routine    CCTK_TimerCreateI
   @date       Fri Oct 22 10:21:14 1999
   @author     Tom Goodale
   @desc
               Creates a timer with a unique name.
   @enddesc
   @calls      CCTKi_TimerCreate

   @returntype int
   @returndesc
               return code of @seeroutine CCTKi_TimerCreate
   @endreturndesc
@@*/
int CCTK_TimerCreateI (void)
{
  int retval;
  char timername[40];


  sprintf (timername, "UNNAMED TIMER %5d", n_timers);
  retval = CCTKi_TimerCreate (timername);

  return (retval);
}

void CCTK_FCALL cctk_timercreatei_
                           (int *ierr)
{
  *ierr = CCTK_TimerCreateI ();
}


 /*@@
   @routine    CCTKi_TimerCreate
   @date       Wed Sep  1 10:09:57 1999
   @author     Tom Goodale
   @desc
               Creates a new timer with the name given
   @enddesc

   @var        timername
   @vdesc      name of the timer 
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               >= 0 for the handle of the new timer, or<BR>
               -1   if timer with this name already exists<BR>
               -2   couldn't allocated memory for new timer structure
   @endreturndesc
@@*/
static int CCTKi_TimerCreate (const char *timername)
{
  int retval;
  t_Timer *timer;
  const cClockFuncs *funcs;
  int this_timer;
  int handle;


  if (Util_GetHandle (timers, timername, (void **) &timer) >= 0)
  {
    /* Handle already exists */
    retval = -1;
  }
  else
  {
    timer = (t_Timer *) malloc (sizeof (t_Timer));

    if (timer)
    {
      timer->data = (void **) malloc (n_clocks * sizeof (void *));

      if (timer->data)
      {
        /* Store the data structure for this timer */
        this_timer = Util_NewHandle (&timers, timername, timer);
        n_timers++;

        /* Create the timer info for this timer */
        for (handle = 0; handle < n_clocks; handle++)
        {
          funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);

          timer->data[handle] = funcs->create (this_timer);
        }
        retval = this_timer;
      }
      else
      {
        free (timer);
        retval = -2;
      }
    }
    else
    {
      retval = -2;
    }
  }

  return (retval);
}


 /*@@
   @routine    CCTK_TimerDestroy
   @date       Wed Sep  1 10:10:20 1999
   @author     Tom Goodale
   @desc
               Destroys an old timer
   @enddesc
   @calls      CCTKi_TimerDestroy

   @var        timername
   @vdesc      name of timer
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 for success, or -1 if timer doesn't exist
   @endreturndesc
@@*/
int CCTK_TimerDestroy (const char *timername)
{
  t_Timer *timer;
  int this_timer;


  this_timer = Util_GetHandle (timers,  timername, (void **) &timer);
  if (this_timer >= 0)
  {
    CCTKi_TimerDestroy (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerDestroy: Timer '%s' not found", timername);
  }

  return (this_timer >= 0 ? 0 : -1);
}

void CCTK_FCALL cctk_timerdestroy_
                           (int *ierr, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (timername)
  *ierr = CCTK_TimerDestroy (timername);
  free (timername);
}


 /*@@
   @routine    CCTK_TimerDestroyI
   @date       Thu Oct 21 14:12:51 1999
   @author     Tom Goodale
   @desc
               Destroys a timer by its handle index
   @enddesc
   @calls      CCTKi_TimerDestroy

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 for success, or -1 if timer handle is invalid
   @endreturndesc
@@*/
int CCTK_TimerDestroyI (int this_timer)
{
  t_Timer *timer;


  timer = Util_GetHandledData (timers, this_timer);
  if (timer)
  {
    CCTKi_TimerDestroy (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerDestroyI: Timer %d not found",this_timer);
  }

  return (timer ? 0 : -1);
}

void CCTK_FCALL cctk_timerdestroyi_
                           (int *ierr, int *this_timer)
{
  *ierr = CCTK_TimerDestroyI (*this_timer);
}


 /*@@
   @routine    CCTKi_TimerDestroy
   @date       Thu Oct 21 14:14:58 1999
   @author     Tom Goodale
   @desc
               Internal function which destroys a timer.
   @enddesc

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar
   @var        timer
   @vdesc      timer structure
   @vtype      t_Timer *
   @vio        in
   @endvar
@@*/
static void CCTKi_TimerDestroy (int this_timer, t_Timer *timer)
{
  int handle;
  const cClockFuncs *funcs;


  if (timer && timer->data)
  {
    /* Destroy the timer info for this timer */
    for (handle = 0; handle < n_clocks; handle++)
    {
      funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);
      funcs->destroy (this_timer, timer->data[handle]);
    }
    free (timer->data);
    free (timer);
    Util_DeleteHandle (timers, this_timer);
    n_timers--;
  }
}


 /*@@
   @routine    CCTK_TimerStart
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Starts a timer counting.
   @enddesc
   @calls      CCTKi_TimerStart

   @var        timer name
   @vdesc      name for the timer 
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer doesn't exist
   @endreturndesc
@@*/
int CCTK_TimerStart (const char *timername)
{
  t_Timer *timer;
  int this_timer;


  this_timer = Util_GetHandle (timers, timername, (void **) &timer);
  if (this_timer >= 0)
  {
    CCTKi_TimerStart (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerStart: Timer %s not found",timername);
  }

  return (this_timer >= 0 ? 0 : -1);
}

void CCTK_FCALL cctk_timerstart_
                           (int *ierr, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (timername)
  *ierr = CCTK_TimerStart (timername);
  free (timername);
}


 /*@@
   @routine    CCTK_TimerStartI
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Starts a timer counting given by its handle.
   @enddesc
   @calls      CCTKi_TimerStart

   @var        this_timer
   @vdesc      handle for the timer 
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer handle is invalid
   @endreturndesc
@@*/
int CCTK_TimerStartI (int this_timer)
{
  t_Timer *timer;


  timer = Util_GetHandledData (timers, this_timer);
  if (timer)
  {
    CCTKi_TimerStart (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerStartI: Timer %d not found",this_timer);
  }

  return (timer ? 0 : -1);
}

void CCTK_FCALL cctk_timerstarti_
                           (int *ierr, int *this_timer)
{
  *ierr = CCTK_TimerStartI (*this_timer);
}


 /*@@
   @routine    CCTKi_TimerStart
   @date       Thu Oct 21 14:14:58 1999
   @author     Tom Goodale
   @desc
               Internal function which starts a timer.
   @enddesc

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar
   @var        timer
   @vdesc      timer structure
   @vtype      t_Timer *
   @vio        in
   @endvar
@@*/
static void CCTKi_TimerStart (int this_timer, t_Timer *timer)
{
  const cClockFuncs *funcs;
  int handle;


  if (timer && timer->data)
  {
    /* Start the timer info for this timer */
    for (handle = 0; handle < n_clocks; handle++)
    {
      funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);
      funcs->start (this_timer, timer->data[handle]);
    }
  }
}


 /*@@
   @routine    CCTK_TimerStop
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Stops a timer counting.
   @enddesc
   @calls      CCTKi_TimerStop

   @var        timername
   @vdesc      name of the timer
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer doesn't exist
   @endreturndesc
@@*/
int CCTK_TimerStop (const char *timername)
{
  t_Timer *timer;
  int this_timer;


  this_timer = Util_GetHandle (timers, timername, (void **)&timer);
  if (this_timer >= 0)
  {
    CCTKi_TimerStop (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerStop: Timer %s not found",timername);
  }

  return (this_timer >= 0 ? 0 : -1);
}

void CCTK_FCALL cctk_timerstop_
                           (int *ierr, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (timername)
  *ierr = CCTK_TimerStop (timername);
  free (timername);
}


 /*@@
   @routine    CCTK_TimerStopI
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Stops a timer counting given by its handle.
   @enddesc
   @calls      CCTKi_TimerStop

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer handle is invalid
   @endreturndesc
@@*/
int CCTK_TimerStopI (int this_timer)
{
  t_Timer *timer;


  timer = Util_GetHandledData (timers, this_timer);
  if (timer)
  {
    CCTKi_TimerStop (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerStopI: Timer %d not found",this_timer);
  }

  return (timer ? 0 : -1);
}

void CCTK_FCALL cctk_timerstopi_
                           (int *ierr, int *this_timer)
{
  *ierr = CCTK_TimerStopI (*this_timer);
}


 /*@@
   @routine    CCTKi_TimerStop
   @date       Thu Oct 21 14:14:58 1999
   @author     Tom Goodale
   @desc
               Internal function which stops a timer.
   @enddesc

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar
   @var        timer
   @vdesc      timer structure
   @vtype      t_Timer *
   @vio        in
   @endvar
@@*/
static void CCTKi_TimerStop (int this_timer, t_Timer *timer)
{
  const cClockFuncs *funcs;
  int handle;

  if (timer && timer->data)
  {
    /* Stop the timer info for this timer */
    for (handle = 0; handle < n_clocks; handle++)
    {
      funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);
      funcs->stop (this_timer, timer->data[handle]);
    }
  }
}


 /*@@
   @routine    CCTK_TimerReset
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Resets a timer.
   @enddesc
   @calls      CCTKi_TimerReset

   @var        timername
   @vdesc      name of the timer
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer doesn't exist
   @endreturndesc
@@*/
int CCTK_TimerReset (const char *timername)
{
  t_Timer *timer;
  int this_timer;


  this_timer = Util_GetHandle (timers, timername, (void **) &timer);
  if (this_timer >= 0)
  {
    CCTKi_TimerReset (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerReset: Timer %s not found",timername);
  }

  return (this_timer >= 0 ? 0 : -1);
}

void CCTK_FCALL cctk_timerreset_
                           (int *ierr, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (timername)
  *ierr = CCTK_TimerReset (timername);
  free (timername);
}


 /*@@
   @routine    CCTK_TimerResetI
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Resets a timer given by its handle.
   @enddesc
   @calls      CCTKi_TimerReset

   @var        this_timer
   @vdesc      handle for the timer
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if handle is invalid
   @endreturndesc
@@*/
int CCTK_TimerResetI (int this_timer)
{
  t_Timer *timer;


  timer = Util_GetHandledData (timers, this_timer);
  if (timer)
  {
    CCTKi_TimerReset (this_timer, timer);
  }
  else
  {
    CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                "CCTK_TimerResetI: Timer %d not found",this_timer);
  }

  return (timer ? 0 : -1);
}

void CCTK_FCALL cctk_timerreseti_
                           (int *ierr, int *this_timer)
{
  *ierr = CCTK_TimerResetI (*this_timer);
}


 /*@@
   @routine    CCTKi_TimerReset
   @date       Thu Oct 21 14:14:58 1999
   @author     Tom Goodale
   @desc
               Internal function which resets a timer.
   @enddesc

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar
   @var        timer
   @vdesc      timer structure
   @vtype      t_Timer *
   @vio        in
   @endvar
@@*/
static void CCTKi_TimerReset (int this_timer, t_Timer *timer)
{
  const cClockFuncs *funcs;
  int handle;


  if (timer && timer->data)
  {
    /* Reset the timer info for this timer */
    for (handle = 0; handle < n_clocks; handle++)
    {
      funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);
      funcs->reset (this_timer, timer->data[handle]);
    }
  }
}


 /*@@
   @routine    CCTK_Timer
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Gets the values of a timer.
   @enddesc
   @calls      CCTKi_Timer

   @var        timername
   @vdesc      name of the timer
   @vtype      const char *
   @vio        in
   @endvar
   @var        info
   @vdesc      user-supplied info structure to store the timer values in
   @vtype      cTimerData *
   @vio        out
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer doesn't exist
   @endreturndesc
@@*/
int CCTK_Timer (const char *timername, cTimerData *info)
{
  t_Timer *timer;
  int this_timer;


  this_timer = Util_GetHandle (timers, timername, (void **) &timer);
  if (this_timer >= 0)
  {
    CCTKi_Timer (this_timer, timer, info);
  }

  return (this_timer >= 0 ? 0 : -1);
}


 /*@@
   @routine    CCTK_TimerI
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Gets the values of a timer given by its handle.
   @enddesc
   @calls      CCTKi_Timer

   @var        this_timer
   @vdesc      handle for the timer
   @vtype      int
   @vio        in
   @endvar
   @var        info
   @vdesc      user-supplied info structure to store the timer values in
   @vtype      cTimerData *
   @vio        out
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if handle is invalid
   @endreturndesc
@@*/
int CCTK_TimerI (int this_timer, cTimerData *info)
{
  t_Timer *timer;


  timer = Util_GetHandledData (timers, this_timer);
  if (timer)
  {
    CCTKi_Timer (this_timer, timer, info);
  }

  return (timer ? 0 : -1);
}


 /*@@
   @routine    CCTKi_Timer
   @date       Thu Oct 21 14:14:58 1999
   @author     Tom Goodale
   @desc
               Internal function which gets the values of a timer.
   @enddesc

   @var        this_timer
   @vdesc      handle for timer
   @vtype      int
   @vio        in
   @endvar
   @var        timer
   @vdesc      timer structure
   @vtype      t_Timer *
   @vio        in
   @endvar
   @var        info
   @vdesc      user-supplied info structure to store the timer values in
   @vtype      cTimerData *
   @vio        out
   @endvar
@@*/
static void CCTKi_Timer (int this_timer, t_Timer *timer, cTimerData *info)
{
  const cClockFuncs *funcs;
  int handle, total_vars;


  total_vars = 0;
  if (timer && timer->data)
  {
    /* Start the timer info for this timer */
    for (handle = 0; handle < n_clocks; handle++)
    {
      funcs = (const cClockFuncs *) Util_GetHandledData (clocks, handle);
      funcs->get (this_timer, timer->data[handle], &(info->vals[total_vars]));

      total_vars += funcs->n_vals;
    }
  }
  info->n_vals = total_vars;
}


 /*@@
   @routine    CCTK_TimerCreateData
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Allocates a timer data structure
   @enddesc

   @returntype cTimerData *
   @returndesc
               pointer to the allocated structure, or NULL if allocation failed
   @endreturndesc
@@*/
cTimerData *CCTK_TimerCreateData (void)
{
  cTimerData *retval;


  retval = (cTimerData *) malloc (sizeof (cTimerData));
  if (retval)
  {
    retval->n_vals = n_clock_vals;

    retval->vals = (cTimerVal *) malloc (n_clock_vals * sizeof (cTimerVal));
    if (! retval->vals)
    {
      free (retval);
      retval = NULL;
    }
  }

  return (retval);
}


 /*@@
   @routine    CCTK_TimerDestroyData
   @date       Wed Sep  1 10:10:38 1999
   @author     Tom Goodale
   @desc
               Frees a timer data structure allocated with CCTK_TimerCreateData()
   @enddesc

   @var        info
   @vdesc      timer data structure
   @vtype      cTimerData *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success
   @endreturndesc
@@*/
int CCTK_TimerDestroyData (cTimerData *info)
{
  if (info)
  {
    if (info->vals)
    {
      free (info->vals);
      info->vals = NULL;
    }
    free (info);
  }

  return (0);
}


 /*@@
   @routine    CCTK_TimerPrintDataI
   @date
   @author     David Rideout
   @desc
               Print the values of a timer for a given clock
   @enddesc
   @calls      CCTK_TimerCreateData
               CCTK_TimerDestroyData

   @var        this_timer
   @vdesc      handle for the timer
   @vtype      int
   @vio        in
   @endvar
   @var        this_clock
   @vdesc      handle for the clock
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer handle is invalid
   @endreturndesc
@@*/
int CCTK_TimerPrintDataI (int this_timer, int this_clock)
{
  cTimerData *info;
  int i, timer, retval;
  int firstclock, lastclock;
  int firsttimer, lasttimer;


  retval = 0;

  if (this_timer == -1)
  {
    firsttimer = 0;
    lasttimer = CCTK_NumTimers ();
  }
  else
  {
    firsttimer = this_timer;
    lasttimer = firsttimer + 1;
    if (firsttimer < 0 || firsttimer > CCTK_NumTimers ())
    {
      CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                  "CCTK_TimerPrintDataI: Timer %d not found", this_timer);
      retval = -1;
      lasttimer = firsttimer;
    }
  }

  for (timer = firsttimer; timer < lasttimer; timer++)
  {
    info = CCTK_TimerCreateData ();
    CCTK_TimerI (timer, info); /* return values are always 0 */

    printf ("Results from timer \"%s\":\n", CCTK_TimerName (timer));

    if (this_clock == -1)
    {
      firstclock = 0;
      lastclock = info->n_vals;
    }
    else
    {
      firstclock = this_clock;
      lastclock = this_clock + 1;
    }

    for (i = firstclock; i < lastclock; i++)
    {
      switch (info->vals[i].type)
      {
        case val_int:
          printf ("\t%s: %d %s\n", info->vals[i].heading,
                  info->vals[i].val.i, info->vals[i].units);
          break;

        case val_long:
          printf ("\t%s: %d %s\n", info->vals[i].heading,
                  (int) info->vals[i].val.l, info->vals[i].units);
          break;

        case val_double:
          printf ("\t%s: %.3f %s\n", info->vals[i].heading,
                  info->vals[i].val.d, info->vals[i].units);
          break;

        default:
          CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                      "CCTK_TimerPrintDataI: Unknown data type for timer info");
          break;
      }
    }
    CCTK_TimerDestroyData (info);
  }

  return (retval);
}

void CCTK_FCALL cctk_timerprintdatai_
                           (int *ierr, int *this_timer, int *this_clock)
{
  *ierr = CCTK_TimerPrintDataI (*this_timer, *this_clock);
}


 /*@@
   @routine    CCTK_TimerPrintData
   @date
   @author     David Rideout
   @desc
               Print the values of a timer for a given clock
   @enddesc
   @calls      CCTK_TimerPrintDataI

   @var        timername
   @vdesc      name of the timer
   @vtype      int
   @vio        in
   @endvar
   @var        clockname
   @vdesc      name of the clock
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if timer or clock don't exist
   @endreturndesc
@@*/
int CCTK_TimerPrintData (const char *timername, const char *clockname)
{
  int this_timer, this_clock, retval;


  retval = 0;

  if (! clockname)
  {
    this_clock = -1;
  }
  else
  {
    this_clock = CCTK_ClockHandle (clockname);
    if (this_clock == -1)
    {
      CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                  "CCTK_TimerPrintData: Clock %s not found", clockname);
      retval = -1;
    }
  }

  if (! timername)
  {
    this_timer = -1;
  }
  else
  {
    this_timer = Util_GetHandle (timers, timername, NULL);
    if (this_timer == -1)
    {
      CCTK_VWarn (8, __LINE__, __FILE__, "Cactus",
                  "CCTK_TimerPrintData: Timer %s not found", timername);
      retval = -1;
    }
  }

  if (retval == 0)
  {
    retval = CCTK_TimerPrintDataI (this_timer, this_clock);
  }

  return (retval);
}

void CCTK_FCALL cctk_timerprintdata_
                           (int *ierr, TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (timer, clock)


  *ierr = CCTK_TimerPrintData (*timer ? timer : NULL, *clock ? clock : NULL);
  free (timer);
  free (clock);
}
