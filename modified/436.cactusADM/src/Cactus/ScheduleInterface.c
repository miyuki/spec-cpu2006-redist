#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/*@@
   @file      ScheduleInterface.c
   @date      Thu Sep 16 14:06:21 1999
   @author    Tom Goodale
   @desc
   Routines to interface the main part of Cactus to the schedular.
   @enddesc
   @version $Header: /cactus/Cactus/src/main/ScheduleInterface.c,v 1.68 2001/12/21 14:30:59 tradke Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_WarnLevel.h"
#include "cctk_Misc.h"

#include "cctk_Schedule.h"
#include "cctki_ScheduleBindings.h"
#include "cctki_Schedule.h"

#include "cctk_Comm.h"
#include "cctk_Sync.h"

#include "cctk_Constants.h"
#include "cctk_Groups.h"
#include "cctk_GroupsOnGH.h"

#include "cctki_FortranWrappers.h"

#include "cctk_Timers.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/ScheduleInterface.c,v 1.68 2001/12/21 14:30:59 tradke Exp $";

CCTK_FILEVERSION(main_ScheduleInterface_c)


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

typedef enum {sched_none, sched_group, sched_function} iSchedType;
typedef enum {schedpoint_misc, schedpoint_analysis} iSchedPoint;

typedef struct
{
  /* Static data */
  char *description;

  /*char *thorn; MOVED TO FunctionData */
  char *implementation;

  iSchedType type;

  cFunctionData FunctionData;

  int n_mem_groups;
  int *mem_groups;

  int n_comm_groups;
  int *comm_groups;

  /* Timer data */

  int timer_handle;

  /* Dynamic data */
  int *CommOnEntry;
  int *StorageOnEntry;

  int done_entry;

} t_attribute;

typedef struct
{
  cGH *GH;
  iSchedPoint schedpoint;

  cTimerData *info;
  cTimerData *total_time;
  int print_headers;
  int synchronised;

  /* Stuff passed in in user calls */

  int (*CallFunction)(void *, cFunctionData *, void *);

} t_sched_data;


/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static int ScheduleTraverse(const char *where,
                            void *GH,
                            int (*CallFunction)(void *, cFunctionData *, void *));

static t_attribute *CreateAttribute(const char *where,
				    const char *routine,
				    const char *description,
                                    const char *language,
                                    const char *name,
                                    const char *thorn,
                                    int n_mem_groups,
                                    int n_comm_groups,
                                    int n_trigger_groups,
                                    int n_sync_groups,
                                    int n_options,
                                    va_list *ap);

static int ParseOptionList(int n_items,
                           t_attribute *attribute,
                           va_list *ap);

static int InitialiseOptionList(t_attribute *attribute);

static int ParseOption(t_attribute *attribute,
                       const char *option);

static t_sched_modifier *CreateModifiers(int n_before,
                                         int n_after,
                                         int n_while,
                                         va_list *ap);

int ValidateModifiers(t_sched_modifier *modifier);

static int CreateGroupIndexList(int n_items, int *array, va_list *ap);
static t_sched_modifier *CreateTypedModifier(t_sched_modifier *modifier,
                                             const char *type,
                                             int n_items,
                                             va_list *ap);
static cFunctionType TranslateFunctionType(const char *where);

static int SchedulePrint(const char *where);

static int CCTKi_SchedulePrintEntry(t_attribute *attribute, t_sched_data *data);
static int CCTKi_SchedulePrintExit(t_attribute *attribute, t_sched_data *data);
static int CCTKi_SchedulePrintWhile(int n_whiles,
                                    char **whiles,
                                    t_attribute *attribute,
                                    t_sched_data *data,
                                    int first);
static int CCTKi_SchedulePrintFunction(void *function, t_attribute *attribute, t_sched_data *data);

static int CCTKi_ScheduleCallEntry(t_attribute *attribute, t_sched_data *data);
static int CCTKi_ScheduleCallExit(t_attribute *attribute, t_sched_data *data);
static int CCTKi_ScheduleCallWhile(int n_whiles,
                                   char **whiles,
                                   t_attribute *attribute,
                                   t_sched_data *data,
                                   int first);
static int CCTKi_ScheduleCallFunction(void *function, t_attribute *attribute, t_sched_data *data);

static int SchedulePrintTimes(const char *where, t_sched_data *data);
static int CCTKi_SchedulePrintTimesFunction(void *function, t_attribute *attribute, t_sched_data *data);
static void CCTKi_SchedulePrintTimerInfo(cTimerData *info,
                                         cTimerData *total_time,
                                         const char *where,
                                         const char *description);
static void CCTKi_SchedulePrintTimerHeaders(cTimerData *info);
static void PrintDelimiterLine (char delimiter, const cTimerData *timer);


/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/* FIXME: these should be put in a header somewhere */

int CCTKi_TriggerSaysGo(cGH *GH, int variable);
int CCTKi_TriggerAction(void *GH, int variable);


/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/


static int indent_level = 0;

static int n_scheduled_comm_groups = 0;
static int *scheduled_comm_groups = NULL;

static int n_scheduled_storage_groups = 0;
static int *scheduled_storage_groups = NULL;

static cTimerData *timerinfo = NULL;
static int total_timer = -1;


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTK_CallFunction
   @date       Thu Jan 27 11:29:47 2000
   @author     Tom Goodale
   @desc
   Calls a function depending upon the data passed in the the
   fdata structure.
   @enddesc
   @calls

   @var     function
   @vdesc   pointer to function
   @vtype   void *
   @vio     in
   @endvar
   @var     fdata
   @vdesc   data about the function
   @vtype   cFunctionData *
   @vio     in
   @endvar
   @var     data
   @vdesc   Data to be passed to the function
   @vtype   void *
   @vio     inout
   @endvar

   @returntype int
   @returndesc
   0 - didn't synchronise
   @endreturndesc
@@*/
int CCTK_CallFunction(void *function,
                      cFunctionData *fdata,
                      void *data)
{
  void (*standardfunc)(void *);

  int (*noargsfunc)(void);

  int (*oneargfunc)(void *);

  switch(fdata->type)
  {
    case FunctionNoArgs:
      noargsfunc = (int (*)(void))function;
      noargsfunc();
      break;
    case FunctionOneArg:
      oneargfunc = (int (*)(void *))function;
      oneargfunc(data);
      break;
    case FunctionStandard:
      switch(fdata->language)
      {
        case LangC:
          standardfunc = (void (*)(void *))function;
          standardfunc(data);
          break;
        case LangFortran:
          fdata->FortranCaller(data, function);
          break;
        default :
          CCTK_Warn(1,__LINE__,__FILE__,"Cactus",
                    "CCTK_CallFunction: Unknown language.");
      }
      break;
    default :
      CCTK_Warn(1,__LINE__,__FILE__,"Cactus",
                "CCTK_CallFunction: Unknown function type.");
  }

  /* Return 0, meaning didn't synchronise */
  return 0;
}

/*@@
   @routine    CCTKi_ScheduleFunction
   @date       Thu Sep 16 18:19:01 1999
   @author     Tom Goodale
   @desc
   Schedules a function.
   @enddesc
   @calls

   @var     function
   @vdesc   function to be scheduled
   @vtype   void *
   @vio     in
   @endvar
   @var     name
   @vdesc   name of function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     thorn
   @vdesc   name of thorn providing function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     implementation
   @vdesc   name of implementation thorn belongs to
   @vtype   const char *
   @vio     in
   @endvar
   @var     description
   @vdesc   desciption of function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     where
   @vdesc   where to schedule the function
   @vtype   const char *
   @vio     in
   @endvar
   @var     language
   @vdesc   language of function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     n_mem_groups
   @vdesc   Number of groups needing memory switched on during this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_comm_groups
   @vdesc   Number of groups needing communication switched on during this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_trigger_groups
   @vdesc   Number of groups to trigger this function on
   @vtype   int
   @vio     in
   @endvar
   @var     n_sync_groups
   @vdesc   Number of groups needing synchronisation after this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_options
   @vdesc   Number of options for this schedule block
   @vtype   int
   @vio     in
   @endvar
   @var     n_before
   @vdesc   Number of functions/groups to schedule before
   @vtype   int
   @vio     in
   @endvar
   @var     n_after
   @vdesc   Number of functions/groups to schedule after
   @vtype   int
   @vio     in
   @endvar
   @var     n_while
   @vdesc   Number of vars to schedule while
   @vtype   int
   @vio     in
   @endvar
   @var     ...
   @vdesc   remaining options
   @vtype   multiple const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   Return val of DoScheduleFunction or
   -1 - memory failure
   @endreturndesc
@@*/
int CCTKi_ScheduleFunction(void *function,
                           const char *name,
                           const char *thorn,
                           const char *implementation,
                           const char *description,
                           const char *where,
                           const char *language,
                           int n_mem_groups,
                           int n_comm_groups,
                           int n_trigger_groups,
                           int n_sync_groups,
                           int n_options,
                           int n_before,
                           int n_after,
                           int n_while,
                           ...
                           )
{
  int retcode;
  t_attribute *attribute;
  t_sched_modifier *modifier;
  va_list ap;

  va_start(ap, n_while);

  attribute = CreateAttribute(where,name,description, language, thorn, implementation,
                              n_mem_groups, n_comm_groups, n_trigger_groups,
                              n_sync_groups, n_options, &ap);
  modifier  = CreateModifiers(n_before, n_after, n_while, &ap);

  va_end(ap);

  ValidateModifiers(modifier);

  if(attribute && (modifier || (n_before == 0 && n_after == 0 && n_while == 0)))
  {
    attribute->FunctionData.type = TranslateFunctionType(where);

    retcode = CCTKi_DoScheduleFunction(where, name, function, modifier, (void *)attribute);

#ifdef DEBUG
    fprintf(stderr, "Scheduled %s at %s\n", name, where);
#endif
  }
  else
  {
    fprintf(stderr, "Internal error: Failed to schedule %s at %s!!!\n", name, where);
    exit(2);
    retcode = -1;
  }

  return retcode;
}

/*@@
   @routine    CCTKi_ScheduleGroup
   @date       Thu Sep 16 18:19:18 1999
   @author     Tom Goodale
   @desc
   Schedules a group.
   @enddesc
   @calls

   @var     name
   @vdesc   name of group to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     thorn
   @vdesc   name of thorn providing group to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     implementation
   @vdesc   name of implementation group belongs to
   @vtype   const char *
   @vio     in
   @endvar
   @var     description
   @vdesc   desciption of group to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     where
   @vdesc   where to schedule the group
   @vtype   const char *
   @vio     in
   @endvar
   @var     n_mem_groups
   @vdesc   Number of groups needing memory switched on during this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_comm_groups
   @vdesc   Number of groups needing communication switched on during this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_trigger_groups
   @vdesc   Number of groups to trigger this function on
   @vtype   int
   @vio     in
   @endvar
   @var     n_sync_groups
   @vdesc   Number of groups needing synchronisation after this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_options
   @vdesc   Number of options for this schedule block
   @vtype   int
   @vio     in
   @endvar
   @var     n_before
   @vdesc   Number of functions/groups to schedule before
   @vtype   int
   @vio     in
   @endvar
   @var     n_after
   @vdesc   Number of functions/groups to schedule after
   @vtype   int
   @vio     in
   @endvar
   @var     n_while
   @vdesc   Number of vars to schedule while
   @vtype   int
   @vio     in
   @endvar
   @var     ...
   @vdesc   remaining options
   @vtype   multiple const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   Return val of DoScheduleGroup or
   -1 - memory failure
   @endreturndesc
@@*/
int CCTKi_ScheduleGroup(const char *name,
                        const char *thorn,
                        const char *implementation,
                        const char *description,
                        const char *where,
                        int n_mem_groups,
                        int n_comm_groups,
                        int n_trigger_groups,
                        int n_sync_groups,
                        int n_options,
                        int n_before,
                        int n_after,
                        int n_while,
                        ...
                        )
{
  int retcode;
  t_attribute *attribute;
  t_sched_modifier *modifier;
  va_list ap;

  va_start(ap, n_while);

  attribute = CreateAttribute(where,name,description, NULL, thorn, implementation,
                              n_mem_groups, n_comm_groups, n_trigger_groups,
                              n_sync_groups, n_options, &ap);
  modifier  = CreateModifiers(n_before, n_after, n_while, &ap);

  va_end(ap);

  ValidateModifiers(modifier);

  if(attribute && (modifier || (n_before == 0 && n_after == 0 && n_while == 0)))
  {
    retcode = CCTKi_DoScheduleGroup(where, name, modifier, (void *)attribute);
#ifdef DEBUG
    fprintf(stderr, "Scheduled %s at %s\n", name, where);
#endif
  }
  else
  {
#ifdef DEBUG
    fprintf(stderr, "Failed to schedule %s at %s!!!\n", name, where);
#endif
    retcode = -1;
  }

  return retcode;

}


/*@@
   @routine    CCTKi_ScheduleGroupStorage
   @date       Fri Sep 17 18:55:59 1999
   @author     Tom Goodale
   @desc
   Schedules a group for storage when a GH is created.
   @enddesc
   @calls

   @var     group
   @vdesc   group name
   @vtype   const char *
   @vio     in
   @endvar
   @returntype int
   @returndesc
   Group index or
   -1 - memory failure
   @endreturndesc
@@*/
int CCTKi_ScheduleGroupStorage(const char *group)
{
  int *temp;

  temp = (int *) realloc(scheduled_storage_groups,
                         (n_scheduled_storage_groups+1) * sizeof(int));
  if(temp)
  {
    temp[n_scheduled_storage_groups++] = CCTK_GroupIndex(group);
    scheduled_storage_groups = temp;
  }

  return (temp ? temp[n_scheduled_storage_groups-1] : -1);
}


/*@@
   @routine    CCTKi_ScheduleGroupComm
   @date       Fri Sep 17 18:55:59 1999
   @author     Tom Goodale
   @desc
   Schedules a group for communication when a GH is created.
   @enddesc
   @calls

   @var     group
   @vdesc   group name
   @vtype   const char *
   @vio     in
   @endvar
   @returntype int
   @returndesc
   Group index or
   -1 - memory failure
   @endreturndesc
@@*/
int CCTKi_ScheduleGroupComm(const char *group)
{
  int *temp;

  temp = (int *) realloc(scheduled_comm_groups,
                         (n_scheduled_comm_groups+1) * sizeof(int));
  if(temp)
  {
    temp[n_scheduled_comm_groups++] = CCTK_GroupIndex(group);
    scheduled_comm_groups = temp;
  }

  return (temp ? temp[n_scheduled_comm_groups-1] : -1);
}


 /*@@
   @routine    CCTK_ScheduleTraverse
   @date       Tue Apr  4 08:05:27 2000
   @author     Tom Goodale
   @desc
   Traverses a schedule point, and its entry and exit points if necessary.
   @enddesc
   @calls

   @var     where
   @vdesc   Schedule point
   @vtype   const char *
   @vio     in
   @endvar
   @var     GH
   @vdesc   GH data
   @vtype   void *
   @vio     inout
   @endvar
   @var     CallFunction
   @vdesc   Function called to call a function
   @vtype   int (*)(void *, cFubctionData, void *)
   @vio     in
   @vcomment
   Set to NULL to use the default
   @endvar

   @returntype int
   @returndesc
   0 - success
   1 - memory failure
   @endreturndesc
@@*/
int CCTK_ScheduleTraverse(const char *where,
                          void *GH,
                          int (*CallFunction)(void *, cFunctionData *, void *))
{
  int retcode;

  int special;
  const char *current;

  static char *current_point = NULL;
  static unsigned int current_length = 0;
  char *temp;

  special=0;

  /* Special entry points have $ in them */
  for(current=where; *current; current++)
  {
    if(*current == '$')
    {
      special = 1;
      break;
    }
  }

  retcode = 0;

  if(special)
  {
    ScheduleTraverse(where, GH, CallFunction);
  }
  else
  {
    if(current_length < strlen(where) + 7)
    {
      current_length = strlen(where)+7;

      temp = realloc(current_point, current_length);

      if(temp)
      {
        current_point = temp;
      }
      else
      {
        retcode = 1;
      }
    }
    if(retcode == 0)
    {
      sprintf(current_point, "%s$%s", where, "ENTRY");
      ScheduleTraverse(current_point, GH, CallFunction);

      ScheduleTraverse(where, GH, CallFunction);

      sprintf(current_point, "%s$%s", where, "EXIT");
      ScheduleTraverse(current_point, GH, CallFunction);
    }
  }

  return retcode;
}


/*@@
   @routine    CCTKi_ScheduleGHInit
   @date       Fri Sep 17 21:25:13 1999
   @author     Tom Goodale
   @desc
   Does any scheduling stuff setup which requires a GH.
   @enddesc
   @calls

   @var     GH
   @vdesc   GH data
   @vtype   void *
   @vio     inout
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
int CCTKi_ScheduleGHInit(void *GH)
{
  int i;


  /* create and start the CCTK total timer */
  total_timer = CCTK_TimerCreate ("CCTK total time");
  if (total_timer >= 0)
  {
    CCTK_TimerStartI (total_timer);
  }
  else
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                "Couldn't create CCTK total timer. "
                "No timing information will be available.");
  }

  for(i = 0; i < n_scheduled_storage_groups; i++)
  {
    CCTK_EnableGroupStorageI(GH,scheduled_storage_groups[i]);
  }

  for(i = 0; i < n_scheduled_comm_groups; i++)
  {
    CCTK_EnableGroupCommI(GH,scheduled_comm_groups[i]);
  }

  return 0;
}

/*@@
   @routine    CCTK_SchedulePrint
   @date       Fri Sep 17 21:52:44 1999
   @author     Tom Goodale
   @desc
   Prints out the schedule info.
   @enddesc
   @calls

   @var     where
   @vdesc   Schedule point
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
int CCTK_SchedulePrint(const char *where)
{
  if(!where)
  {
    printf ("  if (recover)\n");
    printf ("    Recover parameters\n");
    printf ("  endif\n\n");
    printf ("  Startup routines\n");
    SchedulePrint("CCTK_STARTUP");
    printf("\n");
    printf ("  Parameter checking routines\n");
    SchedulePrint("CCTK_PARAMCHECK");
    printf("\n");
    printf("  Initialisation\n");
    SchedulePrint("CCTK_BASEGRID$ENTRY");
    SchedulePrint("CCTK_BASEGRID");
    SchedulePrint("CCTK_BASEGRID$EXIT");
    SchedulePrint("CCTK_INITIAL$ENTRY");
    SchedulePrint("CCTK_INITIAL");
    SchedulePrint("CCTK_INITIAL$EXIT");
    SchedulePrint("CCTK_POSTINITIAL$ENTRY");
    SchedulePrint("CCTK_POSTINITIAL");
    SchedulePrint("CCTK_POSTINITIAL$EXIT");
    SchedulePrint("CCTK_POSTSTEP$ENTRY");
    SchedulePrint("CCTK_POSTSTEP");
    SchedulePrint("CCTK_POSTSTEP$EXIT");
    printf ("    if (recover)\n");
    indent_level +=2;
    SchedulePrint("CCTK_RECOVER_VARIABLES");
    indent_level -=2;
    printf ("    endif\n");
    printf ("    if (checkpoint initial data)\n");
    indent_level +=2;
    SchedulePrint("CCTK_CPINITIAL");
    indent_level -=2;
    printf ("    endif\n");
    printf ("    if (analysis)\n");
    indent_level +=2;
    SchedulePrint("CCTK_ANALYSIS$ENTRY");
    SchedulePrint("CCTK_ANALYSIS");
    SchedulePrint("CCTK_ANALYSIS$EXIT");
    indent_level -=2;
    printf ("    endif\n");
    printf("\n");
    printf ("  do loop over timesteps\n");
    printf ("    Rotate timelevels\n");
    printf ("    iteration = iteration + 1\n");
    printf ("    t = t+dt\n");
    SchedulePrint("CCTK_PRESTEP$ENTRY");
    SchedulePrint("CCTK_PRESTEP");
    SchedulePrint("CCTK_PRESTEP$EXIT");
    SchedulePrint("CCTK_EVOL$ENTRY");
    SchedulePrint("CCTK_EVOL");
    SchedulePrint("CCTK_EVOL$EXIT");
    SchedulePrint("CCTK_POSTSTEP$ENTRY");
    SchedulePrint("CCTK_POSTSTEP");
    SchedulePrint("CCTK_POSTSTEP$EXIT");
    printf ("    if (checkpoint)\n");
    indent_level +=2;
    SchedulePrint("CCTK_CHECKPOINT");
    indent_level -=2;
    printf ("    endif\n");
    printf ("    if (analysis)\n");
    indent_level +=2;
    SchedulePrint("CCTK_ANALYSIS$ENTRY");
    SchedulePrint("CCTK_ANALYSIS");
    SchedulePrint("CCTK_ANALYSIS$EXIT");
    indent_level -=2;
    printf ("    endif\n");
    printf ("  enddo\n");
    printf ("  Termination routines\n");
    SchedulePrint("CCTK_TERMINATE");
    printf ("  Shutdown routines\n");
    SchedulePrint("CCTK_SHUTDOWN");
  }
  else
  {
    SchedulePrint(where);
  }

  return 0;
}

/*@@
   @routine    CCTK_SchedulePrintTimes
   @date       Fri Sep 17 21:52:44 1999
   @author     Tom Goodale
   @desc
   Prints out the schedule timings.
   @enddesc
   @calls

   @var     where
   @vdesc   Schedule point
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
int CCTK_SchedulePrintTimes(const char *where)
{
  t_sched_data data;

  if(!timerinfo)
  {
    timerinfo = CCTK_TimerCreateData();
  }

  data.GH = NULL;
  data.schedpoint = schedpoint_misc;
  data.print_headers = 1;
  data.info = timerinfo;
  data.total_time = CCTK_TimerCreateData();

  if(!where)
  {
    SchedulePrintTimes("CCTK_RECOVER_VARIABLES", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_CHECKPOINT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_STARTUP", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_PARAMCHECK", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_BASEGRID$ENTRY", &data);
    SchedulePrintTimes("CCTK_BASEGRID", &data);
    SchedulePrintTimes("CCTK_BASEGRID$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_INITIAL$ENTRY", &data);
    SchedulePrintTimes("CCTK_INITIAL", &data);
    SchedulePrintTimes("CCTK_INITIAL$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_POSTINITIAL$ENTRY", &data);
    SchedulePrintTimes("CCTK_POSTINITIAL", &data);
    SchedulePrintTimes("CCTK_POSTINITIAL$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_PRESTEP$ENTRY", &data);
    SchedulePrintTimes("CCTK_PRESTEP", &data);
    SchedulePrintTimes("CCTK_PRESTEP$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_EVOL$ENTRY", &data);
    SchedulePrintTimes("CCTK_EVOL", &data);
    SchedulePrintTimes("CCTK_EVOL$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_POSTSTEP$ENTRY", &data);
    SchedulePrintTimes("CCTK_POSTSTEP", &data);
    SchedulePrintTimes("CCTK_POSTSTEP$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_ANALYSIS$ENTRY", &data);
    SchedulePrintTimes("CCTK_ANALYSIS", &data);
    SchedulePrintTimes("CCTK_ANALYSIS$EXIT", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_TERMINATE", &data);
/*  printf("\n"); */
    SchedulePrintTimes("CCTK_SHUTDOWN", &data);
  }
  else
  {
    SchedulePrintTimes(where, &data);
  }

  CCTK_TimerDestroyData(data.total_time);

  /* also print total time at the bottom */
  if (total_timer >= 0)
  {
    CCTK_TimerStopI (total_timer);
    CCTK_TimerI (total_timer, timerinfo);
    CCTKi_SchedulePrintTimerInfo (timerinfo, NULL, "", "Total time for simulation");

    /* just in case this is not at termination yet ... */
    CCTK_TimerStartI (total_timer);
  }

  return 0;
}

/*@@
   @routine    CCTK_TranslateLanguage
   @date       Thu Sep 16 18:18:31 1999
   @author     Tom Goodale
   @desc
   Translates a language string into an internal enum.
   @enddesc
   @calls

   @var     sval
   @vdesc   Language
   @vtype   const char *
   @vio     in
   @endvar

   @returntype cLanguage
   @returndesc
   The language
   @endreturndesc
@@*/
cLanguage CCTK_TranslateLanguage(const char *sval)
{
  cLanguage retcode;

  if(CCTK_Equals(sval, "C"))
  {
    retcode = LangC;
  }
  else if(CCTK_Equals(sval, "Fortran"))
  {
    retcode = LangFortran;
  }
  else
  {
    fprintf(stderr, "Unknown language %s\n", sval);
    retcode = LangNone;
  }

  return retcode;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

/*@@
   @routine    ScheduleTraverse
   @date       Fri Sep 17 21:52:44 1999
   @author     Tom Goodale
   @desc
   Traverses the given schedule point.
   @enddesc
   @calls

   @var     where
   @vdesc   Schedule point
   @vtype   const char *
   @vio     in
   @endvar
   @var     GH
   @vdesc   GH data
   @vtype   void *
   @vio     inout
   @endvar
   @var     CallFunction
   @vdesc   Function called to call a function
   @vtype   int (*)(void *, cFubctionData, void *)
   @vio     in
   @vcomment
   Set to NULL to use the default
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/

static int ScheduleTraverse(const char *where,
                            void *GH,
                            int (*CallFunction)(void *, cFunctionData *, void *))
{
  t_sched_data data;
  int (*calling_function)(void *, t_attribute *, t_sched_data *);

  data.GH = (cGH *)GH;
  data.CallFunction = CallFunction ? CallFunction : CCTK_CallFunction;
  data.schedpoint = CCTK_Equals(where, "CCTK_ANALYSIS") ?
                    schedpoint_analysis : schedpoint_misc;
  calling_function = CCTKi_ScheduleCallFunction;

  CCTKi_DoScheduleTraverse(where,
     (int (*)(void *, void *))                    CCTKi_ScheduleCallEntry,
     (int (*)(void *, void *))                    CCTKi_ScheduleCallExit,
     (int  (*)(int, char **, void *, void *, int))CCTKi_ScheduleCallWhile,
     (int (*)(void *, void *, void *))            calling_function,
     (void *)&data);

  return 0;
}

 /*@@
   @routine    CreateAttribute
   @date       Thu Sep 16 18:22:48 1999
   @author     Tom Goodale
   @desc
   Creates an attribute structure for a schedule item.
   @enddesc
   @calls

   @var     description
   @vdesc   desciption of function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     language
   @vdesc   language of function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     thorn
   @vdesc   name of thorn providing function to be scheduled
   @vtype   const char *
   @vio     in
   @endvar
   @var     implementation
   @vdesc   name of implementation thorn belongs to
   @vtype   const char *
   @vio     in
   @endvar
   @var     n_mem_groups
   @vdesc   Number of groups needing memory switched on during this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_comm_groups
   @vdesc   Number of groups needing communication switched on during this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_trigger_groups
   @vdesc   Number of groups to trigger this function on
   @vtype   int
   @vio     in
   @endvar
   @var     n_sync_groups
   @vdesc   Number of groups needing synchronisation after this function
   @vtype   int
   @vio     in
   @endvar
   @var     n_options
   @vdesc   Number of options for this schedule block
   @vtype   int
   @vio     in
   @endvar
   @var     ap
   @vdesc   options
   @vtype   va_list of multiple const char *
   @vio     inout
   @vcomment
   This should have as many items as the sum of the above n_* options
   @endvar

   @returntype t_attribute
   @returndesc
   The attribute
   @endreturndesc
@@*/
static t_attribute *CreateAttribute(const char *where,
				    const char *name,
				    const char *description,
                                    const char *language,
                                    const char *thorn,
                                    const char *implementation,
                                    int n_mem_groups,
                                    int n_comm_groups,
                                    int n_trigger_groups,
                                    int n_sync_groups,
                                    int n_options,
                                    va_list *ap)
{
  char *timername;
  t_attribute *this;

  this = (t_attribute *)malloc(sizeof(t_attribute));

  if(this)
  {
    this->FunctionData.where = (char *)malloc((strlen(where)+1)*sizeof(char));
    this->FunctionData.routine = (char *)malloc((strlen(name)+1)*sizeof(char));
    this->description    = 
      (char *)malloc((strlen(description)+1)*sizeof(char));
    this->FunctionData.thorn = (char *)malloc((strlen(thorn)+1)*sizeof(char));
    this->implementation = 
      (char *)malloc((strlen(implementation)+1)*sizeof(char));
    this->mem_groups     = (int *)malloc(n_mem_groups*sizeof(int));
    this->comm_groups    = (int *)malloc(n_comm_groups*sizeof(int));
    this->FunctionData.TriggerGroups = 
      (int *)malloc(n_trigger_groups*sizeof(int));
    this->FunctionData.SyncGroups = (int *)malloc(n_sync_groups*sizeof(int));
    this->StorageOnEntry = (int *)malloc(n_mem_groups*sizeof(int));
    this->CommOnEntry    = (int *)malloc(n_comm_groups*sizeof(int));

    if(this->FunctionData.where &&
       this->FunctionData.routine &&
       this->description     &&
       this->FunctionData.thorn  &&
       this->implementation  &&
       (this->mem_groups || n_mem_groups==0)         &&
       (this->comm_groups || n_comm_groups==0)       &&
       (this->FunctionData.TriggerGroups || n_trigger_groups==0) &&
       (this->FunctionData.SyncGroups || n_sync_groups==0))
    {
      strcpy(this->FunctionData.where,where);
      strcpy(this->FunctionData.routine,name);
      strcpy(this->description,    description);
      strcpy(this->FunctionData.thorn, thorn);
      strcpy(this->implementation, implementation);

      if(language)
      {
        this->type = sched_function;
        this->FunctionData.language = CCTK_TranslateLanguage(language);
        this->FunctionData.FortranCaller = (int (*)(cGH *,void *))CCTKi_FortranWrapper(thorn);
      }
      else
      {
        this->type = sched_group;
      }

      /* Create the lists of indices of groups we're interested in. */
      CreateGroupIndexList(n_mem_groups,     this->mem_groups, ap);
      CreateGroupIndexList(n_comm_groups,    this->comm_groups, ap);
      CreateGroupIndexList(n_trigger_groups, this->FunctionData.TriggerGroups, ap);
      CreateGroupIndexList(n_sync_groups,    this->FunctionData.SyncGroups, ap);

      /* Check the miscellaneous options */

      InitialiseOptionList(this);
      ParseOptionList(n_options, this, ap);

      this->n_mem_groups     = n_mem_groups;
      this->n_comm_groups    = n_comm_groups;
      this->FunctionData.n_TriggerGroups = n_trigger_groups;
      this->FunctionData.n_SyncGroups = n_sync_groups;

      /* Add a timer to the item */
      timername = (char *) malloc (strlen (thorn) + strlen (description) + 3);
      sprintf (timername, "%s: %s", thorn, description);
      this->timer_handle = CCTK_TimerCreate(timername);
      if (this->timer_handle < 0)
      {
        CCTK_VWarn (5, __LINE__, __FILE__, "Cactus",
                   "Couldn't create timer with name '%s'", timername);
      }
      free (timername);
    }
    else
    {
      free(this->FunctionData.where);
      free(this->FunctionData.routine);
      free(this->description);
      free(this->comm_groups);
      free(this->FunctionData.TriggerGroups);
      free(this->FunctionData.SyncGroups);
      free(this);
      this = NULL;
    }
  }

  return this;
}

/*@@
   @routine    CreateModifier
   @date       Thu Sep 16 18:23:13 1999
   @author     Tom Goodale
   @desc
   Creates a schedule modifier list.
   @enddesc
   @calls

   @var     n_before
   @vdesc   Number of functions/groups to schedule before
   @vtype   int
   @vio     in
   @endvar
   @var     n_after
   @vdesc   Number of functions/groups to schedule after
   @vtype   int
   @vio     in
   @endvar
   @var     n_while
   @vdesc   Number of vars to schedule while
   @vtype   int
   @vio     in
   @endvar
   @var     ap
   @vdesc   options
   @vtype   va_list of multiple const char *
   @vio     inout
   @vcomment
   This should have as many items as the sum of the above n_* options
   @endvar

   @returntype t_sched_modifier *
   @returndesc
   the schedule modifier
   @endreturndesc
@@*/
static t_sched_modifier *CreateModifiers(int n_before,
                                         int n_after,
                                         int n_while,
                                         va_list *ap)
{
  t_sched_modifier *modifier;

  modifier = CreateTypedModifier(NULL, "before", n_before, ap);
  modifier = CreateTypedModifier(modifier, "after", n_after, ap);
  modifier = CreateTypedModifier(modifier, "while", n_while, ap);

  return modifier;
}

/*@@
   @routine    ValidateModifier
   @date       Sat Apr 14 18:28:13 2001
   @author     Gabrielle Allen
   @desc
   Validates a schedule modifier list. At the moment just check that
   the while modifier uses a CCTK_INT grid variable.
   @enddesc
   @calls

   @var     modifier
   @vdesc
   @vtype   t_sched_modifier *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   Negative if modifier not valid, zero if modifier is valid.
   @endreturndesc
@@*/
int ValidateModifiers(t_sched_modifier *modifier)
{
  int retval = 0;
  int vindex;
  int type;

  for (;modifier;modifier=modifier->next)
  {
    if (modifier->type == sched_while)
    {
      vindex = CCTK_VarIndex(modifier->argument);
      type = CCTK_VarTypeI(vindex);
      if (type != CCTK_VARIABLE_INT)
      {
        CCTK_VWarn(0,__LINE__,__FILE__,"Cactus",
                   "While qualifier %s is not a CCTK_INT grid variable",
                   modifier->argument);
        retval = -1;
      }
    }
  }
  return retval;
}

/*@@
   @routine    CreateGroupIndexList
   @date       Fri Sep 17 21:51:51 1999
   @author     Tom Goodale
   @desc
   Gets the next n_items group names from the variable argument list
   and converts them to indices.
   @enddesc
   @calls

   @var     n_items
   @vdesc   number of items on the list
   @vtype   int
   @vio     in
   @endvar
   @var     array
   @vdesc   array of indices
   @vtype   int *
   @vio     out
   @endvar
   @var     ap
   @vdesc   argument list
   @vtype   va_list of const char *
   @vio     inout
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
static int CreateGroupIndexList(int n_items, int *array, va_list *ap)
{
  int i;
  const char *item;

  for(i=0; i < n_items; i++)
  {
    item = va_arg(*ap, const char *);

    array[i] = CCTK_GroupIndex(item);
  }

  return 0;
}


 /*@@
   @routine    ParseOptionList
   @date       Thu Jan 27 20:26:42 2000
   @author     Tom Goodale
   @desc
   Extracts the list of miscellaneous options in a schedule
   group definition.
   @enddesc
   @calls

   @var     n_items
   @vdesc   number of items on the list
   @vtype   int
   @vio     in
   @endvar
   @var     attribute
   @vdesc   attribute list
   @vtype   t_attribute *
   @vio     inout
   @endvar
   @var     ap
   @vdesc   argument list
   @vtype   va_list of const char *
   @vio     inout
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
static int ParseOptionList(int n_items,
                           t_attribute *attribute,
                           va_list *ap)
{
  int i;
  const char *item;

  for(i=0; i < n_items; i++)
  {
    item = va_arg(*ap, const char *);

    ParseOption(attribute, item);
  }

  return 0;
}

 /*@@
   @routine    InitialiseOptionList
   @date       Thu Jan 27 20:36:54 2000
   @author     Tom Goodale
   @desc
   Initialises the miscellaneous option list for a schedule group.
   @enddesc
   @calls

   @var     attribute
   @vdesc   option attribute
   @vtype   t_attribute *
   @vio     out
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
static int InitialiseOptionList(t_attribute *attribute)
{
  attribute->FunctionData.global = 0;

  return 0;
}

 /*@@
   @routine    ParseOption
   @date       Thu Jan 27 20:29:36 2000
   @author     Tom Goodale
   @desc
   Parses an individual option to a schedule group.
   @enddesc
   @calls

   @var     attribute
   @vdesc   option attribute
   @vtype   t_attribute *
   @vio     out
   @endvar
   @var     option
   @vdesc   Option
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
static int ParseOption(t_attribute *attribute,
                       const char *option)
{
  if(CCTK_Equals(option, "GLOBAL"))
  {
    attribute->FunctionData.global = 1;
  }
  else
  {
    CCTK_Warn(1,__LINE__,__FILE__,"Cactus",
              "ParseOption: Unknown option for schedule group.\n");
  }

  return 0;
}

/*@@
   @routine    CreateTypedModifier
   @date       Fri Sep 17 21:50:59 1999
   @author     Tom Goodale
   @desc
   Adds the next n_items items from the variable argument list
   onto the modifer.
   @enddesc
   @calls

   @var     modifier
   @vdesc   base schedule modifier
   @vtype   t_sched_modifier
   @vio     inout
   @vcomment
   This is a list which gets expanded by this function
   @endvar
   @var     type
   @vdesc   modifier type
   @vtype   const char *
   @vio     in
   @vcomment
   before, after, while
   @endvar
   @var     n_items
   @vdesc   Number of items on list
   @vtype   int
   @vio     in
   @endvar
   @var     ap
   @vdesc   argument list
   @vtype   va_list of const char *
   @vio     inout
   @endvar

   @returntype t_sched_modifier *
   @returndesc
   modifier list
   @endreturndesc

@@*/
static t_sched_modifier *CreateTypedModifier(t_sched_modifier *modifier,
                                             const char *type,
                                             int n_items,
                                             va_list *ap)
{
  int i;
  const char *item;

  for(i=0; i < n_items; i++)
  {
    item = va_arg(*ap, const char *);

    modifier = CCTKi_ScheduleAddModifier(modifier, type, item);
  }

  return modifier;
}

/*@@
   @routine    TranslateFunctionType
   @date       Mon Jan 24 16:52:06 2000
   @author     Tom Goodale
   @desc
   Translates a string saying what schedule point
   a function is registered at into the appropriate
   function type.
   @enddesc
   @calls

   @var     where
   @vdesc   schedule point
   @vtype   const char *
   @vio     in
   @endvar

   @returntype cFunctionType
   @returndesc
   The function type
   @endreturndesc
@@*/
static cFunctionType TranslateFunctionType(const char *where)
{
  cFunctionType retcode;

  int special;
  const char *current;

  special = 0;

  /* Special entry points have $ in them */
  for(current=where; *current; current++)
  {
    if(*current == '$')
    {
      special = 1;
      break;
    }
  }

  if(special)
  {
    retcode = FunctionOneArg;
  }
  else if(CCTK_Equals(where, "CCTK_STARTUP"))
  {
    retcode = FunctionNoArgs;
  }
  else if(CCTK_Equals(where, "CCTK_SHUTDOWN"))
  {
    retcode = FunctionNoArgs;
  }
  else
  {
    retcode = FunctionStandard;
  }

  return retcode;
}

/*@@
   @routine    SchedulePrint
   @date       Sun Sep 19 13:31:23 1999
   @author     Tom Goodale
   @desc
   Traverses the schedule data for a particular entry point and
   prints out the data.
   @enddesc
   @calls

   @var     where
   @vdesc   Schedule point
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
   return of DoScheduleTravers or
   0 - where is NULL
   @endreturndesc
@@*/
static int SchedulePrint(const char *where)
{
  int retcode;
  t_sched_data data;

  data.GH = NULL;
  data.schedpoint = schedpoint_misc;

  if(where)
  {
    retcode = CCTKi_DoScheduleTraverse(where,
       (int (*)(void *, void *))                    CCTKi_SchedulePrintEntry,
       (int (*)(void *, void *))                    CCTKi_SchedulePrintExit,
       (int  (*)(int, char **, void *, void *, int))CCTKi_SchedulePrintWhile,
       (int (*)(void *, void *, void *))            CCTKi_SchedulePrintFunction,
       (void *)&data);
  }
  else
  {
    retcode = 0;
  }

  return retcode;
}

/*@@
   @routine    SchedulePrintTimes
   @date       Fri Oct 22 12:35:06 1999
   @author     Tom Goodale
   @desc
   Prints the times for a particular schedule entry point.
   @enddesc
   @calls

   @var     where
   @vdesc   Schedule point
   @vtype   const char *
   @vio     in
   @endvar
   @var     data
   @vdesc   schedule data
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   return of DoScheduleTravers or
   0 - where is NULL
   @endreturndesc
@@*/
static int SchedulePrintTimes(const char *where, t_sched_data *data)
{
  int i;
  int retcode;
  char *description;

  if(where)
  {
    memset (data->total_time->vals, 0,
            data->total_time->n_vals * sizeof (data->total_time->vals[0]));

    retcode = CCTKi_DoScheduleTraverse(where, NULL, NULL, NULL,
       (int (*)(void *, void *, void *)) CCTKi_SchedulePrintTimesFunction,
       (void *)data);

    if (retcode >= 0)
    {
      for (i = 0; i < data->total_time->n_vals; i++)
      {
        data->total_time->vals[i].type = data->info->vals[i].type;
        data->total_time->vals[i].units = data->info->vals[i].units;
        data->total_time->vals[i].heading = data->info->vals[i].heading;
      }
      description = (char *) malloc (strlen (where) + 16);
      sprintf (description, "Total time for %s", where);
      CCTKi_SchedulePrintTimerInfo(data->total_time, NULL, "", description);
      free (description);
    }
  }
  else
  {
    retcode = 0;
  }

  return retcode;
}

/********************************************************************
 *********************     Printing Routines   **********************
 ********************************************************************/


 /*@@
   @routine    CCTKi_SchedulePrintEntry
   @date       Sun Sep 19 13:31:23 1999
   @author     Tom Goodale
   @desc
   Routine called on entry to a group when traversing for printing.
   @enddesc
   @calls

   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - schedule item is inactive
   1 - schedule item is active
   @endreturndesc
@@*/
static int CCTKi_SchedulePrintEntry(t_attribute *attribute,
                                    t_sched_data *data)
{
  /* prevent compiler warnings about unused parameters */
  attribute = attribute;
  data = data;

  indent_level += 2;

  return 1;
}

/*@@
   @routine    CCTKi_SchedulePrintExit
   @date       Sun Sep 19 13:31:23 1999
   @author     Tom Goodale
   @desc
   Routine called on exit to a group when traversing for printing.
   @enddesc
   @calls

   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   1 - this has no meaning
   @endreturndesc
@@*/
static int CCTKi_SchedulePrintExit(t_attribute *attribute,
                                   t_sched_data *data)
{
  /* prevent compiler warnings about unused parameters */
  attribute = attribute;
  data = data;

  indent_level -=2;

  return 1;
}

/*@@
   @routine    CCTKi_SchedulePrintWhile
   @date       Sun Sep 19 13:31:23 1999
   @author     Tom Goodale
   @desc
   Routine called for while of a group when traversing for printing.
   @enddesc
   @calls

   @var     n_whiles
   @vdesc   number of while statements
   @vtype   int
   @vio     in
   @endvar
   @var     whiles
   @vdesc   while statements
   @vtype   char **
   @vio     in
   @endvar
   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar
   @var     first
   @vdesc   flag - is this the first time we are checking while on this schedule item
   @vtype   int
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - schedule item is inactive
   1 - schedule item is active
   @endreturndesc
@@*/
static int CCTKi_SchedulePrintWhile(int n_whiles,
                                    char **whiles,
                                    t_attribute *attribute,
                                    t_sched_data *data,
                                    int first)
{
  int i;

  /* prevent compiler warnings about unused parameters */
  attribute = attribute;
  data = data;

  if(first)
  {
    printf("%*s", indent_level + 2 + 7, "while (");

    for(i = 0; i < n_whiles; i++)
    {
      if(i > 0)
      {
        printf(" && ");
      }

      printf("%s", whiles[i]);
    }

    printf(")\n");
  }
  else
  {
    printf("%*s", indent_level + 9, "end while\n");
  }

  return first;
}

/*@@
   @routine    CCTKi_SchedulePrintFunction
   @date       Sun Sep 19 13:36:25 1999
   @author     Tom Goodale
   @desc
   Function which actually prints out data about a group or a function.
   @enddesc
   @calls

   @var     function
   @vdesc   the function to be called
   @vtype   void *
   @vio     in
   @endvar
   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   1 - this has no meaning
   @endreturndesc
@@*/
static int CCTKi_SchedulePrintFunction(void *function,
                                       t_attribute *attribute,
                                       t_sched_data *data)
{
  /* prevent compiler warnings about unused parameters */
  function = function;
  data = data;

  if (indent_level > 0)
  {
    printf ("%*s", indent_level, " ");
  }
  printf("%s: %s\n", attribute->FunctionData.thorn, attribute->description);

  return 1;
}


/********************************************************************
 *********************     Calling Routines   ***********************
 ********************************************************************/


 /*@@
   @routine    CCTKi_ScheduleCallEntry
   @date       Sun Sep 19 13:24:06 1999
   @author     Tom Goodale
   @desc
   Routine called when a schedule group is entered.
   @enddesc
   @calls

   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - schedule item is inactive
   1 - schedule item is active
   @endreturndesc
@@*/
static int CCTKi_ScheduleCallEntry(t_attribute *attribute,
                                   t_sched_data *data)
{
  int i;
  int indx;
  int last;
  int go;

  if(attribute)
  {
    go = 0;

    if(data->schedpoint == schedpoint_analysis)
    {
      /* In analysis, so check triggers */
      for (i = 0; i < attribute->FunctionData.n_TriggerGroups ; i++)
      {
        indx = CCTK_FirstVarIndexI(attribute->FunctionData.TriggerGroups[i]);
        last  = indx + CCTK_NumVarsInGroupI(attribute->FunctionData.TriggerGroups[i]) -1;
        for(; indx <= last ; indx++)
        {
          go = go || CCTKi_TriggerSaysGo(data->GH, indx);
        }
      }
    }
    else
    {
      go = 1;
    }

    if(go)
    {
      /* Switch on storage for groups */
      for(i = 0; i < attribute->n_mem_groups; i++)
      {
        attribute->StorageOnEntry[i] = CCTK_EnableGroupStorageI(data->GH,attribute->mem_groups[i]);
      }

      /* Switch on communication for groups. */
      for(i = 0; i < attribute->n_comm_groups; i++)
      {
        attribute->CommOnEntry[i] = CCTK_EnableGroupCommI(data->GH,attribute->comm_groups[i]);
      }
    }

    /* Remember if we have switched on storage and comm or not. */
    attribute->done_entry = go;
  }
  else
  {
    go = 1;
  }

  /* Initialise then synchronised flag. */
  data->synchronised = 0;

  return go;
}

/*@@
   @routine    CCTKi_ScheduleCallExit
   @date       Sun Sep 19 13:25:24 1999
   @author     Tom Goodale
   @desc
   Routine called on exit from a schedule group.
   @enddesc
   @calls

   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   1 - this has no meaning
   @endreturndesc
@@*/
static int CCTKi_ScheduleCallExit(t_attribute *attribute,
                                  t_sched_data *data)
{
  int i;
  int vindex;
  int last;

  /* Only do this if the entry routine did stuff. */
  if(attribute && attribute->done_entry)
  {

    /* Synchronise variable groups associated with this schedule group. */
    if(attribute->FunctionData.n_SyncGroups > 0 && ! data->synchronised)
    {
      CCTK_SyncGroupsI(data->GH,
                       attribute->FunctionData.n_SyncGroups,
                       attribute->FunctionData.SyncGroups);
      data->synchronised = 0;
    }

    if(data->schedpoint == schedpoint_analysis)
    {
      /* In analysis, so do any trigger actions. */
      for (i = 0; i < attribute->FunctionData.n_TriggerGroups ; i++)
      {
        vindex = CCTK_FirstVarIndexI(attribute->FunctionData.TriggerGroups[i]);
        last  = vindex + CCTK_NumVarsInGroupI(attribute->FunctionData.TriggerGroups[i]) - 1;
        for(; vindex <= last ; vindex++)
        {
          CCTKi_TriggerAction(data->GH, vindex);
        }
      }
    }

    /* Switch off communication if it was done in entry. */
    for(i = 0; i < attribute->n_comm_groups; i++)
    {
      if(!attribute->CommOnEntry[i])
      {
        CCTK_DisableGroupCommI(data->GH,attribute->comm_groups[i]);
      }
    }

    /* Switch off storage if it was switched on in entry. */
    for(i = 0; i < attribute->n_mem_groups; i++)
    {
      if(!attribute->StorageOnEntry[i])
      {
        CCTK_DisableGroupStorageI(data->GH,attribute->mem_groups[i]);
      }
    }

  }

  return 1;
}

/*@@
   @routine    CCTKi_ScheduleCallWhile
   @date       Sun Sep 19 13:27:53 1999
   @author     Tom Goodale
   @desc
   Routine called to check variables to see if a group or function should be executed.
   @enddesc
   @calls

   @var     n_whiles
   @vdesc   number of while statements
   @vtype   int
   @vio     in
   @endvar
   @var     whiles
   @vdesc   while statements
   @vtype   char **
   @vio     in
   @endvar
   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar
   @var     first
   @vdesc   flag - is this the first time we are checking while on this schedule item
   @vtype   int
   @vio     in
   @endvar

   @returntype int
   @returndesc
   0 - schedule item is inactive
   1 - schedule item is active
   @endreturndesc
@@*/
static int CCTKi_ScheduleCallWhile(int n_whiles,
                                   char **whiles,
                                   t_attribute *attribute,
                                   t_sched_data *data,
                                   int first)
{
  int i;
  int retcode;

  /* prevent compiler warnings about unused parameters */
  attribute = attribute;
  first = first;

  retcode = 1;

  /* FIXME - should do a lot of validation either here or on registration */
  for(i = 0; i < n_whiles; i++)
  {
    retcode = retcode && *((CCTK_INT *)CCTK_VarDataPtr(data->GH, 0, whiles[i]));
  }

  return retcode;
}

/*@@
   @routine    CCTKi_ScheduleCallFunction
   @date       Sun Sep 19 13:29:14 1999
   @author     Tom Goodale
   @desc
   The routine which actually calls a function.
   @enddesc
   @calls

   @var     function
   @vdesc   the function to be called
   @vtype   void *
   @vio     in
   @endvar
   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   1 - this has no meaning
   @endreturndesc
@@*/
static int CCTKi_ScheduleCallFunction(void *function,
                                      t_attribute *attribute,
                                      t_sched_data *data)
{
  if (attribute->timer_handle >= 0)
  {
    CCTK_TimerStartI(attribute->timer_handle);
  }

  /* Use whatever has been chosen as the calling function for this
   * function.
   */
  data->synchronised = data->CallFunction(function, &(attribute->FunctionData), data->GH);

  if (attribute->timer_handle >= 0)
  {
    CCTK_TimerStopI(attribute->timer_handle);
  }

  return 1;
}

/********************************************************************
 ****************     Timer Printing Routines   *********************
 ********************************************************************/

/*@@
   @routine    CCTKi_SchedulePrintTimesFunction
   @date       Fri Oct 22 12:26:26 1999
   @author     Tom Goodale
   @desc
   Function which actually prints out data about a group or a function.
   @enddesc
   @calls

   @var     function
   @vdesc   the function to be called
   @vtype   void *
   @vio     in
   @endvar
   @var     attribute
   @vdesc   schedule item attributes
   @vtype   t_attribute *
   @vio     in
   @endvar
   @var     data
   @vdesc   data associated with schedule item
   @vtype   t_sched_data
   @vio     in
   @endvar

   @returntype int
   @returndesc
   1 - this has no meaning
   @endreturndesc
@@*/
static int CCTKi_SchedulePrintTimesFunction(void *function,
                                            t_attribute *attribute,
                                            t_sched_data *data)
{
  /* prevent compiler warnings about unused parameters */
  function = function;

  if (attribute->timer_handle >= 0)
  {
    CCTK_TimerI(attribute->timer_handle, data->info);

    if(data->print_headers)
    {
      CCTKi_SchedulePrintTimerHeaders(data->info);

      data->print_headers = 0;
    }

    CCTKi_SchedulePrintTimerInfo(data->info, data->total_time,
                                 attribute->FunctionData.thorn, 
                                 attribute->description);
  }

  return 1;
}

static void CCTKi_SchedulePrintTimerInfo(cTimerData *timer,
                                         cTimerData *total_time,
                                         const char *where,
                                         const char *description)
{
  int i, j;


  if (indent_level > 0)
  {
    printf ("%*s", indent_level, " ");
  }

  /* print delimiter line */
  if (*where == 0)
  {
    PrintDelimiterLine ('-', timer);
  }

  /* print the timer description */
  printf ("%-16.16s| %-40.40s", where, description);

  /* print the actual timer values */
  for (i = 0; i < timer->n_vals; i++)
  {
    j = strlen (timer->vals[i].heading) + strlen (timer->vals[i].units) + 3;

    switch (timer->vals[i].type)
    {
      case val_int:
        printf ("| %*d ", j, timer->vals[i].val.i);
        if (total_time)
        {
          total_time->vals[i].val.i += timer->vals[i].val.i;
        }
        break;
      case val_long:
        printf ("| %*ld ", j, timer->vals[i].val.l);
        if (total_time)
        {
          total_time->vals[i].val.l += timer->vals[i].val.l;
        }
        break;
      case val_double:
        printf ("| %*.8f ", j, timer->vals[i].val.d);
        if (total_time)
        {
          total_time->vals[i].val.d += timer->vals[i].val.d;
        }
        break;
      default:
        printf ("Unknown value type at line %d of %s\n", __LINE__, __FILE__);
    }
  }

  putchar ('\n');

  /* print delimiter line */
  if (*where == 0)
  {
    PrintDelimiterLine ('-', timer);
  }
}


static void CCTKi_SchedulePrintTimerHeaders (cTimerData *timer)
{
  int i;


  PrintDelimiterLine ('=', timer);

  printf ("%-16.16s| %-40.40s", "Thorn", "Scheduled routine in time bin");
  for (i = 0; i < timer->n_vals; i++)
  {
    printf ("| %s [%s] ", timer->vals[i].heading, timer->vals[i].units);
  }
  putchar ('\n');

  PrintDelimiterLine ('=', timer);
}


static void PrintDelimiterLine (char delimiter, const cTimerData *timer)
{
  int i, len;


  len = 58;
  for (i = 0; i < timer->n_vals; i++)
  {
    len += strlen (timer->vals[i].heading) + strlen (timer->vals[i].units) + 6;
  }
  for (i = 0; i < len; i++)
  {
    putchar (delimiter);
  }
  putchar ('\n');
}
