#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ScheduleTraverse.c
   @date      Thu Sep 16 08:58:37 1999
   @author    Tom Goodale
   @desc 
   Routins to traverse schedule groups.
   @enddesc 
   @version $Header: /cactus/Cactus/src/schedule/ScheduleTraverse.c,v 1.8 2001/05/10 12:35:19 goodale Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>

#include "cctk_Flesh.h"

#include "cctki_Schedule.h"
#include "StoreHandledData.h"
#include "Schedule.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/schedule/ScheduleTraverse.c,v 1.8 2001/05/10 12:35:19 goodale Exp $";

CCTK_FILEVERSION(schedule_ScheduleTraverse_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static int ScheduleTraverseGroup(cHandledData *schedule_groups, 
                                 t_sched_group *group,
                                 void *attributes,
                                 int n_whiles,
                                 char **whiles,
                                 int (*item_entry)(void *, void *),
                                 int (*item_exit)(void *, void *),
                                 int  (*while_check)(int, char **, void *, void *, int),
                                 int (*function_process)(void *, void *, void *),
                                 void *data);

static int ScheduleTraverseFunction(void *function,
                                    void *attributes,
                                    int n_whiles,
                                    char **whiles,
                                    int (*item_entry)(void *, void *),
                                    int (*item_exit)(void *, void *),
                                    int  (*while_check)(int, char **, void *, void *, int),
                                    int (*function_process)(void *, void *, void *),
                                    void *data);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/

 /*@@
   @routine    CCTKi_DoScheduleTraverse
   @date       Thu Sep 16 09:05:23 1999
   @author     Tom Goodale
   @desc 
   Traverses the group with the given name.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     group_name
   @vdesc   group to traverse
   @vtype   const char *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     item_entry
   @vdesc   function to be called on entry to an item
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     item_exit
   @vdesc   function to be called on exit from an item
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     while_check
   @vdesc   function to be called to check a while statement
   @vtype   int (*)(int, char **, void *, void *, int)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     function_process
   @vdesc   function to be called on any function
   @vtype   int (*)(void *, void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     data
   @vdesc   data to be passed to the functions
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   Return code of @seeroutine ScheduleTraverseGroup or return value of 
   @seeroutine Util_GetHandle if that fails.
   @endreturndesc
@@*/
int CCTKi_DoScheduleTraverse(const char *group_name,
                             int (*item_entry)(void *, void *),
                             int (*item_exit)(void *, void *),
                             int  (*while_check)(int, char **, void *, void *, int),
                             int (*function_process)(void *, void *, void *),
                             void *data)
{
  cHandledData *schedule_groups;
  t_sched_group *group;
  int handle;
  int retcode;

  schedule_groups = CCTKi_DoScheduleGetGroups();

  handle = Util_GetHandle(schedule_groups, group_name, (void *)&group);

  if(handle >= 0)
  {
    retcode = ScheduleTraverseGroup(schedule_groups, 
                                    group, 
                                    NULL,
                                    0,
                                    NULL,
                                    item_entry, 
                                    item_exit, 
                                    while_check, 
                                    function_process,
                                    data);
  }
  else
  {
    retcode = handle;
  }

  return retcode;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    ScheduleTraverseGroup
   @date       Thu Sep 16 09:07:44 1999
   @author     Tom Goodale
   @desc 
   Traverses the given schedule group.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     schedule_groups
   @vdesc   the schedule groups
   @vtype   cHandledData
   @vio     in
   @vcomment 
 
   @endvar 
   @var     group
   @vdesc   the group to traverse
   @vtype   t_sched_group
   @vio     in
   @vcomment 
 
   @endvar 
   @var     attributes
   @vdesc   group attributes
   @vtype   void *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     n_whiles
   @vdesc   number of whiles
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     whiles
   @vdesc   array of while strings
   @vtype   char **
   @vio     in
   @vcomment 
 
   @endvar 
   @var     item_entry
   @vdesc   function to be called on entry to an item
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     item_exit
   @vdesc   function to be called on exit from an item
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     while_check
   @vdesc   function to be called to check a while statement
   @vtype   int (*)(int, char **, void *, void *, int)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     function_process
   @vdesc   function to be called on any function
   @vtype   int (*)(void *, void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     data
   @vdesc   data to be passed to the functions
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - success
   @endreturndesc
@@*/
static int ScheduleTraverseGroup(cHandledData *schedule_groups, 
                                 t_sched_group *group,
                                 void *attributes,
                                 int n_whiles,
                                 char **whiles,
                                 int (*item_entry)(void *, void *),
                                 int (*item_exit)(void *, void *),
                                 int  (*while_check)(int, char **, void *, void *, int),
                                 int (*function_process)(void *, void *, void *),
                                 void *data)
{
  int item;
  int doit;
  int called_item_entry;
  t_sched_group *newgroup;

  /* If there is a while-list associated with this item, check if the group should be
   * exectuted at all.
   */

  if(n_whiles > 0 && while_check)
  {
    doit = while_check(n_whiles, whiles, attributes, data,1);
  }
  else
  {
    doit = 1;
  }

  /* Call a item entry function if it is defined. */
  if(doit)
  {
    called_item_entry = 1;

    if(item_entry)
    {
      doit = item_entry(attributes, data);
    }
  }
  else
  {
    called_item_entry = 0;
  }

  /* Now traverse the group. */
  while(doit )
  {
      
    /* Traverse in the sorted order - assumes group has been sorted ! */
    for(item = 0 ; item < group->n_scheditems; item++)
    {
      switch(group->scheditems[group->order[item]].type)
      {
        case sched_function :
          ScheduleTraverseFunction(group->scheditems[group->order[item]].function, 
                                   group->scheditems[group->order[item]].attributes,
                                   group->scheditems[group->order[item]].n_whiles,
                                   group->scheditems[group->order[item]].whiles,
                                   item_entry,
                                   item_exit,                                  
                                   while_check,
                                   function_process,
                                   data);
          break;
        case sched_group :
          newgroup = (t_sched_group *)Util_GetHandledData(schedule_groups, 
                                                          group->scheditems[group->order[item]].group);
          ScheduleTraverseGroup(schedule_groups,
                                newgroup, 
                                group->scheditems[group->order[item]].attributes,
                                group->scheditems[group->order[item]].n_whiles,
                                group->scheditems[group->order[item]].whiles,
                                item_entry, 
                                item_exit, 
                                while_check, 
                                function_process,
                                data);
          break;
        default :
          fprintf(stderr, "Unknown schedule item type %d\n", group->scheditems[group->order[item]].type);
      }
    }

    /* Check the while_list again. */
    if(n_whiles > 0 && while_check)
    {
      doit = while_check(n_whiles, whiles, attributes, data,0);
    }
    else
    {
      doit = 0;
    }
  }

  /* Call the group_exit function if it's defined. */
  if(called_item_entry)
  {
    if(item_exit)
    {
      item_exit(attributes, data);
    }
  }

  return 0;
}

 /*@@
   @routine    ScheduleTraverseFunction
   @date       Thu Sep 16 11:51:58 1999
   @author     Tom Goodale
   @desc 
   Deals with a function in the schedule list
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     function
   @vdesc   the function to be called
   @vtype   void *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     attributes
   @vdesc   function attributes
   @vtype   void *
   @vio     in
   @vcomment 
 
   @endvar 
   @var     n_whiles
   @vdesc   number of whiles
   @vtype   int
   @vio     in
   @vcomment 
 
   @endvar 
   @var     whiles
   @vdesc   array of while strings
   @vtype   char **
   @vio     in
   @vcomment 
 
   @endvar 
   @var     item_entry
   @vdesc   function to be called on entry to an item
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     item_exit
   @vdesc   function to be called on exit from an item
   @vtype   int (*)(void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     while_check
   @vdesc   function to be called to check a while statement
   @vtype   int (*)(int, char **, void *, void *, int)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     function_process
   @vdesc   function to be called on any function
   @vtype   int (*)(void *, void *, void *)
   @vio     in
   @vcomment 
 
   @endvar 
   @var     data
   @vdesc   data to be passed to the functions
   @vtype   void *
   @vio     inout
   @vcomment 
 
   @endvar 

   @returntype int
   @returndesc 
   0 - success
   @endreturndesc
@@*/
static int ScheduleTraverseFunction(void *function,
                                    void *attributes,
                                    int n_whiles,
                                    char **whiles,
                                    int (*item_entry)(void *, void *),
                                    int (*item_exit)(void *, void *),
                                    int  (*while_check)(int, char **, void *, void *, int),
                                    int (*function_process)(void *, void *, void *),
                                    void *data)
{
  int doit;
  int called_item_entry;

  /* If there is a while-list associated with this function, check if the function should be
   * executed at all.
   */

  if(n_whiles > 0 && while_check)
  {
    doit = while_check(n_whiles, whiles, attributes, data,1);
  }
  else
  {
    doit = 1;
  }

  /* Call a item entry function if it is defined. */
  if(doit)
  {
    called_item_entry = 1;

    if(item_entry)
    {
      doit = item_entry(attributes, data);
    }
  }
  else
  {
    called_item_entry = 0;
  }

  /* Now traverse the . */
  while(doit )
  {
    
    /* Now actually do something with the function. */
    function_process(function, attributes, data);

    /* Check the while_list again. */
    if(n_whiles > 0 && while_check)
    {
      doit = while_check(n_whiles, whiles, attributes, data,0) ;
    }
    else

    {
      doit = 0;
    }
  }

  /* Call the item_exit function if it's defined. */
  if(called_item_entry)
  {
    if(item_exit)
    {
      item_exit(attributes, data);
    }
  }

  return 0;
}

/********************************************************************
 ********************************************************************
 ********************************************************************/

#ifdef TEST_SCHEDULETRAVERSE

#define func_x(x) \
int func_ ## x (void) { return printf("I'm func " #x "\n"); }

func_x(a)
func_x(b)
func_x(c)

int fprocess(void *function, void *attributes, void *data)
{
  int (*func)(void);

  func = (int (*)(void)) function;

  func();

  return 1;
}

int main(int argc, char *argv[])
{
  t_sched_modifier *modifier;

  modifier = CCTKi_DoScheduleAddModifer(NULL, "before", "c");
  modifier = CCTKi_DoScheduleAddModifer(modifier, "after",  "a");

  CCTKi_DoScheduleFunction("group_a", "c", func_c, NULL, NULL);
  CCTKi_DoScheduleFunction("group_a", "b", func_b, modifier, NULL);
  CCTKi_DoScheduleFunction("group_a", "a", func_a, NULL, NULL);
  CCTKi_DoScheduleFunction("group_b", "a", func_a, NULL, NULL);
  CCTKi_DoScheduleFunction("group_b", "b", func_b, NULL, NULL);
  CCTKi_DoScheduleGroup("group_a", "group_b", modifier, NULL);

  CCTKi_DoScheduleSortAllGroups();

  CCTKi_DoScheduleTraverse("group_a", NULL, NULL, NULL, fprocess, NULL);

  return 0;
}
#endif
