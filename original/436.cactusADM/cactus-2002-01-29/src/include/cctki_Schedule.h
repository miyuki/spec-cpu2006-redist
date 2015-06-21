 /*@@
   @header    cctki_Schedule.h
   @date      Wed Sep 15 22:49:24 1999
   @author    Tom Goodale
   @desc 
   Schedule stuff from the core schedule subsystem
   which can be seen by the rest of the flesh.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctki_Schedule.h,v 1.5 2000/10/12 08:01:34 goodale Exp $
 @@*/

#ifndef _CCTKI_SCHEDULE_H_
#define _CCTKI_SCHEDULE_H_ 1

/* Types needed by other routines. */

typedef enum {sched_mod_none, sched_before, sched_after, sched_while} t_sched_modifier_type;

typedef struct T_SCHED_MODIFIER
{
  struct T_SCHED_MODIFIER *next;

  t_sched_modifier_type type;
  
  char *argument;

} t_sched_modifier;


#ifdef __cplusplus
extern "C" 
{
#endif

/* Routines to create items */
t_sched_modifier *CCTKi_ScheduleAddModifier(t_sched_modifier *orig, 
                                           const char *modifier, 
                                           const char *argument);



int CCTKi_DoScheduleFunction(const char *gname, 
                             const char *fname, 
                             void *func, 
                             t_sched_modifier *modifiers, 
                             void *attributes);

int CCTKi_DoScheduleGroup
  (
   const char *gname, 
   const char *thisname, 
   t_sched_modifier *modifiers, 
   void *attributes
  );

/* Routine to sort the groups - must be called before traversal. */
int CCTKi_DoScheduleSortAllGroups(void);

/* Traversal routine */
int CCTKi_DoScheduleTraverse(const char *group_name,
                             int (*item_entry)(void *, void *),
                             int (*item_exit)(void *, void *),
                             int  (*while_check)(int, char **, void *, void *, int),
                             int (*function_process)(void *, void *, void *),
                             void *data);

#ifdef __cplusplus
}
#endif

#endif /* _CCTKI_SCHEDULE_H_ */
