 /*@@
   @header    cctki_ScheduleBindings.h
   @date      Thu Jan 27 10:39:40 2000
   @author    Tom Goodale
   @desc 
   Routines which should be called from the bindings and rest of cactus
   to do schedule stuff, but is cactus specific, not in the core schedule
   subsystem.
   @enddesc
   @version $Header: /cactus/Cactus/src/include/cctki_ScheduleBindings.h,v 1.6 2000/01/28 02:47:01 goodale Exp $
 @@*/

#ifndef _CCTKI_SCHEDUULEBINDINGS_H_
#define _CCTKI_SCHEDUULEBINDINGS_H_

#ifdef __cplusplus
extern "C" 
{
#endif

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
                           ...);

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
                        ...);

int CCTKi_ScheduleGroupStorage(const char *group);

int CCTKi_ScheduleGroupComm(const char *group);

int CCTKi_ScheduleGHInit(void *GH);

#ifdef __cplusplus
}
#endif

#endif /* _CCTKI_SCHEDUULEBINDINGS_H_*/
