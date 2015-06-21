 /*@@
   @file      Slicing.h
   @date      Wed Jul 14 16:20:03 1999
   @author    Gerd Lanfermann
   @desc 
   Prototypes and structure for Einstein_slicing.

   Einstein_slicing holds:
   1) the name of the slicing as specified in the parameter 
      slicing / mixed_slicing
   2) the param_active number, which also indicates priority
      when running mixed
   3) a function pointer: a "time-to-slice" function can
      be registered which is  called and which can be used
      to do fancy conditionals, like use slicing XYZ if a 
      grid functions has collapsed in some region.
   @enddesc 
 @@*/

#include "cGH.h"

#ifdef __cplusplus 
extern "C" {
#endif

  /* Return values for the time-to-slice fucntion */
#define SLICING_YES       1
#define SLICING_DONTCARE  0
#define SLICING_NO       -1

struct Einstein_slicing { 
  char *name;
  int   param_active;
  int  (*timetoslice)(cGH*);
};

int Einstein_RegisterSlicing(const char *name);

int Einstein_SetSlicingPriority(int handle,int flag);

int Einstein_RegisterTimeToSlice(int handle,int (*func)(cGH *));

int Einstein_GetSlicingHandle(const char *name);

void Einstein_SetNextSlicing(cGH *);

#ifdef __cplusplus 
}
#endif





