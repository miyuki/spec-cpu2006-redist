#ifdef SPEC_CPU
# define THORN_IS_Einstein
#endif /* SPEC_CPU */
 /*@@
   @file      Slicing.c
   @date      Wed Jul 14 16:03:12 1999
   @author    Gerd Lanfermann
   @desc 
     A flexible calling structure to calling slicing functions:
     * a slicing is registered with its NAME, with a NUMBER derived from
       the position in the parameter and indicating its priority, with a 
       FUNCTION, which tells if the slicing should be used in the next 
       iteration. 
     * The slicing is REGISTERED in a startup routine of the thorn 
       providing the slicing.
     * This structure (HandledData) is created during STARTUP
     * This strucure is initialized during CCTK_INITIAL
     * This structure is evaluted during a CCTK_PRESTEP and an integer
       active_slicing_handle defines which slicing is next. 
       All slicings have to check if they match this handle and only 
       then take their turn.
   @enddesc 
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_FortranString.h"
#include "StoreHandledData.h"
#include "Slicing.h"

int CCTK_FCALL einstein_registerslicing_(ONE_FORTSTRING_ARG);
int CCTK_FCALL einstein_getslicinghandle_(ONE_FORTSTRING_ARG);
void Einstein_ActivateSlicing(cGH *GH) ;

static const char *rcsid = "$Header: /cactus/CactusEinstein/Einstein/src/Slicing.c,v 1.25 2002/01/05 18:58:18 allen Exp $";

CCTK_FILEVERSION(CactusEinstein_Einstein_Slicing_c)


/*#define ESLIC_DEBUG*/


/* Local data holding info on slicings..*/

static int num_slicings = 0;
static cHandledData *Eslicings = NULL;

 /*@@
   @routine    Einstein_RegisterSlicing
   @date       Wed Jul 14 15:57:32 1999
   @author     Gerd Lanfermann
   @desc 
   This C routine registers a slicing in a structure which provides information
   whether it is activated by parameter and which function is responsible 
   for evaluating whether the slicing should be used in the next iteartion.

   Called at STARTUP by the thorn which provides the slicing.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int Einstein_RegisterSlicing(const char *slice_name)
{
  int handle;

  struct Einstein_slicing *new_slicing;

  /*Check that this Slicing has not been registered, yet */
  handle = Util_GetHandle(Eslicings,slice_name,NULL);
  
  if (handle<0) 
  {
    /* Allocate new slicing structure */
    new_slicing = (struct Einstein_slicing*) 
      malloc(sizeof(struct Einstein_slicing));

    if (new_slicing) 
    {
      
      /* store new_slicing */
      handle = Util_NewHandle(&Eslicings, slice_name, new_slicing);
    
#ifdef ESLIC_DEBUG
      printf("REGISTER_SLICING: >>%s<<  new handle %d \n",slice_name,handle);
#endif

      /* Initialize the Einstein_slicing structure */

      /* we know the slicing name */
      new_slicing->name = (char *)slice_name; 

      /* Function pointer , evaluates if to perform this slicings, allowing 
	 for iteration checks, gridfunction behavior, etc.  Will be set by
         Einstein_RegisterTimeToSlice */
      new_slicing->timetoslice = NULL;
      
      /* Flag to indicate if this slicing activated by a parameter [0/1] */
      new_slicing->param_active = 0;

      /*increase the counter for registered slicings */
      num_slicings++;
      
    } 
    else 
    {
      /* Memory failure */
      CCTK_WARN(0,"cannot allocate memory for new slicing");
      handle = -2; 
    }
  }
  else
  { 
    /* Extension handle already exists */
    CCTK_VWarn(0,__LINE__,__FILE__,CCTK_THORNSTRING,
	      "New handle %s (%d) already in use",
	      slice_name,handle);
    handle = -1;
  }

  return handle;
}

/*@@
   @routine    Einstein_RegisterSlicing 
   @date       Wed Jul 14 15:57:32 1999
   @author     Gerd Lanfermann
   @desc 
   This FORTRAN routine registers a slicing in a structure which provides 
   information whether it is activated by parameter and which function is 
   responsible for evaluating whether the slicing should be used in the 
   next iteartion.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
  
int CCTK_FCALL einstein_registerslicing_(ONE_FORTSTRING_ARG)
{
  int handle;
  ONE_FORTSTRING_CREATE(name)
  handle = CCTK_VarIndex(name); 
  free(name);
  return(handle);
}
 

 /*@@
   @routine    Einstein_RegisterTimeToSlice
   @date       Wed Jul 14 15:52:09 1999
   @author     Gerd Lanfermann
   @desc 
      Routine registers a function with the slicing. This function returns
      information whether the slicing is supposed to used during the next
      iteration. The user can register any function to check for iteration 
      conditions, grid function behavior, etc. 

      This has to be in C currently. We cannot wrap this easily (TOM).
   @enddesc 
   @calls     
   @calledby   The user
   @history 
 
   @endhistory 

@@*/

int Einstein_RegisterTimeToSlice(int handle, int (*func)(cGH *))
{
  int return_code=1;
  struct Einstein_slicing *slicing;

  slicing = Util_GetHandledData(Eslicings, handle);
  
  if (slicing)  
  {
    slicing->timetoslice=func;
    return_code = 1;
  }
  else 
  {
    /* handle didn't return the data pointer */ 
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	      "Could not obtain slicing structure for handle %d",
	      handle);
    return_code = 0;
  }   

#ifdef ESLIC_DEBUG
    printf("REGISTER_SLICING: func OK for handle %d\n",handle);
#endif

  return return_code;
}


/*@@
   @routine    Einstein_ActivateSlicing
   @date       Thu Jul 15 17:44:04 1999
   @author     Gerd Lanfermann
   @desc 
     After the slicings have been registered by the thorns (in Startup .e.g.),
     this routines checks which slicing is specified in the parameter database
     and activates them. The activation flag takes a number, which reflect 
     the order in which the keywords appear in the parameter: 
     first = 1 = highest priority, sec. = 2 ,lower priority.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
  

void Einstein_ActivateSlicing(cGH *GH) 
{

  DECLARE_CCTK_PARAMETERS

  struct Einstein_slicing *slic;
  CCTK_INT  *active_slicing_handle;

  int handle, priority, i;
  int handle2;
  char *err, *split_string;
  char *item=NULL, *after=NULL;

  /* Get our grid scalar pointer: initialize with -1*/
  i = CCTK_VarIndex("Einstein::active_slicing_handle");
  if (i<0) 
  {
    CCTK_WARN(0,"Cannot find grid scalar: active_slicing_handle");
  }
  active_slicing_handle = (CCTK_INT *)CCTK_VarDataPtrI(GH,0,i);
  *active_slicing_handle = -1;


  /* we have a clearly defined slicing - no "mixed" */
  if (!CCTK_Equals(slicing,"mixed")) 
    { 
      handle= Util_GetHandle(Eslicings,slicing,NULL);

      if (handle<0) 
      {

	/* If slicing is set to "none", only warn level 2 (could be initial data only) */
	if (CCTK_Equals(slicing,"none")) 
	{
	  CCTK_WARN(2,"No slicing set. Probably fatal for evolution");
	}
        else if (CCTK_Equals(slicing,"static"))
	{
          handle2=Einstein_RegisterSlicing("static");
          if (handle2<0) 
	  {
	    CCTK_WARN(1,"Cannot register static slicing");
	  }
	}
        else if (CCTK_Equals(slicing,"geodesic"))
	{
          handle2=Einstein_RegisterSlicing("geodesic");
          if (handle2<0) 
	  {
	    CCTK_WARN(1,"Cannot register geodesic slicing");
	  }
	} 
	else 
	{
	  CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
		     "Cannot get handle for slicing %s",slicing);
	}
	return;
      }

      slic  = Util_GetHandledData(Eslicings,handle);
      if (!slic)
      {
	CCTK_WARN(0,"Cannot access handle structure!");
      }
      slic->param_active = 1;

      /*Some error checking: */
      if (slic->timetoslice!=NULL) 
      {
	err = (char*)malloc(256*sizeof(char));
	sprintf(err,"%s%s%s%s",
		"ERROR: You have specified a unique slicing (",slic->name,
		") AND a condition-function is registered for that slicing.\n ",
		"Impossible, what should I do inbetween ? Rather set slicing -> mixed");
	CCTK_WARN(0,err);
	if (err) free(err);
      }
    } 
  /* We have mixed slicing: we check the mixed_slicing parameter */
  else
  {
    /* for all slicings in the mix: e.g. mixed_slicing = "1+log maximal static" 
       1) get the handle for each slicing 
       
       2) set slicing->param_active, counting from 1 to n according to the
       position. The higher the number the lower the priority (0=inactive!!) 
    */

    /* we attach an additional space for string splitting */
    split_string= (char*)malloc(strlen(mixed_slicing)*sizeof(char)+2);
    sprintf(split_string,"%s ",mixed_slicing);

    /* Here we take the string appart */
    Util_SplitString(&item,&after,split_string," ");
    /*printf(" item >%s< after: >%s< \n",item,after);*/

    priority    = 1;      
    while (item)
    {
      if (CCTK_Equals(item,"static"))
      {
	printf("Registering Static\n");
	handle2=Einstein_RegisterSlicing("static");
	if (handle2<0) 
	{
	  CCTK_WARN(1,"Cannot register static slicing");
	}
      }
      else if (CCTK_Equals(item,"geodesic"))
      {
	handle2=Einstein_RegisterSlicing("geodesic");
	if (handle2<0) 
	{
	  CCTK_WARN(1,"Cannot register geodesic slicing");
	}
      } 

      slic = Util_GetHandledData(Eslicings,Util_GetHandle(Eslicings,item,NULL));

      if (!slic) 
      { 
	CCTK_VWarn(0,__LINE__,__FILE__,CCTK_THORNSTRING,
		   "No registered slicing for <%s>",item);
      }

      slic->param_active = priority;
#ifdef  ESLIC_DEBUG
      printf("ACTIVATE_SLICING: found %s priority %d \n",item, priority);
#endif
      priority++;
      split_string = after;
      Util_SplitString(&item,&after,split_string," ");
    }
      
    if (item) free(item); 
    if (after)free(after); 
    if (split_string) free(split_string);
  }

  /* We initialize the active_slicing_handle at CACTUS_BASE 
     for the first time. This is important as some thorns might depend on
     slicing info quite early.
     Problem: if s.th. uses this handle before we set it, its not initialized!
     FIXEME: this routine should be registered at startup AFTER 
     the Startup things. */
    Einstein_SetNextSlicing(GH);


#ifdef ESLIC_DEBUG   
  if (CCTK_Equals(slicing_verbose,"yes")) 
  { 
    int h;
    /* Some diagnostic output */
    for (h=0;h<num_slicings;h++) 
    {
      slic = Util_GetHandledData(Eslicings,h);
      if (!slic) 
      {
	CCTK_WARN(0,"Cannot get slicing for handle.");
      }
      printf(" Einstein_ActivateSlicing:  >>%s<<   priority: %d   %s \n",
	     slic->name,
	     slic->param_active,
	     (char*)((slic->timetoslice)?"FUNC":"nofunc "));
    }
  }
#endif

  USE_CCTK_PARAMETERS; }



 /*@@
   @routine    Einstein_GetSlicingHandle
   @date       Thu Jul 22 11:30:47 1999
   @author     Gerd Lanfermann
   @desc 
     When called with the name of a (registered!!) slicing, 
     it returns the associated handle (a number). This handle can be used
     to compare to active_slicing_handle, which defines the slicing 
     for the next iteration. 
   @enddesc 
   @calls   CCTK_GetHandle.c  
   @calledby   
   @history 
 
   @endhistory 

@@*/

int Einstein_GetSlicingHandle(const char *name) 
{
  int handle;
  
  handle =  Util_GetHandle(Eslicings,name,NULL);
  if (handle<0) 
  {
    CCTK_VWarn(1,__LINE__,__FILE__,CCTK_THORNSTRING,
	       "Einstein_GetSlicingHandle: cannot get handle for slicing %s",
	       name);
  }
  return(handle);
}

int CCTK_FCALL einstein_getslicinghandle_(ONE_FORTSTRING_ARG) 
{
  int handle;
  ONE_FORTSTRING_CREATE(name)
  handle = Util_GetHandle(Eslicings,name,NULL);
  free(name);
  return(handle);
}



/*@@
   @routine    Einstein_SetNextSlicing
   @date       Thu Jul 22 11:30:47 1999
   @author     Gerd Lanfermann
   @desc 
   Sets the param_active flag to a number: 
   0    indicates not active (e.g. not specified in the parameter file)
   1..n priority of the slicing, in the order it is specified in the par file,
        e.g. "1+log maximal static"

	This is only called from CCTK_PRESTEP
   @calls 
   @calledby   
   @history 
   @endhistory 
@@*/

void Einstein_SetNextSlicing(cGH *GH) 
{
  DECLARE_CCTK_PARAMETERS

  struct    Einstein_slicing *slic;
  CCTK_INT  *active_slicing_handle;
  int i;
  char *info;
  
  int h,doicare = 0;
  int h_yes = num_slicings, 
      h_no  = num_slicings,
      h_egal= num_slicings;


  /* Get our grid scalar pointer */
  i = CCTK_VarIndex("Einstein::active_slicing_handle");
  if (i<0) 
  {
    CCTK_WARN(0,"Cannot find grid scalar: active_slicing_handle");
  }
  active_slicing_handle = (CCTK_INT *)CCTK_VarDataPtrI(GH,0,i);

  if (!CCTK_Equals(slicing,"mixed")) 
  {
    h = Util_GetHandle(Eslicings,slicing,NULL);
    if (h<0) 
    {
      CCTK_WARN(0,"Slicing parameter specifies non-activated/registered slicing!");
    }
      
    slic = (struct Einstein_slicing *)Util_GetHandledData(Eslicings,h);
    if (!slic) 
    {
      CCTK_WARN(0,"Slicing registry out of sync! No slicing found!");
    }
      
    /* Set the active handle to the only slicing and return*/
    /*$*active_slicing_handle = h;$*/
    h_yes = h;
    
  }
  /* We run mixed: Loop over all registered handles, 
     check if active, get the timetoslice function and decide which slicing
     to use  */
  else
  {
    for (h=0;h<num_slicings;h++) 
    {
#ifdef ESLIC_DEBUG	  
      printf("HANDLE LOOP: %d of %d\n",h,num_slicings);
#endif	  
      slic = (struct Einstein_slicing *)Util_GetHandledData(Eslicings,h);
      if (!slic)
      {
	CCTK_WARN(0,"Slicing registry out of sync! No slicing found!");
      }
      else
      {
	/* we have two competing flags: the priority and the 
	   timetoslice function. We pick two handles for
	   1) highest priority (lowest param_active number) 
	   for timetoslice = yes  (1)
	   2) highest priority (lowest param_active number) 
	   for timetoslice = don't care (0)
	   set the active_slicing_handle to one of the two handles 
	   in this order
	*/
	if ((slic->param_active>0)) 
	{
	  doicare = 0;
	  if (slic->timetoslice!=NULL) 
	  {
	    doicare = slic->timetoslice(GH);
	  }
	  if ((doicare== SLICING_YES) && (slic->param_active<h_yes))  
	  {
	    h_yes = h; 
	  }
	  if ((doicare== SLICING_DONTCARE) && (slic->param_active<h_egal)) 
	  {
	    h_egal= h;
	  }
	  if ((doicare== SLICING_NO) && (slic->param_active<h_no ))  
	  {
	    h_no  = h;
	  }
	}
#ifdef ESLIC_DEBUG
	printf(" handle:%d name:%s param_active:%d h_yes:%d h_egal:%d \n",
	       h,slic->name,slic->param_active,h_yes, h_egal);
#endif
      }
    }
  }
  
  /* First try to set the scalar to the highest priority that 
     cares (h_yes), else to the highest priority that doesn;t 
     care (h_egal). If that fails, we are in trouble.  */

  if (h_yes!=num_slicings) 
  {
    *active_slicing_handle = h_yes;
  }
  else 
  {
    if (h_egal != num_slicings)
    {
      *active_slicing_handle = h_egal;
    }
    else 
    {
      printf("ERROR: no active slicing found: hyes/hegal/hno: %d %d %d \n",
	     h_yes,h_egal,h_no);
    }
  }
  
  if (CCTK_Equals(slicing_verbose,"yes")) 
  { 
    info = (char*)malloc(256*sizeof(char));
    slic = (struct  Einstein_slicing *)
      Util_GetHandledData(Eslicings,*active_slicing_handle);
    if (!slic) 
    {
      CCTK_WARN(0,"Cannot find slicing handle");
    }
    
    sprintf(info,"Next slicing: %s (%d)",
	    slic->name,*active_slicing_handle);
    CCTK_INFO(info);
    if (info) free(info);
  }

  

  USE_CCTK_PARAMETERS; }





   
    
    
