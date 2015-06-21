#ifdef SPEC_CPU
# define THORN_IS_CactusBindings
#endif /* SPEC_CPU */
#define THORN_IS_Einstein

#include <stdarg.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctki_ScheduleBindings.h"

/* Prototypes for functions to be registered. */


/*@@
  @routine    CCTKi_BindingsParameterRecovery_Einstein
  @date       
  @author     
  @desc 
  Creates the parameter recovery bindings for thorn Einstein
  @enddesc 
  @calls     
  @calledby   
  @history 

  @endhistory

@@*/
int CCTKi_BindingsParameterRecovery_Einstein(void);
int CCTKi_BindingsParameterRecovery_Einstein(void)
{
  DECLARE_CCTK_PARAMETERS
  int result = 0;


if (!CCTK_Equals(shift,"none")) 

{


}

if (use_conformal) {


}

if (use_conformal_derivs) {


}

if (use_mask) {



}


if (einstein_register_slicing)

{


}


if (CCTK_Equals(initial_data,"flat")) 

{


}

if (CCTK_Equals(initial_lapse,"one") || CCTK_Equals(slicing,"geodesic"))

{


}

if (CCTK_Equals(initial_lapse,"gaussian"))

{


}

if (CCTK_Equals(initial_lapse,"psiminustwo"))

{


}

if (CCTK_Equals(initial_lapse,"isotropic"))

{


}

if (!CCTK_Equals(shift,"none"))

{

   if (CCTK_Equals(initial_shift,"zero"))

   {


   }

   if (CCTK_Equals(initial_shift,"rotation"))

   {


   }

}

if (einstein_register_slicing)

{


}




if (CCTK_Equals(timestep_method,"courant") || CCTK_Equals(timestep_method,"courant_time")) 

{


}


  USE_CCTK_PARAMETERS;   return (result);
}
