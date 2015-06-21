#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Time.c
   @date      Wed Sep 17 2000
   @author    Gabrielle Allen
   @desc 
   Date and time routines
   @enddesc 
   @version $Header: /cactus/Cactus/src/util/Time.c,v 1.4 2001/11/05 14:58:55 tradke Exp $
 @@*/

/* #define DEBUG_TIME */

#include <time.h>
#include <string.h>

#include "cctk_Flesh.h"
#include "cctk_Misc.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/Time.c,v 1.4 2001/11/05 14:58:55 tradke Exp $";

CCTK_FILEVERSION(util_Time_c)


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    Util_CurrentTime
   @date       Tue Sep 19
   @author     Gabrielle Allen
   @desc 
   Fills string with current local time, returning the number
   of characters filled.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int Util_CurrentTime(int len, char *now)
{
  int retval;
  time_t timep;
  const char *fmt = "%X";

#ifdef SPEC_CPU
  return 0;
#else
  timep = time(NULL);
  strftime(now, len, fmt, localtime(&timep));

  retval = strlen(now);
  retval=retval > len ? 0 : retval;

#ifdef DEBUG_TIME
  printf("CurrentTime = %s\n",now);
#endif

  return retval;
#endif
}


 /*@@
   @routine    Util_CurrentDate
   @date       Tue Sep 19
   @author     Gabrielle Allen
   @desc 
   Fills string with current local date, returning the number of
   characters filled.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int Util_CurrentDate(int len, char *now)
{
  int retval;
  time_t timep;
  const char *fmt = "%b %d %Y";

#ifdef SPEC_CPU
  return 0;
#else
  timep = time(NULL);
  strftime(now, 50, fmt, localtime(&timep));

  retval = strlen(now);
  retval=retval > len ? 0 : retval;

#ifdef DEBUG_TIME
  printf("CurrentDate = %s\n",thedate);
#endif

  return retval;
#endif
}
