 /*@@
   @file      datestamp.c
   @date      Mon May 11 10:20:58 1998
   @author    Paul Walker
   @desc 
              Functions to return the Cactus compile time and version.
   @enddesc 
   #version   $Id: datestamp.c,v 1.13 2001/12/06 15:07:33 tradke Exp $
 @@*/

#include <stdio.h>
#include <string.h>

#include "cctk_Version.h"
#include "cctki_version.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/datestamp.c,v 1.13 2001/12/06 15:07:33 tradke Exp $";

const char *CCTKi_version_src_datestamp_c(void);
const char *CCTKi_version_src_datestamp_c(void) { return rcsid; }

 /*@@
   @routine    CCTKi_DateStamp
   @date       Mon May 11 10:20:58 1998
   @author     Paul Walker
   @desc 
               Prints the date stamp when Cactus was compiled
               as a formatted string to stdout.
   @enddesc 
@@*/
void CCTKi_DateStamp (void) 
{
  printf ("  Compiled on %s at %s\n", __DATE__, __TIME__);
}


 /*@@
   @routine    CCTK_CompileTime
   @date       Mon May 11 10:20:58 1998
   @author     Paul Walker
   @desc 
               Returns a pointer to a formatted string with the time stamp
               when Cactus was compiled.
   @enddesc 

   @returntype  const char *
   @returndesc
                pointer to the static time stamp string buffer
   @endreturndesc
@@*/
const char *CCTK_CompileTime (void) 
{
  return (__TIME__);
}


 /*@@
   @routine    CCTK_CompileDate
   @date       Mon May 11 10:20:58 1998
   @author     Paul Walker
   @desc 
               Returns a pointer to a formatted string with the date stamp
               when Cactus was compiled.
   @enddesc 

   @returntype  const char *
   @returndesc
                pointer to the static date stamp string buffer
   @endreturndesc
@@*/
const char *CCTK_CompileDate (void) 
{
  static char date[sizeof (__DATE__) + 1];


  /* make the day really a 2-digit number to be consistent with
     CCTK_CurrentDate() */
  strcpy (date, __DATE__);
  if (date[4] == ' ')
  {
    date[4] = '0';
  }

  return (date);
}


/* Macros to turn things into strings. */

#define STRINGIFY(a) REALSTRINGIFY(a)

#define REALSTRINGIFY(a) #a


 /*@@
   @routine    CCTK_FullVersion CCTK_MajorVersion CCTK_MinorVersion
   @date       Mon May 11 10:20:58 1998
   @author     Paul Walker
   @desc 
               Returns a pointer to a formatted string with the current Cactus
               version.
   @enddesc 

   @returntype  const char *
   @returndesc
                pointer to the static version string buffer
   @endreturndesc
@@*/
const char *CCTK_FullVersion (void)
{
  return (STRINGIFY(CCTK_VERSION));
}

const char *CCTK_MajorVersion (void)
{
  return (STRINGIFY(CCTK_VERSION_MAJOR));
}

const char *CCTK_MinorVersion (void)
{
  return (STRINGIFY(CCTK_VERSION_MINOR));
}

const char *CCTK_MicroVersion (void)
{
  return (STRINGIFY(CCTK_VERSION_OTHER));
}

/*#define MAKETEST*/
#ifdef MAKETEST
int main(void)
{
  printf("CCTK maketest compiled on %s\n", compileDate());

  return 0;
}
#endif
