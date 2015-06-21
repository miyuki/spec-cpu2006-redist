#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/*@@
   @file      Banner.c
   @date      July 16 00:11:26 1999
   @author    Gabrielle Allen
   @desc
              Routines to deal with the Cactus banners.
   @enddesc
   @version   $Id: Banner.c,v 1.33 2002/01/04 13:30:18 tradke Exp $
 @@*/

/* #define DEBUG_BANNER 1 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cctk_Banner.h"
#include "cctk_Config.h"
#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_Parameter.h"
#include "cctk_Version.h"
#include "cctk_CommandLine.h"
#include "util_Network.h"
#include "util_String.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/Banner.c,v 1.33 2002/01/04 13:30:18 tradke Exp $";

CCTK_FILEVERSION(main_Banner_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

#define DATALENGTH 255

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

int CCTKi_PrintBanners (void);
void CCTKi_CactusBanner (void);
void  CCTK_FCALL cctk_registerbanner_
                            (int *ierr, ONE_FORTSTRING_ARG);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

static int number_banners = 0;
static char **banner_strings = NULL;
static const char *delimiter = "--------------------------------------------------------------------------------\n";

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/


 /*@@
   @routine    CCTKi_CactusBanner
   @date       Wed Oct 13 21:41:28 CEST 1999
   @author     Gabrielle Allen
   @desc
               Prints the Cactus Banner
   @enddesc
   @calls      CCTK_FullVersion
               CCTK_CompileDate
               CCTK_CompileTime
               Util_CurrentDate
               Util_CurrentTime
               Util_GetHostName
               CCTK_CommandLine
               CCTK_ParameterFilename
@@*/
void CCTKi_CactusBanner (void)
{
  char **commandargs;
  char buffer[DATALENGTH+1];
  const char *banner = "       10                                  \n"
                       "  1   0101       ************************  \n"
                       "  01  1010 10      The Cactus Code V4.0    \n"
                       " 1010 1101 011      www.cactuscode.org     \n"
                       "  1001 100101    ************************  \n"
                       "    00010101                               \n"
                       "     100011     (c) Copyright The Authors  \n"
                       "      0100      GNU Licensed. No Warranty  \n"
                       "      0101                                 \n";


  puts (delimiter);
  puts (banner);
  puts (delimiter);

  printf ("Cactus version: %s\n", CCTK_FullVersion ());
#if !defined(SPEC_CPU)
  printf ("Compile date:   %s (%s)\n", CCTK_CompileDate (), CCTK_CompileTime());
  Util_CurrentDate (DATALENGTH, buffer);
  printf ("Run date:       %s", buffer);
  Util_CurrentTime (DATALENGTH, buffer);
  printf (" (%s)\n", buffer);
  Util_GetHostName (buffer, DATALENGTH);
  printf ("Run host:       %s\n", buffer);
  CCTK_CommandLine (&commandargs);
  printf ("Executable:     %s\n", commandargs[0]);
#endif /* SPEC_CPU */
  CCTK_ParameterFilename (DATALENGTH, buffer);
  printf ("Parameter file: %s\n", buffer);

  puts (delimiter);
}


 /*@@
   @routine    CCTK_RegisterBanner
   @date       July 16 00:11:26 1999
   @author     Gabrielle Allen
   @desc
               Registers a string as a banner
   @enddesc

   @var        banner
   @vdesc      The banner as a C string
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0 for success, or -1 if memory allocation failed
   @endreturndesc
@@*/
int CCTK_RegisterBanner (const char *banner)
{
  int retval;
  char **old_banner_strings;


  retval = 0;
  number_banners++;

  /* Resize the array of banner strings */
  if (number_banners == 1)
  {
    banner_strings = (char **) malloc (number_banners * sizeof (char *));
  }
  else
  {
    old_banner_strings = banner_strings;
    banner_strings = (char **) realloc (banner_strings,
                                        number_banners * sizeof (char *));
    if (banner_strings == NULL)
    {
      banner_strings = old_banner_strings;
      number_banners--;
    }
  }

  /* If this was succesful, copy the data into the array */
  if (banner_strings)
  {
    banner_strings[number_banners - 1] = Util_Strdup (banner);
    if (banner_strings[number_banners - 1] == NULL)
    {
      number_banners--;
      retval = -1;
    }
  }
  else
  {
    retval = -1;
  }

#ifdef DEBUG_BANNER
  printf("Registering banner .... \n%s\n",banner_strings[number_banners-1]);
#endif

  return (retval);
}


void  CCTK_FCALL cctk_registerbanner_
                            (int *ierr,
                             ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (message)
  *ierr = CCTK_RegisterBanner (message);
  free (message);
}


 /*@@
   @routine    CCTKi_PrintBanners
   @date       July 16 00:11:26 1999
   @author     Gabrielle Allen
   @desc
               Print all registered banners
   @enddesc
   @calls      CCTK_ParameterGet

   @returntype int
   @returndesc
               0 -- success
   @endreturndesc
@@*/
int CCTKi_PrintBanners (void)
{
  int i;
  const CCTK_INT *cctk_show_banners;


  cctk_show_banners = (const CCTK_INT *)
                      CCTK_ParameterGet ("cctk_show_banners", "Cactus", &i);
  if (*cctk_show_banners)
  {
    for (i = 0; i < number_banners; i++)
    {
      if (banner_strings[i])
      {
        printf ("%s%s\n", delimiter, banner_strings[i]);
      }
    }
    puts (delimiter);
  }

  return (0);
}
