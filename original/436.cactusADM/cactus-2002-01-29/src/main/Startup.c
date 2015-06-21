/*@@
   @file      Startup.c
   @date      July 29 1999
   @author    Joan Masso
   @desc 
    This is the artistic contribution from Joan to the flesh code ;-)
   @enddesc 
 @@*/

#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/Startup.c,v 1.8 2001/05/10 12:35:16 goodale Exp $";

CCTK_FILEVERSION(main_Startup_c)

int CactusStartup(void)
{
  
  const char *string;

#define B_1 "       10                                  "
#define B_2 "  1   0101       ************************  "
#define B_3 "  01  1010 10      The Cactus Code V4.0    "
#define B_4 " 1010 1101 011      www.cactuscode.org     "
#define B_5 "  1001 100101    ************************  "
#define B_6 "    00010101                               "
#define B_7 "     100011     (c) Copyright The Authors  "
#define B_8 "      0100      GNU Licensed. No Warranty  "
#define B_9 "      0101                                 "


#define B_ANNERLINE B_1 "\n" B_2 "\n" B_3 "\n" B_4 "\n" B_5 "\n" B_6 "\n" B_7 "\n" B_8 "\n" B_9 "\n"
 
  string = B_ANNERLINE;

  CCTK_RegisterBanner(string);
  
  return 0;

}
