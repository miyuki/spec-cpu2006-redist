#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      String.c
   @date      Tue May  2 10:44:19 2000
   @author    Tom Goodale
   @desc
              Routines dealing with strings.
   @enddesc
   @version   $Id: String.c,v 1.10 2002/01/28 19:45:20 tradke Exp $
 @@*/

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "util_String.h"

#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/String.c,v 1.10 2002/01/28 19:45:20 tradke Exp $";

CCTK_FILEVERSION(util_String_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine    CCTK_StrSep
   @date       Tue May  2 10:29:07 2000
   @author     Tom Goodale
   @desc
     The strsep() function returns the next token from the string stringp which is delimited by delim.  The token
     is terminated with a `\0' character and stringp is updated to point past the token.

     RETURN VALUE
     The strsep() function returns a pointer to the token, or NULL if delim is not found in stringp.

   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     stringp
   @vdesc   The string to search for a token in.
   @vtype   const char **stringp
   @vio     inout
   @vcomment

   @endvar
   @var     delim
   @vdesc   The delimiter
   @vtype   const char *delim
   @vio     in
   @vcomment

   @endvar

   @returntype const char *
   @returndesc
   a pointer to the token, or NULL if delim is not found in stringp.
   @endreturndesc

@@*/
const char *Util_StrSep(const char **stringp, const char *delim)
{
  int retlength = 0;
  static char *retval = NULL;
  char *temp;
  const char *start;
  const char *end;

  start = *stringp;

  end = strstr(start, delim);

  /* Is the delimiter part of the string */
  if(end)
  {
    if(retlength < (end-start)+1)
    {
      temp = realloc(retval, (end-start+1));

      if(temp)
      {
        retval = temp;
        retlength = end-start+1;
      }
      else
      {
        free(retval);
        retval = NULL;
        retlength = 0;
      }
    }

    if(retval)
    {
      strncpy(retval, start, (size_t)(end-start));
      retval[end-start] = '\0';

      *stringp = end+strlen(delim);
    }

  }
  else
  {
    free(retval);
    retval = NULL;
    retlength = 0;
  }

  return retval;
}

 /*@@
   @routine    Util_SplitString
   @date       Wed Jan 20 10:14:00 1999
   @author     Tom Goodale
   @desc
   Splits a string into two parts at the given seperator.
   Assigns memory for the two resulting strings, so this should be freed
   when no longer needed.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     before
   @vdesc   String before seperator
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     after
   @vdesc   String after seperator
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     string
   @vdesc   String to seperate
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     sep
   @vdesc   String seperator
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0 - success
   1 - seperator not found
   2 - out of memory
   @endreturndesc

@@*/
int Util_SplitString(char **before, char **after, const char *string, const char *sep)
{
  int retval=0;
  char *position;

  /* Find location of the seperator */
  position = strstr(string, sep);

  if(position)
  {
    /*Allocate memory for return strings. */
    *before = (char *)malloc((size_t)((position-string+1)*sizeof(char)));
    *after  = (char *)malloc((size_t)((strlen(string)-(position-string)-strlen(sep)+1)*sizeof(char)));

    /* Check that the allocation succeeded. */
    if(!*before || !*after)
    {
      free(*before);
      *before = NULL;
      free(*after);
      *after = NULL;
      retval = 2;
    }
    else
    {
      retval = 3;
    }
  }
  else
  {
    *before = NULL;
    *after = NULL;
    retval = 1;
  }

  if(position && *before && *after)
  {
    /* Copy the data */
    strncpy(*before, string, (int)(position-string));
    (*before)[(int)(position-string)] = '\0';

    strncpy(*after, position+strlen(sep), strlen(string)-(int)(position-string)-strlen(sep));
    (*after)[strlen(string)-(position-string)-strlen(sep)] = '\0';

    retval = 0;
  }

  return retval;
}

 /*@@
   @routine    Util_Strdup
   @date       Thu Mar 28 11:20:27 2000
   @author     Gerd Lanfermann
   @desc
   Homegrown ersion of strdup, since it's not guaranteed to be there.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     s
   @vdesc   string to be duplicated
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype char *
   @returndesc
   the duplicate string.
   @endreturndesc
@@*/
char *Util_Strdup(const char *s)
{
  char *retstr=NULL;

  retstr = (char*) malloc((strlen(s)+1)*sizeof(char));
  if(retstr)
  {
    strcpy(retstr,s);
  }

  return retstr;
}

 /*@@
   @routine    Util_StrCmpi
   @date       Mon Jul  5 01:19:00 1999
   @author     Tom Goodale
   @desc
   Case independent strcmp
   @enddesc
   @calls
   @calledby
   @history
   @hdate Wed Oct 13 15:30:57 1999 @hauthor Tom Goodale
   @hdesc Checks the length of the two string first.
   @endhistory
   @var     string1
   @vdesc   First string in comparison
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     string2
   @vdesc   Second string in comparison
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   +ve - string1 > string2
   0   - string1 = string2
   -ve - string1 < string2
   @endreturndesc
@@*/
int Util_StrCmpi (const char *string1, const char *string2)
{
  int retval;


  do
  {
    retval = tolower (*string1) - tolower (*string2);
  } while (! retval && *string1++ && *string2++);

  return (retval);
}

 /*@
   @routine    Util_SplitFilename
   @date       Wed Oct 4 10:14:00 2000
   @author     Gabrielle Allen
   @desc
   Splits a filename into its directory and basic filename parts.
   Assigns memory for the two resulting strings, so this should be freed
   when no longer needed.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     dir
   @vdesc   The directory part
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     file
   @vdesc   The file part
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     string
   @vdesc   The string to split
   @vtype   const char *
   @vio     out
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0  - success
   -1 - out of memory
   @endreturndesc
@@*/
int Util_SplitFilename (char **dir, char **file, const char *string)
{
  char *position;


  *file = Util_Strdup (string);

  if (*file)
  {
    /* Find location of the seperator */
    position = strrchr (*file, '/');
    if (position)
    {
      *dir = *file;
      *position = 0;
      *file = Util_Strdup (position + 1);
    }
    else
    {
      *dir = NULL;
    }
  }

  return (*file ? 0 : -1);
}

 /*@@
   @routine    Util_asprintf
   @date       Thu May 24 16:55:26 2001
   @author     Tom Goodale
   @desc
   Sprintf with memory allocation.  On input
   the buffer should point to a NULL area of memory.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     buffer
   @vdesc   Buffer to which to print the string.
   @vtype   char **
   @vio     out
   @vcomment
   *buffer should be NULL on entry.  The routine
   allocates the memory, so the previous contents of
   the pointer are lost.
   On exit the buffer size will be return-value+1 (i.e
   the length of the string plus the \0 ).
   @endvar
   @var     format
   @vdesc   sprintf format string
   @vtype   const char *
   @vio     in
   @vcomment
   This is a standard sprintf format string.
   @endvar
   @var     ...
   @vdesc   Rest of arguments
   @vtype   varargs
   @vio     in
   @vcomment
   These are the arguments necessary for the format string.
   @endvar

   @returntype int
   @returndesc
   The number of bytes written to the buffer.
   @endreturndesc
@@*/
int Util_asprintf(char **buffer, const char *fmt, ...)
{
  int count;
  va_list args;

  va_start(args,fmt);

  count = Util_vsnprintf(NULL, 0, fmt, args);

  *buffer = (char *)malloc(count+1);

  if(*buffer)
  {
    va_start(args,fmt);

    Util_vsnprintf(*buffer,count+1,fmt,args);

    va_end(args);
  }
  else
  {
    count = 0;
  }

  return count;
}

 /*@@
   @routine    Util_asprintf
   @date       Thu May 24 16:55:26 2001
   @author     Tom Goodale
   @desc
   Sprintf with memory allocation if necessary.  On input
   the buffer should point to an area of memory of length 'size' .
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     buffer
   @vdesc   Buffer to which to print the string.
   @vtype   char **
   @vio     out
   @vcomment
   Buffer to which to print string.  If the buffer is too
   small, the buffer is freed and a new buffer big enough to hold
   the string and its null-termination is created.
   @endvar
   @var     size
   @vdesc   initial size of the buffer
   @vtype   int
   @vio     in
   @vcomment
   This is the initial size of the buffer.
   @endvar
   @var     format
   @vdesc   sprintf format string
   @vtype   const char *
   @vio     in
   @vcomment
   This is a standard sprintf format string.
   @endvar
   @var     ...
   @vdesc   Rest of arguments
   @vtype   varargs
   @vio     in
   @vcomment
   These are the arguments necessary for the format string.
   @endvar

   @returntype int
   @returndesc
   The number of bytes written to the buffer.
   @endreturndesc
@@*/
int Util_asnprintf(char **buffer, size_t size, const char *fmt, ...)
{
  size_t count;
  va_list args;

  va_start(args,fmt);

  count = Util_vsnprintf(NULL, 0, fmt, args);

  if(count+1 > size)
  {
    /* Use free followed by malloc as realloc may copy memory
     * we are not interested in.
     */
    free(*buffer);
    *buffer = (char *)malloc(count+1);
  }

  if(*buffer)
  {
    va_start(args,fmt);

    Util_vsnprintf(*buffer,count+1,fmt,args);

    va_end(args);
  }
  else
  {
    count = 0;
  }

  return count;
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/



#ifdef TEST_Util_STRSEP

#include <stdio.h>

int main(int argc, char *argv[])
{
  const char *argument;
  char *delim;
  const char *token;

  if(argc < 3)
  {
    printf("Usage: %s <string> <delim>\n", argv[0]);
    exit(1);
  }

  argument = argv[1];
  delim = argv[2];

  while((token = Util_StrSep(&argument, delim)))
  {
    printf("Token is     '%s'\n", token);
  }

  if(argument - argv[1] < strlen(argv[1]))
  {
    printf("Remainder is '%s'\n", argument);
  }

  return 0;

}
#endif /*TEST_CCTK_STRSEP */
