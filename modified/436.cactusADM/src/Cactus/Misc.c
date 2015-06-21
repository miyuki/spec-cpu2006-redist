#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Misc.c
   @date      Wed Jan 20 10:06:35 1999
   @author    Tom Goodale
   @desc
   Miscellaneuous routines.
   @enddesc
   @version $Header: /cactus/Cactus/src/util/Misc.c,v 1.61 2001/12/04 10:42:39 allen Exp $
 @@*/

/*#define DEBUG_MISC*/

#include "cctk_Config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#include <limits.h>
#include <math.h>
#include <float.h>

#include "cctk_GNU.h"

#include "cctk_Flesh.h"
#include "cctk_Misc.h"
#include "cctk_FortranString.h"
#include "cctk_WarnLevel.h"
#include "util_String.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/Misc.c,v 1.61 2001/12/04 10:42:39 allen Exp $";

CCTK_FILEVERSION(util_Misc_c)


/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

int CCTK_RegexMatch(const char *string,
                    const char *pattern,
                    const int nmatch,
                    regmatch_t *pmatch);
int CCTK_SetStringInRegexList(char **data, const char *value,
                                     int n_elements, ...);
void CCTK_PrintString(const char *data);

int CCTK_FCALL cctk_equals_
     (const char **arg1,ONE_FORTSTRING_ARG);

void CCTK_FCALL cctk_printstring_
     (const char **arg1);

void CCTK_FCALL cctk_fortranstring_
                           (CCTK_INT *nchars,
                            const char *const *cstring,
                            ONE_FORTSTRING_ARG);

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
   @routine    CCTK_Equals
   @date       Wed Jan 20 10:25:30 1999
   @author     Tom Goodale
   @desc
   Does a case independent comparison of strings.
   Returns true if they are equal.
   @enddesc
   @calls
   @calledby
   @history

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
   1 - equal
   0 - not equal
   @endreturndesc
@@*/
int CCTK_Equals(const char *string1, const char *string2)
{
  int retval;

  retval = 1;

  /* Check that string1 isn't null */
  if (!string1 || !string2)
  {
    retval = 0;
    if (!string1 && string2)
    {
      CCTK_VWarn(0,__LINE__,__FILE__,"Cactus",
                 "CCTK_Equals: First string null (2nd is %s)",string2);

    }
    else if (string1 && !string2)
    {
      CCTK_VWarn(0,__LINE__,__FILE__,"Cactus",
                 "CCTK_Equals: Second string null (1st is %s)",string1);
    }
    else
    {
      CCTK_Warn(0,__LINE__,__FILE__,"Cactus",
                "CCTK_Equals: Both strings null");
    }
  }
  else
  {
    if (Util_StrCmpi(string1,string2))
    {
      retval = 0;
    }
  }
  return retval;
}

int CCTK_FCALL cctk_equals_
     (const char **arg1,ONE_FORTSTRING_ARG)
{
  int retval;
  ONE_FORTSTRING_CREATE(arg2)
  retval = CCTK_Equals(*arg1,arg2);
  free(arg2);
  return(retval);
}


 /*@@
   @routine Util_NullTerminateString
   @author Paul Walker
   @desc
   Null terminates a fortran string. Need to free the
   string it returns
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     instring
   @vdesc   String to null terminate
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     len
   @vdesc   Length of string to be null terminated
   @vtype   unsigned int
   @vio     in
   @vcomment

   @endvar

   @returntype char *
   @returndesc
   The null terminated string.
   @endreturndesc
@@*/

char *Util_NullTerminateString(const char *instring, unsigned int len)
{
  char *outstring;
  unsigned int i;
  unsigned int position;

  if (len > 100000)
  {
    CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
               "Null Terminating a string with length %d !!\n"
               "This is probably an error in calling a C routine from Fortran",
               len);
  }

#ifdef DEBUG_MISC
  printf("Util_NullTerminateString: -%s-, (%u)\n",instring,len);
#endif

  position = len;
  while (position > 0 && instring[position-1] == ' ')
  {
    position--;
  }

  outstring = (char *)malloc((position+1)*sizeof(char));

  if (outstring)
  {
    for (i=0;i<position;i++)
    {
      outstring[i] = instring[i];
    }
    outstring[position] = '\0';
  }
  return(outstring);
}


 /*@@
   @routine    Util_InList
   @date       Wed Jan 20 10:31:25 1999
   @author     Tom Goodale
   @desc
   Determines if a string is in a list of other strings.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     string1
   @vdesc   The string to search for
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     ...
   @vdesc   List of strings to search.
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - In list
   0 - Not in list
   @endreturndesc
@@*/
int Util_InList(const char *string1, int n_elements, ...)
{
  int retval;
  int arg;
  va_list ap;

  char *element;

  retval = 0;

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(CCTK_Equals(string1, element))
    {
      retval = 1;
      break;
    }
  }

  va_end(ap);

  return retval;

}


 /*@@
   @routine    Util_IntInRange
   @date       Wed Jan 20 10:32:36 1999
   @author     Tom Goodale
   @desc
   This routine will determine if an integer is in the range specified
   in the range string.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     inval
   @vdesc   The value to check
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     range
   @vdesc   The range to look in
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range
   0 - not in range
   @endreturndesc
@@*/
int Util_IntInRange(int inval, const char *range)
{
  int retval;
  regmatch_t pmatch[6];
  int start_closed, end_closed;
  int start, end, step;

  retval = 0;

  /* Valid ranges are of the form start:end:step
   * possibly preceeded by a [ or ( and ended by a ) or ] to indicate
   * closure.  The end and step are optional.  A * can also be used
   * to indicate any value.
   *
   * The following regular expression may match five substrings:
   *
   * 1 - [ or (
   * 2 - start
   * 3 - end
   * 4 - step
   * 5 - ) or ]
   */

  if(CCTK_RegexMatch(range,
                     "(\\[|\\()?([^]):]*):?([^]):]*)?:?([^]):]*)?(\\]|\\))?",
                     6, pmatch) != 0)
  {
    /* First work out if the range is closed at the lower end. */
    if(pmatch[1].rm_so != -1)
    {
      switch(range[pmatch[1].rm_so])
      {
        case '(' : start_closed = 0; break;
        case '[' :
        default  : start_closed = 1;
      }
    }
    else
    {
      start_closed = 1;
    }

    /* Next find the start of the range */
    if(pmatch[2].rm_so != -1 &&
       (pmatch[2].rm_eo-pmatch[2].rm_so > 0) &&
       range[pmatch[2].rm_so] != '*')
    {
      start = atoi(range+pmatch[2].rm_so);
    }
    else
    {
      /* No start range given, so use the smallest integer available. */
      start = INT_MIN;
    }

    /* Next find the end of the range */
    if(pmatch[3].rm_so != -1 &&
       (pmatch[3].rm_eo-pmatch[3].rm_so > 0) &&
       range[pmatch[3].rm_so] != '*')
    {
      end = atoi(range+pmatch[3].rm_so);
    }
    else
    {
      /* No end range given, so use the largest integer available. */
      end = INT_MAX;
    }

    /* Next find the step of the range */
    if(pmatch[4].rm_so != -1 && (pmatch[4].rm_eo-pmatch[4].rm_so > 0))
    {
      step = atoi(range+pmatch[4].rm_so);
    }
    else
    {
      /* No step given, so default to 1. */
      step = 1;
    }

    /* Finally work out if the range is closed at the upper end. */
    if(pmatch[5].rm_so != -1)
    {
      switch(range[pmatch[5].rm_so])
      {
        case ')' : end_closed = 0; break;
        case ']' :
        default  : end_closed = 1;
      }
    }
    else
    {
      end_closed = 1;
    }

    if(inval >= start + !start_closed &&
       inval <= end   - !end_closed   &&
       ! ((inval-start) % step))
    {
      retval = 1;
    }

  }
  else
  {
    CCTK_Warn(1, __LINE__, __FILE__, "Flesh", "Invalid range");
  }

  return retval;
}

 /*@@
   @routine    Util_DoubleInRange
   @date       Wed Jan 20 10:32:36 1999
   @author     Tom Goodale
   @desc
   This routine will determine if a double is in the range specified
   in the range string.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     inval
   @vdesc   The value to check
   @vtype   double
   @vio     in
   @vcomment

   @endvar
   @var     range
   @vdesc   The range to look in
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range
   0 - not in range
   @endreturndesc
@@*/
int Util_DoubleInRange(double inval, const char *range)
{
  int retval;
  regmatch_t pmatch[6];
  double start, end;
#if 0
  int start_closed, end_closed;
  double step;
#endif

  retval = 0;

  /* Valid ranges are of the form start:end:step
   * possibly preceeded by a [ or ( and ended by a ) or ] to indicate
   * closure.  The end and step are optional.  A * can also be used
   * to indicate any value.
   *
   * The following regular expression may match five substrings:
   *
   * 1 - [ or (
   * 2 - start
   * 3 - end
   * 4 - step
   * 5 - ) or ]
   */

  if(CCTK_RegexMatch(range,
                     "(\\[|\\()?([^]):]*):?([^]):]*)?:?([^]):]*)?(\\]|\\))?",
                     6, pmatch) != 0)
  {
#if 0
    /* First work out if the range is closed at the lower end. */
    if(pmatch[1].rm_so != -1)
    {
      switch(range[pmatch[1].rm_so])
      {
        case '(' : start_closed = 0; break;
        case '[' :
        default  : start_closed = 1;
      }
    }
    else
    {
      start_closed = 1;
    }
#endif

    /* Next find the start of the range */
    if(pmatch[2].rm_so != -1 &&
       (pmatch[2].rm_eo-pmatch[2].rm_so > 0) &&
       range[pmatch[2].rm_so] != '*')
    {
      start = atof(range+pmatch[2].rm_so);
    }
    else
    {
      /* No start range given, so use the smallest float available. */
      start = -FLT_MAX;
    }

    /* Next find the end of the range */
    if(pmatch[3].rm_so != -1 &&
       (pmatch[3].rm_eo-pmatch[3].rm_so > 0) &&
       range[pmatch[3].rm_so] != '*')
    {
      end = atof(range+pmatch[3].rm_so);
    }
    else
    {
      /* No end range given, so use the largest float available. */
      end = FLT_MAX;
    }

#if 0
    /* Next find the step of the range */
    if(pmatch[4].rm_so != -1 && (pmatch[4].rm_eo-pmatch[4].rm_so > 0))
    {
      step = atof(range+pmatch[4].rm_so);
    }
    else
    {
      /* No step given, so default to 1. */
      step = 1;
    }

    /* Finally work out if the range is closed at the upper end. */
    if(pmatch[5].rm_so != -1)
    {
      switch(range[pmatch[5].rm_so])
      {
        case ')' : end_closed = 0; break;
        case ']' :
        default  : end_closed = 1;
      }
    }
    else
    {
      end_closed = 1;
    }
#endif

    if(inval >= start /*+ !start_closed */&&
       inval <= end  /* - !end_closed */ /* &&
                                       ! ((inval-start) % step)*/)
    {
      retval = 1;
    }

  }
  else
  {
    CCTK_Warn(1, __LINE__, __FILE__, "Flesh", "Invalid range");
  }

  return retval;
}


 /*@@
   @routine    Util_IntInRangeList
   @date       Wed Jan 20 10:36:31 1999
   @author     Tom Goodale
   @desc
   Determines if an integer is in a given list of ranges.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     inval
   @vdesc   The value to check
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar

   @var     ...
   @vdesc   The list of ranges to look in
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range in list
   0 - not in range in list
   @endreturndesc
@@*/
int Util_IntInRangeList(int inval, int n_elements, ...)
{
  int retval;
  int arg;
  va_list ap;

  char *element;

  retval = 0;

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(Util_IntInRange(inval, element))
    {
      retval = 1;
      break;
    }
  }

  va_end(ap);

  return retval;

}


 /*@@
   @routine    Util_DoubleInRangeList
   @date       Wed Jan 20 10:36:31 1999
   @author     Tom Goodale
   @desc
   Determines if a double is in a given list of ranges.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     inval
   @vdesc   The value to check
   @vtype   double
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar

   @var     ...
   @vdesc   The list of ranges to look in
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range in list
   0 - not in range in list
   @endreturndesc
@@*/
int Util_DoubleInRangeList(double inval, int n_elements, ...)
{
  int retval;
  int arg;
  va_list ap;

  char *element;

  retval = 0;

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(Util_DoubleInRange(inval, element))
    {
      retval = 1;
      break;
    }
  }

  va_end(ap);

  return retval;

}


 /*@@
   @routine    CCTK_SetDoubleInRangeList
   @date       Thu Jan 21 09:41:21 1999
   @author     Tom Goodale
   @desc
   Sets the value of a double if the desired value is in one of
   the specified ranges.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   Pointer to the value to set
   @vtype   CCTK_REAL *
   @vio     out
   @vcomment

   @endvar
   @var     value
   @vdesc   The value to check
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar

   @var     ...
   @vdesc   The list of ranges to look in
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range in list
   0 - not in range in list
   @endreturndesc
@@*/
int CCTK_SetDoubleInRangeList(CCTK_REAL *data, const char *value,
                              int n_elements, ...)
{
  int retval;
  char temp[1001];
  unsigned int p;
  int arg;
  va_list ap;

  char *element;

  CCTK_REAL inval;

  retval = 1;

  /* Convert the value string to a double.
   * Allow various formats.
   */
  strncpy(temp, value, 1000);

  for (p=0;p<strlen(temp);p++)
  {
    if (temp[p] == 'E' ||
        temp[p] == 'd' ||
        temp[p] == 'D')
    {
      temp[p] = 'e';
      break;
    }
  }

  inval = atof(temp);

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(Util_DoubleInRange(inval, element))
    {
      retval = 0;
      *data = inval;
      break;
    }
  }

  va_end(ap);

  return retval;
}

 /*@@
   @routine    CCTK_SetIntInRangeList
   @date       Thu Jan 21 10:27:26 1999
   @author     Tom Goodale
   @desc
   Sets the value of an integer if the desired value is in one of
   the specified ranges.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   Pointer to the value to set
   @vtype   CCTK_INT *
   @vio     out
   @vcomment

   @endvar
   @var     value
   @vdesc   The value to check
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar

   @var     ...
   @vdesc   The list of ranges to look in
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range in list
   0 - not in range in list
   @endreturndesc
@@*/
int CCTK_SetIntInRangeList(CCTK_INT *data, const char *value,
                           int n_elements, ...)
{
  int retval;
  int arg;
  va_list ap;

  char *element;

  CCTK_INT inval;

  retval = 1;

  /* Convert the value string to an int.*/

  inval = atoi(value);

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(Util_IntInRange(inval, element))
    {
      retval = 0;
      *data = inval;
      break;
    }
  }

  va_end(ap);

  return retval;
}

 /*@@
   @routine    CCTK_SetKeywordInRangeList
   @date       Thu Jan 21 10:28:00 1999
   @author     Tom Goodale
   @desc
   Sets the value of a keyword if the desired value is in one of
   the specified ranges.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   Pointer to the value to set
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     value
   @vdesc   The value to check
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar

   @var     ...
   @vdesc   The list of ranges to look in
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range in list
   0 - not in range in list
   @endreturndesc
@@*/
int CCTK_SetKeywordInRangeList(char **data, const char *value,
                               int n_elements, ...)
{
  int retval;
  int arg;
  va_list ap;

  char *element;

  retval = 1;

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(CCTK_Equals(value, element))
    {
      if(*data) free(*data);
      *data = (char *)malloc((strlen(value)+1)*sizeof(char));
      if(*data)
      {
        strcpy(*data, value);
        retval = 0;
      }
      else
      {
        retval =-1;
      }
      break;
    }
  }

  va_end(ap);

  return retval;
}


 /*@@
   @routine    CCTK_SetStringInRegexList
   @date       Fri Apr 16 08:37:02 1999
   @author     Tom Goodale
   @desc
   Sets the value of a string if it matches any of the given regular
   expressions.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   Pointer to the value to set
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     value
   @vdesc   The value to check
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     n_elements
   @vdesc   The number of elements in the list
   @vtype   int
   @vio     in
   @vcomment

   @endvar

   @var     ...
   @vdesc   The list of ranges to look in
   @vtype   multiple const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - in range in list
   0 - not in range in list
   @endreturndesc
@@*/
int CCTK_SetStringInRegexList(char **data, const char *value,
                               int n_elements, ...)
{
  int retval;
  int arg;
  va_list ap;

  char *element;

  retval = 1;

  /* Walk through the element list. */
  va_start(ap, n_elements);

  for(arg = 0; arg < n_elements; arg++)
  {
    element = va_arg(ap, char *);

    if(CCTK_RegexMatch(value, element, 0, NULL))
    {
      retval = CCTK_SetString(data, value);
      break;
    }
  }

  va_end(ap);

  return retval;
}

 /*@@
   @routine    CCTK_SetString
   @date       Thu Jan 21 10:28:27 1999
   @author     Tom Goodale
   @desc
   Sets the value of a string
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   Pointer to the value to set
   @vtype   char **
   @vio     out
   @vcomment

   @endvar
   @var     value
   @vdesc   The value to check
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0  - success
   -1 - out of memory
   @endreturndesc
@@*/
int CCTK_SetString(char **data, const char *value)
{
  int retval;

  retval = 1;

  if(*data) free(*data);
  *data = (char *)malloc((strlen(value)+1)*sizeof(char));
  if(*data)
  {
    strcpy(*data, value);
    retval = 0;
  }
  else
  {
    retval = -1;
  }

  return retval;
}


 /*@@
   @routine    CCTK_SetBoolean
   @date       Thu Jan 21 10:35:11 1999
   @author     Tom Goodale
   @desc
   Sets the value of a boolean to true or false according to
   the value of the value string.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   Pointer to the value to set
   @vtype   CCTK_INT *
   @vio     out
   @vcomment

   @endvar
   @var     value
   @vdesc   The value to check
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0  - success
   -1 - out of memory
   @endreturndesc
@@*/
int CCTK_SetBoolean(CCTK_INT *data, const char *value)
{
  int retval = 1;

  if(Util_InList(value, 5, "true", "t", "yes", "y", "1"))
  {
    *data = 1;
    retval = 0;
  }
  else if(Util_InList(value, 5, "false", "f", "no", "n", "0"))
  {
    *data = 0;
    retval = 0;
  }
  else
  {
    retval = -1;
  }

  return retval;
}

 /*@@
   @routine    CCTK_RegexMatch
   @date       Fri Apr 16 08:40:14 1999
   @author     Tom Goodale
   @desc
   Perform a regular expression match of string against pattern.
   Also returns the specified number of matched substrings as
   give by regexec.
   This is a modified form of the example routine given in the SGI
   man page for regcomp.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     string
   @vdesc   String to match against
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     pattern
   @vdesc   Regex pattern
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar
   @var     nmatch
   @vdesc   The size of the pmatch array
   @vtype   const int
   @vio     in
   @vcomment

   @endvar
   @var     pmatch
   @vdesc   Array in which to place the matches
   @vtype   regmatch_t
   @vio     out
   @vcomment

   @endvar

   @returntype int
   @returndesc
   1 - pattern matches
   0 - pattern doesn't match
   @endreturndesc
@@*/

int CCTK_RegexMatch(const char *string,
                    const char *pattern,
                    const int nmatch,
                    regmatch_t *pmatch)
{
  int retval;
  int status;
  regex_t re;

  if (regcomp(&re, pattern, REG_EXTENDED) == 0)
  {
    status = regexec(&re, string, (size_t)nmatch, pmatch, 0);
    regfree(&re);
    if (status != 0)
    {
      retval = 0;      /* report error */
    }
    else
    {
      retval = 1;
    }
  }
  else
  {
    retval = 0;
  }

  return retval;
}

 /*@@
   @routine    CCTK_PrintString
   @date       Fri Apr 1 1999
   @author     Gabrielle Allen
   @desc
   Prints the value of a string (this is for fortran)
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     data
   @vdesc   string to print
   @vtype   const char *
   @vio     in
   @vcomment

   @endvar

@@*/
void CCTK_PrintString(const char *data)
{
  printf("%s\n",data);
}

void CCTK_FCALL cctk_printstring_
     (const char **arg1)
{
  CCTK_PrintString(*arg1);
}


/*@@
   @routine    CCTK_FortranString
   @date       Thu Jan 22 14:44:39 1998
   @author     Paul Walker
   @desc
               Copies a C string into a Fortran string.
   @enddesc

   @var        nchars
   @vdesc      Number of characters in the C string
   @vtype      CCTK_INT *
   @vio        out
   @vcomment
               It will copy only as many characters as fit into the fortran
               string. You should check for truncation by comparing 'nchars'
               against the length of your fortran string.
   @endvar
   @var        c_string
   @vdesc      C string to be copied
   @vtype      const char *const *
   @vio        in
   @endvar
   @var        ONE_FORTSTRING_ARG
   @vdesc      Fortran string
   @vtype      FORTRAN string macro
   @vio        out
   @endvar
@@*/
void CCTK_FCALL cctk_fortranstring_
                           (CCTK_INT *nchars,
                            const char *const *c_string,
                            ONE_FORTSTRING_ARG)
{
  size_t c_strlen;
  ONE_FORTSTRING_CREATE (fstring)
  ONE_FORTSTRING_PTR (fortran_string)


  *nchars = c_strlen = strlen (*c_string);
  if (c_strlen > (size_t) cctk_strlen1)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus",
                "CCTK_FortranString: fortran string buffer is too short to "
                "hold C string '%s, string will be truncated", *c_string);
    c_strlen = (size_t) cctk_strlen1;
  }

  /* copy up to the size of the fortran string
     and pad remaining chars in the fortran string with spaces */
  memcpy (fortran_string, *c_string, c_strlen);
  memset (fortran_string + c_strlen, ' ', (size_t) cctk_strlen1 - c_strlen);

  free (fstring);
}
