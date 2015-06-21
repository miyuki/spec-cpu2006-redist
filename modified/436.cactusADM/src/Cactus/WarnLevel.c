#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/*@@
   @file      WarnLevel.c
   @date      Wed Feb 17 00:30:09 1999
   @author    Tom Goodale
   @desc
              Routines to deal with warning levels.
   @enddesc
   @version   $Id: WarnLevel.c,v 1.56 2001/12/10 00:46:25 tradke Exp $
 @@*/

#include "cctk_Config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "StoreKeyedData.h"

#include "cctk_Comm.h"
#include "cctk_Parameter.h"
#include "cctk_FortranString.h"
#include "cctk_WarnLevel.h"
#include "cctki_WarnLevel.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/WarnLevel.c,v 1.56 2001/12/10 00:46:25 tradke Exp $";
CCTK_FILEVERSION(main_WarnLevel_c)


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/
/* prototypes for external C routines are declared in headers
   cctk_WarnLevel.h and cctki_WarnLevel.h
   here only follow the fortran wrapper prototypes */
void CCTK_FCALL cctk_info_
                           (TWO_FORTSTRING_ARG);
void CCTK_FCALL cctk_warn_
                           (const int *level,
                            const int *line,
                            THREE_FORTSTRING_ARG);
void CCTK_FCALL cctk_paramwarn_
                           (TWO_FORTSTRING_ARG);
int CCTK_FCALL cctk_messageformat_
                          (ONE_FORTSTRING_ARG);
void CCTK_FCALL cctki_expecterror_
                           (const int *in,
                            const int *err,
                            const int *warnonerr,
                            const int *line,
                            THREE_FORTSTRING_ARG);
void CCTK_FCALL cctki_expectok_
                           (const int *in,
                            const int *ok,
                            const int *warnonerr,
                            const int *line,
                            THREE_FORTSTRING_ARG);
void CCTK_FCALL cctki_notyetimplemented_
                           (ONE_FORTSTRING_ARG);


/********************************************************************
 *********************    Static Data   *****************************
 ********************************************************************/

/* Store the parameter checking level */
static int parameter_level = CCTK_PARAMETER_NORMAL;

/* Store the number of parameter errors */
static int param_errors = 0;

/* Store the warning level - warnings of this severity or worse will
 * be reported
 */
static int warning_level = 1;

/* Store the error level - warnings of this severity or worse will stop
 * the code.
 */
static int error_level = 0;

/* Store a list of format strings */
static int n_formats = 0;
static pKeyedData *formatlist = NULL;


/*@@
   @routine    CCTK_Info
   @date       Tue Mar 30 1999
   @author     Gabrielle Allen
   @desc
               Print an information message to stdout
   @enddesc

   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        message
   @vdesc      the warning message to output
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_VInfo
   @endreturndesc
@@*/
int CCTK_Info (const char *thorn, const char *message)
{
  return (CCTK_VInfo (thorn, "%s", message));
}

void CCTK_FCALL cctk_info_
                           (TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (thorn, message)
  CCTK_Info (thorn, message);
  free (thorn);
  free (message);
}


/*@@
   @routine    CCTK_VInfo
   @date       Mon Apr 10
   @author     Thomas Radke
   @desc
               Info output routine with variable argument list
   @enddesc

   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        format
   @vdesc      format string for message
   @vtype      const char *
   @vio        in
   @endvar
   @var        ...
   @vdesc      variable argument list for format string
   @vtype      multiple arguments
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0  - success
   @endreturndesc
@@*/
int CCTK_VInfo (const char *thorn, const char *format, ...)
{
  va_list ap;


  va_start (ap, format);

  fprintf (stdout, "INFO (%s): ", thorn);
  vfprintf (stdout, format, ap);
  fprintf (stdout, "\n");
  fflush (stdout);

  va_end (ap);

  return (0);
}


/*@@
   @routine    CCTK_Warn
   @date       Wed Feb 17 00:45:07 1999
   @author     Tom Goodale
   @desc
               Warn the user of something if the warning level is suitable.
   @enddesc
   @calls      CCTK_VWarn

   @var        level
   @vdesc      The warning level
   @vtype      int
   @vio        in
   @endvar
   @var        line
   @vdesc      Line number of warning in originating file
   @vtype      int
   @vio        in
   @endvar
   @var        file
   @vdesc      Name of originating file
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        message
   @vdesc      warning message to output to stderr
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               return code of @seeroutine CCTK_VWarn
   @endreturndesc
@@*/
int CCTK_Warn (int level,
               int line,
               const char *file,
               const char *thorn,
               const char *message)
{
  return (CCTK_VWarn (level, line, file, thorn, "%s", message));
}

void CCTK_FCALL cctk_warn_
                           (const int *level,
                            const int *line,
                            THREE_FORTSTRING_ARG)
{
  THREE_FORTSTRING_CREATE (file, thorn, message)
  CCTK_Warn (*level, *line, file, thorn, message);
  free (thorn);
  free (message);
  free (file);
}


/*@@
   @routine    CCTK_VWarn
   @date       Sun Nov 14 00:23:29 1999
   @author     Tom Goodale
   @desc
               Warning routine with variable argument list

               If the given warning level is less or equal to the current one,
               it will print the given warning message to stderr.
               On processors other than 0 it will also print it to stdout.
   @enddesc
   @calls      CCTK_ParameterGet

   @var        level
   @vdesc      The warning level
   @vtype      int
   @vio        in
   @endvar
   @var        line
   @vdesc      Line number of warning in originating file
   @vtype      int
   @vio        in
   @endvar
   @var        file
   @vdesc      Name of originating file
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        format
   @vdesc      Format string for following arguments
   @vtype      const char *
   @vio        in
   @endvar
   @var        ...
   @vdesc      variable argument list for format string
   @vtype      multiple arguments
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0  - success
   @endreturndesc
@@*/
int CCTK_VWarn (int level,
                int line,
                const char *file,
                const char *thorn,
                const char *format,
                ...)
{
  const CCTK_INT *cctk_full_warnings;
  int param_type;
  int myproc;
  va_list ap;

  if (level <= warning_level)
  {
    myproc = CCTK_MyProc(NULL);

    cctk_full_warnings = (const CCTK_INT *)
                         CCTK_ParameterGet ("cctk_full_warnings", "Cactus",
                                            &param_type);
    if ((level <= error_level) || (cctk_full_warnings && *cctk_full_warnings))
    {
      fprintf (stderr, "WARNING level %d in thorn %s processor %d\n"
                       "  (line %d of %s): \n"
                       "  -> ",
               level, thorn, myproc, line, file);
      if (myproc)
      {
        fprintf (stdout, "WARNING level %d in thorn %s processor %d\n"
                         "  (line %d of %s): \n"
                         "  -> ",
                 level, thorn, myproc, line, file);
      }
    }
    else
    {
      fprintf (stderr, "WARNING[L%d,P%d] (%s): ", level, myproc, thorn);
      if (myproc)
      {
        fprintf (stdout, "WARNING[L%d,P%d] (%s): ", level, myproc, thorn);
      }
    }

    va_start (ap, format);
    vfprintf (stderr, format, ap);
    fprintf (stderr, "\n");
    fflush (stderr);
    if (myproc)
    {
      vfprintf (stdout, format, ap);
      fprintf (stdout, "\n");
      fflush (stdout);
    }
    va_end (ap);
  }

  if (level <= error_level)
  {
    CCTK_Abort (NULL, 0);
  }

  return (0);
}


 /*@@
   @routine    CCTK_ParameterLevel
   @date       Wed Feb 21 2001
   @author     Gabrielle Allen
   @desc
               Returns the parameter checking level
   @enddesc

   @returntype int
   @returndesc
               parameter checking level now being used
   @endreturndesc
@@*/
int CCTK_ParameterLevel (void)
{
  return (parameter_level);
}


/*@@
   @routine    CCTK_ParamWarn
   @date       Wed Feb 17 00:45:07 1999
   @author     Tom Goodale
   @desc
               Warn the user if a parameter error is found
   @enddesc
   @calls      CCTK_ParameterGet

   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        message
   @vdesc      Warning message to output
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
               0  - success
   @endreturndesc
@@*/
int CCTK_ParamWarn (const char *thorn, const char *message)
{
  const CCTK_INT *cctk_strong_param_check;
  int param_type;


  cctk_strong_param_check = (const CCTK_INT *)
                              CCTK_ParameterGet ("cctk_strong_param_check",
                                                 "Cactus", &param_type);
  fprintf (stderr, "PARAM %s (%s): %s\n",
           *cctk_strong_param_check ? "ERROR" : "WARNING", thorn, message);
  fflush (stderr);
  param_errors++;

  return (0);
}

void CCTK_FCALL cctk_paramwarn_
                           (TWO_FORTSTRING_ARG)
{
  TWO_FORTSTRING_CREATE (thorn, message)
  CCTK_ParamWarn (thorn, message);
  free (thorn);
  free (message);
}


/*@@
   @routine    CCTK_MessageFormat
   @date       Mon Jul 26 19:51:26 1999
   @author     Tom Goodale
   @desc
               Stores a format for messages from Fortran.
   @enddesc
   @calls      StoreKeyedData

   @var        format
   @vdesc      Format string
   @vtype      Fortran string macro
   @vio        in
   @endvar

   @returntype int
   @returndesc
               number of previous formats
   @endreturndesc
@@*/
int CCTK_FCALL cctk_messageformat_
                          (ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (format)

  StoreKeyedData (&formatlist, n_formats , format);

  return (n_formats++);
}


/********************************************************************
 *********************     CCTKi Routines   *************************
 ********************************************************************/

 /*@@
   @routine    CCTKi_SetParameterLevel
   @date       Wed Feb 21 2001
   @author     Gabrielle Allen
   @desc
               Sets the parameter checking level
   @enddesc
   @calls      CCTK_VWarn

   @var        level
   @vdesc      level to set
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
               parameter checking level now being used
   @endreturndesc
@@*/
int CCTKi_SetParameterLevel (int level)
{
  if (level == CCTK_PARAMETER_STRICT ||
      level == CCTK_PARAMETER_NORMAL ||
      level == CCTK_PARAMETER_RELAXED)
  {
    parameter_level = level;
  }
  else
  {
    CCTK_VWarn (1, __LINE__, __FILE__, "Cactus", "CCTKi_SetParameterLevel: "
                "Parameter checking level %d not recognised, level unchanged",
                level);
  }

  return (parameter_level);
}


 /*@@
   @routine    CCTKi_SetWarnLevel
   @date       Wed Feb 17 00:42:16 1999
   @author     Tom Goodale
   @desc
               Sets the warning level
   @enddesc
   @calls      CCTK_VWarn
               CCTK_VInfo

   @var        level
   @vdesc      warning level to set
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 - increased warning level <BR>
                0 - warning level unchanged <BR>
               -1 - decreased warning level
   @endreturndesc
@@*/
int CCTKi_SetWarnLevel (int level)
{
  int retval;


  if (warning_level != level)
  {
    retval = level > warning_level ? +1 : -1;
    CCTK_VInfo ("Cactus", "%s warning level from %d to %d",
                retval > 0 ? "Increasing" : "Decreasing", warning_level, level);
    warning_level = level;
  }
  else
  {
    CCTK_VInfo ("Cactus",
                "Warning level is already %d", level);
    retval = 0;
  }

  if (warning_level < error_level)
  {
    error_level = warning_level;
    CCTK_VWarn (2, __LINE__, __FILE__, "Cactus",
                "Decreasing error level to new warning level", warning_level);
  }

  return (retval);
}


/*@@
   @routine    CCTKi_SetErrorLevel
   @date       Wed Feb 17 00:48:02 1999
   @author     Tom Goodale
   @desc
               Sets the error level
   @enddesc
   @calls      CCTK_VWarn               
               CCTK_VInfo

   @var        level
   @vdesc      error level to set
   @vtype      int
   @vio        in
   @endvar

   @returntype int
   @returndesc
                1 - increased error level <BR>
                0 - error level unchanged <BR>
               -1 - decreased error level
   @endreturndesc
@@*/
int CCTKi_SetErrorLevel (int level)
{
  int retval;

  if (level <= warning_level)
  {
    if (error_level != level)
    {
      retval = level > error_level ? +1 : -1;
      CCTK_VInfo ("Cactus", "%s error level from %d to %d",
                  retval > 0 ? "Increasing" : "Decreasing", error_level, level);
      error_level = level;
    }
    else
    {
      CCTK_VWarn (3, __LINE__, __FILE__, "Cactus",
                  "Error level is already %d", level);
      retval = 0;
    }
  }
  else
  {
    retval = level > error_level ? +1 : -1;
    error_level = level;
    CCTK_VInfo ("Cactus",
                "Increasing warning level from %d to match error level %d",
                warning_level,error_level);
    warning_level = level;
    retval = 0;
  }

  return (retval);
}


/*@@
   @routine    CCTKi_ExpectError
   @date       Thanksgiving 99
   @author     Gerd Lanfermann
   @desc
               Used by CCTKi_EXPCTERR macro (src/include/cctk.h)
               allows testing for error return value, will return a
               warning statement if error is found.
   @enddesc
   @calls      CCTK_Warn

   @var        in
   @vdesc      real return value to check against
   @vtype      int
   @vio        in
   @endvar
   @var        err
   @vdesc      expected return value for error condition
   @vtype      int
   @vio        in
   @endvar
   @var        warnonerr
   @vdesc      the warning level to use for output
   @vtype      int
   @vio        in
   @endvar
   @var        line
   @vdesc      Line number of warning in originating file
   @vtype      int
   @vio        in
   @endvar
   @var        file
   @vdesc      Name of originating file
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        message
   @vdesc      Warning message to print
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_ExpectError (int in,
                        int err,
                        int warnonerr,
                        int line,
                        const char *file,
                        const char *thorn,
                        const char *message)
{
  if (in == err)
  {
    CCTK_Warn (warnonerr, line, file, thorn, message);
  }
}

void CCTK_FCALL cctki_expecterror_
                           (const int *in,
                            const int *err,
                            const int *warnonerr,
                            const int *line,
                            THREE_FORTSTRING_ARG)
{
  THREE_FORTSTRING_CREATE (file, thorn, message)
  CCTKi_ExpectError (*in, *err, *warnonerr, *line, file, thorn, message);
  free (file);
  free (thorn);
  free (message);
}


/*@@
   @routine    CCTKi_ExpectOK
   @date       Thanksgiving 99
   @author     Gerd Lanfermann
   @desc
               Used by CCTKi_EXPCTOK macro (src/include/cctk.h)
               allows testing for success return value, will return a
               warning statement otherwise
   @enddesc
   @calls      CCTK_Warn

   @var        in
   @vdesc      real return value to check against
   @vtype      int
   @vio        in
   @endvar
   @var        ok
   @vdesc      expected return value for success
   @vtype      int
   @vio        in
   @endvar
   @var        warnonerr
   @vdesc      the warning level to use for output
   @vtype      int
   @vio        in
   @endvar
   @var        line
   @vdesc      Line number of warning in originating file
   @vtype      int
   @vio        in
   @endvar
   @var        file
   @vdesc      Name of originating file
   @vtype      const char *
   @vio        in
   @endvar
   @var        thorn
   @vdesc      Name of originating thorn
   @vtype      const char *
   @vio        in
   @endvar
   @var        message
   @vdesc      Warning message to print
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_ExpectOK (int in,
                     int ok,
                     int warnonerr,
                     int line,
                     const char *file,
                     const char *thorn,
                     const char *message)
{
  if (in != ok)
  {
    CCTK_Warn (warnonerr, line, file, thorn, message);
  }
}

void CCTK_FCALL cctki_expectok_
                           (const int *in,
                            const int *ok,
                            const int *warnonerr,
                            const int *line,
                            THREE_FORTSTRING_ARG)
{
  THREE_FORTSTRING_CREATE (file, thorn, message)
  CCTKi_ExpectOK (*in, *ok, *warnonerr, *line, file, thorn, message);
  free (file);
  free (thorn);
  free (message);
}


/*@@
   @routine    CCTKi_FinaliseParamWarn
   @date       June 1999
   @author     Gabrielle Allen
   @desc
               Die if errors were encountered during param check
   @enddesc
   @calls      CCTK_ParameterGet
@@*/
void CCTKi_FinaliseParamWarn (void)
{
  int param_type;
  const CCTK_INT *cctk_strong_param_check;


  if (param_errors)
  {
    cctk_strong_param_check = (const CCTK_INT *)
                                CCTK_ParameterGet ("cctk_strong_param_check",
                                                   "Cactus", &param_type);
    if (*cctk_strong_param_check)
    {
      fprintf (stderr, "\nFailed parameter check (%d errors)\n\n", param_errors);
      fflush (stderr);
      exit (99);
    }
    else
    {
      if (param_errors==1)
      {
        fprintf (stderr, "\nThere was 1 parameter warning\n\n");
      }
      else
      {
        fprintf (stderr, "\nThere were %d parameter warnings\n\n", param_errors);
      }
      fflush (stderr);
    }
  }
}


/*@@
   @routine    CCTKi_NotYetImplemented
   @date       July 1999
   @author     Gabrielle Allen
   @desc
               Report on features not yet added to code
   @enddesc

   @var        message
   @vdesc      message to be printed
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void CCTKi_NotYetImplemented (const char *message)
{
#if 0
  CCTK_VWarn (0, __LINE__, __FILE__, "Cactus",
              "%s: this planned feature is not yet implemented in the code.\n"
              "If you need this feature please contact the "
              "Cactus maintainers.", message);
#endif
  CCTK_VWarn (0, __LINE__, __FILE__, "Cactus", "%s: feature not implemented",
              message);
}

void CCTK_FCALL cctki_notyetimplemented_
                           (ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (message)
  CCTKi_NotYetImplemented (message);
  free (message);
}


#if 0
 /*@@
   @routine    CCTK_VINFO
   @date       Wed Oct  4 21:03:08 2000
   @author     Tom Goodale
   @desc
   Fortran version of CCTK_VInfo
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     format_number
   @vdesc   format string handle
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     ...
   @vdesc   arguments for format string
   @vtype   multiple arguments
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   FIXME - UNFINISHED
   @endreturndesc
@@*/
void CCTK_FCALL cctk_vinfo_
                           (int format_number,
                            ...)
{
  char *format_string;
  char *message;
  int message_length;
  int current_place;

  if (format_number < n_formats)
  {
    format_string = (char *)GetKeyedData (formatlist, format_number);

    /* Pick an arbitrary starting length for the message */
    message_length=5*strlen (format_string);

    message = (char *)malloc (message_length);

    /* Loop through the format string */
    for (current_place=0; format_string; format_string++)
    {
      if (*format_string != '%')
      {
        message[current_place] = *format_string;
        current_place++;
        if (current_place >= message_length)
        {
          message = (char *)realloc (message, message_length*2);
          message_length *=2;
        }
      }
      else
      {
        /* FIXME */
      }
    }

  }

}
#endif
