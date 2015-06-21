#ifdef SPEC_CPU
# define THORN_IS_IOUtil
#endif /* SPEC_CPU */
/*@@
   @file      CheckpointRecovery.c
   @date      Jun 04 1999
   @author    Thomas Radke
   @desc
              Utility routines for checkpointing/recovery and the filereader
              The actual work is done by the IO thorns.
   @enddesc
   @version   $Id: CheckpointRecovery.c,v 1.31 2002/01/15 18:21:06 tradke Exp $
 @@*/

#include "cctk.h"
#include "cctk_Parameters.h"
#include "StoreHandledData.h"

#include "ioGH.h"
#include "ioutil_CheckpointRecovery.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

static const char *rcsid = "$Header: /cactus/CactusBase/IOUtil/src/CheckpointRecovery.c,v 1.31 2002/01/15 18:21:06 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOUtil_CheckpointRecovery_c)


/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/
void IOUtil_RecoverGH (cGH *GH);
void IOUtil_RecoverIDFromDatafiles (cGH *GH);


/********************************************************************
 ********************    Static Variables   *************************
 ********************************************************************/
/* Local data holding info on Recover Functions */
static cHandledData *RecoverFunctions = NULL;
static int num_functions = 0;
static int checkpoint_file_exists = 0;


/********************************************************************
 ********************    Internal Typedefs   ************************
 ********************************************************************/
typedef struct
{
  char *basename;
  int iteration;
} filelist_t;


/********************************************************************
 ********************    Internal Routines   ************************
 ********************************************************************/
static void SetInputFlag (int vindex, const char *optstring, void *arg);
#ifdef HAVE_DIRENT_H
static int CompareFiles (const void *a, const void *b);
#endif


/************************************************************************
 *
 *  Registration functions for Restoring from IO Files
 *
 ************************************************************************/

 /*@@
   @routine    IOUtil_RegisterRecover
   @date       Monday 21 June 1999
   @author     Gabrielle Allen
   @desc
               Registers a new recovery method
   @enddesc
   @calls      Util_GetHandle
               Util_NewHandle

   @var        name
   @vdesc      The name of the function for recovery
   @vtype      const char *
   @vio        in
   @vcomment
   @endvar

   @returntype int
   @returndesc
               -1  method with this name already registered, or
               return code of @seeroutine Util_NewHandle
   @endreturndesc
@@*/
int IOUtil_RegisterRecover (const char *name,
                            int (*func) (cGH *, const char *, int))
{
  int handle;


  /* Check that the method hasn't already been registered */
  handle = Util_GetHandle (RecoverFunctions, name, NULL);
  if (handle < 0)
  {
    /* New function - get a handle for it. */
    handle = Util_NewHandle (&RecoverFunctions, name, func);

    /* Remember how many methods there are */
    num_functions++;
  }
  else
  {
    handle = -1;
  }

  return (handle);
}


 /*@@
   @routine    IOUtil_PrepareFilename
   @date       Fri Aug 21 14:54:38 1998
   @author     Gerd Lanfermann
   @desc
               This routine prepares the filenames for the checkpoint/recovery
               and filereader files, paying attention to the different types:

                 * it returns the full filename (directory+filename)

                 * it prepends "Ln_" level indicators to the filename
                   and "low_"/"med_" for convergence levels > 1

                 * for cp files it prepends the iteration number as "it_%d"

                 * for chunked files it prepends the file number as "file_%d"
   @enddesc
   @calls
   @history
   @hdate      Nov 4 1998 @hauthor Gabrielle Allen
   @hdesc      A file_* in the name indicates it needs recombining
   @hdate      Apr 14 1999 @hauthor Thomas Radke
   @hdesc      Removed code for expanding "basedir" and "nameofparfile"
   @hdate      May 06 1999 @hauthor Thomas Radke
   @hdesc      Added parameter unchunked to be independent of
               current chunking mode for recovery
   @endhistory

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        basefilename
   @vdesc      basefilename of the file(s) to recover from
   @vtype      const char *
   @vio        in
   @endvar
   @var        fname
   @vdesc      the resulting filename
   @vtype      char *
   @vio        out
   @endvar
   @var        called_from
   @vdesc      indicates the caller function:
                 * either Filereader (FILEREADER_DATA)
                 * or IOUtil_RecoverGH() (CP_RECOVER_DATA)
   @vtype      int
   @vio        in
   @endvar
   @var        file_ioproc
   @vdesc      the IO processor number (for chunked files)
   @vtype      int
   @vio        in
   @endvar
   @var        file_unchunked
   @vdesc      flag to indicate whether file mode is unchunked or not
   @vtype      int
   @vio        in
   @endvar
@@*/
void IOUtil_PrepareFilename (const cGH *GH,
                             const char *basefilename,
                             char *fname,
                             int called_from,
                             int file_ioproc,
                             int file_unchunked)
{
  DECLARE_CCTK_PARAMETERS


  /* get the right parameters */
  switch (called_from)
  {
    case CP_INITIAL_DATA:
      sprintf (fname, "%s/%s", checkpoint_dir, checkpoint_ID_file);
      break;

    case CP_EVOLUTION_DATA:
      sprintf (fname, "%s/%s", checkpoint_dir, checkpoint_file);
      break;

    case CP_RECOVER_DATA:
    case CP_RECOVER_PARAMETERS:
    case FILEREADER_DATA:
      sprintf (fname, "%s/%s", recovery_dir, basefilename ? basefilename:recover_file);
      break;

    default:
      CCTK_VWarn (0, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOUtil_PrepareFilename: Unknown calling mode %d",
                  called_from);
      break;
  }

  /* add refinement factor and convergence level (med/low) inbetween: */
  /* FIXME Gab ... asymmetric levfac */
  if (GH)
  {
    if (GH->cctk_levfac[0] > 1)
    {
      sprintf (fname, "%sL%d_", fname, GH->cctk_levfac[0]);
    }
    if (GH->cctk_convlevel > 0)
    {
      strcat (fname, GH->cctk_convlevel == 1 ? "med_" : "low_");
    }
  }

  /* If checkpoint filename, merge in the iteration number
     and for chunked files also the file number */
  if (called_from == CP_INITIAL_DATA || called_from == CP_EVOLUTION_DATA)
  {
    sprintf (fname, "%s.it_%d", fname, (int) GH->cctk_iteration);
  }

  /* If not one unchunked file give a file number */
  if (! file_unchunked)
  {
    sprintf (fname, "%s.file_%d", fname, file_ioproc);
  }
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    IOUtil_RecoverFromFile
   @date       Jun 14 1999
   @author     Thomas Radke
   @desc
               Recover from a given file.
               This routine loops through all XXX_RecoverGH routines
               registered by IO thorns.
   @enddesc
   @calls      Util_GetHandledData
               <registered RecoverGH routines>

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        basefilename
   @vdesc      basefilename of the file(s) to recover from
   @vtype      const char *
   @vio        in
   @endvar
   @var        called_from
   @vdesc      indicates the caller function:
                 * either Filereader (FILEREADER_DATA)
                 * or IOUtil_RecoverGH() (CP_RECOVER_DATA)
   @vtype      int
   @vio        in
   @endvar
@@*/
static int IOUtil_RecoverFromFile (cGH *GH,
                                   const char *basefilename,
                                   int called_from)
{
  int handle;
  int retval;
  int (*recoverFn) (cGH *, const char *, int);


  retval = -1;

  for (handle = 0; handle < num_functions; handle++)
  {

    recoverFn = (int (*) (cGH *, const char *, int))
                Util_GetHandledData (RecoverFunctions, handle);
    if (recoverFn)
    {
      retval = recoverFn (GH, basefilename, called_from);
      if (retval >= 0)
      {
        break;
      }
    }
  }

  if (num_functions <= 0)
  {
    CCTK_WARN (1, "IOUtil_RecoverFromFile: No recovery routines "
                  "were registered");
  }

  return (retval);
}


 /*@@
   @routine    IOUtil_RecoverGH
   @date       Jun 14 1999
   @author     Thomas Radke
   @desc
               The rfr-registered recovery routine.
               Just calls IOUtil_RecoverFromFile()
               with called_from == CP_RECOVER_DATA.
   @enddesc
   @calls      IOUtil_RecoverFromFile

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
@@*/
void IOUtil_RecoverGH (cGH *GH)
{
  ioGH *myGH;


  myGH = (ioGH *) CCTK_GHExtension (GH, "IO");

  if (checkpoint_file_exists)
  {
    myGH->recovered = IOUtil_RecoverFromFile (GH, NULL, CP_RECOVER_DATA) >= 0;

    /* stop if recovery failed */
    if (! myGH->recovered)
    {
      CCTK_WARN (0, "Failed to restart from recovery !");
    }
  }
  else
  {
    myGH->recovered = 0;
  }
}


 /*@@
   @routine    IOUtil_RecoverVarsFromDatafiles
   @date       Wed Apr 19 2000
   @author     Thomas Radke
   @desc
               IOUtil's function interface to recover variables from data files.
               Any thorn can call this routine with a list of datafile names
               and a list of variables to read in from these files. <BR>

               It just calls IOUtil_RecoverFromFile() with
               called_from == FILEREADER_DATA for each data file
               from the given file list.
   @enddesc

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
   @var        in_files
   @vdesc      list of filenames to process (separated by spaces)
   @vtype      const char *
   @vio        in
   @endvar
   @var        in_vars
   @vdesc      list of variable fullnames to read in from the files
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void IOUtil_RecoverVarsFromDatafiles (cGH *GH,
                                      const char *in_files,
                                      const char *in_vars)
{
  DECLARE_CCTK_PARAMETERS
  ioGH *myGH;
  char *basefilename, *delim, delim_char;


  myGH = (ioGH *) CCTK_GHExtension (GH, "IO");

  if (CCTK_NumVars () > 0)
  {
    myGH->do_inVars = (char *) calloc (CCTK_NumVars (), sizeof (char));
    CCTK_TraverseString (in_vars, SetInputFlag, myGH->do_inVars,
                         CCTK_GROUP_OR_VAR);
  }
  else
  {
    myGH->do_inVars = NULL;
  }

  /* duplicate the filename list and parse it */
  basefilename = strdup (in_files);
  while (basefilename)
  {
    /* skip leading spaces */
    while (isspace ((int) *basefilename))
    {
      basefilename++;
    }
    if (! *basefilename)
    {
      break;
    }

    /* find delimiter for current filename and cut there */
    for (delim = basefilename + 1; ! isspace ((int) *delim) && *delim; delim++);
    delim_char = *delim;
    *delim = 0;

    if (verbose)
    {
      CCTK_VInfo (CCTK_THORNSTRING, "Reading variables from data file '%s'",
                  basefilename);
    }

    if (IOUtil_RecoverFromFile (GH, basefilename, FILEREADER_DATA) < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Failed to read variables from data file '%s'", basefilename);
    }

    *delim = delim_char;
    basefilename = delim;
  }

  if (basefilename)
  {
    free (basefilename - strlen (in_files));
  }

  if (myGH->do_inVars)
  {
    free (myGH->do_inVars);
    myGH->do_inVars = NULL;
  }
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    IOUtil_RecoverIDFromDatafiles
   @date       Wed Apr 19 2000
   @author     Thomas Radke
   @desc
               The rfr-registered initial data recovery routine.
               Just calls IOUtil's generic routine
               IOUtil_RecoverVarsFromDatafiles() with the recover_ID_XXX
               parameters.
   @enddesc
   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar
@@*/
void IOUtil_RecoverIDFromDatafiles (cGH *GH)
{
  DECLARE_CCTK_PARAMETERS


  IOUtil_RecoverVarsFromDatafiles (GH, recover_ID_files, recover_ID_vars);
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    IOUtil_RecoverParameters
   @date       Apr 22 2000
   @author     Thomas Radke
   @desc
               The generic parameter recovery routine.
               It is called by the IO thorns' parameter recovery routines
               scheduled at CCTK_RECOVER_PARAMETERS, and simply calls
               the given callback routine with its arguments
               plus a checkpoint filename.
   @enddesc

   @var        recoverFn
   @vdesc      callback function for recovery of parameters
               from a given checkpoint file
   @vtype      int (*) (cGH *, const char *, int)
   @vio        in
   @endvar
   @var        fileExtension
   @vdesc      extension of valid checkpoint files for given callback
   @vtype      const char *
   @vio        in
   @endvar
   @var        fileType
   @vdesc      string to describe the type of checkpoint file
               (used for warning/info messages)
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 if in "autoprobe" mode and no cp files were found, or<BR>
               +1 if parameter recovery was successful for some cp file,<BR>
               -1 if in "auto" mode and no checkpoint files were found,
                  or if parameter recovery failed for some cp file,<BR>
               -2 if in "auto*" mode and recovery dir doesn't exist
   @endreturndesc
@@*/
int IOUtil_RecoverParameters (int (*recoverFn) (cGH *GH,
                                                const char *basefilename,
                                                int called_from),
                              const char *fileExtension,
                              const char *fileType)
{
  int retval;
#ifdef HAVE_DIRENT_H
  int len, extension_len, recover_file_len;
  unsigned int num_files;
  const char *p;
  DIR *dir;
  struct dirent *file;
  filelist_t *filelist, *tmp;
#endif
  DECLARE_CCTK_PARAMETERS


  if (CCTK_Equals (recover, "auto") || CCTK_Equals (recover, "autoprobe"))
  {
#ifdef HAVE_DIRENT_H
    if (verbose)
    {
      CCTK_VInfo (CCTK_THORNSTRING, "Searching for %s checkpoint files "
                  "with basefilename '%s' in directory '%s'",
                  fileType, recover_file, recovery_dir);
    }

    dir = opendir (recovery_dir);
    if (! dir)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Recovery directory '%s' doesn't exist", recovery_dir);
      return (-2);
    }

    /* get the list of potential recovery files */
    extension_len = strlen (fileExtension);
    recover_file_len = strlen (recover_file);
    num_files = 0;
    filelist = NULL;

    while ((file = readdir (dir)) != NULL)
    {
      /* first check the file prefix */
      if (strncmp (file->d_name, recover_file, recover_file_len) ||
          strncmp (file->d_name + recover_file_len, ".it_", 4))
      {
        continue;
      }

      /* now check if there is an iteration number following the file prefix */
      for (p = file->d_name + recover_file_len + 4; *p && *p != '.'; p++)
      {
        if (! isdigit ((int) *p))
        {
          break;
        }
      }
      if (*p != '.')
      {
        continue;
      }

      /* check for a '.file_<processor>' suffix for chunked output
         We only select the chunked output file of processor 0 in that case. */
      if (! strncmp (p, ".file_", 6) && strncmp (p, ".file_0", 7))
      {
        continue;
      }

      /* finally check the file type suffix */
      len = strlen (file->d_name);
      if (len < extension_len ||
          strcmp (file->d_name + len - extension_len, fileExtension))
      {
        continue;
      }

      /* found a recovery file by that basename */
      if (num_files == 0)
      {
        tmp = (filelist_t *) malloc (sizeof (filelist_t));
      }
      else
      {
        tmp = (filelist_t *) realloc (filelist,
                                      (num_files+1) * sizeof (filelist_t));
      }
      if (tmp == NULL)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Failed to allocate memory for file list");
        continue;
      }
      filelist = tmp;
      filelist[num_files].basename = strdup (file->d_name);
      /* cut the filename after the iteration number field */
      len = p - file->d_name;
      filelist[num_files].basename[len] = 0;
      filelist[num_files].iteration = atoi (file->d_name + recover_file_len+4);

      num_files++;
    }
    closedir (dir);

    retval = CCTK_Equals (recover, "auto") ? -1 : 0;
    if (num_files)
    {
      /* sort the list according to their iteration numbers */
      qsort (filelist, num_files, sizeof (filelist_t), CompareFiles);

      /* loop over all recovery files found and call the callback routine;
         skip all following files after the first successful recovery (when
         recoverFn() returned a positive value) */
      while (num_files--)
      {
        if (retval <= 0)
        {
          retval = recoverFn (NULL, filelist[num_files].basename,
                              CP_RECOVER_PARAMETERS);
        }
        free (filelist[num_files].basename);
      }
      free (filelist);
    }
    else
    {
      CCTK_VWarn (retval ? 1 : 3, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "No %s checkpoint files with basefilename '%s' found in "
                  "recovery directory '%s'",
                  fileType, recover_file, recovery_dir);
    }
#else
    fileExtension = fileExtension;

    /* No opendir(3) ??? It's probably a Windows box, so just give up ! */
    CCTK_WARN (0, "You cannot use 'IO::recover = \"auto*\"' on "
               "this architecture because it doesn't provide opendir(3) to "
               "automatically look for checkpoint files.\n"
               "Please use 'IO::recover = \"manual\"' instead !");
    retval = -1;
#endif
  }
  else
  {
    /* just call the recovery routine */
    retval = (*recoverFn) (NULL, recover_file, CP_RECOVER_PARAMETERS);
  }

  if (retval < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Could not recover parameters from %s checkpoint file(s) "
                "with basefilename '%s' in recovery directory '%s'",
                fileType, recover_file, recovery_dir);
  }

  /* remember parameter recovery status for later evaluation in
     IOUtil_RecoverGH() */
  checkpoint_file_exists = retval > 0;

  USE_CCTK_PARAMETERS;   return (retval);
}


 /*@@
   @routine    IOUtil_GetAllParameters
   @date       Mon Apr 10 2000
   @author     Thomas Radke
   @desc
               Collect all parameters of active implementations
               into a single string which can then be dumped as an attribute.
   @enddesc

   @var        GH
   @vdesc      Pointer to CCTK grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar

   @returntype char *
   @returndesc the allocated string or NULL
   @endreturndesc
@@*/
char *IOUtil_GetAllParameters (const cGH *GH)
{
  int i, first;
  char *param;
  const char *thorn;
  const cParamData *pdata;
#define BUFFER_INCREMENT  1024
  int currentLen = 0, maxLen = 0;
  char *parameters = NULL;


  /* avoid compiler warning about unused parameter */
  GH = GH;

  /* loop over all thorns */
  for (i = CCTK_NumCompiledThorns () - 1; i >= 0; i--)
  {
    thorn = CCTK_CompiledThorn (i);

    /* skip all inactive thorns */
    if (! CCTK_IsThornActive (thorn))
    {
      continue;
    }

    /* now walk through all parameters of given thorn */
    first = 1;
    while (CCTK_ParameterWalk (first, thorn, &param, &pdata) == 0)
    {
      char *value;

      first = 0;

      /* skip the parameters which weren't explicitely set */
      if (pdata->n_set > 0)
      {
        value = CCTK_ParameterValString (pdata->name, pdata->thorn);
        if (value == NULL)
        {
          CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                      "Couldn't get value for parameter '%s'", param);
        }
        else
        {
          int addLen = strlen (param) + strlen (value) + 5;

          if (currentLen + addLen >= maxLen)
          {
            char *newBuffer;

            newBuffer = (char*) realloc(parameters, maxLen+=BUFFER_INCREMENT);
            if (! newBuffer)
            {
              CCTK_VWarn (0, __LINE__, __FILE__, CCTK_THORNSTRING,
                          "Failed to allocate buffer of %d bytes", maxLen);
            }

            parameters = newBuffer;
          }
          sprintf (parameters + currentLen, "%s = %s\n", param, value);
          currentLen += addLen - 1;

          free (value);
        }
      }

      free (param);

    } /* end of loop walking over all parameters of given thorn */
  } /* end of looping over all thorns */

  return (parameters);
}


 /*@@
   @routine    IOUtil_SetAllParameters
   @date       Mon Apr 10 2000
   @author     Thomas Radke
   @desc
               Parse the given string for parameters
               and call CCTK_SetParameter() for each.
   @enddesc

   @var        parameters
   @vdesc      the parameter string
   @vtype      const char *
   @vio        in
   @endvar
@@*/
void IOUtil_SetAllParameters (const char *parameters)
{
  DECLARE_CCTK_PARAMETERS
  char *tmp, *nextparam, *avalue, *param;
  char oldchar;
  char *name, *thorn_impl, *parameter_string, *free_me;
  const char *thorn;


  parameter_string = free_me = strdup (parameters);
  while (*parameter_string)
  {
    nextparam = parameter_string;
    while (*nextparam != '\n' && *nextparam)
    {
      nextparam++;
    }
    oldchar = *nextparam;
    *nextparam = 0;

    tmp = parameter_string;
    while (*tmp != ' ')
    {
      tmp++;
    }
    *tmp = 0;

    param = parameter_string;
    avalue = tmp + 3;

    name = thorn_impl = NULL;
    if (Util_SplitString (&thorn_impl, &name, param, "::") == 0)
    {
      /* find out the implementing thorn of the parameter given */
      thorn = CCTK_IsImplementationActive (thorn_impl) ?
              CCTK_ImplementationThorn (thorn_impl) : thorn_impl;

      /* set parameter only if it belongs to an active implementation */
      if (CCTK_IsThornActive (thorn))
      {
        if (CCTK_ParameterSet (name, thorn, avalue) < 0)
          CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                      "Couldn't set parameter '%s' to '%s'", param, avalue);
      }
      else if (verbose)
      {
        CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Ignoring inactive parameter '%s' for recovery", param);
      }

      if (name)
      {
        free (name);
      }
      if (thorn_impl)
      {
        free (thorn_impl);
      }
    }

    *nextparam = oldchar;
    parameter_string = nextparam;
    if (*parameter_string == '\n')
    {
      parameter_string++;
    }
  }

  free (free_me);
  USE_CCTK_PARAMETERS; }


int IOUtil_RestartFromRecovery (const cGH *GH)
{
  const ioGH *myGH;


  myGH = (const ioGH *) CCTK_GHExtension (GH, "IO");

  return (myGH ? myGH->recovered : 0);
}


/***************************** local routines ******************************/

/* callback for CCTK_TraverseString() to set the input flag
   for the given variable */
static void SetInputFlag (int vindex, const char *optstring, void *flags)
{
  ((char *) flags)[vindex] = 1;

  if (optstring)
  {
    CCTK_VWarn (5, __LINE__, __FILE__, CCTK_THORNSTRING,
                "SetInputFlag: Optional string '%s' in variable name ignored",
                optstring);
  }
}


#ifdef HAVE_DIRENT_H
/* callback for qsort() to sort the list of recovery files found */
static int CompareFiles (const void *a, const void *b)
{
  return (((const filelist_t *) a)->iteration -
          ((const filelist_t *) b)->iteration);
}
#endif
