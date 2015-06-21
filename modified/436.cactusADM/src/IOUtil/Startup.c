#ifdef SPEC_CPU
# define THORN_IS_IOUtil
#endif /* SPEC_CPU */
 /*@@
   @file      Startup.c
   @date      Sat Feb 6 1999
   @author    Gabrielle Allen
   @desc
              Startup routines for IOUtil.
   @enddesc
   @version   $Id: Startup.c,v 1.13 2001/12/18 12:04:01 tradke Exp $
 @@*/

#include "cctk.h"
#include "cctk_Misc.h"
#include "cctk_Version.h"
#include "cctk_Parameters.h"
#include "util_Network.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ioGH.h"
#include "ioutil_Utils.h"
#include "ioutil_AdvertisedFiles.h"

/* the rcs ID and its dummy funtion to use it */
static const char *rcsid="$Header: /cactus/CactusBase/IOUtil/src/Startup.c,v 1.13 2001/12/18 12:04:01 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOUtil_Startup_c)


/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/
void IOUtil_Startup (void);
int IOUtil_UpdateParFile (const cGH *GH);


/********************************************************************
 *********************     Internal Typedefs   **********************
 ********************************************************************/
/* structure defining a single-linked list of parameters
   (stores the name of the parameter, its stringified value, and its type) */
typedef struct t_param_list
{
  char *name;
  char *value;
  const char *format_string;
  struct t_param_list *next;
} t_param_list;


/********************************************************************
 **********************     Static Routines     *********************
 ********************************************************************/
static void *SetupGH (tFleshConfig *config, int convergence_level, cGH *GH);
static int CopyParFile (const char *new_parfilename, const char *outdir);
static int GenerateParFile (const char *new_parfilename, const char *outdir);
static int DumpParameters (FILE *outfile);


 /*@@
   @routine    IOUtil_Startup
   @date       Sat Feb 6 1999
   @author     Gabrielle Allen
   @desc
               The startup registration routine for IOUtil.<BR>
               It registers the GH extension "IO" for IOUtil, along with its
               setup routine.
               It also advertises the original parameter file.
   @enddesc
   @calls      CCTK_RegisterGHExtension
               CCTK_RegisterGHExtensionSetupGH
               CCTK_ParameterFilename
               IOUtil_AdvertiseFile
@@*/
void IOUtil_Startup (void)
{
  char parfile[256];
  ioAdvertisedFileDesc advertised_file;
  DECLARE_CCTK_PARAMETERS


  CCTK_RegisterGHExtensionSetupGH (CCTK_RegisterGHExtension ("IO"), SetupGH);

  /* advertise the parameter file */
  parfile[0] = 0;
  CCTK_ParameterFilename (sizeof (parfile), parfile);
  advertised_file.slice = "";
  advertised_file.thorn = CCTK_THORNSTRING;
  advertised_file.varname = "";
  advertised_file.description = "Parameter File";
  advertised_file.mimetype = "text/plain";
  IOUtil_AdvertiseFile (NULL, parfile, &advertised_file);
  USE_CCTK_PARAMETERS; }


 /*@@
   @routine    IOUtil_UpdateParFile
   @date       Tue 18 Dec 2001
   @author     Thomas Radke
   @desc
               Updates the parameter file for all parameters which got steered
               during its last invocation.
   @enddesc
   @calls      

   @var        GH
   @vdesc      pointer to grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar

   @returntype void *
   @returndesc 0 for success
   @endreturndesc
@@*/
int IOUtil_UpdateParFile (const cGH *GH)
{
  DECLARE_CCTK_PARAMETERS


  /* check if it's time to update */
  if (CCTK_MyProc (GH) > 0 || GH->cctk_iteration % parfile_update_every)
  {
    return (0);
  }

  CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
              "Not yet implemented: updating parameter file for steered "
              "parameters up to iteration %d", GH->cctk_iteration);

  USE_CCTK_PARAMETERS;   return (0);
}


/********************************************************************
 ***********************    Local Functions   ***********************
 ********************************************************************/
 /*@@
   @routine    SetupGH
   @date       Tue May 09 2000
   @author     Thomas Radke
   @desc
               The GH allocation and initialization routine for IOUtil.<BR>
               Necessary output dirs are created, checkpoint/recovery timers
               are created if timing information is wanted, and a parameter
               file is written to 'IO::outdir' if requested.
   @enddesc
   @calls      IOUtil_CreateDirectory
               CopyParFile
               GenerateParFile

   @var        config
   @vdesc      flesh configuration structure (unused)
   @vtype      tFleshConfig *
   @vio        in
   @endvar
   @var        convergence_level
   @vdesc      convergence level (unused)
   @vtype      int
   @vio        in
   @endvar
   @var        GH
   @vdesc      pointer to grid hierarchy
   @vtype      cGH *
   @vio        in
   @endvar

   @returntype void *
   @returndesc the pointer to IOUtil's GH extension structure, or<BR>
               NULL if memory allocation failed
   @endreturndesc
@@*/
static void *SetupGH (tFleshConfig *config, int convergence_level, cGH *GH)
{
  int i, maxdim, myproc;
  ioGH *newGH;
  DECLARE_CCTK_PARAMETERS


  /* avoid compiler warnings about unused parameters */
  config = config;
  convergence_level = convergence_level;

  myproc = CCTK_MyProc (GH);
  newGH = (ioGH *) malloc (sizeof (ioGH));
  if (! newGH)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Couldn't allocate GH extension structure for IOUtil");
    return (NULL);
  }

  if (CCTK_Equals (out3D_mode, "proc"))
  {
    newGH->ioproc = myproc;
    newGH->nioprocs = CCTK_nProcs (GH);
    newGH->ioproc_every = 1;
  }
  else if (CCTK_Equals (out3D_mode, "np"))
  {
    if (out3D_procs > CCTK_nProcs (GH))
    {
      newGH->ioproc_every = CCTK_nProcs (GH);
      CCTK_VInfo (CCTK_THORNSTRING, "Reducing 'IO::ioproc_every' to %d",
                  newGH->ioproc_every);
    }
    else
    {
      newGH->ioproc_every = out3D_procs;
    }

    newGH->nioprocs = CCTK_nProcs (GH) / newGH->ioproc_every +
                      (CCTK_nProcs (GH) % newGH->ioproc_every ? 1 : 0);
    newGH->ioproc   = myproc - (myproc % newGH->ioproc_every);
  }
  else         /* IO::out3D_mode = "onefile" */
  {
    newGH->ioproc   = 0;
    newGH->nioprocs = 1;
    newGH->ioproc_every = CCTK_nProcs (GH);
  }

  /* For now we can only have unchunked for a single output file */
  if (out3D_unchunked || CCTK_nProcs (GH) == 1)
  {
    if (newGH->ioproc_every >= CCTK_nProcs (GH))
    {
      newGH->unchunked = 1;
    }
    else
    {
      CCTK_INFO ("Unchunked output not supported for multiple "
                 "output files. Output will be chunked.");
      newGH->unchunked = 0;
    }
  }
  else
  {
    newGH->unchunked = 0;
  }

  /* create the checkpoint directory */
  i = IOUtil_CreateDirectory (GH, checkpoint_dir,
                              ! CCTK_Equals (out3D_mode, "onefile"),
                              newGH->ioproc);
  if (i < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Problem creating checkpoint directory '%s'",
                checkpoint_dir);
  }
  else if (i > 0 && CCTK_Equals (newverbose, "full"))
  {
    CCTK_VInfo (CCTK_THORNSTRING,
                "IOUtil_InitGH: checkpoint directory '%s' already exists",
                checkpoint_dir);
  }

  /* save downsampling parameters in ioUtilGH because they are temporarily
     reset during checkpointing */
  /* for now we have only parameters for the first 3 dimensions
     the rest is constantly initialized to 1 */
  maxdim = CCTK_MaxDim ();
  newGH->downsample = (int *) malloc (maxdim * sizeof (int));

  switch (maxdim > 3 ? 3 : maxdim)
  {
    case 3 : newGH->downsample [2] = out3D_downsample_z;
    case 2 : newGH->downsample [1] = out3D_downsample_y;
    case 1 : newGH->downsample [0] = out3D_downsample_x;
  }

  for (i = 3; i < maxdim; i++)
  {
    newGH->downsample [i] = 1;
  }

  /* evaluate the 'IO::out_single' parameter only
     if Cactus was compiled with double precision */
#ifdef SINGLE_PRECISION
  newGH->out_single = 0;
#else
  newGH->out_single = out3D_single;
#endif

  /* reset the flag incidicating restart from recovery */
  newGH->recovered = 0;

  /* reset the flags array for the file reader */
  newGH->do_inVars = NULL;

  /* write the parameter file if requested */
  if (myproc == 0)
  {
    if (CCTK_Equals (parfile_write, "copy"))
    {
      CopyParFile (parfile_name, outdir);
    }
    else if (CCTK_Equals (parfile_write, "generate"))
    {
      GenerateParFile (parfile_name, outdir);
    }
  }

  USE_CCTK_PARAMETERS;   return (newGH);
}


 /*@@
   @routine    CopyParFile
   @date       Tue 18 Dec 2001
   @author     Thomas Radke
   @desc
               Copies the original parameter file to a new one in 'IO::outdir'.
               Note that the new parameter file is only written if no such
               file existed before.
   @enddesc
   @calls      CCTK_ParameterFilename

   @var        parfile_name
   @vdesc      name of the new parfile (or empty string if the original
               parfile name should be used)
   @vtype      const char *
   @vio        in
   @endvar
   @var        outdir
   @vdesc      directory to write the new parfile to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if original parameter file couldn't be opened for reading,<BR>
               -2 if new parameter file couldn't be opened for writing<BR>
   @endreturndesc
@@*/
static int CopyParFile (const char *parfile_name, const char *outdir)
{
  int in_parfile, out_parfile, bytes;
  char *out_parfilename, buffer[256];
  struct stat stat_buf;


  /* get the name of the original parfile and open it for reading */
  CCTK_ParameterFilename (sizeof (buffer), buffer);
  in_parfile = open (buffer, O_RDONLY);
  if (in_parfile < 0)
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Couldn't open original parameter file '%s' (%s)",
                buffer, strerror (errno));
    return (-1);
  }
  /* use the file mode of the original parfile to create the new one */
  if (fstat (in_parfile, &stat_buf))
  {
    stat_buf.st_mode = 0644;
  }

  /* build the name of the output parfile */
  if (! *parfile_name)
  {
    /* cut off any path names */
    parfile_name = strrchr (buffer, '/');
    if (parfile_name)
    {
      parfile_name++;
    }
    else
    {
      parfile_name = buffer;
    }
  }
  out_parfilename = (char *) malloc (strlen (outdir) + strlen (parfile_name)+2);
  sprintf (out_parfilename, "%s/%s", outdir, parfile_name);

  /* binary-copy the input parfile to the output parfile */
  out_parfile = open (out_parfilename, O_CREAT | O_WRONLY | O_EXCL,
                      stat_buf.st_mode);
  if (out_parfile >= 0)
  {
    while ((bytes = read (in_parfile, buffer, sizeof (buffer))) > 0)
    {
      write (out_parfile, buffer, bytes);
    }
    close (out_parfile);
  }
  else
  {
    CCTK_VWarn (3, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Couldn't write parameter file '%s' (%s)",
                out_parfilename, strerror (errno));
  }

  /* clean up */
  close (in_parfile);
  free (out_parfilename);

  return (out_parfile >= 0 ? 0 : -2);
}


 /*@@
   @routine    DumpParameters
   @date       Tue 18 Dec 2001
   @author     Thomas Radke
   @desc
               Generates a new parameter file in 'IO::outdir' with an
               ActiveThorns list and a sorted list of all active parameters
               which have been set in the original parameter file.<BR>
               Note that the new parameter file is only written if no such
               file existed before.
   @enddesc
   @calls      CCTK_ParameterFilename

   @var        parfile_name
   @vdesc      name of the new parfile (or empty string if the original
               parfile name should be used)
   @vtype      const char *
   @vio        in
   @endvar
   @var        outdir
   @vdesc      directory to write the new parfile to
   @vtype      const char *
   @vio        in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if original parameter file couldn't be stat(2),<BR>
               -2 if new parameter file couldn't be opened for writing<BR>
   @endreturndesc
@@*/
static int GenerateParFile (const char *parfile_name,
                            const char *outdir)
{
  FILE *outfile;
  int out_parfile;
  char *out_parfilename, buffer[256];
  struct stat stat_buf;


  /* get the name of the original parfile and stat(2) it */
  CCTK_ParameterFilename (sizeof (buffer), buffer);
  if (stat (buffer, &stat_buf))
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Couldn't stat(2) original parameter file '%s' (%s)",
                buffer, strerror (errno));
    return (-1);
  }
  /* use the file mode of the original parfile to create the new one */
  if (stat (buffer, &stat_buf))
  {
    stat_buf.st_mode = 0644;
  }

  /* build the name of the output parfile */
  if (! *parfile_name)
  {
    /* cut off any path names */
    parfile_name = strrchr (buffer, '/');
    if (parfile_name)
    {
      parfile_name++;
    }
    else
    {
      parfile_name = buffer;
    }
  }
  out_parfilename = (char *) malloc (strlen (outdir) + strlen (parfile_name)+2);
  sprintf (out_parfilename, "%s/%s", outdir, parfile_name);

  /* open the output parfile for writing */
  out_parfile = open (out_parfilename, O_CREAT | O_WRONLY | O_EXCL,
                      stat_buf.st_mode);
  if (out_parfile >= 0)
  {
    outfile = fdopen (out_parfile, "w");
  }
  if (out_parfile >= 0 && outfile)
  {
#if !defined(SPEC_CPU)
    fprintf (outfile, "# '%s' automatically generated by Cactus version %s\n",
             out_parfilename, CCTK_FullVersion ());
    fprintf (outfile, "# Original parameter file was '%s'\n", buffer);
    Util_CurrentTime (sizeof (buffer), buffer);
    fprintf (outfile, "# Run time/date was %s ", buffer);
    Util_CurrentDate (sizeof (buffer), buffer);
    fprintf (outfile, "%s ", buffer);
    Util_GetHostName (buffer, sizeof (buffer));
    fprintf (outfile, "on host '%s' with %d processor(s)\n\n",
             buffer, CCTK_nProcs (NULL));
#endif /* SPEC_CPU */
    DumpParameters (outfile);
    fclose (outfile);
  }
  else
  {
    CCTK_VWarn (3, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Couldn't write parameter file '%s' (%s)",
                out_parfilename, strerror (errno));
  }
  if (out_parfile >= 0)
  {
    close (out_parfile);
  }

  /* clean up */
  free (out_parfilename);

  return (out_parfile >= 0 ? 0 : -2);
}


 /*@@
   @routine    DumpParameters
   @date       Thu Oct 25 2001
   @author     Thomas Radke
   @desc
               Dumps parameter settings of all active thorns into a file.
   @enddesc
   @calls      CCTK_NumCompiledThorns
               CCTK_CompiledThorn
               CCTK_IsThornActive
               CCTK_ParameterWalk
               CCTK_ParameterValString

   @var        parfile
   @vdesc      open file descriptor for the output file
   @vtype      FILE *
   @vio        out
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if file descriptor is invalid
   @endreturndesc
@@*/
static int DumpParameters (FILE *outfile)
{
  int thorn, first, num_thorns;
  int len, maxname_len;
  const char *thornname;
  char *param;
  const cParamData *pdata;
  t_param_list *list, *last, *new;
  const char *quoted_format = "%s::%-*s = \"%s\"\n",
             *non_quoted_format = "%s::%-*s = %s\n";


  /* check passed file descriptor */
  if (outfile == NULL)
  {
    return (-1);
  }

  num_thorns = CCTK_NumCompiledThorns ();

  /* loop over all thorns to print the 'ActiveThorns' list */
  first = 1;
  fputs ("ActiveThorns = \"", outfile);
  for (thorn = 0; thorn < num_thorns; thorn++)
  {
    thornname = CCTK_CompiledThorn (thorn);

    /* skip all inactive thorns and "Cactus" */
    if (strcmp ("Cactus", thornname) && CCTK_IsThornActive (thornname))
    {
      if (! first)
      {
        putc (' ', outfile);
      }
      fputs (thornname, outfile);
      first = 0;
    }
  }
  fputs ("\"\n", outfile);

  /* loop over all thorns */
  for (thorn = 0; thorn < num_thorns; thorn++)
  {
    thornname = CCTK_CompiledThorn (thorn);

    /* skip all inactive thorns */
    if (! CCTK_IsThornActive (thornname))
    {
      continue;
    }

    /* now walk through all parameters of given thorn */
    maxname_len = 0;
    list = last = NULL;
    first = 1;
    while (CCTK_ParameterWalk (first, thornname, &param, &pdata) == 0)
    {
      first = 0;

      /* skip the parameters which weren't explicitely set */
      if (pdata->n_set > 0)
      {
        new = (t_param_list *) malloc (sizeof (t_param_list));
        if (new)
        {
          new->value = CCTK_ParameterValString (pdata->name, pdata->thorn);
          if (new->value)
          {
            new->format_string = pdata->type == PARAMETER_INT ||
                                 pdata->type == PARAMETER_REAL ?
                                 non_quoted_format : quoted_format;
            new->name = pdata->name;
            len = strlen (new->name);
            if (len > maxname_len)
            {
              maxname_len = len;
            }
            if (last)
            {
              last->next = new; last = new;
            }
            else
            {
              list = last = new;
            }
            last->next = NULL;
          }
          else
          {
            CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                        "Couldn't get value for parameter '%s'", param);
            free (new);
          }
        }
        else
        {
          CCTK_WARN (1, "Couldn't allocate list element");
        }
      }
      free (param);
    } /* end of loop walking over all parameters of given thorn */

    /* finally dump out the list for this thorn */
    if (list)
    {
      fprintf (outfile, "\n# Parameters of thorn %s (implementing %s)\n",
               thornname, CCTK_ThornImplementation (thornname));
    }
    while (list)
    {
      fprintf (outfile, list->format_string, thornname, maxname_len,
               list->name, list->value);
      free (list->value);
      last = list->next;
      free (list);
      list = last;
    }
  } /* end of looping over all thorns */

  return (0);
}
