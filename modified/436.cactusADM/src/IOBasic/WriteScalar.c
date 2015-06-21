#ifdef SPEC_CPU
# define THORN_IS_IOBasic
#endif /* SPEC_CPU */
/*@@
   @routine    WriteScalar.c
   @date       18th September 1999
   @author     Gabrielle Allen
   @desc 
               Dumps data for IOBasic's "Scalar" I/O method to output files
   @enddesc 
@@*/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>      /* strlen(3) */
#include <sys/types.h>
#include <sys/stat.h>    /* stat(2) */

#include "cctk.h"
#include "cctk_Parameters.h"
#include "ioutil_AdvertisedFiles.h"
#include "ioutil_CheckpointRecovery.h"
#include "iobasicGH.h"

static const char *rcsid = "$Header: /cactus/CactusBase/IOBasic/src/WriteScalar.c,v 1.7 2002/01/18 15:30:33 tradke Exp $";

CCTK_FILEVERSION(CactusBase_IOBasic_WriteScalar_c)


static FILE *OpenScalarFile (const cGH *GH,
                             int vindex,
                             const char *filename,
                             const char *slicename,
                             const char *description,
                             const char *aliasname);

 /*@@
   @routine   IOBasic_WriteScalarGA
   @date      Mon Jun 19 2000
   @author    Thomas Radke
   @desc
              Apply the given reduction operators on the CCTK_ARRAY or
              CCTK_GF variable and output the result into a text file
              suitable for postprocessing by either gluplot or xgraph.
   @enddesc

   @calls     CCTK_QueryGroupStorageI
              CCTK_Reduce
              CCTK_ReductionHandle
              IOUtil_RestartFromRecovery
              IOUtil_AdvertiseFile

   @var       GH
   @vdesc     Pointer to CCTK grid hierarchy
   @vtype     const cGH *
   @vio       in
   @endvar
   @var       vindex
   @vdesc     CCTK index of the variable to output
   @vtype     int
   @vio       in
   @endvar
   @var       alias
   @vdesc     alias name to use for building the output filename
   @vtype     const char *
   @vio       in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if variable has no storage assigned
   @endreturndesc
@@*/
int IOBasic_WriteScalarGA (const cGH *GH, int vindex, const char *alias)
{
  int ierr;
  int reduction_handle;
  iobasicGH *myGH;
  FILE *file;
  char *filename;
  char *reduction_op;
  char *string_start;
  char *string_end;
  char format_str[15];
  const char *file_extension;
  char *fullname;
  CCTK_REAL reduction_value;
  union
  {
    char *non_const_ptr;
    const char *const_ptr;
  } reductions;
  /*** FIXME: can CCTK_Reduce() have a 'const cGH *' parameter ?? ***/
  union
  {
    const cGH *const_ptr;
    cGH *non_const_ptr;
  } GH_fake_const;
  DECLARE_CCTK_PARAMETERS


  /* this union helps us to avoid compiler warning about discarding
     the const qualifier from a pointer target type */
  reductions.const_ptr = outScalar_reductions;
  GH_fake_const.const_ptr = GH;

  /* first, check if variable has storage assigned */
  if (! CCTK_QueryGroupStorageI (GH, CCTK_GroupIndexFromVarI (vindex)))
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOBasic_WriteScalarGA: No scalar output for '%s' (no storage)", 
                fullname);
    free (fullname);
    return (-1);
  }

  /* set output format */
  sprintf (format_str, "%%%s\t%%%s\n", out_format, out_format);

  /* set the output file extension and the output style */
  file_extension = CCTK_Equals (outScalar_style, "gnuplot") ? ".asc" : ".xg";

  /* get the GH extension handle for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  /* allocate string for the reduction operator */
  reduction_op = (char *) malloc (strlen (reductions.const_ptr) + 1);

  /* now loop over all reduction operators */
  string_start = reductions.non_const_ptr;
  while (string_start && *string_start)
  {
    /* skip leading spaces */
    while (isspace ((int) *string_start))
    {
      string_start++;
    }
    if (! *string_start)
    {
      break;
    }

    /* advance to end of the operator string */
    string_end = string_start + 1;
    while (*string_end && ! isspace ((int) *string_end))
    {
      string_end++;
    }

    /* copy the operator string */
    strncpy (reduction_op, string_start, string_end - string_start);
    reduction_op[string_end - string_start] = 0;
    string_start = string_end;

    /* get the reduction handle from the reduction operator */
    reduction_handle = CCTK_ReductionHandle (reduction_op);
    if (reduction_handle < 0) 
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOBasic_WriteScalarGA: Invalid reduction operator '%s'", 
                  reduction_op);
      continue;
    }

    /* do the reduction (all processors) */
    ierr = CCTK_Reduce (GH_fake_const.non_const_ptr, 0, reduction_handle, 1,
                        CCTK_VARIABLE_REAL, &reduction_value, 1, vindex);

    /* dump the reduction value to file by processor 0 */
    if (ierr == 0 && CCTK_MyProc (GH) == 0)
    {

      if (new_filename_scheme)
      {
        /* allocate filename string buffer and build the filename */
        filename = (char *) malloc (strlen (myGH->outdirScalar) +
                                    strlen (alias) + strlen (reduction_op) +
                                    strlen (file_extension) + 2);
        sprintf (filename, "%s%s_%s%s", myGH->outdirScalar, alias,
                                        reduction_op, file_extension);
      }
      else
      {
        /* FIXME: this check can go if we switch to the new filename scheme */
        if (strcmp (reduction_op, "minimum") == 0)
        {
          file_extension = "min";
        }
        else if (strcmp (reduction_op, "maximum") == 0)
        {
          file_extension = "max";
        }
        else if (strcmp (reduction_op, "norm1") == 0)
        {
          file_extension = "nm1";
        }
        else if (strcmp (reduction_op, "norm2") == 0)
        {
          file_extension = "nm2";
        }
        else
        {
          file_extension = reduction_op;
        }

        /* allocate filename string buffer and build the filename */
        filename = (char *) malloc (strlen (myGH->outdirScalar) +
                                    strlen (alias) + strlen (file_extension)+5);
        sprintf (filename, "%s%s_%s.tl", myGH->outdirScalar, alias,
                 file_extension);
      }

      file = OpenScalarFile (GH, vindex, filename, reduction_op,
                             "Reduction on Grid Arrays", alias);
      if (file)
      {
        /* write the data and close the file */
        fprintf (file, format_str, GH->cctk_time, reduction_value);
        fclose (file);
      }
      else
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "IOBasic_WriteScalarGA: Could not open output file '%s'", 
                    filename);
      }
      free (filename);
    }
  }

  /* free allocated resources */
  free (reduction_op);

  USE_CCTK_PARAMETERS;   return (0);
}


 /*@@
   @routine   IOBasic_WriteScalarGS
   @date      Mon Jun 19 2000
   @author    Thomas Radke
   @desc
              Output the value of a CCTK_SCALAR variable into an ASCII file
              suitable for postprocessing by either gluplot or xgraph.
   @enddesc

   @calls     CCTK_QueryGroupStorageI
              CCTK_Reduce
              CCTK_ReductionHandle
              IOUtil_RestartFromRecovery
              IOUtil_AdvertiseFile

   @var       GH
   @vdesc     Pointer to CCTK grid hierarchy
   @vtype     const cGH *
   @vio       in
   @endvar
   @var       vindex
   @vdesc     CCTK index of the variable to output
   @vtype     int
   @vio       in
   @endvar
   @var       alias
   @vdesc     alias name to use for building the output filename
   @vtype     const char *
   @vio       in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if variable has no storage assigned
   @endreturndesc
@@*/
int IOBasic_WriteScalarGS (const cGH *GH, int vindex, const char *alias)
{
  DECLARE_CCTK_PARAMETERS
  FILE *file;
  void *data;
  iobasicGH *myGH;
  char *fullname, *filename;
  const char *file_extension;
  char format_str_real[15], format_str_int[15];


  /* output is done by processor 0 only */
  if (CCTK_MyProc (GH) != 0) 
  {
    return (0);
  }

  /* first, check if variable has storage assigned */
  if (! CCTK_QueryGroupStorageI (GH, CCTK_GroupIndexFromVarI (vindex)))
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOBasic_WriteScalarGS: No scalar output for '%s' (no storage)",
                fullname);
    free (fullname);
    return (-1);
  }

  /* set the output format string for the desired notation */
  sprintf (format_str_real, "%%%s\t%%%s\n", out_format, out_format);
  sprintf (format_str_int,  "%%%s\t%%d\n", out_format);

  /* get the GH extensions for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  /* set the output file extension according to the output style */
  if (new_filename_scheme)
  {
    file_extension = CCTK_Equals (outScalar_style, "gnuplot") ? ".asc" : ".xg";
  }
  else
  {
    file_extension = ".tl";
  }

  /* build the output filename */
  filename = (char *) malloc (strlen (myGH->outdirScalar) + strlen (alias) +
                              strlen (file_extension) + 1);
  sprintf (filename, "%s%s%s", myGH->outdirScalar, alias, file_extension);

  /* create/reopen the file */
  file = OpenScalarFile (GH, vindex, filename, "tl", "Scalar value", alias);
  if (file)
  {
    /* get the data pointer */
    data = CCTK_VarDataPtrI (GH, 0, vindex); 

    switch (CCTK_VarTypeI (vindex))
    {
      case CCTK_VARIABLE_REAL:
        fprintf (file, format_str_real, GH->cctk_time,
                 (double) *(CCTK_REAL *) data);
        break;
      case CCTK_VARIABLE_INT:
        fprintf (file, format_str_int, GH->cctk_time,
                 (int) *(CCTK_INT *) data);
        break;
      default:
        CCTK_WARN (3, "Unsupported data type");
        break;
    }

    /* close the output file */
    fclose (file);
  }
  else
  {
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOBasic_WriteScalarGS: Could not open output file '%s'",
                filename);
  }

  /* clean up */
  free (filename);

  USE_CCTK_PARAMETERS;   return (0);
}


static FILE *OpenScalarFile (const cGH *GH,
                             int vindex,
                             const char *filename,
                             const char *slicename,
                             const char *description,
                             const char *aliasname)
{
  DECLARE_CCTK_PARAMETERS
  FILE *file;
  char comment_char, buffer[128];
  ioAdvertisedFileDesc advertised_file; 
  iobasicGH *myGH;
  char *openmode, *fullname;
  struct stat fileinfo;


  /* get the GH extension handle for IOBasic */
  myGH = (iobasicGH *) CCTK_GHExtension (GH, "IOBasic");

  /* get the variable's full name */
  fullname = CCTK_FullName (vindex);

  /* see if output files for this alias name were already created */
  if (GetNamedData (myGH->filenameListScalar, filename) == NULL)
  {
    /* if restart from recovery, all existing files are opened
       in append mode */
    if (IOUtil_RestartFromRecovery (GH))
      openmode = stat (filename, &fileinfo) == 0 ? "a" : "w";
    else
      openmode = "w";
  }
  else
  {
    openmode = "a";
  }

  /* open the output file with the given mode */
  file = fopen (filename, openmode);
  if (file && *openmode == 'w')
  {
    if (CCTK_Equals (outScalar_style, "gnuplot")) 
    {
      comment_char = '#';
      advertised_file.mimetype = "application/gnuplot";
    }
    else
    {
      comment_char = '"';    /* this is for xgraph */
      advertised_file.mimetype = "application/x-graph";
    }

    /* just store a non-NULL pointer in database */
    StoreNamedData (&myGH->filenameListScalar, filename, (void *) 1);

    /* advertise the file for downloading */
    advertised_file.slice = slicename;
    advertised_file.thorn = CCTK_THORNSTRING;
    advertised_file.varname = fullname;
    advertised_file.description = description;

    IOUtil_AdvertiseFile (GH, filename, &advertised_file);

    /* write the file info and the header */
    if (CCTK_Equals (out_fileinfo, "parameter filename") ||
        CCTK_Equals (out_fileinfo, "all"))
    {
      buffer[0] = 0;
      CCTK_ParameterFilename (sizeof (buffer), buffer);
      fprintf (file, "%cParameter file %s\n", comment_char, buffer);
    }
    if (CCTK_Equals (out_fileinfo, "creation date") ||
        CCTK_Equals (out_fileinfo, "all"))
    {
      buffer[0] = 0;
      Util_CurrentDate (sizeof (buffer), buffer);
      fprintf (file, "%cCreated %s ", comment_char, buffer);
      Util_CurrentTime (sizeof (buffer), buffer);
      fprintf (file, "%s\n", buffer);
    }
    if (CCTK_Equals (out_fileinfo, "axis labels") ||
        CCTK_Equals (out_fileinfo, "all"))
    {
      fprintf (file, "%cx-label time\n", comment_char);
      fprintf (file, "%cy-label %s\n", comment_char, advertised_file.varname);
    }
    fprintf (file, "%c%s v time\n", comment_char, aliasname);

  }

  free (fullname);

  USE_CCTK_PARAMETERS;   return (file);
}
