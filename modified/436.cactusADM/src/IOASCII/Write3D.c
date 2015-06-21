#ifdef SPEC_CPU
# define THORN_IS_IOASCII
#endif /* SPEC_CPU */
/*@@
   @file      Write3D.c
   @date      Wed 12 Dec 2001
   @author    Thomas Radke
   @desc
              Three-dimensional output of variables in ASCII gnuplot format.
   @enddesc
   @version   $Id: Write3D.c,v 1.2 2001/12/28 21:28:37 tradke Exp $
 @@*/


#include <stdlib.h>
#include <string.h>

#include "cctk.h"
#include "cctk_Parameters.h"
#include "Hyperslab.h"
#include "ioutil_AdvertisedFiles.h"
#include "ioutil_CheckpointRecovery.h"
#include "ioASCIIGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Id: Write3D.c,v 1.2 2001/12/28 21:28:37 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOASCII_Write3D_c)

/* enable debug output */
/*#define IOASCII_DEBUG 1*/


/* macro to output a time volume as typed data */
#define OUTPUT_TYPED_DATA(hsize, coord, stagger_offset, cctk_type, c_type,    \
                          data, have_coords, is_cmplx_type, fmt_string, file) \
          {                                                                   \
            int _i, _j, _k;                                                   \
            const cctk_type *_typed_data = (const cctk_type *) (data);        \
                                                                              \
                                                                              \
            /* output coordinates if available otherwise just the indices */  \
            if (have_coords)                                                  \
            {                                                                 \
              for (_k = 0; _k < hsize[2]; _k++)                               \
              {                                                               \
                for (_j = 0; _j < hsize[1]; _j++)                             \
                {                                                             \
                  for (_i = 0; _i < hsize[0]; _i++)                           \
                  {                                                           \
                    fprintf (file, fmt_string,                                \
                             (double) (*coord[0]++ + stagger_offset[0]),      \
                             (double) (*coord[1]++ + stagger_offset[1]),      \
                             (double) (*coord[2]++ + stagger_offset[2]),      \
                             (c_type) *_typed_data++);                        \
                    if (is_cmplx_type)                                        \
                    {                                                         \
                      fprintf (file, "\t\t");                                 \
                      fprintf (file, out_real_format, (double)*_typed_data++);\
                    }                                                         \
                    fprintf (file, "\n");                                     \
                  }                                                           \
                  fprintf (file, "\n");                                       \
                }                                                             \
              }                                                               \
              coord[0] -= hsize[0] * hsize[1] * hsize[2];                     \
              coord[1] -= hsize[0] * hsize[1] * hsize[2];                     \
              coord[2] -= hsize[0] * hsize[1] * hsize[2];                     \
            }                                                                 \
            else                                                              \
            {                                                                 \
              for (_k = 0; _k < hsize[2]; _k++)                               \
              {                                                               \
                for (_j = 0; _j < hsize[1]; _j++)                             \
                {                                                             \
                  for (_i = 0; _i < hsize[0]; _i++)                           \
                  {                                                           \
                    fprintf (file, fmt_string,                                \
                             (double) _i,                                     \
                             (double) _j,                                     \
                             (double) _k,                                     \
                             (c_type) *_typed_data++);                        \
                    if (is_cmplx_type)                                        \
                    {                                                         \
                      fprintf (file, "\t\t");                                 \
                      fprintf (file, out_real_format, (double)*_typed_data++);\
                    }                                                         \
                    fprintf (file, "\n");                                     \
                  }                                                           \
                  fprintf (file, "\n");                                       \
                }                                                             \
              }                                                               \
            }                                                                 \
          }


/*@@
   @routine   IOASCII_Write3D
   @date      Wed 12 Dec 2001
   @author    Thomas Radke
   @desc
              Writes the 3D volume of a variable into a gnuplot ASCII file.
   @enddesc
   @calls     IOUtil_RestartFromRecovery
              IOUtil_AdvertiseFile

   @var       GH
   @vdesc     Pointer to CCTK GH
   @vtype     const cGH *
   @vio       in
   @endvar
   @var       vindex
   @vdesc     index of variable to output
   @vtype     int
   @vio       in
   @endvar
   @var       alias
   @vdesc     alias name of variable to output
   @vtype     const char *
   @vio       in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if variable has no storage assigned<BR>
               -2 if output file couldn't be opened<BR>
               -3 if hyperslab for coordinates and/or variable couldn't be
                  extracted
   @endreturndesc
@@*/
int IOASCII_Write3D (const cGH *GH, int vindex, const char *alias)
{
  DECLARE_CCTK_PARAMETERS
  int myproc, groupindex, dir, have_coords;
  asciiioGH *myGH;
  char header_fmt_string[30];        /* header format string */
  char out_real_format[30];          /* data format string for a real type */
  char time_fmt_string[30];          /* time format string */
  char data_fmt_string_int[30];      /* data format string for int types */
  char data_fmt_string_real[30];     /* data format string for real types */
  FILE **file;
  int coord_index[3];                /* variable indices for xyz coordinates */
  CCTK_REAL coord_lower[3];          /* coordinates' minima */
  CCTK_REAL dummy;
  char *filename;
  char *fullname;
  cGroup groupinfo;
  ioAdvertisedFileDesc advertised_file;
  char buffer[128];
  const int directions[3] = {1, 1, 1};
  const int origin[3] = {0, 0, 0};
  const int lengths[3] = {-1, -1, -1};
  const int downsamples[3] = {1, 1, 1};
  int hsize[3];
  CCTK_REAL *coord_data[3], stagger_offset[3];
  void *data;


  /* to make the compiler happy */
  file = NULL;
  filename = fullname = NULL;

  /* get the variable group information */
  groupindex = CCTK_GroupIndexFromVarI (vindex);
  CCTK_GroupData (groupindex, &groupinfo);

  /* check if variable has storage assigned */
  if (! CCTK_QueryGroupStorageI (GH, groupindex))
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "No IOASCII 3D output for '%s' (no storage)", fullname);
    free (fullname);
    return (-1);
  }

  /* Get the handle for IOASCII extensions */
  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  /* set header and data format strings */
  sprintf (header_fmt_string, "\n\n#Time = %%%s\n", out_format);
  sprintf (out_real_format, "%%%s", out_format);
  dir = 0;
  if (CCTK_Equals (out3D_style, "gnuplot f(t,x,y,z)"))
  {
    sprintf (time_fmt_string,  "%%%s\t\t", out_format);
    sprintf (data_fmt_string_int,  time_fmt_string, (double) GH->cctk_time);
    sprintf (data_fmt_string_real, time_fmt_string, (double) GH->cctk_time);
    dir = strlen (data_fmt_string_int);
  }
  sprintf (data_fmt_string_int + dir, "%%%s\t\t%%%s\t\t%%d\t\t%%d",
           out_format, out_format);
  sprintf (data_fmt_string_real + dir, "%%%s\t\t%%%s\t\t%%%s\t\t%%%s",
           out_format, out_format, out_format, out_format);

  /* get the coordinate indices if we output a grid function */
  if (groupinfo.grouptype == CCTK_GF)
  {
    have_coords = 1;
    for (dir = 0; dir < 3; dir++)
    {
      CCTK_CoordRange (GH, &coord_lower[dir], &dummy, dir+1, NULL, "cart3d");
      coord_index[dir] = CCTK_CoordIndex (dir + 1, NULL,  "cart3d");
      have_coords &= coord_index[dir] >= 0;
    }

    if (! have_coords)
    {
      CCTK_VWarn (8, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_Write3D: No coordinate ranges found for '%s'",
                   "cart3d");
    }
  }
  else
  {
    /* CCTK_ARRAY variables never have coordinates associated */
    have_coords = 0;
  }

  /* What processor are we on? */
  myproc = CCTK_MyProc (GH);

  /* Open the files on the first trip through if we are proc. 0 */
  if (myproc == 0)
  {
    /* see if output file for this alias name was already created */
    file = (FILE **) GetNamedData (myGH->fileList_3D, alias);
    if (file == NULL)
    {
      file = (FILE **) malloc (sizeof (FILE *));
      filename = (char *) malloc (strlen (myGH->outdir3D) + strlen (alias) + 9);

      /* get the variable's full name */
      fullname = CCTK_FullName (vindex);

      /* Open/Create the file */
      /* skip pathname if output goes into current directory */
      if (strcmp (myGH->outdir3D, "."))
      {
        sprintf (filename, "%s/%s_3D.asc", myGH->outdir3D, alias);
      }
      else
      {
        sprintf (filename, "%s_3D.asc", alias);
      }

      /* if restart from recovery, try to open an existing file ... */
      *file = NULL;
      if (IOUtil_RestartFromRecovery (GH))
      {
        *file = fopen (filename, "a");
      }

      /* otherwise or if that failed, create a new one */
      if (! *file)
      {
        *file = fopen (filename, "w");
      }
      if (! *file)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Cannot open 3D IOASCII output file '%s'", filename);
        return (-2);
      }

      /* advertise the file for downloading and write file info */
      /* FIXME: this can go when we permanently switch the the
                new filename scheme */
      advertised_file.slice = "";
      advertised_file.thorn = CCTK_THORNSTRING;
      advertised_file.varname = fullname;
      advertised_file.description = "Full-dimensional variable contents";
      advertised_file.mimetype = "application/gnuplot";

      IOUtil_AdvertiseFile (GH, filename, &advertised_file);

      if (CCTK_Equals (out_fileinfo, "parameter filename") ||
          CCTK_Equals (out_fileinfo, "all"))
      {
        buffer[0] = 0;
        CCTK_ParameterFilename (sizeof (buffer), buffer);
        fprintf (*file, "#Parameter file %s\n", buffer);
      }
      if (CCTK_Equals (out_fileinfo, "creation date") ||
          CCTK_Equals (out_fileinfo, "all"))
      {
        buffer[0] = 0;
        Util_CurrentDate (sizeof (buffer), buffer);
        fprintf (*file, "#Created %s ", buffer);
        Util_CurrentTime (sizeof (buffer), buffer);
        fprintf (*file, "%s\n", buffer);
      }
      if (CCTK_Equals (out_fileinfo, "axis labels") ||
          CCTK_Equals (out_fileinfo, "all"))
      {
        fprintf (*file, "#z-label %s\n", advertised_file.varname);
      }
    }

    /* store file desriptors in database */
    StoreNamedData (&myGH->fileList_3D, alias, file);

    free (filename);
    free (fullname);
  }

  /* get the coordinates for grid function output */
  if (have_coords)
  {
    /* get the i-coordinate volume */
    if (Hyperslab_GetHyperslab (GH, 0, coord_index[0], 0, 3,
                                origin, directions, lengths, downsamples,
                                (void **) &coord_data[0], hsize) < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Failed to extract 3D hyperslab for x-coordinate");
      return (-3);
    }

    /* get the j-coordinate volume */
    if (Hyperslab_GetHyperslab (GH, 0, coord_index[1], 0, 3,
                                origin, directions, lengths, downsamples,
                                (void **) &coord_data[1], hsize) < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Failed to extract 3D hyperslab for y-coordinate");
      free (coord_data[0]);
      return (-3);
    }

    /* get the k-coordinate volume */
    if (Hyperslab_GetHyperslab (GH, 0, coord_index[2], 0, 3,
                                origin, directions, lengths, downsamples,
                                (void **) &coord_data[2], hsize) < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Failed to extract 3D hyperslab for z-coordinate");
      free (coord_data[1]);
      free (coord_data[0]);
      return (-3);
    }
  }
  else
  {
    /* grid arrays don't have coordinates assigned */
    coord_data[0] = coord_data[1] = coord_data[2] = NULL;
  }

  /* get the variable volume */
  if (Hyperslab_GetHyperslab (GH, 0, vindex, 0, 3, origin, directions,
                              lengths, downsamples, &data, hsize) < 0)
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                "Failed to extract 3D hyperslab for variable '%s'", fullname);
    free (fullname);
    for (dir = 0; dir < 3; dir++)
    {
      if (coord_data[dir])
      {
        free (coord_data[dir]);
      }
    }
    return (-3);
  }

  /* proc 0 writes */
  if (myproc == 0)
  {
    /* get the staggering offset for the coordinates */
    for (dir = 0; dir < 3; dir++)
    {
      stagger_offset[dir] = CCTK_StaggerDirIndex (dir, groupinfo.stagtype) *
                            0.5 * GH->cctk_delta_space[dir];
    }

    /* print out header */
    fprintf (*file, header_fmt_string, GH->cctk_time);

    switch (groupinfo.vartype)
    {
      case CCTK_VARIABLE_CHAR:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_BYTE, int,
                           data, have_coords, 0, data_fmt_string_int, *file);
        break;

      case CCTK_VARIABLE_INT:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_INT, int,
                           data, have_coords, 0, data_fmt_string_int, *file);
        break;

      case CCTK_VARIABLE_REAL:
      case CCTK_VARIABLE_COMPLEX:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_REAL, double,
                           data, have_coords,
                           groupinfo.vartype == CCTK_VARIABLE_COMPLEX,
                           data_fmt_string_real, *file);
        break;

#ifdef CCTK_INT2
      case CCTK_VARIABLE_INT2:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_INT2, int,
                           data, have_coords, 0, data_fmt_string_int, *file);
        break;
#endif

#ifdef CCTK_INT4
      case CCTK_VARIABLE_INT4:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_INT4, int,
                           data, have_coords, 0, data_fmt_string_int, *file);
        break;
#endif

#ifdef CCTK_INT8
      case CCTK_VARIABLE_INT8:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_INT8, int,
                           data, have_coords, 0, data_fmt_string_int, *file);
        break;
#endif

#ifdef CCTK_REAL4
      case CCTK_VARIABLE_REAL4:
      case CCTK_VARIABLE_COMPLEX8:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_REAL4,
                           double, data, have_coords,
                           groupinfo.vartype == CCTK_VARIABLE_COMPLEX8,
                           data_fmt_string_real, *file);
        break;
#endif

#ifdef CCTK_REAL8
      case CCTK_VARIABLE_REAL8:
      case CCTK_VARIABLE_COMPLEX16:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_REAL8,
                           double, data, have_coords,
                           groupinfo.vartype == CCTK_VARIABLE_COMPLEX16,
                           data_fmt_string_real, *file);
        break;
#endif

#ifdef CCTK_REAL16
      case CCTK_VARIABLE_REAL16:
      case CCTK_VARIABLE_COMPLEX32:
        OUTPUT_TYPED_DATA (hsize, coord_data, stagger_offset, CCTK_REAL16,
                           double, data, have_coords,
                           groupinfo.vartype == CCTK_VARIABLE_COMPLEX32,
                           data_fmt_string_real, *file);
        break;
#endif

      default:
        CCTK_WARN (1, "Unsupported variable type");
        break;
    }

    /* keep the file open but flush it */
    fflush (*file);

    /* free the hyperslabs */
    free (data);
    for (dir = 0; dir < 3; dir++)
    {
      if (coord_data[dir])
      {
        free (coord_data[dir]);
      }
    }

  } /* end of outputting the data by processor 0 */


  USE_CCTK_PARAMETERS;   return (0);
}
