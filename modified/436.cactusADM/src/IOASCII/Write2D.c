#ifdef SPEC_CPU
# define THORN_IS_IOASCII
#endif /* SPEC_CPU */
/*@@
   @file      Write2D.c
   @date      Thu May 11 2000
   @author    Thomas Radke
   @desc
              Output two-dimensional slices in ASCII gnuplot format.
   @enddesc
   @version   $Id: Write2D.c,v 1.30 2001/12/28 21:28:37 tradke Exp $
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
static const char *rcsid = "$Id: Write2D.c,v 1.30 2001/12/28 21:28:37 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOASCII_Write2D_c)

/* enable debug output */
/*#define IOASCII_DEBUG 1*/


/* macro to output a time slice as typed data */
#define OUTPUT_TYPED_DATA(coord_i, coord_j, stagger_offset_i,stagger_offset_j,\
                          cctk_type, c_type, data, hsize, is_cmplx_type,      \
                          have_coords, fmt_string, file)                      \
          {                                                                   \
            int i, j;                                                         \
            const cctk_type *typed_data = (const cctk_type *) (data);         \
                                                                              \
                                                                              \
            /* output coordinates if available otherwise just the indices */  \
            if (have_coords)                                                  \
            {                                                                 \
              for (j = 0; j < hsize[1]; j++)                                  \
              {                                                               \
                for (i = 0; i < hsize[0]; i++)                                \
                {                                                             \
                  fprintf (file, fmt_string,                                  \
                           (double) (*coord_i++ + stagger_offset_i),          \
                           (double) (*coord_j++ + stagger_offset_j),          \
                           (c_type) *typed_data++);                           \
                  if (is_cmplx_type)                                          \
                  {                                                           \
                    fprintf (file, "\t\t");                                   \
                    fprintf (file, out_real_format, (double) *typed_data++);  \
                  }                                                           \
                  fprintf (file, "\n");                                       \
                }                                                             \
                fprintf (file, "\n");                                         \
              }                                                               \
              coord_i -= hsize[0] * hsize[1];                                 \
              coord_j -= hsize[0] * hsize[1];                                 \
            }                                                                 \
            else                                                              \
            {                                                                 \
              for (j = 0; j < hsize[1]; j++)                                  \
              {                                                               \
                for (i = 0; i < hsize[0]; i++)                                \
                {                                                             \
                  fprintf (file, fmt_string,                                  \
                           (double) i,                                        \
                           (double) j,                                        \
                           (c_type) *typed_data++);                           \
                  if (is_cmplx_type)                                          \
                  {                                                           \
                    fprintf (file, "\t\t");                                   \
                    fprintf (file, out_real_format, (double) *typed_data++);  \
                  }                                                           \
                  fprintf (file, "\n");                                       \
                }                                                             \
                fprintf (file, "\n");                                         \
              }                                                               \
            }                                                                 \
          }


/*@@
   @routine   IOASCII_Write2D
   @date      Thu May 11 2000
   @author    Thomas Radke
   @desc
              Writes the 2D slices of a variable into separate ASCII files.
   @enddesc
   @calls     IOUtil_RestartFromRecovery
              IOUtil_AdvertiseFile
              Hyperslab_GetHyperslab

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
int IOASCII_Write2D (const cGH *GH, int vindex, const char *alias)
{
  DECLARE_CCTK_PARAMETERS
  int myproc;
  asciiioGH *myGH;
  char header_fmt_string[30];        /* header format string */
  char zlabel_fmt_string[30];        /* z-label format string */
  char out_real_format[30];          /* data format string for a real type */
  char time_fmt_string[30];          /* time format string */
  char data_fmt_string_int[30];      /* data format string for int types */
  char data_fmt_string_real[30];     /* data format string for real types */
  int dir, maxdir;
  int groupindex;
  int have_coords;
  cGroup groupinfo;
  FILE **fdset_2D;                   /* array of output file pointers */
  int coord_index[3];                /* variable indices for xyz coordinates */
  CCTK_REAL coord_lower[3];          /* coordinates' minima */
  char coord_system[20];             /* name of the coordinate system */
  int origin[3];                     /* the slice origin */
  CCTK_REAL dummy;
  char *filename;
  char *fullname;
  char slicename[20];
  ioAdvertisedFileDesc advertised_file;
  char buffer[128];
  static char *extensions[] = {"xy", "xz", "yz"};


  /* to make the compiler happy */
  fdset_2D = NULL;

  /* get the variable group information */
  groupindex = CCTK_GroupIndexFromVarI (vindex);
  CCTK_GroupData (groupindex, &groupinfo);

  /* check if variable has storage assigned */
  if (! CCTK_QueryGroupStorageI (GH, groupindex))
  {
    fullname = CCTK_FullName (vindex);
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "No IOASCII 2D output for '%s' (no storage)", fullname);
    free (fullname);
    return (-1);
  }

  /* Get the handle for IOASCII extensions */
  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  /* get the number of slices to output */
  /* in general: maxdir = groupinfo.dim * (groupinfo.dim - 1) / 2; */
  maxdir = groupinfo.dim == 2 ? 1 : 3;

  /* set header and data format strings */
  sprintf (header_fmt_string, "\n\n#Time = %%%s\n", out_format);
  sprintf (zlabel_fmt_string, " (%%c = %%%s),", out_format);
  sprintf (out_real_format, "%%%s", out_format);
  dir = 0;
  if (CCTK_Equals (out2D_style, "gnuplot f(t,x,y)"))
  {
    sprintf (time_fmt_string,  "%%%s\t\t", out_format);
    sprintf (data_fmt_string_int,  time_fmt_string, (double) GH->cctk_time);
    sprintf (data_fmt_string_real, time_fmt_string, (double) GH->cctk_time);
    dir = strlen (data_fmt_string_int);
  }
  sprintf (data_fmt_string_int + dir, "%%%s\t\t%%%s\t\t%%d",
           out_format, out_format);
  sprintf (data_fmt_string_real + dir, "%%%s\t\t%%%s\t\t%%%s",
           out_format, out_format, out_format);

  /* get the coordinate indices if we output a grid function */
  if (groupinfo.grouptype == CCTK_GF)
  {
    sprintf (coord_system, "cart%dd", groupinfo.dim);
    have_coords = 1;
    for (dir = 0; dir < groupinfo.dim && dir < 3; dir++)
    {
      CCTK_CoordRange (GH, &coord_lower[dir], &dummy, dir+1, NULL,coord_system);
      coord_index[dir] = CCTK_CoordIndex (dir + 1, NULL, coord_system);
      have_coords &= coord_index[dir] >= 0;
    }

    if (! have_coords)
    {
      CCTK_VWarn (8, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_Write2D: No coordinate ranges found for '%s'",
                  coord_system);
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
    fdset_2D = (FILE **) GetNamedData (myGH->fileList_2D, alias);
    if (fdset_2D == NULL)
    {
      fdset_2D = (FILE **) malloc (3 * sizeof (FILE *));
      filename = (char *) malloc (strlen (myGH->outdir2D) + strlen (alias) +
                                  sizeof (slicename) + 20);

      /* get the variable's full name */
      fullname = CCTK_FullName (vindex);

      /* Open/Create files */
      for (dir = 0; dir < maxdir; dir++)
      {
        /* FIXME: this can go when we permanently switch the the
                  new filename scheme */
        if (new_filename_scheme)
        {
          if (groupinfo.dim == 2)
          {
            strcpy (slicename, "2D");
          }
          else
          {
            /* give the slice origin as range [1 .. n] */
            sprintf (slicename, "%s_[%d]", extensions[dir],
                     myGH->sp2xyz[groupinfo.dim - 1][dir]);
          }

          /* skip pathname if output goes into current directory */
          if (strcmp (myGH->outdir2D, "."))
          {
            sprintf (filename, "%s/%s_%s.asc", myGH->outdir2D, alias,slicename);
          }
          else
          {
            sprintf (filename, "%s_%s.asc", alias, slicename);
          }
        }
        else
        {
          /* skip pathname if output goes into current directory */
          if (strcmp (myGH->outdir2D, "."))
          {
            if (groupinfo.dim == 2)
            {
              sprintf (filename, "%s/%s_2D.gnuplot", myGH->outdir2D, alias);
            }
            else
            {
              sprintf (filename, "%s/%s_2d_%s.gnuplot", myGH->outdir2D, alias,
                       extensions[dir]);
            }
          }
          else
          {
            if (groupinfo.dim == 2)
            {
              sprintf (filename, "%s_2D.gnuplot", alias);
            }
            else
            {
              sprintf (filename, "%s_2d_%s.gnuplot", alias, extensions[dir]);
            }
          }
        }

        /* if restart from recovery, try to open an existing file ... */
        fdset_2D[dir] = NULL;
        if (IOUtil_RestartFromRecovery (GH))
        {
          fdset_2D[dir] = fopen (filename, "a");
        }

        /* otherwise or if that failed, create a new one */
        if (! fdset_2D[dir])
        {
          fdset_2D[dir] = fopen (filename, "w");
        }
        if (! fdset_2D[dir])
        {
          CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                      "Cannot open 2D output file '%s'", filename);
          return (-2);
        }

        /* advertise the file for downloading and write file info */
        /* FIXME: this can go when we permanently switch the the
                  new filename scheme */
        advertised_file.slice = new_filename_scheme ?
                                slicename : extensions[dir];
        advertised_file.thorn = CCTK_THORNSTRING;
        advertised_file.varname = fullname;
        advertised_file.description = "Two-dimensional slice plots";
        advertised_file.mimetype = "application/gnuplot";

        IOUtil_AdvertiseFile (GH, filename, &advertised_file);

        if (CCTK_Equals (out_fileinfo, "parameter filename") ||
            CCTK_Equals (out_fileinfo, "all"))
        {
          buffer[0] = 0;
          CCTK_ParameterFilename (sizeof (buffer), buffer);
          fprintf (fdset_2D[dir], "#Parameter file %s\n", buffer);
        }
        if (CCTK_Equals (out_fileinfo, "creation date") ||
            CCTK_Equals (out_fileinfo, "all"))
        {
          buffer[0] = 0;
          Util_CurrentDate (sizeof (buffer), buffer);
          fprintf (fdset_2D[dir], "#Created %s ", buffer);
          Util_CurrentTime (sizeof (buffer), buffer);
          fprintf (fdset_2D[dir], "%s\n", buffer);
        }
        if (CCTK_Equals (out_fileinfo, "axis labels") ||
            CCTK_Equals (out_fileinfo, "all"))
        {
          fprintf (fdset_2D[dir], "#x-label %c\n", extensions[dir][0]);
          fprintf (fdset_2D[dir], "#y-label %c\n", extensions[dir][1]);
          fprintf (fdset_2D[dir], "#z-label %s", advertised_file.varname);
          if (groupinfo.dim != 2)
          {
            if (have_coords)
            {
              fprintf (fdset_2D[dir], zlabel_fmt_string, 'x' + (maxdir-dir-1),
                       coord_lower[maxdir-dir-1] +
                       GH->cctk_delta_space[maxdir-dir-1] *
                       myGH->sp2xyz[groupinfo.dim-1][dir]);
            }
            fprintf (fdset_2D[dir], " (%ci = %d)",
                     'x' + (maxdir-dir-1), myGH->sp2xyz[groupinfo.dim-1][dir]);
          }
          fputc ('\n', fdset_2D[dir]);
        }
      }

      /* store file desriptors in database */
      StoreNamedData (&myGH->fileList_2D, alias, fdset_2D);

      free (filename);
      free (fullname);
    }
  }

  for (dir = 0; dir < maxdir; dir++)
  {
    int dir_i, dir_j;
    int directions[3];
    const int lengths[2] = {-1, -1};
    const int downsamples[2] = {1, 1};
    int hsize[2];
    CCTK_REAL *coord_data_i, *coord_data_j;
    void *data;

    /* get the directions to span the hyperslab */
    if (dir == 0)
    {
      dir_i = 0; dir_j = 1;   /* xy */
    }
    else if (dir == 1)
    {
      dir_i = 0; dir_j = 2;   /* xz */
    }
    else
    {
      dir_i = 1; dir_j = 2;   /* yz */
    }

    /* set the origin using the slice center from IOUtil */
    memset (origin, 0, GH->cctk_dim * sizeof (int));
    origin[maxdir-dir-1] = myGH->sp2xyz[groupinfo.dim - 1][dir];

    /* set the directions vector */
    memset (directions, 0, sizeof (directions));
    directions[maxdir-dir-1] = 1;

    /* get the coordinates for grid function output */
    if (have_coords)
    {
      /* get the i-coordinate slice */
      if (Hyperslab_GetHyperslab (GH, 0, coord_index[dir_i], 0, 2,
                                  origin, directions, lengths, downsamples,
                                  (void **) &coord_data_i, hsize) < 0)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Failed to extract 2D hyperslab for %c-coordinate",
                    'x' + dir_i);
        return (-3);
      }

      /* get the j-coordinate slice */
      if (Hyperslab_GetHyperslab (GH, 0, coord_index[dir_j], 0, 2,
                                  origin, directions, lengths, downsamples,
                                  (void **) &coord_data_j, hsize) < 0)
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "Failed to extract 2D hyperslab for %c-coordinate",
                    'x' + dir_j);
        free (coord_data_i);
        return (-3);
      }
    }
    else
    {
      /* grid arrays don't have coordinates assigned */
      coord_data_i = NULL;
      coord_data_j = NULL;
    }

    /* get the variable slice */
    if (Hyperslab_GetHyperslab (GH, 0, vindex, 0, 2, origin, directions,
                                lengths, downsamples, &data, hsize) < 0)
    {
      fullname = CCTK_FullName (vindex);
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "Failed to extract 2D hyperslab for variable '%s'", fullname);
      free (fullname);
      if (coord_data_i)
      {
        free (coord_data_i);
      }
      if (coord_data_j)
      {
        free (coord_data_j);
      }
      return (-3);
    }

    /* proc 0 writes */
    if (myproc == 0)
    {
      CCTK_REAL stagger_offset_i, stagger_offset_j;


      /* get the staggering offset for the coordinates */
      stagger_offset_i = CCTK_StaggerDirIndex (dir_i, groupinfo.stagtype) *
                         0.5 * GH->cctk_delta_space[dir_i];
      stagger_offset_j = CCTK_StaggerDirIndex (dir_j, groupinfo.stagtype) *
                         0.5 * GH->cctk_delta_space[dir_j];

      /* print out header */
      fprintf (fdset_2D[dir], header_fmt_string, GH->cctk_time);

      switch (groupinfo.vartype)
      {
        case CCTK_VARIABLE_CHAR:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_BYTE, int, data, hsize,
                             0, have_coords, data_fmt_string_int,fdset_2D[dir]);
          break;

        case CCTK_VARIABLE_INT:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_INT, int, data, hsize,
                             0, have_coords, data_fmt_string_int,fdset_2D[dir]);
          break;

        case CCTK_VARIABLE_REAL:
        case CCTK_VARIABLE_COMPLEX:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_REAL, double, data, hsize,
                             groupinfo.vartype == CCTK_VARIABLE_COMPLEX,
							 have_coords, data_fmt_string_real, fdset_2D[dir]);
          break;

#ifdef CCTK_INT2
        case CCTK_VARIABLE_INT2:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_INT2, int, data, hsize,
                             0, have_coords, data_fmt_string_int,fdset_2D[dir]);
          break;
#endif

#ifdef CCTK_INT4
        case CCTK_VARIABLE_INT4:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_INT4, int, data, hsize,
                             0, have_coords, data_fmt_string_int,fdset_2D[dir]);
          break;
#endif

#ifdef CCTK_INT8
        case CCTK_VARIABLE_INT8:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_INT8, int, data, hsize,
                             0, have_coords, data_fmt_string_int,fdset_2D[dir]);
          break;
#endif

#ifdef CCTK_REAL4
        case CCTK_VARIABLE_REAL4:
        case CCTK_VARIABLE_COMPLEX8:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_REAL4, double, data, hsize,
                             groupinfo.vartype == CCTK_VARIABLE_COMPLEX8,
                             have_coords, data_fmt_string_real, fdset_2D[dir]);
          break;
#endif

#ifdef CCTK_REAL8
        case CCTK_VARIABLE_REAL8:
        case CCTK_VARIABLE_COMPLEX16:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_REAL8, double, data, hsize,
                             groupinfo.vartype == CCTK_VARIABLE_COMPLEX16,
                             have_coords, data_fmt_string_real, fdset_2D[dir]);
          break;
#endif

#ifdef CCTK_REAL16
        case CCTK_VARIABLE_REAL16:
        case CCTK_VARIABLE_COMPLEX32:
          OUTPUT_TYPED_DATA (coord_data_i, coord_data_j, stagger_offset_i,
                             stagger_offset_j, CCTK_REAL16, double, data, hsize,
                             groupinfo.vartype == CCTK_VARIABLE_COMPLEX32,
                             have_coords, data_fmt_string_real, fdset_2D[dir]);
          break;
#endif

        default:
          CCTK_WARN (1, "Unsupported variable type");
          break;
      }

      /* keep the file open but flush it */
      fflush (fdset_2D[dir]);

      /* free the hyperslabs */
      free (data);
      if (coord_data_i)
      {
        free (coord_data_i);
      }
      if (coord_data_j)
      {
        free (coord_data_j);
      }

    } /* end of outputting the data by processor 0 */

  } /* end of looping through xyz directions */

  USE_CCTK_PARAMETERS;   return (0);
}
