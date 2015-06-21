#ifdef SPEC_CPU
# define THORN_IS_IOASCII
#endif /* SPEC_CPU */
/*@@
   @file      Write1D.c
   @date      March 1997
   @author    Paul Walker
   @desc
              Output one-dimensional lines in ASCII xgraph/gnuplot format.
   @enddesc

   @history
   @hauthor   Thomas Radke
   @hdate     30 May 2000
   @hdesc
              Get rid of all the PUGH stuff by using thorn Hyperslab.
   @endhdesc
   @hendhistory
   @version   $Id: Write1D.c,v 1.43 2002/01/04 11:17:28 allen Exp $
@@*/

#include <stdlib.h>
#include <math.h>      /* sqrt(3) */
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>  /* stat(2) */

#include "cctk.h"
#include "cctk_Parameters.h"
#include "Hyperslab.h"
#include "ioutil_AdvertisedFiles.h"
#include "ioutil_CheckpointRecovery.h"
#include "ioASCIIGH.h"

/* the rcs ID and its dummy function to use it */
static const char *rcsid = "$Header: /cactus/CactusBase/IOASCII/src/Write1D.c,v 1.43 2002/01/04 11:17:28 allen Exp $";
CCTK_FILEVERSION(CactusBase_IOASCII_Write1D_c)

/*#define DEBUG_IOASCII 1*/

/* macro to output a 1D line (with coordinates for GFs) as typed data */
#define OUTPUT_TYPED_DATA(grouptype, hstart, hsize, hstride, coord_data,      \
                          stagger_offset, cctk_type, c_type, is_complex,      \
                          data, format, file)                                 \
          {                                                                   \
            int h;                                                            \
            cctk_type *typed_data = (cctk_type *) data;                       \
                                                                              \
                                                                              \
            if (grouptype == CCTK_GF)                                         \
            {                                                                 \
              for (h = hstart; h < hstride * hsize; h += hstride)             \
              {                                                               \
                fprintf (file, format,                                        \
                         (double) (coord_data[h] + stagger_offset),           \
                         (c_type) typed_data[h]);                             \
                if (is_complex && hstride == 1)                               \
                {                                                             \
                  fprintf (file, out_real_format, (c_type) typed_data[h + 1]);\
                }                                                             \
                fputc ('\n', file);                                           \
              }                                                               \
            }                                                                 \
            else                                                              \
            {                                                                 \
              for (h = hstart; h < hstride * hsize; h += hstride)             \
              {                                                               \
                fprintf (file, format, (double) h, (c_type) typed_data[h]);   \
                if (is_complex && hstride == 1)                               \
                {                                                             \
                  fprintf (file, out_real_format, (c_type) typed_data[h + 1]);\
                }                                                             \
                fputc ('\n', file);                                           \
              }                                                               \
            }                                                                 \
          }


/* this is needed by some preprocessors to pass into OUTPUT_TYPED_DATA
   as a dummy macro */
#define NOTHING


/*@@
   @routine IOASCII_Write1D
   @date    March 1999
   @author  Gabrielle Allen
   @desc
            This routine does 1D line output along the orthogonals
            and the diagonal (in case of a cubed grid).
            <p>
            It writes to ASCII files suitable for gnuplot and xgraph.
            A header telling the physical time prefixes the output data.
   @enddesc
   @calls   IOUtil_RestartFromRecovery
            IOUtil_AdvertiseFile
            Hyperslab_GetHyperslab

   @var     GH
   @vdesc   Pointer to CCTK GH
   @vtype   const cGH *
   @vio     in
   @endvar
   @var     vindex
   @vdesc   global index of variable to output
   @vtype   int
   @vio     in
   @endvar
   @var     alias
   @vdesc   alias name (used for creating the output filename)
   @vtype   const char *
   @vio     in
   @endvar

   @returntype int
   @returndesc
                0 for success, or<BR>
               -1 if variable has no storage assigned
   @endreturndesc
@@*/
int IOASCII_Write1D (const cGH *GH, int vindex, const char *alias)
{
  asciiioGH *myGH;                  /* IOASCII extension handle */
  int Do_it[4];                     /* flags indicating actual work */
  int coord_index[3];               /* x,y,z coordinate variable indices */
  int myproc;                       /* identify processor */
  int i, dir;                       /* Loopers */
  int groupindex;                   /* variable's group index */
  int have_coords;                  /* boolean for existance of coordinates */
  char coord_system[20];            /* name of the coordinate system */
  char slicename[20];               /* name of current output slice */
  cGroup group_static_data;         /* variable's group static data */
  cGroupDynamicData group_dynamic_data;/* variable's group dynamic data */
  char *fullname;                   /* variable's full name */
  char header_fmt_string[18];       /* header format string */
  char ylabel1_fmt_string[13];      /* y-label format string */
  char ylabel2_fmt_string[13];      /* y-label format string */
  char time_fmt_string[30];         /* time format string */
  char data_fmt_string_int[30];     /* data format string for int types */
  char data_fmt_string_real[30];    /* data format string for float types */
  char out_real_format[30];         /* data format string for a real type */
  const char *file_extension;       /* filename extension */
  char comment_char;                /* character starting a comment */
  FILE *file[8];                    /* fds for x,y,z,d output (also COMPLEX)*/
  CCTK_REAL coord_lower[3], offset;
  int stride, num_files;
  int upper, lower;
  struct stat fileinfo;
  const char *openmode;
  static char *extensions[] = {"xl", "yl", "zl", "dl"};
  char *filename, *type_extension, buffer[128];
  ioAdvertisedFileDesc advertised_file;
  DECLARE_CCTK_PARAMETERS


  /* get the variable's group index and its full name */
  groupindex = CCTK_GroupIndexFromVarI (vindex);
  fullname = CCTK_FullName (vindex);

  /* check if variable has storage assigned */
  if (! CCTK_QueryGroupStorageI (GH, groupindex))
  {
    CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOASCII_Write1D: No IOASCII_1D output for '%s' (no storage)",
                fullname);
    free (fullname);
    return (-1);
  }

  /* get the handle for IOASCII extensions */
  myGH = (asciiioGH *) CCTK_GHExtension (GH, "IOASCII");

  /* get the variable's group information */
  CCTK_GroupData (groupindex, &group_static_data);
  CCTK_GroupDynamicData (GH, groupindex, &group_dynamic_data);

  /* see what slices should be output */
  Do_it[0] = out1D_x && group_static_data.dim >= 1;
  Do_it[1] = out1D_y && group_static_data.dim >= 2;
  Do_it[2] = out1D_z && group_static_data.dim >= 3;
  /* diagonal slice is done only if variable is non-staggered and 3D */
  Do_it[3] = out1D_d &&
             group_static_data.dim == 3 &&
             group_static_data.stagtype == 0;
  if (out1D_d && ! Do_it[3] && myGH->out1D_last[vindex] < 0)
  {
    CCTK_VWarn (3, __LINE__, __FILE__, CCTK_THORNSTRING,
                "IOASCII_Write1D: No IOASCII_1D diagonal output for '%s' "
                "(only implemented for non-staggered 3D variables)",
                fullname);
  }

  /* return if nothing to do */
  if (! (Do_it[0] || Do_it[1] || Do_it[2] || Do_it[3]))
  {
    free (fullname);
    return (0);
  }

  /* DEPRICATED IN BETA12 */
  if (CCTK_ParameterQueryTimesSet ("out_style", CCTK_THORNSTRING) > 0)
  {
    static int user_was_warned = 0;


    if (! user_was_warned)
    {
      CCTK_WARN (1, "IOASCII_Write1D: parameter 'IOASCII::out_style' is "
                    "deprecated in BETA12, please use 'IOASCII::out1D_style' "
                    "instead");
      user_was_warned = 1;
    }
    if (CCTK_Equals (out_style, "gnuplot"))
    {
      out1D_style = "gnuplot f(x)";
    }
    else
    {
      out1D_style = "xgraph";
    }
  }

  /* set header and data format strings */
  if (CCTK_Equals (out1D_style, "xgraph"))
  {
    comment_char = '"';
    file_extension = ".xg";
    sprintf (header_fmt_string, "\n\n%cTime = %%%s\n", comment_char,out_format);
  }
  else
  {
    comment_char = '#';
    file_extension = ".asc";
    sprintf (header_fmt_string, "\n%cTime = %%%s\n", comment_char, out_format);
  }
  sprintf (ylabel1_fmt_string, " (%%c = %%%s", out_format);
  sprintf (ylabel2_fmt_string, ", %%c = %%%s", out_format);
  sprintf (out_real_format, "\t\t%%%s", out_format);

  /* check whether to include physical time as separate column in the output */
  i = 0;
  if (CCTK_Equals (out1D_style, "gnuplot f(t,x)"))
  {
    sprintf (time_fmt_string,  "%%%s\t\t", out_format);
    sprintf (data_fmt_string_int,  time_fmt_string, (double) GH->cctk_time);
    sprintf (data_fmt_string_real, time_fmt_string, (double) GH->cctk_time);
    i = strlen (data_fmt_string_int);
  }
  sprintf (data_fmt_string_int + i,  "%%%s\t\t%%d", out_format);
  sprintf (data_fmt_string_real + i, "%%%s\t\t%%%s", out_format, out_format);

#ifdef DEBUG_IOASCII
  printf ("\nIn IOASCII Write1D\n------------------\n");
  printf ("  Variable index is %d\n",vindex);
  printf ("  Alias is -%s-\n",alias);
  fflush (stdout);
#endif

  /* get the coordinate indices for CCTK_GF variables */
  if (group_static_data.grouptype == CCTK_GF)
  {
    sprintf (coord_system, "cart%dd", group_static_data.dim);
    have_coords = 1;
    for (i = 0; i < group_static_data.dim && i < 3; i++)
    {
      CCTK_CoordRange (GH, &coord_lower[i], &offset, i + 1, NULL, coord_system);
      coord_index[i] = CCTK_CoordIndex (i + 1, NULL, coord_system);
      have_coords &= coord_index[i] >= 0;
    }

    if (! have_coords)
    {
      CCTK_VWarn (8, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_Write1D: No coordinate ranges found for '%s'",
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

  /* get the stride into the data and the (maximum) number of files to write */
  stride = group_static_data.vartype == CCTK_VARIABLE_COMPLEX &&
           CCTK_Equals (out1D_style, "xgraph") ? 2 : 1;
  num_files = stride * 4;

  /* Processor 0 opens the files with the appropriate name */
  if (myproc == 0)
  {
    /* 20 extra characters should be enough for '/',
       the type extension, the file extension, and the trailing '\0' */
    filename = (char *) malloc (strlen (myGH->outdir1D) + strlen (alias) +
                                sizeof (slicename) + 20);

    for (i = 0; i < num_files; i++)
    {
      dir = i % 4;

      /* skip empty slices */
      if (! Do_it[dir])
      {
        continue;
      }

      /* add suffix for complex type variables in xgraph output */
      if (group_static_data.vartype == CCTK_VARIABLE_COMPLEX && num_files == 8)
      {
        if (new_filename_scheme)
        {
          type_extension = i < 4 ? "re_" : "im_";
        }
        else
        {
          type_extension = i < 4 ? "_re" : "_im";
        }
      }
      else
      {
        type_extension = "";
      }

      /* get the indices into spxyz[] */
      lower = (dir + 1) % 3;
      upper = (dir + 2) % 3;
      if (upper < lower)
      {
        upper = lower;
        lower = 0;
      }

      /* FIXME: this can go after the old filename scheme has gone */
      if (new_filename_scheme)
      {
        if ((i + 1) % 4)
        {
          if (group_static_data.dim == 1)
          {
            sprintf (slicename, "%s1D", type_extension);
          }
          else if (group_static_data.dim == 2)
          {
            /* give the slice origin as range [1 .. n] */
            sprintf (slicename, "%s%c_[%d]", type_extension, 'x' + dir,
                     myGH->spxyz[group_static_data.dim-1][dir][lower]);
          }
          else
          {
            /* give the slice origin as range [1 .. n] */
            sprintf (slicename, "%s%c_[%d][%d]", type_extension, 'x' + dir,
                     myGH->spxyz[group_static_data.dim-1][dir][lower],
                     myGH->spxyz[group_static_data.dim-1][dir][upper]);
          }
        }
        else
        {
          sprintf (slicename, "%s%dD_diagonal", type_extension,
                   group_static_data.dim);
        }

        /* skip the pathname if output goes into current directory */
        if (strcmp (myGH->outdir1D, "."))
        {
          sprintf (filename, "%s/%s_%s%s", myGH->outdir1D, alias,
                   slicename, file_extension);
        }
        else
        {
          sprintf (filename, "%s_%s%s", alias, slicename, file_extension);
        }
      }
      else
      {
        /* skip the pathname if output goes into current directory */
        if (strcmp (myGH->outdir1D, "."))
        {
          sprintf (filename, "%s/%s%s.%s", myGH->outdir1D, alias,
                   type_extension, extensions[dir]);
        }
        else
        {
          sprintf (filename, "%s%s.%s", alias, type_extension, extensions[dir]);
        }
      }

      /* see if output file was already created */
      if (GetNamedData (myGH->filenameList1D, filename) == NULL)
      {
        /* if restart from recovery, all existing files are opened
           in append mode */
        if (IOUtil_RestartFromRecovery (GH))
        {
          openmode = stat (filename, &fileinfo) == 0 ? "a" : "w";
        }
        else
        {
          openmode = "w";
        }

        /* just store a non-NULL pointer in database */
        StoreNamedData (&myGH->filenameList1D, filename, (void *) 1);
      }
      else
      {
        openmode = "a";
      }

      if (! (file[i] = fopen (filename, openmode)))
      {
        CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                    "IOASCII_Write1D: Cannot open 1D output file '%s'",
                    filename);
      }
      else if (*openmode == 'w')
      {
        /* advertise new files for downloading and add file info */
        /* FIXME: this can go after the old filename scheme has gone */
        advertised_file.slice = new_filename_scheme ? slicename:extensions[dir];
        advertised_file.thorn = CCTK_THORNSTRING;
        advertised_file.varname = fullname;
        advertised_file.description = "One-dimensional line plots";
        advertised_file.mimetype = comment_char == '#' ?
                             "application/gnuplot" : "application/x-graph";

        IOUtil_AdvertiseFile (GH, filename, &advertised_file);

        /* add some file information to the output file */
        if (CCTK_Equals (out_fileinfo, "parameter filename") ||
            CCTK_Equals (out_fileinfo, "all"))
        {
          buffer[0] = 0;
          CCTK_ParameterFilename (sizeof (buffer), buffer);
          fprintf (file[i], "%cParameter file %s\n", comment_char, buffer);
        }
        if (CCTK_Equals (out_fileinfo, "creation date") ||
            CCTK_Equals (out_fileinfo, "all"))
        {
          buffer[0] = 0;
          Util_CurrentDate (sizeof (buffer), buffer);
          fprintf (file[i], "%cCreated %s ", comment_char, buffer);
          Util_CurrentTime (sizeof (buffer), buffer);
          fprintf (file[i], "%s\n", buffer);
        }
        if (CCTK_Equals (out_fileinfo, "axis labels") ||
            CCTK_Equals (out_fileinfo, "all"))
        {
          if (dir < 3)
          {
            fprintf (file[i], "%cx-label %c\n", comment_char, 'x' + dir);
          }
          else
          {
            fprintf (file[i], "%cx-label diagonal\n", comment_char);
          }
          fprintf (file[i], "%cy-label %s", comment_char, fullname);
          if (dir < 3)
          {
            if (group_static_data.dim > 1)
            {
              /* output the physical coordinates of the 1D line */
              if (have_coords)
              {
                fprintf (file[i], ylabel1_fmt_string, 'x' + lower,
                         coord_lower[lower] + GH->cctk_delta_space[lower] *
                         myGH->spxyz[group_static_data.dim-1][dir][lower]);
                if (group_static_data.dim > 2)
                {
                  fprintf (file[i], ylabel2_fmt_string, 'x' + upper,
                           coord_lower[upper] + GH->cctk_delta_space[upper] *
                           myGH->spxyz[group_static_data.dim-1][dir][upper]);
                }
                fprintf (file[i], "),");
              }
              /* output the index coordinates of the 1D line */
              fprintf (file[i], " (%ci = %d", 'x' + lower,
                       myGH->spxyz[group_static_data.dim-1][dir][lower]);
              if (group_static_data.dim > 2)
              {
                fprintf (file[i], ", %ci = %d", 'x' + upper,
                         myGH->spxyz[group_static_data.dim-1][dir][upper]);
              }
              fprintf (file[i], ") \n");
            }
          }
        }
      }
    }
    free (filename);
  }

  /* OK so actually do the I/O in each direction */
  for (dir = 0; dir < 4; dir++)
  {
    int length = -1;
    int gsh[3];
    const int downsample = 1;
    const int *origin;
    const int zero_point[3] = {0, 0, 0};
    int directions[3];
    int hsize;
    void *data;
    CCTK_REAL *coord_data;


    /* skip empty slices */
    if (! Do_it[dir])
    {
      continue;
    }

    /* need to pass the extent for diagonals */
    if (dir < 3)
    {
      length = -1;
    }
    else
    {
      CCTK_GroupgshVI (GH, 3, gsh, vindex);
      length = group_dynamic_data.gsh[0];
      if (length > group_dynamic_data.gsh[1])
      {
        length = group_dynamic_data.gsh[1];
      }
      if (length > group_dynamic_data.gsh[2])
      {
        length = group_dynamic_data.gsh[2];
      }
    }

    if (group_static_data.grouptype == CCTK_GF)
    {
      /* for GFs: get the coordinate's 1D data (in xyz direction only) */
      if (dir < 3)
      {
        /* set the origin of the line */
        origin = myGH->spxyz[group_static_data.dim-1][dir];

        /* set the direction vector for the 1D line */
        memset (directions, 0, sizeof (directions));
        directions[dir] = 1;

        if (have_coords)
        {
          if (Hyperslab_GetHyperslab (GH, 0, coord_index[dir], 0, 1,
                                      origin, directions, &length, &downsample,
                                      (void **) &coord_data, &hsize) < 0)
          {
            CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                        "IOASCII_Write1D: Failed to extract hyperslab for"
                        "%c-coordinate", 'x' + dir);
          }
        }
        else
        {
          coord_data = NULL;
        }
      }
      else
      {
        /* set the origin of the line */
        origin = zero_point;

        /* set the direction vector for the diagonal 1D line */
        for (i = 0; i < 3; i++)
        {
          directions[i] = 1;
        }

        /* coordinates are calculated by output processor */
        coord_data = NULL;
      }
    }
    else
    {
      /* set the origin of the line */
      /* FIXME: Should we define some slice center for arrays too ? */
      origin = zero_point;

      /* set the direction vector for the 1D line */
      for (i = 0; i < 3; i++)
      {
        directions[i] = (dir == i || dir == 3) ? 1 : 0;
      }

      /* no coordinates are needed for arrays */
      coord_data = NULL;
    }

    /* get the variable's 1D data */
    if (Hyperslab_GetHyperslab (GH, 0, vindex, 0, 1, origin,
                                directions, &length, &downsample,
                                &data, &hsize) < 0)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
                  "IOASCII_Write1D: Failed to extract hyperslab for "
                  "variable '%s'", fullname);
      if (coord_data)
      {
        free (coord_data);
      }
      continue;
    }

    /* And write it out on processor 0 */
    if (myproc == 0 && file[dir] != NULL)
    {
      if (group_static_data.grouptype == CCTK_GF)
      {
        if (dir < 3)
        {
          /* get the staggering offset for the xyz coordinates */
          offset = CCTK_StaggerDirIndex (dir, group_static_data.stagtype) *
                                         0.5 * GH->cctk_delta_space[dir];
          if (! have_coords)
          {
            coord_data = (CCTK_REAL *) malloc (hsize * sizeof (CCTK_REAL));
            for (i = 0; i < hsize; i++)
            {
              coord_data[i] = i;
            }
          }
        }
        else
        {
          /* get the diagonal coordinates */
          offset = have_coords ? GH->cctk_delta_space[0] * sqrt (3) : sqrt(3);
          coord_data = (CCTK_REAL *) malloc (hsize * sizeof (CCTK_REAL));
          for (i = 0; i < hsize; i++)
          {
            coord_data[i] = i * offset;
          }
          if (have_coords)
          {
            offset = coord_lower[0] * sqrt (3);
          }
          else
          {
            offset = 0.0;
          }
        }
      }

      /* print out header */
      fprintf (file[dir], header_fmt_string, GH->cctk_time);

      /* and then loop through the line points */
      switch (group_static_data.vartype)
      {
        case CCTK_VARIABLE_CHAR:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_BYTE, int, 0, data,
                             data_fmt_string_int, file[dir]);
          break;

        case CCTK_VARIABLE_INT:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_INT, int, 0, data,
                             data_fmt_string_int, file[dir]);
          break;

        case CCTK_VARIABLE_REAL:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_REAL, double, 0, data,
                             data_fmt_string_real, file[dir]);
          break;

        case CCTK_VARIABLE_COMPLEX:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, stride,
                             coord_data, offset, CCTK_REAL, double, 1, data,
                             data_fmt_string_real, file[dir]);
          if (stride == 2)
          {
            /* print out header */
            fprintf (file[dir + 4], header_fmt_string, GH->cctk_time);
            OUTPUT_TYPED_DATA (group_static_data.grouptype, 1, hsize, stride,
                               coord_data, offset, CCTK_REAL, double, 1, data,
                               data_fmt_string_real, file[dir + 4]);
          }
          break;

#ifdef CCTK_INT2
        case CCTK_VARIABLE_INT2:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_INT2, int, 0, data,
                             data_fmt_string_int, file[dir]);
          break;
#endif

#ifdef CCTK_INT4
        case CCTK_VARIABLE_INT4:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_INT4, int, 0, data,
                             data_fmt_string_int, file[dir]);
          break;
#endif

#ifdef CCTK_INT8
        case CCTK_VARIABLE_INT8:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_INT8, int, 0, data,
                             data_fmt_string_int, file[dir]);
          break;
#endif

#ifdef CCTK_REAL4
        case CCTK_VARIABLE_REAL4:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_REAL4, double, 0, data,
                             data_fmt_string_real, file[dir]);
          break;

        case CCTK_VARIABLE_COMPLEX8:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, stride,
                             coord_data, offset, CCTK_REAL4, double, 1, data,
                             data_fmt_string_real, file[dir]);
          if (stride == 2)
          {
            /* print out header */
            fprintf (file[dir + 4], header_fmt_string, GH->cctk_time);
            OUTPUT_TYPED_DATA (group_static_data.grouptype, 1, hsize, stride,
                               coord_data, offset, CCTK_REAL4, double, 1, data,
                               data_fmt_string_real, file[dir + 4]);
          }
          break;
#endif

#ifdef CCTK_REAL8
        case CCTK_VARIABLE_REAL8:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_REAL8, double, 0, data,
                             data_fmt_string_real, file[dir]);
          break;

        case CCTK_VARIABLE_COMPLEX16:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, stride,
                             coord_data, offset, CCTK_REAL8, double, 1, data,
                             data_fmt_string_real, file[dir]);
          if (stride == 2)
          {
            /* print out header */
            fprintf (file[dir + 4], header_fmt_string, GH->cctk_time);
            OUTPUT_TYPED_DATA (group_static_data.grouptype, 1, hsize, stride,
                               coord_data, offset, CCTK_REAL8, double, 1, data,
                               data_fmt_string_real, file[dir + 4]);
          }
          break;
#endif

#ifdef CCTK_REAL16
        case CCTK_VARIABLE_REAL16:
          OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, 1,
                             coord_data, offset, CCTK_REAL16, double, 0, data,
                             data_fmt_string_real, file[dir]);
          break;

        case CCTK_VARIABLE_COMPLEX32:
	  OUTPUT_TYPED_DATA (group_static_data.grouptype, 0, hsize, stride,
                             coord_data, offset, CCTK_REAL16, double, 1, data,
                             data_fmt_string_real, file[dir]);
          if (stride == 2)
          {
            /* print out header */
            fprintf (file[dir + 4], header_fmt_string, GH->cctk_time);
            OUTPUT_TYPED_DATA (group_static_data.grouptype, 1, hsize, stride,
                               coord_data, offset, CCTK_REAL16, double, 1, data,
                               data_fmt_string_real, file[dir + 4]);
          }
          break;
#endif

        default:
          CCTK_WARN (1, "Unsupported variable type");
          break;
      }

      /* close the output file(s) */
      fclose (file[dir]);
      if (num_files == 8)
      {
        fclose (file[dir + 4]);
      }

      /* clean up */
      free (data);
      if (coord_data)
      {
        free (coord_data);
      }
    }

  } /* end of loop through all directions */

  /* free allocated resources */
  free (fullname);

  USE_CCTK_PARAMETERS;   return (0);
}
