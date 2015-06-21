#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      ParseFile.c
   @date      Tue Jan 12 15:58:31 1999
   @author    Tom Goodale
   @desc
              Routines to read in a parameter file and pass the resulting data
              to a user-supplied subroutine.
              Currently taken from the old cactus ones and slightly modifed.
   @enddesc
   @version   $Id: ParseFile.c,v 1.16 2002/01/02 12:24:42 tradke Exp $
 @@*/

/*#define DEBUG*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "cctk_Flesh.h"
#include "cctk_CommandLine.h"
#include "util_String.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

static const char *rcsid = "$Header: /cactus/Cactus/src/util/ParseFile.c,v 1.16 2002/01/02 12:24:42 tradke Exp $";

CCTK_FILEVERSION(util_ParseFile_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static void CheckBuf(int, int);
static void removeSpaces(char *stripMe);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

int ParseFile(FILE *ifp,
              int (*set_function)(const char *, const char *, int),
              tFleshConfig *ConfigData);

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

#ifndef WIN32
#define BOLDON   "\033[1m"
#define BOLDOFF  "\033[0m"
#else
#define BOLDON   ""
#define BOLDOFF  ""
#endif

/* parse buffer size */
#define BUF_SZ   (8 * 1024)

/* line number */
static int lineno = 1;

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/

 /*@@
   @routine ParseFile
   @author Paul Walker
   @desc
   This routine actually parses the parameter file. The
   syntax we allow is
   <ul>
     <li>a = b
         <li>a,b,c = d,e,f
         <li># rest of the line is ignored
         <li>x = "string value"
   </ul>
   So it is easy to parse
   <p>
   We go through the file looking for stuff and then set
   it in the global database using calls to the passed in set_function.
   @enddesc
   @history
   @hdate Tue Jan 12 16:41:36 1999 @hauthor Tom Goodale
   @hdesc Moved to CCTK.
          Changed to pass data to arbitrary function.
          Changed to take a file descriptor rather than a filename.
   @endhistory
   @var     ifp
   @vdesc   The filestream to parse
   @vtype   FILE *
   @vio     in
   @vcomment

   @endvar
   @var     set_function
   @vdesc   The function to call to set the value of a parameter
   @vtype   int (*)(const char *, const char *)
   @vio     in
   @vcomment

   @endvar
   @var     ConfigData
   @vdesc   Flesh configuration data
   @vtype   tFleshConfig *
   @vio     in
   @vcomment

   @endvar

   @returntype int
   @returndesc
   0 - success
   @endreturndesc
@@*/
int ParseFile(FILE *ifp,
              int (*set_function)(const char *, const char *, int),
              tFleshConfig *ConfigData)
{
  /* Buffers for parsing from the file */
  char *tokens, *value;
  char *subtoken, *subvalue;
  /* Positions in the buffers */
  int ntokens;
  /* Status flags */
  int intoken, inval;
  /* Current char. Have to make it an int so we can compare with
     EOF. See man 3 fgetc
   */
  int c;
  int num_errors; /* number of errors in file parsing */

  num_errors = 0;

  /* avoid compiler warning about unused parameter */
  ConfigData = ConfigData;

  /* allocate parse buffers */
  tokens = (char *) malloc (4 * BUF_SZ);
  value    = tokens + 1*BUF_SZ;
  subtoken = tokens + 2*BUF_SZ;
  subvalue = tokens + 3*BUF_SZ;

  intoken = 0; inval = 0;

  while ((c=fgetc(ifp)) != EOF)
  {
#ifdef DEBUG
    printf("%c",c);
#endif
    /* Main Loop */
    while (c == '#' || c == '!' )
    {
      /* Comment line.  So forget rest of line */
      while ((c=fgetc(ifp)) != '\n' && c != EOF)
      {
#ifdef DEBUG
        printf("%c",c);
#endif
      }
      if (c == '\n')
      {
        lineno++;
      }
      c = fgetc(ifp);
#ifdef DEBUG
      printf("%c",c);
#endif
    }

    /* End of line */
    if (c == '\n')
    {
      if(intoken)
      {
        fprintf(stderr, "Parse error at line %d.  No value supplied.\n", lineno);
	num_errors++;
        intoken = 0;
      }

#ifdef DEBUG
      printf ("LINE %d\n",lineno);
#endif
      lineno ++;

    }

    /* Token character */
    if (intoken && c != '=')
    {
      tokens[intoken++] = c;
      CheckBuf(intoken,lineno);
    }


    /* Start of a new token */
    if (c != ' ' && c != '\t' && c != '\n' && !inval && !intoken)
    {
      intoken = 0;
      tokens[intoken++] = c;
    }

    /* End of a token signified by an = */
    if (c == '=')
    {
      if (intoken)
      {
        unsigned int ll;
        tokens[intoken] = '\0';  /* Very important! */
        intoken = 0;
        inval = 0;
        removeSpaces(tokens);
        ntokens = 1;
        for (ll=0;ll < strlen(tokens); ll++)
          if (tokens[ll] == ',') ntokens++;
#ifdef DEBUG
        printf ("\nNew token! >>%s<<\n",tokens);
        printf ("%d token elements\n",ntokens);
#endif

        /* Scan ahead to the beginning of the value
         * and check if the value is a string or not.
         * This parser DOES strip quotes off of the strings.
         */
        while ((c = fgetc(ifp)) == ' ' || c == '\n' || c == '\t')
        {
#ifdef DEBUG
          printf("%c",c);
#endif
          if (c == '\n')
          {
#ifdef DEBUG
            printf ("LINE %d\n",lineno);
#endif
            lineno++;
          }
        }

        if (c == '"')
        {
          /* Just handle the thing. */
          int p = 0;
          if (ntokens > 1)
          {
            fprintf (stderr, "%s%s%s\n",
                     "WARNING: Multiple string ",
                     "tokens not supported for ",
                     tokens);
            fprintf(stderr, "This is a fatal error");
            /* deallocate parse buffers */
            free (tokens);
            return 1;
          }
          while ((c = fgetc(ifp)) != '"')
          {
#ifdef DEBUG
            printf("%c",c);
#endif
            /* Make an important decision NOT to include
             * line feeds in the string parameters
             */
            if (c != '\n') value[p++] = c;
            if (c == '\n')
            {
              printf ("%sWarning:%s Quoted string contains newline for token %s\n",
                      BOLDON, BOLDOFF, tokens);
              printf ("This could indicated a parameter file error or missing quote\n");
#ifdef DEBUG
              printf ("LINE %d\n",lineno);
#endif
              lineno++;
            }
            CheckBuf(p,lineno);
          }
          value[p] = '\0';
#ifdef DEBUG
          printf ("\nString %s -> %s\n",
                  tokens,value);
#endif
          set_function(tokens,value, lineno);
        }
        else if (c == '$')
        {
          /* We got a define */
          /* FIXME: Assume it is a parameter file for now */
          char filename[500];
          char *dir;
          char *file;
          int lpar;

          CCTK_ParameterFilename(500,filename);
          Util_SplitFilename(&dir,&file,filename);

          lpar=((strlen(file)-3)*sizeof(char));

          /* ignore everything else on the line */
          while (!(c==' ' || c=='\t' || c == '\n' || c == EOF))
          {
            c = fgetc(ifp);
#ifdef DEBUG
            printf("%c",c);
#endif
          }
          strncpy(value,file,lpar);
          free(dir);
          free(file);
          value[strlen(value)-1] = '\0';
          set_function(tokens,value,lineno);
        }
        else
        {

          int p = 0;
          value[p++] = c;
          if (ntokens == 1)
          {
            /* Simple case. We have an int
               or a double which contain no
               spaces! */
            c = fgetc(ifp);
#ifdef DEBUG
            printf("%c",c);
#endif
            while (!(c==' ' || c=='\t' || c == '\n' || c == EOF))
            {
              value[p++] = c;
              CheckBuf(p,lineno);
              c = fgetc(ifp);
#ifdef DEBUG
              printf("%c",c);
#endif
            }
            value[p] = '\0';
#ifdef DEBUG
            printf ("Parsed %d characters\n", p);
            printf("\nFloat/Int: %s -> %s\n", tokens,value);
#endif
            set_function(tokens,value,lineno);
            if (c=='\n')
            {
#ifdef DEBUG
              printf ("LINE %d\n",lineno);
#endif
              lineno++;
            }
          }
          else
          {
            /* Harder case of multiple tokens */
            int ncommas = 0;
            int pp=0, i;
            int pt, pv;

            value[pp++] = c;
            /* OK, since we only have numbers in the
               old input stream, we can go along getting
               ntokens-1 commas, stripping spaces, and
               make a nice little string.
               */
            c = fgetc(ifp);
#ifdef DEBUG
            printf("%c",c);
#endif
            while (ncommas < ntokens-1 && c != EOF)
            {
              if (!(c == ' ' || c == '\t' || c == '\n'))
              {
                value[pp++] = c;
                CheckBuf(pp,lineno);
	      }
              if (c == ',') ncommas ++;
              c = fgetc(ifp);
#ifdef DEBUG
              printf("%c",c);
#endif
            }
            if (c == ' ' || c == '\t')
            {
              /* Great now strip out the spaces */
              while((c = fgetc(ifp)) == ' ' || c=='\t' || c == '\n')
              {
#ifdef DEBUG
                printf("%c",c);
#endif
                if (c=='\n')
                {
#ifdef DEBUG
                  printf ("LINE %d\n",lineno);
#endif
                  lineno++;
                }
              }
            }

            /* And tack the rest on */
            value[pp++] = c;
            CheckBuf(p,lineno);

            c = fgetc(ifp);
#ifdef DEBUG
            printf("%c",c);
#endif
            while (c != ' ' && c != '\t' && c != '\n' && c != EOF)
            {
              value[pp++] = c;
              CheckBuf(pp,lineno);
              c = fgetc(ifp);
#ifdef DEBUG
              printf("%c",c);
#endif
            }
            value[pp] = '\0';
#ifdef DEBUG
            printf("Comma list: %s -> %s\n",
                   tokens,value);
#endif
            /* So parse out the tokens */
            pt = 0;
            pv = 0;
            for (i=0;i<ncommas;i++)
            {
              pp = 0;
              while (tokens[pt] != ',')
              {
                subtoken[pp++] = tokens[pt++];
                CheckBuf(p,lineno);
              }
              subtoken[pp] = '\0';
              pp = 0;
              while (value[pv] != ',')
              {
                subvalue[pp++] = value[pv++];
                CheckBuf(pp,lineno);
              }
              subvalue[pp] = '\0';

              set_function(subtoken,subvalue,lineno);
#ifdef DEBUG
              printf("Setting sub-token %s -> %s\n",
                     subtoken, subvalue);
#endif
              /* Now remember we are sitting on a comma
               * in both our input strings, so bump by one
               */
              pv ++; pt ++;
            }
            /* And OK, so now we have one parameter left
             * so lets handle that
             */
            pp = 0;
            while (tokens[pt] != '\0')
            {
              subtoken[pp++] = tokens[pt++];
              CheckBuf(pp,lineno);
            }
            subtoken[pp] = '\0';
            pp = 0;
            while (value[pv] != '\0')
            {
              subvalue[pp++] = value[pv++];
              CheckBuf(pp,lineno);
            }
            subvalue[pp] = '\0';

            set_function(subtoken,subvalue,lineno);
          }
        }
      }
      else
      {
        fprintf (stderr, "Parser failed at = on line %d\n",
                 lineno);
      }
    }
  }

  /* deallocate parse buffers */
  free (tokens);

  return num_errors;
}

/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine CheckBuf
   @author Paul Walker
   @desc
   A simple description and warning message in case of
   a fixed parse buffer overflow.
   @enddesc
   @calls
   @calledby
   @history

   @endhistory
   @var     p
   @vdesc   buffer location
   @vtype   int
   @vio     in
   @vcomment

   @endvar
   @var     l
   @vdesc   Line number
   @vtype   int
   @vio     in
   @vcomment

   @endvar

@@*/

static void CheckBuf(int p, int l)
{
  if (p >= BUF_SZ)
  {
    fprintf(stderr,"WARNING: Parser buffer overflow on line %d\n",
            l);
    fprintf(stderr,"This indicates either an incorrect parm file or\n");
    fprintf(stderr,"the need to recompile " __FILE__ " with a bigger\n");
    fprintf(stderr,"BUF_SZ parm.\n");

    assert(0);
    exit(1);
  }
}


 /*@@
   @routine removeSpaces
   @author Paul Walker
   @desc
   removes the spaces from a char * <b>in place</b>. Beware
   that this function will change the input value and if you
   want to keep a copy you have to do so yourself!
   @enddesc
   @calls
   @calledby
   @history

   @endvar
   @var     stripMe
   @vdesc   String to strip
   @vtype   char *
   @vio     inout
   @vcomment

   @endvar

@@*/
static void removeSpaces(char *stripMe)
{
  char *s;
  unsigned int i,j;
  s = (char *)malloc((strlen(stripMe)+2)*sizeof(char));

  if(s)
  {
    strcpy(s,stripMe);
    for (i=0,j=0;i<strlen(s);i++)
    {
      if (s[i] != ' ' && s[i] != '\t' && s[i] != '\n')
      {
        stripMe[j++] = s[i];
      }
      stripMe[j] = '\0';
    }
  }

  free(s);
}


/* #define TEST_ParseFile */

#ifdef TEST_ParseFile

int parameter_printer(const char *param, const char *val)
{
  printf("Parameter %s has value %s\n", param, val);

  return 0;
}

int main(int argc, char *argv[])
{
  int retval;
  FILE *parameter_file;

  if(argc > 1)
  {
    parameter_file = fopen(argv[1], "r");
    if(parameter_file)
    {
      ParseFile(parameter_file, parameter_printer);
      fclose(parameter_file);
      retval = 0;
    }
    else
    {
      retval=2;
    }
  }
  else
  {
    printf("Usage: %s <filename>\n", argv[0]);
    retval = 1;
  };

  return 0;
}

#endif
