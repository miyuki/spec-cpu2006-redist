#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
/* Table.c -- implementation for key-value tables */
/* $Header: /cactus/Cactus/src/util/Table.c,v 1.6 2002/01/21 14:31:22 rideout Exp $ */

/*@@
 @file          Table.c
 @seeheader     util_Table.h
 @version       $Id: Table.c,v 1.6 2002/01/21 14:31:22 rideout Exp $
 @date          Wed Oct 31 16:17:45 MET 2001
 @author        Jonathan Thornburg <jthorn@aei.mpg.de>
 @desc
        This program implements the key-value table API defined
        in util_Table.h and in the Cactus User's Guide.  A slightly
        earlier version of this is documented in
           http://www.cactuscode.org/Development/Specs/KeyValueLookup.txt
 @enddesc
 @@*/

/*
 * ***** table of contents for this file *****
 *
 * Growable Array Data Structures
 * Table Data Structures
 * Iterator Data Structures
 * Misc Macros for This File
 * Prototypes for Functions Private to This File
 * Main Table API
 *   Util_TableCreate
 *   Util_TableDestroy
 *   Util_TableQueryFlags
 *   Util_TableQueryNKeys
 *   Util_TableQueryMaxKeyLength
 *   Util_TableQueryValueInfo
 *   Util_TableDeleteKey
 *   Util_TableCreateFromString
 *   Util_TableSetFromString
 *   Util_TableSetString
 *   Util_TableGetString
 *   Util_TableSet*
 *   Util_TableSet*Array
 *   Util_TableGet*
 *   Util_TableGet*Array
 * Table Iterator API
 *   Util_TableItCreate
 *   Util_TableItDestroy
 *   Util_TableItQueryIsNull
 *   Util_TableItQueryIsNonNull
 *   Util_TableItQueryTableHandle
 *   Util_TableItQueryKeyValueInfo
 *   Util_TableItAdvance
 *   Util_TableItResetToStart
 *   Util_TableItSetToNull
 *   Util_TableItSetToKey
 * Internal Support Functions
 *   internal_set
 *   internal_get
 *   get_table_header_ptr
 *   delete_key
 *   free_table_entry
 *   bad_key
 *   find_table_entry
 *   get_iterator_ptr
 *   grow_pointer_array
#ifdef UTIL_TABLE_TEST
 * Table and Iterator Dump Routines
 *   print_all_tables
 *   print_table
 *   print_all_iterators
 * Standalone Test Driver
 *   // low-level routines
 *   CHECK_SET_GET_{INT,REAL,COMPLEX}
 *   CHECK_SET_GET_{INT,REAL,COMPLEX}_ARRAY
 *   check_table_contents(int handle)
 *   // higher-level routines
 *   main
 *   test_nonexistent_tables
 *   test_table_create_destroy
 *   test_set_get
 *   test_set_get_array
 *   test_iterators
 *   test_delete_key
 *   test_set_create_from_string
 *   test_set_get_string
 *   test_table_contents
#endif
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/* FIXME: C99 defines <stdbool.h>, we should include that or a fake version */
typedef enum { false = 0, true = 1 } bool;

#ifndef CCODE
  #define CCODE        /* signal Cactus header files that we're C, not Fortran */
#endif

#include "cctk_Types.h"
#include "cctk_Constants.h"
#include "cctk_Groups.h"
#include "cctk_Flesh.h"

#include "util_ErrorCodes.h"
#include "util_String.h"
#include "util_Table.h"

#ifdef UTIL_TABLE_TEST
  #include "cctk_Version.h"
#endif

#ifdef UTIL_TABLE_TEST
/* we build a standalone test driver */
#endif

#ifdef UTIL_TABLE_DEBUG
/* we print various debugging information */
#endif

#ifdef UTIL_TABLE_DEBUG2
  /* we print very verbose debugging information */
  #define UTIL_TABLE_DEBUG
#endif

#ifndef UTIL_TABLE_TEST
  static const char *rcsid = "$Header: /cactus/Cactus/src/util/Table.c,v 1.6 2002/01/21 14:31:22 rideout Exp $";
  CCTK_FILEVERSION(util_Table_c)
#endif

/******************************************************************************/
/***** Growable Array Data Structures *****************************************/
/******************************************************************************/

/*
 * We use "growable arrays" to keep track of all tables and all table
 * iterators.  In both cases we use the same data structure:
 *
 *      int N_objects;          // actual number of tables/iterators
 *      int N_array;            // actual size of growable array
 *      void *array;            // pointer to malloc-allocated growable array
 *                              // indexed by handle/ihandle
 *
 * Note that the pointer must be  void *  so we can use the  grow_array()
 * function; this pointer should be cast into an actual usable type for
 * normal uses.  Null pointers in the array mark unused array elements.
 */

/*
 * growth policy for growable arrays
 * sequence is
#ifdef UTIL_TABLE_TEST
 *      0, 1, 3, 7, 15, ... entries     (very slow growth
 *                                       ==> better exercise growing code)
#else
 *      0, 10, 30, 70, 150, ... entries
#endif
 * n.b. this grows >= a geometric series
 *      ==> total time in realloc is linear in max array size
 *      (if we just grew in an arithmetic progression then the total
 *       time in realloc() would be quadratic in the max array size)
 */
#ifdef UTIL_TABLE_TEST
  #define GROW(old_n)   (2*(old_n) + 1)
#else
  #define GROW(old_n)   (2*(old_n) + 10)
#endif

/******************************************************************************/
/***** Table Data Structures **************************************************/
/******************************************************************************/

/*
 * The present implementation represents a table as a singly-linked
 * list of table entries.  The code is generally programmed for simplicity,
 * not for maximum performance: linear searches are used everywhere.
 * In practice, we don't expect tables to have very many entries, so
 * this shouldn't be a problem.
 */

struct  table_entry
        {
        struct table_entry *next;
        char *key;
        int type_code;
        int N_elements;
        void *value;
        };

struct  table_header
        {
        struct table_entry *head;
        int flags;
        int handle;
        };

/*
 * We keep track of all tables with the following variables
 * (all are static ==> private to this file)
 */

/* number of tables */
static int N_tables = 0;

/* number of elements in the following array */
static int N_thp_array = 0;

/*
 * pointer to growable array of pointers to table headers,
 *            indexed by table handle,
 * with unused array elements set to NULL pointers
 * ... name abbreviates "table-header-pointer array"
 */
void **thp_array = NULL;

/******************************************************************************/
/***** Iterator Data Structures ***********************************************/
/******************************************************************************/

/*
 * This structure represents a table interator.
 *
 * Note that we never modify the table through an iterator,
 * so all the pointers here are to const objects
 */
struct  iterator
        {
        const struct table_header *thp; /* must always be non-NULL */
        const struct table_entry *tep;  /* NULL for iterator in */
                                        /* "null-pointer" state */
        };

/*
 * We keep track of all iterators with the following variables
 * (all are static ==> private to this file)
 */

/* number of iterators */
static int N_iterators = 0;

/* number of elements in the following array */
static int N_ip_array = 0;

/*
 * pointer to growable array of pointers to iterators,
 *            indexed by iterator handle,
 * with unused array elements set to NULL pointers
 * ... name abbreviates "iterator-pointer array"
 */
void **ip_array = NULL;

/******************************************************************************/
/***** Misc Macros for This File **********************************************/
/******************************************************************************/

#define then    /* empty */

#define min(x,y)        ((x < y) ? (x) : (y))
#define max(x,y)        ((x > y) ? (x) : (y))

/******************************************************************************/
/***** Prototypes for Functions Private to This File **************************/
/******************************************************************************/

/*
 * This is the internal function implementing all the
 *      Util_TableSet*()
 *      Util_TableSet*Array()
 * functions.  It returns their desired return value, i.e.
 *      1 for key was already in table before this call
 *        (old value was replaced)
 *        (it doesn't matter what the old value's type_code and
 *         N_elements were, i.e. these do *not* have to match the
 *         new value),
 *      0 for key was not in table before this call,
 *      UTIL_ERROR_BAD_HANDLE           handle is invalid
 *      UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
 *      UTIL_ERROR_BAD_INPUT            N_elements < 0
 *      UTIL_ERROR_NO_MEMORY            unable to allocate memory
 */
static
  int internal_set(int handle,
                   int type_code, int N_elements, const void *value,
                   const char *key);

/*
 * This is the internal function implementing all the
 *      Util_TableGet*()
 *      Util_TableGet*Array()
 * functions.  It returns their desired return value, i.e.
 *      number of values stored in  array[]  if ok,
 *      -ve for error, including
 *      UTIL_ERROR_BAD_HANDLE           handle is invalid
 *      UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
 *      UTIL_ERROR_BAD_INPUT            array != NULL and N_elements < 0
 *      UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
 *      UTIL_ERROR_TABLE_WRONG_DATA_TYPE value has wrong data type
 */
static
  int internal_get(int handle,
                   int type_code, int N_value_buffer, void *value_buffer,
                   const char *key);

/* check table handle for validity, return pointer to table header */
static
  struct table_header *get_table_header_ptr(int handle);

/*
 * delete a key from a table
 * return same as Util_TableDeleteKey(), i.e.
 *      0 for ok (key existed before this call, and has now been deleted)
 *      -ve for error, including
 *      UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
 */
static
  int delete_key(struct table_header *thp, const char *key);

/* free table entry and key/value it points to */
static
  void free_table_entry(struct table_entry *tep);

/*
 * check if key is syntactically "bad" (eg contains '/' character)
 * returns true for bad key, false for ok
 */
static
  bool bad_key(const char *key);

/*
 * find table entry for a given key
 * return pointer to it, or NULL if no such key is present in table
 * if  prev_tep_ptr != NULL,
 *    then also set *prev_tep_ptr to point to table entry one *before*
 *         the one with the given key, or to NULL if the given key is
 *         the starting entry in the table
 */
static
  struct table_entry *find_table_entry
          (const struct table_header *thp, const char *key,
           struct table_entry **prev_tep_ptr);

/* check iterator handle for validity, return pointer to iterator */
static
  struct iterator *get_iterator_ptr(int ihandle);

/*
 * This function grows an malloc-allocated array of  void *  pointers
 * via realloc(), initializing the new space to NULL pointers.
 *
 * Arguments:
 * *pN = (in out) array size
 * *pvp_array = (in out) Pointer to growable array of  void *  pointers.
 *
 * Results:
 * This function returns
 *      0 for ok,
 *      -ve for error, including
 *      UTIL_ERROR_NO_MEMORY            can't allocate memory to grow table
 */
static
  int grow_pointer_array(int *pN, void ***pvp_array);

#ifdef UTIL_TABLE_TEST
/*
 * Print out the table and iterator data structures.
 */
static void print_all_tables(void);
static void print_table(int handle);
static void print_all_iterators(void);
#endif

#ifdef UTIL_TABLE_TEST
/*
 * test drivers
 */
static void test_nonexistent_tables(void);
static void test_table_create_destroy(void);
static void test_set_get(int handle, bool case_insensitive);
static void test_set_get_array(int handle);
static void test_iterators(int handle);
static void test_delete_key(int handle, bool case_insensitive);
static int test_set_create_from_string(void);
static void test_set_get_string(int handle, bool case_insensitive);
#endif

/******************************************************************************/
/***** Main Table API *********************************************************/
/******************************************************************************/

/*@@
  @routine      Util_TableCreate
  @desc         This function creates a new (empty) table.

  @var          flags
  @vtype        int
  @vdesc        inclusive-or of UTIL_TABLE_FLAGS_* bit flags, must be >= 0
                (n.b. for Fortran users: inclusive-or is the same as sum here,
                since the bit masks are all disjoint)
  @endvar

  @comment      We require flags >= 0 so other functions can distinguish
                flags from (negative) error codes
  @endcomment

  @returntype   int
  @returndesc   a handle to the newly-created table,
                -ve for error, including
                UTIL_ERROR_NO_MEMORY            unable to allocate memory
                UTIL_ERROR_TABLE_BAD_FLAGS      flags < 0

  @endreturndesc
  @@*/
int Util_TableCreate(int flags)
{
#ifdef UTIL_TABLE_DEBUG
printf("Util_TableCreate()\n");
#endif

if (flags < 0)
   then return UTIL_ERROR_TABLE_BAD_FLAGS;

if (N_tables == N_thp_array)
   then {
        /* grow  thp_array  to get some room to create the new table */
        #ifdef UTIL_TABLE_DEBUG
        printf("   growing thp_array[] from old size %d\n", N_thp_array);
        #endif
        if (grow_pointer_array(&N_thp_array, &thp_array) < 0)
           then return UTIL_ERROR_NO_MEMORY;
        #ifdef UTIL_TABLE_DEBUG
        printf("                         to new size %d\n", N_thp_array);
        #endif
        }

/* we should now have space to create the new table */
assert(N_tables < N_thp_array);

/* find an unused handle */
#ifdef UTIL_TABLE_DEBUG
printf("   searching for an unused handle (N_tables=%d N_thp_array=%d)\n",
       N_tables, N_thp_array);
#endif
  {
int handle;
        for (handle = 0 ; handle < N_thp_array ; ++handle)
        {
        #ifdef UTIL_TABLE_DEBUG2
        printf("      checking handle=%d\n", handle);
        #endif
        if (thp_array[handle] == NULL)
           then {
                /* we've found an unused handle ==> create the table */
                struct table_header *const thp
                        = (struct table_header *)
                          malloc(sizeof(struct table_header));
                if (thp == NULL)
                   then return UTIL_ERROR_NO_MEMORY;

                #ifdef UTIL_TABLE_DEBUG
                printf("   using handle=%d\n", handle);
                #endif

                thp->head = NULL;
                thp->flags = flags;
                thp->handle = handle;

                ++N_tables;
                thp_array[handle] = (void *) thp;

                return handle;
                }
        }

/* we should never get to here! */
assert(false);
abort();                                /* internal error (core dump) */
/* prevent compiler warning 'function should return a value' */
return(0);
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableDestroy
  @desc         This function destroys a table.

                (Of course, this invalidates any iterators for this table.)

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @returntype   int
  @returndesc   0 for ok,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
  @endreturndesc
  @@*/
int Util_TableDestroy(int handle)
{
struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

#ifdef UTIL_TABLE_DEBUG
printf("Util_TableDestroy(handle=%d)\n", handle);
#endif

/* delete all the keys */
  {
struct table_entry *tep, *next_tep;
        for (tep = thp->head ; tep != NULL ; tep = next_tep)
        {
        #ifdef UTIL_TABLE_DEBUG2
        printf("   deleting key \"%s\"\n", tep->key);
        #endif
        next_tep = tep->next;
        free_table_entry(tep);
        }

--N_tables;
thp_array[handle] = NULL;
free(thp);

return 0;
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableQueryFlags
  @desc         This function queries a table's flags word.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @returntype   int
  @returndesc   flags if table exists,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
  @endreturndesc
  @@*/
int Util_TableQueryFlags(int handle)
{
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

return thp->flags;
}

/******************************************************************************/

/*@@
  @routine      Util_TableQueryNKeys
  @desc         This function queries the total number of key/value entries
                in a table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @returntype   int
  @returndesc   number of entries (>= 0),
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
  @endreturndesc
  @@*/
int Util_TableQueryNKeys(int handle)
{
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

  {
int N = 0;
const struct table_entry *tep;
        for (tep = thp->head ; tep != NULL ; tep = tep->next)
        {
        ++N;
        }
return N;
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableQueryMaxKeyLength
  @desc         This function queries the maximum key length in a table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @returntype   int
  @returndesc   maximum key length (>= 0),
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
  @endreturndesc
  @@*/
int Util_TableQueryMaxKeyLength(int handle)
{
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

  {
int max_length = 0;
const struct table_entry *tep;
        for (tep = thp->head ; tep != NULL ; tep = tep->next)
        {
        const int length = strlen(tep->key);
        if (length > max_length)
           then max_length = length;
        }
return max_length;
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableQueryValueInfo
  @desc         This function queries the type and number of elements
                of the value corresponding to a specified key in a table.
                It can also be used to "just" determine whether or not
                a specified key is present in a table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          type_code
  @vtype        int *
  @vdesc        pointer to where this function should store
                the value's type code
                (one of the CCTK_VARIABLE_* constants from "cctk_Types.h"),
                or NULL pointer to skip storing this
  @endvar

  @var          N_elements
  @vtype        int *
  @vdesc        pointer to where this function should store
                the number of array elements in the value,
                or NULL pointer to skip storing this
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   1 for key is in table,
                0 for no such key in table
                  (in this case nothing is stored in *type and *N_elements)
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
  @comment      Unlike all the other query functions, this function
                returns 0 for no such key in table.  The rationale
                for this design is that by passing NULL pointers for
                type_code and N_elements, this function is then a
                Boolean "is key in table?" predicate.
  @endreturndesc
  @@*/
int Util_TableQueryValueInfo(int handle,
                             CCTK_INT *type_code, CCTK_INT *N_elements,
                             const char *key)
{
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

if (bad_key(key))
   then return UTIL_ERROR_TABLE_BAD_KEY;
  {
const struct table_entry *const tep = find_table_entry(thp, key, NULL);
if (tep == NULL)
   then return 0;                       /* no such key in table */

if (type_code != NULL)
   then *type_code = tep->type_code;
if (N_elements != NULL)
   then *N_elements = tep->N_elements;
return 1;                               /* key is in table */
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableDeleteKey
  @desc         This function deletes a key (and the corresponding value)
                from a table.

                Note that this invalidates any iterators for this table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   0 for ok (key existed before this call,
                          and has now been deleted)
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
  @endreturndesc
  @@*/
int Util_TableDeleteKey(int handle, const char *key)
{
struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

if (bad_key(key))
   then return UTIL_ERROR_TABLE_BAD_KEY;

return delete_key(thp, key);
}

/******************************************************************************/

/*@@
  @routine      Util_TableCreateFromString
  @desc         This function creates a new table (with the case-insensitive
		flag set), and sets values in it based on a string argument.
		The string is interpreted with "parameter-file" semantics.

  @comment      The "Implementation Restriction" of Util_TableSetFromString()
		applies here as well.
  @endcomment

  @var          string
  @vtype        const char *
  @vdesc        C-style null-terminated string specifying table contents;
                string has parameter-file semantics
  @endvar

  @returntype   int
  @returndesc   a handle to the newly-created table,
                -ve for error, including
                UTIL_ERROR_NO_MEMORY    unable to allocate memory
                UTIL_ERROR_BAD_INPUT    invalid input: can't parse input string
                and any error codes returned by
                Util_TableCreate() or Util_TableSetFromString()
  @endreturndesc
  @@*/
int Util_TableCreateFromString(const char string[])
{
const int handle = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
if (handle < 0)
   then return handle;                  /* error creating table */

  {
const int status = Util_TableSetFromString(handle, string);
if (status < 0)
   then return status;                  /* error setting values in table */

return handle;
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableSetFromString
  @desc         This function does a sequence of Util_TableSet*() calls
		to set table entries based on a parameter-file--like
		string argument.  For example,
		   Util_TableSetFromString(handle, "order=3 dx=0.1")
		is equivalent to
		   Util_TableSetInt(handle, 3, "order");
		   Util_TableSetReal(handle, 0.1, "dx");

  @comment      Implementation Restriction:
                The present implementation only recognises integer or
                real values (not complex or character), and only scalars
                (not arrays).  In more detail, the strings recognized
                are defined by the following BNF:
                        string -> assign*
                        assign -> whitespace* key = value ;? whitespace*
                        value  -> int_value | real_value
                        int_value -> contains only chars from int_chars,
                                     and is recognized as valid by sscanf()
                                     with a "%d" format
                        real_value -> contains one or more chars not in
                                      int_chars, and is recognized as
                                      valid by sscanf() with a "%lf" format
                where
                int_chars is the constant string defined in the code below,
                * denotes 0 or more repetitions, and
                ? denotes optional items, i.e. 0 or 1 repetitions.

                Notice that whitespace separates "key=value" assignments,
                and thus that no whitespace may appear with a "key=value"
                assignment.
  @endcomment

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          string
  @vtype        const char *
  @vdesc        C-style null-terminated string which is parsed as
		described above to determine the keys and values to be
		set in the table.
  @endvar

  @returntype   int
  @returndesc   the number of successful Util_TableSet*() calls made, or
                -ve for error, including
                UTIL_ERROR_NO_MEMORY    unable to allocate memory
                UTIL_ERROR_BAD_INPUT    invalid input: can't parse input string
                and any error codes returned by the Util_TableSet*() functions
		Note that in the event of an error return, assignments
		lexicographically earlier in the input string than where
		the error was detected will already have been made in the
		table.
  @endreturndesc
  @@*/
int Util_TableSetFromString(int handle, const char string[])
{
const char *const delimiters = "; \t\n";
const char *const whitespace =  " \t\n";
const char *const int_chars  = "-+0123456789";

#ifdef UTIL_TABLE_DEBUG
printf("Util_TableSetFromString(handle=%d, \"%s\")\n", handle, string);
#endif

/* make a copy of the string so we can write null characters into it */
/* to partition it into substrings */
  {
char *const buffer = Util_Strdup(string);
if (buffer == NULL)
   then return UTIL_ERROR_NO_MEMORY;

  {
int Set_count = 0;
char *p = buffer;
        while (*p != '\0')
        {
        /*
         * each pass through this loop processes a single key=value
         * assignment starting at p, creating a table entry for it
         */

        /* skip over any leading whitespace */
        const size_t N_white = strspn(p, whitespace);
        p += N_white;
        #ifdef UTIL_TABLE_DEBUG2
        printf("   skipped over delimiters to p-buffer=%d\n", (int) (p-buffer));
        #endif

          {
        const char *const key = p;      /* key -> "key=value..." */
        char *q = strchr(p, '=');
        if (q == NULL)
           then {
                free(buffer);
                return UTIL_ERROR_BAD_INPUT;  /* no '=" in "key=value" string */
                }
        *q++ = '\0';                    /* key -> "key", q -> "value..." */
          {
        char *const value = q;          /* value -> "value..." */
        const size_t value_length = strcspn(value, delimiters);
        q = value + value_length;       /* q -> delimiter */
        if (*q != '\0')         /* if we're already at the end of the */
                                /* buffer, we don't want to advance further */
           then *q++ = '\0';            /* value -> "value", q -> "..." */
        #ifdef UTIL_TABLE_DEBUG
        printf("   at p-buffer=%d, got key=\"%s\" value=\"%s\"\n",
               (int) (p-buffer), key, value);
        #endif

        if (strspn(value, int_chars) == value_length)
           then {
                /* value is made up solely of chars which can appear */ 
                /* in an integer ==> assume value is an integer */
                int value_int;
                if (sscanf(value, "%d", &value_int) != 1)
                   then {
                        free(buffer);
                        return UTIL_ERROR_BAD_INPUT;
                                        /* can't parse integer value */
                        }
                #ifdef UTIL_TABLE_DEBUG2
                printf("   ==> storing key=\"%s\", value_int=%d\n",
                       key, value_int);
                #endif
                  {
                const int status = Util_TableSetInt(handle, value_int, key);
                if (status < 0)
                   then {
                        free(buffer);
                        return status;  /* error setting key=integer in table */
                        }
                  }
                }
           else {
                /* value contains at least one character which can't */
                /* appear in an integer ==> assume value is a real */
                double value_double;
                if (sscanf(value, "%lf", &value_double) != 1)
                   then {
                        free(buffer);
                        return UTIL_ERROR_BAD_INPUT;
                                        /* can't parse real value */
                        }
                #ifdef UTIL_TABLE_DEBUG2
                printf("   ==> storing key=\"%s\", value_double=%g\n",
                       key, value_double);
                #endif
                  {
                const int status = Util_TableSetReal(handle, value_double, key);
                if (status < 0)
                   then {
                        free(buffer);
                        return status;  /* error setting key=real in table */
                        }
                  }
                }

	++Set_count;
        p = q;
        #ifdef UTIL_TABLE_DEBUG2
        printf("   after key=value, advanced p to p-buffer=%d\n",
               (int) (p-buffer));
        #endif
          }
          }
        }

free(buffer);
return Set_count;
  }
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableSetString
  @desc         This function sets the value associated with a specified
                key to be (a copy of) a specified character string.

                Note that this invalidates any iterators for this table.

  @comment      This function stores the value as array of strlen(string)
                CCTK_CHARs; the stored value does *not* include a terminating
                null character.  (This is convenient for Fortran.)
  @endcomment

  @comment      The implementation assumes (as is presently the case)
                that a string is in fact an array of CCTK_CHAR, i.e.
                that CCTK_CHAR is the same type as (or at least
                compatible with) char.
  @endcomment

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          string
  @vtype        const char *
  @vdesc        pointer to the (C-style null-terminated) string
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   Same as all the other  Util_TableSet*  functions, namely
                1 for key was already in table before this call
                  (old value was replaced)
                  (it doesn't matter what the old value's type_code and
                   N_elements were, i.e. these do *not* have to match the
                   new value),
                0 for key was not in table before this call,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_NO_MEMORY            unable to allocate memory
  @endreturndesc
  @@*/
int Util_TableSetString(int handle,
                        const char *string,
                        const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_CHAR, strlen(string), (const void *) string,
                    key);
}

/******************************************************************************/

/*@@
  @routine      Util_TableGetString
  @desc         This function gets a copy of the character-string value
                associated with a specified key, and stores it (or at least
                as much of it as will fit) in a specified character string.

  @comment      This function assumes that the value stored in the table
                is an array of CCTK_CHARs, which does *not* include a
                terminating null character.
  @endcomment

  @comment      The implementation assumes (as is presently the case)
                that a string is in fact an array of CCTK_CHAR, i.e.
                that CCTK_CHAR is the same type as (or at least
                compatible with) char.
  @endcomment

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          buffer_length
  @vtype        int (must be >= 1 if buffer != NULL)
  @vdesc        size of  buffer[]
  @endvar

  @var          buffer
  @vtype        char[]
  @vdesc        a buffer into which this function should store
                (at most  buffer_length-1  characters of) the value,
                terminated by a null character as usual for C strings,
                or NULL pointer to skip storing this
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   the string length of the value (as per strlen()),
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_BAD_INPUT            buffer != NULL
                                                and buffer_length <= 0
                UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
                UTIL_ERROR_TABLE_WRONG_DATA_TYPE    value has data type
                                                    other than CCTK_CHAR
                UTIL_ERROR_TABLE_STRING_TRUNCATED   buffer != NULL and
                                                    value was truncated
                                                    to fit in buffer[]
  @endreturndesc
  @@*/
int Util_TableGetString(int handle,
                        int buffer_length, char buffer[],
                        const char *key)
{
/* actual length of string, not counting terminating null character */
int string_length = internal_get(handle,
                                 CCTK_VARIABLE_CHAR,
                                 buffer_length-1, (void *) buffer,
                                 key);
if (string_length < 0)
   then return string_length;           /* error return from internal_get() */

/* explicitly add the terminating null character */
if (buffer != NULL)
   then {
        assert(buffer_length >= 1);     /* this should never fail: */
                                        /* internal_get() should return */
                                        /* an error if buffer != NULL */
                                        /* and buffer_length-1 < 0 */
          {
        int null_posn = min(string_length, buffer_length-1);
        buffer[null_posn] = '\0';
          }
        }

return ((buffer != NULL) && (string_length > buffer_length-1))
       ? UTIL_ERROR_TABLE_STRING_TRUNCATED
       : string_length;
}

/******************************************************************************/

/*@@
  @routine      Util_TableSet*
  @desc         This is a family of functions, one for each Cactus data type,
                to set the value associated with a specified key to be a
                specified value (treated as a 1-element array).

                Note that this invalidates any iterators for this table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          value
  @vtype        one of
                   CCTK_POINTER, CCTK_FN_POINTER,
                   CCTK_CHAR,
                   CCTK_INT, CCTK_INT2, CCTK_INT4, CCTK_INT8,
                   CCTK_REAL, CCTK_REAL4, CCTK_REAL8, CCTK_REAL16,
                   CCTK_COMPLEX, CCTK_COMPLEX8, CCTK_COMPLEX16, CCTK_COMPLEX32
                (not all of these may be supported on any given system)
  @vdesc        the value to be associated with the specified key
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   1 for key was already in table before this call
                  (old value was replaced)
                  (it doesn't matter what the old value's type_code and
                   N_elements were, i.e. these do *not* have to match the
                   new value),
                0 for key was not in table before this call,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_NO_MEMORY            unable to allocate memory
  @endreturndesc
  @@*/

/**************************************/

/*
 * pointers
 */

int Util_TableSetPointer(int handle, CCTK_POINTER value, const char *key)
{
return Util_TableSetPointerArray(handle, 1, &value, key);
}

int Util_TableSetFnPointer(int handle, CCTK_FN_POINTER value, const char *key)
{
return Util_TableSetFnPointerArray(handle, 1, &value, key);
}

/**************************************/

/*
 * a single character
 */

int Util_TableSetChar(int handle, CCTK_CHAR value, const char *key)
{
return Util_TableSetCharArray(handle, 1, &value, key);
}

/**************************************/

/*
 * integers
 */

int Util_TableSetInt(int handle, CCTK_INT value, const char *key)
{
return Util_TableSetIntArray(handle, 1, &value, key);
}

#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableSetInt2(int handle, CCTK_INT2 value, const char *key)
{
return Util_TableSetInt2Array(handle, 1, &value, key);
}
#endif

#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableSetInt4(int handle, CCTK_INT4 value, const char *key)
{
return Util_TableSetInt4Array(handle, 1, &value, key);
}
#endif

#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableSetInt8(int handle, CCTK_INT8 value, const char *key)
{
return Util_TableSetInt8Array(handle, 1, &value, key);
}
#endif

/**************************************/

/*
 * real numbers
 */

int Util_TableSetReal(int handle, CCTK_REAL value, const char *key)
{
return Util_TableSetRealArray(handle, 1, &value, key);
}

#ifdef CCTK_REAL_PRECISION_4
int Util_TableSetReal4(int handle, CCTK_REAL4 value, const char *key)
{
return Util_TableSetReal4Array(handle, 1, &value, key);
}
#endif

#ifdef CCTK_REAL_PRECISION_8
int Util_TableSetReal8(int handle, CCTK_REAL8 value, const char *key)
{
return Util_TableSetReal8Array(handle, 1, &value, key);
}
#endif

#ifdef CCTK_REAL_PRECISION_16
int Util_TableSetReal16(int handle, CCTK_REAL16 value, const char *key)
{
return Util_TableSetReal16Array(handle, 1, &value, key);
}
#endif

/**************************************/

/*
 * complex numbers
 */

int Util_TableSetComplex(int handle, CCTK_COMPLEX value, const char *key)
{
return Util_TableSetComplexArray(handle, 1, &value, key);
}

#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableSetComplex8(int handle, CCTK_COMPLEX8 value, const char *key)
{
return Util_TableSetComplex8Array(handle, 1, &value, key);
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableSetComplex16(int handle, CCTK_COMPLEX16 value, const char *key)
{
return Util_TableSetComplex16Array(handle, 1, &value, key);
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableSetComplex32(int handle, CCTK_COMPLEX32 value, const char *key)
{
return Util_TableSetComplex32Array(handle, 1, &value, key);
}
#endif

/******************************************************************************/

/*@@
  @routine      Util_TableSet*Array
  @desc         This is a family of functions, one for each Cactus data type,
                to set the value associated with the specified key to be
                (a copy of) a specified array.

                Note that this invalidates any iterators for this table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          N_elements
  @vtype        int (must be >= 0)
  @vdesc        number of elements in  array[]
  @endvar

  @var          array
  @vtype        const T[], where T is one of
                   CCTK_POINTER, CCTK_FN_POINTER,
                   CCTK_CHAR,
                   CCTK_INT, CCTK_INT2, CCTK_INT4, CCTK_INT8,
                   CCTK_REAL, CCTK_REAL4, CCTK_REAL8, CCTK_REAL16,
                   CCTK_COMPLEX, CCTK_COMPLEX8, CCTK_COMPLEX16, CCTK_COMPLEX32
                (not all of these may be supported on any given system)
  @vdesc        a pointer to the array (a copy of) which
                is to be associated with the specified key
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   1 for key was already in table before this call
                  (old value was replaced)
                  (it doesn't matter what the old value's type_code and
                   N_elements were, i.e. these do *not* have to match the
                   new value),
                0 for key was not in table before this call,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_BAD_INPUT            N_elements < 0
                UTIL_ERROR_NO_MEMORY            unable to allocate memory
  @endreturndesc
  @@*/

/**************************************/

/*
 * arrays of pointers
 */

int Util_TableSetPointerArray(int handle,
                              int N_elements, const CCTK_POINTER array[],
                              const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_POINTER, N_elements, (const void *) array,
                    key);
}

int Util_TableSetFnPointerArray(int handle,
                                int N_elements, const CCTK_FN_POINTER array[],
                                const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_FN_POINTER, N_elements, (const void *) array,
                    key);
}

/**************************************/

/*
 * arrays of characters (i.e. character strings)
 */

int Util_TableSetCharArray(int handle,
                           int N_elements, const CCTK_CHAR array[],
                           const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_CHAR, N_elements, (const void *) array,
                    key);
}

/**************************************/

/*
 * arrays of integers
 */

int Util_TableSetIntArray(int handle,
                          int N_elements, const CCTK_INT array[],
                          const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_INT, N_elements, (const void *) array,
                    key);
}

#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableSetInt2Array(int handle,
                           int N_elements, const CCTK_INT2 array[],
                           const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_INT2, N_elements, (const void *) array,
                    key);
}
#endif

#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableSetInt4Array(int handle,
                           int N_elements, const CCTK_INT4 array[],
                           const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_INT4, N_elements, (const void *) array,
                    key);
}
#endif

#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableSetInt8Array(int handle,
                           int N_elements, const CCTK_INT8 array[],
                           const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_INT8, N_elements, (const void *) array,
                    key);
}
#endif

/**************************************/

/*
 * arrays of real numbers
 */

int Util_TableSetRealArray(int handle,
                           int N_elements, const CCTK_REAL array[],
                           const char *key)
{
return
  internal_set(handle,
                  CCTK_VARIABLE_REAL, N_elements, (const void *) array,
                  key);
}

#ifdef CCTK_REAL_PRECISION_4
int Util_TableSetReal4Array(int handle,
                            int N_elements, const CCTK_REAL4 array[],
                            const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_REAL4, N_elements, (const void *) array,
                    key);
}
#endif

#ifdef CCTK_REAL_PRECISION_8
int Util_TableSetReal8Array(int handle,
                            int N_elements, const CCTK_REAL8 array[],
                            const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_REAL8, N_elements, (const void *) array,
                    key);
}
#endif

#ifdef CCTK_REAL_PRECISION_16
int Util_TableSetReal16Array(int handle,
                             int N_elements, const CCTK_REAL16 array[],
                             const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_REAL16, N_elements, (const void *) array,
                    key);
}
#endif

/**************************************/

/*
 * arrays of complex numbers
 */

int Util_TableSetComplexArray(int handle,
                              int N_elements, const CCTK_COMPLEX array[],
                              const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_COMPLEX, N_elements, (const void *) array,
                    key);
}

#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableSetComplex8Array(int handle,
                               int N_elements, const CCTK_COMPLEX8 array[],
                               const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_COMPLEX8, N_elements, (const void *) array,
                    key);
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableSetComplex16Array(int handle,
                                int N_elements, const CCTK_COMPLEX16 array[],
                                const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_COMPLEX16, N_elements, (const void *) array,
                    key);
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableSetComplex32Array(int handle,
                                int N_elements, const CCTK_COMPLEX32 array[],
                                const char *key)
{
return internal_set(handle,
                    CCTK_VARIABLE_COMPLEX32, N_elements, (const void *) array,
                    key);
}
#endif

/******************************************************************************/

/*@@
  @routine      Util_TableGet*
  @desc         This is a family of functions, one for each Cactus data type,
                to get a copy of the scalar (1-element array) value, or more
                generally the first array element of the value, associated
                with a specified key.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          value
  @vtype        T *, where T is one of
                   CCTK_POINTER, CCTK_FN_POINTER,
                   CCTK_CHAR,
                   CCTK_INT, CCTK_INT2, CCTK_INT4, CCTK_INT8,
                   CCTK_REAL, CCTK_REAL4, CCTK_REAL8, CCTK_REAL16,
                   CCTK_COMPLEX, CCTK_COMPLEX8, CCTK_COMPLEX16, CCTK_COMPLEX32
                (not all of these may be supported on any given system)
  @vdesc        pointer to where this function should store
                a copy of the value associated with the specified key,
                or NULL pointer to skip storing this
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   the number of elements in the value,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
                UTIL_ERROR_TABLE_WRONG_DATA_TYPE value has wrong data type
                UTIL_ERROR_TABLE_VALUE_IS_EMPTY value is an empty
                                                (0-element) array
  @comment      Note that it is *not* an error for the value to actually
                be an array with > 1 elements elements; in this case only
                the first element is stored.

                The rationale for this design is that the caller may
                know or suspect that the value is a large array, but
                may only want the first array element; in this case
                this design avoids the caller having to allocate a
                large buffer unnecessarily.

                In contrast, it *is* an error for the value to actually
                be an empty (0-length) array, because then there is no
                ``first array element'' to get.
  @endcomment
  @endreturndesc
  @@*/

/**************************************/

/* pointers */
int Util_TableGetPointer(int handle, CCTK_POINTER *value, const char *key)
{
int status = Util_TableGetPointerArray(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}

int Util_TableGetFnPointer(int handle, CCTK_FN_POINTER *value, const char *key)
{
int status = Util_TableGetFnPointerArray(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}

/**************************************/

/* a single character */
int Util_TableGetChar(int handle, CCTK_CHAR *value, const char *key)
{
int status = Util_TableGetCharArray(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}

/**************************************/

/* integers */
int Util_TableGetInt(int handle, CCTK_INT *value, const char *key)
{
int status = Util_TableGetIntArray(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}

#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableGetInt2(int handle, CCTK_INT2 *value, const char *key)
{
int status = Util_TableGetInt2Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableGetInt4(int handle, CCTK_INT4 *value, const char *key)
{
int status = Util_TableGetInt4Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableGetInt8(int handle, CCTK_INT8 *value, const char *key)
{
int status = Util_TableGetInt8Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

/**************************************/

/* real numbers */
int Util_TableGetReal(int handle, CCTK_REAL *value, const char *key)
{
int status = Util_TableGetRealArray(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}

#ifdef CCTK_REAL_PRECISION_4
int Util_TableGetReal4(int handle, CCTK_REAL4 *value, const char *key)
{
int status = Util_TableGetReal4Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

#ifdef CCTK_REAL_PRECISION_8
int Util_TableGetReal8(int handle, CCTK_REAL8 *value, const char *key)
{
int status = Util_TableGetReal8Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

#ifdef CCTK_REAL_PRECISION_16
int Util_TableGetReal16(int handle, CCTK_REAL16 *value, const char *key)
{
int status = Util_TableGetReal16Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

/**************************************/

/* complex numbers */
int Util_TableGetComplex(int handle, CCTK_COMPLEX *value, const char *key)
{
int status = Util_TableGetComplexArray(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}

#ifdef CCTK_COMPLEX_PRECISION_8
int Util_TableGetComplex8(int handle, CCTK_COMPLEX8 *value, const char *key)
{
int status = Util_TableGetComplex8Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableGetComplex16(int handle, CCTK_COMPLEX16 *value, const char *key)
{
int status = Util_TableGetComplex16Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableGetComplex32(int handle, CCTK_COMPLEX32 *value, const char *key)
{
int status = Util_TableGetComplex32Array(handle, 1, value, key);
return (status == 0)
       ? UTIL_ERROR_TABLE_VALUE_IS_EMPTY
       : status;
}
#endif

/******************************************************************************/

/*@@
  @routine      Util_TableGet*Array
  @desc         This is a family of functions, one for each Cactus data type,
                to get a copy of the value associated with a specified key
                (or at least as much of the value as will fit into the
                caller's array).

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          N_array
  @vtype        int (must be >= 0)
  @vdesc        number of elements in  array[]
  @endvar

  @var          array
  @vtype        T[], where T is one of
                   CCTK_POINTER, CCTK_FN_POINTER,
                   CCTK_CHAR,
                   CCTK_INT, CCTK_INT2, CCTK_INT4, CCTK_INT8,
                   CCTK_REAL, CCTK_REAL4, CCTK_REAL8, CCTK_REAL16,
                   CCTK_COMPLEX, CCTK_COMPLEX8, CCTK_COMPLEX16, CCTK_COMPLEX32
                (not all of these may be supported on any given system)
  @vdesc        an array into which this function should store
                (at most  N_array  elements of) a copy of the value
                associated with the specified key,
                or NULL pointer to skip storing this
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   the number of elements in the value,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_BAD_INPUT            array != NULL and N_array < 0
                UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
                UTIL_ERROR_TABLE_WRONG_DATA_TYPE value has wrong data type
  @comment      Note that it is *not* an error for the value to have
                > N_array elements; in this case only N_array are
                stored.  The caller can detect this by comparing the
                return value with N_array.

                The rationale for this design is that the caller may
                know or suspect that the value is a large array, but
                may only want the first few array elements; in this
                case this design avoids the caller having to allocate
                a large buffer unnecessarily.

                It is also *not* an error for the value to have < N_array
                elements; again the caller can detect this by comparing the
                return value with N_array.
  @endcomment
  @endreturndesc
  @@*/

/**************************************/

/* arrays of pointers */
int Util_TableGetPointerArray(int handle,
                              int N_array, CCTK_POINTER array[],
                              const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_POINTER, N_array, (void *) array,
                    key);
}

int Util_TableGetFnPointerArray(int handle,
                                int N_array, CCTK_FN_POINTER array[],
                                const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_FN_POINTER, N_array, (void *) array,
                    key);
}

/**************************************/

/* arrays of characters (i.e. character strings) */
int Util_TableGetCharArray(int handle,
                           int N_array, CCTK_CHAR array[],
                           const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_CHAR, N_array, (void *) array,
                    key);
}

/**************************************/

/* integers */
int Util_TableGetIntArray(int handle,
                          int N_array, CCTK_INT array[],
                          const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_INT, N_array, (void *) array,
                    key);
}

#ifdef CCTK_INTEGER_PRECISION_2
int Util_TableGetInt2Array(int handle,
                           int N_array, CCTK_INT2 array[],
                           const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_INT2, N_array, (void *) array,
                    key);
}
#endif

#ifdef CCTK_INTEGER_PRECISION_4
int Util_TableGetInt4Array(int handle,
                           int N_array, CCTK_INT4 array[],
                           const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_INT4, N_array, (void *) array,
                    key);
}
#endif

#ifdef CCTK_INTEGER_PRECISION_8
int Util_TableGetInt8Array(int handle,
                           int N_array, CCTK_INT8 array[],
                           const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_INT8, N_array, (void *) array,
                    key);
}
#endif

/**************************************/

/* real numbers */
int Util_TableGetRealArray(int handle,
                           int N_array, CCTK_REAL array[],
                           const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_REAL, N_array, (void *) array,
                    key);
}
#ifdef CCTK_REAL_PRECISION_4

int Util_TableGetReal4Array(int handle,
                            int N_array, CCTK_REAL4 array[],
                            const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_REAL4, N_array, (void *) array,
                    key);
}
#endif

#ifdef CCTK_REAL_PRECISION_8
int Util_TableGetReal8Array(int handle,
                            int N_array, CCTK_REAL8 array[],
                            const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_REAL8, N_array, (void *) array,
                    key);
}
#endif

#ifdef CCTK_REAL_PRECISION_16
int Util_TableGetReal16Array(int handle,
                             int N_array, CCTK_REAL16 array[],
                             const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_REAL16, N_array, (void *) array,
                    key);
}
#endif

/**************************************/

/* complex numbers */
int Util_TableGetComplexArray(int handle,
                              int N_array, CCTK_COMPLEX array[],
                              const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_COMPLEX, N_array, (void *) array,
                    key);
}
#ifdef CCTK_COMPLEX_PRECISION_8

int Util_TableGetComplex8Array(int handle,
                               int N_array, CCTK_COMPLEX8 array[],
                               const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_COMPLEX8, N_array, (void *) array,
                    key);
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_16
int Util_TableGetComplex16Array(int handle,
                                int N_array, CCTK_COMPLEX16 array[],
                                const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_COMPLEX16, N_array, (void *) array,
                    key);
}
#endif

#ifdef CCTK_COMPLEX_PRECISION_32
int Util_TableGetComplex32Array(int handle,
                                int N_array, CCTK_COMPLEX32 array[],
                                const char *key)
{
return internal_get(handle,
                    CCTK_VARIABLE_COMPLEX16, N_array, (void *) array,
                    key);
}
#endif

/******************************************************************************/
/***** Table Iterator API *****************************************************/
/******************************************************************************/

/*@@
  @routine      Util_TableItCreate
  @desc         This function creates a new table iterator.  The iterator
                points to the starting entry in the table's traversal order.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @returntype   int
  @returndesc   a handle to the newly-created iterator,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           table handle is invalid
                UTIL_ERROR_NO_MEMORY            unable to allocate memory
  @endreturndesc
  @@*/
int Util_TableItCreate(int handle)
{
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

#ifdef UTIL_TABLE_DEBUG
printf("Util_TableItCreate(handle=%d)\n", handle);
#endif

if (N_iterators == N_ip_array)
   then {
        /* grow  iterator_array  to get some room to create the new table */
        #ifdef UTIL_TABLE_DEBUG
        printf("   growing ip_array[] from old size %d\n",
               N_ip_array);
        #endif
        if (grow_pointer_array(&N_ip_array, &ip_array) < 0)
           then return UTIL_ERROR_NO_MEMORY;    /* can't grow array */
        #ifdef UTIL_TABLE_DEBUG
        printf("                        to new size %d\n",
               N_ip_array);
        #endif
        }

/* we should now have space to create the new iterator */
assert(N_iterators < N_ip_array);

/* find an unused iterator handle */
#ifdef UTIL_TABLE_DEBUG
printf("   searching for an unused iterator handle\n");
printf("   (N_iterators=%d N_ip_array=%d\n", N_iterators, N_ip_array);
#endif
  {
int ihandle;
        for (ihandle = 0 ; ihandle < N_ip_array ; ++ihandle)
        {
        #ifdef UTIL_TABLE_DEBUG2
        printf("      checking ihandle=%d\n", ihandle);
        #endif
        if (ip_array[ihandle] == NULL)
           then {
                /* we've found an unused ihandle ==> create the iterator */
                struct iterator *const ip
                        = (struct iterator *) malloc(sizeof(struct iterator));
                if (ip == NULL)
                   then return UTIL_ERROR_NO_MEMORY;
                                        /* can't allocate new iterator */

                #ifdef UTIL_TABLE_DEBUG2
                printf("   using ihandle=%d\n", ihandle);
                #endif

                ip->thp = thp;
                ip->tep = thp->head;    /* iterator initially */
                                        /* -> start of table */

                ++N_iterators;
                ip_array[ihandle] = (void *) ip;

                return ihandle;
                }
        }

/* we should never get to here! */
assert(false);
abort();                                /* internal error (core dump) */
/* prevent compiler warning 'function should return a value' */
return(0);
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableItDestroy
  @desc         This function destroys a table iterator.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   0 for ok,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItDestroy(int ihandle)
{
struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

#ifdef UTIL_TABLE_DEBUG
printf("Util_TableItDestroy(ihandle=%d)\n", ihandle);
#endif

--N_iterators;
ip_array[ihandle] = NULL;
free(ip);

return 0;                               /* ok */
}

/******************************************************************************/

/*@@
  @routine      Util_TableItQueryIsNull
  @desc         This function queries whether a table iterator is in the
                "null-pointer" state, i.e. whether it does *not* point
                to some table entry.

                Bad things (garbage results, core dumps) may happen if
                you call this function on a table iterator which has been
                invalidated by a change in the table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   1 for iterator is in "null-pointer" state,
                0 for iterator points to some table entry,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItQueryIsNull(int ihandle)
{
const struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

return (ip->tep == NULL)
       ? 1                              /* iterator in "null-pointer" state */
       : 0;                             /* iterator -> some table entry */
}

/******************************************************************************/

/*@@
  @routine      Util_TableItQueryIsNonNull
  @desc         This function queries whether a table iterator is *not* in
                the "null-pointer" state, i.e. whether it points to some
                table entry.

                Bad things (garbage results, core dumps) may happen if
                you call this function on an iterator which has been
                invalidated by a change in the table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   1 for iterator points to some table entry,
                0 for iterator is in "null-pointer" state,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItQueryIsNonNull(int ihandle)
{
const struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

return (ip->tep == NULL)
       ? 0                              /* iterator in "null-pointer" state */
       : 1;                             /* iterator -> some table entry */
}

/******************************************************************************/

/*@@
  @routine      Util_TableItQueryTableHandle
  @desc         This function queries which table a table iterator points
                into.

                Note that this is always well-defined, even if the iterator
                is in the "null-pointer" state, and even if the iterator
                has been invalidated by a change in the table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   table handle,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItQueryTableHandle(int ihandle)
{
const struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

return ip->thp->handle;
}

/******************************************************************************/

/*@@
  @routine      Util_TableItQueryKeyValueInfo
  @desc         This function queries the key and the type and number of
                elements of the value corresponding to that key, of the
                table entry to which an iterator points.  This is in fact
                the main purpose of iterators.

                Bad things (garbage results, core dumps) may happen if
                you call this function on an iterator which has been
                invalidated by a change in the table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @var          key_buffer_length,
  @vtype        int (must be >= 1 if key_buffer != NULL)
  @vdesc        length of  key_buffer[]  buffer
  @endvar

  @var          key_buffer,
  @vtype        char []
  @vdesc        a buffer into which this function should store
                (at most  key_buffer_length-1  characters of) the key,
                terminated by a null character as usual for C strings,
                or NULL pointer to skip storing this
  @endvar

  @var          type_code
  @vtype        CCTK_INT *
  @vdesc        pointer to where this function should store
                the value's type code
                (one of the CCTK_VARIABLE_* constants from "cctk_Types.h"),
                or NULL pointer to skip storing this
  @endvar

  @var          N_elements
  @vtype        CCTK_INT *
  @vdesc        pointer to where this function should store
                the number of array elements in the value,
                or NULL pointer to skip storing this
  @endvar

  @returntype   int
  @returndesc   the string length of the key (as per strlen()),
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
                UTIL_ERROR_TABLE_ITERATOR_IS_NULL  iterator is in
                                                   "null-pointer" state
                UTIL_ERROR_TABLE_STRING_TRUNCATED  key_buffer != NULL and
                                                   key was truncated
                                                   to fit in key_buffer[]
  @endreturndesc
  @@*/
int Util_TableItQueryKeyValueInfo(int ihandle,
                                  int key_buffer_length, char key_buffer[],
                                  CCTK_INT *type_code, CCTK_INT *N_elements)
{
const struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

  {
const struct table_entry *const tep = ip->tep;
if (tep == NULL)
   then return UTIL_ERROR_TABLE_ITERATOR_IS_NULL;

  {
const int actual_key_length = strlen(tep->key);

/* store the fixed-length output arguments first, so the caller */
/* will have them even if we hit an error trying to copy the key */
if (type_code != NULL)
   then *type_code = tep->type_code;
if (N_elements != NULL)
   then *N_elements = tep->N_elements;

if (key_buffer != NULL)
   then {
        const int N_key_copy = min(key_buffer_length-1, actual_key_length);
        if (N_key_copy < 0)     /* can only happen if key_buffer_length <= 0 */
           then {
                /*
                 * We have to bail out now, before trying the memcpy(),
                 * because memcpy() takes a size_t (= unsigned) value for
                 * its count of how many chars to copy, and converting our
                 * -ve N_key_copy to size_t would give a huge +ve count :( :(
                 */
                return UTIL_ERROR_TABLE_STRING_TRUNCATED;
                }
        memcpy(key_buffer, tep->key, N_key_copy);
        key_buffer[N_key_copy] = '\0';
        if (N_key_copy < actual_key_length)
           then return UTIL_ERROR_TABLE_STRING_TRUNCATED;
        }

return actual_key_length;               /* ok */
  }
  }
}

/******************************************************************************/

/*@@
  @routine      Util_TableItAdvance
  @desc         This function advances a table iterator to the next entry
                in the table's traversal order.

                Bad things (garbage results, core dumps) may happen if
                you call this function on an iterator which has been
                invalidated by a change in the table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   same as that of Util_TableItQueryNonNull(ihandle)
                after advancing the iterator, i.e.
                1 for ok and iterator now points to some table element,
                0 for advance-past-last-entry
                  (sets iterator to "null-pointer" state),
                0 if iterator was already in "null-pointer" state)
                  (in this case this call is a no-op),
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItAdvance(int ihandle)
{
struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

if (ip->tep == NULL)
   then return 0;                       /* iterator was already in */
                                        /* "null-pointer" state */

ip->tep = ip->tep->next;

return (ip->tep == NULL)
       ? 0              /* advance past last entry */
                        /* ==> iterator now in "null-pointer" state */
       : 1;             /* ok */
}

/******************************************************************************/

/*@@
  @routine      Util_TableItResetToStart
  @desc         This function resets a table iterator to point to the
                starting entry in the table's traversal order.

                Note that it is always ok to call this function, even
                if the iterator has been invalidated by a change in the
                table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   same as that of Util_TableItQueryNonNull(ihandle)
                after resetting the iterator, i.e.
                1 for ok and iterator now points to some table element,
                0 for ok and iterator is now in "null-pointer" state
                  (means table is empty)
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItResetToStart(int ihandle)
{
struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

ip->tep = ip->thp->head;
return (ip->tep == NULL)
       ? 0              /* ok, iterator is now in "null-pointer" state */
                        /*     (table must be empty) */
       : 1;             /* ok, iterator points to some table element */
}

/******************************************************************************/

/*@@
  @routine      Util_TableItSetToNull
  @desc         This function sets a table iterator to the "null-pointer"
                state.

                Note that it is always ok to call this function, even
                if the iterator has been invalidated by a change in the
                table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @returntype   int
  @returndesc   0 for ok,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
  @endreturndesc
  @@*/
int Util_TableItSetToNull(int ihandle)
{
struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

ip->tep = NULL;
return 0;                               /* ok */
}

/******************************************************************************/

/*@@
  @routine      Util_TableItSetToKey
  @desc         This function sets a table iterator to point to a
                specified table entry.  It has the same effect as
                Util_TableItResetToStart() followed by repeatedly
                calling Util_TableItAdvance() until the iterator
                points to the desired table entry.

                Note that it is always ok to call this function, even
                if the iterator has been invalidated by a change in the
                table's contents.

  @var          ihandle
  @vtype        int
  @vdesc        handle to the iterator
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   0 for ok,
                UTIL_ERROR_BAD_HANDLE           iterator handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
  @endreturndesc
  @@*/
int Util_TableItSetToKey(int ihandle, const char *key)
{
struct iterator *const ip = get_iterator_ptr(ihandle);
if (ip == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

if (bad_key(key))
   then return UTIL_ERROR_TABLE_BAD_KEY;

ip->tep = find_table_entry(ip->thp, key, NULL);
if (ip->tep == NULL)
   then return UTIL_ERROR_TABLE_NO_SUCH_KEY;

return 0;
}

/******************************************************************************/
/***** Internal Support Functions *********************************************/
/******************************************************************************/

/*@@
  @routine      internal_set
  @desc         This is the internal function implementing all the
                        Util_TableSet*()
                        Util_TableSet*Array()
                functions except Util_TableSetString().  It sets the
                value associated with a specified key, to be a copy
                of a specified array.

                Note that this invalidates any iterators for this table.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          N_elements
  @vtype        int (must be >= 0)
  @vdesc        number of elements in  array[]
  @endvar

  @var          array
  @vtype        const T[], where T is one of
                   CCTK_POINTER, CCTK_FN_POINTER,
                   CCTK_CHAR,
                   CCTK_INT, CCTK_INT2, CCTK_INT4, CCTK_INT8,
                   CCTK_REAL, CCTK_REAL4, CCTK_REAL8, CCTK_REAL16,
                   CCTK_COMPLEX, CCTK_COMPLEX8, CCTK_COMPLEX16, CCTK_COMPLEX32
                (not all of these may be supported on any given system)
  @vdesc        the array (a copy of) which is to be associated with
                the specified key
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   1 for key was already in table before this call
                  (old value was replaced)
                  (it doesn't matter what the old value's type_code and
                   N_elements were, i.e. these do *not* have to match the
                   new value),
                0 for key was not in table before this call,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_BAD_INPUT            N_elements < 0
                UTIL_ERROR_NO_MEMORY            unable to allocate memory
  @endreturndesc
  @@*/
static
  int internal_set(int handle,
                   int type_code, int N_elements, const void *value,
                   const char *key)
{
struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

#ifdef UTIL_TABLE_DEBUG
printf("internal_set(handle=%d, type_code=%d, N_elements=%d, key=\"%s\")\n",
       handle, type_code, N_elements, key);
#endif

if (bad_key(key))
   then return UTIL_ERROR_TABLE_BAD_KEY;
if (N_elements < 0)
   then return UTIL_ERROR_BAD_INPUT;

/* if key is already in table, delete it */
/* ... this is a harmless no-op if it's not already in the table */
  {
int return_value;
switch  (delete_key(thp, key))
        {
case 0:
        return_value = 1;       /* key was already in table before this call */
                                /* (we've just deleted it, and we're about */
                                /*  to set the replacement in the table) */
        break;
case UTIL_ERROR_TABLE_NO_SUCH_KEY:
        return_value = 0;       /* key was not in table before this call */
        break;
default:
        /* unexpected return code from  delete_key() */
        /* (this should never happen!) */
        assert(false);
        abort();                        /* internal error (core dump) */
        }

/* allocate a new table entry */
  {
struct table_entry *tep
        = (struct table_entry *) malloc(sizeof(struct table_entry));
if (tep == NULL)
   then return UTIL_ERROR_NO_MEMORY;    /* can't allocate new table entry */

/* set up the new table entry */
tep->key = Util_Strdup(key);
if (tep->key == NULL)
   then {
        free(tep);
        return UTIL_ERROR_NO_MEMORY;    /* can't allocate memory to copy key */
        }

tep->type_code = type_code;
tep->N_elements = N_elements;

  {
size_t sizeof_value = N_elements * CCTK_VarTypeSize(type_code);
#ifdef UTIL_TABLE_DEBUG2
printf("   allocating new buffer of size sizeof_value=%d bytes\n",
       (int) sizeof_value);
#endif
  {
void *buffer = malloc(sizeof_value);
if (buffer == NULL)
   then {
        free(tep->key);
        free(tep);
        return UTIL_ERROR_NO_MEMORY;    /* can't allocate memory */
                                        /* to copy value */
        }
#ifdef UTIL_TABLE_DEBUG
printf("   copying sizeof_value=%d bytes into buffer\n", (int) sizeof_value);
#endif
memcpy(buffer, value, sizeof_value);
tep->value = buffer;

/* insert the table entry into the table's linked list */
/* (we could insert it anywhere; for simplicity we insert it at the head) */
tep->next = thp->head;
thp->head = tep;

return return_value;
  }
  }
  }
  }
}

/******************************************************************************/

/*@@
  @routine      internal_get
  @desc         This is the internal function implementing all the
                        Util_TableGet*()
                        Util_TableGet*Array()
                functions except for Util_TableGetString().  It copies
                up to N_elements of the value associated with a specified
                key, into a user-supplied buffer.

  @var          handle
  @vtype        int
  @vdesc        handle to the table
  @endvar

  @var          N_value_buffer
  @vtype        int (must be >= 0)
  @vdesc        number of elements in  array[]
  @endvar

  @var          value_buffer
  @vtype        T[], where T is one of
                   CCTK_POINTER, CCTK_FN_POINTER,
                   CCTK_CHAR,
                   CCTK_INT, CCTK_INT2, CCTK_INT4, CCTK_INT8,
                   CCTK_REAL, CCTK_REAL4, CCTK_REAL8, CCTK_REAL16,
                   CCTK_COMPLEX, CCTK_COMPLEX8, CCTK_COMPLEX16, CCTK_COMPLEX32
                (not all of these may be supported on any given system)
  @vdesc        an array into which this function should store
                (at most  N_elements  elements of) a copy of the value
                associated with the specified key,
                or NULL pointer to skip storing this
  @endvar

  @var          key
  @vtype        const char *
  @vdesc        pointer to the key (a C-style null-terminated string)
  @endvar

  @returntype   int
  @returndesc   number of elements in the value,
                -ve for error, including
                UTIL_ERROR_BAD_HANDLE           handle is invalid
                UTIL_ERROR_TABLE_BAD_KEY        key contains '/' character
                UTIL_ERROR_BAD_INPUT            N_value_buffer < 0
                UTIL_ERROR_BAD_INPUT            value_buffer != NULL
                                                and N_value_buffer < 0
                UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
                UTIL_ERROR_TABLE_WRONG_DATA_TYPE value has wrong data type
  @comment      Note that it is *not* an error for the value to have
                > N_value_buffer elements; in this case only N_value_buffer
                are stored.  The caller can detect this by comparing the
                return value with N_value_buffer.
  @endcomment
  @endreturndesc
  @@*/
static
  int internal_get(int handle,
                   int type_code, int N_value_buffer, void *value_buffer,
                   const char *key)
{
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then return UTIL_ERROR_BAD_HANDLE;

#ifdef UTIL_TABLE_DEBUG
printf("internal_get(handle=%d, type_code=%d, N_value_buffer=%d, key=\"%s\")\n",
       handle, type_code, N_value_buffer, key);
#endif

if (bad_key(key))
   then return UTIL_ERROR_TABLE_BAD_KEY;

  {
const struct table_entry *const tep = find_table_entry(thp, key, NULL);
if (tep == NULL)
   then return UTIL_ERROR_TABLE_NO_SUCH_KEY;    /* no such key in table */

if (tep->type_code != type_code)
   then return UTIL_ERROR_TABLE_WRONG_DATA_TYPE; /* value has wrong data type */

if (value_buffer != NULL)
   then {
        if (N_value_buffer < 0)
           then return UTIL_ERROR_BAD_INPUT;
          {
        const int N_copy = min(N_value_buffer, tep->N_elements);
        const size_t sizeof_N_copy_elements
                = N_copy * CCTK_VarTypeSize(type_code);
        #ifdef UTIL_TABLE_DEBUG
        printf(
           "   copying N_copy=%d elements (sizeof_N_copy_elements=%d bytes)\n",
               N_copy, (int) sizeof_N_copy_elements);
        #endif
        memcpy(value_buffer, tep->value, sizeof_N_copy_elements);
          }
        }

return tep->N_elements;
  }
}

/******************************************************************************/

/*
 * This function gets a pointer to a table's header, given the table handle.
 *
 * Arguments:
 * handle = The table handle.
 *
 * Results:
 * If the handle is invalid (i.e. there is no such table), this function
 *    returns NULL.
 * If the handle is valid, this function returns a pointer to the table header.
 */
static
  struct table_header *get_table_header_ptr(int handle)
{
return ((handle >= 0) && (handle < N_thp_array))
       ? (struct table_header *) thp_array[handle]      /* valid handle */
       : NULL;                                          /* invalid handle */
}

/******************************************************************************/

/*
 * This function deletes a key from a table.
 *
 * Results:
 * The return value is the same as for Util_TableDeleteKey(), i.e.
 *      0 for ok (key existed before this call, and has now been deleted)
 *      -ve for error, including
 *      UTIL_ERROR_TABLE_NO_SUCH_KEY    no such key in table
 */
static
  int delete_key(struct table_header *thp, const char *key)
{
struct table_entry *prev_tep;
struct table_entry *const tep = find_table_entry(thp, key, &prev_tep);
if (tep == NULL)
   then return UTIL_ERROR_TABLE_NO_SUCH_KEY;

  {
/* unlink the table entry from the list */
struct table_entry *next_tep = tep->next;
if (prev_tep == NULL)
   then thp->head = next_tep;           /* it was the starting entry */
                                        /* in the list */
   else prev_tep->next = next_tep;      /* it was somewhere in the middle */

free_table_entry(tep);
return 0;                               /* ok: key existed before this call, */
                                        /* and has now been deleted */
  }
}

/******************************************************************************/

/*
 * This function frees a table entry and the key/value it holds.
 *
 * Arguments:
 * tep -> The table entry to be freed.
 */
static
  void free_table_entry(struct table_entry *tep)
{
assert(tep != NULL);

assert(tep->key != NULL);
free(tep->key);

assert(tep->value != NULL);
free(tep->value);

free(tep);
}

/******************************************************************************/

/*
 * check if key is syntactically "bad" (eg contains '/' character)
 * returns true for bad key, false for ok
 */
static
  bool bad_key(const char *key)
{
assert(key != NULL);

if (strchr(key, '/') != NULL)
   then return true;

return false;                           /* ok */
}

/******************************************************************************/

/*
 * This function finds the (first) table entry with a given key.
 * Optionally, it also finds the table entry *before* that one in
 * the linked list.
 *
 * Arguments:
 * thp -> The table header.
 * key -> The key to search for.
 * prev_tep_ptr = If this is non-NULL, then this function sets
 *                *prev_tep_ptr to -> the table entry *before* the one
 *                with the given key, or NULL if the table entry with
 *                the given key is the starting one in the table.
 *                Thus if  prev_tep_ptr  is non-NULL, then after this
 *                function returns, the returned result is
 *                (*prev_tep_ptr == NULL) ? thp->head : (*prev_tep)->next
 *
 * Results:
 * The function returns a pointer to the table entry, or NULL if the
 * key isn't found in the table.
 */
static
  struct table_entry *find_table_entry(const struct table_header *thp,
                                       const char *key,
                                       struct table_entry **prev_tep_ptr)
{
assert(thp != NULL);
assert(key != NULL);

  {
const int flags = thp->flags;
struct table_entry *prev_tep = NULL;
struct table_entry *tep = thp->head;
        for ( ; tep != NULL ; prev_tep = tep, tep = tep->next)
        {
        if (  (flags & UTIL_TABLE_FLAGS_CASE_INSENSITIVE)
              ?  (Util_StrCmpi(key, tep->key) == 0)
              :  (     strcmp (key, tep->key) == 0)  )
           then {
                if (prev_tep_ptr != NULL)
                   *prev_tep_ptr = prev_tep;
                return tep;             /* key found in table */
                }
        }

return NULL;                            /* key not found in table */
  }
}

/******************************************************************************/

/*
 * This function gets a pointer to an iterator, given the iterator handle.
 *
 * Arguments:
 * ihandle = The iterator handle.
 *
 * Results:
 * If the handle is invalid (i.e. there is no such table), this function
 *    returns NULL.
 * If the handle is valid, this function returns a pointer to the iterator.
 */
static
  struct iterator *get_iterator_ptr(int ihandle)
{
return ((ihandle >= 0) && (ihandle < N_ip_array))
       ? (struct iterator *) ip_array[ihandle]          /* valid handle */
       : NULL;                                          /* invalid handle */
}

/******************************************************************************/

/*
 * This function grows an malloc-allocated array of  void *  pointers
 * via realloc(), initializing the new space to NULL pointers.
 *
 * Arguments:
 * *pN = (in out) array size
 * *pvp_array = (in out) Pointer to growable array of  void *  pointers.
 *
 * Results:
 * This function returns
 *      0 for ok,
 *      -ve for error, including
 *      UTIL_ERROR_NO_MEMORY            can't allocate memory to grow table
 */
static
  int grow_pointer_array(int *pN, void ***pvp_array)
{
int N = *pN;
void **vp_array = *pvp_array;
int new_N = GROW(N);
void **new_vp_array = realloc(vp_array, new_N*sizeof(void *));
if (new_vp_array == NULL)
   then return UTIL_ERROR_NO_MEMORY;    /* can't grow array */

/* initialize the new space to NULL pointers */
  {
int i;
        for (i = N ; i < new_N ; ++i)
        {
        new_vp_array[i] = NULL;
        }
  }

*pvp_array = new_vp_array;
*pN = new_N;
return 0;                               /* ok */
}

/******************************************************************************/
/***** Table and Iterator Dump Routines ***************************************/
/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function prints out all the tables and their data structures.
 */
static
  void print_all_tables(void)
{
int handle;

printf("N_tables=%d N_thp_array=%d\n", N_tables, N_thp_array);
        for (handle = 0 ; handle < N_thp_array ; ++handle)
        {
        print_table(handle);
        }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function prints out a table.
 */
static
  void print_table(int handle)
{
printf("thp_array[%d]: ", handle);
  {
const struct table_header *const thp = get_table_header_ptr(handle);
if (thp == NULL)
   then printf("NULL\n");
   else {
        printf("flags=0x%x handle=%d\n", thp->flags, thp->handle);
          {
        const struct table_entry *tep = thp->head;
                for ( ; tep != NULL ; tep = tep->next)
                {
                printf("    [tep=%p]\n", (const void *) tep);
                printf("\tkey=\"%s\"\n", tep->key);
                printf("\ttype_code=%d N_elements=%d\n",
                       tep->type_code, tep->N_elements);
                  {
                int i;
                switch  (tep->type_code)
                        {
                case CCTK_VARIABLE_INT:
                        printf("\t[int]");
                                for (i = 0 ; i < tep->N_elements ; ++i)
                                {
                                const CCTK_INT *value_int
                                        = (const CCTK_INT *) tep->value;
                                printf("\t%d", (int) value_int[i]);
                                }
                        break;
                case CCTK_VARIABLE_REAL:
                        printf("\t[real]");
                                for (i = 0 ; i < tep->N_elements ; ++i)
                                {
                                const CCTK_REAL *value_real
                                        = (const CCTK_REAL *) tep->value;
                                printf("\t%g", (double) value_real[i]);
                                }
                        break;
                case CCTK_VARIABLE_COMPLEX:
                        printf("\t[complex]");
                                for (i = 0 ; i < tep->N_elements ; ++i)
                                {
                                const CCTK_COMPLEX *value_complex
                                        = (const CCTK_COMPLEX *) tep->value;
                                printf("\t(%g,%g)",
                                       (double) value_complex[i].Re,
                                       (double) value_complex[i].Im);
                                }
                        }
                printf("\n");
                  }
                }
          }
        }
  }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function prints out all the iterators and their data structures.
 */
static
  void print_all_iterators(void)
{
int ihandle;

printf("N_iterators=%d N_ip_array=%d\n", N_iterators, N_ip_array);
        for (ihandle = 0 ; ihandle < N_ip_array ; ++ihandle)
        {
        const struct iterator *const ip = get_iterator_ptr(ihandle);
        printf("ip_array[%d]: ", ihandle);
        if (ip == NULL)
           then printf("NULL\n");
           else printf("thp=%p tep=%p\n",
                       (const void *) ip->thp, (const void *) ip->tep);
        }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/
/***** Standalone Test Driver *************************************************/
/******************************************************************************/

#ifdef UTIL_TABLE_TEST

/*
 * low-level macros to test set/get of scalars of various types
 */

#define CHECK_SET_GET_INT(handle, type,                                  \
                         key_already_exists, case_insensitive,          \
                         set_fn, get_fn)                                \
  {                                                                     \
type x = 42;                                                            \
assert( set_fn(handle, x, "int_x") == key_already_exists );             \
x = 1;                                                                  \
assert( get_fn(handle, &x, "int_x") == 1 );                             \
assert( x == 42 );                                                      \
if (case_insensitive)                                                   \
   then {                                                               \
        x = 2;                                                          \
        assert( get_fn(handle, &x, "Int_X") == 1 );                     \
        assert( x == 42 );                                              \
        }                                                               \
   else assert( get_fn(handle, &x, "Int_X")                             \
                == UTIL_ERROR_TABLE_NO_SUCH_KEY );                      \
  }                                                             /* end macro */

/**************************************/

#define CHECK_SET_GET_REAL(handle, type,                                 \
                          key_already_exists, case_insensitive, \
                          set_fn, get_fn)                               \
  {                                                                     \
type y = 42.25;                                                         \
assert( set_fn(handle, y, "REAL_y") == key_already_exists );            \
y = 1.25;                                                               \
assert( get_fn(handle, &y, "REAL_y") == 1 );                            \
assert( y == 42.25 );                                                   \
if (case_insensitive)                                                   \
   then {                                                               \
        y = 1.5;                                                        \
        assert( get_fn(handle, &y, "real_y") == 1 );                    \
        assert( y == 42.25 );                                           \
        }                                                               \
   else assert( get_fn(handle, &y, "real_y")                            \
                == UTIL_ERROR_TABLE_NO_SUCH_KEY );                      \
  }                                                             /* end macro */

/**************************************/

#define CHECK_SET_GET_COMPLEX(handle, type,                              \
                             key_already_exists, case_insensitive,      \
                             set_fn, get_fn)                            \
  {                                                                     \
static type z = { 42.25, 105.5 };                                       \
assert( set_fn(handle, z, "COMPlex_Z") == key_already_exists );         \
z.Re = 1.25;            z.Im = -2.78;                                   \
assert( get_fn(handle, &z, "COMPlex_Z") == 1 );                         \
assert( z.Re == 42.25 );                                                \
assert( z.Im == 105.5 );                                                \
if (case_insensitive)                                                   \
   then {                                                               \
        z.Re = 1.5;             z.Im = -2.83;                           \
        assert( get_fn(handle, &z, "COMPLEX_Z") == 1 );                 \
        assert( z.Re == 42.25 );                                        \
        assert( z.Im == 105.5 );                                        \
        }                                                               \
   else assert( get_fn(handle, &z, "COMPLEX_Z")                         \
                == UTIL_ERROR_TABLE_NO_SUCH_KEY );                      \
  }                                                             /* end macro */

/******************************************************************************/

/*
 * low-level macros to test set/get of arrays of various types
 */

#define CHECK_SET_GET_INT_ARRAY(handle, type, key_already_exists,        \
                               set_fn, get_fn)                          \
  {                                                                     \
static type xx[5] = { 41, 42, 48, 45, 47 };                             \
assert( set_fn(handle, 3, xx, "xx") == key_already_exists );            \
xx[0] = 14;  xx[1] = 15;  xx[2] = 16;  xx[3] = 17;  xx[4] = 19;         \
/* try to get 4 values, but only 3 were stored ==> only get 3 */        \
assert( get_fn(handle, 4, xx, "xx") == 3 );                             \
assert( xx[0] == 41 );                                                  \
assert( xx[1] == 42 );                                                  \
assert( xx[2] == 48 );                                                  \
assert( xx[3] == 17 );                                                  \
assert( xx[4] == 19 );                                                  \
  }                                                             /* end macro */

/**************************************/

#define CHECK_SET_GET_REAL_ARRAY(handle, type, key_already_exists,       \
                                set_fn, get_fn)                         \
  {                                                                     \
static type yy[5] = { 41.25, 42.5, 48.0, 45.75, 47.125 };               \
assert( set_fn(handle, 4, yy, "yy") == key_already_exists );            \
yy[0] = 14.0;  yy[1] = 15.5;  yy[2] = 16.0;                             \
yy[3] = 17.5;  yy[4] = 19.5;                                            \
/* only get 3 of 4 stored values */                                     \
assert( get_fn(handle, 3, yy, "yy") == 4 );                             \
assert( yy[0] == 41.25 );                                               \
assert( yy[1] == 42.5 );                                                \
assert( yy[2] == 48.0 );                                                \
assert( yy[3] == 17.5 );                                                \
assert( yy[4] == 19.5 );                                                \
  }                                                             /* end macro */

/**************************************/

#define CHECK_SET_GET_COMPLEX_ARRAY(handle, type, key_already_exists,    \
                                   set_fn, get_fn)                      \
  {                                                                     \
static type zz[5]                                                       \
        = { {3.5,1.25}, {9.5,4.5}, {0.5,8.0}, {5.0,5.5}, {4.5,7.25} };  \
assert( set_fn(handle, 4, zz, "zz") == key_already_exists );            \
zz[0].Re = 10.25;       zz[0].Im = 11.75;                               \
zz[1].Re = -2.5;        zz[1].Im = 3.5;                                 \
zz[2].Re = 14.0;        zz[2].Im = -8.5;                                \
zz[3].Re = 0.25;        zz[3].Im = 8.875;                               \
zz[4].Re = -0.25;       zz[4].Im = -0.75;                               \
/* only get 3 of 4 stored values */                                     \
assert( get_fn(handle, 3, zz, "zz") == 4 );                             \
assert( zz[0].Re == 3.5 );      assert( zz[0].Im == 1.25 );             \
assert( zz[1].Re == 9.5 );      assert( zz[1].Im == 4.5 );              \
assert( zz[2].Re == 0.5 );      assert( zz[2].Im == 8.0 );              \
assert( zz[3].Re == 0.25 );     assert( zz[3].Im == 8.875 );            \
assert( zz[4].Re == -0.25 );    assert( zz[4].Im == -0.75 );            \
  }                                                             /* end macro */

#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function does a sequence of assert() calls to verify that
 * a table contains the 3 keys
 *	ij = 42
 *	real1 = 3.5
 *	real_e = 2.75
 * inserted in that order.
 *
 * Bugs:
 * This test is tied to the present implementation -- it assumes a
 * specific ordering of table elements returned by an iterator.
 */
static
  void check_table_contents(int handle)
{
assert( Util_TableQueryNKeys(handle) == 3 );

/* set up the key buffer */
  {
int max_key_length = Util_TableQueryMaxKeyLength(handle);
assert( max_key_length == (int)strlen("real_e") );
  {
const int N_key_buffer = max_key_length + 1;
char *const key_buffer = malloc(N_key_buffer);
assert( key_buffer != NULL );

/* walk through the table to verify contents {"real_e", "real1", "ij"} */
/* n.b. implementation-dependence here for order of table elements */
  {
int ihandle = Util_TableItCreate(handle);
CCTK_INT key_length, type_code, N_elements;

/* real_e = 2.75 */
type_code = 123456;
N_elements = 54321;
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("real_e") );
assert( strcmp(key_buffer, "real_e") == 0 );
assert( type_code = CCTK_VARIABLE_REAL );
assert( N_elements == 1 );
  {
CCTK_REAL value_real;
assert( Util_TableGetReal(handle, &value_real, key_buffer) == 1 );
assert( value_real == 2.75 );

/* real1 = 3.5 */
assert( Util_TableItAdvance(ihandle) == 1 );
type_code = 123456;
N_elements = 54321;
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("real1") );
assert( strcmp(key_buffer, "real1") == 0 );
assert( type_code = CCTK_VARIABLE_REAL );
assert( N_elements == 1 );
assert( Util_TableGetReal(handle, &value_real, key_buffer) == 1 );
assert( value_real == 3.5 );

/* ij = 42 */
assert( Util_TableItAdvance(ihandle) == 1 );
type_code = 123456;
N_elements = 54321;
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("ij") );
assert( strcmp(key_buffer, "ij") == 0 );
assert( type_code = CCTK_VARIABLE_REAL );
assert( N_elements == 1 );
  {
CCTK_INT value_int;
assert( Util_TableGetInt(handle, &value_int, key_buffer) == 1 );
assert( value_int == 42 );

assert( Util_TableItAdvance(ihandle) == 0 );
  }
  }
  }
  }
  }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/
/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This program is a standalone test driver for the key/value table system.
 */
int main(void)
{
test_nonexistent_tables();
test_table_create_destroy();

  {
int handle = Util_TableCreate(UTIL_TABLE_FLAGS_DEFAULT);
assert( handle == 0 );
assert( Util_TableSetInt(handle, 42, "foo/") == UTIL_ERROR_TABLE_BAD_KEY );

  {
int HANDLE = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);

#ifdef UTIL_TABLE_DEBUG
printf("--- printing handle=%d table (should be empty)\n", handle);
print_table(handle);
printf("--- about to test set/get on handle=%d table\n", handle);
#endif
test_set_get(handle, false);
test_set_get(HANDLE, true);

test_iterators(handle);
test_delete_key(handle, false);
test_iterators(HANDLE);
test_delete_key(HANDLE, true);

test_set_get_array(handle);
test_set_get_array(HANDLE);

test_set_get_string(handle, false);

  {
int HANDLE2 = test_set_create_from_string();
test_set_get_string(HANDLE2, true);

printf("all ok!\n" );
return 0;
  }
  }
  }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/
/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests that various operations on nonexistent tables
 * and iterators give error returns.
 */
static
  void test_nonexistent_tables(void)
{
assert( Util_TableDestroy(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableQueryFlags(-42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableQueryNKeys(0) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableDeleteKey(-1, "pickle") == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableSetInt(-1, 42, "fourty-two") == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableGetReal(-1, NULL, "something wierd")
        == UTIL_ERROR_BAD_HANDLE );

assert( Util_TableItCreate(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItDestroy(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItQueryIsNull(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItQueryIsNonNull(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItQueryTableHandle(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItQueryKeyValueInfo(42,
                                      0, NULL,
                                      NULL, NULL) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItAdvance(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItResetToStart(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItSetToNull(42) == UTIL_ERROR_BAD_HANDLE );
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests creation and destruction of tables.
 * It also tests
 * - querying flags words
 * - querying NKeys and MaxKeyLength for empty tables
 * - deleting keys from empty tables
 *
 * It assumes that no tables exist when the function is called,
 * and it eventually destroys all the tables it creates.
 *
 * Bugs:
 * Parts of this test are tied to the present implementation -- it
 * uses local variables of the implementation, and it assumes a specific
 * strategy for allocating handles.
 */
static
  void test_table_create_destroy(void)
{
assert( N_tables == 0 );

assert( Util_TableCreate(UTIL_TABLE_FLAGS_DEFAULT) == 0 );
assert( N_tables == 1 );
assert( Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE) == 1 );
assert( N_tables == 2 );
assert( Util_TableCreate(UTIL_TABLE_FLAGS_DEFAULT) == 2 );
assert( N_tables == 3 );
assert( Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE) == 3 );
assert( N_tables == 4 );
assert( get_table_header_ptr(0) != NULL );
assert( get_table_header_ptr(1) != NULL );
assert( get_table_header_ptr(2) != NULL );
assert( get_table_header_ptr(3) != NULL );
assert( Util_TableQueryFlags(0) == UTIL_TABLE_FLAGS_DEFAULT );
assert( Util_TableQueryFlags(1) == UTIL_TABLE_FLAGS_CASE_INSENSITIVE );
assert( Util_TableQueryFlags(2) == UTIL_TABLE_FLAGS_DEFAULT );
assert( Util_TableQueryFlags(3) == UTIL_TABLE_FLAGS_CASE_INSENSITIVE );

assert( Util_TableDeleteKey(3, "pickle") == UTIL_ERROR_TABLE_NO_SUCH_KEY );
assert( Util_TableDeleteKey(3, "Pickle") == UTIL_ERROR_TABLE_NO_SUCH_KEY );
assert( Util_TableDeleteKey(3, "PICKLE") == UTIL_ERROR_TABLE_NO_SUCH_KEY );

assert( Util_TableDestroy(2) == 0 );
assert( N_tables == 3 );
assert( get_table_header_ptr(0) != NULL );
assert( get_table_header_ptr(1) != NULL );
assert( get_table_header_ptr(2) == NULL );
assert( get_table_header_ptr(3) != NULL );

assert( Util_TableCreate(0x43) == 2 );
assert( N_tables == 4 );
assert( get_table_header_ptr(0) != NULL );
assert( get_table_header_ptr(1) != NULL );
assert( get_table_header_ptr(2) != NULL );
assert( Util_TableQueryFlags(2) == 0x43);
assert( get_table_header_ptr(3) != NULL );

assert( Util_TableDestroy(1) == 0 );
assert( N_tables == 3 );
assert( get_table_header_ptr(0) != NULL );
assert( Util_TableQueryNKeys(0) == 0 );
assert( get_table_header_ptr(1) == NULL );
assert( Util_TableQueryMaxKeyLength(1) == UTIL_ERROR_BAD_HANDLE );
assert( get_table_header_ptr(2) != NULL );
assert( Util_TableQueryMaxKeyLength(2) == 0 );
assert( get_table_header_ptr(3) != NULL );
assert( Util_TableDeleteKey(3, "pickle") == UTIL_ERROR_TABLE_NO_SUCH_KEY );

assert( Util_TableDestroy(1) == UTIL_ERROR_BAD_HANDLE );
assert( N_tables == 3 );
assert( get_table_header_ptr(0) != NULL );
assert( get_table_header_ptr(1) == NULL );
assert( get_table_header_ptr(2) != NULL );
assert( get_table_header_ptr(3) != NULL );

assert( Util_TableDestroy(0) == 0 );
assert( Util_TableDestroy(2) == 0 );
assert( Util_TableDestroy(3) == 0 );

assert( N_tables == 0 );
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests set/get of various-typed scalars.
 * It also tests querying NKeys, MaxKeyLength, and some keys.
 *
 * It assumes the table is empty when this function is called;
 * it leaves entries in the table.
 */
static
  void test_set_get(int handle, bool case_insensitive)
{
/*
 * Note we put a test of a type that's guaranteed to be defined...
 * - at the *beginning* of each group of tests, so we can properly
 *   assert whether or not the key was already in table beforehand.
 * - at the *end* of each group of tests, so the final table contents
 *   are known independently of which types are and aren't defined.
 */

/* integers */
CHECK_SET_GET_INT(handle, CCTK_INT, 0, case_insensitive,
                 Util_TableSetInt, Util_TableGetInt);
#ifdef CCTK_INTEGER_PRECISION_2
CHECK_SET_GET_INT(handle, CCTK_INT2, 1, case_insensitive,
                 Util_TableSetInt2, Util_TableGetInt2);
#endif
#ifdef CCTK_INTEGER_PRECISION_4
CHECK_SET_GET_INT(handle, CCTK_INT4, 1, case_insensitive,
                 Util_TableSetInt4, Util_TableGetInt4);
#endif
#ifdef CCTK_INTEGER_PRECISION_8
CHECK_SET_GET_INT(handle, CCTK_INT8, 1, case_insensitive,
                 Util_TableSetInt8, Util_TableGetInt8);
#endif
CHECK_SET_GET_INT(handle, CCTK_INT, 1, case_insensitive,
                 Util_TableSetInt, Util_TableGetInt);
assert( Util_TableQueryNKeys(handle) == 1 );
assert( Util_TableQueryMaxKeyLength(handle) == (int)strlen("int_x") );

/* complex numbers */
CHECK_SET_GET_COMPLEX(handle, CCTK_COMPLEX, 0, case_insensitive,
                     Util_TableSetComplex, Util_TableGetComplex);
#ifdef CCTK_COMPLEX_PRECISION_8
CHECK_SET_GET_COMPLEX(handle, CCTK_COMPLEX8, 1, case_insensitive,
                     Util_TableSetComplex8, Util_TableGetComplex8);
#endif
#ifdef CCTK_COMPLEX_PRECISION_16
CHECK_SET_GET_COMPLEX(handle, CCTK_COMPLEX16, 1, case_insensitive,
                     Util_TableSetComplex16, Util_TableGetComplex16);
#endif
#ifdef CCTK_COMPLEX_PRECISION_32
CHECK_SET_GET_COMPLEX(handle, CCTK_COMPLEX32, 1, case_insensitive,
                     Util_TableSetComplex32, Util_TableGetComplex32);
#endif
CHECK_SET_GET_COMPLEX(handle, CCTK_COMPLEX, 1, case_insensitive,
                     Util_TableSetComplex, Util_TableGetComplex);
assert( Util_TableQueryNKeys(handle) == 2 );
assert( Util_TableQueryMaxKeyLength(handle) == (int)strlen("COMPlex_Z") );

/* reals */
CHECK_SET_GET_REAL(handle, CCTK_REAL, 0, case_insensitive,
                  Util_TableSetReal, Util_TableGetReal);
#ifdef CCTK_REAL_PRECISION_4
CHECK_SET_GET_REAL(handle, CCTK_REAL4, 1, case_insensitive,
                  Util_TableSetReal4, Util_TableGetReal4);
#endif
#ifdef CCTK_REAL_PRECISION_8
CHECK_SET_GET_REAL(handle, CCTK_REAL8, 1, case_insensitive,
                  Util_TableSetReal8, Util_TableGetReal8);
#endif
#ifdef CCTK_REAL_PRECISION_16
CHECK_SET_GET_REAL(handle, CCTK_REAL16, 1, case_insensitive,
                  Util_TableSetReal16, Util_TableGetReal16);
#endif
CHECK_SET_GET_REAL(handle, CCTK_REAL, 1, case_insensitive,
                  Util_TableSetReal, Util_TableGetReal);
assert( Util_TableQueryNKeys(handle) == 3 );
assert( Util_TableQueryMaxKeyLength(handle) == (int)strlen("COMPlex_Z") );

  {
CCTK_INT type_code, N_elements;
assert( Util_TableQueryValueInfo(handle, &type_code, &N_elements, "COMPlex_Z") == 1 );
assert( type_code == CCTK_VARIABLE_COMPLEX );
assert( N_elements == 1 );

assert( Util_TableQueryValueInfo(handle, &type_code, &N_elements, "pickle") == 0 );

assert( Util_TableQueryValueInfo(handle, NULL, NULL, "int_x") == 1 );
assert( Util_TableQueryValueInfo(handle, NULL, NULL, "Int_x")
        == (case_insensitive ? 1 : 0) );
assert( Util_TableQueryValueInfo(handle, NULL, NULL, "real_y")
        == (case_insensitive ? 1 : 0) );
assert( Util_TableQueryValueInfo(handle, NULL, NULL, "COMPLEX_Z")
        == (case_insensitive ? 1 : 0) );
  }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests set/get of various-typed arrays.
 *
 * It assumes the table is empty when this function is called;
 * it leaves entries in the table.
 */
static
  void test_set_get_array(int handle)
{
/* the comments of  test_set_get()  about test ordering, also apply here */

/* integers */
CHECK_SET_GET_INT_ARRAY(handle, CCTK_CHAR, 0,
                       Util_TableSetCharArray, Util_TableGetCharArray);
CHECK_SET_GET_INT_ARRAY(handle, CCTK_INT, 1,
                       Util_TableSetIntArray, Util_TableGetIntArray);
#ifdef CCTK_INTEGER_PRECISION_2
CHECK_SET_GET_INT_ARRAY(handle, CCTK_INT2, 1,
                       Util_TableSetInt2Array, Util_TableGetInt2Array);
#endif
#ifdef CCTK_INTEGER_PRECISION_4
CHECK_SET_GET_INT_ARRAY(handle, CCTK_INT4, 1,
                       Util_TableSetInt4Array, Util_TableGetInt4Array);
#endif
#ifdef CCTK_INTEGER_PRECISION_8
CHECK_SET_GET_INT_ARRAY(handle, CCTK_INT8, 1,
                       Util_TableSetInt8Array, Util_TableGetInt8Array);
#endif
CHECK_SET_GET_INT_ARRAY(handle, CCTK_INT, 1,
                       Util_TableSetIntArray, Util_TableGetIntArray);

/* reals */
CHECK_SET_GET_REAL_ARRAY(handle, CCTK_REAL, 0,
                        Util_TableSetRealArray, Util_TableGetRealArray);
#ifdef CCTK_REAL_PRECISION_4
CHECK_SET_GET_REAL_ARRAY(handle, CCTK_REAL4, 1,
                        Util_TableSetReal4Array, Util_TableGetReal4Array);
#endif
#ifdef CCTK_REAL_PRECISION_8
CHECK_SET_GET_REAL_ARRAY(handle, CCTK_REAL8, 1,
                        Util_TableSetReal8Array, Util_TableGetReal8Array);
#endif
#ifdef CCTK_REAL_PRECISION_16
CHECK_SET_GET_REAL_ARRAY(handle, CCTK_REAL16, 1,
                        Util_TableSetReal16Array, Util_TableGetReal16Array);
#endif
CHECK_SET_GET_REAL_ARRAY(handle, CCTK_REAL, 1,
                        Util_TableSetRealArray, Util_TableGetRealArray);

/* complex numbers */
CHECK_SET_GET_COMPLEX_ARRAY(handle, CCTK_COMPLEX, 0,
                           Util_TableSetComplexArray,
                           Util_TableGetComplexArray);
#ifdef CCTK_COMPLEX_PRECISION_8
CHECK_SET_GET_COMPLEX_ARRAY(handle, CCTK_COMPLEX8, 1,
                           Util_TableSetComplex8Array,
                           Util_TableGetComplex8Array);
#endif
#ifdef CCTK_COMPLEX_PRECISION_16
CHECK_SET_GET_COMPLEX_ARRAY(handle, CCTK_COMPLEX16, 1,
                           Util_TableSetComplex16Array,
                           Util_TableGetComplex16Array);
#endif
#ifdef CCTK_COMPLEX_PRECISION_32
CHECK_SET_GET_COMPLEX_ARRAY(handle, CCTK_COMPLEX32, 1,
                           Util_TableSetComplex32Array,
                           Util_TableGetComplex32Array);
#endif
CHECK_SET_GET_COMPLEX_ARRAY(handle, CCTK_COMPLEX, 1,
                           Util_TableSetComplexArray,
                           Util_TableGetComplexArray);
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests iterating through a table and resetting an iterator.
 * It assumes the initial table contents are those generated by
 * test_get_set() , namely 3 keys "REAL_y", "COMPlex_Z", "int_x".
 *
 * Bugs:
 * This test is tied to the present implementation -- it assumes a
 * specific ordering of table elements returned by an iterator.
 */
static
  void test_iterators(int handle)
{
int ihandle = Util_TableItCreate(handle);
assert( ihandle >= 0 );
assert( Util_TableItQueryTableHandle(ihandle) == handle );
assert( Util_TableItQueryIsNonNull(ihandle) == 1);
assert( Util_TableItQueryIsNull(ihandle) == 0);

/* set up the key buffer */
  {
int max_key_length = Util_TableQueryMaxKeyLength(handle);
assert( max_key_length == (int)strlen("COMPlex_Z") );
  {
const int N_key_buffer = max_key_length + 1;
char *const key_buffer = malloc(N_key_buffer);
assert( key_buffer != NULL );

/* walk the table to verify iterator traversal */
  {
CCTK_INT type_code, N_elements;

/* REAL_y */
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("REAL_y") );
assert( strcmp(key_buffer, "REAL_y") == 0 );
assert( type_code == CCTK_VARIABLE_REAL );
assert( N_elements == 1 );

/* COMPlex_Z */
type_code = 123456;
N_elements = 54321;
assert( Util_TableItAdvance(ihandle) == 1 );
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("COMPlex_Z") );
assert( strcmp(key_buffer, "COMPlex_Z") == 0 );
assert( type_code = CCTK_VARIABLE_COMPLEX );
assert( N_elements == 1 );

/* int_x */
type_code = 123456;
N_elements = 54321;
assert( Util_TableItAdvance(ihandle) == 1 );
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("int_x") );
assert( strcmp(key_buffer, "int_x") == 0 );
assert( type_code = CCTK_VARIABLE_INT );
assert( N_elements == 1 );

/* advance past last table entry ==> "null-pointer" state */
assert( Util_TableItAdvance(ihandle) == 0 );
assert( Util_TableItQueryIsNull(ihandle) == 1);
assert( Util_TableItQueryIsNonNull(ihandle) == 0);

/* advance again ==> stays in "null-pointer" state */
assert( Util_TableItAdvance(ihandle) == 0 );
assert( Util_TableItQueryIsNull(ihandle) == 1);
assert( Util_TableItQueryIsNonNull(ihandle) == 0);
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      0, NULL,
                                      NULL, NULL)
        == UTIL_ERROR_TABLE_ITERATOR_IS_NULL );

/* test reset to starting point */
assert( Util_TableItResetToStart(ihandle) == 1 );
assert( Util_TableItQueryIsNonNull(ihandle) == 1 );
assert( Util_TableItQueryIsNull(ihandle) == 0 );

/* COMPlex_Z */
type_code = 123456;
N_elements = 54321;
assert( Util_TableItAdvance(ihandle) == 1 );
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("COMPlex_Z") );
assert( strcmp(key_buffer, "COMPlex_Z") == 0 );
assert( type_code = CCTK_VARIABLE_COMPLEX );
assert( N_elements == 1 );

/* test reset to "null-pointer" state */
assert( Util_TableItSetToNull(ihandle) == 0 );
assert( Util_TableItQueryIsNull(ihandle) == 1);
assert( Util_TableItQueryIsNonNull(ihandle) == 0);

/* test set to key "REAL_y" */
assert( Util_TableItSetToKey(ihandle, "REAL_y") == 0 );
assert( Util_TableItQueryIsNonNull(ihandle) == 1);
assert( Util_TableItQueryIsNull(ihandle) == 0);
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("REAL_y") );
assert( strcmp(key_buffer, "REAL_y") == 0 );
assert( type_code == CCTK_VARIABLE_REAL );
assert( N_elements == 1 );

free(key_buffer);
  }
  }
  }
}

/******************************************************************************/

/*
 * This function tests deleting table entries.
 * It assumes the initial table contents are those generated by
 * test_get_set() , namely 3 keys {"REAL_y", "COMPlex_Z", "int_x"}.
 *
 * Bugs:
 * This test is tied to the present implementation -- it assumes a
 * specific ordering of table elements returned by an iterator.
 */
static
  void test_delete_key(int handle, bool case_insensitive)
{
/* set up the key buffer */
int max_key_length = Util_TableQueryMaxKeyLength(handle);
assert( max_key_length == (int)strlen("COMPlex_Z") );
  {
const int N_key_buffer = max_key_length + 1;
char *const key_buffer = malloc(N_key_buffer);
assert( key_buffer != NULL );

/*
 * delete the starting table entry "REAL_y"
 * (this is a special case in the implementation)
 */

assert( Util_TableQueryNKeys(handle) == 3 );
assert( Util_TableDeleteKey(handle,
                            case_insensitive ? "rEAL_y" : "REAL_y")
        == 0 );
assert( Util_TableQueryNKeys(handle) == 2 );

/* walk the table again to verify remaining keys {"COMPlex_Z", "int_x"} */
assert( Util_TableQueryNKeys(handle) == 2 );
  {
int ihandle = Util_TableItCreate(handle);
assert( ihandle >= 0 );
assert( Util_TableItQueryTableHandle(ihandle) == handle );
assert( Util_TableItQueryIsNonNull(ihandle) == 1);
assert( Util_TableItQueryIsNull(ihandle) == 0);

/* COMPlex_Z */
  {
CCTK_INT type_code = 123456;
CCTK_INT N_elements = 54321;
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("COMPlex_Z") );
assert( strcmp(key_buffer, "COMPlex_Z") == 0 );
assert( type_code = CCTK_VARIABLE_COMPLEX );
assert( N_elements == 1 );

/* int_x */
type_code = 123456;
N_elements = 54321;
assert( Util_TableItAdvance(ihandle) == 1 );
assert( Util_TableItQueryKeyValueInfo(ihandle,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("int_x") );
assert( strcmp(key_buffer, "int_x") == 0 );
assert( type_code = CCTK_VARIABLE_INT );
assert( N_elements == 1 );

/* advance past last table entry ==> "null-pointer" state */
assert( Util_TableItAdvance(ihandle) == 0 );
assert( Util_TableItQueryIsNull(ihandle) == 1);
assert( Util_TableItQueryIsNonNull(ihandle) == 0);

/* delete the last key "int_x" */
assert( Util_TableDeleteKey(handle,
                            case_insensitive ? "INT_X" : "int_x")
        == 0 );

/* walk the table again to verify remaining key {"COMPlex_Z"} */
assert( Util_TableQueryNKeys(handle) == 1 );
  {
int ihandle2 = Util_TableItCreate(handle);
assert( ihandle2 >= 0 );
assert( Util_TableItQueryTableHandle(ihandle2) == handle );
assert( Util_TableItQueryIsNonNull(ihandle2) == 1);
assert( Util_TableItQueryIsNull(ihandle2) == 0);

/* COMPlex_Z */
type_code = 123456;
N_elements = 54321;
assert( Util_TableItQueryKeyValueInfo(ihandle2,
                                      N_key_buffer, key_buffer,
                                      &type_code, &N_elements)
        == (int)strlen("COMPlex_Z") );
assert( strcmp(key_buffer, "COMPlex_Z") == 0 );
assert( type_code = CCTK_VARIABLE_COMPLEX );
assert( N_elements == 1 );

/* advance past last table entry ==> "null-pointer" state */
assert( Util_TableItAdvance(ihandle2) == 0 );
assert( Util_TableItQueryIsNull(ihandle2) == 1);
assert( Util_TableItQueryIsNonNull(ihandle2) == 0);

/* delete the last key "COMPlex_Z" */
assert( Util_TableQueryNKeys(handle) == 1 );
assert( Util_TableDeleteKey(handle,
                            case_insensitive ? "INT_X" : "int_x")
        == UTIL_ERROR_TABLE_NO_SUCH_KEY );
assert( Util_TableQueryNKeys(handle) == 1 );
assert( Util_TableDeleteKey(handle,
                            case_insensitive ? "compLEX_z" : "COMPlex_Z")
        == 0 );
assert( Util_TableQueryNKeys(handle) == 0 );

  {
/* check that table is indeed now empty */
int ihandle3 = Util_TableItCreate(handle);
assert( ihandle3 >= 0 );
assert( Util_TableItQueryIsNull(ihandle3) == 1);
assert( Util_TableItQueryIsNonNull(ihandle3) == 0);

/* clean up our iterators */
assert( Util_TableItDestroy(ihandle2) == 0 );
assert( Util_TableItDestroy(42) == UTIL_ERROR_BAD_HANDLE );
assert( Util_TableItDestroy(ihandle3) == 0 );
assert( Util_TableItDestroy(ihandle) == 0 );
free(key_buffer);
  }
  }
  }
  }
  }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests
 *	Util_TableSetFromSTring()
 *	Util_TableCreateFromString()
 * It returns the handle of one of the newly-created tables.
 *
 * Bugs:
 * This test is tied to the present implementation -- it assumes a
 * specific ordering of table elements returned by an iterator.
 */
static
  int test_set_create_from_string(void)
{
/*
 * Test an empty string
 */
int handle = Util_TableCreateFromString("");
assert( Util_TableQueryNKeys(handle) == 0 );

/*
 * Test some error cases
 */
assert( Util_TableSetFromString(handle, "foo" ) == UTIL_ERROR_BAD_INPUT );
assert( Util_TableSetFromString(handle, "foo/" ) == UTIL_ERROR_BAD_INPUT );
assert( Util_TableSetFromString(handle, "foo/=12" )
	== UTIL_ERROR_TABLE_BAD_KEY );
assert( Util_TableSetFromString(handle, "foo= 12") == UTIL_ERROR_BAD_INPUT );
assert( Util_TableCreateFromString("foo" ) == UTIL_ERROR_BAD_INPUT );
assert( Util_TableCreateFromString("foo/" ) == UTIL_ERROR_BAD_INPUT );
assert( Util_TableCreateFromString("foo/=12" ) == UTIL_ERROR_TABLE_BAD_KEY );
assert( Util_TableCreateFromString("foo= 12") == UTIL_ERROR_BAD_INPUT );

/*
 * Test some "good" strings
 */
  {
int handle2 = Util_TableCreateFromString("ij=42 real1=3.5; real_e=2.75");
assert( handle2 >= 0 );
assert( Util_TableQueryFlags(handle2) == UTIL_TABLE_FLAGS_CASE_INSENSITIVE );
assert( Util_TableQueryNKeys(handle2) == 3 );
check_table_contents(handle2);

  {
int handle3 = Util_TableCreate(UTIL_TABLE_FLAGS_DEFAULT);
assert( Util_TableSetFromString(handle3, "ij=42 real1=3.5;") == 2);
assert( Util_TableQueryNKeys(handle3) == 2 );
assert( Util_TableSetFromString(handle3, "real_e=2.75") == 1);
assert( Util_TableQueryNKeys(handle3) == 3 );
check_table_contents(handle3);

assert( Util_TableDestroy(handle3) == 0 );

return handle2;
  }
  }
}
#endif  /* UTIL_TABLE_TEST */

/******************************************************************************/

#ifdef UTIL_TABLE_TEST
/*
 * This function tests  Util_Table{Set,Get}String()
 */
static
  void test_set_get_string(int handle, bool case_insensitive)
{
assert( Util_TableSetString(handle, "Germany", "AEI") == 0 );
assert( Util_TableSetString(handle, "Golm", "AEI") == 1 );

  {
CCTK_INT type_code, N_elements;
assert( Util_TableQueryValueInfo(handle,
                                 &type_code, &N_elements,
                                 case_insensitive ? "aei" : "AEI") == 1 );
assert( type_code = CCTK_VARIABLE_CHAR );
assert( N_elements = (int)strlen("Golm") );

  {
const int N_buffer = N_elements+1;
char *const buffer = (char *) malloc(N_buffer);
assert( buffer != NULL );
assert( Util_TableGetCharArray(handle,
                               N_buffer, buffer,
                               "AEI") == (int)strlen("Golm") );
assert( Util_TableGetString(handle,
                            0, NULL,
                            "AEI") == (int)strlen("Golm") );
assert( Util_TableGetString(handle,
                            N_buffer, buffer,
                            case_insensitive ? "aEI" : "AEI")
        == (int)strlen("Golm") );
assert( strcmp(buffer, "Golm") == 0 );

/* check getting string longer than buffer */
assert( Util_TableSetString(handle, "Max-Planck", "famous") == 0 );
type_code = 123;
N_elements = 456;
assert( Util_TableQueryValueInfo(handle,
                                 &type_code, &N_elements,
                                 case_insensitive ? "aei" : "AEI") == 1 );
assert( type_code = CCTK_VARIABLE_CHAR );
assert( N_elements = (int)strlen("Max-Planck") );
assert( Util_TableGetString(handle,
                            N_buffer, buffer,
                            case_insensitive ? "FAMouS" : "famous")
        == UTIL_ERROR_TABLE_STRING_TRUNCATED );
assert( strcmp(buffer, "Max-") == 0 );
  }
  }
}
#endif  /* UTIL_TABLE_TEST */
