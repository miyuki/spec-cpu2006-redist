 /*@@
   @header    cctk_Parameter.h
   @date      Wed Sep  8 10:46:19 MSZ 1999
   @author    Andre Merzky
   @desc 
   Public defines for parameter stuff
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/cctk_Parameter.h,v 1.11 2001/09/20 21:45:26 tradke Exp $
 @@*/

#ifndef _CCTK_PARAMETER_H
#define _CCTK_PARAMETER_H 1

/* this include file declares all parameter structs/defines which 
 * should be visible and useable for some thorn programmers.
 * At first of all this means (read) access to parameter properties.
 * Direct acces to parameter values (or its data pointers) are not 
 * allowed. The functions/declarations for this are in 
 * ParameterBindings.h, which in turn includes this file.
 */

/* Parameter checking levels */
#define CCTK_PARAMETER_STRICT  0
#define CCTK_PARAMETER_NORMAL  1
#define CCTK_PARAMETER_RELAXED 2

/* these SCOPE* defines are used as flags fo parameter scopes. */

#define SCOPE_GLOBAL        1 /* parameter is visible everywhere             */
#define SCOPE_RESTRICTED    2 /* parameter is visible for friend thorns only */
#define SCOPE_PRIVATE       3 /* parameter is visible for parent thorn only  */
#define SCOPE_NOT_GLOBAL    4 /* parameter is not visible everywhere         */
#define SCOPE_ANY           5 /* parameter scope is undefined/arbitrary      */

#ifdef NEED_PARAMETER_SCOPE_STRINGS
static const char *cctk_parameter_scopes[] = {"GLOBAL", 
                                        "RESTRICTED", 
                                        "PRIVATE", 
                                        "NOT GLOBAL", 
                                        "ANY"};
#endif /* NEED_SCOPE_STRINGS */

/* parameter types  */

#define PARAMETER_KEYWORD   1 /* parameter is keyword  */
#define PARAMETER_STRING    2 /* parameter is string   */
#define PARAMETER_SENTENCE  3 /* parameter is sentence */
#define PARAMETER_INT       4 /* parameter is integer  */
#define PARAMETER_INTEGER   4 /* parameter is integer  */
#define PARAMETER_REAL      5 /* parameter is float    */
#define PARAMETER_BOOLEAN   6 /* parameter is bool     */

#ifdef NEED_PARAMETER_TYPE_STRINGS
static const char *cctk_parameter_type_names[] = {"KEYWORD", 
                                                  "STRING", 
                                                  "SENTENCE", 
                                                  "INTEGER", 
                                                  "REAL", 
                                                  "BOOLEAN"};
#endif /* NEED_PARAMETER_TYPE_STRINGS */

/* parameter set mask flags */
#define PARAMETER_RECOVERY_PRE     0
#define PARAMETER_RECOVERY_IN      1
#define PARAMETER_RECOVERY_POST    2


/* what is a parameter range:
 * list of independent ranges, each with
 *  - orig string (range)
 *  - active flag
 *  - description
 */
typedef struct RANGE
{
    struct RANGE *last;
    struct RANGE *next;
    char         *range;
    char         *origin;
    int           active;
    char         *description;
} t_range;


/* what are parameter properties:
 * everything users may know about paras:
 * - name
 * - thorn it belongs to
 * - scope
 * - description
 * - default value
 * - type
 * - range (see above) 
 * - number of times it has been set
 * - steerable flag
 */
typedef struct PARAM_PROPS
{
  char    *name;
  char    *thorn;
  int      scope;
  
  char    *description;
  char    *defval;

  int      type;
  t_range *range;

  int      n_set;
  int      steerable;

} cParamData;

#ifdef __cplusplus
extern "C" 
{
#endif

/* return the parameter checking level */ 
int CCTK_ParameterLevel(void);

/* set the value of a parameter */
int CCTK_ParameterSet (const char *name,      /* The name of the parameter  */
                       const char *thorn,     /* The originating thorn      */
                       const char *value);    /* The value of the parameter */

/* get the data pointer to and type of a parameter's value */
const void *CCTK_ParameterGet (const char *name,  /* The name of the parameter*/
                               const char *thorn, /* The originating thorn    */
                               int *type);        /* Holds type of parameter  */

/* get the string representation of a parameter's value
   (string should be freed afterwards) */
char *CCTK_ParameterValString (const char *name,   /* The name of the parameter  */
                               const char *thorn); /* The originating thorn      */

/* walk through list of parameters */
int CCTK_ParameterWalk(int first,           /* Get first parameter or not */
                       const char *origin,  /* Origin of this walk  */
                       char **pfullname,    /* Address of string pointer */
                 const cParamData **pdata); /* Address of parameter data ptr */

/* get parameter properties for given parameter/thorn pair */
const cParamData *CCTK_ParameterData (const char *name, 
                                      const char *thorn);

  /* Return number of times a parameter has been set */
int CCTK_ParameterQueryTimesSet(const char *name,
				const char *thorn);


#ifdef __cplusplus
}
#endif

#endif /*  _CCTK_PARAMETER_H */
