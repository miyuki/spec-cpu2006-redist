 /*@@
   @header    StoreNamedData.h
   @date      Tue Sep  1 10:45:08 1998
   @author    Tom Goodale
   @desc 
   Contains structures and function prototypes necessary to use
   the StoreNamedData routines.
   @enddesc 

   @version $Id: StoreNamedData.h,v 1.4 2000/07/14 23:22:04 allen Exp $
 @@*/

#ifndef _STORENAMEDDATA_H_

#define _STORENAMEDDATA_H_

/* Define a data type for the storage. */
typedef struct PNamedData
{
  struct PNamedData *last;
  struct PNamedData *next;

  char *name;

  void *data;
} pNamedData;

/* Function prototypes. */

#ifdef __cplusplus
extern "C" {
#endif

/* Store the data. */
int StoreNamedData(pNamedData **list, const char *name, void *data);

/* Fetch the data. */
void *GetNamedData(pNamedData *list, const char *name);

/* destroy the data. */
void DestroyNamedDataList(pNamedData *list);

#ifdef __cplusplus
}
#endif

#endif
