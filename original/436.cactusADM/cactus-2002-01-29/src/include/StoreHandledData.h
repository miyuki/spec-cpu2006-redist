 /*@@
   @header    StoreHandledData.h
   @date      Tue Feb  2 10:56:31 1999
   @author    Tom Goodale
   @desc 
   Header file for handled data routines
   @enddesc
   @version $Id: StoreHandledData.h,v 1.6 2001/07/03 21:49:42 tradke Exp $
 @@*/

#ifndef _STOREHANDLEDDATA_H_

#define _STOREHANDLEDDATA_H_

/* Define a data types for the storage. */
typedef struct 
{
  unsigned int in_use;
  char *name;
  void *data;
} cHandleStorage;

typedef struct
{
  cHandleStorage *array;
  unsigned array_size;
  unsigned first_unused;
} cHandledData;

/* Function prototypes. */

#ifdef __cplusplus 
extern "C" 
{
#endif

int Util_NewHandle(cHandledData **storage, const char *name, void *data);
int Util_DeleteHandle(cHandledData *storage, int handle);
void *Util_GetHandledData(cHandledData *storage, int handle);
int Util_GetHandle(cHandledData *storage, const char *name, void **data);
const char *Util_GetHandleName(cHandledData *storage, int handle);

#ifdef __cplusplus 
}
#endif

#endif
