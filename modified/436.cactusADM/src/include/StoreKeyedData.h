 /*@@
   @header    StoreKeyedData.h
   @date      Tue Sep  1 10:45:08 1998
   @author    Tom Goodale
   @desc 
   Contains structures and function prototypes necessary to use
   the StoreKeyedData routines.
   @enddesc 

   @version $Id: StoreKeyedData.h,v 1.1 1998/09/29 16:19:53 goodale Exp $
 @@*/

#ifndef _STOREKEYEDDATA_H_

#define _STOREKEYEDDATA_H_

/* Define a data type for the storage. */
typedef struct PKeyedData
{
  struct PKeyedData *last;
  struct PKeyedData *next;

  int key;

  void *data;
} pKeyedData;

/* Function prototypes. */

/* Store the data. */
int StoreKeyedData(pKeyedData **storage, int key , void *data);

/* Fet the data. */
void *GetKeyedData(pKeyedData *storage, int key);

/* destroy the data. */
void DestroyKeyedData(pKeyedData *storage);

#endif
