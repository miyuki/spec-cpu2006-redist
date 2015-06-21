#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Hash.c
   @date      Wed Oct 27 20:27:36 1999
   @author    Tom Goodale
   @desc 
   Generic hash stuff.
   @enddesc 
   @version $Header: /cactus/Cactus/src/util/Hash.c,v 1.10 2001/06/25 20:36:46 goodale Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util_Hash.h"
#include "cctk_Flesh.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/util/Hash.c,v 1.10 2001/06/25 20:36:46 goodale Exp $";

CCTK_FILEVERSION(util_Hash_c)

/********************************************************************
 *********************     Local Data Types   ***********************
 ********************************************************************/

/********************************************************************
 ********************* Local Routine Prototypes *********************
 ********************************************************************/

static iHashEntry *HashFind(uHash *hash, 
                            unsigned int klen, 
                            const char *key, 
                            unsigned int hashval);

static int HashRehash(uHash *hash);

/********************************************************************
 ********************* Other Routine Prototypes *********************
 ********************************************************************/

/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/********************************************************************
 ********************    External Routines   ************************
 ********************************************************************/


 /*@@
   @routine    Util_HashCreate
   @date       Wed Oct 27 23:31:11 1999
   @author     Tom Goodale
   @desc 
   Creates a new hash table. 
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     initial_size
   @vdesc   Initial size of hash table
   @vtype   int
   @vio     in
   @vcomment 
   Should be the size of a typical table.
   @endvar 

   @returntype uHash *
   @returndesc
   The new hash table object.
   @endreturndesc

@@*/
uHash *Util_HashCreate(unsigned int initial_size)
{
  uHash *retval;

  retval = (uHash *)malloc(sizeof(uHash));

  if(retval)
  {
    retval->size = 2;

    /* Set the initial size to be the first power of two greater than
     * the specified initial size. 
     */
    while(retval->size < initial_size) retval->size *=2;

    retval->fill = 0;
    retval->keys = 0;

    retval->array = (iHashEntry **)calloc(sizeof(iHashEntry *), retval->size);

    if(! retval->array)
    {
      free(retval);

      retval = NULL;
    }

  }

  return retval;
}

 /*@@
   @routine    Util_HashDestroy
   @date       Wed Oct 27 23:32:12 1999
   @author     Tom Goodale
   @desc 
   Destroys a hash table.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   A hash table
   @vtype   uHash *
   @vio     inout
   @vcomment 
   This is the hash table to be destroyed.
   @endvar 
   @var     delete_entry
   @vdesc   Function used to destroy each entry.
   @vtype   void (*)(void *)
   @vio     in
   @vcomment 
   This is called on each entry before the hash table is destroyed.
   @endvar 

   @returntype int
   @returndesc
   Always 0.
   @endreturndesc

@@*/
int Util_HashDestroy(uHash *hash, void (*delete_entry)(void *))
{
  unsigned int size;

  iHashEntry **array;
  iHashEntry *entry;
  iHashEntry *next;

  unsigned int location;

  size  = hash->size;
  array = hash->array;
  
  for(location = 0; location < size; location++)
  {
    for(entry = array[location]; entry; entry = next)
    {
      next = entry->next;
      free(entry->key);
      if(delete_entry)
      {
        delete_entry(entry->data);
      }
      free(entry);
    }
  }
  free(hash->array);
  return 0;
}

 /*@@
   @routine    Util_HashStore
   @date       Wed Oct 27 23:44:14 1999
   @author     Tom Goodale
   @desc 
   Stores a value in the hash table.  If the entry already
   exists it just changes the data stored there.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   A hash table
   @vtype   uHash *
   @vio     inout
   @vcomment 
   This is the hash table to store the data in.
   @endvar 
   @var     klen
   @vdesc   The length of the key passed in
   @vtype   unsigned int
   @vio     in
   @vcomment 
   This is the number of significant bytes of the key. 
   @endvar 
   @var     key
   @vdesc   The key to the data
   @vtype   const char *
   @vio     in
   @vcomment 
   This can in fact be any array of klen bytes, so if you want
   to hash on an integer foo, say, you can pass in 
   (const char *)&foo .
   @endvar 
   @var     hashval
   @vdesc   A precalculated hashvalue
   @vtype   unsigned int
   @vio     in
   @vcomment 
   If this is 0 the default hash calculation is used.
   @endvar 
   @var     data
   @vdesc   Data to be stored with the key
   @vtype   void *
   @vio     in
   @vcomment 
   This is an arbitrary data structure.
   @endvar 

   @returntype int
   @returndesc
   0  - success
   else return value of @seeroutine Util_HashAdd
   @endreturndesc

@@*/
int Util_HashStore(uHash *hash, 
                   unsigned int klen, 
                   const char *key, 
                   unsigned int hashval,
                   void *data)
{
  int retval;
  iHashEntry *entry;

  entry = HashFind(hash, klen, key, hashval);

  if(entry)
  {
    entry->data = data;
    retval = 0;
  }
  else
  {
    retval = Util_HashAdd(hash, klen, key, hashval, data);
  }

  return retval;
}

 /*@@
   @routine    Util_HashAdd
   @date       Wed Oct 27 23:32:40 1999
   @author     Tom Goodale
   @desc 
   Adds a new entry to the hash table.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   A hash table
   @vtype   uHash *
   @vio     inout
   @vcomment 
   This is the hash table to add data to.
   @endvar 
   @var     klen
   @vdesc   The length of the key passed in
   @vtype   unsigned int
   @vio     in
   @vcomment 
   This is the number of significant bytes of the key. 
   @endvar 
   @var     key
   @vdesc   The key to the data
   @vtype   const char *
   @vio     in
   @vcomment 
   This can in fact be any array of klen bytes, so if you want
   to hash on an integer foo, say, you can pass in 
   (const char *)&foo .
   @endvar 
   @var     hashval
   @vdesc   A precalculated hashvalue
   @vtype   unsigned int
   @vio     in
   @vcomment 
   If this is 0 the default hash calculation is used.
   @endvar 
   @var     data
   @vdesc   Data to be stored with the key
   @vtype   void *
   @vio     in
   @vcomment 
   This is an arbitrary data structure.
   @endvar 

   @returntype int
   @returndesc
   0  - success
   -1 - duplicate entry
   -2 - memory failure
   @endreturndesc

@@*/
int Util_HashAdd(uHash *hash, 
                 unsigned int klen, 
                 const char *key, 
                 unsigned int hashval, 
                 void *data)
{
  int retval;
  iHashEntry *entry;
  iHashEntry *lastentry;
  /* FIXME: workaround for Hitachi compiler bug */
  volatile unsigned int location;
  int duplicate;
  unsigned int i;

  /* Calculate the hash value if necessary */
  if(!hashval)
  {
    hashval = Util_HashHash(klen, key);
  }

  /* Get its location in the table */
  location = hashval % hash->size;

  /* See what's there */
  entry = hash->array[location];

  duplicate = 0;

  /* Find the last entry on the list, checking for duplicates 
   * on the way 
   */
  lastentry = NULL;
  while(entry)
  {
    lastentry = entry;
    if(hashval == entry->hash)
    {
      duplicate = 1;
      break;
    }
    else
    {
      entry = entry->next;
    }
  }
  
  if(!duplicate)
  {
    /* Create a new entry */
    entry = (iHashEntry *)malloc(sizeof(iHashEntry));

    if(entry)
    {
      entry->hash = hashval;
      entry->klen = klen;
      entry->key  = (char *)malloc(sizeof(char)*klen);
      entry->data = data;
      entry->next = NULL;

      if(entry->key)
      {
        for(i=0; i < klen; i++)
        {
          entry->key[i] = key[i];
        }

        /* Link it in */
        if(lastentry)
        {
          lastentry->next = entry;
        }
        else
        {
          hash->array[location] = entry;
          hash->fill++;
        }

        entry->last = lastentry;
        hash->keys++;
      }
      else
      {
        free(entry);
        entry = NULL;
      }
    }
  }

  if(duplicate)
  {
    retval = -1;
  }
  else if(!entry)
  {
    retval = -2;
  }
  else
  {
    retval = 0;
  }

  HashRehash(hash);

  return retval;
}

 /*@@
   @routine    Util_HashDelete
   @date       Wed Oct 27 23:33:42 1999
   @author     Tom Goodale
   @desc 
   Deletes an entry from a hash table.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   A hash table
   @vtype   uHash *
   @vio     inout
   @vcomment 
   This is the hash table to delete data from.
   @endvar 
   @var     klen
   @vdesc   The length of the key passed in
   @vtype   unsigned int
   @vio     in
   @vcomment 
   This is the number of significant bytes of the key. 
   @endvar 
   @var     key
   @vdesc   The key to the data
   @vtype   const char *
   @vio     in
   @vcomment 
   This can in fact be any array of klen bytes, so if you want
   to hash on an integer foo, say, you can pass in 
   (const char *)&foo .
   @endvar 
   @var     hashval
   @vdesc   A precalculated hashvalue
   @vtype   unsigned int
   @vio     in
   @vcomment 
   If this is 0 the default hash calculation is used.
   @endvar 

   @returntype int
   @returndesc
   1  - entry found
   0  - non-existent entry
   @endreturndesc

@@*/
int Util_HashDelete(uHash *hash, 
                    unsigned int klen, 
                    const char *key, 
                    unsigned int hashval)
{
  iHashEntry *entry;
  /* FIXME: the volatile qualifier just serves as a workaround
            for the Hitachi compiler bug */
  volatile unsigned int location;

  /* Calculate the hash value if necessary */
  if(!hashval)
  {
    hashval = Util_HashHash(klen, key);
  }

  /* Get its location in the table */
  location = hashval % hash->size;

  /* See what's there */
  entry = hash->array[location];

  /* Find the entry on the list.*/
  for(entry = hash->array[location]; entry; entry = entry->next)
  {
    if(hashval == entry->hash)
    {
      break;
    }
  }

  if(entry)
  {
    /* Maintain the linked list */
    if(entry->next && entry->last)
    {
      entry->next->last = entry->last;
      entry->last->next = entry->next;
    }
    else if(entry->last)
    {
      entry->last->next = NULL;
    }
    else if(entry->next)
    {
      entry->next->last = NULL;
      hash->array[location] = entry->next;
    }
    else
    {
      hash->array[location] = NULL;
      hash->fill--;
    }

    free(entry->key);
    free(entry);
    
    hash->keys--;
  }

  return (entry != NULL);

}

 /*@@
   @routine    Util_HashData
   @date       Wed Oct 27 23:35:17 1999
   @author     Tom Goodale
   @desc 
   Gets the data associated with a hash table entry.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   A hash table
   @vtype   uHash *
   @vio     inout
   @vcomment 
   This is the hash table with the data in.
   @endvar 
   @var     klen
   @vdesc   The length of the key passed in
   @vtype   unsigned int
   @vio     in
   @vcomment 
   This is the number of significant bytes of the key. 
   @endvar 
   @var     key
   @vdesc   The key to the data
   @vtype   const char *
   @vio     in
   @vcomment 
   This can in fact be any array of klen bytes, so if you want
   to hash on an integer foo, say, you can pass in 
   (const char *)&foo .
   @endvar 
   @var     hashval
   @vdesc   A precalculated hashvalue
   @vtype   unsigned int
   @vio     in
   @vcomment 
   If this is 0 the default hash calculation is used.
   @endvar 

   @returntype void *
   @returndesc
   The data associated with the key if found, ogtherwise NULL.
   @endreturndesc

@@*/
void *Util_HashData(uHash *hash, 
                    unsigned int klen, 
                    const char *key, 
                    unsigned int hashval)
{
  void *retval;
  iHashEntry *entry;

  entry = HashFind(hash, klen, key, hashval);

  if(entry)
  {
    retval = entry->data;
  }
  else
  {
    retval = NULL;
  }

  return retval;
}



 /*@@
   @routine    Util_HashHash
   @date       Wed Oct 27 22:15:17 1999
   @author     Tom Goodale
   @desc 
   Util_Hashing function.  I took this from the book 
      'Advanced Perl Programming' 
   published by O'Reilly, and it is apparently the
   algorithm used in Perl, and that is GPL.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     klen
   @vdesc   The length of the key passed in
   @vtype   unsigned int
   @vio     in
   @vcomment 
   This is the number of significant bytes of the key. 
   @endvar 
   @var     key
   @vdesc   The key to the data
   @vtype   const char *
   @vio     in
   @vcomment 
   This can in fact be any array of klen bytes, so if you want
   to hash on an integer foo, say, you can pass in 
   (const char *)&foo .
   @endvar 

   @returntype unsigned int
   @returndesc
   The hash value
   @endreturndesc

@@*/
unsigned int Util_HashHash(unsigned int klen, 
                           const char *key)
{
  unsigned int hash;
  int i;
  const char *pos;

  i   = klen;  
  pos = key;

  hash = 0;

  while(i--)
  {
    hash = hash*33 + *pos++;
  }

#ifdef DEBUG_HASH
  printf("key '%s', length %d, has hash %u\n", key, klen, hash);
#endif

  return hash;
}


/********************************************************************
 *********************     Local Routines   *************************
 ********************************************************************/

 /*@@
   @routine    HashFind
   @date       Wed Oct 27 23:34:09 1999
   @author     Tom Goodale
   @desc 
   Finds an entry in a hash table.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   A hash table
   @vtype   uHash *
   @vio     inout
   @vcomment 
   This is the hash table to find the data in.
   @endvar 
   @var     klen
   @vdesc   The length of the key passed in
   @vtype   unsigned int
   @vio     in
   @vcomment 
   This is the number of significant bytes of the key. 
   @endvar 
   @var     key
   @vdesc   The key to the data
   @vtype   const char *
   @vio     in
   @vcomment 
   This can in fact be any array of klen bytes, so if you want
   to hash on an integer foo, say, you can pass in 
   (const char *)&foo .
   @endvar 
   @var     hashval
   @vdesc   A precalculated hashvalue
   @vtype   unsigned int
   @vio     in
   @vcomment 
   If this is 0 the default hash calculation is used.
   @endvar 

   @returntype iHashEntry *
   @returndesc
   The hash entry if found, otherwise NULL.
   @endreturndesc

@@*/
static iHashEntry *HashFind(uHash *hash, 
                            unsigned int klen, 
                            const char *key, 
                            unsigned int hashval)
{
  iHashEntry *entry;
  /* FIXME: the volatile qualifier just serves as a workaround
            for the Hitachi compiler bug */
  volatile unsigned int location;

  /* Calculate the hash value if necessary */
  if(!hashval)
  {
    hashval = Util_HashHash(klen, key);
  }

  /* Get its location in the table */
  location = hashval % hash->size;

  /* See what's there */
  entry = hash->array[location];

  /* Find the entry on the list.*/
  for(; entry; entry = entry->next)
  {
    if(hashval == entry->hash)
    {
      break;
    }
  }

  return entry;
}


 /*@@
   @routine    HashRehash
   @date       Wed Oct 27 23:34:32 1999
   @author     Tom Goodale
   @desc 
   Resizes a hash table.
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 
   @var     hash
   @vdesc   The hash table to rehash
   @vtype   uHash *
   @vio     inout
   @vcomment 
   
   @endvar 

   @returntype int
   @returndesc
   0  - success
   -1 - memory failure
   @endreturndesc

@@*/
static int HashRehash(uHash *hash)
{
  int retval;

  unsigned int old_size;
  unsigned int new_size;

  unsigned int old_location;
  /* FIXME: the volatile qualifier just serves as a workaround
            for the Hitachi compiler bug */
  volatile unsigned int location;

  unsigned int new_fill;

  iHashEntry **oldarray;
  iHashEntry **newarray;

  iHashEntry *entry;
  iHashEntry *next;
  iHashEntry *entry2;

  if(hash->keys > hash->fill)
  {
    old_size = hash->size;
    new_size = hash->size*2;
    newarray = (iHashEntry **)calloc(sizeof(iHashEntry *), new_size);
    oldarray = hash->array;
    new_fill = 0;

#ifdef DEBUG_HASH
    printf("Resizing hash from %u to %u - fill is %u\n", old_size, new_size, hash->fill);
#endif


    if(newarray)
    {
      for(old_location = 0; old_location < old_size; old_location++)
      {
        /* Move these entries to new array */
        for(entry = oldarray[old_location]; entry; entry = next)
        {
          next = entry->next;

          location = entry->hash % new_size;

          if(!newarray[location])
          {
            new_fill++;
            newarray[location] = entry;
            entry->last = NULL;
            entry->next = NULL;
          }
          else
          {
            /* Find the end of this list */
            for(entry2 = newarray[location]; entry2->next; entry2 = entry2->next);

            /* Add the entry at the end. */
            entry2->next = entry;
            entry->last  = entry2;
            entry->next  = NULL;
          }
        }
      }
      
      /* Free the old data and update the hash. */
      free(hash->array);

      hash->array = newarray;
      hash->fill  = new_fill;
      hash->size  = new_size;
      retval = 0;
    }
    else
    {
      retval = -1;
    }
  }
  else
  {
    retval = 0;
  }

  return retval;
}


/********************************************************************
 *********************     Test Routines    *************************
 ********************************************************************/
  
#ifdef TESUHASH

int main(int argc, char *argv[])
{
  uHash *hash;
  char key[40];

  char *value;
  int i;

  int n_strings;
  int initial_size;

  n_strings = 5;
  initial_size = 0;

  switch(argc)
  {
    case 3: initial_size = atoi(argv[2]);
    case 2: n_strings    = atoi(argv[1]);
  } 

  hash = Util_HashCreate(initial_size);

  for(i = 0; i < n_strings; i++)
  {
    sprintf(key, "key_%d", i);

    value = (char *)malloc(sizeof(char)*(strlen(key)+7));

    sprintf(value, "%s_value", key);

    printf("Adding key %d\n", i);

    Util_HashStore(hash, strlen(key), key, 0, (void *)value);
  }

  for(i = 0; i < n_strings; i++)
  {
    sprintf(key, "key_%d", i);

    printf("Key %s -> '%s'\n", key, (char *)Util_HashData(hash, strlen(key), key, 0));
  }

  for(i = 0; i < n_strings; i++)
  {
    sprintf(key, "key_%d", i);
    Util_HashDelete(hash, strlen(key), key, 0);
  }

  for(i = 0; i < n_strings; i++)
  {
    sprintf(key, "key_%d", i);

    value = (char *)Util_HashData(hash, strlen(key), key, 0);

    if(value)
    {
      printf("Key %s -> '%s', but should be NULL!\n", key, value);
    }
  }

  return 0;
}

#endif /* TEST_HASH */
