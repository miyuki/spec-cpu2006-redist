 /*@@
   @header    Hash.h
   @date      Wed Oct 27 23:28:53 1999
   @author    Tom Goodale
   @desc 
   Header file for hash stuff.
   @enddesc 
   @version $Header: /cactus/Cactus/src/include/util_Hash.h,v 1.4 2000/09/14 16:51:07 goodale Exp $
 @@*/

#ifndef _UTIL_HASH_H_
#define _UTIL_HASH_H_ 1

#ifdef __cplusplus
extern "C" 
{
#endif

typedef struct T_HASH_ENTRY
{
  struct T_HASH_ENTRY *last;
  struct T_HASH_ENTRY *next;
 
  unsigned int hash;

  unsigned int klen;
  char *key;

  void *data;
} iHashEntry;

typedef struct T_HASH
{
  unsigned int size;
  unsigned int fill;
  unsigned int keys;

  iHashEntry **array;
} uHash;

uHash *Util_HashCreate(unsigned int initial_size);

int Util_HashDestroy(uHash *hash, void (*delete_entry)(void *));

int Util_HashStore(uHash *hash, 
                   unsigned int klen, 
                   const char *key, 
                   unsigned int hashval,
                   void *data);

int Util_HashAdd(uHash *hash, 
                 unsigned int klen, 
                 const char *key, 
                 unsigned int hashval, 
                 void *data);

int Util_HashDelete(uHash *hash, 
                    unsigned int klen, 
                    const char *key, 
                    unsigned int hashval);

void *Util_HashData(uHash *hash, 
                    unsigned int klen, 
                    const char *key, 
                    unsigned int hashval);

unsigned int Util_HashHash(unsigned int klen, 
                           const char *key);

#ifdef __cplusplus
}
#endif

#endif /* _UTIL_HASH_H_ */
