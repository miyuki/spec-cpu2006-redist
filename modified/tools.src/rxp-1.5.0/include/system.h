#define SOCKETS_IMPLEMENTED

#define STD_API
#define XML_API

#if defined(HAVE_CONFIG_H)
#include "config.h"
#elif defined(WIN32)
#include "win32.h"
#endif

void *Malloc(int bytes);
void *Realloc(void *mem, int bytes);
void Free(void *mem);
