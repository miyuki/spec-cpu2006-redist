#ifdef SPEC_CPU
# define THORN_IS_IOUtil
#endif /* SPEC_CPU */
  /*@@
   @file      AdvertisedFiles.c
   @date      Mon 18 Sep 2000
   @author    Thomas Radke
   @desc
              IOUtil file advertising stuff.
   @enddesc
   @history
   @endhistory
 @@*/

#include <stdlib.h>

#include "cctk.h"
#include "StoreNamedData.h"
#include "ioGH.h"
#include "ioutil_AdvertisedFiles.h"


static const char *rcsid = "$Header: /cactus/CactusBase/IOUtil/src/AdvertisedFiles.c,v 1.5 2001/10/30 13:56:31 tradke Exp $";
CCTK_FILEVERSION(CactusBase_IOUtil_AdvertisedFiles_c)


/********************************************************************
 *********************     Local Data   *****************************
 ********************************************************************/

/* database holding all registered listeners for file advertising */
static pNamedData *listener_DB = NULL;


 /*@@
   @routine    IOUtil_RegisterAdvertisedFileListener
   @date       Wed May 17 2000
   @author     Thomas Radke
   @desc
               Registers a listener with its callbacks for
               advertised file handling
   @enddesc
   @history
   @endhistory
   @var        GH
   @vdesc      pointer to grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        listener
   @vdesc      identification of the listener to be registered
   @vtype      const char *
   @vio        in
   @endvar
   @var        callbacks
   @vdesc      structure containing the listener's callback routines
   @vtype      const ioAdvertisedFileListenerCallbacks *
   @vio        in
   @endvar
   @returntype int
   @returndesc This routine returns
                  0  on success
                  -1 if memory allocation failed or NULL pointers were passed
   @endreturndesc
@@*/

int IOUtil_RegisterAdvertisedFileListener (const cGH *GH, const char *listener,
                     const ioAdvertisedFileListenerCallbacks *callbacks)
{
  int retval = -1;
  ioAdvertisedFileListenerCallbacks *new_callbacks;


  /* prevent compiler warning about unused parameter */
  GH = GH;

  if (listener && callbacks)
  {
    new_callbacks = (ioAdvertisedFileListenerCallbacks *)
                     malloc (sizeof (ioAdvertisedFileListenerCallbacks));
    if (new_callbacks)
    {
      *new_callbacks = *callbacks;
      retval = StoreNamedData (&listener_DB, listener, new_callbacks);
      if (retval)
        free (new_callbacks);
    }
  }

  return (retval);
}


/*@@
   @routine    IOUtil_AdvertiseFile
   @date       Wed May 17 2000
   @author     Thomas Radke
   @desc
               Adds a filename to the list of advertised files for downloading.
               The routine calls all listeners which are currently registered
               for advertised file handling.
   @enddesc
   @history
   @endhistory
   @var        GH
   @vdesc      pointer to grid hierarchy
   @vtype      const cGH *
   @vio        in
   @endvar
   @var        filename
   @vdesc      name of the file to advertise
   @vtype      const char *
   @vio        in
   @endvar
   @var        description
   @vdesc      structure which describes the file
   @vtype      const ioAdvertisedFileDesc *
   @vio        in
   @endvar
   @returntype int
   @returndesc This routine returns the logically OR'ed return values
               of all listeners' advertise callbacks called.
   @endreturndesc
@@*/

int IOUtil_AdvertiseFile (const cGH *GH, const char *filename,
                          const ioAdvertisedFileDesc *description)
{
  int retval = 0;
  pNamedData *listener;
  ioAdvertisedFileListenerCallbacks *callbacks;


  /* get the listener database from IOUtil's GH extension */
  listener = listener_DB;

  /* loop through all listeners' advertise() callbacks */
  while (listener)
  {
    callbacks = (ioAdvertisedFileListenerCallbacks *) listener->data;
    if (callbacks && callbacks->advertise)
      retval |= (callbacks->advertise) (GH, filename, description);
    listener = listener->next;
  }

  return (retval);
}
