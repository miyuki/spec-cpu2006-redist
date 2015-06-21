 /*@@
   @header    ioutil_AdvertisedFiles.h
   @date      Tue 19 Sep 2000
   @author    Thomas Radke
   @desc 
              Structures and function prototypes for advertising files.
   @history
   @endhistory
   @version $Header: /cactus/CactusBase/IOUtil/src/ioutil_AdvertisedFiles.h,v 1.2 2001/10/30 13:56:31 tradke Exp $
 @@*/

#ifndef _IOUTIL_ADVERTISED_FILES_H_
#define _IOUTIL_ADVERTISED_FILES_H_


#ifdef __cplusplus
extern "C"
{
#endif


/* structure describing an advertised file
   It contains pointer to strings holding
     - the thorn which created the file
     - the variable name the file contents data belongs to
     - the MIME type for the file format used
     - a short slice string indication the dimensionality of the data
     - an arbitrary additional description of the file contents */
typedef struct
{
  const char *thorn;
  const char *varname;
  const char *mimetype;
  const char *slice;
  const char *description;
} ioAdvertisedFileDesc;


/* structure containing function pointers for handling with advertised files
   For the moment it contains only a registration routine for new files to
   be advertised. In the future we might also want something like
   unadvertising, or notification when the contents of the file changed. */
typedef struct
{
  int (*advertise) (const cGH *GH,
                    const char *filename,
                    const ioAdvertisedFileDesc *description);
} ioAdvertisedFileListenerCallbacks;


/* register a new listener with its own callbacks for file advertising */
int IOUtil_RegisterAdvertisedFileListener (const cGH *GH,
                                           const char *listener,
                           const ioAdvertisedFileListenerCallbacks *callbacks);

/* advertise a file */
int IOUtil_AdvertiseFile (const cGH *GH,
                          const char *filename,
                          const ioAdvertisedFileDesc *desc);

#ifdef __cplusplus
}
#endif

#endif  /* _IOUTIL_ADVERTISED_FILES_H_ */
