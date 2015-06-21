/* config.h - Generated from config.h.in by configure on MinGW/MSYS.
 * $Id: config.h 6361 2011-03-03 22:29:36Z cloyce $
 */

/* Define if the compiler is building for multiple architectures of Apple
   platforms at once. */
/* #undef AA_APPLE_UNIVERSAL_BUILD */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to the number of bits in type 'ptrdiff_t'. */
/* #undef BITSIZEOF_PTRDIFF_T */

/* Define to the number of bits in type 'sig_atomic_t'. */
/* #undef BITSIZEOF_SIG_ATOMIC_T */

/* Define to the number of bits in type 'size_t'. */
/* #undef BITSIZEOF_SIZE_T */

/* Define to the number of bits in type 'wchar_t'. */
/* #undef BITSIZEOF_WCHAR_T */

/* Define to the number of bits in type 'wint_t'. */
/* #undef BITSIZEOF_WINT_T */

/* Define to 1 if the `closedir' function returns void instead of `int'. */
/* #undef CLOSEDIR_VOID */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to 1 if // is a file system root distinct from /. */
#define DOUBLE_SLASH_IS_DISTINCT_ROOT 1

/* Define on systems for which file names may have a so-called `drive letter'
   prefix, define this to compute the length of that prefix, including the
   colon. */
#define FILE_SYSTEM_ACCEPTS_DRIVE_LETTER_PREFIX 1

/* Define if the backslash character may also serve as a file name component
   separator. */
#define FILE_SYSTEM_BACKSLASH_IS_FILE_NAME_SEPARATOR 1

/* Define if a drive letter prefix denotes a relative path if it is not
   followed by a file name component separator. */
#define FILE_SYSTEM_DRIVE_PREFIX_CAN_BE_RELATIVE 1

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module close-stream shall be considered present. */
#define GNULIB_CLOSE_STREAM 1

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module freopen-safer shall be considered present. */
#define GNULIB_FREOPEN_SAFER 1

/* Define to 1 when the gnulib module dup2 should be tested. */
#define GNULIB_TEST_DUP2 1

/* Define to 1 when the gnulib module environ should be tested. */
#define GNULIB_TEST_ENVIRON 1

/* Define to 1 when the gnulib module freopen should be tested. */
#define GNULIB_TEST_FREOPEN 1

/* Define to 1 when the gnulib module getdelim should be tested. */
#define GNULIB_TEST_GETDELIM 1

/* Define to 1 when the gnulib module getline should be tested. */
#define GNULIB_TEST_GETLINE 1

/* Define to 1 when the gnulib module getopt-gnu should be tested. */
#define GNULIB_TEST_GETOPT_GNU 1

/* Define to 1 when the gnulib module getpagesize should be tested. */
#define GNULIB_TEST_GETPAGESIZE 1

/* Define to 1 when the gnulib module lstat should be tested. */
#define GNULIB_TEST_LSTAT 1

/* Define to 1 when the gnulib module malloc-posix should be tested. */
#define GNULIB_TEST_MALLOC_POSIX 1

/* Define to 1 when the gnulib module mbrtowc should be tested. */
#define GNULIB_TEST_MBRTOWC 1

/* Define to 1 when the gnulib module mbsinit should be tested. */
#define GNULIB_TEST_MBSINIT 1

/* Define to 1 when the gnulib module memchr should be tested. */
#define GNULIB_TEST_MEMCHR 1

/* Define to 1 when the gnulib module open should be tested. */
#define GNULIB_TEST_OPEN 1

/* Define to 1 when the gnulib module putenv should be tested. */
#define GNULIB_TEST_PUTENV 1

/* Define to 1 when the gnulib module realloc-posix should be tested. */
#define GNULIB_TEST_REALLOC_POSIX 1

/* Define to 1 when the gnulib module setenv should be tested. */
#define GNULIB_TEST_SETENV 1

/* Define to 1 when the gnulib module stat should be tested. */
#define GNULIB_TEST_STAT 1

/* Define to 1 when the gnulib module strerror should be tested. */
#define GNULIB_TEST_STRERROR 1

/* Define to 1 when the gnulib module strtoull should be tested. */
#define GNULIB_TEST_STRTOULL 1

/* Define to 1 when the gnulib module symlink should be tested. */
#define GNULIB_TEST_SYMLINK 1

/* Define to 1 when the gnulib module unsetenv should be tested. */
#define GNULIB_TEST_UNSETENV 1

/* Define to 1 when the gnulib module wctob should be tested. */
#define GNULIB_TEST_WCTOB 1

/* Define to 1 if you have 'alloca' after including <alloca.h>, a header that
   may be supplied by this distribution. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
/* #undef HAVE_ALLOCA_H */

/* Define to 1 if you have the `atexit' function. */
#define HAVE_ATEXIT 1

/* Define to 1 if you have the <bp-sym.h> header file. */
/* #undef HAVE_BP_SYM_H */

/* Define to 1 if you have the declaration of `getc_unlocked', and to 0 if you
   don't. */
#define HAVE_DECL_GETC_UNLOCKED 0

/* Define to 1 if you have the declaration of `getdelim', and to 0 if you
   don't. */
#define HAVE_DECL_GETDELIM 0

/* Define to 1 if you have the declaration of `getenv', and to 0 if you don't.
   */
#define HAVE_DECL_GETENV 1

/* Define to 1 if you have the declaration of `getline', and to 0 if you
   don't. */
#define HAVE_DECL_GETLINE 0

/* Define to 1 if you have the declaration of `getopt_clip', and to 0 if you
   don't. */
#define HAVE_DECL_GETOPT_CLIP 0

/* Define to 1 if you have the declaration of `iswblank', and to 0 if you
   don't. */
#define HAVE_DECL_ISWBLANK 0

/* Define to 1 if you have the declaration of `optreset', and to 0 if you
   don't. */
#define HAVE_DECL_OPTRESET 0

/* Define to 1 if you have the declaration of `program_invocation_name', and
   to 0 if you don't. */
#define HAVE_DECL_PROGRAM_INVOCATION_NAME 0

/* Define to 1 if you have the declaration of `program_invocation_short_name',
   and to 0 if you don't. */
#define HAVE_DECL_PROGRAM_INVOCATION_SHORT_NAME 0

/* Define to 1 if you have the declaration of `setenv', and to 0 if you don't.
   */
#define HAVE_DECL_SETENV 0

/* Define to 1 if you have the declaration of `strerror', and to 0 if you
   don't. */
#define HAVE_DECL_STRERROR 1

/* Define to 1 if you have the declaration of `strerror_r', and to 0 if you
   don't. */
#define HAVE_DECL_STRERROR_R 0

/* Define to 1 if you have the declaration of `unsetenv', and to 0 if you
   don't. */
#define HAVE_DECL_UNSETENV 0

/* Define to 1 if you have the declaration of `wctob', and to 0 if you don't.
   */
#define HAVE_DECL_WCTOB 1

/* Define to 1 if you have the declaration of `__fpending', and to 0 if you
   don't. */
#define HAVE_DECL___FPENDING 0

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the `dup2' function. */
#define HAVE_DUP2 1

/* Define if you have the declaration of environ. */
/* #undef HAVE_ENVIRON_DECL */

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the <features.h> header file. */
/* #undef HAVE_FEATURES_H */

/* Define to 1 if you have the `flockfile' function. */
/* #undef HAVE_FLOCKFILE */

/* Define to 1 if you have the `funlockfile' function. */
/* #undef HAVE_FUNLOCKFILE */

/* Define to 1 if you have the `getdelim' function. */
/* #undef HAVE_GETDELIM */

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getopt_long_only' function. */
#define HAVE_GETOPT_LONG_ONLY 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if the compiler supports one of the keywords 'inline',
   '__inline__', '__inline' and effectively inlines functions marked as such.
   */
#define HAVE_INLINE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `iswblank' function. */
#define HAVE_ISWBLANK 1

/* Define to 1 if you have the `iswcntrl' function. */
#define HAVE_ISWCNTRL 1

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
/* #undef HAVE_LANGINFO_CODESET */

/* Define to 1 if you have the <libintl.h> header file. */
#define HAVE_LIBINTL_H 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if the system has the type `long long int'. */
#define HAVE_LONG_LONG_INT 1

/* Define to 1 if you have the `lstat' function. */
/* #undef HAVE_LSTAT */

/* Define to 1 if your system has a GNU libc compatible `malloc' function, and
   to 0 otherwise. */
#define HAVE_MALLOC 1

/* Define if the 'malloc' function is POSIX compliant. */
/* #undef HAVE_MALLOC_POSIX */

/* Define to 1 if mmap()'s MAP_ANONYMOUS flag is available after including
   config.h and <sys/mman.h>. */
/* #undef HAVE_MAP_ANONYMOUS */

/* Define to 1 if you have the `mbrtowc' function. */
#define HAVE_MBRTOWC 1

/* Define to 1 if you have the `mbsinit' function. */
#define HAVE_MBSINIT 1

/* Define to 1 if <wchar.h> declares mbstate_t. */
#define HAVE_MBSTATE_T 1

/* Define to 1 if you have the `memchr' function. */
#define HAVE_MEMCHR 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* Define to 1 if you have the `mprotect' function. */
#define HAVE_MPROTECT 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the <OS.h> header file. */
/* #undef HAVE_OS_H */

/* Define to 1 if you have the `pathconf' function. */
/* #undef HAVE_PATHCONF */

/* Define to 1 if you have the `posix_fadvise' function. */
/* #undef HAVE_POSIX_FADVISE */

/* Define to 1 if you have the <random.h> header file. */
/* #undef HAVE_RANDOM_H */

/* Define to 1 if atoll is declared even after undefining macros. */
#define HAVE_RAW_DECL_ATOLL 1

/* Define to 1 if btowc is declared even after undefining macros. */
#define HAVE_RAW_DECL_BTOWC 1

/* Define to 1 if canonicalize_file_name is declared even after undefining
   macros. */
/* #undef HAVE_RAW_DECL_CANONICALIZE_FILE_NAME */

/* Define to 1 if chown is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_CHOWN */

/* Define to 1 if dprintf is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_DPRINTF */

/* Define to 1 if dup2 is declared even after undefining macros. */
#define HAVE_RAW_DECL_DUP2 1

/* Define to 1 if dup3 is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_DUP3 */

/* Define to 1 if endusershell is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_ENDUSERSHELL */

/* Define to 1 if environ is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_ENVIRON */

/* Define to 1 if euidaccess is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_EUIDACCESS */

/* Define to 1 if faccessat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FACCESSAT */

/* Define to 1 if fchdir is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCHDIR */

/* Define to 1 if fchmodat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCHMODAT */

/* Define to 1 if fchownat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCHOWNAT */

/* Define to 1 if fcntl is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCNTL */

/* Define to 1 if fpurge is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FPURGE */

/* Define to 1 if fseeko is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FSEEKO */

/* Define to 1 if fstatat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FSTATAT */

/* Define to 1 if fsync is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FSYNC */

/* Define to 1 if ftello is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FTELLO */

/* Define to 1 if ftruncate is declared even after undefining macros. */
#define HAVE_RAW_DECL_FTRUNCATE 1

/* Define to 1 if futimens is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FUTIMENS */

/* Define to 1 if getcwd is declared even after undefining macros. */
#define HAVE_RAW_DECL_GETCWD 1

/* Define to 1 if getdelim is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETDELIM */

/* Define to 1 if getdomainname is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETDOMAINNAME */

/* Define to 1 if getdtablesize is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETDTABLESIZE */

/* Define to 1 if getgroups is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETGROUPS */

/* Define to 1 if gethostname is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETHOSTNAME */

/* Define to 1 if getline is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLINE */

/* Define to 1 if getloadavg is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLOADAVG */

/* Define to 1 if getlogin is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLOGIN */

/* Define to 1 if getlogin_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLOGIN_R */

/* Define to 1 if getpagesize is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETPAGESIZE */

/* Define to 1 if getsubopt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETSUBOPT */

/* Define to 1 if getusershell is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETUSERSHELL */

/* Define to 1 if grantpt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GRANTPT */

/* Define to 1 if initstat_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_INITSTAT_R */

/* Define to 1 if lchmod is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LCHMOD */

/* Define to 1 if lchown is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LCHOWN */

/* Define to 1 if link is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LINK */

/* Define to 1 if linkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LINKAT */

/* Define to 1 if lseek is declared even after undefining macros. */
#define HAVE_RAW_DECL_LSEEK 1

/* Define to 1 if lstat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LSTAT */

/* Define to 1 if mbrlen is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBRLEN 1

/* Define to 1 if mbrtowc is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBRTOWC 1

/* Define to 1 if mbsinit is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBSINIT 1

/* Define to 1 if mbsnrtowcs is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MBSNRTOWCS */

/* Define to 1 if mbsrtowcs is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBSRTOWCS 1

/* Define to 1 if memmem is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MEMMEM */

/* Define to 1 if mempcpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MEMPCPY */

/* Define to 1 if memrchr is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MEMRCHR */

/* Define to 1 if mkdirat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKDIRAT */

/* Define to 1 if mkdtemp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKDTEMP */

/* Define to 1 if mkfifo is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKFIFO */

/* Define to 1 if mkfifoat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKFIFOAT */

/* Define to 1 if mknod is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKNOD */

/* Define to 1 if mknodat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKNODAT */

/* Define to 1 if mkostemp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKOSTEMP */

/* Define to 1 if mkostemps is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKOSTEMPS */

/* Define to 1 if mkstemp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKSTEMP */

/* Define to 1 if mkstemps is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKSTEMPS */

/* Define to 1 if openat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_OPENAT */

/* Define to 1 if pipe is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PIPE */

/* Define to 1 if pipe2 is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PIPE2 */

/* Define to 1 if popen is declared even after undefining macros. */
#define HAVE_RAW_DECL_POPEN 1

/* Define to 1 if pread is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PREAD */

/* Define to 1 if ptsname is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PTSNAME */

/* Define to 1 if pwrite is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PWRITE */

/* Define to 1 if random_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RANDOM_R */

/* Define to 1 if rawmemchr is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RAWMEMCHR */

/* Define to 1 if readlink is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_READLINK */

/* Define to 1 if readlinkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_READLINKAT */

/* Define to 1 if realpath is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_REALPATH */

/* Define to 1 if renameat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RENAMEAT */

/* Define to 1 if rmdir is declared even after undefining macros. */
#define HAVE_RAW_DECL_RMDIR 1

/* Define to 1 if rpmatch is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RPMATCH */

/* Define to 1 if setenv is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SETENV */

/* Define to 1 if setstate_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SETSTATE_R */

/* Define to 1 if setusershell is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SETUSERSHELL */

/* Define to 1 if sleep is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SLEEP */

/* Define to 1 if snprintf is declared even after undefining macros. */
#define HAVE_RAW_DECL_SNPRINTF 1

/* Define to 1 if srandom_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SRANDOM_R */

/* Define to 1 if stat is declared even after undefining macros. */
#define HAVE_RAW_DECL_STAT 1

/* Define to 1 if stpcpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STPCPY */

/* Define to 1 if stpncpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STPNCPY */

/* Define to 1 if strcasestr is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRCASESTR */

/* Define to 1 if strchrnul is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRCHRNUL */

/* Define to 1 if strdup is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRDUP 1

/* Define to 1 if strerror_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRERROR_R */

/* Define to 1 if strncat is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRNCAT 1

/* Define to 1 if strndup is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRNDUP */

/* Define to 1 if strnlen is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRNLEN */

/* Define to 1 if strpbrk is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRPBRK 1

/* Define to 1 if strsep is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRSEP */

/* Define to 1 if strsignal is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRSIGNAL */

/* Define to 1 if strtod is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRTOD 1

/* Define to 1 if strtok_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRTOK_R */

/* Define to 1 if strtoll is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRTOLL 1

/* Define to 1 if strtoull is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRTOULL 1

/* Define to 1 if strverscmp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRVERSCMP */

/* Define to 1 if symlink is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SYMLINK */

/* Define to 1 if symlinkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SYMLINKAT */

/* Define to 1 if tmpfile is declared even after undefining macros. */
#define HAVE_RAW_DECL_TMPFILE 1

/* Define to 1 if ttyname_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_TTYNAME_R */

/* Define to 1 if unlink is declared even after undefining macros. */
#define HAVE_RAW_DECL_UNLINK 1

/* Define to 1 if unlinkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UNLINKAT */

/* Define to 1 if unlockpt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UNLOCKPT */

/* Define to 1 if unsetenv is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UNSETENV */

/* Define to 1 if usleep is declared even after undefining macros. */
#define HAVE_RAW_DECL_USLEEP 1

/* Define to 1 if utimensat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UTIMENSAT */

/* Define to 1 if vdprintf is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_VDPRINTF */

/* Define to 1 if vsnprintf is declared even after undefining macros. */
#define HAVE_RAW_DECL_VSNPRINTF 1

/* Define to 1 if wcrtomb is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCRTOMB 1

/* Define to 1 if wcsnrtombs is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCSNRTOMBS */

/* Define to 1 if wcsrtombs is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSRTOMBS 1

/* Define to 1 if wctob is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCTOB 1

/* Define to 1 if wcwidth is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCWIDTH */

/* Define to 1 if _Exit is declared even after undefining macros. */
#define HAVE_RAW_DECL__EXIT 1

/* Define if the 'realloc' function is POSIX compliant. */
/* #undef HAVE_REALLOC_POSIX */

/* Define to 1 if you have the <search.h> header file. */
#define HAVE_SEARCH_H 1

/* Define to 1 if you have the `setenv' function. */
/* #undef HAVE_SETENV */

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if 'sig_atomic_t' is a signed integer type. */
/* #undef HAVE_SIGNED_SIG_ATOMIC_T */

/* Define to 1 if 'wchar_t' is a signed integer type. */
/* #undef HAVE_SIGNED_WCHAR_T */

/* Define to 1 if 'wint_t' is a signed integer type. */
/* #undef HAVE_SIGNED_WINT_T */

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio_ext.h> header file. */
/* #undef HAVE_STDIO_EXT_H */

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strerror_r' function. */
/* #undef HAVE_STRERROR_R */

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the `strtoull' function. */
#define HAVE_STRTOULL 1

/* Define to 1 if the system has the type `struct random_data'. */
/* #undef HAVE_STRUCT_RANDOM_DATA */

/* Define to 1 if `st_blksize' is member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_BLKSIZE */

/* Define to 1 if you have the `symlink' function. */
/* #undef HAVE_SYMLINK */

/* Define to 1 if you have the <sys/bitypes.h> header file. */
/* #undef HAVE_SYS_BITYPES_H */

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/file.h> header file. */
#define HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/inttypes.h> header file. */
/* #undef HAVE_SYS_INTTYPES_H */

/* Define to 1 if you have the <sys/mman.h> header file. */
/* #undef HAVE_SYS_MMAN_H */

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
/* #undef HAVE_SYS_SOCKET_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <time.h> header file. */
#define HAVE_TIME_H 1

/* Define to 1 if you have the `tsearch' function. */
#define HAVE_TSEARCH 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `unsetenv' function. */
/* #undef HAVE_UNSETENV */

/* Define to 1 if the system has the type `unsigned long long int'. */
#define HAVE_UNSIGNED_LONG_LONG_INT 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define if you have the 'wchar_t' type. */
#define HAVE_WCHAR_T 1

/* Define to 1 if you have the `wctob' function. */
#define HAVE_WCTOB 1

/* Define to 1 if you have the <wctype.h> header file. */
#define HAVE_WCTYPE_H 1

/* Define to 1 if you have the <winsock2.h> header file. */
#define HAVE_WINSOCK2_H 1

/* Define if you have the 'wint_t' type. */
#define HAVE_WINT_T 1

/* Define to 1 if O_NOATIME works. */
#define HAVE_WORKING_O_NOATIME 0

/* Define to 1 if O_NOFOLLOW works. */
#define HAVE_WORKING_O_NOFOLLOW 0

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Define to 1 if you have the `__fpending' function. */
/* #undef HAVE___FPENDING */

#if FILE_SYSTEM_BACKSLASH_IS_FILE_NAME_SEPARATOR
# define ISSLASH(C) ((C) == '/' || (C) == '\\')
#else
# define ISSLASH(C) ((C) == '/')
#endif

/* Define to 1 if `lstat' dereferences a symlink specified with a trailing
   slash. */
/* #undef LSTAT_FOLLOWS_SLASHED_SYMLINK */

/* If malloc(0) is != NULL, define this to 1. Otherwise define this to 0. */
#define MALLOC_0_IS_NONNULL 1

/* Define to a substitute value for mmap()'s MAP_ANONYMOUS flag. */
/* #undef MAP_ANONYMOUS */

/* Define if the mbrtowc function has the NULL pwc argument bug. */
/* #undef MBRTOWC_NULL_ARG1_BUG */

/* Define if the mbrtowc function has the NULL string argument bug. */
/* #undef MBRTOWC_NULL_ARG2_BUG */

/* Define if the mbrtowc function does not return 0 for a NUL character. */
/* #undef MBRTOWC_NUL_RETVAL_BUG */

/* Define if the mbrtowc function returns a wrong return value. */
/* #undef MBRTOWC_RETVAL_BUG */

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* Define to 1 if open() fails to recognize a trailing slash. */
/* #undef OPEN_TRAILING_SLASH_BUG */

/* Name of package */
#define PACKAGE "specsum"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "support@spec.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "specsum"

/* String identifying the packager of this software */
/* #undef PACKAGE_PACKAGER */

/* Packager info for bug reports (URL/e-mail/...) */
/* #undef PACKAGE_PACKAGER_BUG_REPORTS */

/* Packager-specific version information */
/* #undef PACKAGE_PACKAGER_VERSION */

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "specsum 1.2"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "specsum"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.2"

/* the number of pending output bytes on stream `fp' */
#define PENDING_OUTPUT_N_BYTES fp->_ptr - fp->_base

/* Define to the type that is the result of default argument promotions of
   type mode_t. */
#define PROMOTED_MODE_T int

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'ptrdiff_t'. */
/* #undef PTRDIFF_T_SUFFIX */

/* Define to 1 if stat needs help when passed a directory name with a trailing
   slash */
#define REPLACE_FUNC_STAT_DIR 1

/* Define to 1 if stat needs help when passed a file name with a trailing
   slash */
/* #undef REPLACE_FUNC_STAT_FILE */

/* Define this to 1 if strerror is broken. */
#define REPLACE_STRERROR 1

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'sig_atomic_t'. */
/* #undef SIG_ATOMIC_T_SUFFIX */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'size_t'. */
/* #undef SIZE_T_SUFFIX */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if the `S_IS*' macros in <sys/stat.h> do not work properly. */
/* #undef STAT_MACROS_BROKEN */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if strerror_r returns char *. */
/* #undef STRERROR_R_CHAR_P */

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Define to the prefix of C symbols at the assembler and linker level, either
   an underscore or empty. */
#define USER_LABEL_PREFIX _

/* Version number of package */
#define VERSION "1.2"

/* Define to 1 if unsetenv returns void instead of int. */
/* #undef VOID_UNSETENV */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wchar_t'. */
/* #undef WCHAR_T_SUFFIX */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wint_t'. */
/* #undef WINT_T_SUFFIX */

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define for Solaris 2.5.1 so the uint32_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef were allowed, the
   #define below would cause a syntax error. */
/* #undef _UINT32_T */

/* Define to 500 only on HP-UX. */
/* #undef _XOPEN_SOURCE */

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Define to rpl_ if the getopt replacement functions and variables should be
   used. */
#define __GETOPT_PREFIX rpl_

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* A replacement for va_copy, if needed.  */
#define gl_va_copy(a,b) ((a) = (b))

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Work around a bug in Apple GCC 4.0.1 build 5465: In C99 mode, it supports
   the ISO C 99 semantics of 'extern inline' (unlike the GNU C semantics of
   earlier versions), but does not display it by setting __GNUC_STDC_INLINE__.
   __APPLE__ && __MACH__ test for MacOS X.
   __APPLE_CC__ tests for the Apple compiler and its version.
   __STDC_VERSION__ tests for the C99 mode.  */
#if defined __APPLE__ && defined __MACH__ && __APPLE_CC__ >= 5465 && !defined __cplusplus && __STDC_VERSION__ >= 199901L && !defined __GNUC_STDC_INLINE__
# define __GNUC_STDC_INLINE__ 1
#endif

/* Define to rpl_malloc if the replacement function should be used. */
/* #undef malloc */

/* Define to a type if <wchar.h> does not define. */
/* #undef mbstate_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to the type of st_nlink in struct stat, or a supertype. */
#define nlink_t int

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#define restrict __restrict
/* Work around a bug in Sun C++: it does not support _Restrict, even
   though the corresponding Sun C compiler does, which causes
   "#define restrict _Restrict" in the previous line.  Perhaps some future
   version of Sun C++ will work with _Restrict; if so, it'll probably
   define __RESTRICT, just as Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef ssize_t */

/* Define to the type of an unsigned integer type of width exactly 32 bits if
   such a type exists and the standard includes do not define it. */
/* #undef uint32_t */

/* Define as a marker that can be attached to declarations that might not
    be used.  This helps to reduce warnings, such as from
    GCC -Wunused-parameter.  */
#if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
# define _GL_UNUSED __attribute__ ((__unused__))
#else
# define _GL_UNUSED
#endif
/* The name _UNUSED_PARAMETER_ is an earlier spelling, although the name
   is a misnomer outside of parameter lists.  */
#define _UNUSED_PARAMETER_ _GL_UNUSED


/* Define as a macro for copying va_list variables. */
/* #undef va_copy */
