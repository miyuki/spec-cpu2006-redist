#
# Makefile for GNU tar on Windows using MinGW GCC
#
# $Id: Makefile 6361 2011-03-03 22:29:36Z cloyce $

CC=gcc

# These settings are for debugging
CFLAGS = -std=gnu99 -ggdb -O0 -fno-inline-functions -Wall -Wextra

# And these aren't
CFLAGS = -std=gnu99 -O3 -Wall

CPPFLAGS = -I. -I../gnu -I../lib -DHAVE_CONFIG_H -DWINVER=0x500

LIBSRC = $(addprefix ../gnu/, \
	   argmatch.c areadlink.c areadlinkat.c argp-ba.c argp-eexst.c \
	   argp-fmtstream.c argp-fs-xinl.c argp-help.c argp-parse.c \
	   argp-pin.c argp-pv.c argp-pvh.c argp-xinl.c argp-version-etc.c \
	   asnprintf.c asprintf.c at-func2.c backupfile.c basename-lgpl.c \
	   basename.c c-ctype.c canonicalize-lgpl.c chdir-long.c chown.c \
	   cloexec.c close-hook.c close-stream.c close.c closeout.c dirfd.c \
	   dirname-lgpl.c dirname.c dup-safer.c dup2.c error.c exclude.c \
	   exitfail.c fchdir.c fchmodat.c fchownat.c fclose.c fcntl.c \
	   fd-safer.c fdopendir.c fdutimensat.c fileblocks.c \
	   filenamecat-lgpl.c fnmatch.c fpending.c fseeko.c fstatat.c \
	   full-write.c futimens.c getdelim.c getdtablesize.c getline.c \
	   getopt.c getopt1.c getpagesize.c gettime.c hash.c human.c \
	   imaxtostr.c inttostr.c lchown.c link.c linkat.c localcharset.c \
	   lseek.c lstat.c malloc.c malloca.c mbchar.c mbscasecmp.c \
	   mempcpy.c memrchr.c mkdir.c mkdirat.c mkdtemp.c mkfifo.c \
	   mkfifoat.c mknod.c mknodat.c modechange.c nl_langinfo.c \
	   obstack.c offtostr.c open.c openat-die.c openat-proc.c openat.c \
	   opendir-safer.c parse-datetime.c pipe-safer.c printf-args.c \
	   printf-parse.c priv-set.c progname.c quote.c quotearg.c \
	   rawmemchr.c readlink.c readlinkat.c realloc.c regex.c rename.c \
	   renameat.c rmdir.c rpmatch.c safe-read.c safe-write.c save-cwd.c \
	   savedir.c setenv.c sleep.c stat.c stpcpy.c strchrnul.c strdup.c \
	   strerror.c stripslash.c strndup.c strnlen.c strnlen1.c symlink.c \
	   symlinkat.c tempname.c time_r.c uinttostr.c umaxtostr.c \
	   uniwidth/width.c unlink.c unlinkat.c unlinkdir.c unsetenv.c \
	   utimens.c utimensat.c vasnprintf.c vasprintf.c version-etc-fsf.c \
	   version-etc.c wcwidth.c xalloc-die.c xasprintf.c xgetcwd.c \
	   xmalloc.c xstrndup.c xstrtol-error.c xstrtol.c xstrtoul.c \
	   xstrtoumax.c xvasprintf.c \
         ) \
	$(addprefix ../lib/, \
	   paxerror.c paxexit.c paxnames.c prepargs.c rtapelib.c stdopen.c \
	 )

TARSRC = $(addprefix ../src/, \
	   buffer.c checkpoint.c compare.c create.c delete.c exit.c \
	   extract.c incremen.c list.c misc.c names.c sparse.c suffix.c \
	   system.c tar.c transform.c unlink.c update.c utf8.c warning.c \
	   xheader.c \
         )

LIBOBJS = $(LIBSRC:.c=.o)
TAROBJS = $(TARSRC:.c=.o)
LIBS = # -lmingwex

all: tar.exe

libtar.a: $(LIBOBJS)
	$(AR) r $@ $(LIBOBJS)

tar.exe: libtar.a $(TAROBJS)
	$(CC) $(CFLAGS) -o $@ $(TAROBJS) libtar.a $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

.c.i:
	$(CC) $(CFLAGS) $(CPPFLAGS) -C -E $< > $@

clean:
	del /q /s /f $(subst /,\\,$(LIBOBJS))
	del /q /s /f $(subst /,\\,$(TAROBJS))
	del /q /s /f libtar.a tar.exe
