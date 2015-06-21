#
# Makefile for SPEC's hacked up md5sum for MinGW GCC on Windows
# Copyright 2009-2011 Standard Performance Evaluation Corporation
#
# $Id: Makefile 6361 2011-03-03 22:29:36Z cloyce $

all: specmd5sum.exe specsha1sum.exe specsha224sum.exe specsha256sum.exe \
     specsha384sum.exe specsha512sum.exe

CC=gcc
CFLAGS=-O3 -I../src -I../gnulib -I. -DHAVE_CONFIG_H -DNO_MSYS -Wall -Wextra

LIBSRC = $(addprefix ../gnulib/, \
            basename-lgpl.c close-stream.c closeout.c dirname-lgpl.c dup2.c \
            error.c exitfail.c fadvise.c fpending.c freopen-safer.c \
            freopen.c getdelim.c getline.c getopt.c getopt1.c \
            localcharset.c malloc.c md5.c open.c progname.c quote.c \
            quotearg.c realloc.c sha1.c sha256.c sha512.c stat.c strerror.c \
            stripslash.c version-etc-fsf.c version-etc.c xalloc-die.c \
            xfreopen.c xmalloc.c \
         ) \
         $(addprefix ../src/, \
            version.c \
          ) \
          dirent.c

LIBOBJS = $(LIBSRC:.c=.o)

libsum.a: $(LIBOBJS)
	$(AR) r $@ $(LIBOBJS)

spec%sum.exe: ../src/md5sum.c libsum.a
	$(CC) $(CFLAGS) -DHASH_ALGO_$(call uc,$*)=1 -o $@ $< -L. -lsum

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@ 

.c.i:
	$(CC) $(CFLAGS) -C -E $< > $@

clean:
	del /s /f /q ..\gnulib\*.o ..\src\*.o *.o *.exe *.pdb libsum.a

distclean: clean

# This is janky, but there's no tr(1) on Windows...
uc = $(subst a,A,$(subst b,B,$(subst c,C,$(subst d,D,$(subst e,E,$(subst f,F,$(subst g,G,$(subst h,H,$(subst i,I,$(subst j,J,$(subst k,K,$(subst l,L,$(subst m,M,$(subst n,N,$(subst o,O,$(subst p,P,$(subst q,Q,$(subst r,R,$(subst s,S,$(subst t,T,$(subst u,U,$(subst v,V,$(subst w,W,$(subst x,X,$(subst y,Y,$(subst z,Z,$1))))))))))))))))))))))))))

