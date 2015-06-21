AM_CPPFLAGS = -I$(top_srcdir)/gnulib

bin_PROGRAMS = specmd5sum specsha1sum specsha224sum specsha256sum specsha384sum specsha512sum

noinst_LIBRARIES = libver.a
nodist_libver_a_SOURCES = version.c version.h

LDADD = libver.a $(top_srcdir)/gnulib/libspecsum.a

specmd5sum_SOURCES = md5sum.c
specmd5sum_CPPFLAGS = -DHASH_ALGO_MD5=1 $(AM_CPPFLAGS)
specsha1sum_SOURCES = md5sum.c
specsha1sum_CPPFLAGS = -DHASH_ALGO_SHA1=1 $(AM_CPPFLAGS)
specsha224sum_SOURCES = md5sum.c
specsha224sum_CPPFLAGS = -DHASH_ALGO_SHA224=1 $(AM_CPPFLAGS)
specsha256sum_SOURCES = md5sum.c
specsha256sum_CPPFLAGS = -DHASH_ALGO_SHA256=1 $(AM_CPPFLAGS)
specsha384sum_SOURCES = md5sum.c
specsha384sum_CPPFLAGS = -DHASH_ALGO_SHA384=1 $(AM_CPPFLAGS)
specsha512sum_SOURCES = md5sum.c
specsha512sum_CPPFLAGS = -DHASH_ALGO_SHA512=1 $(AM_CPPFLAGS)
