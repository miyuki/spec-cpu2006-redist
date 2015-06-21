bin_PROGRAMS=rxp
lib_LTLIBRARIES=librxp.la
dist_man_MANS=doc/rxp.1

librxp_la_SOURCES=src/charset.c src/ctype16.c src/dtd.c src/hash.c src/http.c src/infoset-print.c src/input.c src/namespaces.c src/nf16check.c src/nf16data.c src/stdio16.c src/string16.c src/system.c src/url.c src/version.c src/xmlparser.c \
catalog/catalog.c catalog/catalog_dtd.c catalog/catutil.c catalog/entityopener.c catalog/resolve.c \
src/iso-8859/iso-8859-10 src/iso-8859/iso-8859-11 src/iso-8859/iso-8859-13 src/iso-8859/iso-8859-14 src/iso-8859/iso-8859-15 src/iso-8859/iso-8859-2 src/iso-8859/iso-8859-3 src/iso-8859/iso-8859-4 src/iso-8859/iso-8859-5 src/iso-8859/iso-8859-6 src/iso-8859/iso-8859-7 src/iso-8859/iso-8859-8 src/iso-8859/iso-8859-9

pkginclude_HEADERS=include/charset.h include/ctype16.h include/dtd.h include/hash.h include/http.h include/infoset-print.h include/input.h include/namespaces.h include/nf16check.h include/nf16data.h include/rxputil.h include/stdio16.h include/string16.h include/system.h include/url.h include/version.h include/win32.h include/xmlparser.h \
catalog/catalog.h catalog/catutil.h

AM_CPPFLAGS=-I$(srcdir)/include -I$(srcdir)/catalog -DCHAR_SIZE=16

rxp_SOURCES=src/rxp.c 
rxp_LDADD=librxp.la

EXTRA_DIST=doc/COPYING doc/COPYRIGHT doc/Manual doc/Threads
