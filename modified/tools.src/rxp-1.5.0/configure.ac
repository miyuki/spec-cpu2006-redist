dnl # Bugs in this version of RXP are solely the responsibility of SPEC
dnl #AC_INIT(rxp, 1.5.0)
AC_INIT([specrxp], [1.5.0], [support@spec.org])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_TYPE_LONG_LONG_INT
AC_TYPE_LONG_DOUBLE
#AC_CHECK_LIB(z, deflate)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES(Makefile)

AC_PROG_CC
AC_PROG_CC_C89
AC_PROG_LIBTOOL

AC_CANONICAL_HOST
case $host in
  *-*-mingw*) LIBS="-lwsock32 $LIBS";;
esac

AC_SEARCH_LIBS([gethostbyname], [nsl])
AC_SEARCH_LIBS([socket], [socket])

AC_OUTPUT
