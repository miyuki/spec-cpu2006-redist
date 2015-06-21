#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(specsum, 1.2, support@spec.org)
AC_CONFIG_SRCDIR([src/config.h.in])
AC_CONFIG_HEADER([src/config.h])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE(specsum, 1.2)

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
gl_EARLY
gl_INIT
AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h libintl.h locale.h memory.h stdlib.h string.h strings.h sys/file.h sys/param.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STAT
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_STRUCT_TM
AC_TYPE_UINT32_T
AC_CHECK_MEMBERS([struct stat.st_blksize])

# Checks for library functions.
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_ERROR_AT_LINE
dnl AC_FUNC_MALLOC
AC_FUNC_SETVBUF_REVERSED
AC_CHECK_FUNCS([atexit memset setlocale strchr strtoull])

AC_CONFIG_FILES(
                Makefile
                gnulib/Makefile
                src/Makefile
                tests/Makefile
)
AC_OUTPUT
