#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([feappv], [3.1], [onursolmaz@gmail.com], [feappv], [http://kreix.com])

LT_INIT()
#LT_INIT([disable-static])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([subdir-objects])
AM_MAINTAINER_MODE([enable])
: ${CFLAGS=""}
: ${CXXFLAGS=""}

#AC_CONFIG_SRCDIR([include/prflag.h])
#AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_F77
AC_PROG_FC

# Checks for libraries.

# Checks for header files.
AC_PATH_X
AC_CHECK_HEADERS([stdlib.h string.h sys/time.h unistd.h])


# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT(Makefile)
#AC_CONFIG_FILES([Makefile
                 #main/makefile])
#AC_CONFIG_SUBDIRS([bak])
#AC_OUTPUT
