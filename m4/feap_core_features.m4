# -------------------------------------------------------------
# -------------------------------------------------------------
AC_DEFUN([FEAP_CORE_FEATURES],
[
AC_MSG_RESULT(---------------------------------------------)
AC_MSG_RESULT(----- Configuring core library features -----)
AC_MSG_RESULT(---------------------------------------------)


# --------------------------------------------------------------
# library warnings - enable by default
# --------------------------------------------------------------
AC_ARG_ENABLE(warnings,
              [AC_HELP_STRING([--enable-warnings],[Display warnings when using deprecated or experimental codes])],
              enablewarnings=$enableval,
              enablewarnings=yes)

AC_SUBST(enablewarnings)
if test "$enablewarnings" != yes ; then
  AC_MSG_RESULT([>>> INFO: Disabling library warnings <<<])
  AC_MSG_RESULT([>>> Configuring library without warnings <<<])
else
  AC_MSG_RESULT([<<< Configuring library with warnings >>>])
  AC_DEFINE(ENABLE_WARNINGS, 1,
           [Flag indicating if the library should have warnings enabled])
fi
# --------------------------------------------------------------




# --------------------------------------------------------------
# legacy include paths - disabled by default
# --------------------------------------------------------------
AC_ARG_ENABLE(legacy-include-paths,
              [AC_HELP_STRING([--enable-legacy-include-paths],[allow for e.g. #include "header.h" instead of #include "feap/header.h"])],
              enablelegacyincludepaths=$enableval,
              enablelegacyincludepaths=no)

AC_SUBST(enablelegacyincludepaths)
if test "$enablelegacyincludepaths" != no ; then
  AC_MSG_RESULT([>>> WARNING: using a legacy option <<<])
  AC_MSG_RESULT([>>> Configuring library to dump old header paths into include args <<<])
else
  AC_MSG_RESULT([<<< Configuring library to require ``include "feap/etc.h"'' style >>>])
fi
# --------------------------------------------------------------




# --------------------------------------------------------------
# Write stack trace output files on error() - disabled by default
# --------------------------------------------------------------
AC_ARG_ENABLE(tracefiles,
              AC_HELP_STRING([--enable-tracefiles],
                             [write stack trace files on unexpected errors]),
              enabletracefiles=$enableval,
              enabletracefiles=$enableeverything)

if test "$enabletracefiles" != no ; then
  AC_DEFINE(ENABLE_TRACEFILES, 1,
           [Flag indicating if the library should be built to write stack trace files on unexpected errors])
  AC_MSG_RESULT(<<< Configuring library with stack trace file support >>>)
fi
# --------------------------------------------------------------


AC_MSG_RESULT(---------------------------------------------)
AC_MSG_RESULT(-- Done configuring core library features ---)
AC_MSG_RESULT(---------------------------------------------)
])
