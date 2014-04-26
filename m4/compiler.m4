# -------------------------------------------------------------
# -------------------------------------------------------------
AC_DEFUN([FEAP_SET_COMPILERS],
[


  # --------------------------------------------------------------
  # look for a decent C compiler or honor --with-cc=...
  CC_TRY_LIST="gcc icc pgcc cc"
  AC_ARG_WITH([cc],
  	    AC_HELP_STRING([--with-cc=CC],
                             [C compiler to use]),
  	    [CC="$withval"],
  	    [])

  # --------------------------------------------------------------
  # Determine a C compiler to use.  If CC is not already set, checks for
  # gcc, cc, and other C compilers.  Then sets the CC variable to the result.
  # --------------------------------------------------------------
  AC_PROG_CC([$CC_TRY_LIST])
  # --------------------------------------------------------------
  
  AC_PROG_F77()

  # --------------------------------------------------------------
  # libMesh itself is not written in any Fortran and does not need
  # a Fortran compiler.  Many optional packages however are and
  # we need the compiler to figure out how to link those libraries
  #
  # note though than on OSX for example the XCode tools provide
  # a 'mpif77' which will be detected below but is actually an
  # emtpy shell script wrapper.  Then the compiler will fail to
  # make executables and we will wind up with static libraries
  # due to a bizarre chain of events.  So, add support for
  # --disable-fortran
  # --------------------------------------------------------------
  AC_ARG_ENABLE(fortran,
                AC_HELP_STRING([--enable-fortran],
                               [build with Fortran language support]),
		[case "${enableval}" in
		  yes)  enablefortran=yes ;;
		   no)  enablefortran=no ;;
 		    *)  AC_MSG_ERROR(bad value ${enableval} for --enable-fortran) ;;
		 esac],
		 [enablefortran=yes])



    # look for a decent F90+ compiler or honor --with-fc=...
    FC_TRY_LIST="gfortran ifort pgf90 xlf95"
    AC_ARG_WITH([fc],
    	    AC_HELP_STRING([--with-fc=FC],
                               [Fortran compiler to use]),
    	    [FC="$withval"],
    	    [])

    # --------------------------------------------------------------
    # Determine a F90+ compiler to use.
    # --------------------------------------------------------------
    AC_PROG_FC([$FC_TRY_LIST])

    if (test "x$FC" = "x"); then
      AC_MSG_RESULT(>>> No valid Fortran compiler <<<)
      FC=no
      enablefortran=no
    fi
    # --------------------------------------------------------------



    # --------------------------------------------------------------
    # look for a decent F77 compiler or honor --with-77=...
    F77_TRY_LIST="gfortran g77 ifort f77 xlf frt pgf77 fort77 fl32 af77 f90 xlf90 pgf90 epcf90 f95 fort xlf95 ifc efc pgf95 lf95"
    AC_ARG_WITH([f77],
    	    AC_HELP_STRING([--with-f77=F77],
                               [Fortran compiler to use]),
    	    [F77="$withval"],
    	    [])

    # --------------------------------------------------------------
    # Determine a F77 compiler to use.
    # --------------------------------------------------------------
    AC_PROG_F77([$F77_TRY_LIST])

    if (test "x$F77" = "x"); then
      AC_MSG_RESULT(>>> No valid Fortran 77 compiler <<<)
      F77=no
      enablefortran=no
    fi

    # --------------------------------------------------------------
  else
      # when --disable-fortran is specified, explicitly set these
      # to "no" to instruct libtool not to bother with them.
      AC_MSG_RESULT(>>> Disabling Fortran language support per user request <<<)
      FC=no
      F77=no
])


