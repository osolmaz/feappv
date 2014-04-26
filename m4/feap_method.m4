# -------------------------------------------------------------
AC_DEFUN([FEAP_SET_METHODS],
[
 AC_ARG_VAR([METHODS], [methods used to build libMesh, e.g. "opt dbg devel". Possibilities include: (opt,dbg,devel,prof,oprof)])


 # accept --with-methods=METHODS.  but default to $METHODS, which is either set
 # by the user already or defaulted above
 AC_ARG_WITH(methods,
             AC_HELP_STRING([--with-methods=METHODS],
                            [methods used to build libMesh (opt,dbg,devel,prof,oprof)]),
             [for method in ${withval} ; do
                # make sure each method specified makes sense
	        case "${method}" in
		    optimized|opt)      ;;
		    debug|dbg)          ;;
		    devel)              ;;
		    profiling|pro|prof) ;;
		    oprofile|oprof)     ;;
                    *)
			AC_MSG_ERROR(bad value ${method} for --with-methods)
			;;
                esac
              done
	      METHODS=${withval}],
              [
	       # default METHOD is opt if not specified.
	        if (test "x${METHODS}" = x); then
  		  METHODS="dbg devel opt"
   		  AC_MSG_RESULT([No build methods specified, defaulting to "$METHODS"])
 		fi
	      ])

 AC_MSG_RESULT([<<< Configuring libMesh with methods "$METHODS" >>>])

 AC_ARG_VAR([libmesh_CPPFLAGS], [User-specified C/C++ preprocessor flags])
 AC_ARG_VAR([libmesh_CXXFLAGS], [User-specified C++ compliation flags])
 AC_ARG_VAR([libmesh_CFLAGS],   [User-specified C compliation flags])

 build_opt=no
 build_dbg=no
 build_devel=no
 build_prof=no
 build_oprof=no

 # define compiler flags for all methods
 CFLAGS_OPT="$CFLAGS_OPT $libmesh_CFLAGS"

 CFLAGS_DBG="$CFLAGS_DBG $libmesh_CFLAGS"

 CFLAGS_DEVEL="$CFLAGS_DEVEL $libmesh_CFLAGS"

 # profiling-specific flags are derivatives of optimized flags
 CFLAGS_PROF="$CFLAGS_OPT $PROFILING_FLAGS"

 CFLAGS_OPROF="$CFLAGS_OPT $OPROFILE_FLAGS"

 #AC_CONFIG_FILES(Make.common:Make.common.in)

 # conditionally compile selected methods
 for method in ${METHODS}; do
     case "${method}" in
         optimized|opt)      build_opt=yes   ;;
         debug|dbg)          build_dbg=yes   ;;
         devel)              build_devel=yes ;;
         profiling|pro|prof) build_prof=yes  ;;
         oprofile|oprof)     build_oprof=yes ;;
         *)
	     AC_MSG_ERROR(bad value ${method} for --with-methods)
	     ;;
     esac
 done

 AM_CONDITIONAL(FEAP_OPT_MODE,   test x$build_opt   = xyes)
 AM_CONDITIONAL(FEAP_DBG_MODE,   test x$build_dbg   = xyes)
 AM_CONDITIONAL(FEAP_DEVEL_MODE, test x$build_devel = xyes)
 AM_CONDITIONAL(FEAP_PROF_MODE,  test x$build_prof = xyes)
 AM_CONDITIONAL(FEAP_OPROF_MODE, test x$build_oprof = xyes)

  # substitute all methods
  AC_SUBST(CFLAGS_DBG)

  AC_SUBST(CFLAGS_DEVEL)

  AC_SUBST(CFLAGS_OPT)

  AC_SUBST(CFLAGS_PROF)

  AC_SUBST(CFLAGS_OPROF)

])
# -------------------------------------------------------------


