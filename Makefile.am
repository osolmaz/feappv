#bindir=@bindir@/@host@_@METHOD@
#libdir=@libdir@/@host@_@METHOD@


AUTOMAKE_OPTIONS = subdir-objects
#SUBDIRS = include
ACLOCAL_AMFLAGS = -I m4

# Tools in the auxillary directory
AUX_DIST = build-aux/install-sh \
           build-aux/missing

INCLUDE_DIRS = -I$(abs_srcdir)/include/libfeappv -I$(abs_srcdir)/include/libfeappv_integer8
AM_CFLAGS = -g -Wall -O2 $(INCLUDE_DIRS)
AM_FFLAGS = -g $(INCLUDE_DIRS)
AM_FCFLAGS = -g $(INLUDE_DIRS)
AM_LDFLAGS = -L/usr/X11R6/lib -lX11 -lm

EXTRA_DIST = src/libfeappv_SOURCES \
	     src/rebuild_libfeappv_SOURCES.sh \
	     include/rebuild_include_HEADERS.sh \
	     include/include_HEADERS

#includedir=$(prefix)/include/libfeappv
include src/libfeappv_la_SOURCES
include include/include_HEADERS

#libfeappv_HEADERS = $(include_HEADERS)

#libfeappvdir = lel

# this lists the binaries to produce, the (non-PHONY, binary) targets in
# the previous manual Makefil

lib_LTLIBRARIES  = libfeappv.la
#libfeappv_SOURCES  = $(libfeappv_SOURCES)




bin_PROGRAMS = feappv 
feappv_SOURCES = src/apps/feappv.f
feappv_LDADD = libfeappv.la

