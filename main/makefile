D=../
include $(D)/makefile.in


OBJECTS = feappv.o

SOURCES = $(FSOURCE)feappv.$(FEXT)

feappv: $(OBJECTS) $(SOURCES) $(ARFEAP)
	ranlib $(ARFEAP)
	$(FF) -g -o feappv $(OBJECTS) $(ARFEAP) \
	$(LDOPTIONS)

.f.o:
	$(FF) -c $(FFOPTFLAG) -I$(FINCLUDE) $*.f -o $*.o

.c.o:
	$(CC) -c $(CCOPTFLAG)  $*.c -o $*.o

clean:
	rm -f *.o

