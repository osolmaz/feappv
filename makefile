
# N.B.  It is necessary to modify 'makefile.in' before using make.
D=./
include $(D)/makefile.in

CLEANDIRS = elements plot program unix user windows main

feappv: archive
	(cd main; make feappv)
	@@echo "--> FEAPpv executable made <--"

archive:   
	(cd program; make archive)
	(cd elements; make archive)
	(cd plot; make archive)
	(cd unix; make archive)
	(cd user; make archive)
	@@echo "--> FEAPpv Archive updated <--"

install: archive feappv

clean:
	for i in $(CLEANDIRS); do (cd $$i; make clean); done
	if [ -f $(ARFEAP) ]; then rm $(ARFEAP); fi
	@echo $(ARFEAP)
	@@echo "--> FEAPpv cleaned <--"

cleanbackup:
	find ./ -name '*~' -exec rm '{}' \; -print -or -name ".*~" -exec rm {} \; -print
