Attic=/var/tmp/mirror-backup/Attic


default: clean
	@echo Targets...
	@egrep : Makefile


Attic:
	mkdir -p "$@"
	
trash=-name "*~"
clean:
	@find . ${trash} |cpio -pvdm ${Attic}
	@find . ${trash}  -delete --printf "DELE %p"

mirror-backup.sh: 
	racket mirror-backup.rkt

test:   mirror-backup.sh
	script -c "sh -x $<" "test.log"

