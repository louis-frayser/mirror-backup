Attic=/var/tmp/mirror-backup/Attic


default:
	@echo Targets...
	@egrep : Makefile


Attic:
	mkdir -p "$@"
	
trash=-name "*~"
clean:
	@find . ${trash} |cpio -pvdm ${Attic}
	@find . ${trash}  -delete -printf "DELE %p"

run mirror-backup.sh: *.rkt
	racket mirror-backup.rkt

test:   mirror-backup.sh
	script -c "sh -x $<" "test.log"

format fmt:
	raco fmt -i --width 78 *.rkt
