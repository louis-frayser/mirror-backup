Attic=/var/tmp/mirror-backup/Attic


default:
	@echo Targets...
	@egrep : Makefile


Attic:
	mkdir -p "$@"
	
find-common=find . -maxdepth 1 -name  "*~" -print
clean:
	@${find-common} |cpio -pvdm ${Attic}
	@${find-common} -delete -exec echo DELE {} +

run mirror-backup.sh: *.rkt
	racket mirror-backup.rkt

test:   mirror-backup.sh
	script -c "sh -x $<" "test.log"

format fmt:
	raco fmt -i --width 78 *.rkt
