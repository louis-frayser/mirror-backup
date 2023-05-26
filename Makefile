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

uut=mirror-backup-test.sh
test:   mirror-backup.sh
	cp $< ${uut}
	script -c "sh -x ${uut}" "test.log"

clobber: clean
	rm -f ${uut} test.log

format fmt:
	raco fmt -i --width 78 *.rkt
