#lang debug racket
(provide %excludes
         %rsync-exclude-flags
         %default-rsync-flags
         %rsync-flags
         backup-plan
         get-backup-plans
         gen-script
         gen-script
         srcs-alist
         check-for-dup-srcs?
         check-for-dup-srcs?)
(require (only-in srfi/1 lset-intersection))
(require rebellion/type/record)
(require (only-in "../../dev-scheme/racket-hacks/main.rkt"
                  #;racket-hacks
                  strings->string
                  #;get-submounts
                  #;~0
                  #;basename
                  #;tstamp))
;(require file/glob)
;;; ============================================================================
;(define get-plans get-backup-plans)
;;; ============================================================================
(define %excludes
  '(".Trash*" "*~"
              "*.bak"
              "*\\#"
              ".dtrash"
              ".xvscatter"
              "Attic"
              "Cache"
              "CacheStorage"
              ".cache"
              "ccache"
              "dvds"
              "tmp"
              "Code Cache"
              ".thumbnails"
              ".gvfs"
              "_data"))

(define %rsync-exclude-flags
  (string-append "--delete-excluded --delete-before "
                 (apply string-append
                        (map (curry string-append " --exclude ")
                             (map ~s %excludes)))))

(define %default-rsync-flags "-AHXau ")
(define %rsync-flags
  (string-append %default-rsync-flags " -x -v " %rsync-exclude-flags))

(define-record-type
 backup-plan
 (name src-paths ignore-paths merge-on-target? need-mounted-target?))

;;; Semantics
;;; 1. It's implied that mountoints are not traversed
;;; 2. The #:name is use to create a top-level directory in the destination
;;; 3. Merge-on-target? means target name is basename on backp; so
;;;    srcs: a/b/c d/e/f becore target/b/c and target/e/f
;;; 4. If need-mounted-target? is true the target must be a mountpoint
;;;    this is security for cases requring an encrypting destinatin directory.
;;; 5. Sensitive? => log to a encrypted directory.
(define (get-backup-plans #:arcsrcs arcsrcs
                          #:expsrcs expsrcs
                          #:hostname hostname
                          #:srvsrvs srvsrcs
                          #:crysrvs crysrcs)
  (list (backup-plan #:name "encdocs"
                     #:src-paths '("/volumes/crypt/encdocs")
                     #:ignore-paths '()
                     #:merge-on-target? #t
                     #:need-mounted-target? #t)
        (backup-plan #:name "export"
                     #:src-paths expsrcs ; includes /export/home
                     #:ignore-paths '()
                     #:merge-on-target? #f
                     #:need-mounted-target? #t)
        (backup-plan #:name "lucho"
                     #:src-paths '("/usr/lucho")
                     #:ignore-paths '()
                     #:merge-on-target? #t
                     #:need-mounted-target? #f)
        (backup-plan #:name (string-append "by-host/" hostname)
                     ;; /home is /z/USERDATA & redundant
                     #:src-paths '("/etc" "/var/log" "/root")
                     #:ignore-paths '()
                     #:merge-on-target? #f
                     #:need-mounted-target? #f)
        (backup-plan #:name "archives"
                     #:src-paths arcsrcs
                     #:ignore-paths '()
                     #:merge-on-target? #f
                     #:need-mounted-target? #f)
        (backup-plan #:name "cache"
                     #:src-paths '("/var/db/repos")
                     #:ignore-paths '()
                     #:merge-on-target? #f
                     #:need-mounted-target? #f)
        (backup-plan #:name "srv"
                     #:src-paths srvsrcs
                     #:ignore-paths '()
                     #:merge-on-target? #f
                     #:need-mounted-target? #f)
        (backup-plan #:name "the_crypt"
                     #:src-paths crysrcs
                     #:ignore-paths '()
                     #:merge-on-target? #f
                     #:need-mounted-target? #t)))

;; .............................................................................
(define check-for-dup-srcs?
  ;;; Check for dups
  (lambda (srcs-alist)
    (define srcs-pairs (combinations srcs-alist 2))
    (define (lsets-intersect? src-pr)
      (let ([sp1 (car src-pr)] [sp2 (cadr src-pr)])
        (and (cons? (lset-intersection string=? (cdr sp1) (cdr sp2)))
             (cons (car sp1) (car sp2)))))
    (map lsets-intersect? srcs-pairs)
    (define r (filter lsets-intersect? srcs-pairs))

    (define dups (map (lambda (pr) (map car pr)) r))
    (if (pair? dups) dups #f)))

;; .............................................................................
(define (srcs-alist backup-plans)
  (map (lambda (r) (cons (backup-plan-name r) (backup-plan-src-paths r)))
       backup-plans))

;; .............................................................................

(define (gen-script backup-plans)
  (define (gen-plan-backup a-plan)
    a-plan
    (let* ([fn (if (backup-plan-merge-on-target? a-plan)
                   (lambda (p) (string-append p "/"))
                   (lambda (p) p))]
           [srcs-str (strings->string
                      (map fn (backup-plan-src-paths a-plan)))]
           [dest-dir "/volumes/arc/mirror"]
           [dest-subdir
            (string-append dest-dir "/" (backup-plan-name a-plan) "/")])
      (format
       #<<JCL
if prep_dir  ~s  ~s
then rsync "${RsyncFlags[@]}" ~a ~a
fi
echo~n~n
JCL
       dest-subdir
       (if (backup-plan-need-mounted-target? a-plan) 'need-mount 'false)
       srcs-str
       dest-subdir)))

  (string-append
   (format
    #<<HEADER
#! /bin/sh

prep_dir(){
   test -d "$1" || mkdir -p "$1"
   if test "$2" = need-mount
   then mountpoint -q "$1" || mount "$1"
   fi
}~n
RsyncFlags=(~a)~n~n
HEADER
    %rsync-flags)
   (apply string-append (map gen-plan-backup backup-plans))))
