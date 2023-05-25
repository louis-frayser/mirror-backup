#lang debug racket ;; app-data.rkt
(provide %excludes
         %rsync-exclude-flags
         %default-rsync-flags
         %rsync-flags
         backup-plans
         (record-out backup-plan)
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
  (name src-paths target-path ignore-paths merge-on-target? need-mounted-target?))

;;; Semantics
;;; 1. It's implied that mountpoints are not traversed
;;;    To include submounts: add '(#:allmounts "top-mount-dir") to the list:
;;;    (("regular-dir" "another regular") (#:all-mounts "topdir"...))
;;; 2. The #:name is use to create a top-level directory in the destination
;;;    If name is 'by-host/hostname instead of a string, then
;;;    "by-host/$(hostname)" is used as the name.
;;; 3. Merge-on-target? means target name is basename on backp; so
;;;    srcs: a/b/c d/e/f becore target/b/c and target/e/f
;;; 4. If need-mounted-target? is true the target must be a mountpoint
;;;    this is security for cases requring an encrypting destinatin directory.
;;; 5. Sensitive? => log to a encrypted directory.
(define (backup-plans)
  (list
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #t
    #:name "encdocs"
    #:need-mounted-target? #t
    #:src-paths '(("/volumes/crypt/encdocs"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #f
    #:name "export"
    #:need-mounted-target? #t
    #:src-paths '((#:all-mounts "/export"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #t
    #:name "lucho"
    #:need-mounted-target? #f
    #:src-paths '(("/usr/lucho"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #f
    #:name 'by-host/hostname
    #:need-mounted-target? #f
    #:src-paths '(("/etc" "/var/log" "/root"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #f
    #:name "archives"
    #:need-mounted-target? #f
    #:src-paths
    '(("/volumes/arc/dskimgs" "/volumes/work")
      (#:all-mounts "/volumes/arc/archives"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #f
    #:name "cache"
    #:need-mounted-target? #f
    #:src-paths '(("/var/db/repos"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #f
    #:name "srv"
    #:need-mounted-target? #f
    #:src-paths '((#:all-mounts "/srv"))
    #:target-path 'default)
   (backup-plan
    #:ignore-paths '()
    #:merge-on-target? #f
    #:name "the_crypt/xv"
    #:need-mounted-target? #t
    #:src-paths '((#:all-mounts "/volumes/crypt/the-crypt/xv"))
    #:target-path 'alternate)
   (backup-plan
    #:ignore-paths '("the_crypt/xv")
    #:merge-on-target? #f
    #:name "the_crypt"
    #:need-mounted-target? #t
    #:src-paths '((#:all-mounts "/volumes/crypt/the-crypt"))
    #:target-path 'default)))

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
    (define custom-excludes
      (apply string-append (map (lambda(pth)(format " --exclude ~s " pth))
                                (backup-plan-ignore-paths a-plan))))
    (let* ([fn (if (backup-plan-merge-on-target? a-plan)
                   (lambda (p) (string-append p "/"))
                   (lambda (p) p))]
           [srcs-str (strings->string
                      (map fn (backup-plan-src-paths a-plan)))]
           [dest-dir (backup-plan-target-path a-plan)]
           [dest-subdir
            (string-append dest-dir "/" (backup-plan-name a-plan) "/")])
      (format
       #<<JCL
if prep_dir  ~s  ~s
then rsync "${RsyncFlags[@]}" ~a ~a ~a
fi
echo~n~n
JCL
       dest-subdir
       (if (backup-plan-need-mounted-target? a-plan) 'need-mount 'false)
       custom-excludes
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
