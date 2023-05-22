#!/usr/bin/racket
#lang debug racket
(require (only-in srfi/1 lset-intersection))
(require rebellion/type/record)
(require (only-in racket/os gethostname))
(require (only-in racket-hacks strings->string get-submounts))
(require file/glob)
(define (string-quote s)
  (format "~s" s))
;;; ============================================================================================
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
              "tmp"
              "Code Cache"
              ".thumbnails"
              ".gvfs"
              "_data"))

(define %rsync-exclude-flags
  (string-append "--delete-excluded --delete-before "
                 (apply string-append (map (curry string-append " --exclude ") (map ~s %excludes)))))

(define %default-rsync-flags "-AHXau ")
(define %rsync-flags (string-append %default-rsync-flags " -x -v " %rsync-exclude-flags))

(define-record-type backup-plan (name src-paths ignore-paths merge-on-target? need-mounted-target?))

;;; Semantics
;;; 1. It's implied that mountoints are not traversed
;;; 2. The #:name is use to create a top-level directory in the destination
;;; 3. Merge-on-target? means target name is basename on backp; so
;;;    srcs: a/b/c d/e/f becore target/b/c and target/e/f
;;; 4. If need-mounted-target? is true the target must be a mountpoint
;;;    this is security for cases requring an encrypting destinatin directory.
;;; 5. Sensitive? => log to a encrypted directory.
(define %backup-plans
  (let* ([vsrcs1 (map (curry string-append "/volumes/") '("av" "work"))]
         [osrcs '("/srv" "/srv/shared")]
         [arcsrcs (append vsrcs1 osrcs)]
         [crypt-srcs (map path->string (glob "/volumes/crypt/the_crypt/*"))])

    (list (backup-plan #:name "encdocs"
                       #:src-paths '("/volumes/crypt/encdocs")
                       #:ignore-paths '()
                       #:merge-on-target? #t
                       #:need-mounted-target? #t)
          (backup-plan #:name "export"
                       #:src-paths (cons "/export" (get-submounts "/export"))
                       #:ignore-paths '()
                       #:merge-on-target? #f
                       #:need-mounted-target? #t)
          (backup-plan #:name "lucho"
                       #:src-paths '("/usr/lucho")
                       #:ignore-paths '()
                       #:merge-on-target? #t
                       #:need-mounted-target? #f)
          (backup-plan #:name (string-append "by-host/" (gethostname))
                       #:src-paths '("/etc" "/var/logs" "/root" "/home")
                       #:ignore-paths '()
                       #:merge-on-target? #f
                       #:need-mounted-target? #f)
          (backup-plan #:name "archive"
                       #:src-paths arcsrcs
                       #:ignore-paths '()
                       #:merge-on-target? #f
                       #:need-mounted-target? #f)
          (backup-plan #:name "cache"
                       #:src-paths '("/var/db/repos")
                       #:ignore-paths '()
                       #:merge-on-target? #f
                       #:need-mounted-target? #f)
          (backup-plan #:name "the_crypt"
                       #:src-paths crypt-srcs
                       #:ignore-paths '()
                       #:merge-on-target? #f
                       #:need-mounted-target? #t))))

;; ................................................................................................
(define (back-it-up! a-plan)
  (let* ([fn (if (backup-plan-merge-on-target? a-plan)
                 (lambda (p) (string-append p "/"))
                 (lambda (p) p))]
         [srcs-str (strings->string (map fn (backup-plan-src-paths a-plan)))]
         [dest-dir "/volumes/arc/mirror"]
         [dest-subdir (string-append dest-dir "/" (backup-plan-name a-plan) "/")])
    (printf #<<JCL

if prep_dir  ~s  ~s
then rsync "${RsyncFlags[@]}" ~a ~a
fi
echo~n
JCL
            dest-subdir
            (if (backup-plan-need-mounted-target? a-plan) 'need-mount 'false)
            srcs-str
            dest-subdir)))
;; ................................................................................................
;;; Check for dups
((lambda ()
   (define srcs-alist
     (map (lambda (r) (cons (backup-plan-name r) (backup-plan-src-paths r))) %backup-plans))
   (define srcs-pairs (combinations srcs-alist 2))
   (define (lsets-intersect? src-pr)
     (let ([sp1 (car src-pr)] [sp2 (cadr src-pr)])
       (and (cons? (lset-intersection string=? (cdr sp1) (cdr sp2))) (cons (car sp1) (car sp2)))))
   (map lsets-intersect? srcs-pairs)
   (define r (filter lsets-intersect? srcs-pairs))

   (define dups (map (lambda (pr) (map car pr)) r))
   (when (pair? dups)
     (error 'user-error:backup-plans
            (format "Each of following plan pairs duplicate some source paths between them: ~s"
                    dups)))))
;; ................................................................................................

(begin
  (define (writer)
    (displayln
     (format
      #<<HEADER
#! /bin/sh

prep_dir(){
   test -d "$1" || mkdir -p "$1"
   if test "$2" = need-mount
   then mountpoint -q "$1" || mount "$1"
   fi
}
RsyncFlags=(~a)
HEADER
      %rsync-flags))
    (for-each back-it-up! %backup-plans))

  (with-output-to-file "mirror-backup.sh" writer #:exists 'replace #:permissions #o750)
  (displayln (file->string "mirror-backup.sh") (current-error-port)))

(eprintf "# Run the backup script was written to ~amirror-backup.sh to effect the actual backup.\n"
         (current-directory))
