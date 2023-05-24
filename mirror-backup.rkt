#!/usr/bin/racket
#lang debug racket
(require (only-in racket/os gethostname))
(require (only-in "../../dev-scheme/racket-hacks/main.rkt"
                  get-submounts
                  ~0
                  basename
                  tstamp))
(require (only-in "app-data.rkt"
                  %excludes
                  %rsync-exclude-flags
                  %default-rsync-flags
                  %rsync-flags
                  backup-plan
                  get-backup-plans
                  srcs-alist
                  check-for-dup-srcs?
                  gen-script))
;;; ============================================================================
;;; Semantics
;;; 1. It's implied that mountoints are not traversed
;;; 2. The #:name is use to create a top-level directory in the destination
;;; 3. Merge-on-target? means target name is basename on backp; so
;;;    srcs: a/b/c d/e/f becore target/b/c and target/e/f
;;; 4. If need-mounted-target? is true the target must be a mountpoint
;;;    this is security for cases requring an encrypting destinatin directory.
;;; 5. Sensitive? => log to a encrypted directory.
(define %backup-plans
  (let* ([dir-with-submounts get-submounts]
         [arcsrcs (append (dir-with-submounts "/volumes/arc/archives")
                          '("/volumes/arc/dskimgs" "/volumes/work"))])
    (get-backup-plans
     #:arcsrcs arcsrcs
     #:expsrcs (dir-with-submounts "/export") ; includes /export/home
     #:hostname (gethostname)
     #:srvsrvs (dir-with-submounts "/srv")
     #:crysrvs (dir-with-submounts "/volumes/crypt/the_crypt"))))

;; .............................................................................
;; a-list of (plan-name . plan-srcs) assocs
(define %srcs-alist (srcs-alist %backup-plans))
;; .............................................................................
;;; Check for dups
(let ([dups? (check-for-dup-srcs? %srcs-alist)])
  (when dups?
    (error
     'user-error:backup-plans
     (format
      "Each of following plan pairs duplicate some source paths between them: ~s"
      dups?))))
;; .............................................................................
;;; Verify that each mentioned path is present
(define (get-missing-dirs-app-accum src-assoc missing-assocs)
  (define (excluded? dir)
    (member (basename dir) %excludes))

  (define (available-for-backup? dir)
    (or (excluded? dir) (directory-exists? dir)))

  (let* ([assoc-dirs-out
          (foldl (lambda (dir mdirs)
                   (if (available-for-backup? dir) mdirs (cons dir mdirs)))
                 '()
                 (cdr src-assoc))])
    (if (null? assoc-dirs-out)
        missing-assocs
        (cons (cons (car src-assoc) assoc-dirs-out) missing-assocs))))

(define unviable-subplans (foldr get-missing-dirs-app-accum '() %srcs-alist))
(when (not (null? unviable-subplans))
  (error 'non-viable-plans
         "The following (plan files) sets aren't available '~a'"
         unviable-subplans))
;; .............................................................................
;;; Write resultant script to file...
(with-output-to-file "mirror-backup.sh"
                     (lambda () (display (gen-script %backup-plans)))
                     #:exists 'replace
                     #:permissions #o750)

;; Show the result from file...
(displayln (file->string "mirror-backup.sh") (current-error-port))

(eprintf
 "# Run the backup script that was written to ~amirror-backup.sh to effect the actual backup.\n"
 (current-directory))
