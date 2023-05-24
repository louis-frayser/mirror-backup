#!/usr/bin/racket
#lang debug racket
(require (only-in srfi/1 lset-intersection))
;(require rebellion/type/record)
(require (only-in racket/os gethostname))
(require (only-in "../../dev-scheme/racket-hacks/main.rkt"
                  #;racket-hacks
                  #;strings->string get-submounts ~0 basename tstamp))
;(require file/glob)
(require (only-in
          "app-data.rkt"
          %excludes %rsync-exclude-flags %default-rsync-flags %rsync-flags
          backup-plan  get-backup-plans gen-plan-backup srcs-alist))
;;; ============================================================================

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
((lambda ()
   (define srcs-pairs (combinations %srcs-alist 2))
   (define (lsets-intersect? src-pr)
     (let ([sp1 (car src-pr)] [sp2 (cadr src-pr)])
       (and (cons? (lset-intersection string=? (cdr sp1) (cdr sp2)))
            (cons (car sp1) (car sp2)))))
   (map lsets-intersect? srcs-pairs)
   (define r (filter lsets-intersect? srcs-pairs))

   (define dups (map (lambda (pr) (map car pr)) r))
   (when (pair? dups)
     (error
      'user-error:backup-plans
      (format
       "Each of following plan pairs duplicate some source paths between them: ~s"
       dups)))))
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
}~n
RsyncFlags=(~a)
HEADER
      %rsync-flags))
    (for-each back-it-up! %backup-plans))
  (define (back-it-up! a-plan) (displayln (gen-plan-backup a-plan)))

  (with-output-to-file "mirror-backup.sh"
    writer
    #:exists 'replace
    #:permissions #o750)
  (displayln (file->string "mirror-backup.sh") (current-error-port)))

(eprintf
 "# Run the backup script was written to ~amirror-backup.sh to effect the actual backup.\n"
 (current-directory))
