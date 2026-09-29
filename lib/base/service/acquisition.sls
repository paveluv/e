;; File acquisition belongs to the base; placement and mode detection do not.
(import (only (foundation edoc) elibrary))
(elibrary (service acquisition)
  (export acquire!)
  (import (chezscheme) (prefix (core property) property:)
          (prefix (foundation string) string:) (prefix (service file) file:)
          (prefix (service log) log:) (prefix (state store) store:)
          (prefix (sys activity) activity:) (prefix (sys sys) sys:))

  (define (disk-facts disk)
    (list (cons 'base (car disk)) (cons 'stamp (cdr disk))
      (cons 'trailing (file:ends-in-newline? (car disk)))))

  (define (reopen! actor id path)
    ;; Capture before disk I/O. A concurrent writer must not attach this
    ;; observation to a different baseline, path or conflict state.
    (let-values ([(text revision facts) (store:snapshot-state id)])
      (let ([base (cond [(assq 'base facts) => cdr] [else #f])])
        (cond
          [(not (and base (equal? (cdr (assq 'file facts)) path))) #f]
          [(not (file-exists? path)) "Cannot reread the file; shared work was retained"]
          [else
           (let* ([identity (sys:file-identity path)] [disk (file:read-state path)])
             (unless (and identity (equal? identity (sys:file-identity path)))
               (error 'reopen! "File identity changed during read" path))
             (cond
               [(string=? base (car disk))
                (store:set-properties! actor id (list (cons 'stamp (cdr disk)))
                  (property:select facts '(file base stamp))) #f]
               [else
                ;; No store lock spans a filesystem read. Compare contents,
                ;; not only mtime, before attempting the guarded transaction.
                (unless (and (equal? (car disk) (car (file:read-state path)))
                             (equal? identity (sys:file-identity path)))
                  (error 'reopen! "Disk changed again; visit it again" path))
                (let ([review (cons revision facts)] [updates (disk-facts disk)])
                  (let-values ([(status detail) (store:reload! actor id (file:lines (car disk)) updates 'any review)])
                    (cond
                      [(eq? status 'applied)
                       (log:add! 'acquisition:reopen!
                         (format "Reloaded ~a with ~a conflicts; the buffer's edits were merged" path (length (cadr detail)))) #f]
                      [(eq? detail 'pending-edits) "Resolve the pending conflicts first; further edits were retained"]
                      [(memq detail '(no-base basis-too-old))
                       (let-values ([(status detail) (store:reread! actor id (file:lines (car disk)) updates 'any review)])
                         (if (eq? status 'applied)
                           (begin (log:add! 'acquisition:reopen!
                                    (format "Reread ~a; undo brings the buffer's text back" path)) #f)
                           (format "File review refused (~a); shared work was retained" detail)))]
                      [else (format "File review refused (~a); shared work was retained" detail)])))]))]))))

  (edoc "Acquire a file or directory in the base without placing it in a window. Missing files and parents are created exclusively; existing shared work is reused and disk changes merge undoably. Returns (directory path) or (buffer id admitted? path diagnostic). Filesystem failures raise."
        (actor actor "requesting head") (path string "absolute path, trailing slash requests a directory")
        (proposal (list-of any) "optional (kind existing-parent filesystem-identity) creation witness")
        (returns list))
  (define (acquire! actor path . proposal)
    (unless (and (string? path) (string:prefix? "/" path) (<= (length proposal) 1))
      (error 'acquire! "expected absolute path and optional creation witness"))
    (activity:call-with
      (lambda ()
        (let* ([directory? (string:suffix? "/" path)] [full (file:canonical path)]
               [expected (and (pair? proposal) (car proposal))]
               [anchor (let parent ([p (file:canonical (file:directory-part full))])
                         (if (file-directory? p) p
                           (if (string=? p "/") (error 'acquire! "no accessible parent" full)
                             (parent (file:canonical (file:directory-part p))))))]
               [resolved (sys:file-identity anchor)])
          (define (check-parent!)
            (unless (and resolved (file-directory? anchor) (equal? resolved (sys:file-identity anchor)))
              (error 'acquire! "Parent directory changed; refresh and try again" anchor))
            (when expected
              (unless (and (list? expected) (= (length expected) 3)
                           (eq? (car expected) (if directory? 'directory 'file))
                           (string? (cadr expected)) (list? (caddr expected))
                           (string:prefix? (file:absolute "" (cadr expected)) full)
                           (file-directory? (cadr expected))
                           (equal? (caddr expected) (sys:file-identity (cadr expected))))
                (error 'acquire! "Creation proposal changed; refresh and try again" full))))
          (check-parent!)
          (when (and expected (file-exists? full #f))
            (error 'acquire! "Proposed entry already exists; refresh and try again" full))
          (cond
            [(or directory? (file-directory? full))
             (let ([identity (file:make-directories! full check-parent!)])
               (check-parent!)
               (unless (equal? identity (sys:file-identity full)) (error 'acquire! "Directory changed during creation" full)))
             (list 'directory (file:visit-path full))]
            [else
             (let* ([path (file:visit-path full)] [existing (store:find-file path)])
               (if existing
                 (list 'buffer existing #f path
                   (guard (ex [else (format "Cannot reread ~a; shared work was retained" path)])
                     (reopen! actor existing path)))
                 (begin
                   (unless (file-exists? full #f)
                     (let* ([parent (file:directory-part full)] [identity (file:make-directories! parent check-parent!)])
                       (check-parent!)
                       (unless (equal? identity (sys:file-identity parent)) (error 'acquire! "Parent directory changed" parent))
                       (guard (ex [(and (not expected) (i/o-file-already-exists-error? ex)) (void)] [else (raise ex)])
                         (file:create! full))))
                   (check-parent!)
                   (let* ([identity (sys:file-identity full)] [path (file:visit-path full)] [disk (file:read-state path)])
                     (check-parent!)
                     (unless (and identity (equal? identity (sys:file-identity full))) (error 'acquire! "File identity changed during read" full))
                     (let-values ([(id admitted?)
                                   (store:visit! actor (file:base-name path) (file:lines (car disk))
                                     (cons (cons 'file path) (disk-facts disk)))])
                       (list 'buffer id admitted? path #f))))))]))))))
