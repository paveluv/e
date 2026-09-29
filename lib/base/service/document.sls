;; Document filesystem operations belong to the base, independently of views.
(import (only (foundation edoc) elibrary))
(elibrary (service document)
  (export acquire! check! reload! reread!)
  (import (chezscheme) (prefix (core property) property:)
          (prefix (foundation string) string:) (prefix (service file) file:)
          (prefix (service log) log:) (prefix (state store) store:)
          (prefix (sys activity) activity:) (prefix (sys sys) sys:))

  (define (disk-facts disk)
    (list (cons 'base (car disk)) (cons 'stamp (cdr disk))
      (cons 'trailing (file:ends-in-newline? (car disk)))))

  (define (fact facts key) (cond [(assq key facts) => cdr] [else #f]))

  (define (read-disk path)
    (let* ([identity (sys:file-identity path)] [disk (file:read-state path)])
      (unless (and identity (equal? identity (sys:file-identity path)))
        (error 'read-disk "File identity changed during read" path))
      (values disk identity)))

  (define (apply-disk! actor id path review disk identity replace?)
    ;; Neither a store lock nor a head event loop owns the read. The disk
    ;; witness and coherent store review both have to survive admission.
    (let-values ([(current current-identity) (read-disk path)])
      (unless (and (equal? identity current-identity) (equal? (car disk) (car current)))
        (error 'apply-disk! "Disk changed again; review the file again" path)))
    (let-values ([(status detail)
                  ((if replace? store:reread! store:reload!) actor id (file:lines (car disk))
                   (disk-facts disk) 'any review)])
      (when (eq? status 'applied)
        (log:add! 'document:apply-disk!
          (if replace?
            (format "Reread ~a; undo brings the buffer's text back" path)
            (let ([n (length (cadr detail))])
              (if (zero? n) (format "Reloaded ~a, the buffer's edits merged" path)
                (format "Reloaded ~a with ~a conflict~a" path n (if (= n 1) "" "s")))))))
      (values status detail)))

  (define (reload-document! actor id replace?)
    (activity:call-with
      (lambda ()
        (let-values ([(text revision facts) (store:snapshot-state id)])
          (let ([path (fact facts 'file)])
            (unless path (error 'reload-document! "This buffer visits no file"))
            (let-values ([(disk identity) (read-disk path)])
              (apply-disk! actor id path (cons revision facts) disk identity replace?)))))))

  (edoc "Reload a document from its file in the base, merging local edits as one undoable action without erasing earlier history. Refuse if its reviewed text or file facts changed during I/O. Returns status and detail, applied with (revision conflicts)."
        (actor actor "requesting actor") (id integer "buffer identity"))
  (define (reload! actor id) (reload-document! actor id #f))

  (edoc "Replace a document with its file's text in the base as one undoable action, settling pending conflicts. Refuse if its reviewed text or file facts changed during I/O. Returns status and detail, applied with the revision."
        (actor actor "requesting actor") (id integer "buffer identity"))
  (define (reread! actor id) (reload-document! actor id #t))

  (edoc "Check a document's disk content against its baseline. An unchanged stamp avoids reading; equal content updates only the reviewed stamp. Unreadable or unvisited files return false. A true result is a hint to request a fresh guarded reload, not permission to overwrite a later state."
        (actor actor "requesting actor") (id integer "buffer identity") (returns boolean))
  (define (check! actor id)
    (activity:call-with
      (lambda ()
        (let-values ([(text revision facts) (store:snapshot-state id)])
          (let ([path (fact facts 'file)] [base (fact facts 'base)])
            (and path base
              (let ([stamp (file:stamp path)])
                (and (not (and stamp (equal? stamp (fact facts 'stamp))))
                  (guard (ex [else #f])
                    (let-values ([(disk identity) (read-disk path)])
                      (if (string=? base (car disk))
                        (begin
                          (store:set-properties! actor id (list (cons 'stamp (cdr disk)))
                            (property:select facts '(file base stamp))) #f)
                        #t)))))))))))

  (define (reopen! actor id path)
    ;; Capture before disk I/O. A concurrent writer must not attach this
    ;; observation to a different baseline, path or conflict state.
    (let-values ([(text revision facts) (store:snapshot-state id)])
      (let ([base (cond [(assq 'base facts) => cdr] [else #f])])
        (cond
          [(not (and base (equal? (cdr (assq 'file facts)) path))) #f]
          [(not (file-exists? path)) "Cannot reread the file; shared work was retained"]
          [else
           (let-values ([(disk identity) (read-disk path)])
             (cond
               [(string=? base (car disk))
                (store:set-properties! actor id (list (cons 'stamp (cdr disk)))
                  (property:select facts '(file base stamp))) #f]
               [else
                (let ([review (cons revision facts)])
                  (let-values ([(status detail) (apply-disk! actor id path review disk identity #f)])
                    (cond
                      [(eq? status 'applied) #f]
                      [(eq? detail 'pending-edits) "Resolve the pending conflicts first; further edits were retained"]
                      [(memq detail '(no-base basis-too-old))
                       (let-values ([(status detail) (apply-disk! actor id path review disk identity #t)])
                         (if (eq? status 'applied)
                           #f
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
