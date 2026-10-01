;; Document filesystem operations belong to the base, independently of views.
(import (only (foundation edoc) elibrary))
(elibrary (service document)
  (export acquire! check! reload! reread! save!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (foundation string) string:) (prefix (service file) file:)
          (prefix (service log) log:) (prefix (state actor) actor:) (prefix (state store) store:)
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

  (define (back-up! actor path disk)
    (let* ([sum (file:checksum (car disk))]
           [same (find (lambda (entry)
                         (let* ([facts (cadr entry)] [backup (fact facts 'backup)])
                           (and backup (fact facts 'trashed) (not (fact facts 'internal))
                                (actor:in-audience? actor (fact facts 'audience))
                                (equal? (car backup) path) (equal? (caddr backup) sum))))
                       (cadr (store:metadata)))])
      (if same (fact (cadr same) 'name)
        (store:buffer-name
          (store:create! actor (string-append (file:base-name path) ".bak") (file:lines (car disk))
            (list (cons 'trailing (file:ends-in-newline? (car disk)))
                  ;; No head registry belongs here. Restoring a backup detects
                  ;; its mode from this provenance and its own first line.
                  (cons 'source-file path)
                  (list 'trashed (time-second (current-time 'time-utc)) actor)
                  (list 'backup path (cdr disk) sum)))))))

  (define (save-document! actor id path adoption)
    (let ([written? #f])
      (call/cc
        (lambda (return)
          (define (refuse message) (return (list 'refused message)))
          (guard (ex [else
                      (let ([message (if written?
                                       (format "Wrote ~a, but could not finish saving: ~a" path (kernel:condition-text ex))
                                       (format "Save failed: ~a" (kernel:condition-text ex)))])
                        (log:add! 'document:save-document! message)
                        (list 'failed message))])
            (let-values ([(text revision facts) (store:snapshot-state id)])
              (define (check-source! facts)
                (when (and (fact facts 'app) (fact facts 'alive))
                  (refuse "Cannot save a buffer that belongs to a live app"))
                (when (> (or (fact facts 'conflicts) 0) 0) (refuse "Resolve the conflicts first")))
              (check-source! facts)
              (unless (and (list? adoption) (= (length adoption) 2)
                           (string? (car adoption)) (or (not (cadr adoption)) (string? (cadr adoption))))
                (error 'save-document! "expected (first-line detected-mode)"))
              (let ([adopted? (not (equal? path (fact facts 'file)))])
                (when (and adopted? (not (equal? (car adoption) (vector-ref text 0))))
                  (refuse "The first line changed; review the detected mode and save again"))
                (let-values ([(disk identity) (if (file-exists? path #f) (read-disk path) (values #f #f))])
                  (when (and disk (not adopted?) (not (fact facts 'modified))
                             (equal? (car disk) (fact facts 'base)))
                    (return '(unchanged "No changes to save")))
                  (when (and disk (not adopted?) (not (equal? (car disk) (fact facts 'base))))
                    (let-values ([(status detail)
                                  (apply-disk! actor id path (cons revision facts) disk identity #f)])
                      (cond
                        [(eq? status 'applied)
                         (unless (null? (cadr detail)) (refuse "Resolve the conflicts first"))
                         (let-values ([(merged rev current) (store:snapshot-state id)])
                           (unless (and (equal? (fact current 'file) path)
                                        (equal? (fact current 'base) (car disk)))
                             (refuse "Buffer's file or baseline changed; review the file again"))
                           (check-source! current)
                           (set! text merged) (set! revision rev) (set! facts current))]
                        [(memq detail '(no-base basis-too-old))
                         (let-values ([(status detail)
                                       (apply-disk! actor id path (cons revision facts) disk identity #t)])
                           (unless (eq? status 'applied) (refuse (format "File review refused (~a)" detail)))
                           (refuse (format "~a changed on disk and was reread instead of saved; undo brings your text back"
                                     (file:base-name path))))]
                        [else (refuse (format "File review refused (~a); further edits have been preserved" detail))])))
                  (let* ([trailing (cond [(assq 'trailing facts) => cdr] [else #t])]
                         [written (file:text text trailing)]
                         [review (property:select facts
                                   (append '(file base app alive)
                                     (if adopted? '(read-only disposable mode mode-auto) '())))]
                         [updates (append (list (cons 'file path) (cons 'base written) '(stamp . #f))
                                    (if adopted? `((read-only . #f) (disposable . #f)
                                                   (mode . ,(cadr adoption)) (mode-auto . #t)) '()))]
                         [kept (and disk (not (string=? (car disk) written)) (back-up! actor path disk))])
                    ;; Check after backup publication too: its subscribers may
                    ;; replace the target. Never overwrite a newly appeared file.
                    (let-values ([(current current-identity)
                                  (if (file-exists? path #f) (read-disk path) (values #f #f))])
                      (unless (and (equal? identity current-identity)
                                   (equal? (and disk (car disk)) (and current (car current))))
                        (refuse "Disk changed again; operation cancelled. Review the file again.")))
                    (file:write! path text trailing)
                    (set! written? #t)
                    ;; Text may advance during I/O; its modified flag remains
                    ;; derived against these exact written bytes. Metadata may
                    ;; not be retargeted or readopted underneath this receipt.
                    (unless (store:set-properties! actor id updates review (file:base-name path))
                      (error 'save-document! "Buffer's file state changed; saved baseline was not updated."))
                    (let ([message (if (and adopted? kept)
                                     (format "Wrote ~a; what it held is kept as ~a" path kept)
                                     (format "Wrote ~a" path))])
                      (log:add! 'document:save-document! message)
                      (list 'saved message)))))))))))

  (edoc "Save a shared document in the base, merging external edits undoably and backing up overwritten bytes. File facts, name and an adopted mode commit together; newer text remains dirty. Returns (saved message), (unchanged message), (refused message) or (failed message); a failed save can already have written bytes. Hooks belong to the caller."
        (actor actor "requesting actor") (id integer "document identity")
        (path string "canonical target") (adoption list "(reviewed-first-line detected-mode-name-or-false), used for Save As")
        (returns list))
  (define (save! actor id path adoption)
    (activity:call-with (lambda () (save-document! actor id path adoption))))

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
