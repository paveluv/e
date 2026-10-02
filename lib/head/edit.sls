;; Canonical commands compose explicit editor views, base documents and
;; clipboard/journal services. Head presentation follows widget capabilities.
(import (only (foundation edoc) elibrary))
(elibrary (head edit)
  (export answer! backups backward-expression! backward-kill-expression! (rename (editor:basis basis))
    beginning-of-form! buffer-clean? buffer-text call-as-one-edit! copy-region! copy-text copy-text!
    (rename (editor:create-view! create-view!)) current-batch (rename (editor:delete! delete!))
    delete-trashed! down-expression! empty-trash! end-of-form! format-buffer! format-region!
    forward-copy-buffer-to-system-clipboard forward-expression! indent-buffer! indent-expression!
    indent-line! indent-region! indent-tab! init! (rename (editor:insert! insert!)) kill-buffer!
    kill-expression! kill-line! kill-region! mark-expression! mark-form! (rename (editor:move! move!))
    next-list! open-line! (rename (editor:page! page!)) (rename (editor:paste! paste!))
    present-log-entries! previous-list! redo! region-text
    (rename (text-control:register-policy! register-policy!)) reload!
    (rename (editor:replace-region! replace-region-text!)) reread! restore!
    (rename (editor:rewrite-regions! rewrite-regions!)) save! save-file!
    (rename (editor:scroll! scroll!)) (rename (editor:select! select!)) (rename (editor:selection selection))
    (rename (editor:set-mark! set-mark!)) transpose-expressions! trash undo! undo-actor!
    (rename (text-control:undo-scope undo-scope)) up-expression! visit-file! yank!)


  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core region) region:)
    (prefix (foundation datum) datum:) (prefix (foundation string) string:)
    (prefix (foundation text) text:) (prefix (head editor) editor:) (prefix (head head) head:)
    (prefix (head interaction) interaction:) (prefix (head keymap) keymap:) (prefix (head mode) mode:)
    (prefix (head style) style:) (prefix (head text-control) text-control:)
    (prefix (head text-source) text-source:) (prefix (head tui) tui:) (prefix (head widget) widget:)
    (prefix (service clipboard) clipboard:) (prefix (service document) document:)
    (prefix (service file) file:) (prefix (service log) log:) (prefix (state actor) actor:)
    (prefix (state store) store:) (prefix (state view) view:) (prefix (sys sys) sys:))

  (edoc     "The batch label of the edits in the current one-edit group, the (actor token) pair they share in the delta log, unique across head reattachments, or #f outside a group."
    (returns (or list #f))
    (public))
  (define (current-batch) (text-source:current-batch head:ui-actor))

  (edoc     "Bundle every edit the thunk makes into one labeled undo step per buffer it touches; nested groups defer to the outermost."
    (label (or string #f) "the undo label")
    (thunk thunk "the edits to group")
    (returns any "what the thunk returns"))
  (define (call-as-one-edit! label thunk) (text-source:call-grouped! head:ui-actor label thunk))

  (edoc "Undo one action within undo-scope. An explicit editor view returns journal status and detail." (id model "editor view") (edits) (receiver id (view editor)))
  (define (undo! id) (editor:history! id 'undo (text-control:undo-scope)))

  (edoc "Reverse this head's latest undo in an explicit editor view, returning journal status and detail." (id model "editor view") (edits) (receiver id (view editor)))
  (define (redo! id) (editor:history! id 'redo 'mine))

  (edoc "Undo an actor's latest live action in an explicit editor view, returning journal status and detail." (who actor "the actor's identity") (id model "editor view") (edits) (public) (receiver id (view editor)))
  (define (undo-actor! who id) (editor:history! id 'undo (list 'actor who)))

  (edoc "Move point forward over one expression: the atom around point, else the next expression inside the enclosing one; the C-M-f of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (forward-expression! id) (editor:expression! id 'forward))

  (edoc "Move point backward over one expression: the atom around point, else the last expression ending by it inside the enclosing one; the C-M-b of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (backward-expression! id) (editor:expression! id 'backward))

  (edoc "Kill from point to the end of the next expression into the copy buffer; consecutive kills accumulate; the C-M-k of Emacs." (edits) (id model "editor view") (receiver id (view editor)))
  (define (kill-expression! id) (editor:transfer! id 'forward publish-view-kill!))

  (edoc "Kill from the start of the expression before point to point into the copy buffer, ahead of a preceding kill; the C-M-BACKSPACE of Emacs." (edits) (id model "editor view") (receiver id (view editor)))
  (define (backward-kill-expression! id)
    (editor:transfer!
      id
      'backward
      (lambda (text accumulate?) (publish-view-kill! text accumulate? #t))))

  (edoc "Set the mark at the end of the next expression and activate it, point staying; with the mark active beyond point, extend it by one more expression; the C-M-SPC of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (mark-expression! id) (editor:expression! id 'mark))

  (edoc "Mark the top-level form around point: point at its start, the mark at its end; the C-M-h of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (mark-form! id) (editor:expression! id 'form))

  (edoc "Move point up out of the enclosing list or vector, to its start; the C-M-u of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (up-expression! id) (editor:expression! id 'up))

  (edoc "Move point down into the next list or vector, just past its opening delimiter; the C-M-d of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (down-expression! id) (editor:expression! id 'down))

  (edoc "Move point over the next list or vector, skipping atoms; the C-M-n of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (next-list! id) (editor:expression! id 'next))

  (edoc "Move point back over the previous list or vector, skipping atoms; the C-M-p of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (previous-list! id) (editor:expression! id 'previous))

  (edoc "Move point to the start of the last top-level form beginning before point, the enclosing one included; the C-M-a of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (beginning-of-form! id) (editor:expression! id 'start))

  (edoc "Move point to the end of the first top-level form ending after point, the enclosing one included; the C-M-e of Emacs." (id model "editor view") (receiver id (view editor)))
  (define (end-of-form! id) (editor:expression! id 'end))

  (edoc "Swap the expression before point with the one after it, point ending after both; the C-M-t of Emacs." (edits) (id model "editor view") (receiver id (view editor)))
  (define (transpose-expressions! id) (editor:expression! id 'transpose))

  (edoc "Indent the lines of the next expression after its first by the mode's indenter; the C-M-q of Emacs." (edits) (id model "editor view") (receiver id (view editor)))
  (define (indent-expression! id) (editor:format! id 'indent-expression))

  (edoc     "Whether every kill and copy, and every other change to *copy*, also reaches the terminal's clipboard, through OSC 52."
    (value boolean))
  (define forward-copy-buffer-to-system-clipboard
    (make-parameter
      #f
      (lambda (enabled?)
        (unless (boolean? enabled?)
          (error 'forward-copy-buffer-to-system-clipboard "expected a boolean" enabled?))
        enabled?)))
  (define base64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (define (base64-encode bytes)
    (let ([length (bytevector-length bytes)])
      (let loop ([at 0] [parts '()])
        (if (= at length)
          (apply string-append (reverse parts))
          (let* ([remaining (- length at)]
                 [a (bytevector-u8-ref bytes at)]
                 [b (if (> remaining 1) (bytevector-u8-ref bytes (+ at 1)) 0)]
                 [c (if (> remaining 2) (bytevector-u8-ref bytes (+ at 2)) 0)]
                 [bits (+ (bitwise-arithmetic-shift-left a 16) (bitwise-arithmetic-shift-left b 8) c)]
                 [digit (lambda (shift)
                          (string
                            (string-ref
                              base64-alphabet
                              (bitwise-and (bitwise-arithmetic-shift-right bits shift) 63))))]
                 [chunk (string-append
                          (digit 18)
                          (digit 12)
                          (if (> remaining 1) (digit 6) "=")
                          (if (> remaining 2) (digit 0) "="))])
            (loop (+ at (min 3 remaining)) (cons chunk parts)))))))
  (define (publish-system-clipboard! text)
    (when (and (forward-copy-buffer-to-system-clipboard) (tui:screen-live?))
      (with-mutex tui:redraw-lock
        (tui:ansi! "\x1B;]52;c;" (base64-encode (string->utf8 text)) "\x1B;\\")
        (flush-output-port (sys:terminal-output-port)))))
  (define copy-document #f) (define published-copy (cons #f #f))
  (define (copy-source create?)
    (unless (and copy-document (store:exists? copy-document))
      (set! copy-document (actor:call-as head:ui-actor (lambda () (clipboard:open! create?)))))
    (and copy-document (text-source:open! head:ui-actor copy-document)))
  (define (publish-copy-changes!)
    (when copy-document
      (if (not (store:exists? copy-document))
        (set! copy-document #f)
        (let-values ([(lines revision facts) (store:snapshot-state copy-document)])
          (unless (and (equal? copy-document (car published-copy)) (eqv? revision (cdr published-copy)))
            (let ([known? (equal? copy-document (car published-copy))])
              (set! published-copy (cons copy-document revision))
              (when known?
                (publish-system-clipboard! (text:to-string lines (cdr (assq 'trailing facts)))))))))))
  (define (replace-copy-text! text label)
    (let* ([source (copy-source #t)]
           [id (text-source:id source)]
           [revision (text-source:revision source)]
           [old (text-source:lines source)]
           [key (list head:ui-actor (gensym->unique-string (gensym "copy")))])
      (let-values ([(lines trailing?) (text:from-string text)])
        (let-values ([(span replacement) (text:difference old lines)])
          (let-values ([(lines now changes points committed)
                        (text-source:edit! head:ui-actor (list old id revision) span replacement
                          (list
                            key
                            (format "~a ~s" label (string:elide text 40))
                            (list 'undo (cons 'trailing trailing?))
                            (list 'labels (cons 'batch key)))
                          '())])
            (text-source:adopt! source revision lines now changes)
            (follow-copy! id lines now)
            (set! published-copy (cons id now)))))
      (publish-system-clipboard! text)))
  (define (follow-copy! document lines revision)
    (let* ([row (- (vector-length lines) 1)] [end (cons row (string-length (vector-ref lines row)))])
      (define (walk frame)
        (let ([d (widget:frame-descriptor frame)])
          (when (and (eq? (view:kind d) 'editor) (equal? document (view:source d)))
            (let-values ([(source current) (text-control:context (widget:frame-id frame) 'editor 'current)])
              (when (and (equal? document (view:source current))
                         (= (view:generation d) (view:generation current)))
                (interaction:set-state!
                  head:ui-actor
                  (widget:frame-id frame)
                  revision
                  (list end end (caddr (view:state current)) #f)))))
          (for-each walk (widget:frame-children frame))))
      (for-each (lambda (p) (walk (car p))) (widget:shown))))
  (define (killing?)
    (let* ([action (head:last-command)]
           [procedure (if (keymap:call-action? action) (keymap:call-action-procedure action) action)])
      (and (memq procedure (list kill-line! kill-region! kill-expression! backward-kill-expression!))
           #t)))
  (define (publish-view-kill! text accumulate? . prepend?)
    (replace-copy-text!
      (if (and accumulate? (killing?))
        (if (and (pair? prepend?) (car prepend?))
            (string-append text (copy-text))
            (string-append (copy-text) text))
        text)
      "kill"))

  (edoc     "Copy text into the copy buffer without changing a buffer or point; C-y pastes it."
    (text string "the text to copy"))
  (define (copy-text! text)
    (unless (string? text) (error 'copy-text! "expected a string" text))
    (replace-copy-text! text "copy")
    (void))

  (edoc "Kill from point to the end of the line, or the line break when point is at the end; consecutive kills accumulate." (id model "editor view") (edits) (receiver id (view editor)))
  (define (kill-line! id) (editor:transfer! id 'line publish-view-kill!))

  (edoc "The copy buffer's text." (returns string) (effects internal))
  (define (copy-text)
    (if (copy-source #f)
      (let-values ([(lines revision facts) (store:snapshot-state copy-document)])
        (text:to-string lines (cdr (assq 'trailing facts))))
      ""))

  (edoc "Insert the copy buffer's text at point." (id model "editor view") (edits) (receiver id (view editor)))
  (define (yank! id) (let ([text (copy-text)]) (unless (string=? text "") (editor:paste! id text))))

  (edoc "Copy the text between mark and point to the copy buffer without deleting it; the mark deactivates. An explicit view refuses if the selected text changed." (id model "editor view") (receiver id (view editor)))
  (define (copy-region! id) (editor:transfer! id 'copy (lambda (text accumulate?) (copy-text! text))))

  (edoc "Kill the text between mark and point into the copy buffer." (id model "editor view") (edits) (receiver id (view editor)))
  (define (kill-region! id) (editor:transfer! id 'cut publish-view-kill!))

  (edoc     "Visit a file through the base, creating it and its missing parents if needed; a trailing slash creates/navigates a directory. Reopening preserves shared edits and merges disk changes undoably. An explicit destination receives directory/path or buffer/reference; the caller owns placement."
    (path file "the path to visit") (destination procedure "explicit placement callback")
    (proposal any "optional Finder creation witness") (returns boolean))
  (define visit-file!
    (case-lambda
      [(path destination) (visit-file! path destination #f)]
      [(path destination proposal)
       (unless (procedure? destination)
         (error 'visit-file! "expected a destination procedure" destination))
       (guard (ex
                [else
                 (log:add! 'edit:visit-file! (format "Cannot open ~a: ~a" path (kernel:condition-text ex)))
                 #f])
         (let ([result (document:acquire! head:ui-actor (file:expand (file:absolute path)) proposal)])
           (case (car result)
             [(directory) (destination 'directory (cadr result))]
             [(buffer)
              (begin
                (destination 'buffer (cadr result))
                (log:add!
                  'edit:visit-file!
                  (cons (if (caddr result) "Loaded" "Visited") (list-ref result 3))
                  (caddr result))
                (when (list-ref result 4) (log:add! 'edit:visit-file! (list-ref result 4))))])
           #t))]))
  (define (refuse! message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (refuse-file! message) (refuse! message))

  (edoc     "Save an explicit editor's document through the base. External changes merge undoably; conflicts refuse, and overwritten bytes become a shared backup. Hooks receive the destination path and this editor; changing its source during a pre-save hook refuses before writing. Mode, file and name are adopted atomically."
    (receiver editor (view editor)) (editor model "source editor")
    (target file "destination to write and visit") (returns boolean "whether saving completed"))
  (define (save-file! editor target)
    (let-values ([(source d) (text-control:context editor 'editor)])
      (let* ([id (view:source d)] [path (file:visit-path target)])
        (define (check-source!)
          (unless (text-control:current? editor source d)
            (refuse-file! "The source editor changed while preparing the save"))
          (when (and (store:property id 'app #f) (store:property id 'alive #f))
            (refuse-file! "This document belongs to a running app")))
        (check-source!)
        (when (> (store:property id 'conflicts 0) 0) (refuse-file! "Resolve the conflicts first"))
        (file:run-pre-save-hooks! path editor)
        (check-source!)
        (let-values ([(text revision facts) (store:snapshot-state id)])
          (let* ([detected (mode:detect path (vector-ref text 0))]
                 [adoption (list (vector-ref text 0) (and detected (mode:name detected)))]
                 [result (document:save! head:ui-actor id path adoption)])
            (text-source:open! head:ui-actor id)
            (case (car result)
              [(refused) (refuse-file! (cadr result))]
              [(unchanged) (head:report! (cadr result)) #f]
              [(failed) #f]
              [(saved)
               (guard (ex
                        [else
                         (log:add!
                           'edit:save-file!
                           (format "Wrote ~a, but could not finish saving: ~a" path (kernel:condition-text ex)))
                         #f])
                 (file:run-post-save-hooks! path editor)
                 #t)]))))))
  (define (merge-failure detail)
    (case detail
      [(no-base) "without a saved baseline to merge from"]
      [(basis-too-old) "past the log's reach to merge"]
      [(pending-edits) "resolve the pending conflicts first; further edits have been preserved"]
      [else (format "not merged (~a)" detail)]))
  (define (reload-document! replace? editor)
    (let-values ([(source d) (text-control:context editor 'editor)])
      (let ([id (view:source d)])
        (when (and replace?
                (or (cond [(assq 'read-only (view:options d)) => cdr] [else #f])
                    (store:property id 'read-only #f)))
          (refuse-file! "This editor is read-only"))
        (let-values ([(status detail)
                      (guard (ex
                               [(kernel:refusal? ex) (raise ex)]
                               [else (refuse-file! (kernel:condition-text ex))])
                        (if replace? (document:reread! head:ui-actor id) (document:reload! head:ui-actor id)))])
          (text-source:open! head:ui-actor id)
          (unless (eq? status 'applied)
            (refuse-file!
              (format
                "~a could not be ~a: ~a"
                (store:buffer-name id)
                (if replace? "reread" "reloaded")
                (merge-failure detail))))))))


  (edoc     "Reread an editor's file as an undoable replacement, settling pending conflicts. Concurrent edits or retargeting during the read refuse; earlier undo history remains."
    (receiver editor (view editor))
    (editor model "source editor")
    (edits))
  (define (reread! editor) (reload-document! #t editor))

  (edoc     "Reload an editor's file as an undoable merge, preserving earlier undo history. Concurrent edits or retargeting during the read refuse. Undo restores the pre-reload text while remembering the observed disk version, so saving can overwrite it."
    (receiver editor (view editor))
    (editor model "source editor")
    (public))
  (define (reload! editor) (reload-document! #f editor))

  (edoc     "A buffer's text as its file would hold it: the lines joined with newlines, ending in one when the buffer keeps a trailing newline."
    (b buffer "the shared document to read")
    (returns string))
  (define (buffer-text b)
    (let-values ([(lines revision facts) (store:snapshot-state b)])
      (file:text lines (cond [(assq 'trailing facts) => cdr] [else #f]))))

  (edoc     "Whether a buffer can be discarded without losing work: unmodified, or marked disposable; #f when its state cannot be read."
    (b buffer "the shared document to judge")
    (returns boolean)
    (public))
  (define (buffer-clean? b)
    (guard (ex [else #f])
      (let-values ([(text revision facts) (store:snapshot-state b)]) (file:state-clean? text facts))))

  (edoc     "Queue several existing log records for the echo area and repaint once, with a ghost text after the last one when given."
    (entries (list-of datum) "the log records")
    (tail string "a ghost text after the last one"))
  (define present-log-entries!
    (case-lambda
      [(entries) (present-log-entries-with! entries "")]
      [(entries tail) (present-log-entries-with! entries tail)]))
  (define (present-log-entries-with! entries tail)
    (let ([host (widget:command-owner (or (widget:target) (widget:focused)) 'notification)])
      (when host (widget:invoke! host 'notification entries tail))))

  (define (age-text seconds)
    (cond
      [(< seconds 60) (format "~a s" seconds)]
      [(< seconds 3600) (format "~a min" (quotient seconds 60))]
      [(< seconds 86400) (format "~a h" (quotient seconds 3600))]
      [else (format "~a d" (quotient seconds 86400))]))
  (define (now-seconds) (time-second (current-time 'time-utc)))
  (define (trashed-entries)
    (list-sort
      (lambda (a b) (or (> (caddr a) (caddr b)) (and (= (caddr a) (caddr b)) (> (cadar a) (cadar b)))))
      (filter
        values
        (map (lambda (entry)
               (let* ([m (cadr entry)] [t (cdr (assq 'trashed m))])
                 (and t
                      (actor:in-audience? head:ui-actor (cdr (assq 'audience m)))
                      (not (cdr (assq 'internal m)))
                      (list (car entry) (cdr (assq 'name m)) (car t) (cadr t) (cdr (assq 'backup m))
                        (cdr (assq 'version m))))))
             (cadr (store:metadata))))))
  (define (trash-entries) (filter (lambda (entry) (not (list-ref entry 4))) (trashed-entries)))
  (define (backup-entries) (filter (lambda (entry) (list-ref entry 4)) (trashed-entries)))
  (define (archive-entry! entry action)
    (let-values ([(status metadata)
                  (store:archive! head:ui-actor (car entry) (list-ref entry 5) action)])
      (unless (eq? status 'applied)
        (error 'archive-entry! "the entry changed; choose it again" (cadr entry)))))

  (edoc     "Trash a shared document by reference. Disposable output is deleted. Every window showing the document chooses its normal fallback."
    (document buffer "shared document to discard"))
  (define (kill-buffer! document)
    (unless (store:visible? head:ui-actor document)
      (error 'kill-buffer! "document is not visible" document))
    (let* ([rows (cadr (store:metadata (list document)))] [m (and (pair? rows) (cadar rows))])
      (unless m (error 'kill-buffer! "document no longer exists" document))
      (let ([name (cdr (assq 'name m))]
            [unsaved? (cdr (assq 'modified m))]
            [disposable? (cdr (assq 'disposable m))])
        (let-values ([(status current)
                      (store:archive! head:ui-actor document (cdr (assq 'version m)) 'trash)])
          (unless (eq? status 'applied) (error 'kill-buffer! "document changed; choose it again" name)))
        (log:add!
          'edit:kill-buffer!
          (cond
            [disposable? (format "Killed ~a" name)]
            [unsaved? (format "Killed ~a; its unsaved work is in the trash" name)]
            [else (format "Killed ~a; it is in the trash" name)])))))


  (edoc     "The trashed buffers, the backups aside, newest first, as (name killed-at actor): killed-at in UTC seconds; each expires store:trash-retention days after it was killed."
    (returns (list-of list)))
  (define (trash) (map (lambda (entry) (list-head (cdr entry) 3)) (trash-entries)))

  (edoc     "The backups, the versions saves wrote over, newest first, as (name path observed stamp checksum actor): the file's path, when it was read in UTC seconds, its modification time then as (seconds . nanoseconds) or #f, the checksum of its text and who saved; each expires store:trash-retention days after it was read, and a file keeps store:backups-kept of them."
    (returns (list-of list))
    (public))
  (define (backups)
    (map (lambda (entry)
           (let ([backup (list-ref entry 4)])
             (list (cadr entry) (car backup) (caddr entry) (cadr backup) (caddr backup) (cadddr entry))))
         (backup-entries)))

  (edoc-type trashed "the name of a buffer in the trash, a backup included"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete
      (lambda (partial)
        (let ([now (now-seconds)])
          (map (lambda (entry)
                 (let ([backup (list-ref entry 4)] [ago (age-text (- now (caddr entry)))])
                   (list
                     (cadr entry)
                     #f
                     (if backup
                       (format "backup of ~a, ~a ago" (file:abbreviate (car backup)) ago)
                       (format "killed ~a ago" ago)))))
               (trashed-entries)))))
    (write (lambda (v) (format "~s" v))) (within string))

  (edoc     "Restore the newest archived document with this name, including backups, preserving its text and history. Return its buffer reference for explicit placement; another document with the same name causes a unique suffix."
    (name trashed "the buffer's name in the trash")
    (returns buffer)
    (public))
  (define (restore! name)
    (let ([entry (find (lambda (entry) (string=? (cadr entry) name)) (trashed-entries))])
      (unless entry (error 'restore! "no such buffer in the trash" name))
      (let ([id (car entry)])
        (archive-entry! entry 'restore)
        (log:add! 'edit:restore! (format "Restored ~a" (store:buffer-name id)))
        id)))

  (edoc     "Permanently delete one trashed buffer or backup by name, including its history; live buffers and changed entries are refused. The original file on disk is untouched."
    (name trashed "the buffer's name in Trash or Backups")
    (public))
  (define (delete-trashed! name)
    (let ([entry (find (lambda (entry) (string=? (cadr entry) name)) (trashed-entries))])
      (unless entry (error 'delete-trashed! "no such buffer in the trash" name))
      (archive-entry! entry 'delete)
      (log:add! 'edit:delete-trashed! (format "Permanently deleted ~a" name))))

  (edoc     "Delete every trashed buffer for good, the backups kept; how many went."
    (returns integer)
    (public))
  (define (empty-trash!)
    (let ([count 0])
      (for-each
        (lambda (entry)
          (let-values ([(status m) (store:archive! head:ui-actor (car entry) (list-ref entry 5) 'delete)])
            (when (eq? status 'applied) (set! count (+ 1 count)))))
        (trash-entries))
      (log:add!
        'edit:empty-trash!
        (format "Emptied the trash: ~a buffer~a" count (if (= count 1) "" "s")))
      count))

  (edoc "Indent the current line by the mode's indenter, cycling through its stops; point lands on the indentation." (edits) (id model "editor view") (receiver id (view editor)))
  (define (indent-line! id) (editor:format! id 'indent-line))

  (edoc "What TAB does: indent the current line when the mode's indenter asked for it, else nothing." (edits) (id model "editor view") (receiver id (view editor)))
  (define (indent-tab! id) (editor:format! id 'tab))

  (edoc "Indent the lines between mark and point by the mode's indenter, each settling on its nearest stop." (edits) (id model "editor view") (receiver id (view editor)))
  (define (indent-region! id) (editor:format! id 'indent-region))

  (edoc "Indent every line of the current buffer by the mode's indenter." (edits) (id model "editor view") (receiver id (view editor)))
  (define (indent-buffer! id) (editor:format! id 'indent-buffer))

  (edoc "Rewrite the lines between mark and point with the mode's formatter." (edits) (id model "editor view") (receiver id (view editor)))
  (define (format-region! id) (editor:format! id 'format-region))

  (edoc "Rewrite the whole current buffer with the mode's formatter." (edits) (id model "editor view") (receiver id (view editor)))
  (define (format-buffer! id) (editor:format! id 'format-buffer))

  (edoc-type answer "an answer to the oldest pending question, one of its choices"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete
      (lambda (partial)
        (let ([asks (actor:pending head:ui-actor)])
          (if (null? asks)
            '()
            (let ([ask (car asks)]) (map (lambda (choice) (list choice #f (caddr ask))) (cadddr ask)))))))
    (write (lambda (v) (format "~s" v))) (within string))

  (edoc     "Answer the oldest question another actor posed through the interaction protocol; its choices complete."
    (choice answer "the answer"))
  (define (answer! choice)
    (let ([asks (actor:pending head:ui-actor)])
      (head:report!
        (cond
          [(null? asks) "Nothing to answer"]
          [(actor:answer! (car (car asks)) choice) "Answered"]
          [else "That question was withdrawn"]))))

  (edoc     "Save an editor's document to its visited file; without one, use save-file! with a path."
    (receiver editor (view editor))
    (editor model "source editor")
    (returns boolean "whether the file was written"))
  (define (save! editor)
    (let-values ([(source d) (text-control:context editor 'editor)])
      (let ([path (store:property (view:source d) 'file #f)])
        (unless path
          (refuse-file! "This document has no file: use edit:save-file! with this editor and a path"))
        (save-file! editor path))))

  (edoc "Insert a line break at this editor's caret, leaving it before the insertion."
    (receiver id (view editor)) (id model "editor view") (edits))
  (define (open-line! id) (editor:insert-at! id "\n" #t))


  (edoc     "Read a region from one document snapshot, rows joined with newlines; unavailable documents or coordinates outside the text refuse. No displayed buffer is required."
    (r region "the region to read")
    (returns string))
  (define (region-text r)
    (let ([id (region:buffer r)] [start (region:start r)] [end (region:end r)])
      (string:join (store:extract id (text:make-span (car start) (cdr start) (car end) (cdr end))) "\n")))

  (edoc     "Install explicit editor commands, clipboard observation and file log formatters. Loading creates no document, window or composition."
    (public))
  (define (init!)
    (kernel:load-module! "clipboard")
    (copy-source #f)
    (publish-copy-changes!)
    (let ([take #f])
      (let-values ([(token drain)
                    (store:watch!
                      (lambda ()
                        (head:run-on-main!
                          (lambda ()
                            (let ([changes (take)])
                              (when (and copy-document (or (not changes) (assoc copy-document changes)))
                                (publish-copy-changes!)))))))])
        (set! take drain)
        (head:add-shutdown-hook! (lambda () (store:unsubscribe! token)))))
    (keymap:bind-default! 'widget-editor "C-x C-s" (keymap:call save! widget:target))
    (keymap:bind-default! 'widget-editor "C-x C-w" (keymap:prefill save-file! widget:target))
    (keymap:bind-default! 'widget-editor "C-x C-r" (keymap:call reread! widget:target))
    (editor:register!
      (list (cons 'undo undo!) (cons 'redo redo!) (cons 'page editor:page!) (cons 'paste editor:paste!)
        (cons 'kill-line kill-line!) (cons 'kill-region kill-region!) (cons 'copy-region copy-region!)
        (cons 'yank yank!) (cons 'forward-expression forward-expression!)
        (cons 'backward-expression backward-expression!) (cons 'up-expression up-expression!)
        (cons 'down-expression down-expression!) (cons 'next-list next-list!)
        (cons 'previous-list previous-list!) (cons 'beginning-of-form beginning-of-form!)
        (cons 'end-of-form end-of-form!) (cons 'mark-expression mark-expression!)
        (cons 'mark-form mark-form!) (cons 'transpose-expressions transpose-expressions!)
        (cons 'kill-expression kill-expression!) (cons 'backward-kill-expression backward-kill-expression!)
        (cons 'indent-tab indent-tab!) (cons 'indent-expression indent-expression!)
        (cons 'indent-region indent-region!) (cons 'indent-line indent-line!)
        (cons 'indent-buffer indent-buffer!) (cons 'format-region format-region!)
        (cons 'format-buffer format-buffer!)))
    (let ([fmt (lambda (d) (if (pair? d) (format "~a ~a" (car d) (cdr d)) (format "~a" d)))])
      (log:register-formatter! 'edit:visit-file! fmt)
      (log:register-formatter! 'edit:save-file! fmt))
    (style:set-changed-hook! (lambda () (tui:invalidate-screen-cache!)))
    (style:color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! style:color-scheme!)
    (keymap:bind-default! 'widget-editor "C-o" (keymap:call open-line! widget:target)))
)
