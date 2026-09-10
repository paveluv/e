;; file.e -- the disk: the library (file).
;;
;; Disk services, free of buffers and screens: path algebra (directory
;; and base parts, shared (path) expansion/canonicalization, abbreviation,
;; the stable identity of a visited file), reading, modification stamps,
;; permission-preserving writes, the line/trailing-newline algebra a
;; file's text and a buffer's line vector convert through, the
;; three-way merge of base, buffer, and disk, and completion over a
;; directory listing.  No dialogs and no bookkeeping: what to do when
;; the disk disagrees with a buffer is the commands' decision; this
;; module only reads, compares, merges, and writes.
;;
;; This iteration's attached heads run on the same SSH host and call
;; this module directly; a future remote transport needs a file service.
;; Exported names
;; drop the module stem: (file:read path), (file:lines text),
;; (file:write! path lines trailing?), (file:merge path base mine
;; disk).

(library (file)
  (export read read-state stamp write! call-with-port
          lines ends-in-newline? text
          merge conflict-count
          directory-part base-name abbreviate absolute
          (rename (path:expand expand) (path:canonical canonical))
          visit-path complete data-directory
          add-pre-save-hook! add-post-save-hook!
          run-pre-save-hooks! run-post-save-hooks!)
  ;; These are Chez names too; importers always see this library's exports
  ;; under the file: prefix.
  (import (except (chezscheme) read expand merge call-with-port)
          (prefix (only (sys) canonical-file-path) sys:)
          (prefix (only (diff) merge3 merge-report-lines) diff:)
          (prefix (path) path:)
          (prefix (string) string:)
          (prefix (text) text:)
          (prefix (log) log:)
          (prefix (kernel) kernel:))

  ;;; Paths ---------------------------------------------------------------------

  (define (directory-part path)
    ;; Everything up to and including the last slash, or #f without one.
    (let loop ([i (- (string-length path) 1)])
      (cond [(< i 0) #f]
            [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
            [else (loop (- i 1))])))

  (define (base-name path)
    (let ([dir (directory-part path)])
      (if dir (string:tail path (string-length dir)) path)))

  (define (abbreviate path)
    ;; The inverse of path:expand, for display: home becomes ~.
    (let ([home (getenv "HOME")])
      (if (and home (string:prefix? (string-append home "/") path))
          (string-append "~" (string:tail path (string-length home)))
          path)))

  (define (absolute path)
    ;; A relative path is relative to the process working directory,
    ;; which never changes.
    (if (or (string:prefix? "/" path) (string:prefix? "~" path))
        path
        (string-append (current-directory) "/" path)))

  (define (visit-path path)
    ;; One stable identity for visited files. Existing paths chase symbolic
    ;; links; for a new file, chase its existing parent and retain the final
    ;; component. Textual normalization is the portable fallback.
    (let* ([full (path:canonical (path:expand path))]
           [real (sys:canonical-file-path full)])
      (or real
          (let* ([dir (or (directory-part full) "/")]
                 [parent (if (and (> (string-length dir) 1)
                                  (string:suffix? "/" dir))
                             (substring dir 0 (- (string-length dir) 1))
                             dir)]
                 [real-parent (sys:canonical-file-path parent)])
            (if real-parent
                (string-append real-parent "/" (base-name full))
                full)))))

  (define (complete s)
    ;; Completion candidates for the partial path s: the entries of its
    ;; directory whose names extend its final component, as full paths, with
    ;; a trailing slash on directories so completion can descend into them.
    ;; A leading ~ is kept in the candidates but expanded for the lookups.
    ;; Dotfiles are offered only once the component starts with a dot.
    (guard (ex [else '()])
      (let* ([dir (or (directory-part s) "")]
             [part (string:tail s (string-length dir))]
             [listing (directory-list
                        (path:expand
                          (cond [(string=? dir "") "."]
                                [(string=? dir "/") "/"]
                                [else (substring dir 0 (- (string-length dir) 1))])))])
        (map (lambda (name)
               (let ([full (string-append dir name)])
                 (if (file-directory? (path:expand full))
                     (string-append full "/")
                     full)))
             (sort string<?
                   (filter (lambda (name)
                             (and (string:prefix? part name)
                                  (or (not (string=? part ""))
                                      (not (string:prefix? "." name)))))
                           listing))))))

  (define (data-directory)
    ;; Where commands and apps keep built or fetched data, out of git:
    ;; the installation's data directory, created on first use. Each
    ;; concern takes a subdirectory -- the describe corpus lives in
    ;; data/describe.
    (let ([dir (path:canonical (string-append (kernel:installation-directory) "/data"))])
      (unless (file-directory? dir) (mkdir dir))
      dir))

  ;;; Reading and writing ---------------------------------------------------------

  (define (call-with-port path output? use)
    ;; Chez's file combinators close only on normal return. Own the port
    ;; across exceptions and engine expiry too; protect acquisition/release,
    ;; while leaving the read/write body interruptible. Output replaces the
    ;; file but restores its permissions, best-effort, even if closing raises.
    ;; A closed scope cannot be resumed against a newly opened/truncated file.
    (let ([port #f] [mode #f])
      (dynamic-wind #t
        (lambda ()
          (when port (error 'file:call-with-port "file port scope has ended" path))
          (when (and output? (file-exists? path))
            (set! mode (guard (ex [else #f]) (get-mode path))))
          (set! port (if output? (open-output-file path 'replace) (open-input-file path))))
        (lambda () (use port))
        (lambda ()
          (dynamic-wind void (lambda () (close-port port))
            (lambda () (when mode (guard (ex [else (void)]) (chmod path mode)))))))))

  (define (read path)
    ;; the file's whole text ("" when empty); raises when unreadable
    (call-with-port path #f
      (lambda (p)
        (let ([s (get-string-all p)])
          (if (eof-object? s) "" s)))))

  (define (stamp path)
    ;; The file's mtime as (seconds . nanoseconds), or #f.
    (guard (ex [else #f])
      (and (file-exists? path)
           (let ([t (file-modification-time path)])
             (cons (time-second t) (time-nanosecond t))))))

  (define (read-state path)
    ;; (text . stamp), with #f for an uncertain stamp. Only cache an mtime
    ;; observed on both sides of the read; a later stamp must not certify
    ;; earlier bytes. This is a cache hint, not an atomic filesystem snapshot:
    ;; explicit file decisions still compare contents, including after prompts.
    (let* ([before (stamp path)] [content (read path)])
      (cons content (and before (equal? before (stamp path)) before))))

  (define (write! path v trailing?)
    ;; The line vector v as path's text, a newline after every line but
    ;; the last unless trailing?. The port scope owns permission restoration;
    ;; a failed or interrupted write can still leave partial contents.
    (let ([n (vector-length v)])
      (call-with-port path #t
        (lambda (p)
          (let loop ([i 0])
            (when (< i n)
              (display (vector-ref v i) p)
              (when (or (< i (- n 1)) trailing?) (newline p))
              (loop (+ i 1))))))))

  ;;; Text and lines --------------------------------------------------------------

  ;; A file's text and a buffer's line vector convert both ways; the
  ;; one bit a line vector does not carry -- whether the text ended in
  ;; a newline -- travels alongside as the trailing flag.

  (define (lines s)
    ;; s split at newlines, a trailing newline yielding no empty last
    ;; line: the shape comparisons and merges run on.
    (let-values ([(lines trailing?) (text:from-string s)]) lines))

  (define (ends-in-newline? s)
    (and (> (string-length s) 0)
         (char=? (string-ref s (- (string-length s) 1)) #\newline)))

  (define text text:to-string)

  ;;; Merging -----------------------------------------------------------------------

  (define (merge-trailing-newline base mine theirs)
    ;; Three-way merge for the one bit line vectors do not carry.  With a
    ;; boolean, two sides that both differ from base necessarily agree.
    (cond [(eq? mine base) theirs]
          [(eq? theirs base) mine]
          [else mine]))

  (define (merge path base mine disk)
    ;; The three-way merge of a file's text as loaded (base), as the
    ;; buffer has it (mine), and as the disk has it now: -> (values
    ;; merged-lines trailing? conflicts report-lines).  Conflicts stay
    ;; in the lines as markers; the report is diff's rendering of the
    ;; merge, for a *merge-...* buffer.
    (let ([base-lines (lines base)])
      (let-values ([(merged conflicts report)
                    (diff:merge3 base-lines (lines mine) (lines disk))])
        (values (if (null? merged) (vector "") (list->vector merged))
                (merge-trailing-newline (ends-in-newline? base)
                                        (ends-in-newline? mine)
                                        (ends-in-newline? disk))
                conflicts
                (diff:merge-report-lines path base-lines report conflicts)))))

  (define (conflict-count v)
    ;; how many merge conflict markers a line vector still holds
    (let loop ([i 0] [n 0])
      (if (= i (vector-length v))
          n
          (loop (+ i 1)
                (if (string:prefix? "<<<<<<<" (vector-ref v i)) (+ n 1) n)))))


  ;;; Save hooks --------------------------------------------------------------------

  ;; Modules may hook a save: pre-save hooks run before anything is
  ;; checked or written (formatting, say), post-save hooks after a
  ;; successful write (the module reload lives there).  Each receives
  ;; the path being written; a raising hook reports to the log and the
  ;; save goes on.
  (define pre-save-hooks (kernel:make-registry))
  (define post-save-hooks (kernel:make-registry))

  (define (add-pre-save-hook! proc) (kernel:registry-add! pre-save-hooks proc))
  (define (add-post-save-hook! proc) (kernel:registry-add! post-save-hooks proc))

  (define (run-hooks! hooks path)
    (for-each (lambda (p)
                (guard (ex [else (log:add! 'save-file!
                                   (format "Save hook failed: ~a"
                                           (kernel:condition-text ex)))])
                  (p path)))
              (kernel:registry-items hooks)))

  (define (run-pre-save-hooks! path) (run-hooks! pre-save-hooks path))
  (define (run-post-save-hooks! path) (run-hooks! post-save-hooks path))
) ;; library (file)
