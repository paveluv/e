;; file.e -- the disk: the library (file), v2 core dissolution
;; (dev/DESIGN2.md).
;;
;; Everything the editor does with the file system, below the seams
;; and free of buffers and screens: path algebra (directory and base
;; parts, ~ expansion and abbreviation, textual canonicalization, the
;; stable identity of a visited file), reading, modification stamps,
;; permission-preserving writes, the line/trailing-newline algebra a
;; file's text and a buffer's line vector convert through, the
;; three-way merge of base, buffer, and disk, and completion over a
;; directory listing.  No dialogs and no bookkeeping: what to do when
;; the disk disagrees with a buffer is the commands' decision; this
;; module only reads, compares, merges, and writes.
;;
;; Over the wire the disk is the server's: a remote head asks for a
;; save, and this module answers where the file is.  Exported names
;; drop the module stem: (file:read path), (file:lines text),
;; (file:write! path lines trailing?), (file:merge path base mine
;; disk).

(library (file)
  (export read stamp write!
          lines ends-in-newline? text
          merge conflict-count
          directory-part base-name expand abbreviate absolute
          canonical visit-path complete data-directory
          add-pre-save-hook! add-post-save-hook!
          run-pre-save-hooks! run-post-save-hooks!)
  ;; read, expand, and merge are Chez names too; importers always see
  ;; these under the file: prefix
  (import (except (chezscheme) read expand merge)
          (prefix (only (sys) canonical-file-path) sys:)
          (prefix (only (diff) merge3 merge-report-lines) diff:)
          (prefix (string) string:)
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

  (define (expand path)
    ;; Expand a leading ~ to the home directory.
    (let ([home (getenv "HOME")])
      (cond [(not home) path]
            [(string=? path "~") home]
            [(string:prefix? "~/" path) (string-append home (string:tail path 1))]
            [else path])))

  (define (abbreviate path)
    ;; The inverse of expand, for display: home becomes ~.
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

  (define (canonical path*)
    ;; path made absolute, with ".", "..", and empty segments resolved
    ;; textually (symbolic links are not chased) -- enough to recognize
    ;; the editor's own files whichever way they are named.
    (let* ([path (if (string:prefix? "/" path*)
                     path*
                     (string-append (current-directory) "/" path*))]
           [n (string-length path)])
      (let loop ([i 0] [start 0] [stack '()])
        (define (push seg)
          (cond [(or (string=? seg "") (string=? seg ".")) stack]
                [(string=? seg "..") (if (pair? stack) (cdr stack) stack)]
                [else (cons seg stack)]))
        (cond [(> i n) (string-append "/" (string:join (reverse stack) "/"))]
              [(or (= i n) (char=? (string-ref path i) #\/))
               (loop (+ i 1) (+ i 1) (push (substring path start i)))]
              [else (loop (+ i 1) start stack)]))))

  (define (visit-path path)
    ;; One stable identity for visited files. Existing paths chase symbolic
    ;; links; for a new file, chase its existing parent and retain the final
    ;; component. Textual normalization is the portable fallback.
    (let* ([full (canonical (expand path))]
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
                        (expand
                          (cond [(string=? dir "") "."]
                                [(string=? dir "/") "/"]
                                [else (substring dir 0 (- (string-length dir) 1))])))])
        (map (lambda (name)
               (let ([full (string-append dir name)])
                 (if (file-directory? (expand full))
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
    ;; the data directory next to lib, created on first use.  Each
    ;; concern takes a subdirectory -- the describe corpus lives in
    ;; data/describe.
    (let ([dir (string-append (caar (library-directories)) "/../data")])
      (unless (file-directory? dir) (mkdir dir))
      dir))

  ;;; Reading and writing ---------------------------------------------------------

  (define (read path)
    ;; the file's whole text ("" when empty); raises when unreadable
    (call-with-input-file path
      (lambda (p)
        (let ([s (get-string-all p)])
          (if (eof-object? s) "" s)))))

  (define (stamp path)
    ;; The file's mtime as (seconds . nanoseconds), or #f.
    (guard (ex [else #f])
      (and (file-exists? path)
           (let ([t (file-modification-time path)])
             (cons (time-second t) (time-nanosecond t))))))

  (define (write! path v trailing?)
    ;; The line vector v as path's text, a newline after every line but
    ;; the last unless trailing?.  Rewriting recreates the file:
    ;; remember its permissions (the exec bit on a script, say) and put
    ;; them back after -- best-effort, while a failed write raises.
    (let ([mode (and (file-exists? path)
                     (guard (ex [else #f]) (get-mode path)))]
          [n (vector-length v)])
      (call-with-output-file path
        (lambda (p)
          (let loop ([i 0])
            (when (< i n)
              (display (vector-ref v i) p)
              (when (or (< i (- n 1)) trailing?) (newline p))
              (loop (+ i 1)))))
        'replace)
      (when mode (guard (ex [else (void)]) (chmod path mode)))))

  ;;; Text and lines --------------------------------------------------------------

  ;; A file's text and a buffer's line vector convert both ways; the
  ;; one bit a line vector does not carry -- whether the text ended in
  ;; a newline -- travels alongside as the trailing flag.

  (define (lines s)
    ;; s split at newlines, a trailing newline yielding no empty last
    ;; line: the shape comparisons and merges run on.
    (let* ([n (string-length s)]
           [body (if (and (> n 0)
                          (char=? (string-ref s (- n 1)) #\newline))
                     (substring s 0 (- n 1))
                     s)])
      (list->vector (string:lines body))))

  (define (ends-in-newline? s)
    (and (> (string-length s) 0)
         (char=? (string-ref s (- (string-length s) 1)) #\newline)))

  (define (text v trailing?)
    ;; the inverse of lines: the file's text for a line vector
    (let ([n (vector-length v)])
      (if (= n 0)
          (if trailing? "\n" "")
          (let loop ([i (- n 1)] [acc (if trailing? (list "\n") '())])
            (let ([acc (cons (vector-ref v i) acc)])
              (if (= i 0)
                  (apply string-append acc)
                  (loop (- i 1) (cons "\n" acc))))))))

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
