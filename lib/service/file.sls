;; file.sls -- the disk: the library (file).
;;
;; Disk services, free of buffers and screens: path algebra (directory
;; and base parts, shared (path) expansion/canonicalization, abbreviation,
;; the stable identity of a visited file), reading, modification stamps,
;; permission-preserving writes, the line/trailing-newline algebra a
;; file's text and a buffer's line vector convert through, and
;; completion over a directory listing.  No dialogs and no bookkeeping: what to do when
;; the disk disagrees with a buffer is the commands' decision; this
;; module only reads, compares and writes.
;;
;; This iteration's attached heads run on the same SSH host and call
;; this module directly; a future remote transport needs a file service.
;; Exported names
;; drop the module stem: (file:read path), (file:lines text),
;; (file:write! path lines trailing?).

(import (only (foundation edoc) elibrary))
(elibrary (service file)
  (export abbreviate absolute add-post-save-hook! add-pre-save-hook! base-name call-with-port
          (rename (path:canonical canonical)) complete completion create!
          data-directory directory-part ends-in-newline? (rename (path:expand expand)) lines
          make-directories! read read-state run-post-save-hooks! run-pre-save-hooks! stamp
          state-clean? text visit-path write!)
  (import (except (chezscheme) read expand merge call-with-port)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (service log) log:)
          (prefix (sys path) path:)
          (prefix (only (sys sys) canonical-file-path) sys:))

  ;; Discard consent is the same for local and shared buffers. Compare the
  ;; captured text outside its writer lock; an unreadable disk is not clean.
  (edoc "Whether a buffer's lines and facts may be discarded without losing work: disposable, unmodified, or equal to its file on disk; an unreadable disk is not clean."
        (lines vector "the text")
        (facts list "the buffer facts")
        (returns boolean))
  (define (state-clean? lines facts)
    (define (fact key fallback) (cond [(assq key facts) => cdr] [else fallback]))
    (guard (ex [else #f])
      (or (fact 'disposable #f)
          (not (fact 'modified #f))
          (let ([path (fact 'file #f)])
            (if path
                (and (file-exists? path)
                     (string=? (text lines (fact 'trailing #t)) (read path)))
                (and (= (vector-length lines) 1) (string=? (vector-ref lines 0) "")))))))

  ;;; Paths ---------------------------------------------------------------------

  (edoc "Everything of a path up to and including its last slash, or #f without one."
        (path string "the path")
        (returns (or string #f)))
  (define (directory-part path)
    ;; Everything up to and including the last slash, or #f without one.
    (let loop ([i (- (string-length path) 1)])
      (cond [(< i 0) #f]
            [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
            [else (loop (- i 1))])))

  (edoc "The last component of a path."
        (path string "the path")
        (returns string))
  (define (base-name path)
    (let ([dir (directory-part path)])
      (if dir (string:tail path (string-length dir)) path)))

  ;; Paths as edoc types: completion offers the entries of the partial
  ;; path's directory, relative to the working directory, as the completion
  ;; parameter says; M-x matches the token against them and descends into
  ;; a directory it completes.
  (edoc-type file "a file, by its path"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete (lambda (partial) (map (lambda (path) (cons path #f)) (offered partial))))
    (write (lambda (v) (call-with-string-output-port (lambda (p) (write v p))))))

  (edoc-type directory "a directory, by its path"
    (predicate (lambda (v) (and (string? v) (guard (ex [else #f]) (file-directory? (path:canonical (path:expand v)))))))
    (complete (lambda (partial)
                (map (lambda (path) (cons path #f))
                     (filter (lambda (path) (string:suffix? "/" path)) (offered partial)))))
    (write (lambda (v) (call-with-string-output-port (lambda (p) (write v p))))))

  (edoc "How M-x completes a path inside a string: fuzzy, the default, offers every entry of the partial path's directory for the matcher's segments, prefix only the entries that extend its last component, deep every entry below the directory as well."
        (value (one-of prefix fuzzy deep)))
  (define completion
    (make-parameter 'fuzzy
      (lambda (v)
        (unless (memq v '(prefix fuzzy deep)) (error 'file:completion "prefix, fuzzy or deep" v))
        v)))

  (define (offered partial)
    ;; the paths a partial path offers at M-x, per the completion parameter;
    ;; ~ alone offers the home directory to descend into
    (if (string=? partial "~") (list "~/") (offered-entries partial)))

  (define (offered-entries partial)
    (case (completion)
      [(prefix) (complete partial)]
      [else (guard (ex [else '()]) (entries partial (eq? (completion) 'deep)))]))

  (define (entries partial deep?)
    ;; every entry of the partial path's directory as a full path, a
    ;; directory with a trailing slash, and with deep? the entries below its
    ;; subdirectories too, breadth first, a few thousand at most; hidden
    ;; entries only once the component starts with a dot
    (let* ([dir (or (directory-part partial) "")]
           [part (string:tail partial (string-length dir))]
           [hidden? (string:prefix? "." part)])
      (define (listing prefix)
        (map (lambda (name)
               (let ([full (string-append prefix name)])
                 (if (file-directory? (path:canonical (path:expand full))) (string-append full "/") full)))
             (sort string<?
               (filter (lambda (name) (or hidden? (not (string:prefix? "." name))))
                       (guard (ex [else '()]) (directory-list (path:canonical (path:expand prefix))))))))
      (let walk ([queue (list dir)] [out '()] [count 0])
        (if (or (null? queue) (>= count 2000))
            (reverse out)
            (let ([here (listing (car queue))])
              (walk (append (cdr queue) (if deep? (filter (lambda (path) (string:suffix? "/" path)) here) '()))
                    (append (reverse here) out)
                    (+ count (length here))))))))

  (edoc "A path for display, the home directory as ~: the inverse of expand."
        (path string "the path")
        (returns string))
  (define (abbreviate path)
    ;; The inverse of path:expand, for display: home becomes ~.
    (let ([home (getenv "HOME")])
      (if (and home (string:prefix? (string-append home "/") path))
          (string-append "~" (string:tail path (string-length home)))
          path)))

  (edoc "A path resolved against a directory; an absolute or home path stays as it is."
        (path string "the path")
        (directory directory "the base directory")
        (returns string))
  (define absolute
    ;; Resolve a relative path against an explicit directory or the process
    ;; working directory. Keep home notation for editable path prompts.
    (case-lambda
      [(path)
       (absolute path (current-directory))]
      [(path directory)
       (if (or (string:prefix? "/" path) (string=? "~" path) (string:prefix? "~/" path)) path
           (string-append directory (if (string:suffix? "/" directory) "" "/") path))]))

  (edoc "One stable identity for a visited file: symbolic links chased for an existing path, for a new file its parent's; textual normalization as the fallback."
        (path string "the path as typed")
        (returns file))
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
            ;; realpath spells the root as "/", the one parent that already
            ;; ends in the separator the join adds.
            (cond [(not real-parent) full]
                  [(string=? real-parent "/") (string-append "/" (base-name full))]
                  [else (string-append real-parent "/" (base-name full))])))))

  (edoc "The completions of a partial path relative to a directory: the entries extending its last component, as full paths, directories with a trailing slash."
        (s string "the partial path")
        (directory directory "the base directory")
        (returns (list-of string)))
  (define complete
    ;; Completion candidates for the partial path s: the entries of its
    ;; directory whose names extend its final component, as full paths, with
    ;; a trailing slash on directories so completion can descend into them.
    ;; A leading ~ is kept in the candidates but expanded for the lookups.
    ;; Dotfiles are offered only once the component starts with a dot.
    ;; Resolve the directory like visiting/browser navigation, while retaining
    ;; the input's spelling in candidates (including . and .. components).
    (case-lambda
      [(s directory)
       (let* ([full (absolute s directory)] [prefix (- (string-length full) (string-length s))])
         (map (lambda (value) (string:tail value prefix)) (complete full)))]
      [(s)
       (if (string=? s "~") '("~/")
         (guard (ex [else '()])
           (let* ([dir (or (directory-part s) "")]
                  [part (string:tail s (string-length dir))]
                  [listing (directory-list (path:canonical (path:expand dir)))])
             (map (lambda (name)
                    (let ([full (string-append dir name)])
                      (if (file-directory? (path:canonical (path:expand full)))
                        (string-append full "/")
                        full)))
               (sort string<?
                 (filter (lambda (name)
                           (and (string:prefix? part name)
                                (or (not (string=? part ""))
                                    (not (string:prefix? "." name)))))
                         listing))))))]))

  (edoc "Where commands and apps keep built or fetched data, out of git: the installation's data directory, created on first use."
        (returns directory)
        (effects internal))
  (define (data-directory)
    ;; Where commands and apps keep built or fetched data, out of git:
    ;; the installation's data directory, created on first use. Each
    ;; concern takes a subdirectory -- the describe corpus lives in
    ;; data/describe.
    (let ([dir (path:canonical (string-append (kernel:installation-directory) "/data"))])
      (unless (file-directory? dir) (mkdir dir))
      dir))

  (edoc "Create a directory and its missing parents; an existing directory is fine, a file in the way is an error."
        (path string "the directory"))
  (define (make-directories! path)
    ;; Existing directories (including links to them) are fine. A file or
    ;; dangling link is an error, never something to replace. A concurrent
    ;; mkdir is fine too, provided the resulting path is a directory.
    (let create ([path (path:canonical (path:expand path))])
      (unless (file-directory? path)
        (when (file-exists? path #f) (error 'make-directories! "not a directory" path))
        (create (path:canonical (directory-part path)))
        (guard (ex [(and (i/o-file-already-exists-error? ex) (file-directory? path)) (void)]
                   [else (raise ex)])
          (create! (absolute "" path)))))
    (void))

  ;;; Reading and writing ---------------------------------------------------------

  (edoc "Create an empty file, or a directory for a trailing slash, exclusively; an existing target raises the already-exists condition."
        (path string "the path"))
  (define (create! path)
    ;; A trailing slash requests a directory; both kinds create exclusively.
    ;; Chez's mkdir needs its existing-directory error normalized to the
    ;; same condition that the default output-file options already raise.
    (let ([directory? (string:suffix? "/" path)]
          [path (path:canonical (path:expand path))])
      (if directory?
          (guard (ex [(file-directory? path)
                      (raise (condition (make-i/o-file-already-exists-error path)
                                        (make-message-condition "directory already exists")))]
                     [(file-exists? path #f) (error 'create! "not a directory" path)]
                     [else (raise ex)])
            (mkdir path))
          (dynamic-wind disable-interrupts
            (lambda () (close-port (open-file-output-port path)))
            enable-interrupts))
      (log:add! 'file
        (string-append (if directory? "Created directory " "Created file ")
                       (if directory? (absolute "" path) path))))
    (void))

  (edoc "Open a file for reading or replacing and call use with the port, closing it across exceptions too; output restores the file's permissions."
        (path file "the file")
        (output? boolean "whether to write it")
        (use procedure "(use port)")
        (returns any "what use returns"))
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

  (edoc "A file's whole text, empty for an empty file; raises when unreadable or not a regular file."
        (path file "the file")
        (returns string))
  (define (read path)
    ;; the file's whole text ("" when empty); raises when unreadable
    (unless (file-regular? path)
      (error 'read "not a regular file" path))
    (call-with-port path #f
      (lambda (p)
        (let ([s (get-string-all p)])
          (if (eof-object? s) "" s)))))

  (edoc "A file's modification time as (seconds . nanoseconds), or #f."
        (path file "the file")
        (returns (or pair #f)))
  (define (stamp path)
    ;; The file's mtime as (seconds . nanoseconds), or #f.
    (guard (ex [else #f])
      (and (file-exists? path)
           (let ([t (file-modification-time path)])
             (cons (time-second t) (time-nanosecond t))))))

  (edoc "A file's (text . stamp), the stamp #f unless it was the same on both sides of the read."
        (path file "the file")
        (returns pair))
  (define (read-state path)
    ;; (text . stamp), with #f for an uncertain stamp. Only cache an mtime
    ;; observed on both sides of the read; a later stamp must not certify
    ;; earlier bytes. This is a cache hint, not an atomic filesystem snapshot:
    ;; explicit file decisions still compare contents, including after prompts.
    (let* ([before (stamp path)] [content (read path)])
      (cons content (and before (equal? before (stamp path)) before))))

  (edoc "Write a line vector as a file's text, a newline after every line but the last unless trailing?."
        (path file "the file")
        (v vector "the lines")
        (trailing? boolean "whether the last line ends in a newline"))
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

  (edoc "A text split at newlines into a line vector, a trailing newline yielding no empty last line."
        (s string "the text")
        (returns vector))
  (define (lines s)
    ;; s split at newlines, a trailing newline yielding no empty last
    ;; line: the shape comparisons and merges run on.
    (let-values ([(lines trailing?) (text:from-string s)]) lines))

  (edoc "Whether a text ends in a newline."
        (s string "the text")
        (returns boolean))
  (define (ends-in-newline? s)
    (and (> (string-length s) 0)
         (char=? (string-ref s (- (string-length s) 1)) #\newline)))

  (define text text:to-string)

  ;;; Save hooks --------------------------------------------------------------------

  ;; Modules may hook a save: pre-save hooks run before anything is
  ;; checked or written (formatting, say), post-save hooks after a
  ;; successful write (the module reload lives there).  Each receives
  ;; the path being written; a raising hook reports to the log and the
  ;; save goes on.
  (define pre-save-hooks (kernel:make-registry))
  (define post-save-hooks (kernel:make-registry))

  (edoc "Register a hook run with the path before a file is saved."
        (proc procedure "(hook path)"))
  (define (add-pre-save-hook! proc)
    (kernel:registry-add! pre-save-hooks proc))

  (edoc "Register a hook run with the path after a file was saved."
        (proc procedure "(hook path)"))
  (define (add-post-save-hook! proc)
    (kernel:registry-add! post-save-hooks proc))

  (define (run-hooks! hooks path)
    (for-each (lambda (p)
                (guard (ex [else (log:add! 'save-file!
                                   (format "Save hook failed: ~a"
                                           (kernel:condition-text ex)))])
                  (p path)))
              (kernel:registry-items hooks)))

  (edoc "Run the pre-save hooks for a path."
        (path file "the file"))
  (define (run-pre-save-hooks! path)
    (run-hooks! pre-save-hooks path))

  (edoc "Run the post-save hooks for a path."
        (path file "the file"))
  (define (run-post-save-hooks! path)
    (run-hooks! post-save-hooks path))
) ;; library (file)
