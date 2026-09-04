;; git.e -- structured access to common Git operations: the library (git).
;;
;; Git's plumbing-oriented, NUL-delimited output is parsed at this boundary;
;; callers work only with Scheme records. Using the executable keeps Git's
;; repository semantics and configuration without coupling e to libgit2's ABI.

(library (git)
  (export init!
          git-repository? git-repository-path git-open git-current-branch
          git-status-entry? git-status-path git-status-original-path
          git-status-index git-status-worktree git-status
          git-branch? git-branch-name git-branch-current? git-branch-hash
          git-branch-upstream git-branch-ahead git-branch-behind git-branches
          git-commit? git-commit-hash git-commit-parents
          git-commit-author-name git-commit-author-email git-commit-time
          git-commit-subject git-commit-body git-log git-commit-files
          git-diff-entry? git-diff-status git-diff-path
          git-diff-original-path git-diff
          git-patch? git-patch-commit git-patch-path git-patch-lines
          git-patch-line? git-patch-line-kind git-patch-line-text
          git-file-patch
          git-error? git-error-code git-error-command git-error-stderr)
  (import (chezscheme)
          (prefix (doc) doc:))

  (define-record-type git-repository-record
    (fields (immutable path git-repository-path)))
  (define (git-repository? value) (git-repository-record? value))

  (define-record-type git-status-entry
    (fields (immutable path git-status-path)
            (immutable original-path git-status-original-path)
            (immutable index git-status-index)
            (immutable worktree git-status-worktree)))

  (define-record-type git-branch
    (fields (immutable name git-branch-name)
            (immutable current? git-branch-current?)
            (immutable hash git-branch-hash)
            (immutable upstream git-branch-upstream)
            (immutable ahead git-branch-ahead)
            (immutable behind git-branch-behind)))

  (define-record-type git-commit
    (fields (immutable hash git-commit-hash)
            (immutable parents git-commit-parents)
            (immutable author-name git-commit-author-name)
            (immutable author-email git-commit-author-email)
            (immutable time git-commit-time)
            (immutable subject git-commit-subject)
            (immutable body git-commit-body)))

  (define-record-type git-diff-entry
    (fields (immutable status git-diff-status)
            (immutable path git-diff-path)
            (immutable original-path git-diff-original-path)))

  (define-record-type git-patch
    (fields (immutable commit git-patch-commit)
            (immutable path git-patch-path)
            (immutable lines git-patch-lines)))

  (define-record-type git-patch-line
    (fields (immutable kind git-patch-line-kind)
            (immutable text git-patch-line-text)))

  (define-condition-type &git-error &error make-git-error git-error?
    (code git-error-code)
    (command git-error-command)
    (stderr git-error-stderr))

  (define (find-string text needle)
    (let ([n (string-length text)] [m (string-length needle)])
      (let loop ([i 0] [found #f])
        (cond [(> (+ i m) n) found]
              [(string=? (substring text i (+ i m)) needle)
               (loop (+ i 1) i)]
              [else (loop (+ i 1) found)]))))

  (define (prefix? prefix text)
    (let ([n (string-length prefix)])
      (and (<= n (string-length text))
           (string=? prefix (substring text 0 n)))))

  (define (shell-quote value)
    (let ([s (if (string? value) value (format "~a" value))])
      (string-append
        "'"
        (let loop ([i 0] [parts '()])
          (if (= i (string-length s))
              (apply string-append (reverse parts))
              (loop (+ i 1)
                    (cons (if (char=? (string-ref s i) #\') "'\\''"
                              (string (string-ref s i)))
                          parts))))
        "'")))

  (define (read-port port)
    (let ([value (get-string-all port)])
      (if (eof-object? value) "" value)))

  (define failure-marker "__E_GIT_STATUS__=")

  (define (run-git directory arguments)
    (let* ([words (map shell-quote
                       (append (list "git" "-C" directory) arguments))]
           [plain (apply string-append
                         (let loop ([xs words])
                           (if (null? xs) '()
                               (cons (car xs)
                                     (map (lambda (x) (string-append " " x))
                                          (cdr xs))))))]
           [command (format "~a || { code=$?; printf '\\n~a%s\\n' \"$code\" >&2; exit \"$code\"; }"
                            plain failure-marker)])
      (let-values ([(input output error pid)
                    (open-process-ports command 'block (native-transcoder))])
        (close-port input)
        (let ([out (box "")] [err (box "")])
          (let ([out-reader (fork-thread
                              (lambda () (set-box! out (read-port output))))]
                [err-reader (fork-thread
                              (lambda () (set-box! err (read-port error))))])
            (thread-join out-reader)
            (thread-join err-reader)
            (close-port output)
            (close-port error)
            (let* ([stderr (unbox err)]
                   [failure (find-string stderr failure-marker)])
              (if failure
                  (let* ([start (+ failure (string-length failure-marker))]
                         [end (let loop ([i start])
                                (if (or (= i (string-length stderr))
                                        (char=? (string-ref stderr i) #\newline))
                                    i
                                    (loop (+ i 1))))]
                         [code (string->number (substring stderr start end))])
                    (raise (condition
                             (make-git-error (or code 1) arguments
                                             (substring stderr 0 failure))
                             (make-message-condition
                               (format "git ~a failed (~a): ~a"
                                       (car arguments) (or code 1)
                                       (substring stderr 0 failure))))))
                  (unbox out))))))))

  (define (trim-newlines text)
    (let loop ([end (string-length text)])
      (if (and (> end 0)
               (memv (string-ref text (- end 1)) '(#\newline #\return)))
          (loop (- end 1))
          (substring text 0 end))))

  (define (split-at text separator)
    (let ([n (string-length text)])
      (let loop ([i 0] [start 0] [parts '()])
        (cond [(= i n) (reverse (cons (substring text start i) parts))]
              [(char=? (string-ref text i) separator)
               (loop (+ i 1) (+ i 1) (cons (substring text start i) parts))]
              [else (loop (+ i 1) start parts)]))))

  (define (repository-path repository)
    (if (git-repository? repository)
        (git-repository-path repository)
        (error 'git "expected a git repository" repository)))

  (define (git-open . path)
    (let* ([candidate (if (null? path) "." (car path))]
           [directory (if (file-directory? candidate)
                          candidate
                          (let loop ([i (- (string-length candidate) 1)])
                            (cond [(< i 0) "."]
                                  [(char=? (string-ref candidate i) #\/)
                                   (if (= i 0) "/" (substring candidate 0 i))]
                                  [else (loop (- i 1))])))])
      (make-git-repository-record
        (trim-newlines
          (run-git directory
                   '("rev-parse" "--show-toplevel"))))))

  (define (git-current-branch repository)
    (let ([name (trim-newlines
                  (run-git (repository-path repository)
                           '("branch" "--show-current")))])
      (and (> (string-length name) 0) name)))

  (define (status-state ch)
    (case ch
      [(#\space) #f]
      [(#\?) 'untracked]
      [(#\!) 'ignored]
      [(#\M) 'modified]
      [(#\A) 'added]
      [(#\D) 'deleted]
      [(#\R) 'renamed]
      [(#\C) 'copied]
      [(#\U) 'unmerged]
      [(#\T) 'type-changed]
      [else (string->symbol (string ch))]))

  (define (git-status repository)
    (let loop ([tokens (split-at
                         (run-git (repository-path repository)
                                  '("status" "--porcelain=v1" "-z"
                                    "--untracked-files=all"))
                         #\nul)]
               [entries '()])
      (if (or (null? tokens) (string=? (car tokens) ""))
          (reverse entries)
          (let* ([head (car tokens)]
                 [x (string-ref head 0)] [y (string-ref head 1)]
                 [path (substring head 3 (string-length head))]
                 [renamed? (or (char=? x #\R) (char=? x #\C)
                               (char=? y #\R) (char=? y #\C))]
                 [original (and renamed? (cadr tokens))])
            (loop (if renamed? (cddr tokens) (cdr tokens))
                  (cons (make-git-status-entry
                          path original (status-state x) (status-state y))
                        entries))))))

  (define (track-count text label)
    (let ([at (find-string text label)])
      (if at
          (let* ([start (+ at (string-length label))]
                 [end (let loop ([i start])
                        (if (and (< i (string-length text))
                                 (char-numeric? (string-ref text i)))
                            (loop (+ i 1)) i))])
            (or (string->number (substring text start end)) 0))
          0)))

  (define (git-branches repository)
    (map (lambda (line)
           (let ([fields (split-at line #\nul)])
             (make-git-branch
               (list-ref fields 0) (string=? (list-ref fields 1) "*")
               (list-ref fields 2)
               (let ([upstream (list-ref fields 3)])
                 (and (> (string-length upstream) 0) upstream))
               (track-count (list-ref fields 4) "ahead ")
               (track-count (list-ref fields 4) "behind "))))
         (filter (lambda (line) (> (string-length line) 0))
                 (split-at
                   (run-git
                     (repository-path repository)
                     '("for-each-ref"
                       "--sort=refname"
                       "--format=%(refname:short)%00%(HEAD)%00%(objectname)%00%(upstream:short)%00%(upstream:track,nobracket)"
                       "refs/heads"))
                   #\newline))))

  (define (words text)
    (filter (lambda (x) (> (string-length x) 0))
            (split-at text #\space)))

  (define (git-log repository . limit)
    (let* ([count (if (null? limit) 50 (car limit))]
           [tokens (split-at
                     (run-git
                       (repository-path repository)
                       (list "log" "-z" (format "-n~a" count)
                             "--format=%H%x00%P%x00%an%x00%ae%x00%at%x00%s%x00%B%x00"))
                     #\nul)])
      (let loop ([fields tokens] [commits '()])
        (if (< (length fields) 8)
            (reverse commits)
            (loop (list-tail fields 8)
                  (cons (make-git-commit
                          (list-ref fields 0) (words (list-ref fields 1))
                          (list-ref fields 2) (list-ref fields 3)
                          (string->number (list-ref fields 4))
                          (list-ref fields 5) (list-ref fields 6))
                        commits))))))

  (define (diff-state code)
    (status-state (string-ref code 0)))

  (define (parse-name-status output)
    (let loop ([tokens (split-at output #\nul)] [entries '()])
      (if (or (null? tokens) (string=? (car tokens) ""))
          (reverse entries)
          (let* ([code (car tokens)]
                 [renamed? (memv (string-ref code 0) '(#\R #\C))]
                 [original (and renamed? (cadr tokens))]
                 [path (if renamed? (caddr tokens) (cadr tokens))])
            (loop (if renamed? (cdddr tokens) (cddr tokens))
                  (cons (make-git-diff-entry
                          (diff-state code) path original)
                        entries))))))

  (define (git-diff repository . staged)
    (parse-name-status
      (run-git (repository-path repository)
               (append '("diff" "--name-status" "-z")
                       (if (and (pair? staged) (car staged))
                           '("--cached") '())))))

  (define (commit-id commit)
    (if (git-commit? commit) (git-commit-hash commit) commit))

  (define (git-commit-files repository commit)
    (parse-name-status
      (run-git (repository-path repository)
               (list "diff-tree" "--root" "--no-commit-id"
                     "--name-status" "-r" "-z" (commit-id commit)))))

  (define (patch-kind line)
    (cond [(prefix? "@@" line) 'hunk]
          [(or (prefix? "diff --git " line)
               (prefix? "index " line)) 'header]
          [(or (prefix? "--- " line)
               (prefix? "+++ " line)) 'file]
          [(prefix? "+" line) 'addition]
          [(prefix? "-" line) 'deletion]
          [(prefix? "\\" line) 'meta]
          [else 'context]))

  (define (git-file-patch repository commit path)
    (let ([id (commit-id commit)])
      (make-git-patch
        id path
        (map (lambda (line) (make-git-patch-line (patch-kind line) line))
             (split-at
               (run-git (repository-path repository)
                        (list "show" "--format=" "--no-ext-diff" "--patch"
                              id "--" path))
               #\newline)))))

  (define (init!)
    (doc:register!
      '(((git-open) (("procedure" . "(git-open [path])")) "git-repository"
         ("(git)") git "Git" #f
         "Open the Git worktree containing `path`, which defaults to the current directory, and return a structured repository object.")
        ((git-current-branch)
         (("procedure" . "(git-current-branch repository)")) "string or #f"
         ("(git)") git "Git" #f
         "Return the current local branch name, or #f for a detached HEAD.")
        ((git-status) (("procedure" . "(git-status repository)"))
         "list of git-status-entry" ("(git)") git "Git" #f
         "Return staged, worktree, and untracked changes as structured status records. Paths containing whitespace or newlines are preserved.")
        ((git-branches) (("procedure" . "(git-branches repository)"))
         "list of git-branch" ("(git)") git "Git" #f
         "Return local branches with current, object, upstream, ahead, and behind fields.")
        ((git-log) (("procedure" . "(git-log repository [limit])"))
         "list of git-commit" ("(git)") git "Git" #f
         "Return recent commits as structured records; `limit` defaults to 50.")
        ((git-diff) (("procedure" . "(git-diff repository [staged?])"))
         "list of git-diff-entry" ("(git)") git "Git" #f
         "Return changed paths and statuses from the unstaged diff, or the index diff when `staged?` is true.")
        ((git-commit-files)
         (("procedure" . "(git-commit-files repository commit)"))
         "list of git-diff-entry" ("(git)") git "Git" #f
         "Return the files changed by a commit as structured path and status records.")
        ((git-file-patch)
         (("procedure" . "(git-file-patch repository commit path)"))
         "git-patch" ("(git)") git "Git" #f
         "Return one file's patch in a commit. Its lines are classified as headers, hunks, additions, deletions, metadata, or context.")))))
