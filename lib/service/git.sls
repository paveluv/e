;; git.sls -- structured access to common Git operations: the library (git).
;;
;; Git's plumbing-oriented, NUL-delimited output is parsed at this boundary;
;; callers work only with Scheme records. Using the executable keeps Git's
;; repository semantics and configuration without coupling e to libgit2's ABI.

(import (only (foundation edoc) elibrary))
(elibrary (service git)
  (export init!
          (rename (git-repository? repository?)) (rename (git-repository-path repository-path)) (rename (git-open open)) (rename (git-current-branch current-branch))
          (rename (git-status-entry? status-entry?)) (rename (git-status-path status-path)) (rename (git-status-original-path status-original-path))
          (rename (git-status-index status-index)) (rename (git-status-worktree status-worktree)) (rename (git-status status))
          (rename (git-branch? branch?)) (rename (git-branch-name branch-name)) (rename (git-branch-current? branch-current?)) (rename (git-branch-hash branch-hash))
          (rename (git-branch-upstream branch-upstream)) (rename (git-branch-ahead branch-ahead)) (rename (git-branch-behind branch-behind)) (rename (git-branches branches))
          (rename (git-commit? commit?)) (rename (git-commit-hash commit-hash)) (rename (git-commit-parents commit-parents))
          (rename (git-commit-author-name commit-author-name)) (rename (git-commit-author-email commit-author-email)) (rename (git-commit-time commit-time))
          (rename (git-commit-subject commit-subject)) (rename (git-commit-body commit-body)) (rename (git-log log)) (rename (git-commit-files commit-files))
          (rename (git-diff-entry? diff-entry?)) (rename (git-diff-status diff-status)) (rename (git-diff-path diff-path))
          (rename (git-diff-original-path diff-original-path)) (rename (git-diff diff))
          (rename (git-patch? patch?)) (rename (git-patch-commit patch-commit)) (rename (git-patch-path patch-path)) (rename (git-patch-lines patch-lines))
          (rename (git-patch-line? patch-line?)) (rename (git-patch-line-kind patch-line-kind)) (rename (git-patch-line-text patch-line-text))
          (rename (git-file-patch file-patch))
          (rename (git-error? error?)) (rename (git-error-code error-code)) (rename (git-error-command error-command)) (rename (git-error-stderr error-stderr)))
  (import (chezscheme)
          (prefix (sys sys) sys:)
          (prefix (service doc) doc:))

  (edoc "An opened git repository."
        (path directory "the worktree path"))
  (define-record-type git-repository-record
    (fields (immutable path git-repository-path)))
  (edoc "Whether a value is an opened git repository."
        (value any "the value")
        (returns boolean))
  (define (git-repository? value)
    (git-repository-record? value))

  (edoc "One line of git status."
        (path string "the file")
        (original-path (or string #f) "where a renamed file came from")
        (index char "the index status letter")
        (worktree char "the worktree status letter"))
  (define-record-type git-status-entry
    (fields (immutable path git-status-path)
            (immutable original-path git-status-original-path)
            (immutable index git-status-index)
            (immutable worktree git-status-worktree)))

  (edoc "A branch of the repository."
        (name string "the branch name")
        (current? boolean "whether it is checked out")
        (hash string "the commit it points at")
        (upstream (or string #f) "the tracked remote branch")
        (ahead integer "commits ahead of the upstream")
        (behind integer "commits behind the upstream"))
  (define-record-type git-branch
    (fields (immutable name git-branch-name)
            (immutable current? git-branch-current?)
            (immutable hash git-branch-hash)
            (immutable upstream git-branch-upstream)
            (immutable ahead git-branch-ahead)
            (immutable behind git-branch-behind)))

  (edoc "A commit."
        (hash string "the full hash")
        (parents (list-of string) "the parent hashes")
        (author-name string "who wrote it")
        (author-email string "the author's email")
        (time integer "when, in seconds since the epoch")
        (subject string "the first line of the message")
        (body string "the whole message"))
  (define-record-type git-commit
    (fields (immutable hash git-commit-hash)
            (immutable parents git-commit-parents)
            (immutable author-name git-commit-author-name)
            (immutable author-email git-commit-author-email)
            (immutable time git-commit-time)
            (immutable subject git-commit-subject)
            (immutable body git-commit-body)))

  (edoc "A changed file."
        (status char "the change letter: A, M, D, R and so on")
        (path string "the file")
        (original-path (or string #f) "where a renamed file came from"))
  (define-record-type git-diff-entry
    (fields (immutable status git-diff-status)
            (immutable path git-diff-path)
            (immutable original-path git-diff-original-path)))

  (edoc "A commit's patch to one file."
        (commit string "the commit hash")
        (path string "the file")
        (lines (list-of (record git-patch-line)) "the patch lines"))
  (define-record-type git-patch
    (fields (immutable commit git-patch-commit)
            (immutable path git-patch-path)
            (immutable lines git-patch-lines)))

  (edoc "One line of a patch."
        (kind symbol "header, hunk, add, remove or context")
        (text string "the line"))
  (define-record-type git-patch-line
    (fields (immutable kind git-patch-line-kind)
            (immutable text git-patch-line-text)))

  (edoc "A git command failed."
        (code integer "its exit code")
        (command (list-of string) "the command line")
        (stderr string "what it wrote to stderr"))
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

  (define (run-git directory arguments)
    (let ([process #f])
      (dynamic-wind #t
        (lambda ()
          (when process (error 'git "command scope has ended"))
          (set! process (sys:open-process (append (list "git" "-C" directory) arguments))))
        (lambda ()
          (sys:write-process! process #f)
          (let ([out (get-bytevector-all (sys:process-input process))])
            (let-values ([(code stderr) (sys:process-result process)])
              (if (zero? code)
                  (if (eof-object? out) "" (utf8->string out))
                  (raise (condition
                           (make-git-error code arguments stderr)
                           (make-message-condition
                             (format "git ~a failed (~a): ~a" (car arguments) code stderr))))))))
        (lambda () (sys:close-process! process)))))

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

  (edoc "The worktree path of a repository; an error for anything else."
        (repository (record git-repository-record) "the repository")
        (returns directory))
  (define (repository-path repository)
    (if (git-repository? repository)
        (git-repository-path repository)
        (error 'git "expected a git repository" repository)))

  (edoc "Open the git worktree containing a path, the current directory by default."
        (path (list-of string) "a path inside the worktree, at most one")
        (returns (record git-repository-record)))
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

  (edoc "The name of a repository's current branch, or #f when detached."
        (repository (record git-repository-record) "the repository")
        (returns (or string #f)))
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

  (edoc "A repository's status entries, untracked files included."
        (repository (record git-repository-record) "the repository")
        (returns (list-of (record git-status-entry))))
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

  (edoc "A repository's branches with their upstreams and ahead and behind counts."
        (repository (record git-repository-record) "the repository")
        (returns (list-of (record git-branch))))
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

  (edoc "A repository's newest commits, 50 by default."
        (repository (record git-repository-record) "the repository")
        (limit (list-of integer) "how many at most, at most one")
        (returns (list-of (record git-commit))))
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

  (edoc "The changed files of the worktree, or of the index when staged."
        (repository (record git-repository-record) "the repository")
        (staged (list-of boolean) "whether to diff the index, at most one")
        (returns (list-of (record git-diff-entry))))
  (define (git-diff repository . staged)
    (parse-name-status
      (run-git (repository-path repository)
               (append '("diff" "--name-status" "-z")
                       (if (and (pair? staged) (car staged))
                           '("--cached") '())))))

  (define (commit-id commit)
    (if (git-commit? commit) (git-commit-hash commit) commit))

  (edoc "The files a commit changed, with their statuses."
        (repository (record git-repository-record) "the repository")
        (commit (or (record git-commit) string) "the commit, or its hash")
        (returns (list-of (record git-diff-entry))))
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

  (edoc "The patch a commit made to one file, its lines classified."
        (repository (record git-repository-record) "the repository")
        (commit (or (record git-commit) string) "the commit, or its hash")
        (path string "the file")
        (returns (record git-patch)))
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

  (edoc "Register the describe entries of the git service.")
  (define (init!)
    (doc:register!
      '(((git:open) (("procedure" . "(git:open [path])")) "git-repository"
         ("(service git)") git "Git" #f
         "Open the Git worktree containing `path`, which defaults to the current directory, and return a structured repository object.")
        ((git:current-branch)
         (("procedure" . "(git:current-branch repository)")) "string or #f"
         ("(service git)") git "Git" #f
         "Return the current local branch name, or #f for a detached HEAD.")
        ((git:status) (("procedure" . "(git:status repository)"))
         "list of git-status-entry" ("(service git)") git "Git" #f
         "Return staged, worktree, and untracked changes as structured status records. Paths containing whitespace or newlines are preserved.")
        ((git:branches) (("procedure" . "(git:branches repository)"))
         "list of git-branch" ("(service git)") git "Git" #f
         "Return local branches with current, object, upstream, ahead, and behind fields.")
        ((git:log) (("procedure" . "(git:log repository [limit])"))
         "list of git-commit" ("(service git)") git "Git" #f
         "Return recent commits as structured records; `limit` defaults to 50.")
        ((git:diff) (("procedure" . "(git:diff repository [staged?])"))
         "list of git-diff-entry" ("(service git)") git "Git" #f
         "Return changed paths and statuses from the unstaged diff, or the index diff when `staged?` is true.")
        ((git:commit-files)
         (("procedure" . "(git:commit-files repository commit)"))
         "list of git-diff-entry" ("(service git)") git "Git" #f
         "Return the files changed by a commit as structured path and status records.")
        ((git:file-patch)
         (("procedure" . "(git:file-patch repository commit path)"))
         "git-patch" ("(service git)") git "Git" #f
         "Return one file's patch in a commit. Its lines are classified as headers, hunks, additions, deletions, metadata, or context.")))))
