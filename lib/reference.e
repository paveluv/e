;; reference.e -- the base's documentation corpus, fetcher, and queries.
;;
;; The HTML extractor and indexed TSPL/CSUG corpus have one owner, below
;; head presentation. Queries include the live doc: registry; fetching uses
;; https: and reports through log:. The describe facade reexports these
;; operations and adds prompts, key annotations, and a Markdown viewer.

(library (reference)
  (export fetch! page page! (rename (doc-lookup lookup) (doc-entries entries)
                              (doc-browser-url browser-url)))
  (import (chezscheme) (prefix (doc) doc:) (prefix (file) file:)
          (prefix (https) https:) (prefix (log) log:)
          (prefix (actor) actor:) (prefix (store) store:)
          (prefix (string) string:) (prefix (text) text:))

  (define (data-dir)
    (string-append (file:data-directory) "/describe"))

  (define (data-path)
    (string-append (data-dir) "/describe.sdata"))

  ;;; Markdown source pages ----------------------------------------------

  ;; The base produces Markdown source without wrapping it for a head.
  ;; Selection and publication identity live with that source in the store;
  ;; no head callback or second buffer directory survives a reload here.
  (define producer '(app describe))

  (define (check-head head)
    (unless (and (actor:identity? head) (eq? (car head) 'head))
      (error 'reference "expected a requesting head" head)))

  (define (page head)
    ;; -> (id revision selected-name), or #f if absent/hidden. This receipt
    ;; can be the basis of a refresh, so it cannot replace a newer query or
    ;; recreate a page deleted while its source was being computed.
    (check-head head)
    (let ([id (store:publication producer head)])
      (and id
           (guard (ex [else #f])
             (let-values ([(lines revision facts) (store:snapshot-state id)])
               (let ([audience (assq 'audience facts)])
                 (and (actor:in-audience? head (if audience (cdr audience) 'all))
                      (list id revision (cdr (assq 'reference-query facts))))))))))

  (define (page! head name keys . basis)
    ;; Keys are plain annotations supplied by the requesting head. Omit
    ;; basis for an explicit selection; pass (id . revision) to refresh it.
    (check-head head)
    (unless (and (or (symbol? name) (string? name))
                 (list? keys) (for-all string? keys)
                 (<= (length basis) 1)
                 (or (null? basis)
                     (let ([b (car basis)])
                       (and (pair? b) (integer? (car b)) (exact? (car b)) (> (car b) 0)
                            (integer? (cdr b)) (exact? (cdr b)) (>= (cdr b) 0)))))
      (error 'page! "expected a name, key strings and optional (id . revision)" name keys basis))
    (let* ([name (if (string? name) (string->symbol name) name)]
           [entries (doc-lookup name)])
      (and (or (pair? entries) (pair? basis))
           (let ([lines (if (pair? entries) (page-lines entries keys)
                            (list (format "No documentation for ~a" name)))])
             (apply store:publish! producer head "*describe*" lines
               `((audience . (,head)) (reference-query . ,name)
                 (mode . "markdown") (mode-auto . #f) (read-only . #t)
                 (disposable . #t) (trailing . #t) (wrap . default)
                 (base . ,(text:to-string (list->vector lines) #t)))
               (if (null? basis) '()
                   (list (list (caar basis) (cdar basis)
                               (cons 'audience (list head)) (cons 'reference-query name)))))))))

  (define (entry-lines entry)
    (append
      ;; the header block: each line carries markdown's hard break
      ;; (two trailing spaces), so a renderer keeps them as lines
      (map (lambda (l) (string-append l "  "))
           (append
             (map (lambda (form)
                    (if (string:search (cdr form) "`" 0 (string-length (cdr form)))
                        ;; a template holding a backtick (quasiquote's
                        ;; abbreviations): double-tick delimiters
                        (format "**~a**: `` ~a ``" (car form) (cdr form))
                        (format "**~a**: `~a`" (car form) (cdr form))))
                  (doc:forms entry))
             (if (doc:returns entry)
                 (list (format "returns: ~a" (doc:returns entry)))
                 '())
             (if (pair? (doc:libraries entry))
                 (list (format "libraries: ~a"
                               (string:join (doc:libraries entry) ", ")))
                 '())
             (list (format "source: ~a, ~a"
                           (case (doc:source entry)
                             [(tspl) "TSPL4"]
                             [(csug) "Chez Scheme User's Guide"]
                             [else (doc:source entry)])
                           (doc:chapter entry)))
             (if (doc:url entry)
                 (list (format "url: ~a" (doc-browser-url entry)))
                 '())))
      (list "")
      (string:lines (doc:description entry))))

  (define (page-lines entries keys)
    ;; Render a structured page as Markdown buffer lines.
    (append
      (if (pair? keys)
          (list (format "**keys**: ~a  "
                        (string:join keys ", "))
                "")
          '())
      (let loop ([entries entries] [acc '()])
        (if (null? entries)
            (reverse acc)
            (loop (cdr entries)
                  (append
                    (reverse (entry-lines (car entries)))
                    (if (null? acc)
                        acc
                        (cons "" (cons (make-string 72 #\-)
                                       (cons "" acc))))))))))

  ;;; HTML to text ---------------------------------------------------------------

  ;; Both books are generated by the same LaTeX-to-HTML converter, so
  ;; one parser covers them.  An entry is one or more consecutive lines
  ;;
  ;;   <span class=formdef><b>KIND</b>: <tt>TEMPLATE</tt></span>
  ;;
  ;; followed by optional "returns:" and "libraries:" lines and prose
  ;; paragraphs, up to the next entry or section heading.

  (define (find-str s needle start)
    ;; Index of needle in s at or after start, or #f.
    (let ([n (string-length s)] [m (string-length needle)])
      (let loop ([i start])
        (cond [(> (+ i m) n) #f]
              [(let ok ([j 0])
                 (or (= j m)
                     (and (char=? (string-ref s (+ i j)) (string-ref needle j))
                          (ok (+ j 1)))))
               i]
              [else (loop (+ i 1))]))))

  ;; &nbsp; decodes to a hard space (code indentation and alignment), so
  ;; tidy can collapse the soft spaces that come from source-line
  ;; wrapping without touching it; hard spaces become plain at the end.
  (define hard-space #\xA0)

  (define (tag-name tag)
    ;; "b" for "b" or "/b"; "p" for "p align=..."
    (let* ([t (if (and (> (string-length tag) 0)
                       (char=? (string-ref tag 0) #\/))
                  (substring tag 1 (string-length tag))
                  tag)]
           [n (string-length t)])
      (let loop ([i 0])
        (if (and (< i n) (char-alphabetic? (string-ref t i)))
            (loop (+ i 1))
            (substring t 0 i)))))

  (define (closing-tag? tag)
    (and (> (string-length tag) 0) (char=? (string-ref tag 0) #\/)))

  (define (html->text html) (convert-html html #f))

  ;; The markdown variant, for description bodies: <tt> becomes inline
  ;; `code`, or a fenced block when it spans lines (the example groups);
  ;; <i> and <b> outside code become *emphasis* and **bold**.
  (define (html->markdown html) (convert-html html #t))

  (define (convert-html html md?)
    ;; <br> and <p> become line and paragraph breaks, the eval-arrow
    ;; images become =>, sup becomes ^, other tags vanish; entities are
    ;; decoded; the HTML source's own newlines are soft spaces.
    (let ([out (open-output-string)]
          [n (string-length html)]
          [tt #f])                ; capture inside a <tt> span, when md?
      (define (sink) (or tt out))
      (define (end-tt!)
        (let ([content (get-output-string tt)])
          (set! tt #f)
          (cond [(string=? content "") (void)]
                [(find-str content "\n" 0)
                 ;; an example group: a fenced scheme block.  Each
                 ;; line's soft leading spaces drop (artifacts of the
                 ;; HTML source's own line wrapping); the hard
                 ;; alignment spaces stay.
                 (let* ([end (let trim ([e (string-length content)])
                               (if (and (> e 0)
                                        (memv (string-ref content (- e 1))
                                              '(#\newline #\space)))
                                   (trim (- e 1))
                                   e))])
                   (put-string out "\n\n```scheme\n")
                   (let loop ([i 0] [line-start #t])
                     (when (< i end)
                       (let ([c (string-ref content i)])
                         (cond
                           [(and line-start (char=? c #\space))
                            (loop (+ i 1) #t)]
                           [else
                            (put-char out c)
                            (loop (+ i 1) (char=? c #\newline))]))))
                   (put-string out "\n```\n\n"))]
                [(find-str content "`" 0)
                 ;; a literal backtick inside: markdown's double-tick
                 ;; delimiters, spaces keeping the ticks apart
                 (put-string out "`` ")
                 (put-string out content)
                 (put-string out " ``")]
                [else (put-char out #\`)
                      (put-string out content)
                      (put-char out #\`)])))
      (let loop ([i 0])
        (when (< i n)
          (let ([c (string-ref html i)])
            (cond
              [(char=? c #\newline)
               (put-char (sink) #\space)
               (loop (+ i 1))]
              [(char=? c #\<)
               (let* ([end (let scan ([j (+ i 1)] [quoted #f])
                             ;; the tag's closing >, honoring quoted
                             ;; attribute values (alt="<graphic>")
                             (cond [(>= j n) (- n 1)]
                                   [(char=? (string-ref html j) #\")
                                    (scan (+ j 1) (not quoted))]
                                   [(and (not quoted)
                                         (char=? (string-ref html j) #\>))
                                    j]
                                   [else (scan (+ j 1) quoted)]))]
                      [tag (substring html (+ i 1) end)]
                      [name (tag-name tag)])
                 (cond [(string=? name "br") (put-char (sink) #\newline)]
                       [(and (string=? name "p") (not (closing-tag? tag)))
                        (put-string (sink) "\n\n")]
                       [(string=? name "img") (put-string (sink) " => ")]
                       [(and (string=? name "sup") (not (closing-tag? tag)))
                        (put-char (sink) #\^)]
                       [(and md? (string=? name "tt"))
                        (if (closing-tag? tag)
                            (when tt (end-tt!))
                            (unless tt (set! tt (open-output-string))))]
                       [(and md? (not tt) (string=? name "i"))
                        (put-char out #\*)]
                       [(and md? (not tt) (string=? name "b"))
                        (put-string out "**")])
                 (loop (+ end 1)))]
              [(char=? c #\&)
               (let* ([end (or (find-str html ";" i) (- n 1))]
                      [entity (and (< (- end i) 8)
                                   (substring html i (+ end 1)))])
                 (cond [(not entity) (put-char (sink) c) (loop (+ i 1))]
                       [(string=? entity "&nbsp;")
                        (put-char (sink) hard-space) (loop (+ end 1))]
                       [(string=? entity "&lt;")
                        (put-char (sink) #\<) (loop (+ end 1))]
                       [(string=? entity "&gt;")
                        (put-char (sink) #\>) (loop (+ end 1))]
                       [(string=? entity "&amp;")
                        (put-char (sink) #\&) (loop (+ end 1))]
                       [(string=? entity "&quot;")
                        (put-char (sink) #\") (loop (+ end 1))]
                       [else (put-char (sink) c) (loop (+ i 1))]))]
              [else (put-char (sink) c) (loop (+ i 1))]))))
      (when tt (end-tt!))
      (tidy (get-output-string out))))

  (define (tidy s)
    ;; Collapse soft-space runs, trim line edges, cap blank runs at one
    ;; line, drop leading/trailing blank lines, harden the hard spaces.
    (let ([out (open-output-string)]
          [n (string-length s)])
      (let loop ([i 0] [line-start? #t] [blanks 0] [pending-space #f] [any? #f])
        (if (= i n)
            (void)
            (let ([c (string-ref s i)])
              (cond
                [(char=? c #\space)
                 (loop (+ i 1) line-start? blanks (not line-start?) any?)]
                [(char=? c #\newline)
                 (loop (+ i 1) #t (+ blanks 1) #f any?)]
                [else
                 (when (and any? (> blanks 0))
                   (put-string out (if (> blanks 1) "\n\n" "\n")))
                 (when (and pending-space (not line-start?) (= blanks 0))
                   (put-char out #\space))
                 (put-char out (if (char=? c hard-space) #\space c))
                 (loop (+ i 1) #f 0 #f #t)]))))
      (get-output-string out)))

  ;;; Entry parsing ---------------------------------------------------------------

  (define formdef-mark "<span class=formdef>")

  (define (formdef-positions s)
    (let loop ([i 0] [acc '()])
      (let ([p (find-str s formdef-mark i)])
        (if p (loop (+ p 1) (cons p acc)) (reverse acc)))))

  (define (parse-formdef s pos)
    ;; -> (values kind template end-of-span)
    (let* ([end (or (find-str s "</span>" pos) (string-length s))]
           [inner (substring s (+ pos (string-length formdef-mark)) end)]
           [kb (find-str inner "<b>" 0)]
           [ke (find-str inner "</b>" 0)]
           [kind (if (and kb ke)
                     (html->text (substring inner (+ kb 3) ke))
                     "")]
           [colon (and ke (find-str inner ":" ke))]
           [template (if colon
                         (html->text (substring inner (+ colon 1)
                                                (string-length inner)))
                         "")])
      (values kind template (+ end 7))))

  (define (separators-only? s)
    ;; Is this inter-formdef gap just whitespace, breaks, and anchors?
    (let ([t (html->text s)])
      (let loop ([i 0])
        (or (= i (string-length t))
            (and (memv (string-ref t i) '(#\space #\newline))
                 (loop (+ i 1)))))))

  (define (template-names template)
    ;; The symbols a template defines: the operator of a parenthesized
    ;; form, or a bare token; quote/backquote/hash shorthands define
    ;; nothing themselves.
    (define (token-end i)
      (let loop ([i i])
        (if (or (= i (string-length template))
                (memv (string-ref template i) '(#\space #\( #\) #\[ #\])))
            i
            (loop (+ i 1)))))
    (let ([n (string-length template)])
      (cond [(= n 0) '()]
            [(char=? (string-ref template 0) #\()
             (let ([end (token-end 1)])
               (if (> end 1)
                   (list (string->symbol (substring template 1 end)))
                   '()))]
            [(memv (string-ref template 0) '(#\' #\` #\, #\#)) '()]
            [(= (token-end 0) n) (list (string->symbol template))]
            [else '()])))

  (define (extract-title s)
    (let* ([a (find-str s "<title>" 0)]
           [b (and a (find-str s "</title>" a))])
      (if (and a b) (html->text (substring s (+ a 7) b)) "")))

  (define (nearest-anchor s pos)
    ;; The last <a name="./..."> before pos, for the entry's url.
    (let loop ([i 0] [best #f])
      (let ([p (find-str s "<a name=\"./" i)])
        (if (and p (< p pos))
            (loop (+ p 1) p)
            (and best
                 (let ([end (find-str s "\"" (+ best 9))])
                   (and end (substring s (+ best 9) end))))))))

  (define (parse-labeled-line body label start limit)
    ;; The text of a `<b>label</b>value` line near start: (value . end),
    ;; or #f when the label is not there.
    (let ([p (find-str body label start)])
      (and p (< p limit)
           (let* ([vs (+ p (string-length label))]
                  [br (find-str body "<br>" vs)]
                  [blank (find-str body "\n\n" vs)]
                  [end (min (or br (string-length body))
                            (or blank (string-length body)))])
             (cons (html->text (substring body vs end)) end)))))

  (define (split-libraries s)
    ;; "(rnrs base), (rnrs)" -> ("(rnrs base)" "(rnrs)")
    (let loop ([i 0] [start 0] [depth 0] [acc '()])
      (define (grab end acc)
        (let ([piece (html->text (substring s start end))])
          (if (string=? piece "") acc (cons piece acc))))
      (cond [(= i (string-length s)) (reverse (grab i acc))]
            [(char=? (string-ref s i) #\()
             (loop (+ i 1) start (+ depth 1) acc)]
            [(char=? (string-ref s i) #\))
             (loop (+ i 1) start (- depth 1) acc)]
            [(and (char=? (string-ref s i) #\,) (= depth 0))
             (loop (+ i 1) (+ i 1) 0 (grab i acc))]
            [else (loop (+ i 1) start depth acc)])))

  (define (parse-body body)
    ;; -> (values returns libraries description)
    (let* ([r (parse-labeled-line body "<b>returns: </b>" 0 200)]
           [l (parse-labeled-line body "<b>libraries: </b>"
                                  (if r (cdr r) 0)
                                  (+ (if r (cdr r) 0) 60))]
           [rest-start (cond [l (cdr l)] [r (cdr r)] [else 0])]
           [stop (let loop ([candidates '("<h2" "<h3" "<h4" "<hr")]
                            [best (string-length body)])
                   (if (null? candidates)
                       best
                       (loop (cdr candidates)
                             (let ([p (find-str body (car candidates)
                                                rest-start)])
                               (if (and p (< p best)) p best)))))])
      (values (and r (car r))
              (if l
                  (split-libraries
                    (substring body
                               (+ (find-str body "<b>libraries: </b>"
                                            (if r (cdr r) 0))
                                  18)
                               (cdr l)))
                  '())
              (html->markdown (substring body rest-start stop)))))

  (define (parse-page path source page)
    ;; Every entry of one chapter page, as raw entry lists.
    (let* ([s (file:read path)]
           [chapter (extract-title s)])
      ;; group consecutive formdefs separated only by breaks and anchors
      (let loop ([ps (formdef-positions s)] [entries '()])
        (if (null? ps)
            (reverse entries)
            (let gather ([pos (car ps)] [rest (cdr ps)] [forms '()])
              (let-values ([(kind template span-end) (parse-formdef s pos)])
                (let ([forms (cons (cons kind template) forms)])
                  (if (and (pair? rest)
                           (separators-only?
                             (substring s span-end (car rest))))
                      (gather (car rest) (cdr rest) forms)
                      (let* ([body-end (if (pair? rest)
                                           (car rest)
                                           (string-length s))]
                             [body (substring s span-end body-end)]
                             [forms (reverse forms)]
                             [names (apply append
                                           (map (lambda (f)
                                                  (template-names (cdr f)))
                                                forms))]
                             [anchor (nearest-anchor
                                       (substring s (max 0 (- pos 400)) pos)
                                       400)])
                        (let-values ([(returns libraries description)
                                      (parse-body body)])
                          (loop rest
                                (cons (list names forms returns libraries
                                            source chapter
                                            (if anchor
                                                (format "~a.html#~a"
                                                        page anchor)
                                                (format "~a.html" page))
                                            description)
                                      entries))))))))))))

  ;;; Fetching ------------------------------------------------------------------

  (define tspl-base "https://www.scheme.com/tspl4/")
  (define csug-base "https://cisco.github.io/ChezScheme/csug10.0/")

  (define tspl-pages
    '("binding" "control" "exceptions" "io" "libraries" "objects"
      "records" "syntax"))
  (define csug-pages
    '("binding" "compat" "control" "debug" "expeditor" "foreign" "io"
      "libraries" "numeric" "objects" "smgmt" "syntax" "system" "threads"))

  (define (doc-browser-url entry)
    ;; The entry's documentation in the browser: its page anchor made
    ;; absolute against its book's site.  Locally registered entries may
    ;; have no URL.
    (and (doc:url entry)
         (string-append (case (doc:source entry)
                          [(tspl) tspl-base]
                          [(csug) csug-base]
                          [else ""])
                        (doc:url entry))))

  (define (ensure-directory! path)
    (unless (file-directory? path) (mkdir path)))

  (define (fetch-book! ref book base pages progress)
    (for-each (lambda (page)
                (progress (format "~a/~a.html" book page))
                (https:download (format "~a~a.html" base page)
                                (format "~a/~a/~a.html" ref book page)))
              pages))

  (define (parse-book ref book source pages)
    (apply append
           (map (lambda (page)
                  (parse-page (format "~a/~a/~a.html" ref book page)
                              source page))
                pages)))

  (define (call-with-data-port output? use)
    ;; Chez's file combinators close only on normal return. Corpus reads
    ;; can exhaust sandbox fuel, so their ports need an unwind owner too.
    ;; Critical entry/exit closes the fuel gap during ownership transfer;
    ;; reading/writing in the body remains interruptible.
    (let ([path (data-path)] [port #f])
      (dynamic-wind #t
        (lambda () (set! port (if output? (open-output-file path 'replace) (open-input-file path))))
        (lambda () (use port))
        (lambda () (close-port port)))))

  (define (write-data! entries)
    (call-with-data-port #t
      (lambda (port)
        (display ";; generated by (reference) -- do not edit\n(\n" port)
        (for-each (lambda (e) (write e port) (newline port)) entries)
        (display ")\n" port))))

  (define (fetch!)
    ;; One fetch owns the downloaded chapter files at a time. Readers keep
    ;; the last complete index; no log or transport call runs under its lock.
    (dynamic-wind #t
      (lambda ()
        (with-mutex corpus-lock
          (when fetching? (error 'reference:fetch! "a reference fetch is already running"))
          (set! fetching? #t)))
      (lambda ()
        (let* ([ref (data-dir)]
               [total (+ (length tspl-pages) (length csug-pages))]
               [done 0]
               [progress (lambda (what)
                           (set! done (+ done 1))
                           (log:add! 'describe (format "Fetching ~a (~a/~a)" what done total) #f))])
          (ensure-directory! ref)
          (ensure-directory! (string-append ref "/tspl4"))
          (ensure-directory! (string-append ref "/csug"))
          (fetch-book! ref "tspl4" tspl-base tspl-pages progress)
          (fetch-book! ref "csug" csug-base csug-pages progress)
          (log:add! 'describe "Extracting the reference corpus..." #f)
          (let* ([data (append (parse-book ref "tspl4" 'tspl tspl-pages)
                               (parse-book ref "csug" 'csug csug-pages))]
                 [next (index-data data)])
            (with-mutex corpus-lock
              (write-data! data)
              (set! corpus next))
            (log:add! 'describe
              (format "Describe database ready: ~a entries covering ~a names"
                      (length (car next))
                      (hashtable-size (cdr next)))))
          (void)))
      (lambda () (with-mutex corpus-lock (set! fetching? #f)))))

  ;;; Loading and queries ------------------------------------------------------

  ;; Publish entries and their index together, never an empty/partial table.
  ;; The cache belongs to this base library instance; a reload reads the disk
  ;; afresh. Module registrations remain live and are never cached here.
  (define corpus-lock (make-mutex))
  (define corpus #f)
  (define fetching? #f)

  (define (index-data data)
    (let ([entries (map (lambda (entry) (apply doc:make entry)) data)]
          [by-name (make-eq-hashtable)])
      (for-each
        (lambda (entry)
          (for-each (lambda (name)
                      (eq-hashtable-update! by-name name (lambda (old) (cons entry old)) '()))
                    (doc:names entry)))
        (reverse entries))
      (cons entries by-name)))

  (define (load-data!)
    (with-mutex corpus-lock
      (or corpus
          (let ([next (index-data (if (file-exists? (data-path))
                                    (call-with-data-port #f read) '()))])
            (set! corpus next)
            next))))

  (define (doc-lookup name)
    ;; Corpus order (TSPL before CSUG), followed by current module entries.
    (let* ([snapshot (load-data!)]
           [name (if (string? name) (string->symbol name) name)])
      (append (eq-hashtable-ref (cdr snapshot) name '())
              (filter (lambda (entry) (memq name (doc:names entry))) (doc:entries)))))

  (define (doc-entries . maybe-pred)
    (let ([entries (append (car (load-data!)) (doc:entries))])
      (if (pair? maybe-pred) (filter (car maybe-pred) entries) entries))))
