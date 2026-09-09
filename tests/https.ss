#!/usr/bin/env scheme-script

;; The https module: HTTP framing against a local server, and the
;; default TLS connector against live hosts -- including the reject
;; paths, which are the security surface.  Run from the repository
;; root; the TLS checks report and skip when the network is absent.

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (https) https:) (prefix (sys) sys:) (prefix (test) test:))

     (define check test:check)
     (define native-tls-before (foreign-entry? "SSL_new"))
     (define destination (format "/tmp/e-https-download-~a" (get-process-id)))
     (define base "https://audit.invalid:8443/dir/start?old=1")
     (define ok "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
     (define (redirect location . status)
       (format "HTTP/1.1 ~a Redirect\r\nLocation: ~a\r\nContent-Length: 0\r\n\r\n"
               (if (pair? status) (car status) 302) location))
     (define (wire authority target)
       (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nConnection: close\r\n\r\n" target authority))
     (define (fetch kind url)
       (if (eq? kind 'get) (https:get url)
           (dynamic-wind void
             (lambda ()
               (https:download url destination)
               (call-with-input-file destination get-string-all))
             (lambda () (when (file-exists? destination) (delete-file destination))))))

     ;; One transport fixture observes public calls, wire bytes, and ownership.
     ;; An open predecessor at connect time or a duplicate close is observable.
     (define (captured replies thunk)
       (let ([opened 0] [closed 0] [connections '()] [requests '()])
         (define (connect host port)
           (set! connections (cons (list host port (- opened closed)) connections))
           (set! opened (+ opened 1))
           (let ([in (open-bytevector-input-port (string->utf8 (car replies)))])
             (set! replies (cdr replies))
             (https:make-channel
               (lambda (bv start count)
                 (let ([n (get-bytevector-n! in bv start count)]) (if (eof-object? n) 0 n)))
               (lambda (bv) (set! requests (cons (utf8->string bv) requests)))
               (lambda () (set! closed (+ closed 1)) (close-port in)))))
         (let ([outcome (guard (ex [else 'raised])
                          (parameterize ([https:backend 'native] [https:connector connect])
                            (thunk)))])
           (list outcome (reverse connections) (reverse requests) closed))))

     ;; Each consumer traverses the same table; the expected destination is
     ;; independent of the resolver and includes the actual HTTP Host field.
     (for-each
       (lambda (kind)
         (for-each
           (lambda (entry)
             (let ([location (car entry)] [host (cadr entry)] [port (caddr entry)]
                   [authority (cadddr entry)] [target (list-ref entry 4)])
               (check (list 'redirect kind location)
                 (captured (list (redirect location) ok) (lambda () (fetch kind base)))
                 (list "ok" (list '("audit.invalid" 8443 0) (list host port 0))
                       (list (wire "audit.invalid:8443" "/dir/start?old=1") (wire authority target)) 2))))
           '(("/next" "audit.invalid" 8443 "audit.invalid:8443" "/next")
             ("next" "audit.invalid" 8443 "audit.invalid:8443" "/dir/next")
             ("../next" "audit.invalid" 8443 "audit.invalid:8443" "/next")
             ("../../../next" "audit.invalid" 8443 "audit.invalid:8443" "/next")
             ("." "audit.invalid" 8443 "audit.invalid:8443" "/dir/")
             (".." "audit.invalid" 8443 "audit.invalid:8443" "/")
             ("/a//b/../" "audit.invalid" 8443 "audit.invalid:8443" "/a//")
             ("next?x=/../y" "audit.invalid" 8443 "audit.invalid:8443" "/dir/next?x=/../y")
             ("%2e%2e/next" "audit.invalid" 8443 "audit.invalid:8443" "/dir/%2e%2e/next")
             ("?v=2" "audit.invalid" 8443 "audit.invalid:8443" "/dir/start?v=2")
             ("?" "audit.invalid" 8443 "audit.invalid:8443" "/dir/start?")
             ("#part" "audit.invalid" 8443 "audit.invalid:8443" "/dir/start?old=1")
             ("" "audit.invalid" 8443 "audit.invalid:8443" "/dir/start?old=1")
             ("//other.invalid/next" "other.invalid" 443 "other.invalid" "/next")
             ("//other.invalid?x" "other.invalid" 443 "other.invalid" "/?x")
             ("HTTPS://other.invalid:9443/a/../b#part" "other.invalid" 9443 "other.invalid:9443" "/b")
             ("//[::1]:9443/next" "::1" 9443 "[::1]:9443" "/next")))
         (for-each
           (lambda (reply)
             (check (list 'failed-body-closes kind)
               (captured (list reply) (lambda () (fetch kind base)))
               (list 'raised '(("audit.invalid" 8443 0))
                     (list (wire "audit.invalid:8443" "/dir/start?old=1")) 1)))
           '("HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nx"
             "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nx"
             "HTTP/1.1 404 Missing\r\nContent-Length: 0\r\n\r\n"))
         (check (list 'relative-chain-and-five-redirect-statuses kind)
           (captured
             (append (map (lambda (location status) (redirect location status))
                          '("../next/page" "child" "?q=1" "/last/" "done") '(301 302 303 307 308))
                     (list ok))
             (lambda () (fetch kind base)))
           (list "ok" (make-list 6 '("audit.invalid" 8443 0))
                 (map (lambda (target) (wire "audit.invalid:8443" target))
                      '("/dir/start?old=1" "/next/page" "/next/child" "/next/child?q=1" "/last/" "/last/done")) 6))
         (for-each
           (lambda (location)
             (check (list 'invalid-redirect-closes kind location)
               (captured (list (redirect location)) (lambda () (fetch kind base)))
               (list 'raised '(("audit.invalid" 8443 0))
                     (list (wire "audit.invalid:8443" "/dir/start?old=1")) 1)))
           '("ftp://other.invalid/file" "//other.invalid:bad/file"))
         (check (list 'redirect-limit-closes kind)
           (let ([result (captured (make-list 6 (redirect "/again")) (lambda () (fetch kind base)))])
             (list (car result) (map caddr (cadr result)) (cadddr result)))
           '(raised (0 0 0 0 0 0) 6)))
       '(get download))
     (for-each
       (lambda (entry)
         (check (list 'request-authority (car entry))
           (captured (list ok) (lambda () (https:get (car entry))))
           (list "ok" (list (list (cadr entry) (caddr entry) 0))
                 (list (wire (cadddr entry) (list-ref entry 4))) 1)))
       '(("https://audit.invalid" "audit.invalid" 443 "audit.invalid" "/")
         ("https://audit.invalid:443?x#fragment" "audit.invalid" 443 "audit.invalid:443" "/?x")
         ("https://audit.invalid:/" "audit.invalid" 443 "audit.invalid:" "/")
         ("https://[::1]/path" "::1" 443 "[::1]" "/path")))
     (check 'explicit-close-owns-body-port
       (captured (list ok)
         (lambda ()
           (let ([response (https:request 'GET base)])
             (https:close! response)
             (https:close! response)
             (port-closed? (https:response-port response)))))
       (list #t '(("audit.invalid" 8443 0)) (list (wire "audit.invalid:8443" "/dir/start?old=1")) 1))
     (check 'failed-destination-closes-response
       (captured (list ok)
         (lambda () (https:download base (string-append destination "/missing/file"))))
       (list 'raised '(("audit.invalid" 8443 0)) (list (wire "audit.invalid:8443" "/dir/start?old=1")) 1))
     (for-each
       (lambda (url)
         (check (list 'invalid-url-before-connect url)
           (captured '() (lambda () (https:get url))) '(raised () () 0)))
       '("/relative" "https:///missing" "https://host:bad/" "https://host:65536/"
         "https://[::1/" "https://user@host/" "https://host/a\r\nInjected:yes"))
     (check 'custom-transport-does-not-initialize-native-tls
       (foreign-entry? "SSL_new") native-tls-before)

     ;; Run only the deterministic transport checks while developing them.
     (when (member "--memory" (command-line-arguments)) (test:finish! 'https) (exit))

     ;; -- a local fixture server: fixed responses over plain TCP ------

     (define server-script
       (string-append
         "import socket\n"
         "s = socket.socket()\n"
         "s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)\n"
         "s.bind((\"127.0.0.1\", 0))\n"
         "s.listen(5)\n"
         "print(\"READY\", s.getsockname()[1], flush=True)\n"
         "while True:\n"
         "    c, _ = s.accept()\n"
         "    req = b\"\"\n"
         "    while b\"\\r\\n\\r\\n\" not in req:\n"
         "        d = c.recv(4096)\n"
         "        if not d: break\n"
         "        req += d\n"
         "    parts = req.split(b\"\\r\\n\", 1)[0].decode().split()\n"
         "    path = parts[1] if len(parts) > 1 else \"/\"\n"
         "    authority = b\"\\r\\nHost: 127.0.0.1:\" + str(s.getsockname()[1]).encode() + b\"\\r\\n\"\n"
         "    if authority not in req:\n"
         "        c.sendall(b\"HTTP/1.1 400 Bad Host\\r\\nContent-Length: 0\\r\\n\\r\\n\")\n"
         "    elif path == \"/plain\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\nContent-Length: 11\\r\\n\\r\\nhello world\")\n"
         "    elif path == \"/chunked\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n6\\r\\nchunk \\r\\n3\\r\\none\\r\\nB\\r\\n and chunk2\\r\\n0\\r\\n\\r\\n\")\n"
         "    elif path == \"/redirect\":\n"
         "        c.sendall(b\"HTTP/1.1 302 Found\\r\\nLocation: nested/redirect\\r\\nContent-Length: 0\\r\\n\\r\\n\")\n"
         "    elif path == \"/nested/redirect\":\n"
         "        c.sendall(b\"HTTP/1.1 307 Redirect\\r\\nLocation: ../plain\\r\\nContent-Length: 0\\r\\n\\r\\n\")\n"
         "    elif path == \"/eof\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\n\\r\\nstreamed to eof\")\n"
         "    elif path == \"/truncated\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\nContent-Length: 5\\r\\n\\r\\nx\")\n"
         "    elif path == \"/broken-head\":\n"
         "        c.sendall(b\"malformed\\r\\n\\r\\n\")\n"
         "    elif path == \"/slow\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\nContent-Length: 10000\\r\\n\\r\\nx\")\n"
         "        c.settimeout(3)\n"
         "        try: c.recv(1)\n"
         "        except (TimeoutError, ConnectionResetError): pass\n"
         "    else:\n"
         "        c.sendall(b\"HTTP/1.1 404 Not Found\\r\\nContent-Length: 9\\r\\n\\r\\nnot found\")\n"
         "    c.close()\n"))

     ;; The same integration table exercises both consumers and backends.
     ;; The fixture is itself an owned child; request cleanup must leave it
     ;; available for later requests, and closing the fixture must reap it.
     (let ([before (list (test:child-pids) (test:fd-count))] [server #f] [server-port #f])
       (define (local path) (format "http://127.0.0.1:~a~a" server-port path))
       (dynamic-wind #t
         (lambda () (set! server (sys:open-process (list "python3" "-u" "-c" server-script))))
         (lambda ()
           (sys:write-process! server #f)
           (let ([line (get-line (transcoded-port (sys:process-input server) (native-transcoder)))])
             (unless (and (string? line) (> (string-length line) 6) (string=? (substring line 0 6) "READY "))
               (error 'https-test "fixture server did not start" line))
             (set! server-port (string->number (substring line 6 (string-length line)))))
           (for-each
             (lambda (backend)
               (let ([resources (list (test:child-pids) (test:fd-count))])
                 (parameterize ([https:backend backend])
                   (for-each
                     (lambda (kind)
                       (check (list 'framing backend kind)
                         (map (lambda (path) (fetch kind (local path))) '("/plain" "/chunked" "/eof" "/redirect"))
                         '("hello world" "chunk one and chunk2" "streamed to eof" "hello world"))
                       (check (list 'response-errors backend kind)
                         (map (lambda (path) (test:raises? (lambda () (fetch kind (local path)))))
                              '("/missing" "/truncated" "/broken-head"))
                         '(#t #t #t)))
                     '(get download))
                   (let ([response (https:request 'GET (local "/plain"))])
                     (check (list 'response backend)
                       (list (https:response-status response)
                             (cdr (assoc "content-type" (https:response-headers response)))
                             (https:response-text response))
                       '(200 "text/plain" "hello world")))
                   (let* ([start (current-time 'time-monotonic)]
                          [response (https:request 'GET (local "/slow"))]
                          [byte (get-u8 (https:response-port response))])
                     (https:close! response)
                     (https:close! response)
                     (check (list 'stream-before-completion-and-close backend)
                       (list (https:response-status response) byte
                             (port-closed? (https:response-port response))
                             (< (time-second (time-difference (current-time 'time-monotonic) start)) 1))
                       '(200 120 #t #t))))
                 (check (list 'requests-release-resources backend)
                   (list (test:child-pids) (test:fd-count)) resources)))
             '(native curl)))
         (lambda () (sys:close-process! server)))
       (check 'local-fixture-releases-resources (list (test:child-pids) (test:fd-count)) before))

     ;; -- the TLS connector, against live hosts ------------------------

     (define network
       (guard (ex [else #f]) (https:get "https://example.com/")))

     (if (not network)
         (display "TLS skipped: no network\n")
         (for-each
           (lambda (backend)
             (parameterize ([https:backend backend])
               (check (list 'tls-fetches backend)
                 (> (string-length (if (eq? backend 'native) network (https:get "https://example.com/"))) 0) #t)
               (check (list 'tls-rejects backend)
                 (map (lambda (url) (test:raises? (lambda () (https:get url))))
                      '("https://wrong.host.badssl.com/" "https://expired.badssl.com/"))
                 '(#t #t))))
           '(native curl)))
     (test:finish! 'https)))
