#!/usr/bin/env scheme-script

;; The https module: HTTP framing against a local server, and the
;; default TLS connector against live hosts -- including the reject
;; paths, which are the security surface.  Run from the repository
;; root; the TLS checks report and skip when the network is absent.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (https))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'https-test label actual expected)))

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
         "    if path == \"/plain\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\nContent-Length: 11\\r\\n\\r\\nhello world\")\n"
         "    elif path == \"/chunked\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n6\\r\\nchunk \\r\\n3\\r\\none\\r\\nB\\r\\n and chunk2\\r\\n0\\r\\n\\r\\n\")\n"
         "    elif path == \"/redirect\":\n"
         "        c.sendall(b\"HTTP/1.1 302 Found\\r\\nLocation: /plain\\r\\nContent-Length: 0\\r\\n\\r\\n\")\n"
         "    elif path == \"/eof\":\n"
         "        c.sendall(b\"HTTP/1.1 200 OK\\r\\n\\r\\nstreamed to eof\")\n"
         "    else:\n"
         "        c.sendall(b\"HTTP/1.1 404 Not Found\\r\\nContent-Length: 9\\r\\n\\r\\nnot found\")\n"
         "    c.close()\n"))

     (define-values (to-server from-server server-error server-pid)
       (let ([path (format "/tmp/e-https-test-~a.py" (getenv "USER"))])
         (call-with-output-file path
           (lambda (port) (put-string port server-script))
           'replace)
         (open-process-ports (format "exec python3 ~a" path)
                             'block (native-transcoder))))

     (define server-port
       (let ([line (get-line from-server)])
         (unless (and (string? line)
                      (> (string-length line) 6)
                      (string=? (substring line 0 6) "READY "))
           (error 'https-test "fixture server did not start" line))
         (string->number (substring line 6 (string-length line)))))

     (define (local path)
       (format "http://127.0.0.1:~a~a" server-port path))

     ;; -- framing ------------------------------------------------------

     (check 'content-length-body (https-get (local "/plain")) "hello world")

     (check 'chunked-body (https-get (local "/chunked"))
            "chunk one and chunk2")

     (check 'read-to-close-body (https-get (local "/eof"))
            "streamed to eof")

     (check 'redirect-followed (https-get (local "/redirect"))
            "hello world")

     (check 'error-status-raises
            (guard (ex [else 'raised]) (https-get (local "/missing")))
            'raised)

     (let ([response (https-request 'GET (local "/plain"))])
       (check 'response-status (https-response-status response) 200)
       (check 'response-header
              (cdr (assoc "content-type" (https-response-headers response)))
              "text/plain")
       (check 'response-streams (https-response-text response)
              "hello world"))

     (let ([path (format "/tmp/e-https-download-~a" (getenv "USER"))])
       (https-download (local "/plain") path)
       (check 'download-writes-the-body
              (call-with-input-file path get-string-all)
              "hello world")
       (delete-file path))

     (system (format "kill ~a 2>/dev/null" server-pid))

     ;; -- the TLS connector, against live hosts ------------------------

     (define network
       (guard (ex [else #f]) (https-get "https://example.com/")))

     (if (not network)
         (format #t "~a https checks passed (TLS skipped: no network)\n"
                 checks)
         (begin
           (check 'tls-fetches
                  (and (string? network) (> (string-length network) 0)) #t)
           ;; the reject paths are the point: a hostname mismatch and an
           ;; expired certificate must fail the connect loudly
           (check 'tls-rejects-wrong-host
                  (guard (ex [else 'rejected])
                    (https-get "https://wrong.host.badssl.com/")
                    'accepted)
                  'rejected)
           (check 'tls-rejects-expired
                  (guard (ex [else 'rejected])
                    (https-get "https://expired.badssl.com/")
                    'accepted)
                  'rejected)
           (format #t "~a https checks passed\n" checks)))))
