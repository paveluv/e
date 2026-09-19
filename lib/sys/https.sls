;; https.sls -- an HTTP(S) client for the e editor, TLS via the system's
;; libssl through Chez's FFI.
;;
;; An e extension module: the library (https), infrastructure with no
;; init!.  Nothing loads until the first request, so the editor starts
;; fine on a system without libssl and only a request reports it.
;;
;; The module is layered so the transport can be swapped without
;; touching the HTTP client:
;;
;;   channel        -- a record of read!/write!/close! procedures over
;;                     an established byte stream.  make-channel is
;;                     exported: any transport that can carry bytes can
;;                     wear it.
;;   https:connector -- a parameter holding (lambda (host port) channel)
;;                     for the secure transport.  The default speaks
;;                     TLS via libssl over an FFI socket; a pure-Scheme
;;                     TLS, an `openssl s_client` pipe, or a test
;;                     double can replace it wholesale.
;;   HTTP client    -- request writing and response framing
;;                     (Content-Length, chunked, read-to-close) written
;;                     only against the channel.
;;
;; Security posture of the default connector: certificate verification
;; against the system root store (SSL_CTX_set_default_verify_paths +
;; SSL_VERIFY_PEER) and hostname checking (SSL_set1_host), with SNI.
;; A failed verification fails the connect loudly.
;;
;; Blocking calls (DNS, connect, TLS reads and writes) are declared
;; __collect_safe so a stalled network peer never stalls the collector
;; -- and therefore never freezes the editor's other threads.  For the
;; same reason they exchange bytes through foreign buffers, never
;; through Scheme bytevectors that the collector could move mid-call.

(import (only (edoc) elibrary))
(elibrary (https)
  (export (rename (https-get get)) (rename (https-download download)) (rename (https-request request))
          (rename (https-response-status response-status)) (rename (https-response-headers response-headers)) (rename (https-response-port response-port))
          (rename (https-response-text response-text)) (rename (https-close! close!))
          (rename (https-connector connector)) (rename (https-timeout timeout)) (rename (https-backend backend))
          make-channel channel-read! channel-write! channel-close!
          tcp-connect tls-connect)
  (import (chezscheme) (prefix (string) string:) (prefix (sys) sys:))

  ;;; Foreign library loading ---------------------------------------------

  (define (try-load-shared names)
    (and (pair? names)
         (or (guard (ex [else #f]) (load-shared-object (car names)) #t)
             (try-load-shared (cdr names)))))

  (define os
    ;; The machine-type suffix names the platform: ...le is Linux,
    ;; ...fb FreeBSD, ...osx macOS, ...ob and ...nb the other BSDs.
    (let ([name (symbol->string (machine-type))])
      (define (suffix? s)
        (let ([n (string-length name)] [m (string-length s)])
          (and (>= n m) (string=? (substring name (- n m) n) s))))
      (cond [(suffix? "le") 'linux]
            [(suffix? "fb") 'freebsd]
            [(suffix? "osx") 'macos]
            [(suffix? "ob") 'openbsd]
            [(suffix? "nb") 'netbsd]
            [else 'linux])))

  (define bsd-sockets? (memq os '(freebsd macos openbsd netbsd)))

  ;; The sonames to probe, per platform.  On macOS the unversioned
  ;; /usr/lib stubs ABORT THE PROCESS when loaded (Apple removed the
  ;; ABI): never probe a bare .dylib there -- only the versioned
  ;; libraries Homebrew and MacPorts install, by their absolute homes
  ;; first since dlopen's default search may not cover them.
  (define (versioned-dylibs stem)
    (append
      (map (lambda (root) (format "~a/lib/lib~a.3.dylib" root stem))
           '("/opt/homebrew/opt/openssl@3" "/opt/homebrew"
             "/usr/local/opt/openssl@3" "/usr/local" "/opt/local"))
      (list (format "lib~a.3.dylib" stem)
            (format "lib~a.1.1.dylib" stem))))

  (define (tls-names stem)
    (if (eq? os 'macos)
        (versioned-dylibs stem)
        (list (format "lib~a.so.3" stem) (format "lib~a.so.1.1" stem)
              (format "lib~a.so.30" stem) (format "lib~a.so.111" stem)
              (format "lib~a.so" stem))))

  (define tls-loaded
    (let ([state 'no])
      (lambda ()
        (when (eq? state 'no)
          ;; libssl usually pulls libcrypto in; loading it first is
          ;; belt and braces and may fail silently.
          (try-load-shared (tls-names "crypto"))
          (set! state
            (if (try-load-shared (tls-names "ssl")) 'yes 'missing)))
        (when (eq? state 'missing)
          (error 'https
                 (if (eq? os 'macos)
                     "no TLS library found: install OpenSSL (brew install openssl@3)"
                     "no TLS library found (libssl)"))))))

  (define-syntax define-foreign
    ;; A lazily created binding: the shared object loads on first call.
    (syntax-rules ()
      [(_ name loaded entry (arg ...) result)
       (define name
         (let ([procedure #f])
           (lambda args
             (unless procedure
               (loaded)
               (set! procedure
                 (foreign-procedure entry (arg ...) result)))
             (apply procedure args))))]))

  (define-syntax define-foreign-blocking
    (syntax-rules ()
      [(_ name loaded entry (arg ...) result)
       (define name
         (let ([procedure #f])
           (lambda args
             (unless procedure
               (loaded)
               (set! procedure
                 (foreign-procedure __collect_safe entry (arg ...) result)))
             (apply procedure args))))]))

  (define libc-loaded
    ;; The process links libc, but its symbols still need the object
    ;; in the lookup namespace.
    (let ([done #f])
      (lambda ()
        (unless done
          (try-load-shared
            (if (eq? os 'macos)
                '("libSystem.B.dylib")
                '("libc.so.6" "libc.so.7" "libc.so")))
          (set! done #t)))))

  ;;; Foreign memory helpers ----------------------------------------------

  (define (call-with-foreign buffers use)
    ;; Own each allocation before copying, which may exhaust an engine.
    ;; The copies remain immovable throughout collect-safe foreign calls.
    (let ([pointers '()])
      (call-with-scope void
        (lambda ()
          (apply use
            (map (lambda (bytes)
                   (let ([pointer
                          (with-interrupts-disabled
                            (let ([pointer (foreign-alloc (bytevector-length bytes))])
                              (set! pointers (cons pointer pointers))
                              pointer))])
                     (copy-to-foreign! bytes 0 (bytevector-length bytes) pointer)
                     pointer))
                 buffers)))
        (lambda () (for-each foreign-free pointers)))))

  (define (cstring text) (string->utf8 (string-append text "\x0;")))

  (define (foreign-cstring pointer)
    (let loop ([i 0] [acc '()])
      (let ([byte (foreign-ref 'unsigned-8 pointer i)])
        (if (zero? byte)
            (utf8->string (u8-list->bytevector (reverse acc)))
            (loop (+ i 1) (cons byte acc))))))

  (define (copy-to-foreign! bv start count pointer)
    (do ([i 0 (+ i 1)]) ((= i count))
      (foreign-set! 'unsigned-8 pointer i
                    (bytevector-u8-ref bv (+ start i)))))

  (define (copy-from-foreign! pointer bv start count)
    (do ([i 0 (+ i 1)]) ((= i count))
      (bytevector-u8-set! bv (+ start i)
                          (foreign-ref 'unsigned-8 pointer i))))

  ;;; Sockets (libc) --------------------------------------------------------

  (define-foreign c-socket libc-loaded "socket" (int int int) int)
  (define-foreign c-close libc-loaded "close" (int) int)
  (define-foreign c-setsockopt libc-loaded "setsockopt"
    (int int int uptr int) int)
  (define-foreign c-freeaddrinfo libc-loaded "freeaddrinfo" (uptr) void)
  (define-foreign c-gai-strerror libc-loaded "gai_strerror" (int) uptr)
  (define-foreign-blocking c-connect libc-loaded "connect"
    (int uptr int) int)
  (define-foreign-blocking c-getaddrinfo libc-loaded "getaddrinfo"
    (uptr uptr uptr uptr) int)
  (define-foreign-blocking c-read libc-loaded "read" (int uptr long) long)
  (define-foreign-blocking c-write libc-loaded "write" (int uptr long) long)

  ;; struct addrinfo (64-bit): flags, family, socktype, protocol at
  ;; 0/4/8/12 and addrlen at 16 everywhere; then glibc orders addr,
  ;; canonname at 24/32 while the BSDs and macOS order canonname, addr
  ;; -- reading the wrong slot hands connect a canonname pointer.
  ;; next sits at 40 on both.
  (define addrinfo-addr-offset (if bsd-sockets? 32 24))
  (define (addrinfo-family info) (foreign-ref 'int info 4))
  (define (addrinfo-addrlen info) (foreign-ref 'unsigned-32 info 16))
  (define (addrinfo-addr info)
    (foreign-ref 'void* info addrinfo-addr-offset))
  (define (addrinfo-next info)
    (let ([next (foreign-ref 'void* info 40)])
      (and (not (zero? next)) next)))

  (edoc "Seconds a stalled peer may hold a read or write before the request fails."
        (value integer))
  (define https-timeout
    ;; Seconds a stalled peer may hold a read or write before the
    ;; request fails.
    (make-parameter 60
      (lambda (seconds)
        (unless (and (fixnum? seconds) (> seconds 0))
          (error 'https-timeout "must be a positive integer" seconds))
        seconds)))

  ;; Linux numbers the socket level and timeout options 1/20/21; the
  ;; BSDs and macOS use #xffff/#x1006/#x1005.
  (define sol-socket (if bsd-sockets? #xffff 1))
  (define so-rcvtimeo (if bsd-sockets? #x1006 20))
  (define so-sndtimeo (if bsd-sockets? #x1005 21))

  (define (set-socket-timeouts! fd seconds)
    ;; SO_RCVTIMEO/SO_SNDTIMEO with a struct timeval (two longs).
    (call-with-foreign (list (make-bytevector 16 0))
      (lambda (time)
        (foreign-set! 'long time 0 seconds)
        (c-setsockopt fd sol-socket so-rcvtimeo time 16)
        (c-setsockopt fd sol-socket so-sndtimeo time 16)))
    ;; and a write to a peer-closed connection must error, not raise
    ;; SIGPIPE: SO_NOSIGPIPE where it exists (macOS #x1022, FreeBSD
    ;; #x800); Linux writes report EPIPE to blocked signals anyway
    (let ([option (case os [(macos) #x1022] [(freebsd) #x800] [else #f])])
      (when option
        (call-with-foreign (list (make-bytevector 4 0))
          (lambda (on)
            (foreign-set! 'int on 0 1)
            (c-setsockopt fd sol-socket option on 4))))))

  (define (connect-socket host port open!)
    ;; The native channel owns every socket opened while trying addresses.
    ;; DNS writes its result into owned memory, even if expiry follows the
    ;; foreign return before Scheme has read that pointer.
    (call-with-foreign
      (list (cstring host) (cstring (number->string port))
            (make-bytevector 48 0) (make-bytevector 8 0))
      (lambda (host* port* hints result*)
        (foreign-set! 'int hints 8 1)   ; ai_socktype = SOCK_STREAM
        (call-with-scope void
          (lambda ()
            (let ([status (c-getaddrinfo host* port* hints result*)])
              (unless (zero? status)
                (error 'https
                       (format "cannot resolve ~a: ~a"
                               host (foreign-cstring (c-gai-strerror status)))))
              (let try ([info (foreign-ref 'void* result* 0)])
                (unless (and info (not (zero? info)))
                  (error 'https (format "cannot connect to ~a:~a" host port)))
                (let ([fd (open! (addrinfo-family info))])
                  (if (and (>= fd 0)
                           (zero? (c-connect fd (addrinfo-addr info) (addrinfo-addrlen info))))
                      (set-socket-timeouts! fd (https-timeout))
                      (try (addrinfo-next info)))))))
          (lambda ()
            (let ([first (foreign-ref 'void* result* 0)])
              (unless (zero? first) (c-freeaddrinfo first))))))))

  ;;; Channels ---------------------------------------------------------------

  ;; A channel is the transport abstraction: read! fills a bytevector
  ;; range and returns the count (0 at orderly close), write! sends a
  ;; whole bytevector, close! releases the transport.
  (edoc "A byte channel to a peer: how to read, write and close it."
        (read! procedure "(read! bytevector start count) giving the bytes read")
        (write! procedure "(write! bytevector start count)")
        (close! thunk "closes the channel"))
  (define-record-type channel
    (fields read! write! close!))

  (define transfer-buffer-size 32768)

  ;; Native setup can block before returning a channel. Install its result
  ;; in the active request atomically, including through connector wrappers.
  (define adopt-channel! (make-parameter values))

  (edoc "A plain TCP channel to a host and port."
        (host string "the host")
        (port integer "the port")
        (returns (record channel)))
  (define (tcp-connect host port)
    (native-connect host port #f))

  (edoc "A TLS channel to a host and port, verified."
        (host string "the host")
        (port integer "the port")
        (returns (record channel)))
  (define (tls-connect host port)
    (native-connect host port #t))

  ;;; TLS (libssl) -----------------------------------------------------------

  (define-foreign ssl-client-method tls-loaded "TLS_client_method" () uptr)
  (define-foreign ssl-ctx-new tls-loaded "SSL_CTX_new" (uptr) uptr)
  (define-foreign ssl-ctx-free tls-loaded "SSL_CTX_free" (uptr) void)
  (define-foreign-blocking ssl-ctx-set-default-verify-paths tls-loaded
    "SSL_CTX_set_default_verify_paths" (uptr) int)
  (define-foreign ssl-ctx-set-verify tls-loaded "SSL_CTX_set_verify"
    (uptr int uptr) void)
  (define-foreign ssl-new tls-loaded "SSL_new" (uptr) uptr)
  (define-foreign ssl-free tls-loaded "SSL_free" (uptr) void)
  (define-foreign ssl-set-fd tls-loaded "SSL_set_fd" (uptr int) int)
  (define-foreign ssl-ctrl tls-loaded "SSL_ctrl" (uptr int long uptr) long)
  (define-foreign ssl-set1-host tls-loaded "SSL_set1_host" (uptr string) int)
  (define-foreign ssl-get-error tls-loaded "SSL_get_error" (uptr int) int)
  (define-foreign ssl-get-verify-result tls-loaded "SSL_get_verify_result"
    (uptr) long)
  (define-foreign x509-verify-error-string tls-loaded
    "X509_verify_cert_error_string" (long) uptr)
  (define-foreign-blocking ssl-shutdown tls-loaded "SSL_shutdown" (uptr) int)
  (define-foreign-blocking ssl-connect-call tls-loaded "SSL_connect"
    (uptr) int)
  (define-foreign-blocking ssl-read tls-loaded "SSL_read" (uptr uptr int) int)
  (define-foreign-blocking ssl-write tls-loaded "SSL_write"
    (uptr uptr int) int)

  (define ssl-verify-peer 1)
  (define ssl-ctrl-set-tlsext-hostname 55)
  (define ssl-error-zero-return 6)

  (define (native-connect host port secure?)
    ;; One owner covers partial setup and the live channel.
    (let ([fd -1] [ctx 0] [ssl 0] [ready? #f] [transferred? #f]
          [in-buffer #f] [out-buffer #f] [open? #t])
      (define (close!)
        (with-interrupts-disabled
          (when open?
            (set! open? #f)
            (unless (zero? ssl)
              (when ready? (guard (ex [else (void)]) (ssl-shutdown ssl)))
              (ssl-free ssl))
            (unless (zero? ctx) (ssl-ctx-free ctx))
            (when (>= fd 0) (c-close fd))
            (when in-buffer (foreign-free in-buffer))
            (when out-buffer (foreign-free out-buffer)))))
      (define (check-open!)
        (unless open? (error 'https "connection is closed")))
      (define (read! bv start count)
        (check-open!)
        (let* ([limit (min count transfer-buffer-size)]
               [got (if secure? (ssl-read ssl in-buffer limit) (c-read fd in-buffer limit))]
               [n (cond [(> got 0) got]
                        [secure?
                         (let ([status (ssl-get-error ssl got)])
                           (if (= status ssl-error-zero-return) 0
                               (error 'https (format "TLS read failed (status ~a)" status))))]
                        [(zero? got) 0]
                        [else (error 'https "read failed (timeout or reset)")])])
          (copy-from-foreign! in-buffer bv start n)
          n))
      (define (write! bv)
        (check-open!)
        (let send ([start 0])
          (let ([left (- (bytevector-length bv) start)])
            (when (> left 0)
              (let ([count (min left transfer-buffer-size)])
                (copy-to-foreign! bv start count out-buffer)
                (let ([sent (if secure? (ssl-write ssl out-buffer count) (c-write fd out-buffer count))])
                  (unless (> sent 0) (error 'https "connection closed while writing"))
                  (send (+ start sent))))))))
      (call-with-scope void
        (lambda ()
          (connect-socket host port
            (lambda (family)
              (with-interrupts-disabled
                (when (>= fd 0) (c-close fd) (set! fd -1))
                (set! fd (c-socket family 1 0))
                fd)))
          (when secure?
            (let ([method (ssl-client-method)])
              (with-interrupts-disabled (set! ctx (ssl-ctx-new method))))
            (when (zero? ctx) (error 'https "SSL_CTX_new failed"))
            (unless (= 1 (ssl-ctx-set-default-verify-paths ctx))
              (error 'https "cannot load the default TLS trust store"))
            (ssl-ctx-set-verify ctx ssl-verify-peer 0)
            (with-interrupts-disabled (set! ssl (ssl-new ctx)))
            (when (zero? ssl) (error 'https "SSL_new failed"))
            (call-with-foreign (list (cstring host))
              (lambda (name)
                (unless (= 1 (ssl-ctrl ssl ssl-ctrl-set-tlsext-hostname 0 name))
                  (error 'https "cannot set TLS server name"))))
            (unless (= 1 (ssl-set1-host ssl host)) (error 'https "SSL_set1_host failed"))
            (unless (= 1 (ssl-set-fd ssl fd)) (error 'https "SSL_set_fd failed"))
            (unless (= 1 (ssl-connect-call ssl))
              (let ([verify (ssl-get-verify-result ssl)])
                (error 'https
                       (if (zero? verify) (format "TLS handshake with ~a failed" host)
                           (format "certificate for ~a rejected: ~a" host
                                   (foreign-cstring (x509-verify-error-string verify)))))))
            (set! ready? #t))
          (with-interrupts-disabled
            (set! in-buffer (foreign-alloc transfer-buffer-size))
            (set! out-buffer (foreign-alloc transfer-buffer-size))
            (let ([channel (make-channel read! write! close!)])
              ((adopt-channel!) channel)
              (set! transferred? #t)
              channel)))
        (lambda () (unless transferred? (close!))))))

  (define (tls-available?)
    (guard (ex [else #f]) (tls-loaded) #t))

  (edoc "The secure-transport provider: (connect host port) giving a channel."
        (value procedure))
  (define https-connector
    ;; The secure-transport provider: replace it to switch the TLS
    ;; implementation (a pure-Scheme TLS, an openssl pipe, a test
    ;; double) without touching the HTTP client.
    (make-parameter tls-connect
      (lambda (connect)
        (unless (procedure? connect)
          (error 'https-connector "expected a procedure (host port)"))
        connect)))

  ;;; HTTP ---------------------------------------------------------------

  (define-record-type uri (fields scheme authority path query))

  (define (uri-end text separators start end)
    (let scan ([i start])
      (if (or (= i end) (memv (string-ref text i) separators)) i (scan (+ i 1)))))

  (define (parse-reference url)
    ;; HTTP URI references share one grammar. Keep an absent query distinct
    ;; from an empty one; fragments never belong to a fetch target.
    (unless (and (string? url)
                 (for-all (lambda (c)
                            (let ([n (char->integer c)]) (and (> n 32) (not (<= 127 n 159)))))
                          (string->list url)))
      (error 'https "expected a URL without spaces or controls" url))
    (let* ([end (uri-end url '(#\#) 0 (string-length url))]
           [query-at (uri-end url '(#\?) 0 end)]
           [colon (uri-end url '(#\: #\/) 0 query-at)]
           [scheme (and (< colon query-at) (char=? (string-ref url colon) #\:)
                        (string-downcase (substring url 0 colon)))]
           [start (if scheme (+ colon 1) 0)]
           [authority? (and (<= (+ start 2) query-at)
                            (string=? (substring url start (+ start 2)) "//"))]
           [path-at (if authority? (uri-end url '(#\/) (+ start 2) query-at) start)]
           [authority (and authority? (substring url (+ start 2) path-at))])
      (when (and scheme (not (and (member scheme '("http" "https")) authority)))
        (error 'https "expected an http(s) URL" url))
      (make-uri scheme authority (substring url path-at query-at)
                (and (< query-at end) (substring url (+ query-at 1) end)))))

  (define (query-suffix query) (if query (string-append "?" query) ""))

  (define (normalize-path path)
    ;; RFC 3986 section 5.2: resolve complete dot segments, preserving empty
    ;; segments, trailing slashes and percent escapes. HTTP paths are rooted.
    (if (string=? path "") ""
        (let scan ([start 1] [parts '()])
          (let* ([end (uri-end path '(#\/) start (string-length path))]
                 [part (substring path start end)]
                 [parts (cond [(string=? part ".") parts]
                              [(string=? part "..") (if (pair? parts) (cdr parts) '())]
                              [else (cons part parts)])])
            (if (= end (string-length path))
                (string-append "/" (string:join
                                     (reverse (if (member part '("." "..")) (cons "" parts) parts)) "/"))
                (scan (+ end 1) parts))))))

  (define (resolve-url url location)
    (let* ([base (parse-reference url)] [ref (parse-reference location)]
           [replaces? (or (uri-scheme ref) (uri-authority ref))]
           [path (uri-path ref)]
           [same-path? (and (not replaces?) (string=? path ""))]
           [path (cond [same-path? (uri-path base)]
                       [(or replaces? (string:prefix? "/" path)) (normalize-path path)]
                       [else
                        (let* ([old (uri-path base)]
                               [end (let scan ([i (string-length old)])
                                      (if (or (zero? i) (char=? (string-ref old (- i 1)) #\/))
                                          i (scan (- i 1))))])
                          (normalize-path (string-append (if (zero? end) "/" (substring old 0 end)) path)))])])
      (string-append (or (uri-scheme ref) (uri-scheme base)) "://"
                     (if replaces? (uri-authority ref) (uri-authority base)) path
                     (query-suffix (if same-path? (or (uri-query ref) (uri-query base)) (uri-query ref))))))

  (define (parse-url url)
    ;; One authority supplies connection host/port and the HTTP Host field.
    ;; Strip IPv6 brackets only for the connector; preserve explicit ports.
    (let* ([ref (parse-reference url)] [authority (uri-authority ref)]
           [secure? (equal? (uri-scheme ref) "https")])
      (unless (and (uri-scheme ref) authority (> (string-length authority) 0))
        (error 'https "expected an http(s) URL" url))
      (let* ([n (string-length authority)] [bracket? (string:prefix? "[" authority)]
             [end (if bracket? (+ 1 (uri-end authority '(#\]) 1 n))
                      (uri-end authority '(#\:) 0 n))]
             [host (and (<= end n) (substring authority (if bracket? 1 0) (if bracket? (- end 1) end)))]
             [default (if secure? 443 80)]
             [port (cond [(= end n) default]
                         [(and (< end n) (char=? (string-ref authority end) #\:))
                          (let ([digits (substring authority (+ end 1) n)])
                            (if (string=? digits "") default
                                (and (for-all (lambda (c) (char<=? #\0 c #\9)) (string->list digits))
                                     (string->number digits))))]
                         [else #f])])
        (unless (and host (> (string-length host) 0) port (<= 0 port 65535)
                     (for-all (lambda (c) (not (memv c '(#\@ #\[ #\] #\\)))) (string->list host)))
          (error 'https "invalid HTTP authority" authority))
        (values secure? host port
                (string-append (if (string=? (uri-path ref) "") "/" (uri-path ref))
                               (query-suffix (uri-query ref))) authority))))

  (edoc "An HTTP response with its body still open."
        (status integer "the status code")
        (headers list "(name . value) header pairs")
        (port port "the body port"))
  (define-record-type https-response
    (fields status headers port))

  (edoc "Close a response's body port."
        (response (record https-response) "the response"))
  (define (https-close! response)
    (close-port (https-response-port response)))

  (define (call-with-scope start! use finish!)
    ;; Only acquisition/release is critical. The body may block or exhaust
    ;; its engine; cleanup must finish without replacing that original escape.
    (let ([ended? #f] [failure #f])
      (call-with-values
        (lambda ()
          (dynamic-wind #t
            (lambda ()
              (when ended? (error 'https "HTTP scope has ended"))
              (start!))
            use
            (lambda ()
              (set! ended? #t)
              (guard (ex [else (set! failure (list ex))]) (finish!)))))
        (lambda results
          (when failure (raise (car failure)))
          (apply values results)))))

  (define (call-with-body response consume)
    (call-with-scope void
      (lambda () (consume (https-response-port response)))
      (lambda () (https-close! response))))

  (define (header-ref headers name)
    (cond [(assoc name headers) => cdr] [else #f]))

  (define (read-until-blank-line channel)
    ;; -> (values header-text leftover-bytes) -- everything up to the
    ;; CRLFCRLF, and whatever body bytes followed it in the same reads.
    (let ([buffer (make-bytevector 4096)])
      (let loop ([acc '()] [total 0])
        (let ([got ((channel-read! channel) buffer 0 4096)])
          (when (zero? got)
            (error 'https "connection closed before response headers"))
          (let* ([chunk (let ([bv (make-bytevector got)])
                          (bytevector-copy! buffer 0 bv 0 got)
                          bv)]
                 [acc (cons chunk acc)]
                 [whole (join-bytevectors (reverse acc))]
                 [end (find-blank-line whole)])
            (if end
                (values (utf8->string
                          (bytevector-slice whole 0 end))
                        (bytevector-slice whole (+ end 4)
                                          (bytevector-length whole)))
                (loop acc (+ total got))))))))

  (define (join-bytevectors parts)
    (let* ([total (fold-left + 0 (map bytevector-length parts))]
           [whole (make-bytevector total)])
      (let place ([parts parts] [at 0])
        (if (null? parts)
            whole
            (begin
              (bytevector-copy! (car parts) 0 whole at
                                (bytevector-length (car parts)))
              (place (cdr parts) (+ at (bytevector-length (car parts)))))))))

  (define (bytevector-slice bv start end)
    (let ([out (make-bytevector (- end start))])
      (bytevector-copy! bv start out 0 (- end start))
      out))

  (define (find-blank-line bv)
    (let ([n (bytevector-length bv)])
      (let scan ([i 0])
        (cond [(> (+ i 4) n) #f]
              [(and (= (bytevector-u8-ref bv i) 13)
                    (= (bytevector-u8-ref bv (+ i 1)) 10)
                    (= (bytevector-u8-ref bv (+ i 2)) 13)
                    (= (bytevector-u8-ref bv (+ i 3)) 10))
               i]
              [else (scan (+ i 1))]))))

  (define (parse-response-head text)
    ;; -> (values status headers), header names lowercased.
    (let ([lines (split-crlf text)])
      (when (null? lines)
        (error 'https "empty response head"))
      (let* ([status-line (car lines)]
             [status (let ([space (let scan ([i 0])
                                    (if (or (= i (string-length status-line))
                                            (char=? (string-ref status-line
                                                                i)
                                                    #\space))
                                        i
                                        (scan (+ i 1))))])
                       (or (and (< (+ space 4)
                                   (string-length status-line))
                                (string->number
                                  (substring status-line (+ space 1)
                                             (+ space 4))))
                           (error 'https "malformed status line"
                                  status-line)))])
        (values status
                (map (lambda (line)
                       (let ([colon (let scan ([i 0])
                                      (if (or (= i (string-length line))
                                              (char=? (string-ref line i)
                                                      #\:))
                                          i
                                          (scan (+ i 1))))])
                         (cons (string-downcase (substring line 0 colon))
                               (trim (substring line (min (+ colon 1)
                                                          (string-length
                                                            line))
                                                (string-length line))))))
                     (cdr lines))))))

  (define (split-crlf text)
    (let loop ([start 0] [acc '()])
      (let ([at (let scan ([i start])
                  (cond [(>= (+ i 1) (string-length text)) #f]
                        [(and (char=? (string-ref text i) #\return)
                              (char=? (string-ref text (+ i 1)) #\newline))
                         i]
                        [else (scan (+ i 1))]))])
        (if at
            (loop (+ at 2) (cons (substring text start at) acc))
            (reverse (if (< start (string-length text))
                         (cons (substring text start (string-length text))
                               acc)
                         acc))))))

  (define (trim s)
    (let* ([n (string-length s)]
           [from (let scan ([i 0])
                   (if (and (< i n) (char=? (string-ref s i) #\space))
                       (scan (+ i 1)) i))]
           [to (let scan ([i n])
                 (if (and (> i from) (char=? (string-ref s (- i 1)) #\space))
                     (scan (- i 1)) i))])
      (substring s from to)))

  (define (body-port channel leftover headers)
    ;; A binary input port over the response body, decoding the
    ;; framing: Content-Length, chunked transfer, or read-to-close.
    (define buffered leftover)   ; bytes read past the framing point
    (define (take! bv start count)
      ;; serve from the buffer, else from the channel
      (if (> (bytevector-length buffered) 0)
          (let ([n (min count (bytevector-length buffered))])
            (bytevector-copy! buffered 0 bv start n)
            (set! buffered
              (bytevector-slice buffered n (bytevector-length buffered)))
            n)
          ((channel-read! channel) bv start count)))
    (define (take-body! bv start count)
      ;; Framed payload/framing bytes cannot end early. Only an unframed
      ;; read-to-close body may use transport EOF as successful completion.
      (let ([got (take! bv start count)])
        (when (and (> count 0) (zero? got)) (error 'https "connection closed mid-body"))
        got))
    (define (take-exactly! bv start count)
      (let loop ([start start] [count count])
        (when (> count 0)
          (let ([got (take-body! bv start count)])
            (loop (+ start got) (- count got))))))
    (define (read-framing-line)
      ;; a CRLF-terminated ASCII line (chunk sizes and trailers)
      (let loop ([acc '()])
        (let ([one (make-bytevector 1)])
          (take-exactly! one 0 1)
          (let ([byte (bytevector-u8-ref one 0)])
            (cond [(= byte 10)
                   (list->string
                     (map integer->char
                          (reverse (if (and (pair? acc) (= (car acc) 13))
                                       (cdr acc)
                                       acc))))]
                  [else (loop (cons byte acc))])))))
    (define reader
      (cond
        [(let ([te (header-ref headers "transfer-encoding")])
           (and te (string=? (string-downcase te) "chunked")))
         ;; chunked: size lines frame the data; a zero closes
         (let ([remaining 0] [done #f])
           (lambda (bv start count)
             (cond
               [done 0]
               [(zero? remaining)
                (let ([size (string->number (strip-chunk-extension
                                              (read-framing-line))
                                            16)])
                  (unless size (error 'https "malformed chunk size"))
                  (if (zero? size)
                      (begin
                        ;; consume trailers up to the blank line
                        (let drain ()
                          (unless (string=? (read-framing-line) "")
                            (drain)))
                        (set! done #t)
                        0)
                      (begin (set! remaining size)
                             (let ([got (take-body! bv start
                                          (min count remaining))])
                               (set! remaining (- remaining got))
                               (when (zero? remaining)
                                 (read-framing-line))   ; chunk's CRLF
                               got))))]
               [else
                (let ([got (take-body! bv start (min count remaining))])
                  (set! remaining (- remaining got))
                  (when (and (zero? remaining) (> got 0))
                    (read-framing-line))
                  got)])))]
        [(let ([length (header-ref headers "content-length")])
           (and length (string->number (trim length))))
         => (lambda (length)
              (let ([remaining length])
                (lambda (bv start count)
                  (if (zero? remaining)
                      0
                      (let ([got (take-body! bv start (min count remaining))])
                        (set! remaining (- remaining got))
                        got)))))]
        [else take!]))   ; read to connection close
    (make-custom-binary-input-port
      "https body" reader #f #f
      (lambda () ((channel-close! channel)))))

  (define (strip-chunk-extension line)
    (let ([semi (let scan ([i 0])
                  (cond [(= i (string-length line)) #f]
                        [(char=? (string-ref line i) #\;) i]
                        [else (scan (+ i 1))]))])
      (if semi (substring line 0 semi) line)))

  (edoc "Which machinery performs requests: native, the FFI TLS connector, or curl, a subprocess."
        (value (one-of native curl)))
  (define https-backend
    ;; Which machinery performs requests: 'native is the FFI TLS
    ;; connector; 'curl delegates whole requests to a curl subprocess.
    ;; Native additionally falls back to curl by itself when no TLS
    ;; library can be found.
    (make-parameter 'native
      (lambda (backend)
        (unless (memq backend '(native curl))
          (error 'https-backend "expected native or curl" backend))
        backend)))

  (define curl-available
    (let ([known 'no])
      (lambda ()
        (when (eq? known 'no)
          (set! known
            (zero? (system "command -v curl >/dev/null 2>&1"))))
        known)))

  (define (open-curl method url headers body-bytes)
    (sys:open-process
      (append (list "curl" "-sS" "--no-buffer" "-i" "--max-time" (number->string (https-timeout))
                    "-X" (symbol->string method))
              (apply append (map (lambda (header)
                                   (list "-H" (format "~a: ~a" (car header) (cdr header)))) headers))
              (if body-bytes '("--data-binary" "@-") '()) (list url))))

  (define (curl-channel process)
    ;; Curl supplies headers and an already de-framed body. EOF also checks
    ;; process completion, so truncated transfers cannot report success.
    (make-channel
      (lambda (bv start count)
        (let ([got (get-bytevector-some! (sys:process-input process) bv start count)])
          (if (eof-object? got)
              (let-values ([(code complaint) (sys:process-result process)])
                (unless (zero? code)
                  (error 'https (format "curl failed (~a): ~a" code (trim complaint))))
                0)
              got)))
      (lambda (bv) (error 'https "the curl channel is read-only"))
      (lambda () (sys:close-process! process))))

  (edoc "Perform an HTTPS request with a method and URL, and optional headers and body; the response with its body port open."
        (method string "GET, POST and so on")
        (url string "the URL")
        (options (list-of any) "headers, then a body")
        (returns (record https-response)))
  (define (https-request method url . options)
    (apply call-with-request method url #f options))

  (define (call-with-request method url consume . options)
    ;; options: an optional header alist, then an optional body
    ;; (string or bytevector). A consumer stays inside the request's owner;
    ;; only the public streaming request transfers its live body to a caller.
    (let-values ([(secure? host port path authority) (parse-url url)])
      (let* ([headers (if (pair? options) (car options) '())]
             [body (and (pair? options) (pair? (cdr options))
                        (cadr options))]
             [body-bytes (cond [(not body) #f]
                               [(string? body) (string->utf8 body)]
                               [else body])]
             [curl? (or (eq? (https-backend) 'curl)
                        (and secure? (eq? (https-connector) tls-connect)
                             (not (tls-available?)) (curl-available)))]
             [process #f] [channel #f] [response #f] [transferred? #f])
        (call-with-scope
          (lambda ()
            (when curl?
              (set! process (open-curl method (string-append (if secure? "https://" "http://") authority path)
                                       headers body-bytes))))
          (lambda ()
            (parameterize ([adopt-channel! (lambda (opened) (set! channel opened))])
              (set! channel (if curl? (curl-channel process)
                              ((if secure? (https-connector) tcp-connect) host port))))
            (if curl? (sys:write-process! process body-bytes)
                (write-request channel method path authority headers body-bytes))
            (let-values ([(head leftover) (read-until-blank-line channel)])
              (let-values ([(status headers) (parse-response-head head)])
                (set! response (make-https-response status headers (body-port channel leftover (if curl? '() headers))))
                (if consume (consume response)
                    (begin (set! transferred? #t) response)))))
          (lambda ()
            (unless transferred?
              (cond [response (https-close! response)]
                    [channel ((channel-close! channel))]
                    [process (sys:close-process! process)])))))))

  (define (write-request channel method path authority headers body-bytes)
    ((channel-write! channel)
     (string->utf8
       (apply string-append
              (format "~a ~a HTTP/1.1\r\n" method path)
              (format "Host: ~a\r\n" authority)
              "Connection: close\r\n"
              (append
                (map (lambda (header)
                       (format "~a: ~a\r\n" (car header) (cdr header)))
                     headers)
                (if body-bytes
                    (list (format "Content-Length: ~a\r\n"
                                  (bytevector-length body-bytes)))
                    '())
                '("\r\n")))))
    (when body-bytes ((channel-write! channel) body-bytes)))

  (define (body-text port)
    (let loop ([parts '()])
      (let ([chunk (get-bytevector-n port 32768)])
        (if (eof-object? chunk)
            (utf8->string (join-bytevectors (reverse parts)))
            (loop (cons chunk parts))))))

  (edoc "A response's whole body as text, closing it."
        (response (record https-response) "the response")
        (returns string))
  (define (https-response-text response)
    (call-with-body response body-text))

  (define (call-with-get url consume)
    ;; One redirect and lifetime rule for every GET consumer. Release a
    ;; response before resolving its Location or opening the next transport.
    (let fetch ([url url] [hops 0])
      (when (> hops 5)
        (error 'https "too many redirects" url))
      (let-values ([(location result)
                    (call-with-request 'GET url
                      (lambda (response)
                        (let* ([status (https-response-status response)]
                               [location (and (memv status '(301 302 303 307 308))
                                              (header-ref (https-response-headers response) "location"))])
                          (values location
                            (cond [location #f]
                                  [(<= 200 status 299) (consume (https-response-port response))]
                                  [else (error 'https (format "~a fetching ~a" status url))])))))])
        (if location (fetch (resolve-url url location) (+ hops 1)) result))))

  (edoc "The body of a URL as text."
        (url string "the URL")
        (returns string))
  (define (https-get url)
    (call-with-get url body-text))

  (edoc "Save the body of a URL to a file."
        (url string "the URL")
        (path file "where to save it"))
  (define (https-download url path)
    (call-with-get url
      (lambda (in)
        (let ([out #f])
          (call-with-scope
            (lambda () (set! out (open-file-output-port path (file-options no-fail))))
            (lambda ()
              (let loop ()
                (let ([chunk (get-bytevector-n in 32768)])
                  (unless (eof-object? chunk) (put-bytevector out chunk) (loop))))
              path)
            (lambda () (close-port out))))))))
