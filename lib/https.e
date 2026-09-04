;; https.e -- an HTTP(S) client for the e editor, TLS via the system's
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

(library (https)
  (export (rename (https-get get)) (rename (https-download download)) (rename (https-request request))
          (rename (https-response-status response-status)) (rename (https-response-headers response-headers)) (rename (https-response-port response-port))
          (rename (https-response-text response-text)) (rename (https-close! close!))
          (rename (https-connector connector)) (rename (https-timeout timeout)) (rename (https-backend backend))
          make-channel channel-read! channel-write! channel-close!
          tcp-connect tls-connect)
  (import (chezscheme) (prefix (string) string:))

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

  (define (foreign-zeroed size)
    (let ([pointer (foreign-alloc size)])
      (do ([i 0 (+ i 1)]) ((= i size) pointer)
        (foreign-set! 'unsigned-8 pointer i 0))))

  (define (foreign-string text)
    ;; A NUL-terminated UTF-8 copy the collector will never move.
    (let* ([bytes (string->utf8 text)]
           [n (bytevector-length bytes)]
           [pointer (foreign-alloc (+ n 1))])
      (do ([i 0 (+ i 1)]) ((= i n))
        (foreign-set! 'unsigned-8 pointer i (bytevector-u8-ref bytes i)))
      (foreign-set! 'unsigned-8 pointer n 0)
      pointer))

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
    (let ([time (foreign-zeroed 16)])
      (foreign-set! 'long time 0 seconds)
      (c-setsockopt fd sol-socket so-rcvtimeo time 16)
      (c-setsockopt fd sol-socket so-sndtimeo time 16)
      (foreign-free time))
    ;; and a write to a peer-closed connection must error, not raise
    ;; SIGPIPE: SO_NOSIGPIPE where it exists (macOS #x1022, FreeBSD
    ;; #x800); Linux writes report EPIPE to blocked signals anyway
    (let ([option (case os [(macos) #x1022] [(freebsd) #x800] [else #f])])
      (when option
        (let ([on (foreign-zeroed 4)])
          (foreign-set! 'int on 0 1)
          (c-setsockopt fd sol-socket option on 4)
          (foreign-free on)))))

  (define (connect-socket host port)
    ;; -> a connected stream socket fd, trying each resolved address.
    (let ([host* (foreign-string host)]
          [port* (foreign-string (number->string port))]
          [hints (foreign-zeroed 48)]
          [result* (foreign-zeroed 8)])
      (foreign-set! 'int hints 8 1)   ; ai_socktype = SOCK_STREAM
      (let ([status (c-getaddrinfo host* port* hints result*)])
        (foreign-free host*)
        (foreign-free port*)
        (foreign-free hints)
        (unless (zero? status)
          (foreign-free result*)
          (error 'https
                 (format "cannot resolve ~a: ~a"
                         host (foreign-cstring (c-gai-strerror status)))))
        (let* ([first (foreign-ref 'void* result* 0)]
               [fd (let try ([info first])
                     (if (not info)
                         #f
                         (let ([fd (c-socket (addrinfo-family info) 1 0)])
                           (if (< fd 0)
                               (try (addrinfo-next info))
                               (if (zero? (c-connect
                                            fd (addrinfo-addr info)
                                            (addrinfo-addrlen info)))
                                   fd
                                   (begin (c-close fd)
                                          (try (addrinfo-next info))))))))])
          (c-freeaddrinfo first)
          (foreign-free result*)
          (unless fd
            (error 'https (format "cannot connect to ~a:~a" host port)))
          (set-socket-timeouts! fd (https-timeout))
          fd))))

  ;;; Channels ---------------------------------------------------------------

  ;; A channel is the transport abstraction: read! fills a bytevector
  ;; range and returns the count (0 at orderly close), write! sends a
  ;; whole bytevector, close! releases the transport.
  (define-record-type channel
    (fields read! write! close!))

  (define transfer-buffer-size 32768)

  (define (channel-over-fd fd read-raw write-raw cleanup!)
    ;; Bytes cross the FFI through foreign buffers: the reader may be
    ;; parked in a blocking call while the collector runs.
    (let ([in-buffer (foreign-alloc transfer-buffer-size)]
          [out-buffer (foreign-alloc transfer-buffer-size)]
          [open #t])
      (make-channel
        (lambda (bv start count)
          (let* ([limit (min count transfer-buffer-size)]
                 [got (read-raw in-buffer limit)])
            (copy-from-foreign! in-buffer bv start got)
            got))
        (lambda (bv)
          (let send ([start 0])
            (let ([left (- (bytevector-length bv) start)])
              (when (> left 0)
                (let ([count (min left transfer-buffer-size)])
                  (copy-to-foreign! bv start count out-buffer)
                  (let ([sent (write-raw out-buffer count)])
                    (unless (> sent 0)
                      (error 'https "connection closed while writing"))
                    (send (+ start sent))))))))
        (lambda ()
          (when open
            (set! open #f)
            (cleanup!)
            (c-close fd)
            (foreign-free in-buffer)
            (foreign-free out-buffer))))))

  (define (tcp-connect host port)
    ;; A plain byte stream -- http://, and the substrate under TLS.
    (let ([fd (connect-socket host port)])
      (channel-over-fd
        fd
        (lambda (buffer limit)
          (let ([got (c-read fd buffer limit)])
            (if (< got 0)
                (error 'https "read failed (timeout or reset)")
                got)))
        (lambda (buffer count) (c-write fd buffer count))
        void)))

  ;;; TLS (libssl) -----------------------------------------------------------

  (define-foreign ssl-client-method tls-loaded "TLS_client_method" () uptr)
  (define-foreign ssl-ctx-new tls-loaded "SSL_CTX_new" (uptr) uptr)
  (define-foreign ssl-ctx-free tls-loaded "SSL_CTX_free" (uptr) void)
  (define-foreign ssl-ctx-set-default-verify-paths tls-loaded
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
  (define-foreign ssl-shutdown tls-loaded "SSL_shutdown" (uptr) int)
  (define-foreign-blocking ssl-connect-call tls-loaded "SSL_connect"
    (uptr) int)
  (define-foreign-blocking ssl-read tls-loaded "SSL_read" (uptr uptr int) int)
  (define-foreign-blocking ssl-write tls-loaded "SSL_write"
    (uptr uptr int) int)

  (define ssl-verify-peer 1)
  (define ssl-ctrl-set-tlsext-hostname 55)
  (define ssl-error-zero-return 6)

  (define (tls-connect host port)
    ;; The default secure connector: verified TLS to host, as a channel.
    (let* ([fd (connect-socket host port)]
           [ctx (ssl-ctx-new (ssl-client-method))])
      (when (zero? ctx)
        (c-close fd)
        (error 'https "SSL_CTX_new failed"))
      (ssl-ctx-set-default-verify-paths ctx)
      (ssl-ctx-set-verify ctx ssl-verify-peer 0)
      (let ([ssl (ssl-new ctx)])
        (when (zero? ssl)
          (ssl-ctx-free ctx)
          (c-close fd)
          (error 'https "SSL_new failed"))
        (let ([name (foreign-string host)])
          (ssl-ctrl ssl ssl-ctrl-set-tlsext-hostname 0 name)   ; SNI
          (foreign-free name))
        (unless (= 1 (ssl-set1-host ssl host))
          (error 'https "SSL_set1_host failed"))
        (ssl-set-fd ssl fd)
        (unless (= 1 (ssl-connect-call ssl))
          (let ([verify (ssl-get-verify-result ssl)]
                [finish (lambda () (ssl-free ssl) (ssl-ctx-free ctx)
                          (c-close fd))])
            (if (zero? verify)
                (begin (finish)
                       (error 'https
                              (format "TLS handshake with ~a failed" host)))
                (begin (finish)
                       (error 'https
                              (format "certificate for ~a rejected: ~a"
                                      host
                                      (foreign-cstring
                                        (x509-verify-error-string
                                          verify))))))))
        (channel-over-fd
          fd
          (lambda (buffer limit)
            (let ([got (ssl-read ssl buffer limit)])
              (if (> got 0)
                  got
                  (let ([status (ssl-get-error ssl got)])
                    (if (= status ssl-error-zero-return)
                        0
                        (error 'https
                               (format "TLS read failed (status ~a)"
                                       status)))))))
          (lambda (buffer count) (ssl-write ssl buffer count))
          (lambda ()
            (guard (ex [else (void)]) (ssl-shutdown ssl))
            (ssl-free ssl)
            (ssl-ctx-free ctx))))))

  (define (tls-available?)
    (guard (ex [else #f]) (tls-loaded) #t))

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

  (define (parse-url url)
    ;; -> (values secure? host port path)
    (define (split-scheme)
      (cond [(string:prefix? "https://" url) (values #t 8 443)]
            [(string:prefix? "http://" url) (values #f 7 80)]
            [else (error 'https "expected an http(s) URL" url)]))
    (let-values ([(secure? start default-port) (split-scheme)])
      (let* ([slash (let scan ([i start])
                      (cond [(= i (string-length url)) i]
                            [(char=? (string-ref url i) #\/) i]
                            [else (scan (+ i 1))]))]
             [authority (substring url start slash)]
             [path (if (= slash (string-length url))
                       "/"
                       (substring url slash (string-length url)))]
             [colon (let scan ([i 0])
                      (cond [(= i (string-length authority)) #f]
                            [(char=? (string-ref authority i) #\:) i]
                            [else (scan (+ i 1))]))])
        (values secure?
                (if colon (substring authority 0 colon) authority)
                (if colon
                    (string->number
                      (substring authority (+ colon 1)
                                 (string-length authority)))
                    default-port)
                path))))

  (define-record-type https-response
    (fields status headers port channel)
    (protocol (lambda (new) (lambda (s h p c) (new s h p c)))))

  (define (https-close! response)
    ((channel-close! (https-response-channel response))))

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
    (define (take-exactly! bv start count)
      (let loop ([start start] [count count])
        (when (> count 0)
          (let ([got (take! bv start count)])
            (when (zero? got)
              (error 'https "connection closed mid-body"))
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
                             (let ([got (take! bv start
                                               (min count remaining))])
                               (set! remaining (- remaining got))
                               (when (zero? remaining)
                                 (read-framing-line))   ; chunk's CRLF
                               got))))]
               [else
                (let ([got (take! bv start (min count remaining))])
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
                      (let ([got (take! bv start (min count remaining))])
                        (when (and (zero? got) (> remaining 0))
                          (error 'https "connection closed mid-body"))
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

  (define (shell-quoted text)
    (string-append
      "'"
      (apply string-append
             (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                  (string->list text)))
      "'"))

  (define (curl-request method url headers body-bytes)
    ;; The whole request through a curl subprocess: -i puts the status
    ;; line and headers on stdout ahead of the body, which curl has
    ;; already de-framed -- so the body reads to process end,
    ;; whatever the transfer encoding was.
    (let-values ([(to from errors pid)
                  (open-process-ports
                    (apply string-append
                           "exec curl -sS -i --max-time "
                           (number->string (https-timeout))
                           " -X " (symbol->string method)
                           (append
                             (map (lambda (header)
                                    (string-append
                                      " -H "
                                      (shell-quoted
                                        (format "~a: ~a" (car header)
                                                (cdr header)))))
                                  headers)
                             (if body-bytes
                                 '(" --data-binary @-")
                                 '())
                             (list " " (shell-quoted url))))
                    'block)])
      (when body-bytes (put-bytevector to body-bytes))
      (close-port to)
      (let ([channel
             (make-channel
               (lambda (bv start count)
                 (let ([got (get-bytevector-n! from bv start count)])
                   (if (eof-object? got) 0 got)))
               (lambda (bv) (error 'https "the curl channel is read-only"))
               (lambda ()
                 (close-port from)
                 (close-port errors)))])
        (guard (ex [else
                    (let ([complaint
                           (guard (e2 [else ""])
                             (let ([bytes (get-bytevector-all errors)])
                               (if (eof-object? bytes)
                                   ""
                                   (utf8->string bytes))))])
                      ((channel-close! channel))
                      (if (string=? complaint "")
                          (raise ex)
                          (error 'https
                                 (format "curl: ~a" (trim complaint)))))])
          (let-values ([(head leftover) (read-until-blank-line channel)])
            (let-values ([(status headers) (parse-response-head head)])
              (make-https-response
                status headers
                ;; no framing headers: curl already decoded the body
                (body-port channel leftover '())
                channel)))))))

  (define (https-request method url . options)
    ;; options: an optional header alist, then an optional body
    ;; (string or bytevector).  -> an https-response whose port streams
    ;; the body; close it with https:close! (draining closes too).
    (let-values ([(secure? host port path) (parse-url url)])
      (let* ([headers (if (pair? options) (car options) '())]
             [body (and (pair? options) (pair? (cdr options))
                        (cadr options))]
             [body-bytes (cond [(not body) #f]
                               [(string? body) (string->utf8 body)]
                               [else body])])
        (if (or (eq? (https-backend) 'curl)
                (and secure? (not (tls-available?)) (curl-available)))
            (curl-request method url headers body-bytes)
            (native-request method secure? host port path
                            headers body-bytes)))))

  (define (native-request method secure? host port path headers body-bytes)
    (let ([channel (if secure?
                       ((https-connector) host port)
                       (tcp-connect host port))])
      (guard (ex [else ((channel-close! channel)) (raise ex)])
        ((channel-write! channel)
         (string->utf8
           (apply string-append
                  (format "~a ~a HTTP/1.1\r\n" method path)
                  (format "Host: ~a\r\n" host)
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
        (when body-bytes ((channel-write! channel) body-bytes))
        (let-values ([(head leftover) (read-until-blank-line channel)])
          (let-values ([(status headers) (parse-response-head head)])
            (make-https-response
              status headers
              (body-port channel leftover headers)
              channel))))))

  (define (https-response-text response)
    ;; Drain the body as UTF-8 and close the connection.
    (let ([port (https-response-port response)])
      (let loop ([parts '()])
        (let ([chunk (get-bytevector-n port 32768)])
          (if (eof-object? chunk)
              (begin (close-port port)
                     (utf8->string (join-bytevectors (reverse parts))))
              (loop (cons chunk parts)))))))

  (define (https-get url)
    ;; The body text of a 2xx response, following up to five redirects.
    (let fetch ([url url] [hops 0])
      (when (> hops 5)
        (error 'https "too many redirects" url))
      (let* ([response (https-request 'GET url)]
             [status (https-response-status response)])
        (cond
          [(and (memv status '(301 302 303 307 308))
                (header-ref (https-response-headers response) "location"))
           => (lambda (location)
                (https-close! response)
                (fetch (if (string:prefix? "http" location)
                           location
                           (let-values ([(secure? host port path)
                                         (parse-url url)])
                             (format "~a://~a:~a~a"
                                     (if secure? "https" "http")
                                     host port location)))
                       (+ hops 1)))]
          [(<= 200 status 299) (https-response-text response)]
          [else
           (https-close! response)
           (error 'https (format "~a fetching ~a" status url))]))))

  (define (https-download url path)
    ;; Fetch url into a file, following redirects like https:get.
    (let fetch ([url url] [hops 0])
      (when (> hops 5)
        (error 'https "too many redirects" url))
      (let* ([response (https-request 'GET url)]
             [status (https-response-status response)])
        (cond
          [(and (memv status '(301 302 303 307 308))
                (header-ref (https-response-headers response) "location"))
           => (lambda (location)
                (https-close! response)
                (fetch location (+ hops 1)))]
          [(<= 200 status 299)
           (let ([in (https-response-port response)]
                 [out (open-file-output-port
                        path (file-options no-fail))])
             (let loop ()
               (let ([chunk (get-bytevector-n in 32768)])
                 (unless (eof-object? chunk)
                   (put-bytevector out chunk)
                   (loop))))
             (close-port out)
             (close-port in)
             path)]
          [else
           (https-close! response)
           (error 'https (format "~a fetching ~a" status url))])))))
