# HTTPS

The `(https)` module is an HTTP(S) client with TLS provided by the
system's libssl through Chez's FFI -- no external processes. Nothing
loads until the first request, so the editor starts fine without
libssl installed; the describe corpus download rides on it.

## Scheme API

```scheme
(https-get url)                    ; => body text; follows redirects,
                                   ;    errors on non-2xx
(https-download url path)          ; fetch into a file
(https-request method url [headers [body]])
                                   ; => response; body streams
(https-response-status r)          ; => 200 ...
(https-response-headers r)         ; => (("content-type" . "...") ...)
(https-response-port r)            ; => binary input port over the body
(https-response-text r)            ; drain as UTF-8 and close
(https-close! r)                   ; abandon a response early
(https-timeout [seconds])          ; stalled-peer cutoff (default 60)
```

Header names arrive lowercased. The response port decodes the
transfer framing -- `Content-Length`, chunked, or read-to-close -- so
callers only ever see body bytes; closing the port closes the
connection. `http://` URLs work too, over a plain socket.

## The transport abstraction

The HTTP client is written against a *channel* -- a record of
`read!`/`write!`/`close!` procedures over an established byte stream
-- and obtains secure channels from the `https-connector` parameter:

```scheme
(https-connector)                  ; (lambda (host port) channel)
```

The default connector speaks TLS 1.x via libssl with certificate
verification against the system root store, hostname checking
(`SSL_set1_host`), and SNI; a failed verification fails the connect
loudly. Swapping the parameter swaps the whole transport without
touching the HTTP layer: a pure-Scheme TLS, an `openssl s_client`
pipe, or a test double are all just procedures returning a
`make-channel`.

## Threading

Blocking foreign calls (DNS, connect, TLS reads and writes) are
declared `__collect_safe`, so a stalled peer parks only its own
thread -- the collector, and with it the rest of the editor, keeps
running. Bytes cross the FFI through foreign buffers for the same
reason. Requests still block the calling thread; anything
interactive should call from a worker.

## Portability

The libssl soname is probed (`libssl.so.3`, `.1.1`, plain); the
`struct addrinfo` field layout currently assumes Linux/glibc.
