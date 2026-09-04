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

The HTTP client is written against a *channel* -- a record of read, write,
and close procedures (`channel-read!`, `channel-write!`, `channel-close!`)
over an established byte stream
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

## The curl backend

`https-backend` selects the machinery: `'native` (the default) is the
FFI TLS connector; `'curl` hands whole requests to a curl subprocess
presenting the same response interface -- curl then does the TLS and
certificate verification. Set `(https-backend 'curl)` in config.e to
prefer it. The native backend also falls back to curl on its own when
no TLS library can be found, so a system without libssl but with curl
still works out of the box. Under curl the body always streams to
process end: curl has already decoded the transfer framing.

## Portability

The platform comes from Chez's `machine-type`: the `struct addrinfo`
field order and the socket-option numbers differ between glibc and
the BSDs (macOS included), and both layouts are supported. The libssl
soname is probed across current and previous OpenSSL majors on Linux
and the BSD base systems (`.so.3`, `.so.1.1`, `.so.30`, `.so.111`,
plain). On macOS the unversioned `/usr/lib` stubs abort any process
that loads them, so only the versioned Homebrew/MacPorts dylibs are
probed -- `brew install openssl@3` provides one. Certificate
verification uses the library's default root store; on a FreeBSD
without one populated (`certctl rehash`, or the `ca_root_nss` port),
connections fail verification loudly.
