(import (only (foundation edoc) elibrary))
(elibrary (service environment)
  (export cancel! close! completion create! evaluate! release! reset!)
  (import (chezscheme) (prefix (core client) client:))
  (define (request actor operation . args)
    (unless (equal? actor (client:identity)) (error 'environment "actor differs from connection identity"))
    (apply client:request operation args))

  (edoc "Create a lazy base Scheme environment. Recipe fields are imports, absolute roots/directory, optional copied values and named borrowed resources. Persistence retains recipes and portable outcomes, never native bindings or replay."
    (actor actor "connection attribution") (recipe list "portable recipe")
    (persistence (one-of transient persistent) "recovery policy") (returns model))
  (define (create! actor recipe persistence) (request actor 'environment-create recipe persistence))

  (edoc "Queue Scheme source at an explicit namespace generation and return its job model immediately. Accepted jobs survive head detach; inspect model:snapshots for state and output references."
    (actor actor "connection attribution") (id model "environment") (generation integer "expected generation")
    (source string "Scheme forms") (returns model))
  (define (evaluate! actor id generation source) (request actor 'environment-evaluate id generation source))

  (edoc "Cancel a queued job without resetting definitions. Running cancellation resets the namespace and reaps its worker; committed effects remain. Return queued, reset or finished."
    (actor actor "connection attribution") (id model "job") (returns symbol))
  (define (cancel! actor id) (request actor 'environment-cancel id))

  (edoc "Reset an environment at the expected generation, expiring live handles and pending jobs. Its recipe is lazily initialized again; borrowed resources and completed portable results survive."
    (actor actor "connection attribution") (id model "environment") (generation integer "expected generation"))
  (define (reset! actor id generation) (request actor 'environment-reset id generation))

  (edoc "Release a completed job, its owned output and retained result. Cancel pending jobs first."
    (actor actor "connection attribution") (id model "job") (returns boolean))
  (define (release! actor id) (request actor 'environment-release id))

  (edoc "Read up to 256 names from the last completed catalogue: (generation catalogue count names), or false for a changed basis. Fetch pages off the input/paint path and filter cached names locally."
    (id model "environment") (generation integer "namespace generation") (catalogue integer "catalogue revision")
    (offset integer "first name") (count integer "1 through 256") (returns (or list #f)) (effects remote))
  (define (completion id generation catalogue offset count)
    (client:request 'environment-completion id generation catalogue offset count))

  (edoc "Close an environment, reap its worker and release owned jobs/output. Borrowed documents and models survive."
    (actor actor "connection attribution") (id model "environment") (generation integer "expected generation"))
  (define (close! actor id generation) (request actor 'environment-close id generation)))
