;; Logical window operations run on the base. Rendering belongs to head adapters.
(import (only (foundation edoc) elibrary))
(elibrary (service window)
  (export close! create-manager! current document documents find-app init! link! links (rename (windows list)) numbered open-document! resize! restore-manager! return! select! set-display! split! unlink!)
  (import (chezscheme) (prefix (core operation) operation:) (prefix (state model) model:))

  (edoc "Create a persistent window-manager view for this head with one empty window numbered 1. Construction starts no renderer; mount the manager through a composition."
        (owner (or model #f) "lifetime owner, false for a session root") (returns model))
  (define-operation (create-manager! owner)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:create! (actor:current) owner))

  (edoc "Build a candidate manager from this head's saved screen checkpoint; false means no checkpoint. Authored local text becomes private durable documents and retries reuse its origin identity. The input remains until admission succeeds."
    (owner (or model #f) "candidate lifetime") (returns (or model #f)))
  (define-operation (restore-manager! owner)
    (import (prefix (state screen-import) screen-import:) (prefix (state actor) actor:))
    (screen-import:create! (actor:current) owner (actor:checkpoint (actor:current))))

  (edoc "List a manager's window models in topology order. Displayed numbers are local selectors, not identities."
        (manager model "manager view") (returns list))
  (define-operation (windows manager)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:windows (actor:current) manager))

  (edoc "Resolve a manager-local window number, or false. Reusing a number never reuses the retired window's model identity."
        (manager model "manager view") (number integer "positive displayed label") (returns (or model #f)))
  (define-operation (numbered manager number)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:numbered (actor:current) manager number))

  (edoc "Read the focused descendant's window, or the manager's saved selection while focus is elsewhere in the composition."
        (manager model "manager view") (returns model))
  (define-operation (current manager)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:current (actor:current) manager))

  (edoc "Read a window's active document identity, or false when empty. Retirement selects its most recent surviving retained document, never a same-name replacement."
        (manager model "manager") (window model "window") (returns (or buffer model #f)))
  (define-operation (document manager window)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:document (actor:current) manager window))

  (edoc "List retained document identities in this window's most-recently-opened order. Hidden presentations keep their state without being mounted."
        (manager model "manager") (window model "window") (returns list))
  (define-operation (documents manager window)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:documents (actor:current) manager window))

  (edoc "Find a retained app by its app-key. Prefer this window, then other panes in topology order. Return (owning-window app) or false. A result from another pane must be explicitly forked before opening it here."
        (manager model "manager") (window model "destination") (key string "nonempty app key") (returns any))
  (define-operation (find-app manager window key)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:find-app (actor:current) manager window key))

  (edoc "Open a buffer or prepared catalogue app in an explicit window. Buffers retain independent editor or terminal presentations; opening a terminal never starts a process. Apps must already have this window's lifetime and a persistent unmounted subtree; use view:fork! for another presentation. Placement, recency, focus and app origins commit together. Other panes and prompts stay unchanged. Return the presentation view."
        (manager model "manager") (window model "window") (document (or buffer model) "catalogue text or prepared app view") (returns model))
  (define-operation (open-document! manager window document)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:open-document! (actor:current) manager window document))

  (edoc "Return the expected active app to its saved document origin, falling back to the most recent surviving retained document. Return false without changing placement if none survives. Hidden app state survives. Refuse if the app is no longer active; preserve unrelated panes and prompt focus."
        (manager model "manager") (window model "window") (app model "expected active app") (returns (or model #f)))
  (define-operation (return! manager window app)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:return! (actor:current) manager window app))

  (edoc "Select a window atomically with the containing root's focus."
        (manager model "manager view") (window model "window view"))
  (define-operation (select! manager window)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:select! (actor:current) manager window))

  (edoc "Set window display preferences without changing focus or ownership. The alist accepts wrap and line-numbers as booleans or default, and scrollbar as boolean, default, left, right or auto. Wrap applies to retained and future ordinary editors; terminals and app-owned editors keep their own preferences. Splitting copies the policy."
        (manager model "manager") (window model "window") (preferences list "display preference alist"))
  (define-operation (set-display! manager window preferences)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:set-display! (actor:current) manager window preferences))

  (edoc "Split beside an existing window, preserving selection and copying its current presentation and saved app-return chain. Sources and processes remain shared; hidden history outside that chain is not copied. Failed preparation removes the new pane. The new window takes the smallest free number; orientation and proportions have no display units."
        (manager model "manager view") (window model "existing window")
        (direction (one-of left right above below) "new window's side") (returns model))
  (define-operation (split! manager window direction)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:split! (actor:current) manager window direction))

  (edoc "Close a window, collapse its split and remove its links atomically with its owned presentations and resources. Borrowed contents become unowned roots and shared sources survive. Output disposal is recoverable across interruption or restart. The last window returns false."
        (manager model "manager view") (window model "window view") (returns boolean))
  (define-operation (close! manager window)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:close! (actor:current) manager window))

  (edoc "Resize a logical split only while its two children still match the displayed divider. Geometry conversion belongs to the renderer."
        (manager model "manager view") (split model "split view")
        (expected list "two child references, in order") (weights list "two positive rational weights"))
  (define-operation (resize! manager split expected weights)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:resize! (actor:current) manager split expected weights))

  (edoc "Read directed (from to tag) window links in creation order."
        (manager model "manager view") (returns list))
  (define-operation (links manager)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:links (actor:current) manager))

  (edoc "Add a directed link between two windows. Adding an existing link has no effect."
        (manager model "manager view") (from model "source window") (to model "target window") (tag symbol "semantic tag"))
  (define-operation (link! manager from to tag)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:link! (actor:current) manager from to tag))

  (edoc "Remove one directed window link."
        (manager model "manager view") (from model "source window") (to model "target window") (tag symbol "semantic tag"))
  (define-operation (unlink! manager from to tag)
    (import (prefix (state manager) manager:) (prefix (state actor) actor:))
    (manager:unlink! (actor:current) manager from to tag))

  (edoc "Register logical window operations through the module lifecycle.")
  (define (init!)
    (operation:register! 'window:create-manager! create-manager! 'control)
    (operation:register! 'window:restore-manager! restore-manager! 'control)
    (operation:register! 'window:list windows 'read)
    (operation:register! 'window:numbered numbered 'read)
    (operation:register! 'window:current current 'read)
    (operation:register! 'window:document document 'read)
    (operation:register! 'window:documents documents 'read)
    (operation:register! 'window:find-app find-app 'read)
    (operation:register! 'window:open-document! open-document! 'control)
    (operation:register! 'window:return! return! 'control)
    (operation:register! 'window:select! select! 'control)
    (operation:register! 'window:set-display! set-display! 'control)
    (operation:register! 'window:split! split! 'control)
    (operation:register! 'window:close! close! 'control)
    (operation:register! 'window:resize! resize! 'control)
    (operation:register! 'window:links links 'read)
    (operation:register! 'window:link! link! 'control)
    (operation:register! 'window:unlink! unlink! 'control))
)
