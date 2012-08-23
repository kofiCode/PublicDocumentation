## Referencing other source files

Use syntax like:

```clojure
(ns com.oracle.git.cx_ws.core
  (:use [com.oracle.git.cx_ws.utils :only (apply-template read-template)])
  (:use [com.oracle.git.cx_ws.connection :only (service-call)]))
```

as opposed to:

```clojure
(:require ...) ;; requires prefixing functions, which is okay, but 
;; :use allows no prefix.
;; OR
(:use [lib])  ;; no :only clause which is very helpful to know where
;; function come from
```

## Parens

Dont put parens on their own line, indentation should indicate
nesting, not parens on their own lines.

DO:

```clojure
(defn abc [parameter] 
  "docs"
  (if parameter 
    (true)
    (false)))
```

DONT:

```clojure
(defn abc [parameter] 
  "docs"
  (if parameter 
    (true)
    (false)
  )
)
```
