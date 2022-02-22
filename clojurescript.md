# ClojureScript

## Getting started

Create the directory structure

```
+-- hello-world/
    +-- deps.edn
    +-- src/
        +-- hello_world/
            +-- core.cljs
```

;; deps.edn
{:deps {org.clojure/clojurescript {:mvn/version "1.10.758"}}}


;; core.cljs
(ns hello-world.core)

(println "Hello, world!")


## Starting the REPL

Open deps.edn in Emacs and call M-x cider-jack-in-cljs (C-c M-J, with a capital J)

Type "browser" as the REPL type

Functions defined in core.cljs such as

(defn f [x] (js/console.log (+ x 1)))

may be called with (hello-world.core/f 20)