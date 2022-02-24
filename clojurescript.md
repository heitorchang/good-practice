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
(ns hello-world.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(println "Hello, world!")


## Starting the REPL

Open deps.edn in Emacs and call M-x cider-jack-in-cljs (C-c M-J, with a capital J)

Type "browser" as the REPL type

Functions defined in core.cljs (the definition must be evaluated) such as:

(defn f [x] (js/console.log (+ x 1)))

may be called on the Clojure side with (hello-world.core/f 20)

In the browser, hello_world.core.f(30) calls the function.

Export functions with ^:export

(defn ^:export g [x] ...)

In the directory with deps.edn, compile a main.js file with:

clj -M -m cljs.main --optimizations advanced -c my_project.core

main.js will be in the out/ directory
