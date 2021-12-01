(defproject com.heitorchang/cljrepl "0.0.1"
  :description "clj-repl"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :repl-options {:init-ns clj-repl
                 :init (do
                         (set! *print-length* 100)
                         (require '[clojure.string :as str]
                                  '[clojure.math.numeric-tower :as math]))})

;; WAIT for REPL to load
