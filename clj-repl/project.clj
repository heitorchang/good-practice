(defproject com.heitorchang/cljrepl "0.0.1"
  :description "clj-repl"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :repl-options {:init-ns clj-repl
                 :init (do
                         (set! *print-length* 100)
                         (require '[clojure.string :as str]
                                  '[clojure.set :as cset]   ;; https://clojure.org/guides/repl/navigating_namespaces
                                  '[clojure.math.combinatorics :as combo]
                                  '[clojure.math.numeric-tower :as math]))})

;; WAIT for REPL to load
