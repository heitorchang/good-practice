# Clojure

## Clojure 1.10

Installed following instructions at https://clojure.org/guides/getting_started

### clj command-line tool

Running `clj` in a folder with `deps.edn` will set up the right dependencies and classpaths.

In Emacs' shell, `clojure` will not cause the rlwrap error.

### Cider debugging

`C-c C-q` quits.

`C-u C-M-x` calls `cider-debug-defun-at-point`, allowing step-by-step navigation
  (press n to continue, q to quit)

`C-M-x` turns off debugging. It also sends the function to the REPL (`cider-eval-defun-at-point`).

`C-c M-t n` turns on tracing for the current namespace (`cider-toggle-trace-ns`)

`C-c C-z` switches to the REPL

### Debugging

Call `(pst)` in the REPL to get the stack trace (not terribly useful because local variable info is not printed)

### Tracing

Install https://github.com/clojure/tools.trace

Add to deps.edn:

```
{:deps
 {org.clojure/tools.trace {:mvn/version "0.7.11"}}
 :paths ["."]}
```

Then call `(trace-ns *ns*)` (most useful)

## Emacs keys

C-M-k kill-sexp
M-left and M-right backward, forward-sexp

## Running examples in Programming Clojure, 3rd edition

Visit (C-x C-f) `deps.edn` in the top-level directory, to set Emacs' working directory to that directory.

Call `M-x cider-jack-in` 

```
(require 'examples.introduction)
(examples.introduction/hello "Cosmos")
```

`(require '[examples.introduction :as intro])`
`(require '(clojure zip [set as :s]))` loads clojure.zip and clojure.set as s

To bring in all vars from another lib, call (not recommended)
`(require '[examples.exploring :refer :all])`

## ns macro

According to `(doc ns)`, use of ns is preferred to individual calls to in-ns/require/use/import

## Alternative installation method

Install Java and Maven, and build the release files manually

* Download a Clojure release zip

Run `mvn -Plocal -Dmaven.test.skip=true package`

Copy the outputted `clojure.jar` to somewhere convenient

Run the REPL with `java -jar clojure.jar`

## Emacs

Install `clojure-mode` and `cider`

In a shell, run `lein.bat new PROJECT_NAME`

Open `project.clj`, then `M-x cider-j` (cider-jack-in)

Add to `.emacs`

```
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (auto-complete-mode 1)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (define-key clojure-mode-map (kbd "<M-left>") 'backward-sexp)
            (define-key clojure-mode-map (kbd "<M-right>") 'forward-sexp)
            (define-key clojure-mode-map (kbd "<C-return>") 'my-cider-save-and-compile-buffer)
            (local-set-key (kbd "<S-return>") 'cider-eval-last-sexp)))
```