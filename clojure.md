# Clojure

## Clojure 1.10

Installed following instructions at https://clojure.org/guides/getting_started
In Windows, installed lein.bat

### clj command-line tool

Running `clj` in a folder with `deps.edn` will set up the right dependencies and classpaths.

In Emacs' shell, `clojure` will not cause the rlwrap error.

### docs

(doc +)
(find-doc "lazy") ; searches doc text
(apropos "info") ; prints names of matching functions

### multiline comment macro

(comment
  (defn some-function [x y]
    (+ x y)))

### Cider debugging

`C-c C-q` quits.

`C-u C-M-x` calls `cider-debug-defun-at-point`, allowing step-by-step navigation
  (press n to continue, q to quit)

`C-M-x` turns off debugging. It also sends the function to the REPL (`cider-eval-defun-at-point`).

`C-c M-t n` turns on tracing for the current namespace (`cider-toggle-trace-ns`)

`C-c C-z` switches to the REPL

### Debugging

Call `(pst)` in the REPL to get the stack trace (not terribly useful because local variable info is not printed)

In the REPL, *e prints the error and stack trace.

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

## Leiningen

Could not find a way to automatically require libs with the CLI command `clj`.

With `lein`, there is `:repl-options` that can be put in `project.clj`.

*print-length* limits the number of items that will be printed (with println)

Save in /home/heitor/cljrepl/project.clj

```
(defproject com.heitorchang/cljrepl "0.0.1"
  :description "Clojure REPL"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :repl-options {:init-ns clojure-repl
                 :init (do
                         (set! *print-length* 100)
                         (require '[clojure.string :as str]
                                  '[clojure.math.numeric-tower :as math]))})

;; WAIT for REPL to load
```

### Create a new project

Note: dashes and underscores are converted automatically depending on context (directory vs. namespace)

lein new app my_project
Open my_app/project.clj in Emacs and type F2 (cider-jack-in)
Code in my-app/src/my_app/core.clj

### lein not in exec-path

To solve this, type M-x customize-group RET cider. Then look for Cider lein command and set the absolute path of lein.

### Running examples in Programming Clojure, 3rd edition

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

(ns com.example.my-app
  "My app"
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

## Current working directory

`(System/getProperty "user.dir")`

It appears that there is no simple way to change the working directory

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

## REPL Utilities

*1, *2, and *3 represent the latest results
*e is the last exception thrown

## REPL require

(require '[clojure.string :as str])

the vector is quoted because in effect its contents become quoted

'[a b c] is ['a 'b 'c]

## Read entire contents of a file

(slurp "/tmp/myfile.txt")

## Write content to a file

(spit "/tmp/output.txt" "my string")

## SQLite

;; project.clj
;; run cider-jack-in
(defproject com.heitorchang/sqlite "0.1.0"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.xerial/sqlite-jdbc "3.36.0.3"]]
  :repl-options {:init-ns com.heitorchang
                 :init (do
                         (set! *print-length* 100)
                         (require '[clojure.string :as str]
                                  '[clojure.pprint :as p]
                                  '[clojure.java.jdbc :as j]))})

In the REPL:

(def db {:classname "org.sqlite.JDBC"
         :subprotocol "sqlite"
         :subname "/home/heitor/experimental/clojure/sqlite/mydata.db"})

Show table names:
(p/print-table (j/query db "select name from sqlite_master where type = 'table' order by name"))

Query a table:
(p/print-table (j/query db "select * from my_table"))

### nil and booleans

nil is Java's null
Everything other than false and nil is considered true. There is a 'true' value

### numbers

Integer (Long) 42, 0x2a, 052, 36r16
Big integer (bigint) 42N
Ratio (rationalize) 1/3
Big decimal (bigdec) 2.79M
Floating point (double) 2.92e9

Arithmetic functions +' -' *' inc' dec' promote the result to big values

## IntelliJ IDEA with Cursive

In Ubuntu, disable these keyboard shortcuts (Open Settings > Keyboard > Shortcuts)
Navigation:
Hide all normal windows
Move to workspace on the left/right

Set Alt + - to Jump to REPL Editor
Set Alt + 0 to Focus editor
Set Alt + 9 to Tool Windows > Debug

Set Ctrl + 0 to Unsplit all
Set Ctrl + 9 to Split right

Set Ctrl + Alt + Left to Goto Previous Splitter
Set Ctrl + Alt + Right to Goto Next Splitter

Set Ctrl + Alt + [ to Move Caret to code block start
Set Ctrl + Alt + ] to Move Caret to code block end

;; (default) Alt + Shift + P sends top form to REPL
Set Ctrl + Enter to Send top form to REPL
Set Shift + Enter to Start new line

Main Menu > Navigate > Structural Movement
Set Ctrl + P to Move Forward Out of Sexp
Set Ctrl + O to Move Backward Out of Sexp
Set Alt + P to Move Forward Into Sexp
Set Alt + O to Move Backward Into Sexp

Ctrl + Alt + D cuts expression
Ctrl + ( wraps a sexp
Alt + S removes surrounding parens (splice sexp)
Alt + R raises sexp
Alt + Shift + J/K slurps/barfs forwards
Ctrl + Alt + J/K slurps/barfs backwards
(include/exclude items at either end of the sexp)
Ctrl + W repeatedly selects surrounding forms

Ctrl + Alt + I auto indents line

Click on bottom-right corner to toggle Structural Editing Style (Paredit mode does not allow entering parens anywhere)

REPL: Click on the dropdown on the top-right and Edit Configurations...
Select Clojure REPL > Local
Type Shift + F9 to debug
Click on the gear > Move to > Bottom left
Ctrl + Up/Down scrolls history items