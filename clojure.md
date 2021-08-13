# Clojure

## Clojure 1.10

Installed following instructions at https://clojure.org/guides/getting_started

### clj command-line tool

Running `clj` in a folder with `deps.edn` will set up the right dependencies and classpaths.

### Debugging

Call `(pst)` in the REPL to get the stack trace

## Alternative method

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