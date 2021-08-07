# Common Lisp

https://lispcookbook.github.io/cl-cookbook/debugging.html

## Packages

See Practical Common Lisp, ch. 21

```
(defpackage "MY-PACKAGE"
  (:use "COMMON-LISP"))

(in-package "MY-PACKAGE")
```

## SLIME Operations

Modeled after Common Lisp, An Interactive Approach, Section B.2

Version of Common Lisp: Clozure Common Lisp 1.12
.emacs variable: (setq inferior-lisp-program "/home/heitor/installers/ccl/lx86cl64 -l /home/heitor/lisp/heitor.lisp")
How to run Lisp: M-x slime
How to exit Lisp: (quit)
Interrupt: C-c C-c
Exit the debugger completely: q

## Clozure Common Lisp (CCL)

`declaim` speed or debug in a startup file does not seem to have any effect. Must evaluate it in the REPL.

C-f2 is set up to insert the following form:

(declaim (optimize (speed 0) (safety 0) (space 0) (debug 3)))

### Tail call optimization

Use declare. Without it, stack will overflow at around 65536.
```
(defun self-inc (n)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (format t "~a~%" n)
  (if (< n 1000000)
      (self-inc (1+ n))))
```

## SBCL

### Tail call optimization

To enable, evaluate:

`(declaim (optimize (speed 3) (space 0) (debug 0)))`

Functions need to be redefined after this change.

### Debugger

To enable a more complete backtrace, evaluate in the beginning:

`(declaim (optimize (speed 0) (space 0) (debug 3)))`

Add the above line to ~/.sbclrc, optionally add `(format t "Debug set to 3")`

If declaiming in the REPL, functions need to be redefined after this change.

`ba` print backtrace

`top` returns to top level

#### Restarting from a specific frame (SLIME debugger, sldb)

If the error was caused by an invalid user input, place the cursor above the right frame and press `r`.

## CGI

(from nearlyfreespeech forums)

```
#!/usr/local/bin/sbcl

(format t "Content-type: text/html%~%~")
(format t "<h1>Hello</h1>")
```

Save as filename.cgi and chmod 755 filename.cgi

## Hunchentoot

First, install Quicklisp

`curl -O https://beta.quicklisp.org/quicklisp.lisp`

`sbcl --load quicklisp.lisp`

`* (quicklisp-quickstart:install)`

Add to `~/.sbclrc`:

```
(load "/home/heitor/quicklisp/setup.lisp")
```

In the REPL, type:

`(ql:quickload 'hunchentoot)`

### To start a server

Create an acceptor:

`(defparameter *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))`

Start serving:

`(hunchentoot:start *acceptor*)`

Stop serving:
`(hunchentoot:stop *acceptor*)`

## Literal data should not be modified

Applying destructive actions such as `setf` and `incf` to literal data `'(1 2 3)` is undefined.

Use `(list 1 2 3)` or `copy-list` to avoid this problem (or avoid destructive actions altogether).
