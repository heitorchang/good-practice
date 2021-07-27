# Common Lisp

https://lispcookbook.github.io/cl-cookbook/debugging.html

## SLIME Operations

Modeled after Common Lisp, An Interactive Approach, Section B.2

Version of Common Lisp: Clozure Common Lisp 1.12
.emacs variable: (setq inferior-lisp-program "/home/heitor/installers/ccl/lx86cl64 -l /home/heitor/lisp/heitor.lisp")
How to run Lisp: M-x slime
How to exit Lisp: (quit)
Interrupt: C-c C-c
Exit the debugger completely: q

## SBCL

### Debugger

To enable a more complete backtrace, evaluate in the beginning:

`(declaim (optimize (speed 0) (space 0) (debug 3)))`

Add the above line to ~/.sbclrc, optionally add `(format t "Debug set to 3")`

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