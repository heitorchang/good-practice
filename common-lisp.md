# Common Lisp

https://lispcookbook.github.io/cl-cookbook/debugging.html

## Packages

See Practical Common Lisp, ch. 21

```
(defpackage :my-package
  (:use :cl)) ; same as :common-lisp

(in-package :my-package)
```

## SLIME Operations

Modeled after Common Lisp, An Interactive Approach, Section B.2

Version of Common Lisp: Clozure Common Lisp 1.12
.emacs variable: (setq inferior-lisp-program "/home/heitor/installers/ccl/lx86cl64 -l /home/heitor/lisp/heitor.lisp")
How to run Lisp: M-x slime
How to exit Lisp: (quit)
Interrupt: C-c C-c
Exit the debugger completely: q

## CLISP

In the default REPL, type :q to exit the debugger.

## Clozure Common Lisp (CCL)

`declaim` speed or debug in a startup file does not seem to have any effect. Must evaluate it in the REPL.

C-f2 is set up to insert the following form:

(declaim (optimize (debug 3) (safety 3) (space 0) (speed 0)))

(debug 3) may be written simply as debug

### Tail call optimization

Use declare. Without it, stack will overflow at around 65536.
```
(defun self-inc (n)
  (declare (optimize (debug 0) (safety 0) (space 0) (speed 3)))
  (format t "~a~%" n)
  (if (< n 1000000)
      (self-inc (1+ n))))
```

## SBCL

### Tail call optimization

To enable, evaluate:

`(declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))`

Functions need to be redefined after this change.

### Debugger

To enable a more complete backtrace, evaluate in the beginning:

`(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))`

Add the above line to ~/.sbclrc, optionally add `(format t "Debug set to 3")`

If declaiming in the REPL, functions need to be redefined after this change.

`ba` print backtrace

`top` returns to top level

#### Restarting from a specific frame (SLIME debugger, sldb)

If the error was caused by an invalid user input, place the cursor above the right frame and press `r`.

#### Entering newlines in previous evaluations

Type C-j to insert a newline when editing a previously evaluated expression

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

## Association Lists (alists)

Add:

(setf *alist* (acons 'a 1 *alist*))

Update:

(setf (cdr (assoc 'a *alist*)) 10)

## Parsing JSON in a file

(ql:quickload :st-json)
(use-package :st-json)

Assuming the file was created by JSON.stringify in a browser:

"{\"name\":\"Jack\",\"age\":8,\"pets\":{\"Skip\":5,\"Lucy\":3}}"

(defparameter *json-data* nil)

(with-open-file (in "/home/heitor/tmp/jack.json")
  (setf *json-data* (read-json (read in))))

Get keys of a jso

(defun jso-keys (jso)
   (let ((keys ()))
     (mapjso #'(lambda (k v) (push k keys)) jso)
     keys))

(getjso "age" *json-data*)
(getjso* "pets.Lucy" *json-data*)

(mapjso #'(lambda (k v) (print (list k v))) *json-data*)

## Files

(uiop:getcwd)

(uiop:chdir "~/tmp")

(uiop:directory-files "")

(uiop:subdirectories "")

## Variable arguments to macros might have to be evaled

For example, calling the ST-JSON macro getjso* does not work when passing a var. It works with a string argument.

(defmacro jso*-wrapper (k jso)
  `(getjso* ,(eval k) ,jso))

(defparameter *key* "enaBasinObs")

(jso*-wrapper *key* *tok-json*)

## Format a float in scientific notation

(defun e (val) (format t "  ~e" val) t)

## Get previous values in the REPL

*, **, and *** are bound to the previously computed values.

## SBCL executable

Load the source file
(load "/home/heitor/experimental/lisp/hello.lisp")

Then build it
(sb-ext:save-lisp-and-die "/home/heitor/hellobin" :toplevel #'hello-function :executable t)

Use (progn ...) to make it a single sexp.

#|

(progn
  (load "/home/heitor/experimental/lisp/cmd-adder.lisp")
  (sb-ext:save-lisp-and-die "/home/heitor/bin/add" :toplevel #'main :executable t))

|#