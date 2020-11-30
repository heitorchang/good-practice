# Common Lisp

https://lispcookbook.github.io/cl-cookbook/debugging.html

## SBCL

### Debugger

To enable a more complete backtrace, evaluate in the beginning:

`(declaim (optimize (speed 0) (space 0) (debug 3)))`

Add the above line to ~/.sbclrc, optionally add `(format t "Debug set to 3")`

`ba` print backtrace

`top` returns to top level

## CGI

(from nearlyfreespeech forums)

#!/usr/local/bin/sbcl

(format t "Content-type: text/html%~%~")
(format t "<h1>Hello</h1>")

Save as filename.cgi and chmod 755 filename.cgi