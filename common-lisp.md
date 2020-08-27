# Common Lisp

https://lispcookbook.github.io/cl-cookbook/debugging.html

## SBCL

### Debugger

To enable a more complete backtrace, evaluate in the beginning:

`(declaim (optimize (speed 0) (space 0) (debug 3)))`

`ba` print backtrace

`top` return to top level