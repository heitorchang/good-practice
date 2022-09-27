# Hunchentoot on NearlyFreeSpeech

Install quicklisp in /home/private/quicklisp

Add to /home/protected/hunch.lisp:

```
(load "/home/private/quicklisp/setup.lisp")
(format t "Starting server")

(ql:quickload 'hunchentoot)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
```

Add to /home/protected/run-hunch.sh

```
#!/bin/sh

sbcl --script /home/protected/hunch.lisp
```

chmod +x run-hunch.sh

Setup a daemon and http proxy on port 4242