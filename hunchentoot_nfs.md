# Hunchentoot on NearlyFreeSpeech

Install quicklisp in /home/private/quicklisp

ssh to nfsnssh.com
cd /home/private
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
(quicklisp-quickstart:install)
;; no need to add to init file

Add to /home/protected/hunch.lisp:

```
(load "/home/private/quicklisp/setup.lisp")
(format t "Starting server")

(ql:quickload 'hunchentoot)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
```

Add to /home/protected/runhunch.sh

```
#!/bin/sh

sbcl --script /home/protected/hunch.lisp
```

chmod +x runhunch.sh

Setup a daemon and http proxy on port 4242