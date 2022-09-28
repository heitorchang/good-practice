# Hunchentoot on NearlyFreeSpeech

Install quicklisp in /home/private/quicklisp

ssh to nfsnssh.com
cd /home/private
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file) ; edits .sbclrc


Add to /home/protected/hunch.lisp:

```
(ql:quickload 'hunchentoot)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
```

Snooze

snooze.lisp
```
(ql:quickload "snooze")
(ql:quickload "hunchentoot")

(defvar *visitors* '())

(snooze:defroute my-snooze (:get :text/* name)
  (setf *visitors* (cons name *visitors*))
  (format nil "Hello ~{~a ~}" *visitors*))

(push (snooze:make-hunchentoot-app) hunchentoot:*dispatch-table*)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
```

Daemon should run in the foreground

runsnooze.sh
```
#!/bin/sh

exec sbcl --load /home/protected/snooze.lisp
```

chmod a+x runsnooze.shSnooze

Setup the daemon to run as "me"

Setup http proxy on port 4242
