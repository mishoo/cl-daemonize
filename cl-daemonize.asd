;;;; cl-daemonize.asd

(asdf:defsystem #:cl-daemonize
  :serial t
  :depends-on (#:cl-fad #:cffi #:trivial-backtrace #:swank)
  :components ((:file "package")
               (:file "cl-daemonize")))
