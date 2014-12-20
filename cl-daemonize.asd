;;;; cl-daemonize.asd

(asdf:defsystem #:cl-daemonize
  :serial t
  :depends-on (#:cffi #:trivial-backtrace)
  :components ((:file "package")
               (:file "cl-daemonize")))
