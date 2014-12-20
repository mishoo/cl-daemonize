;;;; package.lisp

(defpackage #:cl-daemonize
  (:use #:cl #:sb-alien #:cffi)
  (:export #:daemonize #:restart-process))
