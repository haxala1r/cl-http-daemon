(defpackage :local.http-server
  (:use :common-lisp)
  (:export
    :main))

(ql:quickload "usocket")
(ql:quickload "bordeaux-threads")

(load "proxy-string.lisp")
(load "proxy-stream.lisp")
(load "proxy.lisp")
