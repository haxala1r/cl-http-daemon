(defpackage :http-daemon
  (:use :common-lisp
	:usocket
	:bordeaux-threads
	:uiop)
  (:export
    :main))
