(defpackage :http-daemon
  (:use :common-lisp
	:usocket
	:bordeaux-threads)
  (:export
    :main))
