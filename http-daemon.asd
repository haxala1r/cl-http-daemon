(defpackage :http-daemon-system
    (:use :common-lisp :asdf))
(in-package :http-daemon-system)
(defsystem "http-daemon"
  :build-operation "program-op"
  :build-pathname "cl-http-daemon"
  :entry-point "http-daemon:main"
  :depends-on ("usocket" "bordeaux-threads")
  :components (
		(:file "src/package")
		(:file "src/string" :depends-on ("src/package"))
		(:file "src/stream" :depends-on ("src/package"))
		(:file "src/arguments" :depends-on ("src/package"))
		(:file "src/main" :depends-on ("src/package"))))
