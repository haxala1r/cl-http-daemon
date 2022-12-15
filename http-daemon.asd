(defsystem "http-daemon"
  :build-operation program-op
  :build-pathname "cl-http-daemon"
  :entry-point "http-daemon:main"
  :depends-on ("usocket" "bordeaux-threads")
  :components ( 
		(:file "src/http-daemon-package")
		(:file "src/string")
		(:file "src/stream")
		(:file "src/main")))
