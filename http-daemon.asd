(defsystem "http-daemon"
  :depends-on ("usocket" "bordeaux-threads")
  :components ( 
		(:file "src/http-daemon-package")
		(:file "src/string")
		(:file "src/stream")
		(:file "src/main")))
