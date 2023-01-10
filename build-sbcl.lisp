;; Load the required libraries.
;; You need to have quicklisp installed for this to work.
(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

;; Compile and load everything
(defvar *sources* 
  (list
    (pathname "src/package.lisp")
    (pathname "src/string.lisp")
    (pathname "src/stream.lisp")
    (pathname "src/arguments.lisp")
    (pathname "src/main.lisp")))
(loop for i in *sources* do
      (load (compile-file i)))

;; TODO: check if we're on windows and change executable
;; name if we are.
(sb-ext:save-lisp-and-die "cl-http-daemon"
			  :toplevel 'http-daemon:main
			  :executable t
			  :compression 22)
