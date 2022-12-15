(defvar *sources* 
  (list
    (pathname "src/http-daemon-package.lisp")
    (pathname "src/string.lisp")
    (pathname "src/stream.lisp")
    (pathname "src/main.lisp")))
(loop for i in *sources* do
      (load (compile-file i)))

;; Call main with no parameters.
(http-daemon:main)
