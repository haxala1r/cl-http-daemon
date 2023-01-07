(require :asdf)
(asdf:load-system :usocket)
(asdf:load-system :bordeaux-threads)

(defvar *sources* 
  (list
    (pathname "src/package.lisp")
    (pathname "src/string.lisp")
    (pathname "src/stream.lisp")
    (pathname "src/arguments.lisp")
    (pathname "src/main.lisp")))
(loop for i in *sources* do
      (load (compile-file i)))

;; Call main with no parameters.
(http-daemon:main)
